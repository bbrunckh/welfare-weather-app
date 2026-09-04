# ============================================================================ #
# tests/testthat/test-fct_hexmap.R                                             #
# Hex-map engine: payload contract, density payload/ramp parity with the       #
# Leaflet fallback, message senders, and container markup.                     #
# ============================================================================ #

library(testthat)

make_density_cells <- function(n = 4) {
  data.frame(
    h3      = sprintf("89754e6ffffff%02x", seq_len(n) - 1L),
    geom    = rep('{"type":"Polygon"}', n),
    n_units = c(1, 3.5, 40, 250)[seq_len(n)],
    xmin    = seq(-19.5, length.out = n),
    ymin    = seq(27.0, length.out = n),
    xmax    = seq(-19.1, length.out = n),
    ymax    = seq(27.4, length.out = n),
    stringsAsFactors = FALSE
  )
}

# ---- hexmap_payload ---------------------------------------------------------

test_that("hexmap_payload: columnar shape, unnamed vectors, action = set", {
  pl <- hexmap_payload(
    h3 = c("89a", "89b"), v = c(1, 2), v_kind = "continuous",
    stops = list(domain = c(0, 1), colors = c("#000000", "#ffffff")),
    bounds = c(1, 2, 3, 4), label = "L", unit = "u"
  )
  expect_identical(pl$action, "set")
  expect_identical(pl$h3, c("89a", "89b"))
  expect_null(names(pl$h3))
  expect_null(names(pl$v))
  expect_identical(pl$v_kind, "continuous")
  expect_identical(pl$bounds, c(1, 2, 3, 4))
  expect_identical(pl$label, "L")
  expect_identical(pl$unit, "u")
})

test_that("hexmap_payload: drops NA/blank cells and NA values pass through", {
  pl <- hexmap_payload(
    h3 = c("89a", NA, "", "89c"), v = c(1, 2, 3, NA), v_kind = "continuous",
    stops = list(domain = c(0, 1), colors = c("#000000", "#ffffff"))
  )
  expect_identical(pl$h3, c("89a", "89c"))
  expect_identical(pl$v, c(1, NA))
})

test_that("hexmap_payload: binned values are coerced to character levels", {
  pl <- hexmap_payload(
    h3 = "89a", v = factor("bin2"), v_kind = "binned",
    stops = list(levels = c("bin1", "bin2"), colors = c("#111111", "#222222"))
  )
  expect_identical(pl$v, "bin2")
  expect_identical(pl$stops$levels, c("bin1", "bin2"))
})

test_that("hexmap_payload: validates lengths and bounds", {
  expect_error(
    hexmap_payload(h3 = c("a", "b"), v = 1, v_kind = "continuous",
                   stops = list(domain = c(0, 1), colors = c("#000", "#fff"))),
    "same length"
  )
  expect_error(
    hexmap_payload(h3 = "a", v = 1, v_kind = "continuous",
                   stops = list(domain = c(0, 1), colors = c("#000", "#fff")),
                   bounds = c(1, 2, 3)),
    "bounds"
  )
  expect_error(
    hexmap_payload(h3 = "a", v = 1, v_kind = "continuous",
                   stops = list(domain = c(0, 1), colors = c("#000", "#fff")),
                   v_log = c(1, 2)),
    "v_log"
  )
})

# ---- density payload --------------------------------------------------------

test_that("density payload: binned levels, stops, bbox bounds", {
  cells <- make_density_cells() # n_units = 1, 3.5, 40, 250
  pl <- wiseapp:::.density_hex_payload(cells, "households")

  expect_identical(pl$payload$action, "set")
  expect_identical(pl$payload$v_kind, "binned")
  expect_identical(pl$payload$h3, cells$h3)

  # v holds one level string per cell; every level is in the ramp stops.
  lvls <- pl$payload$stops$levels
  expect_true(all(pl$payload$v %in% lvls))
  expect_identical(pl$payload$stops$levels, lvls)
  expect_length(pl$payload$stops$colors, length(lvls))
  # The fixture's smallest cell (1 hh) is not thin: no "< 1" bin appears.
  expect_false("< 1" %in% lvls)

  # Bounds span the per-cell bbox columns.
  expect_identical(pl$payload$bounds,
                   c(min(cells$xmin), min(cells$ymin),
                     max(cells$xmax), max(cells$ymax)))

  # Legend is binned over the same levels.
  expect_true(pl$legend$binned)
  expect_identical(pl$legend$levels, lvls)
  expect_identical(pl$legend$title, "Households per area")
})

test_that("density payload: thin cells land in the pale '< 1' bin", {
  cells <- make_density_cells()
  cells$n_units <- c(0.5, 0.02, 40, 250)
  pl <- wiseapp:::.density_hex_payload(cells, "households")

  lvls <- pl$payload$stops$levels
  expect_identical(lvls[1], "< 1")
  expect_identical(pl$payload$v[1:2], c("< 1", "< 1"))
  # Occupied bins split the >= 1 range at its quantiles.
  expect_true(all(nzchar(pl$payload$v)))
})

test_that("density payload: degenerate and empty inputs return NULL", {
  expect_null(wiseapp:::.density_hex_payload(NULL, "households"))
  expect_null(wiseapp:::.density_hex_payload(
    data.frame(h3 = character(0), n_units = numeric(0)), "households"))
  # All counts non-positive -> nothing to draw.
  expect_null(wiseapp:::.density_hex_payload(
    data.frame(h3 = "89a", n_units = 0), "households"))
})

test_that("density ramp: '< 1' bin plus quantile bins above", {
  cells <- make_density_cells()
  cells$n_units <- c(0.5, 0.02, 40, 250)
  r <- wiseapp:::.density_ramp(cells$n_units)

  # The thin bin exists and the occupied range is split at its quantiles.
  expect_identical(r$levels[1], "< 1")
  expect_true(length(r$levels) >= 3)
  expect_length(r$colors, length(r$levels))
  expect_identical(r$cuts[1], 1)
  expect_true(r$thin)

  # No thin cells: no "< 1" level, and the occupied quantile bins remain.
  r2 <- wiseapp:::.density_ramp(c(1, 3.5, 40, 250))
  expect_false("< 1" %in% r2$levels)
  expect_true(all(nzchar(r2$levels)))
  expect_false(r2$thin)
})

# ---- senders ----------------------------------------------------------------

test_that("senders: set/clear/fit route on the fixed type with namespaced id", {
  sent <- list()
  session <- list(
    sendCustomMessage = function(type, msg) {
      sent[[length(sent) + 1L]] <<- list(type = type, msg = msg)
    }
  )
  ns <- function(x) paste0("mod-", x)

  hexmap_update(session, ns, "density_map",
                hexmap_payload(h3 = "89a", v = 1, v_kind = "continuous",
                               stops = list(domain = c(0, 1),
                                            colors = c("#000", "#fff"))))
  hexmap_clear(session, ns, "density_map")
  hexmap_fit(session, ns, "density_map", c(1, 2, 3, 4))
  hexmap_fit(session, ns, "density_map", NULL)          # skipped
  hexmap_fit(session, ns, "density_map", c(1, 2, 3))    # invalid: skipped

  expect_length(sent, 3L)
  expect_identical(sent[[1]]$type, "hexmap")
  expect_identical(sent[[1]]$msg$id, "mod-density_map")
  expect_identical(sent[[1]]$msg$action, "set")
  expect_identical(sent[[2]]$msg$action, "clear")
  expect_identical(sent[[3]]$msg$action, "fit")
  expect_identical(sent[[3]]$msg$bounds, c(1, 2, 3, 4))
})

# ---- UI ---------------------------------------------------------------------

test_that("hexmap_ui: container id, aria, input hooks, legend slot", {
  ui <- hexmap_ui("mod-density_map", height = "100%",
                  aria_label = "Density map",
                  legend = shiny::tags$div("legend"))
  html <- as.character(htmltools::doRenderTags(ui))

  expect_match(html, "id=\"mod-density_map\"", fixed = TRUE)
  expect_match(html, "role=\"region\"", fixed = TRUE)
  expect_match(html, "aria-label=\"Density map\"", fixed = TRUE)
  expect_match(html, "data-hexmap-click=\"mod-density_map_hex_click\"",
               fixed = TRUE)
  expect_match(html, "hexmap-legend", fixed = TRUE)
  expect_false(grepl("data-hexmap-webgl", html, fixed = TRUE))
})

test_that("hexmap_dependency: scripts in engine order", {
  dep <- hexmap_dependency()
  expect_identical(
    dep$script,
    c("vendor/maplibre-gl.js", "vendor/h3-js.umd.js", "hexmap.js")
  )
  expect_identical(dep$stylesheet, "vendor/maplibre-gl.css")
  for (s in dep$script) {
    expect_true(file.exists(file.path(dep$src[[1]], s)), info = s)
  }
})
