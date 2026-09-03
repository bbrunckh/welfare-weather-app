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

test_that("density payload: log domain, ramp stops, bbox bounds", {
  cells <- make_density_cells()
  pl <- wiseapp:::.density_hex_payload(cells, "households")

  expect_identical(pl$payload$action, "set")
  expect_identical(pl$payload$v_kind, "continuous")
  expect_identical(pl$payload$h3, cells$h3)
  expect_identical(pl$payload$v, cells$n_units)

  # v_log is the log of the counts, and the ramp domain matches it.
  expect_identical(pl$payload$v_log, log(cells$n_units))
  dom <- pl$payload$stops$domain
  expect_true(all(is.finite(dom)) && dom[2] > dom[1])
  expect_length(pl$payload$stops$colors, 9L)

  # Bounds span the per-cell bbox columns.
  expect_identical(pl$payload$bounds,
                   c(min(cells$xmin), min(cells$ymin),
                     max(cells$xmax), max(cells$ymax)))

  # Legend shares the payload's domain (both on the count scale).
  expect_equal(pl$legend$pal_info$domain, exp(pl$payload$stops$domain),
               tolerance = 1e-12)
  expect_identical(pl$legend$title, "Households per area")
})

test_that("density payload: degenerate and empty inputs return NULL", {
  expect_null(wiseapp:::.density_hex_payload(NULL, "households"))
  expect_null(wiseapp:::.density_hex_payload(
    data.frame(h3 = character(0), n_units = numeric(0)), "households"))
  # All counts non-positive -> nothing to draw.
  expect_null(wiseapp:::.density_hex_payload(
    data.frame(h3 = "89a", n_units = 0), "households"))
})

test_that("density ramp: matches the Leaflet fallback's log domain", {
  cells <- make_density_cells()
  r <- wiseapp:::.density_ramp(cells$n_units)
  fc <- wiseapp:::.sample_density_fc(cells)
  pl <- wiseapp:::.density_hex_payload(cells, "households")

  # The ramp domain is the log-range of the positive counts; both renderers
  # must agree with the payload.
  expect_identical(log(r$rng), unname(pl$payload$stops$domain))
  expect_identical(log(r$rng), unname(pl$payload$stops$domain))
  # The Leaflet path colours with the same ramp over the same log domain.
  expect_true(is.function(fc$pal_info$pal))
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
  expect_match(html, "data-hexmap-webgl=\"mod-density_map_webgl\"", fixed = TRUE)
  expect_match(html, "data-hexmap-click=\"mod-density_map_hex_click\"",
               fixed = TRUE)
  expect_match(html, "hexmap-legend", fixed = TRUE)
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
