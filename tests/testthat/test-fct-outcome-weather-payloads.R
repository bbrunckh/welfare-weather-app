# ============================================================================ #
# tests/testthat/test-fct-outcome-weather-payloads.R                           #
# Phase 2/3 payload builders: outcome coverage and weather cell maps.          #
# ============================================================================ #

library(testthat)

make_cell_geo <- function(n = 3) {
  data.frame(
    h3   = sprintf("87975404%dfffff", 8:10)[seq_len(n)],
    geom = rep('{"type":"Polygon"}', n),
    xmin = seq(-19.6, length.out = n),
    ymin = seq(27.0, length.out = n),
    xmax = seq(-19.2, length.out = n),
    ymax = seq(27.4, length.out = n),
    stringsAsFactors = FALSE
  )
}

make_cmap <- function(geo) {
  data.frame(
    code     = "TST",
    year     = "2021",
    survname = "SRV",
    loc_id   = paste0("L", seq_along(geo$h3)),
    h3       = geo$h3,
    pop_2020 = c(10, 20, 30)[seq_along(geo$h3)],
    stringsAsFactors = FALSE
  )
}

# ---- outcome coverage payload ------------------------------------------------

test_that("coverage payload: per-cell coverage, ramp, identifiers, bounds", {
  geo  <- make_cell_geo(2)
  cmap <- make_cmap(geo)
  df <- data.frame(
    code = "TST", year = "2021", survname = "SRV",
    loc_id = paste0("L", 1:2),
    welfare = c(1.2, NA),          # L1 100% coverage, L2 0%
    stringsAsFactors = FALSE
  )

  pl <- wiseapp:::.coverage_hex_payload(geo, cmap, df, "welfare")

  expect_identical(pl$payload$action, "set")
  expect_identical(pl$payload$v_kind, "continuous")
  expect_identical(pl$payload$v, c(100, 0))
  expect_identical(pl$payload$stops$colors,
                   c("#D55E00", "#E69F00", "#009E73"))
  # Tooltip identifiers come from the cell map columns.
  expect_match(pl$payload$info[1], "TST 2021 SRV")
  expect_identical(pl$payload$bounds,
                   c(min(geo$xmin), min(geo$ymin),
                     max(geo$xmax), max(geo$ymax)))
  # Legend shares the payload's domain.
  expect_equal(as.numeric(pl$legend$pal_info$domain),
               pl$payload$stops$domain, tolerance = 1e-9)
  expect_identical(pl$legend$title, "% available")
})

test_that("coverage payload: cells without a value ride along as NA (grey)", {
  geo  <- make_cell_geo(2)
  cmap <- make_cmap(geo)
  # L3 has no rows at all: its cell keeps its place in the drawn set.
  cmap$h3[2] <- geo$h3[2]
  df <- data.frame(
    code = "TST", year = "2021", survname = "SRV",
    loc_id = c("L1"), welfare = 2,
    stringsAsFactors = FALSE
  )
  cmap <- cmap[cmap$loc_id %in% c("L1", "L2"), , drop = FALSE]
  cmap$h3 <- geo$h3[1:2]

  pl <- wiseapp:::.coverage_hex_payload(geo, cmap, df, "welfare")
  expect_identical(pl$payload$h3, geo$h3)
  expect_true(is.na(pl$payload$v[2]))
})

test_that("coverage payload: degenerate inputs return NULL", {
  geo  <- make_cell_geo(2)
  cmap <- make_cmap(geo)
  df <- data.frame(code = "TST", year = "2021", survname = "SRV",
                   loc_id = "L1", welfare = 1)
  expect_null(wiseapp:::.coverage_hex_payload(NULL, cmap, df, "welfare"))
  expect_null(wiseapp:::.coverage_hex_payload(geo, NULL, df, "welfare"))
  expect_null(wiseapp:::.coverage_hex_payload(geo, cmap, NULL, "welfare"))
})

test_that("coverage payload: single value gets a 1-wide domain", {
  geo  <- make_cell_geo(2)
  cmap <- make_cmap(geo)
  df <- data.frame(
    code = "TST", year = "2021", survname = "SRV",
    loc_id = paste0("L", 1:2), welfare = c(1, 2),
    stringsAsFactors = FALSE
  )
  pl <- wiseapp:::.coverage_hex_payload(geo, cmap, df, "welfare")
  # Same uniform-coverage token span: a single value gets a 1-wide domain.
  expect_equal(diff(pl$payload$stops$domain), 1, tolerance = 1e-9)
})

# ---- weather payload ---------------------------------------------------------

test_that("weather payload: continuous values, averaged dash, notes", {
  geo  <- make_cell_geo(3)
  cmap <- make_cmap(geo)
  sub <- data.frame(
    code = "TST", year = "2021", survname = "SRV",
    loc_id = geo$h3, value = c(1.5, NA, 2.5),
    n_hh = c(30, 10, 30), n_months = c(2L, 1L, 1L),
    stringsAsFactors = FALSE
  )
  pal <- wiseapp:::.weather_map_palette(c(0.5, 2.8), FALSE, NULL, "None")

  pl <- wiseapp:::.weather_hex_payload(geo, cmap, sub, pal)

  expect_identical(pl$payload$v_kind, "continuous")
  expect_identical(pl$payload$v, c(1.5, NA, 2.5))
  # Only the multi-month cell is dashed; its tooltip says so.
  expect_identical(pl$payload$dash, c(TRUE, FALSE, FALSE))
  expect_match(pl$payload$info[1], "2 interview months averaged")
  expect_true(all(is.na(pl$payload$info[2:3])))
  expect_identical(pl$payload$stops$domain, c(0.5, 2.8))
  expect_length(pl$payload$stops$colors, 9L)
  # Notes count the greys and the dashed cells.
  expect_match(pl$legend$notes, "1 of 3 areas without weather")
  expect_match(pl$legend$notes, "1 of 3 areas averaged")
})

test_that("weather payload: binned variables send a level match ramp", {
  geo  <- make_cell_geo(3)
  cmap <- make_cmap(geo)
  sub <- data.frame(
    code = "TST", year = "2021", survname = "SRV",
    loc_id = geo$h3, value = c("Low", "High", "Low"),
    n_hh = c(30, 10, 30), n_months = 1L,
    stringsAsFactors = FALSE
  )
  attr(sub, "binned") <- TRUE
  attr(sub, "levels") <- c("Low", "High")
  pal <- wiseapp:::.weather_map_palette(c("Low", "High"), TRUE,
                                        c("Low", "High"), "None")

  pl <- wiseapp:::.weather_hex_payload(geo, cmap, sub, pal)

  expect_identical(pl$payload$v_kind, "binned")
  expect_identical(pl$payload$v, c("Low", "High", "Low"))
  expect_identical(pl$payload$stops$levels, c("Low", "High"))
  expect_length(pl$payload$stops$colors, 2L)
  expect_false(any(pl$payload$dash))
})

test_that("weather payload: degenerate inputs return NULL", {
  geo  <- make_cell_geo(2)
  cmap <- make_cmap(geo)
  sub <- data.frame(code = "TST", year = "2021", survname = "SRV",
                    loc_id = geo$h3[1], value = 1, n_hh = 1, n_months = 1L)
  pal <- wiseapp:::.weather_map_palette(1, FALSE, NULL, "None")
  expect_null(wiseapp:::.weather_hex_payload(NULL, cmap, sub, pal))
  expect_null(wiseapp:::.weather_hex_payload(geo, NULL, sub, pal))
  expect_null(wiseapp:::.weather_hex_payload(geo, cmap, NULL, pal))
})

test_that("weather payload: palette carries hex colours for both renderers", {
  # The payload's colour stops must come from the same palette the Leaflet
  # fallback samples, so the two maps cannot drift.
  pal <- wiseapp:::.weather_map_palette(c(1, 5), FALSE, NULL, "None")
  expect_identical(pal$colors,
                   pal$pal(seq(pal$domain[1], pal$domain[2], length.out = 9)))
  pal_d <- wiseapp:::.weather_map_palette(c(-2, 2), FALSE, NULL,
                                          "None", force = "diverging")
  expect_identical(pal_d$colors,
                   pal_d$pal(seq(pal_d$domain[1], pal_d$domain[2],
                                 length.out = 9)))
})
