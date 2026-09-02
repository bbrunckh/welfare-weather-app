# ============================================================================ #
# tests/testthat/test-geojson-bounds.R                                         #
# PERF-36: .geojson_bounds() prefers the DuckDB-computed per-feature bbox      #
# (properties$bbox = [xmin, ymin, xmax, ymax]) and never re-parses geometry    #
# when every feature carries one. build_cell_features() must propagate the     #
# bbox columns from the cell geometry frame into feature properties.           #
# ============================================================================ #

library(testthat)

make_hex_geom_json <- function(lng, lat, d = 0.01) {
  xs <- format(c(lng - d, lng + d, lng + d, lng - d, lng - d),
               digits = 12, trim = TRUE)
  ys <- format(c(lat - d, lat - d, lat + d, lat + d, lat - d),
               digits = 12, trim = TRUE)
  pairs <- paste0("[", xs, ",", ys, "]", collapse = ",")
  sprintf('{"type":"Polygon","coordinates":[[%s]]}', pairs)
}

test_that(".geojson_bounds bbox fast path matches the parsed-geometry fallback", {
  pts <- data.frame(
    lng = c(10.2, 10.9, 11.4),
    lat = c(-1.3, -1.1, -1.9)
  )
  feats <- lapply(seq_len(nrow(pts)), function(i) {
    list(
      type       = "Feature",
      geom_json  = make_hex_geom_json(pts$lng[i], pts$lat[i]),
      properties = list(loc_id = paste0("L", i))
    )
  })

  parsed   <- wiseapp:::.geojson_bounds(list(features = feats))

  feats_bbox <- lapply(seq_along(feats), function(i) {
    f <- feats[[i]]
    d <- 0.01
    # The bbox of the ring above is (lng - d, lat - d, lng + d, lat + d).
    f$properties$bbox <- as.numeric(c(pts$lng[i] - d, pts$lat[i] - d,
                                      pts$lng[i] + d, pts$lat[i] + d))
    f
  })
  fast <- wiseapp:::.geojson_bounds(list(features = feats_bbox))

  expect_equal(fast$lng1, parsed$lng1)
  expect_equal(fast$lat1, parsed$lat1)
  expect_equal(fast$lng2, parsed$lng2)
  expect_equal(fast$lat2, parsed$lat2)
})

test_that(".geojson_bounds falls back to parsing when any feature lacks a bbox", {
  f1 <- list(type = "Feature",
             geom_json = make_hex_geom_json(10.2, -1.3),
             properties = list(loc_id = "L1",
                               bbox = c(10.19, -1.31, 10.21, -1.29)))
  f2 <- list(type = "Feature",
             geom_json = make_hex_geom_json(11.4, -1.9),
             properties = list(loc_id = "L2"))
  mixed <- wiseapp:::.geojson_bounds(list(features = list(f1, f2)))

  expect_equal(mixed$lng1, 10.19)
  expect_equal(mixed$lat1, -1.91, tolerance = 1e-12)
  expect_equal(mixed$lng2, 11.41, tolerance = 1e-12)
  expect_equal(mixed$lat2, -1.29)
})

test_that(".geojson_bounds bbox path narrows correctly on wave-filtered features", {
  mk <- function(lng, lat, wave) {
    list(type = "Feature", geom_json = make_hex_geom_json(lng, lat),
         properties = list(loc_id = wave,
                           bbox = c(lng - 0.01, lat - 0.01, lng + 0.01, lat + 0.01)))
  }
  feats <- list(mk(10, 0, "A"), mk(11, 1, "A"), mk(30, 20, "B"))
  subset_bounds <- wiseapp:::.geojson_bounds(list(features = feats[1:2]))

  expect_equal(subset_bounds$lng1, 9.99)
  expect_equal(subset_bounds$lng2, 11.01)
  expect_equal(subset_bounds$lat1, -0.01)
  expect_equal(subset_bounds$lat2, 1.01)
})

test_that("build_cell_features propagates bbox columns into properties", {
  cell_geo <- data.frame(
    h3   = c("8001", "8002"),
    geom = c(make_hex_geom_json(10, 0), make_hex_geom_json(11, 1)),
    xmin = c(9.99, 10.99), ymin = c(-0.01, 0.99),
    xmax = c(10.01, 11.01), ymax = c(0.01, 1.01),
    stringsAsFactors = FALSE
  )
  cell_map <- data.frame(
    code     = "TST",
    year     = "2021",
    survname = "SRV",
    loc_id   = c("L1", "L1", "L2"),
    h3       = c("8001", "8002", "8002"),
    stringsAsFactors = FALSE
  )

  out <- build_cell_features(cell_geo, cell_map, by_wave = TRUE)
  expect_length(out$features, 2L) # distinct (code, year, survname, h3) combos
  expect_true(all(vapply(out$features, function(f)
    is.numeric(f$properties$bbox) && length(f$properties$bbox) == 4L, logical(1))))
  expect_equal(out$features[[1]]$properties$bbox, c(9.99, -0.01, 10.01, 0.01))
  # Fast path engages: bounds come straight from the bboxes.
  b <- wiseapp:::.geojson_bounds(out)
  expect_equal(b$lng1, 9.99)
  expect_equal(b$lat2, 1.01)

  # Without bbox columns the output is unchanged apart from the missing prop.
  cell_geo_plain <- cell_geo[, c("h3", "geom")]
  out_plain <- build_cell_features(cell_geo_plain, cell_map, by_wave = TRUE)
  expect_null(out_plain$features[[1]]$properties$bbox)
})
