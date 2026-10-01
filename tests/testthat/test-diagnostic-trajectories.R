test_that("trajectory chart uses data-bounded axes and explicit envelope polygons", {
  ts <- expand.grid(
    scenario = c("Historical", "SSP2-4.5 / 2030-2040"),
    model_id = c("model__one", "model_two"),
    sim_year = c(1999L, 2020L, 2021L),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  ts$value <- ifelse(ts$scenario == "Historical", 12, 15) +
    ifelse(ts$model_id == "model__one", 0.2, -0.2) + seq_len(nrow(ts)) / 100
  ts$is_historical <- ts$scenario == "Historical"
  chart <- echart_timeseries_spaghetti(ts, "Mean welfare", c(lo = .1, hi = .9))

  expect_s3_class(chart, "echarts4r")
  expect_gt(chart$x$opts$xAxis$min, 1998)
  expect_lt(chart$x$opts$xAxis$min, 1999)
  expect_gt(chart$x$opts$xAxis$max, 2021)
  expect_gt(chart$x$opts$yAxis$min, 0)
  expect_lt(chart$x$opts$yAxis$min, min(ts$value))
  expect_gt(chart$x$opts$yAxis$max, max(ts$value))
  expect_match(as.character(chart$x$opts$xAxis$axisLabel$formatter), "Math.round", fixed = TRUE)

  bands <- Filter(function(s) identical(s$type, "custom"), chart$x$opts$series)
  expect_length(bands, 1L)
  expect_length(bands[[1L]]$data, 1L)
  points <- jsonlite::fromJSON(sub(".*var pts = (.*); return.*", "\\1",
    as.character(bands[[1L]]$renderItem)))
  band_point <- points[2L, ]
  expected_lower <- unname(stats::quantile(
    ts$value[ts$scenario != "Historical" & ts$sim_year == band_point[[1L]]], .1
  ))
  expect_equal(band_point[[2L]], expected_lower)
  expect_match(as.character(bands[[1L]]$renderItem), "api.coord(p)", fixed = TRUE)
  expect_equal(nrow(points), 6)
  expect_true("Historical" %in% as.character(unlist(chart$x$opts$legend$data)))
  expect_identical(chart$x$opts$legend$left, "center")
})

test_that("trajectory source groups stay distinct and model tooltips are bounded", {
  ts <- expand.grid(
    scenario = c("Historical", "SSP3-7.0 / 2030-2040"),
    source = c("Baseline", "Policy"),
    model_id = c("m1", "m2"),
    sim_year = 2020:2021,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  ts$value <- 10 + match(ts$source, c("Baseline", "Policy")) +
    match(ts$model_id, c("m1", "m2")) / 10
  ts$is_historical <- ts$scenario == "Historical"
  ts$member_id <- paste0("member-", ts$model_id)
  chart <- echart_timeseries_spaghetti(ts, "Poverty rate (percent)")

  lines <- Filter(function(s) identical(s$type, "line"), chart$x$opts$series)
  model_lines <- Filter(function(s) any(vapply(s$data, function(p) identical(p$kind, "model"), logical(1))), lines)
  median_lines <- Filter(function(s) any(vapply(s$data, function(p) identical(p$kind, "median"), logical(1))), lines)
  # Historical policy rows are intentionally excluded by the chart contract.
  expect_length(model_lines, 6L)
  expect_length(median_lines, 3L)
  expect_true(all(vapply(model_lines, function(s) !isTRUE(s$silent), logical(1))))
  expect_true(all(vapply(model_lines, function(s) !is.null(s$data[[1L]]$member), logical(1))))
  expect_true(all(grepl(" / (Baseline|Policy)$", vapply(median_lines, `[[`, character(1), "name"))))
  expect_match(as.character(chart$x$opts$yAxis$axisLabel$formatter), "v*100", fixed = TRUE)
  expect_match(as.character(chart$x$opts$tooltip$formatter), "Min ", fixed = TRUE)
  expect_match(as.character(chart$x$opts$tooltip$formatter), "Weather year", fixed = TRUE)
  expect_match(as.character(chart$x$opts$tooltip$formatter), " / Median ", fixed = TRUE)
  expect_identical(chart$x$opts$yAxis$nameLocation, "end")
  expect_equal(chart$x$opts$yAxis$nameRotate, 0)
  expect_identical(chart$x$opts$legend$top, 4)
  expect_lte(chart$x$opts$grid$left, 10)
})

test_that("trajectory chart returns an empty state without finite inputs", {
  ts <- data.frame(
    scenario = "Historical", model_id = "m1", sim_year = 2020,
    value = NA_real_, is_historical = TRUE
  )
  chart <- echart_timeseries_spaghetti(ts)
  expect_match(chart$x$opts$title[[1L]]$text, "No finite model trajectories available.", fixed = TRUE)
})
