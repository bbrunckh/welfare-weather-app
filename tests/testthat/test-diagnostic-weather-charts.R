diag_weather_chart_fixture <- function() {
  survey <- data.frame(
    loc_id = "a", int_month = 1L,
    timestamp = as.Date(c("2020-01-01", "2021-01-01"))
  )
  raw <- data.frame(
    loc_id = "a", int_month = 1L,
    timestamp = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
    temp = c(8, 9, 10), rain = c(1, 2, 3),
    temp_bin = factor(c("(6, 8]", "(8, 10]", "(8, 10]"),
      levels = c("(6, 8]", "(8, 10]", "(10, 12]")),
    rain_bin = factor(c("(0, 2]", "(2, 4]", "(2, 4]"),
      levels = c("(0, 2]", "(2, 4]"))
  )
  scenarios <- list(
    "ssp245_2041_2060_MPI" = data.frame(
      temp = c(11, 12), rain = c(4, 5),
      temp_bin = factor(c("(10, 12]", "(10, 12]"), levels = levels(raw$temp_bin)),
      rain_bin = factor(c("(2, 4]", "(2, 4]"), levels = levels(raw$rain_bin))
    )
  )
  list(survey = survey, raw = raw, scenarios = scenarios)
}

test_that("Step 2 weather chart honors selected binned metadata", {
  f <- diag_weather_chart_fixture()
  specs <- data.frame(
    name = "temp_bin", label = "Temperature", units = "degC",
    cont_binned = "Binned", stringsAsFactors = FALSE
  )
  ch <- echart_weather_density_panel(
    f$survey, f$raw, "temp_bin", weather_specs = specs,
    scenario_weather = f$scenarios, show_regression = TRUE
  )

  expect_identical(ch$x$opts$xAxis$type, "category")
  expect_match(ch$x$opts$xAxis$name, "Temperature bins \\(degC\\)")
  expect_true(all(c("Historical", "Model support", "SSP2-4.5 / 2041-2060") %in%
    vapply(ch$x$opts$series, `[[`, character(1L), "name")))
  expect_identical(ch$x$opts$legend$top, 4)
  expect_true(is.null(names(ch$x$opts$series)))
})

test_that("continuous weather ridges retain every selected variable and source", {
  f <- diag_weather_chart_fixture()
  specs <- data.frame(
    name = c("temp", "rain"), label = c("Temperature", "Rainfall"),
    units = c("degC", "mm"), cont_binned = "Continuous",
    stringsAsFactors = FALSE
  )
  ch <- echart_weather_density_panel(
    f$survey, f$raw, c("temp", "rain"), weather_specs = specs,
    scenario_weather = f$scenarios, show_regression = TRUE
  )
  series_names <- vapply(ch$x$opts$series, `[[`, character(1L), "name")

  expect_s3_class(ch, "echarts4r")
  expect_true(any(grepl("Temperature \\| Historical", series_names)))
  expect_true(any(grepl("Rainfall \\| Historical", series_names)))
  expect_true(any(grepl("Temperature \\| Model support", series_names)))
  expect_true(any(grepl("Rainfall \\| SSP2-4.5 / 2041-2060", series_names)))
  expect_identical(ch$x$opts$legend$top, 4)
  expect_true(is.null(names(ch$x$opts$series)))
})

test_that("single continuous weather uses the shared 256-point ridge", {
  f <- diag_weather_chart_fixture()
  specs <- data.frame(
    name = "temp", label = "Temperature", units = "degC",
    cont_binned = "Continuous", stringsAsFactors = FALSE
  )
  ch <- echart_weather_density_panel(
    f$survey, f$raw, "temp", weather_specs = specs,
    scenario_weather = f$scenarios, show_regression = TRUE
  )
  names <- vapply(ch$x$opts$series, `[[`, character(1L), "name")

  expect_identical(names, c("Historical", "Model support", "SSP2-4.5 / 2041-2060"))
  expect_true(all(vapply(ch$x$opts$series, function(s) length(s$data) >= 256L, logical(1L))))
  expect_identical(ch$x$opts$legend$top, 4)
  expect_match(ch$x$opts$tooltip$formatter, "< x")
  expect_false(grepl("Density", ch$x$opts$yAxis$name %||% "", fixed = TRUE))
  expect_true(is.null(names(ch$x$opts$series)))
  log_chart <- echart_weather_density_panel(
    f$survey, f$raw, "temp", weather_specs = specs, log_x = TRUE
  )
  log_points <- log_chart$x$opts$series[[1L]]$data
  expect_true(any(vapply(log_points, function(p) {
    x <- p[[1L]]
    is.finite(x) && x >= log_chart$x$opts$xAxis$min && x <= log_chart$x$opts$xAxis$max
  }, logical(1))))
})

test_that("numeric columns selected as binned render categorically", {
  f <- diag_weather_chart_fixture()
  specs <- data.frame(
    name = "temp", label = "Temperature", units = "degC",
    cont_binned = "Binned", stringsAsFactors = FALSE
  )
  ch <- echart_weather_density_panel(
    f$survey, f$raw, "temp", weather_specs = specs,
    scenario_weather = f$scenarios
  )

  expect_identical(ch$x$opts$xAxis$type, "category")
  expect_true(all(c("8", "9", "10") %in% unlist(ch$x$opts$xAxis$data)))
  expect_false("Model support" %in% vapply(ch$x$opts$series, `[[`, character(1), "name"))
  expect_null(ch$x$opts$yAxis$name)
})

test_that("members sharing a scenario label are pooled rather than overwritten", {
  f <- diag_weather_chart_fixture()
  specs <- data.frame(name = "temp", label = "Temperature", units = "degC",
    cont_binned = "Binned")
  lo <- hi <- f$raw
  lo$temp <- 8
  hi$temp <- 10
  chart <- echart_weather_density_panel(f$survey, f$raw, "temp",
    weather_specs = specs, scenario_weather = list(
      "ssp3_7_0_2025_2035_model_a" = lo,
      "ssp3_7_0_2025_2035_model_b" = hi
    ))
  future <- Filter(function(s) grepl("SSP3", s$name), chart$x$opts$series)
  expect_length(future, 1L)
  expect_equal(unlist(future[[1L]]$data), c(.5, 0, .5))
})

test_that("stored cuts include numeric historical and future values", {
  survey <- data.frame(
    loc_id = "a", int_month = 1L,
    timestamp = as.Date(c("2020-01-01", "2021-01-01"))
  )
  raw <- data.frame(
    loc_id = "a", int_month = 1L,
    timestamp = as.Date(c("2019-01-01", "2020-01-01", "2021-01-01")),
    temp = c(8, 9, 10)
  )
  breaks <- list(temp = c(-Inf, 8, 10, Inf))
  attr(raw, "stored_breaks") <- breaks
  attr(raw, "continuous_weather") <- raw
  raw$temp <- cut(raw$temp, breaks = breaks$temp, include.lowest = TRUE)
  scenario <- list("ssp245_2041_2060_MPI" = data.frame(temp = c(11, 12)))
  specs <- data.frame(
    name = "temp", label = "Temperature", units = "degC",
    cont_binned = "Binned", stringsAsFactors = FALSE
  )
  ch <- echart_weather_density_panel(
    survey, raw, "temp", weather_specs = specs,
    scenario_weather = scenario, show_regression = TRUE,
    stored_breaks = breaks
  )

  expect_equal(unlist(ch$x$opts$series[[1L]]$data), c(1 / 3, 2 / 3, 0), tolerance = 0.001)
  expect_equal(unlist(ch$x$opts$series[[2L]]$data), c(0, 1, 0))
  expect_equal(unlist(ch$x$opts$series[[3L]]$data), c(0, 0, 1))
})

test_that("diagnostics removes rendered weather support table but keeps warning", {
  html <- as.character(htmltools::renderTags(
    mod_2_03_diagnostics_ui("diagnostics")
  )$html)
  expect_match(html, "diagnostics-weather_support_warning_ui", fixed = TRUE)
  expect_false(grepl("weather_support_table", html, fixed = TRUE))
  expect_false(grepl("simulation_weather_support_summary.csv", html, fixed = TRUE))
})
