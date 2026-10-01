test_that("exceedance charts keep return-period labels, titles and policy encoding", {
  curves <- expand.grid(
    rank = seq_len(50), model_id = c("m1", "m2"),
    scenario = c("Historical", "SSP3-7.0 / 2025-2035"),
    source = c("Baseline", "Policy"), stringsAsFactors = FALSE
  )
  curves$is_historical <- curves$scenario == "Historical"
  curves$exceed_prob <- (curves$rank - 0.5) / 100
  curves$welfare_val <- 4 + curves$exceed_prob +
    ifelse(curves$source == "Policy", 0.5, 0) +
    ifelse(curves$model_id == "m2", 0.1, 0)
  curves$coef_sd <- 0.02
  chart <- echart_exceedance(curves, "Welfare per day", n_sim_years = 100)
  axis <- chart$x$opts$xAxis
  expect_true(all(c(0.5, 0.2, 0.1, 0.05, 0.02) %in% unlist(axis$axisLabel$customValues)))
  expect_identical(axis$axisTick$customValues, axis$axisLabel$customValues)
  expect_true(axis$axisLabel$showMinLabel)
  expect_gte(chart$x$opts$grid$bottom, 75)
  expect_gte(chart$x$opts$grid$left, 60)
  expect_identical(chart$x$opts$yAxis$name, "Welfare per day")
  lines <- Filter(function(s) !is.null(s$endLabel), chart$x$opts$series)
  expect_length(lines, 3L)
  expect_true(all(vapply(lines, function(s) isTRUE(s$endLabel$show), logical(1))))
  expect_true(all(vapply(lines, function(s) is.null(s$data[[length(s$data)]]$label), logical(1))))
  policy <- Filter(function(s) grepl("Policy$", s$name), lines)[[1L]]
  expect_identical(policy$lineStyle$color, .wise_policy)
  expect_identical(policy$lineStyle$type, "dashed")
  expect_match(as.character(chart$x$opts$tooltip$formatter), "maximumFractionDigits: percent ? 1 : 3", fixed = TRUE)
  expect_match(as.character(chart$x$opts$tooltip$formatter), "annual probability", fixed = TRUE)

  step2_curves <- curves[curves$source == "Baseline", setdiff(names(curves), "source")]
  step2 <- echart_exceedance(step2_curves, "Welfare per day", n_sim_years = 100)
  step2_lines <- Filter(function(s) !is.null(s$endLabel), step2$x$opts$series)
  expect_length(step2_lines, 2L)
  baseline <- Filter(function(s) grepl("Baseline$", s$name) && !grepl("Historical", s$name), lines)[[1L]]
  expect_identical(step2_lines[[2L]]$lineStyle$color, baseline$lineStyle$color)
  rates <- echart_exceedance(step2_curves, metric_axis_label("headcount_ratio"), n_sim_years = 100)
  expect_match(as.character(rates$x$opts$yAxis$axisLabel$formatter), "v*100", fixed = TRUE)
  expect_match(as.character(rates$x$opts$tooltip$formatter), "percent = true", fixed = TRUE)
  expect_false(grepl("Poverty rate", as.character(rates$x$opts$tooltip$formatter), fixed = TRUE))

})

test_that("uncertainty charts reserve space for centered titles and round tooltips", {
  chart <- echart_variance_contribution(data.frame(
    scenario = c("Historical", "SSP3-7.0 / 2025-2035"),
    var_coef = c(0.01, 0.02), var_within = c(0.001, 0.003),
    var_across = c(0, 0.001)
  ))
  expect_identical(chart$x$opts$xAxis$nameLocation, "middle")
  expect_gte(chart$x$opts$grid$bottom, 85)
  expect_match(as.character(chart$x$opts$tooltip$formatter), "maximumFractionDigits: 3", fixed = TRUE)
  expect_true(chart$x$opts$tooltip$confine)
  rate_chart <- echart_variance_contribution(data.frame(scenario = "Historical",
    var_coef = .01, var_within = .02, var_across = 0), percent = TRUE)
  expect_match(rate_chart$x$opts$xAxis$name, "percentage points", fixed = TRUE)
  expect_match(as.character(rate_chart$x$opts$xAxis$axisLabel$formatter), "v*100", fixed = TRUE)
})
