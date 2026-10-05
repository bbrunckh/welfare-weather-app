test_that("annual charts use closed violins, readable numeric axes, and honest tooltips", {
  tbl <- data.frame(
    scenario = rep(c("Historical", "SSP2-4.5 / 2030-2040"), each = 8),
    value = c(seq(1, 8), seq(2, 9)),
    source = rep(rep(c("Baseline", "Policy"), each = 4), 2),
    stringsAsFactors = FALSE
  )
  set.seed(88)
  rng_before <- .Random.seed
  step2 <- echart_annual_distribution(tbl, "Annual outcome", "violin")
  expect_identical(.Random.seed, rng_before)
  step3 <- echart_step3_annual_distribution(tbl, "Annual outcome", "violin")

  for (chart in list(step2, step3)) {
    expect_s3_class(chart, "echarts4r")
    dots <- Filter(function(s) identical(s$name, "Draws"), chart$x$opts$series)[[1L]]
    expect_true(all(vapply(dots$data, function(p) identical(p$itemStyle$opacity, .2), logical(1))))
    violins <- Filter(function(s) identical(s$type, "custom"), chart$x$opts$series)
    expect_length(violins, 4L)
    expect_true(all(vapply(violins, function(s) {
      !is.null(s$renderItem) && length(s$data) == 1L &&
        length(s$data[[1L]]) == 4L
    }, logical(1L))))
    for (violin in violins) {
      points_json <- sub(".*var pts = (.*); return.*", "\\1", as.character(violin$renderItem))
      points <- jsonlite::fromJSON(points_json)
      n <- nrow(points) / 2L
      upper <- points[seq_len(n), , drop = FALSE]
      lower <- points[nrow(points) + 1L - seq_len(n), , drop = FALSE]
      expect_equal(upper[, 1L], lower[, 1L])
      center <- mean(c(violin$data[[1L]][[2L]], violin$data[[1L]][[4L]]))
      expect_equal(upper[, 2L] + lower[, 2L], rep(2 * center, n))
    }
    expect_true(all(vapply(Filter(function(s) !is.null(s$areaStyle), chart$x$opts$series),
      function(s) !identical(s$type, "line"), logical(1L))))
    expect_identical(chart$x$opts$xAxis$scale, TRUE)
    expect_identical(chart$x$opts$yAxis$min, 0)
    expect_identical(chart$x$opts$yAxis$max, 3L)
    expect_null(chart$x$opts$yAxis$axisLabel$customValues)
    expect_gte(chart$x$opts$grid$bottom, 55)
    expect_lte(chart$x$opts$grid$left, 12)
    expect_match(as.character(chart$x$opts$tooltip$formatter), "outcome", ignore.case = TRUE)
    expect_match(as.character(chart$x$opts$tooltip$formatter), "scenario", ignore.case = TRUE)
    expect_match(as.character(chart$x$opts$tooltip$formatter), "mean", ignore.case = TRUE)
  }

  policy_means <- Filter(function(s) identical(s$name, "Mean Policy"), step3$x$opts$series)[[1L]]
  expect_identical(policy_means$itemStyle$color, .wise_policy)
  baseline_means <- Filter(function(s) identical(s$name, "Mean Baseline"), step3$x$opts$series)[[1L]]
  expect_identical(baseline_means$itemStyle$color, "white")

  box <- echart_step3_annual_distribution(tbl, plot_type = "boxplot")
  boxes <- Filter(function(s) identical(s$type, "custom"), box$x$opts$series)
  expect_length(boxes, 4L)
  expect_true(all(vapply(boxes, function(s) length(s$data[[1L]]) == 4L, logical(1L))))
  expect_false(step3$x$opts$xAxis$axisLabel$showMaxLabel)
  expect_true(step3$x$opts$tooltip$confine)
  expect_match(as.character(step3$x$opts$tooltip$formatter), "maximumFractionDigits: percent ? 1 : 3", fixed = TRUE)
  expect_match(as.character(step3$x$opts$tooltip$formatter), "esc(d.scenario", fixed = TRUE)

  single <- echart_annual_distribution(tbl[tbl$source == "Baseline", c("scenario", "value")])
  means <- Filter(function(s) identical(s$name, "Mean All"), single$x$opts$series)[[1L]]
  expect_identical(means$data[[1L]]$scenario, "Historical")
  expect_true(means$data[[1L]]$is_mean)
  expect_equal(means$data[[1L]]$value[[2L]], 2)
  expect_identical(means$data[[2L]]$scenario, "SSP2-4.5 / 2030-2040")
  expect_equal(means$data[[2L]]$value[[2L]], 1)
  invalid <- echart_annual_distribution(transform(tbl, value = NA_real_))
  expect_match(invalid$x$opts$title[[1L]]$text, "No finite annual", fixed = TRUE)
  dots <- Filter(function(s) identical(s$name, "Draws"), step3$x$opts$series)[[1L]]
  expect_equal(dots$symbolSize, 5)
  expect_lt(dots$z, min(vapply(boxes, function(s) s$z, numeric(1))))
  expect_gt(policy_means$z, dots$z)
  expect_identical(dots$symbol, "circle")
  expect_true(all(vapply(dots$data, function(p) identical(p$itemStyle$opacity, .2), logical(1))))
  expect_true(all(vapply(dots$data, function(p) is.null(p$mean), logical(1))))
  expect_true(all(vapply(violins, function(s) s$areaStyle$opacity <= .35, logical(1))))
  policy_dots <- Filter(function(p) identical(p$source, "Policy"), dots$data)
  baseline_dots <- Filter(function(p) identical(p$source, "Baseline"), dots$data)
  expect_identical(policy_dots[[1L]]$itemStyle$color, baseline_dots[[1L]]$itemStyle$color)
  centers <- vapply(dots$data, function(p) {
    row <- if (identical(p$scenario, "Historical")) 2 else 1
    row + if (identical(p$source, "Policy")) -.19 else .19
  }, numeric(1))
  jitter <- abs(vapply(dots$data, function(p) p$value[[2L]], numeric(1)) - centers)
  expect_gt(max(jitter), .17)
  expect_lte(max(jitter), .20)
  expect_gt(length(unique(vapply(dots$data, function(p) p$value[[2L]], numeric(1)))), 4)
  expect_match(as.character(step3$x$opts$tooltip$formatter), "d.is_mean ? 'Scenario mean'", fixed = TRUE)
  expect_match(step3$jsHooks$render[[1L]]$code, "ResizeObserver", fixed = TRUE)
})

test_that("rate and binary mean charts format fractional outcomes as percentages", {
  expect_identical(metric_metadata("mean", list(type = "logical"))$format, "percent")
  expect_identical(metric_metadata("mean", list(type = "continuous"))$format, "number")
  expect_identical(metric_axis_label("headcount_ratio"), "Poverty rate")
  expect_identical(metric_axis_label("gap"), "Poverty gap")
  expect_identical(metric_axis_label("fgt2"), "Poverty severity")
  expect_identical(metric_axis_label("gini"), "Gini coefficient")
  for (label in c(metric_axis_label("headcount_ratio"), metric_axis_label("mean", list(type = "binary")))) {
    chart <- echart_annual_distribution(data.frame(scenario = "Historical", value = c(.4, .42, .43)), label)
    expect_match(as.character(chart$x$opts$xAxis$axisLabel$formatter), "v*100", fixed = TRUE)
    expect_match(as.character(chart$x$opts$tooltip$formatter), "percent = true", fixed = TRUE)
    draws <- Filter(function(s) identical(s$name, "Draws"), chart$x$opts$series)[[1L]]
    expect_equal(draws$data[[1L]]$outcome, .4)
  }
})

test_that("rate deviations and policy changes use percentage points", {
  for (method in c("headcount_ratio", "gap", "fgt2")) {
    metadata <- metric_metadata(method)
    expect_identical(metadata$native_unit, "fraction")
    expect_identical(metadata$level_unit, "percent")
    expect_identical(metadata$change_unit, "pp")
    label <- metric_axis_label(method, deviation = "mean")
    chart <- echart_annual_distribution(
      data.frame(scenario = "Historical", value = c(-.04, .02, .03)), label)
    expect_match(as.character(chart$x$opts$xAxis$axisLabel$formatter), "v*100", fixed = TRUE)
    expect_match(as.character(chart$x$opts$xAxis$axisLabel$formatter), '" pp"', fixed = TRUE)
    expect_match(as.character(chart$x$opts$tooltip$formatter), "percent = true", fixed = TRUE)
    expect_match(as.character(chart$x$opts$tooltip$formatter), '" pp"', fixed = TRUE)
  }
  tooltip <- as.character(.wise_result_tooltip(metric_axis_label("headcount_ratio")))
  expect_match(tooltip, "change ? ' pp'", fixed = TRUE)
  expect_match(tooltip, "num(d.policy - d.baseline, true)", fixed = TRUE)
})

test_that("annual scatter retains finite jittered coordinates at production draw counts", {
  tbl <- data.frame(
    scenario = rep(c("Historical", "SSP3-7.0 / 2025-2035"), each = 6000),
    value = 4.5 + sin(seq_len(12000)) * .15
  )
  for (type in c("violin", "boxplot")) {
    chart <- echart_annual_distribution(tbl, plot_type = type)
    draws <- Filter(function(s) identical(s$name, "Draws"), chart$x$opts$series)[[1L]]
    expect_length(draws$data, 10000L)
    expect_lt(draws$z, min(vapply(Filter(function(s) identical(s$type, "custom"),
      chart$x$opts$series), function(s) s$z, numeric(1))))
    expect_true(all(vapply(draws$data, function(p) all(is.finite(p$value)), logical(1))))
    expect_true(all(c("Historical", "SSP3-7.0 / 2025-2035") %in%
      vapply(draws$data, `[[`, character(1), "scenario")))
  }
})

test_that("matrix-derived named years serialize annual draws as a JSON array", {
  values <- matrix(4.5 + sin(seq_len(72)) * .15, nrow = 2,
    dimnames = list(c("historical", "m1"), as.character(2025:2060)))
  tbl <- dplyr::bind_rows(lapply(seq_len(nrow(values)), function(i) {
    tibble::tibble(
      scenario = if (i == 1) "Historical" else "SSP3-7.0 / 2025-2035",
      sim_year = as.integer(colnames(values)), value = values[i, ]
    )
  }))
  expect_false(is.null(names(tbl$value)))
  for (type in c("violin", "boxplot")) {
    chart <- echart_annual_distribution(tbl, plot_type = type)
    draws <- Filter(function(s) identical(s$name, "Draws"), chart$x$opts$series)[[1L]]
    expect_null(names(draws$data))
    expect_length(draws$data, 72L)
    expect_match(htmlwidgets:::toJSON(draws$data), "^\\[")
    expect_equal(draws$data[[1L]]$outcome, unname(values[1L, 1L]))
  }
  policy_tbl <- dplyr::bind_rows(
    dplyr::mutate(tbl, source = "Baseline"),
    dplyr::mutate(tbl, source = "Policy", value = value + .1)
  )
  policy_chart <- echart_step3_annual_distribution(policy_tbl)
  policy_draws <- Filter(function(s) identical(s$name, "Draws"), policy_chart$x$opts$series)[[1L]]
  expect_null(names(policy_draws$data))
  expect_length(policy_draws$data, 144L)
  expect_match(htmlwidgets:::toJSON(policy_draws$data), "^\\[")
})

test_that("adverse chart intervals share their scenario marker dodge", {
  labels <- factor(
    c("Expected", "Adverse 1-in-10", "Expected", "Adverse 1-in-10"),
    levels = rev(c("Expected", "Adverse 1-in-5", "Adverse 1-in-10",
      "Adverse 1-in-20", "Adverse 1-in-50"))
  )
  tbl <- data.frame(
    scenario = rep(c("SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040"), each = 2),
    rp_label = labels,
    value = c(10, 11, 12, 13),
    is_historical = FALSE,
    intermod_lo = c(9, 10, 11, 12),
    intermod_hi = c(11, 12, 13, 14)
  )
  chart <- echart_step2_adverse_dot(tbl, "Outcome")
  spreads <- Filter(function(s) grepl("__spread$", s$name), chart$x$opts$series)
  markers <- Filter(function(s) identical(s$type, "scatter"), chart$x$opts$series)
  expect_length(spreads, 4L)
  expect_length(markers, 2L)
  marker_y <- unlist(lapply(markers, function(s) vapply(s$data, function(p) p$value[[2L]], numeric(1))))
  spread_y <- vapply(spreads, function(s) s$data[1L, 2L], numeric(1))
  expect_setequal(spread_y, marker_y)
  expect_identical(chart$x$opts$yAxis$type, "value")
  expect_identical(chart$x$opts$yAxis$min, 0)
  expect_identical(chart$x$opts$yAxis$interval, 1)
  expect_null(chart$x$opts$yAxis$axisLabel$customValues)
  expect_gte(chart$x$opts$grid$bottom, 55)
  expect_lte(chart$x$opts$grid$left, 12)

  equal_chart <- echart_step2_adverse_dot(transform(
    tbl[1:2, ], scenario = "SSP2 / 2030", rp_label = factor(c("Expected", "Adverse 1-in-10"),
      levels = rev(c("Expected", "Adverse 1-in-5", "Adverse 1-in-10", "Adverse 1-in-20", "Adverse 1-in-50"))),
    value = 5, is_historical = FALSE, intermod_lo = 5, intermod_hi = 5
  ))
  expect_true(is.finite(equal_chart$x$opts$xAxis$min))
  expect_true(equal_chart$x$opts$xAxis$max > equal_chart$x$opts$xAxis$min)

  step3_tbl <- data.frame(
    scenario = "SSP2-4.5 / 2030-2040", is_historical = FALSE,
    rp_label = factor("Expected", levels = rev(c("Expected", "Adverse 1-in-5",
      "Adverse 1-in-10", "Adverse 1-in-20", "Adverse 1-in-50"))),
    baseline_val = 10, policy_val = 11, policy_lo = 10.5, policy_hi = 11.5,
    base_lo = 9.5, base_hi = 10.5, effect = 1, ssp_key = "SSP2-4.5",
    yr_lbl = "2030-2040"
  )
  step3_adverse <- echart_step3_adverse_dot(step3_tbl)
  expect_identical(step3_adverse$x$opts$yAxis$min, 0)
  expect_identical(step3_adverse$x$opts$yAxis$interval, 1)
  expect_identical(step3_adverse$x$opts$yAxis$max, 6L)
  expect_null(step3_adverse$x$opts$yAxis$axisLabel$customValues)
  expect_false(step3_adverse$x$opts$yAxis$axisLabel$showMaxLabel)
  expect_false(step3_adverse$x$opts$xAxis$axisLabel$showMaxLabel)
  expect_identical(vapply(step3_adverse$x$opts$legend$data, `[[`, character(1), "name"),
    c("Baseline", "Policy"))
  expect_false(step3_adverse$x$opts$legend$selectedMode)
  expect_match(as.character(step3_adverse$x$opts$tooltip$formatter), "Policy change", fixed = TRUE)
  policy <- Filter(function(s) identical(s$name, "Policy"), step3_adverse$x$opts$series)[[1L]]
  expect_equal(policy$data[[1L]]$outcome, 11)
  expect_equal(policy$data[[1L]]$baseline, 10)
  expect_identical(policy$data[[1L]]$rp_label, "Expected")
  expect_match(as.character(chart$x$opts$tooltip$formatter), "Climate-model spread", fixed = TRUE)
  expect_identical(markers[[1L]]$data[[1L]]$rp_label, "Expected")
  invalid <- echart_step2_adverse_dot(transform(tbl,
    value = NA_real_, intermod_lo = NA_real_, intermod_hi = NA_real_))
  expect_match(invalid$x$opts$title[[1L]]$text, "No finite adverse", fixed = TRUE)
  invalid_policy <- echart_step3_adverse_dot(transform(step3_tbl,
    baseline_val = Inf, policy_val = Inf, policy_lo = NA_real_, policy_hi = NA_real_,
    base_lo = NA_real_, base_hi = NA_real_))
  expect_match(invalid_policy$x$opts$title[[1L]]$text, "No finite adverse", fixed = TRUE)
})
