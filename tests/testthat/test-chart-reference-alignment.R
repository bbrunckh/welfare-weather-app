test_that("annual historical reference follows plotted baseline aggregation", {
  tbl <- data.frame(
    scenario = rep(c("Historical", "SSP2-4.5 / 2030-2040"), each = 4),
    source = rep(rep(c("Baseline", "Policy"), each = 2), 2),
    value = c(2, 6, 20, 22, 3, 5, 21, 23),
    stringsAsFactors = FALSE
  )

  charts <- list(
    echart_annual_distribution(tbl),
    echart_step3_annual_distribution(tbl)
  )
  for (chart in charts) {
    reference <- Filter(function(s) !is.null(s$markLine), chart$x$opts$series)[[1L]]$markLine
    expect_equal(reference$data[[1L]]$xAxis, mean(c(2, 6)))
    expect_identical(reference$label$position, "insideEndTop")
    expect_identical(reference$label$rotate, 0)
    expect_identical(reference$label$align, "left")
    expect_equal(reference$precision, 12)
  }

  centered <- tbl
  centered$value[centered$scenario == "Historical" & centered$source == "Baseline"] <-
    centered$value[centered$scenario == "Historical" & centered$source == "Baseline"] - mean(c(2, 6))
  centered$value[centered$scenario == "Historical" & centered$source == "Policy"] <-
    centered$value[centered$scenario == "Historical" & centered$source == "Policy"] - mean(c(2, 6))
  for (centered_chart in list(
    echart_annual_distribution(centered),
    echart_step3_annual_distribution(centered)
  )) {
    centered_reference <- Filter(
      function(s) !is.null(s$markLine), centered_chart$x$opts$series
    )[[1L]]$markLine
    expect_equal(centered_reference$data[[1L]]$xAxis, 0)
  }
})

test_that("Step 3 adverse markers match Step 2 dot size", {
  tbl <- data.frame(
    scenario = "SSP2-4.5 / 2030-2040",
    is_historical = FALSE,
    rp_label = factor("Expected", levels = rev(c(
      "Expected", "Adverse 1-in-5", "Adverse 1-in-10",
      "Adverse 1-in-20", "Adverse 1-in-50"
    ))),
    baseline_val = 10,
    policy_val = 11,
    policy_lo = 10.5,
    policy_hi = 11.5,
    base_lo = 9.5,
    base_hi = 10.5,
    effect = 1,
    ssp_key = "SSP2-4.5",
    yr_lbl = "2030-2040"
  )

  chart <- echart_step3_adverse_dot(tbl)
  baseline <- Filter(function(s) identical(s$name, "Baseline"), chart$x$opts$series)[[1L]]
  policy <- Filter(function(s) identical(s$name, "Policy"), chart$x$opts$series)[[1L]]
  connectors <- Filter(function(s) identical(s$type, "lines"), chart$x$opts$series)[[1L]]
  expect_identical(baseline$symbolSize, 8)
  expect_identical(policy$symbolSize, 8)
  expect_identical(connectors$lineStyle$width, 1.6)
})
