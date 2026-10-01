test_that("policy diagnostic binary distributions show named percent shares", {
  w <- echart_before_after_hist(
    c(`row-1` = 0, `row-2` = 1, `row-3` = Inf, `row-4` = NA_real_),
    c(`row-1` = 1, `row-2` = 1, `row-3` = 0),
    "Access to electricity"
  )

  expect_s3_class(w, "echarts4r")
  expect_length(w$x$opts$series, 2L)
  expect_identical(
    vapply(w$x$opts$series, `[[`, character(1L), "name"),
    c("Baseline", "Policy")
  )
  expect_identical(levels(w$x$opts$xAxis[[1L]]$data), c("No", "Yes"))
  expect_match(w$x$opts$xAxis[[1L]]$name, "Access to electricity", fixed = TRUE)
  expect_identical(w$x$opts$yAxis[[1L]]$max, 1)
  expect_match(w$x$opts$yAxis[[1L]]$axisLabel$formatter, "%")
  expect_null(w$x$opts$yAxis[[1L]]$name)
  expect_match(w$x$opts$tooltip$valueFormatter, "minimumFractionDigits:2")
  expect_identical(w$x$opts$legend$left, "center")
  expect_identical(unlist(w$x$opts$color), c(.wise_baseline, .wise_policy))
})

test_that("policy diagnostic continuous distributions use shared finite ridges", {
  baseline <- stats::setNames(c(0, 0.5, 1, 2, 3, NA_real_, Inf), paste0("b", 1:7))
  policy <- stats::setNames(c(0.2, 0.8, 1.2, 2.2, 4), paste0("p", 1:5))
  w <- echart_before_after_hist(baseline, policy, "Transfer amount")

  expect_s3_class(w, "echarts4r")
  expect_length(w$x$opts$series, 2L)
  expect_identical(
    vapply(w$x$opts$series, `[[`, character(1L), "name"),
    c("Baseline", "Policy")
  )
  expect_true(all(vapply(w$x$opts$series, function(s) {
    identical(s$type, "line") && is.null(s$renderItem) &&
      !is.null(s$areaStyle) && is.matrix(s$data) && ncol(s$data) == 3L &&
      all(is.finite(s$data))
  }, logical(1L))))
  expect_true(all(vapply(w$x$opts$series, function(s) {
    nrow(s$data) %% 2L == 0L && all(s$data[, 3L] >= 0 & s$data[, 3L] <= 1)
  }, logical(1L))))
  expect_identical(w$x$opts$series[[1L]]$lineStyle$color, .wise_slate)
  expect_identical(w$x$opts$series[[2L]]$lineStyle$color, .wise_policy_dark)
  expect_match(w$x$opts$tooltip$formatter, "CDF share")
  expect_match(w$x$opts$tooltip$formatter, "minimumFractionDigits:2")
  expect_match(w$x$opts$xAxis$axisLabel$formatter, "minimumFractionDigits:2")
  expect_identical(w$x$opts$xAxis$name, "Transfer amount")
  expect_true(length(w$jsHooks$render) > 0L)
})

test_that("policy diagnostic distributions preserve the empty state", {
  w <- echart_before_after_hist(c(NA_real_, Inf), c(-Inf), "Outcome")

  expect_s3_class(w, "echarts4r")
  expect_match(w$x$opts$title[[1L]]$text, "No data available", fixed = TRUE)
})
