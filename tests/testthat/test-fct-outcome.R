# ============================================================================ #
# tests/testthat/test-fct-outcome.R                                            #
# Fast, histogram-first outcome distribution plots.                            #
# ============================================================================ #

library(testthat)

make_outcome_plot_df <- function(n = 1000L) {
  set.seed(17)
  data.frame(
    code        = "TST",
    countryyear = sample(c("TST, 2018", "TST, 2021"), n, replace = TRUE),
    welfare     = rgamma(n, shape = 2, rate = 0.4),
    poor        = rbinom(n, 1, 0.35),
    stringsAsFactors = FALSE
  )
}

test_that("ridge distribution data is bounded by fixed grid size", {
  df <- make_outcome_plot_df(5000L)
  out <- wiseapp:::build_ridge_distribution_data(
    df, "welfare", n_bins = 128L, n_grid = 96L, log_transform = TRUE
  )

  expect_type(out, "list")
  expect_setequal(out$groups, c("TST, 2018", "TST, 2021"))
  expect_equal(nrow(out$data), 2L * 96L)
  expect_true(all(is.finite(out$data$x)))
  expect_true(all(out$data$height >= 0 & out$data$height <= 1))
})

test_that("ridge echarts use validated explicit colours", {
  rd <- data.frame(
    x = rep(seq(1, 3, length.out = 4), 2),
    y = rep(c(1, 2), each = 4),
    height = c(0, 0.7, 1, 0, 0, 0.5, 0.9, 0),
    group = rep(c("a", "b"), each = 4),
    ridge = rep(c("A", "B"), each = 4),
    stringsAsFactors = FALSE
  )
  styles <- data.frame(
    group = c("a", "b"),
    fill = c("#0072B2", NA_character_),
    line = c("#D55E00", "not-a-colour"),
    dashed = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )
  p <- wiseapp:::ridge_echart_widget(rd, c("A", "B"), c("A", "B"), styles)
  expect_s3_class(p, "echarts4r")
  expect_length(p$x$opts$series, 2L)
  expect_true(all(vapply(p$x$opts$series, function(s) {
    identical(s$type, "line") && is.null(s$renderItem) &&
      !is.null(s$lineStyle) && !is.null(s$areaStyle)
  }, logical(1))))
  expect_identical(p$x$opts$series[[1]]$areaStyle$color, "#0072B2")
  expect_identical(p$x$opts$series[[2]]$areaStyle$color, "rgba(0,0,0,0)")
  expect_identical(p$x$opts$series[[2]]$lineStyle$color, "#002244")
})

test_that("ridge tooltips retain distinct sample and historical series", {
  rd <- data.frame(
    x = rep(seq(1, 3, length.out = 4), 2),
    y = rep(c(1, 2), each = 4),
    height = c(0, 0.7, 1, 0, 0, 0.5, 0.9, 0),
    group = rep(c("A - Survey", "A - Historical"), each = 4),
    ridge = rep("A", 8),
    tooltip_value = rep(c(0.2, 0.4, 0.6, 0.8), 2),
    stringsAsFactors = FALSE
  )
  styles <- data.frame(
    group = c("A - Survey", "A - Historical"),
    fill = c("#0072B2", NA_character_),
    line = c("#D55E00", "#002244"),
    dashed = c(FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  p <- wiseapp:::ridge_echart_widget(
    rd, "A", "A", styles, hide_extreme_x = TRUE
  )

  expect_identical(
    vapply(p$x$opts$series, `[[`, character(1), "name"),
    c("A - Survey", "A - Historical")
  )
  expect_false(isTRUE(p$x$opts$xAxis$axisLabel$showMinLabel))
  expect_false(isTRUE(p$x$opts$xAxis$axisLabel$showMaxLabel))
  expect_equal(p$x$opts$xAxis$splitNumber, 6)
})

test_that("outcome distribution plots support continuous and binary outcomes", {
  skip_if_not_installed("ggplot2")

  df <- make_outcome_plot_df()
  p_cont <- plot_welfare_dist(
    df, outcome = "welfare", label = "Welfare", type = "numeric",
    poverty_lines = NULL
  )
  p_bin <- plot_welfare_dist(
    df, outcome = "poor", label = "Poor", type = "logical",
    poverty_lines = NULL
  )

  expect_s3_class(p_cont, "ggplot")
  expect_s3_class(p_bin, "ggplot")
  expect_setequal(
    unique(ggplot2::ggplot_build(p_cont)$data[[1]]$fill),
    c("#0071BC")
  )
  expect_true(any(vapply(p_bin$layers, function(x) {
    inherits(x$geom, "GeomRect")
  }, logical(1))))
  expect_true(any(vapply(p_bin$layers, function(x) {
    inherits(x$geom, "GeomText")
  }, logical(1))))
  bin_data <- ggplot2::ggplot_build(p_bin)$data[[1]]
  expect_setequal(unique(bin_data$fill), c("#D9EFF8", "#0071BC"))
  expect_equal(
    as.numeric(tapply(bin_data$ymax - bin_data$ymin, bin_data$x, sum)),
    c(1, 1)
  )
})

test_that("interactive outcome distributions honor currency and concise tooltips", {
  df <- make_outcome_plot_df(200L)
  df$ppp2021 <- rep(c(2, 4), each = 100)

  ppp <- echart_welfare_dist(
    df, outcome = "welfare", type = "numeric", currency = "PPP",
    poverty_lines = NULL
  )
  lcu <- echart_welfare_dist(
    df, outcome = "welfare", type = "numeric", currency = "LCU",
    poverty_lines = NULL
  )

  expect_equal(ppp$x$opts$xAxis$axisLabel$rotate, 0)
  expect_equal(lcu$x$opts$xAxis$name, "LCU per day (2021)")
  expect_equal(ppp$x$opts$xAxis$name, "$ per day (2021 PPP)")
  expect_equal(lcu$x$opts$grid$left, 8)
  expect_equal(lcu$x$opts$yAxis$nameLocation, "end")
  expect_equal(lcu$x$opts$yAxis$nameRotate, 0)
  expect_equal(lcu$x$opts$yAxis$nameTextStyle$align, "left")
  expect_equal(lcu$x$opts$yAxis$nameTextStyle$verticalAlign, "top")
  expect_true(inherits(ppp$x$opts$tooltip$formatter, "JS_EVAL"))
  expect_false(grepl("__lower|__upper", ppp$x$opts$tooltip$formatter))
  expect_match(ppp$x$opts$tooltip$formatter, "% <")
  expect_setequal(
    vapply(ppp$x$opts$series, function(s) s$name, character(1)),
    c("TST, 2018", "TST, 2021")
  )

  ppp_max <- max(vapply(ppp$x$opts$series, function(s) {
    max(vapply(s$data, function(row) as.numeric(row[[1L]]), numeric(1)), na.rm = TRUE)
  }, numeric(1)))
  lcu_max <- max(vapply(lcu$x$opts$series, function(s) {
    max(vapply(s$data, function(row) as.numeric(row[[1L]]), numeric(1)), na.rm = TRUE)
  }, numeric(1)))
  expect_gt(lcu_max, ppp_max)
})

test_that("binary outcome y-axis title sits above the compact plot", {
  df <- data.frame(
    countryyear = rep(c("TST, 2018", "TST, 2021"), each = 20),
    poor = rep(c(0, 1), 20)
  )
  p <- echart_welfare_dist(
    df, outcome = "poor", label = "Poor households", type = "logical",
    poverty_lines = NULL
  )
  expect_equal(p$x$opts$grid$left, 8)
  expect_null(p$x$opts$yAxis$name)
  expect_equal(p$x$opts$title$left, 8)
  expect_equal(p$x$opts$title$top, 0)
})

test_that("binary poor outcome title uses the configured poverty line", {
  df <- data.frame(
    countryyear = rep(c("TST, 2018", "TST, 2021"), each = 20),
    poor = rep(c(0, 1), 20)
  )
  p <- echart_welfare_dist(
    df, outcome = "poor", label = "Poor (welfare < poverty line)",
    type = "logical", poverty_lines = data.frame(value = 4.2)
  )
  expect_true(grepl("Poor ($4.20/day)", p$x$opts$title$text, fixed = TRUE))
})

test_that("binary outcome titles omit the share prefix", {
  df <- data.frame(
    countryyear = rep(c("TST, 2018", "TST, 2021"), each = 20),
    employed = rep(c(0, 1), 20)
  )
  p <- echart_welfare_dist(
    df, outcome = "employed", label = "Employed", type = "logical",
    poverty_lines = NULL
  )
  expect_identical(p$x$opts$title$text, "Employed")
})

test_that("ridge distribution data rejects unusable inputs", {
  expect_null(wiseapp:::build_ridge_distribution_data(NULL, "welfare"))
  expect_null(wiseapp:::build_ridge_distribution_data(
    data.frame(welfare = 1), "welfare"
  ))
  expect_null(wiseapp:::build_ridge_distribution_data(
    data.frame(
      code = "TST", countryyear = "TST, 2021", welfare = NA_real_
    ),
    "welfare"
  ))
})
