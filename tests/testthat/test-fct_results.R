library(testthat)

# ---- helpers ----------------------------------------------------------------

make_so <- function(name = "welfare", units = "PPP", transform = "log",
                     povline = NA_real_) {
  data.frame(name = name, units = units, transform = transform,
             povline = povline, stringsAsFactors = FALSE)
}

make_df <- function(n = 50) {
  set.seed(1)
  data.frame(
    welfare  = exp(rnorm(n, log(3.5), 0.8)),
    poor     = rep(0L, n),
    ppp2021  = rep(50, n),
    stringsAsFactors = FALSE
  )
}

make_fixest <- function() {
  skip_if_not_installed("fixest")
  set.seed(1)
  d <- data.frame(y = rnorm(100), tx = rnorm(100), urban = rbinom(100, 1, 0.5))
  fixest::feols(y ~ tx + urban + tx:urban, data = d)
}

make_fixest_logistic <- function() {
  skip_if_not_installed("fixest")
  set.seed(1)
  d <- data.frame(y = rbinom(100, 1, 0.4), tx = rnorm(100), urban = rbinom(100, 1, 0.5))
  fixest::feglm(y ~ tx + urban + tx:urban, data = d, family = binomial)
}

# ============================================================================ #
# prepare_outcome_df                                                           #
# ============================================================================ #

test_that("prepare_outcome_df log-transforms outcome", {
  df  <- make_df()
  out <- prepare_outcome_df(df, make_so(transform = "log"))
  expect_equal(out$welfare, log(df$welfare))
})

test_that("prepare_outcome_df skips log when transform is not log", {
  df  <- make_df()
  out <- prepare_outcome_df(df, make_so(transform = "identity"))
  expect_equal(out$welfare, df$welfare)
})

test_that("prepare_outcome_df applies LCU back-conversion", {
  df  <- make_df()
  out <- prepare_outcome_df(df, make_so(units = "LCU", transform = "identity"))
  expect_equal(out$welfare, df$welfare * df$ppp2021)
})

test_that("prepare_outcome_df skips LCU when ppp2021 absent", {
  df  <- make_df()[, c("welfare", "poor")]
  out <- prepare_outcome_df(df, make_so(units = "LCU", transform = "identity"))
  expect_equal(out$welfare, df$welfare)
})

test_that("prepare_outcome_df creates binary poor indicator", {
  df  <- make_df()
  so  <- make_so(name = "poor", units = "PPP", transform = "identity", povline = 3.0)
  out <- prepare_outcome_df(df, so)
  expect_equal(out$poor, as.numeric(df$welfare < 3.0))
})

test_that("prepare_outcome_df skips poor indicator when povline is NA", {
  df  <- make_df()
  so  <- make_so(name = "poor", units = "PPP", transform = "identity", povline = NA_real_)
  out <- prepare_outcome_df(df, so)
  expect_equal(out$poor, df$poor)
})

# ============================================================================ #
# weather_coef_names                                                           #
# ============================================================================ #

test_that("weather_coef_names finds base term", {
  fit <- make_fixest()
  expect_true("tx" %in% weather_coef_names(fit, "tx", character(0)))
})

test_that("weather_coef_names finds interaction term", {
  fit  <- make_fixest()
  coef <- weather_coef_names(fit, "tx", "tx:urban")
  expect_true("tx:urban" %in% coef)
})

test_that("weather_coef_names returns character(0) for missing term", {
  fit <- make_fixest()
  expect_length(weather_coef_names(fit, "precip", character(0)), 0L)
})

# ============================================================================ #
# coef_label / make_coef_map                                                  #
# ============================================================================ #

test_that("coef_label returns identity when label_fun is identity", {
  expect_equal(coef_label("tx"), "tx")
})

test_that("coef_label joins interaction parts with ×", {
  expect_equal(coef_label("tx:urban"), "tx \u00d7 urban")
})

test_that("coef_label applies label_fun to each component", {
  lf <- function(x) toupper(x)
  expect_equal(coef_label("tx:urban", label_fun = lf), "TX \u00d7 URBAN")
})

test_that("make_coef_map returns named vector with correct names", {
  m <- make_coef_map(c("tx", "tx:urban"))
  expect_named(m)
  expect_equal(unname(m), c("tx", "tx:urban"))
})

# ============================================================================ #
# make_coefplot                                                                #
# ============================================================================ #

test_that("make_coefplot returns blank ggplot when no coefs match", {
  skip_if_not_installed("fixest")
  fit <- make_fixest()
  p   <- make_coefplot(fit, fit, fit,
                       weather_terms     = "precip",
                       interaction_terms = character(0),
                       engine            = "fixest")
  expect_s3_class(p, "ggplot")
})

test_that("make_coefplot returns ggplot when coefs match", {
  skip_if_not_installed("fixest")
  fit <- make_fixest()
  p   <- make_coefplot(fit, fit, fit,
                       weather_terms     = "tx",
                       interaction_terms = "tx:urban",
                       outcome_label     = "Welfare",
                       engine            = "fixest")
  expect_s3_class(p, "ggplot")
})

# ============================================================================ #
# make_weather_effect_plot                                                     #
# ============================================================================ #

test_that("make_weather_effect_plot returns blank ggplot when pred_var absent", {
  skip_if_not_installed("fixest")
  fit   <- make_fixest()
  p     <- make_weather_effect_plot(fit, pred_var = "missing_var", engine = "fixest")
  expect_s3_class(p, "ggplot")
  built  <- ggplot2::ggplot_build(p)
  labels <- unlist(lapply(built$data, function(d) d$label))
  expect_true(any(grepl("not found", labels, ignore.case = TRUE)))
})

test_that("make_weather_effect_plot returns ggplot for continuous predictor, no interaction", {
  skip_if_not_installed("fixest")
  fit <- make_fixest()
  p   <- make_weather_effect_plot(fit, pred_var = "tx", engine = "fixest")
  expect_s3_class(p, "ggplot")
})

test_that("make_weather_effect_plot returns ggplot with interaction (continuous)", {
  skip_if_not_installed("fixest")
  fit <- make_fixest()
  p   <- make_weather_effect_plot(
    fit               = fit,
    pred_var          = "tx",
    interaction_terms = "tx:urban",
    is_binned         = FALSE,
    engine            = "fixest"
  )
  expect_s3_class(p, "ggplot")
})

test_that("make_weather_effect_plot returns ggplot with interaction (binned)", {
  skip_if_not_installed("fixest")
  set.seed(1)
  d <- data.frame(
    y     = rnorm(100),
    tx    = cut(rnorm(100), breaks = c(-Inf, -1, 0, 1, Inf)),
    urban = rbinom(100, 1, 0.5)
  )
  fit <- fixest::feols(y ~ tx + urban + tx:urban, data = d)
  p   <- make_weather_effect_plot(
    fit               = fit,
    pred_var          = "tx",
    interaction_terms = "tx:urban",
    is_binned         = TRUE,
    engine            = "fixest"
  )
  expect_s3_class(p, "ggplot")
})

test_that("make_weather_effect_plot applies label_fun to axis labels", {
  skip_if_not_installed("fixest")
  fit <- make_fixest()
  p   <- make_weather_effect_plot(fit, pred_var = "tx", label_fun = toupper, engine = "fixest")
  expect_s3_class(p, "ggplot")
  expect_equal(p$labels$x, "TX")
})

test_that("make_weather_effect_plot does not error for logistic model", {
  skip_if_not_installed("fixest")
  fit <- make_fixest_logistic()
  p   <- make_weather_effect_plot(fit, pred_var = "tx", engine = "fixest")
  expect_s3_class(p, "ggplot")
})

# ============================================================================ #
# make_regtable                                                                #
# ============================================================================ #

test_that("make_regtable returns HTML", {
  skip_if_not_installed("fixest")
  skip_if_not_installed("knitr")
  fit <- make_fixest()
  out <- make_regtable(fit, fit, fit,
                       weather_terms     = "tx",
                       interaction_terms = "tx:urban",
                       engine            = "fixest")
  expect_true(inherits(out, "html") || is.character(out))
})

# ============================================================================ #
# plot_resid_weather                                                           #
# ============================================================================ #

test_that("plot_resid_weather returns NULL when haz_var absent", {
  fit <- make_fixest()
  expect_null(plot_resid_weather(fit, "missing_var"))
})

test_that("plot_resid_weather returns ggplot for valid haz_var", {
  skip_if_not_installed("ggplot2")
  fit <- make_fixest()
  p   <- plot_resid_weather(fit, "tx", "Max temperature")
  expect_s3_class(p, "ggplot")
})

# ============================================================================ #
# plot_pred_vs_actual                                                          #
# ============================================================================ #

test_that("plot_pred_vs_actual returns ggplot for linear model", {
  skip_if_not_installed("ggplot2")
  fit <- make_fixest()
  p   <- plot_pred_vs_actual(fit, is_logistic = FALSE, outcome_label = "Welfare")
  expect_s3_class(p, "ggplot")
})

test_that("plot_pred_vs_actual returns ggplot for logistic model", {
  skip_if_not_installed("ggplot2")
  fit <- make_fixest_logistic()
  p   <- plot_pred_vs_actual(fit, is_logistic = TRUE, outcome_label = "Poor")
  expect_s3_class(p, "ggplot")
})

# ============================================================================ #
# calc_fit_stats                                                               #
# ============================================================================ #

test_that("calc_fit_stats returns 4-row data frame for linear model", {
  fit <- make_fixest()
  out <- calc_fit_stats(fit, is_logistic = FALSE, engine = "fixest")
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 4L)
  expect_setequal(names(out), c("Statistic", "Value"))
})

test_that("calc_fit_stats returns 3-row data frame for logistic model", {
  fit <- make_fixest_logistic()
  out <- calc_fit_stats(fit, is_logistic = TRUE, engine = "fixest")
  expect_equal(nrow(out), 3L)
})

# ============================================================================ #
# plot_relaimpo                                                                #
# ============================================================================ #

test_that("plot_relaimpo returns ggplot", {
  skip_if_not_installed("relaimpo")
  skip_if_not_installed("ggplot2")
  # relaimpo only supports lm — use base lm for this test
  set.seed(1)
  d   <- data.frame(y = rnorm(100), tx = rnorm(100), urban = rbinom(100, 1, 0.5))
  fit <- lm(y ~ tx + urban, data = d)
  p   <- plot_relaimpo(fit)
  expect_s3_class(p, "ggplot")
})

test_that("plot_relaimpo attaches labels from var_info when provided", {
  skip_if_not_installed("relaimpo")
  skip_if_not_installed("ggplot2")
  set.seed(1)
  d        <- data.frame(y = rnorm(100), tx = rnorm(100), urban = rbinom(100, 1, 0.5))
  fit      <- lm(y ~ tx + urban, data = d)
  var_info <- data.frame(name = c("tx", "urban"), label = c("Max temp", "Urban"),
                          stringsAsFactors = FALSE)
  p        <- plot_relaimpo(fit, var_info = var_info)
  expect_s3_class(p, "ggplot")
})