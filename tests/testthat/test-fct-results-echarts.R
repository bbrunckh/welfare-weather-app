# ============================================================================ #
# tests/testthat/test-fct-results-echarts.R                                    #
# Echarts counterparts of the Step 1 results / model-fit figures (guidelines   #
# §7): widget class, series structure and axis names for each builder branch.  #
# ============================================================================ #

library(testthat)

set.seed(7)
n_ec <- 300
ec_dat <- data.frame(
  welfare = rnorm(n_ec),
  tx = runif(n_ec, 20, 40),
  pr = runif(n_ec, 0, 100),
  urban = rbinom(n_ec, 1, 0.5)
)
ec_dat$welfare <- 5 + 0.3 * ec_dat$tx + 0.02 * ec_dat$tx^2 -
  0.01 * ec_dat$urban * ec_dat$tx + rnorm(n_ec)
ec_dat$tx_bin <- cut(ec_dat$tx, 4)

ec_lf <- function(x) switch(x,
  tx = "Max temp (deg C)", tx_bin = "Max temp (deg C)",
  pr = "Precip (mm)", urban = "Urban", welfare = "Welfare", x
)

ec_fit_cont <- fixest::feols(welfare ~ tx + I(tx^2) | urban, data = ec_dat)
# Moderation via an explicit interaction column (the moderator must be part
# of the design matrix, as it is for user-specified interactions).
ec_fit_mod <- fixest::feols(
  welfare ~ tx + I(tx^2) + tx:urban + urban, data = ec_dat
)
ec_fit_bin <- fixest::feols(welfare ~ tx_bin + urban, data = ec_dat)

ec_taus <- seq(0.1, 0.9, by = 0.1)
ec_mk_grid <- function(terms, taus, model = 3L) {
  do.call(rbind, lapply(terms, function(t) data.frame(
    term = t, tau = taus, model = model,
    estimate = rnorm(length(taus)), std.error = runif(length(taus), .01, .05),
    conf.low = -0.2, conf.high = 0.2, statistic = 0, p.value = 0.1,
    stringsAsFactors = FALSE
  )))
}
ec_terms_bin <- paste0("tx_bin", levels(ec_dat$tx_bin)[2:4])

# ---- echart_weather_effect_plot ---------------------------------------------

test_that("continuous branch draws ribbon + line + rug as one widget", {
  skip_if_not_installed("echarts4r")
  ch <- echart_weather_effect_plot(
    ec_fit_cont, "tx", character(0), FALSE, ec_lf, "fixest",
    x_label = "Max temp (deg C)", y_label = "Change in welfare per +1 unit"
  )
  expect_s3_class(ch, "echarts4r")
  # One ribbon trio (lo, band, est line) + the observed-value rug scatter.
  expect_equal(length(ch$x$opts$series), 4L)
  expect_equal(ch$x$opts$series[[1]]$type, "line")
  expect_true(!is.null(ch$x$opts$series[[2]]$areaStyle))
  expect_equal(ch$x$opts$series[[4]]$name, "Observed")
  expect_match(ch$x$opts$xAxis[[1]]$name, "Max temp")
  expect_match(ch$x$opts$yAxis[[1]]$name, "Change in welfare")
  # Mean reference and the dashed zero line ride on the estimate series.
  expect_equal(length(ch$x$opts$series[[3]]$markLine$data), 2L)
})

test_that("moderated continuous branch draws one curve per moderator level", {
  skip_if_not_installed("echarts4r")
  ch <- echart_weather_effect_plot(
    ec_fit_mod, "tx", "tx:urban", FALSE, ec_lf, "fixest"
  )
  expect_s3_class(ch, "echarts4r")
  # 2 moderator levels x ribbon trio + rug.
  expect_equal(length(ch$x$opts$series), 7L)
  expect_false(is.null(ch$x$opts$legend))
})

test_that("binned branch draws a ribbon and estimate line over bins", {
  skip_if_not_installed("echarts4r")
  ch <- echart_weather_effect_plot(
    ec_fit_bin, "tx_bin", character(0), TRUE, ec_lf, "fixest",
    weather_df = data.frame(tx_bin = ec_dat$tx_bin),
    x_label = "Max temp bins", y_label = "Effect vs reference bin"
  )
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$series), 3L)
  expect_equal(ch$x$opts$series[[2]]$type, "custom")
  expect_equal(ch$x$opts$series[[3]]$type, "line")
  expect_equal(ch$x$opts$series[[3]]$symbol, "circle")
  expect_equal(unlist(ch$x$opts$series[[3]]$markLine$symbol), c("none", "none"))
  expect_match(ch$x$opts$tooltip$formatter, "95% CI", fixed = TRUE)
  expect_match(ch$x$opts$xAxis[[1]]$name, "Max temp bins")
})

test_that("RIF multi-bin branch facets one grid per bin with tau marks", {
  skip_if_not_installed("echarts4r")
  grid <- ec_mk_grid(ec_terms_bin, ec_taus)
  ch <- echart_weather_effect_plot(
    ec_fit_bin, "tx_bin", character(0), TRUE, ec_lf, "rif",
    rif_grid = grid, mark_taus = c(0.1, 0.5, 0.9)
  )
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$grid), 3L)
  # One ribbon trio per bin; the estimate series carries zero + 3 tau marks.
  expect_equal(length(ch$x$opts$series), 9L)
  expect_equal(length(ch$x$opts$series[[3]]$markLine$data), 4L)
  # x labels are percent-formatted tau.
  expect_s3_class(ch$x$opts$xAxis[[1]]$axisLabel$formatter, "JS_EVAL")
})

test_that("RIF single-term branch draws one beta(tau) panel", {
  skip_if_not_installed("echarts4r")
  grid <- ec_mk_grid("tx", ec_taus)
  ch <- echart_weather_effect_plot(
    ec_fit_cont, "tx", character(0), FALSE, ec_lf, "rif", rif_grid = grid
  )
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$series), 3L)
  expect_match(ch$x$opts$xAxis[[1]]$name, "Welfare quantile")
})

test_that("RIF moderated branch draws one curve per moderator level", {
  skip_if_not_installed("echarts4r")
  grid <- rbind(
    ec_mk_grid("tx", ec_taus),
    ec_mk_grid("tx:urban", ec_taus)
  )
  ch <- echart_weather_effect_plot(
    ec_fit_cont, "tx", "tx:urban", FALSE, ec_lf, "rif",
    rif_grid = grid, weather_df = ec_dat
  )
  expect_s3_class(ch, "echarts4r")
  # 2 moderator levels x ribbon trio.
  expect_equal(length(ch$x$opts$series), 6L)
  expect_false(is.null(ch$x$opts$legend))

  # mode = "main" collapses to a single averaged curve.
  ch_main <- echart_weather_effect_plot(
    ec_fit_cont, "tx", "tx:urban", FALSE, ec_lf, "rif",
    rif_grid = grid, weather_df = ec_dat, mode = "main"
  )
  expect_s3_class(ch_main, "echarts4r")
  expect_equal(length(ch_main$x$opts$series), 3L)
})

test_that("unusable inputs return an informative blank widget", {
  skip_if_not_installed("echarts4r")
  ch <- echart_weather_effect_plot(
    ec_fit_cont, "zz", character(0), FALSE, ec_lf, "fixest"
  )
  expect_s3_class(ch, "echarts4r")
  expect_match(ch$x$opts$title[[1]]$text, "not found in model frame")

  grid <- ec_mk_grid("tx", ec_taus)
  ch2 <- echart_weather_effect_plot(
    ec_fit_cont, "tx", character(0), FALSE, ec_lf, "rif", rif_grid = grid
  )
  # rif_grid holds tx but pred_var is requested for a term absent from it
  ch3 <- echart_weather_effect_plot(
    ec_fit_bin, "zz", character(0), TRUE, ec_lf, "rif", rif_grid = grid
  )
  expect_match(ch3$x$opts$title[[1]]$text, "No RIF terms found")
})

# ---- echart_make_coefplot ----------------------------------------------------

test_that("echart coefplot draws dodged specifications with CI whiskers", {
  skip_if_not_installed("echarts4r")
  ch <- echart_make_coefplot(
    fit1 = fixest::feols(welfare ~ tx, data = ec_dat),
    fit2 = fixest::feols(welfare ~ tx | urban, data = ec_dat),
    fit3 = ec_fit_cont,
    weather_terms = "tx", interaction_terms = character(0),
    outcome_label = "Welfare", label_fun = ec_lf, engine = "fixest",
    pred_var = "tx", x_label = "Coefficient (log points)",
    has_controls = TRUE
  )
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$series), 3L)
  expect_equal(ch$x$opts$yAxis$type, "category")
  expect_match(ch$x$opts$xAxis$name, "Coefficient")
  # CI whiskers + dashed zero reference on the first series' markLine.
  expect_true(length(ch$x$opts$series[[1]]$markLine$data) > 1L)

  # The RIF branch is suppressed (the quantile curve lives in "Who is most
  # affected?").
  expect_null(echart_make_coefplot(
    fit1 = NULL, fit2 = NULL, fit3 = NULL,
    weather_terms = "tx", interaction_terms = character(0),
    engine = "rif", rif_grid = ec_mk_grid("tx", ec_taus)
  ))
})

# ---- echart_importance -------------------------------------------------------

test_that("echart importance draws horizontal share bars, largest on top", {
  skip_if_not_installed("echarts4r")
  ch <- echart_importance(ec_fit_cont, label_fun = ec_lf)
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$series), 1L)
  expect_equal(ch$x$opts$series[[1]]$type, "bar")
  shares <- vapply(ch$x$opts$series[[1]]$data, function(d) d[[1]], numeric(1))
  expect_true(all(diff(shares) >= 0)) # ascending = largest lands on top
  expect_match(ch$x$opts$xAxis[[1]]$name, "Share of explained variation")
  # Unresolvable model matrix -> informative blank widget.
  expect_s3_class(echart_importance(structure(list(), class = "nope")),
                  "echarts4r")
})

# ---- echart_residual_panels --------------------------------------------------

test_that("echart residual panels draw a two-grid widget for linear models", {
  skip_if_not_installed("echarts4r")
  ch <- echart_residual_panels(ec_fit_cont, is_logistic = FALSE)
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$grid), 2L)
  # Panel 1 scatter + loess trend, panel 2 QQ points.
  expect_equal(length(ch$x$opts$series), 3L)
  expect_match(ch$x$opts$xAxis[[1]]$name, "Fitted values")
  expect_match(ch$x$opts$xAxis[[2]]$name, "Theoretical quantiles")
  # QQ reference line through the quartiles.
  expect_equal(length(ch$x$opts$series[[3]]$markLine$data[[1]]), 2L)
})

test_that("echart residual panels fall back to binned residuals for logit", {
  skip_if_not_installed("echarts4r")
  m_glm <- glm(urban ~ tx + pr, data = ec_dat, family = binomial)
  ch <- echart_residual_panels(m_glm, is_logistic = TRUE)
  expect_s3_class(ch, "echarts4r")
  expect_equal(ch$x$opts$series[[3]]$name, "Binned residuals")
  expect_true(!is.null(ch$x$opts$series[[2]]$areaStyle))
})

# ---- echart_pred_vs_actual ---------------------------------------------------

test_that("echart pred vs actual overlays actual and predicted histograms", {
  skip_if_not_installed("echarts4r")
  ch <- echart_pred_vs_actual(ec_fit_cont, is_logistic = FALSE,
                              outcome_label = "Welfare ($/day)")
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$series), 2L)
  expect_equal(vapply(ch$x$opts$series, function(s) s$name, character(1)),
               c("Actual", "Predicted"))
  expect_match(ch$x$opts$xAxis[[1]]$name, "Welfare")
  expect_match(ch$x$opts$yAxis[[1]]$name, "Share of households")
})

test_that("echart pred vs actual draws the calibration curve for logit", {
  skip_if_not_installed("echarts4r")
  m_glm <- glm(urban ~ tx + pr, data = ec_dat, family = binomial)
  ch <- echart_pred_vs_actual(m_glm, is_logistic = TRUE)
  expect_s3_class(ch, "echarts4r")
  expect_equal(ch$x$opts$xAxis[[1]]$min, 0)
  expect_equal(ch$x$opts$xAxis[[1]]$max, 1)
  # Diagonal reference: one two-coordinate segment.
  expect_equal(length(ch$x$opts$series[[3]]$markLine$data[[1]]), 2L)
})

# ---- echart_welfare_quantile_hist --------------------------------------------

test_that("echart welfare histogram marks the estimated quantiles", {
  skip_if_not_installed("echarts4r")
  ch <- echart_welfare_quantile_hist(ec_dat$welfare, c(0.1, 0.5, 0.9),
                                     "Welfare ($/day)")
  expect_s3_class(ch, "echarts4r")
  expect_equal(ch$x$opts$series[[1]]$type, "bar")
  expect_equal(length(ch$x$opts$series[[1]]$markLine$data), 3L)
  expect_match(ch$x$opts$yAxis[[1]]$name, "Share of households")
  # Empty input -> informative blank widget.
  expect_s3_class(echart_welfare_quantile_hist(numeric(0), 0.5, "y"),
                  "echarts4r")
})

# ---- echart_resid_weather ----------------------------------------------------

test_that("echart resid weather draws scatter + bin means over a value axis", {
  skip_if_not_installed("echarts4r")
  ch <- echart_resid_weather(ec_fit_cont, "tx", weather_df = ec_dat,
                             x_label = "Max temp (deg C)")
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$series), 3L)
  expect_equal(vapply(ch$x$opts$series, function(s) s$type, character(1)),
               c("scatter", "line", "scatter"))
  expect_match(ch$x$opts$yAxis[[1]]$name, "Residuals")
  expect_null(echart_resid_weather(ec_fit_cont, "zz", weather_df = ec_dat))
})

test_that("echart resid weather orders binned predictors numerically", {
  skip_if_not_installed("echarts4r")
  ch <- echart_resid_weather(ec_fit_bin, "tx_bin",
                             weather_df = data.frame(tx_bin = ec_dat$tx_bin))
  expect_s3_class(ch, "echarts4r")
  # One mean point per bin: 3 modelled bins + the omitted reference bin
  # recovered from the binned-column fallback.
  expect_equal(length(ch$x$opts$series[[2]]$data), 4L)
  expect_s3_class(ch$x$opts$xAxis[[1]]$axisLabel$formatter, "JS_EVAL")
})
