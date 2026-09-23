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
    x_label = "Max temp (deg C)", y_label = "Effect size (log points)"
  )
  expect_s3_class(ch, "echarts4r")
  # One ribbon trio (lo, band, est line) + the observed-value rug scatter.
  expect_equal(length(ch$x$opts$series), 4L)
  expect_equal(ch$x$opts$series[[1]]$type, "line")
  expect_true(!is.null(ch$x$opts$series[[2]]$areaStyle))
  expect_equal(ch$x$opts$series[[4]]$name, "Observed")
  expect_match(ch$x$opts$xAxis[[1]]$name, "Max temp")
  expect_false(ch$x$opts$xAxis[[1]]$axisLine$onZero)
  expect_equal(ch$x$opts$yAxis[[1]]$name, "Effect size (log points)")
  x_sample <- ec_dat$tx
  expect_equal(ch$x$opts$xAxis[[1]]$min, min(x_sample, na.rm = TRUE))
  expect_equal(ch$x$opts$xAxis[[1]]$max, max(x_sample, na.rm = TRUE))
  expect_gt(ch$x$opts$xAxis[[1]]$min, 0)
  expect_equal(ch$x$opts$title[[length(ch$x$opts$title)]]$textAlign, "left")
  expect_false(ch$x$opts$grid$containLabel)
  expect_equal(ch$x$opts$grid$left, 64)
  # Mean reference and the dashed zero line ride on the estimate series.
  expect_equal(length(ch$x$opts$series[[3]]$markLine$data), 2L)
})

test_that("continuous all-negative effect plot leaves room for a labeled zero", {
  fit <- fixest::feols(welfare ~ tx, data = transform(ec_dat, welfare = -tx))
  ch <- echart_weather_effect_plot(
    fit, "tx", character(0), FALSE, ec_lf, "fixest",
    weather_df = ec_dat
  )
  expect_lt(ch$x$opts$yAxis[[1]]$min, 0)
  expect_gt(ch$x$opts$yAxis[[1]]$max, 0)
  zero_mark <- ch$x$opts$series[[3]]$markLine$data[[1]]
  expect_true(zero_mark$label$show)
  expect_equal(zero_mark$label$formatter, "0")
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
    rif_grid = grid, mark_taus = c(0.1, 0.5, 0.9), weather_df = ec_dat
  )
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$grid), 3L)
  expect_true(all(vapply(ch$x$opts$grid, function(g) !is.na(g$width), logical(1))))
  expect_true(all(vapply(ch$x$opts$grid, function(g) identical(g$containLabel, FALSE), logical(1))))
  expect_equal(vapply(ch$x$opts$xAxis, `[[`, integer(1), "gridIndex"), 0:2)
  expect_equal(vapply(ch$x$opts$yAxis, `[[`, integer(1), "gridIndex"), 0:2)
  expect_match(ch$x$opts$yAxis[[1]]$nameLocation, "middle")
  expect_equal(ch$x$opts$yAxis[[1]]$nameRotate, 90)
  expect_true(all(vapply(ch$x$opts$title[seq_len(3)], function(t) {
    startsWith(t$text, "Bin: ")
  }, logical(1))))
  note <- ch$x$opts$title[[length(ch$x$opts$title)]]$subtext
  expect_match(note, paste0("Omitted weather bin: ", .cut_bin_label(levels(ec_dat$tx_bin)[1])))
  expect_true(all(vapply(ch$x$opts$grid, function(g) g$bottom >= 76, logical(1))))
  # One ribbon trio per bin; the estimate series carries zero + 3 tau marks.
  expect_equal(length(ch$x$opts$series), 9L)
  expect_equal(vapply(ch$x$opts$series, `[[`, integer(1), "xAxisIndex"), rep(0:2, each = 3))
  expect_equal(vapply(ch$x$opts$series, `[[`, integer(1), "yAxisIndex"), rep(0:2, each = 3))
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
  expect_equal(ch_main$x$opts$yAxis[[1]]$name, "Effect size (log points)")
})

test_that("RIF binned Who plot separates readable panels from its legend", {
  skip_if_not_installed("echarts4r")
  bins <- paste0("tx_bin", levels(ec_dat$tx_bin)[2:4])
  rif_grid <- do.call(rbind, lapply(c(bins, paste0(bins, ":urban")), function(term) {
    ec_mk_grid(term, ec_taus)
  }))
  ch <- echart_weather_effect_plot(
    ec_fit_bin, "tx_bin", "tx_bin:urban", TRUE, ec_lf, "rif",
    rif_grid = rif_grid, weather_df = ec_dat, mode = "auto"
  )

  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$grid), length(bins))
  expect_true(all(vapply(ch$x$opts$grid, function(g) !is.na(g$width), logical(1))))
  expect_true(all(vapply(ch$x$opts$grid, function(g) identical(g$containLabel, FALSE), logical(1))))
  expect_equal(vapply(ch$x$opts$xAxis, `[[`, integer(1), "gridIndex"), seq_along(bins) - 1L)
  expect_equal(vapply(ch$x$opts$yAxis, `[[`, integer(1), "gridIndex"), seq_along(bins) - 1L)
  expect_true(all(vapply(ch$x$opts$title[seq_along(bins)], function(t) {
    startsWith(t$text, "Bin: ") && grepl("–", t$text, fixed = TRUE)
  }, logical(1))))
  expect_match(ch$x$opts$yAxis[[1]]$nameLocation, "middle")
  expect_equal(ch$x$opts$yAxis[[1]]$nameRotate, 90)
  expect_true(all(vapply(ch$x$opts$grid, function(g) g$bottom >= 98, logical(1))))
  who_note <- ch$x$opts$title[[length(ch$x$opts$title)]]$subtext
  expect_match(who_note, "Omitted weather bin:")
  expect_equal(ch$x$opts$legend$top, 4)
  expect_true(all(vapply(ch$x$opts$grid, function(g) g$top >= 58, logical(1))))
  expect_true(all(vapply(ch$x$opts$grid, function(g) !is.na(g$width), logical(1))))
  expect_true(all(vapply(ch$x$opts$series, function(s) {
    identical(s$xAxisIndex, s$yAxisIndex)
  }, logical(1))))
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
    pred_var = "tx", x_label = "Effect size (log points)",
    has_controls = TRUE
  )
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$series), 3L)
  expect_equal(vapply(ch$x$opts$series, `[[`, character(1), "name"),
    c("No FE or covariates", "No covariates", "FE + covariates"))
  expect_equal(ch$x$opts$yAxis$type, "value")
  expect_match(ch$x$opts$xAxis$name, "Effect size")
  # CI whiskers + dashed zero reference on the first series' markLine.
  expect_true(length(ch$x$opts$series[[1]]$markLine$data) > 1L)
  expect_s3_class(ch$x$opts$yAxis$axisLabel$formatter, "JS_EVAL")
  expect_match(ch$x$opts$yAxis$axisLabel$formatter, "Max temp", fixed = TRUE)
  expect_match(ch$x$opts$yAxis$axisLabel$formatter, "²", fixed = TRUE)
  expect_true(ch$x$opts$yAxis$axisLabel$showMinLabel)
  expect_true(ch$x$opts$yAxis$axisLabel$showMaxLabel)
  model_y <- lapply(ch$x$opts$series, function(s) {
    vapply(s$data, function(d) d$value[[2]], numeric(1))
  })
  first_term_positions <- vapply(model_y, `[[`, numeric(1), 1L)
  expect_length(unique(first_term_positions), 3L)
  expect_equal(first_term_positions, c(0.72, 1, 1.28))
  expect_equal(ch$x$opts$yAxis$min, 0)
  expect_equal(ch$x$opts$yAxis$max, 3)
  expect_equal(model_y[[2]], seq_along(model_y[[2]]))

  # RIF uses the same specification plot, filtered to one selected tau.
  rif_data <- do.call(rbind, lapply(1:3, function(model) {
    d <- ec_mk_grid("tx", ec_taus, model = model)
    d$estimate <- d$estimate + model
    d
  }))
  rif_ch <- echart_make_coefplot(
    fit1 = NULL, fit2 = NULL, fit3 = NULL,
    weather_terms = "tx", interaction_terms = character(0),
    engine = "rif", rif_grid = rif_data, pred_var = "tx", tau = 0.7,
    x_label = "Effect size (log points)"
  )
  expect_s3_class(rif_ch, "echarts4r")
  expect_equal(length(rif_ch$x$opts$series), 3L)
  expect_equal(vapply(rif_ch$x$opts$series, `[[`, character(1), "name"),
    c("No FE or covariates", "No covariates", "FE + covariates"))
  expect_equal(rif_ch$x$opts$legend$left, "center")
  expect_equal(rif_ch$x$opts$xAxis$name, "Effect size (log points)")
  estimates <- lapply(rif_ch$x$opts$series, function(s) s$data[[1]]$value[[1]])
  expect_equal(unlist(estimates), vapply(1:3, function(model) {
    idx <- which(rif_data$model == model & abs(rif_data$tau - 0.7) < 1e-8)
    rif_data$estimate[idx[1]]
  }, numeric(1)))
  median_ch <- echart_make_coefplot(
    fit1 = NULL, fit2 = NULL, fit3 = NULL,
    weather_terms = "tx", interaction_terms = character(0),
    engine = "rif", rif_grid = rif_data, pred_var = "tx"
  )
  median_estimates <- lapply(median_ch$x$opts$series, function(s) s$data[[1]]$value[[1]])
  expect_equal(unlist(median_estimates), vapply(1:3, function(model) {
    idx <- which(rif_data$model == model & abs(rif_data$tau - 0.5) < 1e-8)
    rif_data$estimate[idx[1]]
  }, numeric(1)))
})

test_that("RIF polynomial stability combines coefficients into total effect", {
  skip_if_not_installed("echarts4r")
  tau <- 0.5
  terms <- c("tx", "I(tx^2)")
  grid <- do.call(rbind, lapply(1:3, function(model) {
    data.frame(
      term = terms, tau = tau, model = model,
      estimate = c(1, 0.1) * model,
      std.error = c(0.2, 0.05),
      conf.low = c(0.608, 0.002), conf.high = c(1.392, 0.198),
      stringsAsFactors = FALSE
    )
  }))
  ch <- echart_make_coefplot(
    fit1 = NULL, fit2 = NULL, fit3 = ec_fit_cont,
    weather_terms = "tx", interaction_terms = character(0),
    engine = "rif", rif_grid = grid, pred_var = "tx", tau = tau,
    label_fun = ec_lf, x_label = "Effect size (log points)",
    train_data = ec_dat
  )
  mm <- resolve_model_matrix(ec_fit_cont)
  x_mean <- mean(mm$tx)
  expected <- c(1, 2, 3) * (1 + 2 * 0.1 * x_mean)
  actual <- vapply(ch$x$opts$series, function(s) s$data[[1]]$value[[1]], numeric(1))
  expect_equal(actual, expected)
  expect_equal(ch$x$opts$xAxis$name, "Effect size (log points)")
  expect_true(grepl("Polynomial weather terms combined", ch$x$opts$title[[1]]$subtext))
})

test_that("RIF stability predictor lookup matches wrapped predictor tokens only", {
  grid <- data.frame(
    term = rep(c("t", "I(t^2)", "temp", "I(temp^2)", "t:urban"), 3),
    tau = 0.5, model = rep(1:3, each = 5),
    estimate = seq_len(15) / 10, std.error = 0.05,
    stringsAsFactors = FALSE
  )
  ch <- echart_make_coefplot(
    fit1 = NULL, fit2 = NULL, fit3 = NULL,
    weather_terms = "t", interaction_terms = "t:urban",
    engine = "rif", rif_grid = grid, pred_var = "t", tau = 0.5
  )
  expect_s3_class(ch, "echarts4r")
  expect_false(grepl("No RIF coefficients found", ch$x$opts$title[[1]]$text))
  expect_equal(length(ch$x$opts$series), 3L)
})

test_that("RIF continuous polynomial effects use one marginal-effect panel", {
  skip_if_not_installed("echarts4r")
  rif_poly <- ec_mk_grid(c("tx", "I(tx^2)"), ec_taus)
  ch <- echart_weather_effect_plot(
    ec_fit_cont, "tx", character(0), FALSE, ec_lf, "rif",
    rif_grid = rif_poly, mark_taus = NULL
  )
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$grid), 5L)
  expect_equal(length(ch$x$opts$series), 3L)
  expect_true(any(vapply(ch$x$opts$series, function(s) !is.null(s$markLine), logical(1))))
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
  expect_false(any(grepl("\\[|\\]", ch$x$opts$yAxis[[1]]$data)))
  expect_false(any(grepl("tx_bin", ch$x$opts$yAxis[[1]]$data, fixed = TRUE)))
  # Unresolvable model matrix -> informative blank widget.
  expect_s3_class(echart_importance(structure(list(), class = "nope")),
                  "echarts4r")

  binned <- fixest::feols(welfare ~ tx_bin + urban, data = ec_dat)
  binned_ch <- echart_importance(binned, label_fun = ec_lf)
  labels <- binned_ch$x$opts$yAxis[[1]]$data
  expect_false(any(grepl("tx_bin|[\\[\\]]", labels)))
  expect_true(any(grepl("–", labels, fixed = TRUE)))
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
  expect_equal(ch$x$opts$yAxis[[1]]$min, 0)
  expect_lt(ch$x$opts$yAxis[[1]]$max, 100)
  expect_equal(ch$x$opts$yAxis[[1]]$interval, ch$x$opts$yAxis[[1]]$max / 5)
  expect_true(all(vapply(ch$x$opts$series, function(s) {
    all(unlist(s$data) >= 0 & unlist(s$data) <= 100)
  }, logical(1))))
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
