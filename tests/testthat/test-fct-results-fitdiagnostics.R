# ============================================================================ #
# tests/testthat/test-fct-results-fitdiagnostics.R                             #
# Model fit diagnostics: residual panels, calibration curves and fit stats.    #
# Covers linear (fixest), binary (glm + feglm) and RIF-style model lists.      #
# ============================================================================ #

library(testthat)

set.seed(42)
n <- 200
dat <- data.frame(
  y   = rnorm(n, mean = 10, sd = 2),
  x   = rnorm(n),
  g   = rep(1:5, each = n / 5),
  bin = rbinom(n, 1, plogis(-0.5 + 0.3 * rnorm(n)))
)
dat$bin_x <- rnorm(n)
dat$bin_p <- plogis(-0.5 + 0.4 * dat$bin_x)
dat$bin   <- rbinom(n, 1, dat$bin_p)

m_lin   <- fixest::feols(y ~ x | g, data = dat)
m_glm   <- stats::glm(bin ~ bin_x, data = dat, family = stats::binomial())
m_feglm <- fixest::feglm(bin ~ bin_x | g, data = dat,
                         family = stats::binomial())

# ---- calc_fit_stats ----------------------------------------------------------

test_that("linear fit stats use fit-card names and formatting", {
  s <- calc_fit_stats(m_lin, is_logistic = FALSE, engine = "fixest")
  expect_identical(s$Statistic,
                   c("Observations", "R\u00b2", "Adj. R\u00b2", "Within R\u00b2"))
  expect_equal(s$Value[1], "200")
  expect_match(s$Value[2], "^(<0\\.01|[01]\\.\\d{2})$")
  expect_false(any(grepl("R-squared", s$Statistic)))
})

test_that("sub-0.005 R2 renders as '<0.01' and missing stats as em dash", {
  # y orthogonal to x by construction: exact zero R-squared
  d0 <- data.frame(y = c(1:10, 1:10), x = c(rep(0, 10), rep(1, 10)))
  s0 <- calc_fit_stats(fixest::feols(y ~ x, data = d0),
                       is_logistic = FALSE, engine = "fixest")
  expect_equal(s0$Value[s0$Statistic == "R\u00b2"], "<0.01")
})

test_that("logistic fit stats report McFadden R2 and AIC", {
  s <- calc_fit_stats(m_glm, is_logistic = TRUE, engine = "fixest")
  expect_identical(s$Statistic, c("Observations", "McFadden R\u00b2", "AIC"))
  expect_match(s$Value[2], "^(<0\\.01|[01]\\.\\d{2})$")
  expect_match(s$Value[3], "^[0-9,]+$")
})

test_that("feglm binomial with FE works for both stat variants", {
  expect_no_error(s <- calc_fit_stats(m_feglm, is_logistic = TRUE,
                                      engine = "fixest"))
  expect_identical(s$Statistic, c("Observations", "McFadden R\u00b2", "AIC"))
})

test_that("RIF engine returns one formatted row per requested tau", {
  rif_list <- list(
    fixest::feols(y ~ x, data = dat),
    fixest::feols(y ~ x, data = dat),
    fixest::feols(y ~ x, data = dat)
  )
  s <- calc_fit_stats(rif_list, is_logistic = FALSE, engine = "rif",
                      taus = c(0.25, 0.5, 0.75))
  expect_identical(names(s), c("Quantile", "N", "R\u00b2", "Within R\u00b2"))
  expect_identical(s$Quantile,
                   paste0("\u03c4 = ", c("0.25", "0.5", "0.75")))
  expect_match(s[["Within R\u00b2"]], "^(<0\\.01|[01]\\.\\d{2}|\u2014)$")

  # taus shorter than the model list are honoured; NULL falls back to the grid
  s2 <- calc_fit_stats(rif_list, is_logistic = FALSE, engine = "rif",
                       taus = c(0.25, 0.75))
  expect_equal(nrow(s2), 2)
  s3 <- calc_fit_stats(rif_list, is_logistic = FALSE, engine = "rif")
  expect_equal(nrow(s3), 3)
  expect_match(s3$Quantile[1], "0\\.1")
})
