library(testthat)

# Synthetic OLS fixture: log-welfare ~ temp + transfer + temp:transfer.
# `transfer` flips from 0 to 1 between baseline and policy survey frames.
make_ols_fixture <- function(N = 300, seed = 1L) {
  set.seed(seed)
  df <- data.frame(
    welfare  = exp(stats::rnorm(N, log(3), 0.25)),
    temp     = stats::rnorm(N, 25, 2),
    transfer = stats::rbinom(N, 1, 0.3),
    weight   = stats::runif(N, 0.5, 2.0)
  )
  fit <- stats::lm(log(welfare) ~ temp + transfer + temp:transfer, data = df)
  list(
    model_fit = list(
      engine        = "fixest",  # OLS path inside decompose_policy_effect
      fit3          = fit,
      weather_terms = "temp",
      train_data    = df
    ),
    so          = list(name = "welfare", transform = "log"),
    svy_base    = df,
    svy_policy  = (function() {
      p <- df
      p$transfer       <- 1L
      p$._sp_transfer  <- 5
      p
    })()
  )
}

test_that("point-estimate additivity: delta_total == delta_main + delta_res1 + delta_res2", {
  fx <- make_ols_fixture()
  r  <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                          fx$model_fit, fx$so)
  expect_s3_class(r, "data.frame")
  err <- max(abs(r$delta_total - (r$delta_main + r$delta_res1 + r$delta_res2)))
  expect_lt(err, 1e-12)
})

test_that("variance additivity: Var(total) == Var(main) + Var(res1) + Var(res2)", {
  # Under the diagonal-Σ approximation documented in fct_policy_decompose.R,
  # channels are independent and variances add exactly.
  fx <- make_ols_fixture()
  r  <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                          fx$model_fit, fx$so)
  expect_true(all(c("sd_main", "sd_res1", "sd_res2", "sd_total") %in% names(r)))
  err <- max(abs(r$sd_total^2 - (r$sd_main^2 + r$sd_res1^2 + r$sd_res2^2)))
  expect_lt(err, 1e-10)
})

test_that("skip_coef = TRUE zeroes out every per-channel SE", {
  fx <- make_ols_fixture()
  r0 <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                          fx$model_fit, fx$so,
                                          skip_coef = TRUE)
  expect_equal(max(c(r0$sd_main, r0$sd_res1, r0$sd_res2, r0$sd_total)), 0)
})

test_that("non-zero SE appears where the model actually has uncertainty", {
  fx <- make_ols_fixture()
  r  <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                          fx$model_fit, fx$so)
  # Main effect picks up the `transfer` coefficient SE (Δ transfer = 1 for
  # the 70% of households whose policy value flipped from 0 to 1) and the
  # interaction has the `temp:transfer` coefficient SE times haz · Δx.
  expect_true(any(r$sd_main > 0))
  expect_true(any(r$sd_res2 > 0))
  # OLS path: no repositioning channel → sd_res1 is identically 0.
  expect_equal(max(r$sd_res1), 0)
})

test_that("aggregated total SE remains consistent under household weighting", {
  # Var(Σ w·δ / Σ w) under household independence should match the sum of
  # the three per-channel weighted variances (independence ⇒ Cov = 0).
  fx <- make_ols_fixture()
  r  <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                          fx$model_fit, fx$so)
  w_norm <- r$weight / sum(r$weight)
  agg_var <- function(sd_col) sum((w_norm^2) * (r[[sd_col]])^2)
  v_components <- agg_var("sd_main") + agg_var("sd_res1") + agg_var("sd_res2")
  v_total      <- agg_var("sd_total")
  expect_lt(abs(v_total - v_components) / pmax(v_total, 1e-12), 1e-10)
})
