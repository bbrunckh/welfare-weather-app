library(testthat)

# Synthetic fixture — coefficient SE chosen to be realistic for a fitted
# welfare-on-weather regression (~1% log-scale per-obs SE). Larger SEs make
# the lognormal Var(exp(y)) too non-linear for first-order delta method.
make_pipeline <- function(N = 2000, K = 8, seed = 1) {
  set.seed(seed)
  X        <- matrix(stats::rnorm(N * K, 0, 0.1), N, K)
  beta     <- stats::rnorm(K, 0, 0.2)
  Sigma    <- (crossprod(matrix(stats::rnorm(K * K, 0, 0.05), K, K)) / K
                + diag(K) * 0.0001)
  L        <- t(chol(Sigma))
  y_point  <- as.numeric(X %*% beta) + log(3.0)
  F_loading <- X %*% L
  weights  <- stats::runif(N, 0.5, 2.0)
  list(y_point = y_point, F_loading = F_loading,
       weights = weights, sigma_e = 0.05)
}

# Monte Carlo reference SE for a given method
mc_se <- function(pipe, method, weights = NULL, pov_line = NULL,
                  is_log = TRUE, S = 5000, residuals = "none",
                  sigma_e = 0) {
  set.seed(42)
  N <- length(pipe$y_point); K <- ncol(pipe$F_loading)
  Z <- matrix(stats::rnorm(S * K), S, K)
  perturb <- pipe$F_loading %*% t(Z)   # N x S
  vals <- numeric(S)
  agg_fn <- wiseapp:::resolve_agg_fn(method)
  for (s in seq_len(S)) {
    eps <- if (sigma_e > 0) stats::rnorm(N, 0, sigma_e) else 0
    y_s <- pipe$y_point + perturb[, s] + eps
    w_s <- if (is_log) exp(y_s) else y_s
    vals[s] <- agg_fn(w_s, weights, pov_line)
  }
  stats::sd(vals)
}

test_that("delta-method mean matches MC SE within 5%", {
  pipe <- make_pipeline()
  res <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point   = pipe$y_point,
    F_loading = pipe$F_loading,
    method    = "mean",
    weights   = pipe$weights,
    is_log    = TRUE
  )
  se_delta <- sqrt(res$var_coef)
  se_mc    <- mc_se(pipe, "mean", weights = pipe$weights)
  expect_lt(abs(se_delta - se_mc) / se_mc, 0.05)
})

test_that("delta-method total matches MC SE within 5%", {
  pipe <- make_pipeline()
  res <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point   = pipe$y_point,
    F_loading = pipe$F_loading,
    method    = "total",
    weights   = pipe$weights
  )
  se_delta <- sqrt(res$var_coef)
  se_mc    <- mc_se(pipe, "total", weights = pipe$weights)
  expect_lt(abs(se_delta - se_mc) / se_mc, 0.05)
})

test_that("delta-method gap matches MC SE within 10%", {
  pipe <- make_pipeline()
  res <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point   = pipe$y_point,
    F_loading = pipe$F_loading,
    method    = "gap",
    weights   = pipe$weights,
    pov_line  = 3.00
  )
  se_delta <- sqrt(res$var_coef)
  se_mc    <- mc_se(pipe, "gap", weights = pipe$weights, pov_line = 3.00)
  expect_lt(abs(se_delta - se_mc) / se_mc, 0.10)
})

test_that("delta-method fgt2 matches MC SE within 10%", {
  pipe <- make_pipeline()
  res <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point   = pipe$y_point,
    F_loading = pipe$F_loading,
    method    = "fgt2",
    weights   = pipe$weights,
    pov_line  = 3.00
  )
  se_delta <- sqrt(res$var_coef)
  se_mc    <- mc_se(pipe, "fgt2", weights = pipe$weights, pov_line = 3.00)
  expect_lt(abs(se_delta - se_mc) / se_mc, 0.10)
})

test_that("delta-method headcount with smoothing returns finite SE", {
  # Headcount is a kernel-smoothed approximation; absolute accuracy depends on
  # how clustered the welfare distribution is around the poverty line and on
  # the bandwidth choice. Test that the SE is positive, finite, and within an
  # order of magnitude of the MC reference (true tuning lives in the UI knob).
  pipe <- make_pipeline()
  res <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point      = pipe$y_point,
    F_loading    = pipe$F_loading,
    method       = "headcount_ratio",
    weights      = pipe$weights,
    pov_line     = 3.00,
    bandwidth_p0 = 0.05
  )
  se_delta <- sqrt(res$var_coef)
  se_mc    <- mc_se(pipe, "headcount_ratio",
                    weights = pipe$weights, pov_line = 3.00)
  expect_true(is.finite(se_delta) && se_delta > 0)
  expect_lt(abs(se_delta - se_mc) / se_mc, 0.5)
})

test_that("F_loading = NULL gives zero coefficient variance", {
  pipe <- make_pipeline()
  res <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point   = pipe$y_point,
    F_loading = NULL,
    method    = "mean",
    weights   = pipe$weights
  )
  expect_equal(res$var_coef, 0)
  expect_equal(res$value_lo, res$value)
  expect_equal(res$value_hi, res$value)
})

test_that("combine_ensemble_results: 1-member ensemble has degenerate thick band", {
  pipe <- make_pipeline()
  m <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point = pipe$y_point, F_loading = pipe$F_loading,
    method = "mean", weights = pipe$weights
  )
  comb <- wiseapp::combine_ensemble_results(list(m))
  expect_equal(comb$value, m$value)
  expect_equal(comb$value_lo, comb$value_hi)  # one member -> degenerate
  expect_gt(comb$coef_hi - comb$coef_lo, 0)   # but coef band non-trivial
})

test_that("combine_ensemble_results: pooled SE matches mean(var) + var(values)", {
  pipe <- make_pipeline()
  # Build 5 fake members by perturbing y_point
  set.seed(7)
  members <- lapply(1:5, function(i) {
    p <- pipe
    p$y_point <- p$y_point + stats::rnorm(1, 0, 0.05)
    wiseapp:::aggregate_with_uncertainty_delta(
      y_point = p$y_point, F_loading = p$F_loading,
      method = "mean", weights = p$weights
    )
  })
  comb <- wiseapp::combine_ensemble_results(members)
  vals <- vapply(members, `[[`, numeric(1), "value")
  vc   <- vapply(members, `[[`, numeric(1), "var_coef")
  expected_var <- mean(vc) + stats::var(vals)
  expect_equal(comb$var_pool, expected_var, tolerance = 1e-10)
})
