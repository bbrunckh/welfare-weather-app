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

# Compare a finite-difference aggregate move against the analytic gradient's
# prediction, scaled by the larger of the two to stay meaningful near zero.
expect_near_fd <- function(object, expected, tol = 1e-4, info = NULL) {
  scale <- max(abs(expected), abs(object), 1e-12)
  testthat::expect_true(
    abs(object - expected) <= tol * scale,
    info = paste0(info, ": observed=", format(object),
                  " expected=", format(expected))
  )
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

test_that("avg_poverty SE matches MC for the days-needed-to-earn-$1 metric", {
  # avg_poverty is mean(1 / welfare) over valid rows — "days needed to earn
  # $1" — not the conditional mean among the poor (method_uncertainty.md
  # §3.7). Gradient: h_i = -1/(n_ok * mu_i) unweighted and
  # -w_i / (W_ok * mu_i) weighted; strictly negative for every valid
  # household because raising welfare lowers days-to-$1, zero otherwise.
  pipe <- make_pipeline()

  mu <- exp(pipe$y_point)
  w_tilde <- pipe$weights / sum(pipe$weights)
  value_pt <- wiseapp:::resolve_agg_fn("avg_poverty")(mu, pipe$weights, NULL)
  h <- wiseapp:::gradient_for_method(
    method   = "avg_poverty",
    mu       = mu,
    weights  = pipe$weights,
    pov_line = NULL,
    value_pt = value_pt
  )
  ok <- is.finite(mu) & mu > 0
  expect_true(all(h[ok] < 0))              # every valid household lowers T
  expect_equal(h[!ok], rep(0, sum(!ok)))   # invalid rows never contribute
  W_ok <- sum(pipe$weights[ok])
  expect_equal(h[ok], -pipe$weights[ok] / (W_ok * mu[ok]))  # exact formula

  # SE accuracy vs. MC
  res <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point   = pipe$y_point,
    F_loading = pipe$F_loading,
    method    = "avg_poverty",
    weights   = pipe$weights
  )
  se_delta <- sqrt(res$var_coef)
  se_mc    <- mc_se(pipe, "avg_poverty", weights = pipe$weights)
  expect_true(is.finite(se_delta) && se_delta > 0)
  expect_lt(abs(se_delta - se_mc) / se_mc, 0.10)
})

test_that("prosperity_gap gradient matches MC and exact formula", {
  # prosperity_gap is mean(pmax(28 / welfare, 1)) — the average factor by
  # which incomes must rise to reach $28/day. Gradient below the threshold:
  # h_i = -28/(N * mu_i) unweighted, -(w_i/W) * 28/mu_i weighted; zero above
  # (pmax is flat) and for non-positive mu.
  pipe <- make_pipeline()

  mu <- exp(pipe$y_point)
  h <- wiseapp:::gradient_for_method(
    method   = "prosperity_gap",
    mu       = mu,
    weights  = pipe$weights,
    pov_line = NULL,
    value_pt = wiseapp:::resolve_agg_fn("prosperity_gap")(mu, pipe$weights, NULL)
  )
  below <- is.finite(mu) & mu > 0 & mu < 28
  W <- sum(pipe$weights)
  expect_true(all(h[!below] == 0))
  expect_equal(h[below], -(pipe$weights[below] / W) * 28 / mu[below])  # exact

  res <- wiseapp:::aggregate_with_uncertainty_delta(
    y_point   = pipe$y_point,
    F_loading = pipe$F_loading,
    method    = "prosperity_gap",
    weights   = pipe$weights
  )
  se_delta <- sqrt(res$var_coef)
  se_mc    <- mc_se(pipe, "prosperity_gap", weights = pipe$weights)
  expect_true(is.finite(se_delta) && se_delta > 0)
  expect_lt(abs(se_delta - se_mc) / se_mc, 0.10)
})

test_that("all smooth delta-method gradients match finite differences (unweighted)", {
  # h_i = (dT/dw_i) * w_i, so a small relative welfare perturbation
  # dw_i/w_i = eps must move the point estimate by h_i * eps. Excluded:
  # median (piecewise-constant estimate — Hampel IF is not FD-visible) and
  # headcount_ratio (discontinuous estimate; the gradient is defined on the
  # kernel-smoothed surrogate, validated separately below).
  pipe  <- make_pipeline()
  mu    <- exp(pipe$y_point)
  eps   <- 1e-6
  idx   <- 101L
  specs <- list(
    list(method = "mean",           args = list(), tol = 1e-4),
    list(method = "total",          args = list(), tol = 1e-4),
    list(method = "gap",            args = list(pov_line = 3.00), tol = 1e-4),
    list(method = "fgt2",           args = list(pov_line = 3.00), tol = 1e-4),
    list(method = "prosperity_gap", args = list(), tol = 1e-4),
    list(method = "avg_poverty",    args = list(), tol = 1e-4),
    list(method = "gini",           args = list(), tol = 1e-2)
  )
  for (sp in specs) {
    agg_fn <- wiseapp:::resolve_agg_fn(sp$method)
    z <- if (is.null(sp$args$pov_line)) 1 else sp$args$pov_line
    T0 <- agg_fn(mu, NULL, z)
    y1 <- pipe$y_point; y1[idx] <- y1[idx] + eps
    T1 <- agg_fn(exp(y1), NULL, z)
    h <- wiseapp:::gradient_for_method(
      method = sp$method, mu = mu, weights = NULL,
      pov_line = sp$args$pov_line, value_pt = T0
    )
    expect_near_fd(T1 - T0, h[idx] * eps, tol = sp$tol,
                   info = paste(sp$method, "unweighted"))
  }
})

test_that("all smooth delta-method gradients match finite differences (weighted)", {
  pipe  <- make_pipeline()
  mu    <- exp(pipe$y_point)
  eps   <- 1e-6
  idx   <- 51L
  specs <- list(
    list(method = "mean",           args = list(), tol = 1e-4),
    list(method = "total",          args = list(), tol = 1e-4),
    list(method = "gap",            args = list(pov_line = 3.00), tol = 1e-4),
    list(method = "fgt2",           args = list(pov_line = 3.00), tol = 1e-4),
    list(method = "prosperity_gap", args = list(), tol = 1e-4),
    list(method = "avg_poverty",    args = list(), tol = 1e-4),
    list(method = "gini",           args = list(), tol = 1e-2)
  )
  for (sp in specs) {
    agg_fn <- wiseapp:::resolve_agg_fn(sp$method)
    z <- if (is.null(sp$args$pov_line)) 1 else sp$args$pov_line
    T0 <- agg_fn(mu, pipe$weights, z)
    y1 <- pipe$y_point; y1[idx] <- y1[idx] + eps
    T1 <- agg_fn(exp(y1), pipe$weights, z)
    h <- wiseapp:::gradient_for_method(
      method = sp$method, mu = mu, weights = pipe$weights,
      pov_line = sp$args$pov_line, value_pt = T0
    )
    expect_near_fd(T1 - T0, h[idx] * eps, tol = sp$tol,
                   info = paste(sp$method, "weighted"))
  }
})

test_that("kernel-smoothed headcount_ratio gradient matches finite differences", {
  # The gradient is defined on the kernel-smoothed surrogate of the hard
  # headcount (the raw estimate is discontinuous and not FD-visible). With
  # F_loading = NULL the bandwidth is the fixed user value b_w = p0 * z, so
  # the surrogate is differentiable and FD-comparable.
  pipe <- make_pipeline()
  mu   <- exp(pipe$y_point)
  eps  <- 1e-6
  idx  <- 77L
  pov_line <- 3.00
  b_w  <- 0.05 * pov_line
  w_tilde <- pipe$weights / sum(pipe$weights)
  smooth <- function(mu_vec) sum(w_tilde * stats::pnorm((pov_line - mu_vec) / b_w))

  T0 <- smooth(mu)
  y1 <- pipe$y_point; y1[idx] <- y1[idx] + eps
  T1 <- smooth(exp(y1))
  h <- wiseapp:::gradient_for_method(
    method = "headcount_ratio", mu = mu, weights = pipe$weights,
    pov_line = pov_line, value_pt = T0, bandwidth_p0 = 0.05
  )
  expect_near_fd(T1 - T0, h[idx] * eps, tol = 1e-4, info = "headcount_ratio")
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
