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

test_that("point-estimate fast path returns exact values without uncertainty work", {
  pipe <- make_pipeline(N = 80, K = 3)
  pipe$train_aug <- data.frame(.resid = stats::rnorm(80))
  pipe$sim_year <- rep(c(2030L, 2031L), each = 40L)

  for (method in c("mean", "median", "total", "headcount_ratio", "gap",
                   "fgt2", "gini", "prosperity_gap", "avg_poverty")) {
    pov <- if (method %in% c("headcount_ratio", "gap", "fgt2")) 3 else NULL
    fast <- aggregate_pipeline_per_year(
      pipe, method = method, weighted = TRUE, pov_line = pov,
      residuals = "none", skip_coef = TRUE, is_log = TRUE
    )
    oracle <- lapply(seq_along(fast), function(i) {
      idx <- pipe$sim_year == fast[[i]]$sim_year
      mu <- exp(pipe$y_point[idx])
      aggregate_point_estimate(mu, method, pipe$weights[idx], pov) |>
        within(sim_year <- fast[[i]]$sim_year)
    })
    expect_identical(fast, oracle, info = method)
    expect_true(all(vapply(fast, function(x) is.null(x$F_agg), logical(1))))
  }
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
  # median (piecewise-constant estimate; the gradient is the smoothed-quantile
  # derivative, validated against Monte Carlo below) and
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

test_that("median delta-method SD matches Monte Carlo (R2-BUG-03)", {
  # The Hampel-IF gradient gave ratios of ~0 (intercept-like loading) and
  # ~0.25 (correlated loading); the smoothed-quantile derivative gives ~1.
  set.seed(3)
  N <- 4000
  y <- stats::rnorm(N, log(3), 0.6)
  w <- stats::runif(N, 0.5, 2)
  cases <- list(
    intercept  = matrix(0.05, N, 1),
    correlated = cbind(0.03 + 0.02 * (y - mean(y)), stats::rnorm(N, 0, 0.02)),
    mixed      = cbind(rep(0.04, N), stats::rnorm(N, 0, 0.05),
                       stats::rnorm(N, 0, 0.05))
  )
  for (nm in names(cases)) {
    pipe <- list(y_point = y, F_loading = cases[[nm]])
    res <- wiseapp:::aggregate_with_uncertainty_delta(
      y_point = y, F_loading = pipe$F_loading, method = "median",
      weights = w, residuals = "none"
    )
    ratio <- sqrt(res$var_coef) / mc_se(pipe, "median", weights = w, S = 2000)
    expect_gt(ratio, 0.9, label = paste(nm, "ratio"))
    expect_lt(ratio, 1.1, label = paste(nm, "ratio"))
  }

  # Common log shift: gradients sum to ~ m (median moves by m * delta).
  h <- wiseapp:::gradient_for_method(
    method = "median", mu = exp(y), weights = w, pov_line = NULL,
    value_pt = wiseapp:::resolve_agg_fn("median")(exp(y), w, NULL)
  )
  m <- wiseapp:::resolve_agg_fn("median")(exp(y), w, NULL)
  expect_lt(abs(sum(h) / m - 1), 0.02)
})


test_that("one non-finite row is dropped and counted, not zeroing variance (R2-BUG-02)", {
  pipe <- make_pipeline()
  run <- function(y, F, w, method) {
    pov <- if (method == "headcount_ratio") 3 else NULL
    wiseapp:::aggregate_with_uncertainty_delta(
      y_point = y, F_loading = F, method = method, weights = w,
      pov_line = pov, residuals = "none"
    )
  }
  for (method in c("mean", "median", "gini", "headcount_ratio")) {
    clean <- run(pipe$y_point, pipe$F_loading, pipe$weights, method)
    expect_identical(clean$n_coef_dropped, 0L)

    F_na <- pipe$F_loading; F_na[17L, 2L] <- NA
    w_na <- pipe$weights;   w_na[17L] <- NA
    y_na <- pipe$y_point;   y_na[17L] <- NA
    cases <- list(
      F_row  = run(pipe$y_point, F_na, pipe$weights, method),
      y      = run(y_na, pipe$F_loading, pipe$weights, method)
    )
    # An NA weight makes most point estimates NA (resolver behaviour, out of
    # scope here); the mean gradient does not depend on the point estimate.
    if (method == "mean") {
      cases$weight <- run(pipe$y_point, pipe$F_loading, w_na, method)
    }
    for (nm in names(cases)) {
      res <- cases[[nm]]
      info <- paste(method, nm)
      expect_identical(res$n_coef_dropped, 1L, info = info)
      expect_gt(res$var_coef, 0)
      expect_lt(abs(res$var_coef / clean$var_coef - 1), 0.01, label = info)
    }
  }
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
