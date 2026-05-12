# fct_aggregation_delta.R
# -----------------------
# Closed-form delta-method uncertainty aggregation.
#
# Replaces the Monte Carlo path in aggregate_with_uncertainty() for the common
# case where we just want (value, var) tuples that feed into combine_ensemble_results.
#
# For aggregate T = g(welfare), with welfare_i = exp(y_point_i + resid_i + F_loading[i,] %*% z),
# the first-order Taylor expansion gives:
#
#   Var(T) approx || F^T h ||^2 + sigma_e^2 * sum(h^2)
#
# where h_i = (dT/dwelfare_i) * mu_i is the welfare-scale gradient lifted to log
# scale via chain rule, and mu_i = exp(y_point_i + resid_i) is the point welfare.
#
# Exports:
#   aggregate_with_uncertainty_delta
#
# Internal:
#   gradient_<method> helpers
#   apply_band_transform


#' Aggregate welfare predictions with delta-method coefficient uncertainty
#'
#' Pure closed-form variance — no Monte Carlo. Returns the point estimate and
#' analytic band endpoints in O(N*K) time.
#'
#' @param y_point Numeric vector length N. Log-scale point predictions.
#' @param F_loading Numeric matrix N x K, or \code{NULL}. Factor loading from
#'   \code{compute_factor_loading()}. \code{NULL} skips coefficient variance.
#' @param method Character. One of the methods returned by
#'   \code{hist_aggregate_choices()}.
#' @param weights Numeric vector length N or \code{NULL}.
#' @param pov_line Numeric scalar or \code{NULL}. Required for poverty methods.
#' @param residuals Character. One of \code{"none"}, \code{"original"},
#'   \code{"normal"}, \code{"resample"}.
#' @param train_aug Data frame with \code{.resid} column, or \code{NULL}.
#' @param id_vec,id_col Vector and character. For \code{residuals = "original"}.
#' @param is_log Logical. \code{TRUE} (default) back-transforms via \code{exp()}.
#' @param band_q Named numeric \code{c(lo=, hi=)}.
#' @param bandwidth_p0 Numeric scalar. Kernel smoothing bandwidth for
#'   \code{headcount_ratio}. Default 0.05 (log-welfare scale).
#'
#' @return Named list:
#'   \describe{
#'     \item{value}{Numeric scalar. Point estimate.}
#'     \item{value_lo, value_p50, value_hi}{Numeric scalars. Coefficient-uncertainty band.}
#'     \item{var_coef, var_resid}{Numeric scalars. Variance components.}
#'     \item{draw_values}{\code{NULL}. Compatibility slot; downstream code reads
#'       \code{value_lo}/\code{value_hi} directly.}
#'   }
#' @export
aggregate_with_uncertainty_delta <- function(y_point,
                                              F_loading,
                                              method,
                                              weights      = NULL,
                                              pov_line     = NULL,
                                              residuals    = "none",
                                              train_aug    = NULL,
                                              id_vec       = NULL,
                                              id_col       = NULL,
                                              is_log       = TRUE,
                                              band_q       = c(lo = 0.10, hi = 0.90),
                                              bandwidth_p0 = 0.05) {

  N <- length(y_point)
  stopifnot(is.numeric(y_point) && N > 0)

  resid_vec <- draw_residuals_vec(residuals, train_aug, N, id_vec, id_col)
  mu        <- if (is_log) exp(y_point + resid_vec) else y_point + resid_vec

  # Point estimate via existing resolver
  agg_fn   <- resolve_agg_fn(method)
  value_pt <- agg_fn(mu, weights, pov_line)

  # Welfare-scale gradient h_i = (dT/dw_i) * mu_i
  h <- gradient_for_method(method, mu, weights, pov_line, value_pt,
                           bandwidth_p0 = bandwidth_p0,
                           F_loading    = F_loading)

  # Coefficient variance: ||F' h||^2
  # F_agg is the per-coefficient gradient of the aggregated scalar with
  # respect to beta; ||F_agg||^2 = var_coef. Exposing F_agg lets callers
  # build contrast variances such as ||F_agg_scn - F_agg_hist||^2, which
  # is the right SE for paired counterfactuals on the same population.
  F_agg <- if (!is.null(F_loading) && !any(!is.finite(h))) {
    as.numeric(crossprod(F_loading, h))
  } else NULL
  var_coef <- if (!is.null(F_agg)) sum(F_agg * F_agg) else 0

  # Residual variance (only for stochastic residual draws)
  var_resid <- if (residuals %in% c("normal", "resample") &&
                   !is.null(train_aug) && ".resid" %in% names(train_aug)) {
    sigma_e2 <- stats::var(train_aug$.resid, na.rm = TRUE)
    if (is.finite(sigma_e2)) sigma_e2 * sum(h * h, na.rm = TRUE) else 0
  } else 0

  var_total <- var_coef + var_resid
  se        <- sqrt(max(var_total, 0))

  # Band via method-appropriate transform
  z_lo <- stats::qnorm(band_q[["lo"]])
  z_hi <- stats::qnorm(band_q[["hi"]])
  band <- apply_band_transform(method, value_pt, se, z_lo, z_hi)

  list(
    value       = value_pt,
    value_lo    = band$lo,
    value_p50   = value_pt,
    value_hi    = band$hi,
    var_coef    = var_coef,
    var_resid   = var_resid,
    F_agg       = F_agg,
    draw_values = NULL
  )
}


# ---------------------------------------------------------------------------- #
# Per-method gradient functions                                                #
# ---------------------------------------------------------------------------- #
# Each returns h = (dT/dwelfare) * mu, length N. Sign matters — preserves the
# direction of perturbation so var = ||F' h||^2 reflects the true Taylor
# expansion. For aggregates where the sign cancels in the variance (everything
# here), we still keep it explicit for clarity.

gradient_for_method <- function(method, mu, weights, pov_line, value_pt,
                                bandwidth_p0 = 0.05,
                                F_loading    = NULL) {
  N <- length(mu)
  W <- if (!is.null(weights)) sum(weights, na.rm = TRUE) else N
  if (!is.finite(W) || W <= 0) W <- N
  w_tilde <- if (!is.null(weights)) weights / W else rep(1 / N, N)

  # F_loading carried in for headcount bandwidth tuning (closed over below).
  switch(method,
    mean = w_tilde * mu,

    total = if (!is.null(weights)) weights * mu else mu,

    gap = {
      stopifnot(!is.null(pov_line))
      -w_tilde * mu * as.numeric(mu < pov_line) / pov_line
    },

    prosperity_gap = {
      stopifnot(!is.null(pov_line))
      -w_tilde * mu * as.numeric(mu < pov_line)
    },

    fgt2 = {
      stopifnot(!is.null(pov_line))
      -2 * w_tilde * mu * pmax(pov_line - mu, 0) / pov_line^2
    },

    headcount_ratio = {
      stopifnot(!is.null(pov_line))
      # Kernel-smoothed indicator on welfare scale:
      #   1{w < z_p} ~ Phi((z_p - w)/b_w)
      # Auto-tune bandwidth: max(user_b * z_p, median per-obs welfare SE).
      # The per-obs welfare SE is mu_i * sqrt(sum(F_i^2)). Using a bandwidth
      # smaller than the typical perturbation magnitude yields an undersmoothed
      # kernel that under-counts threshold crossings.
      b_user <- bandwidth_p0 * pov_line
      b_auto <- if (!is.null(F_loading)) {
        log_se <- sqrt(rowSums(F_loading * F_loading))
        stats::median(mu * log_se, na.rm = TRUE)
      } else 0
      b_w <- max(b_user, b_auto, .Machine$double.eps)
      arg <- (pov_line - mu) / b_w
      -w_tilde * stats::dnorm(arg) * mu / b_w
    },

    avg_poverty = {
      stopifnot(!is.null(pov_line))
      # T = sum_p w_i mu_i / sum_p w_i over poor (mu_i < pov_line)
      # h_i = w_tilde_i / B * 1{poor} * (mu_i - T) * mu_i
      poor <- as.numeric(mu < pov_line)
      B    <- sum(w_tilde * poor, na.rm = TRUE)
      if (!is.finite(B) || B <= 0) {
        rep(0, N)
      } else {
        (w_tilde / B) * poor * (mu - value_pt) * mu
      }
    },

    median = {
      # Hampel IF: IF_i = -(1{w_i<=m} - 0.5) / f(m)
      # Lift to log scale: h_i = w_tilde_i * mu_i * IF_i, with mu_i chain rule
      # cancelling because median is in welfare units.
      f_hat <- tryCatch({
        d <- if (!is.null(weights))
               stats::density(mu, weights = weights / sum(weights, na.rm = TRUE),
                              na.rm = TRUE)
             else stats::density(mu, na.rm = TRUE)
        approx_y <- stats::approx(d$x, d$y, xout = value_pt)$y
        if (is.finite(approx_y) && approx_y > 1e-12) approx_y else NA_real_
      }, error = function(e) NA_real_)
      if (is.na(f_hat)) return(rep(0, N))
      w_tilde * (0.5 - as.numeric(mu <= value_pt)) / f_hat
    },

    gini = {
      # Partial-derivative gradient of the weighted Gini wrt y_i, used in
      # the delta-method propagation of regression coefficient uncertainty
      # through y_i = exp(X_i beta). Distinct from Monti's (1991) IF (which
      # is correct for sample-variance estimation but inflates the SE when
      # mis-used in the chain rule through beta — it violates Gini's scale
      # invariance under intercept shifts).
      #
      # Derivation (ordering held locally fixed):
      #   G = (1/mu_bar) * sum_i w_tilde_i * y_i * (2 F_i - 1)
      #   dG/dy_i = (w_tilde_i / mu_bar) * (2 F_i - 1 - G)
      #   h_i = (dG/dy_i) * y_i = w_tilde_i * y_i * (2 F_i - 1 - G) / mu_bar
      # Scale invariance check: sum h_i = (G*mu_bar - G*mu_bar)/mu_bar = 0.
      valid <- !is.na(mu)
      if (sum(valid) < 2L) return(rep(0, N))
      ord    <- order(mu)
      mu_o   <- mu[ord]
      w_o    <- if (!is.null(weights)) weights[ord] else rep(1, N)
      w_n    <- w_o / sum(w_o, na.rm = TRUE)
      F_vec  <- cumsum(w_n) - w_n / 2
      mu_bar <- sum(w_n * mu_o, na.rm = TRUE)
      if (!is.finite(mu_bar) || mu_bar <= 0) return(rep(0, N))
      G_pt   <- value_pt
      h_o    <- w_n * mu_o * (2 * F_vec - 1 - G_pt) / mu_bar
      h      <- numeric(N)
      h[ord] <- h_o
      h
    },

    stop(sprintf("[gradient_for_method] unsupported method: '%s'", method))
  )
}


# ---------------------------------------------------------------------------- #
# Band transforms                                                              #
# ---------------------------------------------------------------------------- #
# Some aggregates are bounded (headcount in [0,1], gap in [0,1]) or strictly
# positive (mean welfare). Build bands on a transformed scale and invert so
# they respect natural bounds.

apply_band_transform <- function(method, value_pt, se, z_lo, z_hi) {
  if (!is.finite(se) || se == 0) {
    return(list(lo = value_pt, hi = value_pt))
  }

  use_logit <- method %in% c("headcount_ratio", "gap", "fgt2")
  use_log   <- method %in% c("median")

  if (use_logit && value_pt > 0 && value_pt < 1) {
    # SE on logit scale via delta: d logit / d p = 1 / (p(1-p))
    se_t <- se / (value_pt * (1 - value_pt))
    lt   <- log(value_pt / (1 - value_pt))
    lo   <- 1 / (1 + exp(-(lt + z_lo * se_t)))
    hi   <- 1 / (1 + exp(-(lt + z_hi * se_t)))
  } else if (use_log && value_pt > 0) {
    se_t <- se / value_pt
    lo   <- exp(log(value_pt) + z_lo * se_t)
    hi   <- exp(log(value_pt) + z_hi * se_t)
  } else {
    lo <- value_pt + z_lo * se
    hi <- value_pt + z_hi * se
  }

  list(lo = unname(lo), hi = unname(hi))
}
