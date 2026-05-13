# fct_aggregation_delta.R
# -----------------------
# Closed-form delta-method uncertainty aggregation.
#
# Closed-form variance for (value, var) tuples that feed into
# combine_ensemble_results().
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
                                              residuals    = "original",
                                              train_aug    = NULL,
                                              id_vec       = NULL,
                                              id_col       = NULL,
                                              is_log       = TRUE,
                                              band_q       = c(lo = 0.10, hi = 0.90),
                                              bandwidth_p0 = 0.05) {

  N <- length(y_point)
  stopifnot(is.numeric(y_point) && N > 0)

  # Demote to "none" when train_aug is unavailable (e.g. RIF pipelines set
  # train_aug = NULL by construction). Avoids forcing every caller to repeat
  # the safety check now that "original" is the default.
  if (!identical(residuals, "none") &&
      (is.null(train_aug) || !".resid" %in% names(train_aug))) {
    residuals <- "none"
  }

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
      # Partial derivative holding the poverty indicator fixed:
      #   dT/dmu_j = w_tilde_j * 1{poor_j} / B
      # (Numerator depends on mu_j only through w_j*1{poor}*mu_j; B does not
      # depend on mu_j because the indicator is held fixed.) Chain-lift to
      # log scale by multiplying by mu_j:
      #   h_j = w_tilde_j * 1{poor_j} * mu_j / B
      # The (mu_j - T) factor that appears in the empirical influence
      # function is *not* used here — same convention as Gini (see §3.9 in
      # method_uncertainty.md). IF is the right object for sample-variance
      # estimation of T; for delta-method propagation of parametric
      # beta-uncertainty through mu_i(beta), the partial derivative is the
      # correct functional gradient.
      poor <- as.numeric(mu < pov_line)
      B    <- sum(w_tilde * poor, na.rm = TRUE)
      if (!is.finite(B) || B <= 0) {
        rep(0, N)
      } else {
        (w_tilde / B) * poor * mu
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


# ---------------------------------------------------------------------------- #
# Shared per-year pipeline aggregator                                          #
# ---------------------------------------------------------------------------- #

#' Aggregate a single pipeline-like list across simulation years
#'
#' Thin wrapper around \code{aggregate_with_uncertainty_delta()} that captures
#' the repeated "subset by sim_year, build F_loading slice, call aggregator,
#' summarise" pattern used by both Step 2's results module and Step 3's
#' baseline/policy comparator. Keeping the logic in one place is what stops
#' the two callers from drifting apart when the aggregator interface evolves.
#'
#' @param pipe A pipeline-like named list with elements \code{y_point},
#'   \code{F_loading}, \code{sim_year}, optional \code{weight}, \code{id_vec},
#'   \code{id_col}, \code{train_aug}.
#' @param method Aggregation method (see \code{aggregate_with_uncertainty_delta}).
#' @param weighted Logical. Use \code{pipe$weight} when present.
#' @param pov_line Numeric. Poverty line.
#' @param residuals Character. Residual draw mode.
#' @param is_log Logical. Whether y_point is on log scale.
#' @param band_q Named numeric \code{c(lo=, hi=)}.
#' @param skip_coef Logical. When TRUE, drop \code{F_loading} (no coefficient
#'   uncertainty).
#' @param bandwidth_p0 Numeric. Kernel bandwidth for headcount methods.
#'
#' @return List with one entry per unique \code{sim_year}, each a result list
#'   from \code{aggregate_with_uncertainty_delta()} augmented with a
#'   \code{sim_year} scalar.
#' @export
aggregate_pipeline_per_year <- function(pipe,
                                        method,
                                        weighted    = TRUE,
                                        pov_line    = NULL,
                                        residuals   = "original",
                                        is_log      = TRUE,
                                        band_q      = c(lo = 0.10, hi = 0.90),
                                        skip_coef   = FALSE,
                                        bandwidth_p0 = 0.05) {
  if (is.null(pipe) || is.null(pipe$y_point)) return(list())

  yrs <- sort(unique(pipe$sim_year))
  F_full <- pipe$F_loading
  # Promote a length-K numeric to a 1xK matrix so row-subsetting below never
  # fails with "incorrect number of dimensions".
  if (!is.null(F_full) && is.null(dim(F_full))) {
    F_full <- matrix(F_full, nrow = 1L)
  }
  # train_aug carries .resid for "original"/"resample" residual paths. RIF
  # pipelines set train_aug = NULL by construction; honour that.
  res_mode <- residuals %||% "original"
  if (is.null(pipe$train_aug) && !identical(res_mode, "none"))
    res_mode <- "none"

  lapply(yrs, function(yr) {
    idx <- pipe$sim_year == yr
    F_idx <- if (!is.null(F_full) && !isTRUE(skip_coef))
               F_full[idx, , drop = FALSE] else NULL
    w_idx <- if (isTRUE(weighted) && !is.null(pipe$weight)) pipe$weight[idx] else NULL
    id_idx <- if (!is.null(pipe$id_vec)) pipe$id_vec[idx] else NULL
    m <- aggregate_with_uncertainty_delta(
      y_point      = pipe$y_point[idx],
      F_loading    = F_idx,
      method       = method,
      weights      = w_idx,
      pov_line     = pov_line,
      residuals    = res_mode,
      train_aug    = pipe$train_aug,
      id_vec       = id_idx,
      id_col       = pipe$id_col,
      is_log       = is_log,
      band_q       = band_q,
      bandwidth_p0 = bandwidth_p0
    )
    m$sim_year <- yr
    m
  })
}
