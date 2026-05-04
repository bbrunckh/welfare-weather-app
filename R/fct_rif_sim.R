# ============================================================================ #
# Pure functions for Unconditional Quantile Regression (RIF).                  #
#                                                                              #
# Implements the Recentered Influence Function approach of                     #
# Firpo, Fortin & Lemieux (2009) for estimating distributional impacts.        #
#                                                                              #
# Used by:                                                                     #
#   Module 1 — fct_fit_model.R (compute_rif, build_rif_grid)                   #
#   Module 2 — fct_simulations.R (predict_rif)                                 #
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# RIF computation                                                               #
# ---------------------------------------------------------------------------- #

#' Compute the Recentered Influence Function for a given quantile
#'
#' Transforms the outcome \code{y} into its RIF representation at quantile
#' \code{tau}, following Firpo, Fortin & Lemieux (2009). The resulting vector
#' can be used as a dependent variable in OLS to estimate unconditional
#' quantile partial effects.
#'
#' @param y   Numeric vector (the outcome).
#' @param tau Scalar in (0, 1) specifying the quantile.
#' @param bw  Optional bandwidth for kernel density estimation. When
#'   \code{NULL} (default), uses \code{stats::bw.SJ()}.
#'
#' @return Numeric vector of RIF values, same length as \code{y}.
#'
#' @export
compute_rif <- function(y, tau, bw = NULL) {
  y_obs <- y[!is.na(y)]
  q_tau <- stats::quantile(y_obs, probs = tau, names = FALSE)

  bw_use <- if (is.null(bw)) stats::bw.SJ(y_obs) else bw
  dens   <- stats::density(y_obs, bw = bw_use, n = 512)
  f_q    <- stats::approx(dens$x, dens$y, xout = q_tau)$y

  if (is.na(f_q) || f_q <= 0) {
    warning(sprintf("Density near zero at quantile %.2f; RIF may be unstable.", tau))
    f_q <- max(f_q, 1e-6)
  }

  q_tau + (tau - as.numeric(y <= q_tau)) / f_q
}


# ---------------------------------------------------------------------------- #
# Grid construction                                                             #
# ---------------------------------------------------------------------------- #

#' Build a tidy data frame of RIF regression coefficients ("beta curves")
#'
#' Extracts coefficients from a \code{fixest_multi} object (stacked feols
#' result) and arranges them into a long-format grid with one row per
#' (quantile x term) combination.
#'
#' @param fits_multi A \code{fixest_multi} object returned by
#'   \code{fixest::feols()} with a stacked LHS.
#' @param taus Numeric vector of quantile values corresponding to each
#'   element in \code{fits_multi}.
#' @param model_id Integer model identifier (1 = weather only, 2 = + FE,
#'   3 = + FE + controls).
#'
#' @return A data frame with columns: \code{tau}, \code{term},
#'   \code{estimate}, \code{std.error}, \code{conf.low}, \code{conf.high},
#'   \code{model}.
#'
#' @export
build_rif_grid <- function(fits_multi, taus, model_id) {
  purrr::map_dfr(seq_along(taus), function(i) {
    tbl       <- broom::tidy(fits_multi[[i]], conf.int = TRUE)
    tbl$tau   <- taus[i]
    tbl$model <- model_id
    tbl
  })
}


# ---------------------------------------------------------------------------- #
# RIF Simulation: Delta Method Prediction                                      #
# ---------------------------------------------------------------------------- #

#' Predict welfare outcomes using RIF delta method
#'
#' For each household, assigns a quantile position via ecdf, predicts
#' the change in RIF values between baseline and scenario weather at that
#' quantile, then adds the delta to the observed baseline welfare.
#'
#' @param fit_multi A \code{fixest_multi} object (9 sub-models, one per tau).
#' @param newdata Data frame from \code{prepare_hist_weather()} — has scenario
#'   weather columns and \code{.svy_row_id}.
#' @param svy The raw survey data passed to \code{prepare_hist_weather()}.
#' @param train_data Training data used for ecdf quantile assignment.
#' @param taus Numeric vector of quantiles (e.g. \code{seq(0.1, 0.9, 0.1)}).
#' @param outcome Character; name of the outcome column.
#' @param weather_cols Character vector of weather variable column names.
#' @param so Selected outcome metadata (list with \code{$transform}).
#'   When \code{so$transform == "log"}, the baseline outcome is log-transformed
#'   to match the scale used during model fitting.
#' @param chol_list Optional list of Cholesky factor matrices (one per tau),
#'   as returned by \code{compute_chol_vcov(fit_multi)}. When provided,
#'   an \code{F_loading} matrix is attached as an attribute of the result for
#'   use in \code{aggregate_with_uncertainty()}.
#'
#' @return \code{newdata} augmented with \code{.fitted}, \code{.residual}, and outcome.
#'   When \code{chol_list} is non-NULL, also carries \code{attr(., "F_loading")}.
#'
#' @export
predict_rif <- function(fit_multi, newdata, svy, train_data, taus, outcome,
                        weather_cols, so = NULL, chol_list = NULL) {
  stopifnot(
    ".svy_row_id must be present in newdata" = ".svy_row_id" %in% names(newdata),
    "taus must be non-empty" = length(taus) > 0,
    "fit_multi must have same length as taus" = length(fit_multi) == length(taus)
  )

  svy_row    <- newdata$.svy_row_id
  y_raw      <- svy[[outcome]][svy_row]
  n          <- nrow(newdata)
  K          <- length(taus)

  # Transform y_baseline to model scale (log if applicable)
  # train_data[[outcome]] is already in model scale (log-transformed by
  # prepare_outcome_df before fitting), so ecdf and predictions are in log scale.
  is_log     <- isTRUE(so$transform == "log")
  y_baseline <- if (is_log) log(y_raw) else y_raw

  # Assign quantile position via ecdf of training data (in model scale)
  F_hat <- stats::ecdf(train_data[[outcome]])
  tau_i <- pmin(pmax(F_hat(y_baseline), min(taus)), max(taus))

  # Swap weather columns: save scenario, insert baseline from svy
  saved_weather <- newdata[, weather_cols, drop = FALSE]

  # Pre-build baseline and scenario data frames ONCE (avoid K×2 column copies)
  newdata_base <- newdata
  for (wc in weather_cols) {
    newdata_base[[wc]] <- svy[[wc]][svy_row]
  }
  newdata_scen <- newdata  # already has scenario weather

  # Predict at each quantile for baseline and scenario weather
  # Store deltas in a matrix: rows = observations, cols = quantiles
  delta_mat <- matrix(NA_real_, nrow = n, ncol = K)

  # For F_loading: store X_diff per quantile (X_scenario - X_baseline)
  compute_loading <- !is.null(chol_list) && length(chol_list) == K
  X_diff_list    <- if (compute_loading) vector("list", K) else NULL

  for (k in seq_len(K)) {
    # Baseline weather prediction
    pred_base <- as.numeric(stats::predict(fit_multi[[k]], newdata = newdata_base,
                                           type = "response"))

    # Capture baseline design matrix for loading computation
    if (compute_loading) {
      X_base_k <- tryCatch(
        stats::model.matrix(fit_multi[[k]], data = newdata_base, type = "rhs"),
        error = function(e) NULL
      )
    }

    # Scenario weather prediction
    pred_new <- as.numeric(stats::predict(fit_multi[[k]], newdata = newdata_scen,
                                          type = "response"))

    # Capture scenario design matrix and compute X_diff
    if (compute_loading && !is.null(X_base_k)) {
      X_new_k <- tryCatch(
        stats::model.matrix(fit_multi[[k]], data = newdata_scen, type = "rhs"),
        error = function(e) NULL
      )
      if (!is.null(X_new_k) && nrow(X_new_k) == nrow(X_base_k)) {
        X_diff_list[[k]] <- X_new_k - X_base_k
      } else {
        compute_loading <- FALSE
      }
    }

    delta_mat[, k] <- pred_new - pred_base
  }

  # Restore scenario weather in newdata (for output)
  for (wc in weather_cols) newdata[[wc]] <- saved_weather[[wc]]

  # Clean up pre-built frames

  rm(newdata_base, newdata_scen, saved_weather)

  # Interpolate delta at each household's tau_i position
  delta_i <- interpolate_delta(delta_mat, taus, tau_i)

  # Diagnostic: log mean absolute delta for first call
  message(sprintf(
    "[predict_rif] n=%d | mean|delta|=%.4f | mean(y_base)=%.4f",
    n, mean(abs(delta_i), na.rm = TRUE), mean(y_baseline, na.rm = TRUE)
  ))

  # Assemble output
  newdata$.fitted    <- y_baseline + delta_i
  newdata$.residual  <- NA_real_
  newdata[[outcome]] <- y_baseline + delta_i

  # Compute F_loading by interpolating X_diff %*% t(L_k) at each tau_i
  if (compute_loading && !any(vapply(X_diff_list, is.null, logical(1)))) {
    F_loading <- tryCatch({
      interpolate_F_loading(X_diff_list, chol_list, taus, tau_i)
    }, error = function(e) {
      warning("[predict_rif] F_loading interpolation failed: ", conditionMessage(e))
      NULL
    })
    if (!is.null(F_loading)) attr(newdata, "F_loading") <- F_loading
  }

  newdata
}


#' Interpolate factor loadings at arbitrary quantile positions
#'
#' For each household i, linearly interpolates between the two adjacent
#' quantile factor-loading rows to produce a single N x P F_loading matrix.
#'
#' Each per-quantile loading is \eqn{F_k = X\_diff_k \%*\% t(L_k)}, where
#' \eqn{X\_diff_k = X_{scenario} - X_{baseline}} at quantile k.
#'
#' @param X_diff_list List of K matrices (n x p), one per quantile.
#' @param chol_list   List of K Cholesky factor matrices (p x p), one per quantile.
#' @param taus        Numeric vector of length K (sorted quantile grid).
#' @param tau_i       Numeric vector of length n (household quantile positions).
#'
#' @return Numeric n x p matrix of interpolated factor loadings.
#'
#' @keywords internal
interpolate_F_loading <- function(X_diff_list, chol_list, taus, tau_i) {
  n <- nrow(X_diff_list[[1]])
  K <- length(taus)

  # Find interval: taus[idx] <= tau_i < taus[idx+1]
  idx <- findInterval(tau_i, taus, all.inside = TRUE)
  idx_hi <- pmin(idx + 1L, K)

  # Interpolation weights
  tau_lo <- taus[idx]
  tau_hi <- taus[idx_hi]
  w      <- ifelse(tau_hi > tau_lo, (tau_i - tau_lo) / (tau_hi - tau_lo), 0)

  # Pre-compute F_k = X_diff_k %*% t(L_k) for the unique quantile indices needed
  needed_idx <- unique(c(idx, idx_hi))
  F_cache <- vector("list", K)
  for (k in needed_idx) {
    F_cache[[k]] <- X_diff_list[[k]] %*% t(chol_list[[k]])
  }

  # Vectorised blend: extract rows from F_cache matrices using batch indexing
  p <- ncol(F_cache[[needed_idx[1]]])

  # Build F_lo and F_hi matrices by row-selecting from cached results
  # Each row i of F_lo is F_cache[[idx[i]]][i, ]
  F_lo <- matrix(NA_real_, nrow = n, ncol = p)
  F_hi <- matrix(NA_real_, nrow = n, ncol = p)

  for (k in needed_idx) {
    rows_lo <- which(idx == k)
    if (length(rows_lo) > 0L) F_lo[rows_lo, ] <- F_cache[[k]][rows_lo, , drop = FALSE]
    rows_hi <- which(idx_hi == k)
    if (length(rows_hi) > 0L) F_hi[rows_hi, ] <- F_cache[[k]][rows_hi, , drop = FALSE]
  }

  # Vectorised linear interpolation: (1-w) * F_lo + w * F_hi
  (1 - w) * F_lo + w * F_hi
}


#' Interpolate delta values at arbitrary quantile positions
#'
#' @param delta_mat Matrix (n x K) of delta values at each quantile.
#' @param taus Numeric vector of length K (sorted quantile grid).
#' @param tau_i Numeric vector of length n (household quantile positions).
#'
#' @return Numeric vector of length n with interpolated deltas.
#'
#' @keywords internal
interpolate_delta <- function(delta_mat, taus, tau_i) {
  n <- length(tau_i)
  K <- length(taus)

  # Find interval: idx such that taus[idx] <= tau_i < taus[idx+1]
  idx <- findInterval(tau_i, taus, all.inside = TRUE)

  # Linear interpolation weights
  tau_lo <- taus[idx]
  tau_hi <- taus[pmin(idx + 1L, K)]
  w      <- ifelse(tau_hi > tau_lo, (tau_i - tau_lo) / (tau_hi - tau_lo), 0)

  # Interpolated delta
  delta_lo <- delta_mat[cbind(seq_len(n), idx)]
  delta_hi <- delta_mat[cbind(seq_len(n), pmin(idx + 1L, K))]

  delta_lo * (1 - w) + delta_hi * w
}
