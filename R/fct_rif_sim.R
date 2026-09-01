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
#'   \code{NULL} (default), uses \code{stats::bw.SJ()}. Ignored when
#'   \code{dens} is supplied.
#' @param dens Optional pre-built \code{stats::density()} object for
#'   \code{y[is.finite(y)]}. Bandwidth selection and the KDE depend only on
#'   \code{y}, not on \code{tau}, so callers that evaluate several taus for
#'   the same \code{y} (e.g. \code{prepare_outcome()}'s 9-tau RIF grid) can
#'   build this once and pass it in to avoid repeating \code{bw.SJ()}/
#'   \code{density()} per tau (see PERF-03). When \code{NULL} (default), it
#'   is built from \code{y}/\code{bw} exactly as before.
#'
#' @return Numeric vector of RIF values, same length as \code{y}.
#'
#' @export
compute_rif <- function(y, tau, bw = NULL, dens = NULL) {
  na_mask <- !is.finite(y)
  y_obs   <- y[!na_mask]
  q_tau   <- stats::quantile(y_obs, probs = tau, names = FALSE)

  # Robust bandwidth: SJ can fail on large/multimodal data

  if (is.null(dens)) {
    bw_use <- bw
    if (is.null(bw_use)) {
      bw_use <- tryCatch(stats::bw.SJ(y_obs), error = function(e) stats::bw.nrd0(y_obs))
    }

    # Standard KDE with interpolation at the quantile

    dens <- stats::density(y_obs, bw = bw_use, n = 1024)
  }
  f_q  <- stats::approx(dens$x, dens$y, xout = q_tau)$y

  # Scale-aware floor: fraction of peak density

  if (is.na(f_q) || f_q <= 0) {
    f_q <- max(dens$y) * 0.01
    warning(sprintf("Density near zero at quantile %.2f; using floor.", tau))
  }
  f_q <- max(f_q, max(dens$y) * 0.001)

  rif <- rep(NA_real_, length(y))
  rif[!na_mask] <- q_tau + (tau - as.numeric(y_obs <= q_tau)) / f_q
  rif
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
    fit_i <- fits_multi[[i]]
    # Try fit-time VCV first (respects cluster= passed at estimation)
    tbl <- tryCatch(broom::tidy(fit_i, conf.int = TRUE), error = function(e) NULL)
    if (is.null(tbl)) {
      for (spec in list(COEF_VCOV_SPEC, ~loc_id, "HC1", "iid")) {
        tbl <- tryCatch(
          broom::tidy(fit_i, conf.int = TRUE, vcov = spec),
          error = function(e) NULL
        )
        if (!is.null(tbl)) break
      }
    }
    if (is.null(tbl)) tbl <- broom::tidy(fit_i, conf.int = TRUE)
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
#'   use in \code{aggregate_with_uncertainty_delta()}.
#' @param ecdf_train Optional pre-built \code{stats::ecdf()} of
#'   \code{train_data[[outcome]]}. \code{train_data} is identical across
#'   simulation keys for a given model fit, so callers that invoke
#'   \code{predict_rif()} in a per-key loop (e.g. \code{run_sim_pipeline()})
#'   can build this once and pass it in to avoid rebuilding the same
#'   empirical CDF on every key. When \code{NULL} (default), it is built
#'   from \code{train_data} as before.
#'
#' @return \code{newdata} augmented with \code{.fitted}, \code{.residual}, and outcome.
#'   When \code{chol_list} is non-NULL, also carries \code{attr(., "F_loading")}.
#'
#' @export
predict_rif <- function(fit_multi, newdata, svy, train_data, taus, outcome,
                        weather_cols, so = NULL, chol_list = NULL,
                        ecdf_train = NULL) {
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

  # Assign quantile position via ecdf of training data (in model scale).
  # Reuse a caller-supplied ecdf when available (see PERF-27); otherwise
  # build it here as before.
  F_hat <- if (!is.null(ecdf_train)) ecdf_train else stats::ecdf(train_data[[outcome]])
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

  # For F_loading: the scenario design matrix X_scenario %*% L_k gives the
  # delta-method gradient of the *predicted welfare level* under the RIF
  # regression at quantile k, consistent with the OLS path's F_loading
  # construction. Downstream:
  #   - Level mode: ||F_loading||^2 = level-CI of the predicted welfare
  #     under the RIF model at this household's quantile position tau_i.
  #     Comparable in width to OLS level bands.
  #   - Deviation/contrast mode: the aggregation layer subtracts the
  #     historical reference F_agg, so F_agg_s - F_agg_h reduces (per
  #     household i) to (X_scenario_i - X_hist_i) %*% t(L_tau_i) — the
  #     paired-contrast variance, matching the X_diff construction we
  #     previously used. Deviation bands stay tight.
  #
  # The displayed point estimate remains y_observed + delta_i; the level
  # SE refers to the predicted level under the regression, not to that
  # observed-anchored displayed value (analogous to how OLS's level SE
  # refers to its predicted, not "true," welfare).
  #
  # Memory: rather than materialising all K full N×P scenario design matrices
  # up front (≈ K × N × p × 8 bytes — ~8 GB for a large-N country with K=9),
  # interpolate_F_loading() builds each quantile's design matrix on demand for
  # only the rows that need it (it processes rows grouped by their (lo, hi)
  # quantile pair). We hand it a lazy accessor that calls model.matrix() on a
  # row subset of newdata_scen. Bit-identical to the all-at-once form because
  # matrix multiply distributes over row subsetting.
  compute_loading <- !is.null(chol_list) && length(chol_list) == K

  for (k in seq_len(K)) {
    # Baseline weather prediction (needed for the climate-delta point
    # estimate; not used for F_loading any more).
    pred_base <- as.numeric(stats::predict(fit_multi[[k]], newdata = newdata_base,
                                           type = "response"))

    # Scenario weather prediction
    pred_new <- as.numeric(stats::predict(fit_multi[[k]], newdata = newdata_scen,
                                          type = "response"))

    delta_mat[, k] <- pred_new - pred_base
  }

  # Restore scenario weather in newdata (for output)
  for (wc in weather_cols) newdata[[wc]] <- saved_weather[[wc]]

  # Clean up pre-built frames. newdata_scen is retained until after F_loading
  # is built below, since the lazy design-matrix accessor reads from it.
  rm(newdata_base, saved_weather)

  # Interpolate delta at each household's tau_i position
  delta_i <- interpolate_delta(delta_mat, taus, tau_i)

  # Assemble output
  newdata$.fitted    <- y_baseline + delta_i
  newdata$.residual  <- NA_real_
  newdata[[outcome]] <- y_baseline + delta_i

  # Compute F_loading by interpolating X_scenario %*% L_k at each tau_i.
  # X_scen_fn(k, rows) builds the quantile-k scenario design matrix for the
  # given rows only — model.matrix() on a row subset of newdata_scen.
  if (compute_loading) {
    active_mask <- attr(chol_list, "active_mask")
    X_scen_fn <- function(k, rows) {
      stats::model.matrix(fit_multi[[k]], data = newdata_scen[rows, , drop = FALSE],
                          type = "rhs")
    }
    F_loading <- tryCatch({
      interpolate_F_loading(X_scen_fn, chol_list, taus, tau_i,
                             active_mask = active_mask)
    }, error = function(e) {
      warning("[predict_rif] F_loading interpolation failed: ", conditionMessage(e))
      NULL
    })
    if (!is.null(F_loading)) {
      attr(newdata, "F_loading") <- F_loading
      # Diagnostic: confirm the additive-decomposition mask reached predict_rif.
      # Compare ncol(F_loading) against the per-tau design width — if the mask
      # is active, ncol should be < length(active_mask).
      if (!is.null(active_mask) && ncol(F_loading) < length(active_mask)) {
        message(sprintf(
          "[predict_rif] additive-decomposition mask applied: F_loading is %d x %d (out of %d coefficients).",
          nrow(F_loading), ncol(F_loading), length(active_mask)))
      }
    }
  }

  rm(newdata_scen)

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
#' @param X_diff_fn  Function \code{(k, rows)} returning the quantile-k design
#'   matrix (\code{length(rows) x p}) for the requested rows only — built on
#'   demand so all K full N×p matrices never coexist in memory. (For backward
#'   compatibility, a list of K full \code{n x p} matrices is also accepted and
#'   wrapped automatically.)
#' @param chol_list  List of K Cholesky factor matrices (p x p), one per quantile.
#' @param taus       Numeric vector of length K (sorted quantile grid).
#' @param tau_i      Numeric vector of length n (household quantile positions).
#' @param active_mask Optional logical vector of length p. When supplied,
#'   each per-quantile loading is subset to the active columns before the
#'   interpolation blend (additive-decomposition SE). NULL = no masking.
#'
#' @return Numeric n x p matrix of interpolated factor loadings (or
#'   n x sum(active_mask) when masking is applied).
#'
#' @keywords internal
interpolate_F_loading <- function(X_diff_fn, chol_list, taus, tau_i,
                                   active_mask = NULL) {
  K <- length(taus)

  # Accept a prebuilt list of K matrices (legacy callers / tests) by wrapping
  # it in the same (k, rows) accessor the streaming path expects.
  if (is.list(X_diff_fn) && !is.function(X_diff_fn)) {
    X_diff_list <- X_diff_fn
    n <- nrow(X_diff_list[[1]])
    X_diff_fn <- function(k, rows) X_diff_list[[k]][rows, , drop = FALSE]
  } else {
    n <- length(tau_i)
  }

  # Find interval: taus[idx] <= tau_i < taus[idx+1]
  idx <- findInterval(tau_i, taus, all.inside = TRUE)
  idx_hi <- pmin(idx + 1L, K)

  # Interpolation weights
  tau_lo <- taus[idx]
  tau_hi <- taus[idx_hi]
  w      <- ifelse(tau_hi > tau_lo, (tau_i - tau_lo) / (tau_hi - tau_lo), 0)

  # F = X %*% L (lower triangular L with LL' = Sigma) so that
  # F F' = X L L' X' = X Sigma X' — the correct level variance.
  # Earlier versions used t(L) here, which gives X L' L X' (not Sigma)
  # and inflated SEs by ~25% on realistic Sigma. The linear path
  # (compute_factor_loading) was fixed for this; this matches.
  #
  # When active_mask is set, `chol_list[[k]]` is expected to be the
  # K_active x K_active Cholesky of the active block of Sigma (built by
  # attach_active_mask), and the design matrix is the full N x K design.
  # We subset X to active columns before multiplying.
  #
  # Memory: each output row i reads from exactly two quantile loadings,
  # at idx[i] and idx_hi[i]. Rather than materialising all K full N×P design
  # matrices (≈ K × N × p × 8 bytes), we process rows grouped by their
  # (lo, hi) quantile pair and build (via X_diff_fn) only that group's rows of
  # the at-most-two quantiles it needs. Matrix multiply distributes over row
  # subsetting exactly — (X[rows, ] %*% C) is row-for-row identical to
  # (X %*% C)[rows, ] — so the result is bit-equivalent to the all-at-once
  # form, at a fraction of the peak allocation (one N×P result + small
  # per-group scratch).

  # Subset a group's design matrix to active columns when masking is on.
  apply_mask <- function(Xk) {
    if (!is.null(active_mask) && length(active_mask) == ncol(Xk)) {
      Xk[, active_mask, drop = FALSE]
    } else {
      Xk
    }
  }
  p <- ncol(chol_list[[idx[1]]])

  F <- matrix(NA_real_, nrow = n, ncol = p)

  # Group rows by their (lo, hi) quantile-index pair. Each group needs at
  # most two per-quantile matmuls, restricted to the group's rows.
  pair_key <- idx + idx_hi * (K + 1L)  # unique per (lo, hi) combination
  for (key in unique(pair_key)) {
    rows <- which(pair_key == key)
    a <- idx[rows[1]]
    b <- idx_hi[rows[1]]
    w_g <- w[rows]

    F_a <- apply_mask(X_diff_fn(a, rows)) %*% chol_list[[a]]
    F_b <- if (b == a) F_a else apply_mask(X_diff_fn(b, rows)) %*% chol_list[[b]]

    F[rows, ] <- (1 - w_g) * F_a + w_g * F_b
  }

  F
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
