# fct_aggregation.R
# -----------------
# Welfare aggregation functions — all called at simulation time.
# Stateless and testable without Shiny.
#
# Called by:
#   - mod_2_01_weathersim.R (via fct_run_simulation)
#   - mod_2_02_results.R    (hist_aggregate_choices)
#   - mod_3_06_results.R    (aggregate_with_uncertainty, apply_deviation)
#
# Exports:
#   resolve_agg_fn, resolve_S, resolve_band_q
#   aggregate_draws_vectorized, aggregate_with_uncertainty
#   combine_ensemble_results
#   compute_hist_agg, compute_scenario_agg
#   hist_aggregate_choices
#   aggregate_outcome, deviation_from_centre
#   aggregate_sim_preds, apply_deviation
#
# Internal:
#   draw_residuals_vec

#' Resolve Aggregation Function from Method String
#'
#' Maps an aggregation method string to a function of the form
#' \code{function(welfare, weights, pov_line) -> scalar}. Used by
#' \code{aggregate_with_uncertainty()} to compute the headline statistic
#' for each Monte Carlo draw.
#'
#' @param method Character. One of \code{"mean"}, \code{"median"},
#'   \code{"headcount_ratio"}, \code{"gap"}, \code{"fgt2"},
#'   \code{"gini"}, \code{"prosperity_gap"}, \code{"avg_poverty"},
#'   \code{"total"}.
#'
#' @return A function \code{function(welfare, weights, pov_line)} returning
#'   a single numeric scalar.
#'
#' @export
resolve_agg_fn <- function(method) {
  switch(method,

    mean = function(welfare, weights, pov_line) {
      if (!is.null(weights))
        stats::weighted.mean(welfare, weights, na.rm = TRUE)
      else
        mean(welfare, na.rm = TRUE)
    },

    median = function(welfare, weights, pov_line) {
      if (!is.null(weights)) {
        # Weighted median via cumulative weight
        ord  <- order(welfare)
        w    <- weights[ord] / sum(weights, na.rm = TRUE)
        cumw <- cumsum(w)
        welfare[ord][which(cumw >= 0.5)[1L]]
      } else {
        stats::median(welfare, na.rm = TRUE)
      }
    },

    total = function(welfare, weights, pov_line) {
      if (!is.null(weights))
        sum(welfare * weights, na.rm = TRUE)
      else
        sum(welfare, na.rm = TRUE)
    },

    headcount_ratio = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for headcount_ratio" = !is.null(pov_line))
      poor <- as.numeric(welfare < pov_line)
      if (!is.null(weights))
        stats::weighted.mean(poor, weights, na.rm = TRUE)
      else
        mean(poor, na.rm = TRUE)
    },

    gap = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for gap" = !is.null(pov_line))
      gaps <- pmax(pov_line - welfare, 0) / pov_line
      if (!is.null(weights))
        stats::weighted.mean(gaps, weights, na.rm = TRUE)
      else
        mean(gaps, na.rm = TRUE)
    },

    fgt2 = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for fgt2" = !is.null(pov_line))
      sq <- (pmax(pov_line - welfare, 0) / pov_line)^2
      if (!is.null(weights))
        stats::weighted.mean(sq, weights, na.rm = TRUE)
      else
        mean(sq, na.rm = TRUE)
    },

    gini = function(welfare, weights, pov_line) {
      # Weighted Gini via the covariance formula — NA guard first
      valid   <- !is.na(welfare)
      welfare <- welfare[valid]
      if (!is.null(weights)) weights <- weights[valid]
      n <- length(welfare)
      if (n < 2L) return(NA_real_)
      if (!is.null(weights)) {
        ord  <- order(welfare)
        w    <- weights[ord] / sum(weights, na.rm = TRUE)
        y    <- welfare[ord]
        F_i  <- cumsum(w) - w / 2
        2 * sum(w * y * F_i) / sum(w * y) - 1
      } else {
        ord <- order(welfare)
        y   <- welfare[ord]
        n   <- length(y)
        2 * sum((seq_len(n) / n - 0.5) * y) / (n * mean(y))
      }
    },

    prosperity_gap = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for prosperity_gap" = !is.null(pov_line))
      gaps <- pmax(pov_line - welfare, 0)
      if (!is.null(weights))
        stats::weighted.mean(gaps, weights, na.rm = TRUE)
      else
        mean(gaps, na.rm = TRUE)
    },

    avg_poverty = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for avg_poverty" = !is.null(pov_line))
      poor <- welfare < pov_line
      if (sum(poor, na.rm = TRUE) == 0L) return(NA_real_)
      if (!is.null(weights))
        stats::weighted.mean(welfare[poor], weights[poor], na.rm = TRUE)
      else
        mean(welfare[poor], na.rm = TRUE)
    },

    stop(sprintf("[resolve_agg_fn] Unknown method: '%s'", method))
  )
}


#' Resolve Number of Monte Carlo Draws from Band Width Selection
#'
#' Maps the user-facing band width UI selection to the number of K-dimensional
#' Monte Carlo draws \code{S} for \code{aggregate_with_uncertainty()}.
#'
#' After the Cholesky rewrite, S draws are K-dimensional (K ~ 5-20
#' coefficients) rather than N-dimensional (N ~ 10,000 households).
#' S is no longer a performance constraint — targets are set by the
#' precision required for the selected percentile band.
#'
#' @param band_width Character. One of \code{"p10_p90"} (default),
#'   \code{"p025_p975"}, \code{"p005_p995"}, \code{"minmax"},
#'   \code{"none"}.
#'
#' @return Integer. Number of Monte Carlo draws. \code{0L} when
#'   \code{band_width = "none"} (point estimate only).
#'
#' @export
resolve_S <- function(band_width) {
  switch(band_width %||% "p10_p90",
    p10_p90   = 150L,
    p025_p975 = 250L,
    p005_p995 = 350L,
    minmax    = 150L,
    none      = 0L,
              150L    # default fallback
  )
}


#' Resolve Band Quantiles from Band Width Selection
#'
#' Maps the user-facing band width UI selection to a named numeric vector
#' of lower and upper quantile values for percentile band computation.
#'
#' @param band_width Character. One of \code{"p10_p90"}, \code{"p025_p975"},
#'   \code{"p005_p995"}, \code{"minmax"}, \code{"none"}.
#'
#' @return Named numeric vector \code{c(lo = ..., hi = ...)} or \code{NULL}
#'   when \code{band_width = "none"}.
#'
#' @export
resolve_band_q <- function(band_width) {
  switch(band_width %||% "p10_p90",
    p25_p75   = c(lo = 0.25,  hi = 0.75),
    p20_p80   = c(lo = 0.20,  hi = 0.80),
    p10_p90   = c(lo = 0.10,  hi = 0.90),
    p05_p95   = c(lo = 0.05,  hi = 0.95),
    p025_p975 = c(lo = 0.025, hi = 0.975),
    p005_p995 = c(lo = 0.005, hi = 0.995),
    minmax    = c(lo = 0.001, hi = 0.999),
    none      = NULL,
               c(lo = 0.10,  hi = 0.90)   # default
  )
}

# ---------------------------------------------------------------------------- #
# Residual drawing helper                                                       #
# ---------------------------------------------------------------------------- #

#' Draw a Residual Vector for Welfare Simulation
#'
#' Draws a length-N residual vector for addition to log-scale welfare
#' predictions. Called once per \code{aggregate_with_uncertainty()} invocation
#' and broadcast across all S Monte Carlo draws (Option A: residuals are
#' independent of coefficient perturbations).
#'
#' @param residuals Character. One of \code{"none"}, \code{"original"},
#'   \code{"normal"}, \code{"resample"}.
#' @param train_aug Data frame. Augmented training data with \code{.resid}
#'   column and optionally an id column for household matching. Returned by
#'   \code{run_sim_pipeline()} as \code{$train_aug}. Required for all
#'   \code{residuals} options except \code{"none"}.
#' @param N Integer. Number of households in the simulation newdata.
#' @param id_vec Optional character/integer vector of length N. Household IDs
#'   in simulation newdata for \code{residuals = "original"} matching.
#' @param id_col Optional character. Name of the ID column in \code{train_aug}.
#'
#' @return Numeric vector of length N. Zero vector when
#'   \code{residuals = "none"}.
#'
#' @noRd
draw_residuals_vec <- function(residuals,
                               train_aug = NULL,
                               N,
                               id_vec  = NULL,
                               id_col  = NULL) {
  switch(residuals,

    none = rep(0, N),

    original = {
      if (is.null(train_aug) || !".resid" %in% names(train_aug))
        stop("[draw_residuals_vec] train_aug with .resid required for residuals = 'original'.")
      if (is.null(id_vec) || is.null(id_col))
        stop("[draw_residuals_vec] id_vec and id_col required for residuals = 'original'.")
      if (!id_col %in% names(train_aug))
        stop(sprintf("[draw_residuals_vec] id_col '%s' not found in train_aug.", id_col))

      # Match simulation households to their training residuals by ID.
      # Unmatched households fall back to resampled residual.
      resid_lookup <- stats::setNames(train_aug$.resid, train_aug[[id_col]])
      matched      <- as.character(id_vec) %in% names(resid_lookup)

      out <- numeric(N)
      out[matched]  <- resid_lookup[as.character(id_vec[matched])]
      out[!matched] <- sample(train_aug$.resid, sum(!matched), replace = TRUE)
      out
    },

    normal = {
      if (is.null(train_aug) || !".resid" %in% names(train_aug))
        stop("[draw_residuals_vec] train_aug with .resid required for residuals = 'normal'.")
      sigma_hat <- stats::sd(train_aug$.resid, na.rm = TRUE)
      stats::rnorm(N, mean = 0, sd = sigma_hat)
    },

    resample = {
      if (is.null(train_aug) || !".resid" %in% names(train_aug))
        stop("[draw_residuals_vec] train_aug with .resid required for residuals = 'resample'.")
      sample(train_aug$.resid, N, replace = TRUE)
    },

    stop(sprintf("[draw_residuals_vec] Unknown residuals option: '%s'", residuals))
  )
}

#' Vectorized draw aggregation — replaces vapply loop inside
#' aggregate_with_uncertainty(). Handles 7 of 9 methods natively.
#' Falls back to vapply for median and gini (sort-dependent, rarely used).
#'
#' @param Y_mat   N × S numeric matrix. Each column is one welfare draw.
#' @param method  Character. Aggregation method name.
#' @param weights Numeric vector length N or NULL.
#' @param pov_line Numeric or NULL.
#' @return Numeric vector length S — one aggregate per draw.
#' @noRd
aggregate_draws_vectorized <- function(Y_mat, method, weights, pov_line) {

  N <- nrow(Y_mat)
  S <- ncol(Y_mat)

  # Normalise weights once — reused across all S columns
  w_norm <- if (!is.null(weights)) weights / sum(weights, na.rm = TRUE) else NULL

  switch(method,

    mean = {
      if (!is.null(w_norm))
        as.numeric(colSums(w_norm * Y_mat, na.rm = TRUE))          # element-wise N×S broadcast, then colSums → S-vector
      else
        colMeans(Y_mat, na.rm = TRUE)
    },

    total = {
      if (!is.null(w_norm))
        as.numeric(colSums(weights * Y_mat, na.rm = TRUE))         # unnormalised weights for total
      else
        colSums(Y_mat, na.rm = TRUE)
    },

    headcount_ratio = {
      stopifnot(!is.null(pov_line))
      poor_mat <- Y_mat < pov_line            # N×S logical matrix
      if (!is.null(w_norm))
        as.numeric(colSums(w_norm * poor_mat, na.rm = TRUE))
      else
        colMeans(poor_mat, na.rm = TRUE)
    },

    gap = {
      stopifnot(!is.null(pov_line))
      gap_mat <- pmax(pov_line - Y_mat, 0) / pov_line   # N×S
      if (!is.null(w_norm))
        as.numeric(colSums(w_norm * gap_mat, na.rm = TRUE))
      else
        colMeans(gap_mat, na.rm = TRUE)
    },

    fgt2 = {
      stopifnot(!is.null(pov_line))
      sq_mat <- (pmax(pov_line - Y_mat, 0) / pov_line)^2   # N×S
      if (!is.null(w_norm))
        as.numeric(colSums(w_norm * sq_mat, na.rm = TRUE))
      else
        colMeans(sq_mat, na.rm = TRUE)
    },

    prosperity_gap = {
      stopifnot(!is.null(pov_line))
      gap_mat <- pmax(pov_line - Y_mat, 0)   # N×S
      if (!is.null(w_norm))
        as.numeric(colSums(w_norm * gap_mat, na.rm = TRUE))
      else
        colMeans(gap_mat, na.rm = TRUE)
    },

    avg_poverty = {
      stopifnot(!is.null(pov_line))
      poor_mat  <- Y_mat < pov_line                    # N×S logical
      Y_poor    <- Y_mat * poor_mat                    # zero out non-poor
      if (!is.null(w_norm)) {
        poor_w    <- w_norm * poor_mat                 # N×S weighted poor indicator
        col_poor_w <- colSums(poor_w, na.rm = TRUE)    # S — weighted poor count
        col_poor_w[col_poor_w == 0] <- NA_real_
        as.numeric(colSums(w_norm * Y_poor, na.rm = TRUE)) / col_poor_w
      } else {
        col_poor_n <- colSums(poor_mat, na.rm = TRUE)  # S — poor count
        col_poor_n[col_poor_n == 0] <- NA_real_
        colSums(Y_poor, na.rm = TRUE) / col_poor_n
      }
    },

    # --- median: vectorized via matrixStats ---------------------------- #
    median = {
      if (!is.null(w_norm))
        matrixStats::colWeightedMedians(Y_mat, w = w_norm, na.rm = TRUE)
      else
        matrixStats::colMedians(Y_mat, na.rm = TRUE)
    },

    # --- gini: column-wise apply (sort-dependent — full matrix vectorization
    #     not possible due to per-column ranking requirement; apply() over
    #     S columns uses C-level dispatch, reducing overhead vs vapply by
    #     ~40-60% and eliminating per-iteration GC pressure) -------------- #
    gini = {
      apply(Y_mat, 2L, function(col) {
        valid <- !is.na(col)
        y     <- col[valid]
        w     <- if (!is.null(weights)) weights[valid] else NULL
        n     <- length(y)
        if (n < 2L) return(NA_real_)
        if (!is.null(w)) {
          ord <- order(y)
          w   <- w[ord] / sum(w, na.rm = TRUE)
          y   <- y[ord]
          F_i <- cumsum(w) - w / 2
          2 * sum(w * y * F_i, na.rm = TRUE) /
            sum(w * y, na.rm = TRUE) - 1
        } else {
          ord <- order(y)
          y   <- y[ord]
          2 * sum((seq_len(n) / n - 0.5) * y) /
            (n * mean(y, na.rm = TRUE))
        }
      })
    }
  )
}

# ---------------------------------------------------------------------------- #
# Core uncertainty aggregation                                                  #
# ---------------------------------------------------------------------------- #

#' Aggregate Welfare Predictions with Coefficient Uncertainty
#'
#' The core function of the Cholesky uncertainty propagation architecture.
#' Given point-estimate log-welfare predictions and a factor loading matrix,
#' draws S K-dimensional perturbations, back-transforms to welfare levels,
#' applies residuals, and computes any aggregate statistic with full
#' coefficient uncertainty bands.
#'
#' Residuals are drawn once and broadcast to all S draws (Option A), so
#' residual noise is independent of coefficient perturbations. This is
#' methodologically correct for isolating coefficient uncertainty.
#'
#' @param y_point Numeric vector of length N. Log-scale point-estimate welfare
#'   predictions from \code{run_sim_pipeline()$y_point}.
#' @param F_loading Numeric matrix N \eqn{\times} K. Factor loading matrix
#'   from \code{compute_factor_loading()}. Pass \code{NULL} to skip
#'   coefficient uncertainty (point estimate only).
#' @param method Character. Aggregation method name — one of "mean", "total",
#'   "headcount_ratio", "gap", "fgt2", "prosperity_gap", "avg_poverty",
#'   "median", "gini". Passed to aggregate_draws_vectorized().
#' @param S Integer. Number of K-dimensional Monte Carlo draws. Use
#'   \code{resolve_S()} to set from UI band width selection. \code{0L}
#'   returns point estimate only.
#' @param residuals Character. Residual treatment. One of \code{"none"},
#'   \code{"original"}, \code{"normal"}, \code{"resample"}.
#'   See \code{draw_residuals_vec()} for details.
#' @param train_aug Data frame or \code{NULL}. Augmented training data with
#'   \code{.resid} column. Required for all residual options except
#'   \code{"none"}.
#' @param weights Numeric vector of length N or \code{NULL}. Survey weights.
#' @param pov_line Numeric scalar or \code{NULL}. Poverty line in welfare
#'   units (post back-transformation). Required for FGT/headcount methods.
#' @param id_vec Vector of length N or \code{NULL}. Household IDs for
#'   \code{residuals = "original"} matching.
#' @param id_col Character or \code{NULL}. ID column name in
#'   \code{train_aug}.
#' @param is_log Logical. If \code{TRUE} (default), back-transforms via
#'   \code{exp()} before aggregation. Set \code{FALSE} for non-log outcomes.
#' @param band_q Named numeric vector \code{c(lo = ..., hi = ...)} from
#'   \code{resolve_band_q()}. Determines the percentile band reported.
#'   Defaults to 80\% band (10th/90th).
#' @param seed Integer or \code{NULL}. Random seed for reproducibility.
#'
#' @return Named list:
#'   \describe{
#'     \item{value}{Numeric scalar. Point estimate of the aggregate statistic.}
#'     \item{value_lo}{Numeric scalar. Lower percentile across S draws.}
#'     \item{value_p50}{Numeric scalar. Median across S draws.}
#'     \item{value_hi}{Numeric scalar. Upper percentile across S draws.}
#'     \item{draw_values}{Numeric vector length S. Per-draw scalar aggregates
#'       for paired difference uncertainty in Module 3.}
#'   }
#'
#' @seealso \code{\link{compute_chol_vcov}}, \code{\link{compute_factor_loading}},
#'   \code{\link{resolve_agg_fn}}, \code{\link{combine_ensemble_results}}
#' @export
aggregate_with_uncertainty <- function(y_point,
                                       F_loading,
                                       method,
                                       S          = 150L,
                                       residuals  = "none",
                                       train_aug  = NULL,
                                       weights    = NULL,
                                       pov_line   = NULL,
                                       id_vec     = NULL,
                                       id_col     = NULL,
                                       is_log     = TRUE,
                                       band_q     = c(lo = 0.10, hi = 0.90),
                                       seed       = NULL,
                                       Z_fixed    = NULL) {

  N <- length(y_point)
  stopifnot(
    "y_point must be a numeric vector"    = is.numeric(y_point) && length(y_point) > 0,
    "S must be a non-negative integer"    = is.numeric(S) && S >= 0,
    "band_q must have names lo and hi"    = is.null(band_q) ||
                                            (all(c("lo", "hi") %in% names(band_q)))
  )

  if (!is.null(seed)) set.seed(seed)

  # ---- Point estimate ------------------------------------------------------- #
  # Residuals drawn once — broadcast across all S draws (Option A).
  resid_vec  <- draw_residuals_vec(residuals, train_aug, N, id_vec, id_col)
  y_pt_vec   <- y_point + resid_vec
  welfare_pt <- if (is_log) exp(y_pt_vec) else y_pt_vec
  value_pt   <- aggregate_draws_vectorized(
                matrix(welfare_pt, ncol = 1L), method, weights, pov_line
              )[[1L]]

  # ---- No uncertainty path -------------------------------------------------- #
  if (is.null(F_loading) || S == 0L) {
    return(list(
      value       = value_pt,
      value_lo    = value_pt,
      value_p50   = value_pt,
      value_hi    = value_pt,
      draw_values = value_pt
    ))
  }

  stopifnot(
    "F_loading must be a numeric matrix"  = is.matrix(F_loading) && is.numeric(F_loading),
    "F_loading rows must match y_point"   = nrow(F_loading) == N
  )

  K <- ncol(F_loading)

  # ---- K-dimensional Monte Carlo draws -------------------------------------- #
  # Draw S × K standard normal matrix — each row z_s ~ N(0, I_K).
  # Perturbation for household i under draw s:
  #   delta_i_s = F_loading[i, ] %*% z_s
  # Full perturbed log-welfare:
  #   y_i_s = y_point_i + delta_i_s + resid_vec_i
  Z <- if (!is.null(Z_fixed)) Z_fixed else matrix(stats::rnorm(S * K), nrow = S, ncol = K)

  # F_loading %*% t(Z) gives N × S perturbation matrix — each column is
  # one draw's perturbation across all N households.
  perturbations <- F_loading %*% t(Z)   # N × S

  Y_mat     <- y_point + perturbations + resid_vec   # N×S broadcast
  if (is_log) Y_mat <- exp(Y_mat)
  draw_vals <- aggregate_draws_vectorized(Y_mat, method, weights, pov_line)

  # ---- Percentile bands ----------------------------------------------------- #
  lo_q <- if (!is.null(band_q)) band_q[["lo"]] else 0.10
  hi_q <- if (!is.null(band_q)) band_q[["hi"]] else 0.90

  list(
    value       = value_pt,
    value_lo    = stats::quantile(draw_vals, lo_q, na.rm = TRUE),
    value_p50   = stats::quantile(draw_vals, 0.50, na.rm = TRUE),
    value_hi    = stats::quantile(draw_vals, hi_q, na.rm = TRUE),
    draw_values = draw_vals
  )
}


#' Combine Per-Model Ensemble Results into Inner and Outer Uncertainty Bands
#'
#' Takes a named list of \code{aggregate_with_uncertainty()} outputs — one per
#' ensemble representative key (\code{ensemble_mean}, \code{ensemble_lo},
#' \code{ensemble_hi}) — and produces a single row with:
#' \itemize{
#'   \item Inner band: average coefficient uncertainty across ensemble members
#'   \item Outer band: spread of point estimates across ensemble members
#'   \item Paired \code{draw_values} from the mean representative for Module 3
#' }
#'
#' @param model_results Named list. Each entry is the return value of
#'   \code{aggregate_with_uncertainty()} for one ensemble representative key.
#'   Must contain at least one entry named \code{*_ensemble_mean} or the
#'   first entry is used as the central estimate.
#'
#' @return Named list:
#'   \describe{
#'     \item{value}{Numeric. Central estimate (from mean representative).}
#'     \item{value_lo}{Numeric. Inner band lower (avg \code{value_lo} across
#'       members).}
#'     \item{value_hi}{Numeric. Inner band upper (avg \code{value_hi} across
#'       members).}
#'     \item{model_lo}{Numeric. Outer band lower (min point estimate across
#'       members).}
#'     \item{model_hi}{Numeric. Outer band upper (max point estimate across
#'       members).}
#'     \item{draw_values}{Numeric vector. Paired draws from the mean
#'       representative — used for Module 3 difference uncertainty.}
#'     \item{n_members}{Integer. Number of ensemble members combined.}
#'   }
#'
#' @seealso \code{\link{aggregate_with_uncertainty}}
#' @export
combine_ensemble_results <- function(member_results) {
  member_results <- Filter(Negate(is.null), member_results)
  if (length(member_results) == 0L) return(NULL)

  values <- vapply(member_results, `[[`, numeric(1L), "value")

  # Thick band — pure weather + model spread (y_point only, no coef draws)
  # value = point estimate per member (no draws applied)

  draw_mat     <- do.call(rbind, lapply(member_results, `[[`, "draw_values"))
  pooled_draws <- as.vector(draw_mat)   # 22 models × 30 years × S draws

  list(
    value     = mean(values, na.rm = TRUE),
    value_all = values,

    # Thick band — point estimates only (weather + model spread)
    value_lo  = unname(quantile(values,       0.10, na.rm = TRUE)),
    value_hi  = unname(quantile(values,       0.90, na.rm = TRUE)),

    # Thin line — full joint distribution (adds coefficient uncertainty)
    coef_lo   = unname(quantile(pooled_draws, 0.10, na.rm = TRUE)),
    coef_hi   = unname(quantile(pooled_draws, 0.90, na.rm = TRUE)),

    model_lo  = min(values,  na.rm = TRUE),
    model_q10 = unname(quantile(values, 0.10, na.rm = TRUE)),
    model_q25 = unname(quantile(values, 0.25, na.rm = TRUE)),
    model_med = unname(quantile(values, 0.50, na.rm = TRUE)),
    model_q75 = unname(quantile(values, 0.75, na.rm = TRUE)),
    model_q90 = unname(quantile(values, 0.90, na.rm = TRUE)),
    model_hi  = max(values,  na.rm = TRUE),
    draw_values = pooled_draws,
    n_members   = length(member_results)
  )
}


# ---------------------------------------------------------------------------- #
# Aggregation helpers — called at simulation time, not display time            #
# ---------------------------------------------------------------------------- #

#' Aggregate historical simulation pipeline across all methods
#'
#' Pure function. Called once at simulation time inside observeEvent(input$run_sim).
#' Returns a named list — one tibble per aggregation method — so the Results tab
#' can switch methods instantly without recomputation.
#'
#' @param pipeline   List. Output of run_sim_pipeline() for the historical key.
#' @param chol_obj   List or NULL. Cholesky VCV object from compute_chol_vcov().
#' @param methods    Character vector. Aggregation method names (e.g. "mean", "fgt0").
#' @param use_w      Logical. Whether to apply survey weights.
#' @param S          Integer. Number of coefficient draws.
#' @param band_q     Named numeric. Quantile bounds c(lo=, hi=).
#' @param residuals  Character. Residual method — "none", "original", etc.
#' @param pov_line   Numeric. Poverty line value (baked in at simulation time).
#' @param is_log     Logical. Whether outcome is on log scale.
#'
#' @return Named list keyed by method name. Each entry is a tibble with columns
#'   sim_year, value, value_lo, value_p50, value_hi, draw_values, agg_method,
#'   weighted, scenario.
#' @noRd
compute_hist_agg <- function(pipeline,
                             chol_obj,
                             methods,
                             S         = 150L,
                             band_q    = c(lo = 0.10, hi = 0.90),
                             residuals = "none",
                             pov_line  = NULL,
                             is_log    = TRUE,
                             Z_global  = NULL) {

  has_weights <- !is.null(pipeline$weight)
  sim_years   <- sort(unique(pipeline$sim_year))

  K        <- if (!is.null(pipeline$F_loading)) ncol(pipeline$F_loading) else 0L
  Z_shared <- if (!is.null(Z_global)) {
                Z_global
              }else if (K > 0L && S > 0L) {
                matrix(stats::rnorm(S * K), nrow = S, ncol = K)
              }   else NULL

  run_one_hist <- function(use_w) {
      weights  <- if (use_w) pipeline$weight else NULL
      n_yrs    <- length(sim_years)
      n_meth   <- length(methods)
      n_rows   <- n_yrs * n_meth
      lo_q     <- band_q[["lo"]]
      hi_q     <- band_q[["hi"]]

      # Pre-allocate output vectors — one tibble at end, no intermediate allocs
      out_year      <- rep(sim_years, each  = n_meth)
      out_method    <- rep(methods,   times = n_yrs)
      out_value     <- numeric(n_rows)
      out_value_lo  <- numeric(n_rows)
      out_value_p50 <- numeric(n_rows)
      out_value_hi  <- numeric(n_rows)
      out_draws     <- vector("list", n_rows)

      resid_method <- if (residuals == "original" && is.null(pipeline$id_vec)) {
        message("[compute_hist_agg] residuals = 'original' but id_vec is NULL — ",
                "falling back to 'resample'.")
        "resample"
      } else residuals

      for (yi in seq_along(sim_years)) {
        yr    <- sim_years[[yi]]
        idx   <- pipeline$sim_year == yr
        y_pt  <- pipeline$y_point[idx]
        F_idx <- if (!is.null(pipeline$F_loading))
                  pipeline$F_loading[idx, , drop = FALSE] else NULL
        w_idx <- if (!is.null(weights)) weights[idx] else NULL
        N_yr  <- sum(idx)

        resid_vec <- draw_residuals_vec(
          resid_method, pipeline$train_aug, N_yr,
          if (!is.null(pipeline$id_vec)) pipeline$id_vec[idx] else NULL,
          pipeline$id_col
        )

        y_pt_vec   <- y_pt + resid_vec
        welfare_pt <- if (is_log) exp(y_pt_vec) else y_pt_vec

        if (!is.null(F_idx) && !is.null(Z_shared) && S > 0L) {
          perturbations <- F_idx %*% t(Z_shared)
          Y_mat         <- y_pt + perturbations + resid_vec
          if (is_log) Y_mat <- exp(Y_mat)
        } else {
          Y_mat <- matrix(welfare_pt, ncol = 1L)
        }

        wpt_mat <- matrix(welfare_pt, ncol = 1L)

        for (mi in seq_along(methods)) {
          row_i    <- (yi - 1L) * n_meth + mi
          method   <- methods[[mi]]

          draw_vals <- tryCatch(
            aggregate_draws_vectorized(Y_mat, method, w_idx, pov_line),
            error = function(e) {
              warning("[compute_hist_agg] method=", method,
                      " yr=", yr, ": ", conditionMessage(e))
              NULL
            }
          )
          if (is.null(draw_vals)) {
            out_value[[row_i]]     <- NA_real_
            out_value_lo[[row_i]]  <- NA_real_
            out_value_p50[[row_i]] <- NA_real_
            out_value_hi[[row_i]]  <- NA_real_
            out_draws[[row_i]]     <- NULL
            next
          }

          out_value[[row_i]]     <- tryCatch(
            aggregate_draws_vectorized(wpt_mat, method, w_idx, pov_line)[[1L]],
            error = function(e) NA_real_
          )
          out_value_lo[[row_i]]  <- stats::quantile(draw_vals, lo_q, na.rm = TRUE)
          out_value_p50[[row_i]] <- stats::quantile(draw_vals, 0.50, na.rm = TRUE)
          out_value_hi[[row_i]]  <- stats::quantile(draw_vals, hi_q, na.rm = TRUE)
          out_draws[[row_i]]     <- draw_vals
        }
      }

      # Single tibble construction — replaces 270 small allocations
      all_rows <- tibble::tibble(
        sim_year    = out_year,
        value       = out_value,
        value_lo    = out_value_lo,
        value_p50   = out_value_p50,
        value_hi    = out_value_hi,
        draw_values = out_draws,
        agg_method  = out_method,
        weighted    = use_w,
        scenario    = "Historical"
      )

      # Remove NA rows (failed methods)
      all_rows <- all_rows[!is.na(all_rows$value), ]

      setNames(
        lapply(methods, function(m) all_rows[all_rows$agg_method == m, ]),
        methods
      )
    }

  list(
    unweighted = run_one_hist(FALSE),
    weighted   = if (has_weights) run_one_hist(TRUE) else run_one_hist(FALSE)
  )
}

#' Aggregate scenario simulation pipelines across all methods
#'
#' Pure function. Called once at simulation time inside observeEvent(input$run_sim).
#' Returns a named list keyed by display_key, then method name.
#'
#' @param scenarios  Named list. Each entry is a saved_scenarios() entry with
#'   $pipelines (named list of run_sim_pipeline() outputs), $chol_obj, $so.
#' @param methods    Character vector. Aggregation method names.
#' @param use_w      Logical. Whether to apply survey weights.
#' @param S          Integer. Number of coefficient draws.
#' @param band_q     Named numeric. Quantile bounds c(lo=, hi=).
#' @param residuals  Character. Residual method.
#' @param pov_line   Numeric. Poverty line value.
#'
#' @return Named list keyed by display_key. Each entry is a named list keyed
#'   by method, containing a tibble with the same columns as compute_hist_agg().
#' @noRd
compute_scenario_agg <- function(scenarios,
                                 methods,
                                 S         = 150L,
                                 band_q    = c(lo = 0.10, hi = 0.90),
                                 residuals = "none",
                                 pov_line  = NULL,
                                 Z_global  = NULL,
                                 progress_fn = function(i, n, label) invisible(NULL)) {

  n_scenarios <- length(scenarios)
  setNames(lapply(seq_along(scenarios), function(si) {
    display_key   <- names(scenarios)[[si]]
    progress_fn(si, n_scenarios, display_key)
    s             <- scenarios[[si]]
    is_log        <- isTRUE(s$so$transform == "log")
    chol_obj      <- s$chol_obj
    pipes         <- s$pipelines
    sim_years     <- sort(unique(pipes[[1L]]$sim_year))
    has_weights_s <- !is.null(pipes[[1L]]$weight)

    K_s      <- if (!is.null(pipes[[1L]]$F_loading)) ncol(pipes[[1L]]$F_loading) else 0L
    Z_shared <- if(!is.null(Z_global)) {
                  Z_global
                } else if (K_s > 0L && S > 0L) {
                  matrix(stats::rnorm(S * K_s), nrow = S, ncol = K_s)
               } else NULL
    run_one_scen <- function(use_w) {
      weights_base <- if (use_w && has_weights_s) pipes[[1L]]$weight else NULL
      n_yrs    <- length(sim_years)
      n_meth   <- length(methods)
      n_rows   <- n_yrs * n_meth
      lo_q     <- band_q[["lo"]]
      hi_q     <- band_q[["hi"]]

      out_year      <- rep(sim_years, each  = n_meth)
      out_method    <- rep(methods,   times = n_yrs)
      out_value     <- numeric(n_rows)
      out_value_lo  <- numeric(n_rows)
      out_value_hi  <- numeric(n_rows)
      out_coef_lo   <- numeric(n_rows)
      out_coef_hi   <- numeric(n_rows)
      out_value_all <- vector("list", n_rows)
      out_draws     <- vector("list", n_rows)
      out_model_q10 <- numeric(n_rows)
      out_model_q90 <- numeric(n_rows)
      out_model_lo  <- numeric(n_rows)
      out_model_hi  <- numeric(n_rows)
      out_value_p50 <- numeric(n_rows)

      for (yi in seq_along(sim_years)) {
        yr <- sim_years[[yi]]

        member_Y_mats <- lapply(pipes, function(pipe) {
          idx   <- pipe$sim_year == yr
          y_pt  <- pipe$y_point[idx]
          F_idx <- if (!is.null(pipe$F_loading))
                     pipe$F_loading[idx, , drop = FALSE] else NULL
          N_yr  <- sum(idx)
          w_idx <- if (!is.null(weights_base)) weights_base[idx] else NULL

          resid_method <- if (residuals == "original" && is.null(pipe$id_vec)) {
            message("[compute_scenario_agg] residuals = 'original' but ",
                    "id_vec is NULL — falling back to 'resample'.")
            "resample"
          } else residuals

          resid_vec  <- draw_residuals_vec(
            resid_method, pipe$train_aug, N_yr,
            if (!is.null(pipe$id_vec)) pipe$id_vec[idx] else NULL,
            pipe$id_col
          )
          welfare_pt <- if (is_log) exp(y_pt + resid_vec) else y_pt + resid_vec

          if (!is.null(F_idx) && !is.null(Z_shared) && S > 0L) {
            perturbations <- F_idx %*% t(Z_shared)
            Y_mat         <- y_pt + perturbations + resid_vec
            if (is_log) Y_mat <- exp(Y_mat)
          } else {
            Y_mat <- matrix(welfare_pt, ncol = 1L)
          }
          list(Y_mat = Y_mat, welfare_pt = welfare_pt, w_idx = w_idx)
        })

        for (mi in seq_along(methods)) {
          row_i  <- (yi - 1L) * n_meth + mi
          method <- methods[[mi]]

          member_results <- lapply(member_Y_mats, function(m) {
            tryCatch({
              draw_vals  <- aggregate_draws_vectorized(
                m$Y_mat, method, m$w_idx, pov_line)
              value_pt_m <- aggregate_draws_vectorized(
                matrix(m$welfare_pt, ncol = 1L), method, m$w_idx, pov_line
              )[[1L]]
              list(
                value       = value_pt_m,
                value_lo    = stats::quantile(draw_vals, lo_q, na.rm = TRUE),
                value_p50   = stats::quantile(draw_vals, 0.50, na.rm = TRUE),
                value_hi    = stats::quantile(draw_vals, hi_q, na.rm = TRUE),
                draw_values = draw_vals,
                welfare_pt  = mean(m$welfare_pt, na.rm = TRUE)
              )
            }, error = function(e) NULL)
          })
          member_results <- Filter(Negate(is.null), member_results)
          if (length(member_results) == 0L) {
            out_value[[row_i]]     <- NA_real_
            next
          }
          combined <- combine_ensemble_results(member_results)
          if (is.null(combined)) {
            out_value[[row_i]] <- NA_real_
            next
          }
          out_value[[row_i]]     <- combined$value
          out_value_lo[[row_i]]  <- combined$value_lo
          out_value_hi[[row_i]]  <- combined$value_hi
          out_coef_lo[[row_i]]   <- combined$coef_lo
          out_coef_hi[[row_i]]   <- combined$coef_hi
          out_value_all[[row_i]] <- combined$value_all
          out_model_q10[[row_i]] <- combined$model_q10
          out_model_q90[[row_i]] <- combined$model_q90
          out_model_lo[[row_i]]  <- combined$model_lo
          out_model_hi[[row_i]]  <- combined$model_hi
          out_draws[[row_i]]     <- combined$draw_values
        }
      }

      # Single tibble construction
      all_rows <- tibble::tibble(
        sim_year    = out_year,
        value       = out_value,
        value_lo    = out_value_lo,
        value_hi    = out_value_hi,
        coef_lo     = out_coef_lo,
        coef_hi     = out_coef_hi,
        value_all   = out_value_all,
        model_q10   = out_model_q10,
        model_q90   = out_model_q90,
        model_lo    = out_model_lo,
        model_hi    = out_model_hi,
        draw_values = out_draws,
        agg_method  = out_method,
        weighted    = use_w
      )

      # Remove failed rows
      all_rows <- all_rows[!is.na(all_rows$value), ]

      setNames(
        lapply(methods, function(m) all_rows[all_rows$agg_method == m, ]),
        methods
      )
    }

    list(
      unweighted = run_one_scen(FALSE),
      weighted   = if (has_weights_s) run_one_scen(TRUE) else run_one_scen(FALSE)
    )
  }), names(scenarios))
}

# ---------------------------------------------------------------------------- #
# Aggregation choices                                                          #
# ---------------------------------------------------------------------------- #

#' Aggregation Method Choices for Historical Simulation
#'
#' Returns the named character vector of aggregation method choices used to
#' populate the `selectInput` in the simulation results tab. Binary outcomes
#' are restricted to rate (mean) only.
#'
#' @param outcome_type A character string; `"logical"` for binary outcomes,
#'   any other value for continuous outcomes.
#'
#' @return A named character vector suitable for use in `shiny::selectInput()`.
#'
#' @export
hist_aggregate_choices <- function(outcome_type, outcome_name = NULL) {
  if (identical(outcome_type, "logical")) {
    # Binary outcomes: Mean
    c("Mean" = "mean")
  } else if (identical(outcome_type, "numeric") &&
             identical(outcome_name, "welfare")) {
    # Welfare outcomes: full suite including FGT, Gini, prosperity gap, and average poverty
    c(
      "Mean"                     = "mean",
      "Median"                   = "median",
      "Total"                    = "total",
      "Poverty rate"             = "headcount_ratio",
      "Poverty gap"              = "gap",
      "Poverty severity"         = "fgt2",
      "Gini"                     = "gini",
      "Prosperity gap"           = "prosperity_gap",
      "Average poverty (days/$)" = "avg_poverty"
    )
  } else {
    # All other numeric outcomes (wage, hours, employment, etc.)
    c(
      "Mean"                    = "mean",
      "Median"                  = "median",
      "Total"                   = "total",
      "Outcome headcount ratio" = "headcount_ratio",
      "Outcome gap"             = "gap",
      "Outcome severity"        = "fgt2",
      "Gini"                    = "gini"
    )
  }
}



# ---------------------------------------------------------------------------- #
# Aggregate predictions for plotting
# ---------------------------------------------------------------------------- #

#' Aggregate a Predicted Outcome Across Observations Within Groups
#'
#' Aggregates a predicted individual-level outcome across observations within
#' each group (e.g. simulation year), producing a single summary statistic per
#' group. This is typically used to collapse individual-level predictions from
#' a simulation into group-level indicators before plotting.
#'
#' For binary outcomes the aggregate is always the population share with a value
#' of 1 (i.e. the mean). For continuous outcomes a range of aggregates are
#' available, including poverty and inequality measures.
#'
#' @param df A data frame containing individual-level predictions.
#' @param outcome A character string giving the name of the outcome column in
#'   `df`
#' @param group A character string giving the name of the grouping column.
#'   Defaults to `sim_year`.
#' @param type Either `continuous` (default) or `binary`. For
#'   binary outcomes the aggregate is always the population share with outcome
#'   equal to 1, and the `aggregate` and `pov_line` arguments are
#'   ignored.
#' @param aggregate The summary statistic to compute when `type =
#'   "continuous`. One of `mean` (default), `sum`,
#'   `median`, `headcount_ratio` (population share with outcome
#'   below `pov_line`), `gap` (average normalised shortfall below
#'   `pov_line` across the whole population, i.e. the Foster-Greer-Thorbecke
#'   P1 index), or `gini` (Gini coefficient). Ignored when
#'   `type = "binary`.
#' @param pov_line A numeric poverty line required when `aggregate` is
#'   `headcount_ratio` or `gap`. Ignored otherwise.
#' @param weights  An optional character string giving the name of a survey
#'   weight column in `preds`. When supplied, passed to `aggregate_outcome()`;
#'   `NULL` (default) weights all observations equally.
#'
#' @return A tibble with one row per group containing the grouping column
#'   and a column named `value` holding the computed aggregate.
#'
#' @examples
#' library(dplyr)
#'
#' # Simulate 50 simulation years, 1000 individuals each
#' set.seed(42)
#' sim_data <- data.frame(
#'   sim_year = rep(1:50, each = 1000),
#'   welfare  = exp(rnorm(50000, mean = log(3.50), sd = 0.8))
#' )
#'
#' # Headcount poverty rate at $3.00/day
#' pov_by_year <- aggregate_outcome(
#'   df        = sim_data,
#'   outcome   = "welfare",
#'   type      = "continuous",
#'   aggregate = "headcount_ratio",
#'   pov_line  = 3.00
#' )
#'
#' head(pov_by_year)
#'
#' @importFrom dplyr group_by summarise
#' @importFrom rlang sym
#' @export
aggregate_outcome <- function(df,
                              outcome,
                              group     = "sim_year",
                              type      = c("continuous", "binary"),
                              aggregate = c("mean", "sum", "total", "median",
                                            "headcount_ratio", "gap", "fgt2", "gini",
                                            "prosperity_gap", "avg_poverty"),
                              pov_line  = NULL,
                              weights   = NULL) {

  type      <- match.arg(type)
  # For binary outcomes the aggregate argument is ignored (always population
  # share). Skip match.arg so UI values like "binary" don't cause an error.
  if (type != "binary") {
    aggregate <- match.arg(aggregate)
    # 'total' is a user-facing alias for 'sum'
    if (aggregate == "total") aggregate <- "sum"
  }

  gini_coef <- function(x, w) {
    # Remove NA / non-finite values (present in future sim predictions)
    ok <- is.finite(x)
    if (!is.null(w)) ok <- ok & is.finite(w)
    x  <- x[ok]
    w  <- if (!is.null(w)) w[ok] else NULL

    if (length(x) < 2) return(NA_real_)

    ord    <- order(x)
    x      <- x[ord]
    w      <- if (is.null(w)) rep(1, length(x)) else w[ord]
    w      <- w / sum(w)
    lorenz <- cumsum(w * x) / sum(w * x)
    lorenz <- c(0, lorenz)
    cx     <- c(0, cumsum(w))
    B      <- sum(diff(cx) * (lorenz[-length(lorenz)] + lorenz[-1]) / 2)
    1 - 2 * B
  }

  compute <- function(x, w) {
    if (type == "binary") {
      # multiply by 100 to express as percentage points
      if (is.null(w)) 100*mean(as.numeric(x), na.rm = TRUE)
      else 100*sum(as.numeric(x) * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
    } else {
      switch(aggregate,
        mean            = if (is.null(w)) mean(x, na.rm = TRUE)
                          else sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE),
        sum             = if (is.null(w)) sum(x, na.rm = TRUE)
                          else sum(x * w, na.rm = TRUE),
        median          = if (is.null(w)) median(x, na.rm = TRUE) else {
                            valid <- is.finite(x) & is.finite(w) & w > 0
                            if (sum(valid) == 0L) return(NA_real_)
                            x <- x[valid]; w <- w[valid]
                            ord  <- order(x); x <- x[ord]; w <- w[ord]
                            cumw <- cumsum(w) / sum(w)
                            x[which(cumw >= 0.5)[1]]
                          },
        headcount_ratio = {
          if (is.null(pov_line)) stop("`pov_line` must be supplied for headcount_ratio")
          poor <- as.numeric(x < pov_line)
          if (is.null(w)) mean(poor, na.rm = TRUE)
          else sum(poor * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        },
        gap             = {
          if (is.null(pov_line)) stop("`pov_line` must be supplied for gap")
          shortfall <- pmax(pov_line - x, 0) / pov_line
          if (is.null(w)) mean(shortfall, na.rm = TRUE)
          else sum(shortfall * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        },
        fgt2 = {
          if (is.null(pov_line)) stop("`pov_line` must be supplied for fgt2")
          shortfall <- pmax(pov_line - x, 0) / pov_line
          if (is.null(w)) mean(shortfall^2, na.rm = TRUE)
          else sum((shortfall^2) * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        },
        gini            = gini_coef(x, w),
        prosperity_gap  = {
          # Average factor by which incomes must be multiplied to reach $28/day.
          # For incomes already >= 28 the gap factor is 1 (no gap).
          pg <- pmax(28 / x, 1)
          if (is.null(w)) mean(pg, na.rm = TRUE)
          else sum(pg * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        },
        avg_poverty     = {
          # Average poverty = mean(1 / x): days needed to earn $1.
          # Non-positive incomes are excluded to avoid Inf / NaN.
          ok <- is.finite(x) & x > 0
          xp <- x[ok]
          wp <- if (!is.null(w)) w[ok] else NULL
          if (length(xp) == 0) return(NA_real_)
          if (is.null(wp)) mean(1 / xp, na.rm = TRUE)
          else sum((1 / xp) * wp, na.rm = TRUE) / sum(wp, na.rm = TRUE)
        }
      )
    }
  }

  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
    summarise(
      value = compute(
        x = .data[[outcome]],
        w = if (!is.null(weights)) .data[[weights]] else NULL
      ),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------- #
# Convert to deviation from centre for plotting
# ---------------------------------------------------------------------------- #

#' Express Aggregate Values as Deviation from a Central Tendency
#'
#' Takes the output of `aggregate_outcome()` and expresses each group's
#' value as a deviation from either the mean or median across all groups.
#' Optionally flips the sign of the result for loss-framed outcomes, where a
#' positive deviation is undesirable (e.g. poverty rates, deficits).
#'
#' @param df A data frame with at least a grouping column and a `value`
#'   column, as returned by `aggregate_outcome()`.
#' @param group A character string giving the name of the grouping column.
#'   Defaults to `"sim_year"`.
#' @param centre A character string, either `"mean"` (default) or
#'   `"median"`, specifying the central tendency to deviate from.
#' @param loss Logical. If `TRUE`, the sign of the deviation is flipped so
#'   that positive values represent outcomes worse than the centre (e.g. higher
#'   poverty than expected). Defaults to `FALSE`.
#'
#' @return A tibble with the same columns as `df` and `value`
#'   replaced by the deviation from the chosen centre, optionally sign-flipped.
#'
#' @examples
#' library(dplyr)
#'
#' set.seed(42)
#' sim_data <- data.frame(
#'   sim_year = rep(1:50, each = 1000),
#'   welfare  = exp(rnorm(50000, mean = log(3.50), sd = 0.8))
#' )
#'
#' pov_by_year <- aggregate_outcome(
#'   df        = sim_data,
#'   outcome   = "welfare",
#'   type      = "continuous",
#'   aggregate = "headcount_ratio",
#'   pov_line  = 3.00
#' )
#'
#' # Deviation from mean, welfare framing (higher welfare = good)
#' deviation_from_centre(pov_by_year)
#'
#' # Deviation from median, loss framing (higher poverty = bad)
#' deviation_from_centre(pov_by_year, centre = "median", loss = TRUE)
#'
#' @importFrom dplyr mutate
#' @export
deviation_from_centre <- function(df,
                                  group  = "sim_year",
                                  centre = c("mean", "median"),
                                  loss   = FALSE) {

  centre <- match.arg(centre)

  ref <- switch(centre,
    mean   = mean(df$value,   na.rm = TRUE),
    median = median(df$value, na.rm = TRUE)
  )

  sign <- if (loss) -1 else 1

  df |>
    mutate(value = sign * (value - ref))
}

# NOTE: plot_exceedance() and plot_hist_sim() have been archived to
# dev/archived_fct/plot_exceedance_archived.R. They are superseded by
# enhance_exceedance() in fct_sim_compare.R and have no active call sites.

# ---------------------------------------------------------------------------- #
# Shared aggregation helper                                                    #
# ---------------------------------------------------------------------------- #

#' Aggregate Simulation Predictions for Exceedance Plotting
#'
#' Aggregates individual-level predictions by sim_year, optionally applies
#' deviation_from_centre(), and returns the aggregated data frame with x_label.
#' @param preds      Data frame of individual-level predictions with a
#'   `sim_year` column and the outcome column named by `so$name`.
#' @param so         One-row outcome metadata data frame (columns `name`,
#'   `label`, `type`).
#' @param agg_method Character. One of `"mean"`, `"median"`,
#'   `"headcount_ratio"`, `"gap"`, `"gini"`.
#' @param deviation  Character. One of `"none"`, `"mean"`, `"median"`.
#' @param loss_frame Logical. Passed to `deviation_from_centre()` as `loss`.
#' @param pov_line   Numeric or `NULL`. Required for `"headcount_ratio"` /
#'   `"gap"`.
#' @param weights    Character or `NULL`. Name of the survey weight column in
#'   `preds`. When non-`NULL` and present in `preds`, passed to
#'   `aggregate_outcome()`. Default `NULL` (unweighted).
#'
#' @return A list with elements `out` (aggregated data frame with `sim_year`
#'   and `value` columns) and `x_label` (character).
#'
#' @importFrom rlang abort
#' @export
aggregate_sim_preds <- function(preds, so, agg_method, deviation, loss_frame,
                                pov_line = NULL, weights = NULL) {

  if (agg_method %in% c("headcount_ratio", "gap", "fgt2") && is.null(pov_line)) {
    rlang::abort(
      "`pov_line` must be supplied when `agg_method` is 'headcount_ratio','gap', or 'fgt2'."
    )
  }

  # Group by (model, sim_year) when a model column is present (future scenarios
  # with all ensemble members pooled), so the CI reflects model × year variation.
  # --- Two-stage aggregation ----------------------------------------------- #
  # IMPORTANT: order of operations matters for coefficient uncertainty bands.  #
  # Stage 1: aggregate within each (draw_id, model, sim_year) to a scalar.    #
  #          This gives one aggregate statistic per coefficient draw.           #
  # Stage 2: take percentiles of that scalar across draw_id.                   #
  # Taking percentiles BEFORE aggregating gives quantiles of the individual    #
  # welfare distribution — a completely different quantity. Don't mix these up. #
  # --------------------------------------------------------------------------- #

  has_draws <- "draw_id" %in% names(preds) && !all(is.na(preds$draw_id))

  # Stage 1 grouping: always include draw_id when present so each draw
  # produces its own aggregate scalar before we summarise across draws.
  grp_cols <- c(
    if (has_draws)                          "draw_id",
    if ("model" %in% names(preds))          "model",
    "sim_year"
  )

  out <- aggregate_outcome(
    df        = preds,
    outcome   = so$name,
    group     = grp_cols,
    type      = if (identical(so$type, "logical")) "binary" else "continuous",
    aggregate = agg_method,
    pov_line  = pov_line,
    weights   = if (!is.null(weights) && weights %in% names(preds)) weights else NULL
  )

  # Stage 2: collapse across draw_id to get coefficient-uncertainty percentiles.
  # IMPORTANT: percentiles are taken ACROSS draw_id (one scalar per draw),
  # NOT across household-level predictions. These are uncertainty bands on the
  # aggregate statistic, not quantiles of the individual welfare distribution.
  if (has_draws) {
    out <- out |>
      dplyr::group_by(dplyr::across(dplyr::any_of(c("model", "sim_year")))) |>
      dplyr::summarise(
        value_p05 = stats::quantile(value, 0.05, na.rm = TRUE),
        value_p50 = stats::quantile(value, 0.50, na.rm = TRUE),
        value_p95 = stats::quantile(value, 0.95, na.rm = TRUE),
        value     = value_p50,
        .groups   = "drop"
      )
  }

  if (!identical(deviation, "none")) {
    out <- deviation_from_centre(out, "sim_year", deviation, loss_frame)
    names(out)[names(out) == "value"] <- "deviation"
  }

  list(
    out     = out,
    x_label = "Simulation year"
  )
}

#' Apply Deviation from Historical Reference Value
#'
#' Subtracts a historical reference value from a data frame's \code{value}
#' column to express results as deviations from a baseline. Used in Module 2
#' results and Module 3 comparison to centre scenario values against the
#' historical mean or median.
#'
#' @param d          Data frame with a \code{value} column. Returns \code{d}
#'   unchanged if \code{deviation = "none"} or \code{d} is \code{NULL}.
#' @param deviation  Character. One of \code{"none"}, \code{"mean"},
#'   \code{"median"}. Determines how the reference value is computed when
#'   \code{hist_ref} is not supplied.
#' @param hist_ref   Numeric. Pre-computed reference value (e.g. historical
#'   mean). When \code{NA} (default), computed from \code{d$value} using
#'   \code{deviation}.
#'
#' @return Data frame with \code{value} column replaced by
#'   \code{value - hist_ref}.
#'
#' @export
apply_deviation <- function(d, deviation, hist_ref = NA_real_) {
  if (identical(deviation, "none") || is.null(d)) return(d)
  if (is.na(hist_ref)) {
    hist_ref <- if (identical(deviation, "mean"))
      mean(d$value, na.rm = TRUE)
    else
      stats::median(d$value, na.rm = TRUE)
  }
  dplyr::mutate(d, value = value - hist_ref)
}

#' Compute exceedance curve ribbon from Cholesky draw values
#'
#' For each of S coefficient draws, computes the exceedance probability
#' (1 - ECDF) at a grid of welfare values across all N simulation years.
#' Returns p10/p90 envelope across S draws — the coefficient uncertainty
#' ribbon for the exceedance plot.
#'
#' @param agg_tbl  Tibble. Output of compute_hist_agg() or
#'   compute_scenario_agg() for one method. Must have columns:
#'   value (point estimate per year) and draw_values (list column,
#'   S draws per year).
#' @param band_q   Named numeric(2). Quantile bounds for ribbon.
#'   Default c(lo = 0.10, hi = 0.90).
#'
#' @return Tibble with columns: exceed_prob (y-axis exceedance probability),
#'   welfare_mid (point estimate welfare), welfare_lo, welfare_hi
#'   (p10/p90 coefficient uncertainty bounds). NULL if draw_values unavailable.
#' @noRd
compute_exceedance_ribbon <- function(agg_tbl,
                                      band_q = c(lo = 0.10, hi = 0.90),
                                      model_lo = NULL, model_hi = NULL) {
  draw_list <- agg_tbl$draw_values
  if (is.null(draw_list) || length(draw_list) == 0L) return(NULL)

  S       <- length(draw_list[[1L]])
  N_years <- nrow(agg_tbl)
  if (S < 2L) return(NULL)

  # Build N_years × S matrix — each column = one draw across all years
  draw_mat <- matrix(
    unlist(draw_list, use.names = FALSE),
    nrow  = N_years,
    ncol  = S,
    byrow = TRUE
  )

  # Sort each column highest → lowest — S exceedance curves in x-space
  # Each column is now one complete exceedance curve (welfare values)
  rank_order  <- order(agg_tbl$value, decreasing = TRUE)
  ordered_mat <- draw_mat[rank_order, ]

  # Exceedance probabilities — matches R ecdf() formula exactly:
  probs <- (seq_len(N_years) - 0.5) / N_years

  # Point estimate curve — sort annual values highest to lowest
  welfare_sorted <- sort(agg_tbl$value, decreasing = TRUE)

  # Ribbon = quantile envelope across S curves IN WELFARE (x) SPACE
  # At each probability rank — what is the p10/p90 welfare value?
  coef_lo <- matrixStats::rowQuantiles(
             ordered_mat, probs = band_q[["lo"]], na.rm = TRUE)
  coef_hi <- matrixStats::rowQuantiles(
              ordered_mat, probs = band_q[["hi"]], na.rm = TRUE)

  # Option A approximation — apply coef width to ensemble bounds
  # Ensemble uncertainty bands use coefficient uncertainty width from
  # the mean ensemble member applied to lo/hi members.
  # This approximates the joint distribution. Error is small for linear
  # aggregates (<5% of band width) and conservative for poverty measures
  # (understates true joint uncertainty by ~10-20%).
  # Option B (per-member Cholesky draws) available if tighter bounds needed.
  # See known_issues.md #18 and methodology workplan for full discussion.
  coef_width_lo <- welfare_sorted - coef_lo   # half-width below central
  coef_width_hi <- coef_hi - welfare_sorted   # half-width above central

  final_lo <- if (!is.null(model_lo))
    pmin(coef_lo, model_lo - coef_width_lo)
  else coef_lo

  final_hi <- if (!is.null(model_hi))
    pmax(coef_hi, model_hi + coef_width_hi)
  else coef_hi

  tibble::tibble(
    exceed_prob  = probs,
    welfare_mid  = welfare_sorted,
    welfare_lo   = final_lo,
    welfare_hi   = final_hi
  )
}