# ============================================================================ #
# Pure functions used by mod_2_01.
# Stateless and testable without Shiny.
#
# Shared constants (used across multiple modules):
#   RP_LOW            -- low-tail return period map  (name -> quantile prob)
#   RP_HIGH           -- high-tail return period map (name -> quantile prob)
#   SSP_SHORT_LABELS  -- canonical SSP key -> short display label
#
# Shared UI helpers (called inside module server functions):
#   residual_method_ui(ns, input_id)  -- radio buttons + helpText for residuals
#
# Simulation pipeline helper:
#   run_sim_pipeline()  -- weather join -> predict -> back-transform in one call

# ============================================================================ #

# ---- Internal colour / style helpers ---------------------------------------
# Used by enhance_exceedance() and plot_pointrange_climate(). Not exported.

# Canonical SSP keys must match what .normalise_ssp() returns
.ssp_colours <- c(
  "SSP2-4.5" = "#4dac26",   # green  (lower emissions)
  "SSP3-7.0" = "#2166ac",   # blue   (mid emissions)
  "SSP5-8.5" = "#c0392b"    # red    (high emissions)
)



# Normalise any SSP token found in a scenario name to a canonical key
# Strips trailing " / P{pct}" suffix first so keys like "SSP3-7.0 / 2030 / P50"
# are handled identically to the old "SSP3-7.0 / 2030" format.
# e.g. "SSP2" -> "SSP2-4.5", "SSP3-7.0" -> "SSP3-7.0", NA -> NA
.normalise_ssp <- function(nm) {
  nm_clean <- sub(" / P[0-9]+$", "", nm)
  m_full <- regmatches(nm_clean, regexpr("SSP[0-9]-[0-9.]+", nm_clean))
  if (length(m_full) > 0) return(m_full)
  m_short <- regmatches(nm_clean, regexpr("SSP([2-9])", nm_clean))
  if (length(m_short) == 0) return(NA_character_)
  digit  <- sub("SSP", "", m_short)
  lookup <- c("2" = "SSP2-4.5", "3" = "SSP3-7.0", "5" = "SSP5-8.5")
  if (digit %in% names(lookup)) lookup[[digit]] else NA_character_
}

.parse_year <- function(nm) {
  if (length(nm) == 1L) {
    m <- regexpr("\\d{4}-\\d{4}", nm)
    if (m == -1L) return(NA_character_)
    return(regmatches(nm, m))
  }
  # Vectorised path
  m <- gregexpr("\\d{4}-\\d{4}", nm)
  vapply(seq_along(nm), function(i) {
    matches <- regmatches(nm[i], m[i])[[1]]
    if (length(matches) == 0) NA_character_ else matches[1]
  }, character(1))
}

# ---------------------------------------------------------------------------- #
# Shared constants                                                             #
# ---------------------------------------------------------------------------- #

#' Symmetric Return-Period Probability Maps
#'
#' Named numeric vectors mapping return-period labels to exceedance
#' probabilities. Used consistently across the threshold table and exceedance
#' curve in mod_2_06 and fct_sim_compare.
#'
#' \describe{
#'   \item{RP_LOW}{Rare low-outcome tail: 1:50, 1:20, 1:10, 1:5}
#'   \item{RP_HIGH}{Rare high-outcome tail: 4:5, 9:10, 19:20, 49:50}
#' }
#' @export
RP_LOW  <- c("1:50" = 0.02, "1:20" = 0.05, "1:10" = 0.10, "1:5" = 0.20)

#' @rdname RP_LOW
#' @export
RP_HIGH <- c("4:5" = 0.80, "9:10" = 0.90, "19:20" = 0.95, "49:50" = 0.98)

#' Short Display Labels for SSP Scenarios
#'
#' Maps canonical SSP keys (as returned by `.normalise_ssp()`) to short labels
#' used in UI checkboxes and plot legends.
#'
#' @export
SSP_SHORT_LABELS <- c(
  "SSP2-4.5" = "SSP2",
  "SSP3-7.0" = "SSP3",
  "SSP5-8.5" = "SSP5"
)

# ---------------------------------------------------------------------------- #
# Coefficient-draw constants and helpers                                       #
# ---------------------------------------------------------------------------- #

#' Format elapsed seconds into a human-readable string (e.g. "2m 14s")
#' Used by the simulation progress bar.
#' @noRd
format_elapsed <- function(secs) {
  secs <- round(secs)
  if (secs < 60L) return(sprintf("%ds", secs))
  sprintf("%dm %02ds", secs %/% 60L, secs %% 60L)
}

# SE clustering specification — confirmed default: ~loc_id
# Methodological justification: more conservative than ~loc_id:int_month
# (Moulton minimum). Absorbs within-location serial correlation across
# months and years. Weather data has real within-location temporal
# correlation that ~loc_id:int_month does not correct.
#
# Cluster count decision tree:
#   G >= 50 at ~loc_id : use ~loc_id (default)
#   40 <= G < 50        : use ~loc_id, flag in methodology note
#   G < 40              : warn user, wild cluster bootstrap recommended
#
# Named alternatives — available as constants, not default.
# Use compute_cluster_counts() to check G before switching.
COEF_VCOV_SPEC              <- ~loc_id
COEF_VCOV_SPEC_MOULTON      <- ~loc_id:int_month
COEF_VCOV_SPEC_CONSERVATIVE <- ~code + year + survname + loc_id

#' Compute Cluster Counts for SE Specification Diagnostics
#'
#' Computes the number of clusters at each relevant clustering level for the
#' fitted model data. Used to validate the SE specification and warn when
#' cluster counts are too small for reliable asymptotic inference.
#'
#' @param data Data frame used at model fit time. Must contain \code{loc_id}
#'   and \code{int_month} columns. Optionally \code{code}, \code{year},
#'   \code{survname} for the conservative multi-way spec.
#'
#' @return Named list with integer cluster counts:
#'   \describe{
#'     \item{loc_id}{Number of distinct location clusters (primary spec).}
#'     \item{loc_id_int_month}{Number of distinct location × month clusters
#'       (Moulton minimum).}
#'     \item{conservative}{Number of distinct code × year × survname × loc_id
#'       clusters (conservative multi-way spec). \code{NA} if any column
#'       is absent.}
#'   }
#'
#' @section Cluster count thresholds:
#' \itemize{
#'   \item G >= 50: reliable asymptotic inference, use \code{COEF_VCOV_SPEC}
#'   \item 40 <= G < 50: flag in methodology note
#'   \item G < 40: wild cluster bootstrap recommended (not yet implemented)
#' }
#'
#' @export
compute_cluster_counts <- function(data) {
  has_cols <- function(...) all(c(...) %in% names(data))

  g_loc        <- if (has_cols("loc_id"))
                    dplyr::n_distinct(data[["loc_id"]])
                  else NA_integer_

  g_loc_month  <- if (has_cols("loc_id", "int_month"))
                    dplyr::n_distinct(paste(data[["loc_id"]], data[["int_month"]]))
                  else NA_integer_

  g_conserv    <- if (has_cols("code", "year", "survname", "loc_id"))
                    dplyr::n_distinct(paste(data[["code"]], data[["year"]],
                                           data[["survname"]], data[["loc_id"]]))
                  else NA_integer_

  counts <- list(
    loc_id           = g_loc,
    loc_id_int_month = g_loc_month,
    conservative     = g_conserv
  )

  # Runtime warnings — surface immediately at model fit time
  if (!is.na(g_loc) && g_loc < 40L) {
    warning(sprintf(
      "[SE] Only %d clusters at ~loc_id. VCV SEs may be unreliable. Wild cluster bootstrap recommended (not yet implemented).",
      g_loc
    ))
  } else if (!is.na(g_loc) && g_loc < 50L) {
    message(sprintf(
      "[SE] %d clusters at ~loc_id — borderline. Flag in methodology note.",
      g_loc
    ))
  }

  counts
}

# ---------------------------------------------------------------------------- #
# Cholesky uncertainty propagation                                              #
# ---------------------------------------------------------------------------- #

#' Compute Cholesky Factor of Model VCV Matrix
#'
#' Computes the lower-triangular Cholesky factor of the coefficient covariance
#' matrix for the non-fixed-effect coefficients of a fitted \code{fixest}
#' model. Used once per fitted model — not per weather key.
#'
#' The Cholesky decomposition \eqn{L L' = \Sigma} allows efficient K-dimensional
#' Monte Carlo draws: instead of drawing full N-dimensional prediction vectors,
#' draw \eqn{z_s \sim N(0, I_K)} and compute the perturbation as
#' \eqn{F_i \cdot z_s} where \eqn{F = X_{nonFE} L'} is the factor loading
#' matrix computed once per key in \code{compute_factor_loading()}.
#'
#' @param fit A fitted \code{fixest} model object (from \code{fixest::feols()}).
#' @param vcov_spec A one-sided formula specifying the clustering structure for
#'   the VCV matrix. Defaults to \code{COEF_VCOV_SPEC} (\code{~loc_id}).
#'   See \code{COEF_VCOV_SPEC_MOULTON} and \code{COEF_VCOV_SPEC_CONSERVATIVE}
#'   for alternatives.
#'
#' @return A named list:
#'   \describe{
#'     \item{L}{K \eqn{\times} K lower-triangular Cholesky factor of
#'       \eqn{\Sigma}.}
#'     \item{K}{Integer. Number of non-FE coefficients.}
#'     \item{beta}{Named numeric vector of point-estimate non-FE
#'       coefficients.}
#'     \item{spec}{The VCV formula used.}
#'   }
#'
#' @seealso \code{\link{compute_factor_loading}},
#'   \code{\link{aggregate_with_uncertainty}}
#' @export
compute_chol_vcov <- function(fit, vcov_spec = COEF_VCOV_SPEC) {
  stopifnot(
    "fit must be a fixest model" = inherits(fit, "fixest")
  )

  beta  <- coef(fit)
  Sigma <- vcov(fit, vcov = vcov_spec)

  # Cholesky decomposition — chol() returns upper triangular R such that
  # t(R) %*% R == Sigma. We store L = t(R) (lower triangular) so that
  # L %*% t(L) == Sigma, consistent with the factor loading formula
  # F = X_nonFE %*% t(L).
  L <- tryCatch(
    t(chol(Sigma)),
    error = function(e) stop(sprintf(
      "[compute_chol_vcov] Cholesky decomposition failed. ",
      "Check that VCV matrix is positive definite at spec '%s': %s",
      deparse(vcov_spec), conditionMessage(e)
    ))
  )

  list(
    L    = L,
    K    = nrow(L),
    beta = beta,
    spec = vcov_spec
  )
}


#' Compute Factor Loading Matrix for Coefficient Uncertainty
#'
#' Computes the N \eqn{\times} K factor loading matrix
#' \eqn{F = X_{nonFE} L'} where \eqn{L} is the Cholesky factor from
#' \code{compute_chol_vcov()} and \eqn{X_{nonFE}} is the non-fixed-effect
#' design matrix for the counterfactual data.
#'
#' The factor loading encodes how coefficient uncertainty propagates to
#' prediction uncertainty for each household. Given a K-dimensional standard
#' normal draw \eqn{z_s \sim N(0, I_K)}, the perturbed log-welfare prediction
#' for household \eqn{i} under draw \eqn{s} is:
#' \deqn{y_i^{(s)} = y_i^{point} + F_i \cdot z_s}
#'
#' This is mathematically identical to the previous N \eqn{\times} S matrix
#' approach but requires only K-dimensional draws (K ~ 5-20) instead of
#' N-dimensional draws (N ~ 10,000), giving ~200x speedup.
#'
#' @param X_nonFE Numeric matrix. N \eqn{\times} K non-FE design matrix from
#'   \code{model.matrix(model, data = newdata, type = "rhs")}. Column names
#'   must match \code{names(chol_obj$beta)} exactly.
#' @param chol_obj Named list returned by \code{compute_chol_vcov()}.
#'
#' @return Numeric matrix of dimensions N \eqn{\times} K. Each row \eqn{i}
#'   is the factor loading vector for household \eqn{i}.
#'
#' @seealso \code{\link{compute_chol_vcov}},
#'   \code{\link{aggregate_with_uncertainty}}
#' @export
compute_factor_loading <- function(X_nonFE, chol_obj) {
  stopifnot(
    "X_nonFE must be a numeric matrix"        = is.matrix(X_nonFE) && is.numeric(X_nonFE),
    "chol_obj must contain L and beta"        = all(c("L", "beta") %in% names(chol_obj)),
    "X_nonFE columns must match chol_obj$beta names" =
      identical(colnames(X_nonFE), names(chol_obj$beta))
  )

  # F = X_nonFE %*% t(L)   →   N × K loading matrix
  # Each row i: F_i = x_i' L'  such that  F_i F_i' = x_i' Σ x_i
  X_nonFE %*% chol_obj$L #Bug fixed here, orriginally t(chol_obj$L) - may require other fixes in description.
}
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
      # Weighted Gini via the covariance formula
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
    p10_p90   = c(lo = 0.10,  hi = 0.90),
    p025_p975 = c(lo = 0.025, hi = 0.975),
    p005_p995 = c(lo = 0.005, hi = 0.995),
    minmax    = c(lo = 0.00,  hi = 1.00),
    none      = NULL,
              c(lo = 0.10,  hi = 0.90)
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
#' @param agg_fn Function of the form
#'   \code{function(welfare, weights, pov_line) -> scalar}. Created by
#'   \code{resolve_agg_fn()}.
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
                                       agg_fn,
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
  welfare_pt <- if (is_log) exp(y_point + resid_vec) else y_point + resid_vec
  value_pt   <- agg_fn(welfare_pt, weights, pov_line)

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

  draw_vals <- vapply(seq_len(S), function(s) {
    y_s       <- y_point + perturbations[, s] + resid_vec
    welfare_s <- if (is_log) exp(y_s) else y_s
    agg_fn(welfare_s, weights, pov_line)
  }, numeric(1L))

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
combine_ensemble_results <- function(model_results) {
  stopifnot(
    "model_results must be a non-empty named list" =
      is.list(model_results) && length(model_results) > 0
  )

  # Identify the mean representative as the central estimate.
  # Falls back to first entry if no *_ensemble_mean key present.
  mean_key <- grep("ensemble_mean", names(model_results), value = TRUE)
  central  <- if (length(mean_key) > 0L)
                model_results[[mean_key[[1L]]]]
              else
                model_results[[1L]]

  # Filter to non-NULL entries with valid value fields before extracting
  model_results <- Filter(function(x) !is.null(x) && !is.null(x$value) && !is.na(x$value), model_results)
  if (length(model_results) == 0L) return(NULL)

  values   <- vapply(model_results, `[[`, numeric(1L), "value")
  value_lo <- vapply(model_results, `[[`, numeric(1L), "value_lo")
  value_hi <- vapply(model_results, `[[`, numeric(1L), "value_hi")

  list(
    value       = central$value,
    value_lo    = mean(value_lo, na.rm = TRUE),   # inner band — avg coef uncertainty
    value_hi    = mean(value_hi, na.rm = TRUE),   # inner band — avg coef uncertainty
    model_lo    = min(values,    na.rm = TRUE),   # outer band — ensemble spread
    model_hi    = max(values,    na.rm = TRUE),   # outer band — ensemble spread
    draw_values = central$draw_values,            # paired draws from mean representative
    n_members   = length(model_results)
  )
}

#' Draw Coefficient Vectors from a Fitted Model
#'
#' Generates \code{S} draws of the full coefficient vector for propagating
#' parameter uncertainty into counterfactual simulations. The parametric
#' (VCV) branch draws from the multivariate normal defined by the fitted
#' coefficient vector and its variance-covariance matrix. Bootstrap branches
#' are stubbed so call sites do not need to change when they are implemented.
#'
#' @param fit      A fitted \code{feols} model object.
#' @param S        Integer. Number of draws. Default 500.
#' @param method   Character. One of \code{"vcov"} (default),
#'   \code{"bootstrap"}, \code{"wild_bootstrap"}. The latter two are
#'   not yet implemented.
#' @param vcov_spec Formula or character passed to \code{vcov(fit, vcov = ...)}.
#'   Defaults to \code{COEF_VCOV_SPEC}.
#' @param seed     Optional integer seed for reproducibility.
#'
#' @return An \code{S x K} numeric matrix with column names matching
#'   \code{coef(fit)}.
#'
#' @export
draw_coefs <- function(fit,
                       S         = 500,
                       method    = c("vcov", "bootstrap", "wild_bootstrap"),
                       vcov_spec = COEF_VCOV_SPEC,
                       seed      = NULL) {
  method <- match.arg(method)
  if (!is.null(seed)) set.seed(seed)

  if (method == "vcov") {
    beta_hat <- coef(fit)
    Sigma    <- vcov(fit, vcov = vcov_spec)
    draws    <- MASS::mvrnorm(n = S, mu = beta_hat, Sigma = Sigma)
    # Ensure matrix form even when S = 1
    if (is.null(dim(draws))) {
      draws <- matrix(draws, nrow = 1L, dimnames = list(NULL, names(beta_hat)))
    }
    return(draws)
  }

  stop(sprintf("draw_coefs: method '%s' not yet implemented", method))
}

# ---------------------------------------------------------------------------- #
# Shared UI helper                                                             #
# ---------------------------------------------------------------------------- #

#' Residual Method Radio Buttons UI
#'
#' Produces a consistent `radioButtons` widget + `helpText` block for choosing
#' how prediction residuals are handled. Used in both `mod_2_01_historical` and
#' `mod_2_03_future` to avoid duplicating the same 24-line block.
#'
#' @param ns       The module namespace function (from `session$ns`).
#' @param input_id The input id for the radio buttons (unnamespaced).
#'
#' @return A `tagList` containing `radioButtons` and `helpText`.
#' @export
residual_method_ui <- function(ns, input_id) {
  shiny::tagList(
    shiny::radioButtons(
      inputId  = ns(input_id),
      label    = "Residuals method",
      choices  = residual_choices(),
      selected = "original"
    ),
    shiny::helpText(
      shiny::tags$b("none:"), " return fitted values only.", shiny::tags$br(),
      shiny::tags$b("original:"), " match each observation's own training residual",
      " by ID, preserving individual-level heterogeneity across simulation years.",
      shiny::tags$br(),
      shiny::tags$b("empirical:"), " resample residuals from the training",
      " distribution (non-parametric bootstrap).", shiny::tags$br(),
      shiny::tags$b("normal:"), " draw residuals from N(0, \u03c6) where \u03c6",
      " is the training residual SD.",
      style = "font-size:11px;"
    )
  )
}

# ---------------------------------------------------------------------------- #
# Simulation pipeline helper                                                   #
# ---------------------------------------------------------------------------- #

#' Resolve the ID Column for Residual Matching
#'
#' Returns the first of `c("pid", "hhid", "fid")` that exists in both data
#' frames, or `NULL` if none match.  Used by `run_sim_pipeline()` to enable
#' ID-based residual matching when `residuals = "original"`.
#' @noRd
resolve_id_col <- function(a, b) {
  candidates <- c("pid", "hhid", "fid")
  shared     <- intersect(names(a), names(b))
  match      <- candidates[candidates %in% shared]
  if (length(match) == 0L) NULL else match[[1L]]
}

#' Run Simulation Pipeline for One Weather Key
#'
#' Prepares counterfactual survey data for one weather key, computes point-
#' estimate log-welfare predictions, and returns the factor loading matrix for
#' downstream coefficient uncertainty propagation via
#' \code{aggregate_with_uncertainty()}.
#'
#' This function is called once per weather key (historical + future
#' representatives). It does NOT draw coefficient perturbations — all
#' uncertainty propagation is deferred to display time via
#' \code{aggregate_with_uncertainty()}, making poverty line, weights, and
#' aggregation method fully reactive without re-simulation.
#'
#' @param weather_raw Data frame. One weather key's prepared data from
#'   \code{get_weather()} or \code{summarise_ensemble()}.
#' @param svy Data frame. Survey microdata joined to weather reference data.
#' @param sw One-row data frame of selected weather variable metadata.
#' @param so One-row data frame of selected outcome variable metadata.
#' @param model Fitted \code{fixest} model object.
#' @param residuals Character. Residual treatment passed through to
#'   \code{aggregate_with_uncertainty()}. One of \code{"none"},
#'   \code{"original"}, \code{"normal"}, \code{"resample"}.
#' @param train_data Data frame. Training data used to fit \code{model}.
#'   Used to compute training residuals for \code{train_aug}.
#' @param engine Character. Model engine identifier (e.g. \code{"fixest"}).
#' @param chol_obj Named list from \code{compute_chol_vcov()} or \code{NULL}.
#'   When \code{NULL}, \code{F_loading} in the return value is \code{NULL}
#'   (point estimates only — no coefficient uncertainty).
#'
#' @return Named list or \code{NULL} on prediction failure:
#'   \describe{
#'     \item{y_point}{Numeric vector length N. Log-scale point-estimate welfare
#'       predictions. Back-transformation via \code{exp()} happens inside
#'       \code{aggregate_with_uncertainty()}, not here.}
#'     \item{F_loading}{N \eqn{\times} K numeric matrix from
#'       \code{compute_factor_loading()}, or \code{NULL} when
#'       \code{chol_obj = NULL}.}
#'     \item{sim_year}{Integer vector length N. Simulation year per row.}
#'     \item{weight}{Numeric vector length N or \code{NULL}. Survey weights.}
#'     \item{id_vec}{Vector length N or \code{NULL}. Household IDs for
#'       \code{residuals = "original"} matching.}
#'     \item{id_col}{Character or \code{NULL}. Name of the ID column.}
#'     \item{n_pre_join}{Integer. Number of survey rows before weather join.}
#'     \item{weather_raw}{Data frame. The input weather key data (for
#'       diagnostics).}
#'     \item{train_aug}{Data frame. Training data augmented with \code{.fitted}
#'       and \code{.resid} columns for residual drawing in
#'       \code{aggregate_with_uncertainty()}.}
#'   }
#'
#' @seealso \code{\link{aggregate_with_uncertainty}},
#'   \code{\link{compute_chol_vcov}}, \code{\link{compute_factor_loading}}
#' @export
run_sim_pipeline <- function(weather_raw,
                             svy,
                             sw,
                             so,
                             model,
                             residuals,
                             train_data,
                             engine,
                             chol_obj = NULL) {

  n_pre_join    <- nrow(svy)
  survey_wd_sim <- prepare_hist_weather(weather_raw, svy, sw, so$name)

  # Resolve ID column for "original" residual matching
  id_col <- if (residuals == "original")
               resolve_id_col(train_data, survey_wd_sim)
             else
               NULL

  # ---- Point estimate prediction ------------------------------------------ #
  # predict_outcome() called ONCE — no coefficient draws here.
  # All uncertainty propagation is deferred to aggregate_with_uncertainty().
  pred_out <- tryCatch(
    predict_outcome(
      model      = model,
      newdata    = survey_wd_sim,
      residuals  = "none",     # residuals drawn at display time, not here
      outcome    = so$name,
      id         = id_col,
      train_data = train_data,
      engine     = engine
    ),
    error = function(e) {
      warning("[run_sim_pipeline] predict_outcome() failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(pred_out)) {
    rm(survey_wd_sim)
    return(NULL)
  }

  # y_point stays log-scale — back-transformation happens inside
  # aggregate_with_uncertainty() after coefficient perturbation.
  # NOTE: apply_log_backtransform() is NOT called here.
  y_point  <- pred_out$.fitted

  # ---- Simulation year and weights ---------------------------------------- #
  sim_year <- pred_out$sim_year

  wt_col   <- grep("^weight$|^hhweight$|^wgt$|^pw$", names(pred_out),
                   value = TRUE, ignore.case = TRUE)
  weight   <- if (length(wt_col) > 0L) pred_out[[wt_col[[1L]]]] else NULL

  id_vec   <- if (!is.null(id_col) && id_col %in% names(pred_out))
                pred_out[[id_col]]
              else
                NULL

  # ---- Factor loading matrix ---------------------------------------------- #
  # Computed once per key — not per draw.
  # F_loading = X_nonFE %*% L  where L is the Cholesky factor of Sigma.
  # NULL when chol_obj = NULL (point estimates only).
  F_loading <- NULL
  if (!is.null(chol_obj)) {
    X_nonFE <- tryCatch(
      model.matrix(model, data = survey_wd_sim, type = "rhs"),
      error = function(e) {
        warning("[run_sim_pipeline] model.matrix() failed: ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(X_nonFE)) {
      # Column alignment guard — surfaces misalignment bugs immediately
      stopifnot(
        "X_nonFE columns must match chol_obj$beta names" =
          identical(colnames(X_nonFE), names(chol_obj$beta))
      )
      F_loading <- compute_factor_loading(X_nonFE, chol_obj)
    }
  }

  # ---- Training augmentation for residual drawing ------------------------- #
  # train_aug carries .resid for "original" and "resample" residual paths
  # inside aggregate_with_uncertainty(). Computed once per pipeline call.
  train_aug <- tryCatch({
    fitted_train <- as.numeric(stats::predict(model, newdata = train_data))
    train_data |>
      dplyr::mutate(
        .fitted = fitted_train,
        .resid  = !!rlang::sym(so$name) - fitted_train
      )
  }, error = function(e) {
    warning("[run_sim_pipeline] train_aug computation failed: ", conditionMessage(e))
    NULL
  })
  
  # Store prepared weather data for diagnostics plots — has loc_id, int_month
  # joined from svy. Used by plot_weather_density_panel() in mod_2_03.
  weather_prepared <- survey_wd_sim
  rm(survey_wd_sim)

  list(
    y_point     = y_point,
    F_loading   = F_loading,
    sim_year    = sim_year,
    weight      = weight,
    id_vec      = id_vec,
    id_col      = id_col,
    n_pre_join  = n_pre_join,
    weather_raw = weather_raw,
    weather_prepared = weather_prepared,
    train_aug   = train_aug
  )
}

# ---------------------------------------------------------------------------- #
# Simulation date grid                                                         #
# ---------------------------------------------------------------------------- #

#' Build the Date Grid for the Historical Simulation
#'
#' Constructs a vector of first-of-month dates covering every combination of
#' interview month present in `survey_weather` and every year in the requested
#' historical period. These dates are passed directly to `get_weather()` as the
#' `dates` argument.
#'
#' @param survey_weather A data frame containing at least an `int_month` column
#'   (integer, 1-12) representing the interview month of each survey
#'   observation.
#' @param year_range A length-2 integer vector `c(start_year, end_year)`
#'   defining the historical period to simulate over.
#'
#' @return A `Date` vector of first-of-month dates (one per month x year
#'   combination).
#'
#' @examples
#' svy <- data.frame(int_month = c(3L, 6L, 9L))
#' build_hist_sim_dates(svy, c(2000L, 2002L))
#'
#' @export
build_hist_sim_dates <- function(survey_weather, year_range) {
  months <- unique(survey_weather$int_month)
  years  <- seq(year_range[1], year_range[2])

  with(
    expand.grid(int_month = months, int_year = years),
    as.Date(paste(int_year, int_month, "01", sep = "-"))
  )
}

# ---------------------------------------------------------------------------- #
# Residual choice helpers                                                      #
# ---------------------------------------------------------------------------- #

#' Available residual handling choices
#'
#' @return A named character vector suitable for use in `radioButtons()`.
#' @export
residual_choices <- function() {
  c(
    "Original" = "original",
    "Resample"  = "resample",
    "Normal"     = "normal",
    "None"      = "none"
  )
}


# ---------------------------------------------------------------------------- #
# Perturbation method helper                                                   #
# ---------------------------------------------------------------------------- #

#' Build a Perturbation Method Vector for Climate Simulations
#'
#' Derives the named `perturbation_method` vector required by `get_weather()`
#' when an SSP scenario is active. Precipitation and similar accumulation
#' variables (units `"mm"` or `"days"`) use `"multiplicative"` scaling;
#' all other variables (e.g. temperature in `"°C"`) use `"additive"` delta.
#'
#' @param selected_weather A data frame with columns `name` and `units`, as
#'   returned by `build_selected_weather()`.
#'
#' @return A named character vector with one entry per row in
#'   `selected_weather`, values being `"additive"` or `"multiplicative"`.
#'
#' @export
build_perturbation_method <- function(selected_weather) {
  method <- ifelse(
    selected_weather$units %in% c("mm", "days"),
    "multiplicative",
    "additive"
  )
  stats::setNames(method, selected_weather$name)
}


# ---------------------------------------------------------------------------- #
# Weather preparation for simulation                                           #
# ---------------------------------------------------------------------------- #

#' Prepare Historical Weather Data for Simulation
#'
#' Takes raw output from `get_weather()` and joins it back to the survey frame,
#' adding `sim_year` and ensuring `year` is a factor consistent with the
#' training data. Weather and outcome columns from the survey frame are dropped
#' before the join to avoid duplication.
#'
#' @param weather_raw A data frame returned by `get_weather()`, containing at
#'   least columns `code`, `year`, `survname`, `loc_id`, `int_month`, and
#'   `timestamp`.
#' @param survey_weather A data frame of the merged survey-weather training
#'   data. Must contain columns `code`, `year`, `survname`, `loc_id`, and
#'   `int_month`.
#' @param selected_weather A data frame of selected weather variable metadata
#'   with at least a `name` column.
#' @param outcome_name A character string giving the name of the outcome column
#'   in `survey_weather` to exclude before the join.
#'
#' @return A tibble with the weather variables from `weather_raw` joined to the
#'   survey covariate columns from `survey_weather`, with additional columns
#'   `sim_year` (integer) and `year` (factor).
#'
#' @importFrom dplyr mutate select inner_join any_of
#' @export
prepare_hist_weather <- function(weather_raw,
                                 survey_weather,
                                 selected_weather,
                                 outcome_name) {
  drop_cols <- c(selected_weather$name, outcome_name)

    weather_raw |>
    dplyr::mutate(
      year      = as.character(year),
      int_month = as.integer(format(timestamp, "%m")),
      sim_year  = as.integer(format(timestamp, "%Y"))
    ) |>
    dplyr::select(-timestamp) |>
    dplyr::inner_join(
      survey_weather |>
        dplyr::mutate(year = as.character(year)) |>
        dplyr::select(-dplyr::any_of(drop_cols)),
      by = c("code", "year", "survname", "loc_id", "int_month")
    ) |>
    dplyr::mutate(year = as.factor(year))
}


# ---------------------------------------------------------------------------- #
# Back-transformation                                                          #
# ---------------------------------------------------------------------------- #

#' Back-Transform a Log-Transformed Outcome Column
#'
#' Exponentiates the named outcome column in `preds` when `so$transform` is
#' `"log"`. Returns `preds` unchanged for any other transformation or when
#' `so$transform` is `NULL` / `NA`.
#'
#' @param preds A data frame of predictions from `predict_outcome()`.
#' @param so A one-row data frame of outcome metadata as returned by
#'   `build_selected_outcome()`. Must contain columns `name` and `transform`.
#'
#' @return `preds` with the outcome column exponentiated if applicable.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang sym .data
#' @export
apply_log_backtransform <- function(preds, so) {
  if (!isTRUE(so$transform == "log")) return(preds)

  preds |>
    dplyr::mutate(!!rlang::sym(so$name) := exp(.data[[so$name]]))
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
