# Simulation helpers ----
# Pure functions used by mod_2_01.
# Stateless and testable without Shiny.
#
# Shared constants (used across multiple modules):
#   RP_LOW            -- low-tail return period map  (name -> quantile prob)
#   RP_HIGH           -- high-tail return period map (name -> quantile prob)
#   SSP_SHORT_LABELS  -- canonical SSP key -> short display label
#
# Simulation pipeline helper:
#   run_sim_pipeline()  -- weather join -> predict -> back-transform in one call


# Internal colour / style helpers ----
# Used by enhance_exceedance() and plot_pointrange_climate(). Not exported.

# SSP scenario colours (.ssp_colours) live in utils_plot_theme.R together with
# the rest of the shared colour system.


# Normalise any SSP token found in a scenario name to a canonical key
# Strips trailing " / P{pct}" suffix first so keys like "SSP3-7.0 / 2030 / P50"
# are handled identically to the old "SSP3-7.0 / 2030" format.
# e.g. "SSP2" -> "SSP2-4.5", "SSP3-7.0" -> "SSP3-7.0", NA -> NA
.normalise_ssp <- function(nm) {
  nm_clean <- sub(" / P[0-9]+$", "", nm)
  m_full <- regmatches(nm_clean, regexpr("SSP[0-9]-[0-9.]+", nm_clean))
  if (length(m_full) > 0) {
    return(m_full)
  }
  m_short <- regmatches(nm_clean, regexpr("SSP([2-9])", nm_clean))
  if (length(m_short) == 0) {
    return(NA_character_)
  }
  digit <- sub("SSP", "", m_short)
  lookup <- c("2" = "SSP2-4.5", "3" = "SSP3-7.0", "5" = "SSP5-8.5")
  if (digit %in% names(lookup)) lookup[[digit]] else NA_character_
}

.parse_year <- function(nm) {
  m <- regexpr("\\d{4}-\\d{4}", nm)
  out <- regmatches(nm, m)
  out[m == -1L] <- NA_character_
  out
}

# Shared constants ----

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
RP_LOW <- c("1:50" = 0.02, "1:20" = 0.05, "1:10" = 0.10, "1:5" = 0.20)

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

# Coefficient-draw constants and helpers ----

#' Format elapsed seconds into a human-readable string (e.g. "2m 14s")
#' Used by the simulation progress bar.
#' @noRd
format_elapsed <- function(secs) {
  secs <- round(secs)
  if (secs < 60L) {
    return(sprintf("%ds", secs))
  }
  sprintf("%dm %02ds", secs %/% 60L, secs %% 60L)
}

# SE clustering specification - confirmed default: ~loc_id_panel
# Methodological justification: more conservative than ~loc_id:int_month
# (Moulton minimum). Absorbs within-location serial correlation across
# months and years. Weather data has real within-location temporal
# correlation that ~loc_id:int_month does not correct.
#
# Cluster count decision tree:
#   G >= 50 at ~loc_id_panel : use ~loc_id_panel (default)
#   40 <= G < 50             : use ~loc_id_panel, flag in methodology note
#   G < 40                   : warn user, wild cluster bootstrap recommended
#
# Named alternatives - available as constants, not default.
# Use compute_cluster_counts() to check G before switching.
COEF_VCOV_SPEC <- ~loc_id_panel
COEF_VCOV_SPEC_MOULTON <- ~ loc_id_panel:int_month

#' Compute Cluster Counts for SE Specification Diagnostics
#'
#' Computes the number of clusters at each relevant clustering level for the
#' fitted model data. Used to validate the SE specification and warn when
#' cluster counts are too small for reliable asymptotic inference.
#'
#' @param data Data frame used at model fit time. Must contain \code{loc_id_panel}
#'   and \code{int_month} columns. Optionally \code{code}, \code{year},
#'   \code{survname} for the conservative multi-way spec.
#'
#' @return Named list with integer cluster counts:
#'   \describe{
#'     \item{loc_id_panel}{Number of distinct panel location clusters (primary spec).}
#'     \item{loc_id_panel_int_month}{Number of distinct panel location * month clusters
#'       (Moulton minimum).}
#'     \item{conservative}{Number of distinct code * year * survname * loc_id_panel
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

  g_loc <- if (has_cols("loc_id_panel")) {
    dplyr::n_distinct(data[["loc_id_panel"]])
  } else {
    NA_integer_
  }

  g_loc_month <- if (has_cols("loc_id_panel", "int_month")) {
    dplyr::n_distinct(paste(data[["loc_id_panel"]], data[["int_month"]]))
  } else {
    NA_integer_
  }

  g_conserv <- if (has_cols("code", "year", "survname", "loc_id_panel")) {
    dplyr::n_distinct(paste(
      data[["code"]], data[["year"]],
      data[["survname"]], data[["loc_id_panel"]]
    ))
  } else {
    NA_integer_
  }

  counts <- list(
    loc_id_panel           = g_loc,
    loc_id_panel_int_month = g_loc_month,
    conservative           = g_conserv
  )

  # Runtime warnings - surface immediately at model fit time
  if (!is.na(g_loc) && g_loc < 40L) {
    warning(sprintf(
      "[SE] Only %d clusters at ~loc_id_panel. VCV SEs may be unreliable. Wild cluster bootstrap recommended (not yet implemented).",
      g_loc
    ))
  } else if (!is.na(g_loc) && g_loc < 50L) {
    message(sprintf(
      "[SE] %d clusters at ~loc_id_panel - borderline. Flag in methodology note.",
      g_loc
    ))
  }

  counts
}

# Cholesky uncertainty propagation ----

#' Compute Cholesky Factor of Model VCV Matrix
#'
#' Computes the lower-triangular Cholesky factor of the coefficient covariance
#' matrix for the non-fixed-effect coefficients of a fitted \code{fixest}
#' model. Used once per fitted model - not per weather key.
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
#'   \code{\link{aggregate_with_uncertainty_delta}}
#' @export
compute_chol_vcov <- function(fit, vcov_spec = COEF_VCOV_SPEC) {
  # fixest_multi: iterate over sub-models (needed for RIF quantile fits)
  if (inherits(fit, "fixest_multi")) {
    return(lapply(seq_along(fit), function(i) {
      compute_chol_vcov(fit[[i]], vcov_spec = vcov_spec)
    }))
  }

  stopifnot("fit must be a fixest model" = inherits(fit, "fixest"))
  beta <- stats::coef(fit)

  # Try fit-time VCV first (respects cluster= passed at estimation), then
  # requested spec, then fallback chain
  vcov_fallbacks <- list(vcov_spec, ~loc_id, "HC1", "iid")
  Sigma <- tryCatch(stats::vcov(fit), error = function(e) NULL)
  if (is.null(Sigma) || !all(is.finite(Sigma))) {
    Sigma <- NULL
    for (spec in vcov_fallbacks) {
      Sigma <- tryCatch(
        stats::vcov(fit, vcov = spec),
        error = function(e) NULL
      )
      if (!is.null(Sigma) && all(is.finite(Sigma))) {
        message("[compute_chol_vcov] fell back to vcov spec: ", deparse(spec))
        break
      }
      Sigma <- NULL
    }
  }

  if (is.null(Sigma)) {
    stop("[compute_chol_vcov] all vcov specs failed - cannot compute Sigma.")
  }

  L <- tryCatch(
    t(chol(Sigma)),
    error = function(e) {
      stop(
        "[compute_chol_vcov] Cholesky decomposition failed: ",
        conditionMessage(e)
      )
    }
  )
  list(L = L, K = nrow(L), beta = beta, spec = vcov_spec)
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
#' @param chol_obj Named list returned by \code{compute_chol_vcov()}. May
#'   include an optional `active_mask` logical vector of length K; when
#'   present, columns where `active_mask == FALSE` are dropped from the
#'   returned matrix (additive-decomposition SE; see
#'   \code{build_active_coef_mask()}).
#'
#' @return Numeric matrix of dimensions N \eqn{\times} K (or N \eqn{\times}
#'   K_active if `active_mask` is set). Each row \eqn{i} is the factor
#'   loading vector for household \eqn{i}.
#'
#' @seealso \code{\link{compute_chol_vcov}},
#'   \code{\link{aggregate_with_uncertainty_delta}},
#'   \code{\link{build_active_coef_mask}}
#' @export
compute_factor_loading <- function(X_nonFE, chol_obj) {
  stopifnot(
    "X_nonFE must be a numeric matrix" = is.matrix(X_nonFE) && is.numeric(X_nonFE),
    "chol_obj must contain L and beta" = all(c("L", "beta") %in% names(chol_obj)),
    "chol_obj$beta must be a named numeric vector" =
      is.numeric(chol_obj$beta) && !is.null(names(chol_obj$beta)),
    "X_nonFE columns must match chol_obj$beta names" =
      length(intersect(colnames(X_nonFE), names(chol_obj$beta))) > 0L
  )

  X_nonFE <- align_factor_loading_matrix(X_nonFE, names(chol_obj$beta))

  stopifnot(
    "X_nonFE columns must match chol_obj$beta names" =
      identical(colnames(X_nonFE), names(chol_obj$beta))
  )

  # Additive-decomposition SE: when an active mask is set, build F_loading
  # from the *active block of Sigma* - i.e. F = X_active %*% L_active where
  # L_active = chol(Sigma[mask, mask]). This is mathematically distinct
  # from (and smaller than) F[, mask] subset, which can pick up
  # off-diagonal Sigma contributions from inactive coefficients.
  active_mask <- chol_obj$active_mask
  if (!is.null(active_mask) && !is.null(chol_obj$L_active) &&
    length(active_mask) == ncol(X_nonFE)) {
    return(X_nonFE[, active_mask, drop = FALSE] %*% chol_obj$L_active)
  }

  # Legacy: F = X %*% L (N * K).
  X_nonFE %*% chol_obj$L
}


# `model.matrix()` can omit valid coefficient columns when a prediction slice
# has no observations for a factor level, and some model classes return the
# same columns in a different order. Align by coefficient name before applying
# the VCV factor so coefficient uncertainty remains attached to the right term.
align_factor_loading_matrix <- function(X_nonFE, beta_names) {
  stopifnot(
    "X_nonFE must be a numeric matrix" = is.matrix(X_nonFE) && is.numeric(X_nonFE),
    "beta_names must be non-empty and unique" =
      length(beta_names) > 0L && !anyDuplicated(beta_names)
  )

  x_names <- colnames(X_nonFE)
  if (is.null(x_names) || anyDuplicated(x_names)) {
    stop("Prediction design matrix must have unique column names.", call. = FALSE)
  }

  if (!length(intersect(x_names, beta_names))) {
    stop("Prediction design matrix has no columns matching fitted coefficients.",
      call. = FALSE
    )
  }

  # The common prediction path already emits coefficient-order columns. Avoid
  # the subset/reorder allocation when the names are an exact match.
  if (identical(x_names, beta_names)) {
    return(X_nonFE)
  }

  common <- intersect(beta_names, x_names)
  aligned <- X_nonFE[, common, drop = FALSE]
  missing <- setdiff(beta_names, common)
  if (length(missing)) {
    aligned <- cbind(
      aligned,
      matrix(0,
        nrow = nrow(X_nonFE), ncol = length(missing),
        dimnames = list(NULL, missing)
      )
    )
  }

  aligned[, beta_names, drop = FALSE]
}


# Simulation pipeline helper ----

#' Resolve the ID Column for Residual Matching
#'
#' Returns the first of `c("pid", "hhid", "fid")` that exists in both data
#' frames, or `NULL` if none match.  Used by `run_sim_pipeline()` to enable
#' ID-based residual matching when `residuals = "original"`.
#' @noRd
resolve_id_col <- function(a, b) {
  candidates <- c("pid", "hhid", "fid")
  shared <- intersect(names(a), names(b))
  match <- candidates[candidates %in% shared]
  if (length(match) == 0L) NULL else match[[1L]]
}

.prediction_profile_enabled <- function() {
  identical(tolower(Sys.getenv("WISEAPP_PREDICTION_PROFILE", "")), "1")
}

.prediction_profile_record <- function(profile, stage, started, value = NULL,
                                       rows = NA_integer_, detail = NULL) {
  if (is.null(profile)) return(invisible(NULL))
  rss <- if (exists(".wx_process_tree_rss_bytes", mode = "function")) {
    .wx_process_tree_rss_bytes()
  } else {
    NA_real_
  }
  profile$records[[length(profile$records) + 1L]] <- data.frame(
    stage = stage,
    elapsed_seconds = proc.time()[["elapsed"]] - started,
    rows = if (is.null(value)) rows else if (is.data.frame(value)) nrow(value) else rows,
    frame_bytes = if (is.null(value)) NA_real_ else as.numeric(utils::object.size(value)),
    rss_bytes = rss,
    detail = detail %||% "",
    stringsAsFactors = FALSE
  )
  invisible(NULL)
}

#' Run Simulation Pipeline for One Weather Key
#'
#' Prepares counterfactual survey data for one weather key, computes point-
#' estimate log-welfare predictions, and returns the factor loading matrix for
#' downstream coefficient uncertainty propagation via
#' \code{aggregate_with_uncertainty_delta()}.
#'
#' This function is called once per weather key (historical + future
#' representatives). It does NOT draw coefficient perturbations - all
#' uncertainty propagation is deferred to display time via
#' \code{aggregate_with_uncertainty_delta()}, making poverty line, weights, and
#' aggregation method fully reactive without re-simulation.
#'
#' @param weather_raw Data frame. One weather key's prepared data from
#'   \code{get_weather()}.
#' @param svy Data frame. Survey microdata joined to weather reference data.
#' @param sw One-row data frame of selected weather variable metadata.
#' @param so One-row data frame of selected outcome variable metadata.
#' @param model Fitted \code{fixest} model object.
#' @param residuals Character. Residual treatment passed through to
#'   \code{aggregate_with_uncertainty_delta()}. One of \code{"none"},
#'   \code{"original"}, \code{"normal"}, \code{"resample"}.
#' @param train_data Data frame. Training data used to fit \code{model}.
#'   Used to compute training residuals for \code{train_aug}.
#' @param engine Character. Model engine identifier (e.g. \code{"fixest"}).
#' @param chol_obj Named list from \code{compute_chol_vcov()} or \code{NULL}.
#'   When \code{NULL}, \code{F_loading} in the return value is \code{NULL}
#'   (point estimates only - no coefficient uncertainty).
#' @param precomputed_train_aug Data frame or \code{NULL}. When supplied,
#'   used directly as \code{train_aug} in the return value instead of
#'   recomputing \code{predict(model, train_data)} per call. Passed by
#'   \code{fct_run_simulation()} to avoid redundant work across keys.
#' @param precomputed_ecdf_train Optional pre-built \code{stats::ecdf()} of
#'   the training outcome, used only on the RIF path. \code{train_data} is
#'   identical across simulation keys for a given model fit, so
#'   \code{fct_run_simulation()} builds this once and passes it through to
#'   \code{predict_rif()} instead of rebuilding the same ecdf per key
#'   (see PERF-27). When \code{NULL}, \code{predict_rif()} builds it itself.
#' @param svy_prepared Data frame or \code{NULL}. Pre-prepared survey data
#'   with weather/outcome columns dropped and year converted to character.
#'   Skips redundant per-key column manipulation in the weather join.
#' @param chol_Sigma Deprecated alias of \code{chol_obj} (older Step 2
#'   outputs stored the Cholesky factor under this name). Accepted for
#'   compatibility; \code{chol_obj} takes precedence.
#' @param slim Accepted for backwards compatibility; ignored.
#' @param fit_multi Optional \code{fixest_multi} object of per-quantile RIF
#'   fits. Activates the RIF path together with \code{taus} and
#'   \code{weather_cols}.
#' @param taus Numeric vector. Quantile levels used on the RIF path.
#' @param weather_cols Character vector. Weather column names used to build
#'   the RIF design matrix.
#' @param svy_baseline Data frame or \code{NULL}. Baseline (pre-policy)
#'   survey-weather frame; on the RIF policy path, coefficient deltas are
#'   computed against this frame.
#' @param rif_grid Optional tidy data frame of RIF beta curves (from
#'   \code{fit_model()}), attached to the pipeline output for diagnostics.
#' @param rif_policy_deltas Optional precomputed RIF policy covariate deltas.
#'   Reused across weather keys when supplied.
#'
#' @return Named list or \code{NULL} on prediction failure:
#'   \describe{
#'     \item{y_point}{Numeric vector length N. Log-scale point-estimate welfare
#'       predictions. Back-transformation via \code{exp()} happens inside
#'       \code{aggregate_with_uncertainty_delta()}, not here.}
#'     \item{F_loading}{N \eqn{\times} K numeric matrix from
#'       \code{compute_factor_loading()}, or \code{NULL} when
#'       \code{chol_obj = NULL}.}
#'     \item{sim_year}{Integer vector length N. Simulation year per row.}
#'     \item{weather_exposure}{Compact (schema 2) exact weather exposure recipe and
#'       prediction-row mapping, or explicit unavailable status if prediction
#'       output loses its row identity.}
#'     \item{weight}{Numeric vector length N or \code{NULL}. Survey weights.}
#'     \item{id_vec}{Vector length N or \code{NULL}. Household IDs for
#'       \code{residuals = "original"} matching.}
#'     \item{id_col}{Character or \code{NULL}. Name of the ID column.}
#'     \item{n_pre_join}{Integer. Number of survey rows before weather join.}
#'     \item{weather_raw}{Data frame. The input weather key data (for
#'       diagnostics).}
#'     \item{train_aug}{Data frame. Training data augmented with \code{.fitted}
#'       and \code{.resid} columns for residual drawing in
#'       \code{aggregate_with_uncertainty_delta()}.}
#'   }
#'
#' @seealso \code{\link{aggregate_with_uncertainty_delta}},
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
                             chol_obj = NULL,
                             chol_Sigma = NULL, # golem compat alias
                             slim = FALSE, # accepted, ignored
                             # RIF
                             fit_multi = NULL,
                             taus = NULL,
                             weather_cols = NULL,
                             precomputed_train_aug = NULL,
                             svy_prepared = NULL,
                             weather_join_cache = NULL,
                             batch_rif_predictions = FALSE,
                             direct_rif_predictions = FALSE,
                             direct_rif_metadata = NULL,
                             direct_rif_baseline_cache = NULL,
                             svy_baseline = NULL,
                             rif_policy_deltas = NULL,
                             rif_grid = NULL,
                             precomputed_ecdf_train = NULL) {
  prediction_profile <- if (.prediction_profile_enabled()) new.env(parent = emptyenv()) else NULL
  if (!is.null(prediction_profile)) {
    prediction_profile$records <- list()
    prediction_profile$started <- proc.time()[["elapsed"]]
  }
  profile_stage <- function(stage, expr, rows = NA_integer_, detail = NULL) {
    started <- proc.time()[["elapsed"]]
    value <- force(expr)
    .prediction_profile_record(prediction_profile, stage, started, value, rows, detail)
    value
  }
  n_pre_join <- nrow(svy)

  # Define is_rif once - all conditions in one place
  is_rif <- identical(engine, "rif") &&
    !is.null(fit_multi) &&
    !is.null(taus) &&
    !is.null(weather_cols)

  # RIF policy mode: caller supplied an svy_baseline so we can separate the
  # baseline (no-policy) RIF prediction from the policy net level effect.
  # We predict against svy_baseline and then add the decomposition's
  # delta_total - matching the Decomposition pane's totals exactly. The
  # OLS path doesn't need this because predict_outcome() naturally picks up
  # the policy level shift from the policy-modified design matrix.
  is_rif_policy <- is_rif && !is.null(svy_baseline) &&
    !is.null(rif_grid) && !is.null(train_data)
  svy_for_predict <- if (is_rif_policy) svy_baseline else svy

  # Tag svy with row IDs so downstream consumers can broadcast per-household
  # quantities (RIF delta correction; Module 3 policy-delta application) back
  # to the (HH x year) rows produced by prepare_hist_weather().
  svy_for_predict$.svy_row_id <- seq_len(nrow(svy_for_predict))

  # Use pre-prepared survey (columns already dropped, year converted) when
  # available. RIF policy mode and Module 3 callers prepare svy differently,
  # so fall back to full prepare_hist_weather() when svy_prepared is NULL.
  if (!is.null(svy_prepared)) {
    svy_join <- svy_prepared
    svy_join$.svy_row_id <- svy_for_predict$.svy_row_id
  } else {
    drop_cols <- c(sw$name, so$name)
    svy_join <- svy_for_predict |>
      dplyr::mutate(year = as.character(year)) |>
      dplyr::select(-dplyr::any_of(drop_cols))
  }

  # Keep a narrow locator to the exact prepared weather rows used below.
  # IDs are assigned before either join path so duplicate timestamps remain
  # distinguishable and cached/inline joins share the same mapping contract.
  weather_exposure_table <- .policy_exposure_table(
    weather_raw, weather_columns = unique(c(sw$name, weather_cols))
  )
  weather_join <- weather_raw
  weather_join$.policy_exposure_id <- seq_len(nrow(weather_join))

  survey_wd_sim <- profile_stage("join", if (!is.null(weather_join_cache) && !is_rif_policy) {
    join_weather_survey_cached(weather_join, weather_join_cache)
  } else {
    weather_join |>
      .add_sim_timestamp_fields() |>
      dplyr::select(-timestamp) |>
      dplyr::inner_join(svy_join, by = c("code", "year", "survname", "loc_id", "int_month")) |>
      dplyr::mutate(year = as.factor(year))
  }, detail = if (!is.null(weather_join_cache) && !is_rif_policy) "compact_cache" else "inline")
  rm(svy_join)
  survey_wd_sim$.policy_prediction_row_id <- seq_len(nrow(survey_wd_sim))

  # Resolve ID column for "original" residual matching
  id_col <- if (residuals == "original") {
    resolve_id_col(train_data, survey_wd_sim)
  } else {
    NULL
  }

  # Prediction - dispatch on engine ----

  out <- if (is_rif) {
    # RIF path - quantile delta method
    # Use chol_obj (our format) or chol_Sigma (golem format) for RIF
    chol_src <- if (!is.null(chol_obj)) chol_obj else chol_Sigma

    # For RIF: chol_src is list of list(L, K, beta, spec [, L_active]) per tau.
    # interpolate_F_loading() needs list of L matrices only.
    # When an active mask is set, we extract L_active (Cholesky of the
    # weather/policy block of Sigma) instead of the full L, so that
    # F_loading = X[, active] %*% L_active produces the correct
    # additive-decomposition variance (h' X_w Sigma_ww X_w' h). Subsetting
    # columns of F = X %*% L_full would be incorrect when Sigma has
    # off-diagonal terms between active and inactive coefficients.
    chol_list <- if (!is.null(chol_src) && is.list(chol_src) &&
      !("L" %in% names(chol_src))) {
      active_mask <- attr(chol_src, "active_mask")
      use_active <- !is.null(active_mask) &&
        all(vapply(
          chol_src,
          function(x) "L_active" %in% names(x),
          logical(1)
        ))
      tmp <- lapply(chol_src, function(x) {
        if (use_active && is.matrix(x$L_active)) {
          x$L_active
        } else if (is.list(x) && "L" %in% names(x)) {
          x$L
        } else if (is.matrix(x)) {
          x
        } else {
          NULL
        }
      })
      if (use_active) attr(tmp, "active_mask") <- active_mask
      tmp
    } else {
      NULL
    }
    profile_stage("prediction", predict_rif(
      fit_multi = fit_multi,
      newdata = survey_wd_sim, # joined,
      svy = svy_for_predict,
      train_data = train_data,
      taus = taus,
      outcome = so$name,
      weather_cols = weather_cols,
      so = so,
      chol_list = chol_list,
      ecdf_train = precomputed_ecdf_train,
      batch_predictions = batch_rif_predictions,
      direct_predictions = direct_rif_predictions,
      direct_metadata = direct_rif_metadata,
      direct_baseline_cache = direct_rif_baseline_cache,
      prediction_profile = prediction_profile
    ), detail = "rif")
  } else {
    # Standard OLS path - unchanged
    tryCatch(
      profile_stage("prediction", predict_outcome(
        model      = model,
        newdata    = survey_wd_sim,
        residuals  = "none", # residuals drawn at display time, not here
        outcome    = so$name,
        id         = id_col,
        train_data = train_data,
        engine     = engine
      ), detail = "ols"),
      error = function(e) {
        warning("[run_sim_pipeline] predict_outcome() failed: ", conditionMessage(e))
        NULL
      }
    )
  }

  if (is.null(out)) {
    rm(survey_wd_sim)
    return(NULL)
  }

  # y_point stays log-scale - back-transformation happens inside
  # aggregate_with_uncertainty_delta() after coefficient perturbation.
  y_point <- out$.fitted

  # RIF policy correction ----
  # In RIF policy mode the prediction above was made against svy_baseline,
  # so y_point currently holds the *baseline-x* level (matching what Mod 2's
  # Step 2 hist_sim shows). Add the decomposition's delta_total - which
  # includes delta_main (SP + Beta_x.Delta_x), delta_res1 (repositioning),
  # and delta_res2 (Beta_int.haz.Delta_x) - so the Results pane reflects the
  # net policy effect on the welfare level. This skips the level-scale SP
  # block below because delta_sp is already inside delta_total.
  if (is_rif_policy) {
    corr <- .compute_rif_policy_correction(
      svy_baseline = svy_baseline,
      svy_policy   = svy,
      weather_raw  = weather_raw,
      weather_cols = weather_cols,
      rif_grid     = rif_grid,
      taus         = taus,
      train_data   = train_data,
      outcome      = so$name,
      is_log       = isTRUE(so$transform == "log"),
      deltas       = rif_policy_deltas
    )
    # corr is one entry per household (nrow(svy_baseline)); broadcast it to
    # each expanded survey*weather row via .svy_row_id (set by predict_rif()
    # on `out`). Without this, hist_sim has y_point per (HH * month/year)
    # but corr is per HH, so the lengths mismatch and the correction would
    # be silently dropped.
    if (!is.null(corr) && ".svy_row_id" %in% names(out) &&
      length(corr) == nrow(svy_baseline)) {
      y_point <- y_point + corr[out$.svy_row_id]
    } else {
      warning(
        "[run_sim_pipeline] RIF policy correction unavailable; ",
        "policy y_point will reflect weather-sensitivity changes only."
      )
    }
  }

  # SP cash transfer (post-prediction, level scale) ----
  # SP_TRANSFER_COL is set on svy by apply_policy_to_svy() in fct_policy_sim.R.
  # The transfer is a direct welfare boost, not a regression covariate, so it
  # is added after prediction. To stay consistent with the decomposition
  # (.decompose_ols / .compute_rif_channels in fct_policy_decompose.R, which
  # define delta_sp = log(exp(y) + sp) - y), the boost is applied on the level
  # scale and re-logged when so$transform == "log".
  #
  # Skipped in RIF policy mode because delta_sp is already inside the
  # correction added above. (svy_for_predict = svy_baseline carries no
  # SP_TRANSFER_COL, so `out` wouldn't have it anyway - the guard is
  # defensive.)
  sp_vec <- if (!is_rif_policy && SP_TRANSFER_COL %in% names(out)) {
    out[[SP_TRANSFER_COL]]
  } else {
    NULL
  }
  if (!is.null(sp_vec) && any(sp_vec > 0, na.rm = TRUE)) {
    is_log <- isTRUE(so$transform == "log")
    if (is_log) {
      welfare_lvl <- exp(y_point) + sp_vec
      y_point <- log(pmax(welfare_lvl, .Machine$double.eps))
    } else {
      y_point <- y_point + sp_vec
    }
  }

  # Simulation year and weights ----
  sim_year <- out$sim_year

  weight <- if ("weight" %in% names(out)) out$weight else NULL

  id_vec <- if (!is.null(id_col) && id_col %in% names(out)) {
    out[[id_col]]
  } else {
    NULL
  }

  # Pipeline row -> baseline household lookup. Used by Module 3 to broadcast
  # per-household policy deltas (decompose_policy_effect output, indexed by
  # baseline survey row) onto the expanded (HH x year) prediction rows.
  svy_row_id <- if (".svy_row_id" %in% names(out)) out$.svy_row_id else NULL
  weather_exposure <- .policy_exposure_mapping(
    out = out,
    table = weather_exposure_table,
    n_joined = nrow(survey_wd_sim),
    sim_year = sim_year,
    weight = weight,
    id_vec = id_vec,
    id_col = id_col
  )

  # Factor loading matrix ----
  # Computed once per key - not per draw.
  # F_loading = X_nonFE %*% L  where L is the Cholesky factor of Sigma.
  # NULL when chol_obj = NULL (point estimates only).
  #
  # PERF-32: `out` duplicates the joined prediction frame column-for-column
  # (plus .fitted). Every vector the return value needs is extracted above,
  # and the RIF F_loading attribute is captured first, so `out` is released
  # before the N x K design/factor matrices are allocated - the peak-memory
  # moment of this function. X_nonFE is dropped as soon as F_loading exists,
  # and survey_wd_sim right after the block (train_aug does not read it).
  F_loading <- NULL
  if (is_rif) {
    # RIF path - F_loading computed inside predict_rif() via interpolate_F_loading()
    F_loading <- attr(out, "F_loading")
    rm(out)
  } else {
    rm(out)
    if (!is.null(chol_obj)) {
      # Standard OLS path only - skip for RIF (model is fixest_multi)
      X_nonFE <- profile_stage("design_matrix", tryCatch(
        stats::model.matrix(model, data = survey_wd_sim, type = "rhs"),
        error = function(e) {
          warning("[run_sim_pipeline] model.matrix() failed: ", conditionMessage(e))
          NULL
        }
      ), detail = "ols")
      if (!is.null(X_nonFE)) {
        if (is.list(chol_obj) && "L" %in% names(chol_obj)) {
          # Our named list format - use compute_factor_loading()
          F_loading <- profile_stage("factor_loading",
            compute_factor_loading(X_nonFE, chol_obj), detail = "ols")
        } else if (is.matrix(chol_obj)) {
          # Golem matrix format - inline multiply
          F_loading <- profile_stage("factor_loading",
            X_nonFE %*% t(chol_obj), detail = "ols")
        }
      }
      rm(X_nonFE)
    }
  }

  rm(survey_wd_sim)

  # Training augmentation for residual drawing ----
  # train_aug carries .resid for "original" and "resample" residual paths
  # inside aggregate_with_uncertainty_delta(). Identical across keys - prefer
  # the precomputed version when available; fall back to per-call computation
  # for backward compat (Module 3 callers that don't precompute).
  train_aug <- if (is_rif) {
    NULL
  } else if (!is.null(precomputed_train_aug)) {
    precomputed_train_aug
  } else {
    tryCatch(
      {
        fitted_train <- as.numeric(stats::predict(model, newdata = train_data))
        train_data |>
          dplyr::mutate(
            .fitted = fitted_train,
            .resid  = !!rlang::sym(so$name) - fitted_train
          )
      },
      error = function(e) {
        warning(
          "[run_sim_pipeline] train_aug computation failed: ",
          conditionMessage(e)
        )
        NULL
      }
    )
  }

  result <- list(
    y_point     = y_point,
    F_loading   = F_loading,
    sim_year    = sim_year,
    weight      = weight,
    id_vec      = id_vec,
    id_col      = id_col,
    svy_row_id  = svy_row_id,
    weather_exposure = .policy_exposure_compact(weather_exposure),
    n_pre_join  = n_pre_join,
    weather_raw = weather_raw,
    train_aug   = train_aug
  )
  if (!is.null(prediction_profile)) {
    .prediction_profile_record(
      prediction_profile, "assembly", prediction_profile$started,
      result, detail = if (is_rif) "rif" else "ols"
    )
    attr(result, "prediction_profile") <- do.call(rbind, prediction_profile$records)
  }
  result
}

.policy_exposure_key_cols <- c(
  ".policy_exposure_id", "code", "year", "survname", "loc_id",
  "int_month", "sim_year", "timestamp"
)

.policy_exposure_table <- function(weather_raw, weather_columns = NULL,
                                   ts_parts = NULL) {
  if (!is.data.frame(weather_raw) || !all(c(
    "code", "year", "survname", "loc_id", "timestamp"
  ) %in% names(weather_raw))) {
    return(NULL)
  }
  derived <- .add_sim_timestamp_fields(weather_raw, ts_parts)
  key_cols <- .policy_exposure_key_cols
  weather_cols <- intersect(weather_columns %||% character(), names(weather_raw))
  weather_cols <- setdiff(weather_cols, c(
    "code", "year", "survname", "loc_id", "timestamp"
  ))
  out <- derived[, unique(c(key_cols[key_cols %in% names(derived)], weather_cols)),
    drop = FALSE
  ]
  out$.policy_exposure_id <- seq_len(nrow(out))
  out
}

.policy_exposure_mapping <- function(out, table, n_joined, sim_year,
                                     weight, id_vec, id_col) {
  unavailable <- function(reason) {
    list(status = "unavailable", available = FALSE, reason = reason, table = table)
  }
  if (is.null(table)) return(unavailable("weather exposure table unavailable"))
  required <- c(".policy_exposure_id", ".policy_prediction_row_id", ".svy_row_id")
  if (!all(required %in% names(out))) {
    return(unavailable("prediction output lost weather or survey row identity"))
  }
  exposure_id <- out$.policy_exposure_id
  prediction_row_id <- out$.policy_prediction_row_id
  svy_row_id <- out$.svy_row_id
  n <- nrow(out)
  if (length(exposure_id) != n || length(prediction_row_id) != n ||
    length(svy_row_id) != n || length(sim_year) != n ||
    (!is.null(weight) && length(weight) != n) ||
    (!is.null(id_vec) && length(id_vec) != n)) {
    return(unavailable("prediction row metadata is not aligned"))
  }
  integral <- function(x) is.numeric(x) && all(is.finite(x)) &&
    all(x == as.integer(x))
  if (!integral(exposure_id) || !integral(prediction_row_id) ||
    !integral(svy_row_id)) {
    return(unavailable("prediction row metadata contains nonintegral identifiers"))
  }
  row_index <- as.integer(exposure_id)
  if (anyNA(row_index) || any(row_index < 1L | row_index > nrow(table)) ||
    anyNA(prediction_row_id) || any(prediction_row_id < 1L |
      prediction_row_id > n_joined) || anyDuplicated(prediction_row_id) ||
    anyNA(svy_row_id) || anyNA(sim_year)) {
    return(unavailable("prediction row metadata contains invalid identifiers"))
  }
  list(
    status = "ok",
    available = TRUE,
    reason = NULL,
    table = table,
    row_index = row_index,
    prediction_row_id = as.integer(prediction_row_id),
    svy_row_id = svy_row_id,
    sim_year = sim_year,
    weight = weight,
    id_vec = id_vec,
    id_col = id_col
  )
}

# Compact exposure (schema 2) ----
# The full mapping is about 28 MB per climate-model pipeline: its `table` is a
# deterministic function of the pipeline's own `weather_raw`, and svy_row_id /
# sim_year / weight / id_vec duplicate the pipeline's fields. Pipelines store
# only the recipe; consumers rebuild the exact schema-1 mapping on demand with
# step2_exposure_resolve(). See review/step2_weather_exposure_dedup_plan.md.
.policy_exposure_compact <- function(mapping) {
  if (!is.list(mapping) || !identical(mapping$status, "ok") ||
    !is.data.frame(mapping$table)) {
    if (is.list(mapping)) mapping$table <- NULL
    return(mapping)
  }
  list(
    schema = 2L,
    status = "ok",
    available = TRUE,
    reason = NULL,
    row_index = mapping$row_index,
    prediction_row_id = mapping$prediction_row_id,
    id_col = mapping$id_col,
    weather_columns = setdiff(names(mapping$table), .policy_exposure_key_cols),
    table_class = class(mapping$table),
    table_rows = nrow(mapping$table)
  )
}

#' Rebuild a pipeline's exact schema-1 weather exposure mapping
#'
#' Schema-1 mappings (and unavailable ones) pass through unchanged. For
#' schema 2 the exposure table is rebuilt from the pipeline's `weather_raw`,
#' resolved against `owner` (the scenario that owns shared or referenced
#' weather). `cache`, an environment shared across the members of one owner,
#' reuses the timestamp-derived fields when timestamps are identical.
#' @noRd
step2_exposure_resolve <- function(pipeline, owner = NULL, cache = NULL) {
  ex <- pipeline$weather_exposure
  if (!is.list(ex) || !identical(ex$schema, 2L) || !identical(ex$status, "ok")) {
    return(ex)
  }
  unavailable <- function(reason) {
    list(status = "unavailable", available = FALSE, reason = reason)
  }
  weather <- tryCatch(step2_resolve_weather(pipeline$weather_raw, owner),
    error = function(e) NULL
  )
  if (!is.data.frame(weather) || !identical(nrow(weather), ex$table_rows) ||
    !all(ex$weather_columns %in% names(weather))) {
    return(unavailable("weather exposure source unavailable"))
  }
  ts_parts <- NULL
  if (is.environment(cache)) {
    if (identical(cache$timestamp, weather$timestamp)) {
      ts_parts <- cache$ts_parts
    } else {
      ts_parts <- .sim_timestamp_parts(weather$timestamp)
      cache$timestamp <- weather$timestamp
      cache$ts_parts <- ts_parts
    }
  }
  table <- .policy_exposure_table(weather, ex$weather_columns, ts_parts = ts_parts)
  table <- if ("tbl_df" %in% ex$table_class) {
    tibble::as_tibble(table)
  } else {
    as.data.frame(table)
  }
  if (!identical(class(table), ex$table_class)) {
    return(unavailable("weather exposure table could not be rebuilt"))
  }
  list(
    status = "ok",
    available = TRUE,
    reason = NULL,
    table = table,
    row_index = ex$row_index,
    prediction_row_id = ex$prediction_row_id,
    svy_row_id = pipeline$svy_row_id,
    sim_year = pipeline$sim_year,
    weight = pipeline$weight,
    id_vec = pipeline$id_vec,
    id_col = ex$id_col
  )
}

# Simulation date grid ----

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
  months <- months[!is.na(months)]
  years <- seq(year_range[1], year_range[2])

  with(
    expand.grid(int_month = months, int_year = years),
    as.Date(paste(int_year, int_month, "01", sep = "-"))
  )
}

# Residual choice helpers ----

#' Available residual handling choices
#'
#' 'Normal' is deliberately not offered: drawing residuals from N(0, sigma)
#' assumes normal tails and homoskedasticity, which the other options avoid.
#' 'None' is likewise not offered: it is a diagnostic mode (fitted values
#' only) and understates variance - it remains available internally for the
#' RIF engine, which needs no residual draw.
#'
#' @return A named character vector suitable for use in `radioButtons()`.
#' @export
residual_choices <- function() {
  c(
    "Original" = "original",
    "Resample" = "resample"
  )
}


# Perturbation method helper ----

#' Build a Perturbation Method Vector for Climate Simulations
#'
#' Derives the named `perturbation_method` vector required by `get_weather()`
#' when an SSP scenario is active. Precipitation and similar accumulation
#' variables (units `"mm"` or `"days"`) use `"multiplicative"` scaling;
#' all other variables (e.g. temperature in `"degC"`) use `"additive"` delta.
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


# Weather preparation for simulation ----

#' Add Simulation Month/Year Fields Derived from `timestamp`
#'
#' Converts `timestamp` to `Date`-character `year`, plus integer `int_month`
#' and `sim_year` extracted in a single `as.POSIXlt()` pass. POSIXlt truncates
#' fractional seconds, so timestamps cannot roll across a month/year boundary
#' the way `format()` rounding could.
#'
#' @param df A data frame with a `timestamp` column.
#' @param ts_parts Optional precomputed `.sim_timestamp_parts(df$timestamp)`.
#'
#' @return `df` with `year` as character and `int_month`/`sim_year` integers.
#' @noRd
.add_sim_timestamp_fields <- function(df, ts_parts = NULL) {
  ts_parts <- ts_parts %||% .sim_timestamp_parts(df$timestamp)
  dplyr::mutate(
    df,
    year      = as.character(year),
    int_month = ts_parts$int_month,
    sim_year  = ts_parts$sim_year
  )
}

# The POSIXlt conversion dominates exposure-table rebuilds; members of one
# scenario share timestamps, so callers may compute this once and reuse it.
.sim_timestamp_parts <- function(timestamp) {
  ts_lt <- as.POSIXlt(timestamp)
  list(
    int_month = as.integer(ts_lt$mon + 1L),
    sim_year  = as.integer(ts_lt$year + 1900L)
  )
}

.weather_join_key <- function(df, by) {
  parts <- lapply(df[by], function(x) {
    x <- as.character(x)
    x[is.na(x)] <- "\001"
    x
  })
  do.call(paste, c(parts, sep = "\002"))
}

build_weather_join_cache <- function(survey_join,
                                     by = c(
                                       "code", "year", "survname",
                                       "loc_id", "int_month"
                                     )) {
  stopifnot(is.data.frame(survey_join), all(by %in% names(survey_join)))
  list(
    by = by,
    survey = survey_join,
    survey_nonjoin = setdiff(names(survey_join), by),
    lookup = split(seq_len(nrow(survey_join)),
      .weather_join_key(survey_join, by),
      drop = TRUE
    )
  )
}

join_weather_survey_cached <- function(weather_raw, cache) {
  by <- cache$by
  weather <- weather_raw |>
    .add_sim_timestamp_fields() |>
    dplyr::select(-timestamp)
  matches <- cache$lookup[.weather_join_key(weather, by)]
  n_matches <- lengths(matches)
  if (!any(n_matches)) {
    out <- dplyr::bind_cols(
      tibble::as_tibble(weather[FALSE, , drop = FALSE]),
      tibble::as_tibble(cache$survey[FALSE, cache$survey_nonjoin, drop = FALSE])
    )
    return(as.data.frame(dplyr::mutate(out, year = as.factor(year))))
  }
  weather_rows <- rep.int(seq_len(nrow(weather)), n_matches)
  survey_rows <- unlist(matches[n_matches > 0L], use.names = FALSE)
  as.data.frame(dplyr::mutate(
    dplyr::bind_cols(
      tibble::as_tibble(weather[weather_rows, , drop = FALSE]),
      tibble::as_tibble(cache$survey[survey_rows, cache$survey_nonjoin,
        drop = FALSE
      ])
    ),
    year = as.factor(year)
  ))
}

#' Prepare Historical Weather Data for Simulation
#'
#' Takes raw output from `get_weather()` and joins it back to the survey frame,
#' adding `sim_year` and ensuring `year` is a factor consistent with the
#' training data. Weather and outcome columns from the survey frame are dropped
#' before the join to avoid duplication.
#'
#' @details `int_month` and `sim_year` are derived from `timestamp` via
#'   `as.POSIXlt()` (truncating) rather than `format()`, which rounds
#'   fractional seconds and could shift boundary timestamps into the next
#'   month/year.
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
    .add_sim_timestamp_fields() |>
    dplyr::select(-timestamp) |>
    dplyr::inner_join(
      survey_weather |>
        dplyr::mutate(year = as.character(year)) |>
        dplyr::select(-dplyr::any_of(drop_cols)),
      by = c("code", "year", "survname", "loc_id", "int_month")
    ) |>
    dplyr::mutate(year = as.factor(year))
}

# Stage 2 aggregation lives in fct_aggregation_delta.R
# (aggregate_with_uncertainty_delta).
