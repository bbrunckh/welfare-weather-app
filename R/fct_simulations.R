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
  m   <- regexpr("\\d{4}-\\d{4}", nm)
  out <- regmatches(nm, m)
  out[m == -1L] <- NA_character_
  out
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

# SE clustering specification — confirmed default: ~loc_id_panel
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
# Named alternatives — available as constants, not default.
# Use compute_cluster_counts() to check G before switching.
COEF_VCOV_SPEC              <- ~loc_id_panel
COEF_VCOV_SPEC_MOULTON      <- ~loc_id_panel:int_month

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
#'     \item{loc_id_panel_int_month}{Number of distinct panel location × month clusters
#'       (Moulton minimum).}
#'     \item{conservative}{Number of distinct code × year × survname × loc_id_panel
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

  g_loc        <- if (has_cols("loc_id_panel"))
                    dplyr::n_distinct(data[["loc_id_panel"]])
                  else NA_integer_

  g_loc_month  <- if (has_cols("loc_id_panel", "int_month"))
                    dplyr::n_distinct(paste(data[["loc_id_panel"]], data[["int_month"]]))
                  else NA_integer_

  g_conserv    <- if (has_cols("code", "year", "survname", "loc_id_panel"))
                    dplyr::n_distinct(paste(data[["code"]], data[["year"]],
                                           data[["survname"]], data[["loc_id_panel"]]))
                  else NA_integer_

  counts <- list(
    loc_id_panel           = g_loc,
    loc_id_panel_int_month = g_loc_month,
    conservative           = g_conserv
  )

  # Runtime warnings — surface immediately at model fit time
  if (!is.na(g_loc) && g_loc < 40L) {
    warning(sprintf(
      "[SE] Only %d clusters at ~loc_id_panel. VCV SEs may be unreliable. Wild cluster bootstrap recommended (not yet implemented).",
      g_loc
    ))
  } else if (!is.na(g_loc) && g_loc < 50L) {
    message(sprintf(
      "[SE] %d clusters at ~loc_id_panel — borderline. Flag in methodology note.",
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
#'   \code{\link{aggregate_with_uncertainty_delta}}
#' @export
compute_chol_vcov <- function(fit, vcov_spec = COEF_VCOV_SPEC) {
  # fixest_multi: iterate over sub-models (needed for RIF quantile fits)
  if (inherits(fit, "fixest_multi")) {
    return(lapply(seq_along(fit), function(i)
      compute_chol_vcov(fit[[i]], vcov_spec = vcov_spec)
    ))
  }

  stopifnot("fit must be a fixest model" = inherits(fit, "fixest"))
  beta  <- coef(fit)

  # Try requested spec first — fallback chain if it fails
  vcov_fallbacks <- list(vcov_spec, ~loc_id, "HC1", "iid")
  Sigma <- NULL
  for (spec in vcov_fallbacks) {
    Sigma <- tryCatch(
      vcov(fit, vcov = spec),
      error = function(e) NULL
    )
    if (!is.null(Sigma) && all(is.finite(Sigma))) {
      if (!identical(spec, vcov_spec))
        message("[compute_chol_vcov] fell back to vcov spec: ", deparse(spec))
      break
    }
    Sigma <- NULL
  }

  if (is.null(Sigma))
    stop("[compute_chol_vcov] all vcov specs failed — cannot compute Sigma.")

  L <- tryCatch(
    t(chol(Sigma)),
    error = function(e)
      stop("[compute_chol_vcov] Cholesky decomposition failed: ",
           conditionMessage(e))
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
    "X_nonFE must be a numeric matrix"        = is.matrix(X_nonFE) && is.numeric(X_nonFE),
    "chol_obj must contain L and beta"        = all(c("L", "beta") %in% names(chol_obj)),
    "X_nonFE columns must match chol_obj$beta names" =
      identical(colnames(X_nonFE), names(chol_obj$beta))
  )

  # Additive-decomposition SE: when an active mask is set, build F_loading
  # from the *active block of Sigma* — i.e. F = X_active %*% L_active where
  # L_active = chol(Sigma[mask, mask]). This is mathematically distinct
  # from (and smaller than) F[, mask] subset, which can pick up
  # off-diagonal Sigma contributions from inactive coefficients.
  active_mask <- chol_obj$active_mask
  if (!is.null(active_mask) && !is.null(chol_obj$L_active) &&
      length(active_mask) == ncol(X_nonFE)) {
    return(X_nonFE[, active_mask, drop = FALSE] %*% chol_obj$L_active)
  }

  # Legacy: F = X %*% L (N × K).
  X_nonFE %*% chol_obj$L
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
#' \code{aggregate_with_uncertainty_delta()}.
#'
#' This function is called once per weather key (historical + future
#' representatives). It does NOT draw coefficient perturbations — all
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
#'   (point estimates only — no coefficient uncertainty).
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

                            chol_Sigma  = NULL,   # golem compat alias
                            slim        = FALSE,  # accepted, ignored
                            
                             #RIF
                             fit_multi   = NULL,
                             taus        = NULL,
                             weather_cols = NULL,
                             precomputed_train_resid = NULL,
                             svy_baseline = NULL,
                             rif_grid     = NULL) {

  n_pre_join <- nrow(svy)

  # Define is_rif once — all conditions in one place
  is_rif <- identical(engine, "rif") &&
            !is.null(fit_multi)      &&
            !is.null(taus)           &&
            !is.null(weather_cols)

  # RIF policy mode: caller supplied an svy_baseline so we can separate the
  # baseline (no-policy) RIF prediction from the policy net level effect.
  # We predict against svy_baseline and then add the decomposition's
  # delta_total — matching the Decomposition pane's totals exactly. The
  # OLS path doesn't need this because predict_outcome() naturally picks up
  # the policy level shift from the policy-modified design matrix.
  is_rif_policy <- is_rif && !is.null(svy_baseline) &&
                   !is.null(rif_grid) && !is.null(train_data)
  svy_for_predict <- if (is_rif_policy) svy_baseline else svy

  # Tag svy with row IDs so downstream consumers can broadcast per-household
  # quantities (RIF delta correction; Module 3 policy-delta application) back
  # to the (HH x year) rows produced by prepare_hist_weather().
  svy_for_predict$.svy_row_id <- seq_len(nrow(svy_for_predict))

  survey_wd_sim <- prepare_hist_weather(weather_raw, svy_for_predict, sw, so$name)

  # Resolve ID column for "original" residual matching
  id_col <- if (residuals == "original")
               resolve_id_col(train_data, survey_wd_sim)
             else
               NULL

  # ---- Prediction — dispatch on engine ------------------------------------

  out <- if (is_rif) {
    # RIF path — quantile delta method
    # Use chol_obj (our format) or chol_Sigma (golem format) for RIF
    chol_src <- if (!is.null(chol_obj)) chol_obj else chol_Sigma

    # For RIF: chol_src is list of list(L, K, beta, spec [, L_active]) per tau.
    # interpolate_F_loading() needs list of L matrices only.
    # When an active mask is set, we extract L_active (Cholesky of the
    # weather/policy block of Sigma) instead of the full L, so that
    # F_loading = X[, active] %*% L_active produces the correct
    # additive-decomposition variance (h' X_w Σ_ww X_w' h). Subsetting
    # columns of F = X %*% L_full would be incorrect when Σ has
    # off-diagonal terms between active and inactive coefficients.
    chol_list <- if (!is.null(chol_src) && is.list(chol_src) &&
                     !("L" %in% names(chol_src))) {
      active_mask <- attr(chol_src, "active_mask")
      use_active  <- !is.null(active_mask) &&
                      all(vapply(chol_src,
                                 function(x) "L_active" %in% names(x),
                                 logical(1)))
      tmp <- lapply(chol_src, function(x) {
        if (use_active && is.matrix(x$L_active)) x$L_active
        else if (is.list(x) && "L" %in% names(x)) x$L
        else if (is.matrix(x)) x
        else NULL
      })
      if (use_active) attr(tmp, "active_mask") <- active_mask
      tmp
    } else NULL
    predict_rif(
      fit_multi    = fit_multi,
      newdata      = survey_wd_sim, #joined,
      svy          = svy_for_predict,
      train_data   = train_data,
      taus         = taus,
      outcome      = so$name,
      weather_cols = weather_cols,
      so           = so,
      chol_list    = chol_list
    )
  } else {
    # Standard OLS path — unchanged
    tryCatch(
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
  }
  
  if (is.null(out)) {
    rm(survey_wd_sim)
    return(NULL)
  }

  # y_point stays log-scale — back-transformation happens inside
  # aggregate_with_uncertainty_delta() after coefficient perturbation.
  y_point  <- out$.fitted

  # ---- RIF policy correction --------------------------------------------- #
  # In RIF policy mode the prediction above was made against svy_baseline,
  # so y_point currently holds the *baseline-x* level (matching what Mod 2's
  # Step 2 hist_sim shows). Add the decomposition's delta_total — which
  # includes delta_main (SP + Beta_x.Delta_x), delta_res1 (repositioning),
  # and delta_res2 (Beta_int.haz.Delta_x) — so the Results pane reflects the
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
      is_log       = isTRUE(so$transform == "log")
    )
    # corr is one entry per household (nrow(svy_baseline)); broadcast it to
    # each expanded survey×weather row via .svy_row_id (set by predict_rif()
    # on `out`). Without this, hist_sim has y_point per (HH × month/year)
    # but corr is per HH, so the lengths mismatch and the correction would
    # be silently dropped.
    if (!is.null(corr) && ".svy_row_id" %in% names(out) &&
        length(corr) == nrow(svy_baseline)) {
      y_point <- y_point + corr[out$.svy_row_id]
    } else {
      warning("[run_sim_pipeline] RIF policy correction unavailable; ",
              "policy y_point will reflect weather-sensitivity changes only.")
    }
  }

  # ---- SP cash transfer (post-prediction, level scale) -------------------- #
  # SP_TRANSFER_COL is set on svy by apply_policy_to_svy() in fct_policy_sim.R.
  # The transfer is a direct welfare boost, not a regression covariate, so it
  # is added after prediction. To stay consistent with the decomposition
  # (.decompose_ols / .compute_rif_channels in fct_policy_decompose.R, which
  # define δ_sp = log(exp(y) + sp) − y), the boost is applied on the level
  # scale and re-logged when so$transform == "log".
  #
  # Skipped in RIF policy mode because delta_sp is already inside the
  # correction added above. (svy_for_predict = svy_baseline carries no
  # SP_TRANSFER_COL, so `out` wouldn't have it anyway — the guard is
  # defensive.)
  sp_vec <- if (!is_rif_policy && SP_TRANSFER_COL %in% names(out))
              out[[SP_TRANSFER_COL]] else NULL
  if (!is.null(sp_vec) && any(sp_vec > 0, na.rm = TRUE)) {
    is_log <- isTRUE(so$transform == "log")
    if (is_log) {
      welfare_lvl <- exp(y_point) + sp_vec
      y_point     <- log(pmax(welfare_lvl, .Machine$double.eps))
    } else {
      y_point <- y_point + sp_vec
    }
  }

  # ---- Simulation year and weights ---------------------------------------- #
  sim_year <- out$sim_year

  wt_col   <- grep("^weight$|^hhweight$|^wgt$|^pw$", names(out),
                   value = TRUE, ignore.case = TRUE)
  weight   <- if (length(wt_col) > 0L) out[[wt_col[[1L]]]] else NULL

  id_vec   <- if (!is.null(id_col) && id_col %in% names(out))
                out[[id_col]]
              else
                NULL

  # Pipeline row -> baseline household lookup. Used by Module 3 to broadcast
  # per-household policy deltas (decompose_policy_effect output, indexed by
  # baseline survey row) onto the expanded (HH x year) prediction rows.
  svy_row_id <- if (".svy_row_id" %in% names(out)) out$.svy_row_id else NULL

  # ---- Factor loading matrix ---------------------------------------------- #
  # Computed once per key — not per draw.
  # F_loading = X_nonFE %*% L  where L is the Cholesky factor of Sigma.
  # NULL when chol_obj = NULL (point estimates only).
  F_loading <- NULL
  if (is_rif && !is.null(attr(out, "F_loading"))) {
    # RIF path — F_loading computed inside predict_rif() via interpolate_F_loading()
    F_loading <- attr(out, "F_loading")
  } else if (!is_rif && !is.null(chol_obj)) {
      # Standard OLS path only — skip for RIF (model is fixest_multi)
    X_nonFE <- tryCatch(
      model.matrix(model, data = survey_wd_sim, type = "rhs"),
      error = function(e) {
        warning("[run_sim_pipeline] model.matrix() failed: ", conditionMessage(e))
        NULL
      }
    )
if (!is.null(X_nonFE)) {
      if (is.list(chol_obj) && "L" %in% names(chol_obj)) {
        # Our named list format — use compute_factor_loading()
        stopifnot(
          "X_nonFE columns must match chol_obj$beta names" =
            identical(colnames(X_nonFE), names(chol_obj$beta))
        )
        F_loading <- compute_factor_loading(X_nonFE, chol_obj)
      } else if (is.matrix(chol_obj)) {
        # Golem matrix format — inline multiply
        F_loading <- X_nonFE %*% t(chol_obj)
      }
    }
  }

  # ---- Training augmentation for residual drawing ------------------------- #
  # train_aug carries .resid for "original" and "resample" residual paths
  # inside aggregate_with_uncertainty_delta(). Computed once per pipeline call.
  # train_aug only needed for residual drawing — skip for RIF (uses "none")
  train_aug <- if (is_rif) NULL else tryCatch({
    fitted_train <- as.numeric(stats::predict(model, newdata = train_data))
    train_data |>
      dplyr::mutate(
        .fitted = fitted_train,
        .resid  = !!rlang::sym(so$name) - fitted_train
      )
  }, error = function(e) {
    warning("[run_sim_pipeline] train_aug computation failed: ",
            conditionMessage(e))
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
    svy_row_id  = svy_row_id,
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

# Stage 2 aggregation lives in fct_aggregation_delta.R
# (aggregate_with_uncertainty_delta).