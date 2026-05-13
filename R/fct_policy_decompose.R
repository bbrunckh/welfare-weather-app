# ============================================================================ #
# Harmonized policy effect decomposition for RIF and OLS/fixest engines.
#
# Decomposes total policy effect into:
#   - Main effect (direct welfare shift from policy)
#   - Resilience effect:
#       - Repositioning (RIF only): movement along weather beta curve
#       - Interaction: weather x policy interaction terms
#
# The shared helper .compute_rif_channels() is the single source of truth
# for all RIF channel calculations. Both the decomposition display AND the
# RIF simulation pipeline (.compute_rif_policy_correction below, invoked
# from run_sim_pipeline in fct_simulations.R) call it, so the Results pane
# and the Decomposition pane always agree numerically.
#
# Used by: mod_3_05_policy_sim.R (decomposition display)
#          fct_simulations.R     (run_sim_pipeline RIF policy correction)
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Shared helpers                                                               #
# ---------------------------------------------------------------------------- #

#' Compute covariate deltas between baseline and policy survey data
#'
#' @param svy_baseline Baseline survey data frame.
#' @param svy_policy Policy-modified survey data frame.
#' @param outcome Outcome variable name.
#' @param weather_vars Weather variable names to exclude from deltas.
#' @return Named list of numeric delta vectors for changed covariates.
#' @keywords internal
.compute_policy_deltas <- function(svy_baseline, svy_policy, outcome, weather_vars) {
  exclude_cols <- c(outcome, SP_TRANSFER_COL, ".svy_row_id", weather_vars,
                    "year", "sim_year", "int_month", "code", "survname", "loc_id",
                    grep("^weight$|^hhweight$|^wgt$|^pw$", names(svy_baseline),
                         value = TRUE, ignore.case = TRUE))
  check_cols <- setdiff(intersect(names(svy_baseline), names(svy_policy)), exclude_cols)

  deltas <- list()
  for (col in check_cols) {
    b <- svy_baseline[[col]]
    p <- svy_policy[[col]]
    # Coerce factors to numeric for comparison (suppress warnings for non-numeric levels)
    if (is.factor(b)) b <- suppressWarnings(as.numeric(as.character(b)))
    if (is.factor(p)) p <- suppressWarnings(as.numeric(as.character(p)))
    if (is.numeric(b) && is.numeric(p) && !all(is.na(b)) && !all(is.na(p))) {
      d <- p - b
      if (any(abs(d) > 1e-10, na.rm = TRUE)) {
        deltas[[col]] <- d
      }
    }
  }
  deltas
}


#' Compute all RIF policy channels (single source of truth)
#'
#' Returns a list with all decomposition channels and (optionally) per-channel
#' analytic standard errors via the delta method. Both the decomposition
#' display and the simulation correction call this function to ensure
#' the numbers are identical.
#'
#' **Uncertainty assumption (v1):** per-channel SEs are computed under a
#' diagonal coefficient covariance — each `(τ, term)` β draws from
#' `rif_grid$std.error` with zero off-diagonal covariance. Under that
#' assumption channels are independent, so
#' `Var(Δ_total) = Var(Δ_main) + Var(Δ_res1) + Var(Δ_res2)` exactly. This
#' is conservative for channels using *different* τ slots (Δ_main at τ_pre,
#' Δ_res2 at τ_post) and slightly understates total variance for channels
#' sharing τ (Δ_main / Δ_res1 at τ_pre, Δ_res1 / Δ_res2 at τ_post).
#'
#' @param svy_baseline Baseline survey data frame.
#' @param deltas Named list of covariate delta vectors (from .compute_policy_deltas).
#' @param sp_transfer Numeric vector of SP cash transfer amounts (level scale).
#' @param hazard_values Named list of hazard value vectors (one per weather var).
#' @param weather_vars Character vector of weather variable names.
#' @param rif_grid RIF coefficient grid data frame (must have model, term, tau, estimate).
#' @param taus Numeric vector of quantile grid points.
#' @param train_data Training data (for ecdf).
#' @param outcome Character, outcome variable name.
#' @param is_log Logical, whether outcome is log-transformed.
#' @param skip_coef Logical. When TRUE, all per-channel SEs are returned as 0.
#' @return Named list with delta_* and sd_* vectors of length n, plus
#'   tau_i_pre / tau_i_post / has_interactions. NULL if inputs invalid.
#' @keywords internal
.compute_rif_channels <- function(svy_baseline, deltas, sp_transfer,
                                  hazard_values, weather_vars,
                                  rif_grid, taus, train_data,
                                  outcome, is_log,
                                  skip_coef = FALSE) {
  n <- nrow(svy_baseline)

  # Only use model 3 coefficients
  grid3 <- rif_grid[rif_grid$model == 3L, ]
  if (nrow(grid3) == 0) return(NULL)
  all_terms <- unique(grid3$term)

  # Beta curve interpolation helper
  beta_at <- function(term_name, tau_values) {
    rows <- grid3[grid3$term == term_name, ]
    if (nrow(rows) == 0) return(rep(0, length(tau_values)))
    stats::approx(x = rows$tau, y = rows$estimate, xout = tau_values, rule = 2)$y
  }
  # SE curve interpolation helper (same shape; returns 0 if SE absent)
  se_at <- function(term_name, tau_values) {
    if (isTRUE(skip_coef)) return(rep(0, length(tau_values)))
    rows <- grid3[grid3$term == term_name, ]
    if (nrow(rows) == 0 || is.null(rows$std.error)) return(rep(0, length(tau_values)))
    stats::approx(x = rows$tau, y = rows$std.error, xout = tau_values, rule = 2)$y
  }

  # Baseline welfare in model scale
  y_raw <- svy_baseline[[outcome]]
  y_baseline <- if (is_log) log(pmax(y_raw, 1e-10)) else y_raw

  # Baseline quantile position via ecdf of training outcome
  F_hat <- stats::ecdf(train_data[[outcome]])
  tau_i_pre <- pmin(pmax(F_hat(y_baseline), min(taus)), max(taus))

  # --- Main effect: SP cash (log-scale) ---
  delta_sp <- if (is_log) {
    log(pmax(exp(y_baseline) + sp_transfer, 1e-10)) - y_baseline
  } else {
    sp_transfer
  }

  # --- Main effect: covariate shifts beta_x(tau_pre) * delta_x ---
  # Accumulate per-channel variance in parallel under the diagonal-Σ
  # approximation:  Var = Σ_term (Δx · SE_term(τ))²
  delta_main_covar <- rep(0, n)
  var_main <- rep(0, n)
  for (v in names(deltas)) {
    term_v <- if (v %in% all_terms) {
      v
    } else {
      # Fuzzy match for factor-expanded names
      matches <- grep(paste0("^", v), all_terms, value = TRUE)
      if (length(matches) > 0) matches[1] else NULL
    }
    if (!is.null(term_v)) {
      beta_v <- beta_at(term_v, tau_i_pre)
      se_v   <- se_at(term_v, tau_i_pre)
      delta_main_covar <- delta_main_covar + beta_v * deltas[[v]]
      var_main         <- var_main + (deltas[[v]] * se_v)^2
    }
  }

  delta_main <- delta_sp + delta_main_covar
  # delta_sp is a function of policy and observed welfare only — no β
  # dependence — so it adds 0 to var_main.

  # --- Repositioning (res1): tau shift from main effect changes weather beta ---
  # Post-main-effect position on baseline CDF (SP + covariate shifts move tau)
  baseline_cdf <- stats::ecdf(y_baseline)
  y_post_main <- y_baseline + delta_main
  tau_i_post <- pmin(pmax(baseline_cdf(y_post_main), min(taus)), max(taus))

  delta_res1 <- rep(0, n)
  var_res1   <- rep(0, n)
  for (wv in weather_vars) {
    haz <- hazard_values[[wv]]
    if (wv %in% all_terms) {
      # Continuous weather: repositioning via change in beta along the curve
      beta_pre  <- beta_at(wv, tau_i_pre)
      beta_post <- beta_at(wv, tau_i_post)
      se_pre    <- se_at(wv, tau_i_pre)
      se_post   <- se_at(wv, tau_i_post)
      delta_res1 <- delta_res1 + (beta_post - beta_pre) * haz
      # Independent-τ assumption: Var((β_post - β_pre) · haz) = haz² (SE_pre² + SE_post²)
      var_res1 <- var_res1 + (haz^2) * (se_pre^2 + se_post^2)
    } else if (wv %in% names(svy_baseline) && is.factor(svy_baseline[[wv]])) {
      # Binned (factor) weather: use year-specific bin from hazard_values[[wv]],
      # derived from that year's weather_raw via loc_id join — this varies by year.
      # Falls back to svy_baseline[[wv]] when weather_raw is NULL (historical run).
      fac_col <- if (is.factor(hazard_values[[wv]])) hazard_values[[wv]] else svy_baseline[[wv]]
      for (lv in levels(fac_col)) {
        term_name <- paste0(wv, lv)
        if (!term_name %in% all_terms) next   # reference level — contribution is 0
        active <- !is.na(fac_col) & fac_col == lv
        if (!any(active)) next
        beta_pre_b  <- beta_at(term_name, tau_i_pre[active])
        beta_post_b <- beta_at(term_name, tau_i_post[active])
        se_pre_b    <- se_at(term_name, tau_i_pre[active])
        se_post_b   <- se_at(term_name, tau_i_post[active])
        delta_res1[active] <- delta_res1[active] + (beta_post_b - beta_pre_b)
        var_res1[active]   <- var_res1[active]   + (se_pre_b^2 + se_post_b^2)
      }
    }
  }

  # --- Interaction (res2): beta_Haz:x(tau_post) * Haz * delta_x ---
  # Evaluated at tau_i_post (not tau_i_pre) because the household's welfare

  # level has shifted due to the main effect (SP + covariate changes), and
  # the interaction beta should reflect the vulnerability at the new position.
  # Continuous weather: beta_int(tau_post) * haz_mean * delta_x
  # Binned weather:     beta_int_bin_k(tau_post) * 1 * delta_x  (only for active bin)
  delta_res2 <- rep(0, n)
  var_res2   <- rep(0, n)
  has_interactions <- FALSE

  for (wv in weather_vars) {
    haz_int   <- hazard_values[[wv]]
    is_binned <- is.factor(haz_int)

    for (v in names(deltas)) {
      if (is_binned) {
        for (lv in levels(haz_int)) {
          cand1 <- paste0(wv, lv, ":", v)
          cand2 <- paste0(v, ":", wv, lv)
          it_name <- if (cand1 %in% all_terms) cand1 else if (cand2 %in% all_terms) cand2 else NULL
          if (is.null(it_name)) next
          has_interactions <- TRUE
          active <- !is.na(haz_int) & haz_int == lv
          if (!any(active)) next
          beta_int <- beta_at(it_name, tau_i_post[active])
          se_int   <- se_at(it_name, tau_i_post[active])
          delta_res2[active] <- delta_res2[active] + beta_int * deltas[[v]][active]
          var_res2[active]   <- var_res2[active]   + (deltas[[v]][active] * se_int)^2
        }
      } else {
        int_term <- NULL
        if      (paste0(wv, ":", v) %in% all_terms) int_term <- paste0(wv, ":", v)
        else if (paste0(v, ":", wv) %in% all_terms) int_term <- paste0(v, ":", wv)
        else {
          pattern  <- paste0("^", wv, "[^:]*:", v, "|^", v, "[^:]*:", wv)
          matches  <- grep(pattern, all_terms, value = TRUE)
          if (length(matches) > 0) int_term <- matches[1]
        }
        if (!is.null(int_term)) {
          has_interactions <- TRUE
          beta_int   <- beta_at(int_term, tau_i_post)
          se_int     <- se_at(int_term, tau_i_post)
          delta_res2 <- delta_res2 + beta_int * haz_int * deltas[[v]]
          var_res2   <- var_res2   + (haz_int * deltas[[v]] * se_int)^2
        }
      }
    }
  }

  delta_total <- delta_main + delta_res1 + delta_res2

  # Total variance under the channel-independence approximation. Holds exactly
  # when channels use disjoint (τ, term) sets (the common case once SP/cov
  # vs. weather effects are separated); approximate otherwise.
  var_total <- var_main + var_res1 + var_res2

  list(
    delta_sp         = delta_sp,
    delta_main_covar = delta_main_covar,
    delta_main       = delta_main,
    delta_res1       = delta_res1,
    delta_res2       = delta_res2,
    delta_total      = delta_total,
    sd_main          = sqrt(var_main),
    sd_res1          = sqrt(var_res1),
    sd_res2          = sqrt(var_res2),
    sd_total         = sqrt(var_total),
    tau_i_pre        = tau_i_pre,
    tau_i_post       = tau_i_post,
    has_interactions = has_interactions
  )
}


#' Compute per-household hazard values for each weather variable
#'
#' Continuous weather: per-location mean across `weather_raw` rows, joined to
#' each household via `loc_id`. Binned (factor) weather: per-location modal
#' bin. Both fall back to the household's own survey value when `weather_raw`
#' or `loc_id` is unavailable, so the function is safe to call from any
#' year-slice context (historical run, single-year scenario slice, etc.).
#'
#' @param svy_baseline Baseline survey data frame (provides loc_id and the
#'   fallback weather columns).
#' @param weather_raw Data frame of weather values for the period being
#'   evaluated. May be NULL.
#' @param weather_vars Character vector of weather variable names.
#' @return Named list (length `length(weather_vars)`) of hazard vectors, each
#'   of length `nrow(svy_baseline)`.
#' @keywords internal
.compute_hazard_values <- function(svy_baseline, weather_raw, weather_vars) {
  n <- nrow(svy_baseline)
  hazard_values <- lapply(weather_vars, function(wv) {
    if (!is.null(weather_raw) && wv %in% names(weather_raw)) {
      vals    <- weather_raw[[wv]]
      has_loc <- "loc_id" %in% names(weather_raw) &&
                 "loc_id" %in% names(svy_baseline)

      if (is.factor(vals) || is.character(vals)) {
        if (has_loc) {
          chr_vals  <- as.character(vals)
          modal_bin <- tapply(chr_vals, weather_raw[["loc_id"]], function(x) {
            tbl <- table(x[!is.na(x)])
            if (length(tbl) == 0) return(NA_character_)
            names(tbl)[which.max(tbl)]
          })
          hh_bin      <- modal_bin[as.character(svy_baseline[["loc_id"]])]
          orig_levels <- levels(svy_baseline[[wv]])
          return(factor(hh_bin, levels = orig_levels))
        }
        return(svy_baseline[[wv]])
      }

      if (!is.numeric(vals)) return(rep(0, n))

      if (has_loc) {
        loc_means  <- tapply(vals, weather_raw[["loc_id"]], mean, na.rm = TRUE)
        hh_vals    <- loc_means[as.character(svy_baseline[["loc_id"]])]
        grand_mean <- mean(vals, na.rm = TRUE)
        hh_vals[is.na(hh_vals)] <- grand_mean
        return(as.numeric(hh_vals))
      }
      return(rep(mean(vals, na.rm = TRUE), n))

    } else if (wv %in% names(svy_baseline)) {
      vals <- svy_baseline[[wv]]
      if (is.factor(vals) || is.character(vals)) return(vals)
      if (!is.numeric(vals)) return(rep(0, n))
      return(vals)
    }
    rep(0, n)
  })
  names(hazard_values) <- weather_vars
  hazard_values
}


#' Compute the net policy effect on welfare level for RIF, in model scale
#'
#' Single source of truth for the RIF policy adjustment used by the
#' simulation pipeline. Sums all decomposition channels:
#'   delta_total = delta_main (SP + covariate level) + delta_res1
#'                 (repositioning) + delta_res2 (weather x policy interaction).
#'
#' `run_sim_pipeline()` calls this in policy mode and adds the returned
#' vector to the baseline-x RIF prediction so that the Results pane's
#' policy y_point reflects the same net effect the Decomposition pane
#' reports.
#'
#' @param svy_baseline Baseline survey data frame.
#' @param svy_policy   Policy-adjusted survey data frame.
#' @param weather_raw  Period weather data frame (passed to hazard helper).
#' @param weather_cols Character vector of weather variable names from the
#'   fitted RIF model (`model_fit$weather_terms`).
#' @param rif_grid     RIF coefficient grid (`model_fit$rif_grid`).
#' @param taus         Numeric vector of quantile grid points
#'   (`model_fit$taus`).
#' @param train_data   Training data used for the ecdf
#'   (`model_fit$train_data`).
#' @param outcome      Character outcome variable name.
#' @param is_log       Logical; whether the outcome is log-transformed.
#' @return Numeric vector of length `nrow(svy_baseline)` giving delta_total
#'   in model scale, or NULL if any required input is missing.
#' @keywords internal
.compute_rif_policy_correction <- function(svy_baseline, svy_policy,
                                           weather_raw, weather_cols,
                                           rif_grid, taus, train_data,
                                           outcome, is_log) {
  if (is.null(svy_baseline) || is.null(svy_policy) ||
      is.null(rif_grid) || is.null(taus) || is.null(train_data) ||
      length(weather_cols) == 0L) return(NULL)

  n <- nrow(svy_baseline)
  deltas      <- .compute_policy_deltas(svy_baseline, svy_policy,
                                        outcome, weather_cols)
  sp_transfer <- if (SP_TRANSFER_COL %in% names(svy_policy)) {
    svy_policy[[SP_TRANSFER_COL]]
  } else rep(0, n)
  hazard_values <- .compute_hazard_values(svy_baseline, weather_raw,
                                          weather_cols)

  channels <- .compute_rif_channels(
    svy_baseline  = svy_baseline,
    deltas        = deltas,
    sp_transfer   = sp_transfer,
    hazard_values = hazard_values,
    weather_vars  = weather_cols,
    rif_grid      = rif_grid,
    taus          = taus,
    train_data    = train_data,
    outcome       = outcome,
    is_log        = is_log,
    # SEs are not propagated into y_point; the sim pipeline expresses
    # uncertainty through coefficient draws / F_loading, not channel SDs.
    skip_coef     = TRUE
  )
  if (is.null(channels)) return(NULL)
  channels$delta_total
}


# ---------------------------------------------------------------------------- #
# Public API                                                                   #
# ---------------------------------------------------------------------------- #

#' Decompose policy effects into main and resilience channels
#'
#' Harmonized function that works for both RIF and fixest/OLS engines.
#' For RIF: full decomposition (main + repositioning + interaction).
#' For fixest: simplified (main + interaction only, no repositioning).
#'
#' @param svy_baseline Data frame of baseline survey-weather covariates.
#' @param svy_policy Data frame of policy-adjusted survey-weather covariates.
#' @param model_fit Model fit list from Step 1 (contains $fit3, $engine,
#'   $taus, $weather_terms, $rif_grid, $train_data).
#' @param so Selected outcome metadata (list with $name, $transform).
#' @param weather_raw Data frame of weather values for the simulation period.
#'   Used to extract realised hazard values. If NULL, uses baseline survey
#'   weather columns as hazard.
#' @param skip_coef Logical. When TRUE, per-channel analytic standard errors
#'   are forced to 0 (use this when the user has turned coefficient
#'   uncertainty off in Step 2). Default FALSE.
#'
#' @return A data frame with one row per household and decomposition columns,
#'   or NULL if decomposition is not possible.
#' @export
decompose_policy_effect <- function(svy_baseline,
                                    svy_policy,
                                    model_fit,
                                    so,
                                    weather_raw = NULL,
                                    skip_coef = FALSE) {
  if (is.null(svy_baseline) || is.null(svy_policy) || is.null(model_fit)) {
    return(NULL)
  }

  engine <- model_fit$engine
  if (!engine %in% c("rif", "fixest")) return(NULL)

  n <- nrow(svy_baseline)
  outcome <- so$name
  is_log <- isTRUE(so$transform == "log")

  # Identify weather hazard variable(s) and their realised values
  weather_vars <- model_fit$weather_terms
  if (is.null(weather_vars) || length(weather_vars) == 0) return(NULL)

  hazard_values <- .compute_hazard_values(svy_baseline, weather_raw, weather_vars)

  # Compute covariate deltas using shared helper
  deltas <- .compute_policy_deltas(svy_baseline, svy_policy, outcome, weather_vars)

  # SP transfer component
  sp_transfer <- if (SP_TRANSFER_COL %in% names(svy_policy)) {
    svy_policy[[SP_TRANSFER_COL]]
  } else {
    rep(0, n)
  }

  if (engine == "rif") {
    .decompose_rif(svy_baseline, model_fit, so, deltas, sp_transfer,
                   hazard_values, weather_vars, n, skip_coef = skip_coef)
  } else {
    .decompose_ols(svy_baseline, model_fit, so, deltas, sp_transfer,
                   hazard_values, weather_vars, n, skip_coef = skip_coef)
  }
}


# ---------------------------------------------------------------------------- #
# RIF decomposition (delegates to .compute_rif_channels)                       #
# ---------------------------------------------------------------------------- #

.decompose_rif <- function(svy_baseline, model_fit, so, deltas, sp_transfer,
                           hazard_values, weather_vars, n,
                           skip_coef = FALSE) {
  outcome    <- so$name
  is_log     <- isTRUE(so$transform == "log")
  rif_grid   <- model_fit$rif_grid
  taus       <- model_fit$taus
  train_data <- model_fit$train_data

  if (is.null(rif_grid) || is.null(taus)) return(NULL)

  # Delegate to single source of truth
  channels <- .compute_rif_channels(
    svy_baseline  = svy_baseline,
    deltas        = deltas,
    sp_transfer   = sp_transfer,
    hazard_values = hazard_values,
    weather_vars  = weather_vars,
    rif_grid      = rif_grid,
    taus          = taus,
    train_data    = train_data,
    outcome       = outcome,
    is_log        = is_log,
    skip_coef     = skip_coef
  )
  if (is.null(channels)) return(NULL)

  if (!channels$has_interactions && length(deltas) > 0) {
    warning(
      "[decompose_policy_effect] No weather\u00d7policy interaction terms found ",
      "in the model. The interaction channel (res2) will be zero. ",
      "Consider including interaction terms in Step 1 model specification.",
      call. = FALSE
    )
  }

  # Assemble output data frame
  weight_col <- grep("^weight$|^hhweight$|^wgt$|^pw$",
                     names(svy_baseline), value = TRUE, ignore.case = TRUE)[1]

  data.frame(
    id               = seq_len(n),
    weight           = if (!is.na(weight_col)) svy_baseline[[weight_col]] else rep(1, n),
    tau_i_pre        = channels$tau_i_pre,
    tau_i_post       = channels$tau_i_post,
    decile           = pmin(pmax(ceiling(channels$tau_i_pre * 10), 1L), 10L),
    sp_eligible      = sp_transfer > 0,
    delta_main       = channels$delta_main,
    delta_sp         = channels$delta_sp,
    delta_main_covar = channels$delta_main_covar,
    delta_res1       = channels$delta_res1,
    delta_res2       = channels$delta_res2,
    delta_res        = channels$delta_res1 + channels$delta_res2,
    delta_total      = channels$delta_total,
    sd_main          = channels$sd_main,
    sd_res1          = channels$sd_res1,
    sd_res2          = channels$sd_res2,
    sd_total         = channels$sd_total,
    pct_main         = (exp(channels$delta_main) - 1) * 100,
    pct_res1         = (exp(channels$delta_res1) - 1) * 100,
    pct_res2         = (exp(channels$delta_res2) - 1) * 100,
    pct_total        = (exp(channels$delta_total) - 1) * 100,
    stringsAsFactors = FALSE
  )
}


# ---------------------------------------------------------------------------- #
# OLS/fixest decomposition (simplified: main + interaction only)               #
# ---------------------------------------------------------------------------- #

.decompose_ols <- function(svy_baseline, model_fit, so, deltas, sp_transfer,
                           hazard_values, weather_vars, n,
                           skip_coef = FALSE) {
  outcome <- so$name
  is_log <- isTRUE(so$transform == "log")
  fit <- model_fit$fit3

  if (is.null(fit)) return(NULL)

  coefs <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(coefs)) return(NULL)

  # Per-coefficient SE under diagonal-Σ approximation.
  se_vec <- if (isTRUE(skip_coef)) {
    setNames(rep(0, length(coefs)), names(coefs))
  } else {
    vc <- tryCatch(stats::vcov(fit), error = function(e) NULL)
    if (is.null(vc)) {
      setNames(rep(0, length(coefs)), names(coefs))
    } else {
      d <- diag(vc); d[d < 0 | is.na(d)] <- 0
      setNames(sqrt(d), names(coefs))
    }
  }
  se_of <- function(term) {
    s <- se_vec[term]
    if (is.na(s)) 0 else as.numeric(s)
  }

  # Baseline welfare
  y_raw <- svy_baseline[[outcome]]
  y_baseline <- if (is_log) log(pmax(y_raw, 1e-10)) else y_raw

  # --- Main effect ---
  delta_sp <- if (is_log) {
    log(pmax(exp(y_baseline) + sp_transfer, 1e-10)) - y_baseline
  } else {
    sp_transfer
  }
  delta_main_covar <- rep(0, n)
  var_main         <- rep(0, n)

  coef_names <- names(coefs)
  for (v in names(deltas)) {
    term_used <- v
    beta_v <- coefs[v]
    if (is.na(beta_v)) {
      matches <- grep(paste0("^", v), coef_names, value = TRUE)
      if (length(matches) > 0) {
        beta_v    <- coefs[matches[1]]
        term_used <- matches[1]
      }
    }
    if (!is.na(beta_v)) {
      delta_main_covar <- delta_main_covar + beta_v * deltas[[v]]
      var_main         <- var_main + (deltas[[v]] * se_of(term_used))^2
    }
  }
  delta_main <- delta_sp + delta_main_covar

  # --- Interaction (res2) --- OLS has no repositioning
  # Continuous weather: coef_int * haz_mean * delta_x
  # Binned weather:     coef_int_bin_k * 1 * delta_x  (only for active bin)
  delta_res2 <- rep(0, n)
  var_res2   <- rep(0, n)
  has_interactions <- FALSE

  for (wv in weather_vars) {
    haz_int   <- hazard_values[[wv]]
    is_binned <- is.factor(haz_int)

    for (v in names(deltas)) {
      if (is_binned) {
        for (lv in levels(haz_int)) {
          cand1 <- paste0(wv, lv, ":", v)
          cand2 <- paste0(v, ":", wv, lv)
          it_name <- if (cand1 %in% coef_names) cand1 else if (cand2 %in% coef_names) cand2 else NULL
          if (is.null(it_name)) next
          has_interactions <- TRUE
          active <- !is.na(haz_int) & haz_int == lv
          if (!any(active)) next
          delta_res2[active] <- delta_res2[active] + coefs[it_name] * deltas[[v]][active]
          var_res2[active]   <- var_res2[active]   + (deltas[[v]][active] * se_of(it_name))^2
        }
      } else {
        int_term <- NULL
        if      (paste0(wv, ":", v) %in% coef_names) int_term <- paste0(wv, ":", v)
        else if (paste0(v, ":", wv) %in% coef_names) int_term <- paste0(v, ":", wv)
        else {
          pattern <- paste0("^", wv, "[^:]*:", v, "|^", v, "[^:]*:", wv)
          matches <- grep(pattern, coef_names, value = TRUE)
          if (length(matches) > 0) int_term <- matches[1]
        }
        if (!is.null(int_term)) {
          has_interactions <- TRUE
          delta_res2 <- delta_res2 + coefs[int_term] * haz_int * deltas[[v]]
          var_res2   <- var_res2   + (haz_int * deltas[[v]] * se_of(int_term))^2
        }
      }
    }
  }

  if (!has_interactions && length(deltas) > 0) {
    warning(
      "[decompose_policy_effect] No weather\u00d7policy interaction terms found ",
      "in the model. The interaction channel will be zero. ",
      "Consider including interaction terms in Step 1 model specification.",
      call. = FALSE
    )
  }

  delta_total <- delta_main + delta_res2
  var_total   <- var_main + var_res2   # OLS has no res1 channel

  weight_col <- grep("^weight$|^hhweight$|^wgt$|^pw$",
                     names(svy_baseline), value = TRUE, ignore.case = TRUE)[1]

  data.frame(
    id               = seq_len(n),
    weight           = if (!is.na(weight_col)) svy_baseline[[weight_col]] else rep(1, n),
    tau_i_pre        = NA_real_,
    tau_i_post       = NA_real_,
    decile           = pmin(pmax(ceiling(stats::ecdf(y_baseline)(y_baseline) * 10), 1L), 10L),
    sp_eligible      = sp_transfer > 0,
    delta_main       = delta_main,
    delta_sp         = delta_sp,
    delta_main_covar = delta_main_covar,
    delta_res1       = rep(0, n),
    delta_res2       = delta_res2,
    delta_res        = delta_res2,
    delta_total      = delta_total,
    sd_main          = sqrt(var_main),
    sd_res1          = rep(0, n),
    sd_res2          = sqrt(var_res2),
    sd_total         = sqrt(var_total),
    pct_main         = (exp(delta_main) - 1) * 100,
    pct_res1         = rep(0, n),
    pct_res2         = (exp(delta_res2) - 1) * 100,
    pct_total        = (exp(delta_total) - 1) * 100,
    stringsAsFactors = FALSE
  )
}
