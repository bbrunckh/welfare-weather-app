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
# simulation correction (.compute_rif_policy_correction in fct_simulations.R)
# call it, guaranteeing numerical consistency.
#
# Used by: mod_3_05_policy_sim.R (decomposition display)
#          fct_simulations.R (.compute_rif_policy_correction)
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
  exclude_cols <- c(outcome, "._sp_transfer", ".svy_row_id", weather_vars,
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
#' Returns a list with all decomposition channels. Both the decomposition
#' display and the simulation correction call this function to ensure
#' the numbers are identical.
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
#' @return Named list with: delta_sp, delta_main_covar, delta_main, delta_res1,
#'   delta_res2, delta_total, tau_i_pre, tau_i_post, has_interactions.
#'   All delta_* are numeric vectors of length n. Returns NULL if inputs invalid.
#' @keywords internal
.compute_rif_channels <- function(svy_baseline, deltas, sp_transfer,
                                  hazard_values, weather_vars,
                                  rif_grid, taus, train_data,
                                  outcome, is_log) {
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
  delta_main_covar <- rep(0, n)
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
      delta_main_covar <- delta_main_covar + beta_v * deltas[[v]]
    }
  }

  delta_main <- delta_sp + delta_main_covar

  # --- Repositioning (res1): tau shift from main effect changes weather beta ---
  # Post-main-effect position on baseline CDF (SP + covariate shifts move tau)
  baseline_cdf <- stats::ecdf(y_baseline)
  y_post_main <- y_baseline + delta_main
  tau_i_post <- pmin(pmax(baseline_cdf(y_post_main), min(taus)), max(taus))

  delta_res1 <- rep(0, n)
  for (wv in weather_vars) {
    haz <- hazard_values[[wv]]
    if (wv %in% all_terms) {
      # Continuous weather: repositioning via change in beta along the curve
      beta_pre  <- beta_at(wv, tau_i_pre)
      beta_post <- beta_at(wv, tau_i_post)
      delta_res1 <- delta_res1 + (beta_post - beta_pre) * haz
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
        delta_res1[active] <- delta_res1[active] + (beta_post_b - beta_pre_b)
      }
    }
  }

  # --- Interaction (res2): beta_Haz:x(tau_pre) * Haz * delta_x ---
  # Continuous weather: beta_int(tau_pre) * haz_mean * delta_x
  # Binned weather:     beta_int_bin_k(tau_pre) * 1 * delta_x  (only for active bin)
  delta_res2 <- rep(0, n)
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
          beta_int <- beta_at(it_name, tau_i_pre[active])
          delta_res2[active] <- delta_res2[active] + beta_int * deltas[[v]][active]
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
          beta_int   <- beta_at(int_term, tau_i_pre)
          delta_res2 <- delta_res2 + beta_int * haz_int * deltas[[v]]
        }
      }
    }
  }

  delta_total <- delta_main + delta_res1 + delta_res2

  list(
    delta_sp         = delta_sp,
    delta_main_covar = delta_main_covar,
    delta_main       = delta_main,
    delta_res1       = delta_res1,
    delta_res2       = delta_res2,
    delta_total      = delta_total,
    tau_i_pre        = tau_i_pre,
    tau_i_post       = tau_i_post,
    has_interactions = has_interactions
  )
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
#'
#' @return A data frame with one row per household and decomposition columns,
#'   or NULL if decomposition is not possible.
#' @export
decompose_policy_effect <- function(svy_baseline,
                                    svy_policy,
                                    model_fit,
                                    so,
                                    weather_raw = NULL) {
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

  # Compute hazard values per household:
  #   - Continuous weather: mean weather for this period *per location*, joined
  #     to each HH via loc_id. This gives an HH-length vector that varies
  #     both across households (different locations) and across years (different
  #     weather realisations in each year-slice passed from mod_3_05).
  #     Falls back to grand mean only when loc_id is unavailable.
  #   - Binned (factor) weather: modal bin per location for this year-slice,
  #     joined by loc_id. Varies across years as temperatures shift bins.
  #     Falls back to svy_baseline bin when loc_id is unavailable.
  hazard_values <- lapply(weather_vars, function(wv) {
    if (!is.null(weather_raw) && wv %in% names(weather_raw)) {
      vals    <- weather_raw[[wv]]
      has_loc <- "loc_id" %in% names(weather_raw) && "loc_id" %in% names(svy_baseline)

      if (is.factor(vals) || is.character(vals)) {
        # Binned weather: modal bin per location for this year-slice
        if (has_loc) {
          chr_vals  <- as.character(vals)
          modal_bin <- tapply(chr_vals, weather_raw[["loc_id"]], function(x) {
            tbl <- table(x[!is.na(x)])
            if (length(tbl) == 0) return(NA_character_)
            names(tbl)[which.max(tbl)]
          })
          hh_bin <- modal_bin[as.character(svy_baseline[["loc_id"]])]
          orig_levels <- levels(svy_baseline[[wv]])
          return(factor(hh_bin, levels = orig_levels))
        }
        return(svy_baseline[[wv]])
      }

      if (!is.numeric(vals)) return(rep(0, n))

      # Continuous weather: per-location mean, joined to each HH by loc_id
      if (has_loc) {
        loc_means <- tapply(vals, weather_raw[["loc_id"]], mean, na.rm = TRUE)
        hh_vals   <- loc_means[as.character(svy_baseline[["loc_id"]])]
        # Fill any unmatched HHs with the grand mean
        grand_mean           <- mean(vals, na.rm = TRUE)
        hh_vals[is.na(hh_vals)] <- grand_mean
        return(as.numeric(hh_vals))
      }
      # No loc_id — fall back to grand mean (scalar broadcast)
      rep(mean(vals, na.rm = TRUE), n)

    } else if (wv %in% names(svy_baseline)) {
      # No weather_raw supplied — use each HH's own survey weather value
      vals <- svy_baseline[[wv]]
      if (is.factor(vals) || is.character(vals)) return(vals)
      if (!is.numeric(vals)) return(rep(0, n))
      vals
    } else {
      rep(0, n)
    }
  })
  names(hazard_values) <- weather_vars

  # Compute covariate deltas using shared helper
  deltas <- .compute_policy_deltas(svy_baseline, svy_policy, outcome, weather_vars)

  # SP transfer component
  sp_transfer <- if ("._sp_transfer" %in% names(svy_policy)) {
    svy_policy[["._sp_transfer"]]
  } else {
    rep(0, n)
  }

  if (engine == "rif") {
    .decompose_rif(svy_baseline, model_fit, so, deltas, sp_transfer,
                   hazard_values, weather_vars, n)
  } else {
    .decompose_ols(svy_baseline, model_fit, so, deltas, sp_transfer,
                   hazard_values, weather_vars, n)
  }
}


# ---------------------------------------------------------------------------- #
# RIF decomposition (delegates to .compute_rif_channels)                       #
# ---------------------------------------------------------------------------- #

.decompose_rif <- function(svy_baseline, model_fit, so, deltas, sp_transfer,
                           hazard_values, weather_vars, n) {
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
    is_log        = is_log
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
                           hazard_values, weather_vars, n) {
  outcome <- so$name
  is_log <- isTRUE(so$transform == "log")
  fit <- model_fit$fit3

  if (is.null(fit)) return(NULL)

  coefs <- tryCatch(stats::coef(fit), error = function(e) NULL)
  if (is.null(coefs)) return(NULL)

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

  coef_names <- names(coefs)
  for (v in names(deltas)) {
    beta_v <- coefs[v]
    if (is.na(beta_v)) {
      # Fuzzy match for factor-expanded names
      matches <- grep(paste0("^", v), coef_names, value = TRUE)
      if (length(matches) > 0) beta_v <- coefs[matches[1]]
    }
    if (!is.na(beta_v)) {
      delta_main_covar <- delta_main_covar + beta_v * deltas[[v]]
    }
  }
  delta_main <- delta_sp + delta_main_covar

  # --- Interaction (res2) --- OLS has no repositioning
  # Continuous weather: coef_int * haz_mean * delta_x
  # Binned weather:     coef_int_bin_k * 1 * delta_x  (only for active bin)
  delta_res2 <- rep(0, n)
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
    pct_main         = (exp(delta_main) - 1) * 100,
    pct_res1         = rep(0, n),
    pct_res2         = (exp(delta_res2) - 1) * 100,
    pct_total        = (exp(delta_total) - 1) * 100,
    stringsAsFactors = FALSE
  )
}
