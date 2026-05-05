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

# .parse_year <- function(nm) {
#   if (length(nm) == 1L) {
#     m <- regexpr("\\d{4}-\\d{4}", nm)
#     if (m == -1L) return(NA_character_)
#     return(regmatches(nm, m))
#   }
#   # Vectorised path
#   m <- gregexpr("\\d{4}-\\d{4}", nm)
#   vapply(seq_along(nm), function(i) {
#     matches <- regmatches(nm[i], m[i])[[1]]
#     if (length(matches) == 0) NA_character_ else matches[1]
#   }, character(1))
# }

.parse_year <- function(nm) {
  m <- regexpr("\\d{4}-\\d{4}", nm)
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

# PROVISIONAL: SE spec pending discussion. See methodology note.
# Options:
#   ~loc_id:int_month  -- Moulton minimum (cluster at regressor level)
#   ~loc_id            -- default used here; more conservative, common in
#                         applied weather-econometrics literature
#   ~region            -- most conservative; requires >30-50 clusters
COEF_VCOV_SPEC <- ~code + year + survname + loc_id

#' Compute Cholesky Factor of Clustered VCV Matrix
#'
#' Returns the upper-triangular Cholesky factor of the variance-covariance
#' matrix for the non-FE coefficients. Used to construct factor loadings
#' F = X %*% t(L) for efficient uncertainty propagation.
#'
#' @param fit       A fitted fixest model object.
#' @param vcov_spec Formula passed to `vcov(fit, vcov = ...)`.
#'   Defaults to `COEF_VCOV_SPEC`.
#' @return For `fixest`: a K x K upper-triangular matrix (Cholesky factor).
#'   For `fixest_multi`: a list of such matrices, one per sub-model.
#' @export
compute_chol_vcov <- function(fit, vcov_spec = COEF_VCOV_SPEC) {
  # fixest_multi: iterate over sub-models and return a list of Cholesky factors
  if (inherits(fit, "fixest_multi")) {
    return(lapply(seq_along(fit), function(i) {
      compute_chol_vcov(fit[[i]], vcov_spec = vcov_spec)
    }))
  }

  # Single fixest model: try the requested spec then fall back
  fallbacks <- list(vcov_spec, ~loc_id, "HC1", "iid")
  for (spec in fallbacks) {
    v <- tryCatch(vcov(fit, vcov = spec), error = function(e) NULL)
    if (is.null(v) || any(!is.finite(v))) next
    L <- tryCatch(chol(v), error = function(e) NULL)
    if (!is.null(L)) {
      if (!identical(spec, vcov_spec))
        message("[compute_chol_vcov] fell back to vcov spec: ",
                if (is.character(spec)) spec else deparse(spec))
      return(L)
    }
  }
  stop("Could not compute a valid Cholesky factor for any vcov specification.")
}


#' Vectorised Column-wise Aggregation for MC Draws
#'
#' Computes one aggregate scalar per column (draw) of an N x S matrix.
#' Avoids per-draw data.frame construction and function dispatch overhead.
#'
#' @param Y_mat N x S numeric matrix of outcome values (one draw per column).
#' @param w     Numeric N-vector of survey weights, or NULL (equal weights).
#' @param agg   Character aggregation method.
#' @param pov_line Numeric poverty line or NULL.
#' @return Numeric S-vector of aggregate values.
#' @keywords internal
.mc_aggregate_cols <- function(Y_mat, w, agg, pov_line = NULL) {

  N <- nrow(Y_mat)
  if (is.null(w)) {
    w_norm <- rep(1 / N, N)
  } else {
    w_norm <- w / sum(w)
  }

  switch(agg,
    "mean" = as.numeric(crossprod(w_norm, Y_mat)),
    "sum"  =, "total" = {
      w_abs <- if (is.null(w)) rep(1, N) else w
      as.numeric(crossprod(w_abs, Y_mat))
    },
    "median" = {
      apply(Y_mat, 2L, function(col) {
        ord  <- order(col)
        cumw <- cumsum(w_norm[ord])
        col[ord][which(cumw >= 0.5)[1]]
      })
    },
    "headcount_ratio" = {
      poor <- Y_mat < pov_line
      as.numeric(crossprod(w_norm, poor + 0L))
    },
    "gap" = {
      shortfall <- pmax(pov_line - Y_mat, 0) / pov_line
      as.numeric(crossprod(w_norm, shortfall))
    },
    "fgt2" = {
      shortfall <- (pmax(pov_line - Y_mat, 0) / pov_line)^2
      as.numeric(crossprod(w_norm, shortfall))
    },
    "gini" = {
      apply(Y_mat, 2L, function(col) {
        ord    <- order(col)
        x_s    <- col[ord]
        w_s    <- w_norm[ord]
        lorenz <- cumsum(w_s * x_s) / sum(w_s * x_s)
        lorenz <- c(0, lorenz)
        cx     <- c(0, cumsum(w_s))
        B      <- sum(diff(cx) * (lorenz[-length(lorenz)] + lorenz[-1]) / 2)
        1 - 2 * B
      })
    },
    "prosperity_gap" = {
      pg <- pmax(28 / Y_mat, 1)
      as.numeric(crossprod(w_norm, pg))
    },
    "avg_poverty" = {
      apply(Y_mat, 2L, function(col) {
        ok <- is.finite(col) & col > 0
        if (!any(ok)) return(NA_real_)
        sum(w_norm[ok] * (1 / col[ok])) / sum(w_norm[ok])
      })
    },
    # Fallback
    apply(Y_mat, 2L, function(col) {
      sum(col * w_norm, na.rm = TRUE)
    })
  )
}


#' Aggregate Predictions with Coefficient Uncertainty
#'
#' Computes an aggregate welfare statistic (mean, headcount, etc.) with
#' optional confidence bands from coefficient uncertainty via Cholesky
#' factor loadings. For linear aggregates on non-log outcomes, uses an
#' exact analytic formula. For non-linear aggregates or log-transformed
#' outcomes, uses S draws of a K-dimensional z-vector.
#'
#' @param y_point   Numeric N-vector of point-estimate fitted values
#'   (log-scale if outcome is log-transformed).
#' @param F_loading Numeric N x K matrix of factor loadings, or NULL
#'   (no uncertainty).
#' @param group_vec Factor or character N-vector defining groups
#'   (typically sim_year).
#' @param so        Selected-outcome list with `$name`, `$type`, `$transform`.
#' @param agg_method Character: aggregation method name (e.g., "mean",
#'   "headcount_ratio").
#' @param weights   Numeric N-vector of survey weights, or NULL.
#' @param pov_line  Numeric poverty line, or NULL.
#' @param train_resid Numeric vector of training residuals, or NULL.
#' @param residual_method Character: "none", "original", "normal", or
#'   "resample".
#' @param id_vec    Character/integer N-vector of household IDs for
#'   "original" residual matching, or NULL.
#' @param S         Integer number of Monte Carlo draws. Default 200.
#' @param seed      Optional integer seed for reproducibility.
#'
#' @return A tibble with columns: `sim_year`, `value`, and optionally
#'   `value_p05`, `value_p50`, `value_p95`.
#' @export
aggregate_with_uncertainty <- function(
    y_point, F_loading, group_vec,
    so, agg_method, weights = NULL,
    pov_line = NULL,
    train_resid = NULL, residual_method = "none",
    id_vec = NULL,
    S = 200L, seed = NULL) {

  N <- length(y_point)
  K <- if (!is.null(F_loading)) ncol(F_loading) else 0L
  is_log <- isTRUE(so$transform == "log")

  # Determine if analytic path is possible
  is_linear_agg <- agg_method %in% c("mean", "sum", "total")
  use_analytic <- is_linear_agg && !is_log

  groups <- unique(group_vec)
  results <- vector("list", length(groups))

  for (gi in seq_along(groups)) {
    g <- groups[gi]
    idx <- which(group_vec == g)
    y_g <- y_point[idx]
    w_g <- if (!is.null(weights)) weights[idx] else NULL
    F_g <- if (!is.null(F_loading)) F_loading[idx, , drop = FALSE] else NULL

    # Residuals (shared across z-draws)
    resid_g <- rep(0, length(idx))
    if (!identical(residual_method, "none") && !is.null(train_resid)) {
      n_g <- length(idx)
      resid_g <- switch(residual_method,
        "original" = rep(0, n_g),
        "normal"   = stats::rnorm(n_g, 0, stats::sd(train_resid, na.rm = TRUE)),
        "resample" = sample(train_resid, n_g, replace = TRUE),
        rep(0, n_g)
      )
    }

    # Helper: compute aggregate from a y-vector
    compute_agg <- function(y_vec) {
      tmp_df <- data.frame(sim_year = g, outcome_col = y_vec)
      if (!is.null(w_g)) tmp_df$wt <- w_g
      aggregate_outcome(
        df        = tmp_df,
        outcome   = "outcome_col",
        group     = "sim_year",
        type      = if (identical(so$type, "logical")) "binary" else "continuous",
        aggregate = agg_method,
        pov_line  = pov_line,
        weights   = if (!is.null(w_g)) "wt" else NULL
      )$value
    }

    # Point estimate
    y_pt <- y_g + resid_g
    if (is_log) y_pt <- exp(y_pt)
    val_pt <- compute_agg(y_pt)

    if (is.null(F_g)) {
      # No uncertainty
      results[[gi]] <- tibble::tibble(sim_year = g, value = val_pt)
    } else if (use_analytic) {
      # Analytic path for linear aggregates on non-log outcomes
      w_norm <- if (!is.null(w_g)) w_g / sum(w_g) else rep(1 / length(idx), length(idx))
      if (agg_method %in% c("sum", "total")) w_norm <- if (!is.null(w_g)) w_g else rep(1, length(idx))
      f_bar <- as.numeric(crossprod(w_norm, F_g))  # K-vector
      se <- sqrt(sum(f_bar^2))
      results[[gi]] <- tibble::tibble(
        sim_year  = g,
        value     = val_pt,
        value_p05 = val_pt + stats::qnorm(0.05) * se,
        value_p50 = val_pt,
        value_p95 = val_pt + stats::qnorm(0.95) * se
      )
    } else {
      # Monte Carlo path — vectorised over S draws
      if (!is.null(seed)) set.seed(seed + gi)
      Z <- matrix(stats::rnorm(S * K), nrow = S, ncol = K)

      # N x S matrix: each column is one draw's y-vector
      # F_g is N x K, Z is S x K, so F_g %*% t(Z) is N x S
      Y_mat <- F_g %*% t(Z)  # N x S perturbations
      Y_mat <- Y_mat + (y_g + resid_g)  # add point estimate (recycled)
      if (is_log) Y_mat <- exp(Y_mat)

      # Vectorised aggregation over columns of Y_mat
      scalars <- .mc_aggregate_cols(Y_mat, w_g, agg_method, pov_line)

      qs <- stats::quantile(scalars, c(0.05, 0.50, 0.95), na.rm = TRUE)
      results[[gi]] <- tibble::tibble(
        sim_year  = g,
        value     = qs[[2]],
        value_p05 = qs[[1]],
        value_p50 = qs[[2]],
        value_p95 = qs[[3]]
      )
    }
  }

  dplyr::bind_rows(results)
}


#' Combine Per-Model Aggregation Results
#'
#' Given M tibbles (one per GCM/ensemble model), each with per-year
#' aggregate values and optional coefficient-uncertainty bands, produce
#' a single tibble with multi-model median as central value and both
#' parameter uncertainty and model spread bands.
#'
#' @param per_model_aggs List of M tibbles, each from
#'   `aggregate_with_uncertainty()`.
#' @return A tibble with columns: `sim_year`, `value`, `value_p05`,
#'   `value_p95`, `model_values`.
#' @export
combine_ensemble_results <- function(per_model_aggs) {
  tagged <- lapply(seq_along(per_model_aggs), function(m) {
    df <- per_model_aggs[[m]]
    df$model_idx <- m
    df
  })
  stacked <- dplyr::bind_rows(tagged)

  has_bands <- "value_p05" %in% names(stacked)

  # Capture raw per-model values BEFORE summarising — must be a separate step
  # so list() captures the M raw values per year, not the post-median scalar.
  model_vals_tbl <- stacked |>
    dplyr::group_by(.data$sim_year) |>
    dplyr::summarise(model_values = list(.data$value), .groups = "drop")

  summary_tbl <- stacked |>
    dplyr::group_by(.data$sim_year) |>
    dplyr::summarise(
      value     = stats::median(.data$value,     na.rm = TRUE),
      value_p05 = if (has_bands) mean(.data$value_p05, na.rm = TRUE) else NA_real_,
      value_p95 = if (has_bands) mean(.data$value_p95, na.rm = TRUE) else NA_real_,
      .groups   = "drop"
    )

  dplyr::left_join(summary_tbl, model_vals_tbl, by = "sim_year")
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

#' Run the Full Simulation Pipeline for One Scenario Row
#'
#' Joins raw weather to survey covariates, generates point-estimate predictions,
#' and computes factor loadings for coefficient uncertainty propagation.
#'
#' @param weather_raw  Data frame returned by `get_weather()$result`.
#' @param svy          Survey-weather data frame (from `survey_weather()`).
#' @param sw           Selected weather metadata (from `selected_weather()`).
#' @param so           Selected outcome metadata (from `selected_outcome()`).
#' @param model        Fitted model object (`mf$fit3`).
#' @param residuals    Character. Residual method, e.g. `"original"`.
#' @param train_data   Data frame used at fit time (`mf$train_data`).
#' @param engine       Character. Model engine, e.g. `"fixest"`.
#' @param chol_Sigma   K x K upper-triangular Cholesky factor from
#'   `compute_chol_vcov()`, or NULL for point estimates only.
#' @param fit_multi    For RIF engine: list of quantile fits.
#' @param taus         For RIF engine: quantile grid.
#' @param weather_cols For RIF engine: weather column names.
#'
#' @return A named list with elements: `y_point` (N-vector), `F_loading`
#'   (N x K matrix or NULL), `sim_year` (N-vector), `weight` (N-vector or
#'   NULL), `id_vec` (N-vector or NULL), `n_pre_join` (integer),
#'   `weather_raw` (data frame), `preds` (full prediction data frame for
#'   historical diagnostics).
#'
#' @export
run_sim_pipeline <- function(weather_raw, svy, sw, so,
                             model, residuals, train_data, engine,
                             chol_Sigma = NULL,
                             slim = FALSE,
                             fit_multi = NULL, taus = NULL,
                             weather_cols = NULL,
                             precomputed_train_resid = NULL,
                             svy_baseline = NULL,
                             rif_grid = NULL) {
  n_pre_join <- nrow(svy)

  # Tag svy with row IDs for RIF delta-method lookups
  is_rif <- identical(engine, "rif") && !is.null(fit_multi)
  if (is_rif) {
    svy$.svy_row_id <- seq_len(nrow(svy))
  }

  survey_wd_sim <- prepare_hist_weather(weather_raw, svy, sw, so$name)
  id_col        <- if (residuals == "original") resolve_id_col(train_data, survey_wd_sim) else NULL

  # RIF dispatch
  if (is_rif) {
    preds <- tryCatch(
      predict_rif(
        fit_multi    = fit_multi,
        newdata      = survey_wd_sim,
        svy          = svy,
        train_data   = train_data,
        taus         = taus,
        outcome      = so$name,
        weather_cols = weather_cols,
        so           = so,
        chol_list    = if (is.list(chol_Sigma)) chol_Sigma else NULL
      ),
      error = function(e) {
        warning("[run_sim_pipeline] predict_rif() failed: ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(preds)) return(NULL)
    F_loading_rif <- attr(preds, "F_loading")

    # --- Policy correction (Option A): add main + repositioning effects ---
    # predict_rif only captures the weather delta; policy covariate shifts
    # (main effect) and the resulting CDF repositioning are missing.
    if (!is.null(svy_baseline) && !is.null(rif_grid)) {
      policy_correction <- .compute_rif_policy_correction(
        svy_baseline = svy_baseline,
        svy_policy   = svy,
        preds        = preds,
        train_data   = train_data,
        taus         = taus,
        rif_grid     = rif_grid,
        weather_cols = weather_cols,
        outcome      = so$name,
        so           = so
      )
      if (!is.null(policy_correction)) {
        preds[[so$name]] <- preds[[so$name]] + policy_correction
        preds$.fitted    <- preds$.fitted + policy_correction
      }
    }

    preds <- apply_log_backtransform(preds, so)
    # SP transfer
    if ("._sp_transfer" %in% names(preds)) {
      preds[[so$name]] <- preds[[so$name]] + preds[["._sp_transfer"]]
    }
    # RIF returns old-style preds (no factor loading)
    wt_cols <- grep("^weight$|^hhweight$|^wgt$|^pw$", names(preds),
                    value = TRUE, ignore.case = TRUE)
    wt_col <- if (length(wt_cols) > 0) wt_cols[1] else NULL
    return(list(
      y_point     = preds$.fitted,
      F_loading   = F_loading_rif,
      sim_year    = preds$sim_year,
      weight      = if (!is.null(wt_col)) preds[[wt_col]] else NULL,
      id_vec      = if (!is.null(id_col)) preds[[id_col]] else NULL,
      n_pre_join  = n_pre_join,
      weather_raw = weather_raw,
      preds       = preds
    ))
  }

  # Standard (fixest/ranger/glmnet) path: single point-estimate prediction

  preds <- tryCatch(
    predict_outcome(
      model      = model,
      newdata    = survey_wd_sim,
      residuals  = residuals,
      outcome    = so$name,
      id         = id_col,
      train_data = train_data,
      engine     = engine,
      precomputed_train_resid = precomputed_train_resid
    ),
    error = function(e) {
      warning("[run_sim_pipeline] predict_outcome() failed: ", conditionMessage(e))
      NULL
    }
  )

  if (is.null(preds)) { rm(survey_wd_sim); return(NULL) }

  # Extract y_point BEFORE back-transforming. aggregate_with_uncertainty()
  # receives log-scale values when so$transform == "log" and applies exp()
  # internally (including on Monte Carlo draws). Extracting after
  # apply_log_backtransform() would cause double-exponentiation and produce
  # wildly inflated simulation results.
  y_point <- preds[[so$name]]

  # Back-transform log outcomes (for preds_out diagnostics only)
  preds <- apply_log_backtransform(preds, so)

  # SP direct transfer (applied on original scale, after back-transform)
  if ("._sp_transfer" %in% names(preds)) {
    preds[[so$name]] <- preds[[so$name]] + preds[["._sp_transfer"]]
    # Re-synchronise y_point: keep in log scale so aggregate_with_uncertainty
    # still receives log-scale inputs and applies exp() exactly once.
    y_point <- if (isTRUE(so$transform == "log"))
      log(pmax(preds[[so$name]], .Machine$double.eps))
    else
      preds[[so$name]]
  }

  sim_year <- preds$sim_year
  wt_cols  <- grep("^weight$|^hhweight$|^wgt$|^pw$", names(preds),
                   value = TRUE, ignore.case = TRUE)
  wt_col   <- if (length(wt_cols) > 0) wt_cols[1] else NULL
  weight   <- if (!is.null(wt_col)) preds[[wt_col]] else NULL
  id_vec   <- if (!is.null(id_col)) preds[[id_col]] else NULL

  # Compute factor loadings for uncertainty propagation
  F_loading <- NULL
  if (!is.null(chol_Sigma) && identical(engine, "fixest") && inherits(model, "fixest")) {
    tryCatch({
      X_nonFE <- stats::model.matrix(model, data = survey_wd_sim, type = "rhs")
      # fixest may drop rows with unobserved FE levels; align to predicted rows
      if (nrow(X_nonFE) == length(y_point)) {
        F_loading <- X_nonFE %*% t(chol_Sigma)
      } else {
        warning("[run_sim_pipeline] model.matrix row mismatch, skipping F_loading")
      }
    }, error = function(e) {
      warning("[run_sim_pipeline] F_loading computation failed: ", conditionMessage(e))
    })
  }

  rm(survey_wd_sim)

  # For slim mode (future keys), don't keep full preds
  preds_out <- if (slim) NULL else preds

  list(
    y_point     = y_point,
    F_loading   = F_loading,
    sim_year    = sim_year,
    weight      = weight,
    id_vec      = id_vec,
    n_pre_join  = n_pre_join,
    weather_raw = weather_raw,
    preds       = preds_out
  )
}

# ---------------------------------------------------------------------------- #
# RIF Policy Correction Helper (Option A)                                      #
# Delegates to .compute_rif_channels() in fct_policy_decompose.R for          #
# numerical consistency between simulation and decomposition display.          #
# ---------------------------------------------------------------------------- #

#' Compute the main effect + repositioning correction for RIF policy sims
#'
#' predict_rif() only captures weather deltas (scenario - baseline weather).
#' When covariates differ between svy_baseline and svy_policy (e.g. binary
#' infrastructure flips), the covariate main effects are lost.
#' This function delegates to .compute_rif_channels() (single source of truth)
#' and returns only the channels NOT already captured by predict_rif:
#'   - delta_main_covar (beta_x * delta_x, excludes SP which is post-hoc)
#'   - delta_res1 (repositioning from tau shift)
#'
#' Interaction (delta_res2) is NOT added because predict_rif already evaluates
#' at policy covariates, so weather×policy interactions activate via the
#' weather delta.
#'
#' @param svy_baseline Original survey data (pre-policy).
#' @param svy_policy Policy-modified survey data (passed as svy to predict_rif).
#' @param preds Prediction output from predict_rif (in log/model scale).
#' @param train_data Training data for ecdf.
#' @param taus Quantile grid.
#' @param rif_grid RIF coefficient grid (from build_rif_grid).
#' @param weather_cols Weather variable names.
#' @param outcome Outcome column name.
#' @param so Selected outcome metadata.
#' @return Numeric vector of length nrow(preds), or NULL.
#' @keywords internal
.compute_rif_policy_correction <- function(svy_baseline, svy_policy, preds,
                                           train_data, taus, rif_grid,
                                           weather_cols, outcome, so) {
  n <- nrow(preds)
  is_log <- isTRUE(so$transform == "log")

  # Compute covariate deltas using shared helper
  deltas <- .compute_policy_deltas(svy_baseline, svy_policy, outcome, weather_cols)

  # If no covariate changes, nothing to correct.
  # SP transfer is handled separately in run_sim_pipeline (post-backtransform).
  if (length(deltas) == 0) return(NULL)

  # SP transfer (needed for tau shift calculation, not for direct addition)
  sp_transfer <- if ("._sp_transfer" %in% names(svy_policy)) {
    svy_policy[["._sp_transfer"]]
  } else {
    rep(0, n)
  }

  # Hazard values from baseline survey weather columns
  hazard_values <- lapply(weather_cols, function(wv) {
    if (wv %in% names(svy_baseline)) {
      vals <- svy_baseline[[wv]]
      if (is.factor(vals) || is.character(vals)) return(rep(1, n))
      if (!is.numeric(vals)) return(rep(0, n))
      vals
    } else {
      rep(0, n)
    }
  })
  names(hazard_values) <- weather_cols

  # Delegate to shared channel computation (single source of truth)
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
    is_log        = is_log
  )
  if (is.null(channels)) return(NULL)

  # Return only main_covar + repositioning.
  # SP is applied post-backtransform in run_sim_pipeline.
  # Interaction is already captured in predict_rif weather delta.
  channels$delta_main_covar + channels$delta_res1
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
      year      = as.factor(year),
      int_month = as.integer(format(timestamp, "%m")),
      sim_year  = as.integer(format(timestamp, "%Y"))
    ) |>
    dplyr::select(-timestamp) |>
    dplyr::inner_join(
      survey_weather |> dplyr::select(-dplyr::any_of(drop_cols)),
      by = c("code", "year", "survname", "loc_id", "int_month")
    )
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
