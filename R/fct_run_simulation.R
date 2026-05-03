# fct_run_simulation.R
# --------------------
# Orchestration function for the full simulation pipeline.
# Pure function — no reactives. Called from mod_2_01_weathersim.R.
#
# Called by:
#   - mod_2_01_weathersim.R (observeEvent(input$run_sim))
#
# Depends on:
#   - fct_simulations.R  (run_sim_pipeline, compute_chol_vcov, format_elapsed)
#   - fct_get_weather.R  (get_weather, summarise_ensemble)
#   - fct_aggregation.R  (compute_hist_agg, compute_scenario_agg)


# ---------------------------------------------------------------------------- #
# Run full simulation pipeline — called once per button click                  #
# ---------------------------------------------------------------------------- #

#' Run the full welfare-weather simulation pipeline
#'
#' Pure function — no reactives. Extracts all business logic from
#' observeEvent(input\$run_sim) in mod_2_01_weathersim.R.
#'
#' @param sw               Data frame. Selected weather variables.
#' @param so               Data frame. Selected outcome (one row).
#' @param svy              Data frame. Baseline survey data.
#' @param ss               Data frame. Selected surveys.
#' @param mf               List. Model fit (fit3, engine, train_data).
#' @param cp               List. Connection parameters.
#' @param fp_list          List of character(2) vectors. Future period date ranges.
#' @param ssps             Character vector. Climate SSP codes.
#' @param residuals        Character. Residual method.
#' @param dev_mode         Logical. If TRUE limit to 1 ensemble member per key.
#' @param skip_coef_draws  Logical. If TRUE bypass Cholesky draws.
#' @param sim_dates        Character vector. Historical simulation dates.
#' @param perturbation_method List or NULL. Built by build_perturbation_method().
#' @param stored_breaks    Named list or NULL. Pre-computed histogram breaks.
#' @param ensemble_band_q  Named numeric. Quantile bounds for ensemble summary.
#' @param full_ensemble    Logical. If TRUE retain all ensemble members.
#' @param notify_fn        Function(msg). Called for user-facing notifications.
#'   Default is message() to console only.
#' @param progress_fn      Function(value, detail). Called to update progress.
#'   Default is a no-op — Shiny passes shiny::setProgress here.
#'
#' @return Named list with elements:
#'   \describe{
#'     \item{hist_sim_result}{List. Historical simulation output.}
#'     \item{new_scenarios}{Named list. Future scenario outputs.}
#'     \item{chol_obj}{List or NULL. Cholesky VCV object.}
#'     \item{n_keys}{Integer. Total number of simulation keys.}
#'     \item{total_runs}{Integer. Total prediction runs.}
#'     \item{t_elapsed}{Numeric. Wall-clock seconds elapsed.}
#'   }
#' @noRd
fct_run_simulation <- function(sw,
                                so,
                                svy,
                                ss,
                                mf,
                                cp,
                                fp_list,
                                ssps,
                                residuals,
                                dev_mode,
                                skip_coef_draws,
                                sim_dates,
                                perturbation_method,
                                stored_breaks,
                                ensemble_band_q,
                                full_ensemble,
                                notify_fn   = function(msg) message(msg),
                                progress_fn = function(value, detail) invisible(NULL)) {

  model      <- mf$fit3
  engine     <- mf$engine
  train_data <- mf$train_data
  has_future <- length(fp_list) > 0 && length(ssps) > 0

  ssp_labels <- c(
    "ssp2_4_5" = "SSP2-4.5",
    "ssp3_7_0" = "SSP3-7.0",
    "ssp5_8_5" = "SSP5-8.5"
  )

  # ---- Weather loading ---------------------------------------------------- #
  progress_fn(0.15, "Loading weather data...")

  weather_result <- get_weather(
    survey_data         = svy,
    selected_surveys    = ss,
    selected_weather    = sw,
    dates               = sim_dates,
    connection_params   = cp,
    ssp                 = if (has_future) ssps else NULL,
    future_period       = if (has_future) fp_list else NULL,
    perturbation_method = perturbation_method,
    stored_breaks       = stored_breaks
  )

  # ---- Cholesky VCV ------------------------------------------------------- #
  chol_obj <- if (isTRUE(skip_coef_draws)) {
    message("[wiseapp] Coefficient draws skipped (point estimates only)")
    NULL
  } else {
    tryCatch(
      compute_chol_vcov(fit = model, vcov_spec = COEF_VCOV_SPEC),
      error = function(e) {
        warning("[fct_run_simulation] compute_chol_vcov() failed — ",
                "falling back to point estimates: ", conditionMessage(e))
        NULL
      }
    )
  }

  # ---- Cluster counts ----------------------------------------------------- #
  cluster_counts <- tryCatch(
    compute_cluster_counts(train_data),
    error = function(e) NULL
  )
  if (!is.null(cluster_counts)) {
    message(sprintf(
      "[wiseapp] Cluster counts — loc_id: %d | loc_id:int_month: %d",
      cluster_counts$loc_id %||% NA_integer_,
      cluster_counts$loc_id_int_month %||% NA_integer_
    ))
  }

  # ---- Ensemble summarisation --------------------------------------------- #
  n_models_before <- length(setdiff(names(weather_result), "historical"))
  #if (!isTRUE(full_ensemble) &&
  #    !isTRUE(dev_mode) &&
  #    !is.null(ensemble_band_q)) {
  #  weather_result <- tryCatch(
  #    summarise_ensemble(
  #      weather_result,
  #      lo_q = ensemble_band_q[["lo"]],
  #      hi_q = ensemble_band_q[["hi"]]
  #    ),
  #    error = function(e) {
  #      warning("[fct_run_simulation] summarise_ensemble() failed — ",
  #              "using full ensemble: ", conditionMessage(e))
  #      weather_result
  #    }
  #  )
  #  n_models_after <- length(setdiff(names(weather_result), "historical"))
  #  message(sprintf(
  #    "[wiseapp] Ensemble summarised: %d models -> %d representatives per SSP/period",
  #    n_models_before, n_models_after
  #  ))
  #} else {
    message(sprintf(
      "[wiseapp] Full ensemble retained: %d future keys%s",
      n_models_before,
      if (isTRUE(dev_mode)) " (dev mode)" else ""
    ))

  # ---- Key loop setup ----------------------------------------------------- #
  progress_fn(0.5, "Running simulations...")

  weight_col_sim <- grep("^weight$|^hhweight$|^wgt$|^pw$",
                          names(svy), value = TRUE, ignore.case = TRUE)[1L]
  if (is.na(weight_col_sim %||% NA)) weight_col_sim <- NULL
  wt_detected <- grep("^weight$|^hhweight$|^wgt$|^pw$",
                       names(svy), value = TRUE, ignore.case = TRUE)
  if (length(wt_detected) > 1L) {
    warning(sprintf(
      "[wiseapp] Multiple weight columns detected: %s. Using '%s'.",
      paste(wt_detected, collapse = ", "), weight_col_sim
    ))
  }

  future_keys <- setdiff(names(weather_result), "historical")
  if (isTRUE(dev_mode)) {
    future_keys <- future_keys[
      !duplicated(stringr::str_extract(future_keys,
                                        "^(?:[^_]+_){4}[^_]+"))
    ]
  }
  all_keys   <- c("historical", future_keys)
  n_keys     <- length(all_keys)

  n_hist_yrs    <- if (!is.null(weather_result[["historical"]]))
    length(unique(format(weather_result[["historical"]]$timestamp, "%Y")))
  else 30L
  n_future_keys <- length(future_keys)
  total_runs    <- n_hist_yrs * (1L + n_future_keys)

  message(sprintf(
    "[wiseapp] Simulation starting: %d keys | Cholesky uncertainty deferred to aggregation",
    n_keys
  ))
  message(sprintf("[wiseapp]   Historical : %d yrs", n_hist_yrs))
  if (n_future_keys > 0L) message(sprintf(
    "[wiseapp]   Future     : %d keys x %d yrs = %d runs",
    n_future_keys, n_hist_yrs, n_future_keys * n_hist_yrs
  ))
  message(sprintf(
    "[wiseapp]   Total      : ~%d prediction runs across all keys",
    total_runs
  ))

  # ---- Key loop ----------------------------------------------------------- #
  t_start          <- proc.time()[["elapsed"]]
  hist_sim_result  <- NULL
  new_scenarios    <- list()
  group_agg        <- list()
  group_weather_rep <- list()
  group_meta       <- list()
  group_n          <- list()

  for (ki in seq_along(all_keys)) {
    key     <- all_keys[[ki]]
    is_hist <- identical(key, "historical")

    t_elapsed <- proc.time()[["elapsed"]] - t_start
    t_remain  <- if (ki > 1L)
      (t_elapsed / (ki - 1L)) * (n_keys - ki + 1L) else NA_real_

    message(sprintf(
      "[wiseapp] Key %d/%d: %s | %d yrs | %s elapsed%s",
      ki, n_keys,
      if (is_hist) "Historical"
      else sub("^(ssp[^_]+_[0-9]+_[0-9]+)_.*$", "\\1", key),
      n_hist_yrs,
      format_elapsed(t_elapsed),
      if (!is.na(t_remain))
        paste0(" | ~", format_elapsed(t_remain), " remaining")
      else " | estimating..."
    ))

    out <- run_sim_pipeline(
      weather_raw = weather_result[[key]],
      svy         = svy,
      sw          = sw,
      so          = so,
      model       = model,
      residuals   = residuals,
      train_data  = train_data,
      engine      = engine,
      chol_obj    = chol_obj
    )

    key_weather_raw        <- if (is_hist) weather_result[[key]] else NULL
    weather_result[[key]]  <- NULL

    t_elapsed_post <- proc.time()[["elapsed"]] - t_start
    t_remain_post  <- if (ki >= 1L)
      (t_elapsed_post / ki) * (n_keys - ki) else NA_real_

    progress_fn(
      value  = 0.5 + 0.45 * (ki / n_keys),
      detail = sprintf(
        "%s | Key %d/%d | %s elapsed%s",
        if (is_hist) "Historical"
        else sub("^(ssp[^_]+_[0-9]+_[0-9]+)_.*$", "\\1", key),
        ki, n_keys,
        format_elapsed(t_elapsed_post),
        if (!is.na(t_remain_post) && t_remain_post > 0)
          paste0(" | ~", format_elapsed(t_remain_post), " remaining")
        else if (ki == n_keys) " | finalising..."
        else ""
      )
    )

    if (is.null(out)) { rm(out); next }

    if (is_hist && is.null(hist_sim_result)) {
      hist_sim_result <- list(
        pipeline       = out,
        chol_obj       = chol_obj,
        so             = so,
        has_weights    = !is.null(out$weight),
        weather_raw    = key_weather_raw,
        train_data     = train_data,
        cluster_counts = cluster_counts
      )
    } else if (!is_hist) {
      ssp_code <- sub("^(ssp[^_]+_[^_]+_[^_]+)_.*", "\\1", key)
      yr_parts <- regmatches(key, gregexpr("[0-9]{4}", key))[[1L]]
      period   <- if (length(yr_parts) >= 2L)
        paste0(yr_parts[[1L]], "_", yr_parts[[2L]]) else "unknown"
      gk       <- paste0(ssp_code, "_", period)

      if (is.null(group_agg[[gk]]))         group_agg[[gk]]         <- list()
      if (is.null(group_weather_rep[[gk]])) group_weather_rep[[gk]] <- out$weather_raw
      if (is.null(group_n[[gk]]))           group_n[[gk]]           <- 0L
      if (is.null(group_meta[[gk]])) {
        group_meta[[gk]] <- list(ssp_code = ssp_code, year_range = yr_parts)
      }

      member_type <- sub(".*_(ensemble_mean|ensemble_lo|ensemble_hi)$",
                          "\\1", key)
      if (!nchar(member_type) || member_type == key)
        member_type <- paste0("model_", group_n[[gk]] + 1L)

      group_agg[[gk]][[member_type]] <- out
      group_n[[gk]] <- group_n[[gk]] + 1L
    }
    rm(out)
    if (ki %% 10L == 0L) gc(verbose = FALSE)
  }

  rm(weather_result); gc(verbose = FALSE)

  # ---- Assemble new_scenarios --------------------------------------------- #
  for (gk in names(group_agg)) {
    meta        <- group_meta[[gk]]
    ssp_pretty  <- ssp_labels[meta$ssp_code] %||% meta$ssp_code
    period_lbl  <- paste0(meta$year_range[1], "-", meta$year_range[2])
    display_key <- paste0(ssp_pretty, " / ", period_lbl)
    new_scenarios[[display_key]] <- list(
      pipelines   = group_agg[[gk]],
      weather_raw = group_weather_rep[[gk]],
      chol_obj    = chol_obj,
      so          = so,
      year_range  = meta$year_range,
      n_models    = group_n[[gk]]
    )
  }
  rm(group_agg, group_weather_rep, group_meta, group_n)
  gc(verbose = FALSE)

  t_elapsed_total <- proc.time()[["elapsed"]] - t_start

  list(
    hist_sim_result     = hist_sim_result,
    new_scenarios       = new_scenarios,
    #hist_agg_result     = hist_agg_result,      # ← new
    #scenario_agg_result = scenario_agg_result,  # ← new
    chol_obj            = chol_obj,
    n_keys              = n_keys,
    total_runs          = total_runs,
    t_elapsed           = t_elapsed_total
  )
}