# =============================================================================
# dev/run_mod2_batch.R
#
# Batch Mod 1 → Mod 2 pipeline per country × weather spec.
# Saves hero plot + exceedance curve for mean and headcount_ratio.
#
# Usage: source("dev/run_mod2_batch.R")
# =============================================================================

rm(list = ls())
pkgload::load_all(quiet = TRUE)

# =============================================================================
# SECTION 1 — CONFIGURATION
# =============================================================================

CONNECTION_TYPE <- "databricks"
OUT_DIR         <- "dev/outputs/projection_batch"

COUNTRY_FILTER  <- c("BEN")   # NULL = all
OUTCOME_NAME    <- "welfare"
CURRENCY        <- "PPP"
POVERTY_LINE    <- 3

MODEL_TYPE      <- "Linear regression"
INTERACTION_VAR <- "electricity"
FIXED_EFFECTS   <- c("year", "gaul1_code")

LASSO_ALPHA         <- 1
LASSO_LAMBDA        <- "lambda.1se"
LASSO_NFOLDS        <- 10L
LASSO_STANDARDIZE   <- TRUE
MI_M                <- 5L
MI_MAXIT            <- 5L
STABILITY_THRESHOLD <- 0.5
LASSO_FORCE_IN  <- list(ind = character(0), hh = character(0),
                        firm = character(0), area = character(0))
LASSO_FORCE_OUT <- list(ind = character(0), hh = character(0),
                        firm = character(0), area = character(0))

WEATHER_SPECS <- list(
  t_12m_binned = list(
    vars = "t",
    t = list(
      ref_period             = 12L,
      transformation         = "Binned",
      weather_transformation = "None",
      temporal_agg           = "Mean",
      n_bins                 = 5L,
      binning_method         = "Equal frequency",
      custom_breaks          = NULL,
      polynomial             = character(0)
    )
  ),
  t_12m_continuous = list(
    vars = "t",
    t = list(
      ref_period             = 12L,
      transformation         = "Continuous",
      weather_transformation = "None",
      temporal_agg           = "Mean",
      n_bins                 = 5L,
      binning_method         = "Equal frequency",
      custom_breaks          = NULL,
      polynomial             = character(0)
    )
  )
)

HIST_YEARS      <- c(1991L, 2020L)
SSP             <- "ssp3_7_0"
FUT_PERIODS     <- list(c(2025L, 2035L), c(2045L, 2055L))
RESIDUALS       <- "original"
N_DRAWS         <- 150L
DEV_MODE        <- FALSE
SKIP_COEF_DRAWS <- FALSE
PROPAGATE_ALL   <- FALSE

AGG_METHODS <- c("mean", "headcount_ratio")
BAND_WIDTH  <- "p10_p90"
ENS_BAND    <- "minmax"
PLOT_WIDTH  <- 10
PLOT_HEIGHT <- 6
PLOT_DPI    <- 150
OVERWRITE   <- TRUE

# =============================================================================
# SECTION 2 — SETUP
# =============================================================================

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

connection_params <- build_connection_params(CONNECTION_TYPE)

var_info    <- load_data("metadata/variable_list.csv", connection_params, collect = TRUE)
survey_list <- load_data("metadata/survey_list.csv",   connection_params, collect = TRUE)
cpi_ppp     <- load_data("metadata/cpi_ppp.csv",       connection_params, collect = TRUE)

surveys_with_fnames <- build_survey_fnames(survey_list, "hh", connection_params)

COUNTRIES <- sort(unique(surveys_with_fnames$code))
if (!is.null(COUNTRY_FILTER))
  COUNTRIES <- intersect(COUNTRIES, COUNTRY_FILTER)

cat(sprintf("Countries: %d (%s)\n\n", length(COUNTRIES),
            paste(COUNTRIES, collapse = ", ")))

fp_list <- lapply(FUT_PERIODS, function(yr)
  c(paste0(yr[1], "-01-01"), paste0(yr[2], "-12-31"))
)

SSP_LABEL <- c(
  "ssp2_4_5" = "SSP2-4.5",
  "ssp3_7_0" = "SSP3-7.0",
  "ssp5_8_5" = "SSP5-8.5"
)[[SSP]]

bq_coef <- resolve_band_q(BAND_WIDTH)
bq_ens  <- resolve_band_q(ENS_BAND)

BASE_EXCLUDE <- c(
  OUTCOME_NAME, FIXED_EFFECTS, INTERACTION_VAR,
  "hhid", "loc_id", "gaul1_code", "weight", "timestamp", "int_month",
  "year", "code", "survname", "source", "sim_year", "pop_2020",
  "loc_id_panel"
)

# =============================================================================
# SECTION 3 — HELPER FUNCTIONS
# =============================================================================

# Mirrors .one_member_delta in mod_2_02_results.R
# Aggregates one pipeline slice (one sim_year × one model) using
# aggregate_with_uncertainty_delta, with NA y_point rows excluded.
.one_member_batch <- function(pipe, idx, method, weighted, pl_v, bq, is_log) {
  valid  <- idx & !is.na(pipe$y_point)
  w_idx  <- if (weighted && !is.null(pipe$weight)) pipe$weight[valid] else NULL
  id_idx <- if (!is.null(pipe$id_vec)) pipe$id_vec[valid] else NULL
  F_idx  <- if (!SKIP_COEF_DRAWS && !is.null(pipe$F_loading)) {
    fl <- pipe$F_loading
    if (is.null(dim(fl))) fl <- matrix(fl, ncol = 1L)
    fl[valid, , drop = FALSE]
  } else NULL
  res_mode <- if (is.null(pipe$train_aug)) "none" else RESIDUALS
  aggregate_with_uncertainty_delta(
    y_point      = pipe$y_point[valid],
    F_loading    = F_idx,
    method       = method,
    weights      = w_idx,
    pov_line     = pl_v,
    residuals    = res_mode,
    train_aug    = pipe$train_aug,
    id_vec       = id_idx,
    id_col       = pipe$id_col,
    is_log       = is_log,
    band_q       = bq,
    bandwidth_p0 = 0.05
  )
}

# Mirrors .build_hist_for_method in mod_2_02_results.R
.build_hist_batch <- function(ws, method) {
  pl     <- ws$hs$pipeline
  yrs    <- sort(unique(pl$sim_year))
  is_log <- isTRUE(ws$hs$so$transform == "log")
  build_for <- function(weighted) {
    rows <- lapply(yrs, function(yr) {
      m    <- .one_member_batch(pl, pl$sim_year == yr, method, weighted,
                                ws$pl_v, ws$bq, is_log)
      sd_m <- sqrt((m$var_coef %||% 0) + (m$var_resid %||% 0))
      tibble::tibble(
        sim_year     = yr,
        value        = m$value,
        model_id     = list("Historical"),
        value_all    = list(m$value),
        value_all_sd = list(sd_m),
        F_agg_all    = list(if (is.null(m$F_agg)) NULL else matrix(m$F_agg, nrow = 1L)),
        var_within   = sd_m^2,
        var_across   = 0,
        agg_method   = method,
        weighted     = weighted,
        scenario     = "Historical"
      )
    })
    setNames(list(dplyr::bind_rows(rows)), method)
  }
  list(
    unweighted = build_for(FALSE),
    weighted   = if (!is.null(pl$weight)) build_for(TRUE) else build_for(FALSE)
  )
}

# Mirrors .build_scn_for_method in mod_2_02_results.R
.build_scn_batch <- function(ws, method) {
  if (length(ws$sc) == 0L) return(NULL)
  setNames(lapply(ws$sc, function(s) {
    pipes  <- s$pipelines
    is_log <- isTRUE(s$so$transform == "log")
    yrs    <- sort(unique(pipes[[1L]]$sim_year))
    build_for <- function(weighted) {
      rows <- lapply(yrs, function(yr) {
        mod_ids <- names(pipes) %||% paste0("m", seq_along(pipes))
        members <- Filter(Negate(is.null), lapply(seq_along(pipes), function(i) {
          m <- .one_member_batch(pipes[[i]], pipes[[i]]$sim_year == yr,
                                 method, weighted, ws$pl_v, ws$bq, is_log)
          if (is.null(m)) return(NULL)
          list(id = mod_ids[[i]], m = m)
        }))
        if (length(members) == 0L) return(NULL)
        comb   <- combine_ensemble_results(lapply(members, `[[`, "m"), band_q = ws$bq)
        if (is.null(comb)) return(NULL)
        vals_m <- vapply(members, function(x) x$m$value, numeric(1L))
        sd_m   <- sqrt(pmax(vapply(members,
                                   function(x) (x$m$var_coef %||% 0) +
                                               (x$m$var_resid %||% 0),
                                   numeric(1L)), 0))
        ids_m  <- vapply(members, function(x) x$id, character(1L))
        F_list <- lapply(members, function(x) x$m$F_agg)
        F_mat  <- if (all(vapply(F_list, is.null, logical(1L)))) NULL
                  else do.call(rbind, lapply(F_list, function(v)
                    if (is.null(v)) rep(NA_real_, length(Filter(Negate(is.null), F_list)[[1L]]))
                    else as.numeric(v)))
        tibble::tibble(
          sim_year     = yr,
          value        = comb$value,
          model_id     = list(ids_m),
          value_all    = list(vals_m),
          value_all_sd = list(sd_m),
          F_agg_all    = list(F_mat),
          var_within   = mean(sd_m^2),
          var_across   = var(vals_m) %||% 0,
          agg_method   = method,
          weighted     = weighted,
          scenario     = s$scenario_name %||% "Future"
        )
      })
      setNames(list(dplyr::bind_rows(Filter(Negate(is.null), rows))), method)
    }
    list(
      unweighted = build_for(FALSE),
      weighted   = if (!is.null(pipes[[1L]]$weight)) build_for(TRUE) else build_for(FALSE)
    )
  }), names(ws$sc))
}

# Builds model matrix from aggregated tbl (value_all × sim_year)
.by_model_matrix_batch <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
  n_models <- max(vapply(tbl$value_all, length, integer(1L)))
  pad <- function(x, n, fill) { length(x) <- n; x[is.na(x)] <- fill; x }
  vals <- do.call(cbind, lapply(tbl$value_all,    pad, n_models, NA_real_))
  sds  <- do.call(cbind, lapply(tbl$value_all_sd, pad, n_models, 0))
  list(
    vals      = vals,
    sds       = sds,
    model_ids = lapply(seq_len(n_models), function(i)
      vapply(tbl$model_id, function(m) if (length(m) >= i) m[[i]] else NA_character_,
             character(1L)))
  )
}

# Mirrors one_scenario() in pointrange_bands_rv (mod_2_02_results.R)
.one_scenario_bands <- function(tbl, scenario_label, is_hist,
                                bq_coef, bq_ens, z_lo, z_hi) {
  if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
  mm          <- .by_model_matrix_batch(tbl)
  if (is.null(mm)) return(NULL)
  model_means <- rowMeans(mm$vals, na.rm = TRUE)
  mean_v      <- mean(model_means, na.rm = TRUE)
  coef_sd     <- sqrt(mean(as.numeric(mm$sds)^2, na.rm = TRUE))
  intermod <- if (is_hist || nrow(mm$vals) <= 1L) {
    c(lo = mean_v, hi = mean_v)
  } else {
    c(lo = unname(quantile(model_means, bq_ens[["lo"]], na.rm = TRUE)),
      hi = unname(quantile(model_means, bq_ens[["hi"]], na.rm = TRUE)))
  }
  interann <- if (is_hist) {
    v <- as.numeric(mm$vals)
    c(lo = unname(quantile(v, bq_ens[["lo"]], na.rm = TRUE)),
      hi = unname(quantile(v, bq_ens[["hi"]], na.rm = TRUE)))
  } else {
    c(lo = mean(apply(mm$vals, 1L, quantile, probs = bq_ens[["lo"]], na.rm = TRUE), na.rm = TRUE),
      hi = mean(apply(mm$vals, 1L, quantile, probs = bq_ens[["hi"]], na.rm = TRUE), na.rm = TRUE))
  }
  tibble::tibble(
    scenario      = scenario_label,
    is_historical = is_hist,
    value         = mean_v,
    intermod_lo   = intermod[["lo"]],
    intermod_hi   = intermod[["hi"]],
    interann_lo   = interann[["lo"]],
    interann_hi   = interann[["hi"]],
    coef_lo       = mean_v + z_lo * coef_sd,
    coef_hi       = mean_v + z_hi * coef_sd
  )
}

# Mirrors one_scenario() in exceedance_curves_rv (mod_2_02_results.R)
# One row per GCM per rank so enhance_exceedance can build inter-model ribbon
.one_scenario_exc <- function(scenario_label, is_hist) {
  if (is_hist) {
    pipe_list <- list(Historical = hist_sim$pipeline)
  } else {
    sc <- saved_scenarios[[scenario_label]]
    if (is.null(sc)) return(NULL)
    pipe_list <- sc$pipelines
  }
  is_log <- isTRUE(hist_sim$so$transform == "log")
  rows <- lapply(names(pipe_list), function(mod_id) {
    pipe <- pipe_list[[mod_id]]
    yrs  <- sort(unique(pipe$sim_year))
    vals <- sapply(yrs, function(yr) {
      y <- pipe$y_point[pipe$sim_year == yr & !is.na(pipe$y_point)]
      if (length(y) == 0L) return(NA_real_)
      mean(if (is_log) exp(y) else y, na.rm = TRUE)
    })
    sds <- sapply(yrs, function(yr) {
      idx <- pipe$sim_year == yr & !is.na(pipe$y_point)
      fl  <- pipe$F_loading
      if (is.null(fl) || SKIP_COEF_DRAWS) return(0)
      if (is.null(dim(fl))) fl <- matrix(fl, ncol = 1L)
      sqrt(sum(colMeans(fl[idx, , drop = FALSE])^2))
    })
    ok      <- is.finite(vals)
    v_ok    <- vals[ok]
    s_ok    <- sds[ok]
    ord     <- order(v_ok)
    n       <- length(ord)
    tibble::tibble(
      scenario      = scenario_label,
      model_id      = mod_id,
      rank          = seq_len(n),
      welfare_val   = v_ok[ord],
      coef_sd       = s_ok[ord],
      exceed_prob   = rev((seq_len(n) - 0.5) / n),
      is_historical = is_hist
    )
  })
  dplyr::bind_rows(rows)
}

# =============================================================================
# SECTION 4 — COMBINATION GRID
# =============================================================================

grid <- expand.grid(
  country = COUNTRIES,
  wx_name = names(WEATHER_SPECS),
  stringsAsFactors = FALSE
)

cat(sprintf("Total runs: %d (%d countries x %d weather specs)\n\n",
            nrow(grid), length(COUNTRIES), length(WEATHER_SPECS)))

# =============================================================================
# SECTION 5 — MAIN LOOP
# =============================================================================

fail_log <- list()

for (ri in seq_len(nrow(grid))) {

  country <- grid$country[ri]
  wx_name <- grid$wx_name[ri]
  wx_prof <- WEATHER_SPECS[[wx_name]]
  wx_vars <- wx_prof$vars

  run_key    <- sprintf("%s / %s", country, wx_name)
  out_subdir <- file.path(OUT_DIR, country, wx_name)
  cat(sprintf("\n[%d/%d] %s\n", ri, nrow(grid), run_key))
  cat(strrep("-", 60), "\n")

  if (!OVERWRITE && dir.exists(out_subdir)) {
    existing <- list.files(out_subdir, pattern = "\\.png$")
    if (length(existing) >= length(AGG_METHODS) * 2L) {
      cat("  SKIP (outputs exist)\n"); next
    }
  }

  t_start <- proc.time()[["elapsed"]]

  # ---- STEP 1: SURVEYS ------------------------------------------------------

  ss_all <- surveys_with_fnames[surveys_with_fnames$code == country, , drop = FALSE]
  if (nrow(ss_all) == 0L) {
    cat("  SKIP — no surveys\n")
    fail_log[[run_key]] <- "no_surveys"; next
  }

  latest_year <- max(ss_all$year)
  ss_latest   <- ss_all[ss_all$year == latest_year, , drop = FALSE]
  cat(sprintf("  Surveys: %d years, Mod 2 baseline: %d\n",
              length(unique(ss_all$year)), latest_year))

  svy_base <- tryCatch({
    df <- load_data(ss_all$fname, connection_params,
                    collect = TRUE, unify_schemas = TRUE)
    df <- add_time_columns(df)
    lcu_vars <- get_lcu_vars(df, var_info)
    df |>
      assign_data_level() |>
      convert_lcu_to_ppp(cpi_ppp, lcu_vars) |>
      apply_policy_derivations()
  }, error = function(e) {
    message("  Survey load: ", conditionMessage(e)); NULL
  })
  if (is.null(svy_base)) { fail_log[[run_key]] <- "survey_load_failed"; next }
  cat(sprintf("  Loaded: %d rows\n", nrow(svy_base)))

  # H3 panel IDs (soft fail)
  svy_base <- tryCatch({
    h3_fnames <- ss_all |>
      dplyr::distinct(code, year, survname, source) |>
      dplyr::mutate(fname = paste0(
        "microdata/h3/", code, "/",
        code, "_", year, "_", survname, "_", source, "_h3.parquet"
      )) |>
      dplyr::pull(fname)
    h3_df     <- load_data(h3_fnames, connection_params)
    panel_map <- loc_panel(h3_df, id_col = loc_id, h3_col = h3,
                           weight_col = pop_2020,
                           group_cols = c("code", "year", "survname"))
    loc_keys  <- h3_df |>
      dplyr::distinct(code, year, survname, loc_id) |>
      dplyr::collect()
    svy_base |>
      dplyr::left_join(
        dplyr::left_join(loc_keys, panel_map,
                         by = c("code", "year", "survname", "loc_id")),
        by = c("code", "year", "survname", "loc_id")
      )
  }, error = function(e) {
    message("  H3 panel (continuing): ", conditionMessage(e))
    svy_base
  })

  # ---- STEP 2: WEATHER + OUTCOME SPEC ---------------------------------------

  spec_inputs <- list()
  for (v in wx_vars) {
    vs <- wx_prof[[v]]
    p  <- paste0(v, "_")
    spec_inputs[[paste0(p, "relativePeriod")]]  <- c(vs$ref_period, vs$ref_period)
    spec_inputs[[paste0(p, "temporalAgg")]]     <- vs$temporal_agg
    spec_inputs[[paste0(p, "varConstruction")]] <- vs$weather_transformation
    spec_inputs[[paste0(p, "contOrBinned")]]    <- vs$transformation
    spec_inputs[[paste0(p, "numBins")]]         <- vs$n_bins
    spec_inputs[[paste0(p, "binningMethod")]]   <- vs$binning_method
    spec_inputs[[paste0(p, "customBreaks")]]    <- vs$custom_breaks
    spec_inputs[[paste0(p, "polynomial")]]      <- vs$polynomial
  }

  selected_weather <- tryCatch(
    build_selected_weather(selected_vars = wx_vars,
                           var_info      = get_weather_vars(var_info),
                           spec_inputs   = spec_inputs),
    error = function(e) { message("  Weather build: ", conditionMessage(e)); NULL }
  )
  if (is.null(selected_weather)) {
    fail_log[[run_key]] <- "weather_build_failed"; next
  }

  selected_outcome <- tryCatch(
    build_selected_outcome(
      info         = var_info[var_info$name == OUTCOME_NAME, ],
      currency     = CURRENCY,
      poverty_line = POVERTY_LINE
    ),
    error = function(e) { message("  Outcome build: ", conditionMessage(e)); NULL }
  )
  if (is.null(selected_outcome)) {
    fail_log[[run_key]] <- "outcome_build_failed"; next
  }

  # ---- STEP 3: HISTORICAL WEATHER (for Mod 1 fitting) ----------------------

  sim_dates <- build_hist_sim_dates(svy_base, HIST_YEARS)

  weather_data <- tryCatch(
    get_weather(
      survey_data       = svy_base,
      selected_surveys  = ss_all,
      selected_weather  = selected_weather,
      dates             = sim_dates,
      connection_params = connection_params
    ),
    error = function(e) { message("  Weather load: ", conditionMessage(e)); NULL }
  )
  if (is.null(weather_data)) {
    fail_log[[run_key]] <- "weather_load_failed"; next
  }

  stored_breaks <- attr(weather_data, "stored_breaks")

  svy_wx <- merge_survey_weather(svy_base, weather_data[["historical"]])
  if (is.null(svy_wx) || nrow(svy_wx) == 0L) {
    fail_log[[run_key]] <- "weather_merge_empty"; next
  }

  survey_prep <- tryCatch(
    prepare_outcome_df(svy_wx, selected_outcome),
    error = function(e) { message("  Outcome prep: ", conditionMessage(e)); NULL }
  )
  if (is.null(survey_prep)) {
    fail_log[[run_key]] <- "outcome_prep_failed"; next
  }

  # ---- STEP 4: LASSO + FIT MOD 1 -------------------------------------------

  exclude_cols <- unique(c(BASE_EXCLUDE, selected_weather$name))

  valid_vl <- tryCatch(
    filter_valid_vars(svy_wx, var_info, min_complete = 0.9,
                      group_cols = c("code", "year", "survname"),
                      outcome    = OUTCOME_NAME),
    error = function(e) { message("  filter_valid_vars: ", conditionMessage(e)); NULL }
  )
  if (is.null(valid_vl)) { fail_log[[run_key]] <- "valid_vars_failed"; next }
  valid_vl <- valid_vl[!valid_vl$name %in% exclude_cols, , drop = FALSE]

  vl_lasso <- valid_vl
  if (length(unlist(LASSO_FORCE_OUT)) > 0)
    vl_lasso <- vl_lasso[!vl_lasso$name %in% unlist(LASSO_FORCE_OUT), , drop = FALSE]

  lasso_res <- tryCatch(
    run_lasso_selection(
      df                  = survey_prep,
      selected_outcome    = selected_outcome,
      weather_vars        = selected_weather$name,
      fe_vars             = FIXED_EFFECTS,
      int_vars            = INTERACTION_VAR,
      valid_vl            = vl_lasso,
      model_type          = MODEL_TYPE,
      alpha               = LASSO_ALPHA,
      lambda_choice       = LASSO_LAMBDA,
      nfolds              = LASSO_NFOLDS,
      standardize         = LASSO_STANDARDIZE,
      mi_m                = MI_M,
      mi_maxit            = MI_MAXIT,
      stability_threshold = STABILITY_THRESHOLD
    ),
    error = function(e) { message("  LASSO: ", conditionMessage(e)); NULL }
  )
  if (is.null(lasso_res)) { fail_log[[run_key]] <- "lasso_failed"; next }

  resolve_role <- function(role) {
    base <- valid_vl$name[
      !is.na(valid_vl[[role]]) &
        valid_vl[[role]] == 1 &
        valid_vl$name %in% lasso_res$selected_covariates
    ]
    setdiff(unique(c(base, LASSO_FORCE_IN[[role]])), LASSO_FORCE_OUT[[role]])
  }

  selected_model <- tryCatch(
    build_selected_model(
      model_type          = MODEL_TYPE,
      interactions        = INTERACTION_VAR,
      interaction_mode    = "pairwise",
      fixedeffects        = FIXED_EFFECTS,
      covariate_selection = "Lasso",
      ind_covariates      = resolve_role("ind"),
      hh_covariates       = resolve_role("hh"),
      firm_covariates     = resolve_role("firm"),
      area_covariates     = resolve_role("area"),
      lasso_alpha         = LASSO_ALPHA,
      lasso_lambda        = LASSO_LAMBDA,
      lasso_nfolds        = LASSO_NFOLDS,
      lasso_standardize   = LASSO_STANDARDIZE,
      mi_m                = MI_M
    ),
    error = function(e) { message("  build_selected_model: ", conditionMessage(e)); NULL }
  )
  if (is.null(selected_model)) { fail_log[[run_key]] <- "model_build_failed"; next }

  model_obj <- tryCatch(
    fit_model(
      df               = survey_prep,
      selected_outcome = selected_outcome,
      selected_weather = selected_weather,
      selected_model   = selected_model
    ),
    error = function(e) { message("  fit_model: ", conditionMessage(e)); NULL }
  )
  if (is.null(model_obj)) { fail_log[[run_key]] <- "fit_model_failed"; next }
  cat(sprintf("  Mod 1 fit OK\n"))

  mf <- list(
    fit3          = model_obj$fit3,
    engine        = model_obj$engine,
    train_data    = model_obj$train_data,
    weather_terms = model_obj$weather_terms
  )

  # ---- STEP 5: MOD 2 SIMULATION ---------------------------------------------

  perturbation_method <- tryCatch(
    build_perturbation_method(selected_weather),
    error = function(e) { message("  perturbation_method: ", conditionMessage(e)); NULL }
  )
  if (is.null(perturbation_method)) { fail_log[[run_key]] <- "perturbation_failed"; next }

  sim_result <- tryCatch(
    fct_run_simulation(
      sw                                  = selected_weather,
      so                                  = selected_outcome,
      svy                                 = svy_base,
      ss                                  = ss_all,
      mf                                  = mf,
      cp                                  = connection_params,
      fp_list                             = fp_list,
      ssps                                = SSP,
      residuals                           = RESIDUALS,
      dev_mode                            = DEV_MODE,
      skip_coef_draws                     = SKIP_COEF_DRAWS,
      propagate_all_covariate_uncertainty = PROPAGATE_ALL,
      sim_dates                           = sim_dates,
      perturbation_method                 = perturbation_method,
      stored_breaks                       = stored_breaks
    ),
    error = function(e) { message("  fct_run_simulation: ", conditionMessage(e)); NULL }
  )
  if (is.null(sim_result)) { fail_log[[run_key]] <- "simulation_failed"; next }

  hist_sim        <- sim_result$hist_sim_result
  saved_scenarios <- sim_result$new_scenarios
  cat(sprintf("  Simulation OK — %d scenarios\n", length(saved_scenarios)))

  # ---- STEP 6: AGGREGATE + PLOT ---------------------------------------------

  dir.create(out_subdir, showWarnings = FALSE, recursive = TRUE)

  ws <- list(
    hs   = hist_sim,
    sc   = saved_scenarios,
    bq   = bq_coef,
    pl_v = POVERTY_LINE,
    bw   = BAND_WIDTH,
    res  = RESIDUALS,
    skip = SKIP_COEF_DRAWS
  )

  z_coef_lo <- stats::qnorm(bq_coef[["lo"]])
  z_coef_hi <- stats::qnorm(bq_coef[["hi"]])
  hist_ref  <- 0

  for (agg_method in AGG_METHODS) {
    cat(sprintf("  Plotting: %s\n", agg_method))

    hist_agg <- tryCatch(
      .build_hist_batch(ws, agg_method),
      error = function(e) { message("  hist_agg: ", conditionMessage(e)); NULL }
    )
    if (is.null(hist_agg)) next

    scn_agg <- tryCatch(
      .build_scn_batch(ws, agg_method),
      error = function(e) { message("  scn_agg: ", conditionMessage(e)); NULL }
    )
    if (is.null(scn_agg)) next

    # --- bands_tbl → hero plot -----------------------------------------------
    bands_tbl <- tryCatch({
      hist_row <- .one_scenario_bands(
        hist_agg$unweighted[[agg_method]], "Historical", TRUE,
        bq_coef, bq_ens, z_coef_lo, z_coef_hi
      )
      scn_rows <- lapply(names(scn_agg), function(sc_nm)
        .one_scenario_bands(
          scn_agg[[sc_nm]]$unweighted[[agg_method]], sc_nm, FALSE,
          bq_coef, bq_ens, z_coef_lo, z_coef_hi
        )
      )
      dplyr::bind_rows(c(list(hist_row), Filter(Negate(is.null), scn_rows)))
    }, error = function(e) { message("  bands_tbl: ", conditionMessage(e)); NULL })

    if (!is.null(bands_tbl) && nrow(bands_tbl) > 0L) {
      p_hero <- tryCatch(
        plot_pointrange_climate(
          bands_tbl = bands_tbl,
          x_label   = label_agg_method(agg_method)
        ),
        error = function(e) { message("  hero plot: ", conditionMessage(e)); NULL }
      )
      if (!is.null(p_hero)) {
        fname <- file.path(out_subdir, sprintf("%s_hero.png", agg_method))
        ggplot2::ggsave(fname, p_hero,
                        width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
        cat(sprintf("    Saved: %s\n", basename(fname)))
      }
    }

    # --- curves_tbl → exceedance plot ----------------------------------------
    curves_tbl <- tryCatch({
      hist_exc <- .one_scenario_exc("Historical", TRUE)
      scn_exc  <- lapply(names(scn_agg), function(sc_nm)
        .one_scenario_exc(sc_nm, FALSE)
      )
      dplyr::bind_rows(c(list(hist_exc), Filter(Negate(is.null), scn_exc)))
    }, error = function(e) { message("  curves_tbl: ", conditionMessage(e)); NULL })

    if (!is.null(curves_tbl) && nrow(curves_tbl) > 0L) {
      p_exc <- tryCatch(
        enhance_exceedance(
          curves_tbl = curves_tbl,
          x_label    = label_agg_method(agg_method),
          band_q     = bq_coef
        ),
        error = function(e) { message("  exceedance plot: ", conditionMessage(e)); NULL }
      )
      if (!is.null(p_exc)) {
        fname <- file.path(out_subdir, sprintf("%s_exceedance.png", agg_method))
        ggplot2::ggsave(fname, p_exc,
                        width = PLOT_WIDTH, height = PLOT_HEIGHT, dpi = PLOT_DPI)
        cat(sprintf("    Saved: %s\n", basename(fname)))
      }
    }

  }  # end agg_method loop

  t_elapsed <- round(proc.time()[["elapsed"]] - t_start, 1)
  cat(sprintf("  Done in %.1fs\n", t_elapsed))

}  # end main loop

# =============================================================================
# SECTION 6 — SUMMARY
# =============================================================================

cat("\n", strrep("=", 60), "\n")
cat("BATCH COMPLETE\n")
cat(strrep("=", 60), "\n\n")

if (length(fail_log) == 0L) {
  cat("All runs succeeded.\n")
} else {
  cat(sprintf("%d run(s) failed:\n\n", length(fail_log)))
  fail_df <- data.frame(
    run    = names(fail_log),
    reason = as.character(unlist(fail_log)),
    stringsAsFactors = FALSE
  )
  print(fail_df, row.names = FALSE)
  readr::write_csv(fail_df, file.path(OUT_DIR, "fail_log.csv"))
  cat(sprintf("\nFail log: %s\n", file.path(OUT_DIR, "fail_log.csv")))
}