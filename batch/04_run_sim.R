# =============================================================================
# batch/04_run_sim.R
#
# Batch model fitting + simulations across countries and specifications.
#
# Outputs:
#   OUT_DIR/simulations/outcomes.csv
#   OUT_DIR/simulations/sim_stats.csv
#   OUT_DIR/simulations/_failures.csv (error logging)
#   OUT_DIR/policy/policy_outcomes.csv
#   OUT_DIR/policy/_failures.csv (error logging)
#
# All user inputs are set in SECTION 1. Vector-valued settings (marked [GRID])
# expand into separate runs via expand.grid(); scalar settings apply uniformly.
#
# Usage: source("batch/04_run_sim.R")
# =============================================================================

pkgload::load_all(quiet = TRUE)
invisible(lapply(list.files("batch/R", pattern = "\\.R$", full.names = TRUE), source))

# =============================================================================
# SECTION 1 — CONFIGURATION
# =============================================================================

# ---- Data source (mod_0) ----------------------------------------------------
# "local"      -> set WISEAPP_DATA_PATH in .Renviron
# "databricks" -> credentials from .Renviron (DATABRICKS_HOST, etc.)
CONNECTION_TYPE <- "local"
DATA_DIR        <- Sys.getenv("WISEAPP_DATA_PATH")
OUT_DIR         <- "dev/outputs/"

# ---- Unit of analysis -------------------------------------------------------
UNIT <- "hh"   # "hh", "ind", or "firm"

# ---- Sample mode ------------------------------------------------------------
POOL_COUNTRIES <- FALSE   # TRUE = one pooled model; FALSE = per-country

# ---- Country / survey sample (mod_1_01) [GRID when !POOL_COUNTRIES] --------
# NULL = all available; c(...) = subset
COUNTRY_FILTER <- "GNB"

# ---- Outcome variable (mod_1_03) --------------------------------------------
OUTCOME_NAME <- "welfare"
CURRENCY     <- "PPP"
POVERTY_LINE <- 3

# ---- Weather specs (mod_1_04) [GRID] ----------------------------------------
# Named list of weather profiles. Each profile maps weather variables to their
# settings (ref_start, ref_end, transformation). A profile with multiple
# variables includes them all in one model.
# Can use expand_weather_specs() to generate single-variable profiles systematically.
# For multi-variable profiles, define configuration manually:
#   list(t_r_12m = list(t = list(ref_start = 1L, ref_end = 12L, transformation = "continuous"),
#                       r = list(ref_start = 1L, ref_end = 12L, transformation = "continuous")))

WEATHER_SPECS <- c(
  expand_weather_specs(
    "t", c(12L),
    transformations    = "continuous",
    var_constructions  = c("None"),
    ref_starts         = 1L        # start month (months before interview); 1 = most recent
  )
)

# ---- Weather defaults (used when a profile omits a setting) -----------------
WEATHER_TRANSFORMATION <- "None"
N_BINS                 <- 5L
BINNING_METHOD         <- "Equal frequency"
CUSTOM_BREAKS          <- NULL
POLYNOMIAL             <- character(0)
WEATHER_AGG_OVERRIDE   <- NULL

# ---- Model type (mod_1_06) [GRID] -------------------------------------------
# "Linear regression", "Quantile regression (RIF)", "Logistic regression"
MODEL_TYPE <- c("Linear regression", "Quantile regression (RIF)")

# ---- Interactions (mod_1_06) [GRID] -----------------------------------------
# character(0) = no interaction; each entry interacts that variable with weather
INTERACTIONS <- list(character(0))

# ---- Fixed effects (mod_1_06) [GRID] ----------------------------------------
# Named list of FE profiles. Values are character vectors passed to fixest.
FIXED_EFFECTS <- list(
  year_admin1 = c("year", "gaul1_code"),
  year_loc    = c("year", "loc_id_panel")
)

# ---- Covariate specs [GRID] -------------------------------------------------
# Named list of covariate profiles. Each must have `method` ("User-defined" or
# "Lasso"). User-defined profiles supply covariates by role.
COVARIATE_SPECS <- list(
  hhsize_urban_area = list(
    method = "User-defined",
    ind = character(0), hh = c("hhsize", "urban"),
    firm = character(0), area = c("area_h3_7")
  )
  #, lasso = list(method = "Lasso")
)

# ---- Lasso settings ---------------------------------------------------------
LASSO_ALPHA         <- 1
LASSO_LAMBDA        <- "lambda.1se"
LASSO_NFOLDS        <- 10L
LASSO_STANDARDIZE   <- TRUE
MI_M                <- 5L
MI_MAXIT            <- 5L
STABILITY_THRESHOLD <- 0.5
LASSO_USE_PARALLEL  <- FALSE
LASSO_N_WORKERS     <- NULL   # NULL = auto-detect all cores
LASSO_PARALLEL_SEED <- NULL   # NULL = auto-managed parallel-safe seeds
LASSO_GLOBALS_MAX   <- NULL   # NULL = max(2 GB, current setting)

LASSO_FORCE_IN <- list(
  ind = character(0), hh = character(0),
  firm = character(0), area = character(0)
)
LASSO_FORCE_OUT <- list(
  ind = character(0), hh = character(0),
  firm = character(0), area = character(0)
)

# ---- Output -----------------------------------------------------------------
OVERWRITE_EXISTING <- TRUE

# =============================================================================
# SECTION 1B — SIMULATION SETTINGS (mod_2)
# =============================================================================

# ---- Historical weather distribution period ---------------------------------
# Used to characterise the baseline weather distribution. 30-year span recommended.
HIST_YEARS <- c(1991L, 2020L)

# ---- Future projection periods (up to 3) ------------------------------------
# Each is a 2-integer vector c(start_year, end_year), or NULL to skip.
FUT_PERIOD_1 <- c(2025L, 2035L)
FUT_PERIOD_2 <- NULL   # e.g. c(2045L, 2055L)
FUT_PERIOD_3 <- NULL   # e.g. c(2070L, 2080L)

# ---- Climate scenarios -------------------------------------------------------
# Any subset of: "ssp2_4_5", "ssp3_7_0", "ssp5_8_5"
# Set to character(0) to run historical simulation only (no future projections).
SSPS <- c("ssp3_7_0")

# ---- Simulation residuals ---------------------------------------------------
# "original"  — (recommended) each observation keeps its own model residual
# "resample"  — residuals resampled from the training distribution
# "normal"    — draw from N(0, sigma); assumes normality and homoskedasticity
# "none"      — DIAGNOSTIC: fitted values only (understates variance)
RESIDUALS <- "original"

# ---- Coefficient uncertainty (delta method) ---------------------------------
# When TRUE, propagates regression-coefficient VCV via the analytic delta method.
INCLUDE_COEF_UNCERTAINTY <- TRUE

# When FALSE (default), only coefficients on variables that change between
# baseline and counterfactual contribute to var_coef (additive-decomposition SE,
# exact under additive separability). Set TRUE to propagate all covariates
# (conservative but inconsistent with the model's own separability assumption).
# Ignored when RESIDUALS != "original".
PROPAGATE_ALL_COVARIATE_UNCERTAINTY <- FALSE

# ---- Monte Carlo draws (fallback path only) ---------------------------------
# The delta-method is used for all standard aggregates; MC is the fallback for
# aggregates where the gradient is unavailable (e.g. avg_poverty with few poor).
SIM_N    <- 150L
DEV_MODE <- FALSE   # TRUE = 1 ensemble member only; fast debug runs

# =============================================================================
# SECTION 1C — POLICY SCENARIO SETTINGS (mod_3)
# =============================================================================
# Each scenario list mirrors the reactive API returned by the corresponding
# mod_3_0* server. Set all fields to 0 / FALSE / NULL to disable a module.
# apply_policy_to_svy() silently skips modules with no active changes.

# ---- Social protection (mod_3_01_sp) ----------------------------------------
# transfer_amount_usd: annual per-household transfer in USD PPP
# budget_fixed: total program budget in USD PPP (used when budget_mode = "budget_first")
# targeting: "universal" | "exante_poor" | "pmt"
#   exante_poor: households below targeting_threshold percentile of welfare
#   pmt: households where pmt_variable is at or below pmt_cutoff
# transfer_n_payments: payments per year (regular programs only)
SP_SCENARIO <- list(
  sp_type             = "regular",
  budget_mode         = "transfer_first",   # "transfer_first" | "budget_first"
  budget_fixed        = 0,
  targeting           = "universal",        # "universal" | "exante_poor" | "pmt"
  targeting_threshold = 20,                 # percentile (exante_poor only)
  pmt_variable        = NA_character_,      # variable name (pmt only)
  pmt_cutoff          = NA_real_,           # cutoff value (pmt only)
  inclusion_error_pct = 10,                 # % non-eligible incorrectly included
  exclusion_error_pct = 10,                 # % eligible incorrectly excluded
  transfer_amount_usd = 0,                  # set > 0 to activate SP
  transfer_frequency  = "regular",
  transfer_n_payments = 6L,
  transfer_timing     = NA_character_,
  timeliness_weeks    = NA_integer_
)

# ---- Infrastructure access (mod_3_02_infra) ----------------------------------
# *_universal: TRUE = set 100% access; FALSE = apply *_access_change_pct
# *_access_change_pct: % of currently-without-access HHs to flip (negative = remove)
# health_mode: "pct" = reduce travel time by %; "max" = cap at health_travel_max mins
INFRA_SCENARIO <- list(
  elec_universal              = FALSE,
  elec_access_change_pct      = 0L,
  water_universal             = FALSE,
  water_access_change_pct     = 0L,
  sanitation_universal        = FALSE,
  sanitation_access_change_pct = 0L,
  piped_universal             = FALSE,
  piped_access_change_pct     = 0L,
  piped_to_prem_universal     = FALSE,
  piped_to_prem_access_change_pct = 0L,
  imp_wat_san_universal       = FALSE,
  imp_wat_san_access_change_pct = 0L,
  health_mode                 = "pct",   # "pct" | "max"
  health_travel_pct           = 0L,      # % change (negative = improvement)
  health_travel_max           = 60L      # max travel time in minutes (max mode only)
)

# ---- Digital inclusion (mod_3_03_digital) ------------------------------------
DIGITAL_SCENARIO <- list(
  internet_universal         = FALSE,
  internet_access_change_pct = 0L,
  mobile_universal           = FALSE,
  mobile_access_change_pct   = 0L
)

# ---- Labor market (mod_3_04_labor) -------------------------------------------
# employment_change_pp: percentage-point change in employment rate (-20 to +20)
# sector_*: share of employment (0-100); agriculture = 100 - manufacturing - services
LABOR_SCENARIO <- list(
  employment_change_pp = 0,
  sector_manufacturing = 0,
  sector_services      = 0,
  sector_agriculture   = 100
)

# =============================================================================
# SECTION 2 — SETUP
# =============================================================================

dir.create(file.path(OUT_DIR, "simulations"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "policy"),      showWarnings = FALSE, recursive = TRUE)

connection_params <- if (identical(CONNECTION_TYPE, "databricks")) {
  build_connection_params("databricks")
} else {
  build_connection_params("local", path = DATA_DIR)
}
stopifnot("Invalid connection_params" = validate_connection_params(connection_params))

cat("================================================================\n")
cat("WISE-APP Batch Pipeline\n")
cat(sprintf("Connection: %s\n",
            if (identical(connection_params$type, "databricks"))
              "Databricks" else paste0("local (", connection_params$path, ")")))
cat(sprintf("OUT_DIR:    %s\n", OUT_DIR))
cat(sprintf("Unit:       %s\n", UNIT))
cat(sprintf("Filter:     %s\n", if (is.null(COUNTRY_FILTER)) "all countries"
            else paste(COUNTRY_FILTER, collapse = ", ")))
cat("================================================================\n\n")

var_info    <- load_data("metadata/variable_list.csv", connection_params, collect = TRUE)
survey_list <- load_data("metadata/survey_list.csv",   connection_params, collect = TRUE)
cpi_ppp     <- load_data("metadata/cpi_ppp.csv",       connection_params, collect = TRUE)

LEVEL   <- switch(UNIT, hh = "hh", ind = "ind", firm = "firm", "hh")
surveys_with_fnames <- build_survey_fnames(survey_list, LEVEL, connection_params)
COUNTRIES <- sort(unique(surveys_with_fnames$code))
if (!is.null(COUNTRY_FILTER))
  COUNTRIES <- intersect(COUNTRIES, COUNTRY_FILTER)

cat(sprintf("Countries: %d (%s)\n\n", length(COUNTRIES),
            paste(COUNTRIES, collapse = ", ")))

# ---- Combination grid -------------------------------------------------------

if (POOL_COUNTRIES) {
  SAMPLE_LABELS <- if (is.null(COUNTRY_FILTER) || length(COUNTRY_FILTER) == length(COUNTRIES))
    "All countries" else paste(COUNTRIES, collapse = "_")
  SAMPLE_CODES  <- setNames(list(COUNTRIES), SAMPLE_LABELS)
} else {
  SAMPLE_LABELS <- COUNTRIES
  SAMPLE_CODES  <- setNames(as.list(COUNTRIES), COUNTRIES)
}

grid <- expand.grid(
  sample      = SAMPLE_LABELS,
  weather     = names(WEATHER_SPECS),
  model_type  = MODEL_TYPE,
  interaction = I(INTERACTIONS),
  fe          = names(FIXED_EFFECTS),
  covariates  = names(COVARIATE_SPECS),
  stringsAsFactors = FALSE
)

cat(sprintf("Mode: %s\n", if (POOL_COUNTRIES) "pooled" else "per-country"))
cat(sprintf(
  "Total specs: %d (%d sample x %d weather x %d model x %d interaction x %d FE x %d cov)\n\n",
  nrow(grid), length(SAMPLE_LABELS), length(WEATHER_SPECS), length(MODEL_TYPE),
  length(INTERACTIONS), length(FIXED_EFFECTS), length(COVARIATE_SPECS)
))

# =============================================================================
# SECTION 3 — STEP 1: MODEL FITTING
# =============================================================================
# Mirrors 03_run_mod1.R but collects fit_store (in-memory) instead of CSVs.
# fit_store is a named list keyed by spec_label; each entry holds everything
# fct_run_simulation() and apply_policy_to_svy() need in Steps 2 and 3.

cat("=== Step 1: Model fitting ===\n")

fit_store <- list()   # passed to Steps 2 and 3
fail_log  <- list()
skip_log  <- list()
run_idx   <- 0L

for (si in SAMPLE_LABELS) {
  sample_codes <- SAMPLE_CODES[[si]]
  cat(sprintf("\n=== %s ===\n", si))

  years_by_code <- setNames(
    lapply(sample_codes, function(ci)
      as.character(sort(unique(
        surveys_with_fnames$year[surveys_with_fnames$code == ci]
      )))),
    sample_codes
  )

  ss <- build_selected_surveys(surveys = surveys_with_fnames,
                               years_by_code = years_by_code)
  if (nrow(ss) == 0) {
    cat("  SKIP — no surveys\n")
    fail_log[[si]] <- "no_surveys"; next
  }

  svy_base <- tryCatch({
    df <- load_data(ss$fname, connection_params, collect = TRUE,
                    unify_schemas = TRUE)
    df <- add_time_columns(df)
    lcu_vars <- get_lcu_vars(df, var_info)
    df |>
      assign_data_level() |>
      convert_lcu_to_ppp(cpi_ppp, lcu_vars) |>
      bottom_code_welfare(0.28) |>
      apply_policy_derivations()
  }, error = function(e) { message("  load failed: ", conditionMessage(e)); NULL })

  if (is.null(svy_base)) { fail_log[[si]] <- "load_failed"; next }
  cat(sprintf("  Loaded: %d rows (%s)\n", nrow(svy_base),
              paste(sample_codes, collapse = ", ")))

  tryCatch({
    h3_fnames <- ss |>
      dplyr::distinct(code, year, survname, source) |>
      dplyr::mutate(fname = paste0(
        "microdata/h3/", code, "/",
        code, "_", year, "_", survname, "_", source, "_h3.parquet"
      )) |>
      dplyr::pull(fname)
    h3_df     <- load_data(h3_fnames, connection_params)
    panel_map <- loc_panel(h3_df, id_col = loc_id, h3_col = h3,
                           weight_col = pop_2020, group_cols = c("code", "year", "survname"))
    loc_keys  <- h3_df |>
      dplyr::distinct(code, year, survname, loc_id) |>
      dplyr::collect()
    svy_base <- svy_base |>
      dplyr::left_join(
        dplyr::left_join(loc_keys, panel_map,
                         by = c("code", "year", "survname", "loc_id")),
        by = c("code", "year", "survname", "loc_id")
      )
    cat(sprintf("  loc_id_panel: %d groups\n",
                length(unique(svy_base$loc_id_panel))))
  }, error = function(e) {
    message("  loc_id_panel failed (SEs fall back to ~loc_id): ",
            conditionMessage(e))
  })

  wx_profiles <- unique(grid$weather[grid$sample == si])

  for (wx_name in wx_profiles) {
    wx_prof <- WEATHER_SPECS[[wx_name]]
    wx_vars <- names(wx_prof)

    spec_inputs <- list()
    for (v in wx_vars) {
      vs <- wx_prof[[v]]
      p  <- paste0(v, "_")
      spec_inputs[[paste0(p, "relativePeriod")]] <- c(vs$ref_start %||% 1L, vs$ref_end)
      spec_inputs[[paste0(p, "temporalAgg")]]    <- vs$temporal_agg %||%
        weather_agg_for(v, get_weather_vars(var_info), WEATHER_AGG_OVERRIDE)
      spec_inputs[[paste0(p, "varConstruction")]] <- vs$weather_transformation %||% WEATHER_TRANSFORMATION
      spec_inputs[[paste0(p, "contOrBinned")]]    <- if (vs$transformation == "binned") "Binned" else "Continuous"
      spec_inputs[[paste0(p, "numBins")]]         <- vs$n_bins %||% N_BINS
      spec_inputs[[paste0(p, "binningMethod")]]   <- vs$binning_method %||% BINNING_METHOD
      spec_inputs[[paste0(p, "customBreaks")]]    <- vs$custom_breaks %||% CUSTOM_BREAKS[[v]]
      spec_inputs[[paste0(p, "polynomial")]]      <- vs$polynomial %||% POLYNOMIAL
    }

    selected_weather <- tryCatch(
      build_selected_weather(selected_vars = wx_vars,
                             var_info = get_weather_vars(var_info),
                             spec_inputs = spec_inputs),
      error = function(e) { message(" weather build: ", conditionMessage(e)); NULL }
    )
    if (is.null(selected_weather) || nrow(selected_weather) == 0) {
      cat(sprintf("  FAIL (weather build) — %s\n", wx_name))
      fail_log[[paste(si, wx_name, sep = "_")]] <- "weather_build_failed"; next
    }

    cat(sprintf("  Loading weather [%s]...", wx_name))
    weather_data <- tryCatch(
      get_weather(survey_data = svy_base, selected_surveys = ss,
                  selected_weather = selected_weather,
                  dates = extract_survey_dates(svy_base),
                  connection_params = connection_params),
      error = function(e) { message(" get_weather: ", conditionMessage(e)); NULL }
    )
    if (is.null(weather_data)) {
      cat(" FAIL\n")
      fail_log[[paste(si, wx_name, sep = "_")]] <- "weather_load_failed"; next
    }
    cat(" done\n")

    stored_breaks <- attr(weather_data, "stored_breaks")
    svy_wx <- merge_survey_weather(svy_base, weather_data[["historical"]])
    if (is.null(svy_wx) || nrow(svy_wx) == 0) {
      cat("  FAIL — weather merge produced 0 rows\n")
      fail_log[[paste(si, wx_name, sep = "_")]] <- "weather_merge_empty"; next
    }
    cat(sprintf("  Merged: %d rows\n", nrow(svy_wx)))

    # Most-recent survey year per country — mirrors app's baseline_svy reactive
    # (mod_2_01_weathersim.R: filters survey_weather to selected baseline survey)
    svy_baseline <- svy_wx |>
      dplyr::group_by(code) |>
      dplyr::filter(year == max(year, na.rm = TRUE)) |>
      dplyr::ungroup()

    wx_col_names <- intersect(selected_weather$name, names(svy_wx))

    model_combos <- grid[grid$sample == si & grid$weather == wx_name, , drop = FALSE]

    for (mi in seq_len(nrow(model_combos))) {
      cur_model_type  <- model_combos$model_type[mi]
      interaction_var <- model_combos$interaction[[mi]]
      fe_label        <- model_combos$fe[mi]
      cov_label       <- model_combos$covariates[mi]
      fe_vec          <- FIXED_EFFECTS[[fe_label]]
      cov_spec        <- COVARIATE_SPECS[[cov_label]]
      cov_method      <- cov_spec$method

      run_idx     <- run_idx + 1L
      inter_label <- if (length(interaction_var) == 0) "noInter" else interaction_var
      mt_label    <- if (grepl("RIF", cur_model_type)) "rif" else "ols"
      spec_label  <- sprintf("%s_%s_%s_%s_%s_%s", si, wx_name, mt_label,
                             fe_label, cov_label, inter_label)

      cat(sprintf("  [%d/%d] %s...", run_idx, nrow(grid), spec_label))

      if (length(interaction_var) > 0 && !interaction_var %in% names(svy_wx)) {
        cat(sprintf(" SKIP — '%s' not in survey\n", interaction_var))
        skip_log[[spec_label]] <- list(
          reason = sprintf("interaction_%s_not_available", interaction_var),
          sample = si, interaction = paste(interaction_var, collapse = "|")
        )
        next
      }

      t0               <- proc.time()[["elapsed"]]
      interaction_mode <- if (length(interaction_var) > 0) "pairwise" else "none"

      exclude_cols <- unique(c(
        wx_col_names, OUTCOME_NAME, fe_vec,
        "hhid", "loc_id", "gaul1_code", "weight", "timestamp", "int_month",
        "year", "code", "survname", "source", "sim_year", "pop_2020",
        "loc_id_panel"
      ))

      selected_outcome <- tryCatch(
        build_selected_outcome(info = var_info[var_info$name == OUTCOME_NAME, ],
                               currency = CURRENCY, poverty_line = POVERTY_LINE),
        error = function(e) { message(" outcome: ", conditionMessage(e)); NULL }
      )
      if (is.null(selected_outcome)) {
        cat(" FAIL (outcome)\n"); fail_log[[spec_label]] <- "outcome_build_failed"; next
      }

      survey_prep <- tryCatch(
        prepare_outcome_df(svy_wx, selected_outcome),
        error = function(e) { message(" prep: ", conditionMessage(e)); NULL }
      )
      if (is.null(survey_prep)) {
        cat(" FAIL (prep)\n"); fail_log[[spec_label]] <- "outcome_prep_failed"; next
      }

      n_complete <- sum(stats::complete.cases(
        survey_prep[, c(OUTCOME_NAME, wx_col_names), drop = FALSE]
      ))
      if (n_complete < 100L) {
        cat(sprintf(" FAIL (%d complete cases)\n", n_complete))
        fail_log[[spec_label]] <- sprintf("only_%d_complete_cases", n_complete); next
      }

      valid_vl <- filter_valid_vars(svy_wx, var_info, min_complete = 0.9,
                                    group_cols = c("code", "year", "survname"),
                                    outcome = OUTCOME_NAME)
      valid_vl <- valid_vl[!valid_vl$name %in% exclude_cols, , drop = FALSE]
      valid_vl <- exclude_selected_vars(valid_vl)

      if (cov_method == "Lasso") {
        vl_for_lasso <- valid_vl
        force_exc <- unique(unlist(LASSO_FORCE_OUT))
        if (length(force_exc) > 0)
          vl_for_lasso <- vl_for_lasso[!vl_for_lasso$name %in% force_exc, , drop = FALSE]

        lasso_res <- tryCatch(
          run_lasso_selection(
            df = survey_prep, selected_outcome = selected_outcome,
            weather_vars = wx_col_names, fe_vars = fe_vec,
            int_vars = interaction_var, valid_vl = vl_for_lasso,
            model_type = cur_model_type, alpha = LASSO_ALPHA,
            lambda_choice = LASSO_LAMBDA, nfolds = LASSO_NFOLDS,
            standardize = LASSO_STANDARDIZE, mi_m = MI_M,
            mi_maxit = MI_MAXIT, stability_threshold = STABILITY_THRESHOLD,
            use_parallel = LASSO_USE_PARALLEL, n_workers = LASSO_N_WORKERS,
            parallel_seed = LASSO_PARALLEL_SEED,
            globals_max_size = LASSO_GLOBALS_MAX
          ),
          error = function(e) { message(" LASSO: ", conditionMessage(e)); NULL }
        )
        if (is.null(lasso_res)) {
          cat(" FAIL (LASSO)\n"); fail_log[[spec_label]] <- "lasso_failed"; next
        }

        resolve_role <- function(role) {
          base <- valid_vl$name[valid_vl[[role]] == 1 &
                                  valid_vl$name %in% lasso_res$selected_covariates]
          setdiff(unique(c(base, LASSO_FORCE_IN[[role]])), LASSO_FORCE_OUT[[role]])
        }
        ind_covs  <- resolve_role("ind")
        hh_covs   <- resolve_role("hh")
        firm_covs <- resolve_role("firm")
        area_covs <- resolve_role("area")
      } else {
        valid_names <- valid_vl$name
        ind_covs  <- intersect(cov_spec$ind  %||% character(0), valid_names)
        hh_covs   <- intersect(cov_spec$hh   %||% character(0), valid_names)
        firm_covs <- intersect(cov_spec$firm  %||% character(0), valid_names)
        area_covs <- intersect(cov_spec$area  %||% character(0), valid_names)
      }

      selected_model <- tryCatch(
        build_selected_model(
          model_type = cur_model_type, interactions = interaction_var,
          interaction_mode = interaction_mode, fixedeffects = fe_vec,
          covariate_selection = cov_method,
          ind_covariates = ind_covs, hh_covariates = hh_covs,
          firm_covariates = firm_covs, area_covariates = area_covs,
          lasso_alpha = LASSO_ALPHA, lasso_lambda = LASSO_LAMBDA,
          lasso_nfolds = LASSO_NFOLDS, lasso_standardize = LASSO_STANDARDIZE,
          mi_m = MI_M, mi_maxit = MI_MAXIT,
          stability_threshold = STABILITY_THRESHOLD
        ),
        error = function(e) { message(" model build: ", conditionMessage(e)); NULL }
      )
      if (is.null(selected_model)) {
        cat(" FAIL (model build)\n"); fail_log[[spec_label]] <- "model_build_failed"; next
      }

      mf <- tryCatch(
        suppressWarnings(fit_model(
          df = survey_prep, selected_outcome = selected_outcome,
          selected_weather = selected_weather, selected_model = selected_model
        )),
        error = function(e) { message(" fit_model: ", conditionMessage(e)); NULL }
      )
      if (is.null(mf) || is.null(mf$fit3)) {
        cat(" FAIL (fit)\n"); fail_log[[spec_label]] <- "fit_model_failed"; next
      }

      cat(sprintf(" %.1fs DONE\n", round(proc.time()[["elapsed"]] - t0, 1)))

      # Store everything fct_run_simulation() and apply_policy_to_svy() need.
      # svy_baseline: most-recent-year survey (simulation input, mirrors app's baseline_svy).
      # svy_wx:       full merged data across all years (policy modification base).
      fit_store[[spec_label]] <- list(
        mf            = mf,
        svy_baseline  = svy_baseline,
        svy_wx        = svy_wx,
        ss            = ss,
        sw            = selected_weather,
        so            = selected_outcome,
        stored_breaks = stored_breaks,
        wx_col_names  = wx_col_names,
        code          = si,
        wx_name       = wx_name,
        mt_label      = mt_label,
        fe_label      = fe_label,
        cov_label     = cov_label,
        inter_label   = inter_label
      )

      rm(mf, survey_prep, selected_model, selected_outcome)
      gc(verbose = FALSE)
    }

    rm(svy_wx, svy_baseline, weather_data)
    gc(verbose = FALSE)
  }
}

n_fitted <- length(fit_store)
cat(sprintf(
  "\nStep 1 complete: %d fitted, %d failed, %d skipped\n",
  n_fitted, length(fail_log), length(skip_log)
))

# =============================================================================
# SECTION 4 — STEP 2: CLIMATE SIMULATIONS
# =============================================================================

cat("\n=== Step 2: Climate simulations ===\n")

# =============================================================================
# SECTION 5 — STEP 3: POLICY SIMULATIONS
# =============================================================================

cat("\n=== Step 3: Policy simulations ===\n")