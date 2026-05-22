# =============================================================================
# batch/04_run_sim.R
#
# Batch model fitting + simulations across countries and specifications.
#
# Outputs (all under OUT_DIR/simulations/):
#   outcomes.parquet                      welfare aggregates — base + policy rows
#   weather_quantiles.csv                p10-p90 per weather variable per scenario
#   sim_metadata.csv                     CMIP6 ensemble info per future scenario
#   return_periods.csv                   RP thresholds with uncertainty bands
#   decile_decomposition_by_year.parquet  policy effect channels by decile and year
#   policy_config.csv                    one row per (spec, policy) run
#   weather_distributions/*.png          overlaid KDE/bar plots per (code, wx_spec)
#   _failures.csv                        combined error log (Sections 3–5)
#
# All user inputs are set in SECTION 1. Vector-valued settings (marked [GRID])
# expand into separate runs via expand.grid(); scalar settings apply uniformly.
# Set OVERWRITE_EXISTING = FALSE to append new specs without recomputing old ones.
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
OUT_DIR         <- Sys.getenv("WISEAPP_RESULTS_PATH")
# OUT_DIR         <- "dev/outputs"

# ---- Unit of analysis -------------------------------------------------------
UNIT <- "hh"   # "hh", "ind", or "firm"

# ---- Sample mode ------------------------------------------------------------
POOL_COUNTRIES <- FALSE   # TRUE = one pooled model; FALSE = per-country

# ---- Country / survey sample (mod_1_01) [GRID when !POOL_COUNTRIES] --------
# NULL = all available; c(...) = subset

# WAEMU
COUNTRY_FILTER <- c("BEN", "BFA", "CIV", "GNB", "MLI", "NER", "SEN", "TGO")

# Other wise-app candidates with >= 2 surveys
# COUNTRY_FILTER <- c( "BRA", "COL", "GMB", "GTM", "IND", "IRN", "LKA", "MRT", "MWI", "TCD", "TJK", "VNM", "ZMB")

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
    c("t", "tx35"), c(12L),
    transformations    = c("binned"),
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
INTERACTIONS <- list(character(0))  # e.g. "urban", "electricity"...

# ---- Fixed effects (mod_1_06) [GRID] ----------------------------------------
# Named list of FE profiles. Values are character vectors passed to fixest.
FIXED_EFFECTS <- list(
  year_loc    = c("year", "loc_id_panel")
  # year_admin1 = c("year", "gaul1_code")
)

# ---- Covariate specs [GRID] -------------------------------------------------
# Named list of covariate profiles. Each must have `method` ("User-defined" or
# "Lasso"). User-defined profiles supply covariates by role.
COVARIATE_SPECS <- list(
  # hhsize_urban = list(
  #   method = "User-defined",
  #   ind = character(0), hh = c("hhsize", "urban"),
  #   firm = character(0), area = character(0)
  # ), 
  lasso = list(method = "Lasso")
)

# ---- Lasso settings ---------------------------------------------------------
LASSO_ALPHA         <- 1
LASSO_LAMBDA        <- "lambda.1se"
LASSO_NFOLDS        <- 10L
LASSO_STANDARDIZE   <- TRUE
MI_M                <- 5L
MI_MAXIT            <- 5L
STABILITY_THRESHOLD <- 0.5
LASSO_USE_PARALLEL  <- TRUE  # use parallel if > 50,000 observations; evaluated dynamically at call site
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

# ---- Aggregation methods (used in Sections 4-5) -----------------------------
# Poverty-line suffix (_300 / _420 / _830) maps to pov_line 3.00 / 4.20 / 8.30.
AGG_METHODS <- c(
  "mean", "median", "gini", "avg_poverty", "prosperity_gap",
  "headcount_ratio_300", "gap_300", "fgt2_300",
  "headcount_ratio_420", "gap_420", "fgt2_420",
  "headcount_ratio_830", "gap_830", "fgt2_830"
)

# ---- Output -----------------------------------------------------------------
OVERWRITE_EXISTING <- FALSE   # FALSE = append+dedup existing CSVs; TRUE = overwrite
SKIP_PLOTS         <- FALSE  # TRUE = skip all PNG generation (faster reruns)

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
# POLICY_SCENARIOS is a named list; one entry per scenario to run.
# Each entry has:
#   policy_keys  — character vector of POLICY_DEFINITIONS keys ("A"–"I"):
#                  A=electricity, B=imp_water, C=sanitation, D=health,
#                  E=internet, F=mobile, G=piped_water, H=piped_on_prem, I=imp_wat_san
#                  Auto-adds the corresponding variable as a weather interaction to
#                  INTERACTIONS, so the base model includes it (matching app's mod_1 flow).
#                  Each policy is applied only to specs with its matching interaction.
#   sp      — social protection module config
#   infra   — infrastructure access config
#   digital — digital inclusion config
#   labor   — labor market config
#
# SP config supports relative transfer sizing via transfer_pctile:
#   transfer_pctile     — integer percentile (e.g. 20); when set, transfer_amount_usd
#                         is resolved at runtime from the weighted welfare distribution:
#                         transfer_amount_usd = P_pctile(welfare) * 365 / transfer_n_payments
#   targeting           — "universal", "exante_poor" (bottom % by welfare), or "pmt"
#   targeting_threshold — percentile cutoff for exante_poor (e.g. 40 = bottom 40%)
#
# Helper defaults (all modules inactive) — use modifyList() to override one field.

.sp_off <- list(
  sp_type             = "regular",
  budget_mode         = "transfer_first",
  budget_fixed        = 0,
  targeting           = "universal",
  targeting_threshold = 20,
  pmt_variable        = NA_character_,
  pmt_cutoff          = NA_real_,
  inclusion_error_pct = 10,
  exclusion_error_pct = 10,
  transfer_amount_usd = 0,
  transfer_frequency  = "regular",
  transfer_n_payments = 6L,
  transfer_timing     = NA_character_,
  timeliness_weeks    = NA_integer_
)
.infra_off <- list(
  elec_universal               = FALSE,
  elec_access_change_pct       = 0L,
  water_universal              = FALSE,
  water_access_change_pct      = 0L,
  sanitation_universal         = FALSE,
  sanitation_access_change_pct = 0L,
  piped_universal              = FALSE,
  piped_access_change_pct      = 0L,
  piped_to_prem_universal      = FALSE,
  piped_to_prem_access_change_pct = 0L,
  imp_wat_san_universal        = FALSE,
  imp_wat_san_access_change_pct = 0L,
  health_mode                  = "pct",
  health_travel_pct            = 0L,
  health_travel_max            = 60L
)
.digital_off <- list(
  internet_universal         = FALSE,
  internet_access_change_pct = 0L,
  mobile_universal           = FALSE,
  mobile_access_change_pct   = 0L
)
.labor_off <- list(
  employment_change_pp = 0,
  sector_manufacturing = 0,
  sector_services      = 0,
  sector_agriculture   = 100
)

# Edit POLICY_SCENARIOS to add/remove scenarios.
# "no_policy" must always be present — it is the base simulation (no resimulation pass).
POLICY_SCENARIOS <- list(

  # no_policy = list(
  #   policy_keys = character(0),
  #   sp      = .sp_off,
  #   infra   = .infra_off,
  #   digital = .digital_off,
  #   labor   = .labor_off
  # ),

  elec_universal = list(
    policy_keys = "A",              # adds weather x electricity interaction to base model
    sp      = .sp_off,
    infra   = modifyList(.infra_off, list(elec_universal = TRUE)),
    digital = .digital_off,
    labor   = .labor_off
  ),

  imp_wat_universal = list(
    policy_keys = "B",              # adds weather x imp_wat_rec interaction
    sp      = .sp_off,
    infra   = modifyList(.infra_off, list(water_universal = TRUE)),
    digital = .digital_off,
    labor   = .labor_off
  ),

  imp_san_universal = list(
    policy_keys = "C",              # adds weather x imp_san_rec interaction
    sp      = .sp_off,
    infra   = modifyList(.infra_off, list(sanitation_universal = TRUE)),
    digital = .digital_off,
    labor   = .labor_off
  ),

  imp_wat_san_universal = list(
    policy_keys = "I",              # adds weather x imp_wat_san_rec interaction
    sp      = .sp_off,
    infra   = modifyList(.infra_off, list(imp_wat_san_universal = TRUE)),
    digital = .digital_off,
    labor   = .labor_off
  ),

    health30min = list(
    policy_keys = "D",              # adds weather x ttime_health interaction
    sp      = .sp_off,
    infra   = modifyList(.infra_off, list(health_mode = "max", health_travel_max = 30)),
    digital = .digital_off,
    labor   = .labor_off
  ),

  sp_p20_bottom40 = list(
    policy_keys = character(0),     # SP only — no model refit needed
    sp      = modifyList(.sp_off, list(
      transfer_pctile     = 20,     # resolve transfer_amount_usd from welfare P20
      transfer_n_payments = 12L,
      targeting            = "exante_poor",
      targeting_threshold  = 40,
      inclusion_error_pct  = 30,
      exclusion_error_pct  = 30
    )),
    infra   = .infra_off,
    digital = .digital_off,
    labor   = .labor_off
  )
)

# Auto-derive interaction variables required by each policy scenario.
# Each policy with policy_keys needs the corresponding variable as an
# interaction in the base model (matching the app's mod_1 → mod_3 flow).
# SP-only policies (no policy_keys) are applied to noInter specs.
POLICY_INTERACTION_MAP <- list()   # pol_name → interaction var (or "noInter")
for (.pn in names(POLICY_SCENARIOS)) {
  .pk <- POLICY_SCENARIOS[[.pn]]$policy_keys %||% character(0)
  if (length(.pk) == 0L) {
    POLICY_INTERACTION_MAP[[.pn]] <- "noInter"
  } else {
    .vars <- unique(unlist(lapply(.pk, function(k) POLICY_DEFINITIONS[[k]]$vars)))
    if (length(.vars) == 1L) {
      POLICY_INTERACTION_MAP[[.pn]] <- .vars
      if (!list(.vars) %in% INTERACTIONS)
        INTERACTIONS <- c(INTERACTIONS, list(.vars))
    } else if (length(.vars) > 1L) {
      POLICY_INTERACTION_MAP[[.pn]] <- paste(.vars, collapse = "_")
      if (!list(.vars) %in% INTERACTIONS)
        INTERACTIONS <- c(INTERACTIONS, list(.vars))
    }
  }
}

# =============================================================================
# SECTION 2 — SETUP
# =============================================================================

dir.create(file.path(OUT_DIR, "simulations"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "simulations", "weather_distributions"),        showWarnings = FALSE, recursive = TRUE)

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

# When not overwriting, identify specs whose results already exist on disk.
# A spec is done if outcomes.parquet has "no_policy" + all policies mapped to
# its interaction label via POLICY_INTERACTION_MAP.
# Build expected policies per interaction label.
.pols_by_inter <- list()
for (.pn in names(POLICY_INTERACTION_MAP)) {
  .il <- POLICY_INTERACTION_MAP[[.pn]]
  .pols_by_inter[[.il]] <- c(.pols_by_inter[[.il]], .pn)
}

.done_specs <- character(0)
if (!OVERWRITE_EXISTING) {
  .chk_path <- file.path(OUT_DIR, "simulations", "outcomes.parquet")
  if (file.exists(.chk_path)) {
    .chk <- arrow::read_parquet(.chk_path, col_select = c("spec_label", "policy_label"))
    .by_spec <- split(.chk$policy_label, .chk$spec_label)
    # All possible inter_labels: "noInter" + policy-derived ones
    .all_inters <- unique(unlist(POLICY_INTERACTION_MAP))
    .spec_complete <- vapply(names(.by_spec), function(sl) {
      pols <- .by_spec[[sl]]
      if (!"no_policy" %in% pols) return(FALSE)
      for (il in .all_inters) {
        if (endsWith(sl, paste0("_", il))) {
          expected <- .pols_by_inter[[il]] %||% character(0)
          return(all(expected %in% pols))
        }
      }
      FALSE
    }, logical(1))
    .done_specs <- names(which(.spec_complete))
    rm(.chk, .by_spec)
    if (length(.done_specs) > 0L)
      cat(sprintf("  %d spec(s) already complete — will skip\n", length(.done_specs)))
  }
}

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
      dplyr::filter(as.integer(as.character(year)) ==
                      max(as.integer(as.character(year)), na.rm = TRUE)) |>
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
      inter_label <- if (length(interaction_var) == 0) "noInter" else paste(interaction_var, collapse = "_")
      mt_label    <- if (grepl("RIF", cur_model_type)) "rif" else "ols"
      spec_label  <- sprintf("%s_%s_%s_%s_%s_%s", si, wx_name, mt_label,
                             fe_label, cov_label, inter_label)

      if (spec_label %in% .done_specs) {
        cat(sprintf("  [%d/%d] %s... SKIP (results exist)\n", run_idx, nrow(grid), spec_label))
        next
      }

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
            use_parallel = LASSO_USE_PARALLEL && nrow(survey_prep) > 50000, n_workers = LASSO_N_WORKERS,
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

# ---- Helpers ----------------------------------------------------------------

# Parse "headcount_ratio_300" → list(method="headcount_ratio", pov_line=3.00)
.parse_agg_method <- function(am) {
  pov_map <- c("_300" = 3.00, "_420" = 4.20, "_830" = 8.30)
  for (sfx in names(pov_map)) {
    if (endsWith(am, sfx))
      return(list(method = sub(paste0(sfx, "$"), "", am), pov_line = pov_map[[sfx]]))
  }
  list(method = am, pov_line = NA_real_)
}

# Aggregate one pipeline object across all AGG_METHODS and return a data frame.
.pipe_to_rows <- function(pipe, spec_meta, pol_label, scen_meta, ensemble_member, is_log) {
  pol      <- POLICY_SCENARIOS[[pol_label]]
  pol_keys <- pol$policy_keys %||% character(0)
  has_sp      <- isTRUE((pol$sp$transfer_amount_usd %||% 0) > 0)
  has_infra   <- any(pol_keys %in% c("A", "B", "C", "D", "G", "H", "I"))
  has_digital <- any(pol_keys %in% c("E", "F"))
  has_labor   <- isTRUE((pol$labor$employment_change_pp %||% 0) != 0)

  all_rows <- list()
  for (am in AGG_METHODS) {
    parsed  <- .parse_agg_method(am)
    pov_arg <- if (is.na(parsed$pov_line)) NULL else parsed$pov_line
    per_yr  <- tryCatch(
      aggregate_pipeline_per_year(
        pipe      = pipe,
        method    = parsed$method,
        pov_line  = pov_arg,
        residuals = RESIDUALS,
        is_log    = is_log,
        skip_coef = !INCLUDE_COEF_UNCERTAINTY
      ),
      error = function(e) {
        message("    agg [", am, "]: ", conditionMessage(e)); NULL
      }
    )
    if (is.null(per_yr)) next
    for (yr in per_yr) {
      all_rows[[length(all_rows) + 1L]] <- data.frame(
        spec_label      = spec_meta$spec_label,
        code            = spec_meta$code,
        wx_name         = spec_meta$wx_name,
        mt_label        = spec_meta$mt_label,
        fe_label        = spec_meta$fe_label,
        cov_label       = spec_meta$cov_label,
        inter_label     = spec_meta$inter_label,
        policy_label    = pol_label,
        has_sp          = has_sp,
        has_infra       = has_infra,
        has_digital     = has_digital,
        has_labor       = has_labor,
        scenario_id     = scen_meta$scenario_id,
        scenario_type   = scen_meta$scenario_type,
        ssp             = scen_meta$ssp,
        period_start    = scen_meta$period_start,
        period_end      = scen_meta$period_end,
        ensemble_member = ensemble_member,
        sim_year        = yr$sim_year,
        agg_method      = am,
        poverty_line    = parsed$pov_line,
        value           = yr$value,
        value_lo        = yr$value_lo,
        value_hi        = yr$value_hi,
        var_coef        = yr$var_coef,
        var_resid       = yr$var_resid,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(all_rows) == 0L) return(NULL)
  do.call(rbind, all_rows)
}

# Compute p10–p90 / mean / sd for each weather column and return a data frame.
.wx_quantiles <- function(weather_raw, wx_col_names, code, wx_name,
                           scenario_id, ensemble_member) {
  if (is.null(weather_raw) || nrow(weather_raw) == 0L) return(NULL)
  wx_cols <- intersect(wx_col_names, names(weather_raw))
  if (length(wx_cols) == 0L) return(NULL)
  rows <- lapply(wx_cols, function(v) {
    vals <- weather_raw[[v]]
    if (!is.numeric(vals)) return(NULL)
    vals <- vals[is.finite(vals)]
    if (length(vals) < 2L) return(NULL)
    qs <- stats::quantile(vals, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)
    data.frame(
      code = code, wx_name = wx_name, scenario_id = scenario_id,
      ensemble_member = ensemble_member, variable = v,
      mean = mean(vals, na.rm = TRUE), sd = stats::sd(vals, na.rm = TRUE),
      p10 = qs[[1]], p20 = qs[[2]], p30 = qs[[3]], p40 = qs[[4]], p50 = qs[[5]],
      p60 = qs[[6]], p70 = qs[[7]], p80 = qs[[8]], p90 = qs[[9]],
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows[!vapply(rows, is.null, logical(1L))])
}

# Overlaid weather distribution plot (continuous → KDE density; binned → bar proportions).
# hist_weather_raw : data frame from hist_sim_result$weather_raw
# scenario_weather : named list of future weather_raw data frames (one per scenario key)
.save_wx_dist_plot <- function(hist_weather_raw, wx_col_names,
                               scenario_weather = NULL, path) {
  if (SKIP_PLOTS) return(invisible(NULL))
  if (!OVERWRITE_EXISTING && file.exists(path)) return(invisible(NULL))
  if (is.null(hist_weather_raw) || nrow(hist_weather_raw) == 0L) return(invisible(NULL))
  wx_cols <- intersect(wx_col_names, names(hist_weather_raw))
  if (length(wx_cols) == 0L) return(invisible(NULL))

  source_levels <- c("historical", names(scenario_weather))
  source_colours <- c(historical = "#808080")
  ssp_palette <- c(ssp1_2_6 = "#1a9641", ssp2_4_5 = "#a6d96a",
                   ssp3_7_0 = "#d7191c", ssp5_8_5 = "#762a83")
  for (nm in names(scenario_weather)) {
    key <- tolower(gsub("[^A-Za-z0-9]", "_",
                        regmatches(nm, regexpr("SSP[^/]+", nm, perl = TRUE))[[1L]] %||% nm))
    source_colours[[nm]] <- if (key %in% names(ssp_palette)) ssp_palette[[key]] else "#4393c3"
  }

  # Build one panel plot per weather variable then combine
  panels <- lapply(wx_cols, function(v) {
    raw_col   <- hist_weather_raw[[v]]
    is_factor <- is.factor(raw_col) || is.character(raw_col)

    # --- collect rows per source ---
    collect_rows <- function(df, src_label) {
      col <- df[[v]]
      if (is_factor) {
        vals <- as.character(col)
        vals <- vals[!is.na(vals)]
        if (length(vals) == 0L) return(NULL)
        data.frame(value = vals, source = src_label, stringsAsFactors = FALSE)
      } else {
        vals <- as.numeric(col)
        vals <- vals[is.finite(vals)]
        if (length(vals) == 0L) return(NULL)
        data.frame(value = vals, source = src_label, stringsAsFactors = FALSE)
      }
    }

    all_df <- collect_rows(hist_weather_raw, "historical")
    for (nm in names(scenario_weather)) {
      sc_df <- scenario_weather[[nm]]
      if (!is.null(sc_df) && v %in% names(sc_df))
        all_df <- rbind(all_df, collect_rows(sc_df, nm))
    }
    if (is.null(all_df) || nrow(all_df) == 0L) return(NULL)

    all_df$source <- factor(all_df$source, levels = source_levels)
    used_colours  <- source_colours[levels(all_df$source)]

    if (is_factor) {
      raw_levels <- if (is.factor(raw_col)) levels(raw_col) else
        sort(unique(as.character(raw_col[!is.na(raw_col)])))
      all_df$value <- factor(all_df$value, levels = raw_levels)

      ggplot2::ggplot(all_df,
        ggplot2::aes(x = .data$value,
                     y = ggplot2::after_stat(prop),
                     group = .data$source,
                     fill  = .data$source,
                     colour = .data$source)) +
        ggplot2::geom_bar(position = ggplot2::position_dodge(preserve = "single"),
                          alpha = 0.6, linewidth = 0.4) +
        ggplot2::scale_fill_manual(values = used_colours, name = NULL) +
        ggplot2::scale_colour_manual(values = used_colours, name = NULL) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        ggplot2::labs(title = v, x = NULL, y = "Relative frequency") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
                       legend.position = "bottom")
    } else {
      ggplot2::ggplot(all_df,
        ggplot2::aes(x = .data$value,
                     fill   = .data$source,
                     colour = .data$source)) +
        ggplot2::geom_density(alpha = 0.35, linewidth = 0.6) +
        ggplot2::scale_fill_manual(values = used_colours, name = NULL) +
        ggplot2::scale_colour_manual(values = used_colours, name = NULL) +
        ggplot2::labs(title = v, x = NULL, y = "Density") +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "bottom")
    }
  })

  panels <- panels[!vapply(panels, is.null, logical(1L))]
  if (length(panels) == 0L) return(invisible(NULL))
  p <- if (length(panels) == 1L) {
    panels[[1L]]
  } else {
    patchwork::wrap_plots(panels, nrow = 1) +
      patchwork::plot_layout(guides = "collect") &
      ggplot2::theme(legend.position = "bottom")
  }
  ggplot2::ggsave(path, p, width = 4 * length(panels) + 1, height = 4, dpi = 120)
  invisible(path)
}


# Parse "SSP3-7.0 / 2025-2035" → list(ssp="ssp3_7_0", period_start=2025, period_end=2035)
.parse_scenario_name <- function(nm) {
  parts   <- strsplit(nm, " / ", fixed = TRUE)[[1]]
  ssp_key <- tolower(gsub("[. -]+", "_", trimws(parts[1])))
  per_yrs <- as.integer(strsplit(trimws(parts[2]), "-", fixed = TRUE)[[1]])
  list(ssp = ssp_key, period_start = per_yrs[1L], period_end = per_yrs[2L])
}

# Resolve relative SP transfer amount from the welfare distribution.
# When sp$transfer_pctile is set (e.g. 20), computes the weighted quantile of
# welfare ($/day PPP) and converts to a per-payment USD amount:
#   transfer_amount_usd = P_pctile * 365 / transfer_n_payments
.resolve_transfer_amount <- function(sp, svy) {
  pctile <- sp$transfer_pctile
  if (is.null(pctile) || is.na(pctile)) return(sp)

  welfare <- svy$welfare
  weight  <- if ("weight" %in% names(svy)) svy$weight else NULL
  ok      <- !is.na(welfare) & is.finite(welfare)
  if (sum(ok) < 10L) {
    warning("Too few observations to compute welfare percentile; transfer_amount_usd = 0")
    sp$transfer_amount_usd <- 0
    return(sp)
  }
  welfare <- welfare[ok]
  weight  <- if (!is.null(weight)) weight[ok] else rep(1, sum(ok))

  # Weighted quantile via cumulative weight interpolation
  ord     <- order(welfare)
  welfare <- welfare[ord]; weight <- weight[ord]
  cw      <- cumsum(weight) / sum(weight)
  p       <- pctile / 100
  idx     <- which(cw >= p)[1L]
  p_value <- if (is.na(idx) || idx <= 1L) welfare[max(1L, idx %||% 1L)] else
    welfare[idx - 1L] + (p - cw[idx - 1L]) / (cw[idx] - cw[idx - 1L]) *
      (welfare[idx] - welfare[idx - 1L])

  n_pay <- sp$transfer_n_payments %||% 12L
  sp$transfer_amount_usd <- p_value * 365 / n_pay
  cat(sprintf("    SP transfer: P%d = %.2f $/day PPP -> %.2f USD/payment x %d = %.0f USD/year\n",
              pctile, p_value, sp$transfer_amount_usd, n_pay, sp$transfer_amount_usd * n_pay))
  sp
}

# Save a data frame, merging with an existing CSV when OVERWRITE_EXISTING = FALSE.
# Existing rows win on dedup (same behaviour as 02_weather_stats.R).
.save_csv <- function(new_df, path, dedup_keys) {
  if (is.null(new_df) || nrow(new_df) == 0L) return(invisible(NULL))
  out_df <- new_df
  if (!OVERWRITE_EXISTING && file.exists(path)) {
    existing <- readr::read_csv(path, show_col_types = FALSE)
    # readr may infer all-NA columns as logical; coerce to match new_df types
    for (col in intersect(names(existing), names(new_df))) {
      if (is.logical(existing[[col]]) && !is.logical(new_df[[col]])) {
        if (is.character(new_df[[col]]))    existing[[col]] <- as.character(existing[[col]])
        else if (is.integer(new_df[[col]])) existing[[col]] <- as.integer(existing[[col]])
        else if (is.double(new_df[[col]]))  existing[[col]] <- as.double(existing[[col]])
      }
    }
    out_df   <- dplyr::bind_rows(existing, new_df)
    out_df   <- dplyr::distinct(out_df, dplyr::across(dplyr::any_of(dedup_keys)),
                                .keep_all = TRUE)
    cat(sprintf("  [merge] %s (%d existing + %d new)\n",
                basename(path), nrow(existing), nrow(new_df)))
  }
  readr::write_csv(out_df, path)
  cat(sprintf("  Saved: %s (%d rows)\n", basename(path), nrow(out_df)))
  invisible(path)
}

.save_parquet <- function(new_df, path, dedup_keys) {
  if (is.null(new_df) || nrow(new_df) == 0L) return(invisible(NULL))
  out_df <- new_df
  if (!OVERWRITE_EXISTING && file.exists(path)) {
    existing <- arrow::read_parquet(path)
    out_df   <- dplyr::bind_rows(existing, new_df)
    out_df   <- dplyr::distinct(out_df, dplyr::across(dplyr::any_of(dedup_keys)),
                                .keep_all = TRUE)
    cat(sprintf("  [merge] %s (%d existing + %d new)\n",
                basename(path), nrow(existing), nrow(new_df)))
  }
  arrow::write_parquet(out_df, path)
  cat(sprintf("  Saved: %s (%d rows)\n", basename(path), nrow(out_df)))
  invisible(path)
}

# Year-by-year decile-level decomposition of policy effects.
# Calls decompose_policy_effect once per sim_year (derived from timestamp),
# aggregating per-HH channels to decile-weighted means. Works for both OLS
# (delta_res1 = 0 by construction) and RIF engines.
.yr_decile_decomp <- function(svy_baseline, svy_policy, mf, so,
                               weather_raw, scenario_id, ensemble_member,
                               spec_label, pol_name) {
  if (is.null(weather_raw) || !"timestamp" %in% names(weather_raw)) return(NULL)
  sim_yrs_vec <- as.integer(format(weather_raw$timestamp, "%Y"))
  years       <- sort(unique(sim_yrs_vec))
  delta_cols  <- c("delta_main", "delta_sp", "delta_main_covar",
                   "delta_res1", "delta_res2", "delta_res", "delta_total")
  rows <- lapply(years, function(yr) {
    wx_yr  <- weather_raw[sim_yrs_vec == yr, , drop = FALSE]
    decomp <- tryCatch(
      decompose_policy_effect(
        svy_baseline = svy_baseline,
        svy_policy   = svy_policy,
        model_fit    = mf,
        so           = so,
        weather_raw  = wx_yr,
        skip_coef    = TRUE
      ),
      error = function(e) NULL
    )
    if (is.null(decomp)) return(NULL)
    dplyr::bind_rows(lapply(1:10, function(d) {
      sub <- decomp[decomp$decile == d, , drop = FALSE]
      if (nrow(sub) == 0L) return(NULL)
      w   <- sub$weight
      row <- data.frame(spec_label = spec_label, policy_label = pol_name,
                        scenario_id = scenario_id,
                        ensemble_member = as.character(ensemble_member),
                        sim_year = yr, decile = d, n_hh = nrow(sub),
                        stringsAsFactors = FALSE)
      for (col in delta_cols)
        if (col %in% names(sub))
          row[[col]] <- stats::weighted.mean(sub[[col]], w, na.rm = TRUE)
      row
    }))
  })
  dplyr::bind_rows(rows[!vapply(rows, is.null, logical(1L))])
}

# Build return-period summary from the outcomes data frame.
# Mirrors the app's threshold_table_rv (mod_2_02_results.R): per
# (spec, policy, scenario, agg_method), rank-interpolate RP thresholds
# separately for each ensemble member then aggregate across members.
# Estimates: Central (P50), Coef P10/P90, and — for future scenarios only —
# Ensemble min/max and Pooled P10/P90 (sqrt of coef² + inter-model variance).
# Historical groups use the single member; ensemble/pooled rows are omitted.
# Values are raw welfare level thresholds (not deviations from baseline).
.build_return_period_df <- function(outcomes_df) {
  if (is.null(outcomes_df) || nrow(outcomes_df) == 0L) return(NULL)

  RPs    <- c(RP_LOW, "1:1" = 0.5, RP_HIGH)
  rp_nms <- names(RPs)

  # P10/P90 coefficient bands; min/max ensemble spread
  z_lo <- stats::qnorm(0.10)
  z_hi <- stats::qnorm(0.90)

  group_keys <- c("spec_label", "policy_label", "scenario_id", "agg_method")
  finite_df  <- outcomes_df[is.finite(outcomes_df$value), ]
  if (nrow(finite_df) == 0L) return(NULL)
  grp_list   <- split(finite_df, finite_df[, group_keys], drop = TRUE)

  all_rows <- lapply(grp_list, function(g) {
    spec    <- g$spec_label[1L]
    pol     <- g$policy_label[1L]
    sc_id   <- g$scenario_id[1L]
    am      <- g$agg_method[1L]
    is_hist <- sc_id == "historical"

    # Build (member × year) value and SD matrices
    members   <- sort(unique(as.character(g$ensemble_member)))
    sim_years <- sort(unique(g$sim_year))
    n_mem <- length(members); n_yrs <- length(sim_years)

    vals_mat <- matrix(NA_real_, nrow = n_mem, ncol = n_yrs,
                       dimnames = list(members, as.character(sim_years)))
    sds_mat  <- matrix(NA_real_, nrow = n_mem, ncol = n_yrs,
                       dimnames = list(members, as.character(sim_years)))
    for (ri in seq_len(nrow(g))) {
      mem <- as.character(g$ensemble_member[ri])
      yr  <- as.character(g$sim_year[ri])
      vals_mat[mem, yr] <- g$value[ri]
      vc <- g$var_coef[ri]
      sds_mat[mem, yr]  <- if (is.finite(vc) && vc >= 0) sqrt(vc) else NA_real_
    }

    # Drop RPs not supported by n_yrs of data (same logic as app)
    rp_ok    <- RPs >= (1 / n_yrs) & RPs <= (1 - 1 / n_yrs)
    RPs_keep <- RPs[rp_ok]
    if (length(RPs_keep) == 0L) return(NULL)
    k_rp <- length(RPs_keep)

    # Per-member rank-interp at each kept RP (member × RP matrix)
    per_model_rp <- matrix(NA_real_, nrow = n_mem, ncol = k_rp)
    for (i in seq_len(n_mem)) {
      v <- vals_mat[i, ]; v <- v[is.finite(v)]
      if (length(v) < 2L) next
      per_model_rp[i, ] <- vapply(RPs_keep, function(p) rank_interp(sort(v), p), numeric(1L))
    }

    # Per-member coef SD at same rank position as the RP value
    per_model_sd <- matrix(NA_real_, nrow = n_mem, ncol = k_rp)
    for (i in seq_len(n_mem)) {
      v <- vals_mat[i, ]; s <- sds_mat[i, ]
      ok <- is.finite(v)
      if (sum(ok) < 2L) next
      s_sorted <- s[ok][order(v[ok])]
      per_model_sd[i, ] <- vapply(RPs_keep, function(p) rank_interp(s_sorted, p), numeric(1L))
    }

    n_pts <- if (is_hist) sum(is.finite(vals_mat)) else n_yrs

    central_vec <- if (is_hist) per_model_rp[1L, ] else
      apply(per_model_rp, 2L, stats::median, na.rm = TRUE)
    coef_sd_vec <- if (is_hist) per_model_sd[1L, ] else
      apply(per_model_sd,  2L, stats::median, na.rm = TRUE)
    coef_lo_vec <- central_vec + z_lo * coef_sd_vec
    coef_hi_vec <- central_vec + z_hi * coef_sd_vec

    make_rp_row <- function(estimate, vec) {
      row <- data.frame(spec_label = spec, policy_label = pol,
                        scenario_id = sc_id, agg_method = am,
                        estimate = estimate, n_obs = n_pts,
                        stringsAsFactors = FALSE)
      for (nm in rp_nms) row[[nm]] <- NA_real_
      for (i in seq_along(RPs_keep)) {
        row[[names(RPs_keep)[i]]] <- if (is.finite(vec[i])) vec[i] else NA_real_
      }
      row
    }

    rows <- list(
      make_rp_row("Central (P50)", central_vec),
      make_rp_row("Coef P10",      coef_lo_vec),
      make_rp_row("Coef P90",      coef_hi_vec)
    )
    if (!is_hist) {
      var_across  <- apply(per_model_rp, 2L, stats::var, na.rm = TRUE)
      var_across[is.na(var_across)] <- 0
      sd_total_vec <- sqrt(pmax(coef_sd_vec^2 + var_across, 0))
      rows <- c(rows, list(
        make_rp_row("Ensemble min", apply(per_model_rp, 2L, min, na.rm = TRUE)),
        make_rp_row("Ensemble max", apply(per_model_rp, 2L, max, na.rm = TRUE)),
        make_rp_row("Pooled P10",   central_vec + z_lo * sd_total_vec),
        make_rp_row("Pooled P90",   central_vec + z_hi * sd_total_vec)
      ))
    }
    dplyr::bind_rows(rows)
  })
  dplyr::bind_rows(all_rows[!vapply(all_rows, is.null, logical(1L))])
}

# ---- Output file paths ------------------------------------------------------

out_outcomes  <- file.path(OUT_DIR, "simulations", "outcomes.parquet")
out_wx_quant  <- file.path(OUT_DIR, "simulations", "weather_quantiles.csv")
out_sim_meta  <- file.path(OUT_DIR, "simulations", "sim_metadata.csv")
out_failures  <- file.path(OUT_DIR, "simulations", "_failures.csv")
out_pol_cfg   <- file.path(OUT_DIR, "simulations", "policy_config.csv")
out_ret_per     <- file.path(OUT_DIR, "simulations", "return_periods.csv")
out_yr_decile   <- file.path(OUT_DIR, "simulations", "decile_decomposition_by_year.parquet")

# ---- In-memory accumulators -------------------------------------------------

sim_outcomes_rows  <- list()
sim_wx_quant_rows  <- list()
sim_meta_rows_all  <- list()
all_fail_rows      <- list()
all_pol_cfg_rows   <- list()
all_yr_decile_rows <- list()

# Seed failures accumulator with Section 3 failures
if (length(fail_log) > 0L) {
  all_fail_rows[["s3"]] <- data.frame(
    spec_label   = names(fail_log),
    policy_label = NA_character_,
    stage        = "model_fitting",
    error        = unlist(fail_log),
    stringsAsFactors = FALSE
  )
}

# ---- Future period config ---------------------------------------------------

.fp_raw     <- Filter(Negate(is.null), list(FUT_PERIOD_1, FUT_PERIOD_2, FUT_PERIOD_3))
.has_future <- length(.fp_raw) > 0L && length(SSPS) > 0L
fp_list     <- lapply(.fp_raw, function(yr)
  c(paste0(yr[1L], "-01-01"), paste0(yr[2L], "-12-31")))

# ---- Main simulation loop ---------------------------------------------------

sim_store   <- list()
sim_run_idx <- 0L

for (spec_label in names(fit_store)) {
  fs          <- fit_store[[spec_label]]
  sim_run_idx <- sim_run_idx + 1L
  cat(sprintf("\n[%d/%d] %s\n", sim_run_idx, length(fit_store), spec_label))

  is_log    <- identical(fs$so$transform, "log")
  spec_meta <- list(spec_label = spec_label, code = fs$code, wx_name = fs$wx_name,
                    mt_label = fs$mt_label, fe_label = fs$fe_label,
                    cov_label = fs$cov_label, inter_label = fs$inter_label)

  sim_dates <- tryCatch(
    build_hist_sim_dates(fs$svy_baseline, HIST_YEARS),
    error = function(e) { message("  sim_dates: ", conditionMessage(e)); NULL }
  )
  if (is.null(sim_dates)) {
    all_fail_rows[[spec_label]] <- data.frame(
      spec_label = spec_label, policy_label = NA_character_,
      stage = "build_hist_sim_dates", error = "failed", stringsAsFactors = FALSE
    )
    next
  }

  cat("  fct_run_simulation...")
  t0_s   <- proc.time()[["elapsed"]]
  result <- tryCatch(
    fct_run_simulation(
      sw          = fs$sw,
      so          = fs$so,
      svy         = fs$svy_baseline,
      ss          = fs$ss,
      mf          = fs$mf,
      cp          = connection_params,
      fp_list     = fp_list,
      ssps        = SSPS,
      residuals   = if (identical(fs$mf$engine, "rif")) "none" else RESIDUALS,
      dev_mode    = DEV_MODE,
      skip_coef_draws             = !INCLUDE_COEF_UNCERTAINTY,
      sim_dates                   = sim_dates,
      perturbation_method         = if (.has_future) build_perturbation_method(fs$sw) else NULL,
      stored_breaks               = fs$stored_breaks,
      propagate_all_covariate_uncertainty = PROPAGATE_ALL_COVARIATE_UNCERTAINTY,
      fit_multi    = if (identical(fs$mf$engine, "rif")) fs$mf$fit3 else NULL,
      taus         = fs$mf$taus,
      weather_cols = fs$mf$weather_terms %||% fs$wx_col_names
    ),
    error = function(e) { message(" FAIL: ", conditionMessage(e)); NULL }
  )
  if (is.null(result)) {
    cat(" FAIL\n")
    all_fail_rows[[spec_label]] <- data.frame(
      spec_label = spec_label, policy_label = NA_character_,
      stage = "fct_run_simulation", error = "failed", stringsAsFactors = FALSE
    )
    next
  }
  cat(sprintf(" %.1fs\n", round(proc.time()[["elapsed"]] - t0_s, 1)))

  sim_store[[spec_label]] <- result

  # Survey data no longer needed — sim_store captures everything for Section 5.
  fit_store[[spec_label]]$svy_wx       <- NULL
  fit_store[[spec_label]]$svy_baseline <- NULL

  # -- Historical aggregation -------------------------------------------------
  .hist_meta <- list(scenario_id = "historical", scenario_type = "historical",
                     ssp = NA_character_, period_start = NA_integer_,
                     period_end = NA_integer_)

  sim_outcomes_rows[[paste(spec_label, "hist", sep = "_")]] <-
    .pipe_to_rows(result$hist_sim_result$pipeline, spec_meta,
                  "no_policy", .hist_meta, 1L, is_log)

  sim_wx_quant_rows[[paste(spec_label, "hist", sep = "_")]] <-
    .wx_quantiles(result$hist_sim_result$weather_raw, fs$wx_col_names,
                  fs$code, fs$wx_name, "historical", 1L)

  # -- Future scenario aggregation --------------------------------------------
  .scenario_wx <- list()  # collect for overlaid distribution plot

  for (sc_name in names(result$new_scenarios)) {
    sc      <- result$new_scenarios[[sc_name]]
    .parsed <- .parse_scenario_name(sc_name)
    sc_meta <- list(scenario_id   = sc_name,
                    scenario_type = "future",
                    ssp           = .parsed$ssp,
                    period_start  = .parsed$period_start,
                    period_end    = .parsed$period_end)
    sc_id   <- gsub("[^A-Za-z0-9]+", "_", sc_name)

    for (em in seq_along(sc$pipelines)) {
      .key <- paste(spec_label, sc_id, em, sep = "_")
      sim_outcomes_rows[[.key]] <-
        .pipe_to_rows(sc$pipelines[[em]], spec_meta, "no_policy", sc_meta, em, is_log)
    }

    sim_wx_quant_rows[[paste(spec_label, sc_id, sep = "_")]] <-
      .wx_quantiles(sc$weather_raw, fs$wx_col_names,
                    fs$code, fs$wx_name, sc_name, NA_integer_)

    .scenario_wx[[sc_name]] <- sc$weather_raw

    sim_meta_rows_all[[paste(spec_label, sc_id, sep = "_")]] <- data.frame(
      spec_label         = spec_label,
      code               = fs$code,
      wx_name            = fs$wx_name,
      scenario_id        = sc_name,
      ssp                = .parsed$ssp,
      period_start       = .parsed$period_start,
      period_end         = .parsed$period_end,
      n_ensemble_members = length(sc$pipelines),
      n_models           = sc$n_models %||% NA_integer_,
      stringsAsFactors   = FALSE
    )
  }

  # Overlaid distribution plot: historical + all future scenarios on one panel
  tryCatch(
    .save_wx_dist_plot(
      result$hist_sim_result$weather_raw, fs$wx_col_names,
      scenario_weather = .scenario_wx,
      path = file.path(OUT_DIR, "simulations", "weather_distributions",
                       sprintf("%s_%s_overlay.png", fs$code, fs$wx_name))
    ),
    error = function(e) message("  wx plot (overlay): ", conditionMessage(e))
  )

  cat(sprintf("  rows accumulated: historical + %d future scenario(s)\n",
              length(result$new_scenarios)))
}

cat(sprintf("\nStep 2 complete: %d simulated (%d stored)\n",
            sim_run_idx, length(sim_store)))

# Strip fit_store down — Section 5 only needs mf, so, and identifiers
for (.k in names(fit_store)) {
  fit_store[[.k]] <- fit_store[[.k]][c("mf", "so", "code", "wx_name", "mt_label",
                                        "fe_label", "cov_label", "inter_label")]
}
gc(verbose = FALSE)

# Checkpoint: write base (no_policy) outcomes so they survive a Section 5 crash
.outcomes_keys  <- c("spec_label", "policy_label", "scenario_id",
                     "ensemble_member", "sim_year", "agg_method")
.save_parquet(dplyr::bind_rows(sim_outcomes_rows), out_outcomes, .outcomes_keys)

# =============================================================================
# SECTION 5 — STEP 3: POLICY SIMULATIONS
# =============================================================================

cat("\n=== Step 3: Policy simulations ===\n")

.pol_active <- setdiff(names(POLICY_SCENARIOS), "no_policy")
cat(sprintf("Policy scenarios: %d (%s)\n\n",
            length(.pol_active),
            if (length(.pol_active) == 0L) "none"
            else paste(.pol_active, collapse = ", ")))

pol_run_idx <- 0L

for (spec_label in names(sim_store)) {
  fs        <- fit_store[[spec_label]]
  sim_res   <- sim_store[[spec_label]]
  is_log    <- identical(fs$so$transform, "log")
  spec_meta <- list(spec_label = spec_label, code = fs$code,
                    wx_name    = fs$wx_name, mt_label  = fs$mt_label,
                    fe_label   = fs$fe_label, cov_label = fs$cov_label,
                    inter_label = fs$inter_label)

  # Only run policies whose required interaction matches this spec
  .spec_inter <- fs$inter_label
  .pols_here  <- Filter(function(pn)
    identical(POLICY_INTERACTION_MAP[[pn]], .spec_inter), .pol_active)
  if (length(.pols_here) == 0L) next

  for (pol_name in .pols_here) {
    pol         <- POLICY_SCENARIOS[[pol_name]]
    pol_run_idx <- pol_run_idx + 1L
    .run_key    <- paste(spec_label, pol_name, sep = ":")
    cat(sprintf("  [%d] %s : %s...", pol_run_idx, spec_label, pol_name))
    t0_p <- proc.time()[["elapsed"]]

    # Policy config row (written before execution so failed runs are still recorded)
    all_pol_cfg_rows[[.run_key]] <- data.frame(
      spec_label              = spec_label,
      policy_label            = pol_name,
      policy_keys             = paste(pol$policy_keys %||% character(0), collapse = "|"),
      transfer_amount_usd     = pol$sp$transfer_amount_usd    %||% 0,
      targeting               = pol$sp$targeting              %||% NA_character_,
      inclusion_error_pct     = pol$sp$inclusion_error_pct    %||% NA_real_,
      exclusion_error_pct     = pol$sp$exclusion_error_pct    %||% NA_real_,
      elec_universal          = isTRUE(pol$infra$elec_universal),
      elec_access_change_pct  = pol$infra$elec_access_change_pct  %||% 0L,
      water_universal         = isTRUE(pol$infra$water_universal),
      water_access_change_pct = pol$infra$water_access_change_pct %||% 0L,
      sanitation_universal    = isTRUE(pol$infra$sanitation_universal),
      internet_universal      = isTRUE(pol$digital$internet_universal),
      internet_access_change_pct = pol$digital$internet_access_change_pct %||% 0L,
      mobile_universal        = isTRUE(pol$digital$mobile_universal),
      employment_change_pp    = pol$labor$employment_change_pp %||% 0,
      run_status              = "started",
      stringsAsFactors = FALSE
    )

    # Resolve relative transfer amount (transfer_pctile → transfer_amount_usd)
    .pol_sp <- .resolve_transfer_amount(pol$sp, sim_res$hist_sim_result$svy)
    all_pol_cfg_rows[[.run_key]]$transfer_amount_usd <- .pol_sp$transfer_amount_usd %||% 0

    svy_policy <- tryCatch(
      apply_policy_to_svy(
        svy           = sim_res$hist_sim_result$svy,  # baseline year only, matching app
        sp            = .pol_sp,
        infra         = pol$infra,
        digital       = pol$digital,
        labor         = pol$labor,
        analysis_unit = UNIT
      ),
      error = function(e) { message(" FAIL (apply_policy): ", conditionMessage(e)); NULL }
    )
    if (is.null(svy_policy)) {
      cat(" FAIL (apply_policy)\n")
      all_fail_rows[[.run_key]] <- data.frame(
        spec_label = spec_label, policy_label = pol_name,
        stage = "apply_policy_to_svy", error = "failed", stringsAsFactors = FALSE
      )
      next
    }

    policy_sim <- tryCatch(
      apply_policy_delta_to_baseline(
        svy_baseline             = sim_res$hist_sim_result$svy,
        svy_policy               = svy_policy,
        model_fit                = fs$mf,
        so                       = sim_res$hist_sim_result$so,
        hist_sim_baseline        = sim_res$hist_sim_result,
        saved_scenarios_baseline = sim_res$new_scenarios,
        skip_coef                = !INCLUDE_COEF_UNCERTAINTY
      ),
      error = function(e) { message(" FAIL (apply_policy_delta): ", conditionMessage(e)); NULL }
    )
    if (is.null(policy_sim)) {
      cat(" FAIL (apply_policy_delta)\n")
      all_fail_rows[[.run_key]] <- data.frame(
        spec_label = spec_label, policy_label = pol_name,
        stage = "apply_policy_delta_to_baseline", error = "failed", stringsAsFactors = FALSE
      )
      next
    }

    # Historical aggregation
    .hist_meta <- list(scenario_id = "historical", scenario_type = "historical",
                       ssp = NA_character_, period_start = NA_integer_,
                       period_end = NA_integer_)
    sim_outcomes_rows[[paste(.run_key, "hist", sep = "_")]] <-
      .pipe_to_rows(policy_sim$hist_sim$pipeline, spec_meta,
                    pol_name, .hist_meta, 1L, is_log)

    # Future scenario aggregation
    for (sc_name in names(policy_sim$saved_scenarios)) {
      sc_pol  <- policy_sim$saved_scenarios[[sc_name]]
      .parsed <- .parse_scenario_name(sc_name)
      sc_meta <- list(scenario_id   = sc_name,
                      scenario_type = "future",
                      ssp           = .parsed$ssp,
                      period_start  = .parsed$period_start,
                      period_end    = .parsed$period_end)
      sc_id   <- gsub("[^A-Za-z0-9]+", "_", sc_name)
      for (em in seq_along(sc_pol$pipelines)) {
        sim_outcomes_rows[[paste(.run_key, sc_id, em, sep = "_")]] <-
          .pipe_to_rows(sc_pol$pipelines[[em]], spec_meta,
                        pol_name, sc_meta, em, is_log)
      }
    }

    # Year-by-decile decomposition — historical (single run, ensemble_member = 1)
    .yd_hist <- tryCatch(
      .yr_decile_decomp(
        svy_baseline    = sim_res$hist_sim_result$svy,
        svy_policy      = svy_policy,
        mf              = fs$mf,
        so              = sim_res$hist_sim_result$so,
        weather_raw     = sim_res$hist_sim_result$weather_raw,
        scenario_id     = "historical",
        ensemble_member = 1L,
        spec_label      = spec_label,
        pol_name        = pol_name
      ),
      error = function(e) { message("  yr_decile (hist): ", conditionMessage(e)); NULL }
    )
    if (!is.null(.yd_hist))
      all_yr_decile_rows[[paste(.run_key, "hist", sep = "_")]] <- .yd_hist
    rm(.yd_hist)

    # Year-by-decile decomposition — future scenarios, one run per ensemble member
    for (sc_name in names(sim_res$new_scenarios)) {
      sc    <- sim_res$new_scenarios[[sc_name]]
      sc_id <- gsub("[^A-Za-z0-9]+", "_", sc_name)
      for (mem_name in names(sc$pipelines)) {
        mem_wx <- sc$pipelines[[mem_name]]$weather_raw
        if (is.null(mem_wx)) next
        .yd_sc <- tryCatch(
          .yr_decile_decomp(
            svy_baseline    = sim_res$hist_sim_result$svy,
            svy_policy      = svy_policy,
            mf              = fs$mf,
            so              = sim_res$hist_sim_result$so,
            weather_raw     = mem_wx,
            scenario_id     = sc_name,
            ensemble_member = mem_name,
            spec_label      = spec_label,
            pol_name        = pol_name
          ),
          error = function(e) { message("  yr_decile (", sc_name, "/", mem_name, "): ", conditionMessage(e)); NULL }
        )
        if (!is.null(.yd_sc))
          all_yr_decile_rows[[paste(.run_key, sc_id, mem_name, sep = "_")]] <- .yd_sc
      }
    }

    # Mark config row as completed
    all_pol_cfg_rows[[.run_key]]$run_status <- "completed"

    cat(sprintf(" %.1fs DONE\n", round(proc.time()[["elapsed"]] - t0_p, 1)))

    rm(svy_policy, policy_sim)
    gc(verbose = FALSE)
  }

  sim_store[[spec_label]] <- NULL
  fit_store[[spec_label]] <- NULL
  gc(verbose = FALSE)
}

cat(sprintf("\nStep 3 complete: %d policy runs\n", pol_run_idx))

# =============================================================================
# SECTION 6 — SAVE OUTPUTS
# =============================================================================

cat("\n=== Saving outputs ===\n")

.outcomes_keys  <- c("spec_label", "policy_label", "scenario_id",
                     "ensemble_member", "sim_year", "agg_method")
.wx_quant_keys  <- c("code", "wx_name", "scenario_id", "ensemble_member", "variable")
.sim_meta_keys  <- c("spec_label", "scenario_id")
.fail_keys      <- c("spec_label", "policy_label", "stage")
.pol_cfg_keys   <- c("spec_label", "policy_label")

.outcomes_df <- dplyr::bind_rows(sim_outcomes_rows)
.save_parquet(.outcomes_df,                       out_outcomes, .outcomes_keys)
.save_csv(dplyr::bind_rows(sim_wx_quant_rows),  out_wx_quant, .wx_quant_keys)
.save_csv(dplyr::bind_rows(sim_meta_rows_all),  out_sim_meta, .sim_meta_keys)

# Return-period summary: per-member rank-interpolated RP thresholds aggregated
# to central/coef/ensemble/pooled uncertainty bands.
.ret_per_df   <- .build_return_period_df(.outcomes_df)
.ret_per_keys <- c("spec_label", "policy_label", "scenario_id", "agg_method", "estimate")
.save_csv(.ret_per_df, out_ret_per, .ret_per_keys)

# Year-by-decile decomposition
.yr_decile_keys <- c("spec_label", "policy_label", "scenario_id", "ensemble_member", "sim_year", "decile")
.save_parquet(dplyr::bind_rows(all_yr_decile_rows), out_yr_decile, .yr_decile_keys)

.save_csv(dplyr::bind_rows(all_fail_rows),       out_failures, .fail_keys)

# policy_config: new run wins on dedup so latest run_status is preserved
if (length(all_pol_cfg_rows) > 0L) {
  new_cfg <- dplyr::bind_rows(all_pol_cfg_rows)
  if (!OVERWRITE_EXISTING && file.exists(out_pol_cfg)) {
    existing_cfg <- readr::read_csv(out_pol_cfg, show_col_types = FALSE)
    new_cfg      <- dplyr::bind_rows(new_cfg, existing_cfg)   # new first → wins
    cat(sprintf("  [merge] %s (%d existing)\n", basename(out_pol_cfg), nrow(existing_cfg)))
  }
  new_cfg <- dplyr::distinct(new_cfg, dplyr::across(dplyr::any_of(.pol_cfg_keys)),
                              .keep_all = TRUE)
  readr::write_csv(new_cfg, out_pol_cfg)
  cat(sprintf("  Saved: %s (%d rows)\n", basename(out_pol_cfg), nrow(new_cfg)))
}

cat("\n========== Simulation pipeline complete ==========\n")