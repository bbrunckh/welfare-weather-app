# =============================================================================
# batch/03_run_mod1.R
#
# Batch Module 1 model fitting across countries and specifications.
# 
# Outputs:
#   OUT_DIR/model_fit/model_coefficients.csv
#   OUT_DIR/model_fit/model_fit_stats.csv
#   OUT_DIR/model_fit/_failures.csv (error logging)
#
# All user inputs are set in SECTION 1. Vector-valued settings (marked [GRID])
# expand into separate runs via expand.grid(); scalar settings apply uniformly.
#
# =============================================================================

pkgload::load_all(quiet = TRUE)
invisible(lapply(list.files("batch/R", pattern = "\\.R$", full.names = TRUE), source))

# =============================================================================
# SECTION 1 — CONFIGURATION
# =============================================================================

# ---- Data source (mod_0) ---------------------------------------------------
# "local"      -> e.g., set WISEAPP_DATA_PATH in .Renviron
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

# ---- Outcome variable (mod_1_03) -------------------------------------------
OUTCOME_NAME <- "welfare"
CURRENCY     <- "PPP"
POVERTY_LINE <- 3

# ---- Weather specs (mod_1_04) [GRID] ---------------------------------------
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
    transformations = "continuous",
    var_constructions = c("None"),
    ref_starts = 1L        # start month (months before interview); 1 = most recent
  )
)

# ---- Weather defaults (used when a profile omits a setting) -----------------
WEATHER_TRANSFORMATION <- "None"
N_BINS                 <- 5L
BINNING_METHOD         <- "Equal frequency"
CUSTOM_BREAKS          <- NULL
POLYNOMIAL             <- character(0)
WEATHER_AGG_OVERRIDE   <- NULL

# ---- Model type (mod_1_06) [GRID] ------------------------------------------
# "Linear regression", "Quantile regression (RIF)", "Logistic regression"
MODEL_TYPE <- c("Linear regression", "Quantile regression (RIF)")

# ---- Interactions (mod_1_06) [GRID] ----------------------------------------
# character(0) = no interaction; each entry interacts that variable with weather
INTERACTIONS <- list(character(0))

# ---- Fixed effects (mod_1_06) [GRID] ---------------------------------------
# Named list of FE profiles. Values are character vectors passed to fixest.
FIXED_EFFECTS <- list(
  year_admin1 = c("year", "gaul1_code"),
  year_loc    = c("year", "loc_id_panel")
  # year_only = c("year")
)

# ---- Covariate specs [GRID] ------------------------------------------------
# Named list of covariate profiles. Each must have `method` ("User-defined" or
# "Lasso"). User-defined profiles supply covariates by role.
COVARIATE_SPECS <- list(
  hhsize_urban_area = list(
    method = "User-defined",
    ind = character(0), hh = c("hhsize", "urban"),
    firm = character(0), area = c("area_h3_7")
  ), 
  lasso = list(method = "Lasso")
)

# ---- Lasso settings --------------------------------------------------------
LASSO_ALPHA         <- 1
LASSO_LAMBDA        <- "lambda.1se"
LASSO_NFOLDS        <- 10L
LASSO_STANDARDIZE   <- TRUE
MI_M                <- 5L
MI_MAXIT            <- 5L
STABILITY_THRESHOLD <- 0.5
LASSO_USE_PARALLEL  <- TRUE # use parallel if > 50,000 observations; evaluated dynamically at call site
LASSO_N_WORKERS     <- NULL
LASSO_PARALLEL_SEED <- NULL
LASSO_GLOBALS_MAX   <- NULL

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
# SECTION 2 — HELPERS
# =============================================================================

clean_names <- function(df) {
  nms <- tolower(names(df))
  nms <- gsub("[. ]+", "_", nms)
  nms <- gsub("_+$", "", nms)
  names(df) <- nms
  df
}

# Replicates the UI's .fixest_coeftable() fallback chain and returns a
# broom-compatible data frame (term, estimate, std.error, statistic, p.value).
tidy_clustered <- function(fit) {
  ct <- tryCatch(
    .fixest_coeftable(fit),  # ~loc_id_panel -> ~loc_id -> HC1 -> iid
    error = function(e) NULL
  )
  if (is.null(ct)) {
    return(broom::tidy(fit))
  }
  data.frame(
    term      = rownames(ct),
    estimate  = ct[["Estimate"]],
    std.error = ct[["Std. Error"]],
    statistic = ct[["t value"]],
    p.value   = ct[["Pr(>|t|)"]],
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

extract_one_fit <- function(fit, model_label, code, wx_label, wx_vars,
                            interaction_var, survey_df, engine,
                            fe_label = NA_character_, fe_vec = NULL,
                            cov_label = NA_character_,
                            cov_method = NA_character_,
                            lasso_selected_vars = NA_character_,
                            taus = NULL) {
  if (is.null(fit)) return(NULL)

  fe_str     <- if (!is.null(fe_vec)) paste(fe_vec, collapse = ",") else NA_character_
  inter_str  <- if (length(interaction_var) > 0) interaction_var else NA_character_
  wx_present <- sum(stats::complete.cases(survey_df[, wx_vars, drop = FALSE]))
  is_rif     <- identical(engine, "rif") && !is.null(taus)

  meta <- data.frame(
    code = code, weather = wx_label, engine = engine,
    fe_profile = fe_label, cov_profile = cov_label, cov_method = cov_method,
    interaction = inter_str, fixedeffects = fe_str, model = model_label,
    stringsAsFactors = FALSE
  )

  append_meta <- function(df) cbind(meta[rep(1L, nrow(df)), , drop = FALSE], df)

  if (is_rif) {
    coefs <- tryCatch({
      cf <- dplyr::bind_rows(lapply(seq_along(taus), function(i) {
        cf_i <- tryCatch(tidy_clustered(fit[[i]]), error = function(e) NULL)
        if (is.null(cf_i)) return(NULL)
        cf_i$tau      <- taus[i]
        cf_i$estimand <- sprintf("UQR p%d", round(taus[i] * 100))
        cf_i
      }))
      append_meta(cf)
    }, error = function(e) NULL)

    fit_stats <- tryCatch({
      fs <- dplyr::bind_rows(lapply(seq_along(taus), function(i) {
        m <- fit[[i]]
        data.frame(
          tau       = taus[i],
          estimand  = sprintf("UQR p%d", round(taus[i] * 100)),
          r2        = tryCatch(fixest::r2(m, "r2"),  error = function(e) NA),
          r2_adj    = NA_real_,
          r2_within = tryCatch(fixest::r2(m, "wr2"), error = function(e) NA),
          aic       = NA_real_,
          n_obs_fit = tryCatch(stats::nobs(m),       error = function(e) NA),
          stringsAsFactors = FALSE
        )
      }))
      fs$n_hh_total     <- nrow(survey_df)
      fs$n_hh_weather   <- wx_present
      fs$pct_weather    <- round(wx_present / nrow(survey_df) * 100, 1)
      fs$lasso_selected <- lasso_selected_vars
      append_meta(fs)
    }, error = function(e) NULL)

  } else {
    coefs <- tryCatch({
      cf <- tidy_clustered(fit)
      cf$tau <- NA_real_; cf$estimand <- "Mean"
      append_meta(cf)
    }, error = function(e) NULL)

    fit_stats <- tryCatch({
      fs <- data.frame(
        tau        = NA_real_, estimand = "Mean",
        r2         = tryCatch(fixest::r2(fit, "r2"),  error = function(e) NA),
        r2_adj     = tryCatch(fixest::r2(fit, "ar2"), error = function(e) NA),
        r2_within  = tryCatch(fixest::r2(fit, "wr2"), error = function(e) NA),
        aic        = tryCatch(stats::AIC(fit),        error = function(e) NA),
        n_obs_fit  = tryCatch(stats::nobs(fit),       error = function(e) NA),
        n_hh_total = nrow(survey_df), n_hh_weather = wx_present,
        pct_weather = round(wx_present / nrow(survey_df) * 100, 1),
        lasso_selected = lasso_selected_vars,
        stringsAsFactors = FALSE
      )
      append_meta(fs)
    }, error = function(e) NULL)
  }

  list(coefs = coefs, fit_stats = fit_stats)
}

# =============================================================================
# SECTION 3 — SETUP
# =============================================================================

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(OUT_DIR, "model_fit"), showWarnings = FALSE, recursive = TRUE)
OUT_MODEL <- file.path(OUT_DIR, "model_fit")

connection_params <- if (identical(CONNECTION_TYPE, "databricks")) {
  build_connection_params("databricks")
} else {
  build_connection_params("local", path = DATA_DIR)
}
stopifnot(
  "Invalid connection_params" = validate_connection_params(connection_params)
)
cat(sprintf("Connection: %s\n",
            if (identical(connection_params$type, "databricks"))
              "Databricks" else paste0("local (", connection_params$path, ")")))

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

# =============================================================================
# SECTION 4 — COMBINATION GRID
# =============================================================================

if (POOL_COUNTRIES) {
  if (is.null(COUNTRY_FILTER) || length(COUNTRY_FILTER) == length(COUNTRIES)) {
    SAMPLE_LABELS <- "All countries"
  } else {
    SAMPLE_LABELS <- paste(COUNTRIES, collapse = "_")
  }
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
  "Total: %d (%d sample x %d weather x %d model x %d interaction x %d FE x %d cov)\n\n",
  nrow(grid), length(SAMPLE_LABELS), length(WEATHER_SPECS), length(MODEL_TYPE),
  length(INTERACTIONS), length(FIXED_EFFECTS), length(COVARIATE_SPECS)
))

# =============================================================================
# SECTION 5 — MAIN LOOP
# =============================================================================

all_coefs     <- list()
all_fit_stats <- list()
run_idx       <- 0L
fail_log      <- list()
skip_log      <- list()
seen_fit1     <- character(0)
seen_fit2     <- character(0)

for (si in SAMPLE_LABELS) {
  sample_codes <- SAMPLE_CODES[[si]]
  cat(sprintf("\n=== %s ===\n", si))

  # Step 1 — Load and preprocess survey data (mod_1_01 + mod_1_02)
  years_by_code <- setNames(
    lapply(sample_codes, function(ci)
      as.character(sort(unique(
        surveys_with_fnames$year[surveys_with_fnames$code == ci]
      )))
    ),
    sample_codes
  )

  ss <- build_selected_surveys(surveys = surveys_with_fnames,
                               years_by_code = years_by_code)
  if (nrow(ss) == 0) {
    cat("  SKIP — no surveys\n")
    fail_log[[si]] <- "no_surveys"
    next
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

  if (is.null(svy_base)) {
    fail_log[[si]] <- "load_failed"
    next
  }
  cat(sprintf("  Loaded: %d rows (%s)\n", nrow(svy_base),
              paste(sample_codes, collapse = ", ")))

  # loc_id_panel for clustered SEs (replicates mod_1_02_surveystats)
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
        dplyr::left_join(loc_keys, panel_map, by = c("code", "year", "survname", "loc_id")),
        by = c("code", "year", "survname", "loc_id")
      )
    cat(sprintf("  loc_id_panel: %d groups, %d locations\n",
                length(unique(svy_base$loc_id_panel)),
                sum(!is.na(svy_base$loc_id_panel))))
  }, error = function(e) {
    message("  loc_id_panel failed (SEs fall back to ~loc_id): ",
            conditionMessage(e))
  })

  wx_profiles <- unique(grid$weather[grid$sample == si])

  # Step 2 — Loop over weather profiles
  for (wx_name in wx_profiles) {
    wx_prof <- WEATHER_SPECS[[wx_name]]
    wx_vars <- names(wx_prof)

    spec_inputs <- list()
    for (v in wx_vars) {
      vs <- wx_prof[[v]]
      p  <- paste0(v, "_")
      spec_inputs[[paste0(p, "relativePeriod")]]  <- c(vs$ref_start %||% 1L, vs$ref_end)
      spec_inputs[[paste0(p, "temporalAgg")]]     <- vs$temporal_agg %||% weather_agg_for(v, get_weather_vars(var_info), WEATHER_AGG_OVERRIDE)
      spec_inputs[[paste0(p, "varConstruction")]]  <- vs$weather_transformation %||% WEATHER_TRANSFORMATION
      spec_inputs[[paste0(p, "contOrBinned")]]     <- if (vs$transformation == "binned") "Binned" else "Continuous"
      spec_inputs[[paste0(p, "numBins")]]          <- vs$n_bins %||% N_BINS
      spec_inputs[[paste0(p, "binningMethod")]]    <- vs$binning_method %||% BINNING_METHOD
      spec_inputs[[paste0(p, "customBreaks")]]     <- vs$custom_breaks %||% CUSTOM_BREAKS[[v]]
      spec_inputs[[paste0(p, "polynomial")]]       <- vs$polynomial %||% POLYNOMIAL
    }

    selected_weather <- tryCatch(
      build_selected_weather(selected_vars = wx_vars,
                             var_info = get_weather_vars(var_info),
                             spec_inputs = spec_inputs),
      error = function(e) { message(" weather build: ", conditionMessage(e)); NULL }
    )
    if (is.null(selected_weather) || nrow(selected_weather) == 0) {
      cat(sprintf("  FAIL (weather build) — %s\n", wx_name))
      fail_log[[paste(si, wx_name, sep = "_")]] <- "weather_build_failed"
      next
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
      fail_log[[paste(si, wx_name, sep = "_")]] <- "weather_load_failed"
      next
    }
    cat(" done\n")

    stored_breaks <- attr(weather_data, "stored_breaks")
    svy_wx <- merge_survey_weather(svy_base, weather_data[["historical"]])
    if (is.null(svy_wx) || nrow(svy_wx) == 0) {
      cat("  FAIL — weather merge produced 0 rows\n")
      fail_log[[paste(si, wx_name, sep = "_")]] <- "weather_merge_empty"
      next
    }
    cat(sprintf("  Merged: %d rows\n", nrow(svy_wx)))

    wx_col_names <- intersect(selected_weather$name, names(svy_wx))

    # Step 3 — Loop over model specs (model_type x interaction x FE x covariates)
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

      if (!OVERWRITE_EXISTING &&
          file.exists(file.path(OUT_MODEL, si,
                                paste0("coefplot_", spec_label, ".png")))) {
        cat(" SKIP (exists)\n")
        next
      }

      t0 <- proc.time()[["elapsed"]]

      if (length(interaction_var) > 0 && !interaction_var %in% names(svy_wx)) {
        cat(sprintf(" SKIP — '%s' not in survey\n", interaction_var))
        skip_log[[spec_label]] <- list(
          reason = sprintf("interaction_%s_not_available", interaction_var),
          sample = si, interaction = paste(interaction_var, collapse = "|")
        )
        next
      }

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
        fail_log[[spec_label]] <- sprintf("only_%d_complete_cases", n_complete)
        next
      }

      # Filter covariates to >=90% non-missing per survey group (matches app).
      valid_vl <- filter_valid_vars(svy_wx, var_info, min_complete = 0.9,
                                    group_cols = c("code", "year", "survname"),
                                    outcome = OUTCOME_NAME)
      valid_vl <- valid_vl[!valid_vl$name %in% exclude_cols, , drop = FALSE]
      # Exclude post-treatment variables (outcome == 1) — matches app behaviour
      valid_vl <- exclude_selected_vars(valid_vl)

      lasso_selected_vars <- NA_character_

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
          setdiff(unique(c(base, LASSO_FORCE_IN[[role]])),
                  LASSO_FORCE_OUT[[role]])
        }
        ind_covs  <- resolve_role("ind")
        hh_covs   <- resolve_role("hh")
        firm_covs <- resolve_role("firm")
        area_covs <- resolve_role("area")

        lasso_selected_vars <- if (length(lasso_res$selected_covariates) > 0)
          paste(lasso_res$selected_covariates, collapse = "|") else ""
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

      engine_used <- selected_model$engine
      fit_args <- list(wx_label = wx_name, wx_vars = wx_col_names,
                       interaction_var = interaction_var, survey_df = svy_wx,
                       engine = engine_used, taus = mf$taus)

      # fit1 (weather only) — deduplicate across FE x cov x model_type
      fit1_key <- paste(si, wx_name, mt_label, inter_label, sep = "|")
      if (!fit1_key %in% seen_fit1) {
        r1 <- do.call(extract_one_fit,
                      c(list(fit = mf$fit1, model_label = "fit1", code = si),
                        fit_args))
        if (!is.null(r1$coefs))     all_coefs[[length(all_coefs) + 1L]]         <- r1$coefs
        if (!is.null(r1$fit_stats)) all_fit_stats[[length(all_fit_stats) + 1L]] <- r1$fit_stats
        seen_fit1 <- c(seen_fit1, fit1_key)
      }

      # fit2 (weather + FE) — deduplicate across cov
      fit2_key <- paste(si, wx_name, mt_label, inter_label, fe_label, sep = "|")
      if (!fit2_key %in% seen_fit2) {
        r2 <- do.call(extract_one_fit,
                      c(list(fit = mf$fit2, model_label = "fit2", code = si,
                             fe_label = fe_label, fe_vec = fe_vec),
                        fit_args))
        if (!is.null(r2$coefs))     all_coefs[[length(all_coefs) + 1L]]         <- r2$coefs
        if (!is.null(r2$fit_stats)) all_fit_stats[[length(all_fit_stats) + 1L]] <- r2$fit_stats
        seen_fit2 <- c(seen_fit2, fit2_key)
      }

      # fit3 (weather + FE + controls) — always unique
      r3 <- do.call(extract_one_fit,
                    c(list(fit = mf$fit3, model_label = "fit3", code = si,
                           fe_label = fe_label, fe_vec = fe_vec,
                           cov_label = cov_label, cov_method = cov_method,
                           lasso_selected_vars = lasso_selected_vars),
                      fit_args))
      if (!is.null(r3$coefs))     all_coefs[[length(all_coefs) + 1L]]         <- r3$coefs
      if (!is.null(r3$fit_stats)) all_fit_stats[[length(all_fit_stats) + 1L]] <- r3$fit_stats

      cat(sprintf(" %.1fs DONE\n", round(proc.time()[["elapsed"]] - t0, 1)))

      # -- memory: release large objects immediately after extracting results
      rm(mf, survey_prep, selected_model, selected_outcome)
      gc(verbose = FALSE)

      # -- checkpoint: flush accumulated results to disk every 20 specs
      if (run_idx %% 20L == 0L && length(all_coefs) > 0) {
        tryCatch({
          readr::write_csv(
            clean_names(dplyr::bind_rows(all_coefs)),
            file.path(OUT_MODEL, "_checkpoint_coefficients.csv")
          )
          if (length(all_fit_stats) > 0)
            readr::write_csv(
              clean_names(dplyr::bind_rows(all_fit_stats)),
              file.path(OUT_MODEL, "_checkpoint_fit_stats.csv")
            )
          cat(sprintf("  [checkpoint @ %d]\n", run_idx))
        }, error = function(e) message("  checkpoint failed: ", conditionMessage(e)))
      }
    }

    # -- memory: release weather-merged data before loading next profile
    rm(svy_wx, weather_data)
    gc(verbose = FALSE)
  }
}

# =============================================================================
# SECTION 6 — SAVE SUMMARY
# =============================================================================

cat("\n=== Saving combined outputs ===\n")

if (length(all_coefs) > 0) {
  summary_coefs <- clean_names(dplyr::bind_rows(all_coefs))
  readr::write_csv(summary_coefs, file.path(OUT_MODEL, "model_coefficients.csv"))
  n_specs <- nrow(
    dplyr::filter(summary_coefs, model == "fit3") |>
      dplyr::distinct(code, weather, engine, fe_profile, cov_profile, interaction)
  )
  cat(sprintf("Coefficients: %d rows (%d fit3 specs), %d samples\n",
              nrow(summary_coefs), n_specs, length(unique(summary_coefs$code))))
}

if (length(all_fit_stats) > 0) {
  summary_stats <- clean_names(dplyr::bind_rows(all_fit_stats))
  readr::write_csv(summary_stats, file.path(OUT_MODEL, "model_fit_stats.csv"))
  cat(sprintf("Fit stats: %d rows\n", nrow(summary_stats)))
}

if (length(fail_log) > 0) {
  fail_df <- data.frame(spec_or_sample = names(fail_log),
                         reason = unlist(fail_log), stringsAsFactors = FALSE)
  readr::write_csv(fail_df, file.path(OUT_MODEL, "_failures.csv"))
  cat(sprintf("Failures: %d\n", nrow(fail_df)))
}

if (length(skip_log) > 0) {
  skip_df <- data.frame(
    spec_label  = names(skip_log),
    reason      = vapply(skip_log, `[[`, character(1), "reason"),
    sample      = vapply(skip_log, `[[`, character(1), "sample"),
    interaction = vapply(skip_log, `[[`, character(1), "interaction"),
    stringsAsFactors = FALSE
  )
  readr::write_csv(skip_df, file.path(OUT_MODEL, "_interactions_not_available.csv"))
  cat(sprintf("Interactions not available: %d skipped\n", nrow(skip_df)))
}

# Clean up checkpoint files now that final outputs are saved
for (cp in c("_checkpoint_coefficients.csv", "_checkpoint_fit_stats.csv")) {
  cp_path <- file.path(OUT_MODEL, cp)
  if (file.exists(cp_path)) {
    file.remove(cp_path)
    cat(sprintf("Removed checkpoint: %s\n", cp))
  }
}

# Per-sample summary
cat("\n--- Results by sample ---\n")
if (length(all_coefs) > 0 || length(skip_log) > 0 || length(fail_log) > 0) {
  grid_count <- as.data.frame(table(grid$sample), stringsAsFactors = FALSE)
  names(grid_count) <- c("sample", "attempted")

  succeeded_count <- if (length(all_fit_stats) > 0) {
    summary_stats |>
      dplyr::filter(model == "fit3") |>
      dplyr::distinct(code, weather, engine, fe_profile, cov_profile, interaction) |>
      dplyr::count(code, name = "succeeded") |>
      dplyr::rename(sample = code)
  } else data.frame(sample = character(0), succeeded = integer(0))

  skipped_count <- if (length(skip_log) > 0) {
    skip_df |> dplyr::count(sample, name = "skipped")
  } else data.frame(sample = character(0), skipped = integer(0))

  failed_count <- if (length(fail_log) > 0) {
    sample_level <- c("no_surveys", "load_failed")
    fail_df$sample <- vapply(seq_len(nrow(fail_df)), function(i) {
      if (fail_df$reason[i] %in% sample_level) return(fail_df$spec_or_sample[i])
      key <- fail_df$spec_or_sample[i]
      hits <- SAMPLE_LABELS[vapply(SAMPLE_LABELS, function(s)
        startsWith(key, paste0(s, "_")), logical(1))]
      if (length(hits) > 0) hits[which.max(nchar(hits))] else key
    }, character(1))
    fail_df[!fail_df$reason %in% sample_level, ] |>
      dplyr::count(sample, name = "failed")
  } else data.frame(sample = character(0), failed = integer(0))

  sample_summary <- grid_count |>
    dplyr::left_join(succeeded_count, by = "sample") |>
    dplyr::left_join(skipped_count,   by = "sample") |>
    dplyr::left_join(failed_count,    by = "sample") |>
    dplyr::mutate(
      succeeded   = ifelse(is.na(succeeded), 0L, succeeded),
      skipped     = ifelse(is.na(skipped),   0L, skipped),
      failed      = ifelse(is.na(failed),    0L, failed),
      eligible    = attempted - skipped,
      pct_success = ifelse(eligible > 0, round(succeeded / eligible * 100, 1), NA_real_)
    ) |>
    dplyr::select(sample, attempted, skipped, eligible,
                  succeeded, failed, pct_success) |>
    dplyr::arrange(dplyr::desc(failed), dplyr::desc(skipped))
  print(sample_summary, row.names = FALSE)
}

n_succeeded <- if (length(all_fit_stats) > 0) {
  sum(vapply(all_fit_stats, function(x) "fit3" %in% x$model, logical(1)))
} else 0L

cat(sprintf("\nTotal combinations: %d\n", nrow(grid)))
cat(sprintf("Attempted:          %d\n", run_idx))
cat(sprintf("Succeeded:          %d\n", n_succeeded))
cat(sprintf("Skipped:            %d\n", length(skip_log)))
cat(sprintf("Failed:             %d\n", run_idx - n_succeeded - length(skip_log)))
cat("========== Batch complete ==========\n")
