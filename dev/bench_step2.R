# Development-only Phase 1 Step 2 benchmark harness.
#
# This script deliberately calls the existing pure Step 2 entry point rather
# than reproducing its orchestration. It supports two input modes:
#
#   1. Local data mode, enabled by WISEAPP_DATA_PATH. The script prepares a
#      country snapshot using the same survey, weather, and model functions as
#      the application.
#   2. Snapshot mode, enabled by WISEAPP_STEP2_SNAPSHOT_RDS. The RDS must be a
#      named list keyed by country. Each value is a list containing at least
#      sw, so, svy, ss, mf, cp, and sim_dates. The mf entry may contain a
#      single model fit or a named `models` list.
#
# The harness does not write snapshots automatically because they can contain
# household-level data and fitted model objects. It writes only benchmark
# summaries and reports under WISEAPP_STEP2_OUTPUT_DIR.
#
# Recommended usage:
#   WISEAPP_DATA_PATH=/path/to/data \
#     WISEAPP_STEP2_COUNTRIES=LKA,IRN \
#     Rscript dev/bench_step2.R
#
# Set WISEAPP_STEP2_PAYLOAD_MODE=compact to benchmark the opt-in compact result
# payload. The default is legacy so existing baseline runs remain comparable.
# Set WISEAPP_STEP2_INCLUDE_STEP3=1 to run deterministic Step 3 policy fixtures
# after successful Step 2 cases. WISEAPP_STEP2_FIXTURE=smoke uses fabricated,
# in-memory data for smoke testing only and is never production-path evidence.
#
# For process-tree RSS, use dev/run_step2_benchmark.sh. The R-side RSS values
# are sampled at stage boundaries; /usr/bin/time -l remains the external peak
# memory measurement and production memory gate.

options(golem.app.prod = FALSE)

# -----------------------------------------------------------------------------
# Configuration helpers
# -----------------------------------------------------------------------------

.bench_script_path <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (!length(file_arg)) return(normalizePath("dev/bench_step2.R", mustWork = FALSE))
  sub("^--file=", "", file_arg[[1L]])
}

.bench_repo_root <- normalizePath(
  file.path(dirname(.bench_script_path()), ".."),
  winslash = "/", mustWork = TRUE
)

pkgload::load_all(.bench_repo_root, quiet = TRUE)
source(file.path(.bench_repo_root, "dev", "bench_step2_helpers.R"), local = TRUE)
source(file.path(.bench_repo_root, "dev", "bench_step3_helpers.R"), local = TRUE)

.bench_env <- function(name, default = "") {
  value <- Sys.getenv(name, unset = default)
  if (is.null(value)) default else value
}

.bench_env_int <- function(name, default) {
  value <- suppressWarnings(as.integer(.bench_env(name, as.character(default))))
  if (length(value) != 1L || is.na(value)) default else value
}

.bench_env_num <- function(name, default) {
  value <- suppressWarnings(as.numeric(.bench_env(name, as.character(default))))
  if (length(value) != 1L || is.na(value)) default else value
}

.bench_env_flag <- function(name, default = FALSE) {
  value <- tolower(trimws(.bench_env(name, if (default) "1" else "0")))
  if (value %in% c("1", "true", "yes", "on")) return(TRUE)
  if (value %in% c("0", "false", "no", "off", "")) return(FALSE)
  warning(name, " has an invalid value; using the configured default.",
          call. = FALSE)
  default
}

.bench_env_csv <- function(name, default = character(0)) {
  raw <- trimws(.bench_env(name, ""))
  if (!nzchar(raw)) return(default)
  out <- trimws(unlist(strsplit(raw, ",", fixed = TRUE), use.names = FALSE))
  unique(out[nzchar(out)])
}

.bench_parse_periods <- function(raw, default) {
  if (!nzchar(trimws(raw))) return(default)
  pieces <- trimws(unlist(strsplit(raw, ";", fixed = TRUE), use.names = FALSE))
  out <- lapply(pieces, function(piece) {
    parts <- suppressWarnings(as.integer(unlist(strsplit(piece, "-", fixed = TRUE))))
    if (length(parts) != 2L || anyNA(parts) || parts[[2L]] <= parts[[1L]]) {
      stop("Invalid projection period '", piece,
           "'; expected START-END with END > START.", call. = FALSE)
    }
    parts
  })
  out
}

.bench_config <- function() {
  default_periods <- list(c(2025L, 2035L), c(2040L, 2060L), c(2080L, 2100L))
  list(
    seed          = .bench_env_int("WISEAPP_STEP2_SEED", 123L),
    repetitions   = max(2L, .bench_env_int("WISEAPP_STEP2_REPETITIONS", 3L)),
    countries     = .bench_env_csv("WISEAPP_STEP2_COUNTRIES", c("LKA")),
    unit          = .bench_env("WISEAPP_STEP2_UNIT", "hh"),
    outcome       = .bench_env("WISEAPP_STEP2_OUTCOME", "welfare"),
    weather       = .bench_env_csv("WISEAPP_STEP2_WEATHER", c("t", "spei6")),
    weather_form  = .bench_env("WISEAPP_STEP2_WEATHER_FORM", "Continuous"),
    ref_end       = .bench_env_int("WISEAPP_STEP2_REF_END", 3L),
    hist_years    = as.integer(c(
      .bench_env_int("WISEAPP_STEP2_HIST_START", 1991L),
      .bench_env_int("WISEAPP_STEP2_HIST_END", 2020L)
    )),
    ssps          = .bench_env_csv(
      "WISEAPP_STEP2_SSPS",
      c("ssp2_4_5", "ssp3_7_0", "ssp5_8_5")
    ),
    periods       = .bench_parse_periods(
      .bench_env("WISEAPP_STEP2_PERIODS", ""), default_periods
    ),
    models        = .bench_env_csv("WISEAPP_STEP2_MODELS", c("ols", "rif")),
    aggregation   = .bench_env_csv(
      "WISEAPP_STEP2_AGG_METHODS",
      c("mean", "median", "gini", "headcount_ratio", "gap", "fgt2")
    ),
    workloads     = .bench_env_csv(
      "WISEAPP_STEP2_WORKLOADS",
      c("historical", "one_ssp_one_period", "three_ssps_three_periods")
    ),
    uncertainty_modes = .bench_env_csv(
      "WISEAPP_STEP2_UNCERTAINTY",
      c("disabled", "enabled")
    ),
    snapshot_rds  = .bench_env("WISEAPP_STEP2_SNAPSHOT_RDS", ""),
    data_path     = .bench_env("WISEAPP_DATA_PATH", ""),
    output_dir    = .bench_env(
      "WISEAPP_STEP2_OUTPUT_DIR",
      file.path(.bench_repo_root, "dev", "outputs", "step2-benchmark")
    ),
    force_cache   = .bench_env_flag("WISEAPP_STEP2_FORCE_CACHE", TRUE),
    include_step3 = .bench_env_flag("WISEAPP_STEP2_INCLUDE_STEP3", FALSE),
    fixture_mode  = .bench_env("WISEAPP_STEP2_FIXTURE", "production"),
    step3_policies = .bench_env_csv(
      "WISEAPP_STEP3_POLICIES", c("covariate", "targeted_sp", "combined")
    ),
    payload_mode  = .bench_env("WISEAPP_STEP2_PAYLOAD_MODE", "compact"),
    weather_storage = .bench_env("WISEAPP_STEP2_WEATHER_STORAGE", "memory"),
    weather_collect = .bench_env("WISEAPP_STEP2_WEATHER_COLLECT", "fast"),
    weather_threads = .bench_env("WISEAPP_STEP2_WEATHER_THREADS", "auto"),
    join_cache = .bench_env_flag("WISEAPP_STEP2_JOIN_CACHE", FALSE),
    direct_rif_predictions = .bench_env_flag("WISEAPP_STEP2_DIRECT_RIF_PREDICTIONS", TRUE),
    key_workers = max(1L, min(2L, .bench_env_int("WISEAPP_STEP2_KEY_WORKERS", 1L)))
  )
}

cfg <- .bench_config()
dir.create(cfg$output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(cfg$output_dir, "weather-cache"),
           recursive = TRUE, showWarnings = FALSE)

# Use the benchmark-owned cache for both local and snapshot modes. Local data
# normally bypasses the application cache, so force it here to make cold/warm
# cache cases observable and reproducible.
Sys.setenv(
  WISEAPP_WEATHER_CACHE_DIR = file.path(cfg$output_dir, "weather-cache"),
  WISEAPP_WEATHER_CACHE_FORCE = if (cfg$force_cache) "1" else "0",
  WISEAPP_WEATHER_CACHE_DISABLE = "0"
)

if (!cfg$unit %in% c("hh", "ind", "firm")) {
  stop("WISEAPP_STEP2_UNIT must be one of hh, ind, or firm.", call. = FALSE)
}
if (!length(cfg$models)) stop("At least one Step 2 model is required.", call. = FALSE)
if (!all(cfg$models %in% c("ols", "rif"))) {
  stop("WISEAPP_STEP2_MODELS values must be ols and/or rif.", call. = FALSE)
}
if (!length(cfg$aggregation)) {
  stop("At least one aggregation method is required.", call. = FALSE)
}
if (!length(cfg$workloads) || !all(cfg$workloads %in% c(
  "historical", "one_ssp_one_period", "two_ssps_two_periods",
  "three_ssps_three_periods"
))) {
  stop(
    "WISEAPP_STEP2_WORKLOADS values must be historical, one_ssp_one_period, ",
    "two_ssps_two_periods, and/or three_ssps_three_periods.", call. = FALSE
  )
}
if (!length(cfg$uncertainty_modes) || !all(cfg$uncertainty_modes %in%
                                            c("disabled", "enabled"))) {
  stop("WISEAPP_STEP2_UNCERTAINTY values must be disabled and/or enabled.",
       call. = FALSE)
}
if (!cfg$payload_mode %in% c("legacy", "compact")) {
  stop("WISEAPP_STEP2_PAYLOAD_MODE must be legacy or compact.", call. = FALSE)
}
if (!cfg$weather_storage %in% c("memory", "reference")) {
  stop("WISEAPP_STEP2_WEATHER_STORAGE must be memory or reference.", call. = FALSE)
}
if (!cfg$weather_collect %in% c("fast", "bounded")) {
  stop("WISEAPP_STEP2_WEATHER_COLLECT must be fast or bounded.", call. = FALSE)
}
if (!cfg$weather_threads %in% c("auto", "1", "2")) {
  stop("WISEAPP_STEP2_WEATHER_THREADS must be auto, 1, or 2.", call. = FALSE)
}
if (!cfg$fixture_mode %in% c("production", "smoke")) {
  stop("WISEAPP_STEP2_FIXTURE must be production or smoke.", call. = FALSE)
}
if (cfg$include_step3) invisible(
  .bench_step3_policy_fixtures(cfg$step3_policies)
)

set.seed(cfg$seed)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

# -----------------------------------------------------------------------------
# Metadata and input preparation
# -----------------------------------------------------------------------------

.bench_connection <- function(config) {
  if (identical(config$fixture_mode, "smoke")) return(NULL)
  if (nzchar(config$data_path)) {
    path <- normalizePath(path.expand(config$data_path), mustWork = TRUE)
    return(build_connection_params("local", path = path))
  }
  if (nzchar(config$snapshot_rds)) return(NULL)
  stop(
    "Set WISEAPP_DATA_PATH for local-data mode or ",
    "WISEAPP_STEP2_SNAPSHOT_RDS for snapshot mode.",
    call. = FALSE
  )
}

connection_params <- .bench_connection(cfg)
metadata <- NULL

if (!is.null(connection_params)) {
  metadata <- load_overview_metadata(connection_params)
  if (!validate_connection_params(connection_params)) {
    stop("The configured connection parameters are invalid.", call. = FALSE)
  }
}

.bench_weather_spec <- function(var_info, selected_vars, config) {
  wx_info <- get_weather_vars(var_info)
  selected_vars <- intersect(selected_vars, wx_info$name)
  if (!length(selected_vars)) {
    stop(
      "None of the requested weather variables are available. Requested: ",
      paste(config$weather, collapse = ", "), call. = FALSE
    )
  }

  spec_inputs <- list()
  for (v in selected_vars) {
    units <- as.character(wx_info$units[match(v, wx_info$name)])
    if (is.na(units)) units <- ""
    spec_inputs[[paste0(v, "_relativePeriod")]] <- c(1L, config$ref_end)
    spec_inputs[[paste0(v, "_temporalAgg")]] <- temporal_agg_default(units)
    spec_inputs[[paste0(v, "_varConstruction")]] <- "None"
    spec_inputs[[paste0(v, "_contOrBinned")]] <- config$weather_form
    spec_inputs[[paste0(v, "_numBins")]] <- 5L
    spec_inputs[[paste0(v, "_binningMethod")]] <- "Equal frequency"
    spec_inputs[[paste0(v, "_customBreaks")]] <- numeric(0)
    spec_inputs[[paste0(v, "_polynomial")]] <- character(0)
  }

  build_selected_weather(
    selected_vars = selected_vars,
    var_info = wx_info,
    spec_inputs = spec_inputs
  )
}

.bench_selected_surveys <- function(survey_list, country, config, cp) {
  level <- switch(config$unit, hh = "hh", ind = "ind", firm = "firm")
  surveys <- build_survey_fnames(survey_list, level, cp)
  available <- list_available_files(cp)
  surveys <- filter_surveys_to_available(surveys, available)
  surveys <- surveys[surveys$code %in% country, , drop = FALSE]
  if (!nrow(surveys)) {
    stop("No ", level, " survey files found for country ", country, ".",
         call. = FALSE)
  }

  years_raw <- .bench_env("WISEAPP_STEP2_SURVEY_YEARS", "")
  years <- if (nzchar(years_raw)) {
    parsed <- suppressWarnings(as.numeric(unlist(strsplit(years_raw, ",", fixed = TRUE))))
    parsed <- parsed[is.finite(parsed)]
    intersect(sort(unique(parsed)), sort(unique(surveys$year)))
  } else sort(unique(surveys$year))
  if (!length(years)) stop("No configured survey years are available for ", country, ".",
                           call. = FALSE)
  build_selected_surveys(surveys, setNames(list(years), country))
}

.bench_load_survey <- function(ss, cp, var_info, cpi_ppp) {
  df <- load_data(ss$fname, cp, collect = TRUE, unify_schemas = TRUE)
  df <- add_time_columns(df)
  lcu_vars <- get_lcu_vars(df, var_info)
  df |>
    assign_data_level() |>
    convert_lcu_to_ppp(cpi_ppp, lcu_vars) |>
    bottom_code_welfare(0.28) |>
    apply_policy_derivations()
}

.bench_add_panel <- function(svy, ss, cp) {
  h3_fnames <- ss |>
    dplyr::distinct(code, year, survname, source) |>
    dplyr::mutate(fname = paste0(
      "microdata/h3/", code, "/", code, "_", year, "_", survname,
      "_", source, "_h3.parquet"
    )) |>
    dplyr::pull(fname)
  h3_df <- load_data(h3_fnames, cp)
  panel_map <- loc_panel(
    h3_df, id_col = loc_id, h3_col = h3, weight_col = pop_2020,
    group_cols = c("code", "year", "survname")
  )
  loc_keys <- h3_df |>
    dplyr::distinct(code, year, survname, loc_id) |>
    collect_deterministic(c("code", "year", "survname", "loc_id"))
  svy |>
    dplyr::left_join(
      dplyr::left_join(loc_keys, panel_map,
                       by = c("code", "year", "survname", "loc_id")),
      by = c("code", "year", "survname", "loc_id")
    )
}

.bench_latest_baseline <- function(svy) {
  year_int <- suppressWarnings(as.integer(as.character(svy$year)))
  keep <- year_int == max(year_int, na.rm = TRUE)
  svy[keep, , drop = FALSE]
}

.bench_model_spec <- function(config, model_label, sw, svy_wx, so) {
  requested_type <- if (identical(model_label, "rif")) {
    "Unconditional quantile regression (RIF)"
  } else {
    "Linear regression"
  }
  fe <- intersect(c("year", "loc_id_panel"), names(svy_wx))
  build_selected_model(
    model_type = requested_type,
    interactions = character(0),
    fixedeffects = fe,
    covariate_selection = "User-defined",
    ind_covariates = character(0),
    hh_covariates = character(0),
    firm_covariates = character(0),
    area_covariates = character(0),
    cluster = if ("loc_id_panel" %in% names(svy_wx)) "loc_id_panel" else NULL
  )
}

.bench_build_local_inputs <- function(country, config, cp, meta) {
  ss <- .bench_selected_surveys(meta$survey_list, country, config, cp)
  svy <- .bench_load_survey(ss, cp, meta$variable_list, meta$cpi_ppp)
  svy <- tryCatch(
    .bench_add_panel(svy, ss, cp),
    error = function(e) {
      warning("Could not add loc_id_panel for ", country, ": ", conditionMessage(e),
              call. = FALSE)
      svy
    }
  )

  sw <- .bench_weather_spec(meta$variable_list, config$weather, config)
  so_info <- meta$variable_list[meta$variable_list$name == config$outcome, , drop = FALSE]
  if (!nrow(so_info)) stop("Outcome not found in variable metadata: ", config$outcome,
                          call. = FALSE)
  so <- build_selected_outcome(so_info[1L, , drop = FALSE], "PPP", 3)

  # This load is the Step 1 weather/model preparation path. Step 2 cases below
  # load historical plus future weather through fct_run_simulation().
  train_weather <- get_weather(
    survey_data = svy,
    selected_surveys = ss,
    selected_weather = sw,
    dates = extract_survey_dates(svy),
    connection_params = cp
  )
  stored_breaks <- attr(train_weather, "stored_breaks")
  svy_wx <- merge_survey_weather(svy, train_weather[["historical"]])
  if (is.null(svy_wx) || !nrow(svy_wx)) {
    stop("Survey-weather merge returned no rows for ", country, ".", call. = FALSE)
  }

  # Fit on all selected survey waves, matching the batch pipeline. Simulate on
  # the latest wave, matching the Step 2 baseline survey selection.
  survey_fit <- prepare_outcome_df(svy_wx, so)
  models <- list()
  for (model_label in config$models) {
    model_spec <- .bench_model_spec(config, model_label, sw, svy_wx, so)
    models[[model_label]] <- tryCatch(
      suppressWarnings(fit_model(
        df = survey_fit,
        selected_outcome = so,
        selected_weather = sw,
        selected_model = model_spec
      )),
      error = function(e) {
        warning("Skipping ", model_label, " for ", country, ": ",
                conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }
  models <- Filter(Negate(is.null), models)
  if (!length(models)) stop("No requested model fit succeeded for ", country, ".",
                            call. = FALSE)

  svy_baseline <- .bench_latest_baseline(svy_wx)
  list(
    country = country,
    sw = sw,
    so = so,
    svy = svy_baseline,
    ss = ss,
    cp = cp,
    models = models,
    sim_dates = build_hist_sim_dates(svy_baseline, config$hist_years),
    stored_breaks = stored_breaks,
    metadata = list(
      selected_surveys = unique(ss[, intersect(c("code", "year", "survname", "source"),
                                                names(ss)), drop = FALSE]),
      n_survey_rows = nrow(svy),
      n_baseline_rows = nrow(svy_baseline),
      n_model_rows = nrow(survey_fit),
      n_complete = sum(stats::complete.cases(
        survey_fit[, unique(c(so$name, sw$name)), drop = FALSE]
      ))
    )
  )
}

.bench_load_snapshots <- function(path, countries) {
  path <- normalizePath(path.expand(path), mustWork = TRUE)
  value <- readRDS(path)
  if (!is.list(value)) stop("Step 2 snapshot RDS must contain a list.", call. = FALSE)
  if (!is.null(value$inputs) && is.list(value$inputs)) value <- value$inputs
  missing <- setdiff(countries, names(value))
  if (length(missing)) {
    stop("Snapshot RDS is missing country entries: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  value[countries]
}

.bench_validate_input <- function(input, country) {
  required <- c("sw", "so", "svy", "ss", "cp", "sim_dates")
  missing <- setdiff(required, names(input))
  if (!is.null(input$models)) {
    if (!is.list(input$models)) stop("Snapshot models for ", country, " must be a list.",
                                    call. = FALSE)
  } else if (!is.null(input$mf)) {
    model_name <- if (identical(input$mf$engine, "rif")) "rif" else "ols"
    input$models <- stats::setNames(list(input$mf), model_name)
  } else {
    missing <- c(missing, "mf or models")
  }
  if (length(missing)) stop("Snapshot for ", country, " is missing: ",
                            paste(unique(missing), collapse = ", "), call. = FALSE)
  input
}

inputs_by_country <- if (identical(cfg$fixture_mode, "smoke")) {
  setNames(lapply(cfg$countries, function(country) {
    .bench_small_step3_input(country, seed = cfg$seed)
  }), cfg$countries)
} else if (nzchar(cfg$snapshot_rds)) {
  .bench_load_snapshots(cfg$snapshot_rds, cfg$countries)
} else {
  setNames(lapply(cfg$countries, function(country) {
    message("Preparing local benchmark input: ", country)
    .bench_build_local_inputs(country, cfg, connection_params, metadata)
  }), cfg$countries)
}
inputs_by_country <- setNames(
  lapply(names(inputs_by_country), function(country) {
    input <- inputs_by_country[[country]]
    if ("default" %in% names(input$models)) {
      model_name <- if (identical(input$models$default$engine, "rif")) "rif" else "ols"
      input$models[[model_name]] <- input$models$default
      input$models$default <- NULL
    }
    .bench_validate_input(input, country)
  }), names(inputs_by_country)
)

# -----------------------------------------------------------------------------
# Measurement helpers
# -----------------------------------------------------------------------------

.bench_rss <- function() {
  raw <- tryCatch(
    system2("ps", c("-axo", "pid=,ppid=,rss="), stdout = TRUE, stderr = FALSE),
    error = function(e) character(0)
  )
  if (!length(raw)) return(c(parent_kb = NA_real_, tree_kb = NA_real_))
  tab <- tryCatch(
    utils::read.table(text = raw, col.names = c("pid", "ppid", "rss")),
    error = function(e) NULL
  )
  if (is.null(tab) || !nrow(tab)) return(c(parent_kb = NA_real_, tree_kb = NA_real_))
  pid <- Sys.getpid()
  parent <- tab[tab$pid == pid, "rss"]
  tree <- pid
  repeat {
    children <- tab$pid[tab$ppid %in% tree]
    new_children <- setdiff(children, tree)
    if (!length(new_children)) break
    tree <- c(tree, new_children)
  }
  c(
    parent_kb = if (length(parent)) as.numeric(parent[[1L]]) else NA_real_,
    tree_kb = sum(tab$rss[tab$pid %in% tree], na.rm = TRUE)
  )
}

.bench_sample_rss <- function(state) {
  rss <- .bench_rss()
  state$rss_parent_peak_kb <- max(state$rss_parent_peak_kb, rss[["parent_kb"]], na.rm = TRUE)
  state$rss_tree_peak_kb <- max(state$rss_tree_peak_kb, rss[["tree_kb"]], na.rm = TRUE)
  invisible(rss)
}

.bench_size <- function(value) {
  list(
    object_bytes = as.numeric(utils::object.size(value)),
    serialized_bytes = length(serialize(value, NULL, version = 3L)),
    deduplicated_bytes = if (requireNamespace("lobstr", quietly = TRUE)) {
      as.numeric(lobstr::obj_size(value))
    } else NA_real_
  )
}

.bench_fingerprint <- function(value) {
  digest::digest(value, algo = "xxhash64", serialize = TRUE)
}

.bench_state <- function() {
  state <- new.env(parent = emptyenv())
  state$weather_elapsed <- NA_real_
  state$weather_result <- NULL
  state$weather_raw_count <- NA_integer_
  state$weather_raw_sizes <- numeric(0)
  state$pipeline_rows <- list()
  state$pipeline_index <- 0L
  state$expected_keys <- character(0)
  state$pipeline_elapsed <- numeric(0)
  state$design_matrix_elapsed <- NA_real_
  state$prediction_elapsed <- NA_real_
  state$factor_loading_elapsed <- NA_real_
  state$join_elapsed <- NA_real_
  state$cache_calls <- NA_integer_
  state$cache_hits <- NA_integer_
  state$cache_misses <- NA_integer_
  state$cache_last_hit <- FALSE
  state$rss_parent_peak_kb <- 0
  state$rss_tree_peak_kb <- 0
  state$memory_profile <- data.frame()
  state
}

.bench_install_traces <- function() {
  trace_specs <- list(
    list(name = "model.matrix.fixest", where = asNamespace("fixest"), slot = "design_matrix_elapsed"),
    list(name = "predict_outcome", where = asNamespace("wiseapp"), slot = "prediction_elapsed"),
    list(name = "predict_rif", where = asNamespace("wiseapp"), slot = "prediction_elapsed"),
    list(name = "compute_factor_loading", where = asNamespace("wiseapp"), slot = "factor_loading_elapsed"),
    list(name = "join_weather_survey_cached", where = asNamespace("wiseapp"), slot = "join_elapsed"),
    list(name = ".wx_cache_load", where = asNamespace("wiseapp"), slot = "cache")
  )
  installed <- list()
  for (spec in trace_specs) {
    entry <- if (identical(spec$slot, "cache")) quote({
      .s <- get(".wiseapp_bench_trace_state", envir = .GlobalEnv)
      .s$cache_last_hit <- exists("path", inherits = FALSE) && file.exists(path)
    }) else substitute({
      .s <- get(".wiseapp_bench_trace_state", envir = .GlobalEnv)
      .s[[slot]] <- (if (is.finite(.s[[slot]])) .s[[slot]] else 0) -
        proc.time()[["elapsed"]]
    }, list(slot = spec$slot))
    exit <- if (identical(spec$slot, "cache")) quote({
      .s <- get(".wiseapp_bench_trace_state", envir = .GlobalEnv)
      .s$cache_calls <- .s$cache_calls + 1L
      if (isTRUE(.s$cache_last_hit)) .s$cache_hits <- .s$cache_hits + 1L
      else .s$cache_misses <- .s$cache_misses + 1L
    }) else substitute({
      .s <- get(".wiseapp_bench_trace_state", envir = .GlobalEnv)
      .s[[slot]] <- .s[[slot]] + proc.time()[["elapsed"]]
    }, list(slot = spec$slot))
    ok <- tryCatch({
      trace(spec$name, tracer = entry, exit = exit, where = spec$where,
            print = FALSE)
      TRUE
    }, error = function(e) {
      warning("Could not install benchmark trace for ", spec$name, ": ",
              conditionMessage(e), call. = FALSE)
      FALSE
    })
    if (ok) {
      installed[[length(installed) + 1L]] <- spec
      state <- get(".wiseapp_bench_trace_state", envir = .GlobalEnv)
      if (identical(spec$slot, "cache")) {
        state$cache_calls <- 0L
        state$cache_hits <- 0L
        state$cache_misses <- 0L
      } else {
        state[[spec$slot]] <- 0
      }
    }
  }
  installed
}

.bench_remove_traces <- function(installed) {
  for (spec in installed) {
    try(untrace(spec$name, where = spec$where), silent = TRUE)
  }
  if (exists(".wiseapp_bench_trace_state", envir = .GlobalEnv, inherits = FALSE)) {
    rm(".wiseapp_bench_trace_state", envir = .GlobalEnv)
  }
  invisible(NULL)
}

.bench_cache_dir <- function(config) {
  file.path(config$output_dir, "weather-cache", "weather", "v1")
}

.bench_prepare_cache <- function(config, state, cache_state) {
  cache_dir <- .bench_cache_dir(config)
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (identical(cache_state, "cold")) {
    unlink(list.files(cache_dir, full.names = TRUE, recursive = TRUE),
           recursive = TRUE, force = TRUE)
  }
  state$cache_files_before <- length(list.files(cache_dir, pattern = "\\.parquet$",
                                                recursive = TRUE))
  invisible(cache_dir)
}

.bench_case_workload <- function(label, config) {
  switch(label,
    historical = list(ssps = character(0), fp_list = list()),
    one_ssp_one_period = list(
      ssps = config$ssps[1L],
      fp_list = list(c(
        paste0(config$periods[[1L]][1L], "-01-01"),
        paste0(config$periods[[1L]][2L], "-12-31")
      ))
    ),
    two_ssps_two_periods = list(
      ssps = config$ssps[seq_len(min(2L, length(config$ssps)))],
      fp_list = lapply(
        config$periods[seq_len(min(2L, length(config$periods)))],
        function(period) c(
          paste0(period[1L], "-01-01"),
          paste0(period[2L], "-12-31")
        )
      )
    ),
    three_ssps_three_periods = list(
      ssps = config$ssps[seq_len(min(3L, length(config$ssps)))],
      fp_list = lapply(
        config$periods[seq_len(min(3L, length(config$periods)))],
        function(period) c(
          paste0(period[1L], "-01-01"),
          paste0(period[2L], "-12-31")
        )
      )
    ),
    stop("Unknown benchmark workload: ", label, call. = FALSE)
  )
}

.bench_case_inputs <- function(input, model_label, workload, config) {
  models <- input$models
  mf <- models[[model_label]] %||% models[["default"]]
  if (is.null(mf)) stop("No model '", model_label, "' is available in the input snapshot.",
                        call. = FALSE)
  run_cfg <- .bench_case_workload(workload, config)
  list(
    sw = input$sw,
    so = input$so,
    svy = input$svy,
    ss = input$ss,
    mf = mf,
    cp = input$cp,
    fp_list = run_cfg$fp_list,
    ssps = run_cfg$ssps,
    residuals = if (identical(mf$engine, "rif")) "none" else "original",
    skip_coef_draws = !isTRUE(config$include_coef_uncertainty),
    sim_dates = input$sim_dates,
    perturbation_method = if (length(run_cfg$ssps)) {
      build_perturbation_method(input$sw)
    } else NULL,
    stored_breaks = input$stored_breaks,
    fit_multi = if (identical(mf$engine, "rif")) mf$fit3 else NULL,
    taus = mf$taus,
    weather_cols = mf$weather_terms %||% input$sw$name,
    payload_mode = config$payload_mode,
    weather_storage = config$weather_storage,
    weather_store_root = config$output_dir,
    weather_collect = config$weather_collect,
    weather_threads = config$weather_threads,
    join_cache = config$join_cache,
    key_workers = config$key_workers,
    direct_rif_predictions = config$direct_rif_predictions
  )
}

.bench_run_aggregation <- function(result, input, case, config) {
  rows <- list()
  pipes <- .bench_aggregation_pipelines(result)
  aggregation_metadata <- .bench_aggregation_metadata(pipes, case$residuals)
  for (method_label in config$aggregation) {
    pov_line <- if (method_label %in% c("headcount_ratio", "gap", "fgt2")) 3 else NULL
    started <- proc.time()[["elapsed"]]
    err <- NULL
    n_years <- NA_integer_
    tryCatch({
      out <- .bench_aggregate_pipelines(
        pipelines = pipes,
        method = method_label,
        weighted = TRUE,
        pov_line = pov_line,
        residuals = case$residuals,
        is_log = identical(input$so$transform, "log"),
        skip_coef = isTRUE(case$skip_coef_draws),
        seed = config$seed
      )
      n_years <- sum(lengths(out))
    }, error = function(e) err <<- conditionMessage(e))
    rows[[length(rows) + 1L]] <- data.frame(
      country = case$country,
      model = case$model,
      workload = case$workload,
      payload_mode = case$payload_mode,
      weather_storage = config$weather_storage,
      uncertainty = if (case$skip_coef_draws) "disabled" else "enabled",
      cache = case$cache,
      repetition = case$repetition,
      method = method_label,
      residuals_requested = aggregation_metadata$residuals_requested,
      residuals_effective = aggregation_metadata$residuals_effective,
      shared_context_available = aggregation_metadata$shared_context_available,
      elapsed_seconds = proc.time()[["elapsed"]] - started,
      n_pipelines = length(pipes),
      n_years = n_years,
      status = if (is.null(err)) "ok" else "error",
      error = err %||% "",
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

.bench_run_case <- function(input, country, model_label, workload, cache_state,
                             uncertainty, repetition, config, traces) {
  state <- .bench_state()
  .bench_initialize_trace_state(state, traces)
  assign(".wiseapp_bench_trace_state", state, envir = .GlobalEnv)
  cache_dir <- .bench_prepare_cache(config, state, cache_state)
  case_config <- config
  old_memory_profile <- Sys.getenv("WISEAPP_MEMORY_PROFILE", unset = NA_character_)
  Sys.setenv(WISEAPP_MEMORY_PROFILE = "1")
  withr::defer(
    if (is.na(old_memory_profile)) Sys.unsetenv("WISEAPP_MEMORY_PROFILE")
    else Sys.setenv(WISEAPP_MEMORY_PROFILE = old_memory_profile),
    envir = environment()
  )
  case_config$include_coef_uncertainty <- identical(uncertainty, "enabled")
  workload_cfg <- .bench_case_workload(workload, case_config)
  case <- list(
    country = country, model = model_label, workload = workload,
    payload_mode = case_config$payload_mode,
    cache = cache_state, repetition = repetition,
    residuals = if (identical(input$models[[model_label]]$engine, "rif")) "none" else "original"
  )
  args <- .bench_case_inputs(input, model_label, workload, case_config)
  case$skip_coef_draws <- isTRUE(args$skip_coef_draws)
  state$expected_keys <- character(0)
  state$cache_files_before <- length(list.files(cache_dir, pattern = "\\.parquet$",
                                                recursive = TRUE))
  started <- proc.time()[["elapsed"]]
  result <- NULL
  error_text <- NULL

  weather_fn <- function(...) {
    t0 <- proc.time()[["elapsed"]]
    value <- get_weather(...)
    state$weather_elapsed <- proc.time()[["elapsed"]] - t0
    state$expected_keys <- c("historical", setdiff(names(value), "historical"))
    state$weather_raw_count <- length(value)
    state$weather_raw_sizes <- vapply(value, function(x) {
      if (is.null(x)) return(NA_real_)
      as.numeric(utils::object.size(x))
    }, numeric(1))
    .bench_sample_rss(state)
    value
  }

  pipeline_fn <- function(...) {
    state$pipeline_index <- state$pipeline_index + 1L
    key <- if (state$pipeline_index <= length(state$expected_keys)) {
      state$expected_keys[[state$pipeline_index]]
    } else paste0("key_", state$pipeline_index)
    t0 <- proc.time()[["elapsed"]]
    value <- tryCatch(run_sim_pipeline(...), error = function(e) {
      attr(e, "wise_bench_key") <- key
      stop(e)
    })
    elapsed <- proc.time()[["elapsed"]] - t0
    state$pipeline_elapsed <- c(state$pipeline_elapsed, elapsed)
    state$pipeline_rows[[length(state$pipeline_rows) + 1L]] <- data.frame(
      country = country,
      model = model_label,
      workload = workload,
      payload_mode = case_config$payload_mode,
      weather_storage = case_config$weather_storage,
      cache = cache_state,
      repetition = repetition,
      key = key,
      elapsed_seconds = elapsed,
      n_y_point = if (is.null(value)) NA_integer_ else length(value$y_point),
      f_loading_rows = if (is.null(value$F_loading)) NA_integer_ else nrow(value$F_loading),
      f_loading_cols = if (is.null(value$F_loading)) NA_integer_ else ncol(value$F_loading),
      weather_raw_bytes = if (is.null(value$weather_raw)) NA_real_ else
        as.numeric(utils::object.size(value$weather_raw)),
      pipeline_object_bytes = if (is.null(value)) NA_real_ else
        as.numeric(utils::object.size(value)),
      stringsAsFactors = FALSE
    )
    .bench_sample_rss(state)
    value
  }

  call_args <- c(
    args,
    list(
      notify_fn = function(...) invisible(NULL),
      progress_fn = function(...) invisible(NULL),
      weather_fn = weather_fn,
      pipeline_fn = pipeline_fn
    )
  )
  evidence_class <- if (identical(config$fixture_mode, "smoke")) {
    "smoke_only_not_production_evidence"
  } else {
    "production_path_read_only"
  }
  result <- if (identical(config$fixture_mode, "smoke")) {
    value <- .bench_small_step2_result(input, model_label, workload)
    value <- .bench_attach_runtime_metadata(value, args, evidence_class)
    .bench_assert_runtime_options(value, args)
    .bench_mark_traces_unavailable(state)
    value
  } else tryCatch(
    .bench_execute_step2(call_args, evidence_class),
    error = function(e) {
      error_text <<- conditionMessage(e)
      NULL
    }
  )
  if (!is.null(result) && !is.null(attr(result, "memory_profile"))) {
    state$memory_profile <- attr(result, "memory_profile")
    memory_path <- file.path(
      config$output_dir,
      sprintf("memory_%s_%s_%s_%s_%d.csv", country, model_label,
              workload, cache_state, repetition)
    )
    write.csv(state$memory_profile, memory_path, row.names = FALSE)
  }
  elapsed_total <- proc.time()[["elapsed"]] - started
  .bench_sample_rss(state)

  result_size <- if (is.null(result)) {
    list(object_bytes = NA_real_, serialized_bytes = NA_real_, deduplicated_bytes = NA_real_)
  } else .bench_size(result)
  result_fingerprint <- if (is.null(result)) NA_character_ else
    .bench_fingerprint(result)
  aggregation <- if (is.null(result)) data.frame() else
    .bench_run_aggregation(result, input, c(case, args), case_config)

  state$cache_files_after <- length(list.files(cache_dir, pattern = "\\.parquet$",
                                               recursive = TRUE))
  pipeline_sum <- sum(state$pipeline_elapsed, na.rm = TRUE)
  assembly <- max(0, elapsed_total - (state$weather_elapsed %||% 0) - pipeline_sum)
  summary <- data.frame(
    country = country,
    model = model_label,
    workload = workload,
    evidence_class = evidence_class,
    runtime_options_executed = result$benchmark_metadata$runtime_options_executed %||%
      NA,
    payload_mode = case_config$payload_mode,
    weather_storage = case_config$weather_storage,
    weather_collect = case_config$weather_collect,
    weather_threads = case_config$weather_threads,
    join_cache = case_config$join_cache,
    key_workers = case_config$key_workers,
    direct_rif_predictions = case_config$direct_rif_predictions,
    uncertainty = if (isTRUE(args$skip_coef_draws)) "disabled" else "enabled",
    cache = cache_state,
    repetition = repetition,
    seed = case_config$seed,
    status = if (is.null(result)) "error" else "ok",
    error = error_text %||% "",
    elapsed_seconds = elapsed_total,
    result_elapsed_seconds = if (is.null(result)) NA_real_ else result$t_elapsed,
    weather_seconds = state$weather_elapsed,
    pipeline_sum_seconds = pipeline_sum,
    pipeline_median_seconds = if (length(state$pipeline_elapsed))
      stats::median(state$pipeline_elapsed) else NA_real_,
    pipeline_p90_seconds = if (length(state$pipeline_elapsed))
      as.numeric(stats::quantile(state$pipeline_elapsed, 0.9, names = FALSE)) else NA_real_,
    result_assembly_seconds = assembly,
    design_matrix_seconds = state$design_matrix_elapsed,
    prediction_seconds = state$prediction_elapsed,
    factor_loading_seconds = state$factor_loading_elapsed,
    survey_weather_join_seconds = state$join_elapsed,
    parent_peak_rss_kb_sampled = state$rss_parent_peak_kb,
    process_tree_peak_rss_kb_sampled = state$rss_tree_peak_kb,
    result_object_bytes = result_size$object_bytes,
    result_serialized_bytes = result_size$serialized_bytes,
    result_deduplicated_bytes = result_size$deduplicated_bytes,
    result_fingerprint = result_fingerprint,
    weather_raw_count = state$weather_raw_count,
    weather_raw_bytes_total = sum(state$weather_raw_sizes, na.rm = TRUE),
    weather_cache_files_before = state$cache_files_before,
    weather_cache_files_after = state$cache_files_after,
    weather_cache_calls = state$cache_calls,
    weather_cache_hits = state$cache_hits,
    weather_cache_misses = state$cache_misses,
    n_keys_requested = if (is.null(result)) length(state$expected_keys) else result$n_keys,
    n_keys_succeeded = if (is.null(result)) NA_integer_ else result$n_keys_ok,
    n_keys_failed = if (is.null(result)) NA_integer_ else length(result$failures %||% list()),
    step3_policy_seconds = NA_real_,
    step3_decomposition_seconds = NA_real_,
    stringsAsFactors = FALSE
  )
  per_key <- if (length(state$pipeline_rows)) do.call(rbind, state$pipeline_rows) else data.frame()
  list(summary = summary, per_key = per_key, aggregation = aggregation,
       result = result)
}

.bench_write_checkpoint <- function(summary_rows, per_key_rows, aggregation_rows,
                                     step3_rows, output_dir) {
  summary_df <- if (length(summary_rows)) do.call(rbind, summary_rows) else data.frame()
  per_key_df <- if (length(per_key_rows)) do.call(rbind, per_key_rows) else data.frame()
  aggregation_df <- if (length(aggregation_rows)) {
    do.call(rbind, aggregation_rows)
  } else data.frame()
  write.csv(summary_df, file.path(output_dir, "step2_summary.csv"), row.names = FALSE)
  write.csv(per_key_df, file.path(output_dir, "step2_per_key.csv"), row.names = FALSE)
  write.csv(aggregation_df, file.path(output_dir, "step2_aggregation.csv"), row.names = FALSE)
  if (length(step3_rows)) {
    write.csv(do.call(rbind, step3_rows),
              file.path(output_dir, "step3_summary.csv"), row.names = FALSE)
  }
  invisible(NULL)
}

# -----------------------------------------------------------------------------
# Run the workload matrix and write reports
# -----------------------------------------------------------------------------

workloads <- cfg$workloads
all_summary <- list()
all_per_key <- list()
all_aggregation <- list()
all_step3 <- list()
trace_state <- .bench_state()
assign(".wiseapp_bench_trace_state", trace_state, envir = .GlobalEnv)
traces <- .bench_install_traces()

for (country in names(inputs_by_country)) {
  input <- inputs_by_country[[country]]
  available_models <- names(input$models)
  models <- intersect(cfg$models, available_models)
  if (!length(models)) {
    warning("No requested models available for ", country, "; skipping.", call. = FALSE)
    next
  }
  for (model_label in models) {
    for (workload in workloads) {
      workload_cfg <- .bench_case_workload(workload, cfg)
      if (identical(workload, "one_ssp_one_period") &&
          (!length(workload_cfg$ssps) || !length(workload_cfg$fp_list))) next
      if (identical(workload, "three_ssps_three_periods") &&
          (length(workload_cfg$ssps) < 3L || length(workload_cfg$fp_list) < 3L)) next
      if (identical(workload, "two_ssps_two_periods") &&
          (length(workload_cfg$ssps) < 2L || length(workload_cfg$fp_list) < 2L)) next
      for (uncertainty in cfg$uncertainty_modes) {
        for (cache_state in c("cold", "warm")) {
          for (repetition in seq_len(cfg$repetitions)) {
            message(sprintf(
              "Benchmarking %s / %s / %s / uncertainty=%s / cache=%s / rep=%d",
              country, model_label, workload,
              uncertainty,
              cache_state, repetition
            ))
            run <- .bench_run_case(
              input = input, country = country, model_label = model_label,
              workload = workload, cache_state = cache_state,
              uncertainty = uncertainty, repetition = repetition,
              config = cfg, traces = traces
            )
            all_summary[[length(all_summary) + 1L]] <- run$summary
            if (nrow(run$per_key)) all_per_key[[length(all_per_key) + 1L]] <- run$per_key
            if (nrow(run$aggregation)) all_aggregation[[length(all_aggregation) + 1L]] <- run$aggregation
            if (cfg$include_step3 && !is.null(run$result)) {
              fixtures <- .bench_step3_policy_fixtures(cfg$step3_policies)
              identity <- list(
                country = country, workload = workload,
                payload_mode = cfg$payload_mode,
                weather_storage = cfg$weather_storage,
                weather_collect = cfg$weather_collect,
                weather_threads = cfg$weather_threads,
                join_cache = cfg$join_cache,
                key_workers = cfg$key_workers,
                direct_rif_predictions = cfg$direct_rif_predictions,
                uncertainty = uncertainty, cache = cache_state,
                repetition = repetition, fixture_mode = cfg$fixture_mode,
                evidence_class = run$result$benchmark_metadata$evidence_class
              )
              for (policy_label in names(fixtures)) {
                message(sprintf(
                  "Benchmarking Step 3 %s / %s / %s / policy=%s",
                  country, model_label, workload, policy_label
                ))
                all_step3[[length(all_step3) + 1L]] <- .bench_run_step3(
                  baseline_result = run$result,
                  input = input,
                  model_label = model_label,
                  policy_label = policy_label,
                  policy_fixture = fixtures[[policy_label]],
                  identity = identity,
                  config = cfg,
                  size_fn = .bench_size,
                  rss_state_fn = .bench_state,
                  rss_sample_fn = .bench_sample_rss
                )
              }
            }
            .bench_write_checkpoint(
              all_summary, all_per_key, all_aggregation, all_step3,
              cfg$output_dir
            )
            gc(verbose = FALSE)
          }
        }
      }
    }
  }
}

summary_df <- if (length(all_summary)) do.call(rbind, all_summary) else data.frame()
per_key_df <- if (length(all_per_key)) do.call(rbind, all_per_key) else data.frame()
aggregation_df <- if (length(all_aggregation)) do.call(rbind, all_aggregation) else data.frame()
step3_df <- if (length(all_step3)) do.call(rbind, all_step3) else data.frame()

write.csv(summary_df, file.path(cfg$output_dir, "step2_summary.csv"), row.names = FALSE)
write.csv(per_key_df, file.path(cfg$output_dir, "step2_per_key.csv"), row.names = FALSE)
write.csv(aggregation_df, file.path(cfg$output_dir, "step2_aggregation.csv"), row.names = FALSE)
if (cfg$include_step3) {
  write.csv(step3_df, file.path(cfg$output_dir, "step3_summary.csv"), row.names = FALSE)
}

.bench_git_revision <- tryCatch(
  system2("git", c("-C", .bench_repo_root, "rev-parse", "HEAD"), stdout = TRUE),
  error = function(e) "unknown"
)
.bench_package_versions <- setNames(
  lapply(c("R", "shiny", "duckdb", "dplyr", "fixest", "future", "future.apply"),
         function(pkg) {
           if (identical(pkg, "R")) return(R.version.string)
           if (!requireNamespace(pkg, quietly = TRUE)) return(NA_character_)
           as.character(utils::packageVersion(pkg))
         }),
  c("R", "shiny", "duckdb", "dplyr", "fixest", "future", "future.apply")
)

report_metadata <- list(
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  git_revision = trimws(.bench_git_revision[[1L]] %||% "unknown"),
  repository = .bench_repo_root,
  configuration = cfg,
  machine = list(
    sys_info = as.list(Sys.info()),
    detected_cores = parallel::detectCores(logical = TRUE),
    physical_cores = parallel::detectCores(logical = FALSE),
    rss_method = "R-side ps samples at stage boundaries; use /usr/bin/time -l for peak"
  ),
  package_versions = .bench_package_versions,
  input_summary = lapply(inputs_by_country, function(input) input$metadata %||% list()),
  notes = list(
    process_tree_rss = "The sampled values are diagnostic only. The external launcher records the complete R process tree peak.",
    step3 = if (cfg$include_step3) paste(
      "Enabled with deterministic policy fixtures:",
      paste(cfg$step3_policies, collapse = ", ")
    ) else "Disabled; set WISEAPP_STEP2_INCLUDE_STEP3=1 to opt in.",
    evidence_class = if (identical(cfg$fixture_mode, "smoke")) {
      "smoke_only_not_production_evidence"
    } else {
      "production_path_read_only"
    },
    fixture = if (identical(cfg$fixture_mode, "smoke")) {
      "Fabricated in-memory smoke fixture. Never use its metrics as production-path evidence."
    } else {
      "Production or supplied snapshot inputs are read-only; artifacts are written only to the benchmark output directory."
    },
    snapshot_contract = "Snapshot entries contain sw, so, svy, ss, cp, sim_dates, and mf or models; optional stored_breaks and metadata are accepted."
  )
)
jsonlite::write_json(
  report_metadata,
  file.path(cfg$output_dir, "step2_metadata.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null"
)

.bench_report <- function(summary_df, aggregation_df, metadata, path) {
  lines <- c(
    "# Step 2 Phase 1 Benchmark Report",
    "",
    paste0("Generated: ", metadata$generated_at_utc),
    paste0("Git revision: `", metadata$git_revision, "`"),
    "",
    "## Scope",
    "",
    "This report is produced by `dev/bench_step2.R`. The serial `fct_run_simulation()` path remains the reference. RSS values in the CSV are sampled diagnostics; complete process-tree peak RSS must be taken from `dev/run_step2_benchmark.sh` or an equivalent deployment measurement.",
    "",
    "## Results",
    ""
  )
  if (!nrow(summary_df)) {
    lines <- c(lines, "No benchmark cases completed.", "")
  } else {
    ok <- summary_df[summary_df$status == "ok", , drop = FALSE]
    if (!nrow(ok)) {
      lines <- c(lines, "No benchmark cases completed successfully.", "")
    } else {
       key <- interaction(ok$country, ok$model, ok$workload, ok$payload_mode,
                          ok$uncertainty, ok$cache, drop = TRUE, lex.order = TRUE)
      med <- aggregate(ok$elapsed_seconds, list(case = key), median, na.rm = TRUE)
      names(med)[2L] <- "median_elapsed_seconds"
      lines <- c(lines, "| Case | Median elapsed (s) |", "|---|---:|",
                 sprintf("| %s | %.3f |", med$case, med$median_elapsed_seconds), "")
      rss <- max(ok$process_tree_peak_rss_kb_sampled, na.rm = TRUE)
      if (is.finite(rss)) lines <- c(lines, sprintf(
        "Sampled process-tree RSS maximum: %.1f MB. This is not the production memory gate.",
        rss / 1024
      ), "")
    }
  }
  lines <- c(lines,
    "## Files",
    "",
    "- `step2_summary.csv`: one row per workload, payload mode, model, cache state, uncertainty setting, and repetition.",
    "- `step2_per_key.csv`: per-key timing, row counts, factor-loading dimensions, and retained weather sizes.",
    "- `step2_aggregation.csv`: display aggregation elapsed time by method.",
    "- `step2_metadata.json`: workload, environment, package, and input metadata.",
    if (metadata$configuration$include_step3) "- `step3_summary.csv`: Step 3 stage runtimes, sizes, sampled RSS, workload identity, evidence classification, and SHA-256 fingerprint." else NULL,
    "- `external_process_metrics.csv`: launcher-level maximum RSS from `/usr/bin/time -l` when supported.",
    "",
    "## Decision Inputs",
    "",
    "Use medians and spread across repetitions, comparing payload modes within the same workload. Rank weather loading, per-key pipelines, result assembly, retained payloads, serialization, and aggregation using the CSV outputs. Do not use `gc()` deltas or sampled RSS as the deployment memory gate.",
    ""
  )
  writeLines(lines, path)
}

.bench_report(summary_df, aggregation_df, report_metadata,
              file.path(cfg$output_dir, "step2_report.md"))

.bench_remove_traces(traces)

message("Step 2 Phase 1 benchmark complete. Outputs: ", cfg$output_dir)
