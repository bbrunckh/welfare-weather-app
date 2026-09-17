# Local W3-A/W3-B/W3-C weather benchmark.
#
# This benchmark intentionally calls get_weather() directly.  It uses both LKA
# household waves and compares the current path with opt-in candidate plans:
# current, shared_hist, and shared_period.  Candidate plans are only active
# when WISEAPP_WEATHER_PROFILE=1 and WISEAPP_WEATHER_W3_PLAN is set.

options(golem.app.prod = FALSE)
args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
script_path <- if (length(file_arg)) sub("^--file=", "", file_arg[[1L]]) else "dev/bench_w3_weather.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), mustWork = TRUE)
pkgload::load_all(repo_root, quiet = TRUE)

env <- function(name, default = "") Sys.getenv(name, unset = default)
env_int <- function(name, default) {
  value <- suppressWarnings(as.integer(env(name, as.character(default))))
  if (length(value) != 1L || is.na(value)) default else value
}

backend <- env("WISEAPP_W3_BACKEND", "local")
data_path <- normalizePath(path.expand(env(
  "WISEAPP_DATA_PATH", "~/Library/CloudStorage/OneDrive-WBG/wiseapp - Documents"
)), mustWork = FALSE)
country <- env("WISEAPP_W3_COUNTRY", "LKA")
survey_years <- as.integer(strsplit(env(
  "WISEAPP_W3_SURVEY_YEARS", if (country == "LKA") "2012,2016" else "2009,2020"
), ",", fixed = TRUE)[[1L]])
output_dir <- normalizePath(path.expand(env(
  "WISEAPP_W3_OUTPUT_DIR", file.path(repo_root, "dev", "outputs", "w3-weather")
)), mustWork = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
repetitions <- max(1L, env_int("WISEAPP_W3_REPETITIONS", 2L))

cp <- if (identical(backend, "databricks")) {
  build_connection_params("databricks")
} else {
  build_connection_params("local", path = data_path)
}
if (!validate_connection_params(cp)) {
  stop("Invalid ", backend, " connection parameters; check configured environment variables.", call. = FALSE)
}
metadata <- load_overview_metadata(cp)
available <- list_available_files(cp)
surveys <- build_survey_fnames(metadata$survey_list, "hh", cp)
surveys <- filter_surveys_to_available(surveys, available)
surveys <- surveys[surveys$code == country & surveys$year %in% survey_years, , drop = FALSE]
if (nrow(surveys) != length(survey_years)) stop("Expected requested household surveys.", call. = FALSE)

survey_data <- load_data(surveys$fname, cp, collect = TRUE, unify_schemas = TRUE)
survey_data <- add_time_columns(survey_data)
dates <- extract_survey_dates(survey_data)
weather_info <- get_weather_vars(metadata$variable_list)
weather_names <- intersect(c("t", "spei6"), weather_info$name)
if (!length(weather_names)) stop("No benchmark weather variables available.", call. = FALSE)
selected_weather <- do.call(rbind, lapply(weather_names, function(name) {
  units <- weather_info$units[match(name, weather_info$name)]
  data.frame(
    name = name, ref_start = 1L, ref_end = 3L,
    temporalAgg = temporal_agg_default(units), cont_binned = "Continuous",
    binning_method = NA_character_, num_bins = NA_integer_,
    transformation = "None", stringsAsFactors = FALSE
  )
}))

periods <- list(
  one_ssp_one_period = list(ssps = "ssp2_4_5", periods = list(c("2025-01-01", "2035-12-31"))),
  two_ssps_two_periods = list(
    ssps = c("ssp2_4_5", "ssp3_7_0"),
    periods = list(c("2025-01-01", "2035-12-31"), c("2040-01-01", "2060-12-31"))
  ),
  boundary_overlap = list(
    ssps = "ssp2_4_5",
    periods = list(c("2025-01-01", "2035-06-30"), c("2030-01-01", "2040-06-30"))
  )
)
cases <- intersect(
  c("historical", names(periods)),
  strsplit(env("WISEAPP_W3_CASES", "historical,one_ssp_one_period,two_ssps_two_periods,boundary_overlap"), ",", fixed = TRUE)[[1L]]
)
plans <- intersect(
  c("current", "shared_hist", "shared_period"),
  strsplit(env("WISEAPP_W3_PLANS", "current,shared_hist,shared_period"), ",", fixed = TRUE)[[1L]]
)
if (!length(plans)) stop("WISEAPP_W3_PLANS selected no valid plans.", call. = FALSE)

rss <- function() .wx_process_tree_rss_bytes()
hash_result <- function(value) {
  digest::digest(lapply(value, function(x) {
    if (!is.data.frame(x)) return(x)
    x
  }), algo = "sha256")
}
cleanup_tables <- function() {
  tables <- tryCatch(DBI::dbListTables(.duck_con()), error = function(e) character())
  sort(tables[grepl("^lw_", tables)])
}

summary_rows <- list()
profile_rows <- list()
baseline_hash <- list()
for (workload in cases) {
  spec <- if (identical(workload, "historical")) {
    list(ssps = NULL, periods = NULL, perturbation = NULL)
  } else {
    cfg <- periods[[workload]]
    list(ssps = cfg$ssps, periods = cfg$periods,
         perturbation = setNames(rep("additive", length(weather_names)), weather_names))
  }
  for (plan in plans) {
    for (rep in seq_len(repetitions)) {
      Sys.setenv(
        WISEAPP_WEATHER_PROFILE = "1",
        WISEAPP_WEATHER_W3_PLAN = plan,
        WISEAPP_WEATHER_CACHE_DISABLE = "1"
      )
      gc(verbose = FALSE)
      before <- rss()
      started <- proc.time()[["elapsed"]]
      result <- tryCatch(
        get_weather(
          survey_data = survey_data,
          selected_surveys = surveys,
          selected_weather = selected_weather,
          dates = dates,
          connection_params = cp,
          ssp = spec$ssps,
          future_period = spec$periods,
          perturbation_method = spec$perturbation,
          weather_collect = "fast",
          weather_threads = "1"
        ),
        error = function(e) e
      )
      elapsed <- proc.time()[["elapsed"]] - started
      after <- rss()
      cleanup_started <- proc.time()[["elapsed"]]
      leftovers <- cleanup_tables()
      cleanup_elapsed <- proc.time()[["elapsed"]] - cleanup_started
      ok <- is.list(result) && !inherits(result, "error")
      result_hash <- if (ok) hash_result(result) else NA_character_
      if (ok && is.null(baseline_hash[[workload]])) baseline_hash[[workload]] <- result_hash
      summary_rows[[length(summary_rows) + 1L]] <- data.frame(
        workload = workload, plan = plan, repetition = rep,
        status = if (ok) "ok" else "error",
        elapsed_seconds = elapsed, rss_before_bytes = before,
        rss_after_bytes = after, result_hash = result_hash,
        cleanup_elapsed_seconds = cleanup_elapsed,
        parity_reference_hash = baseline_hash[[workload]] %||% NA_character_,
        parity = if (ok) identical(result_hash, baseline_hash[[workload]]) else FALSE,
        n_members = if (ok) length(result) else NA_integer_,
        leftover_lw_tables = paste(leftovers, collapse = ";"),
        error = if (ok) "" else conditionMessage(result),
        stringsAsFactors = FALSE
      )
      if (ok && !is.null(attr(result, "weather_profile"))) {
        profile <- attr(result, "weather_profile")
        profile$workload <- workload
        profile$plan <- plan
        profile$repetition <- rep
        profile_rows[[length(profile_rows) + 1L]] <- profile
      }
      message(sprintf("%s / %s / rep %d: %.2fs, %s", workload, plan, rep,
                      elapsed, if (ok) result_hash else conditionMessage(result)))
    }
  }
}

summary <- do.call(rbind, summary_rows)
profiles <- if (length(profile_rows)) do.call(rbind, profile_rows) else data.frame()
write.csv(summary, file.path(output_dir, "w3_summary.csv"), row.names = FALSE)
write.csv(profiles, file.path(output_dir, "w3_profile.csv"), row.names = FALSE)
jsonlite::write_json(list(
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  data_path = if (identical(backend, "local")) data_path else NA_character_,
  backend = backend, country = country, surveys = surveys,
  weather_names = weather_names, periods = periods,
  repetitions = repetitions,
  notes = "Local weather-only W3 benchmark; external /usr/bin/time -l is the peak RSS gate."
), file.path(output_dir, "w3_metadata.json"), auto_unbox = TRUE, pretty = TRUE)
message("W3 benchmark complete: ", output_dir)
