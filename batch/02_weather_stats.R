# =============================================================================
# batch/02_weather_stats.R
#
# Weather summary statistics for all countries.
#
# Outputs:
#   OUT_DIR/weather_stats/weather_stats.csv
#
# All user inputs are in SECTION 1. 
#
# =============================================================================

# Load helpers
pkgload::load_all(quiet = TRUE)
invisible(lapply(list.files("batch/R", pattern = "\\.R$", full.names = TRUE), source))

# =============================================================================
# SECTION 1 — CONFIGURATION
# =============================================================================

# ---- Data source ------------------------------------------------------------
CONNECTION_TYPE <- "local"
DATA_DIR        <- Sys.getenv("WISEAPP_DATA_PATH")
OUT_DIR         <- Sys.getenv("WISEAPP_RESULTS_PATH")

# ---- Unit of analysis -------------------------------------------------------
UNIT <- "hh"   # "hh", "ind", or "firm"

# ---- Country filter ---------------------------------------------------------
COUNTRY_FILTER <- c(
  "BEN", "BFA", "BRA", "CIV", "COL", "GMB", "GNB", "GTM", "IND", "IRN", "LKA",
  "MLI", "MRT", "MWI", "NER", "SEN", "TCD", "TGO", "TJK", "VNM", "ZMB"
)

# ---- Weather specs ----------------------------------------------------------
# Named list of weather profiles (same format as 03_run_mod1.R WEATHER_SPECS).
# Each profile defines one set of weather variables to load and summarise.
WEATHER_SPECS <- c(
  expand_weather_specs("t", c(1L, 3L, 6L, 12L), transformations = "continuous", var_constructions = c("None"), ref_starts = 1L),
  expand_weather_specs("tn", c(1L, 3L, 6L, 12L), transformations = "continuous", var_constructions = c("None", "Deviation from mean"), ref_starts = 1L),
  expand_weather_specs("tx", c(1L, 3L, 6L, 12L), transformations = "continuous", var_constructions = c("None", "Deviation from mean"), ref_starts = 1L),
  expand_weather_specs("tx35", c(1L, 3L, 6L, 12L), transformations = "continuous", var_constructions = c("None", "Deviation from mean"), ref_starts = 1L),
  expand_weather_specs("tr", c(1L, 3L, 6L, 12L), transformations = "continuous", var_constructions = c("None", "Deviation from mean"), ref_starts = 1L),
  expand_weather_specs("r", c(1L, 3L, 6L, 12L), transformations = "continuous", var_constructions = c("None", "Deviation from mean"), ref_starts = 1L),
  expand_weather_specs("rx5day", c(1L, 3L, 6L, 12L), transformations = "continuous", var_constructions = c("None", "Deviation from mean"), ref_starts = 1L),
  expand_weather_specs("r20", c(1L, 3L, 6L, 12L), transformations = "continuous", var_constructions = c("None", "Deviation from mean"), ref_starts = 1L),
  expand_weather_specs("mrsos", c(1L, 3L, 6L, 12L), transformations = "continuous", var_constructions = c("None", "Deviation from mean"), ref_starts = 1L),
  expand_weather_specs("spei6", c(1L, 3L, 6L, 12L), transformations = "continuous", var_constructions = c("None"), ref_starts = 1L)
)

# ---- Weather defaults -------------------------------------------------------
WEATHER_TRANSFORMATION <- "None"
N_BINS                 <- 5L
BINNING_METHOD         <- "Equal frequency"
CUSTOM_BREAKS          <- NULL
POLYNOMIAL             <- character(0)
WEATHER_AGG_OVERRIDE   <- NULL

# ---- Climate reference period -----------------------------------------------
# Long-term reference period for comparing survey-period weather against
# the background climate distribution at the same locations.
CLIMATE_REF_YEARS <- c(1991L, 2020L)

# ---- Output options ---------------------------------------------------------
OVERWRITE_EXISTING <- FALSE

# =============================================================================
# SECTION 2 — SETUP
# =============================================================================

# Output directories
OUT_WEATHER  <- file.path(OUT_DIR, "weather_stats")

for (d in c(OUT_WEATHER))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Connection
connection_params_02 <- if (identical(CONNECTION_TYPE, "databricks")) {
  build_connection_params("databricks")
} else {
  build_connection_params("local", path = DATA_DIR)
}
stopifnot("Invalid connection_params" = validate_connection_params(connection_params_02))
cat(sprintf("Connection: %s\n",
            if (identical(connection_params_02$type, "databricks"))
              "Databricks" else paste0("local (", connection_params_02$path, ")")))

# Metadata
var_info_02    <- load_data("metadata/variable_list.csv", connection_params_02, collect = TRUE)
survey_list_02 <- load_data("metadata/survey_list.csv",   connection_params_02, collect = TRUE)
cpi_ppp_02     <- load_data("metadata/cpi_ppp.csv",       connection_params_02, collect = TRUE)

LEVEL_02 <- switch(UNIT, hh = "hh", ind = "ind", firm = "firm", "hh")
surveys_02 <- build_survey_fnames(survey_list_02, LEVEL_02, connection_params_02)
COUNTRIES_02 <- sort(unique(surveys_02$code))
if (!is.null(COUNTRY_FILTER))
  COUNTRIES_02 <- intersect(COUNTRIES_02, COUNTRY_FILTER)

cat(sprintf("Countries (weather stats): %d (%s)\n", length(COUNTRIES_02),
            paste(COUNTRIES_02, collapse = ", ")))
cat(sprintf("Weather specs: %d (%s)\n\n", length(WEATHER_SPECS),
            paste(names(WEATHER_SPECS), collapse = ", ")))

# ---- Output file path + helpers ---------------------------------------------
out_csv <- file.path(OUT_WEATHER, "weather_stats.csv")

dedup_keys_02 <- c("code", "economy", "survname", "year", "wx_spec",
                    "variable", "ref_period", "temporal_agg", "transformation")

.save_csv_02 <- function(new_df, path) {
  if (is.null(new_df) || nrow(new_df) == 0L) return(invisible(NULL))
  out_df <- new_df
  if (file.exists(path)) {
    existing <- readr::read_csv(path, show_col_types = FALSE)
    out_df   <- dplyr::bind_rows(existing, new_df)
    out_df   <- dplyr::distinct(out_df, dplyr::across(dplyr::any_of(dedup_keys_02)),
                                .keep_all = TRUE)
  }
  col_order <- c(dedup_keys_02, setdiff(names(out_df), dedup_keys_02))
  out_df    <- out_df[order(out_df$code, out_df$wx_spec, out_df$year), col_order]
  readr::write_csv(out_df, path)
  cat(sprintf("  Saved: %s (%d rows)\n", basename(path), nrow(out_df)))
}

# ---- Skip detection ---------------------------------------------------------
# A code×spec pair is "done" only if it has survey rows AND climate ref rows
# (when CLIMATE_REF_YEARS is set). This ensures re-runs backfill climate data.
.done_specs_02 <- character(0)
if (OVERWRITE_EXISTING) {
  for (.f in out_csv) if (file.exists(.f)) file.remove(.f)
  OVERWRITE_EXISTING <- FALSE
} else if (file.exists(out_csv)) {
  .chk <- readr::read_csv(out_csv, show_col_types = FALSE)
  if (nrow(.chk) > 0L) {
    .has_survey <- unique(paste(.chk$code[!is.na(.chk$year)],
                                .chk$wx_spec[!is.na(.chk$year)], sep = "_"))
    if (!is.null(CLIMATE_REF_YEARS)) {
      .has_climate <- unique(paste(.chk$code[is.na(.chk$year)],
                                   .chk$wx_spec[is.na(.chk$year)], sep = "_"))
      .done_specs_02 <- intersect(.has_survey, .has_climate)
    } else {
      .done_specs_02 <- .has_survey
    }
  }
  cat(sprintf("  %d code×spec pair(s) already complete — will skip\n",
              length(.done_specs_02)))
  rm(.chk)
}

# =============================================================================
# SECTION 3 — MAIN LOOP
# =============================================================================

for (code in COUNTRIES_02) {
  cat(sprintf("\n=== %s ===\n", code))

  # Skip entire country if all specs already done (avoids expensive data load)
  country_spec_keys <- paste(code, names(WEATHER_SPECS), sep = "_")
  n_done <- sum(country_spec_keys %in% .done_specs_02)
  if (n_done == length(WEATHER_SPECS)) {
    cat(sprintf("  SKIP — all %d spec(s) already complete\n", n_done))
    next
  } else if (n_done > 0L) {
    cat(sprintf("  %d/%d spec(s) already complete — will skip those\n",
                n_done, length(WEATHER_SPECS)))
  }

  # Build survey file list for this country
  years_by_code <- setNames(
    list(as.character(sort(unique(surveys_02$year[surveys_02$code == code])))),
    code
  )
  ss <- build_selected_surveys(surveys = surveys_02, years_by_code = years_by_code)
  if (nrow(ss) == 0) {
    cat("  SKIP — no surveys found\n")
    next
  }

  # Load and preprocess survey data
  svy_base <- tryCatch({
    df       <- load_data(ss$fname, connection_params_02, collect = TRUE, unify_schemas = TRUE)
    df       <- add_time_columns(df)
    lcu_vars <- get_lcu_vars(df, var_info_02)
    df |>
      assign_data_level() |>
      convert_lcu_to_ppp(cpi_ppp_02, lcu_vars) |>
      bottom_code_welfare(0.28) |>
      apply_policy_derivations()
  }, error = function(e) { message("  load failed: ", conditionMessage(e)); NULL })
  if (is.null(svy_base)) next
  cat(sprintf("  Loaded: %d rows\n", nrow(svy_base)))

  dates <- extract_survey_dates(svy_base)

  # Accumulate plot data across specs: named list keyed by base_var
  country_wx_stats <- list()

  # -- Loop over weather profiles ---------------------------------------------
  for (wx_name in names(WEATHER_SPECS)) {
    spec_key <- paste(code, wx_name, sep = "_")
    if (spec_key %in% .done_specs_02) {
      cat(sprintf("  [%s] SKIP (results exist)\n", wx_name))
      next
    }

    wx_prof <- WEATHER_SPECS[[wx_name]]
    wx_vars <- names(wx_prof)

    # Build spec_inputs
    spec_inputs <- list()
    for (v in wx_vars) {
      vs <- wx_prof[[v]]
      p  <- paste0(v, "_")
      spec_inputs[[paste0(p, "relativePeriod")]]  <- c(vs$ref_start %||% 1L, vs$ref_end)
      spec_inputs[[paste0(p, "temporalAgg")]]      <- vs$temporal_agg %||% weather_agg_for(v, get_weather_vars(var_info_02), WEATHER_AGG_OVERRIDE)
      spec_inputs[[paste0(p, "varConstruction")]]  <- vs$weather_transformation %||% WEATHER_TRANSFORMATION
      spec_inputs[[paste0(p, "contOrBinned")]]     <- if (vs$transformation == "binned") "Binned" else "Continuous"
      spec_inputs[[paste0(p, "numBins")]]          <- vs$n_bins %||% N_BINS
      spec_inputs[[paste0(p, "binningMethod")]]    <- vs$binning_method %||% BINNING_METHOD
      spec_inputs[[paste0(p, "customBreaks")]]     <- vs$custom_breaks %||% CUSTOM_BREAKS[[v]]
      spec_inputs[[paste0(p, "polynomial")]]       <- vs$polynomial %||% POLYNOMIAL
    }

    selected_weather <- tryCatch(
      build_selected_weather(selected_vars = wx_vars,
                             var_info = get_weather_vars(var_info_02),
                             spec_inputs = spec_inputs),
      error = function(e) { message("  weather build failed [", wx_name, "]: ", conditionMessage(e)); NULL }
    )
    if (is.null(selected_weather) || nrow(selected_weather) == 0) next

    cat(sprintf("  Loading weather [%s]...", wx_name))
    weather_data <- tryCatch(
      get_weather(survey_data = svy_base, selected_surveys = ss,
                  selected_weather = selected_weather,
                  dates = dates,
                  connection_params = connection_params_02),
      error = function(e) { message(" get_weather: ", conditionMessage(e)); NULL }
    )
    if (is.null(weather_data)) { cat(" FAIL\n"); next }
    cat(" done\n")

    svy_wx <- merge_survey_weather(svy_base, weather_data[["historical"]])
    if (is.null(svy_wx) || nrow(svy_wx) == 0) {
      cat("  SKIP — weather merge produced 0 rows\n")
      next
    }

    # n_rows_base: total rows BEFORE the weather inner-join, per code-year
    # (pct_missing denominator — some rows may drop out of the join)
    n_rows_base <- with(svy_base, tapply(rep(1L, nrow(svy_base)),
                                         paste(code, year), sum))

    df_wx   <- svy_wx
    vars_wx <- intersect(selected_weather$name[selected_weather$cont_binned == "Continuous"],
                         names(df_wx))
    vars_wx <- vars_wx[vapply(df_wx[vars_wx], is.numeric, logical(1L))]

    # -- Climate reference period ---------------------------------------------
    df_ref <- tryCatch({
      ref_dates   <- build_hist_sim_dates(svy_base, CLIMATE_REF_YEARS)
      ref_weather <- get_weather(
        survey_data       = svy_base,
        selected_surveys  = ss,
        selected_weather  = selected_weather,
        dates             = ref_dates,
        connection_params = connection_params_02
      )
      ref_weather[["historical"]]
    }, error = function(e) {
      message("  climate ref weather failed: ", conditionMessage(e))
      NULL
    })

    # -- Weather summary stats ------------------------------------------------
    tryCatch({
      if (length(vars_wx) > 0) {
        wx_stats <- weighted_weather_stats(
          df               = df_wx,
          vars             = vars_wx,
          selected_weather = selected_weather,
          n_rows_base      = n_rows_base,
          df_ref           = df_ref,
          ref_years        = CLIMATE_REF_YEARS
        )
        if (nrow(wx_stats) > 0) {
          wx_stats$wx_spec <- wx_name
          country_wx_stats[[length(country_wx_stats) + 1L]] <- wx_stats
          cat(sprintf("  Stats: %d rows (%d vars)\n", nrow(wx_stats), length(vars_wx)))
        }
      }
    }, error = function(e) message("  weather stats failed [", wx_name, "]: ", conditionMessage(e)))

    rm(svy_wx, weather_data, df_wx, df_ref)
    gc(verbose = FALSE)
  }

  # -- Flush per-country stats to disk -----------------------------------------
  if (length(country_wx_stats) > 0L) {
    country_df <- dplyr::bind_rows(country_wx_stats)
    country_df <- dplyr::mutate(country_df, year = as.numeric(as.character(year)))
    .save_csv_02(country_df, out_csv)
  }

  # -- Clear country-level objects from memory --------------------------------
  rm(svy_base, dates, country_wx_stats, ss, years_by_code)
  gc(verbose = FALSE)
}

# =============================================================================
# SECTION 4 — SUMMARY
# =============================================================================

if (file.exists(out_csv)) {
  final <- readr::read_csv(out_csv, show_col_types = FALSE)
  cat(sprintf("\nFinal: %s (%d rows, %d cols)\n", out_csv, nrow(final), ncol(final)))
  rm(final)
} else {
  cat("\nNo weather stats accumulated.\n")
}

cat("========== Weather stats complete ==========\n")
