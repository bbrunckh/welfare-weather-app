# =============================================================================
# batch/02_weather_stats.R
#
# Weather summary statistics and distribution plots for all countries.
# Outputs:
#   OUT_DIR/weather_stats/weather_stats.csv
#   OUT_DIR/weather_stats/weather_distributions/{CODE}_{WX_SPEC_NAME}_{VAR}_dist.png
#
# Can be run standalone or sourced from 04_run_all.R (config vars pre-set).
# All user inputs are in SECTION 1. Config vars are guarded so 04_run_all.R
# can pre-set them before sourcing this script.
#
# Usage: source("batch/02_weather_stats.R")
# =============================================================================

# =============================================================================
# SECTION 1 — CONFIGURATION
# =============================================================================

# ---- Data source ------------------------------------------------------------
if (!exists("CONNECTION_TYPE")) CONNECTION_TYPE <- "local"
if (!exists("DATA_DIR"))        DATA_DIR        <- Sys.getenv("WISEAPP_DATA_PATH")
if (!exists("OUT_DIR"))         OUT_DIR         <- "dev/outputs/"

# ---- Unit of analysis -------------------------------------------------------
if (!exists("UNIT")) UNIT <- "hh"   # "hh", "ind", or "firm"

# ---- Country filter ---------------------------------------------------------
if (!exists("COUNTRY_FILTER")) COUNTRY_FILTER <- NULL

# ---- Weather specs ----------------------------------------------------------
# Named list of weather profiles (same format as 03_run_mod1.R WEATHER_SPECS).
# Each profile defines one set of weather variables to load and summarise.
if (!exists("WEATHER_SPECS")) {
  WEATHER_SPECS <- c(
    expand_weather_specs("t", c(1L, 3L, 6L, 12L), c("continuous"), c("None"), ref_starts = 1L),
    expand_weather_specs("r", c(1L, 3L, 6L, 12L), c("continuous"), c("None"), ref_starts = 1L)
  )
}

# ---- Weather defaults -------------------------------------------------------
if (!exists("WEATHER_TRANSFORMATION")) WEATHER_TRANSFORMATION <- "None"
if (!exists("N_BINS"))               N_BINS               <- 5L
if (!exists("BINNING_METHOD"))       BINNING_METHOD       <- "Equal frequency"
if (!exists("CUSTOM_BREAKS"))        CUSTOM_BREAKS        <- NULL
if (!exists("POLYNOMIAL"))           POLYNOMIAL           <- character(0)
if (!exists("WEATHER_AGG_OVERRIDE")) WEATHER_AGG_OVERRIDE <- NULL

# ---- Output options ---------------------------------------------------------
if (!exists("OVERWRITE_EXISTING")) OVERWRITE_EXISTING <- TRUE

# =============================================================================
# SECTION 2 — SETUP
# =============================================================================

if (!exists(".batch_loaded") || !isTRUE(.batch_loaded)) {
  pkgload::load_all(quiet = TRUE)
  invisible(lapply(list.files("batch/R", pattern = "\\.R$", full.names = TRUE), source))
  .batch_loaded <- TRUE
}

# Output directories
OUT_WEATHER  <- file.path(OUT_DIR, "weather_stats")
OUT_WX_DIST  <- file.path(OUT_WEATHER, "weather_distributions")

for (d in c(OUT_WEATHER, OUT_WX_DIST))
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

# =============================================================================
# SECTION 3 — MAIN LOOP
# =============================================================================

all_wx_stats_02 <- list()

for (code in COUNTRIES_02) {
  cat(sprintf("\n=== %s ===\n", code))

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
      apply_policy_derivations()
  }, error = function(e) { message("  load failed: ", conditionMessage(e)); NULL })
  if (is.null(svy_base)) next
  cat(sprintf("  Loaded: %d rows\n", nrow(svy_base)))

  dates <- extract_survey_dates(svy_base)

  # Base observation count per countryyear (for pct_missing denominator)
  base_n_by_cy <- svy_base |>
    dplyr::mutate(countryyear = paste0(.data$economy, ", ", .data$year)) |>
    dplyr::count(countryyear, name = "n_total")

  # -- Loop over weather profiles ---------------------------------------------
  for (wx_name in names(WEATHER_SPECS)) {
    wx_prof <- WEATHER_SPECS[[wx_name]]
    wx_vars <- names(wx_prof)

    # Build spec_inputs (mirrors 03_run_mod1.R lines 435-447)
    spec_inputs <- list()
    for (v in wx_vars) {
      vs <- wx_prof[[v]]
      p  <- paste0(v, "_")
      spec_inputs[[paste0(p, "relativePeriod")]]  <- c(vs$ref_start %||% 1L, vs$ref_end)
      spec_inputs[[paste0(p, "temporalAgg")]]      <- vs$temporal_agg %||% weather_agg_for(v, WEATHER_AGG_OVERRIDE)
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

    df_wx <- svy_wx |>
      dplyr::mutate(countryyear = paste0(.data$economy, ", ", .data$year))
    vars <- intersect(selected_weather$name, names(df_wx))

    # -- Distribution plots (one PNG per weather variable) --------------------
    for (idx in seq_along(vars)) {
      hv          <- vars[idx]
      label       <- selected_weather$label[selected_weather$name == hv][1]
      cont_binned <- selected_weather$cont_binned[selected_weather$name == hv][1]

      tryCatch({
        out_path <- file.path(OUT_WX_DIST,
                              paste0(code, "_", wx_name, "_", hv, "_dist.png"))
        if (OVERWRITE_EXISTING || !file.exists(out_path)) {
          p <- plot_weather_dist(df_wx, hv = hv, label = label %||% hv,
                                 cont_binned = cont_binned)
          save_gg(p, out_path, width = 9, height = 5)
        }
      }, error = function(e) message("  dist plot failed [", hv, "]: ", conditionMessage(e)))
    }

    # -- Weather summary stats ------------------------------------------------
    tryCatch({
      spec_id <- data.frame(code = code, weather = wx_name, stringsAsFactors = FALSE)

      is_num    <- vapply(df_wx[vars], is.numeric, logical(1))
      cont_vars <- vars[is_num]
      bin_vars  <- vars[!is_num]

      # Continuous variables
      if (length(cont_vars) > 0) {
        wx_tab <- weighted_summary_long(df_wx, vars = cont_vars)
        if (nrow(wx_tab) > 0) {
          miss_df <- dplyr::bind_rows(lapply(cont_vars, function(v) {
            n_present <- df_wx |>
              dplyr::filter(!is.na(.data[[v]])) |>
              dplyr::count(countryyear, name = "n_present")
            base_n_by_cy |>
              dplyr::left_join(n_present, by = "countryyear") |>
              dplyr::mutate(
                n_present   = ifelse(is.na(n_present), 0L, n_present),
                pct_missing = round(100 * (1 - n_present / n_total), 2),
                variable    = v
              ) |>
              dplyr::select(countryyear, pct_missing, variable)
          }))
          wx_tab <- dplyr::left_join(wx_tab, miss_df, by = c("countryyear", "variable"))
          wx_tab$type      <- "continuous"
          wx_tab$level     <- NA_character_
          wx_tab$n_bin     <- NA_integer_
          wx_tab$share_pct <- NA_real_
          wx_tab <- cbind(spec_id[rep(1L, nrow(wx_tab)), , drop = FALSE], wx_tab)
          all_wx_stats_02[[length(all_wx_stats_02) + 1L]] <- wx_tab
        }
      }

      # Binned variables
      for (v in bin_vars) {
        counts <- df_wx |>
          dplyr::filter(!is.na(.data[[v]])) |>
          dplyr::group_by(.data$countryyear, .data[[v]]) |>
          dplyr::summarise(n_bin = dplyr::n(), .groups = "drop") |>
          dplyr::group_by(.data$countryyear) |>
          dplyr::mutate(share_pct = round(100 * n_bin / sum(n_bin), 2)) |>
          dplyr::ungroup() |>
          dplyr::rename(level = dplyr::all_of(v))
        counts$level    <- as.character(counts$level)
        counts$variable <- v
        counts$type     <- "binned"
        # add empty weighted-mean columns for schema consistency
        for (col in c("unweighted_mean", "Mean", "Std. Dev.", "Min", "Max", "N", "pct_missing"))
          counts[[col]] <- NA
        counts <- cbind(spec_id[rep(1L, nrow(counts)), , drop = FALSE], counts)
        all_wx_stats_02[[length(all_wx_stats_02) + 1L]] <- counts
      }
    }, error = function(e) message("  weather stats failed [", wx_name, "]: ", conditionMessage(e)))

    rm(svy_wx, weather_data, df_wx)
    gc(verbose = FALSE)
  }
}

# =============================================================================
# SECTION 4 — SAVE OUTPUTS
# =============================================================================

cat("\n=== Saving weather stats outputs ===\n")

if (length(all_wx_stats_02) > 0) {
  out_df  <- dplyr::bind_rows(all_wx_stats_02)
  out_csv <- file.path(OUT_WEATHER, "weather_stats.csv")
  readr::write_csv(out_df, out_csv)
  cat(sprintf("Saved: %s (%d rows)\n", out_csv, nrow(out_df)))
} else {
  cat("No weather stats accumulated.\n")
}

cat("========== Weather stats complete ==========\n")
