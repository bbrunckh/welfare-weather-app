# =============================================================================
# batch/01_survey_stats.R
#
# Survey summary statistics for all countries.
# Outputs:
#   OUT_DIR/survey_stats/survey_stats.csv
#   OUT_DIR/survey_stats/interview_dates/{CODE}_interview_dates.png
#   OUT_DIR/survey_stats/location_maps/{CODE}_location_map.png
#   OUT_DIR/survey_stats/welfare_distributions/{CODE}_welfare_dist.png
#
# Can be run standalone or sourced from 04_run_all.R (config vars pre-set).
# All user inputs are in SECTION 1. Config vars are guarded so 04_run_all.R
# can pre-set them before sourcing this script.
#
# Usage: source("batch/01_survey_stats.R")
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
# NULL = all available countries; character vector = subset
if (!exists("COUNTRY_FILTER")) COUNTRY_FILTER <- "GNB"

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
OUT_SURVEY   <- file.path(OUT_DIR, "survey_stats")
OUT_DATES    <- file.path(OUT_SURVEY, "interview_dates")
OUT_MAPS     <- file.path(OUT_SURVEY, "location_maps")
OUT_WELDIST  <- file.path(OUT_SURVEY, "welfare_distributions")

for (d in c(OUT_SURVEY, OUT_DATES, OUT_MAPS, OUT_WELDIST))
  dir.create(d, showWarnings = FALSE, recursive = TRUE)

# Connection
connection_params_01 <- if (identical(CONNECTION_TYPE, "databricks")) {
  build_connection_params("databricks")
} else {
  build_connection_params("local", path = DATA_DIR)
}
stopifnot("Invalid connection_params" = validate_connection_params(connection_params_01))
cat(sprintf("Connection: %s\n",
            if (identical(connection_params_01$type, "databricks"))
              "Databricks" else paste0("local (", connection_params_01$path, ")")))

# Metadata
var_info_01    <- load_data("metadata/variable_list.csv", connection_params_01, collect = TRUE)
survey_list_01 <- load_data("metadata/survey_list.csv",   connection_params_01, collect = TRUE)
cpi_ppp_01     <- load_data("metadata/cpi_ppp.csv",       connection_params_01, collect = TRUE)

LEVEL_01 <- switch(UNIT, hh = "hh", ind = "ind", firm = "firm", "hh")
surveys_01 <- build_survey_fnames(survey_list_01, LEVEL_01, connection_params_01)
COUNTRIES_01 <- sort(unique(surveys_01$code))
if (!is.null(COUNTRY_FILTER))
  COUNTRIES_01 <- intersect(COUNTRIES_01, COUNTRY_FILTER)

cat(sprintf("Countries (survey stats): %d (%s)\n\n",
            length(COUNTRIES_01), paste(COUNTRIES_01, collapse = ", ")))

# =============================================================================
# SECTION 3 — MAIN LOOP
# =============================================================================

all_svy_stats_01 <- list()

for (code in COUNTRIES_01) {
  cat(sprintf("\n=== %s ===\n", code))

  # Build survey file list for this country
  years_by_code <- setNames(
    list(as.character(sort(unique(surveys_01$year[surveys_01$code == code])))),
    code
  )
  ss <- build_selected_surveys(surveys = surveys_01, years_by_code = years_by_code)
  if (nrow(ss) == 0) {
    cat("  SKIP — no surveys found\n")
    next
  }

  # Load and preprocess survey data (mirrors mod_1_02_surveystats pipeline)
  svy <- tryCatch({
    df       <- load_data(ss$fname, connection_params_01, collect = TRUE, unify_schemas = TRUE)
    df       <- add_time_columns(df)
    lcu_vars <- get_lcu_vars(df, var_info_01)
    df |>
      assign_data_level() |>
      convert_lcu_to_ppp(cpi_ppp_01, lcu_vars) |>
      apply_policy_derivations()
  }, error = function(e) {
    message("  load failed: ", conditionMessage(e))
    NULL
  })
  if (is.null(svy)) next
  cat(sprintf("  Loaded: %d rows\n", nrow(svy)))

  # ------ Interview dates plot -----------------------------------------------
  tryCatch({
    p_dates  <- plot_interview_dates(summarise_interview_dates(svy))
    out_path <- file.path(OUT_DATES, paste0(code, "_interview_dates.png"))
    if (OVERWRITE_EXISTING || !file.exists(out_path))
      save_gg(p_dates, out_path, width = 9, height = 4)
  }, error = function(e) message("  interview dates failed: ", conditionMessage(e)))

  # ------ Location map (static ggplot/sf) ------------------------------------
  tryCatch({
    out_path <- file.path(OUT_MAPS, paste0(code, "_location_map.png"))
    if (OVERWRITE_EXISTING || !file.exists(out_path)) {
      geojson <- build_h3_geojson(ss, connection_params_01)
      if (!is.null(geojson)) {
        p_map <- plot_survey_map_static(geojson)
        save_gg(p_map, out_path, width = 8, height = 6)
      }
    }
  }, error = function(e) message("  location map failed: ", conditionMessage(e)))

  # ------ Welfare distribution ridge plot ------------------------------------
  tryCatch({
    out_path <- file.path(OUT_WELDIST, paste0(code, "_welfare_dist.png"))
    if (OVERWRITE_EXISTING || !file.exists(out_path)) {
      p_welf <- plot_welfare_dist(svy, outcome = "welfare",
                                  poverty_lines = welfare_poverty_lines())
      save_gg(p_welf, out_path, width = 8, height = 5)
    }
  }, error = function(e) message("  welfare dist failed: ", conditionMessage(e)))

  # ------ Weighted summary stats --------------------------------------------
  tryCatch({
    svy_stat_vars <- union(
      intersect(
        var_info_01$name[var_info_01$ind == 1 | var_info_01$hh == 1 |
                           var_info_01$firm == 1 | var_info_01$area == 1],
        names(svy)
      ),
      intersect(c("imp_wat_san_rec"), names(svy))
    )
    svy_stats <- weighted_summary_long(svy, vars = svy_stat_vars)
    if (nrow(svy_stats) > 0) {
      numeric_vars <- svy_stat_vars[vapply(svy[svy_stat_vars], is.numeric, logical(1))]
      miss_list <- lapply(numeric_vars, function(v) {
        svy |>
          dplyr::group_by(countryyear) |>
          dplyr::summarise(pct_missing = 100 * mean(is.na(.data[[v]])),
                           .groups = "drop") |>
          dplyr::mutate(variable = v)
      })
      if (length(miss_list) > 0) {
        svy_stats <- dplyr::left_join(svy_stats, dplyr::bind_rows(miss_list),
                                      by = c("countryyear", "variable"))
      }
      svy_stats$code <- code
      all_svy_stats_01[[code]] <- svy_stats
      cat(sprintf("  Survey stats: %d variables\n", length(numeric_vars)))
    }
  }, error = function(e) message("  survey stats failed: ", conditionMessage(e)))
}

# =============================================================================
# SECTION 4 — SAVE OUTPUTS
# =============================================================================

cat("\n=== Saving survey stats outputs ===\n")

if (length(all_svy_stats_01) > 0) {
  out_df  <- dplyr::bind_rows(all_svy_stats_01)
  out_csv <- file.path(OUT_SURVEY, "survey_stats.csv")
  readr::write_csv(out_df, out_csv)
  cat(sprintf("Saved: %s (%d rows)\n", out_csv, nrow(out_df)))
} else {
  cat("No survey stats accumulated.\n")
}

cat("========== Survey stats complete ==========\n")
