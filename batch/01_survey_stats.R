# =============================================================================
# batch/01_survey_stats.R
#
# Survey summary statistics for all countries.
#
# Outputs:
#   OUT_DIR/survey_stats/survey_stats.csv
#   OUT_DIR/survey_stats/interview_dates/{CODE}_interview_dates.png
#   OUT_DIR/survey_stats/location_maps/{CODE}_location_map.png
#   OUT_DIR/survey_stats/welfare_distributions/{CODE}_welfare_dist.png
#
# All user inputs are in SECTION 1. 
#
# =============================================================================

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
# NULL = all available countries; character vector of country codes = subset
COUNTRY_FILTER <- NULL

# ---- Output options ---------------------------------------------------------
OVERWRITE_EXISTING <- TRUE

# =============================================================================
# SECTION 2 — SETUP
# =============================================================================

# Load helpers and build connection params based on config vars above. Then
pkgload::load_all(quiet = TRUE)
invisible(lapply(list.files("batch/R", pattern = "\\.R$", full.names = TRUE), source))

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

all_svy_stats_01  <- list()
all_welf_agg_01   <- list()

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
      bottom_code_welfare(0.28) |>
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
  #skip for FJI
  if (code == "FJI") {
    cat("  SKIP — no location data for FJI\n")
  } else {
    out_path <- file.path(OUT_MAPS, paste0(code, "_location_map.png"))
    if (OVERWRITE_EXISTING || !file.exists(out_path)) {
      geojson <- build_h3_geojson(ss, connection_params_01)
      if (is.null(geojson)) {
        message("  location map skipped — build_h3_geojson returned NULL")
      } else {
        p_map <- tryCatch(
          plot_survey_map_static(geojson),
          error = function(e) { message("  plot_survey_map_static failed: ", conditionMessage(e)); NULL }
        )
        if (!is.null(p_map))
          save_gg(p_map, out_path, width = 8, height = 6, dpi = 96)
      }
    }
  }

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
    svy_stats <- weighted_survey_stats(svy, vars = svy_stat_vars, var_info = var_info_01)
    if (nrow(svy_stats) > 0) {
      all_svy_stats_01[[code]] <- svy_stats
      cat(sprintf("  Survey stats: %d variables\n", length(unique(svy_stats$variable))))
    }
  }, error = function(e) message("  survey stats failed: ", conditionMessage(e)))

  # ------ Welfare aggregates (mod-2-matching aggregate methods) -------------
  tryCatch({
    if ("welfare" %in% names(svy)) {
      pov_lines <- c(`300` = 3.00, `420` = 4.20, `830` = 8.30)

      # Methods that do NOT need a poverty line
      plain_methods <- c("mean", "median", "total", "gini", "prosperity_gap", "avg_poverty")
      # Methods that DO need a poverty line
      pov_methods   <- c("headcount_ratio", "gap", "fgt2")

      grp_keys <- c("code", "economy", "survname", "year")
      split_df <- split(svy, lapply(grp_keys, function(k) svy[[k]]), drop = TRUE)

      welf_rows <- lapply(split_df, function(sub) {
        welf <- sub[["welfare"]]
        wts  <- if ("weight" %in% names(sub)) sub[["weight"]] else NULL

        row <- sub[1L, grp_keys, drop = FALSE]
        row[grp_keys] <- lapply(grp_keys, function(k) sub[[k]][1L])

        for (m in plain_methods) {
          fn <- resolve_agg_fn(m)
          row[[paste0("welfare_", m)]] <- tryCatch(
            fn(welf, wts, NULL), error = function(e) NA_real_
          )
        }
        for (m in pov_methods) {
          fn <- resolve_agg_fn(m)
          for (sfx in names(pov_lines)) {
            row[[paste0(m, "_", sfx)]] <- tryCatch(
              fn(welf, wts, pov_lines[[sfx]]), error = function(e) NA_real_
            )
          }
        }
        row
      })

      welf_agg <- dplyr::bind_rows(welf_rows)
      if (nrow(welf_agg) > 0) {
        all_welf_agg_01[[code]] <- welf_agg
        cat(sprintf("  Welfare aggregates: %d survey waves\n", nrow(welf_agg)))
      }
    }
  }, error = function(e) message("  welfare aggregates failed: ", conditionMessage(e)))

  # -- Clear country-level objects from memory --------------------------------
  rm(svy, years_by_code, ss)
  gc(verbose = FALSE)
}

# =============================================================================
# SECTION 4 — SAVE OUTPUTS
# =============================================================================

cat("\n=== Saving survey stats outputs ===\n")

if (length(all_svy_stats_01) > 0) {
  out_df  <- dplyr::bind_rows(all_svy_stats_01)
  out_csv <- file.path(OUT_SURVEY, "survey_stats.csv")

  # If not overwriting and file exists, append to existing rows
  if (!OVERWRITE_EXISTING && file.exists(out_csv)) {
    existing <- readr::read_csv(out_csv, show_col_types = FALSE)

    # Align date column types to avoid bind_rows type mismatch
    date_cols <- c("min_date", "max_date")
    for (col in date_cols) {
      if (col %in% names(out_df) && inherits(existing[[col]], "Date")) {
        out_df[[col]] <- as.Date(out_df[[col]])
      }
    }
    
    out_df   <- dplyr::bind_rows(existing, out_df)
    cat(sprintf("  Appending to existing file (%d existing rows)\n", nrow(existing)))
  }

  # Deduplicate on identifying columns
  dedup_keys <- c("code", "economy", "survname", "year", "variable")
  out_df <- dplyr::distinct(out_df, dplyr::across(dplyr::any_of(dedup_keys)), .keep_all = TRUE)

  grp_order <- c("outcome", "policy", "hh", "ind", "area", "firm", "other")
  out_df$var_group <- factor(out_df$var_group, levels = grp_order)
  out_df  <- out_df[order(out_df$code, out_df$var_group, out_df$variable, out_df$year), ]
  out_df$var_group <- as.character(out_df$var_group)
  readr::write_csv(out_df, out_csv)
  cat(sprintf("Saved: %s (%d rows)\n", out_csv, nrow(out_df)))
} else {
  cat("No survey stats accumulated.\n")
}

# ---- Welfare aggregates CSV ------------------------------------------------
welf_csv <- file.path(OUT_SURVEY, "welfare_aggregates.csv")

if (length(all_welf_agg_01) > 0) {
  welf_df <- dplyr::bind_rows(all_welf_agg_01)

  if (!OVERWRITE_EXISTING && file.exists(welf_csv)) {
    existing_welf <- readr::read_csv(welf_csv, show_col_types = FALSE)
    welf_df       <- dplyr::bind_rows(existing_welf, welf_df)
    cat(sprintf("  Appending welfare aggregates to existing file (%d existing rows)\n",
                nrow(existing_welf)))
  }

  welf_dedup_keys <- c("code", "economy", "survname", "year")
  welf_df <- dplyr::distinct(welf_df, dplyr::across(dplyr::any_of(welf_dedup_keys)),
                              .keep_all = TRUE)
  welf_df <- welf_df[order(welf_df$code, welf_df$year), ]

  readr::write_csv(welf_df, welf_csv)
  cat(sprintf("Saved: %s (%d rows, %d columns)\n", welf_csv, nrow(welf_df), ncol(welf_df)))
} else {
  cat("No welfare aggregates accumulated.\n")
}

cat("========== Survey stats complete ==========\n")
