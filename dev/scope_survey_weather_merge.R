# =============================================================================
# dev/scope_survey_weather_merge.R
#
# For each country-survey-year with both microdata and ERA5 weather:
#   1. Join households to ERA5 weather via H3 cell (using DuckDB)
#   2. Report % of households with each weather variable available
#
# Usage: source("dev/scope_survey_weather_merge.R")
# =============================================================================

pkgload::load_all(quiet = TRUE)
rm(list = intersect(ls(), "get_weather"))

# ---- Configuration ----------------------------------------------------------
DATA_DIR    <- "C:/Users/wb617223/Documents/welfare-weather-app/data"
TARGET_VARS <- c("t")
out_path    <- "dev/outputs/survey_weather_merge_coverage.csv"
dir.create("dev/outputs", showWarnings = FALSE, recursive = TRUE)

connection_params <- list(type = "local", path = DATA_DIR)

# ---- Helper — build NA skip row ---------------------------------------------
make_skip_row <- function(code, economy, survname, year,
                          has_micro, has_era5, reason = "") {
  row <- data.frame(
    code       = code,
    economy    = economy,
    survname   = survname,
    year       = year,
    has_micro  = has_micro,
    has_era5   = has_era5,
    reason     = reason,
    n_hh       = NA_integer_,
    n_merged   = NA_integer_,
    pct_merged = NA_real_,
    stringsAsFactors = FALSE
  )
  for (v in TARGET_VARS) row[[paste0(v, "_pct")]] <- NA_real_
  row
}

# ---- Load survey list -------------------------------------------------------
survey_list <- load_data("metadata/survey_list.csv",
                         connection_params, collect = TRUE)
surveys_hh  <- dplyr::filter(survey_list, level == "hh") |>
               dplyr::arrange(code, year)

cat("Household surveys:", nrow(surveys_hh), "\n")

# ---- What files exist? ------------------------------------------------------
micro_files <- list.files(
  file.path(DATA_DIR, "microdata/hh"),
  pattern = "_hh\\.parquet$", recursive = TRUE, full.names = FALSE
)
hist_codes <- sub("/.*", "",
  list.files(file.path(DATA_DIR, "hazard/weather/historical"),
             pattern = "_era5land\\.parquet$", recursive = TRUE))

cat("Microdata files:  ", length(micro_files), "\n")
cat("ERA5 countries:   ", paste(hist_codes, collapse=", "), "\n\n")

# ---- DuckDB connection for H3 join ------------------------------------------
con <- .duck_con()
.duck_load_ext("h3")   # load H3 extension

# ---- Main loop --------------------------------------------------------------
results <- vector("list", nrow(surveys_hh))

for (si in seq_len(nrow(surveys_hh))) {
  code     <- surveys_hh$code[[si]]
  economy  <- surveys_hh$economy[[si]]
  year_val <- as.integer(surveys_hh$year[[si]])
  survname <- surveys_hh$survname[[si]]
  src      <- surveys_hh$source[[si]]

  cat(sprintf("[%d/%d] %s %s %d...", si, nrow(surveys_hh), code, survname, year_val))

  # ---- Check microdata exists -----------------------------------------------
  micro_candidates <- micro_files[
    grepl(paste0("^", code, "/"), micro_files) &
    grepl(as.character(year_val), micro_files)
  ]
  has_micro <- length(micro_candidates) > 0
  has_era5  <- code %in% hist_codes

  if (!has_micro || !has_era5) {
    cat(sprintf(" micro=%s era5=%s — skip\n",
                if (has_micro) "✓" else "✗",
                if (has_era5)  "✓" else "✗"))
    results[[si]] <- make_skip_row(code, economy, survname, year_val,
                                   has_micro, has_era5, "no_data")
    next
  }

  # ---- Load microdata -------------------------------------------------------
  micro_df <- tryCatch(
    load_data(file.path("microdata/hh", micro_candidates[[1L]]),
              connection_params, collect = TRUE),
    error = function(e) { message("  micro failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(micro_df)) {
    results[[si]] <- make_skip_row(code, economy, survname, year_val,
                                   TRUE, TRUE, "micro_load_error")
    cat(" micro load failed\n")
    next
  }

  n_hh <- nrow(micro_df)
    # Skip very large surveys for now — too slow for scoping
  if (n_hh > 2000000L) { #Started at 50k but even 20k is slow on my machine with DuckDB + H3 join
    cat(sprintf(" n=%d TOO LARGE — skip\n", n_hh))
    results[[si]] <- make_skip_row(code, economy, survname, year_val,
                                   TRUE, TRUE, "too_large")
    next
  }
  cat(sprintf(" n=%d", n_hh))

    # ---- Find h3 crosswalk file for this survey ------------------------------
  h3_candidates <- list.files(
    file.path(DATA_DIR, "microdata/h3", code),
    pattern    = paste0(code, "_", year_val, ".*_h3\\.parquet$"),
    full.names = FALSE
  )

  if (length(h3_candidates) == 0) {
    cat(" no h3 crosswalk — skip\n")
    results[[si]] <- make_skip_row(code, economy, survname, year_val,
                                   TRUE, TRUE, "no_h3_crosswalk")
    next
  }

  h3_fname <- file.path("microdata/h3", code, h3_candidates[[1L]])

  # ---- Load h3 crosswalk ---------------------------------------------------
  h3_xwalk <- tryCatch(
    load_data(h3_fname, connection_params, collect = TRUE),
    error = function(e) { message("h3 load failed: ", conditionMessage(e)); NULL }
  )

  if (is.null(h3_xwalk)) {
    results[[si]] <- make_skip_row(code, economy, survname, year_val,
                                   TRUE, TRUE, "h3_load_error")
    cat(" h3 load failed\n")
    next
  }

  # ---- Load ERA5 for survey year only --------------------------------------
  era5_path_full <- file.path(DATA_DIR,
    sprintf("hazard/weather/historical/%s/%s_era5land.parquet", code, code))

  era5_year <- tryCatch({
    df <- load_data(
      sprintf("hazard/weather/historical/%s/%s_era5land.parquet", code, code),
      connection_params, collect = TRUE
    )
    df$year_num <- as.integer(format(df$timestamp, "%Y"))
    df[df$year_num == year_val, ]
  }, error = function(e) { message("ERA5 load failed: ", conditionMessage(e)); NULL })

  if (is.null(era5_year) || nrow(era5_year) == 0) {
    cat(" ERA5 year not found\n")
    results[[si]] <- make_skip_row(code, economy, survname, year_val,
                                   TRUE, TRUE, "era5_year_missing")
    next
  }

  cat(sprintf(" era5_rows=%d", nrow(era5_year)))

  # ---- Aggregate ERA5 to annual mean per h3 cell ---------------------------
  era5_annual <- era5_year |>
    dplyr::group_by(h3) |>
    dplyr::summarise(
      dplyr::across(dplyr::any_of(TARGET_VARS),
                    ~mean(.x, na.rm = TRUE)),
      .groups = "drop"
    )

  # ---- Join chain: micro → h3_xwalk → era5 --------------------------------
  # Step 1: microdata + h3 crosswalk (loc_id join)
  # Ensure loc_id types match
  micro_df$loc_id   <- as.character(micro_df$loc_id)
  h3_xwalk$loc_id   <- as.character(h3_xwalk$loc_id)

  # Take most populated h3 cell per loc_id to avoid many-to-many
  h3_best <- h3_xwalk |>
    dplyr::group_by(loc_id) |>
    dplyr::slice_max(pop_2020, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(loc_id, h3)

  micro_h3 <- dplyr::left_join(
    micro_df,
    h3_best,
    by = "loc_id"
  )

  cat(sprintf(" h3_match=%.0f%%",
              sum(!is.na(micro_h3$h3)) / nrow(micro_h3) * 100))

  # ---- Get ERA5 h3 resolution -----------------------------------------------
  era5_res <- tryCatch({
    # Get resolution of first ERA5 h3 cell
    res_q <- DBI::dbGetQuery(con, sprintf(
      "SELECT h3_get_resolution(h3) AS res
       FROM read_parquet('%s') LIMIT 1",
      gsub("\\\\", "/", era5_path_full)
    ))
    as.integer(res_q$res[[1L]])
  }, error = function(e) 6L)  # default to resolution 6

  cat(sprintf(" era5_res=%d", era5_res))

  # ---- Build era5_annual_str with h3 at ERA5 resolution --------------------
  era5_annual_str <- tryCatch({
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE TEMP VIEW era5_annual_tmp AS
       SELECT h3_h3_to_string(h3) AS h3_str,
              %s
       FROM (
         SELECT h3,
                %s
         FROM read_parquet('%s')
         WHERE YEAR(timestamp) = %d
         GROUP BY h3
       ) sub",
      paste(TARGET_VARS, collapse = ",\n              "),
      paste(sprintf("AVG(%s) AS %s", TARGET_VARS, TARGET_VARS), collapse=",\n                "),
      gsub("\\\\", "/", era5_path_full),
      year_val
    ))
    DBI::dbGetQuery(con, "SELECT * FROM era5_annual_tmp")
  }, error = function(e) {
    message("  ERA5 DuckDB convert failed: ", conditionMessage(e))
    NULL
  })

  if (is.null(era5_annual_str)) {
    results[[si]] <- make_skip_row(code, economy, survname, year_val,
                                   TRUE, TRUE, "h3_convert_error")
    cat(" h3 convert failed\n")
    next
  }

  # ---- Convert micro h3 to ERA5 resolution then join -----------------------
  micro_h3$h3_era5res <- tryCatch({
    # Write micro_h3 h3 column to DuckDB FIRST, then convert resolution
    DBI::dbWriteTable(
      con, "micro_h3_r",
      data.frame(h3 = micro_h3$h3, stringsAsFactors = FALSE),
      overwrite = TRUE
    )
    result <- DBI::dbGetQuery(con, sprintf(
      "SELECT h3_h3_to_string(
               h3_cell_to_parent(h3_string_to_h3(h3), %d)
             ) AS h3_parent
       FROM micro_h3_r",
      era5_res
    ))
    result$h3_parent
  }, error = function(e) {
    message("  h3 parent conversion failed: ", conditionMessage(e))
    micro_h3$h3  # fallback: use original h3 (may not match)
  })
  # ---- Join on ERA5-resolution h3 -----------------------------------------
  merged_df <- dplyr::left_join(
    micro_h3,
    era5_annual_str,
    by = c("h3_era5res" = "h3_str")
  )

  # ---- Compute coverage statistics ------------------------------------------
  n_merged   <- sum(!is.na(merged_df[[TARGET_VARS[[1L]]]]))
  pct_merged <- n_merged / n_hh

  row <- data.frame(
    code       = code,
    economy    = economy,
    survname   = survname,
    year       = year_val,
    has_micro  = TRUE,
    has_era5   = TRUE,
    reason     = "ok",
    n_hh       = n_hh,
    n_merged   = n_merged,
    pct_merged = round(pct_merged, 4),
    stringsAsFactors = FALSE
  )
  for (v in TARGET_VARS) {
    row[[paste0(v, "_pct")]] <- round(
      sum(!is.na(merged_df[[v]])) / n_hh, 4)
  }

  cat(sprintf(" merged=%.1f%% [%s]\n",
              pct_merged * 100,
              paste(sprintf("%s=%.0f%%", TARGET_VARS,
                            sapply(TARGET_VARS, function(v)
                              sum(!is.na(merged_df[[v]])) / n_hh * 100)),
                    collapse=" ")))

  results[[si]] <- row
}

merge_coverage <- dplyr::bind_rows(results) |>
  dplyr::arrange(dplyr::desc(pct_merged), code, year)

# ---- Summary ----------------------------------------------------------------
cat("\n=== Survey-Weather Merge Coverage ===\n\n")
cat("Total surveys:        ", nrow(merge_coverage), "\n")
cat("Successfully merged:  ",
    sum(merge_coverage$reason == "ok", na.rm=TRUE), "\n")
cat("No microdata:         ",
    sum(!merge_coverage$has_micro, na.rm=TRUE), "\n")
cat("No ERA5:              ",
    sum(merge_coverage$has_micro & !merge_coverage$has_era5, na.rm=TRUE), "\n\n")

ok <- merge_coverage[!is.na(merge_coverage$reason) &
                      merge_coverage$reason == "ok", ]

if (nrow(ok) > 0) {
  cat("--- Merge rates ---\n")
  cat(sprintf("Mean: %.1f%%  Min: %.1f%%  Max: %.1f%%\n",
              mean(ok$pct_merged)*100,
              min(ok$pct_merged)*100,
              max(ok$pct_merged)*100))

  cat("\n--- Per-variable coverage ---\n")
  for (v in TARGET_VARS) {
    col <- paste0(v, "_pct")
    cat(sprintf("  %-10s  mean=%.1f%%  min=%.1f%%  max=%.1f%%\n",
                v,
                mean(ok[[col]], na.rm=TRUE)*100,
                min(ok[[col]],  na.rm=TRUE)*100,
                max(ok[[col]],  na.rm=TRUE)*100))
  }

  cat("\n--- Results by survey ---\n")
  print(ok[, c("code","economy","survname","year",
               "n_hh","pct_merged",
               paste0(TARGET_VARS, "_pct"))],
        row.names = FALSE)
}

readr::write_csv(merge_coverage, out_path)
cat("\nSaved to:", out_path, "\n")
cat("========== Complete ==========\n")