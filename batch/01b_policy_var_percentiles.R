# =============================================================================
# batch/01b_policy_var_percentiles.R
#
# Weighted percentile distribution of policy-relevant variables across surveys.
# Supplements survey_stats.csv (which records mean/pct_missing but not quartiles)
# to inform policy scenario thresholds in batch/04_run_sim.R.
#
# Outputs (all under OUT_DIR/survey_stats/):
#   policy_percentiles.csv   weighted p25 / p50 / p75 / p90 + % above thresholds
#
# Usage: source("batch/01b_policy_var_percentiles.R")
# =============================================================================

pkgload::load_all(quiet = TRUE)
invisible(lapply(list.files("batch/R", pattern = "\\.R$", full.names = TRUE), source))

library(tidyverse)

# =============================================================================
# SECTION 1 — CONFIGURATION
# =============================================================================

CONNECTION_TYPE <- "local"
DATA_DIR        <- Sys.getenv("WISEAPP_DATA_PATH")
OUT_DIR         <- Sys.getenv("WISEAPP_RESULTS_PATH")

UNIT <- "hh"

# Restrict to multi-wave countries (consistent with 01a / 02a / 03a)
COUNTRY_FILTER <- NULL   # NULL = all available; or c("BEN", "SEN", ...) to subset

# Variables to summarise
POLICY_VARS <- c("ttime_health", "electricity", "imp_wat_rec", "imp_san_rec",
                 "imp_wat_san_rec", "internet", "cellphone", "piped", "piped_to_prem")

# For ttime_health: thresholds (minutes) to report % of households above
TTIME_THRESHOLDS <- c(15L, 30L, 45L, 60L)

# =============================================================================
# SECTION 2 — SETUP
# =============================================================================

OUT_SURVEY <- file.path(OUT_DIR, "survey_stats")
dir.create(OUT_SURVEY, showWarnings = FALSE, recursive = TRUE)

connection_params <- if (identical(CONNECTION_TYPE, "databricks")) {
  build_connection_params("databricks")
} else {
  build_connection_params("local", path = DATA_DIR)
}
stopifnot("Invalid connection_params" = validate_connection_params(connection_params))

var_info    <- load_data("metadata/variable_list.csv", connection_params, collect = TRUE)
survey_list <- load_data("metadata/survey_list.csv",   connection_params, collect = TRUE)
cpi_ppp     <- load_data("metadata/cpi_ppp.csv",       connection_params, collect = TRUE)

LEVEL <- switch(UNIT, hh = "hh", ind = "ind", firm = "firm", "hh")
surveys_with_fnames <- build_survey_fnames(survey_list, LEVEL, connection_params)

# Restrict to multi-wave countries
welfare_agg      <- read_csv(file.path(OUT_SURVEY, "welfare_aggregates.csv"),
                              show_col_types = FALSE)
multi_wave_codes <- welfare_agg |> count(code) |> filter(n >= 2) |> pull(code)

COUNTRIES <- sort(unique(surveys_with_fnames$code))
COUNTRIES <- intersect(COUNTRIES, multi_wave_codes)
if (!is.null(COUNTRY_FILTER))
  COUNTRIES <- intersect(COUNTRIES, COUNTRY_FILTER)

cat(sprintf("Countries: %d (%s)\n\n", length(COUNTRIES), paste(COUNTRIES, collapse = ", ")))

# =============================================================================
# SECTION 3 — MAIN LOOP
# =============================================================================

wtd_quantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x  <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(setNames(rep(NA_real_, length(probs)), paste0("p", probs * 100)))
  ord <- order(x)
  x   <- x[ord]; w <- w[ord]
  cw  <- cumsum(w) / sum(w)
  setNames(
    sapply(probs, function(p) x[which(cw >= p)[1]]),
    paste0("p", probs * 100)
  )
}

wtd_pct_above <- function(x, w, thresholds) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x  <- x[ok]; w <- w[ok]
  if (length(x) == 0) return(setNames(rep(NA_real_, length(thresholds)),
                                       paste0("pct_above_", thresholds)))
  setNames(
    sapply(thresholds, function(t) sum(w[x > t]) / sum(w) * 100),
    paste0("pct_above_", thresholds)
  )
}

all_rows <- list()

for (code in COUNTRIES) {
  cat(sprintf("=== %s ===\n", code))

  years_by_code <- setNames(
    list(as.character(sort(unique(surveys_with_fnames$year[surveys_with_fnames$code == code])))),
    code
  )
  ss <- build_selected_surveys(surveys = surveys_with_fnames, years_by_code = years_by_code)
  if (nrow(ss) == 0) { cat("  SKIP — no surveys\n"); next }

  svy <- tryCatch({
    df       <- load_data(ss$fname, connection_params, collect = TRUE, unify_schemas = TRUE)
    df       <- add_time_columns(df)
    lcu_vars <- get_lcu_vars(df, var_info)
    df |>
      assign_data_level() |>
      convert_lcu_to_ppp(cpi_ppp, lcu_vars) |>
      bottom_code_welfare(0.28) |>
      apply_policy_derivations()
  }, error = function(e) { message("  load failed: ", conditionMessage(e)); NULL })
  if (is.null(svy)) next

  present_vars <- intersect(POLICY_VARS, names(svy))
  if (length(present_vars) == 0) { cat("  SKIP — no policy vars\n"); next }

  w <- if ("weight" %in% names(svy)) svy$weight else rep(1, nrow(svy))
  w[is.na(w)] <- 0

  for (yr in sort(unique(svy$year))) {
    sub  <- svy[svy$year == yr, , drop = FALSE]
    w_yr <- if ("weight" %in% names(sub)) sub$weight else rep(1, nrow(sub))
    w_yr[is.na(w_yr)] <- 0

    for (v in present_vars) {
      x    <- sub[[v]]
      n_ok <- sum(!is.na(x))
      if (n_ok == 0) next

      qs  <- wtd_quantile(x, w_yr, c(0.25, 0.50, 0.75, 0.90))
      row <- data.frame(
        code = code, year = yr, variable = v,
        n = nrow(sub), n_nonmissing = n_ok,
        pct_missing = round(mean(is.na(x)) * 100, 1),
        mean   = round(weighted.mean(x, w_yr, na.rm = TRUE), 2),
        p25    = qs[["p25"]],
        p50    = qs[["p50"]],
        p75    = qs[["p75"]],
        p90    = qs[["p90"]],
        stringsAsFactors = FALSE
      )

      if (v == "ttime_health") {
        above <- wtd_pct_above(x, w_yr, TTIME_THRESHOLDS)
        row   <- cbind(row, as.data.frame(t(above)))
      }

      all_rows[[length(all_rows) + 1L]] <- row
    }
  }

  rm(svy)
  gc(verbose = FALSE)
}

# =============================================================================
# SECTION 4 — SAVE + SUMMARISE
# =============================================================================

out_df  <- dplyr::bind_rows(all_rows) |> tibble::as_tibble()
out_csv <- file.path(OUT_SURVEY, "policy_percentiles.csv")
readr::write_csv(out_df, out_csv)
cat(sprintf("\nSaved: %s (%d rows)\n\n", out_csv, nrow(out_df)))

# --- Console summary: ttime_health ---
ttime <- out_df |> filter(variable == "ttime_health")

if (nrow(ttime) > 0) {
  cat("=== ttime_health (minutes to health facility) ===\n")
  print(
    ttime |>
      select(code, year, n_nonmissing, pct_missing, mean, p25, p50, p75, p90,
             dplyr::any_of(paste0("pct_above_", TTIME_THRESHOLDS))) |>
      arrange(code, year),
    n = Inf
  )

  cat("\n--- Cross-country summary (median of survey medians) ---\n")
  smry <- ttime |>
    group_by(code) |>
    summarise(
      med_p25 = median(p25, na.rm = TRUE),
      med_p50 = median(p50, na.rm = TRUE),
      med_p75 = median(p75, na.rm = TRUE),
      across(dplyr::starts_with("pct_above_"), ~median(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    arrange(med_p50)
  print(smry, n = Inf)

  grand <- ttime |>
    summarise(
      grand_p25 = median(p25, na.rm = TRUE),
      grand_p50 = median(p50, na.rm = TRUE),
      grand_p75 = median(p75, na.rm = TRUE),
      across(dplyr::starts_with("pct_above_"), ~median(.x, na.rm = TRUE))
    )
  cat("\n--- Grand median across all countries ---\n")
  print(grand)
}

# --- Console summary: binary policy vars ---
bin_vars <- setdiff(POLICY_VARS, "ttime_health")
bin_df   <- out_df |>
  filter(variable %in% bin_vars) |>
  select(code, year, variable, n_nonmissing, pct_missing, mean) |>
  mutate(pct_access = round(mean * 100, 1)) |>
  select(-mean)

if (nrow(bin_df) > 0) {
  cat("\n=== Binary policy variables (% with access) ===\n")
  print(
    bin_df |>
      group_by(variable, code) |>
      summarise(mean_pct_access = round(mean(pct_access, na.rm = TRUE), 1), .groups = "drop") |>
      pivot_wider(names_from = variable, values_from = mean_pct_access) |>
      arrange(code),
    n = Inf
  )
}

cat("\n========== Policy percentiles complete ==========\n")
