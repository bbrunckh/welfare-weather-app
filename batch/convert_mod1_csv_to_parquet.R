# batch/convert_mod1_csv_to_parquet.R
#
# One-off conversion: model_coefficients.csv and model_fit_stats.csv -> .parquet
# Leaves original CSVs in place for manual validation/removal.
# Usage: source("batch/convert_mod1_csv_to_parquet.R")
# =============================================================================

library(readr)
library(arrow)

OUT_DIR <- Sys.getenv("WISEAPP_RESULTS_PATH")
if (!nzchar(OUT_DIR)) OUT_DIR <- "dev/mod1_test"
MOD_DIR <- file.path(OUT_DIR, "model_fit")

# --- model_coefficients -------------------------------------------------------
coef_csv <- file.path(MOD_DIR, "model_coefficients.csv")
coef_pq  <- file.path(MOD_DIR, "model_coefficients.parquet")

if (file.exists(coef_csv)) {
  coef <- read_csv(coef_csv, show_col_types = FALSE)
  write_parquet(coef, coef_pq)
  cat(sprintf("Converted: %s (%d rows) -> %s\n", basename(coef_csv), nrow(coef), basename(coef_pq)))
  rm(coef)
} else {
  cat("Not found:", coef_csv, "\n")
}

# --- model_fit_stats ----------------------------------------------------------
stats_csv <- file.path(MOD_DIR, "model_fit_stats.csv")
stats_pq  <- file.path(MOD_DIR, "model_fit_stats.parquet")

if (file.exists(stats_csv)) {
  stats <- read_csv(stats_csv, show_col_types = FALSE)
  write_parquet(stats, stats_pq)
  cat(sprintf("Converted: %s (%d rows) -> %s\n", basename(stats_csv), nrow(stats), basename(stats_pq)))
  rm(stats)
} else {
  cat("Not found:", stats_csv, "\n")
}

gc(verbose = FALSE)
cat("Done. Original CSVs left in place.\n")
