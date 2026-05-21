# =============================================================================
# batch/04_run_all.R
#
# Orchestrator: runs the full WISE-APP batch pipeline in sequence.
# Sets all config once here, then sources 01 → 02 → 03 in order.
# Sub-scripts use if(!exists(...)) guards so they inherit these values.
#
# Usage: source("batch/04_run_all.R")
# =============================================================================

# =============================================================================
# SECTION 1 — UNIFIED CONFIGURATION
# (Set all values here; sub-scripts will not override them)
# =============================================================================

# ---- Data source ------------------------------------------------------------
CONNECTION_TYPE <- "local"
DATA_DIR        <- Sys.getenv("WISEAPP_DATA_PATH")
OUT_DIR         <- "dev/outputs/"

# ---- Unit of analysis -------------------------------------------------------
UNIT <- "hh"   # "hh", "ind", or "firm"

# ---- Country filter ---------------------------------------------------------
# NULL = all available countries; character vector = subset
COUNTRY_FILTER  <- NULL

# ---- Output options ---------------------------------------------------------
OVERWRITE_EXISTING <- TRUE

# ---- Weather specs (used by 02_weather_stats.R and 03_run_mod1.R) -----------
# NOTE: expand_weather_specs() is loaded below before the source() calls.
# Define specs after loading batch helpers so the function is available.

# ---- Model-specific config (used by 03_run_mod1.R) -------------------------
POOL_COUNTRIES <- FALSE
OUTCOME_NAME   <- "welfare"
CURRENCY       <- "PPP"
POVERTY_LINE   <- 3

WEATHER_TRANSFORMATION <- "None"
N_BINS               <- 5L
BINNING_METHOD       <- "Equal frequency"
CUSTOM_BREAKS        <- NULL
POLYNOMIAL           <- character(0)
WEATHER_AGG_OVERRIDE <- NULL

MODEL_TYPE   <- c("Linear regression", "Quantile regression (RIF)")
INTERACTIONS <- list(character(0))

FIXED_EFFECTS <- list(
  year_admin1 = c("year", "gaul1_code"),
  year_loc    = c("year", "loc_id_panel")
)

COVARIATE_SPECS <- list(
  hhsize_urban_area = list(
    method = "User-defined",
    ind = character(0), hh = c("hhsize", "urban"),
    firm = character(0), area = c("area_h3_7")
  )
)

LASSO_ALPHA         <- 1
LASSO_LAMBDA        <- "lambda.1se"
LASSO_NFOLDS        <- 10L
LASSO_STANDARDIZE   <- TRUE
MI_M                <- 5L
MI_MAXIT            <- 5L
STABILITY_THRESHOLD <- 0.5

LASSO_FORCE_IN  <- list(ind = character(0), hh = character(0),
                         firm = character(0), area = character(0))
LASSO_FORCE_OUT <- list(ind = character(0), hh = character(0),
                         firm = character(0), area = character(0))

SAVE_PLOTS         <- TRUE
SAVE_SUMMARY_STATS <- FALSE

# =============================================================================
# SECTION 2 — LOAD PACKAGES AND HELPERS
# =============================================================================

pkgload::load_all(quiet = TRUE)
invisible(lapply(list.files("batch/R", pattern = "\\.R$", full.names = TRUE), source))
.batch_loaded <- TRUE

# ---- Weather specs (defined after helpers are loaded) ----------------------
WEATHER_SPECS <- c(
  expand_weather_specs("t", c(1L, 3L, 6L, 12L), c("continuous"), c("None"), ref_starts = 1L),
  expand_weather_specs("r", c(1L, 3L, 6L, 12L), c("continuous"), c("None"), ref_starts = 1L)
)

# =============================================================================
# SECTION 3 — RUN PIPELINE
# =============================================================================

cat("================================================================\n")
cat("WISE-APP Batch Pipeline\n")
cat(sprintf("OUT_DIR:  %s\n", OUT_DIR))
cat(sprintf("Unit:     %s\n", UNIT))
cat(sprintf("Filter:   %s\n", if (is.null(COUNTRY_FILTER)) "all countries"
            else paste(COUNTRY_FILTER, collapse = ", ")))
cat("================================================================\n\n")

cat("=== Step 1: Survey stats ===\n")
source("batch/01_survey_stats.R", local = FALSE)

cat("\n=== Step 2: Weather stats ===\n")
source("batch/02_weather_stats.R", local = FALSE)

cat("\n=== Step 3: Model fitting ===\n")
source("batch/03_run_mod1.R", local = FALSE)

# Step 4 (simulations) — to be implemented in batch/04b_simulations.R

cat("\n================================================================\n")
cat("Pipeline complete.\n")
cat(sprintf("Outputs written to: %s\n", normalizePath(OUT_DIR, mustWork = FALSE)))
cat("================================================================\n")
