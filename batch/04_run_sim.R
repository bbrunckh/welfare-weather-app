# =============================================================================
# batch/04_run_sim.R
#
# Batch model fitting + simulations across countries and specifications.
# 
# Outputs:
#   OUT_DIR/model_fit/model_coefficients.csv
#   OUT_DIR/model_fit/model_fit_stats.csv
#   OUT_DIR/model_fit/_failures.csv (error logging)
#   OUT_DIR/simulations/outcomes.csv
#   OUT_DIR/simulations/sim_stats.csv
#   OUT_DIR/simulations/_failures.csv (error logging)
#
# All user inputs are set in SECTION 1. Vector-valued settings (marked [GRID])
# expand into separate runs via expand.grid(); scalar settings apply uniformly.
#
# =============================================================================

# Load helpers
pkgload::load_all(quiet = TRUE)
invisible(lapply(list.files("batch/R", pattern = "\\.R$", full.names = TRUE), source))

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
# SECTION 2 — RUN PIPELINE
# =============================================================================

cat("================================================================\n")
cat("WISE-APP Batch Pipeline\n")
cat(sprintf("OUT_DIR:  %s\n", OUT_DIR))
cat(sprintf("Unit:     %s\n", UNIT))
cat(sprintf("Filter:   %s\n", if (is.null(COUNTRY_FILTER)) "all countries"
            else paste(COUNTRY_FILTER, collapse = ", ")))
cat("================================================================\n\n")

cat("\n=== Step 1: Model fitting ===\n")

cat("\n=== Step 2: Climate simulations ===\n")

cat("\n=== Step 3: Policy simulations ===\n")
