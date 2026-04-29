# dev/bench_rep_selection_setup.R
# --------------------------------
# One-time setup script for bench_representative_selection.R.
# Loads survey data, fits a benchmark model, and loads all ensemble weather.
# Results are cached to dev/outputs/bench_cache/ so the main benchmark script
# can be re-run without repeating expensive setup steps.
#
# Re-run this script only when:
#   - Switching survey (BENCH_CODE / BENCH_YEAR / BENCH_SURVNAME)
#   - Switching SSPs or future periods
#   - Switching outcome or weather variable
#   - After devtools::load_all() resets the session
#
# Usage:
#   devtools::load_all()
#   source("dev/bench_rep_selection_setup.R")
# ============================================================================ #

# ============================================================================ #
# 0 — Config — EDIT HERE                                                       #
# ============================================================================ #

BENCH_CODE      <- "GNB"
BENCH_YEAR      <- 2021L
BENCH_SURVNAME  <- "EHCVM"
BENCH_SOURCE    <- "GMD"
BENCH_OUTCOME   <- "welfare"
BENCH_WEATHER   <- "r"              # single variable confirmed in ERA5 for GNB
BENCH_PERTURB   <- "multiplicative"
BENCH_FE        <- "urban"          # fixed effect for bench model
BENCH_WEIGHT    <- "weight"         # weight column in survey data

BENCH_SSP <- c("ssp2_4_5", "ssp3_7_0")
BENCH_FP  <- list(
  c("2025-01-01", "2035-12-31"),
  c("2035-01-01", "2045-12-31")
)

CACHE_DIR <- "dev/outputs/bench_cache"

# ============================================================================ #
# 1 — Setup                                                                    #
# ============================================================================ #

if (!dir.exists(CACHE_DIR)) dir.create(CACHE_DIR, recursive = TRUE)

CP <- build_connection_params("local")

selected_surveys <- data.frame(
  code     = BENCH_CODE,
  year     = BENCH_YEAR,
  survname = BENCH_SURVNAME,
  source   = BENCH_SOURCE,
  stringsAsFactors = FALSE
)

selected_weather <- data.frame(
  name           = BENCH_WEATHER,
  ref_start      = 0L,
  ref_end        = 2L,
  temporalAgg    = "Mean",
  transformation = "None",
  cont_binned    = NA_character_,
  stringsAsFactors = FALSE
)

perturbation_method <- setNames(BENCH_PERTURB, BENCH_WEATHER)

# ============================================================================ #
# 2 — Survey data                                                              #
# ============================================================================ #

message("--- [1/3] Loading survey data ---")
svy_raw <- load_data(
  sprintf("microdata/hh/%s/%s_%s_%s_%s_hh.parquet",
          BENCH_CODE, BENCH_CODE, BENCH_YEAR, BENCH_SURVNAME, BENCH_SOURCE),
  CP,
  collect = TRUE
)
dates <- sort(unique(svy_raw$timestamp[!is.na(svy_raw$timestamp)]))
message(sprintf("  %d obs | %d timestamps", nrow(svy_raw), length(dates)))

# ============================================================================ #
# 3 — Weather (all ensemble members)                                           #
# ============================================================================ #

message("--- [2/3] Loading all ensemble weather ---")
t_load <- system.time({
  weather_result <- get_weather(
    survey_data         = svy_raw,
    selected_surveys    = selected_surveys,
    selected_weather    = selected_weather,
    dates               = dates,
    connection_params   = CP,
    ssp                 = BENCH_SSP,
    future_period       = BENCH_FP,
    perturbation_method = perturbation_method
  )
})
message(sprintf("  %d keys loaded in %.1fs", length(weather_result), t_load[["elapsed"]]))

# ============================================================================ #
# 4 — Model fit                                                                #
# ============================================================================ #

message("--- [3/3] Fitting benchmark model ---")

# Join historical weather onto survey for training
join_key   <- intersect(names(svy_raw), names(weather_result[["historical"]]))
train_data <- svy_raw |>
  dplyr::left_join(weather_result[["historical"]], by = join_key)

formula_str <- sprintf("%s ~ %s | %s", BENCH_OUTCOME, BENCH_WEATHER, BENCH_FE)
message(sprintf("  Formula: %s | weights: %s", formula_str, BENCH_WEIGHT))

bench_fit <- fixest::feols(
  as.formula(formula_str),
  data    = train_data,
  weights = as.formula(paste0("~", BENCH_WEIGHT)),
  vcov    = "hetero"
)

bench_mf <- list(
  fit3       = bench_fit,
  engine     = "fixest",
  train_data = train_data
)

bench_so <- data.frame(name = BENCH_OUTCOME, stringsAsFactors = FALSE)

# ============================================================================ #
# 5 — Cache everything                                                         #
# ============================================================================ #

message("--- Caching session objects ---")
saveRDS(svy_raw,          file.path(CACHE_DIR, "svy_raw.rds"))
saveRDS(weather_result,   file.path(CACHE_DIR, "weather_result.rds"))
saveRDS(bench_mf,         file.path(CACHE_DIR, "bench_mf.rds"))
saveRDS(bench_so,         file.path(CACHE_DIR, "bench_so.rds"))
saveRDS(dates,            file.path(CACHE_DIR, "dates.rds"))
saveRDS(selected_surveys, file.path(CACHE_DIR, "selected_surveys.rds"))
saveRDS(selected_weather, file.path(CACHE_DIR, "selected_weather.rds"))
saveRDS(perturbation_method, file.path(CACHE_DIR, "perturbation_method.rds"))

# Save config as metadata so benchmark script can display what was cached
config_df <- data.frame(
  key   = c("code", "year", "survname", "source", "outcome",
            "weather", "perturb", "fe", "weight",
            "ssp", "fp"),
  value = c(BENCH_CODE, BENCH_YEAR, BENCH_SURVNAME, BENCH_SOURCE,
            BENCH_OUTCOME, BENCH_WEATHER, BENCH_PERTURB, BENCH_FE,
            BENCH_WEIGHT,
            paste(BENCH_SSP, collapse = "|"),
            paste(vapply(BENCH_FP, paste, character(1L), collapse = ":"), collapse = "|")),
  stringsAsFactors = FALSE
)
write.csv(config_df, file.path(CACHE_DIR, "config.csv"), row.names = FALSE)

message(sprintf("  Cached to: %s", CACHE_DIR))
message("=== Setup complete — run bench_representative_selection.R ===")