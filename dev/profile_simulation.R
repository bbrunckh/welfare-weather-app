# =============================================================================
# dev/profile_simulation.R
#
# Profvis diagnostic for simulation pipeline.
# Run lightweight first, then heavy — compare flamegraphs.
#
# Run with: source("dev/profile_simulation.R")
# Requires: completed simulation in current app session
#           OR saved test fixtures in dev/fixtures/
# =============================================================================

pkgload::load_all()
library(profvis)

cat("========== Simulation profiler ==========\n\n")

# ---- 1. Check fixtures exist -----------------------------------------------
fixture_path <- "dev/fixtures/sim_inputs.rds"

if (!file.exists(fixture_path)) {
  cat("No fixtures found — creating from current session.\n")
  cat("Run the app, complete a simulation, then run:\n\n")
  cat('  saveRDS(list(\n')
  cat('    survey_data  = survey_wd_sim,\n')
  cat('    model        = fitted_model,\n')
  cat('    weather_data = isolate(selected_weather()),\n')
  cat('    pov_line     = 1.90\n')
  cat('  ), "dev/fixtures/sim_inputs.rds")\n\n')
  stop("Create fixtures first — see instructions above.")
}

fx <- readRDS(fixture_path)
cat("Fixtures loaded.\n")
cat("  survey_data rows: ", nrow(fx$survey_data), "\n")
cat("  weather_data keys:", length(unique(fx$weather_data$key)), "\n\n")

# ---- Helper — run N keys sequentially --------------------------------------
run_n_keys <- function(n_keys, n_draws, label) {
  all_keys <- unique(fx$weather_data$key)[seq_len(n_keys)]
  cat(sprintf("Running %s: %d key(s), S=%d...\n", label, n_keys, n_draws))

  for (key in all_keys) {
    wd_key <- dplyr::filter(fx$weather_data, key == !!key)

    # Step 1 — simulation pipeline per key
    pipeline <- run_sim_pipeline(
      key          = key,
      survey_data  = fx$survey_data,
      model        = fx$model,
      weather_data = wd_key,
      n_draws      = n_draws,
      pov_line     = fx$pov_line,
      vcov_spec    = COEF_VCOV_SPEC
    )

    # Step 2 — historical aggregation
    hist_agg <- compute_hist_agg(
      pipeline     = pipeline,
      n_draws      = n_draws,
      methods      = c("mean", "poverty"),
      weights      = NULL
    )

    # Step 3 — scenario aggregation
    scenario_agg <- compute_scenario_agg(
      pipeline     = pipeline,
      n_draws      = n_draws,
      methods      = c("mean", "poverty"),
      weights      = NULL
    )
  }
  cat(sprintf("  Done.\n\n"))
}

# ---- 2. Lightweight profile — 1 key, S=5 -----------------------------------
p_light <- profvis({
  run_n_keys(n_keys = 1L, n_draws = 5L, label = "LIGHTWEIGHT")
}, interval = 0.005)

cat("Lightweight profile complete.\n")
cat("Top functions by time:\n")
print(p_light$x$message$prof |>
  dplyr::group_by(f) |>
  dplyr::summarise(ms = n() * 5, .groups = "drop") |>
  dplyr::arrange(dplyr::desc(ms)) |>
  head(15))
cat("\n")

# ---- 3. Save profiles -------------------------------------------------------
saveRDS(p_light, "dev/fixtures/profile_light.rds")
cat("Profiles saved to dev/fixtures/\n")
cat("View with: print(readRDS('dev/fixtures/profile_light.rds'))\n\n")

cat("========== LIGHTWEIGHT complete ==========\n")
cat("Next: run HEAVY profile after reviewing lightweight results.\n")
cat("  source('dev/profile_simulation_heavy.R')\n")