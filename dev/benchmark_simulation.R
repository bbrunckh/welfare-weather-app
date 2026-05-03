# =============================================================================
# dev/benchmark_simulation.R
#
# Benchmarks fct_run_simulation() across varying key counts and S values.
# Uses saved fixtures — no app or UI needed.
#
# Usage:
#   1. Run app, complete a simulation, wait for [dev] Fixtures saved
#   2. Stop app
#   3. source("dev/benchmark_simulation.R")
# =============================================================================

pkgload::load_all(quiet = TRUE)

# ---- Load fixtures ----------------------------------------------------------
fx_path      <- "dev/fixtures/sim_inputs.rds"
fx_full_path <- "dev/fixtures/sim_inputs_full.rds"

stopifnot(
  "[benchmark] Run app and complete a simulation first." =
    file.exists(fx_path)
)

fx      <- readRDS(fx_path)
fx_full <- if (file.exists(fx_full_path)) {
  cat("Full fixtures found — Section 2 will run.\n")
  readRDS(fx_full_path)
} else {
  cat("Full fixtures not found — Section 2 will be skipped.\n")
  cat("Re-run app with updated fixture saving to enable Section 2.\n\n")
  NULL
}

cat("Slim fixtures loaded.\n")
cat("  hist_sim names:    ", paste(names(fx$hist_sim),    collapse = ", "), "\n")
cat("  scenario_sim names:", paste(names(fx$scenario_sim), collapse = ", "), "\n\n")

pipe_hist <- fx$hist_sim
pipe_sc   <- fx$scenario_sim
chol_1    <- fx$chol_obj

# ---- Benchmark parameters ---------------------------------------------------
S_values    <- c(1, 30L, 50L,  150L) #DRK Note: Using 1 as an approximation for S = 0.
key_counts  <- c(1L, 2L, 5L, 10L)
n_reps      <- 3L   # repeat each combo for stable timing

cat("=== Simulation benchmark ===\n")
cat(sprintf("S values:    %s\n", paste(S_values,   collapse = ", ")))
cat(sprintf("Key counts:  %s\n", paste(key_counts, collapse = ", ")))
cat(sprintf("Repetitions: %d\n\n", n_reps))

# ---- Helper — run compute_hist_agg for n_keys --------------------------------
run_benchmark <- function(n_keys, S, use_weights = FALSE) {
  # Use hist pipeline for all keys (simplest benchmark)
  times <- numeric(n_reps)
  for (rep in seq_len(n_reps)) {
    gc(verbose = FALSE)
    t <- system.time({
      for (ki in seq_len(n_keys)) {
        compute_hist_agg(
          pipeline = pipe_hist,
          chol_obj = chol_1,
          methods  = c("mean", "headcount_ratio"),
          S        = S,
          pov_line = fx$pov_line,
          is_log   = TRUE
        )
      }
    })
    times[[rep]] <- t[["elapsed"]]
  }
  mean(times)
}

# ---- Run benchmarks ---------------------------------------------------------
results <- expand.grid(
  n_keys = key_counts,
  S      = S_values
)
results$elapsed_s     <- NA_real_
results$per_key_s     <- NA_real_
results$proj_23_keys  <- NA_real_
results$proj_226_keys <- NA_real_

cat("Running benchmarks...\n")
for (i in seq_len(nrow(results))) {
  n_keys <- results$n_keys[[i]]
  S      <- results$S[[i]]
  cat(sprintf("  n_keys=%d, S=%d...", n_keys, S))
  elapsed <- run_benchmark(n_keys, S)
  results$elapsed_s[[i]]     <- round(elapsed, 2)
  results$per_key_s[[i]]     <- round(elapsed / n_keys, 3)
  results$proj_23_keys[[i]]  <- round(elapsed / n_keys * 23, 1)
  results$proj_226_keys[[i]] <- round(elapsed / n_keys * 226, 1)
  cat(sprintf(" %.2fs (%.3fs/key)\n", elapsed, elapsed / n_keys))
}

# ---- Summary table ----------------------------------------------------------
cat("\n=== Results ===\n")
cat(sprintf("%-8s %-6s %-12s %-12s %-14s %-16s\n",
            "n_keys", "S", "elapsed_s", "per_key_s",
            "proj_23keys_s", "proj_226keys_s"))
cat(strrep("-", 70), "\n")
for (i in seq_len(nrow(results))) {
  cat(sprintf("%-8d %-6d %-12.2f %-12.3f %-14.1f %-16.1f\n",
              results$n_keys[[i]],
              results$S[[i]],
              results$elapsed_s[[i]],
              results$per_key_s[[i]],
              results$proj_23_keys[[i]],
              results$proj_226_keys[[i]]))
}

# ---- Parallelisation projection ---------------------------------------------
n_cores <- parallelly::availableCores(logical = FALSE)
cat(sprintf("\n=== Parallelisation projection (%d physical cores) ===\n",
            n_cores))
cat(sprintf("%-6s %-14s %-16s %-16s %-16s\n",
            "S", "serial_226s", "4_cores_s", "8_cores_s", "12_cores_s"))
cat(strrep("-", 70), "\n")

for (S in S_values) {
  per_key <- results$per_key_s[results$S == S & results$n_keys == max(key_counts)]
  serial  <- round(per_key * 226, 1)
  cat(sprintf("%-6d %-14.1f %-16.1f %-16.1f %-16.1f\n",
              S,
              serial,
              round(serial / 4,  1),
              round(serial / 8,  1),
              round(serial / 12, 1)))
}

# ---- Section 2: Full run_sim_pipeline() timing ------------------------------
cat("\n=== Section 2: Full run_sim_pipeline() timing ===\n")
cat("(weather join + predict_outcome + Cholesky — everything before aggregation)\n\n")

if (is.null(fx_full)) {
  cat("  [SKIP] Full fixtures not found.\n")
  cat("  Update mod_2_01_weathersim.R fixture saving to include:\n")
  cat("    survey_data, model, engine, train_data, sw, so, residuals\n\n")
  t_pipeline_mean <- NA_real_
} else {
  # Confirm required objects present
  required <- c("survey_data", "model", "engine", "train_data",
                "sw", "so", "residuals", "chol_obj")
  missing  <- setdiff(required, names(fx_full))
  if (length(missing) > 0) {
    cat("  [SKIP] Missing from full fixtures:", paste(missing, collapse = ", "), "\n\n")
    t_pipeline_mean <- NA_real_
  } else {
    cat(sprintf("  survey_data rows: %d\n", nrow(fx_full$survey_data)))
    cat(sprintf("  weather_raw rows: %d\n",
                nrow(fx_full$hist_sim$weather_raw %||% data.frame())))

    # Time run_sim_pipeline — 3 reps
    t_pipeline <- numeric(3L)
    for (rep in seq_len(3L)) {
      gc(verbose = FALSE)
      cat(sprintf("  rep %d...", rep))
      t_pipeline[[rep]] <- system.time(
        run_sim_pipeline(
          weather_raw = fx_full$hist_sim$weather_raw,
          svy         = fx_full$survey_data,
          sw          = fx_full$sw,
          so          = fx_full$so,
          model       = fx_full$model,
          residuals   = fx_full$residuals,
          train_data  = fx_full$train_data,
          engine      = fx_full$engine,
          chol_obj    = fx_full$chol_obj
        )
      )[["elapsed"]]
      cat(sprintf(" %.3fs\n", t_pipeline[[rep]]))
    }
    t_pipeline_mean <- mean(t_pipeline)
    cat(sprintf("\n  Mean run_sim_pipeline():  %.3fs/key\n",   t_pipeline_mean))
    cat(sprintf("  Mean compute_hist_agg():  %.3fs/key (S=30)\n",
                results$per_key_s[results$S == 30 & results$n_keys == 10]))
    cat(sprintf("  Total per key (S=30):     %.3fs\n",
                t_pipeline_mean +
                results$per_key_s[results$S == 30 & results$n_keys == 10]))
    cat(sprintf("\n  Projected 23 keys (S=30):  %.1fs\n",
                (t_pipeline_mean +
                 results$per_key_s[results$S == 30 & results$n_keys == 10]) * 23))
    cat(sprintf("  Projected 226 keys (S=30): %.1fs\n",
                (t_pipeline_mean +
                 results$per_key_s[results$S == 30 & results$n_keys == 10]) * 226))
  }
}

# ---- Section 3: Full end-to-end timing estimate ----------------------------
cat("\n=== Section 3: Full end-to-end time breakdown ===\n\n")

t_agg_per_key      <- results$per_key_s[results$S == 30 & results$n_keys == 10]
t_pipeline_per_key <- if (!is.na(t_pipeline_mean)) t_pipeline_mean else 0.5

cat(sprintf("%-40s %-15s %-15s\n", "Step", "Per key", "23 keys"))
cat(strrep("-", 70), "\n")
cat(sprintf("%-40s %-15s %-15s\n",
            "get_weather() DuckDB query",
            "N/A (once)",
            "~5-15s"))
cat(sprintf("%-40s %-15s %-15s\n",
            "run_sim_pipeline() (predict + Cholesky)",
            sprintf("~%.3fs", t_pipeline_per_key),
            sprintf("~%.1fs", t_pipeline_per_key * 23)))
cat(sprintf("%-40s %-15s %-15s\n",
            "compute_hist_agg() S=30",
            sprintf("~%.3fs", t_agg_per_key),
            sprintf("~%.1fs", t_agg_per_key * 23)))
cat(sprintf("%-40s %-15s %-15s\n",
            "compute_scenario_agg() S=30",
            sprintf("~%.3fs", t_agg_per_key),
            sprintf("~%.1fs", t_agg_per_key * 23)))
cat(sprintf("%-40s %-15s %-15s\n",
            "Shiny reactive + render",
            "N/A (once)",
            "~2-5s"))
cat(strrep("-", 70), "\n")
cat(sprintf("%-40s %-15s %-15s\n",
            "TOTAL (S=30)",
            sprintf("~%.2fs", t_pipeline_per_key + t_agg_per_key * 2),
            sprintf("~%.0f-%.0fs",
                    10 + (t_pipeline_per_key + t_agg_per_key * 2) * 23 + 2,
                    15 + (t_pipeline_per_key + t_agg_per_key * 2) * 23 + 5)))

# ---- Parallelisation projection with full timing ---------------------------
cat("\n=== Parallelisation projection (full pipeline, S=30) ===\n")
t_total_per_key <- t_pipeline_per_key + t_agg_per_key * 2
cat(sprintf("%-12s %-12s %-12s %-12s %-12s\n",
            "n_keys", "serial_s", "4_cores_s", "8_cores_s", "12_cores_s"))
cat(strrep("-", 60), "\n")
for (nk in c(23L, 100L, 226L)) {
  serial <- t_total_per_key * nk
  cat(sprintf("%-12d %-12.1f %-12.1f %-12.1f %-12.1f\n",
              nk, serial,
              serial / 4, serial / 8, serial / 12))
}

# ---- Save results -----------------------------------------------------------
saveRDS(results, "dev/fixtures/benchmark_results.rds")
cat("\nResults saved to dev/fixtures/benchmark_results.rds\n")
cat("========== Benchmark complete ==========\n")