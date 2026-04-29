# dev/bench_representative_selection.R
# -------------------------------------
# Phase 2: Validate and benchmark select_representative_models() approach.
#
# Loads cached session objects from dev/outputs/bench_cache/ (built by
# bench_rep_selection_setup.R). Re-run setup only when switching surveys,
# SSPs, or variables.
#
# Usage:
#   devtools::load_all()
#   source("dev/bench_rep_selection_setup.R")   # once per config change
#   source("dev/bench_representative_selection.R")   # run as many times as needed
#
# Outputs:
#   dev/outputs/rep_selection_{timestamp}/welfare_rankings.csv
#   dev/outputs/rep_selection_{timestamp}/representative_models.csv
#   dev/outputs/rep_selection_{timestamp}/timing_summary.csv
# ============================================================================ #

# ============================================================================ #
# 0 — Optional overrides                                                       #
# Edit these to experiment without re-running setup.                           #
# Set to NULL to use whatever was cached.                                      #
# ============================================================================ #

OVERRIDE_SSP <- NULL    # e.g. c("ssp2_4_5") to test a single SSP
OVERRIDE_FP  <- NULL    # e.g. list(c("2025-01-01", "2035-12-31"))

# Outlier handling: exclude models beyond this many SDs from the group mean
# Set to Inf to disable
OUTLIER_SD_THRESHOLD <- Inf 

# ============================================================================ #
# 1 — Load cache                                                               #
# ============================================================================ #

CACHE_DIR <- "dev/outputs/bench_cache"

if (!dir.exists(CACHE_DIR)) {
  stop(paste0(
    "[bench] Cache not found at '", CACHE_DIR, "'.\n",
    "  Run dev/bench_rep_selection_setup.R first."
  ))
}

message("=== bench_representative_selection.R ===")
message("--- Loading cached objects ---")

svy_raw             <- readRDS(file.path(CACHE_DIR, "svy_raw.rds"))
weather_result      <- readRDS(file.path(CACHE_DIR, "weather_result.rds"))
bench_mf            <- readRDS(file.path(CACHE_DIR, "bench_mf.rds"))
bench_so            <- readRDS(file.path(CACHE_DIR, "bench_so.rds"))
dates               <- readRDS(file.path(CACHE_DIR, "dates.rds"))
selected_surveys    <- readRDS(file.path(CACHE_DIR, "selected_surveys.rds"))
selected_weather    <- readRDS(file.path(CACHE_DIR, "selected_weather.rds"))
perturbation_method <- readRDS(file.path(CACHE_DIR, "perturbation_method.rds"))

config_df <- read.csv(file.path(CACHE_DIR, "config.csv"), stringsAsFactors = FALSE)
message("  Cached config:")
for (i in seq_len(nrow(config_df))) {
  message(sprintf("    %-12s: %s", config_df$key[i], config_df$value[i]))
}

model      <- bench_mf$fit3
engine     <- bench_mf$engine
train_data <- bench_mf$train_data
so         <- bench_so

# Apply overrides
future_keys <- setdiff(names(weather_result), "historical")

if (!is.null(OVERRIDE_SSP)) {
  keep_pattern <- paste(OVERRIDE_SSP, collapse = "|")
  future_keys  <- future_keys[grepl(keep_pattern, future_keys)]
  message(sprintf("  OVERRIDE_SSP applied: %d keys retained", length(future_keys)))
}

# ============================================================================ #
# 2 — Output dir                                                               #
# ============================================================================ #

out_dir <- file.path(
  "dev/outputs",
  paste0("rep_selection_", format(Sys.time(), "%Y%m%d_%H%M%S"))
)
dir.create(out_dir, recursive = TRUE)

# ============================================================================ #
# 3 — Point-estimate welfare loop                                              #
# ============================================================================ #

message(sprintf("\n--- Point-estimate welfare loop (%d keys) ---", length(future_keys)))

parse_key <- function(key) {
  pfx   <- sub("^((ssp[^_]+_[0-9]+_[0-9]+_[0-9]+_[0-9]+))_.*$", "\\1", key)
  model <- sub(paste0("^", pfx, "_"), "", key)
  list(prefix = pfx, model = model)
}

parsed   <- lapply(future_keys, parse_key)
prefixes <- vapply(parsed, `[[`, character(1L), "prefix")

welfare_records <- list()
t_loop <- system.time({
  for (i in seq_along(future_keys)) {
    key <- future_keys[[i]]

    t0  <- proc.time()[["elapsed"]]
    out <- tryCatch(
      run_sim_pipeline(
        weather_raw = weather_result[[key]],
        svy         = svy_raw,
        sw          = selected_weather,
        so          = so,
        model       = model,
        residuals   = "none",
        train_data  = train_data,
        engine      = engine,
        chol_obj    = NULL
      ),
      error = function(e) {
        warning(sprintf("[bench] run_sim_pipeline failed for %s: %s",
                        key, conditionMessage(e)))
        NULL
      }
    )
    t1 <- proc.time()[["elapsed"]]

    welfare_score <- if (!is.null(out)) {
      if (!is.null(out$weight)) {
        weighted.mean(out$y_point, w = out$weight, na.rm = TRUE)
      } else {
        mean(out$y_point, na.rm = TRUE)
      }
    } else {
      NA_real_
    }

    welfare_records[[i]] <- data.frame(
      key           = key,
      prefix        = prefixes[[i]],
      model_name    = parsed[[i]]$model,
      welfare_score = welfare_score,
      elapsed_s     = round(t1 - t0, 2),
      stringsAsFactors = FALSE
    )

    message(sprintf("  [%02d/%02d] %s | welfare=%.4f | %.2fs",
                    i, length(future_keys),
                    parsed[[i]]$model, welfare_score, t1 - t0))

    rm(out); gc(verbose = FALSE)
  }
})

welfare_df <- do.call(rbind, welfare_records)
message(sprintf("\n  Loop complete: %.1fs total | %.2fs per model",
                t_loop[["elapsed"]], t_loop[["elapsed"]] / length(future_keys)))

# ============================================================================ #
# 4 — Optional outlier exclusion                                               #
# ============================================================================ #

if (is.finite(OUTLIER_SD_THRESHOLD)) {
  n_before <- sum(!is.na(welfare_df$welfare_score))
  welfare_df <- welfare_df |>
    dplyr::group_by(prefix) |>
    dplyr::mutate(
      grp_mean = mean(welfare_score, na.rm = TRUE),
      grp_sd   = sd(welfare_score, na.rm = TRUE),
      outlier  = abs(welfare_score - grp_mean) > OUTLIER_SD_THRESHOLD * grp_sd
    ) |>
    dplyr::mutate(welfare_score = dplyr::if_else(outlier, NA_real_, welfare_score)) |>
    dplyr::select(-grp_mean, -grp_sd, -outlier) |>
    dplyr::ungroup()
  n_after <- sum(!is.na(welfare_df$welfare_score))
  message(sprintf("\n  Outlier exclusion (>%.1f SD): %d models excluded",
                  OUTLIER_SD_THRESHOLD, n_before - n_after))
}

# ============================================================================ #
# 5 — Rank and select representatives                                          #
# ============================================================================ #

message("\n--- Ranking and selecting representatives ---")

rep_records <- list()

for (pfx in unique(prefixes)) {
  grp <- welfare_df[welfare_df$prefix == pfx & !is.na(welfare_df$welfare_score), ]
  grp <- grp[order(grp$welfare_score), ]
  n   <- nrow(grp)

  if (n == 0L) {
    warning(sprintf("[bench] No valid scores for %s — skipping", pfx))
    next
  }

  rep_lo     <- grp$model_name[[1L]]
  rep_q10    <- grp$model_name[[which.min(abs(grp$welfare_score - quantile(grp$welfare_score, 0.10)))]]
  rep_q25    <- grp$model_name[[which.min(abs(grp$welfare_score - quantile(grp$welfare_score, 0.25)))]]
  rep_median <- grp$model_name[[which.min(abs(grp$welfare_score - quantile(grp$welfare_score, 0.50)))]]
  rep_mean   <- grp$model_name[[which.min(abs(grp$welfare_score - mean(grp$welfare_score)))]]
  rep_q75    <- grp$model_name[[which.min(abs(grp$welfare_score - quantile(grp$welfare_score, 0.75)))]]
  rep_q90    <- grp$model_name[[which.min(abs(grp$welfare_score - quantile(grp$welfare_score, 0.90)))]]
  rep_hi     <- grp$model_name[[n]]

  message(sprintf("  %s (%d models):", pfx, n))
  message(sprintf("    lo     (worst)  : %-55s [%.2f]", rep_lo,     grp$welfare_score[[1L]]))
  message(sprintf("    q10             : %-55s [%.2f]", rep_q10,    quantile(grp$welfare_score, 0.10)))
  message(sprintf("    q25             : %-55s [%.2f]", rep_q25,    quantile(grp$welfare_score, 0.25)))
  message(sprintf("    median          : %-55s [%.2f]", rep_median, quantile(grp$welfare_score, 0.50)))
  message(sprintf("    mean            : %-55s [%.2f]", rep_mean,   mean(grp$welfare_score)))
  message(sprintf("    q75             : %-55s [%.2f]", rep_q75,    quantile(grp$welfare_score, 0.75)))
  message(sprintf("    q90             : %-55s [%.2f]", rep_q90,    quantile(grp$welfare_score, 0.90)))
  message(sprintf("    hi     (best)   : %-55s [%.2f]", rep_hi,     grp$welfare_score[[n]]))
  message(sprintf("    spread: %.2f (%.1f%%)",
                  spread, 100 * spread / abs(mean(grp$welfare_score))))

  rep_records[[pfx]] <- data.frame(
    prefix     = pfx,
    role       = c("lo", "q10", "q25", "median", "mean", "q75", "q90", "hi"),
    model_name = c(rep_lo, rep_q10, rep_q25, rep_median,
                  rep_mean, rep_q75, rep_q90, rep_hi),
    welfare    = c(
      grp$welfare_score[[1L]],
      quantile(grp$welfare_score, 0.10),
      quantile(grp$welfare_score, 0.25),
      quantile(grp$welfare_score, 0.50),
      mean(grp$welfare_score),
      quantile(grp$welfare_score, 0.75),
      quantile(grp$welfare_score, 0.90),
      grp$welfare_score[[n]]
    ),
    n_models   = n,
    spread_pct = round(100 * spread / abs(mean(grp$welfare_score)), 2),
    stringsAsFactors = FALSE
  )
}

rep_df <- do.call(rbind, rep_records)

message("\n  Representative summary:")
print(rep_df, row.names = FALSE)

# ============================================================================ #
# 6 — Timing summary                                                           #
# ============================================================================ #

timing_df <- data.frame(
  phase         = c("point_estimate_loop", "per_model_avg"),
  elapsed_s     = c(round(t_loop[["elapsed"]], 1),
                    round(t_loop[["elapsed"]] / length(future_keys), 2)),
  n             = c(length(future_keys), 1L),
  stringsAsFactors = FALSE
)
print(timing_df, row.names = FALSE)

# ============================================================================ #
# 7 — Write outputs                                                            #
# ============================================================================ #

write.csv(welfare_df, file.path(out_dir, "welfare_rankings.csv"),       row.names = FALSE)
write.csv(rep_df,     file.path(out_dir, "representative_models.csv"),  row.names = FALSE)
write.csv(timing_df,  file.path(out_dir, "timing_summary.csv"),         row.names = FALSE)

message(sprintf("\n=== Complete. Outputs in: %s ===", out_dir))