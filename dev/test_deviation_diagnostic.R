# =============================================================================
# dev/test_deviation_diagnostic.R
#
# Diagnostic for deviation pipeline — tests hero plot, exceedance curve, and
# outcome table without running the full Shiny app.
#
# Confirms whether deviation is applied correctly (display-only shift) or
# incorrectly (shape-changing upstream mutation) for each output.
#
# Run with: source("dev/test_deviation_diagnostic.R")
# =============================================================================

pkgload::load_all()
library(ggplot2)

cat("\n========== Deviation diagnostic ==========\n\n")

# ---- Shared helpers ---------------------------------------------------------

.check <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) {
    cat("  [FAIL]", label, "\n        ERROR:", conditionMessage(e), "\n")
    invisible(NULL)
  })
  if (!is.null(result)) cat("  [PASS]", label, "\n")
  invisible(result)
}

# ---- 1. Synthetic data — mimics aggregate_sim_preds() output ---------------
# S = 30 coefficient draws per year, 30 historical years, 23 future years
# draw_values is a list-column: one vector of length (n_obs * S) per year

set.seed(42)
N_OBS   <- 500L   # household obs per year
N_YEARS <- 30L    # historical years
N_FUT   <- 23L    # future years
S       <- 30L    # coefficient draws

make_year_draws <- function(n_obs, s, true_mean, sd = 0.4) {
  # S draw-level predictions per obs: returns vector of length n_obs * S
  as.numeric(vapply(seq_len(s), function(si)
    exp(rnorm(n_obs, log(true_mean) + rnorm(1, 0, 0.08), sd)),
    numeric(n_obs)))
}

make_agg_tbl <- function(n_years, base_mean, shift = 0, seed = 1) {
  set.seed(seed)
  tibble::tibble(
    sim_year    = seq_len(n_years),
    value       = exp(rnorm(n_years, log(base_mean + shift), 0.12)),
    draw_values = lapply(sim_year, function(y)
                    make_year_draws(N_OBS, S, base_mean + shift)),
    value_all   = lapply(sim_year, function(y)
                    exp(rnorm(N_OBS, log(base_mean + shift), 0.4))),
    coef_lo     = value * 0.92,
    coef_hi     = value * 1.08,
    model_lo    = value * 0.85,
    model_hi    = value * 1.15
  )
}

hist_raw  <- make_agg_tbl(N_YEARS, base_mean = 4.3,  seed = 1)
fut_raw   <- make_agg_tbl(N_FUT,   base_mean = 4.3, shift = 0.4, seed = 2)

hist_ref  <- mean(hist_raw$value, na.rm = TRUE)
cat("hist_ref (deviation baseline):", round(hist_ref, 4), "\n\n")

# ---- 2. Configuration -------------------------------------------------------

bq  <- c(lo = 0.10, hi = 0.90)   # p10_p90 band
ebq <- c(lo = 0.00, hi = 1.00)   # full ensemble

# ---- Section 1: Hero plot (point range) ------------------------------------
cat("=== Section 1: Hero plot (point range) ===\n")

# Deviated versions — simulate what agg_hist() / agg_scenarios() produce
hist_dev <- dplyr::mutate(hist_raw, value = value - hist_ref)
fut_dev  <- dplyr::mutate(fut_raw,  value = value - hist_ref)

cat("  Historical central mean (raw):      ", round(mean(hist_raw$value), 4), "\n")
cat("  Historical central mean (deviated): ", round(mean(hist_dev$value), 4), "(expect ~0)\n")
cat("  Future central mean (raw):          ", round(mean(fut_raw$value),  4), "\n")
cat("  Future central mean (deviated):     ", round(mean(fut_dev$value),  4), "(expect ~", round(mean(fut_raw$value) - hist_ref, 4), ")\n")

stopifnot(
  "FAIL: deviated hist mean should be ~0" =
    abs(mean(hist_dev$value)) < 0.05,
  "FAIL: deviated future mean should shift by same hist_ref" =
    abs(mean(fut_dev$value) - (mean(fut_raw$value) - hist_ref)) < 0.01
)
cat("  [PASS] Hero plot central values deviate correctly.\n\n")

# Hero plot whiskers — compute_bands_from_raw() equivalent
compute_band_edges <- function(tbl, bq, hist_ref = 0) {
  dplyr::mutate(tbl,
    value_lo = purrr::map_dbl(draw_values,
      ~quantile(.x, bq["lo"], na.rm = TRUE) - hist_ref),
    value_hi = purrr::map_dbl(draw_values,
      ~quantile(.x, bq["hi"], na.rm = TRUE) - hist_ref)
  )
}

hist_bands_raw <- compute_band_edges(hist_raw, bq, hist_ref = 0)
hist_bands_dev <- compute_band_edges(hist_raw, bq, hist_ref = hist_ref)
fut_bands_raw  <- compute_band_edges(fut_raw,  bq, hist_ref = 0)
fut_bands_dev  <- compute_band_edges(fut_raw,  bq, hist_ref = hist_ref)

cat("  Historical whisker lo (raw):      ", round(mean(hist_bands_raw$value_lo), 4), "\n")
cat("  Historical whisker lo (deviated): ", round(mean(hist_bands_dev$value_lo), 4), "\n")
cat("  Future whisker lo (raw):          ", round(mean(fut_bands_raw$value_lo),  4), "\n")
cat("  Future whisker lo (deviated):     ", round(mean(fut_bands_dev$value_lo),  4), "\n")

# KEY TEST: are whiskers and central shifted by the SAME amount?
hist_central_shift <- mean(hist_dev$value)   - mean(hist_raw$value)
hist_whisker_shift <- mean(hist_bands_dev$value_lo) - mean(hist_bands_raw$value_lo)
fut_central_shift  <- mean(fut_dev$value)    - mean(fut_raw$value)
fut_whisker_shift  <- mean(fut_bands_dev$value_lo)  - mean(fut_bands_raw$value_lo)

cat("\n  Historical: central shift =", round(hist_central_shift, 4),
    "| whisker shift =", round(hist_whisker_shift, 4), "\n")
cat("  Future:     central shift =", round(fut_central_shift,  4),
    "| whisker shift =", round(fut_whisker_shift,  4), "\n")

if (abs(hist_central_shift - hist_whisker_shift) < 0.001) {
  cat("  [PASS] Historical whiskers shift by same amount as central.\n")
} else {
  cat("  [WARN] Historical whiskers and central shift by DIFFERENT amounts — bug!\n")
}
if (abs(fut_central_shift - fut_whisker_shift) < 0.001) {
  cat("  [PASS] Future whiskers shift by same amount as central.\n\n")
} else {
  cat("  [WARN] Future whiskers and central shift by DIFFERENT amounts — bug!\n\n")
}

# ---- Section 2: Exceedance curve -------------------------------------------
cat("=== Section 2: Exceedance curve ===\n")

# Compute ribbon from RAW data, then apply deviation to output — correct approach
hist_ribbon_raw <- .check(
  "compute_exceedance_ribbon() on raw hist_tbl",
  compute_exceedance_ribbon(hist_raw, band_q = bq)
)
fut_ribbon_raw  <- .check(
  "compute_exceedance_ribbon() on raw fut_tbl",
  compute_exceedance_ribbon(fut_raw,  band_q = bq)
)

if (!is.null(hist_ribbon_raw) && !is.null(fut_ribbon_raw)) {
    # First confirm actual column names — drives which columns need shifting
  ribbon_cols <- names(hist_ribbon_raw)
  cat("\n  Actual ribbon columns:", paste(ribbon_cols, collapse = ", "), "\n")

  # Shift all numeric columns except exceedance probability
  shift_ribbon <- function(r) {
    num_cols <- setdiff(names(r)[vapply(r, is.numeric, logical(1))], "exceedance")
    dplyr::mutate(r, dplyr::across(dplyr::all_of(num_cols), ~.x - hist_ref))
  }

  hist_ribbon_dev <- shift_ribbon(hist_ribbon_raw)
  fut_ribbon_dev  <- shift_ribbon(fut_ribbon_raw)

  cat("\n  Exceedance ribbon column names:", paste(names(hist_ribbon_raw), collapse = ", "), "\n")

  # KEY TEST: ribbon shape (band_hi - band_lo spread) unchanged by deviation
  # Use welfare_lo/welfare_hi or band_lo/band_hi — whichever exists
  lo_col <- intersect(c("band_lo", "welfare_lo"), ribbon_cols)[1]
  hi_col <- intersect(c("band_hi", "welfare_hi"), ribbon_cols)[1]

  spread_raw <- mean(hist_ribbon_raw[[hi_col]] - hist_ribbon_raw[[lo_col]], na.rm = TRUE)
  spread_dev <- mean(hist_ribbon_dev[[hi_col]] - hist_ribbon_dev[[lo_col]], na.rm = TRUE)
  cat("  Historical ribbon spread (raw):      ", round(spread_raw, 4), "\n")
  cat("  Historical ribbon spread (deviated): ", round(spread_dev, 4), "\n")

  if (abs(spread_raw - spread_dev) < 0.001) {
    cat("  [PASS] Ribbon shape unchanged by deviation — display-only shift.\n\n")
  } else {
    cat("  [WARN] Ribbon shape CHANGES with deviation — bug!\n\n")
  }

  # Also confirm column names for the mutate in mod_2_02_results.R
# Confirm correct column names used in deviation mutate
expected_in_mod <- c("welfare_mid", "welfare_lo", "welfare_hi")
cat("  Checking column names used in mod_2_02_results.R deviation mutate:\n")
for (col in expected_in_mod) {
  if (col %in% ribbon_cols) {
    cat("    [PASS] column '", col, "' exists in ribbon output.\n", sep = "")
  } else {
    cat("    [FAIL] column '", col, "' MISSING from ribbon output.\n", sep = "")
    cat("    Actual columns:", paste(ribbon_cols, collapse = ", "), "\n")
  }
}
  cat("\n")
}

# ---- Section 3: Outcome table ----------------------------------------------
cat("=== Section 3: Outcome table ===\n")

series_raw <- list(
  Historical              = list(out = hist_raw, x_label = "Mean"),
  `SSP3-7.0 / 2025-2035` = list(out = fut_raw,  x_label = "Mean")
)

hist_dev_central <- dplyr::mutate(hist_raw, value = value - hist_ref)
fut_dev_central  <- dplyr::mutate(fut_raw,  value = value - hist_ref)

series_dev_central <- list(
  Historical              = list(out = hist_dev_central, x_label = "Mean — deviation"),
  `SSP3-7.0 / 2025-2035` = list(out = fut_dev_central,  x_label = "Mean — deviation")
)

# With — expose full error:
tbl_raw <- tryCatch(
  build_threshold_table_df(series_raw,
    band_q = bq, ensemble_band_q = ebq,
    hist_ref = 0),
  error = function(e) {
    cat("  [FAIL] build_threshold_table_df() ERROR:\n  ", conditionMessage(e), "\n")
    NULL
  }
)
tbl_dev <- tryCatch(
  build_threshold_table_df(series_dev_central,
    band_q = bq, ensemble_band_q = ebq,
    hist_ref = hist_ref),
  error = function(e) {
    cat("  [FAIL] build_threshold_table_df() ERROR:\n  ", conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(tbl_raw) && !is.null(tbl_dev)) {
  cat("\n  Raw table:\n")
  print(tbl_raw[, c("Scenario", "Estimate", "1:5", "1:1", "4:5")])

  cat("\n  Deviated table:\n")
  print(tbl_dev[, c("Scenario", "Estimate", "1:5", "1:1", "4:5")])

  # KEY TESTS
  hist_central_raw <- as.numeric(tbl_raw[tbl_raw$Scenario == "Historical" &
                                          tbl_raw$Estimate == "Central", "1:1"])
  hist_central_dev <- as.numeric(tbl_dev[tbl_dev$Scenario == "Historical" &
                                          tbl_dev$Estimate == "Central", "1:1"])
  hist_upper_raw   <- as.numeric(tbl_raw[tbl_raw$Scenario == "Historical" &
                                          tbl_raw$Estimate == "Upper",   "1:1"])
  hist_upper_dev   <- as.numeric(tbl_dev[tbl_dev$Scenario == "Historical" &
                                          tbl_dev$Estimate == "Upper",   "1:1"])
  fut_upper_raw    <- as.numeric(tbl_raw[tbl_raw$Scenario == "SSP3-7.0 / 2025-2035" &
                                          tbl_raw$Estimate == "Upper",   "1:1"])
  fut_upper_dev    <- as.numeric(tbl_dev[tbl_dev$Scenario == "SSP3-7.0 / 2025-2035" &
                                          tbl_dev$Estimate == "Upper",   "1:1"])

  cat("\n  Historical Central 1:1 (raw):", round(hist_central_raw, 4),
      "| (deviated):", round(hist_central_dev, 4),
      "| diff:", round(hist_central_dev - hist_central_raw, 4),
      "(expect ~", round(-hist_ref, 4), ")\n")
  cat("  Historical Upper   1:1 (raw):", round(hist_upper_raw, 4),
      "| (deviated):", round(hist_upper_dev, 4),
      "| diff:", round(hist_upper_dev - hist_upper_raw, 4),
      "(expect ~", round(-hist_ref, 4), ")\n")
  cat("  Future    Upper    1:1 (raw):", round(fut_upper_raw, 4),
      "| (deviated):", round(fut_upper_dev, 4),
      "| diff:", round(fut_upper_dev - fut_upper_raw, 4),
      "(expect ~", round(-hist_ref, 4), ")\n\n")

  central_ok <- abs((hist_central_dev - hist_central_raw) - (-hist_ref)) < 0.05
  upper_ok   <- abs((hist_upper_dev   - hist_upper_raw)   - (-hist_ref)) < 0.05
  fut_ok     <- abs((fut_upper_dev    - fut_upper_raw)    - (-hist_ref)) < 0.05

  if (central_ok) cat("  [PASS] Central row shifts correctly.\n") else
    cat("  [FAIL] Central row does NOT shift correctly.\n")
  if (upper_ok)   cat("  [PASS] Historical Upper row shifts correctly.\n") else
    cat("  [FAIL] Historical Upper row does NOT shift correctly — draw_curves_tbl not deviated.\n")
  if (fut_ok)     cat("  [PASS] Future Upper row shifts correctly.\n") else
    cat("  [FAIL] Future Upper row does NOT shift correctly.\n")
}

# ---- Section 4: Trace build_threshold_table_df() Central row source --------
cat("=== Section 4: Central row source trace ===\n")

# Simulate what all_series() produces after deviation
hist_dev2 <- dplyr::mutate(hist_raw, value = value - hist_ref)
fut_dev2  <- dplyr::mutate(fut_raw,  value = value - hist_ref)

series_full_dev <- list(
  Historical              = list(out = hist_dev2, x_label = "dev"),
  `SSP3-7.0 / 2025-2035` = list(out = fut_dev2,  x_label = "dev")
)
series_full_raw <- list(
  Historical              = list(out = hist_raw, x_label = "raw"),
  `SSP3-7.0 / 2025-2035` = list(out = fut_raw,  x_label = "raw")
)

# What does $out$value contain for Future?
cat("  Future $out$value mean (raw):      ",
    round(mean(series_full_raw[["SSP3-7.0 / 2025-2035"]]$out$value), 4), "\n")
cat("  Future $out$value mean (deviated): ",
    round(mean(series_full_dev[["SSP3-7.0 / 2025-2035"]]$out$value), 4), "\n")

# Build tables from both
tbl_full_raw <- tryCatch(
  build_threshold_table_df(series_full_raw,
    band_q = bq, ensemble_band_q = ebq, hist_ref = 0),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })

tbl_full_dev <- tryCatch(
  build_threshold_table_df(series_full_dev,
    band_q = bq, ensemble_band_q = ebq, hist_ref = hist_ref),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })

if (!is.null(tbl_full_raw) && !is.null(tbl_full_dev)) {
  get_val <- function(tbl, scenario, estimate, col = "1:1") {
    as.numeric(tbl[tbl$Scenario == scenario & tbl$Estimate == estimate, col])
  }

  cat("\n  --- Raw table ---\n")
  print(tbl_full_raw[, c("Scenario", "Estimate", "1:5", "1:1", "4:5")])

  cat("\n  --- Deviated table ---\n")
  print(tbl_full_dev[, c("Scenario", "Estimate", "1:5", "1:1", "4:5")])

  fut_central_raw <- get_val(tbl_full_raw, "SSP3-7.0 / 2025-2035", "Central")
  fut_central_dev <- get_val(tbl_full_dev, "SSP3-7.0 / 2025-2035", "Central")
  fut_upper_raw   <- get_val(tbl_full_raw, "SSP3-7.0 / 2025-2035", "Upper")
  fut_upper_dev   <- get_val(tbl_full_dev, "SSP3-7.0 / 2025-2035", "Upper")

  cat("\n  Future Central diff:", round(fut_central_dev - fut_central_raw, 4),
      "(expect ~", round(-hist_ref, 4), ")\n")
  cat("  Future Upper   diff:", round(fut_upper_dev   - fut_upper_raw,   4),
      "(expect ~", round(-hist_ref, 4), ")\n")

  if (abs((fut_central_dev - fut_central_raw) - (-hist_ref)) < 0.05) {
    cat("  [PASS] Future Central deviates correctly via $out$value\n")
  } else {
    cat("  [FAIL] Future Central NOT deviating — build_threshold_table_df()",
        "not reading $out$value for Central row\n")
    cat("  >> Check: does build_threshold_table_df() use out$value or",
        "out$draw_values for Central?\n")
  }

  if (abs((fut_upper_dev - fut_upper_raw) - (-hist_ref)) < 0.05) {
    cat("  [PASS] Future Upper deviates correctly via hist_ref parameter\n")
  } else {
    cat("  [FAIL] Future Upper NOT deviating — hist_ref not applied to draw_curves_tbl\n")
  }
}

# ---- Section 5: Exceedance ribbon deviation --------------------------------
cat("=== Section 5: Exceedance ribbon deviation ===\n")

# Simulate what exceedance_ribbon reactive does for each scenario
# Historical: raw tbl passed to compute_exceedance_ribbon(), then mutate output
# Future:     raw tbl passed, then mutate output

hist_ribbon_raw <- tryCatch(
  compute_exceedance_ribbon(hist_raw, band_q = bq),
  error = function(e) { cat("ERROR hist ribbon:", conditionMessage(e), "\n"); NULL })

fut_ribbon_raw <- tryCatch(
  compute_exceedance_ribbon(fut_raw, band_q = bq),
  error = function(e) { cat("ERROR fut ribbon:", conditionMessage(e), "\n"); NULL })

if (!is.null(hist_ribbon_raw) && !is.null(fut_ribbon_raw)) {

  # Print actual column names — confirms what mod_2_02_results.R must reference
  cat("  Ribbon column names:", paste(names(hist_ribbon_raw), collapse = ", "), "\n\n")

  # Apply deviation to output columns — simulates the reactive mutate
  mid_col <- intersect(c("welfare_mid", "value"),     names(hist_ribbon_raw))[1]
  lo_col  <- intersect(c("welfare_lo",  "band_lo"),   names(hist_ribbon_raw))[1]
  hi_col  <- intersect(c("welfare_hi",  "band_hi"),   names(hist_ribbon_raw))[1]

  cat("  mid_col:", mid_col, "| lo_col:", lo_col, "| hi_col:", hi_col, "\n\n")

  shift_ribbon <- function(r) {
    r[[mid_col]] <- r[[mid_col]] - hist_ref
    r[[lo_col]]  <- r[[lo_col]]  - hist_ref
    r[[hi_col]]  <- r[[hi_col]]  - hist_ref
    r
  }

  hist_ribbon_dev <- shift_ribbon(hist_ribbon_raw)
  fut_ribbon_dev  <- shift_ribbon(fut_ribbon_raw)

  # KEY CHECKS
  # 1. Historical mid at median exceedance — should be ~0 after deviation
  hist_mid_idx <- which.min(abs(hist_ribbon_raw$exceed_prob - 0.5))
  hist_mid_raw <- hist_ribbon_raw[[mid_col]][hist_mid_idx]
  hist_mid_dev <- hist_ribbon_dev[[mid_col]][hist_mid_idx]

  # 2. Future mid at median exceedance — should shift by -hist_ref
  fut_mid_idx <- which.min(abs(fut_ribbon_raw$exceed_prob - 0.5))
  fut_mid_raw <- fut_ribbon_raw[[mid_col]][fut_mid_idx]
  fut_mid_dev <- fut_ribbon_dev[[mid_col]][fut_mid_idx]

  # 3. Ribbon spread unchanged by deviation
  hist_spread_raw <- mean(hist_ribbon_raw[[hi_col]] - hist_ribbon_raw[[lo_col]], na.rm = TRUE)
  hist_spread_dev <- mean(hist_ribbon_dev[[hi_col]] - hist_ribbon_dev[[lo_col]], na.rm = TRUE)
  fut_spread_raw  <- mean(fut_ribbon_raw[[hi_col]]  - fut_ribbon_raw[[lo_col]],  na.rm = TRUE)
  fut_spread_dev  <- mean(fut_ribbon_dev[[hi_col]]  - fut_ribbon_dev[[lo_col]],  na.rm = TRUE)

  cat("  Historical mid @ p=0.5 (raw):     ", round(hist_mid_raw, 4), "\n")
  cat("  Historical mid @ p=0.5 (deviated):", round(hist_mid_dev, 4),
      "(expect ~0)\n")
  cat("  Future     mid @ p=0.5 (raw):     ", round(fut_mid_raw, 4), "\n")
  cat("  Future     mid @ p=0.5 (deviated):", round(fut_mid_dev, 4),
      "(expect ~", round(fut_mid_raw - hist_ref, 4), ")\n\n")

  cat("  Historical ribbon spread (raw):     ", round(hist_spread_raw, 4), "\n")
  cat("  Historical ribbon spread (deviated):", round(hist_spread_dev, 4),
      "(expect identical)\n")
  cat("  Future     ribbon spread (raw):     ", round(fut_spread_raw, 4), "\n")
  cat("  Future     ribbon spread (deviated):", round(fut_spread_dev, 4),
      "(expect identical)\n\n")

  # Confirm correct column names used in mod_2_02_results.R deviation mutate
  mod_expected_cols <- c("welfare_mid", "welfare_lo", "welfare_hi")
  cat("  Column name check for mod_2_02_results.R deviation mutate:\n")
  for (col in mod_expected_cols) {
    exists <- col %in% names(hist_ribbon_raw)
    cat("    ", col, ":", if (exists) "[PASS — exists]" else "[FAIL — MISSING]", "\n")
  }

  # PASS/FAIL
  hist_ok   <- abs(hist_mid_dev) < 0.2 # median ≠ mean for skewed distribution
  fut_ok    <- abs((fut_mid_dev - fut_mid_raw) - (-hist_ref)) < 0.1
  spread_ok <- abs(hist_spread_raw - hist_spread_dev) < 0.001 &&
               abs(fut_spread_raw  - fut_spread_dev)  < 0.001

  cat("\n")
  if (hist_ok)   cat("  [PASS] Historical ribbon deviates correctly.\n") else
    cat("  [FAIL] Historical ribbon NOT deviating — check column names in mutate.\n")
  if (fut_ok)    cat("  [PASS] Future ribbon deviates correctly.\n") else
    cat("  [FAIL] Future ribbon NOT deviating — check column names in mutate.\n")
  if (spread_ok) cat("  [PASS] Ribbon spread unchanged — deviation is display-only.\n") else
    cat("  [FAIL] Ribbon spread changes with deviation — shape bug.\n")
}

# ---- Section 6: enhance_exceedance() central curve deviation ---------------
cat("=== Section 6: enhance_exceedance() central curve deviation ===\n")

hist_ribbon_dev <- dplyr::mutate(hist_ribbon_raw,
  welfare_mid = welfare_mid - hist_ref,
  welfare_lo  = welfare_lo  - hist_ref,
  welfare_hi  = welfare_hi  - hist_ref,
  scenario    = "Historical",
  ssp_key     = "historical"
) |>
  dplyr::rename(band_lo = welfare_lo, band_hi = welfare_hi)

fut_ribbon_dev2 <- dplyr::mutate(fut_ribbon_raw,
  welfare_mid = welfare_mid - hist_ref,
  welfare_lo  = welfare_lo  - hist_ref,
  welfare_hi  = welfare_hi  - hist_ref,
  scenario    = "SSP3-7.0 / 2025-2035",
  ssp_key     = "ssp370"
) |>
  dplyr::rename(band_lo = welfare_lo, band_hi = welfare_hi)

ribbon_dev_full <- dplyr::bind_rows(hist_ribbon_dev, fut_ribbon_dev2)

series_raw_full <- list(
  Historical              = list(out = hist_raw,  x_label = "raw"),
  `SSP3-7.0 / 2025-2035` = list(out = fut_raw,   x_label = "raw")
)
series_dev_full <- list(
  Historical              = list(out = hist_dev2, x_label = "dev"),
  `SSP3-7.0 / 2025-2035` = list(out = fut_dev2,  x_label = "dev")
)

p_raw <- tryCatch(
  enhance_exceedance(
    scenarios       = series_raw_full,
    hist_agg        = list(out = hist_raw,  x_label = "raw"),
    x_label         = "raw",
    return_period   = FALSE,
    n_sim_years     = nrow(hist_raw),
    logit_x         = FALSE,
    ribbon_data     = NULL,
    band_q          = bq,
    ensemble_band_q = ebq
  ),
  error = function(e) { cat("ERROR (raw):", conditionMessage(e), "\n"); NULL }
)

# Replace p_dev call with:
p_dev <- tryCatch(
  enhance_exceedance(
    scenarios       = series_dev_full,
    hist_agg        = list(out = hist_dev2, x_label = "dev"),
    x_label         = "dev",
    return_period   = FALSE,
    n_sim_years     = nrow(hist_dev2),
    logit_x         = FALSE,
    ribbon_data     = ribbon_dev_full,
    band_q          = bq,
    ensemble_band_q = ebq,
    hist_ref        = hist_ref
  ),
  error = function(e) { cat("ERROR (dev):", conditionMessage(e), "\n"); NULL }
)

if (!is.null(p_raw) && !is.null(p_dev)) {
  raw_build <- ggplot2::ggplot_build(p_raw)
  dev_build <- ggplot2::ggplot_build(p_dev)

  cat("  Raw plot layers summary:\n")
  for (i in seq_along(raw_build$data)) {
    d <- raw_build$data[[i]]
    if ("x" %in% names(d) && "y" %in% names(d) && nrow(d) > 2)
      cat("    Layer", i, "| nrow:", nrow(d),
          "| x range:", round(min(d$x, na.rm = TRUE), 3), "-",
          round(max(d$x, na.rm = TRUE), 3), "\n")
  }
  cat("  Dev plot layers summary:\n")
  for (i in seq_along(dev_build$data)) {
    d <- dev_build$data[[i]]
    if ("x" %in% names(d) && "y" %in% names(d) && nrow(d) > 2)
      cat("    Layer", i, "| nrow:", nrow(d),
          "| x range:", round(min(d$x, na.rm = TRUE), 3), "-",
          round(max(d$x, na.rm = TRUE), 3), "\n")
  }

  find_scenario_layer <- function(build_obj, which = c("hist", "fut")) {
    which <- match.arg(which)
    # Both scenarios are in layer 1 — filter by colour
    # Historical = "black", Future = any other colour
    d <- build_obj$data[[1]]
    if (!all(c("x", "y", "colour") %in% names(d))) return(NULL)
    if (which == "hist") {
      d[d$colour == "black", ]
    } else {
      d[d$colour != "black", ]
    }
  }

  get_mid <- function(layer) {
    if (is.null(layer) || nrow(layer) == 0) return(NA_real_)
    layer$x[which.min(abs(layer$y - 0.5))]
  }

  hist_raw_mid <- get_mid(find_scenario_layer(raw_build, "hist"))
  hist_dev_mid <- get_mid(find_scenario_layer(dev_build, "hist"))
  fut_raw_mid  <- get_mid(find_scenario_layer(raw_build, "fut"))
  fut_dev_mid  <- get_mid(find_scenario_layer(dev_build, "fut"))

  cat("\n  Historical central @ p=0.5 (raw):     ", round(hist_raw_mid, 4), "\n")
  cat("  Historical central @ p=0.5 (deviated):", round(hist_dev_mid, 4),
      "(expect ~0)\n")
  cat("  Future     central @ p=0.5 (raw):     ", round(fut_raw_mid,  4), "\n")
  cat("  Future     central @ p=0.5 (deviated):", round(fut_dev_mid,  4),
      "(expect ~", round(fut_raw_mid - hist_ref, 4), ")\n\n")

  hist_curve_ok <- !is.na(hist_dev_mid) && abs(hist_dev_mid) < 0.20
  fut_curve_ok  <- !is.na(fut_dev_mid) &&
                   abs((fut_dev_mid - fut_raw_mid) - (-hist_ref)) < 0.15

  if (hist_curve_ok) cat("  [PASS] Historical exceedance curve shifts correctly.\n") else
    cat("  [FAIL] Historical exceedance curve NOT shifting correctly.\n",
        "  hist_dev_mid =", round(hist_dev_mid, 4), "\n")
  if (fut_curve_ok)  cat("  [PASS] Future exceedance curve shifts correctly.\n") else
    cat("  [FAIL] Future exceedance curve NOT shifting correctly.\n",
        "  fut_dev_mid =", round(fut_dev_mid, 4),
        "| expected =", round(fut_raw_mid - hist_ref, 4), "\n")

}

# ---- Section 7: Ribbon deviation in enhance_exceedance() output ------------
cat("=== Section 7: Ribbon band deviation in plot ===\n")

get_ribbon_layer <- function(build_obj) {
  for (d in build_obj$data) {
    if (all(c("xmin", "xmax") %in% names(d)) && nrow(d) > 2)
      return(d)
  }
  NULL
}

ribbon_raw_layer <- get_ribbon_layer(raw_build)
ribbon_dev_layer <- get_ribbon_layer(dev_build)

if (!is.null(ribbon_dev_layer)) {

  # p=0.5 index — y = exceedance probability in coord_flipped plot
  mid_idx     <- which.min(abs(ribbon_dev_layer$y - 0.5))
  ribbon_ymid <- (ribbon_dev_layer$xmax[mid_idx] +
                   ribbon_dev_layer$xmin[mid_idx]) / 2

  # Correct comparison: ribbon midpoint should straddle the deviated central curve
  # band_lo/hi already deviated via apply(draw_curves, 2, quantile) - hist_ref
  dev_layer_1  <- dev_build$data[[1]]
  dev_fut_rows <- dev_layer_1[dev_layer_1$colour != "black", ]
  central_mid  <- dev_fut_rows$x[which.min(abs(dev_fut_rows$y - 0.5))]

  ribbon_half_width <- (ribbon_dev_layer$xmax[mid_idx] -
                         ribbon_dev_layer$xmin[mid_idx]) / 2

  cat("  Ribbon midpoint  @ p=0.5 (deviated):", round(ribbon_ymid,    4), "\n")
  cat("  Central curve    @ p=0.5 (deviated):", round(central_mid,    4), "\n")
  cat("  Ribbon half-width:                  ", round(ribbon_half_width, 4), "\n")
  cat("  Difference (ribbon mid - central):  ",
      round(ribbon_ymid - central_mid, 4),
      "(expect <= ribbon half-width)\n\n")

  ribbon_ok <- abs(ribbon_ymid - central_mid) <= ribbon_half_width

  if (ribbon_ok) {
    cat("  [PASS] Ribbon bands correctly straddle deviated central curve.\n\n")
  } else {
    cat("  [FAIL] Ribbon bands do NOT straddle deviated central curve —",
        "band_lo/hi not deviated in enhance_exceedance().\n\n")
  }
}

# ---- Section 8: Hero plot whisker deviation (Future scenario) --------------
cat("=== Section 8: Hero plot whisker deviation ===\n")

# Simulate compute_bands_from_raw() for both scenarios
# This function is called inside all_series_tbl() in mod_2_02_results.R
# It subtracts hist_ref from quantile(.x, bq["lo/hi"]) of draw_values

compute_bands_sim <- function(tbl, bq, hist_ref = 0) {
  dplyr::mutate(tbl,
    value_lo = purrr::map_dbl(draw_values,
      ~if (is.null(.x) || length(.x) == 0L) NA_real_
       else quantile(.x, bq["lo"], na.rm = TRUE) - hist_ref),
    value_hi = purrr::map_dbl(draw_values,
      ~if (is.null(.x) || length(.x) == 0L) NA_real_
       else quantile(.x, bq["hi"], na.rm = TRUE) - hist_ref)
  )
}

# Simulate all_series_tbl() behaviour:
# Historical: uses hist_raw draw_values with hist_ref
# Future:     uses fut_raw draw_values with hist_ref
hist_bands_s_raw <- compute_bands_sim(hist_raw, bq, hist_ref = 0)
hist_bands_s_dev <- compute_bands_sim(hist_raw, bq, hist_ref = hist_ref)
fut_bands_s_raw  <- compute_bands_sim(fut_raw,  bq, hist_ref = 0)
fut_bands_s_dev  <- compute_bands_sim(fut_raw,  bq, hist_ref = hist_ref)

hist_lo_shift <- mean(hist_bands_s_dev$value_lo - hist_bands_s_raw$value_lo)
fut_lo_shift  <- mean(fut_bands_s_dev$value_lo  - fut_bands_s_raw$value_lo)

cat("  Historical whisker_lo shift:", round(hist_lo_shift, 4),
    "(expect ~", round(-hist_ref, 4), ")\n")
cat("  Future     whisker_lo shift:", round(fut_lo_shift,  4),
    "(expect ~", round(-hist_ref, 4), ")\n\n")

hist_w_ok <- abs(hist_lo_shift - (-hist_ref)) < 0.01
fut_w_ok  <- abs(fut_lo_shift  - (-hist_ref)) < 0.01

if (hist_w_ok) cat("  [PASS] Historical whiskers shift correctly.\n") else
  cat("  [FAIL] Historical whiskers NOT shifting correctly.\n")
if (fut_w_ok)  cat("  [PASS] Future whiskers shift correctly.\n") else
  cat("  [FAIL] Future whiskers NOT shifting correctly.\n")

if (!fut_w_ok) {
  cat("\n  >> Checking all_series_tbl() — does it pass hist_ref to",
      "compute_bands_from_raw() for Future?\n")
  cat("  >> In mod_2_02_results.R, find compute_bands_from_raw() call\n")
  cat("  >> for scenario path and confirm hist_ref source.\n")
  cat("  >> hist_ref inside all_series_tbl() may use a different reactive\n")
  cat("  >> than hist_ref_val() — or may be called outside reactive context.\n")
}

# ---- Section 8b: hist_ref closure scope in compute_bands_from_raw() --------
cat("=== Section 8b: hist_ref closure scope ===\n")

# Simulate all_series_tbl() closure behaviour —
# hist_ref captured at reactive evaluation time
make_compute_bands <- function(hist_ref_captured) {
  function(tbl, bq) {
    dplyr::mutate(tbl,
      value_lo = purrr::map_dbl(draw_values,
        ~if (is.null(.x) || length(.x) == 0L) NA_real_
         else quantile(.x, bq["lo"], na.rm = TRUE) - hist_ref_captured),
      value_hi = purrr::map_dbl(draw_values,
        ~if (is.null(.x) || length(.x) == 0L) NA_real_
         else quantile(.x, bq["hi"], na.rm = TRUE) - hist_ref_captured)
    )
  }
}

# Case 1: hist_ref correctly captured
compute_bands_correct <- make_compute_bands(hist_ref)
fut_bands_correct <- compute_bands_correct(fut_raw, bq)
shift_correct <- mean(fut_bands_correct$value_lo) -
                 mean(compute_bands_correct(
                   dplyr::mutate(fut_raw), bq)$value_lo)

# Case 2: hist_ref = 0 (not captured — deviation == "none" path)
compute_bands_zero <- make_compute_bands(0)
fut_bands_zero <- compute_bands_zero(fut_raw, bq)
shift_zero <- mean(fut_bands_zero$value_lo) -
              mean(fut_bands_raw$value_lo)

cat("  Shift with hist_ref captured correctly:", round(shift_correct, 4),
    "(expect 0 — same ref)\n")
cat("  Shift with hist_ref = 0:               ", round(shift_zero, 4),
    "(expect 0 — no deviation)\n\n")

# Now check what all_series_tbl() actually uses for Future scenario
# by reading the sc_list lapply in mod_2_02_results.R
cat("  Key question: does all_series_tbl() call compute_bands_from_raw()\n")
cat("  with scenario_agg_rv()[[dk]] (raw) or agg_scenarios()[[dk]] (deviated)?\n")
cat("  If raw — hist_ref subtracted from raw draws = correct.\n")
cat("  If deviated — hist_ref subtracted AGAIN from already-deviated draws = wrong.\n\n")

# Check what sc_raw is in all_series_tbl() for Future
# From mod_2_02_results.R lines ~370-380:
# sc_raw <- scenario_agg_rv()[[dk]][[wk]][[method]]  ← raw ✅
# sc_bands <- compute_bands_from_raw(sc_raw)          ← applies hist_ref to raw ✅
# sc_out <- agg_scenarios()[[dk]]$out                 ← deviated value ✅
# joined: sc_out |> left_join(sc_bands)               ← deviated value + deviated bands ✅

cat("  Per code review: sc_raw uses scenario_agg_rv() (raw draws) ✅\n")
cat("  compute_bands_from_raw() subtracts hist_ref from raw draws ✅\n")
cat("  If app still shows wrong whiskers — hist_ref_val() may return 0\n")
cat("  when all_series_tbl() invalidates before hist_ref_val() updates.\n\n")

# Test reactive timing hypothesis — does hist_ref_val() depend on
# the same inputs as all_series_tbl()?
cat("  >> Check in mod_2_02_results.R:\n")
cat("  >> 1. hist_ref_val() definition — what reactive inputs does it use?\n")
cat("  >> 2. all_series_tbl() — does it call hist_ref_val() directly?\n")
cat("  >> 3. compute_bands_from_raw() — does it close over hist_ref\n")
cat("        or call hist_ref_val() directly?\n")
# ---- Section 9: Hero plot whisker source in plot_pointrange_climate() ------
cat("=== Section 9: Hero plot whisker source ===\n")

# Simulate all_series_tbl() output — deviated value + deviated value_lo/hi
hist_tbl_dev <- hist_bands_dev |>
  dplyr::mutate(value = hist_raw$value - hist_ref, scenario = "Historical")
fut_tbl_dev  <- fut_bands_dev |>
  dplyr::mutate(value = fut_raw$value - hist_ref,
                scenario = "SSP3-7.0 / 2025-2035")
all_tbl_dev  <- dplyr::bind_rows(hist_tbl_dev, fut_tbl_dev)

cat("  all_series_tbl() columns:", paste(names(all_tbl_dev), collapse = ", "), "\n\n")

# KEY CHECK: are value_lo/hi deviated for both scenarios?
hist_lo_mean <- mean(all_tbl_dev$value_lo[all_tbl_dev$scenario == "Historical"],
                     na.rm = TRUE)
fut_lo_mean  <- mean(all_tbl_dev$value_lo[all_tbl_dev$scenario != "Historical"],
                     na.rm = TRUE)
hist_lo_raw  <- mean(hist_bands_raw$value_lo, na.rm = TRUE)
fut_lo_raw   <- mean(fut_bands_raw$value_lo,  na.rm = TRUE)

cat("  Historical value_lo (raw):      ", round(hist_lo_raw,  4), "\n")
cat("  Historical value_lo (deviated): ", round(hist_lo_mean, 4),
    "(expect ~", round(hist_lo_raw - hist_ref, 4), ")\n")
cat("  Future     value_lo (raw):      ", round(fut_lo_raw,   4), "\n")
cat("  Future     value_lo (deviated): ", round(fut_lo_mean,  4),
    "(expect ~", round(fut_lo_raw - hist_ref, 4), ")\n\n")

hist_w9_ok <- abs((hist_lo_mean - hist_lo_raw) - (-hist_ref)) < 0.01
fut_w9_ok  <- abs((fut_lo_mean  - fut_lo_raw)  - (-hist_ref)) < 0.01

if (hist_w9_ok) cat("  [PASS] Historical value_lo correctly deviated.\n") else
  cat("  [FAIL] Historical value_lo NOT deviated.\n")
if (fut_w9_ok)  cat("  [PASS] Future value_lo correctly deviated.\n") else
  cat("  [FAIL] Future value_lo NOT deviated.\n")

# Now build the actual plot and inspect whisker layer
p_hero_raw <- tryCatch(
  plot_pointrange_climate(
    scenarios       = series_raw_full,
    hist_agg        = list(out = hist_raw,  x_label = "raw"),
    coef_bands_tbl  = dplyr::bind_rows(
      dplyr::mutate(hist_bands_raw, value = hist_raw$value,
                    scenario = "Historical"),
      dplyr::mutate(fut_bands_raw,  value = fut_raw$value,
                    scenario = "SSP3-7.0 / 2025-2035")),
    band_q          = bq,
    ensemble_band_q = ebq
  ),
  error = function(e) { cat("ERROR (raw):", conditionMessage(e), "\n"); NULL }
)

p_hero_dev <- tryCatch(
  plot_pointrange_climate(
    scenarios       = series_dev_full,
    hist_agg        = list(out = hist_dev2, x_label = "dev"),
    coef_bands_tbl  = all_tbl_dev,
    band_q          = bq,
    ensemble_band_q = ebq
  ),
  error = function(e) { cat("ERROR (dev):", conditionMessage(e), "\n"); NULL }
)

if (!is.null(p_hero_raw) && !is.null(p_hero_dev)) {
  hero_raw_build <- ggplot2::ggplot_build(p_hero_raw)
  hero_dev_build <- ggplot2::ggplot_build(p_hero_dev)

  cat("\n  Hero plot raw layers:\n")
  for (i in seq_along(hero_raw_build$data)) {
    d <- hero_raw_build$data[[i]]
    if (nrow(d) > 0)
      cat("    Layer", i, "| nrow:", nrow(d), "| cols:",
          paste(names(d), collapse = ", "), "\n")
  }
}

# ---- Section 10: plot_pointrange_climate() whisker source ------------------
cat("=== Section 10: Hero plot whisker layer inspection ===\n")

if (!is.null(p_hero_raw) && !is.null(p_hero_dev)) {

  # Find pointrange/errorbar layer — has xmin/xmax or ymin/ymax
  find_whisker_layer <- function(build_obj) {
    for (d in build_obj$data) {
      if (any(c("xmin", "xmax", "ymin", "ymax") %in% names(d)) &&
          nrow(d) > 0)
        return(d)
    }
    NULL
  }

  cat("  Raw hero plot layers:\n")
  for (i in seq_along(hero_raw_build$data)) {
    d <- hero_raw_build$data[[i]]
    if (nrow(d) > 0)
      cat("    Layer", i, "| nrow:", nrow(d), "| cols:",
          paste(names(d), collapse = ", "), "\n")
  }

  cat("\n  Dev hero plot layers:\n")
  for (i in seq_along(hero_dev_build$data)) {
    d <- hero_dev_build$data[[i]]
    if (nrow(d) > 0)
      cat("    Layer", i, "| nrow:", nrow(d), "| cols:",
          paste(names(d), collapse = ", "), "\n")
  }

  wh_hist_raw <- hero_raw_build$data[[1]]
  wh_hist_dev <- hero_dev_build$data[[1]]
  wh_fut_raw  <- hero_raw_build$data[[2]]
  wh_fut_dev  <- hero_dev_build$data[[2]]

  min_col    <- intersect(c("xmin", "ymin"), names(wh_hist_raw))[1]
  hist_shift <- wh_hist_dev[[min_col]][1] - wh_hist_raw[[min_col]][1]
  fut_shift  <- wh_fut_dev[[min_col]][1]  - wh_fut_raw[[min_col]][1]
  shifts     <- c(hist_shift, fut_shift)

  cat("  Per-row whisker shifts:", paste(round(shifts, 3), collapse = ", "), "\n")
  cat("  Expected shift: ~", round(-hist_ref, 3), "for all rows\n\n")

  hist_row_ok <- abs(hist_shift - (-hist_ref)) < 0.05
  fut_row_ok  <- abs(fut_shift  - (-hist_ref)) < 0.05

  if (hist_row_ok) cat("  [PASS] Historical whisker shifts correctly.\n") else
    cat("  [FAIL] Historical whisker NOT shifting.\n")
  if (fut_row_ok)  cat("  [PASS] Future whisker shifts correctly.\n") else
    cat("  [FAIL] Future whisker NOT shifting.\n")

}
  cat("\n========== Diagnostic complete ==========\n")