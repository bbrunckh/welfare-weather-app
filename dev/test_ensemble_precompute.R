# =============================================================================
# test_ensemble_precompute.R
#
# Integration test for summarise_ensemble().
# Run after devtools::load_all() to verify ensemble precompute logic.
#
# Per spec Section 13: synthetic inputs built from real function schema.
# Each section is independently runnable. Failure mode: stopifnot() +
# cat('PASS: ...') immediately after.
# =============================================================================

devtools::load_all()

# -- Synthetic data ----------------------------------------------------------
# Three fake CMIP6 models x one SSP/period.
# Schema matches get_weather() output: code, year, survname, loc_id, timestamp, <vars>

make_fake_model <- function(model_offset) {
  data.frame(
    code      = "TST",
    year      = 2020L,
    survname  = "test",
    loc_id    = rep(c(1L, 2L), each = 3L),
    timestamp = rep(as.Date(c("2025-01-01", "2025-02-01", "2025-03-01")), 2L),
    temp      = c(20, 21, 22, 23, 24, 25) + model_offset,
    precip    = c(100, 110, 120, 130, 140, 150) + model_offset * 10,
    stringsAsFactors = FALSE
  )
}

weather_result <- list(
  historical                              = make_fake_model(0),
  ssp3_7_0_2025_2035_Model_A              = make_fake_model(1),
  ssp3_7_0_2025_2035_Model_B              = make_fake_model(2),
  ssp3_7_0_2025_2035_Model_C              = make_fake_model(3),
  ssp2_4_5_2025_2035_Model_A              = make_fake_model(10),
  ssp2_4_5_2025_2035_Model_B              = make_fake_model(20)
)

# =============================================================================
# Section 1 — summarise_ensemble() returns correct schema
# =============================================================================
cat("\nSection 1: schema check\n")

res <- summarise_ensemble(weather_result, lo_q = 0.10, hi_q = 0.90)

expected_keys <- c(
  "historical",
  "ssp3_7_0_2025_2035_ensemble_mean",
  "ssp3_7_0_2025_2035_ensemble_lo",
  "ssp3_7_0_2025_2035_ensemble_hi",
  "ssp2_4_5_2025_2035_ensemble_mean",
  "ssp2_4_5_2025_2035_ensemble_lo",
  "ssp2_4_5_2025_2035_ensemble_hi"
)

stopifnot("Output keys match expected" = setequal(names(res), expected_keys))
cat("PASS: output keys correct\n")

# =============================================================================
# Section 2 — historical entry unchanged
# =============================================================================
cat("\nSection 2: historical unchanged\n")

stopifnot(
  "historical rows preserved" = nrow(res[["historical"]]) == nrow(weather_result[["historical"]]),
  "historical values unchanged" = identical(res[["historical"]]$temp, weather_result[["historical"]]$temp)
)
cat("PASS: historical entry unchanged\n")

# =============================================================================
# Section 3 — ensemble_mean values == mean of 3 models at each (loc_id, timestamp)
# =============================================================================
cat("\nSection 3: ensemble_mean correctness\n")

mean_df <- res[["ssp3_7_0_2025_2035_ensemble_mean"]]

# Expected mean for loc_id=1, timestamp=2025-01-01:
# Model_A=21, Model_B=22, Model_C=23 -> mean=22
expected_mean_temp <- mean(c(21, 22, 23))
actual_mean_temp   <- mean_df$temp[mean_df$loc_id == 1L &
                                    mean_df$timestamp == as.Date("2025-01-01")]

stopifnot(
  "ensemble_mean temp is correct" = abs(actual_mean_temp - expected_mean_temp) < 1e-10
)
cat("PASS: ensemble_mean values correct\n")

# =============================================================================
# Section 4 — ensemble_lo/hi are correct quantiles across models
# =============================================================================
cat("\nSection 4: ensemble_lo/hi quantile correctness\n")

lo_df <- res[["ssp3_7_0_2025_2035_ensemble_lo"]]
hi_df <- res[["ssp3_7_0_2025_2035_ensemble_hi"]]

# Expected lo (10th pct) for loc_id=1, timestamp=2025-01-01: quantile(c(21,22,23), 0.10)
expected_lo <- quantile(c(21, 22, 23), 0.10)
expected_hi <- quantile(c(21, 22, 23), 0.90)

actual_lo <- lo_df$temp[lo_df$loc_id == 1L & lo_df$timestamp == as.Date("2025-01-01")]
actual_hi <- hi_df$temp[hi_df$loc_id == 1L & hi_df$timestamp == as.Date("2025-01-01")]

stopifnot(
  "ensemble_lo is correct 10th percentile" = abs(actual_lo - expected_lo) < 1e-10,
  "ensemble_hi is correct 90th percentile" = abs(actual_hi - expected_hi) < 1e-10
)
cat("PASS: ensemble_lo/hi quantiles correct\n")

# =============================================================================
# Section 5 — two SSP/periods produce independent entries
# =============================================================================
cat("\nSection 5: multiple SSP/periods independent\n")

# SSP2 mean for loc_id=1, timestamp=2025-01-01:
# Model_A=30 (10+20), Model_B=40 (20+20) -> mean=35
mean_ssp2 <- res[["ssp2_4_5_2025_2035_ensemble_mean"]]
expected_ssp2_mean <- mean(c(20+10, 20+20))  # offsets 10 and 20 on base 20

# Note: base temp for loc_id=1, timestamp=2025-01-01 is 20
# Model_A offset=10 -> 30; Model_B offset=20 -> 40; mean=35
actual_ssp2_mean <- mean_ssp2$temp[mean_ssp2$loc_id == 1L &
                                     mean_ssp2$timestamp == as.Date("2025-01-01")]

stopifnot(
  "SSP2 and SSP3 entries are independent" =
    !isTRUE(all.equal(res[["ssp2_4_5_2025_2035_ensemble_mean"]]$temp,
                      res[["ssp3_7_0_2025_2035_ensemble_mean"]]$temp)),
  "SSP2 mean temp correct" = abs(actual_ssp2_mean - 35) < 1e-10
)
cat("PASS: multiple SSP/periods independent\n")

# =============================================================================
# Section 6 — band_q = minmax returns min/max correctly
# =============================================================================
cat("\nSection 6: min/max band option\n")

res_mm <- summarise_ensemble(weather_result, lo_q = 0.001, hi_q = 0.999)

lo_mm <- res_mm[["ssp3_7_0_2025_2035_ensemble_lo"]]
hi_mm <- res_mm[["ssp3_7_0_2025_2035_ensemble_hi"]]

# Near-min and near-max of c(21,22,23) at extreme quantiles
stopifnot(
  "near-min is <= mean" = lo_mm$temp[lo_mm$loc_id == 1L & lo_mm$timestamp == as.Date("2025-01-01")] <=
                           mean_df$temp[mean_df$loc_id == 1L & mean_df$timestamp == as.Date("2025-01-01")],
  "near-max is >= mean" = hi_mm$temp[hi_mm$loc_id == 1L & hi_mm$timestamp == as.Date("2025-01-01")] >=
                           mean_df$temp[mean_df$loc_id == 1L & mean_df$timestamp == as.Date("2025-01-01")]
)
cat("PASS: near-min/max quantiles correct\n")

# =============================================================================
# Section 7 — attributes preserved (stored_breaks, ensemble_summarised)
# =============================================================================
cat("\nSection 7: attributes preserved\n")

attr(weather_result, "stored_breaks") <- list(temp = c(-Inf, 22, Inf))
res_attr <- summarise_ensemble(weather_result)

stopifnot(
  "stored_breaks preserved" = !is.null(attr(res_attr, "stored_breaks")),
  "ensemble_summarised flag set" = isTRUE(attr(res_attr, "ensemble_summarised"))
)
cat("PASS: attributes preserved\n")

cat("\n=== ALL SECTIONS PASSED ===\n")
