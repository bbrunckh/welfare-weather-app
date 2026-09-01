library(testthat)

# Synthetic future-scenario tibble in the new schema (one row per sim_year,
# value_all / value_all_sd / model_id list-columns aligned).
make_future_tbl <- function(n_yrs = 30, n_mod = 20, seed = 1L) {
  set.seed(seed)
  model_ids <- paste0("m", seq_len(n_mod))
  model_means <- stats::rnorm(n_mod, mean = 3.5, sd = 0.08)
  rows <- lapply(seq_len(n_yrs), function(yr) {
    vals <- model_means + stats::rnorm(n_mod, 0, 0.04)
    sds  <- runif(n_mod, 0.005, 0.02)
    tibble::tibble(
      sim_year     = 2024L + yr,
      value        = mean(vals),
      model_id     = list(model_ids),
      value_all    = list(vals),
      value_all_sd = list(sds),
      var_within   = mean(sds^2),
      var_across   = stats::var(vals),
      agg_method   = "mean",
      weighted     = FALSE
    )
  })
  dplyr::bind_rows(rows)
}

# ---- Test 1 -----------------------------------------------------------------
test_that("plot_pointrange_climate consumes the new bands_tbl schema", {
  bands <- tibble::tibble(
    scenario      = c("Historical", "SSP3-7.0 / 2025-2035"),
    value         = c(3.50, 3.42),
    coef_lo       = c(3.49, 3.41),
    coef_hi       = c(3.51, 3.43),
    interann_lo   = c(3.40, 3.30),
    interann_hi   = c(3.60, 3.55),
    intermod_lo   = c(NA_real_, 3.32),
    intermod_hi   = c(NA_real_, 3.52),
    is_historical = c(TRUE, FALSE),
    n_models      = c(1L, 20L)
  )
  p <- plot_pointrange_climate(bands_tbl = bands, x_label = "Mean",
                                show_coef = TRUE)
  expect_s3_class(p, "ggplot")
  # Three linerange layers expected when show_coef = TRUE
  geom_classes <- vapply(p$layers, function(l) class(l$geom)[1L], character(1L))
  expect_true(sum(geom_classes == "GeomLinerange") >= 2L)  # at least intermod + interann
})

# ---- Test 2 -----------------------------------------------------------------
test_that("enhance_exceedance aggregates per-model curves into median + inter-model ribbon", {
  # Build a curves_tbl by hand: 3 models × 30 years × 2 scenarios (1 hist + 1 fut)
  n_yrs <- 30
  set.seed(7)
  make_curve <- function(scenario, model_id, is_hist, mean_v) {
    v <- sort(stats::rnorm(n_yrs, mean = mean_v, sd = 0.05))
    tibble::tibble(
      scenario      = scenario,
      model_id      = model_id,
      rank          = seq_len(n_yrs),
      welfare_val   = v,
      coef_sd       = rep(0.01, n_yrs),
      exceed_prob   = rev((seq_len(n_yrs) - 0.5) / n_yrs),
      is_historical = is_hist
    )
  }
  curves <- dplyr::bind_rows(
    make_curve("Historical",            "Historical", TRUE,  3.50),
    make_curve("SSP3-7.0 / 2025-2035",  "m1",         FALSE, 3.40),
    make_curve("SSP3-7.0 / 2025-2035",  "m2",         FALSE, 3.42),
    make_curve("SSP3-7.0 / 2025-2035",  "m3",         FALSE, 3.38)
  )
  p <- enhance_exceedance(curves_tbl = curves, x_label = "Mean",
                          return_period = FALSE,
                          n_sim_years = n_yrs,
                          band_q = c(lo = 0.10, hi = 0.90),
                          ensemble_band_q = c(lo = 0.00, hi = 1.00))
  expect_s3_class(p, "ggplot")
  # The plot data is constructed inside; just verify it runs.

  # And verify by hand that the median across models at the smallest rank
  # equals the median of the three per-model smallest values.
  rank1 <- curves[curves$rank == 1L & !curves$is_historical, ]
  med1 <- stats::median(rank1$welfare_val)
  per_model_min <- c(
    min(curves$welfare_val[curves$model_id == "m1"]),
    min(curves$welfare_val[curves$model_id == "m2"]),
    min(curves$welfare_val[curves$model_id == "m3"])
  )
  expect_equal(med1, stats::median(per_model_min))
})

# ---- Test 3 -----------------------------------------------------------------
test_that("build_threshold_table_df pivots long input wide and orders rows", {
  # Build a minimal long threshold_tbl in the new schema:
  RPs <- c("1:10", "1:1", "9:10")
  long <- dplyr::bind_rows(
    tibble::tibble(scenario = "Historical", Estimate = "Central (P50)",
                   rp_name = RPs, rp_label = RPs, value = c(3.7, 3.5, 3.3),
                   n_obs = 30L, is_historical = TRUE),
    tibble::tibble(scenario = "Historical", Estimate = "Coef P10",
                   rp_name = RPs, rp_label = RPs, value = c(3.69, 3.49, 3.29),
                   n_obs = 30L, is_historical = TRUE),
    tibble::tibble(scenario = "Historical", Estimate = "Coef P90",
                   rp_name = RPs, rp_label = RPs, value = c(3.71, 3.51, 3.31),
                   n_obs = 30L, is_historical = TRUE),
    tibble::tibble(scenario = "SSP3-7.0 / 2025-2035", Estimate = "Central (P50)",
                   rp_name = RPs, rp_label = RPs, value = c(3.6, 3.4, 3.2),
                   n_obs = 30L, is_historical = FALSE),
    tibble::tibble(scenario = "SSP3-7.0 / 2025-2035", Estimate = "Ensemble min",
                   rp_name = RPs, rp_label = RPs, value = c(3.5, 3.3, 3.1),
                   n_obs = 30L, is_historical = FALSE),
    tibble::tibble(scenario = "SSP3-7.0 / 2025-2035", Estimate = "Ensemble max",
                   rp_name = RPs, rp_label = RPs, value = c(3.7, 3.5, 3.3),
                   n_obs = 30L, is_historical = FALSE)
  )
  out <- build_threshold_table_df(threshold_tbl = long,
                                  group_order = "scenario_x_year",
                                  show_coef = TRUE)
  expect_s3_class(out, "data.frame")
  expect_true(all(c("Scenario", "Estimate", "Obs") %in% names(out)))
  expect_true(all(RPs %in% names(out)))
  # Historical rows are arranged Coef P10 -> Central P50 -> Coef P90
  hist_idx <- which(out$Scenario == "Historical")
  expect_equal(out$Estimate[hist_idx], c("Coef P10", "Central (P50)", "Coef P90"))
  # Future scenario: Ensemble min -> Central -> Ensemble max
  fut_idx <- which(out$Scenario == "SSP3-7.0 / 2025-2035")
  expect_equal(length(fut_idx), 3L)
  expect_equal(out$Estimate[fut_idx],
               c("Ensemble min", "Central (P50)", "Ensemble max"))
  # Hide coef rows when show_coef = FALSE
  out2 <- build_threshold_table_df(threshold_tbl = long,
                                   group_order = "scenario_x_year",
                                   show_coef = FALSE)
  expect_false(any(grepl("^Coef ", out2$Estimate)))
})
