library(testthat)

# ---- Threshold table preparation --------------------------------------------
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

  duplicated <- dplyr::bind_rows(long, long)
  out3 <- build_threshold_table_df(
    threshold_tbl = duplicated,
    group_order = "scenario_x_year",
    show_coef = TRUE
  )
  expect_false(any(vapply(out3, is.list, logical(1L))))
})

test_that("threshold table keeps raw values and rounds only for display (R2-BUG-15)", {
  RPs <- c("1:10", "1:1")
  rates <- tibble::tibble(scenario = "Historical", Estimate = "Central (P50)",
                          rp_name = RPs, rp_label = RPs, value = c(0.32154, 0.32449),
                          n_obs = 30L, is_historical = TRUE)
  out <- build_threshold_table_df(rates, show_coef = TRUE)
  # 0.32154 vs 0.32449 differ by < 0.5 pp; both were 0.32 when rounded first.
  expect_equal(out[["1:10"]], 0.32154)
  expect_equal(out[["1:1"]], 0.32449)

  defs <- .threshold_col_defs(out)
  expect_setequal(names(defs), RPs)
  expect_identical(defs[["1:10"]]$format$cell$digits, 4L)

  levels <- out
  levels[RPs] <- lapply(levels[RPs], function(x) x * 1000)
  expect_identical(.threshold_col_defs(levels)[["1:1"]]$format$cell$digits, 2L)

  # Both Step 2 and Step 3 renderers accept the raw frame.
  expect_s3_class(.step2_reactable(out, col_defs = defs), "reactable")
  expect_s3_class(.wise_threshold_reactable(out), "reactable")
})
