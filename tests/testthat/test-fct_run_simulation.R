# ============================================================================ #
# tests/testthat/test-fct_run_simulation.R                                     #
# REACT-12 / TEST-01: direct tests of fct_run_simulation()'s per-key failure   #
# ledger - fail fast on historical or whole-group failure, publish partial     #
# results with ledger + provenance counts otherwise. Weather loading and the   #
# per-key pipeline are injected (weather_fn / pipeline_fn) so no network or    #
# model fit is needed.                                                         #
# ============================================================================ #

library(testthat)

make_ledger_svy <- function(n = 60L) {
  data.frame(
    hhid     = seq_len(n),
    year     = 2020L,
    code     = "TST",
    survname = "SRV",
    loc_id   = sprintf("loc%02d", seq_len(n)),
    welfare  = stats::rnorm(n, 10, 2),
    weight   = 1,
    temp     = stats::rnorm(n),
    stringsAsFactors = FALSE
  )
}

make_ledger_weather_result <- function(with_ssp5 = TRUE) {
  wh <- data.frame(
    code = "TST", year = 2030L, survname = "SRV", loc_id = "loc01",
    temp = 1, timestamp = as.POSIXct("2030-06-01", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  hist_wh <- wh
  hist_wh$year <- 2020L
  hist_wh$timestamp <- as.POSIXct("2020-06-01", tz = "UTC")

  out <- list(
    historical                        = hist_wh,
    "ssp2_4_5_2030_2040_ensemble_mean" = wh,
    "ssp2_4_5_2030_2040_ensemble_hi"   = wh
  )
  if (with_ssp5) {
    out[["ssp5_8_5_2030_2040_ensemble_mean"]] <- wh
  }
  out
}

make_ledger_pipeline_fn <- function() {
  function(weather_raw, ...) {
    if (isTRUE(attr(weather_raw, "fail")))
      stop("injected pipeline failure")
    list(sim_year = 2030L, y_point = 1:10, weight = rep(1, 10),
         weather_raw = weather_raw)
  }
}

run_ledger_sim <- function(weather_result, ...) {
  fct_run_simulation(
    sw                  = data.frame(name = "temp", stringsAsFactors = FALSE),
    so                  = data.frame(name = "welfare", type = "numeric",
                                     transform = "log", label = "Welfare",
                                     stringsAsFactors = FALSE),
    svy                 = make_ledger_svy(),
    ss                  = NULL,
    mf                  = list(fit3 = NULL, engine = "fixest",
                                train_data = make_ledger_svy(),
                                weather_terms = "temp"),
    cp                  = list(type = "local", path = tempdir()),
    fp_list             = list(c("2030-01-01", "2040-12-31")),
    ssps                = c("ssp2_4_5", "ssp5_8_5"),
    residuals           = "none",
    dev_mode            = FALSE,
    skip_coef_draws     = TRUE,
    sim_dates           = c("2020-01-01", "2020-12-31"),
    perturbation_method = NULL,
    stored_breaks       = NULL,
    notify_fn           = function(msg) invisible(NULL),
    weather_fn          = function(...) weather_result,
    pipeline_fn         = make_ledger_pipeline_fn(),
    ...
  )
}

mark_fail <- function(weather_result, keys) {
  for (k in keys) attr(weather_result[[k]], "fail") <- TRUE
  weather_result
}

test_that("all keys succeeding returns an empty ledger and full provenance", {
  res <- suppressWarnings(run_ledger_sim(make_ledger_weather_result()))

  expect_length(res$failures, 0L)
  expect_identical(res$n_keys_ok, res$n_keys)
  # Two requested groups (SSP2 x {mean,hi}, SSP5 x {mean}) -> two scenarios
  expect_setequal(names(res$new_scenarios),
                  c("SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040"))
  expect_identical(res$new_scenarios[["SSP2-4.5 / 2030-2040"]]$n_models, 2L)
  expect_identical(
    res$new_scenarios[["SSP2-4.5 / 2030-2040"]]$n_models_requested, 2L)
  expect_identical(
    res$new_scenarios[["SSP5-8.5 / 2030-2040"]]$n_models_requested, 1L)
})

test_that("historical key failure fails the run with no results published", {
  wr <- make_ledger_weather_result()
  attr(wr$historical, "fail") <- TRUE

  expect_error(
    res <- suppressWarnings(run_ledger_sim(wr)),
    regexp = "Historical simulation failed"
  )
})

test_that("whole-group failure fails the run naming the group", {
  wr <- make_ledger_weather_result(with_ssp5 = TRUE)
  attr(wr[["ssp5_8_5_2030_2040_ensemble_mean"]], "fail") <- TRUE

  expect_error(
    suppressWarnings(run_ledger_sim(wr)),
    regexp = "All ensemble members failed for: SSP5-8.5 / 2030-2040"
  )
})

test_that("partial member failure publishes results with the ledger", {
  wr <- make_ledger_weather_result(with_ssp5 = TRUE)
  attr(wr[["ssp2_4_5_2030_2040_ensemble_hi"]], "fail") <- TRUE

  res <- suppressWarnings(run_ledger_sim(wr))

  # Ledger carries the failed key and its error message
  expect_length(res$failures, 1L)
  expect_identical(res$failures[[1L]]$key, "ssp2_4_5_2030_2040_ensemble_hi")
  expect_match(res$failures[[1L]]$error, "injected pipeline failure",
               fixed = TRUE)
  expect_false(res$failures[[1L]]$is_hist)
  expect_identical(res$failures[[1L]]$gk, "ssp2_4_5_2030_2040")

  # Provenance: scenario survives with 1 of 2 requested members
  s2 <- res$new_scenarios[["SSP2-4.5 / 2030-2040"]]
  expect_identical(s2$n_models, 1L)
  expect_identical(s2$n_models_requested, 2L)
  expect_identical(res$n_keys_ok, 3L)
  expect_identical(res$n_keys, 4L)

  # The successful member is still there under its member_type
  expect_true("ensemble_mean" %in% names(s2$pipelines))

  # The untouched group is intact
  expect_identical(
    res$new_scenarios[["SSP5-8.5 / 2030-2040"]]$n_models, 1L)
})

test_that("multiple partial failures across groups all reach the ledger", {
  wr <- make_ledger_weather_result(with_ssp5 = FALSE)
  # ssp2 group has mean + hi; add one more member so single failures stay partial
  wr[["ssp2_4_5_2030_2040_ensemble_lo"]] <- wr[["ssp2_4_5_2030_2040_ensemble_mean"]]
  attr(wr[["ssp2_4_5_2030_2040_ensemble_hi"]], "fail") <- TRUE
  attr(wr[["ssp2_4_5_2030_2040_ensemble_lo"]], "fail") <- TRUE

  res <- suppressWarnings(run_ledger_sim(wr))

  expect_length(res$failures, 2L)
  expect_identical(res$n_keys_ok, res$n_keys - 2L)
  s2 <- res$new_scenarios[["SSP2-4.5 / 2030-2040"]]
  expect_identical(s2$n_models, 1L)
  expect_identical(s2$n_models_requested, 3L)
  expect_true("ensemble_mean" %in% names(s2$pipelines))
})
