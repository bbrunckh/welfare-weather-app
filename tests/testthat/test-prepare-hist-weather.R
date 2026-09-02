# PERF-26: int_month/sim_year derivation via POSIXlt (truncating) instead of
# format() (which rounds fractional seconds and can roll boundary timestamps
# into the next month/year).

test_that("prepare_hist_weather derives int_month/sim_year from Date timestamps, matching format() for whole-second values", {
  ts  <- as.Date(sprintf("2020-%02d-01", 1:12))
  weather_raw <- dplyr::tibble(
    code      = "H",
    year      = 2020L,
    survname  = "MICS",
    loc_id    = "L1",
    int_month = rep(0L, 12L), # stale pre-existing column must be overwritten
    rain      = 1:12,
    timestamp = ts
  )
  survey_weather <- dplyr::tibble(
    code      = "H",
    year      = 2020L,
    survname  = "MICS",
    loc_id    = "L1",
    int_month = 1:12,
    income    = rnorm(12),
    hhsize    = rep(2, 12)
  )

  out <- prepare_hist_weather(
    weather_raw, survey_weather,
    selected_weather = data.frame(name = "rain"),
    outcome_name     = "income"
  )

  expect_equal(nrow(out), 12L)
  expect_equal(out$int_month, 1:12)
  expect_equal(out$sim_year, rep(2020L, 12L))
  expect_true(is.factor(out$year))
  expect_equal(as.character(out$year), rep("2020", 12L))
  expect_s3_class(out, "data.frame")
  expect_false("timestamp" %in% names(out))
  expect_false("income" %in% names(out))
  expect_true("hhsize" %in% names(out) && "rain" %in% names(out))
  # Output-neutral with the old implementation for whole-second timestamps
  expect_equal(out$int_month, as.integer(format(ts, "%m")))
  expect_equal(out$sim_year, as.integer(format(ts, "%Y")))
})

test_that("prepare_hist_weather truncates fractional-second POSIXct timestamps at the month/year boundary", {
  weather_raw <- dplyr::tibble(
    code      = "H",
    year      = 2019L,
    survname  = "MICS",
    loc_id    = "L1",
    rain      = 1,
    timestamp = as.POSIXct("2019-12-31 23:59:59.7", tz = "UTC")
  )
  survey_weather <- dplyr::tibble(
    code      = "H",
    year      = 2019L,
    survname  = "MICS",
    loc_id    = "L1",
    int_month = 12L,
    income    = 0.5,
    hhsize    = 3
  )

  out <- prepare_hist_weather(
    weather_raw, survey_weather,
    selected_weather = data.frame(name = "rain"),
    outcome_name     = "income"
  )

  # POSIXlt truncates; format() rounding would have yielded month 1 / year 2020
  expect_equal(out$int_month, 12L)
  expect_equal(out$sim_year, 2019L)
  expect_equal(nrow(out), 1L)
})

test_that("prepare_hist_weather drops rows with NA timestamps via the join", {
  weather_raw <- dplyr::tibble(
    code      = "H",
    year      = 2020L,
    survname  = "MICS",
    loc_id    = "L1",
    rain      = c(1, 2),
    timestamp = as.Date(c("2020-01-01", NA))
  )
  survey_weather <- dplyr::tibble(
    code      = "H",
    year      = 2020L,
    survname  = "MICS",
    loc_id    = "L1",
    int_month = 1L,
    income    = 0.1,
    hhsize    = 1
  )

  out <- prepare_hist_weather(
    weather_raw, survey_weather,
    selected_weather = data.frame(name = "rain"),
    outcome_name     = "income"
  )

  expect_equal(nrow(out), 1L)
  expect_equal(out$int_month, 1L)
})
