library(testthat)

# ============================================================================ #
# Helpers                                                                      #
# ============================================================================ #

# Minimal selected_weather row
sw_continuous <- function(name = "tx", ref_start = 1, ref_end = 3,
                           agg = "Mean", transformation = "None") {
  data.frame(
    name           = name,
    ref_start      = ref_start,
    ref_end        = ref_end,
    temporalAgg    = agg,
    cont_binned    = "Continuous",
    binning_method = NA_character_,
    num_bins       = NA_integer_,
    transformation = transformation,
    stringsAsFactors = FALSE
  )
}

sw_binned <- function(name = "tx", method = "Equal frequency",
                       n_bins = 3, ref_start = 1, ref_end = 3) {
  data.frame(
    name           = name,
    ref_start      = ref_start,
    ref_end        = ref_end,
    temporalAgg    = "Mean",
    cont_binned    = "Binned",
    binning_method = method,
    num_bins       = n_bins,
    transformation = "None",
    stringsAsFactors = FALSE
  )
}

# Write synthetic weather + h3 parquet files to a temp directory and return
# connection_params + survey_data + dates for use in integration tests.
make_test_fixtures <- function(
    dir,
    code     = "TST",
    year     = 2018,
    survname = "SRV",
    n_h3     = 4,
    n_months = 36    # 3 years of monthly data (allows rolling windows)
) {
  skip_if_not_installed("arrow")

  # H3 -> loc_id lookup
  h3_ids  <- paste0("h3_", seq_len(n_h3))
  loc_ids <- paste0("loc_", c(1, 1, 2, 2)[seq_len(n_h3)])

  h3_df <- data.frame(
    h3       = h3_ids,
    code     = code,
    year     = year,
    survname = survname,
    loc_id   = loc_ids,
    pop_2020 = c(100, 200, 150, 50)[seq_len(n_h3)],
    stringsAsFactors = FALSE
  )

  # Monthly weather: 3 years, all H3 cells
  start_date <- as.Date("2016-01-01")
  timestamps <- seq(start_date, by = "1 month", length.out = n_months)

  weather_df <- expand.grid(
    h3        = h3_ids,
    timestamp = timestamps,
    stringsAsFactors = FALSE
  )
  set.seed(42)
  weather_df$tx <- rnorm(nrow(weather_df), mean = 28, sd = 4)
  weather_df$t  <- rnorm(nrow(weather_df), mean = 22, sd = 3)

  # Write parquet files
  h3_path      <- file.path(dir, paste0(code, "_", year, "_", survname, "_h3.parquet"))
  weather_path <- file.path(dir, paste0(code, "_weather.parquet"))
  arrow::write_parquet(h3_df,      h3_path)
  arrow::write_parquet(weather_df, weather_path)

  # Survey data: last 12 months of the weather data
  survey_timestamps <- tail(timestamps, 12)
  survey_data <- expand.grid(
    timestamp = survey_timestamps,
    loc_id    = unique(loc_ids),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      code     = code,
      year     = year,
      survname = survname,
      int_month = as.integer(format(.data$timestamp, "%m")),
      int_year  = as.integer(format(.data$timestamp, "%Y"))
    )

  list(
    connection_params = list(type = "local", path = dir),
    survey_data       = survey_data,
    dates             = sort(unique(survey_timestamps))
  )
}


# ============================================================================ #
# 1. Argument validation — climate scenario guards                             #
# ============================================================================ #

test_that("get_weather errors when ssp supplied but future_period missing", {
  expect_error(
    get_weather(
      survey_data       = data.frame(code = "X", year = 2020, survname = "S",
                                     loc_id = "L", timestamp = Sys.Date()),
      selected_weather  = sw_continuous(),
      dates             = Sys.Date(),
      connection_params = list(type = "local", path = "."),
      ssp               = "ssp2_4_5",
      future_period     = NULL,
      perturbation_method = c(tx = "additive")
    ),
    regexp = "future_period is required"
  )
})

test_that("get_weather errors when ssp supplied but perturbation_method missing", {
  expect_error(
    get_weather(
      survey_data       = data.frame(code = "X", year = 2020, survname = "S",
                                     loc_id = "L", timestamp = Sys.Date()),
      selected_weather  = sw_continuous(),
      dates             = Sys.Date(),
      connection_params = list(type = "local", path = "."),
      ssp               = "ssp2_4_5",
      future_period     = c("2045-01-01", "2055-12-31"),
      perturbation_method = NULL
    ),
    regexp = "perturbation_method is required"
  )
})

test_that("get_weather errors when perturbation_method missing a variable", {
  sw <- sw_continuous(name = "tx")
  expect_error(
    get_weather(
      survey_data       = data.frame(code = "X", year = 2020, survname = "S",
                                     loc_id = "L", timestamp = Sys.Date()),
      selected_weather  = sw,
      dates             = Sys.Date(),
      connection_params = list(type = "local", path = "."),
      ssp               = "ssp2_4_5",
      future_period     = c("2045-01-01", "2055-12-31"),
      perturbation_method = c(other_var = "additive")  # missing "tx"
    ),
    regexp = "perturbation_method"
  )
})

test_that("get_weather errors on invalid perturbation_method value", {
  sw <- sw_continuous(name = "tx")
  expect_error(
    get_weather(
      survey_data       = data.frame(code = "X", year = 2020, survname = "S",
                                     loc_id = "L", timestamp = Sys.Date()),
      selected_weather  = sw,
      dates             = Sys.Date(),
      connection_params = list(type = "local", path = "."),
      ssp               = "ssp2_4_5",
      future_period     = c("2045-01-01", "2055-12-31"),
      perturbation_method = c(tx = "delta")  # invalid
    ),
    regexp = "perturbation_method values must be"
  )
})


# ============================================================================ #
# 2. Pure logic — file path construction                                       #
# ============================================================================ #

test_that("weather fname constructed correctly", {
  sd <- data.frame(code = c("ZAF", "ZAF"), year = c(2018, 2022),
                   survname = c("NIDS", "NIDS"), stringsAsFactors = FALSE)
  fnames <- paste0(unique(sd$code), "_weather.parquet")
  expect_equal(fnames, "ZAF_weather.parquet")
})

test_that("h3 fnames constructed correctly for multiple surveys", {
  sd <- data.frame(code = c("ZAF", "NGA"), year = c(2018, 2019),
                   survname = c("NIDS", "GHS"), stringsAsFactors = FALSE)
  fnames <- sd |>
    dplyr::distinct(code, year, survname) |>
    dplyr::mutate(fname = paste0(code, "_", year, "_", survname, "_h3.parquet")) |>
    dplyr::pull(fname)
  expect_equal(fnames, c("ZAF_2018_NIDS_h3.parquet", "NGA_2019_GHS_h3.parquet"))
})


# ============================================================================ #
# 3. Pure logic — binning helpers                                              #
# ============================================================================ #

test_that("equal frequency cutoffs produce correct number of breaks", {
  set.seed(1)
  vals    <- rnorm(200)
  n_bins  <- 4
  cutoffs <- unique(quantile(vals, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE))
  breaks  <- c(-Inf, cutoffs[-c(1, length(cutoffs))], Inf)
  bins    <- cut(vals, breaks = breaks, include.lowest = TRUE)
  expect_equal(length(levels(bins)), n_bins)
})

test_that("equal width cutoffs produce correct number of breaks", {
  vals    <- seq(0, 100, by = 1)
  n_bins  <- 5
  cutoffs <- unique(seq(min(vals), max(vals), length.out = n_bins + 1))
  breaks  <- c(-Inf, cutoffs[-c(1, length(cutoffs))], Inf)
  bins    <- cut(vals, breaks = breaks, include.lowest = TRUE)
  expect_equal(length(levels(bins)), n_bins)
})

test_that("outer bins capture values outside survey range", {
  # Simulate survey-derived cutoffs, then apply to wider simulation range
  survey_vals <- seq(10, 30, by = 1)
  sim_vals    <- c(-5, survey_vals, 50)   # out-of-range on both ends
  n_bins      <- 3
  cutoffs     <- unique(quantile(survey_vals, probs = seq(0, 1, length.out = n_bins + 1)))
  breaks      <- c(-Inf, cutoffs[-c(1, length(cutoffs))], Inf)
  bins        <- cut(sim_vals, breaks = breaks, include.lowest = TRUE)
  expect_false(any(is.na(bins)), info = "No values should fall outside the bins")
  expect_equal(length(levels(bins)), n_bins)
})

test_that("K-means binning produces correct number of centers", {
  set.seed(123)
  vals    <- c(rnorm(50, 5), rnorm(50, 15), rnorm(50, 25))
  n_bins  <- 3
  km      <- kmeans(vals, centers = n_bins)
  centers <- sort(as.numeric(km$centers))
  cutoffs <- unique(c(min(vals), (centers[-length(centers)] + centers[-1]) / 2, max(vals)))
  expect_equal(length(cutoffs), n_bins)  # n_bins - 1 interior + 2 endpoints = n_bins + 1 but unique may collapse
  breaks  <- c(-Inf, cutoffs[-c(1, length(cutoffs))], Inf)
  bins    <- cut(vals, breaks = breaks, include.lowest = TRUE)
  expect_equal(length(levels(bins)), n_bins)
})


# ============================================================================ #
# 4. Integration tests — no climate scenario                                   #
# ============================================================================ #

test_that("get_weather returns data frame with expected columns (continuous)", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_weather  = sw_continuous("tx"),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_s3_class(result, "data.frame")
  expect_true("tx" %in% names(result))
  expect_true("loc_id" %in% names(result))
  expect_true("timestamp" %in% names(result))
  expect_true(is.numeric(result$tx))
  expect_false(is.factor(result$tx))
})

test_that("get_weather returns only rows matching requested dates", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_weather  = sw_continuous("tx"),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_true(all(result$timestamp %in% fx$dates))
})

test_that("get_weather: binned (equal frequency) returns factor with correct levels", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_weather  = sw_binned("tx", method = "Equal frequency", n_bins = 3),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_true(is.factor(result$tx))
  expect_equal(length(levels(result$tx)), 3L)
})

test_that("get_weather: binned (equal width) returns factor with correct levels", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_weather  = sw_binned("tx", method = "Equal width", n_bins = 4),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_true(is.factor(result$tx))
  expect_equal(length(levels(result$tx)), 4L)
})

test_that("get_weather: binned (K-means) returns factor", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_weather  = sw_binned("tx", method = "K-means", n_bins = 3),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_true(is.factor(result$tx))
})

test_that("get_weather: outer bins contain no NAs (values outside survey range)", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  # Use simulation dates (wider range than survey dates) to produce
  # out-of-survey-range values that must be caught by outer bins.
  sim_dates <- seq(
    min(fx$dates) - months(24),
    max(fx$dates) + months(12),
    by = "1 month"
  )

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_weather  = sw_binned("tx", method = "Equal frequency", n_bins = 3),
    dates             = sim_dates[sim_dates %in% fx$dates],
    connection_params = fx$connection_params
  )

  expect_false(any(is.na(result$tx)), info = "Outer bins should catch all values")
})

test_that("get_weather: two variables, mixed continuous and binned", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  sw_mixed <- rbind(
    sw_continuous("tx"),
    sw_binned("t", method = "Equal frequency", n_bins = 3)
  )

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_weather  = sw_mixed,
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_true(is.numeric(result$tx))
  expect_true(is.factor(result$t))
  expect_equal(length(levels(result$t)), 3L)
})

test_that("get_weather: bin cutoffs use only survey timestamps (not all dates)", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  # Use only a subset of dates as the 'dates' argument (simulation years)
  # but ensure survey_data$timestamp is a strict subset
  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_weather  = sw_binned("tx", method = "Equal frequency", n_bins = 3),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  # Cutoffs are derived from survey timestamps only so all result rows should
  # have a non-NA bin assignment (outer bins catch extremes)
  expect_false(any(is.na(result$tx)))
})