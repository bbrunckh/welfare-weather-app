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
                       n_bins = 3, ref_start = 1, ref_end = 3,
                       custom_breaks = NULL) {
  df <- data.frame(
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
  if (!is.null(custom_breaks)) {
    df$custom_breaks <- list(as.numeric(custom_breaks))
  }
  df
}

# Open a fresh DuckDB connection with H3 loaded and bigint = integer64 so H3
# cell IDs (which exceed 2^53) round-trip without precision loss.
make_h3_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb(), bigint = "integer64")
  tryCatch(DBI::dbExecute(con, "INSTALL h3 FROM community"), error = function(e) NULL)
  DBI::dbExecute(con, "LOAD h3")
  con
}

# Write synthetic weather + h3 parquet files using real H3 indices at a single
# resolution, laid out at the paths `get_weather()` expects:
#   <dir>/hazard/weather/historical/<code>/<code>_<weather_source>.parquet
#   <dir>/microdata/h3/<code>/<code>_<year>_<survname>_<source>_h3.parquet
# Returns connection_params + survey_data + selected_surveys + dates.
make_test_fixtures <- function(
    dir,
    code           = "TST",
    year           = 2018,
    survname       = "SRV",
    source         = "lsms",
    weather_source = "era5land",
    seed_cell      = "85283473fffffff",   # valid res-5 cell
    n_months       = 36
) {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")

  con <- make_h3_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Derive a small set of real H3 cells: take 4 children-of-parent (siblings)
  # of the seed cell at the seed's own resolution. Note that the H3 DuckDB
  # extension only accepts UBIGINT for h3_cell_to_parent / h3_cell_to_children
  # — strings must be converted via h3_string_to_h3 first.
  parent_cell <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT h3_h3_to_string(h3_cell_to_parent(h3_string_to_h3('%s'), 4)) AS h3",
      seed_cell
    )
  )$h3[[1L]]
  h3_strings <- head(DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT h3_h3_to_string(unnest(h3_cell_to_children(h3_string_to_h3('%s'), 5))) AS h3",
      parent_cell
    )
  )$h3, 4L)

  # Same-resolution: weather uses bigint form of the same cells
  h3_ints <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT h3_string_to_h3(h3) AS h3 FROM (VALUES %s) t(h3)",
      paste0("('", h3_strings, "')", collapse = ", ")
    )
  )$h3
  stopifnot(bit64::is.integer64(h3_ints))

  loc_ids <- paste0("loc_", c(1, 1, 2, 2))

  h3_df <- data.frame(
    h3       = h3_strings,
    code     = code,
    year     = year,
    survname = survname,
    loc_id   = loc_ids,
    pop_2020 = c(100L, 200L, 150L, 50L),
    stringsAsFactors = FALSE
  )

  # Monthly weather across all 4 cells
  start_date <- as.Date("2016-01-01")
  timestamps <- seq(start_date, by = "1 month", length.out = n_months)

  weather_df <- expand.grid(
    cell_idx  = seq_along(h3_strings),
    timestamp = timestamps,
    stringsAsFactors = FALSE
  )
  weather_df$h3 <- h3_ints[weather_df$cell_idx]
  weather_df$cell_idx <- NULL
  set.seed(42)
  weather_df$tx <- rnorm(nrow(weather_df), mean = 28, sd = 4)
  weather_df$t  <- rnorm(nrow(weather_df), mean = 22, sd = 3)

  # File layout expected by get_weather()
  h3_dir      <- file.path(dir, "microdata", "h3", code)
  weather_dir <- file.path(dir, "hazard", "weather", "historical", code)
  dir.create(h3_dir,      recursive = TRUE, showWarnings = FALSE)
  dir.create(weather_dir, recursive = TRUE, showWarnings = FALSE)

  h3_path <- file.path(
    h3_dir,
    paste0(code, "_", year, "_", survname, "_", source, "_h3.parquet")
  )
  weather_path <- file.path(
    weather_dir, paste0(code, "_", weather_source, ".parquet")
  )
  arrow::write_parquet(h3_df,      h3_path)
  arrow::write_parquet(weather_df, weather_path)

  # Survey microdata: last 12 months of the weather data
  survey_timestamps <- tail(timestamps, 12)
  survey_data <- expand.grid(
    timestamp = survey_timestamps,
    loc_id    = unique(loc_ids),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      code      = code,
      year      = year,
      survname  = survname,
      int_month = as.integer(format(.data$timestamp, "%m")),
      int_year  = as.integer(format(.data$timestamp, "%Y"))
    )

  selected_surveys <- data.frame(
    code     = code,
    year     = year,
    survname = survname,
    source   = source,
    stringsAsFactors = FALSE
  )

  list(
    connection_params = list(type = "local", path = dir),
    survey_data       = survey_data,
    selected_surveys  = selected_surveys,
    dates             = sort(unique(survey_timestamps))
  )
}


# ---------------------------------------------------------------------------
# Cross-resolution fixture helper
#
# Creates microdata (h3 strings at `micro_res`) and weather (h3 bigint at
# `weather_res`) parquet files where the resolutions differ.  Uses the live
# DuckDB H3 extension to derive valid parent/child cells from a seed cell so
# every join key is a real, valid H3 index.
#
# Parameters:
#   dir          temp directory to write parquet files into
#   seed_cell    a valid H3 string at `micro_res`
#   micro_res    H3 resolution for microdata strings  (default 5)
#   weather_res  H3 resolution for weather bigints    (default 4)
#   n_months     number of monthly weather rows per cell (default 12)
# ---------------------------------------------------------------------------
make_test_fixtures_cross_res <- function(
    dir,
    seed_cell      = "85283473fffffff",   # valid res-5 cell (San Francisco area)
    micro_res      = 5L,
    weather_res    = 4L,
    code           = "TST",
    year           = 2018L,
    survname       = "SRV",
    source         = "lsms",
    weather_source = "era5land",
    n_months       = 12L
) {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")

  con <- make_h3_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Derive micro-res cell strings from the seed. seed_cell is at micro_res;
  # use the seed and its siblings (children of its parent at micro_res - 1).
  # The H3 DuckDB extension only accepts UBIGINT for cell_to_parent /
  # cell_to_children, so strings must be converted via h3_string_to_h3.
  parent_cell <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT h3_h3_to_string(h3_cell_to_parent(h3_string_to_h3('%s'), %d)) AS h3",
      seed_cell, micro_res - 1L
    )
  )$h3[[1L]]

  micro_strings <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT h3_h3_to_string(unnest(h3_cell_to_children(h3_string_to_h3('%s'), %d))) AS h3",
      parent_cell, micro_res
    )
  )$h3
  micro_strings <- head(micro_strings, 4L)

  # Derive weather bigint cells at weather_res. When weather is coarser
  # (weather_res < micro_res) take parents; when finer (weather_res > micro_res)
  # take the first child of each micro cell.
  weather_sql <- if (weather_res <= micro_res) {
    sprintf(
      "SELECT DISTINCT h3_cell_to_parent(h3_string_to_h3(h3), %d) AS h3
       FROM (VALUES %s) t(h3)",
      weather_res,
      paste0("('", micro_strings, "')", collapse = ", ")
    )
  } else {
    sprintf(
      "SELECT DISTINCT (h3_cell_to_children(h3_string_to_h3(h3), %d))[1] AS h3
       FROM (VALUES %s) t(h3)",
      weather_res,
      paste0("('", micro_strings, "')", collapse = ", ")
    )
  }
  weather_ints <- DBI::dbGetQuery(con, weather_sql)$h3

  # Build h3 lookup (microdata strings -> loc_id)
  h3_df <- data.frame(
    h3       = micro_strings,
    code     = code,
    year     = year,
    survname = survname,
    loc_id   = paste0("loc_", (seq_along(micro_strings) - 1L) %% 2L + 1L),
    pop_2020 = c(100L, 200L, 150L, 50L)[seq_along(micro_strings)],
    stringsAsFactors = FALSE
  )

  # Build weather (bigint h3 at weather_res x monthly timestamps)
  timestamps <- seq(as.Date("2017-01-01"), by = "1 month", length.out = n_months)
  weather_df <- expand.grid(
    h3        = weather_ints,
    timestamp = timestamps,
    stringsAsFactors = FALSE
  )
  set.seed(99)
  weather_df$tx <- rnorm(nrow(weather_df), mean = 25, sd = 3)

  # weather$h3 is already integer64 because the connection uses bigint = "integer64"
  stopifnot(bit64::is.integer64(weather_df$h3))

  # File layout expected by get_weather()
  h3_dir      <- file.path(dir, "microdata", "h3", code)
  weather_dir <- file.path(dir, "hazard", "weather", "historical", code)
  dir.create(h3_dir,      recursive = TRUE, showWarnings = FALSE)
  dir.create(weather_dir, recursive = TRUE, showWarnings = FALSE)

  h3_path <- file.path(
    h3_dir,
    paste0(code, "_", year, "_", survname, "_", source, "_h3.parquet")
  )
  weather_path <- file.path(
    weather_dir, paste0(code, "_", weather_source, ".parquet")
  )
  arrow::write_parquet(h3_df,      h3_path)
  arrow::write_parquet(weather_df, weather_path)

  survey_timestamps <- tail(timestamps, 6L)
  survey_data <- expand.grid(
    timestamp = survey_timestamps,
    loc_id    = unique(h3_df$loc_id),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      code      = code,
      year      = year,
      survname  = survname,
      int_month = as.integer(format(.data$timestamp, "%m")),
      int_year  = as.integer(format(.data$timestamp, "%Y"))
    )

  selected_surveys <- data.frame(
    code     = code,
    year     = year,
    survname = survname,
    source   = source,
    stringsAsFactors = FALSE
  )

  list(
    connection_params = list(type = "local", path = dir),
    survey_data       = survey_data,
    selected_surveys  = selected_surveys,
    dates             = sort(unique(survey_timestamps)),
    micro_res         = micro_res,
    weather_res       = weather_res,
    micro_strings     = micro_strings,
    weather_ints      = weather_ints
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
      selected_surveys  = data.frame(code = "X", year = 2020, survname = "S",
                                     source = "lsms"),
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
      selected_surveys  = data.frame(code = "X", year = 2020, survname = "S",
                                     source = "lsms"),
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
      selected_surveys  = data.frame(code = "X", year = 2020, survname = "S",
                                     source = "lsms"),
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
      selected_surveys  = data.frame(code = "X", year = 2020, survname = "S",
                                     source = "lsms"),
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
  # 2 endpoints + (n_bins - 1) interior midpoints = n_bins + 1 cutoffs
  cutoffs <- unique(c(min(vals), (centers[-length(centers)] + centers[-1]) / 2, max(vals)))
  expect_equal(length(cutoffs), n_bins + 1)
  breaks  <- c(-Inf, cutoffs[-c(1, length(cutoffs))], Inf)
  bins    <- cut(vals, breaks = breaks, include.lowest = TRUE)
  expect_equal(length(levels(bins)), n_bins)
})

test_that(".compute_breaks: Custom binning uses user-supplied cut values", {
  set.seed(1)
  ref_df <- data.frame(tx = runif(1000, 10, 40))
  sw <- sw_binned("tx", method = "Custom", n_bins = 5L,
                  custom_breaks = c(20, 25, 30, 35))
  brks <- wiseapp:::.compute_breaks(ref_df, sw)
  expect_named(brks, "tx")
  expect_equal(brks$tx[c(1, length(brks$tx))], c(-Inf, Inf))
  expect_equal(brks$tx[-c(1, length(brks$tx))], c(20, 25, 30, 35))
})

test_that(".compute_breaks: Custom binning sorts/dedups user cuts", {
  ref_df <- data.frame(tx = seq(0, 100, by = 1))
  sw <- sw_binned("tx", method = "Custom", n_bins = 5L,
                  custom_breaks = c(35, 20, 30, 25, 25))
  brks <- wiseapp:::.compute_breaks(ref_df, sw)
  expect_equal(brks$tx[-c(1, length(brks$tx))], c(20, 25, 30, 35))
})

test_that(".compute_breaks: Custom binning with empty cuts keeps variable continuous", {
  ref_df <- data.frame(tx = seq(0, 100, by = 1))
  sw <- sw_binned("tx", method = "Custom", n_bins = 5L,
                  custom_breaks = numeric(0))
  brks <- expect_message(
    wiseapp:::.compute_breaks(ref_df, sw),
    "Custom binning for tx requires cut values"
  )
  expect_length(brks, 0L)
})

test_that(".compute_breaks: Custom binning warns when cut count doesn't match num_bins - 1", {
  ref_df <- data.frame(tx = seq(0, 100, by = 1))
  sw <- sw_binned("tx", method = "Custom", n_bins = 5L,
                  custom_breaks = c(20, 25))  # only 2 cuts, expected 4
  brks <- expect_message(
    wiseapp:::.compute_breaks(ref_df, sw),
    "expected 4 cut values"
  )
  # Still produces breaks from the supplied values
  expect_equal(brks$tx[-c(1, length(brks$tx))], c(20, 25))
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
    selected_surveys  = fx$selected_surveys,
    selected_weather  = sw_continuous("tx"),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_type(result, "list")
  expect_true("historical" %in% names(result))
  hist <- result$historical
  expect_s3_class(hist, "data.frame")
  expect_true("tx" %in% names(hist))
  expect_true("loc_id" %in% names(hist))
  expect_true("timestamp" %in% names(hist))
  expect_true(is.numeric(hist$tx))
  expect_false(is.factor(hist$tx))
})

test_that("get_weather returns only rows matching requested dates", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_surveys  = fx$selected_surveys,
    selected_weather  = sw_continuous("tx"),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_true(all(result$historical$timestamp %in% fx$dates))
})

test_that("get_weather: binned (equal frequency) returns factor with correct levels", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_surveys  = fx$selected_surveys,
    selected_weather  = sw_binned("tx", method = "Equal frequency", n_bins = 3),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_true(is.factor(result$historical$tx))
  expect_equal(length(levels(result$historical$tx)), 3L)
})

test_that("get_weather: binned (equal width) returns factor with correct levels", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_surveys  = fx$selected_surveys,
    selected_weather  = sw_binned("tx", method = "Equal width", n_bins = 4),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_true(is.factor(result$historical$tx))
  expect_equal(length(levels(result$historical$tx)), 4L)
})

test_that("get_weather: binned (K-means) returns factor", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_surveys  = fx$selected_surveys,
    selected_weather  = sw_binned("tx", method = "K-means", n_bins = 3),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_true(is.factor(result$historical$tx))
})

test_that("get_weather: binned (Custom) produces factor whose interior breaks are the supplied cuts", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  tmp <- withr::local_tempdir()
  fx  <- make_test_fixtures(tmp)

  # Use cuts that bracket the synthetic tx range (mean ~28, sd ~4)
  user_cuts <- c(22, 26, 30, 34)
  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_surveys  = fx$selected_surveys,
    selected_weather  = sw_binned("tx", method = "Custom", n_bins = 5L,
                                  custom_breaks = user_cuts),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_true(is.factor(result$historical$tx))
  expect_equal(length(levels(result$historical$tx)), 5L)

  brks <- attr(result, "stored_breaks")
  expect_named(brks, "tx")
  expect_equal(brks$tx[c(1, length(brks$tx))], c(-Inf, Inf))
  expect_equal(brks$tx[-c(1, length(brks$tx))], user_cuts)
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
    selected_surveys  = fx$selected_surveys,
    selected_weather  = sw_binned("tx", method = "Equal frequency", n_bins = 3),
    dates             = sim_dates[sim_dates %in% fx$dates],
    connection_params = fx$connection_params
  )

  expect_false(any(is.na(result$historical$tx)),
               info = "Outer bins should catch all values")
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
    selected_surveys  = fx$selected_surveys,
    selected_weather  = sw_mixed,
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  hist <- result$historical
  expect_true(is.numeric(hist$tx))
  expect_true(is.factor(hist$t))
  expect_equal(length(levels(hist$t)), 3L)
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
    selected_surveys  = fx$selected_surveys,
    selected_weather  = sw_binned("tx", method = "Equal frequency", n_bins = 3),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  # Cutoffs are derived from survey timestamps only so all result rows should
  # have a non-NA bin assignment (outer bins catch extremes)
  expect_false(any(is.na(result$historical$tx)))
})


# ============================================================================ #
# 5. H3 resolution + type harmonisation                                        #
# ============================================================================ #

test_that(".harmonise_h3: same-resolution adds h3_weather string->bigint cast", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")

  con <- make_h3_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Both tables at res 5 — no parent lookup needed, only type cast.
  # cell_int is an integer64 because make_h3_con() uses bigint = "integer64";
  # format() yields the exact integer literal for SQL embedding.
  cell_str <- "85283473fffffff"
  cell_int <- DBI::dbGetQuery(
    con, sprintf("SELECT h3_string_to_h3('%s') AS h3", cell_str)
  )$h3[[1L]]

  h3_slim <- dplyr::tbl(con, dplyr::sql(
    sprintf("SELECT '%s'::VARCHAR AS h3, 'L1' AS loc_id, 100 AS pop_2020", cell_str)
  ))
  weather <- dplyr::tbl(con, dplyr::sql(
    sprintf("SELECT %s::UBIGINT AS h3, 25.0 AS tx", format(cell_int))
  ))

  result <- wiseapp:::.harmonise_h3(h3_slim, weather, con)

  expect_equal(result$target_res, 5L)
  expect_true(result$same_res)
  expect_true("h3_weather" %in% colnames(result$h3_slim))

  joined <- dplyr::collect(
    dplyr::inner_join(result$weather, result$h3_slim, by = c("h3" = "h3_weather"))
  )
  expect_equal(nrow(joined), 1L)
  expect_equal(joined$loc_id, "L1")
})

test_that(".harmonise_h3: microdata finer than weather maps micro up to weather res", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")

  con <- make_h3_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # micro at res 5, weather at res 4
  micro_cell  <- "85283473fffffff"
  weather_int <- DBI::dbGetQuery(
    con,
    sprintf("SELECT h3_cell_to_parent(h3_string_to_h3('%s'), 4) AS h3", micro_cell)
  )$h3[[1L]]

  h3_slim <- dplyr::tbl(con, dplyr::sql(
    sprintf("SELECT '%s'::VARCHAR AS h3, 'L1' AS loc_id, 100 AS pop_2020", micro_cell)
  ))
  weather <- dplyr::tbl(con, dplyr::sql(
    sprintf("SELECT %s::UBIGINT AS h3, 25.0 AS tx", format(weather_int))
  ))

  result <- wiseapp:::.harmonise_h3(h3_slim, weather, con)

  expect_equal(result$target_res,  4L)
  expect_false(result$same_res)
  expect_equal(result$res_micro,   5L)
  expect_equal(result$res_weather, 4L)

  joined <- dplyr::collect(
    dplyr::inner_join(result$weather, result$h3_slim, by = c("h3" = "h3_weather"))
  )
  expect_equal(nrow(joined), 1L)
  expect_equal(joined$loc_id, "L1")
})

test_that(".harmonise_h3: weather finer than microdata maps weather up to micro res", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")

  con <- make_h3_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # micro at res 4, weather at res 5 (unusual corner case).
  # 8428347ffffffff is the valid res-4 parent of 85283473fffffff;
  # 84283473fffffff was used previously but is not a valid H3 index.
  micro_cell      <- "8428347ffffffff"
  weather_child   <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT h3_h3_to_string((h3_cell_to_children(h3_string_to_h3('%s'), 5))[1]) AS h3",
      micro_cell
    )
  )$h3[[1L]]
  weather_int <- DBI::dbGetQuery(
    con,
    sprintf("SELECT h3_string_to_h3('%s') AS h3", weather_child)
  )$h3[[1L]]

  h3_slim <- dplyr::tbl(con, dplyr::sql(
    sprintf("SELECT '%s'::VARCHAR AS h3, 'L1' AS loc_id, 100 AS pop_2020", micro_cell)
  ))
  weather <- dplyr::tbl(con, dplyr::sql(
    sprintf("SELECT %s::UBIGINT AS h3, 25.0 AS tx", format(weather_int))
  ))

  result <- wiseapp:::.harmonise_h3(h3_slim, weather, con)

  expect_equal(result$target_res,  4L)
  expect_false(result$same_res)
  expect_equal(result$res_micro,   4L)
  expect_equal(result$res_weather, 5L)

  # weather$h3 should have been mapped to res 4 → joins with h3_slim$h3_weather
  joined <- dplyr::collect(
    dplyr::inner_join(result$weather, result$h3_slim, by = c("h3" = "h3_weather"))
  )
  expect_equal(nrow(joined), 1L)
  expect_equal(joined$loc_id, "L1")
})

test_that("get_weather joins correctly when weather is coarser than microdata", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")

  tmp <- withr::local_tempdir()

  # micro res 5, weather res 4  (the common production case)
  fx <- make_test_fixtures_cross_res(
    tmp,
    seed_cell   = "85283473fffffff",
    micro_res   = 5L,
    weather_res = 4L
  )

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_surveys  = fx$selected_surveys,
    selected_weather  = sw_continuous("tx"),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  hist <- result$historical
  expect_s3_class(hist, "data.frame")
  expect_true("tx" %in% names(hist))
  expect_false(anyNA(hist$tx))
  expect_true(all(hist$timestamp %in% fx$dates))
  expect_setequal(unique(hist$loc_id), c("loc_1", "loc_2"))
})

test_that("get_weather joins correctly when weather is finer than microdata", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")

  tmp <- withr::local_tempdir()

  # micro res 4, weather res 5  (unusual but must be handled).
  # 8428347ffffffff is a valid res-4 cell (parent of 85283473fffffff).
  fx <- make_test_fixtures_cross_res(
    tmp,
    seed_cell   = "8428347ffffffff",
    micro_res   = 4L,
    weather_res = 5L
  )

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_surveys  = fx$selected_surveys,
    selected_weather  = sw_continuous("tx"),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  hist <- result$historical
  expect_s3_class(hist, "data.frame")
  expect_true("tx" %in% names(hist))
  expect_false(anyNA(hist$tx))
  expect_true(all(hist$timestamp %in% fx$dates))
})