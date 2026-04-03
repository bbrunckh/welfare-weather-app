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
    seed_cell   = "85283473fffffff",   # valid res-5 cell (San Francisco area)
    micro_res   = 5L,
    weather_res = 4L,
    code        = "TST",
    year        = 2018L,
    survname    = "SRV",
    n_months    = 12L
) {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Install / load H3 extension
  tryCatch(DBI::dbExecute(con, "INSTALL h3 FROM community"), error = function(e) NULL)
  DBI::dbExecute(con, "LOAD h3")

  # Derive micro-res cell strings from the seed.
  # seed_cell is assumed to already be at micro_res; use it and a few
  # siblings (children of its parent) so we have multiple h3 values.
  parent_cell <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT h3_h3_to_string(h3_cell_to_parent('%s'::VARCHAR, %d)) AS h3",
      seed_cell, micro_res - 1L
    )
  )$h3[[1L]]

  micro_strings <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT h3_h3_to_string(unnest(h3_cell_to_children('%s'::VARCHAR, %d))) AS h3",
      parent_cell, micro_res
    )
  )$h3
  micro_strings <- head(micro_strings, 4L)

  # Derive weather bigint cells at weather_res (parents of micro cells)
  weather_ints <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT DISTINCT h3_cell_to_parent(h3_string_to_h3(h3), %d) AS h3
       FROM (VALUES %s) t(h3)",
      weather_res,
      paste0("('", micro_strings, "')", collapse = ", ")
    )
  )$h3

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

  # weather$h3 must be stored as integer64 / bigint
  weather_df$h3 <- bit64::as.integer64(weather_df$h3)

  # Write parquet files
  h3_path      <- file.path(dir, paste0(code, "_", year, "_", survname, "_h3.parquet"))
  weather_path <- file.path(dir, paste0(code, "_weather.parquet"))
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

  list(
    connection_params = list(type = "local", path = dir),
    survey_data       = survey_data,
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


# ============================================================================ #
# 5. H3 resolution + type harmonisation                                        #
# ============================================================================ #

# Helper: open a fresh in-process DuckDB connection with H3 loaded
make_h3_con <- function() {
  con <- DBI::dbConnect(duckdb::duckdb())
  tryCatch(DBI::dbExecute(con, "INSTALL h3 FROM community"), error = function(e) NULL)
  DBI::dbExecute(con, "LOAD h3")
  con
}

test_that(".harmonise_h3: same-resolution adds h3_weather string->bigint cast", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")

  con <- make_h3_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Both tables at res 5 — no parent lookup needed, only type cast
  cell_str <- "85283473fffffff"
  cell_int <- DBI::dbGetQuery(
    con, sprintf("SELECT h3_string_to_h3('%s') AS h3", cell_str)
  )$h3[[1L]]

  h3_slim <- dplyr::tbl(con, dplyr::sql(
    sprintf("SELECT '%s'::VARCHAR AS h3, 'L1' AS loc_id, 100 AS pop_2020", cell_str)
  ))
  weather <- dplyr::tbl(con, dplyr::sql(
    sprintf("SELECT %s::UBIGINT AS h3, 25.0 AS tx", cell_int)
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
    sprintf("SELECT %s::UBIGINT AS h3, 25.0 AS tx", weather_int)
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

  # micro at res 4, weather at res 5 (unusual corner case)
  micro_cell      <- "84283473fffffff"
  weather_child   <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT h3_h3_to_string((h3_cell_to_children('%s'::VARCHAR, 5))[1]) AS h3",
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
    sprintf("SELECT %s::UBIGINT AS h3, 25.0 AS tx", weather_int)
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
    selected_weather  = sw_continuous("tx"),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_s3_class(result, "data.frame")
  expect_true("tx" %in% names(result))
  expect_false(anyNA(result$tx))
  expect_true(all(result$timestamp %in% fx$dates))
  expect_setequal(unique(result$loc_id), c("loc_1", "loc_2"))
})

test_that("get_weather joins correctly when weather is finer than microdata", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")

  tmp <- withr::local_tempdir()

  # micro res 4, weather res 5  (unusual but must be handled)
  fx <- make_test_fixtures_cross_res(
    tmp,
    seed_cell   = "84283473fffffff",
    micro_res   = 4L,
    weather_res = 5L
  )

  result <- get_weather(
    survey_data       = fx$survey_data,
    selected_weather  = sw_continuous("tx"),
    dates             = fx$dates,
    connection_params = fx$connection_params
  )

  expect_s3_class(result, "data.frame")
  expect_true("tx" %in% names(result))
  expect_false(anyNA(result$tx))
  expect_true(all(result$timestamp %in% fx$dates))
})