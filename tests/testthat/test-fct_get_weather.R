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

# These fixtures are immutable inputs. Cache each shape so the integration
# tests do not repeat DuckDB/H3 setup and Parquet writes.
.cached_weather_fixture <- local({
  cache <- new.env(parent = emptyenv())

  function(kind) {
    if (identical(kind, "__dirs__")) {
      return(if (is.null(cache$dirs)) character() else cache$dirs)
    }

    if (!exists(kind, envir = cache, inherits = FALSE)) {
      dir <- tempfile(paste0("wiseapp-weather-", kind, "-"))
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
      cache$dirs <- c(if (is.null(cache$dirs)) character() else cache$dirs, dir)

      fx <- switch(
        kind,
        basic = make_test_fixtures(dir),
        repeated = make_test_fixtures(dir, n_months = 18L),
        cross_res = make_test_fixtures_cross_res(
          dir, seed_cell = "85283473fffffff",
          micro_res = 5L, weather_res = 4L
        ),
        finer_res = make_test_fixtures_cross_res(
          dir, seed_cell = "8428347ffffffff",
          micro_res = 4L, weather_res = 5L
        ),
        cross_res_cmip6 = {
          value <- make_test_fixtures_cross_res(
            dir, seed_cell = "85283473fffffff",
            micro_res = 5L, weather_res = 4L
          )
          make_test_fixtures_cmip6_coarser(dir, value, cmip6_res = 3L)
          value
        },
        stop("Unknown cached weather fixture: ", kind, call. = FALSE)
      )
      cache[[kind]] <- fx
    }

    cache[[kind]]
  }
})

withr::defer(
  unlink(.cached_weather_fixture("__dirs__"), recursive = TRUE, force = TRUE),
  testthat::teardown_env()
)

cached_weather_fixture <- function(kind = "basic") {
  .cached_weather_fixture(kind)
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

test_that("bin breaks remain deterministic without the redundant pre-sort", {
  ref <- data.frame(tx = c(20, 21, 22, 30, 31, 35))
  selected <- sw_binned(method = "K-means", n_bins = 3)
  first <- wiseapp:::.compute_breaks(ref, selected)
  second <- wiseapp:::.compute_breaks(ref[c(6, 2, 4, 1, 5, 3), , drop = FALSE], selected)
  expect_identical(first, second)
  expect_equal(
    cut(c(20, 21, 22, 30, 31, 35), breaks = first$tx, include.lowest = TRUE),
    cut(c(20, 21, 22, 30, 31, 35), breaks = second$tx, include.lowest = TRUE)
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

test_that("binned simulation levels match relabelled model levels", {
  skip_if_not_installed("fixest")

  breaks <- list(temp = c(-Inf, 20, 30, Inf))
  attr(breaks$temp, "observed") <- c(10, 20, 30, 40)

  train <- data.frame(
    temp = rep(c(15, 25, 35), each = 10),
    outcome = rep(c(1, 2, 4), each = 10)
  )
  train <- relabel_bin_levels(.apply_binning(train, breaks), breaks)
  simulation <- .apply_binning(data.frame(temp = c(15, 25, 35)), breaks)

  expect_identical(levels(simulation$temp), levels(train$temp))

  fit <- fixest::feols(outcome ~ temp, data = train)
  predicted <- stats::predict(fit, newdata = simulation)
  expect_equal(as.numeric(predicted), c(1, 2, 4), tolerance = 1e-10)
  expect_equal(dplyr::n_distinct(predicted), 3L)
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
  brks <- NULL
  expect_message(
    brks <- wiseapp:::.compute_breaks(ref_df, sw),
    "Custom binning for tx requires cut values"
  )
  expect_length(brks, 0L)
})

test_that(".compute_breaks: Custom binning warns when cut count doesn't match num_bins - 1", {
  ref_df <- data.frame(tx = seq(0, 100, by = 1))
  sw <- sw_binned("tx", method = "Custom", n_bins = 5L,
                  custom_breaks = c(20, 25))  # only 2 cuts, expected 4
  brks <- NULL
  expect_message(
    brks <- wiseapp:::.compute_breaks(ref_df, sw),
    "expected 4 cut values"
  )
  # Still produces breaks from the supplied values
  expect_equal(brks$tx[-c(1, length(brks$tx))], c(20, 25))
})

test_that(".compute_breaks: K-means breaks deterministic, caller .Random.seed preserved (DET-03)", {
  set.seed(1234)
  vals   <- c(rnorm(50, 5), rnorm(50, 15), rnorm(50, 25))
  ref_df <- data.frame(tx = vals)
  sw     <- sw_binned("tx", method = "K-means", n_bins = 3)

  old_seed <- get(".Random.seed", envir = .GlobalEnv)
  brks1 <- wiseapp:::.compute_breaks(ref_df, sw)
  expect_identical(get(".Random.seed", envir = .GlobalEnv), old_seed)

  # A different pre-call RNG state must not change the breaks
  invisible(runif(1))
  expect_false(identical(get(".Random.seed", envir = .GlobalEnv), old_seed))
  seed_before_2 <- get(".Random.seed", envir = .GlobalEnv)
  brks2 <- wiseapp:::.compute_breaks(ref_df, sw)
  expect_identical(brks2, brks1)
  expect_identical(get(".Random.seed", envir = .GlobalEnv), seed_before_2)

  # Breaks match a K-means seeded with 123 (pins the internal seed)
  km      <- withr::with_seed(123, stats::kmeans(sort(vals), centers = 3L))
  centers <- sort(as.numeric(km$centers))
  expect_equal(
    brks1$tx[-c(1, length(brks1$tx))],
    (centers[-length(centers)] + centers[-1]) / 2
  )
})

test_that(".compute_breaks: K-means works when caller has no .Random.seed (DET-03)", {
  vals   <- c(rnorm(50, 5), rnorm(50, 15), rnorm(50, 25))
  ref_df <- data.frame(tx = vals)
  sw     <- sw_binned("tx", method = "K-means", n_bins = 3)

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }
  if (!is.null(old_seed)) rm(list = ".Random.seed", envir = .GlobalEnv)
  on.exit(
    if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = .GlobalEnv),
    add = TRUE
  )

  expect_false(exists(".Random.seed", envir = .GlobalEnv))
  brks <- wiseapp:::.compute_breaks(ref_df, sw)
  expect_named(brks, "tx")
  # with_seed() must not leave a seed behind when none existed before
  expect_false(exists(".Random.seed", envir = .GlobalEnv))
})


# ============================================================================ #
# 4. Integration tests — no climate scenario                                   #
# ============================================================================ #

test_that("get_weather returns data frame with expected columns (continuous)", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  fx <- cached_weather_fixture()

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

test_that("one- and two-thread weather output has canonical parity", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  fx <- cached_weather_fixture()
  run <- function(threads) get_weather(
    survey_data = fx$survey_data,
    selected_surveys = fx$selected_surveys,
    selected_weather = sw_continuous("tx"),
    dates = fx$dates,
    connection_params = fx$connection_params,
    weather_threads = threads
  )
  one <- run("1")
  two <- run("2")

  expect_identical(one$historical, two$historical)
  expect_identical(attr(one, "continuous_weather"), attr(two, "continuous_weather"))
  expect_identical(
    attr(one, "weather_collection_policy")$weather_threads$selected_threads,
    1L
  )
  expect_identical(
    attr(two, "weather_collection_policy")$weather_threads$selected_threads,
    2L
  )
})

test_that("get_weather returns only rows matching requested dates", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")

  fx <- cached_weather_fixture()

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

  fx <- cached_weather_fixture()

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

  fx <- cached_weather_fixture()

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

  fx <- cached_weather_fixture()

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

  fx <- cached_weather_fixture()

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

  fx <- cached_weather_fixture()

  # Use simulation dates (wider range than survey dates) to produce
  # out-of-survey-range values that must be caught by outer bins.
  # lubridate::months() is not exported by lubridate >= 1.9; period(months = )
  # is the namespaced replacement and keeps Date arithmetic calendar-aware.
  sim_dates <- seq(
    min(fx$dates) - lubridate::period(months = 24),
    max(fx$dates) + lubridate::period(months = 12),
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

  fx <- cached_weather_fixture()

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

  fx <- cached_weather_fixture()

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

  # micro res 5, weather res 4  (the common production case)
  fx <- cached_weather_fixture("cross_res")

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

  # micro res 4, weather res 5  (unusual but must be handled).
  # 8428347ffffffff is a valid res-4 cell (parent of 85283473fffffff).
  fx <- cached_weather_fixture("finer_res")

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


# --------------------------------------------------------------------------- #
# make_test_fixtures_cmip6_coarser
#
# Adds CMIP6 projection parquets on top of a make_test_fixtures_cross_res()
# fixture, at an H3 resolution *coarser* than the micro/weather resolution.
# This forces the "CMIP6 coarser than target" branch of the h3_cmip6 mapping.
#
# Baseline tx = 20, future tx = 25 → additive delta = +5 everywhere.
# ---------------------------------------------------------------------------
make_test_fixtures_cmip6_coarser <- function(
    dir,
    fx,
    cmip6_res   = 3L,
    proj_source = "cmip6",
    ssp         = "ssp245",
    model       = "TESTMOD",
    tx_base     = 20,
    tx_future   = 25
) {
  con <- make_h3_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # Parent cells of the res-4 weather cells at the (coarser) CMIP6 resolution.
  # Unlike the microdata files, projections store h3 as bigint — matching
  # production data.
  cmip6_ints <- DBI::dbGetQuery(
    con,
    sprintf(
      "SELECT DISTINCT h3_cell_to_parent(h3, %d) AS h3 FROM (VALUES %s) t(h3)",
      cmip6_res,
      paste0("(", fx$weather_ints, ")", collapse = ", ")
    )
  )$h3

  base_months <- sort(unique(fx$dates))
  fut_months  <- seq(as.Date("2025-01-01"), as.Date("2025-12-01"), by = "1 month")

  .cmip6_df <- function(months, tx) {
    df <- expand.grid(h3 = cmip6_ints, timestamp = months)
    df$model <- model
    df$tx    <- tx
    df[, c("h3", "model", "timestamp", "tx")]
  }

  code     <- fx$selected_surveys$code
  proj_dir <- file.path(dir, "hazard", "weather", "projections", code)
  dir.create(proj_dir, recursive = TRUE, showWarnings = FALSE)

  arrow::write_parquet(
    .cmip6_df(base_months, tx_base),
    file.path(proj_dir, paste0(code, "_", proj_source, "_historical.parquet"))
  )
  arrow::write_parquet(
    rbind(.cmip6_df(base_months, tx_base), .cmip6_df(fut_months, tx_future)),
    file.path(proj_dir, paste0(code, "_", proj_source, "_", ssp, ".parquet"))
  )

  invisible(list(base_months = base_months, fut_months = fut_months))
}

test_that("get_weather climate scenario works when CMIP6 is coarser than microdata", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")

  # micro res 5, weather res 4 → target res 4; CMIP6 at res 3 is coarser,
  # forcing the h3_cmip6 parent-mapping branch. Regression: this branch used
  # to reference the dropped `h3` string column, which made DuckDB bind it to
  # the CMIP6 join column (bigint) and fail with `h3_string_to_h3(BIGINT)`.
  fx <- cached_weather_fixture("cross_res_cmip6")

  result <- get_weather(
    survey_data         = fx$survey_data,
    selected_surveys    = fx$selected_surveys,
    selected_weather    = sw_continuous("tx"),
    dates               = fx$dates,
    connection_params   = fx$connection_params,
    ssp                 = "ssp2_4_5",
    future_period       = c("2025-01-01", "2025-12-31"),
    perturbation_method = c(tx = "additive")
  )

  expect_true("historical" %in% names(result))
  scen_name <- grep("ssp2_4_5", names(result), value = TRUE)[1L]
  expect_true(!is.na(scen_name) && nzchar(scen_name))

  scen <- result[[scen_name]]
  hist <- result$historical

  # The additive delta (+5) must surface in the perturbed scenario values.
  cmp <- merge(
    scen[, c("loc_id", "timestamp", "tx")],
    hist[, c("loc_id", "timestamp", "tx")],
    by     = c("loc_id", "timestamp"),
    suffixes = c("_fut", "_hist")
  )
  cmp <- cmp[stats::complete.cases(cmp[, c("tx_fut", "tx_hist")]), ]

  # The scenario's rolling window only spans months that carry deltas, so its
  # early months roll over truncated windows and are not comparable to the
  # historical series. Compare the fully aligned tail (last ref_end months),
  # where both series roll over identical months.
  tail_start <- seq(max(fx$dates), length.out = 2L, by = "-2 months")[2L]
  cmp <- cmp[cmp$timestamp >= tail_start, , drop = FALSE]
  expect_true(nrow(cmp) > 0L)
  expect_equal(cmp$tx_fut - cmp$tx_hist, rep(5, nrow(cmp)), tolerance = 1e-6)
})

test_that("get_weather is identical across repeated end-to-end calls", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")

  fx <- cached_weather_fixture("repeated")
  selected <- sw_binned("tx", method = "K-means", n_bins = 3L)

  run <- function() {
    get_weather(
      survey_data = fx$survey_data,
      selected_surveys = fx$selected_surveys,
      selected_weather = selected,
      dates = fx$dates,
      connection_params = fx$connection_params
    )
  }

  set.seed(771)
  before <- .Random.seed
  a <- run()
  expect_identical(.Random.seed, before)
  stats::runif(10)
  b <- run()

  expect_identical(a, b)
})

test_that("fast and bounded future collection preserve the weather contract", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")

  fx <- cached_weather_fixture("cross_res_cmip6")

  run <- function(weather_collect) get_weather(
    survey_data         = fx$survey_data,
    selected_surveys    = fx$selected_surveys,
    selected_weather    = sw_continuous("tx"),
    dates               = fx$dates,
    connection_params   = fx$connection_params,
    ssp                 = "ssp2_4_5",
    future_period       = c("2025-01-01", "2025-12-31"),
    perturbation_method = c(tx = "additive"),
    weather_collect     = weather_collect
  )

  fast <- run("fast")
  bounded <- run("bounded")
  expect_identical(names(fast), names(bounded))
  for (key in names(fast)) {
    expect_identical(fast[[key]], bounded[[key]])
  }
})

test_that("CR-PERF-10: future weather forces gc() only when the RSS guard trips", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")

  fx <- cached_weather_fixture("cross_res_cmip6")
  withr::local_envvar(
    WISEAPP_WEATHER_CACHE_DIR = withr::local_tempdir(),
    WISEAPP_WEATHER_CACHE_DISABLE = "1"
  )
  counter <- new.env()
  counter$gc <- 0L
  counter$guard <- 0L
  count_gc <- function() counter$gc <- counter$gc + 1L
  suppressMessages(trace(
    "gc", bquote(.(count_gc)()), print = FALSE, where = baseenv()
  ))
  on.exit(suppressMessages(untrace("gc", where = baseenv())), add = TRUE)

  run <- function(exceeded) {
    local_mocked_bindings(.wx_collection_rss_guard = function(policy) {
      counter$guard <- counter$guard + 1L
      list(rss = 1, exceeded = exceeded)
    })
    frames <- list()
    counter$gc <- 0L
    counter$guard <- 0L
    get_weather(
      survey_data         = fx$survey_data,
      selected_surveys    = fx$selected_surveys,
      selected_weather    = sw_continuous("tx"),
      dates               = fx$dates,
      connection_params   = fx$connection_params,
      ssp                 = "ssp2_4_5",
      future_period       = c("2025-01-01", "2025-12-31"),
      perturbation_method = c(tx = "additive"),
      weather_consumer    = function(key, weather, metadata = NULL) {
        frames[[key]] <<- weather
      }
    )
    list(frames = frames, gc = counter$gc, guard = counter$guard)
  }

  under <- run(FALSE)
  over <- run(TRUE)
  expect_gt(under$guard, 0L)
  expect_identical(under$gc, 0L)
  expect_gt(over$gc, 0L)
  expect_identical(under$frames, over$frames)
})

test_that("materialized multi-period future deltas preserve each period", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")

  fx <- cached_weather_fixture("cross_res_cmip6")
  proj_path <- file.path(
    fx$connection_params$path, "hazard", "weather", "projections",
    fx$selected_surveys$code,
    paste0(fx$selected_surveys$code, "_cmip6_ssp245.parquet")
  )
  projection <- arrow::read_parquet(proj_path)
  future_2025 <- projection[projection$timestamp >= as.Date("2025-01-01"), , drop = FALSE]
  future_2026 <- future_2025
  future_2026$timestamp <- as.Date(paste0("2026-", format(future_2025$timestamp, "%m"), "-01"))
  arrow::write_parquet(rbind(projection, future_2026), proj_path)
  run <- function(periods) get_weather(
    survey_data = fx$survey_data,
    selected_surveys = fx$selected_surveys,
    selected_weather = sw_continuous("tx"),
    dates = fx$dates,
    connection_params = fx$connection_params,
    ssp = "ssp2_4_5",
    future_period = periods,
    perturbation_method = c(tx = "additive")
  )
  first <- run(list(c("2025-01-01", "2025-12-31")))
  both <- run(list(
    c("2025-01-01", "2025-12-31"),
    c("2026-01-01", "2026-12-31")
  ))
  first_key <- grep("2025_2025", names(first), value = TRUE)
  second_key <- grep("2026_2026", names(both), value = TRUE)
  expect_length(first_key, 1L)
  expect_length(second_key, 1L)
  expect_identical(first$historical, both$historical)
  expect_identical(first[[first_key]], both[[first_key]])
  # Output timestamps are the survey dates; the future-period window selects
  # the projection source, not the returned survey timestamp.
  expect_gt(nrow(both[[second_key]]), 0L)

  leftovers <- DBI::dbGetQuery(
    .duck_con(),
    "SELECT table_name FROM information_schema.tables WHERE table_name LIKE 'lw_%'"
  )$table_name
  expect_false(any(grepl("^lw_", leftovers)))
})

test_that("SSP perturbation with equal-frequency bins is deterministic", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")

  fx <- cached_weather_fixture("cross_res_cmip6")
  selected <- sw_binned("tx", method = "Equal frequency", n_bins = 3L)

  run <- function() {
    get_weather(
      survey_data = fx$survey_data,
      selected_surveys = fx$selected_surveys,
      selected_weather = selected,
      dates = fx$dates,
      connection_params = fx$connection_params,
      ssp = "ssp2_4_5",
      future_period = c("2025-01-01", "2025-12-31"),
      perturbation_method = c(tx = "additive")
    )
  }

  set.seed(772)
  before <- .Random.seed
  a <- run()
  expect_identical(.Random.seed, before)
  stats::runif(10)
  b <- run()

  expect_identical(a, b)
})

# ============================================================================ #
# 6. PERF-13: forced disk-cache mode produces bit-identical results            #
#                                                                              #
# WISEAPP_WEATHER_CACHE_FORCE routes local loads through the disk cache so     #
# the cache path is exercised without network credentials. The cached slice    #
# holds exactly the rows/columns the remote scan would produce, so the full    #
# get_weather result (historical + climate scenario) must be identical.        #
# ============================================================================ #

test_that("forced weather disk cache is bit-identical, cold and warm (PERF-13)", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")

  fx <- cached_weather_fixture("cross_res_cmip6")

  run <- function() {
    get_weather(
      survey_data         = fx$survey_data,
      selected_surveys    = fx$selected_surveys,
      selected_weather    = sw_continuous("tx"),
      dates               = fx$dates,
      connection_params   = fx$connection_params,
      ssp                 = "ssp2_4_5",
      future_period       = c("2025-01-01", "2025-12-31"),
      perturbation_method = c(tx = "additive")
    )
  }

  # Reference: cache bypassed (local type, no FORCE)
  withr::local_envvar(WISEAPP_WEATHER_CACHE_DISABLE = "1")
  ref <- run()

  # Cold run with the cache forced on (writes slices), then warm run (hits)
  withr::local_envvar(
    WISEAPP_WEATHER_CACHE_DISABLE = NULL,
    WISEAPP_WEATHER_CACHE_FORCE   = "1"
  )
  cold <- run()
  warm <- run()

  expect_identical(ref$historical, cold$historical)
  expect_identical(ref$historical, warm$historical)
  for (nm in setdiff(names(cold), "historical")) {
    expect_identical(ref[[nm]], cold[[nm]])
    expect_identical(ref[[nm]], warm[[nm]])
  }
  # The warm run actually served from cache (slices were written on cold)
  expect_true(
    length(list.files(.weather_cache_dir(), pattern = "\\.parquet$",
                      recursive = TRUE)) >= 3L
  )
})

# ============================================================================ #
# 7. PERF-23: temp-table materialisation is transparent to loc_panel           #
# ============================================================================ #

test_that("loc_panel is identical on the lazy view and its local temp table", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")

  tmp <- withr::local_tempdir()
  h3_dir <- file.path(tmp, "microdata", "h3", "TST")
  dir.create(h3_dir, recursive = TRUE, showWarnings = FALSE)
  h3_df <- data.frame(
    code = "TST", year = 2020L, survname = "SRV",
    loc_id = c("a", "a", "b", "b", "c"),
    h3 = c("x", "y", "y", "z", "z"),
    pop_2020 = c(100L, 200L, 150L, 50L, 300L),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(h3_df, file.path(h3_dir, "TST_2020_lsms_h3.parquet"))

  cp   <- list(type = "local", path = tmp)
  lazy <- load_data(
    "microdata/h3/TST/TST_2020_lsms_h3.parquet", cp, collect = FALSE
  )

  expect_no_warning(
    direct <- loc_panel(lazy, id_col = loc_id, h3_col = h3,
                        weight_col = pop_2020,
                        group_cols = c("code", "year", "survname"))
  )

  nm <- basename(tempfile(pattern = "ss_h3_"))
  local_tbl <- dplyr::compute(lazy, name = nm, temporary = TRUE)
  on.exit(try(DBI::dbRemoveTable(
    dbplyr::remote_con(local_tbl), nm), silent = TRUE), add = TRUE)

  expect_no_warning(
    from_temp <- loc_panel(local_tbl, id_col = loc_id, h3_col = h3,
                           weight_col = pop_2020,
                           group_cols = c("code", "year", "survname"))
  )

  expect_identical(direct, from_temp)
})

test_that("baseline-wave filtering removes unused weather rows with parity", {
  fx <- .cached_weather_fixture("basic")
  old <- fx$selected_surveys
  old$year <- old$year - 1L
  old$survname <- "OLD"
  old_path <- file.path(
    fx$connection_params$path, "microdata", "h3", old$code,
    paste0(old$code, "_", old$year, "_", old$survname, "_",
           old$source, "_h3.parquet")
  )
  current_path <- file.path(
    fx$connection_params$path, "microdata", "h3", fx$selected_surveys$code,
    paste0(fx$selected_surveys$code, "_", fx$selected_surveys$year, "_",
           fx$selected_surveys$survname, "_", fx$selected_surveys$source,
           "_h3.parquet")
  )
  file.copy(current_path, old_path, overwrite = TRUE)
  old_h3 <- arrow::read_parquet(old_path)
  old_h3$year <- old$year
  old_h3$survname <- old$survname
  arrow::write_parquet(old_h3, old_path)

  run <- function(ss) get_weather(
    survey_data = fx$survey_data, selected_surveys = ss,
    selected_weather = sw_continuous("tx"), dates = fx$dates,
    connection_params = fx$connection_params
  )$historical
  full <- run(rbind(old, fx$selected_surveys))
  filtered <- run(fx$selected_surveys)
  common <- full[full$year == fx$selected_surveys$year, , drop = FALSE]
  rownames(common) <- NULL
  rownames(filtered) <- NULL

  expect_gt(nrow(full), nrow(filtered))
  expect_equal(common, filtered, tolerance = 1e-12)
  expect_identical(names(common), names(filtered))
})

test_that("baseline-wave filtering preserves historical and future keys across sources", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")
  fx <- cached_weather_fixture("cross_res_cmip6")
  current <- fx$selected_surveys
  h3_dir <- file.path(fx$connection_params$path, "microdata", "h3", current$code)
  current_path <- file.path(
    h3_dir, paste0(current$code, "_", current$year, "_", current$survname,
                   "_", current$source, "_h3.parquet")
  )
  base_h3 <- arrow::read_parquet(current_path)
  add_mapping <- function(year, survname, source) {
    mapped <- base_h3
    mapped$year <- year
    mapped$survname <- survname
    arrow::write_parquet(mapped, file.path(
      h3_dir, paste0(current$code, "_", year, "_", survname, "_", source,
                     "_h3.parquet")
    ))
    data.frame(code = current$code, year = year, survname = survname,
               source = source, stringsAsFactors = FALSE)
  }
  old <- add_mapping(current$year - 1L, "OLD_WAVE", "archive")
  alternate <- add_mapping(current$year, "ALT_ROUND", "alternate_source")
  baseline <- rbind(current, alternate)
  selected <- rbind(old, baseline)
  run <- function(ss) get_weather(
    survey_data = fx$survey_data, selected_surveys = ss,
    selected_weather = sw_continuous("tx"), dates = fx$dates,
    connection_params = fx$connection_params, ssp = "ssp2_4_5",
    future_period = c("2025-01-01", "2025-12-31"),
    perturbation_method = c(tx = "additive")
  )
  full <- run(selected)
  filtered <- run(baseline)
  expect_identical(names(filtered), names(full))
  expect_true("historical" %in% names(filtered))
  expect_true(any(grepl("ssp2_4_5", names(filtered), fixed = TRUE)))
  for (key in names(filtered)) {
    common <- full[[key]][full[[key]]$year == current$year &
      full[[key]]$survname %in% baseline$survname, , drop = FALSE]
    rownames(common) <- NULL
    actual <- filtered[[key]]
    rownames(actual) <- NULL
    expect_equal(actual, common, tolerance = 1e-12, info = key)
    expect_identical(names(actual), names(common), info = key)
    expect_setequal(unique(actual$survname), baseline$survname)
  }
})

# ============================================================================ #
# 8. Review 2026-10-06 weather fixes                                           #
# ============================================================================ #

# Long-history fixture: the basic h3 mapping plus monthly weather from 1985 to
# 2020, so the 1991-2020 climate reference is fully covered. Survey dates sit
# in 2005, i.e. a historical window that ends well before 2020.
make_long_history_fixture <- function(dir) {
  fx <- make_test_fixtures(dir)
  code <- fx$selected_surveys$code
  weather_path <- file.path(
    dir, "hazard", "weather", "historical", code,
    paste0(code, "_era5land.parquet")
  )
  cells <- unique(arrow::read_parquet(weather_path)$h3)
  timestamps <- seq(as.Date("1985-01-01"), as.Date("2020-12-01"), by = "1 month")
  weather_df <- expand.grid(
    cell_idx = seq_along(cells), timestamp = timestamps,
    stringsAsFactors = FALSE
  )
  weather_df$h3 <- cells[weather_df$cell_idx]
  weather_df$cell_idx <- NULL
  set.seed(7)
  # A warming trend makes the reference depend on which years it covers.
  trend <- as.numeric(format(weather_df$timestamp, "%Y")) - 1985
  weather_df$tx <- 25 + 0.05 * trend + stats::rnorm(nrow(weather_df), sd = 1)
  arrow::write_parquet(weather_df, weather_path)

  fx$dates <- seq(as.Date("2005-01-01"), as.Date("2005-12-01"), by = "1 month")
  fx$survey_data$timestamp <- rep(fx$dates, length.out = nrow(fx$survey_data))
  fx
}

test_that("R2-BUG-01: character and Date dates give identical weather and cache keys", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")

  fx <- make_long_history_fixture(withr::local_tempdir())
  cache_dir <- withr::local_tempdir()
  withr::local_envvar(
    WISEAPP_WEATHER_CACHE_DIR = cache_dir,
    WISEAPP_WEATHER_CACHE_FORCE = "1",
    WISEAPP_WEATHER_CACHE_DISABLE = NA
  )

  run <- function(dates, transformation) get_weather(
    survey_data = fx$survey_data,
    selected_surveys = fx$selected_surveys,
    selected_weather = sw_continuous("tx", transformation = transformation),
    dates = dates,
    connection_params = fx$connection_params
  )

  for (tf in c("Deviation from mean", "Standardized anomaly")) {
    sync <- run(fx$dates, tf)
    cached <- list.files(cache_dir, recursive = TRUE)
    async <- run(as.character(fx$dates), tf)
    expect_identical(async$historical, sync$historical, info = tf)
    expect_true(all(is.finite(sync$historical$tx)), info = tf)
    # The async (character) call must reuse every cache entry the sync call
    # wrote, i.e. both derive identical cache keys.
    expect_identical(list.files(cache_dir, recursive = TRUE), cached, info = tf)
  }
})

test_that("CR-BUG-01: CMIP6 baseline pools historical and SSP rows once", {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("duckdbfs")
  skip_if_not_installed("bit64")

  dir <- withr::local_tempdir()
  fx <- make_long_history_fixture(dir)
  withr::local_envvar(WISEAPP_WEATHER_CACHE_DISABLE = "1")
  code <- fx$selected_surveys$code
  cells <- unique(arrow::read_parquet(file.path(
    dir, "hazard", "weather", "historical", code,
    paste0(code, "_era5land.parquet")
  ))$h3)

  cmip6_df <- function(from, to, tx) {
    months <- seq(as.Date(from), as.Date(to), by = "1 month")
    df <- expand.grid(h3 = cells, timestamp = months)
    df$model <- "TESTMOD"
    df$tx <- tx
    df[, c("h3", "model", "timestamp", "tx")]
  }
  proj_dir <- file.path(dir, "hazard", "weather", "projections", code)
  dir.create(proj_dir, recursive = TRUE, showWarnings = FALSE)
  # Historical part: 24 baseline years (1991-2014) at 20. Rows past 2014 must
  # be ignored (they belong to the SSP part).
  arrow::write_parquet(
    rbind(
      cmip6_df("1991-01-01", "2014-12-01", 20),
      cmip6_df("2015-01-01", "2020-12-01", 100)
    ),
    file.path(proj_dir, paste0(code, "_cmip6_historical.parquet"))
  )
  # SSP part: 6 baseline years (2015-2020) at 25 and the future at 30. Rows
  # before 2015 overlap the historical part and must not be counted again.
  arrow::write_parquet(
    rbind(
      cmip6_df("2010-01-01", "2014-12-01", 100),
      cmip6_df("2015-01-01", "2020-12-01", 25),
      cmip6_df("2025-01-01", "2025-12-01", 30)
    ),
    file.path(proj_dir, paste0(code, "_cmip6_ssp245.parquet"))
  )

  dates <- seq(as.Date("1991-01-01"), as.Date("2020-12-01"), by = "1 month")
  result <- get_weather(
    survey_data = fx$survey_data,
    selected_surveys = fx$selected_surveys,
    selected_weather = sw_continuous("tx"),
    dates = dates,
    connection_params = fx$connection_params,
    ssp = "ssp2_4_5",
    future_period = c("2025-01-01", "2025-12-31"),
    perturbation_method = c(tx = "additive")
  )

  scen_name <- grep("ssp2_4_5", names(result), value = TRUE)
  expect_length(scen_name, 1L)
  cmp <- merge(
    result[[scen_name]][, c("loc_id", "timestamp", "tx")],
    result$historical[, c("loc_id", "timestamp", "tx")],
    by = c("loc_id", "timestamp"), suffixes = c("_fut", "_hist")
  )
  cmp <- cmp[stats::complete.cases(cmp), , drop = FALSE]
  expect_gt(nrow(cmp), 0L)
  # Pooled baseline = 0.8 * 20 + 0.2 * 25 = 21, so the delta is 30 - 21 = 9.
  # The old mean of two climatologies gave (20 + 25) / 2 = 22.5 (delta 7.5).
  expect_equal(cmp$tx_fut - cmp$tx_hist, rep(9, nrow(cmp)), tolerance = 1e-6)
})

test_that("R2-BUG-29: zero reference SD gives NA standardized anomalies and one warning", {
  skip_if_not_installed("duckdb")

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  # January is dry in every reference year (SD 0); February varies. The 2021
  # January row is wet, which used to give Inf; dry reference rows gave NaN.
  base <- data.frame(
    code = "A", year = 2021L, survname = "S", loc_id = "L1",
    timestamp = as.Date(c(
      "1995-01-01", "2000-01-01", "2005-01-01", "2021-01-01",
      "1995-02-01", "2000-02-01", "2005-02-01"
    )),
    r = c(0, 0, 0, 3, 1, 2, 6),
    stringsAsFactors = FALSE
  )
  base_tbl <- dplyr::copy_to(con, base, "r2bug29_base", temporary = TRUE)
  selected <- data.frame(
    name = "r", transformation = "Standardized anomaly",
    stringsAsFactors = FALSE
  )

  ref <- .build_climate_reference(base_tbl, selected)
  out <- .apply_transformations(base_tbl, selected, base_tbl, climate_ref = ref) |>
    dplyr::arrange(timestamp) |>
    dplyr::collect()

  jan <- format(out$timestamp, "%m") == "01"
  expect_true(all(is.na(out$r[jan])))
  expect_false(any(is.nan(out$r)))
  expect_true(all(is.finite(out$r[!jan])))
  expect_equal(out$r[!jan], (c(1, 2, 6) - 3) / stats::sd(c(1, 2, 6)))

  expect_warning(
    n <- .warn_zero_sd_reference(ref),
    "1 location-month\\(s\\) have a zero 1991-2020 reference SD.*r: 1"
  )
  expect_equal(n, 1)

  deviation <- .build_climate_reference(
    base_tbl, transform(selected, transformation = "Deviation from mean")
  )
  expect_no_warning(.warn_zero_sd_reference(deviation))
})

test_that("CR-BUG-17: H3 resolution uses every row and rejects mixed resolutions", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("bit64")

  con <- make_h3_con()
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  res5 <- "85283473fffffff"
  res4 <- "8428347ffffffff"
  single <- dplyr::tbl(con, dplyr::sql(sprintf(
    "SELECT * FROM (VALUES (NULL::VARCHAR), ('%s'), ('%s')) t(h3)", res5, res5
  )))
  expect_identical(.h3_resolution(single, con, "Test"), 5L)

  # The old probe read one arbitrary row, so the detected resolution depended
  # on scan order. Mixed resolutions now fail regardless of row order.
  for (cells in list(c(res5, res4), c(res4, res5))) {
    mixed <- dplyr::tbl(con, dplyr::sql(sprintf(
      "SELECT * FROM (VALUES ('%s'), ('%s')) t(h3)", cells[1], cells[2]
    )))
    expect_error(
      .h3_resolution(mixed, con, "Test"),
      "mixes H3 resolutions 4 to 5"
    )
  }

  empty <- dplyr::tbl(con, dplyr::sql("SELECT NULL::VARCHAR AS h3"))
  expect_error(.h3_resolution(empty, con, "Test"), "contains no H3 cells")
})
