# ============================================================================ #
# tests/testthat/test-fct_wx_cache.R                                           #
# PERF-13: bounded disk cache for remote weather parquet fetches.              #
#  - cached slice is bit-identical to the direct lazy scan                     #
#  - second call reads the cache (source can be removed)                       #
#  - distinct keys for distinct (cols, date range)                             #
#  - eviction keeps the cache under its size budget                            #
#  - graceful fallback when the cache dir is unwritable                        #
#                                                                              #
# WISEAPP_WEATHER_CACHE_FORCE=1 routes local-connection loads through the      #
# cache so the cache mechanics are testable without network credentials.       #
# ============================================================================ #

library(testthat)

make_wx_cache_fixture <- function(dir) {
  skip_if_not_installed("arrow")
  skip_if_not_installed("duckdb")
  d <- file.path(dir, "hazard", "weather", "historical", "TST")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  set.seed(5)
  df <- data.frame(
    h3        = sprintf("cell%03d", 1:50),
    timestamp = rep(seq(as.Date("2018-01-01"), by = "month", length.out = 24), each = 50),
    tx        = rnorm(50 * 24),
    t         = rnorm(50 * 24),
    unused    = rnorm(50 * 24),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(df, file.path(d, "TST_era5land.parquet"))
  list(dir = dir,
       fnames = "hazard/weather/historical/TST/TST_era5land.parquet")
}

with_wx_cache <- function(expr) {
  withr::local_envvar(
    WISEAPP_WEATHER_CACHE_DIR   = withr::local_tempdir(),
    WISEAPP_WEATHER_CACHE_FORCE = "1"
  )
  force(expr)
}

test_that("wx disk cache: cached slice is bit-identical to the direct scan", {
  fx <- make_wx_cache_fixture(withr::local_tempdir())
  cp <- list(type = "local", path = fx$dir)

  direct <- with_wx_cache(
    load_data(fx$fnames, cp, collect = FALSE) |>
      dplyr::select(h3, timestamp, tx) |>
      dplyr::filter(timestamp >= as.Date("2018-06-01"),
                    timestamp <= as.Date("2019-05-31")) |>
      collect_deterministic(c("h3", "timestamp"))
  )

  via_cache <- with_wx_cache(
    .wx_cache_load(
      fx$fnames, cp,
      cols = c("h3", "timestamp", "tx"),
      tmin = as.Date("2018-06-01"), tmax = as.Date("2019-05-31")
    ) |>
      dplyr::select(h3, timestamp, tx) |>
      dplyr::filter(timestamp >= as.Date("2018-06-01"),
                    timestamp <= as.Date("2019-05-31")) |>
      collect_deterministic(c("h3", "timestamp"))
  )

  expect_identical(direct, via_cache)
})

test_that("wx disk cache: second call reads the cache, not the source", {
  fx <- make_wx_cache_fixture(withr::local_tempdir())
  cp <- list(type = "local", path = fx$dir)

  with_wx_cache({
    # Populate
    .wx_cache_load(fx$fnames, cp, cols = c("h3", "timestamp", "tx"),
                   tmin = as.Date("2018-01-01"), tmax = as.Date("2019-12-31"))
    cache_dir <- .weather_cache_dir()
    cache_files <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)
    expect_length(cache_files, 1L)

    # Delete the SOURCE: the second call must still succeed via the cache
    unlink(file.path(fx$dir, "hazard"), recursive = TRUE)
    from_cache <- .wx_cache_load(fx$fnames, cp,
                                 cols = c("h3", "timestamp", "tx"),
                                 tmin = as.Date("2018-01-01"),
                                 tmax = as.Date("2019-12-31")) |>
      collect_deterministic(c("h3", "timestamp"))
    expect_equal(nrow(from_cache), 50L * 24L)
  })
})

test_that("wx disk cache: distinct keys per date range and column slice", {
  fx <- make_wx_cache_fixture(withr::local_tempdir())
  cp <- list(type = "local", path = fx$dir)

  with_wx_cache({
    .wx_cache_load(fx$fnames, cp, cols = c("h3", "timestamp", "tx"),
                   tmin = as.Date("2018-01-01"), tmax = as.Date("2018-12-31"))
    .wx_cache_load(fx$fnames, cp, cols = c("h3", "timestamp", "tx"),
                   tmin = as.Date("2019-01-01"), tmax = as.Date("2019-12-31"))
    .wx_cache_load(fx$fnames, cp, cols = c("h3", "timestamp", "t"),
                   tmin = as.Date("2018-01-01"), tmax = as.Date("2018-12-31"))
    cache_dir <- .weather_cache_dir()
    expect_length(list.files(cache_dir, pattern = "\\.parquet$"), 3L)
  })
})

test_that("wx disk cache: eviction keeps the cache under budget", {
  fx <- make_wx_cache_fixture(withr::local_tempdir())
  cp <- list(type = "local", path = fx$dir)

  with_wx_cache({
    cache_dir <- .weather_cache_dir()
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

    # Two cached slices with distinct mtimes
    .wx_cache_load(fx$fnames, cp, cols = c("h3", "timestamp", "tx"),
                   tmin = as.Date("2018-01-01"), tmax = as.Date("2018-12-31"))
    Sys.sleep(0.05)
    .wx_cache_load(fx$fnames, cp, cols = c("h3", "timestamp", "tx"),
                   tmin = as.Date("2019-01-01"), tmax = as.Date("2019-12-31"))
    expect_length(list.files(cache_dir, pattern = "\\.parquet$"), 2L)

    # Evict to a 0 MB budget: oldest file goes first, both go at 0
    withr::local_envvar(WISEAPP_WEATHER_CACHE_MAX_MB = "0")
    .weather_cache_evict(cache_dir)
    expect_length(list.files(cache_dir, pattern = "\\.parquet$"), 0L)
  })
})

test_that("wx disk cache: unwritable cache dir falls back to the remote scan", {
  fx <- make_wx_cache_fixture(withr::local_tempdir())
  cp <- list(type = "local", path = fx$dir)

  # Point the cache dir at a regular file: dir.create fails, COPY fails,
  # and the function must return the remote-filtered lazy with a warning.
  blocker <- file.path(withr::local_tempdir(), "blocker")
  writeLines("not a dir", blocker)
  withr::local_envvar(
    WISEAPP_WEATHER_CACHE_DIR   = blocker,
    WISEAPP_WEATHER_CACHE_FORCE = "1"
  )

  expect_warning(
    via_fallback <- .wx_cache_load(fx$fnames, cp,
                                   cols = c("h3", "timestamp", "tx"),
                                   tmin = as.Date("2018-06-01"),
                                   tmax = as.Date("2018-12-31")) |>
      collect_deterministic(c("h3", "timestamp")),
    "weather disk cache write failed"
  )
  direct <- load_data(fx$fnames, cp, collect = FALSE) |>
    dplyr::select(h3, timestamp, tx) |>
    dplyr::filter(timestamp >= as.Date("2018-06-01"),
                  timestamp <= as.Date("2018-12-31")) |>
    collect_deterministic(c("h3", "timestamp"))
  expect_identical(via_fallback, direct)
})
