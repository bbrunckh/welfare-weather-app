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
    # Age the first slice past the run timeout instead of sleeping.
    first <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)
    Sys.setFileTime(first, Sys.time() - 3 * 3600)
    .wx_cache_load(fx$fnames, cp, cols = c("h3", "timestamp", "tx"),
                   tmin = as.Date("2019-01-01"), tmax = as.Date("2019-12-31"))
    expect_length(list.files(cache_dir, pattern = "\\.parquet$"), 2L)

    # Evict to a 0 MB budget: the old file goes; the fresh one is younger
    # than the async run timeout (default 90 min) and must survive (CR-SEC-04).
    withr::local_envvar(WISEAPP_WEATHER_CACHE_MAX_MB = "0")
    .weather_cache_evict(cache_dir)
    left <- list.files(cache_dir, pattern = "\\.parquet$", full.names = TRUE)
    expect_length(left, 1L)
    expect_false(first %in% left)

    # With a 1-minute timeout the fresh file is evictable once older than it.
    withr::local_envvar(WISEAPP_ASYNC_TIMEOUT_MIN = "1")
    Sys.setFileTime(left, Sys.time() - 120)
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

test_that("CR-SEC-04: cache keys separate data sources, never include credentials", {
  ids <- list(
    .wx_cache_source_id(list(type = "s3", bucket = "a", prefix = "p", region = "r",
                             key_id = "KEY1", secret = "SECRET1")),
    .wx_cache_source_id(list(type = "s3", bucket = "b", prefix = "p", region = "r")),
    .wx_cache_source_id(list(type = "s3", bucket = "a", prefix = "q", region = "r")),
    .wx_cache_source_id(list(type = "gcs", bucket = "a", prefix = "p")),
    .wx_cache_source_id(list(type = "azure", account = "x", container = "c", prefix = "p")),
    .wx_cache_source_id(list(type = "hf", repo = "org/repo", subdir = "")),
    .wx_cache_source_id(list(type = "databricks", workspace = "https://w1",
                             volume_path = "/Volumes/a")),
    .wx_cache_source_id(list(type = "databricks", workspace = "https://w2",
                             volume_path = "/Volumes/a")),
    .wx_cache_source_id(list(type = "local", path = "/data/one")),
    .wx_cache_source_id(list(type = "local", path = "/data/two"))
  )
  expect_equal(length(unique(ids)), length(ids))
  # Credentials never enter the key material
  expect_identical(
    ids[[1L]],
    .wx_cache_source_id(list(type = "s3", bucket = "a", prefix = "p", region = "r",
                             key_id = "KEY2", secret = "SECRET2"))
  )
  expect_false(any(grepl("KEY1|SECRET1", unlist(ids))))

  # Same relative file names from two local roots never share a slice or a
  # location-month key.
  a <- make_wx_cache_fixture(withr::local_tempdir())
  b <- make_wx_cache_fixture(withr::local_tempdir())
  with_wx_cache({
    .wx_cache_load(a$fnames, list(type = "local", path = a$dir),
                   cols = c("h3", "timestamp", "tx"))
    .wx_cache_load(b$fnames, list(type = "local", path = b$dir),
                   cols = c("h3", "timestamp", "tx"))
    expect_length(list.files(.weather_cache_dir(), pattern = "\\.parquet$"), 2L)
  })
  key <- function(cp) .wx_loc_cache_key("w", "h", "tx", 1, 2, 5L, 5L, cp)
  expect_false(identical(
    key(list(type = "local", path = a$dir)),
    key(list(type = "local", path = b$dir))
  ))
})

test_that("CR-SEC-04: unique temp names, 0700 dir, LRU touch on hit, quoted paths", {
  dir <- file.path(withr::local_tempdir(), "it's here")
  path <- file.path(dir, "abc.parquet")
  tmp1 <- .wx_cache_tmp_path(path)
  tmp2 <- .wx_cache_tmp_path(path)
  expect_false(identical(tmp1, tmp2))
  expect_identical(dirname(tmp1), dir)
  expect_match(basename(tmp1), "^abc\\.parquet-.*\\.tmp$")
  expect_false(grepl("\\.parquet$", tmp1))

  # A cache dir containing a quote still works (paths go through .sql_literal)
  fx <- make_wx_cache_fixture(withr::local_tempdir())
  cp <- list(type = "local", path = fx$dir)
  withr::local_envvar(
    WISEAPP_WEATHER_CACHE_DIR = dir,
    WISEAPP_WEATHER_CACHE_FORCE = "1"
  )
  .wx_cache_load(fx$fnames, cp, cols = c("h3", "timestamp", "tx"))
  cache_dir <- .weather_cache_dir()
  files <- list.files(cache_dir, full.names = TRUE)
  expect_length(files, 1L)
  expect_match(files, "\\.parquet$")
  if (.Platform$OS.type == "unix") {
    expect_identical(format(file.info(cache_dir)$mode), "700")
  }

  # A hit refreshes mtime so LRU eviction sees it as recently used
  Sys.setFileTime(files, Sys.time() - 3 * 3600)
  hit <- .wx_cache_load(fx$fnames, cp, cols = c("h3", "timestamp", "tx")) |>
    collect_deterministic(c("h3", "timestamp"))
  expect_equal(nrow(hit), 50L * 24L)
  age <- difftime(Sys.time(), file.info(files)$mtime, units = "secs")
  expect_lt(as.numeric(age), 600)

  # The location-month cache round-trips through the quoted path as well
  con <- .duck_con()
  DBI::dbExecute(con, "CREATE OR REPLACE TEMP TABLE secq_src AS SELECT 1 AS x")
  withr::defer(try(DBI::dbExecute(con, "DROP TABLE IF EXISTS secq_src"), silent = TRUE))
  .wx_loc_cache_store(con, "secq_src", "secq_key")
  loaded <- .wx_loc_cache_load(con, "secq_key", "secq_loaded")
  withr::defer(try(DBI::dbExecute(con, "DROP TABLE IF EXISTS secq_loaded"), silent = TRUE))
  expect_equal(dplyr::collect(loaded)$x, 1L)
  expect_length(list.files(cache_dir, pattern = "\\.tmp$"), 0L)
})
