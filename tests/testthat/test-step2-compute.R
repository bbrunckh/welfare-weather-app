# Phase 3 pure compute-boundary characterization.

library(testthat)

step2_compute_fixture <- function() {
  list(
    sw = data.frame(name = "temp", stringsAsFactors = FALSE),
    so = data.frame(
      name = "welfare", type = "numeric", transform = "log",
      label = "Welfare", stringsAsFactors = FALSE
    ),
    svy = data.frame(
      hhid = c("hh01", "hh02"), code = "TST", year = 2020L,
      survname = "SRV", loc_id = "loc01", welfare = c(1, 2),
      temp = c(1, 1), weight = c(1, 2), stringsAsFactors = FALSE
    ),
    ss = NULL,
    mf = list(
      fit3 = NULL, engine = "fixest", train_data = data.frame(x = 1:2),
      weather_terms = "temp"
    ),
    cp = list(type = "local", path = tempdir()),
    fp_list = list(c("2030-01-01", "2040-12-31")),
    ssps = "ssp2_4_5",
    residuals = "none",
    skip_coef_draws = TRUE,
    sim_dates = c("2020-01-01", "2020-12-31"),
    perturbation_method = NULL,
    stored_breaks = NULL
  )
}

step2_compute_weather <- function() {
  hist <- data.frame(
    code = "TST", year = 2020L, survname = "SRV", loc_id = "loc01",
    temp = 1, timestamp = as.POSIXct("2020-06-01", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  mean_key <- hist
  mean_key$timestamp <- as.POSIXct("2030-06-01", tz = "UTC")
  mean_key$temp <- 2
  hi_key <- mean_key
  hi_key$temp <- 3
  list(
    historical = hist,
    ssp2_4_5_2030_2040_ensemble_mean = mean_key,
    ssp2_4_5_2030_2040_ensemble_hi = hi_key
  )
}

step2_compute_pipeline <- function(weather_raw, ...) {
  list(
    y_point = c(1, 2),
    F_loading = NULL,
    sim_year = c(2030L, 2030L),
    weight = c(1, 2),
    id_vec = c("hh01", "hh02"),
    id_col = "hhid",
    svy_row_id = c(1L, 2L),
    n_pre_join = 2L,
    weather_raw = weather_raw,
    train_aug = data.frame(hhid = c("hh01", "hh02"), .resid = c(0.1, -0.1))
  )
}

step2_compute_reference <- function(input, weather) {
  do.call(fct_run_simulation, c(input, list(
      progress_fn = function(...) invisible(NULL),
      weather_fn = function(...) weather,
      pipeline_fn = step2_compute_pipeline
    )))
}

test_that("step2_compute validates and does not mutate the input snapshot", {
  # A developer ~/.Renviron often sets WISEAPP_DATA_PATH; the NULL-path fixture
  # below must not silently resolve to it or the "valid connection" check never
  # triggers.
  withr::local_envvar(WISEAPP_DATA_PATH = "")
  input <- step2_compute_fixture()
  before <- input
  expect_error(step2_compute(list()), "named ordinary-object")
  expect_error(
    step2_compute(utils::modifyList(input, list(cp = list(type = "local", path = NULL)))),
    "valid connection"
  )
  expect_identical(input, before)
})

test_that("step2_compute preserves serial result values and attaches metadata", {
  input <- step2_compute_fixture()
  weather <- step2_compute_weather()
  out <- suppressWarnings(step2_compute(
    input, seed = 123L, run_id = "phase3-test",
    weather_fn = function(...) weather,
    pipeline_fn = step2_compute_pipeline
  ))
  reference <- suppressWarnings(step2_compute_reference(input, weather))

  expect_identical(out$result$hist_sim_result$pipeline$y_point,
                   reference$hist_sim_result$pipeline$y_point)
  expect_identical(out$result$n_keys, reference$n_keys)
  expect_identical(out$result$n_keys_ok, reference$n_keys_ok)
  expect_identical(out$result$failures, reference$failures)
  expect_identical(out$result$.sig, out$signature)
  expect_identical(out$result$.run$id, "phase3-test")
  expect_identical(out$result$.run$seed, 123L)
  expect_identical(out$result$.run$schema, 1L)
  expect_true(is.character(out$result$.run$generated_at_utc))
})

test_that("step2_compute emits ordered stage events and restores RNG state", {
  input <- step2_compute_fixture()
  weather <- step2_compute_weather()
  set.seed(902L)
  before <- .Random.seed
  events <- list()
  out <- suppressWarnings(step2_compute(
    input, seed = 123L, event_fn = function(event) {
      events[[length(events) + 1L]] <<- event
    }, weather_fn = function(...) weather,
    pipeline_fn = step2_compute_pipeline
  ))

  expect_identical(.Random.seed, before)
  expect_true(length(events) >= 5L)
  expect_identical(vapply(events, `[[`, character(1), "stage")[1:2],
                   c("initialize", "initialize"))
  expect_true(any(vapply(events, function(x) identical(x$stage, "weather") &&
                              identical(x$status, "completed"), logical(1))))
  expect_true(any(vapply(events, function(x) identical(x$stage, "simulation") &&
                              identical(x$status, "completed"), logical(1))))
  expect_identical(events[[length(events)]]$stage, "publish")
  expect_true(all(vapply(events, function(x) is.numeric(x$elapsed), logical(1))))
  expect_identical(out$events, events)
})

test_that("step2_compute forwards historical previews and cooperative checkpoints", {
  input <- step2_compute_fixture()
  weather <- step2_compute_weather()
  previews <- list()
  checkpoints <- list()
  out <- suppressWarnings(step2_compute(
    input,
    weather_fn = function(...) weather,
    pipeline_fn = step2_compute_pipeline,
    preview_fn = function(value) previews[[length(previews) + 1L]] <<- value,
    checkpoint_fn = function(event) {
      checkpoints[[length(checkpoints) + 1L]] <<- event
    }
  ))

  expect_length(previews, 1L)
  expect_identical(names(previews[[1L]]$data),
                   c("sim_year", "value", "uncertainty"))
  expect_identical(previews[[1L]]$metadata$method, "mean")
  expect_identical(previews[[1L]]$metadata$weighted, TRUE)
  expect_true(any(vapply(checkpoints, function(event) {
    identical(event$stage, "historical_ready") && event$completed == 1L
  }, logical(1))))
  expect_true(any(vapply(out$events, function(event) {
    identical(event$stage, "historical_ready") &&
      identical(event$status, "checkpoint")
  }, logical(1))))
})

test_that("step2_compute cancellation checkpoint errors propagate", {
  input <- step2_compute_fixture()
  weather <- step2_compute_weather()
  cancellation <- structure(
    list(message = "cancelled", call = NULL),
    class = c("wiseapp_step2_cancelled", "error", "condition")
  )
  expect_error(
    step2_compute(
      input,
      weather_fn = function(...) weather,
      pipeline_fn = step2_compute_pipeline,
      checkpoint_fn = function(event) {
        if (identical(event$stage, "historical_ready")) stop(cancellation)
      }
    ),
    class = "wiseapp_step2_cancelled"
  )
})

test_that("step2_compute restores configured cache environment values", {
  input <- step2_compute_fixture()
  weather <- step2_compute_weather()
  withr::local_envvar(c(
    WISEAPP_WEATHER_CACHE_DIR = "cache-before-step2",
    WISEAPP_WEATHER_CACHE_DISABLE = "1"
  ))
  before <- Sys.getenv(c("WISEAPP_WEATHER_CACHE_DIR", "WISEAPP_WEATHER_CACHE_DISABLE"))
  for (fails in c(FALSE, TRUE)) {
    if (fails) {
      expect_error(step2_compute(
        input, cache_dir = tempfile("step2-cache-error-"),
        weather_fn = function(...) stop("injected weather error"),
        pipeline_fn = step2_compute_pipeline
      ), "injected weather error")
    } else {
      suppressWarnings(step2_compute(
        input, cache_dir = tempfile("step2-cache-"),
        weather_fn = function(...) weather,
        pipeline_fn = step2_compute_pipeline
      ))
    }
    expect_identical(Sys.getenv(names(before)), before)
  }

  withr::local_envvar(c(
    WISEAPP_WEATHER_CACHE_DIR = NA_character_,
    WISEAPP_WEATHER_CACHE_DISABLE = NA_character_
  ))
  expect_equal(unname(Sys.getenv(c(
    "WISEAPP_WEATHER_CACHE_DIR", "WISEAPP_WEATHER_CACHE_DISABLE"
  ), unset = NA_character_)), c(NA_character_, NA_character_))
  expect_error(step2_compute(
    input, cache_dir = tempfile("step2-cache-unset-"),
    weather_fn = function(...) stop("injected weather error"),
    pipeline_fn = step2_compute_pipeline
  ), "injected weather error")
  expect_equal(unname(Sys.getenv(c(
    "WISEAPP_WEATHER_CACHE_DIR", "WISEAPP_WEATHER_CACHE_DISABLE"
  ), unset = NA_character_)), c(NA_character_, NA_character_))
})

test_that("step2_compute is deterministic for equal inputs and seed", {
  input <- step2_compute_fixture()
  weather <- step2_compute_weather()
  a <- suppressWarnings(step2_compute(
    input, seed = 123L, run_id = "a",
    weather_fn = function(...) weather,
    pipeline_fn = step2_compute_pipeline
  ))
  stats::runif(10L)
  b <- suppressWarnings(step2_compute(
    input, seed = 123L, run_id = "b",
    weather_fn = function(...) weather,
    pipeline_fn = step2_compute_pipeline
  ))

  expect_identical(a$signature, b$signature)
  expect_identical(a$result$hist_sim_result$pipeline,
                   b$result$hist_sim_result$pipeline)
  expect_identical(a$result$new_scenarios, b$result$new_scenarios)
})

test_that("step2_compute rejects pipelines whose rows are not aligned (CR-BUG-07)", {
  expect_error(
    wiseapp:::.step2_compute_assert_alignment(list(
      y_point = c(1, 2, 3), F_loading = matrix(0, 2, 1), sim_year = 1:3
    )),
    "not aligned with 3 predictions: F_loading has 2"
  )
  aligned <- step2_compute_pipeline(NULL)
  expect_identical(wiseapp:::.step2_compute_assert_alignment(aligned), aligned)

  input <- step2_compute_fixture()
  weather <- step2_compute_weather()
  misaligned <- function(weather_raw, ...) {
    out <- step2_compute_pipeline(weather_raw, ...)
    out$svy_row_id <- 1L
    out
  }
  events <- list()
  try(suppressWarnings(step2_compute(
    input, seed = 123L,
    event_fn = function(event) events[[length(events) + 1L]] <<- event,
    weather_fn = function(...) weather,
    pipeline_fn = misaligned
  )), silent = TRUE)
  failed <- Filter(function(e) identical(e$stage, "pipeline") &&
                     identical(e$status, "failed"), events)
  expect_true(length(failed) > 0L)
  expect_match(paste(unlist(failed), collapse = " "), "svy_row_id has 1")
})
