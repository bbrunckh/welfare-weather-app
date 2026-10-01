library(testthat)

source(testthat::test_path("../../dev/bench_step3_helpers.R"), local = TRUE)
source(testthat::test_path("../../dev/bench_step2_helpers.R"), local = TRUE)

.test_bench_size <- function(value) {
  list(
    object_bytes = as.numeric(utils::object.size(value)),
    serialized_bytes = length(serialize(value, NULL, version = 3L)),
    deduplicated_bytes = NA_real_
  )
}

.test_bench_rss_state <- function() {
  state <- new.env(parent = emptyenv())
  state$rss_parent_peak_kb <- 0
  state$rss_tree_peak_kb <- 0
  state
}

.test_bench_rss_sample <- function(state) invisible(state)

.test_bench_config <- function() {
  list(unit = "hh", seed = 123L, aggregation = "mean")
}

.test_bench_identity <- function(workload, uncertainty = "disabled") {
  list(
    country = "fixture", workload = workload, payload_mode = "compact",
    weather_storage = "memory", weather_collect = "fast", join_cache = FALSE,
    direct_rif_predictions = TRUE, uncertainty = uncertainty, cache = "warm",
    repetition = 1L, fixture_mode = "smoke",
    evidence_class = "smoke_only_not_production_evidence"
  )
}

test_that("Step 3 policy fixtures cover requested deterministic scenarios", {
  fixtures <- .bench_step3_policy_fixtures()

  expect_named(fixtures, c("covariate", "targeted_sp", "combined"))
  expect_true(is.list(fixtures$covariate$infra))
  expect_gt(fixtures$targeted_sp$sp$inclusion_error_pct, 0)
  expect_gt(fixtures$targeted_sp$sp$exclusion_error_pct, 0)
  expect_true(all(c("infra", "sp") %in% names(fixtures$combined)))
  expect_error(.bench_step3_policy_fixtures("unknown"), "Unknown Step 3")
})

test_that("smoke fixture runs active Step 3 path and emits stable metrics", {
  input <- .bench_small_step3_input(n = 60L)
  baseline <- .bench_small_step2_result(input, "ols", "one_ssp_one_period")
  fixture <- .bench_step3_policy_fixtures("combined")[[1L]]
  run <- function() .bench_run_step3(
    baseline_result = baseline,
    input = input,
    model_label = "ols",
    policy_label = "combined",
    policy_fixture = fixture,
    identity = .test_bench_identity("one_ssp_one_period"),
    config = .test_bench_config(),
    size_fn = .test_bench_size,
    rss_state_fn = .test_bench_rss_state,
    rss_sample_fn = .test_bench_rss_sample
  )

  first <- run()
  stats::runif(10)
  second <- run()

  expect_identical(first$status, "ok")
  expect_identical(first$evidence_class,
                   "smoke_only_not_production_evidence")
  expect_false(first$runtime_options_executed)
  expect_identical(first$output_fingerprint_sha256,
                   second$output_fingerprint_sha256)
  expect_match(first$output_fingerprint_sha256, "^[0-9a-f]{64}$")
  expect_gt(first$policy_seconds, 0)
  expect_gt(first$decomposition_seconds, 0)
  expect_gt(first$retained_object_bytes, 0)
  expect_gt(first$retained_serialized_bytes, 0)
  expect_gt(first$n_changed_columns, 1L)
  expect_equal(first$n_future_members, 1L)
  expect_equal(first$n_historical_decomposition_rows, 60L)
  # Production annual summaries are compact member/year rows, not household rows.
  expect_equal(first$n_future_decomposition_rows, 2L)
  expect_identical(first$metric_mean_status, "ok")
  expect_identical(first$metric_headcount_ratio_status, "ok")
  expect_gt(first$metric_mean_annual_rows, 0L)
  expect_gt(first$metric_mean_serialized_bytes, 0)
  expect_equal(first$metric_prediction_reruns, 0L)
  expect_gt(first$context_adverse_cache_hits, 0L)
  expect_gt(first$context_fixed_decile_reuses, 0L)
})

test_that("runtime option metadata records every requested current option", {
  args <- list(
    payload_mode = "legacy", weather_storage = "reference",
    weather_collect = "bounded", join_cache = TRUE,
    direct_rif_predictions = FALSE
  )
  result <- .bench_attach_runtime_metadata(
    list(n_keys = 1L), args, "production_path_read_only"
  )

  expect_silent(.bench_assert_runtime_contract(args))
  expect_silent(.bench_assert_runtime_options(result, args))
  expect_identical(result$benchmark_metadata$requested_runtime_options, args)
  expect_true(result$benchmark_metadata$runtime_options_executed)
  expect_identical(result$benchmark_metadata$evidence_class,
                   "production_path_read_only")
  result$benchmark_metadata$requested_runtime_options$join_cache <- FALSE
  expect_error(
    .bench_assert_runtime_options(result, args),
    "does not match requested runtime options"
  )
})

test_that("Step 2 execution passes current runtime options without filtering", {
  captured <- NULL
  run_fn <- function(payload_mode, weather_storage, weather_collect,
                     join_cache, direct_rif_predictions) {
    captured <<- as.list(environment())
    list(n_keys = 1L)
  }
  args <- list(
    payload_mode = "legacy", weather_storage = "reference",
    weather_collect = "bounded", join_cache = TRUE,
    direct_rif_predictions = FALSE
  )

  result <- .bench_execute_step2(args, "production_path_read_only", run_fn)

  expect_identical(captured, args)
  expect_identical(result$benchmark_metadata$requested_runtime_options, args)
  expect_true(result$benchmark_metadata$runtime_options_executed)
})

test_that("uninstalled trace metrics are unavailable rather than zero", {
  installed <- list(list(slot = "prediction_elapsed"))
  state <- .test_bench_rss_state()

  expect_equal(.bench_trace_metric(installed, "prediction_elapsed", 1.25), 1.25)
  expect_true(is.na(.bench_trace_metric(installed, "join_elapsed", 0)))
  .bench_initialize_trace_state(state, installed)
  expect_equal(state$prediction_elapsed, 0)
  expect_true(is.na(state$join_elapsed))
  expect_true(is.na(state$cache_calls))

  .bench_mark_traces_unavailable(state)
  expect_true(all(is.na(c(
    state$prediction_elapsed, state$join_elapsed, state$cache_calls
  ))))
})

test_that("compact original-residual aggregation receives shared context", {
  train_aug <- data.frame(hhid = 1:2, .resid = c(-1, 1))
  pipe <- list(
    y_point = c(10, 20), sim_year = c(2030L, 2030L), id_vec = 1:2,
    weight = c(1, 1), train_aug = NULL, id_col = NULL
  )
  compact <- list(
    hist_sim_result = list(
      pipeline = pipe,
      shared_context = list(
        schema = 1L, train_aug = train_aug, id_col = "hhid",
        residuals = "original"
      )
    ),
    new_scenarios = list()
  )
  captured <- NULL
  aggregate_fn <- function(pipe, residuals, shared_context, ...) {
    captured <<- list(
      residuals = residuals,
      shared_context = shared_context,
      resolved = step2_pipeline_context(pipe, shared_context)
    )
    list(list(sim_year = 2030L, estimate = 15))
  }

  entries <- .bench_aggregation_pipelines(compact)
  expect_null(entries$historical$pipe$train_aug)
  metadata <- .bench_aggregation_metadata(entries, "original")
  expect_identical(metadata$residuals_requested, "original")
  expect_identical(metadata$residuals_effective, "original")
  expect_true(metadata$shared_context_available)
  output <- .bench_aggregate_pipelines(
    entries, method = "mean", weighted = TRUE, pov_line = NULL,
    residuals = "original", is_log = FALSE, skip_coef = TRUE, seed = 123L,
    aggregate_fn = aggregate_fn
  )

  expect_identical(captured$residuals, "original")
  expect_identical(captured$shared_context, compact$hist_sim_result$shared_context)
  expect_identical(captured$resolved$train_aug, train_aug)
  expect_identical(captured$resolved$id_col, "hhid")
  expect_equal(output$historical[[1L]]$estimate, 15)

  without_context <- entries
  without_context$historical$shared_context <- NULL
  unavailable <- .bench_aggregation_metadata(without_context, "original")
  expect_identical(unavailable$residuals_effective, "unavailable")
  expect_false(unavailable$shared_context_available)
  expect_error(
    .bench_aggregate_pipelines(
      without_context, method = "mean", weighted = TRUE, pov_line = NULL,
      residuals = "original", is_log = FALSE, skip_coef = TRUE, seed = 123L,
      aggregate_fn = aggregate_fn
    ),
    "Original-residual aggregation requires"
  )
})

test_that("smoke fixture supports RIF and historical-only Step 3", {
  input <- .bench_small_step3_input(n = 40L)
  baseline <- .bench_small_step2_result(input, "rif", "historical")
  result <- .bench_run_step3(
    baseline_result = baseline,
    input = input,
    model_label = "rif",
    policy_label = "targeted_sp",
    policy_fixture = .bench_step3_policy_fixtures("targeted_sp")[[1L]],
    identity = .test_bench_identity("historical", "enabled"),
    config = .test_bench_config(),
    size_fn = .test_bench_size,
    rss_state_fn = .test_bench_rss_state,
    rss_sample_fn = .test_bench_rss_sample
  )

  expect_identical(result$status, "ok")
  expect_equal(result$n_future_scenarios, 0L)
  expect_equal(result$n_future_decomposition_rows, 0L)
  expect_gt(result$historical_decomposition_serialized_bytes, 0)
})
