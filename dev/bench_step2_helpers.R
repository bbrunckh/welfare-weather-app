# Development-only helpers shared by the Step 2 and Step 3 benchmark paths.

.bench_runtime_options <- function(args) {
  names <- c(
    "payload_mode", "weather_storage", "weather_collect", "weather_threads", "join_cache",
    "direct_rif_predictions"
  )
  args[intersect(names, names(args))]
}

.bench_assert_runtime_contract <- function(args, run_fn = fct_run_simulation) {
  requested <- names(.bench_runtime_options(args))
  missing <- setdiff(requested, names(formals(run_fn)))
  if (length(missing)) {
    stop(
      "Current fct_run_simulation() does not support requested benchmark ",
      "option(s): ", paste(missing, collapse = ", "), call. = FALSE
    )
  }
  invisible(TRUE)
}

.bench_execute_step2 <- function(args, evidence_class,
                                  run_fn = fct_run_simulation) {
  .bench_assert_runtime_contract(args, run_fn)
  result <- do.call(run_fn, args)
  result <- .bench_attach_runtime_metadata(result, args, evidence_class)
  .bench_assert_runtime_options(result, args)
  result
}

.bench_attach_runtime_metadata <- function(result, args, evidence_class) {
  result$benchmark_metadata <- list(
    requested_runtime_options = .bench_runtime_options(args),
    runtime_options_executed = identical(
      evidence_class, "production_path_read_only"
    ),
    evidence_class = evidence_class
  )
  result
}

.bench_assert_runtime_options <- function(result, args) {
  expected <- .bench_runtime_options(args)
  actual <- result$benchmark_metadata$requested_runtime_options
  if (!identical(actual, expected)) {
    stop(
      "Step 2 result metadata does not match requested runtime options.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.bench_trace_slots <- function(installed) {
  unique(vapply(installed, `[[`, character(1), "slot"))
}

.bench_trace_metric <- function(installed, slot, available = 0, unavailable = NA_real_) {
  if (slot %in% .bench_trace_slots(installed)) available else unavailable
}

.bench_initialize_trace_state <- function(state, installed) {
  for (slot in c(
    "design_matrix_elapsed", "prediction_elapsed",
    "factor_loading_elapsed", "join_elapsed"
  )) {
    state[[slot]] <- .bench_trace_metric(installed, slot, 0)
  }
  if ("cache" %in% .bench_trace_slots(installed)) {
    state$cache_calls <- 0L
    state$cache_hits <- 0L
    state$cache_misses <- 0L
  } else {
    state$cache_calls <- NA_integer_
    state$cache_hits <- NA_integer_
    state$cache_misses <- NA_integer_
  }
  invisible(state)
}

.bench_mark_traces_unavailable <- function(state) {
  state$design_matrix_elapsed <- NA_real_
  state$prediction_elapsed <- NA_real_
  state$factor_loading_elapsed <- NA_real_
  state$join_elapsed <- NA_real_
  state$cache_calls <- NA_integer_
  state$cache_hits <- NA_integer_
  state$cache_misses <- NA_integer_
  invisible(state)
}

.bench_aggregation_pipelines <- function(result) {
  historical <- result$hist_sim_result
  out <- list(historical = list(
    pipe = historical$pipeline,
    shared_context = historical$shared_context %||% NULL
  ))
  for (scenario_name in names(result$new_scenarios %||% list())) {
    scenario <- result$new_scenarios[[scenario_name]]
    for (member_name in names(scenario$pipelines %||% list())) {
      out[[paste(scenario_name, member_name, sep = " / ")]] <- list(
        pipe = scenario$pipelines[[member_name]],
        shared_context = scenario$shared_context %||% NULL
      )
    }
  }
  out
}

.bench_aggregate_pipelines <- function(pipelines, method, weighted, pov_line,
                                       residuals, is_log, skip_coef, seed,
                                       aggregate_fn = aggregate_pipeline_per_year) {
  metadata <- .bench_aggregation_metadata(pipelines, residuals)
  if (identical(residuals, "original") &&
      !isTRUE(metadata$shared_context_available)) {
    stop(
      "Original-residual aggregation requires train_aug and id_col context ",
      "for every compact pipeline.", call. = FALSE
    )
  }
  lapply(pipelines, function(entry) {
    aggregate_fn(
      pipe = entry$pipe,
      method = method,
      weighted = weighted,
      pov_line = pov_line,
      residuals = residuals,
      is_log = is_log,
      skip_coef = skip_coef,
      seed = seed,
      shared_context = entry$shared_context
    )
  })
}

# Mirrors the Step 2 Results weighted arm: one multi-method suite per pipeline
# via aggregate_pipeline_tables_multi(), the path the application actually
# takes when survey weights are present.
.bench_aggregate_pipelines_suite <- function(pipelines, methods, weighted,
                                             pov_line, residuals, is_log,
                                             skip_coef, seed) {
  metadata <- .bench_aggregation_metadata(pipelines, residuals)
  if (identical(residuals, "original") &&
      !isTRUE(metadata$shared_context_available)) {
    stop(
      "Original-residual aggregation requires train_aug and id_col context ",
      "for every compact pipeline.", call. = FALSE
    )
  }
  pov_lines <- setNames(rep(pov_line, length(methods)), methods)
  lapply(pipelines, function(entry) {
    aggregate_pipeline_tables_multi(
      pipelines = entry$pipe,
      methods = methods,
      weighted = weighted,
      pov_lines = pov_lines,
      residuals = residuals,
      is_log = is_log,
      skip_coef = skip_coef,
      seed = seed,
      shared_context = entry$shared_context
    )
  })
}

.bench_aggregation_metadata <- function(pipelines, residuals) {
  available <- all(vapply(pipelines, function(entry) {
    context <- step2_pipeline_context(entry$pipe, entry$shared_context)
    !is.null(context$train_aug) && !is.null(context$id_col)
  }, logical(1)))
  list(
    residuals_requested = residuals,
    residuals_effective = if (!identical(residuals, "original") || available) {
      residuals
    } else {
      "unavailable"
    },
    shared_context_available = available
  )
}
