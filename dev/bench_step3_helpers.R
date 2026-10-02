# Development-only helpers for the opt-in Step 3 benchmark path.

.bench_step3_policy_fixtures <- function(labels = c(
  "covariate", "targeted_sp", "combined"
)) {
  infra <- list(elec_universal = FALSE, elec_access_change_pct = 30)
  sp <- list(
    budget_mode = "transfer_first",
    transfer_n_payments = 4L,
    transfer_amount_usd = 30,
    targeting = "exante_poor",
    targeting_threshold = 30,
    inclusion_error_pct = 10,
    exclusion_error_pct = 10
  )
  fixtures <- list(
    covariate = list(infra = infra),
    targeted_sp = list(sp = sp),
    combined = list(infra = infra, sp = sp)
  )
  unknown <- setdiff(labels, names(fixtures))
  if (length(unknown)) {
    stop("Unknown Step 3 policy fixture(s): ", paste(unknown, collapse = ", "),
         call. = FALSE)
  }
  fixtures[labels]
}

.bench_step3_model_terms <- function(model_fit) {
  if (identical(model_fit$engine, "rif")) {
    return(unique(as.character(model_fit$rif_grid$term)))
  }
  names(tryCatch(stats::coef(model_fit$fit3), error = function(e) numeric(0)))
}

.bench_step3_changed_columns <- function(baseline, policy) {
  columns <- union(names(baseline), names(policy))
  columns[vapply(columns, function(column) {
    !column %in% names(baseline) || !column %in% names(policy) ||
      !identical(baseline[[column]], policy[[column]])
  }, logical(1))]
}

.bench_step3_pipelines <- function(hist_sim, saved_scenarios) {
  out <- list(historical = hist_sim$pipeline)
  for (scenario_name in names(saved_scenarios)) {
    scenario <- saved_scenarios[[scenario_name]]
    for (member_name in names(scenario$pipelines)) {
      out[[paste(scenario_name, member_name, sep = " / ")]] <-
        scenario$pipelines[[member_name]]
    }
  }
  out
}

.bench_step3_aggregate <- function(policy_result, methods, residuals,
                                   skip_coef, is_log, seed) {
  hist_sim <- policy_result$hist_sim
  pipes <- list(historical = list(
    pipe = hist_sim$pipeline,
    shared_context = hist_sim$shared_context %||% NULL
  ))
  for (scenario_name in names(policy_result$saved_scenarios)) {
    scenario <- policy_result$saved_scenarios[[scenario_name]]
    for (member_name in names(scenario$pipelines)) {
      pipes[[paste(scenario_name, member_name, sep = " / ")]] <- list(
        pipe = scenario$pipelines[[member_name]],
        shared_context = scenario$shared_context %||% NULL
      )
    }
  }
  out <- list()
  for (method in methods) {
    poverty_line <- if (method %in% c("headcount_ratio", "gap", "fgt2")) 3 else NULL
    out[[method]] <- .bench_aggregate_pipelines(
      pipelines = pipes,
      method = method,
      weighted = TRUE,
      pov_line = poverty_line,
      residuals = residuals,
      is_log = is_log,
      skip_coef = skip_coef,
      seed = seed
    )
  }
  out
}

.bench_step3_decompose_future <- function(policy_result) {
  policy_result$decomp_scenarios %||% list()
}

.bench_step3_metric_switches <- function(baseline_result, policy_result,
                                         requested_residuals, analysis_unit,
                                         size_fn) {
  prepared <- policy_result$annual_channels
  baseline_hist <- baseline_result$hist_sim_result
  policy_hist <- policy_result$hist_sim
  baseline_scenarios <- baseline_result$new_scenarios %||% list()
  policy_scenarios <- policy_result$saved_scenarios %||% list()
  wrap_scenarios <- function(scenarios) lapply(scenarios, function(scenario) {
    list(pipelines = scenario$pipelines %||% list(),
         shared_context = scenario$shared_context %||% NULL)
  })
  baseline_scenarios <- wrap_scenarios(baseline_scenarios)
  policy_scenarios <- wrap_scenarios(policy_scenarios)
  focus_scenario <- if (length(baseline_scenarios)) {
    names(baseline_scenarios)[[1L]]
  } else baseline_hist$hist_label %||% "Historical"
  methods <- c("mean", "headcount_ratio")
  rows <- lapply(methods, function(method) {
    pov_line <- if (identical(method, "headcount_ratio")) 3 else NULL
    started <- proc.time()[["elapsed"]]
    result <- tryCatch(.policy_metric_decomposition(
      baseline_hist = baseline_hist,
      policy_hist = policy_hist,
      baseline_scenarios = baseline_scenarios,
      policy_scenarios = policy_scenarios,
      prepared = prepared,
      method = method,
      pov_line = pov_line,
      requested_residuals = requested_residuals,
      focus_scenario = focus_scenario,
      analysis_unit = analysis_unit
    ), error = function(e) list(
      status = "error", reason = conditionMessage(e),
      annual = data.frame(), summary = data.frame(),
      return_period = data.frame(), mechanisms = list()
    ))
    elapsed <- proc.time()[["elapsed"]] - started
    result_size <- size_fn(result)
    mechanisms <- result$mechanisms$annual %||% data.frame()
    data.frame(
      method = method,
      status = result$status %||% "error",
      error = result$reason %||% "",
      elapsed_seconds = elapsed,
      scope = if (nrow(result$annual %||% data.frame())) {
        if ("scope" %in% names(result$annual)) unique(result$annual$scope)[[1L]] else
          "production_prediction_rows"
      } else NA_character_,
      n_annual_rows = nrow(result$annual %||% data.frame()),
      n_summary_rows = nrow(result$summary %||% data.frame()),
      n_return_period_rows = nrow(result$return_period %||% data.frame()),
      n_mechanism_rows = nrow(mechanisms),
      result_object_bytes = result_size$object_bytes,
      result_serialized_bytes = result_size$serialized_bytes,
      result_deduplicated_bytes = result_size$deduplicated_bytes,
      preparation_seconds = 0,
      prediction_reruns = 0L,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

.bench_step3_pipeline_rows <- function(pipeline, rows, context) {
  exposure <- pipeline$weather_exposure
  table_rows <- unique(exposure$row_index[rows])
  table <- exposure$table[table_rows, unique(c(
    "code", "year", "survname", "loc_id", "int_month", "timestamp",
    context$weather_vars
  )), drop = FALSE]
  remap <- match(exposure$row_index[rows], table_rows)
  out <- list(
    y_point = pipeline$y_point[rows],
    svy_row_id = pipeline$svy_row_id[rows],
    sim_year = pipeline$sim_year[rows],
    weight = pipeline$weight[rows],
    id_vec = pipeline$id_vec[rows],
    weather_exposure = list(
      status = exposure$status,
      table = table,
      row_index = remap,
      prediction_row_id = exposure$prediction_row_id[rows],
      svy_row_id = exposure$svy_row_id[rows],
      sim_year = exposure$sim_year[rows],
      weight = exposure$weight[rows],
      id_vec = exposure$id_vec[rows]
    )
  )
  out
}

.bench_step3_annual_check <- function(pipeline, prepared, context,
                                      run_identity, max_reference_rows) {
  n <- length(pipeline$y_point)
  rows <- seq_len(min(n, max(1L, as.integer(max_reference_rows))))
  sample_pipe <- .bench_step3_pipeline_rows(pipeline, rows, context)
  started <- proc.time()[["elapsed"]]
  optimized <- .policy_annual_channels(
    pipeline, prepared, run_identity, rows = rows
  )
  optimized_seconds <- proc.time()[["elapsed"]] - started
  started <- proc.time()[["elapsed"]]
  reference <- .policy_annual_channels_reference(
    sample_pipe, context, run_identity
  )
  reference_seconds <- proc.time()[["elapsed"]] - started
  fields <- c(
    "delta_sp", "delta_main_covar", "delta_main", "delta_res1",
    "delta_res2", "delta_total"
  )
  differences <- unlist(lapply(fields, function(field) {
    abs(optimized[[field]] - reference[[field]])
  }), use.names = FALSE)
  list(
    status = if (identical(reference$status, "ok") &&
      identical(optimized$status, "ok")) "ok" else "error",
    error = if (!identical(reference$status, "ok")) {
      reference$reason %||% reference$status
    } else if (!identical(optimized$status, "ok")) {
      optimized$reason %||% optimized$status
    } else "",
    scope = "first_rows_per_pipeline",
    count = length(rows),
    total_rows = n,
    optimized_seconds = optimized_seconds,
    reference_seconds = reference_seconds,
    max_abs_difference = if (length(differences)) max(differences) else NA_real_,
    mean_abs_difference = if (length(differences)) mean(differences) else NA_real_
  )
}

.bench_step3_pipeline_pairs <- function(hist_sim, saved_scenarios) {
  out <- list(historical = list(
    pipeline = hist_sim$pipeline,
    weather = step2_resolve_weather(
      hist_sim$weather_raw %||% hist_sim$pipeline$weather_raw, hist_sim
    )
  ))
  for (scenario_name in names(saved_scenarios)) {
    scenario <- saved_scenarios[[scenario_name]]
    for (member_name in names(scenario$pipelines)) {
      pipe <- scenario$pipelines[[member_name]]
      out[[paste(scenario_name, member_name, sep = " / ")]] <- list(
        pipeline = pipe,
        weather = step2_resolve_weather(pipe$weather_raw %||% scenario$weather_raw,
                                        scenario)
      )
    }
  }
  out
}

.bench_step3_old_correction <- function(pipeline, context, run_identity,
                                        weather_raw) {
  started <- proc.time()[["elapsed"]]
  error <- ""
  value <- tryCatch(
    .policy_central_delta_run(context, run_identity, weather_raw),
    error = function(e) {
      error <<- conditionMessage(e)
      NULL
    }
  )
  list(
    value = value,
    seconds = proc.time()[["elapsed"]] - started,
    error = error
  )
}

.bench_step3_fingerprint <- function(svy_baseline, svy_policy, policy_result,
                                     historical_decomposition,
                                     future_decomposition, aggregations) {
  changed <- .bench_step3_changed_columns(svy_baseline, svy_policy)
  pipes <- .bench_step3_pipelines(
    policy_result$hist_sim, policy_result$saved_scenarios
  )
  decomp_columns <- c(
    "id", "decile", "sp_eligible", "delta_main", "delta_res1", "delta_res2",
    "delta_total", "sd_total", "scenario", "sim_year"
  )
  canonical <- list(
    changed_columns = changed,
    policy_values = svy_policy[intersect(changed, names(svy_policy))],
    pipeline_y_point = lapply(pipes, function(pipe) pipe$y_point),
    historical_decomposition = historical_decomposition[
      intersect(decomp_columns, names(historical_decomposition))
    ],
    future_decomposition = if (.is_compact_decomp_scenarios(
      policy_result$decomp_scenarios %||% list()
    )) policy_result$decomp_scenarios[c(
      "channel_summary", "decile_summary", "scenario_metadata"
    )] else future_decomposition[
      intersect(decomp_columns, names(future_decomposition))
    ],
    aggregations = aggregations
  )
  digest::digest(canonical, algo = "sha256", serialize = TRUE)
}

.bench_step3_empty_sizes <- function() {
  list(object_bytes = NA_real_, serialized_bytes = NA_real_,
       deduplicated_bytes = NA_real_)
}

.bench_run_step3 <- function(baseline_result, input, model_label, policy_label,
                             policy_fixture, identity, config, size_fn,
                             rss_state_fn, rss_sample_fn) {
  started_total <- proc.time()[["elapsed"]]
  rss_state <- rss_state_fn()
  rss_sample_fn(rss_state)
  model_fit <- input$models[[model_label]]
  hist_sim <- baseline_result$hist_sim_result
  saved_scenarios <- baseline_result$new_scenarios %||% list()
  svy_baseline <- input$svy
  so <- input$so
  skip_coef <- identical(identity$uncertainty, "disabled")
  status <- "ok"
  error_text <- ""
  policy_application_seconds <- NA_real_
  analytic_delta_seconds <- NA_real_
  historical_decomposition_seconds <- NA_real_
  future_decomposition_seconds <- NA_real_
  annual_preparation_seconds <- NA_real_
  old_correction_seconds <- NA_real_
  old_correction_calls <- 0L
  annual_reference_seconds <- 0
  annual_optimized_seconds <- 0
  annual_blocking_seconds <- NA_real_
  annual_reference_rows <- 0L
  annual_reference_total_rows <- 0L
  annual_reference_max_abs_difference <- NA_real_
  annual_reference_mean_abs_difference <- NA_real_
  old_new_max_abs_difference <- NA_real_
  old_correction_error <- ""
  annual_reference_error <- ""
  annual_prepared <- NULL
  annual_checks <- list()
  old_differences <- numeric(0)
  results_aggregation_seconds <- NA_real_
  svy_policy <- policy_result <- historical_decomposition <- NULL
  decomp_context <- NULL
  future_decomposition <- list()
  aggregations <- list()
  metric_switches <- data.frame()

  tryCatch({
    t0 <- proc.time()[["elapsed"]]
    svy_policy <- do.call(
      apply_policy_to_svy,
      c(list(
        svy = svy_baseline,
        model_vars = .bench_step3_model_terms(model_fit),
        analysis_unit = config$unit,
        seed = config$seed
      ), policy_fixture)
    )
    policy_application_seconds <- proc.time()[["elapsed"]] - t0
    changed_columns <- .bench_step3_changed_columns(svy_baseline, svy_policy)
    if (!length(changed_columns)) {
      stop("The policy fixture did not change the benchmark survey.", call. = FALSE)
    }
    if (!is.null(policy_fixture$infra) &&
        !"electricity" %in% changed_columns) {
      stop(
        "The covariate fixture requires a non-universal electricity column ",
        "present in the fitted model.", call. = FALSE
      )
    }
    if (!is.null(policy_fixture$sp)) {
      transfer <- svy_policy[[SP_TRANSFER_COL]]
      if (is.null(transfer) || !any(is.finite(transfer) & transfer != 0)) {
        stop("The social-protection fixture produced no transfer.", call. = FALSE)
      }
    }
    rss_sample_fn(rss_state)

    deltas <- .compute_policy_deltas(
      svy_baseline, svy_policy, so$name, model_fit$weather_terms
    )
    F_hat <- if (identical(model_fit$engine, "rif") &&
                 !is.null(model_fit$train_data) &&
                 so$name %in% names(model_fit$train_data)) {
      stats::ecdf(model_fit$train_data[[so$name]])
    } else NULL
    run_identity <- paste0("bench-generation-", identity$repetition)
    decomp_context <- .build_decomposition_context(
      svy_baseline, svy_policy, model_fit, so, deltas, skip_coef, F_hat,
      run_identity = run_identity,
      weather_panels = Filter(Negate(is.null), c(
        list(step2_resolve_weather(hist_sim$weather_raw, hist_sim)),
        lapply(saved_scenarios, function(x) step2_resolve_weather(x$weather_raw, x))
      )),
      adverse_decompositions = list(adverse_10 = data.frame())
    )

    t0 <- proc.time()[["elapsed"]]
    annual_prepared <- .prepare_policy_annual_channels(
      decomp_context, run_identity
    )
    annual_preparation_seconds <- proc.time()[["elapsed"]] - t0
    if (!identical(annual_prepared$status, "ok")) {
      stop("Annual channel preparation failed: ",
           annual_prepared$reason %||% annual_prepared$status, call. = FALSE)
    }

    max_reference_rows <- config$step3_reference_rows %||% 16L
    pipeline_pairs <- .bench_step3_pipeline_pairs(hist_sim, saved_scenarios)
    old_errors <- character(0)
    old_elapsed <- numeric(0)
    for (pair in pipeline_pairs) {
      pipe <- pair$pipeline
      old <- .bench_step3_old_correction(
        pipe, decomp_context, run_identity, pair$weather
      )
      old_elapsed <- c(old_elapsed, old$seconds)
      old_correction_calls <- old_correction_calls + 1L
      if (nzchar(old$error)) old_errors <- c(old_errors, old$error)
      if (!is.null(old$value) && !is.null(pipe$weather_exposure) &&
          identical(pipe$weather_exposure$status, "ok")) {
        rows <- seq_len(min(length(pipe$y_point), max_reference_rows))
        difference <- tryCatch(abs(old$value[pipe$svy_row_id[rows]] -
          .policy_annual_channels(pipe, annual_prepared, run_identity,
            rows = rows)$delta_total), error = function(e) numeric(0))
        old_differences <- c(old_differences, difference)
      }
      check <- tryCatch(.bench_step3_annual_check(
        pipe, annual_prepared, decomp_context, run_identity,
        max_reference_rows
      ), error = function(e) list(
        status = "error", error = conditionMessage(e),
        scope = "first_rows_per_pipeline", count = 0L,
        total_rows = length(pipe$y_point), optimized_seconds = NA_real_,
        reference_seconds = NA_real_, max_abs_difference = NA_real_,
        mean_abs_difference = NA_real_
      ))
      annual_checks[[length(annual_checks) + 1L]] <- check
      annual_reference_rows <- annual_reference_rows + check$count
      annual_reference_total_rows <- annual_reference_total_rows + check$total_rows
      annual_optimized_seconds <- annual_optimized_seconds + check$optimized_seconds
      annual_reference_seconds <- annual_reference_seconds + check$reference_seconds
      if (nzchar(check$error)) {
        annual_reference_error <- paste(annual_reference_error, check$error,
                                        sep = if (nzchar(annual_reference_error)) " | " else "")
      }
    }
    old_correction_seconds <- sum(old_elapsed)
    if (length(old_errors)) old_correction_error <- paste(unique(old_errors),
                                                           collapse = " | ")
    if (length(old_differences)) old_new_max_abs_difference <- max(old_differences)
    check_diffs <- vapply(annual_checks, `[[`, numeric(1), "max_abs_difference")
    check_means <- vapply(annual_checks, `[[`, numeric(1), "mean_abs_difference")
    if (any(is.finite(check_diffs))) {
      annual_reference_max_abs_difference <- max(check_diffs, na.rm = TRUE)
      annual_reference_mean_abs_difference <- mean(check_means, na.rm = TRUE)
    }
    if (any(vapply(annual_checks, function(x) !identical(x$status, "ok"),
                   logical(1))) ||
        (is.finite(annual_reference_max_abs_difference) &&
         annual_reference_max_abs_difference > 1e-10)) {
      stop("Annual optimized/reference parity failed: ",
           if (nzchar(annual_reference_error)) annual_reference_error else
             "difference exceeded tolerance",
           call. = FALSE)
    }

    t0 <- proc.time()[["elapsed"]]
    profile <- Sys.getenv("WISEAPP_STEP3_PROFILE", "")
    if (nzchar(profile)) {
      utils::Rprof(file.path(config$output_dir, paste0(identity$country, "-annual.Rprof")))
      on.exit(utils::Rprof(NULL), add = TRUE)
    }
    policy_result <- apply_policy_delta_to_baseline(
      svy_baseline = svy_baseline,
      svy_policy = svy_policy,
      model_fit = model_fit,
      so = so,
      hist_sim_baseline = hist_sim,
      saved_scenarios_baseline = saved_scenarios,
      skip_coef = skip_coef,
      deltas = deltas,
      F_hat = F_hat,
      decomp_context = decomp_context,
      run_identity = run_identity,
      annual_channels = annual_prepared
    )
    if (nzchar(profile)) utils::Rprof(NULL)
    analytic_delta_seconds <- proc.time()[["elapsed"]] - t0
    annual_blocking_seconds <- analytic_delta_seconds
    if (is.null(policy_result)) {
      stop("Policy delta application produced no results.", call. = FALSE)
    }
    rss_sample_fn(rss_state)

    t0 <- proc.time()[["elapsed"]]
    historical_decomposition <- decompose_policy_effect(
      svy_baseline = svy_baseline,
      svy_policy = svy_policy,
      model_fit = model_fit,
      so = so,
      weather_raw = step2_resolve_weather(hist_sim$weather_raw, hist_sim),
      skip_coef = skip_coef,
      deltas = deltas, F_hat = F_hat,
      context = decomp_context, run_identity = run_identity
    )
    historical_decomposition_seconds <- proc.time()[["elapsed"]] - t0
    if (is.null(historical_decomposition)) {
      stop("Historical effect decomposition produced no results.", call. = FALSE)
    }
    rss_sample_fn(rss_state)

    t0 <- proc.time()[["elapsed"]]
    future_decomposition <- .bench_step3_decompose_future(policy_result)
    future_decomposition_seconds <- proc.time()[["elapsed"]] - t0
    rss_sample_fn(rss_state)

    # Exercise the same cached consumers used by the decomposition and export
    # panes so benchmark counters report observed accesses, not scenario shape.
    if (!is.null(decomp_context)) {
      .decomposition_context_baseline_deciles(decomp_context)
      .decomposition_context_adverse_basis(decomp_context, "mean")
      .decomposition_context_adverse_result(decomp_context, "adverse_10")
    }

    t0 <- proc.time()[["elapsed"]]
    aggregations <- .bench_step3_aggregate(
      policy_result, config$aggregation,
      hist_sim$residuals %||% "original", skip_coef,
      identical(so$transform, "log"), config$seed
    )
    results_aggregation_seconds <- proc.time()[["elapsed"]] - t0
    rss_sample_fn(rss_state)

    metric_switches <- .bench_step3_metric_switches(
      baseline_result, policy_result,
      hist_sim$residuals %||% "original", config$unit, size_fn
    )
    failed_metrics <- metric_switches[metric_switches$status != "ok", , drop = FALSE]
    if (nrow(failed_metrics)) {
      stop("Metric-switch verification failed: ", paste(
        paste0(failed_metrics$method, " (", failed_metrics$status, "): ",
               failed_metrics$error), collapse = " | "), call. = FALSE)
    }
    rss_sample_fn(rss_state)
  }, error = function(e) {
    status <<- "error"
    error_text <<- conditionMessage(e)
  })

  sizes <- list(
    policy_survey = if (is.null(svy_policy)) .bench_step3_empty_sizes() else size_fn(svy_policy),
    policy_result = if (is.null(policy_result)) .bench_step3_empty_sizes() else size_fn(policy_result),
    historical_decomposition = if (is.null(historical_decomposition)) {
      .bench_step3_empty_sizes()
    } else size_fn(historical_decomposition),
    future_decomposition = size_fn(future_decomposition),
    retained = if (identical(status, "ok")) size_fn(list(
      policy_survey = svy_policy,
      policy_result = policy_result,
      historical_decomposition = historical_decomposition,
      future_decomposition = future_decomposition,
      aggregations = aggregations
    )) else .bench_step3_empty_sizes()
  )
  fingerprint <- if (identical(status, "ok")) {
    .bench_step3_fingerprint(
      svy_baseline, svy_policy, policy_result, historical_decomposition,
      future_decomposition, aggregations
    )
  } else NA_character_
  rss_sample_fn(rss_state)

  data.frame(
    country = identity$country,
    model = model_label,
    workload = identity$workload,
    payload_mode = identity$payload_mode,
    weather_storage = identity$weather_storage,
    weather_collect = identity$weather_collect,
    join_cache = identity$join_cache,
    direct_rif_predictions = identity$direct_rif_predictions,
    uncertainty = identity$uncertainty,
    cache = identity$cache,
    repetition = identity$repetition,
    fixture_mode = identity$fixture_mode,
    evidence_class = identity$evidence_class,
    runtime_options_executed = !identical(
      identity$evidence_class, "smoke_only_not_production_evidence"
    ),
    policy_fixture = policy_label,
    seed = config$seed,
    status = status,
    error = error_text,
    elapsed_seconds = proc.time()[["elapsed"]] - started_total,
    policy_seconds = policy_application_seconds + analytic_delta_seconds,
    policy_application_seconds = policy_application_seconds,
    analytic_delta_seconds = analytic_delta_seconds,
    decomposition_seconds = historical_decomposition_seconds +
      future_decomposition_seconds,
    historical_decomposition_seconds = historical_decomposition_seconds,
    future_decomposition_seconds = future_decomposition_seconds,
    results_aggregation_seconds = results_aggregation_seconds,
    parent_peak_rss_kb_sampled = rss_state$rss_parent_peak_kb,
    process_tree_peak_rss_kb_sampled = rss_state$rss_tree_peak_kb,
    policy_survey_object_bytes = sizes$policy_survey$object_bytes,
    policy_survey_serialized_bytes = sizes$policy_survey$serialized_bytes,
    policy_result_object_bytes = sizes$policy_result$object_bytes,
    policy_result_serialized_bytes = sizes$policy_result$serialized_bytes,
    historical_decomposition_object_bytes = sizes$historical_decomposition$object_bytes,
    historical_decomposition_serialized_bytes = sizes$historical_decomposition$serialized_bytes,
    future_decomposition_object_bytes = sizes$future_decomposition$object_bytes,
    future_decomposition_serialized_bytes = sizes$future_decomposition$serialized_bytes,
    retained_object_bytes = sizes$retained$object_bytes,
    retained_serialized_bytes = sizes$retained$serialized_bytes,
    retained_deduplicated_bytes = sizes$retained$deduplicated_bytes,
    n_survey_rows = nrow(svy_baseline),
    n_changed_columns = if (is.null(svy_policy)) NA_integer_ else
      length(.bench_step3_changed_columns(svy_baseline, svy_policy)),
    n_future_scenarios = length(saved_scenarios),
    n_future_members = sum(vapply(saved_scenarios, function(x) {
      length(x$pipelines %||% list())
    }, integer(1))),
    n_historical_decomposition_rows = if (is.null(historical_decomposition)) {
      NA_integer_
    } else nrow(historical_decomposition),
    n_future_decomposition_rows = if (.is_compact_decomp_scenarios(
      future_decomposition
    )) nrow(future_decomposition$channel_summary) else nrow(future_decomposition) %||% 0L,
    annual_correction_version = if (is.environment(annual_prepared)) {
      annual_prepared$correction_version
    } else NA_character_,
    annual_preparation_seconds = annual_preparation_seconds,
    old_full_panel_correction_seconds = old_correction_seconds,
    old_full_panel_correction_calls = old_correction_calls,
    old_full_panel_correction_error = old_correction_error,
    old_new_sample_max_abs_difference = old_new_max_abs_difference,
    annual_reference_scope = if (length(annual_checks)) {
      "first_rows_per_pipeline"
    } else NA_character_,
    annual_reference_sample_rows = annual_reference_rows,
    annual_reference_total_pipeline_rows = annual_reference_total_rows,
    annual_reference_max_rows_per_pipeline = config$step3_reference_rows %||% 16L,
    annual_reference_seconds = annual_reference_seconds,
    annual_optimized_sample_seconds = annual_optimized_seconds,
    annual_reference_max_abs_difference = annual_reference_max_abs_difference,
    annual_reference_mean_abs_difference = annual_reference_mean_abs_difference,
    annual_reference_error = annual_reference_error,
    apply_policy_blocking_seconds = annual_blocking_seconds,
    metric_mean_status = if (nrow(metric_switches)) metric_switches$status[
      match("mean", metric_switches$method)] else NA_character_,
    metric_mean_error = if (nrow(metric_switches)) metric_switches$error[
      match("mean", metric_switches$method)] else NA_character_,
    metric_mean_seconds = if (nrow(metric_switches)) metric_switches$elapsed_seconds[
      match("mean", metric_switches$method)] else NA_real_,
    metric_mean_scope = if (nrow(metric_switches)) metric_switches$scope[
      match("mean", metric_switches$method)] else NA_character_,
    metric_mean_annual_rows = if (nrow(metric_switches)) metric_switches$n_annual_rows[
      match("mean", metric_switches$method)] else NA_integer_,
    metric_mean_summary_rows = if (nrow(metric_switches)) metric_switches$n_summary_rows[
      match("mean", metric_switches$method)] else NA_integer_,
    metric_mean_return_period_rows = if (nrow(metric_switches)) metric_switches$n_return_period_rows[
      match("mean", metric_switches$method)] else NA_integer_,
    metric_mean_mechanism_rows = if (nrow(metric_switches)) metric_switches$n_mechanism_rows[
      match("mean", metric_switches$method)] else NA_integer_,
    metric_mean_object_bytes = if (nrow(metric_switches)) metric_switches$result_object_bytes[
      match("mean", metric_switches$method)] else NA_real_,
    metric_mean_serialized_bytes = if (nrow(metric_switches)) metric_switches$result_serialized_bytes[
      match("mean", metric_switches$method)] else NA_real_,
    metric_mean_deduplicated_bytes = if (nrow(metric_switches)) metric_switches$result_deduplicated_bytes[
      match("mean", metric_switches$method)] else NA_real_,
    metric_headcount_ratio_status = if (nrow(metric_switches)) metric_switches$status[
      match("headcount_ratio", metric_switches$method)] else NA_character_,
    metric_headcount_ratio_error = if (nrow(metric_switches)) metric_switches$error[
      match("headcount_ratio", metric_switches$method)] else NA_character_,
    metric_headcount_ratio_seconds = if (nrow(metric_switches)) metric_switches$elapsed_seconds[
      match("headcount_ratio", metric_switches$method)] else NA_real_,
    metric_headcount_ratio_scope = if (nrow(metric_switches)) metric_switches$scope[
      match("headcount_ratio", metric_switches$method)] else NA_character_,
    metric_headcount_ratio_annual_rows = if (nrow(metric_switches)) metric_switches$n_annual_rows[
      match("headcount_ratio", metric_switches$method)] else NA_integer_,
    metric_headcount_ratio_summary_rows = if (nrow(metric_switches)) metric_switches$n_summary_rows[
      match("headcount_ratio", metric_switches$method)] else NA_integer_,
    metric_headcount_ratio_return_period_rows = if (nrow(metric_switches)) metric_switches$n_return_period_rows[
      match("headcount_ratio", metric_switches$method)] else NA_integer_,
    metric_headcount_ratio_mechanism_rows = if (nrow(metric_switches)) metric_switches$n_mechanism_rows[
      match("headcount_ratio", metric_switches$method)] else NA_integer_,
    metric_headcount_ratio_object_bytes = if (nrow(metric_switches)) metric_switches$result_object_bytes[
      match("headcount_ratio", metric_switches$method)] else NA_real_,
    metric_headcount_ratio_serialized_bytes = if (nrow(metric_switches)) metric_switches$result_serialized_bytes[
      match("headcount_ratio", metric_switches$method)] else NA_real_,
    metric_headcount_ratio_deduplicated_bytes = if (nrow(metric_switches)) metric_switches$result_deduplicated_bytes[
      match("headcount_ratio", metric_switches$method)] else NA_real_,
    metric_preparation_seconds = if (nrow(metric_switches)) max(metric_switches$preparation_seconds) else NA_real_,
    metric_prediction_reruns = if (nrow(metric_switches)) sum(metric_switches$prediction_reruns) else NA_integer_,
    context_hazard_cache_hits = if (is.null(decomp_context)) NA_integer_ else
      .decomposition_context_counter(decomp_context, "hazard_cache_hits", NA_integer_),
    context_adverse_cache_hits = if (is.null(decomp_context)) NA_integer_ else
      .decomposition_context_counter(decomp_context, "adverse_result_cache_hits", NA_integer_),
    context_fixed_decile_reuses = if (is.null(decomp_context)) NA_integer_ else
      .decomposition_context_counter(decomp_context, "fixed_decile_reuses", NA_integer_),
    context_rif_invariant_reuses = if (is.null(decomp_context)) NA_integer_ else
      .decomposition_context_counter(decomp_context, "rif_invariant_reuses", NA_integer_),
    context_term_map_reuses = if (is.null(decomp_context)) NA_integer_ else
      .decomposition_context_counter(decomp_context, "term_map_reuses", NA_integer_),
    context_delta_reuses = if (is.null(decomp_context)) NA_integer_ else
      .decomposition_context_counter(decomp_context, "delta_reuses", NA_integer_),
    output_fingerprint_sha256 = fingerprint,
    stringsAsFactors = FALSE
  )
}

.bench_small_step3_input <- function(country = "fixture", n = 120L,
                                     seed = 123L) {
  withr::with_seed(seed, {
    svy <- data.frame(
      hhid = seq_len(n),
      code = country,
      loc_id = rep(seq_len(6L), length.out = n),
      year = 2020L,
      survname = "fixture",
      int_month = 7L,
      welfare = exp(0.8 + seq(-0.4, 0.4, length.out = n) +
                      stats::rnorm(n, sd = 0.08)),
      hhsize = rep(2:5, length.out = n),
      weight = seq(0.8, 1.2, length.out = n),
      temp = seq(20, 30, length.out = n),
      electricity = rep(c(0L, 1L, 0L), length.out = n),
      stringsAsFactors = FALSE
    )
  })
  ols_fit <- stats::lm(log(welfare) ~ temp * electricity, data = svy)
  taus <- seq(0.1, 0.9, by = 0.1)
  rif_grid <- expand.grid(
    model = 3L,
    term = c("(Intercept)", "temp", "electricity", "temp:electricity"),
    tau = taus,
    stringsAsFactors = FALSE
  )
  term_base <- c(
    "(Intercept)" = 0.8, temp = 0.015, electricity = 0.12,
    "temp:electricity" = 0.004
  )
  rif_grid$estimate <- unname(term_base[rif_grid$term]) * (0.8 + 0.4 * rif_grid$tau)
  rif_grid$std.error <- abs(rif_grid$estimate) * 0.1 + 0.001
  models <- list(
    ols = list(
      engine = "fixest", fit3 = ols_fit, weather_terms = "temp",
      train_data = svy
    ),
    rif = list(
      engine = "rif", fit3 = NULL, weather_terms = "temp",
      train_data = svy, rif_grid = rif_grid, taus = taus
    )
  )
  list(
    country = country,
    sw = data.frame(name = "temp", units = "deg C", stringsAsFactors = FALSE),
    so = list(name = "welfare", transform = "log"),
    svy = svy,
    ss = data.frame(code = country, year = 2020L),
    cp = list(),
    models = models,
    sim_dates = c("2019-01-01", "2020-12-31"),
    stored_breaks = NULL,
    fixture_mode = "smoke",
    metadata = list(
      n_survey_rows = n,
      source = "deterministic in-memory fixture",
      evidence_class = "smoke_only_not_production_evidence"
    )
  )
}

.bench_small_step2_result <- function(input, model_label, workload) {
  model_fit <- input$models[[model_label]]
  svy <- input$svy
  years <- 2019:2020
  make_weather <- function(year_values, offset = 0) {
    grid <- expand.grid(
      loc_id = sort(unique(svy$loc_id)),
      year = year_values,
      KEEP.OUT.ATTRS = FALSE
    )
    grid$timestamp <- as.Date(paste0(grid$year, "-07-01"))
    grid$temp <- 22 + grid$loc_id * 0.5 + (grid$year - min(grid$year)) * 0.1 + offset
    grid
  }
  make_pipeline <- function(year_values, weather, member_offset = 0) {
    row_id <- rep(seq_len(nrow(svy)), times = length(year_values))
    sim_year <- rep(year_values, each = nrow(svy))
    member_weather <- weather
    member_weather$temp <- member_weather$temp + member_offset
    hazard <- mean(member_weather$temp)
    y_base <- if (identical(model_label, "rif")) {
      log(svy$welfare) + 0.01 * (hazard - mean(svy$temp))
    } else {
      as.numeric(stats::predict(model_fit$fit3, newdata = svy)) +
        0.01 * (hazard - mean(svy$temp))
    }
    weather_match <- match(
      paste(sim_year, svy$loc_id[row_id]),
      paste(member_weather$year, member_weather$loc_id)
    )
    exposure_table <- data.frame(
      code = svy$code[row_id],
      year = as.character(svy$year[row_id]),
      survname = svy$survname[row_id],
      loc_id = svy$loc_id[row_id],
      int_month = svy$int_month[row_id],
      sim_year = sim_year,
      timestamp = as.Date(paste0(sim_year, "-07-01")),
      temp = member_weather$temp[weather_match],
      stringsAsFactors = FALSE
    )
    weather_exposure <- list(
      status = "ok",
      table = exposure_table,
      row_index = seq_along(row_id),
      prediction_row_id = seq_along(row_id),
      svy_row_id = row_id,
      sim_year = sim_year,
      weight = rep(svy$weight, times = length(year_values)),
      id_vec = rep(svy$hhid, times = length(year_values))
    )
    list(
      y_point = rep(y_base, times = length(year_values)),
      F_loading = NULL,
      sim_year = sim_year,
      weight = rep(svy$weight, times = length(year_values)),
      id_vec = rep(svy$hhid, times = length(year_values)),
      id_col = "hhid",
      svy_row_id = row_id,
      weather_exposure = weather_exposure,
      train_aug = if (identical(model_label, "rif")) NULL else
        transform(svy, .resid = stats::residuals(model_fit$fit3)),
      weather_raw = member_weather
    )
  }
  historical_weather <- make_weather(years)
  historical <- list(
    pipeline = make_pipeline(years, historical_weather),
    weather_raw = historical_weather,
    so = input$so,
    svy = svy,
    train_data = model_fit$train_data,
    residuals = if (identical(model_label, "rif")) "none" else "original"
  )
  scenarios <- list()
  if (!identical(workload, "historical")) {
    future_years <- if (identical(workload, "one_ssp_one_period")) 2030:2031 else 2030:2032
    future_weather <- make_weather(future_years, offset = 1.5)
    member_count <- if (identical(workload, "one_ssp_one_period")) 1L else 3L
    members <- setNames(lapply(seq_len(member_count), function(i) {
      make_pipeline(future_years, future_weather, member_offset = (i - 1L) * 0.2)
    }), paste0("member_", seq_len(member_count)))
    scenarios[["SSP2-4.5 / fixture"]] <- list(
      pipelines = members,
      weather_raw = future_weather,
      year_range = range(future_years),
      residuals = historical$residuals
    )
  }
  list(
    hist_sim_result = historical,
    new_scenarios = scenarios,
    chol_obj = NULL,
    n_keys = 1L + sum(vapply(scenarios, function(x) length(x$pipelines), integer(1))),
    total_runs = length(years),
    t_elapsed = 0,
    failures = list(),
    n_keys_ok = 1L + sum(vapply(scenarios, function(x) length(x$pipelines), integer(1)))
  )
}
