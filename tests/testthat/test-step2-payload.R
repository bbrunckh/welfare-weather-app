# Phase 4 compact shared-context payload characterization.

library(testthat)

phase4_weather <- function() {
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

phase4_pipeline <- function(weather_raw, ...) {
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
    train_aug = phase4_train_aug()
  )
}

phase4_train_aug <- function() {
  data.frame(
    hhid = paste0("hh", seq_len(50L)),
    .resid = seq_len(50L) - 1.5,
    stringsAsFactors = FALSE
  )
}

phase4_input <- function() {
  train <- data.frame(
    hhid = paste0("hh", seq_len(50L)),
    welfare = seq_len(50L), temp = rep(c(1, 2), length.out = 50L),
    stringsAsFactors = FALSE
  )
  list(
    sw = data.frame(name = "temp", stringsAsFactors = FALSE),
    so = data.frame(name = "welfare", type = "numeric", transform = "log",
                    stringsAsFactors = FALSE),
    svy = transform(
      train, code = "TST", year = 2020L, survname = "SRV", loc_id = "loc01"
    ),
    ss = NULL,
    mf = list(
      fit3 = stats::lm(welfare ~ temp, data = train), engine = "lm",
      train_data = train, weather_terms = "temp"
    ),
    cp = list(type = "local", path = tempdir()),
    fp_list = list(c("2030-01-01", "2040-12-31")),
    ssps = "ssp2_4_5",
    residuals = "original",
    skip_coef_draws = TRUE,
    sim_dates = c("2020-01-01", "2020-12-31"),
    perturbation_method = NULL,
    stored_breaks = NULL
  )
}

phase4_run <- function(payload_mode) {
  input <- phase4_input()
  do.call(fct_run_simulation, c(input, list(
      payload_mode = payload_mode,
      progress_fn = function(...) invisible(NULL),
      weather_fn = function(...) phase4_weather(),
      pipeline_fn = phase4_pipeline
    )))
}

test_that("compact mode preserves science and member-specific weather", {
  legacy <- suppressWarnings(phase4_run("legacy"))
  compact <- suppressWarnings(phase4_run("compact"))

  expect_identical(compact$payload_mode, "compact")
  expect_null(legacy$payload_mode)
  expect_identical(
    compact$hist_sim_result$pipeline$y_point,
    legacy$hist_sim_result$pipeline$y_point
  )
  expect_identical(compact$n_keys, legacy$n_keys)
  expect_identical(compact$n_keys_ok, legacy$n_keys_ok)
  expect_identical(compact$failures, legacy$failures)
  expect_identical(
    step2_resolve_weather(
      compact$new_scenarios[[1L]]$pipelines$ensemble_mean$weather_raw,
      compact$new_scenarios[[1L]]
    )$temp,
    2
  )
  expect_identical(
    step2_resolve_weather(
      compact$new_scenarios[[1L]]$pipelines$ensemble_hi$weather_raw,
      compact$new_scenarios[[1L]]
    )$temp,
    3
  )
})

test_that("compact pipelines resolve shared residual metadata", {
  compact <- suppressWarnings(phase4_run("compact"))
  hist <- compact$hist_sim_result
  pipe <- hist$pipeline

  expect_null(pipe$train_aug)
  expect_null(pipe$id_col)
  expect_identical(hist$shared_context$schema, 1L)
  expect_true(is.data.frame(hist$shared_context$train_aug))
  expect_identical(hist$shared_context$id_col, "hhid")

  legacy <- suppressWarnings(phase4_run("legacy"))
  legacy$hist_sim_result$pipeline$train_aug <-
    compact$hist_sim_result$shared_context$train_aug
  legacy_agg <- aggregate_pipeline_per_year(
    legacy$hist_sim_result$pipeline,
    method = "mean", residuals = "original", is_log = FALSE, seed = 123L
  )
  compact_agg <- aggregate_pipeline_per_year(
    pipe,
    method = "mean", residuals = "original", is_log = FALSE, seed = 123L,
    shared_context = hist$shared_context
  )
  expect_identical(legacy_agg, compact_agg)
})

test_that("compact residual context retains only mode-required columns", {
  train_aug <- data.frame(
    hhid = c("b", "a", "a", "missing"),
    covariate = factor(c("x", "y", "y", "z")), .fitted = 1:4,
    .resid = c(-0.2, 0.1, 0.1, NA_real_), stringsAsFactors = FALSE
  )
  original <- .compact_residual_context(train_aug, "hhid", "original")
  expect_identical(names(original), c("hhid", ".resid"))
  expect_identical(original$hhid, train_aug$hhid)
  expect_identical(original$.resid, train_aug$.resid)
  for (mode in c("normal", "resample")) {
    compact <- .compact_residual_context(train_aug, "hhid", mode)
    expect_identical(names(compact), ".resid", info = mode)
    expect_identical(compact$.resid, train_aug$.resid, info = mode)
  }
  expect_null(.compact_residual_context(train_aug, "hhid", "none"))
  expect_null(.compact_residual_context(NULL, "hhid", "original"))
  expect_identical(.compact_residual_context(
    train_aug, "hhid", "original", compact = FALSE), train_aug)
})

test_that("compact residual context preserves ID alignment and deterministic RNG", {
  train_aug <- data.frame(
    hhid = c("b", "a", "a", "c"), extra = I(list(1, 2, 3, 4)),
    .resid = c(-2, 1, 9, 3), stringsAsFactors = FALSE
  )
  compact <- .compact_residual_context(train_aug, "hhid", "original")
  ids <- c("a", "missing", "a", "c")
  for (mode in c("original", "normal", "resample")) {
    set.seed(915L)
    before <- .Random.seed
    full <- draw_residuals_vec(mode, train_aug, length(ids), ids, "hhid", seed = 51L)
    expect_identical(.Random.seed, before)
    slim <- if (identical(mode, "original")) compact else
      .compact_residual_context(train_aug, "hhid", mode)
    reduced <- draw_residuals_vec(mode, slim, length(ids), ids, "hhid", seed = 51L)
    expect_identical(reduced, full, info = mode)
  }
})

test_that("compact payload is smaller while retaining required uncertainty slots", {
  legacy <- suppressWarnings(phase4_run("legacy"))
  compact <- suppressWarnings(phase4_run("compact"))
  legacy_pipelines <- c(
    list(legacy$hist_sim_result$pipeline),
    unlist(lapply(legacy$new_scenarios, `[[`, "pipelines"), recursive = FALSE)
  )
  compact_pipelines <- c(
    list(compact$hist_sim_result$pipeline),
    unlist(lapply(compact$new_scenarios, `[[`, "pipelines"), recursive = FALSE)
  )
  expect_true(
    sum(vapply(legacy_pipelines, object.size, numeric(1))) >
      sum(vapply(compact_pipelines, object.size, numeric(1)))
  )
  expect_true("F_loading" %in% names(compact$hist_sim_result$pipeline))
  expect_true("weather_raw" %in% names(compact$hist_sim_result$pipeline))
  expect_true("svy_row_id" %in% names(compact$hist_sim_result$pipeline))
  expect_null(compact$hist_sim_result$train_data)
  expect_null(compact$hist_sim_result$cluster_counts)
})

test_that("compact payload retains non-null coefficient uncertainty", {
  legacy <- suppressWarnings(phase4_run("legacy"))
  beta <- c(`(Intercept)` = 1, temp = 0.5)
  legacy$hist_sim_result$chol_obj <- list(L = diag(2), beta = beta)
  legacy$hist_sim_result$pipeline$F_loading <- matrix(
    c(1, 0.5, 1, 1), nrow = 2, byrow = TRUE
  )
  compact <- compact_step2_result(
    legacy, phase4_train_aug(), "hhid", "original",
    legacy$hist_sim_result$chol_obj, legacy$hist_sim_result$so,
    phase4_input()$mf$train_data
  )
  expect_true(is.list(compact$hist_sim_result$chol_obj))
  expect_true(is.matrix(compact$hist_sim_result$pipeline$F_loading))
  expect_null(compact$hist_sim_result$train_data)
  expect_null(compact$hist_sim_result$cluster_counts)
})

test_that("weather references round-trip and reject stale signatures", {
  root <- withr::local_tempdir()
  weather <- phase4_weather()$ssp2_4_5_2030_2040_ensemble_mean
  store <- step2_weather_store_create("test-run", "sig-a", root = root)
  ref <- step2_weather_store_put(store, "member-a", weather)

  expect_identical(step2_weather_store_get(ref, "sig-a"), weather)
  expect_error(
    step2_weather_store_get(ref, "sig-b"),
    "stale"
  )
  expect_true(file.exists(ref$file))
  step2_weather_store_cleanup(store)
  expect_false(dir.exists(store$dir))
})

test_that("plain weather data frames pass through without schema warnings", {
  weather <- phase4_weather()$historical

  expect_warning(
    resolved <- step2_weather_reference(weather),
    NA
  )
  expect_identical(resolved, weather)
})

test_that("tibble weather frames pass through without schema warnings", {
  # Weather frames are tibbles (dplyr::collect); tibbles are lists, so the
  # shared-key descriptor check must not touch $schema/$kind on them.
  weather <- tibble::as_tibble(phase4_weather()$historical)

  expect_warning(
    resolved <- step2_resolve_weather(weather),
    NA
  )
  expect_identical(resolved, weather)
})

test_that("shared weather ownership validates keys and keeps member values", {
  keys <- data.frame(
    code = c("TST", "TST"), year = c(2020L, 2020L), survname = "SRV",
    loc_id = c("a", "b"), timestamp = as.POSIXct(c("2030-01-01", "2030-02-01")),
    temp = c(1, 2), stringsAsFactors = FALSE
  )
  hi <- keys
  hi$temp <- c(3, 4)
  shared <- step2_weather_share_members(list(keys, hi))
  owner <- list(weather_shared = shared$shared)
  expect_identical(step2_weather_resolve_shared(shared$members[[1L]], owner)$temp, c(1, 2))
  expect_identical(step2_weather_resolve_shared(shared$members[[2L]], owner)$temp, c(3, 4))
  expect_error(step2_weather_resolve_shared(shared$members[[1L]]), "owning key")
  changed <- hi
  changed$loc_id[[2L]] <- "different"
  expect_null(step2_weather_share_members(list(keys, changed))$shared)
})

test_that("weather collection policy falls back only when fast exceeds budget", {
  withr::local_envvar(c(
    WISEAPP_STEP2_WEATHER_RSS_BUDGET_MB = "1",
    WISEAPP_STEP2_WEATHER_RSS_MEASURE = "1"
  ))
  policy <- .wx_collection_policy(2 * 1024^2, "fast")
  expect_identical(policy$effective, "bounded")
  expect_true(policy$fallback)
  expect_true(is.numeric(policy$external_rss_before))
  expect_identical(.wx_collection_policy(2 * 1024^2, "bounded")$effective, "bounded")
  withr::local_envvar(WISEAPP_STEP2_WEATHER_RSS_BUDGET_MB = "4096")
  expect_identical(.wx_collection_policy(2 * 1024^2, "fast")$effective, "fast")
})

test_that("weather thread policy keeps auto conservative until rollout is enabled", {
  base <- list(
    connection_type = "local", estimated_bytes = 128 * 1024^2,
    rss_before = 100, budget_bytes = 1024^3, available_cpus = 2L
  )
  disabled <- do.call(.wx_thread_policy, c(list(requested = "auto"), base))
  expect_identical(disabled$selected_threads, 1L)
  expect_identical(disabled$reason, "auto_rollout_disabled")

  enabled <- do.call(.wx_thread_policy, c(
    list(requested = "auto", auto_enabled = TRUE), base
  ))
  expect_identical(enabled$selected_threads, 2L)
  expect_identical(enabled$reason, "auto_preflight_passed")
  expect_identical(enabled$rounding_digits, 5L)
})

test_that("weather thread policy falls back for remote, CPU, and RSS gates", {
  common <- list(
    estimated_bytes = 128 * 1024^2, rss_before = 100,
    budget_bytes = 1024^3, auto_enabled = TRUE
  )
  remote <- do.call(.wx_thread_policy, c(
    list(requested = "auto", connection_type = "databricks", available_cpus = 2L),
    common
  ))
  expect_identical(remote$selected_threads, 1L)
  expect_identical(remote$reason, "remote_backend")

  cpu <- do.call(.wx_thread_policy, c(
    list(requested = "auto", connection_type = "local", available_cpus = 1L),
    common
  ))
  expect_identical(cpu$selected_threads, 1L)
  expect_identical(cpu$reason, "insufficient_cpu")

  rss <- do.call(.wx_thread_policy, c(
    list(requested = "auto", connection_type = "local", available_cpus = 2L,
         budget_bytes = 100),
    list(
      estimated_bytes = common$estimated_bytes, rss_before = common$rss_before,
      auto_enabled = common$auto_enabled
    )
  ))
  expect_identical(rss$selected_threads, 1L)
  expect_identical(rss$reason, "rss_budget_exceeded")

  explicit <- do.call(.wx_thread_policy, c(
    list(requested = "2", connection_type = "local", available_cpus = 2L),
    common
  ))
  expect_identical(explicit$selected_threads, 2L)
  expect_identical(explicit$reason, "explicit_two")
})

test_that("weather output rounding changes finite weather values only", {
  input <- data.frame(
    tx = c(1.123456789012345, NA_real_, Inf, -Inf),
    loc_id = 1:4
  )
  out <- .wx_round_weather_values(input, "tx")
  expect_identical(out$loc_id, input$loc_id)
  expect_equal(out$tx[[1L]], 1.12346)
  expect_true(is.na(out$tx[[2L]]))
  expect_identical(out$tx[3:4], input$tx[3:4])
})

test_that("observed RSS guard records bounded producer evidence", {
  withr::local_envvar(WISEAPP_STEP2_WEATHER_RSS_BUDGET_MB = "4096")
  policy <- .wx_collection_policy(1, "fast")
  guard <- .wx_collection_rss_guard(policy)
  expect_true(is.numeric(guard$rss))
  expect_false(guard$exceeded)
})

test_that("RSS guard activates when observed process-tree RSS exceeds budget", {
  withr::local_envvar(WISEAPP_STEP2_WEATHER_RSS_BUDGET_MB = "0.000001")
  policy <- .wx_collection_policy(1, "fast")
  guard <- .wx_collection_rss_guard(policy)
  expect_true(guard$exceeded)
})

test_that("reference weather storage preserves member-specific payloads", {
  root <- withr::local_tempdir()
  input <- phase4_input()
  result <- suppressWarnings(do.call(fct_run_simulation, c(input, list(
    weather_storage = "reference",
    weather_store_root = root,
    progress_fn = function(...) invisible(NULL),
    weather_fn = function(...) phase4_weather(),
    pipeline_fn = phase4_pipeline
  ))))
  on.exit(step2_weather_store_cleanup(result$weather_store), add = TRUE)

  expect_identical(result$weather_storage, "reference")
  scenario <- result$new_scenarios[[1L]]
  expect_true(is.list(scenario$weather_raw) && is.character(scenario$weather_raw$file))
  mean_weather <- step2_resolve_weather(
    scenario$pipelines$ensemble_mean$weather_raw, scenario
  )
  hi_weather <- step2_resolve_weather(
    scenario$pipelines$ensemble_hi$weather_raw, scenario
  )
  expect_identical(mean_weather$temp, 2)
  expect_identical(hi_weather$temp, 3)
})

test_that("reference storage does not create a future store for historical-only runs", {
  root <- withr::local_tempdir()
  input <- phase4_input()
  input$fp_list <- list()
  input$ssps <- character(0)
  result <- suppressWarnings(do.call(fct_run_simulation, c(input, list(
    weather_storage = "reference", weather_store_root = root,
    progress_fn = function(...) invisible(NULL),
    weather_fn = function(...) list(historical = phase4_weather()$historical),
    pipeline_fn = phase4_pipeline
  ))))

  expect_null(result$weather_store)
  expect_null(result$weather_store_lease)
  expect_length(list.files(root, recursive = TRUE), 0L)
})

test_that("reference stores are retained by a lease and released on replacement", {
  root <- withr::local_tempdir()
  input <- phase4_input()
  first <- suppressWarnings(do.call(fct_run_simulation, c(input, list(
    weather_storage = "reference", weather_store_root = root,
    progress_fn = function(...) invisible(NULL),
    weather_fn = function(...) phase4_weather(),
    pipeline_fn = phase4_pipeline
  ))))
  first_dir <- first$weather_store$dir
  expect_true(dir.exists(first_dir))
  expect_true(dir.exists(first$weather_store$dir))
  expect_true(length(first$weather_store_lease$lease_id) == 1L)
  first_key <- normalizePath(first_dir, winslash = "/", mustWork = FALSE)
  expect_equal(step2_weather_store_registry_snapshot()$refs[[first_key]], 1L)

  second <- suppressWarnings(do.call(fct_run_simulation, c(input, list(
    weather_storage = "reference", weather_store_root = root,
    progress_fn = function(...) invisible(NULL),
    weather_fn = function(...) phase4_weather(),
    pipeline_fn = phase4_pipeline
  ))))
  expect_true(dir.exists(second$weather_store$dir))
  step2_weather_store_release(first$weather_store_lease)
  expect_false(dir.exists(first_dir))
  step2_weather_store_release(second$weather_store_lease)
  expect_false(dir.exists(second$weather_store$dir))
})

test_that("failed reference runs release stores and do not publish failed members", {
  root <- withr::local_tempdir()
  input <- phase4_input()
  weather <- phase4_weather()
  attr(weather$ssp2_4_5_2030_2040_ensemble_hi, "fail") <- TRUE
  pipeline <- function(weather_raw, ...) {
    if (isTRUE(attr(weather_raw, "fail"))) stop("injected failure")
    phase4_pipeline(weather_raw, ...)
  }
  result <- suppressWarnings(do.call(fct_run_simulation, c(input, list(
    weather_storage = "reference", weather_store_root = root,
    progress_fn = function(...) invisible(NULL),
    weather_fn = function(...) weather,
    pipeline_fn = pipeline
  ))))
  on.exit(step2_weather_store_release(result$weather_store_lease), add = TRUE)

  expect_length(result$failures, 1L)
  expect_length(list.files(result$weather_store$dir, pattern = "\\.rds$"), 2L)
  expect_true(all(vapply(result$new_scenarios, function(s) {
    !any(vapply(s$pipelines, function(p)
      identical(p$weather_raw$key, "ssp2_4_5_2030_2040_ensemble_hi"), logical(1)))
  }, logical(1))))
})

test_that("a published policy-style lease protects Step 2 weather on replacement", {
  root <- withr::local_tempdir()
  first <- step2_weather_store_create("step2-run", "sig-a", root)
  second <- step2_weather_store_create("step3-run", "sig-b", root)
  step2_weather_store_put(first, "member-a", phase4_weather()$historical)
  step2_weather_store_put(second, "member-b", phase4_weather()$historical)
  step2_lease <- step2_weather_store_acquire(first)
  policy_lease <- step2_weather_store_acquire_scenarios(
    list(list(weather_store = first), list(weather_store = second))
  )
  on.exit({
    step2_weather_store_release(step2_lease)
    step2_weather_store_release(policy_lease)
  }, add = TRUE)

  expect_warning(step2_weather_store_cleanup(first), "still referenced")
  expect_true(dir.exists(first$dir))
  step2_weather_store_release(step2_lease)
  expect_true(dir.exists(first$dir))
  expect_true(dir.exists(second$dir))

  step2_weather_store_release(policy_lease)
  expect_false(dir.exists(first$dir))
  expect_false(dir.exists(second$dir))
})

test_that("reference-backed context preparation reuses resolved hazard panels", {
  root <- withr::local_tempdir()
  weather <- phase4_weather()$historical
  store <- step2_weather_store_create("context-run", "sig-context", root = root)
  ref <- step2_weather_store_put(store, "historical", weather)
  owner <- list(weather_signature = "sig-context")
  context <- wiseapp:::.build_decomposition_context(
    data.frame(welfare = 1:2, temp = c(1, 2)),
    data.frame(welfare = 1:2, temp = c(1, 2)),
    list(engine = "fixest", fit3 = lm(welfare ~ temp,
                                       data.frame(welfare = 1:2, temp = c(1, 2))),
         weather_terms = "temp"),
    list(name = "welfare", transform = "none"),
    run_identity = "context-run", weather_panels = list(ref)
  )
  result <- wiseapp:::.decomposition_context_hazard_values(
    context, data.frame(welfare = 1:2, temp = c(1, 2)),
    step2_resolve_weather(ref, owner), "temp"
  )
  expect_equal(context$reuse_counters$hazard_cache_hits, 1L)
  expect_equal(result$temp, c(1, 1))
  step2_weather_store_cleanup(store)
})

test_that("experimental join cache preserves simulation outputs", {
  input <- phase4_input()
  input$svy$int_month <- 6L
  input$svy$year <- 2020L
  run <- function(use_cache) suppressWarnings(do.call(
    fct_run_simulation,
    c(input, list(
      join_cache = use_cache,
      progress_fn = function(...) invisible(NULL),
      weather_fn = function(...) phase4_weather(),
      pipeline_fn = phase4_pipeline
    ))
  ))
  baseline <- run(FALSE)
  cached <- run(TRUE)
  expect_identical(cached$n_keys, baseline$n_keys)
  expect_identical(cached$failures, baseline$failures)
  expect_identical(
    cached$hist_sim_result$pipeline$y_point,
    baseline$hist_sim_result$pipeline$y_point
  )
  expect_identical(
    cached$new_scenarios[[1L]]$pipelines$ensemble_mean$y_point,
    baseline$new_scenarios[[1L]]$pipelines$ensemble_mean$y_point
  )
})

test_that("prepared weather cache replays a complete cold run", {
  root <- withr::local_tempdir()
  input <- phase4_input()
  calls <- 0L
  loader <- function(weather_consumer, ...) {
    calls <<- calls + 1L
    weather <- phase4_weather()
    for (key in names(weather)) weather_consumer(key, weather[[key]])
    list()
  }
  run <- function() suppressWarnings(do.call(
    fct_run_simulation,
    c(input, list(
      prepared_weather_cache = "read_write",
      prepared_weather_cache_root = root,
      progress_fn = function(...) invisible(NULL),
      weather_fn = loader,
      pipeline_fn = phase4_pipeline
    ))
  ))

  cold <- run()
  warm <- run()
  expect_identical(calls, 1L)
  expect_identical(warm$n_keys, cold$n_keys)
  expect_identical(warm$failures, cold$failures)
  expect_identical(
    warm$hist_sim_result$pipeline$y_point,
    cold$hist_sim_result$pipeline$y_point
  )
  expect_identical(
    warm$new_scenarios[[1L]]$pipelines$ensemble_hi$weather_raw,
    cold$new_scenarios[[1L]]$pipelines$ensemble_hi$weather_raw
  )
  expect_length(list.dirs(file.path(root, "prepared-weather", STEP2_PREPARED_WEATHER_CACHE_VERSION),
                          recursive = FALSE), 1L)
})

test_that("prepared weather cache signature invalidates changed dates", {
  root <- withr::local_tempdir()
  input <- phase4_input()
  calls <- 0L
  loader <- function(weather_consumer, ...) {
    calls <<- calls + 1L
    weather <- phase4_weather()
    for (key in names(weather)) weather_consumer(key, weather[[key]])
    list()
  }
  run <- function(dates) {
    current <- input
    current$sim_dates <- dates
    suppressWarnings(do.call(
    fct_run_simulation,
    c(current, list(
      prepared_weather_cache = "read_write",
      prepared_weather_cache_root = root,
      progress_fn = function(...) invisible(NULL),
      weather_fn = loader,
      pipeline_fn = phase4_pipeline
    ))))
  }
  run(input$sim_dates)
  run(c("2020-02-01", "2020-12-31"))
  expect_identical(calls, 2L)
  expect_length(list.dirs(file.path(root, "prepared-weather", STEP2_PREPARED_WEATHER_CACHE_VERSION),
                          recursive = FALSE), 2L)
})

test_that("prepared weather cache discards an incomplete manifest", {
  root <- withr::local_tempdir()
  signature <- .step2_prepared_weather_cache_signature(
    phase4_input()$sw, phase4_input()$ss, phase4_input()$svy,
    phase4_input()$cp, phase4_input()$sim_dates, phase4_input()$fp_list,
    phase4_input()$ssps, phase4_input()$perturbation_method,
    phase4_input()$stored_breaks
  )
  cache <- .step2_prepared_weather_cache_create(signature, root)
  saveRDS(list(schema = 1L, signature = signature, keys = "historical"),
          file.path(cache$stage, "manifest.rds"))
  file.rename(cache$stage, cache$target)
  cache <- .step2_prepared_weather_cache_create(signature, root)
  expect_null(.step2_prepared_weather_cache_read(cache, signature))
  expect_true(!dir.exists(cache$target))
  if (!is.null(cache$stage)) unlink(cache$stage, recursive = TRUE)
})

test_that("two models consume one prepared weather manifest", {
  input <- phase4_input()
  calls <- 0L
  loader <- function(...) {
    calls <<- calls + 1L
    phase4_weather()
  }
  manifest <- do.call(prepare_weather_manifest, c(
    list(survey_data = input$svy, selected_surveys = input$ss,
         selected_weather = input$sw, dates = input$sim_dates,
         connection_params = input$cp, ssp = input$ssps,
         future_period = input$fp_list,
         perturbation_method = input$perturbation_method,
         stored_breaks = input$stored_breaks),
    list(prepared_weather_cache = "off", weather_fn = loader)
  ))
  first <- suppressWarnings(do.call(fct_run_simulation, c(
    input, list(weather_manifest = manifest, prepared_weather_cache = "off",
                weather_fn = function(...) stop("weather should not load"),
                progress_fn = function(...) invisible(NULL),
                pipeline_fn = phase4_pipeline)
  )))
  second_input <- input
  second_input$mf$fit3 <- stats::lm(welfare ~ temp + I(temp^2),
                                    data = second_input$mf$train_data)
  second <- suppressWarnings(do.call(fct_run_simulation, c(
    second_input, list(weather_manifest = manifest, prepared_weather_cache = "off",
                       weather_fn = function(...) stop("weather should not load"),
                       progress_fn = function(...) invisible(NULL),
                       pipeline_fn = phase4_pipeline)
  )))
  expect_identical(calls, 1L)
  expect_identical(first$n_keys, second$n_keys)
  expect_identical(first$hist_sim_result$weather_raw,
                   second$hist_sim_result$weather_raw)
})
