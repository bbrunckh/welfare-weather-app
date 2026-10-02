annual_channel_fixture <- function(engine = "rif", binned = FALSE, transform = "none",
                                   zero_policy = FALSE, interaction = TRUE) {
  n <- 12L
  base <- data.frame(welfare = seq(1, 6, length.out = n),
    temp = seq(10, 21), rain = rep(1:3, 4), x = rep(0:1, 6),
    loc_id = rep(1:2, 6), int_month = rep(c(1L, 6L), 6),
    code = "BFA", year = "2018", survname = "wave-a", weight = seq_len(n))
  if (binned) base$temp <- factor(rep(c("low", "mid", "high"), 4))
  policy <- base
  if (!zero_policy) {
    policy$x <- 1
    policy[[SP_TRANSFER_COL]] <- seq(0.5, 3, length.out = n)
  }
  terms <- c(if (binned) c("tempmid", "temphigh") else "temp", "rain", "x")
  if (interaction) terms <- c(terms, if (binned) c("tempmid:x", "x:temphigh") else "x:temp", "rain:x")
  if (engine == "rif") {
    grid <- expand.grid(model = 3L, term = terms, tau = c(0.1, 0.5, 0.9),
      stringsAsFactors = FALSE)
    grid$estimate <- seq_len(nrow(grid)) / 100
    model <- list(engine = "rif", rif_grid = grid, taus = c(0.1, 0.5, 0.9),
      train_data = if (transform == "log") transform(base, welfare = log(welfare)) else base,
      weather_terms = c("temp", "rain"))
  } else {
    train <- expand.grid(temp = if (binned) levels(base$temp) else c(10, 30), rain = c(1, 3), x = 0:1)
    if (binned) train$temp <- factor(train$temp, levels = levels(base$temp))
    y <- 2 + as.numeric(train$temp) / 10 + train$rain / 20 + train$x / 5 +
      if (interaction) as.numeric(train$temp) * train$x / 50 + train$rain * train$x / 100 else 0
    train$welfare <- if (transform == "log") exp(y) else y
    lhs <- if (transform == "log") "log(welfare)" else "welfare"
    rhs <- if (interaction) "(temp + rain) * x" else "temp + rain + x"
    model <- list(engine = "fixest", fit3 = lm(as.formula(paste(lhs, "~", rhs)), train),
      train_data = train, weather_terms = c("temp", "rain"))
  }
  ctx <- .build_decomposition_context(base, policy, model,
    list(name = "welfare", transform = transform, type = "numeric"),
    skip_coef = TRUE, run_identity = "annual-run")
  ids <- rep(c(3L, 1L, 12L, 5L, 3L), 2)
  weather <- base[ids, c("code", "year", "survname", "loc_id", "int_month", "temp", "rain")]
  years <- rep(c(2030L, 2031L), each = 5)
  weather$timestamp <- as.Date(sprintf("%d-%02d-01", years, weather$int_month))
  if (binned) weather$temp <- factor(rep(c("high", "low", "mid", "high", "low"), 2), levels = levels(base$temp)) else {
    weather$temp <- seq(0, 45, length.out = length(ids))
  }
  weather$rain <- rep(c(0, 2, 8, 1, 10), 2)
  pipe <- list(y_point = rep(0, length(ids)), svy_row_id = ids, sim_year = years,
    weight = base$weight[ids], id_vec = ids)
  pipe$weather_exposure <- c(list(status = "ok", table = weather,
    row_index = seq_along(ids), prediction_row_id = seq_along(ids)),
    pipe[c("svy_row_id", "sim_year", "weight", "id_vec")])
  list(base = base, policy = policy, model = model, context = ctx, pipeline = pipe)
}

metric_channel_fixture <- function(engine = "rif", transform = "none", residuals = "none",
                                   zero_policy = FALSE, binned = FALSE) {
  fx <- annual_channel_fixture(engine, binned, transform, zero_policy)
  prepared <- .prepare_policy_annual_channels(fx$context, "annual-run")
  pipe <- fx$pipeline
  pipe$y_point <- seq(1, 2, length.out = length(pipe$y_point))
  shared <- step2_shared_context(data.frame(id = seq_len(12), .resid = seq(-0.2, 0.2, length.out = 12)), "id")
  hist <- list(pipeline = pipe, so = fx$context$so, shared_context = shared, residuals = residuals)
  policy <- .apply_policy_annual_pipeline(pipe, prepared, "annual-run")$pipeline
  policy_hist <- hist; policy_hist$pipeline <- policy
  list(fx = fx, prepared = prepared, hist = hist, policy_hist = policy_hist)
}

test_that("metric states use canonical aggregates and reconcile for all metrics and engines", {
  for (engine in c("fixest", "rif")) for (trans in c("none", "log")) {
    fx <- metric_channel_fixture(engine, trans)
    for (method in names(.WISE_METRIC_REGISTRY)) {
      result <- .policy_metric_decomposition(fx$hist, fx$policy_hist, list(), list(),
        fx$prepared, method, 3, "none")
      expect_identical(result$status, "ok", info = paste(engine, trans, method, result$reason))
      actual <- aggregate_pipeline_per_year(fx$hist$pipeline, method, pov_line = 3,
        residuals = "none", is_log = trans == "log", skip_coef = TRUE,
        shared_context = fx$hist$shared_context)
      expect_equal(result$annual$baseline, vapply(actual, `[[`, numeric(1), "value"))
      actual_policy <- aggregate_pipeline_per_year(fx$policy_hist$pipeline, method, pov_line = 3,
        residuals = "none", is_log = trans == "log", skip_coef = TRUE,
        shared_context = fx$hist$shared_context)
      expect_equal(result$annual$policy, vapply(actual_policy, `[[`, numeric(1), "value"))
      expect_equal(result$annual$total, result$annual$main + result$annual$repositioning + result$annual$interaction)
      expect_equal(result$summary$total, result$summary$policy - result$summary$baseline)
    }
  }
})

test_that("one residual realization is reused across states and compact shared context resolves", {
  for (engine in c("fixest", "rif")) for (trans in c("none", "log")) {
    for (mode in c("none", "original", "normal", "resample")) {
      fx <- metric_channel_fixture(engine, trans, mode, zero_policy = TRUE)
      result <- .policy_metric_decomposition(fx$hist, fx$policy_hist, list(), list(),
        fx$prepared, "mean", requested_residuals = mode)
      expect_identical(result$status, "ok")
      expect_equal(result$annual$total, c(0, 0))
      expect_equal(result$annual$main, c(0, 0))
      expect_identical(unique(result$annual$effective_residuals), mode)
      canonical <- aggregate_pipeline_per_year(fx$hist$pipeline, "mean", residuals = mode,
        is_log = trans == "log", skip_coef = TRUE, shared_context = fx$hist$shared_context)
      expect_equal(result$annual$baseline, vapply(canonical, `[[`, numeric(1), "value"))
    }
  }
  fx <- metric_channel_fixture()
  fx$hist$shared_context <- fx$policy_hist$shared_context <- list(train_aug = data.frame(id = 1:12))
  for (mode in c("original", "normal", "resample")) {
    result <- .policy_metric_decomposition(fx$hist, fx$policy_hist, list(), list(), fx$prepared,
      "mean", requested_residuals = mode)
    expect_identical(result$status, "ok")
    expect_identical(result$metadata$effective_residuals, "none")
    for (skip_coef in c(TRUE, FALSE)) {
      canonical <- aggregate_pipeline_per_year(fx$hist$pipeline, "mean", residuals = mode,
        is_log = FALSE, skip_coef = skip_coef, shared_context = fx$hist$shared_context)
      expect_equal(result$annual$baseline, vapply(canonical, `[[`, numeric(1), "value"))
    }
  }
})

test_that("equal-model summaries and ordered quantile contrasts do not average component medians", {
  annual <- data.frame(model_id = rep(c("a", "b", "c"), c(50, 40, 20)),
    sim_year = c(1:50, 1:40, 1:20), baseline = c(1:50, 1:40, 1:20))
  annual$after_main <- annual$baseline + rep(c(1, 5, -2), c(50, 40, 20))
  annual$after_repositioning <- annual$after_main + rev(annual$baseline) / 10
  annual$policy <- annual$after_repositioning + sin(annual$sim_year) * 5
  annual <- .policy_metric_contributions(annual)
  summary <- .policy_metric_summary(annual, "future")
  expect_equal(summary$baseline, mean(c(mean(1:50), mean(1:40), mean(1:20))))
  expect_false(isTRUE(all.equal(summary$baseline, mean(annual$baseline))))
  for (tail in c("low", "high")) {
    result <- .policy_metric_tails(annual, "future", tail)
    valid <- result[result$status == "ok", ]
    expect_equal(valid$total, valid$main + valid$repositioning + valid$interaction)
    expect_equal(valid$total, valid$policy - valid$baseline)
    expect_true(all(result$status[result$return_period == 50] == "unavailable"))
    expect_true(all(result$scope == "equal_probability"))
    q <- valid[valid$scope == "equal_probability" & valid$return_period == 20, ]
    endpoint <- function(s) median(vapply(split(annual, annual$model_id), function(x) {
      rank_interp(sort(x[[s]]), if (tail == "high") .05 else .95)
    }, numeric(1)))
    expect_equal(q$total, endpoint("policy") - endpoint("baseline"))
  }
})

test_that("endpoint parity failures preserve independent Results summaries and never alter support", {
  fx <- metric_channel_fixture()
  aggregate <- function(hist) list(Historical = list(out = aggregate_pipeline_table(
    hist$pipeline, "mean", residuals = "none", is_log = FALSE, skip_coef = TRUE,
    model_ids = "Historical")))
  b <- aggregate(fx$hist); p <- aggregate(fx$policy_hist)
  run <- function(prepared = fx$prepared, policy = fx$policy_hist) .policy_metric_decomposition(
    fx$hist, policy, list(), list(), prepared, "mean", requested_residuals = "none",
    endpoint_series_baseline = b, endpoint_series_policy = p)
  expect_identical(run()$status, "ok")
  missing <- run(NULL)
  expect_identical(missing$status, "unavailable")
  expect_equal(missing$endpoint_summary$value, run()$summary$total)
  bad <- fx$policy_hist; bad$pipeline$y_point[1] <- bad$pipeline$y_point[1] + 1
  mismatch <- run(policy = bad)
  expect_identical(mismatch$status, "unavailable")
  expect_match(mismatch$reason, "Final cumulative state")
  expect_equal(mismatch$endpoint_summary, run()$endpoint_summary)
  bad <- fx$policy_hist; bad$pipeline$weight <- rev(bad$pipeline$weight)
  expect_identical(run(policy = bad)$status, "unavailable")
  b$Historical$out$value_all[[1L]] <- 999
  expect_match(run()$reason, "endpoint aggregate parity")
})

test_that("mechanisms remain model scale and preserve bins, ranks and missing engine channels", {
  for (engine in c("fixest", "rif")) {
    fx <- metric_channel_fixture(engine, binned = TRUE)
    result <- .policy_metric_decomposition(fx$hist, fx$policy_hist, list(), list(),
      fx$prepared, "headcount_ratio", 3, "none")
    expect_identical(result$status, "ok")
    expect_true(all(result$mechanisms$annual$scale == "model_scale"))
    expect_true(any(result$mechanisms$annual$contrast == "fitted_reference_category_contrast"))
    category_units <- result$mechanisms$annual$weather_units[
      result$mechanisms$annual$contrast == "fitted_reference_category_contrast"]
    expect_true(all(category_units == "category contrast; no per-unit slope"))
    if (engine == "fixest") {
      expect_true(all(is.na(result$mechanisms$annual$repositioning)))
      expect_identical(result$mechanisms$repositioning_status, "Not modeled by this engine")
    } else {
      expect_equal(result$mechanisms$fitted_curve, fx$prepared$context$rif_grid)
      expect_true(any(result$mechanisms$annual$tau_pre != result$mechanisms$annual$tau_post))
    }
  }
})

test_that("strict poverty crossings and changing inverse-welfare eligibility remain canonical", {
  fx <- metric_channel_fixture("fixest")
  pipe <- fx$hist$pipeline
  # Three equally weighted rows per year, first household crosses exactly to 3.
  keep <- c(1L, 2L, 3L, 6L, 7L, 8L)
  for (field in c("y_point", "svy_row_id", "sim_year", "id_vec")) pipe[[field]] <- pipe[[field]][keep]
  pipe$y_point <- rep(c(2, 3, 4), 2)
  pipe$weight <- NULL
  for (field in c("row_index", "prediction_row_id", "svy_row_id", "sim_year", "id_vec")) {
    pipe$weather_exposure[[field]] <- pipe$weather_exposure[[field]][keep]
  }
  pipe$weather_exposure$weight <- NULL
  values <- as.list(fx$prepared)
  values$delta_main <- rep(0, fx$prepared$context$n)
  values$delta_main[pipe$svy_row_id[1L]] <- 1
  for (v in names(values$products)) {
    values$products[[v]]$interaction[] <- 0
    values$products[[v]]$repositioning[] <- 0
  }
  prepared <- list2env(values, parent = emptyenv()); lockEnvironment(prepared, bindings = TRUE)
  fx$hist$pipeline <- pipe
  fx$policy_hist$pipeline <- .apply_policy_annual_pipeline(pipe, prepared, "annual-run")$pipeline
  result <- .policy_metric_decomposition(fx$hist, fx$policy_hist, list(), list(),
    prepared, "headcount_ratio", 3, "none")
  expect_identical(result$status, "ok")
  expect_equal(result$annual$baseline, rep(1 / 3, 2))
  expect_equal(result$annual$policy, c(0, 0))
  expect_equal(result$summary$main, -1 / 3)
  expect_match(format_metric_value(result$summary$main, result$metadata, change = TRUE), "33.33 pp")
  pipe$y_point <- rep(c(-0.5, 3, 4), 2)
  fx$hist$pipeline <- pipe
  fx$policy_hist$pipeline <- .apply_policy_annual_pipeline(pipe, prepared, "annual-run")$pipeline
  inverse <- .policy_metric_decomposition(fx$hist, fx$policy_hist, list(), list(),
    prepared, "avg_poverty", requested_residuals = "none")
  expect_identical(inverse$status, "ok")
  expect_equal(inverse$annual$excluded_baseline, c(1L, 1L))
  expect_equal(inverse$annual$excluded_policy, c(0L, 0L))
  expect_equal(inverse$summary$total, inverse$summary$main)
})

test_that("member exposures and dropped endpoint support are not confused across scenarios", {
  fx <- metric_channel_fixture()
  second <- fx$hist$pipeline
  second$weather_exposure$table$temp <- second$weather_exposure$table$temp * 2
  owners <- list(future = list(pipelines = list(a = fx$hist$pipeline, b = second),
    shared_context = fx$hist$shared_context, so = fx$hist$so))
  policy <- owners
  policy$future$pipelines <- lapply(owners$future$pipelines, function(x) {
    .apply_policy_annual_pipeline(x, fx$prepared, "annual-run")$pipeline
  })
  endpoint <- function(owner) list(future = list(out = aggregate_pipeline_table(
    owner$future$pipelines, "mean", residuals = "none", is_log = FALSE,
    skip_coef = TRUE, model_ids = c("a", "b"))))
  b <- endpoint(owners); p <- endpoint(policy)
  run <- function() .policy_metric_decomposition(fx$hist, fx$policy_hist, owners, policy,
    fx$prepared, "mean", requested_residuals = "none", endpoint_series_baseline = b,
    endpoint_series_policy = p)
  actual <- run()
  expect_identical(actual$status, "ok")
  expect_identical(actual$metadata$focus_scenario, "future")
  expect_false(isTRUE(all.equal(actual$annual$total[actual$annual$member == "a"],
    actual$annual$total[actual$annual$member == "b"])))
  expect_equal(actual$summary$total[actual$summary$scenario == "future"], actual$endpoint_summary$value)
  p$future$out$value_all[[1L]][2L] <- NA_real_
  actual <- run()
  expect_identical(actual$status, "ok")
  expect_equal(actual$summary$n_dropped_model_years[actual$summary$scenario == "future"], 1L)
  expect_equal(actual$summary$total[actual$summary$scenario == "future"], actual$endpoint_summary$value)
})

test_that("tail attribution is withheld when matched support differs from Results thresholds", {
  fx <- metric_channel_fixture()
  pipe <- fx$hist$pipeline
  # Retain exact row anchors but expand their valid calendar years to 20 years.
  repeat_rows <- rep(seq_len(5), 20)
  for (field in c("y_point", "svy_row_id", "weight", "id_vec")) pipe[[field]] <- pipe[[field]][repeat_rows]
  pipe$sim_year <- rep(2030:2049, each = 5)
  pipe$y_point <- rep(seq(1, 20), each = 5)
  weather <- pipe$weather_exposure$table[repeat_rows, ]
  weather$timestamp <- as.Date(sprintf("%d-%02d-01", pipe$sim_year, weather$int_month))
  pipe$weather_exposure <- c(list(status = "ok", table = weather,
    row_index = seq_along(repeat_rows), prediction_row_id = seq_along(repeat_rows)),
    pipe[c("svy_row_id", "sim_year", "weight", "id_vec")])
  fx$hist$pipeline <- pipe
  fx$policy_hist$pipeline <- .apply_policy_annual_pipeline(pipe, fx$prepared, "annual-run")$pipeline
  agg <- function(hist) list(Historical = list(out = aggregate_pipeline_table(hist$pipeline,
    "mean", residuals = "none", is_log = FALSE, skip_coef = TRUE, model_ids = "Historical")))
  b <- agg(fx$hist); p <- agg(fx$policy_hist)
  # Missing policy year drops that year from paired expected support, not baseline thresholds.
  p$Historical$out$value_all[[1]][1] <- NA_real_
  result <- .policy_metric_decomposition(fx$hist, fx$policy_hist, list(), list(), fx$prepared,
    "mean", requested_residuals = "none", endpoint_series_baseline = b, endpoint_series_policy = p)
  expect_identical(result$status, "ok")
  expect_equal(result$summary$n_model_years, 19L)
  expected <- result$return_period[result$return_period$scope == "equal_probability" &
    result$return_period$return_period == 5, ]
  expect_identical(expected$status, "unavailable")
  expect_match(expected$reason, "marginal threshold endpoints")
  expect_true(is.na(expected$total))
  expect_true(all(result$return_period$scope == "equal_probability"))
})

test_that("the real shared Results calculation follows edits without re-preparing or predicting", {
  fx <- metric_channel_fixture("fixest")
  fx$hist$residuals <- "none"
  fx$policy_hist$residuals <- "none"
  local_mocked_bindings(
    .prepare_policy_annual_channels = function(...) stop("channel preparation forbidden"),
    run_sim_pipeline = function(...) stop("prediction rerun forbidden"),
    predict_outcome = function(...) stop("prediction rerun forbidden"),
    .package = "wiseapp")
  # Published technical context may be a finalized clone of the prepared source.
  technical <- list2env(as.list(fx$prepared$context), parent = emptyenv())
  lockEnvironment(technical, bindings = TRUE)
  api <- NULL
  shiny::testServer(function(input, output, session) {
    api <<- .wire_results_pane(input, output, session,
      shiny::reactive(fx$hist), shiny::reactive(list()),
      shiny::reactive(fx$policy_hist), shiny::reactive(list()),
      selected_hist = shiny::reactive(NULL), residuals = shiny::reactive("normal"),
      decomp_context = shiny::reactive(technical), annual_channels = shiny::reactive(fx$prepared))
  }, {
    session$setInputs(cmp_agg_method = "mean")
    result <- api$metric_decomposition()
    expect_identical(result$status, "ok")
    expect_equal(result$summary$total, api$headline_paired_effect_summary()$value)
    expect_identical(result$metadata$requested_residuals, "none")
    session$setInputs(cmp_agg_method = "headcount_ratio", cmp_pov_line = 3)
    session$elapse(500); session$flushReact()
    poverty <- api$metric_decomposition()
    expect_identical(poverty$status, "ok")
    expect_equal(poverty$summary$total, api$headline_paired_effect_summary()$value)
    session$setInputs(cmp_pov_line = 5)
    session$elapse(500); session$flushReact()
    edited <- api$metric_decomposition()
    expect_identical(edited$status, "ok")
    expect_equal(edited$metadata$threshold_value, 5)
    expect_equal(edited$summary$total, api$headline_paired_effect_summary()$value)
    expect_equal(edited$mechanisms$summary, poverty$mechanisms$summary)
  })
})

test_that("annual continuous and category channels equal explicit reference row by row", {
  for (engine in c("fixest", "rif")) for (binned in c(FALSE, TRUE)) for (trans in c("none", "log")) {
    fx <- annual_channel_fixture(engine, binned, trans)
    p <- .prepare_policy_annual_channels(fx$context, "annual-run")
    expect_true(environmentIsLocked(p))
    actual <- .policy_annual_channels(fx$pipeline, p, "annual-run")
    reference <- .policy_annual_channels_reference(fx$pipeline, fx$context, "annual-run")
    for (field in grep("^delta_", names(reference), value = TRUE)) {
      expect_equal(actual[[field]], reference[[field]], tolerance = 1e-12)
    }
    expect_equal(actual$delta_total, actual$delta_main + actual$delta_res1 + actual$delta_res2)
    expect_identical(actual$repositioning_modeled, engine == "rif")
    expect_true(actual$interaction_included)
    expect_false(any(grepl("^sd_", names(actual))))
    block <- .policy_annual_channels(fx$pipeline, p, "annual-run", rows = c(10L, 1L, 3L))
    expect_equal(block$delta_total, actual$delta_total[c(10L, 1L, 3L)])
    expect_identical(fx$pipeline$y_point, rep(0, 10))
  }
})

test_that("fixed comparable exposure reproduces legacy correction and zero policy is zero", {
  for (engine in c("fixest", "rif")) {
    fx <- annual_channel_fixture(engine)
    pipe <- fx$pipeline
    pipe$weather_exposure$table$temp <- 20
    pipe$weather_exposure$table$rain <- 2
    actual <- .policy_annual_channels(pipe,
      .prepare_policy_annual_channels(fx$context, "annual-run"), "annual-run")
    old <- .policy_central_delta_run(fx$context, "annual-run", data.frame(temp = 20, rain = 2))
    expect_equal(actual$delta_total, old[pipe$svy_row_id], tolerance = 1e-12)
    zero <- annual_channel_fixture(engine, zero_policy = TRUE)
    out <- .policy_annual_channels(zero$pipeline,
      .prepare_policy_annual_channels(zero$context, "annual-run"), "annual-run")
    expect_equal(out$delta_total, rep(0, 10))
    expect_equal(out$delta_res1, rep(0, 10))
    expect_equal(out$delta_res2, rep(0, 10))
  }
})

test_that("same household ranks stay fixed while annual hazard protection varies", {
  fx <- annual_channel_fixture()
  p <- .prepare_policy_annual_channels(fx$context, "annual-run")
  out <- .policy_annual_channels(fx$pipeline, p, "annual-run")
  expect_equal(out$delta_main[1], out$delta_main[6])
  expect_false(isTRUE(all.equal(out$delta_res1[1], out$delta_res1[6])))
  expect_identical(p$tau_i_pre, fx$context$tau_i_pre)
  expect_identical(p$tau_i_post, fx$context$tau_i_post)
  expect_equal(out$delta_res1[1], 0) # zero hazards, not absent sensitivity
  expect_true(any(p$products$temp$repositioning != 0))
})

test_that("annual adapter rejects missing, stale, mismatched and invalid exposure context", {
  fx <- annual_channel_fixture(binned = TRUE)
  p <- .prepare_policy_annual_channels(fx$context, "annual-run")
  run <- function(pipe) .policy_annual_channels(pipe, p, "annual-run")
  expect_error(.prepare_policy_annual_channels(fx$context, "stale"), "run identity mismatch")
  expect_error(.policy_annual_channels(fx$pipeline, p, "stale"), "run identity mismatch")
  bad <- fx$pipeline; bad$weather_exposure <- NULL
  expect_error(run(bad), "mapping unavailable")
  for (field in c("svy_row_id", "sim_year", "weight", "id_vec")) {
    bad <- fx$pipeline; bad[[field]] <- rev(bad[[field]])
    expect_error(run(bad), "ordering mismatch")
  }
  bad <- fx$pipeline; bad$weather_exposure$table$temp <- as.character(bad$weather_exposure$table$temp)
  expect_error(run(bad), "invalid weather category")
  bad <- fx$pipeline; bad$weather_exposure$table$rain[1] <- NA
  expect_error(run(bad), "Missing exact weather")
  bad <- fx$pipeline; bad$weather_exposure$table$survname[1] <- "wrong-wave"
  expect_error(run(bad), "survey identity mismatch")
  bad <- fx$pipeline; bad$weather_exposure$table$timestamp[1] <- as.Date("2000-01-01")
  expect_error(run(bad), "anchor does not match")
  bad <- fx$pipeline; bad$weather_exposure$prediction_row_id[2] <- 1L
  expect_error(run(bad), "Invalid prediction-row")
  bad <- fx$pipeline; bad$svy_row_id[1] <- 999L; bad$weather_exposure$svy_row_id <- bad$svy_row_id
  expect_error(run(bad), "Invalid prediction-row")
  expect_error(.policy_annual_channels(fx$pipeline, p, "annual-run", c(1, 1)), "row selection")
  values <- as.list(fx$context)
  values$svy_baseline$survname <- NULL
  ctx <- list2env(values, parent = emptyenv()); lockEnvironment(ctx, bindings = TRUE)
  expect_error(.policy_annual_channels(fx$pipeline,
    .prepare_policy_annual_channels(ctx, "annual-run"), "annual-run"), "Survey join identity unavailable")
})

test_that("unsupported methods and missing model channels are explicit", {
  for (engine in c("ranger", "xgboost")) {
    expect_identical(.policy_annual_channel_status(list(engine = engine, so = list()))$status, "unsupported")
  }
  fx <- annual_channel_fixture()
  values <- as.list(fx$context)
  values$model_type <- "logistic"
  ctx <- list2env(values, parent = emptyenv()); lockEnvironment(ctx, bindings = TRUE)
  expect_identical(.prepare_policy_annual_channels(ctx, "annual-run")$status, "unsupported")
  values$model_type <- NULL; values$so$type <- "binary"
  ctx <- list2env(values, parent = emptyenv()); lockEnvironment(ctx, bindings = TRUE)
  expect_identical(.prepare_policy_annual_channels(ctx, "annual-run")$status, "unsupported")
  fx <- annual_channel_fixture("fixest", interaction = FALSE)
  p <- .prepare_policy_annual_channels(fx$context, "annual-run")
  out <- .policy_annual_channels(fx$pipeline, p, "annual-run")
  expect_false(out$interaction_included)
  expect_false(out$repositioning_modeled)
  expect_equal(out$delta_res2, rep(0, 10))
})

test_that("flat RIF weather curves and reference categories preserve mechanism semantics", {
  fx <- annual_channel_fixture(binned = TRUE)
  model <- fx$model
  model$rif_grid$estimate[model$rif_grid$term %in% c("tempmid", "temphigh", "rain")] <- 0.1
  ctx <- .build_decomposition_context(fx$base, fx$policy, model,
    list(name = "welfare", transform = "none"), skip_coef = TRUE,
    run_identity = "annual-run")
  p <- .prepare_policy_annual_channels(ctx, "annual-run")
  out <- .policy_annual_channels(fx$pipeline, p, "annual-run")
  expect_true(any(p$tau_i_pre != p$tau_i_post))
  expect_equal(out$delta_res1, rep(0, 10))
  expect_equal(p$products$temp$interaction[, match("low", p$products$temp$categories)], rep(0, ctx$n))
  expect_equal(out$delta_total,
    .policy_annual_channels_reference(fx$pipeline, ctx, "annual-run")$delta_total)
})

test_that("central preparation does not request coefficient covariance", {
  fx <- annual_channel_fixture("fixest")
  local_mocked_bindings(vcov = function(...) stop("SE work forbidden"), .package = "stats")
  ctx <- .build_decomposition_context(fx$base, fx$policy, fx$model,
    list(name = "welfare", transform = "none"), skip_coef = TRUE,
    run_identity = "annual-run")
  expect_identical(.prepare_policy_annual_channels(ctx, "annual-run")$status, "ok")
})

test_that("production corrections and compact member summaries share annual channel blocks", {
  for (engine in c("fixest", "rif")) for (binned in c(FALSE, TRUE)) for (trans in c("none", "log")) {
    fx <- annual_channel_fixture(engine, binned, trans)
    p <- fx$pipeline
    p$y_point <- seq(1, 2, length.out = length(p$y_point))
    hist <- list(pipeline = p, shared_context = list(train_aug = fx$base), residuals = "original")
    second <- p
    if (!binned) second$weather_exposure$table$temp <- second$weather_exposure$table$temp * 2
    scenarios <- list(future = list(pipelines = list(a = p, b = second), year_range = c(2030L, 2031L)))
    run <- function(chunk_size) apply_policy_delta_to_baseline(
      fx$base, fx$policy, fx$model, fx$context$so, hist, scenarios,
      decomp_context = fx$context, run_identity = "annual-run", chunk_size = chunk_size
    )
    out <- run(3L)
    one_block <- run(100L)
    reference <- .policy_annual_channels_reference(p, fx$context, "annual-run")
    expect_equal(out$hist_sim$pipeline$y_point, p$y_point + reference$delta_total)
    expect_identical(out$hist_sim$pipeline[names(p)[names(p) != "y_point"]], p[names(p)[names(p) != "y_point"]])
    expect_identical(out$hist_sim$shared_context, hist$shared_context)
    expect_identical(out$correction_version, "row_aligned_annual_v1")
    expect_equal(out$decomp_scenarios, one_block$decomp_scenarios, tolerance = 1e-12)
    tbl <- out$decomp_scenarios$channel_summary
    expect_equal(nrow(tbl), 4L)
    expect_setequal(tbl$member, c("a", "b"))
    for (member in c("a", "b")) {
      pipe <- scenarios$future$pipelines[[member]]
      ch <- .policy_annual_channels(pipe, out$annual_channels, "annual-run")
      expect_equal(out$saved_scenarios$future$pipelines[[member]]$y_point, pipe$y_point + ch$delta_total)
      for (yr in unique(pipe$sim_year)) {
        idx <- which(pipe$sim_year == yr)
        row <- tbl[tbl$member == member & tbl$sim_year == yr, ]
        for (field in c("delta_main", "delta_res1", "delta_res2", "delta_total")) {
          expect_equal(row[[paste0("sum_", field)]] / row[[paste0("weight_", field)]],
            weighted.mean(ch[[field]][idx], pipe$weight[idx]))
        }
      }
    }
    expect_identical(p$y_point, seq(1, 2, length.out = length(p$y_point)))
  }
})

test_that("production validates once per pipeline and fails closed without prediction or SE work", {
  fx <- annual_channel_fixture()
  hist <- list(pipeline = fx$pipeline)
  calls <- 0L
  validate <- .validate_policy_annual_exposure
  local_mocked_bindings(
    .validate_policy_annual_exposure = function(...) { calls <<- calls + 1L; validate(...) },
    .policy_central_delta = function(...) stop("Period mean forbidden"),
    run_sim_pipeline = function(...) stop("Prediction forbidden"),
    .compact_run_future_decomposition = function(...) stop("Duplicate channels forbidden")
  )
  out <- apply_policy_delta_to_baseline(fx$base, fx$policy, fx$model, fx$context$so,
    hist, decomp_context = fx$context, run_identity = "annual-run", chunk_size = 1L)
  expect_equal(calls, 1L)
  bad <- fx$pipeline
  bad$weather_exposure$table$rain[1] <- NA_real_
  expect_error(apply_policy_delta_to_baseline(fx$base, fx$policy, fx$model, fx$context$so,
    hist, list(good = list(pipelines = list(a = fx$pipeline)), bad = list(pipelines = list(b = bad))),
    decomp_context = fx$context, run_identity = "annual-run"), "Missing exact weather")
  expect_identical(out$hist_sim$pipeline$policy_correction$run_identity, "annual-run")
  expect_error(apply_policy_delta_to_baseline(fx$base, fx$policy, fx$model, fx$context$so,
    hist, decomp_context = fx$context, run_identity = "stale"), "run identity mismatch")
  for (engine in c("fixest", "rif")) {
    zero <- annual_channel_fixture(engine, zero_policy = TRUE)
    out <- apply_policy_delta_to_baseline(zero$base, zero$policy, zero$model, zero$context$so,
      list(pipeline = zero$pipeline), decomp_context = zero$context, run_identity = "annual-run")
    expect_identical(out$hist_sim$pipeline$y_point, zero$pipeline$y_point)
  }
})

test_that("technical adverse deciles use the selected member-year support", {
  fx <- annual_channel_fixture()
  other <- fx$pipeline
  other$weather_exposure$table$temp <- other$weather_exposure$table$temp * 10
  out <- apply_policy_delta_to_baseline(fx$base, fx$policy, fx$model, fx$context$so,
    list(pipeline = fx$pipeline),
    list(future = list(pipelines = list(a = fx$pipeline, b = other))),
    decomp_context = fx$context, run_identity = "annual-run")
  compact <- out$decomp_scenarios
  selected <- .compact_future_year(compact, "future", "adverse_20", fx$context$so)
  expect_equal(nrow(selected), 1L)
  same <- compact
  keep <- same$decile_summary$member == selected$member & same$decile_summary$sim_year == selected$sim_year
  same$decile_summary <- same$decile_summary[keep, , drop = FALSE]
  expect_equal(.compact_future_decile_summary(compact, "future", "adverse_20", fx$context$so),
    .compact_future_decile_summary(same, "future", "mean", fx$context$so))
})
