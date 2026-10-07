library(testthat)


.policy_test_attach_exposure <- function(pipeline, baseline, weather_values) {
  ids <- pipeline$svy_row_id
  keys <- c("code", "year", "survname", "loc_id", "int_month")
  table <- baseline[ids, keys, drop = FALSE]
  for (name in names(weather_values)) table[[name]] <- weather_values[[name]]
  table$timestamp <- as.Date(sprintf(
    "%d-%02d-01", pipeline$sim_year, table$int_month
  ))
  pipeline$weather_exposure <- c(
    list(status = "ok", table = table, row_index = seq_along(ids),
         prediction_row_id = seq_along(ids)),
    pipeline[c("svy_row_id", "sim_year", "weight", "id_vec")]
  )
  pipeline
}


.policy_test_context <- function(baseline, policy, model_fit, so, run_identity) {
  .build_decomposition_context(
    baseline, policy, model_fit, so, skip_coef = TRUE,
    run_identity = run_identity
  )
}


expect_central_parity <- function(svy_baseline, svy_policy, model_fit, so,
                                  weather_raw = NULL, deltas = NULL,
                                  F_hat = NULL) {
  full <- decompose_policy_effect(
    svy_baseline, svy_policy, model_fit, so,
    weather_raw = weather_raw, deltas = deltas, F_hat = F_hat
  )
  central <- .policy_central_delta(
    svy_baseline, svy_policy, model_fit, so,
    weather_raw = weather_raw, deltas = deltas, F_hat = F_hat
  )
  expect_identical(central, full$delta_total)
  invisible(central)
}


test_that("central kernel matches OLS and ignores uncertainty mask metadata", {
  set.seed(11)
  n <- 120L
  baseline <- data.frame(
    welfare = exp(rnorm(n, log(4), 0.2)),
    temp = rnorm(n, 25, 2),
    electricity = rbinom(n, 1, 0.4),
    weight = runif(n, 0.5, 2)
  )
  policy <- baseline
  policy$electricity <- 1L
  policy[[SP_TRANSFER_COL]] <- seq(0, 0.5, length.out = n)
  fit <- lm(log(welfare) ~ temp * electricity, data = baseline)
  model_fit <- list(
    engine = "fixest", fit3 = fit, weather_terms = "temp",
    train_data = baseline,
    chol_obj = structure(list(), active_mask = c(temp = TRUE))
  )
  so <- list(name = "welfare", transform = "log")
  weather <- data.frame(temp = c(22, 28, 30))

  delta <- expect_central_parity(
    baseline, policy, model_fit, so, weather_raw = weather
  )
  expect_type(delta, "double")
  expect_length(delta, n)
  expect_identical(names(delta), NULL)
})

test_that("hazard location grouping is reused without changing numeric or factor results", {
  svy <- data.frame(
    loc_id = c(1, 1, 2, 3, NA),
    temp = c(10, 20, 30, 40, 50),
    rain = c(1, 2, 3, 4, 5),
    temp_bin = factor(c("low", "high", "mid", "low", "high"),
                      levels = c("low", "mid", "high"))
  )
  weather <- data.frame(
    loc_id = c(1, 1, 2, 3, NA),
    temp = c(12, 18, 35, 41, NA),
    rain = c(2, 4, 6, 8, 10),
    temp_bin = factor(c("low", "high", "mid", "low", NA),
                      levels = c("low", "mid", "high"))
  )
  ok <- !is.na(weather$loc_id)
  location_group <- collapse::GRP(
    data.frame(loc_id = weather$loc_id[ok]), by = "loc_id"
  )
  expected <- .compute_hazard_values(svy, weather,
                                     c("temp", "rain", "temp_bin"))
  actual <- .compute_hazard_values(svy, weather,
                                   c("temp", "rain", "temp_bin"),
                                   location_group = location_group)

  expect_equal(actual, expected)
  expect_identical(actual$temp_bin, expected$temp_bin)
})

test_that("RIF curve indexing preserves interpolation, endpoints, and missing SEs", {
  grid <- data.frame(
    term = rep(c("temp", "rain", "missing_se"), each = 3L),
    tau = rep(c(0.1, 0.5, 0.9), 3L),
    estimate = c(-2, 0, 2, 1, 3, 5, 4, 6, 8),
    stringsAsFactors = FALSE
  )
  index <- .build_rif_curve_index(grid)
  expect_named(index, c("missing_se", "rain", "temp"))
  expect_equal(stats::approx(index$temp$tau, index$temp$estimate,
                              xout = c(0.1, 0.3, 0.9), rule = 2)$y,
               c(-2, -1, 2))
  expect_null(index$temp$std.error)
  expect_equal(index$rain$tau, c(0.1, 0.5, 0.9))
})



test_that("policy correction changes only y_point and preserves pipeline types", {
  set.seed(111)
  n <- 60L
  baseline <- data.frame(
    hhid = seq_len(n), welfare = exp(rnorm(n)), temp = rnorm(n, 25),
    electricity = rep(0:1, length.out = n), code = "BFA", year = "2018",
    survname = "wave-a", loc_id = seq_len(n), int_month = 1L
  )
  policy <- baseline
  policy$electricity <- 1L
  fit <- lm(log(welfare) ~ temp * electricity, data = baseline)
  model_fit <- list(
    engine = "fixest", fit3 = fit, weather_terms = "temp",
    train_data = baseline
  )
  pipe <- list(
    y_point = unname(predict(fit)),
    F_loading = matrix(seq_len(n * 2L), ncol = 2L),
    train_aug = transform(baseline, .resid = residuals(fit)),
    id_vec = baseline$hhid,
    id_col = "hhid",
    svy_row_id = seq_len(n),
    sim_year = rep(2030L, n),
    weight = seq(0.5, 1.5, length.out = n),
    weather_raw = data.frame(temp = c(22, 28))
  )
  pipe <- .policy_test_attach_exposure(
    pipe, baseline, list(temp = rep(c(22, 28), length.out = n))
  )
  hist <- list(pipeline = pipe, weather_raw = pipe$weather_raw)
  run_identity <- "central-kernel-pipeline"
  context <- .policy_test_context(baseline, policy, model_fit,
                                  list(name = "welfare", transform = "log"),
                                  run_identity)

  out <- apply_policy_delta_to_baseline(
    baseline, policy, model_fit,
    list(name = "welfare", transform = "log"), hist,
    decomp_context = context, run_identity = run_identity
  )$hist_sim$pipeline

  expect_identical(setdiff(names(out), names(pipe)), "policy_correction")
  expect_identical(out[intersect(names(pipe), setdiff(names(out), "y_point"))],
                   pipe[setdiff(names(pipe), "y_point")])
  expect_identical(out$policy_correction$version, "row_aligned_annual_v1")
  expect_type(out$y_point, typeof(pipe$y_point))
  expect_identical(attributes(out$y_point), attributes(pipe$y_point))
  expect_false(identical(out$y_point, pipe$y_point))
})


test_that("production applies exact annual exposure instead of period-mean broadcast", {
  baseline <- data.frame(
    welfare = exp(c(1, 1)), loc_id = 1:2, temp = c(10, 20),
    electricity = c(0, 0), code = "BFA", year = "2018",
    survname = "wave-a", int_month = 1L
  )
  train <- expand.grid(temp = c(10, 20), electricity = 0:1)
  train$welfare <- exp(
    1 + 0.1 * train$temp + 0.2 * train$electricity +
      0.05 * train$temp * train$electricity
  )
  model_fit <- list(
    engine = "fixest",
    fit3 = lm(log(welfare) ~ temp * electricity, data = train),
    weather_terms = "temp",
    train_data = train
  )
  policy <- baseline
  policy$electricity <- 1L
  so <- list(name = "welfare", transform = "log")
  weather <- data.frame(
    loc_id = c(1L, 2L, 1L, 2L),
    sim_year = c(2030L, 2030L, 2031L, 2031L),
    temp = c(10, 20, 30, 40)
  )
  pipeline <- list(
    y_point = rep(0, 4L),
    svy_row_id = c(1L, 2L, 1L, 2L),
    sim_year = c(2030L, 2030L, 2031L, 2031L),
    weight = NULL, id_vec = c(1L, 2L, 1L, 2L)
  )
  pipeline <- .policy_test_attach_exposure(
    pipeline, baseline, list(temp = weather$temp)
  )
  run_identity <- "annual-production-kernel"
  context <- .policy_test_context(baseline, policy, model_fit, so, run_identity)
  annual_channels <- .prepare_policy_annual_channels(context, run_identity)

  production <- apply_policy_delta_to_baseline(
    baseline, policy, model_fit, so,
    list(pipeline = pipeline, weather_raw = weather),
    decomp_context = context, run_identity = run_identity,
    annual_channels = annual_channels, chunk_size = 1L
  )
  expect_identical(
    names(production),
    c("hist_sim", "saved_scenarios", "annual_channels",
      "decomp_scenarios", "correction_version", "n_na_untreated")
  )
  expect_identical(production$correction_version, "row_aligned_annual_v1")
  production_y <- production$hist_sim$pipeline$y_point
  period_mean_delta <- .policy_central_delta(
    baseline, policy, model_fit, so, weather_raw = weather
  )
  explicit_annual_delta <- c(
    .policy_central_delta(
      baseline, policy, model_fit, so,
      weather_raw = weather[weather$sim_year == 2030L, , drop = FALSE]
    ),
    .policy_central_delta(
      baseline, policy, model_fit, so,
      weather_raw = weather[weather$sim_year == 2031L, , drop = FALSE]
    )
  )

  expect_equal(production_y, explicit_annual_delta, tolerance = 1e-10)
  expect_equal(explicit_annual_delta, c(0.7, 1.2, 1.7, 2.2), tolerance = 1e-10)
  expect_equal(period_mean_delta[c(1L, 2L, 1L, 2L)],
               c(1.2, 1.7, 1.2, 1.7), tolerance = 1e-10)
  expect_false(isTRUE(all.equal(production_y,
                                period_mean_delta[c(1L, 2L, 1L, 2L)])))

  unmapped <- pipeline
  unmapped$weather_exposure <- NULL
  expect_error(
    apply_policy_delta_to_baseline(
      baseline, policy, model_fit, so,
      list(pipeline = unmapped, weather_raw = weather),
      decomp_context = context, run_identity = run_identity,
      annual_channels = annual_channels
    ),
    "Exact prediction-row weather exposure mapping unavailable"
  )
  expect_identical(pipeline$y_point, rep(0, 4L))
})


test_that("deployed fixest path resolves weather references and shared IDs", {
  skip_if_not_installed("fixest")
  root <- withr::local_tempdir()
  store <- step2_weather_store_create("policy-path", "sig-policy", root)
  on.exit(step2_weather_store_cleanup(store), add = TRUE)

  set.seed(211)
  n <- 120L
  baseline <- data.frame(
    household_key = sprintf("hh-%03d", seq_len(n)),
    welfare = exp(rnorm(n, log(4), 0.2)),
    loc_id = rep(1:4, length.out = n),
    temp = rnorm(n, 25, 2),
    electricity = rep(0:1, length.out = n),
    weight = runif(n, 0.5, 2), code = "BFA", year = "2018",
    survname = "wave-a", int_month = 6L
  )
  fit <- fixest::feols(
    log(welfare) ~ temp * electricity, data = baseline,
    weights = ~weight
  )
  policy <- baseline
  policy$electricity <- 1L
  model_fit <- list(
    engine = "fixest", fit3 = fit, weather_terms = "temp",
    train_data = baseline
  )
  so <- list(name = "welfare", type = "numeric", transform = "log")

  hist_weather <- data.frame(loc_id = 1:4, temp = c(20, 22, 24, 26))
  future_weather <- data.frame(
    loc_id = rep(1:4, 2), temp = rep(c(28, 30, 32, 34), 2),
    timestamp = as.Date(rep(c("2030-06-01", "2031-06-01"), each = 4))
  )
  hist_ref <- step2_weather_store_put(store, "historical", hist_weather)
  future_ref <- step2_weather_store_put(store, "future-member", future_weather)

  order <- sample(seq_len(n))
  make_pipe <- function(weather_ref, weather_values) {
    id <- order
    pipe <- list(
      y_point = unname(stats::predict(fit))[id],
      F_loading = matrix(rnorm(n * 2L), ncol = 2L),
      id_vec = baseline$household_key[id], id_col = "household_key",
      svy_row_id = id, sim_year = rep(2030L, n),
      weight = baseline$weight[id], weather_raw = weather_ref
    )
    table <- baseline[id, c("code", "year", "survname", "loc_id", "int_month")]
    table$temp <- weather_values[match(table$loc_id, c(1:4))]
    table$timestamp <- as.Date(rep("2030-06-01", n))
    pipe$weather_exposure <- c(
      list(status = "ok", table = table, row_index = seq_len(n),
           prediction_row_id = seq_len(n)),
      pipe[c("svy_row_id", "sim_year", "weight", "id_vec")]
    )
    pipe
  }
  hist_pipe <- make_pipe(hist_ref, hist_weather$temp)
  future_pipe <- make_pipe(future_ref, future_weather$temp[1:4])
  hist <- list(
    pipeline = hist_pipe, weather_raw = hist_ref,
    weather_signature = "sig-policy",
    shared_context = list(id_col = "household_key",
                          train_aug = baseline, residuals = "none"),
    so = so, residuals = "none", S = 40L
  )
  scenarios <- list("SSP2-4.5 / 2030-2040" = list(
    pipelines = list(member_a = future_pipe),
    weather_raw = future_ref,
    weather_signature = "sig-policy",
    shared_context = hist$shared_context,
    so = so,
    year_range = c(2030L, 2040L)
  ))

  run_identity <- "deployed-fixest-path"
  context <- .policy_test_context(baseline, policy, model_fit, so, run_identity)
  annual_channels <- .prepare_policy_annual_channels(context, run_identity)
  out <- apply_policy_delta_to_baseline(
    baseline, policy, model_fit, so, hist, scenarios,
    decomp_context = context, run_identity = run_identity,
    annual_channels = annual_channels
  )
  expect_equal(
    out$hist_sim$pipeline$y_point,
    hist_pipe$y_point + .policy_annual_channels(
      hist_pipe, annual_channels, run_identity
    )$delta_total
  )
  expect_equal(
    out$saved_scenarios[[1]]$pipelines[[1]]$y_point,
    future_pipe$y_point + .policy_annual_channels(
      future_pipe, annual_channels, run_identity
    )$delta_total
  )
  expect_identical(out$hist_sim$pipeline$weather_raw, hist_ref)
  expect_identical(out$saved_scenarios[[1]]$pipelines[[1]]$weather_raw,
                   future_ref)
  expect_identical(out$hist_sim$shared_context, hist$shared_context)
  expect_identical(out$hist_sim$pipeline$F_loading, hist_pipe$F_loading)
  expect_identical(
    out$saved_scenarios[[1]]$pipelines[[1]]$F_loading,
    future_pipe$F_loading
  )
  expect_identical(out$correction_version, "row_aligned_annual_v1")

  baseline_agg <- aggregate_pipeline_per_year(
    hist_pipe, method = "mean", weighted = TRUE,
    residuals = "none", is_log = TRUE,
    shared_context = hist$shared_context
  )
  policy_agg <- aggregate_pipeline_per_year(
    out$hist_sim$pipeline, method = "mean", weighted = TRUE,
    residuals = "none", is_log = TRUE,
    shared_context = out$hist_sim$shared_context
  )
  expect_identical(names(policy_agg), names(baseline_agg))
  expect_true(all(vapply(policy_agg, function(x) {
    all(c("value", "value_lo", "value_p50", "value_hi",
          "var_coef", "var_resid", "F_agg") %in% names(x))
  }, logical(1))))
  expect_true(all(vapply(policy_agg, function(x) {
    all(is.finite(unlist(x[c("value", "value_lo", "value_p50", "value_hi",
                             "var_coef", "var_resid")])))
  }, logical(1))))
})


test_that("central kernel matches the current logistic analytic path", {
  set.seed(12)
  n <- 180L
  baseline <- data.frame(
    welfare = rbinom(n, 1, 0.45),
    temp = rnorm(n, 24, 2),
    internet = rbinom(n, 1, 0.35)
  )
  fit <- fixest::feglm(
    welfare ~ temp * internet, data = baseline,
    family = stats::binomial("logit")
  )
  policy <- baseline
  policy$internet <- 1L
  model_fit <- list(
    engine = "fixest", model_type = "logistic", fit3 = fit,
    weather_terms = "temp", train_data = baseline
  )

  expect_central_parity(
    baseline, policy, model_fit,
    list(name = "welfare", transform = "none")
  )
  so <- list(name = "welfare", type = "numeric", transform = "none")
  ctx <- .build_decomposition_context(baseline, policy, model_fit, so)
  expect_identical(ctx$model_type, "logistic")
  expect_identical(.policy_endpoint_status(so, ctx)$status, "unsupported")
  model_fit$model_type <- NULL
  family_ctx <- .build_decomposition_context(baseline, policy, model_fit, so)
  expect_identical(.policy_endpoint_status(so, family_ctx)$status, "unsupported")
})


test_that("central kernel restores synthetic outcomes like the full path", {
  set.seed(121)
  n <- 100L
  baseline <- data.frame(
    welfare = runif(n, 1, 6),
    temp = rnorm(n, 25, 2),
    electricity = rbinom(n, 1, 0.4)
  )
  train <- transform(baseline, poor = as.numeric(welfare < 3))
  policy <- baseline
  policy$electricity <- 1L
  model_fit <- list(
    engine = "fixest",
    fit3 = lm(poor ~ temp * electricity, data = train),
    weather_terms = "temp",
    train_data = train
  )
  so <- list(
    name = "poor", transform = "none", units = "PPP", povline = 3
  )

  expect_central_parity(baseline, policy, model_fit, so)
  expect_false("poor" %in% names(baseline))
  expect_false("poor" %in% names(policy))
})


test_that("central kernel preserves binned weather and missing-location fallback", {
  set.seed(13)
  n <- 150L
  baseline <- data.frame(
    welfare = exp(rnorm(n, log(3), 0.25)),
    loc_id = rep(c(1L, 2L, 3L), each = n / 3),
    temp_bin = factor(rep(c("low", "mid", "high"), length.out = n),
                      levels = c("low", "mid", "high")),
    electricity = rbinom(n, 1, 0.4)
  )
  fit <- lm(log(welfare) ~ temp_bin * electricity, data = baseline)
  policy <- baseline
  policy$electricity <- 1L
  weather <- data.frame(
    loc_id = c(1L, 1L, 2L, 2L, 9L),
    temp_bin = factor(c("mid", "mid", "high", "high", "low"),
                      levels = levels(baseline$temp_bin))
  )
  model_fit <- list(
    engine = "fixest", fit3 = fit, weather_terms = "temp_bin",
    train_data = baseline
  )

  delta <- expect_central_parity(
    baseline, policy, model_fit,
    list(name = "welfare", transform = "log"), weather_raw = weather
  )
  expect_true(all(is.finite(delta)))
  expect_identical(length(delta), nrow(baseline))
})


test_that("central kernel matches RIF clipping, interpolation, and interaction", {
  baseline <- data.frame(
    welfare = seq(1, 10, length.out = 40),
    temp = rep(c(20, 30), each = 20),
    electricity = rep(c(0, 1), 20)
  )
  policy <- baseline
  policy$electricity <- 1L
  policy[[SP_TRANSFER_COL]] <- seq(0, 1, length.out = nrow(policy))
  taus <- c(0.1, 0.5, 0.9)
  terms <- c("temp", "electricity", "temp:electricity")
  grid <- expand.grid(
    model = 3L, term = terms, tau = taus,
    KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
  )
  grid$estimate <- with(grid, c(
    temp = 0.01, electricity = 0.25, `temp:electricity` = 0.02
  )[term] * (1 + tau))
  grid$std.error <- 0.05
  model_fit <- list(
    engine = "rif", weather_terms = "temp", rif_grid = grid,
    taus = taus, train_data = baseline
  )

  expect_central_parity(
    baseline, policy, model_fit,
    list(name = "welfare", transform = "none"),
    weather_raw = data.frame(temp = c(15, 35))
  )
})


test_that("central kernel preserves no-interaction warning and fallbacks", {
  baseline <- data.frame(
    welfare = c(1.0, 2.2, 2.7, 4.4, 4.8, 6.1, 7.5, 7.9),
    temp = seq(20, 27), electricity = rep(0:1, 4),
    code = "BFA", year = "2018", survname = "wave-a",
    loc_id = seq_len(8), int_month = 6L
  )
  policy <- baseline
  policy$electricity <- 1L
  model_fit <- list(
    engine = "fixest",
    fit3 = lm(welfare ~ temp + electricity, data = baseline),
    weather_terms = "temp", train_data = baseline
  )
  so <- list(name = "welfare", transform = "none")

  expect_warning(
    central <- .policy_central_delta(baseline, policy, model_fit, so),
    "No weather.*policy interaction"
  )
  expect_warning(
    full <- decompose_policy_effect(baseline, policy, model_fit, so),
    "No weather.*policy interaction"
  )
  expect_identical(central, full$delta_total)
  expect_null(.policy_central_delta(
    baseline, policy, modifyList(model_fit, list(engine = "xgboost")), so
  ))

  pipe <- list(y_point = rep(2, nrow(baseline)),
               svy_row_id = seq_len(nrow(baseline)),
               sim_year = rep(2030L, nrow(baseline)), weight = NULL,
               id_vec = seq_len(nrow(baseline)))
  pipe <- .policy_test_attach_exposure(
    pipe, baseline, list(temp = baseline$temp)
  )
  expect_error(
    apply_policy_delta_to_baseline(
      baseline, policy, modifyList(model_fit, list(engine = "xgboost")), so,
      hist_sim_baseline = list(pipeline = pipe)
    ),
    "requires RIF or linear fixest"
  )
})

test_that("context identity failures propagate without returning unchanged pipelines", {
  set.seed(1401)
  n <- 20L
  baseline <- data.frame(
    hhid = seq_len(n), welfare = exp(rnorm(n)), temp = rnorm(n, 25),
    electricity = rep(0:1, length.out = n), code = "BFA", year = "2018",
    survname = "wave-a", loc_id = seq_len(n), int_month = 6L
  )
  policy <- baseline
  policy$electricity <- 1L
  fit <- lm(log(welfare) ~ temp * electricity, data = baseline)
  model_fit <- list(
    engine = "fixest", fit3 = fit, weather_terms = "temp",
    train_data = baseline
  )
  so <- list(name = "welfare", transform = "log")
  context <- .policy_test_context(
    baseline, policy, model_fit, so, "run-a"
  )
  pipe <- list(
    y_point = unname(predict(fit)), svy_row_id = seq_len(n),
    sim_year = rep(2030L, n), weight = NULL, id_vec = seq_len(n),
    weather_raw = data.frame(temp = 25)
  )
  pipe <- .policy_test_attach_exposure(pipe, baseline,
                                       list(temp = rep(25, n)))
  expect_error(
    apply_policy_delta_to_baseline(
      baseline, policy, model_fit, so,
      hist_sim_baseline = list(pipeline = pipe),
      decomp_context = context, run_identity = "run-b",
      annual_channels = .prepare_policy_annual_channels(context, "run-a")
    ),
    "run identity mismatch"
  )
})
