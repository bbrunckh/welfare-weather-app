library(testthat)


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


test_that("policy correction changes only y_point and preserves pipeline types", {
  set.seed(111)
  n <- 60L
  baseline <- data.frame(
    hhid = seq_len(n), welfare = exp(rnorm(n)), temp = rnorm(n, 25),
    electricity = rep(0:1, length.out = n)
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
  hist <- list(pipeline = pipe, weather_raw = pipe$weather_raw)

  out <- apply_policy_delta_to_baseline(
    baseline, policy, model_fit,
    list(name = "welfare", transform = "log"), hist
  )$hist_sim$pipeline

  expect_identical(names(out), names(pipe))
  expect_identical(out[-1L], pipe[-1L])
  expect_type(out$y_point, typeof(pipe$y_point))
  expect_identical(attributes(out$y_point), attributes(pipe$y_point))
  expect_false(identical(out$y_point, pipe$y_point))
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
    weight = runif(n, 0.5, 2)
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
  make_pipe <- function(weather_ref) list(
    y_point = unname(stats::predict(fit))[order],
    F_loading = matrix(rnorm(n * 2L), ncol = 2L),
    id_vec = baseline$household_key[order],
    sim_year = rep(2030L, n),
    weight = baseline$weight[order],
    weather_raw = weather_ref
  )
  hist_pipe <- make_pipe(hist_ref)
  future_pipe <- make_pipe(future_ref)
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

  out <- apply_policy_delta_to_baseline(
    baseline, policy, model_fit, so, hist, scenarios
  )
  hist_delta <- .policy_central_delta(
    baseline, policy, model_fit, so, weather_raw = hist_weather
  )
  future_delta <- .policy_central_delta(
    baseline, policy, model_fit, so, weather_raw = future_weather
  )

  expect_equal(
    out$hist_sim$pipeline$y_point,
    hist_pipe$y_point + hist_delta[order]
  )
  expect_equal(
    out$saved_scenarios[[1]]$pipelines[[1]]$y_point,
    future_pipe$y_point + future_delta[order]
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
    temp = seq(20, 27), electricity = rep(0:1, 4)
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
               svy_row_id = seq_len(nrow(baseline)))
  out <- apply_policy_delta_to_baseline(
    baseline, policy, modifyList(model_fit, list(engine = "xgboost")), so,
    hist_sim_baseline = list(pipeline = pipe)
  )
  expect_identical(out$hist_sim$pipeline, pipe)
})

test_that("context identity failures propagate without returning unchanged pipelines", {
  set.seed(1401)
  n <- 20L
  baseline <- data.frame(
    hhid = seq_len(n), welfare = exp(rnorm(n)), temp = rnorm(n, 25),
    electricity = rep(0:1, length.out = n)
  )
  policy <- baseline
  policy$electricity <- 1L
  fit <- lm(log(welfare) ~ temp * electricity, data = baseline)
  model_fit <- list(
    engine = "fixest", fit3 = fit, weather_terms = "temp",
    train_data = baseline
  )
  so <- list(name = "welfare", transform = "log")
  context <- .build_decomposition_context(
    baseline, policy, model_fit, so, run_identity = "run-a"
  )
  pipe <- list(
    y_point = unname(predict(fit)), svy_row_id = seq_len(n),
    weather_raw = data.frame(temp = 25)
  )
  expect_error(
    apply_policy_delta_to_baseline(
      baseline, policy, model_fit, so,
      hist_sim_baseline = list(pipeline = pipe),
      decomp_context = context, run_identity = "run-b"
    ),
    "run identity mismatch"
  )
})
