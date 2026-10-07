library(testthat)

# Synthetic OLS fixture: log-welfare ~ temp + transfer + temp:transfer.
# `transfer` flips from 0 to 1 between baseline and policy survey frames.
make_ols_fixture <- function(N = 300, seed = 1L) {
  set.seed(seed)
  df <- data.frame(
    welfare  = exp(stats::rnorm(N, log(3), 0.25)),
    temp     = stats::rnorm(N, 25, 2),
    transfer = stats::rbinom(N, 1, 0.3),
    weight   = stats::runif(N, 0.5, 2.0)
  )
  fit <- stats::lm(log(welfare) ~ temp + transfer + temp:transfer, data = df)
  list(
    model_fit = list(
      engine        = "fixest",  # OLS path inside decompose_policy_effect
      fit3          = fit,
      weather_terms = "temp",
      train_data    = df
    ),
    so          = list(name = "welfare", transform = "log"),
    svy_base    = df,
    svy_policy  = (function() {
      p <- df
      p$transfer <- 1L
      p[[wiseapp::SP_TRANSFER_COL]] <- 5
      p
    })()
  )
}

test_that("point-estimate additivity: delta_total == delta_main + delta_res1 + delta_res2", {
  fx <- make_ols_fixture()
  r  <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                          fx$model_fit, fx$so)
  expect_s3_class(r, "data.frame")
  err <- max(abs(r$delta_total - (r$delta_main + r$delta_res1 + r$delta_res2)))
  expect_lt(err, 1e-12)
})

test_that("policy decomposition derives a missing poor outcome", {
  set.seed(12)
  n <- 120L
  base <- data.frame(
    welfare = exp(stats::rnorm(n, log(3), 0.25)),
    temp = stats::rnorm(n, 25, 2),
    transfer = stats::rbinom(n, 1, 0.3),
    weight = stats::runif(n, 0.5, 2)
  )
  base$poor <- as.integer(base$welfare < 3)
  fit <- stats::lm(poor ~ temp + transfer + temp:transfer, data = base)
  policy <- base
  policy$transfer <- 1L
  policy[[wiseapp::SP_TRANSFER_COL]] <- 0
  policy$poor <- NULL
  base$poor <- NULL

  result <- wiseapp::decompose_policy_effect(
    base,
    policy,
    list(
      engine = "fixest",
      fit3 = fit,
      weather_terms = "temp",
      train_data = transform(base, poor = as.integer(welfare < 3))
    ),
    list(name = "poor", units = "PPP", transform = NA_character_, povline = 3)
  )

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), n)
  expect_true(all(is.finite(result$delta_total)))
})

test_that("variance additivity: Var(total) == Var(main) + Var(res1) + Var(res2)", {
  # Under the diagonal-Σ approximation documented in fct_policy_decompose.R,
  # channels are independent and variances add exactly.
  fx <- make_ols_fixture()
  r  <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                          fx$model_fit, fx$so)
  expect_true(all(c("sd_main", "sd_res1", "sd_res2", "sd_total") %in% names(r)))
  err <- max(abs(r$sd_total^2 - (r$sd_main^2 + r$sd_res1^2 + r$sd_res2^2)))
  expect_lt(err, 1e-10)
})

test_that("skip_coef = TRUE zeroes out every per-channel SE", {
  fx <- make_ols_fixture()
  r0 <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                          fx$model_fit, fx$so,
                                          skip_coef = TRUE)
  expect_equal(max(c(r0$sd_main, r0$sd_res1, r0$sd_res2, r0$sd_total)), 0)
})

test_that("non-zero SE appears where the model actually has uncertainty", {
  fx <- make_ols_fixture()
  r  <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                          fx$model_fit, fx$so)
  # Main effect picks up the `transfer` coefficient SE (Δ transfer = 1 for
  # the 70% of households whose policy value flipped from 0 to 1) and the
  # interaction has the `temp:transfer` coefficient SE times haz · Δx.
  expect_true(any(r$sd_main > 0))
  expect_true(any(r$sd_res2 > 0))
  # OLS path: no repositioning channel → sd_res1 is identically 0.
  expect_equal(max(r$sd_res1), 0)
})

test_that("aggregated total SE remains consistent under household weighting", {
  # Var(Σ w·δ / Σ w) under household independence should match the sum of
  # the three per-channel weighted variances (independence ⇒ Cov = 0).
  fx <- make_ols_fixture()
  r  <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                          fx$model_fit, fx$so)
  w_norm <- r$weight / sum(r$weight)
  agg_var <- function(sd_col) sum((w_norm^2) * (r[[sd_col]])^2)
  v_components <- agg_var("sd_main") + agg_var("sd_res1") + agg_var("sd_res2")
  v_total      <- agg_var("sd_total")
  expect_lt(abs(v_total - v_components) / pmax(v_total, 1e-12), 1e-10)
})

test_that("decile decomposition plot uses engine-specific channels", {
  fx <- make_ols_fixture()
  r <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                         fx$model_fit, fx$so)
  tbl <- wiseapp:::decomposition_channels_by_decile(
    r, fx$svy_base, "welfare", is_rif = FALSE
  )
  expect_true(all(c("cash_transfer_percent", "covariate_shift_percent",
                    "interaction_percent",
                    "repositioning_percent") %in% names(tbl)))
  p <- wiseapp:::plot_decomposition_channels_by_decile(tbl, is_rif = FALSE)
  expect_s3_class(p, "ggplot")
  expect_false("Resilience - Repositioning effect" %in% as.character(p$data$channel))
  expect_true(all(c("Main effect (covariate shift)",
                    "Resilience - Interaction effect") %in% as.character(p$data$channel)))
})

test_that("decomposition module renders core plots for OLS and RIF schemas", {
  fx <- make_ols_fixture(N = 180)
  ols <- wiseapp::decompose_policy_effect(
    fx$svy_base, fx$svy_policy, fx$model_fit, fx$so
  )

  check_module <- function(result, engine) {
    model <- fx$model_fit
    model$engine <- engine
    if (identical(engine, "rif")) {
      model$rif_grid <- data.frame(
        model = 3L, term = "temp", tau = c(0.25, 0.5, 0.75),
        estimate = c(-0.02, -0.01, 0), std.error = 0.01,
        conf.low = c(-0.04, -0.03, -0.02),
        conf.high = c(0, 0.01, 0.02)
      )
    }
    scenarios <- dplyr::bind_rows(lapply(2030:2032, function(year) {
      transform(
        result,
        scenario = "SSP2-4.5 / 2030-2040",
        sim_year = year,
        year_start = 2030L,
        year_end = 2040L
      )
    }))
    shiny::testServer(
      wiseapp:::mod_3_09_decomposition_server,
      args = list(
        id = "decomposition",
        decomp_scenarios = shiny::reactiveVal(scenarios),
        model_fit = shiny::reactiveVal(model),
        so = shiny::reactiveVal(fx$so),
        baseline_svy = shiny::reactiveVal(fx$svy_base)
      ),
      {
        session$flushReact()
        expect_false(is.null(session$output$headline_decomp_plot))
        expect_false(is.null(session$output$decomp_bar_plot))
        if (identical(engine, "rif")) {
          expect_false(is.null(session$output$beta_curve_ui))
          expect_false(is.null(session$output$beta_curve_plot1))
        }
      }
    )
  }

  check_module(ols, "fixest")

  rif <- ols
  rif$delta_res1 <- rep(0.002, nrow(rif))
  rif$delta_total <- rif$delta_main + rif$delta_res1 + rif$delta_res2
  check_module(rif, "rif")
})

test_that("metric decomposition is kept separate from Step 3 plots", {
  summary <- data.frame(scenario = "SSP2-4.5", baseline = .32, after_main = .30,
    after_repositioning = .29, policy = .28, main = -.02, repositioning = -.01,
    interaction = -.01, resilience = -.02, total = -.04, n_models = 2L,
    n_model_years = 60L, n_dropped_model_years = 0L, center_method = "equal_model_mean",
    scope = "production_prediction_rows")
  annual <- data.frame(scenario = "SSP2-4.5", member = "model-a", model_id = "model-a",
    sim_year = 2020L, baseline = .31, after_main = .30, after_repositioning = .29,
    policy = .28, main = -.01, repositioning = -.01, interaction = -.01,
    resilience = -.02, total = -.03, n_prediction_rows = 100L, n_retained_rows = 100L,
    n_excluded_rows = 0L, parity_error = 0, requested_residuals = "original",
    effective_residuals = "none", scope = "production_prediction_rows")
  tails <- data.frame(scenario = "SSP2-4.5", return_period = 20,
    scope = "baseline_anchored", adverse_basis = "baseline_selected_metric", status = "ok", reason = "",
    baseline = .1, after_main = .09, after_repositioning = .08, policy = .07,
    main = -.01, repositioning = -.01, interaction = -.01, resilience = -.02,
    total = -.03, probability = .05, n_models = 2L, n_model_years = 60L,
    center_method = "equal_model_mean", ensemble_center = "equal_model_mean",
    quantile_method = "rank_interp_n_p_plus_half", tie_method = "value_then_sim_year_ascending",
    year_lo = 2001, year_hi = 2002, rank_lo = 1L, rank_hi = 2L, weight_lo = .5, weight_hi = .5)
  mechanisms <- data.frame(scenario = "SSP2-4.5", hazard = "temp", category = NA_character_,
    contrast = "continuous_coefficient_change", repositioning = .02, interaction = -.01,
    positive_repositioning_share = .6, negative_repositioning_share = .4,
    positive_interaction_share = .2, negative_interaction_share = .8, tau_pre = .4,
    tau_post = .6, model_units = "log outcome units", weather_units = "C", n_models = 2L)
  result <- list(status = "ok", reason = NULL, summary = summary, return_period = tails,
    annual = annual,
    mechanisms = list(summary = mechanisms, annual = transform(mechanisms, sim_year = 2020L,
      model_id = "model-a"), repositioning_status = "modeled",
      interaction_status = "included", fitted_curve = NULL),
    metadata = list(method = "headcount_ratio", label = "Poverty rate", format = "percent",
      display_multiplier = 100, change_unit = "pp", level_unit = "percent",
      component_order = "main -> repositioning -> interaction", correction_version = "row_aligned_annual_v1",
      focus_scenario = "SSP2-4.5", repositioning_modeled = TRUE, interaction_included = TRUE,
      population_scope = "fixed survey rows", weight_interpretation = "survey weighted",
      native_unit = "fraction", threshold_kind = "selected poverty line", threshold_value = 3,
      threshold_unit = "selected PPP units (2021)", exposure_source = "step2_prediction_row_mapping",
      run_identity = "run-1", requested_residuals = "original", effective_residuals = "none",
      uncertainty = "central_only"),
    scenarios = list("SSP2-4.5" = list(status = "ok", summary = summary,
      annual = annual, return_period = tails, adverse_support = data.frame(), adverse_by_model = data.frame(),
      mechanisms = transform(mechanisms, sim_year = 2020L, model_id = "model-a"))),
    adverse_support = data.frame(), adverse_by_model = data.frame())
  html <- htmltools::renderTags(mod_3_09_decomposition_ui("decomposition"))$html
  expect_false(grepl("Technical Decomposition on the Model Scale", html, fixed = TRUE))
  expect_false(grepl("How the Policy Changes Weather Sensitivity", html, fixed = TRUE))
  expect_match(html, "What drives the total policy effect?", fixed = TRUE)
  expect_match(html, "Who gains, and through which channel?", fixed = TRUE)
})

test_that("Module 3 diagnostics formatters are callable", {
  inputs <- data.frame(
    variable = "income", baseline_mean = 1, policy_mean = 2,
    mean_change = 1, baseline_sd = 0.5, policy_sd = 0.6
  )
  treatment <- data.frame(
    status = "Treated", n = 10, weighted_n = 100, weighted_share = 0.5
  )

  expect_s3_class(wiseapp:::.format_policy_input_table(inputs), "data.frame")
  expect_s3_class(wiseapp:::.format_policy_treatment_table(treatment), "data.frame")
})

test_that("Module 3 diagnostics tables render with policy data", {
  fx <- make_ols_fixture(N = 80)
  shiny::testServer(
    wiseapp:::mod_3_08_diagnostics_server,
    args = list(
      id = "diagnostics",
      baseline_svy = shiny::reactiveVal(fx$svy_base),
      policy_svy = shiny::reactiveVal(fx$svy_policy),
      sim_run_id = shiny::reactiveVal(0L),
      tabset_id = "tabs"
    ),
    {
      session$flushReact()
      expect_false(is.null(session$output$diag_summary_table))
      expect_false(is.null(session$output$treatment_table))
    }
  )
})

test_that("decomposition UI provides collapsible data tables below charts", {
  html <- as.character(htmltools::renderTags(
    wiseapp:::mod_3_09_decomposition_ui("decomposition")
  )$html)

  expect_false(grepl("Policy Effect Decomposition", html, fixed = TRUE))
  expect_false(grepl("Paired policy incidence", html, fixed = TRUE))
  expect_false(grepl("Hierarchical channel details", html, fixed = TRUE))
  expect_false(grepl("scenario_range_table", html, fixed = TRUE))
  expect_true(grepl("What drives the total policy effect?", html, fixed = TRUE))
  expect_true(grepl("Who gains, and through which channel?", html, fixed = TRUE))
  expect_true(grepl("headline_weather_basis_ui", html, fixed = TRUE))
  expect_true(grepl("decile_weather_basis_ui", html, fixed = TRUE))
  expect_true(grepl("Weighted average policy effect by baseline welfare decile", html, fixed = TRUE))
  expect_true(grepl("View decomposition data", html, fixed = TRUE))
  expect_true(grepl("headline_decomp_table", html, fixed = TRUE))
  expect_true(grepl("decile_decomp_table", html, fixed = TRUE))
  expect_false(grepl("decomp_summary_table", html, fixed = TRUE))
})

test_that("one decomposition context preserves central values and schemas", {
  fx <- make_ols_fixture()
  ctx <- wiseapp:::.build_decomposition_context(
    fx$svy_base, fx$svy_policy, fx$model_fit, fx$so,
    deltas = wiseapp:::.compute_policy_deltas(
      fx$svy_base, fx$svy_policy, "welfare", "temp"
    ), skip_coef = FALSE, run_identity = "ols-run-1"
  )
  fresh <- wiseapp::decompose_policy_effect(
    fx$svy_base, fx$svy_policy, fx$model_fit, fx$so
  )
  reused <- wiseapp::decompose_policy_effect(
    fx$svy_base, fx$svy_policy, fx$model_fit, fx$so, context = ctx,
    run_identity = "ols-run-1"
  )
  expect_identical(names(fresh), names(reused))
  expect_equal(fresh, reused, tolerance = 0)
  expect_false(identical(ctx, NULL))
  expect_error(
    wiseapp::decompose_policy_effect(
      fx$svy_base, fx$svy_policy, fx$model_fit, fx$so, context = ctx
    ),
    "Current run identity is required"
  )
})

test_that("RIF context parity caches channels and rejects stale runs", {
  fx <- make_ols_fixture(N = 120)
  mf <- fx$model_fit
  mf$engine <- "rif"
  mf$taus <- c(0.25, 0.5, 0.75)
  mf$rif_grid <- data.frame(
    model = 3L,
    term = rep(c("temp", "transfer", "temp:transfer"), each = 3L),
    tau = rep(mf$taus, 3L),
    estimate = rep(c(-0.02, -0.01, 0.0, 0.1, 0.1, 0.1, 0.02, 0.02, 0.02), each = 1L),
    std.error = 0.01
  )
  ctx <- wiseapp:::.build_decomposition_context(
    fx$svy_base, fx$svy_policy, mf, fx$so,
    run_identity = "rif-run-1", weather_panels = list(fx$svy_base["temp"])
  )
  fresh <- wiseapp::decompose_policy_effect(
    fx$svy_base, fx$svy_policy, mf, fx$so,
    weather_raw = fx$svy_base["temp"], run_identity = "rif-run-1"
  )
  reused <- wiseapp::decompose_policy_effect(
    fx$svy_base, fx$svy_policy, mf, fx$so,
    weather_raw = fx$svy_base["temp"], context = ctx,
    run_identity = "rif-run-1"
  )
  expect_equal(fresh, reused, tolerance = 0)
  expect_gt(length(ctx$hazard_products), 0L)
  expect_equal(ctx$cache_entries$hazard_cache_entries, 1L)
  expect_equal(ctx$cache_entries$fixed_decile_entries, 1L)
  expect_equal(ctx$cache_entries$rif_invariant_entries, 1L)
  expect_gt(ctx$reuse_counters$hazard_cache_hits, 0L)
  expect_gt(ctx$reuse_counters$rif_invariant_reuses, 0L)
  expect_error(
    wiseapp::decompose_policy_effect(
      fx$svy_base, transform(fx$svy_policy, temp = temp + 1), mf, fx$so,
      context = ctx, run_identity = "rif-run-1"
    ),
    "Incompatible or stale"
  )
  expect_error(
    wiseapp::decompose_policy_effect(
      fx$svy_base, fx$svy_policy, mf, fx$so,
      context = ctx, run_identity = "rif-run-2"
    ),
    "run identity mismatch"
  )
  expect_error(
    wiseapp::decompose_policy_effect(
      fx$svy_base, fx$svy_policy, mf, fx$so,
      context = ctx, run_identity = "rif-run-1", skip_coef = TRUE
    ),
    "Incompatible or stale"
  )
  expect_error(
    wiseapp:::.policy_central_delta(
      fx$svy_base, fx$svy_policy, mf, fx$so,
      context = ctx
    ),
    "Current run identity is required"
  )
})

test_that("run-owned context is isolated from data.table mutation; public reuse stays strict", {
  skip_if_not_installed("data.table")
  fx <- make_ols_fixture(N = 120)
  baseline <- data.table::as.data.table(data.table::copy(fx$svy_base))
  policy <- data.table::as.data.table(data.table::copy(fx$svy_policy))
  context <- wiseapp:::.build_decomposition_context(
    baseline, policy, fx$model_fit, fx$so, run_identity = "owned-data-table-run"
  )

  data.table::set(baseline, i = 1L, j = "welfare", value = baseline$welfare[[1L]] + 10)
  data.table::set(policy, i = 1L, j = "transfer", value = 99)

  expect_equal(as.numeric(context$svy_baseline$welfare), fx$svy_base$welfare, tolerance = 0)
  expect_error(
    wiseapp::decompose_policy_effect(
      baseline, policy, fx$model_fit, fx$so, context = context,
      run_identity = "owned-data-table-run"
    ),
    "Incompatible or stale"
  )
})

test_that("decomposition summaries honor fixed cached deciles", {
  fx <- make_ols_fixture(N = 40)
  result <- wiseapp::decompose_policy_effect(
    fx$svy_base, fx$svy_policy, fx$model_fit, fx$so
  )
  cached <- rep(7L, nrow(result))
  tbl <- wiseapp:::decomposition_channels_by_decile(
    result, fx$svy_base, "welfare", is_rif = FALSE,
    baseline_deciles = cached
  )
  expect_identical(unique(tbl$decile), 7L)
})

test_that("failed publication preserves the previous result and context", {
  old <- list(context = "run-1")
  expect_identical(wiseapp:::.publish_decomposition_bundle(old, "run-2", FALSE), old)
  expect_identical(
    wiseapp:::.publish_decomposition_bundle(old, "run-2", TRUE),
    list(context = "run-2")
  )
})
