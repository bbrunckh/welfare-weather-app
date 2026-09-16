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

test_that("headline decomposition reconciles level plus resilience to total", {
  fx <- make_ols_fixture()
  r <- wiseapp::decompose_policy_effect(fx$svy_base, fx$svy_policy,
                                         fx$model_fit, fx$so)
  s <- wiseapp:::decomposition_summary_data(r, is_rif = FALSE)
  rec <- wiseapp:::decomposition_reconciliation(s)
  expect_identical(rec$status, "reconciled")
  expect_lt(abs(rec$residual), 1e-10)
  expect_equal(s$channel[s$channel_id == "resilience"], "Resilience")
})

test_that("empty headline decomposition keeps the render schema", {
  s <- wiseapp:::decomposition_summary_data(NULL, is_rif = FALSE)

  expect_equal(nrow(s), 0L)
  expect_true(all(c("channel_id", "channel", "log_points", "percent",
                    "share_of_total") %in% names(s)))
  expect_equal(wiseapp:::decomposition_reconciliation(s)$status, "unavailable")
})

test_that("decomposition explanation distinguishes OLS and RIF", {
  expect_match(wiseapp:::decomposition_explanation(FALSE)$text,
               "no repositioning")
  expect_match(wiseapp:::decomposition_explanation(TRUE)$text,
               "repositioning")
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
        decomp_result = shiny::reactiveVal(result),
        decomp_scenarios = shiny::reactiveVal(scenarios),
        model_fit = shiny::reactiveVal(model),
        so = shiny::reactiveVal(fx$so),
        baseline_svy = shiny::reactiveVal(fx$svy_base),
        policy_svy = shiny::reactiveVal(fx$svy_policy)
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

test_that("decomposition UI omits redundant cards and tables", {
  html <- as.character(htmltools::renderTags(
    wiseapp:::mod_3_09_decomposition_ui("decomposition")
  )$html)

  expect_false(grepl("Policy Effect Decomposition", html, fixed = TRUE))
  expect_false(grepl("Paired policy incidence", html, fixed = TRUE))
  expect_false(grepl("Hierarchical channel details", html, fixed = TRUE))
  expect_false(grepl("scenario_range_table", html, fixed = TRUE))
  expect_true(grepl("Weather-year basis", html, fixed = TRUE))
})

test_that("technical decomposition table is concise and human readable", {
  fx <- make_ols_fixture()
  result <- wiseapp::decompose_policy_effect(
    fx$svy_base, fx$svy_policy, fx$model_fit, fx$so
  )
  tbl <- wiseapp:::.build_decomp_table(result, is_rif = FALSE)

  expect_identical(
    names(tbl),
    c("Effect component", "Mean effect (%)", "Coefficient SE (%)")
  )
  expect_true(all(c("Total effect",
                    "Main effect (direct transfer and covariate shift)",
                    "Direct transfer component",
                    "Weather-policy interaction") %in% tbl$`Effect component`))
  expect_false("Repositioning effect" %in% tbl$`Effect component`)

  deciles <- wiseapp:::decomposition_channels_by_decile(
    result, fx$svy_base, "welfare", is_rif = FALSE
  )
  exported <- wiseapp:::decomposition_decile_export(deciles, is_rif = FALSE)
  expect_false(any(grepl("_", names(exported), fixed = TRUE)))
  expect_false("Repositioning effect (%)" %in% names(exported))
})

test_that("technical decomposition table includes adverse weather bases", {
  fx <- make_ols_fixture()
  result <- wiseapp::decompose_policy_effect(
    fx$svy_base, fx$svy_policy, fx$model_fit, fx$so
  )
  tbl <- wiseapp:::.build_decomp_table_by_basis(
    list(
      `Mean weather` = result,
      `Adverse 1-in-5` = result,
      `Adverse 1-in-10` = result,
      `Adverse 1-in-20` = result
    ),
    is_rif = FALSE
  )
  expect_identical(
    names(tbl),
    c("Effect component", "Mean weather (%)", "Coefficient SE (%)",
      "Adverse 1-in-5 (%)", "Adverse 1-in-10 (%)", "Adverse 1-in-20 (%)")
  )
  expect_true(all(c("Total effect", "Weather-policy interaction") %in% tbl$`Effect component`))
})

test_that("technical decomposition table handles unavailable weather bases", {
  expect_identical(
    wiseapp:::.build_decomp_table_by_basis(list(NULL, NULL), is_rif = FALSE),
    data.frame()
  )
})

test_that("weather basis selects adverse years for future decompositions", {
  sc <- data.frame(
    scenario = rep("SSP3-7.0 / 2025-2035", 3),
    sim_year = 2025:2027,
    delta_total = c(0.10, 0.30, 0.20),
    weight = 1
  )

  selected <- wiseapp:::select_decomp_weather_basis(
    sc, basis = "adverse_10", so = list(name = "welfare", type = "numeric")
  )
  expect_identical(selected$sim_year, 2025L)
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
  old <- list(result = data.frame(id = 1L), context = "run-1")
  expect_identical(
    wiseapp:::.publish_decomposition_bundle(old, data.frame(id = 2L), "run-2", FALSE),
    old
  )
  expect_identical(
    wiseapp:::.publish_decomposition_bundle(old, data.frame(id = 2L), "run-2", TRUE),
    list(result = data.frame(id = 2L), context = "run-2")
  )
})
