# ============================================================================ #
# tests/testthat/test-policy-sim-compare-agg-cache.R                           #
# PERF-31: Step 3 aggregation cache. The aggregation workspace is keyed by     #
# (source, method, poverty line) only - the deviation control is applied       #
# downstream, so changing it must not re-aggregate anything. Cached results    #
# are identical to freshly computed ones (aggregate_pipeline_per_year is       #
# RNG-deterministic per seed + year, see Batch C tests).                       #
#                                                                              #
# .wire_results_pane() invisibly returns its aggregation internals so these    #
# tests can drive the real reactives.                                          #
# ============================================================================ #

library(testthat)
library(shiny)

make_step3_pipe_fixture <- function(n = 300L, yrs = 2020:2021) {
  set.seed(11)
  expand <- length(yrs)
  list(
    sim_year  = rep(yrs, each = n),
    y_point   = rnorm(n * expand, 1.2, 0.4),
    weight    = rep(c(1, 2), length.out = n * expand),
    # Tiny loadings keep SEs small and deterministic under the delta method
    F_loading = matrix(rnorm(2 * n * expand) * 0.01, nrow = n * expand),
    train_aug = NULL, id_vec = NULL, id_col = NULL
  )
}

make_step3_hist_fixture <- function() {
  list(
    so        = list(type = "numeric", name = "welfare", transform = "log"),
    residuals = "none",
    pipeline  = make_step3_pipe_fixture()
  )
}

make_step3_scenarios_fixture <- function() {
  pipe <- make_step3_pipe_fixture()
  list("SSP2-4.5 / 2030-2040" = list(
    scenario_name = "SSP2-4.5 / 2030-2040",
    so            = list(type = "numeric", name = "welfare", transform = "log"),
    pipelines     = list(ensemble_mean = pipe, ensemble_hi = pipe)
  ))
}

make_step3_scenarios_fixture_multi <- function() {
  pipe  <- make_step3_pipe_fixture()
  so    = list(type = "numeric", name = "welfare", transform = "log")
  list(
    "SSP2-4.5 / 2030-2040" = list(
      scenario_name = "SSP2-4.5 / 2030-2040", so = so,
      pipelines = list(ensemble_mean = pipe, ensemble_hi = pipe)
    ),
    "SSP5-8.5 / 2030-2040" = list(
      scenario_name = "SSP5-8.5 / 2030-2040", so = so,
      pipelines = list(ensemble_mean = pipe, ensemble_hi = pipe)
    )
  )
}

test_that("Results owns validated selection and fixed prosperity threshold without rerunning policy", {
  hs <- reactiveVal(make_step3_hist_fixture())
  internals <- NULL
  testServer(function(input, output, session) {
    internals <<- .wire_results_pane(input, output, session,
      hs, reactiveVal(list()), hs, reactiveVal(list()), selected_hist = reactiveVal(NULL),
      analysis_unit = reactiveVal("hh"))
  }, {
    session$setInputs(cmp_agg_method = "prosperity_gap", cmp_pov_line = 3)
    session$elapse(500); session$flushReact()
    before <- hs()$pipeline
    agg <- internals$baseline_agg_hist()
    keys <- internals$agg_cache_keys()
    expect_null(internals$poverty_line())
    expect_equal(internals$metric_context()$threshold_value, 28)
    expect_identical(internals$metric_context()$analysis_unit, "hh")
    expect_identical(internals$focus_scenario(), "Historical")
    session$setInputs(cmp_pov_line = 7)
    session$elapse(500); session$flushReact()
    expect_identical(internals$baseline_agg_hist(), agg)
    expect_identical(internals$agg_cache_keys(), keys)
    session$setInputs(cmp_agg_method = "headcount_ratio")
    session$flushReact()
    expect_equal(internals$poverty_line(), 7)
    expect_equal(internals$metric_context()$threshold_value, 7)
    export <- session$userData$wise_exports$items$policy_adverse_effect_table$fun()
    if (nrow(export)) expect_true(all(export$threshold_value == 7))
    session$setInputs(cmp_pov_line = 9, cmp_agg_method = "gap")
    session$flushReact()
    expect_identical(internals$aggregation_method(), "gap")
    expect_equal(internals$metric_context()$threshold_value, internals$poverty_line())
    session$elapse(500); session$flushReact()
    expect_equal(internals$metric_context()$threshold_value, 9)
    expect_identical(hs()$pipeline, before)
    changed <- hs()
    changed$so$type <- "logical"
    hs(changed)
    session$flushReact()
    expect_identical(internals$aggregation_method(), "mean")
  })
})

test_that("Results shares metric decomposition reactively and withholds stale channels", {
  hist <- make_step3_hist_fixture()
  scenario <- make_step3_scenarios_fixture()
  run_context <- new.env(parent = emptyenv())
  run_context$run_identity <- "metric-run"
  prepared <- list2env(list(run_identity = "metric-run", context = run_context),
    parent = emptyenv())
  context <- reactiveVal(run_context)
  annual <- reactiveVal(prepared)
  stale <- reactiveVal(FALSE)
  calls <- list()
  local_mocked_bindings(
    .policy_metric_decomposition = function(
      baseline_hist, policy_hist, baseline_scenarios, policy_scenarios,
      prepared, method, pov_line, requested_residuals,
      endpoint_series_baseline, endpoint_series_policy,
      focus_scenario, analysis_unit
    ) {
      calls[[length(calls) + 1L]] <<- list(
        method = method, pov_line = pov_line,
        requested_residuals = requested_residuals,
        focus_scenario = focus_scenario, analysis_unit = analysis_unit,
        prepared = prepared
      )
      list(
        status = if (is.null(prepared)) "unavailable" else "ok",
        reason = if (is.null(prepared)) "Prepared source missing." else NULL,
        annual = if (is.null(prepared)) data.frame() else data.frame(value = 1),
        summary = if (is.null(prepared)) data.frame() else list(method = method, pov_line = pov_line),
        return_period = data.frame(), mechanisms = data.frame(),
        endpoint_summary = list(method = method, pov_line = pov_line),
        scenarios = list(), metadata = list()
      )
    },
    .validate_run_decomposition_context = function(context, run_identity) {
      if (!identical(context$run_identity, run_identity)) {
        stop("decomposition context run identity mismatch", call. = FALSE)
      }
      invisible(context)
    },
    .package = "wiseapp"
  )
  internals <- NULL
  testServer(function(input, output, session) {
    internals <<- .wire_results_pane(
      input, output, session, reactiveVal(hist), reactiveVal(scenario),
      reactiveVal(hist), reactiveVal(scenario),
      selected_hist = reactiveVal(NULL), residuals = reactiveVal("none"),
      stale = stale, decomp_context = context, annual_channels = annual,
      analysis_unit = reactiveVal("ind")
    )
  }, {
    session$setInputs(cmp_agg_method = "headcount_ratio", cmp_pov_line = 3)
    session$elapse(500); session$flushReact()
    result <- internals$metric_decomposition()
    expect_true(identical(result$status, "ok"), info = result$reason)
    expect_identical(calls[[length(calls)]]$method, "headcount_ratio")
    expect_equal(calls[[length(calls)]]$pov_line, 3)
    expect_identical(calls[[length(calls)]]$requested_residuals, "none")
    expect_identical(calls[[length(calls)]]$focus_scenario, names(scenario)[[1L]])
    expect_identical(calls[[length(calls)]]$analysis_unit, "ind")

    session$setInputs(cmp_pov_line = 5)
    session$elapse(500); session$flushReact()
    changed <- internals$metric_decomposition()
    expect_equal(changed$summary$pov_line, 5)
    expect_equal(calls[[length(calls)]]$pov_line, 5)

    stale(TRUE); session$flushReact()
    withheld <- internals$metric_decomposition()
    expect_identical(withheld$status, "unavailable")
    expect_match(withheld$reason, "stale", ignore.case = TRUE)
    expect_equal(nrow(withheld$summary), 0L)
    expect_equal(withheld$endpoint_summary$pov_line, 5)

    stale(FALSE)
    context(list(run_identity = "different-run"))
    session$flushReact()
    mismatch <- internals$metric_decomposition()
    expect_identical(mismatch$status, "unavailable")
    expect_match(mismatch$reason, "run identities differ", fixed = TRUE)
    expect_equal(nrow(mismatch$summary), 0L)
    expect_equal(mismatch$endpoint_summary$pov_line, 5)

    context(run_context)
    annual(NULL)
    session$flushReact()
    missing <- internals$metric_decomposition()
    expect_identical(missing$status, "unavailable")
    expect_equal(nrow(missing$summary), 0L)
    expect_equal(missing$endpoint_summary$pov_line, 5)
  })
})

test_that("unvalidated logistic policy endpoints are withheld in cards, comparisons and exports", {
  hs <- make_step3_hist_fixture()
  hs$so <- list(name = "poor", type = "numeric", transform = "none")
  hs$pipeline$y_point <- rep(c(.2, .4), length.out = length(hs$pipeline$y_point))
  internals <- NULL
  testServer(function(input, output, session) {
    internals <<- .wire_results_pane(input, output, session,
      reactiveVal(hs), reactiveVal(list()), reactiveVal(hs), reactiveVal(list()),
      selected_hist = reactiveVal(NULL),
      decomp_context = reactiveVal(list(model_type = "logistic", run_identity = "logit-run")))
  }, {
    session$flushReact()
    expect_identical(internals$policy_endpoint_status()$status, "unsupported")
    expect_null(internals$policy_agg_hist())
    expect_length(internals$policy_agg_scenarios(), 0)
    expect_equal(nrow(internals$paired_effect_summary()), 0)
    expect_equal(nrow(internals$threshold_table()), 0)
    expect_identical(internals$headline_cards()[[1]]$value, "Unavailable")
    expect_match(internals$headline_cards()[[1]]$note, "response-scale", fixed = TRUE)
    expect_true(all(is.finite(internals$baseline_agg_hist()$out$value)))
    items <- session$userData$wise_exports$items
    for (key in c("policy_paired_effect_summary", "policy_annual_effect_data",
                  "policy_adverse_effects", "policy_distributional_incidence_data")) {
      expect_equal(nrow(items[[key]]$fun()), 0)
    }
    headline <- items$policy_headline_summary$fun()
    expect_true(all(is.na(headline$effect_native)))
    expect_true(all(headline$availability == "unsupported"))
    for (key in c("policy_annual_distribution", "policy_adverse_distribution",
                  "policy_outcome_distribution", "policy_exceedance")) {
      if (!is.null(items[[key]])) expect_no_error(items[[key]]$fun())
    }
  })
})

test_that("Step 3 paired and headline reactives explicitly use equal-model means", {
  hist <- make_step3_hist_fixture()
  hist$so$transform <- "identity"
  baseline <- policy <- make_step3_scenarios_fixture()
  name <- names(baseline)[[1L]]
  pipe <- make_step3_pipe_fixture(n = 20L)
  pipe$y_point <- rep(c(1, 2), each = 20L)
  baseline[[name]]$so <- policy[[name]]$so <- hist$so
  baseline[[name]]$pipelines <- setNames(rep(list(pipe), 3), paste0("m", 1:3))
  policy[[name]]$pipelines <- lapply(c(0, 1, 9), function(delta) {
    out <- pipe
    out$y_point <- out$y_point + delta
    out
  })
  names(policy[[name]]$pipelines) <- names(baseline[[name]]$pipelines)
  internals <- NULL
  testServer(function(input, output, session) {
    internals <<- .wire_results_pane(input, output, session,
      reactiveVal(hist), reactiveVal(baseline), reactiveVal(hist), reactiveVal(policy),
      selected_hist = reactiveVal(NULL), residuals = reactiveVal("none"))
  }, {
    session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none")
    session$flushReact()
    for (summary in list(internals$paired_effect_summary(),
                         internals$headline_paired_effect_summary())) {
      focus <- summary[summary$scenario == name, ]
      expect_equal(focus$value, 10 / 3)
      expect_equal(focus$baseline, 1.5)
      expect_equal(focus$policy - focus$baseline, focus$value)
      expect_identical(focus$center_method, "equal_model_mean")
    }
    expect_match(internals$headline_cards()[[1]]$note,
                 "Policy vs baseline", fixed = TRUE)
    expect_match(internals$headline_cards()[[1]]$info,
                 "Policy: 4.83 outcome units vs baseline: 1.50 outcome units", fixed = TRUE)
    session$setInputs(cmp_deviation = "median")
    session$flushReact()
    summary <- internals$headline_paired_effect_summary()
    expect_equal(summary$baseline[summary$scenario == name], 1.5)
    expect_equal(summary$value[summary$scenario == name], 10 / 3)
  })
})

test_that("future metric switches preserve scenario keys and reuse prepared suites", {
  hist <- make_step3_hist_fixture()
  scenarios <- make_step3_scenarios_fixture_multi()
  policy_scenarios <- scenarios
  for (i in seq_along(policy_scenarios)) {
    policy_scenarios[[i]]$pipelines <- lapply(policy_scenarios[[i]]$pipelines, function(pipe) {
      pipe$y_point <- pipe$y_point + .1
      pipe
    })
  }
  bh <- shiny::reactiveVal(hist)
  ph <- shiny::reactiveVal(hist)
  bsc <- shiny::reactiveVal(scenarios)
  psc <- shiny::reactiveVal(policy_scenarios)
  internals <- NULL
  shiny::testServer(function(input, output, session) {
    internals <<- .wire_results_pane(input, output, session, bh, bsc, ph, psc,
      selected_hist = shiny::reactiveVal(NULL), residuals = shiny::reactiveVal("none"))
  }, {
    session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none", cmp_pov_line = 3)
    session$elapse(500); session$flushReact()
    expect_named(internals$baseline_agg_scenarios(), names(scenarios))
    cache <- attr(internals$agg_cache_ws(), "suite_cache")
    baseline_key <- ls(cache)[grepl("baseline_scn", ls(cache), fixed = TRUE)][[1L]]
    policy_key <- ls(cache)[grepl("policy_scn", ls(cache), fixed = TRUE)][[1L]]
    baseline_suite <- get(baseline_key, envir = cache)
    policy_suite <- get(policy_key, envir = cache)
    for (method in c("gini", "median", "total", "prosperity_gap", "avg_poverty", "mean", "gini")) {
      session$setInputs(cmp_agg_method = method)
      session$flushReact()
      baseline <- internals$baseline_agg_scenarios()
      policy <- internals$policy_agg_scenarios()
      expect_named(baseline, names(scenarios))
      expect_named(policy, names(policy_scenarios))
      for (nm in names(scenarios)) {
        expect_identical(baseline[[nm]]$out, baseline_suite[[nm]][[method]])
        expect_identical(policy[[nm]]$out, policy_suite[[nm]][[method]])
        expect_true(all(is.finite(baseline[[nm]]$out$value)))
      }
      expect_identical(get(baseline_key, envir = cache), baseline_suite)
      expect_identical(get(policy_key, envir = cache), policy_suite)
      expect_true(all(paste("Baseline", names(scenarios), sep = "\r") %in%
        names(internals$matrix_transforms())))
      expect_true(all(names(scenarios) %in% internals$threshold_table()$scenario))
    }
    # Poverty methods share their own threshold-keyed suite.
    session$setInputs(cmp_agg_method = "headcount_ratio")
    session$elapse(500); session$flushReact()
    poverty_keys <- ls(cache)[grepl("baseline_scn", ls(cache), fixed = TRUE)]
    poverty_key <- setdiff(poverty_keys, baseline_key)[[1L]]
    poverty_suite <- get(poverty_key, envir = cache)
    for (method in c("gap", "fgt2", "headcount_ratio")) {
      session$setInputs(cmp_agg_method = method)
      session$flushReact()
      baseline <- internals$baseline_agg_scenarios()
      expect_named(baseline, names(scenarios))
      expect_named(internals$policy_agg_scenarios(), names(policy_scenarios))
      for (nm in names(scenarios)) {
        expect_identical(baseline[[nm]]$out, poverty_suite[[nm]][[method]])
      }
      expect_identical(get(poverty_key, envir = cache), poverty_suite)
      expect_true(all(names(scenarios) %in% internals$threshold_table()$scenario))
    }
  })
})

test_that("threshold edits retain only bounded aggregation suites", {
  hist <- make_step3_hist_fixture()
  scenarios <- make_step3_scenarios_fixture()
  internals <- NULL
  testServer(function(input, output, session) {
    internals <<- .wire_results_pane(input, output, session,
      reactiveVal(hist), reactiveVal(scenarios), reactiveVal(hist), reactiveVal(scenarios),
      selected_hist = reactiveVal(NULL), residuals = reactiveVal("none"))
  }, {
    session$setInputs(cmp_agg_method = "headcount_ratio", cmp_pov_line = 3)
    session$elapse(500); session$flushReact()
    first <- internals$baseline_agg_hist()
    cache <- attr(internals$agg_cache_ws(), "suite_cache")
    first_keys <- ls(cache)
    for (line in 4:14) {
      session$setInputs(cmp_pov_line = line)
      session$elapse(500); session$flushReact()
      internals$baseline_agg_hist()
      internals$policy_agg_hist()
      internals$baseline_agg_scenarios()
      internals$policy_agg_scenarios()
      expect_lte(length(ls(cache)), attr(cache, "max_entries"))
      expect_setequal(ls(cache), attr(cache, "keys"))
    }
    expect_false(any(first_keys %in% ls(cache)))
    session$setInputs(cmp_pov_line = 3)
    session$elapse(500); session$flushReact()
    expect_identical(internals$baseline_agg_hist(), first)
    expect_lte(length(ls(cache)), attr(cache, "max_entries"))
  })
})

test_that("shared pipeline table preserves historical and ensemble schemas", {
  hist_pipe <- make_step3_pipe_fixture(n = 80L, yrs = 2020:2021)
  hist <- aggregate_pipeline_table(
    hist_pipe,
    method    = "mean",
    weighted  = TRUE,
    residuals = "none",
    is_log    = FALSE,
    model_ids = "Historical",
    scenario  = "Historical"
  )
  direct <- aggregate_pipeline_per_year(
    hist_pipe, method = "mean", weighted = TRUE,
    residuals = "none", is_log = FALSE
  )

  expect_identical(hist$sim_year, vapply(direct, `[[`, integer(1L), "sim_year"))
  expect_equal(hist$value, vapply(direct, `[[`, numeric(1L), "value"))
  expect_true(all(vapply(hist$model_id, identical, logical(1L), "Historical")))
  expect_true(all(c("value_all_sd", "F_agg_all", "agg_method", "weighted") %in%
                    names(hist)))
  expect_true(all(hist$var_across == 0))
  expect_setequal(hist$scenario, "Historical")

  ensemble <- list(
    low  = hist_pipe,
    high = utils::modifyList(hist_pipe, list(y_point = hist_pipe$y_point + 0.2))
  )
  combined <- aggregate_pipeline_table(
    ensemble,
    method    = "mean",
    weighted  = TRUE,
    residuals = "none",
    is_log    = FALSE,
    model_ids = names(ensemble)
  )

  expect_true(all(vapply(combined$model_id, identical, logical(1L), names(ensemble))))
  expect_true(all(vapply(combined$value_all, length, integer(1L)) == 2L))
  expect_true(all(combined$var_across > 0))
})

# ---- INT-01: Step 3 filters and poverty line survive pane rebuilds ----------

test_that("Step 3 scenario filter grid and poverty line survive rebuilds (INT-01)", {
  skip_if_not_installed("shiny")

  bh  <- shiny::reactiveVal(make_step3_hist_fixture())
  ph  <- shiny::reactiveVal(make_step3_hist_fixture())
  bsc <- shiny::reactiveVal(make_step3_scenarios_fixture_multi())
  psc <- shiny::reactiveVal(make_step3_scenarios_fixture_multi())

  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim        = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim          = ph,
        policy_saved_scenarios   = psc,
        selected_hist            = shiny::reactiveVal(NULL),
        residuals                = shiny::reactiveVal("none")
      )
      NULL
    },
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      expect_setequal(
        internals$selected_scenario_names(),
        c("SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040")
      )

      # Poverty line: the input is a static conditionalPanel cell (Step 2
      # alignment). The run's value is the aggregation default while the
      # user has not edited it; an edited value survives method toggles and
      # re-runs (INT-01). Assert the debounced aggregation value (a second
      # settle lets the debounce timer fire after a republish).
      session$setInputs(cmp_agg_method = "gap"); settle()
      bh({ hs <- make_step3_hist_fixture(); hs$pov_line <- 2.15; hs }); settle()
      settle()
      expect_equal(internals$pov_line_val(), 2.15)
      session$setInputs(cmp_pov_line = 5.5); settle()
      expect_equal(internals$pov_line_val(), 5.5)
      bh(make_step3_hist_fixture()); settle()       # republish, no run value
      settle()
      expect_equal(internals$pov_line_val(), 5.5)   # user edit sticks
      session$setInputs(cmp_agg_method = "mean"); settle()
      expect_null(internals$pov_line_val())
      session$setInputs(cmp_agg_method = "gap"); settle()
      expect_equal(internals$pov_line_val(), 5.5)
    }
  )
})

# ---- INT-05: the historical label is bound to the simulated run --------------

test_that("Step 3 historical label comes from the run snapshot, not live selection", {
  skip_if_not_installed("shiny")

  bh <- shiny::reactiveVal({
    hs <- make_step3_hist_fixture()
    hs$hist_label <- "Hist run 1991-2020"
    hs
  })
  ph  <- shiny::reactiveVal(make_step3_hist_fixture())
  bsc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  psc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  sel_hist <- shiny::reactiveVal(data.frame(scenario_name = "Live selection"))

  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim        = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim          = ph,
        policy_saved_scenarios   = psc,
        selected_hist            = sel_hist,
        residuals                = shiny::reactiveVal("none")
      )
      NULL
    },
    {
      session$flushReact()
      # Snapshot label wins over the live selection (INT-05)
      expect_identical(internals$hist_label(), "Hist run 1991-2020")
      sel_hist(data.frame(scenario_name = "Live selection changed"))
      session$flushReact()
      expect_identical(internals$hist_label(), "Hist run 1991-2020")

      # Older in-memory result without the field falls back to the live
      # selection, and to "Historical" when there is none at all.
      bh(make_step3_hist_fixture())
      session$flushReact()
      expect_identical(internals$hist_label(), "Live selection changed")
      sel_hist(NULL)
      session$flushReact()
      expect_identical(internals$hist_label(), "Historical")
    }
  )
})

# ---- INT-08: stale banner on the Step 3 results pane -------------------------

test_that("Step 3 results pane shows the stale banner while stale", {
  skip_if_not_installed("shiny")

  bh  <- shiny::reactiveVal(make_step3_hist_fixture())
  ph  <- shiny::reactiveVal(make_step3_hist_fixture())
  bsc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  psc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  stale_flag <- shiny::reactiveVal(FALSE)

  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim        = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim          = ph,
        policy_saved_scenarios   = psc,
        selected_hist            = shiny::reactiveVal(NULL),
        residuals                = shiny::reactiveVal("none"),
        stale                    = stale_flag
      )
      NULL
    },
    {
      session$flushReact()
      stale_flag(TRUE); session$flushReact()
      html <- paste(as.character(session$output$stale_banner_ui), collapse = " ")
      expect_match(html, "Results are out of date", fixed = TRUE)
      expect_match(html, "Step 3 policy results", fixed = TRUE)

      stale_flag(FALSE); session$flushReact()
      html <- paste(as.character(session$output$stale_banner_ui), collapse = " ")
      expect_identical(nchar(html), 0L)
    }
  )
})

# ---- Regression: threshold rows stay unique with one admissible RP ----------

test_that("threshold table has unique keys with two years and two members", {
  skip_if_not_installed("shiny")

  bh  <- shiny::reactiveVal(make_step3_hist_fixture())
  ph  <- shiny::reactiveVal(make_step3_hist_fixture())
  bsc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  psc <- shiny::reactiveVal(make_step3_scenarios_fixture())

  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim        = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim          = ph,
        policy_saved_scenarios   = psc,
        selected_hist            = shiny::reactiveVal(NULL),
        residuals                = shiny::reactiveVal("none")
      )
      NULL
    },
    {
      session$flushReact()
      tbl <- internals$threshold_table()
      key <- c("scenario", "source", "Estimate", "n_obs", "rp_label")
      hit <- duplicated(tbl[key]) | duplicated(tbl[key], fromLast = TRUE)
      expect_false(any(hit))

      # The default no-spread state carries one ensemble median row instead of
      # duplicate lower/upper P50 rows, twice (Baseline + Policy), plus the
      # historical triple per arm.
      expect_equal(nrow(tbl), 18L)
      expect_setequal(
        tbl$Estimate[tbl$scenario != "Historical"],
        c("Central (P50)", "Coef P10", "Coef P90",
          "Ensemble P50", "Pooled P10", "Pooled P90")
      )
    }
  )
})

test_that("Step 3 agg cache: deviation changes reuse cache; method/pov-line key entries", {
  skip_if_not_installed("shiny")

  bh  <- shiny::reactiveVal(make_step3_hist_fixture())
  ph  <- shiny::reactiveVal(make_step3_hist_fixture())
  bsc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  psc <- shiny::reactiveVal(make_step3_scenarios_fixture())

  internals <- NULL
  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim        = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim          = ph,
        policy_saved_scenarios   = psc,
        selected_hist            = shiny::reactiveVal(NULL),
        residuals                = shiny::reactiveVal("none")
      )
      NULL
    },
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      # Count only the baseline_hist cache entries; the pane's other
      # consumers (threshold tables, policy arm) populate other keys.
      bh_keys <- function() {
        grep("^baseline_hist\\r", ls(envir = internals$agg_cache_ws()), value = TRUE)
      }
      session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none")
      session$setInputs(cmp_pov_line = 3.00); settle()

      # First pass populates one baseline_hist entry
      h1 <- internals$baseline_agg_hist()$out
      expect_length(bh_keys(), 1L)

      # Deviation change: NO new entries, identical object served from cache
      session$setInputs(cmp_deviation = "mean"); settle()
      h2 <- internals$baseline_agg_hist()$out
      expect_length(bh_keys(), 1L)
      expect_identical(h1, h2)

      # Method change: separate key, old entry retained
      session$setInputs(cmp_agg_method = "median"); settle()
      m1 <- internals$baseline_agg_hist()$out
      expect_false(identical(h1, m1))
      expect_length(bh_keys(), 2L)

      # Back to mean: served from the original cache entry, bit-identical
      session$setInputs(cmp_agg_method = "mean"); settle()
      h3 <- internals$baseline_agg_hist()$out
      expect_identical(h1, h3)
      expect_length(bh_keys(), 2L)

      # Poverty line (poverty methods only): new keys per line, old kept
      session$setInputs(cmp_agg_method = "gap"); settle()
      g1 <- internals$baseline_agg_hist()$out
      expect_length(bh_keys(), 3L)
      session$setInputs(cmp_pov_line = 5.50); settle()
      g2 <- internals$baseline_agg_hist()$out
      expect_false(identical(g1, g2))
      expect_length(bh_keys(), 4L)
      # Step 3 schema: `out` carries a scalar `value` per year directly
      expect_true(all(g2$value > g1$value))
    }
  )
})

test_that("Step 3 aggregation cache is bounded, LRU, and value-preserving", {
  skip_if_not_installed("shiny")
  bh <- shiny::reactiveVal(make_step3_hist_fixture())
  ph <- shiny::reactiveVal(make_step3_hist_fixture())
  bsc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  psc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  shiny::testServer(function(input, output, session) {
    internals <<- .wire_results_pane(input, output, session, bh, bsc, ph, psc,
      selected_hist = shiny::reactiveVal(NULL))
  }, {
    ws <- internals$agg_cache_ws()
    keys <- paste0("direct-", seq_len(32L))
    values <- lapply(seq_along(keys), function(i) list(value = i))
    for (i in seq_along(keys)) internals$agg_cache_put(ws, keys[[i]], values[[i]])
    expect_length(ls(envir = ws), 32L)
    expect_identical(internals$agg_cache_get(ws, keys[[1L]]), values[[1L]])
    internals$agg_cache_put(ws, "direct-33", list(value = 33L))
    expect_length(ls(envir = ws), 32L)
    expect_true(is.null(internals$agg_cache_get(ws, keys[[2L]])))
    expect_identical(internals$agg_cache_get(ws, keys[[1L]]), values[[1L]])
    expect_identical(internals$agg_cache_get(ws, "direct-33"), list(value = 33L))
  })
})

test_that("Step 3 shared cache keeps baseline and policy historical arms distinct", {
  skip_if_not_installed("shiny")
  baseline <- make_step3_hist_fixture()
  policy <- baseline
  policy$pipeline$y_point <- policy$pipeline$y_point + 5
  bh <- shiny::reactiveVal(baseline)
  ph <- shiny::reactiveVal(policy)
  bsc <- shiny::reactiveVal(list())
  psc <- shiny::reactiveVal(list())
  shared <- new_shared_aggregation_cache()

  shiny::testServer(function(input, output, session) {
    internals <<- .wire_results_pane(
      input, output, session,
      baseline_hist_sim = bh,
      baseline_saved_scenarios = bsc,
      policy_hist_sim = ph,
      policy_saved_scenarios = psc,
      selected_hist = shiny::reactiveVal(NULL),
      residuals = shiny::reactiveVal("none"),
      aggregation_cache = shared
    )
  }, {
    session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none")
    session$flushReact()
    baseline_out <- internals$baseline_agg_hist()$out
    policy_out <- internals$policy_agg_hist()$out
    expect_true(all(policy_out$value > baseline_out$value))
    expect_length(shared$keys, 2L)
  })
})

test_that("historical matrix transforms use the canonical cache key and preserve values", {
  skip_if_not_installed("shiny")
  hist <- make_step3_hist_fixture()
  hist$hist_label <- "Hist run 1991-2020"
  bh <- shiny::reactiveVal(hist)
  ph <- shiny::reactiveVal(make_step3_hist_fixture())
  bsc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  psc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  internals <- NULL
  shiny::testServer(function(input, output, session) {
    internals <<- .wire_results_pane(input, output, session, bh, bsc, ph, psc,
      selected_hist = shiny::reactiveVal(NULL), residuals = shiny::reactiveVal("none"))
  }, {
    session$flushReact()
    cached <- internals$matrix_transforms()
    key <- paste("Baseline", "Historical", sep = "\r")
    expected <- by_model_matrix(internals$baseline_agg_hist()$out)
    expect_true(key %in% names(cached))
    expect_false(paste("Baseline", "Hist run 1991-2020", sep = "\r") %in% names(cached))
    expect_identical(internals$matrix_transform(internals$baseline_agg_hist()$out,
      "Baseline", "Historical"), cached[[key]])
    expect_identical(cached[[key]]$vals, expected$vals)
    expect_identical(cached[[key]]$sds, expected$sds)
  })
})

test_that("switching directly to a poverty method always has a poverty line", {
  skip_if_not_installed("shiny")

  bh  <- shiny::reactiveVal(make_step3_hist_fixture())
  ph  <- shiny::reactiveVal(make_step3_hist_fixture())
  bsc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  psc <- shiny::reactiveVal(make_step3_scenarios_fixture())

  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim = ph,
        policy_saved_scenarios = psc,
        selected_hist = shiny::reactiveVal(NULL),
        residuals = shiny::reactiveVal("none")
      )
    },
    {
      session$setInputs(cmp_agg_method = "mean", cmp_pov_line = 3.00)
      session$elapse(500)
      session$flushReact()

      # Do not elapse the input debounce: this reproduces the UI transition
      # that previously paired headcount_ratio with a stale NULL line.
      session$setInputs(cmp_agg_method = "headcount_ratio")
      session$flushReact()

      expect_equal(internals$pov_line_val(), 3.00)
      expect_no_error(internals$baseline_agg_hist())
      expect_no_error(internals$policy_agg_hist())
      expect_no_error(internals$baseline_agg_scenarios())
      expect_no_error(internals$policy_agg_scenarios())
    }
  )
})

test_that("Step 3 agg cache is invalidated on simulation republish; recompute identical", {
  skip_if_not_installed("shiny")

  bh  <- shiny::reactiveVal(make_step3_hist_fixture())
  ph  <- shiny::reactiveVal(make_step3_hist_fixture())
  bsc <- shiny::reactiveVal(make_step3_scenarios_fixture())
  psc <- shiny::reactiveVal(make_step3_scenarios_fixture())

  internals <- NULL
  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim        = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim          = ph,
        policy_saved_scenarios   = psc,
        selected_hist            = shiny::reactiveVal(NULL),
        residuals                = shiny::reactiveVal("none")
      )
      NULL
    },
    {
      session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none")
      session$setInputs(cmp_pov_line = 3.00)
      session$elapse(500); session$flushReact()

      h1 <- internals$baseline_agg_hist()$out
      expect_length(ls(envir = internals$agg_cache_ws()), 4L)

      # Re-publish the simulation (same content): fresh cache env, entries
      # are recomputed deterministically and stay bit-identical (seeded
      # residual substreams - Batch C).
      bh(make_step3_hist_fixture())
      session$elapse(500); session$flushReact()

      h2 <- internals$baseline_agg_hist()$out
      expect_length(ls(envir = internals$agg_cache_ws()), 4L)
      expect_identical(h1, h2)
    }
  )
})

test_that("cached scenario aggregation is identical to uncached recomputation", {
  skip_if_not_installed("shiny")

  hist <- make_step3_hist_fixture()
  sc   <- make_step3_scenarios_fixture()

  bh  <- shiny::reactiveVal(hist)
  ph  <- shiny::reactiveVal(hist)
  bsc <- shiny::reactiveVal(sc)
  psc <- shiny::reactiveVal(sc)

  internals <- NULL
  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim        = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim          = ph,
        policy_saved_scenarios   = psc,
        selected_hist            = shiny::reactiveVal(NULL),
        residuals                = shiny::reactiveVal("none")
      )
      NULL
    },
    {
      session$setInputs(cmp_agg_method = "gap", cmp_deviation = "none",
                        cmp_pov_line = 3.00)
      session$elapse(500); session$flushReact()

      cached_hist <- internals$baseline_agg_hist()$out
      agg_scn     <- internals$baseline_agg_scenarios()
      # NB: the aggregation must keep the scenario names - every renderer
      # below selects scenarios by name.
      expect_identical(names(agg_scn), names(sc))
      cached_scn  <- agg_scn[["SSP2-4.5 / 2030-2040"]]$out
      expect_false(is.null(cached_scn))
      n_after_first <- length(ls(envir = internals$agg_cache_ws()))
      expect_true(n_after_first >= 4L)

      # Invalidate by re-publishing; deterministic recompute must match
      bh(make_step3_hist_fixture())
      bsc(make_step3_scenarios_fixture())
      session$elapse(500); session$flushReact()

      fresh_hist  <- internals$baseline_agg_hist()$out
      fresh_scn   <- internals$baseline_agg_scenarios()[["SSP2-4.5 / 2030-2040"]]$out

      expect_identical(cached_hist, fresh_hist)
      expect_identical(cached_scn,  fresh_scn)
      expect_length(ls(envir = internals$agg_cache_ws()), n_after_first)
    }
  )
})

test_that("scenario aggregation preserves scenario names across both arms", {
  skip_if_not_installed("shiny")

  # Regression: make_agg_scenarios() iterates with seq_along(sc) so the
  # failure ledger can name the dropped scenario; lapply over the indices
  # silently dropped the list names, so every Step 3 renderer (which selects
  # scenarios by name) showed only the historical series.
  hist <- make_step3_hist_fixture()
  sc   <- make_step3_scenarios_fixture_multi()

  bh  <- shiny::reactiveVal(hist)
  ph  <- shiny::reactiveVal(hist)
  bsc <- shiny::reactiveVal(sc)
  psc <- shiny::reactiveVal(sc)

  internals <- NULL
  shiny::testServer(
    function(input, output, session) {
      internals <<- .wire_results_pane(
        input, output, session,
        baseline_hist_sim        = bh,
        baseline_saved_scenarios = bsc,
        policy_hist_sim          = ph,
        policy_saved_scenarios   = psc,
        selected_hist            = shiny::reactiveVal(NULL),
        residuals                = shiny::reactiveVal("none")
      )
      NULL
    },
    {
      session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none")
      session$elapse(500); session$flushReact()

      agg_b <- internals$baseline_agg_scenarios()
      agg_p <- internals$policy_agg_scenarios()
      expect_identical(names(agg_b), names(sc))
      expect_identical(names(agg_p), names(sc))
      for (nm in names(sc)) {
        expect_false(is.null(agg_b[[nm]]$out), info = nm)
        expect_false(is.null(agg_p[[nm]]$out), info = nm)
        expect_true(nrow(agg_b[[nm]]$out) > 0L, info = nm)
      }
    }
  )
})
