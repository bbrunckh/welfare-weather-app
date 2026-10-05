library(testthat)
library(shiny)


test_that("policy candidate scans preserve generic diagnostics and ordering", {
  baseline <- data.frame(
    id = 1:4,
    electricity = c(0L, 1L, NA_integer_, 0L),
    region = factor(c("a", "a", "b", "b")),
    welfare = c(1, 2, 3, 4),
    extra = c(10, 20, 30, 40)
  )
  policy <- baseline
  policy$electricity <- c(1L, 1L, NA_integer_, 0L)
  policy$region <- factor(c("b", "a", "b", "b"), levels = c("b", "a"))
  policy$extra <- policy$extra + 1

  expect_identical(
    detect_manipulated_vars(baseline, policy),
    c("electricity", "region", "extra")
  )
  expect_identical(
    detect_manipulated_vars(
      baseline, policy, candidates = c("welfare", "electricity")
    ),
    "electricity"
  )
  expect_true(.scenario_has_effect(baseline, policy))
  expect_true(.scenario_has_effect(
    baseline, policy, candidates = "electricity"
  ))
  expect_false(.scenario_has_effect(
    baseline, policy, candidates = "welfare"
  ))

  shorter <- policy[-1, ]
  expect_identical(
    detect_manipulated_vars(
      baseline, shorter, candidates = c("welfare", "electricity")
    ),
    c("electricity", "welfare")
  )
})


test_that("candidate scans preserve factor attributes and NA differences", {
  baseline <- data.frame(
    electricity = factor(c("off", NA, "on"), levels = c("off", "on")),
    unrelated = 1:3
  )
  policy <- baseline
  levels(policy$electricity) <- c("disabled", "enabled")
  policy$unrelated <- 4:6

  expect_identical(
    detect_manipulated_vars(
      baseline, policy, candidates = "electricity"
    ),
    "electricity"
  )

  numeric_base <- data.frame(internet = c(0, NA, 1), unrelated = 1:3)
  numeric_policy <- numeric_base
  numeric_policy$internet[2] <- 0
  expect_identical(
    detect_manipulated_vars(
      numeric_base, numeric_policy, candidates = "internet"
    ),
    "internet"
  )
})


test_that("active lever candidates are conservative and model gated", {
  candidates <- .policy_candidate_cols(
    infra = list(elec_universal = FALSE, elec_access_change_pct = 10),
    digital = list(internet_universal = TRUE),
    labor = list(employment_change_pp = 5, sector_manufacturing = 0,
                 sector_services = 0),
    education = list(primary_universal = FALSE,
                     primary_access_change_pct = 0),
    model_vars = c("temp:electricity", "internet", "employed",
                   "selfemployed", "unemployed")
  )
  expect_identical(
    candidates,
    c("electricity", "internet", "employed", "selfemployed", "unemployed")
  )
})


test_that("diagnostic snapshot caches transfer-adjusted summaries and vectors", {
  baseline <- data.frame(
    welfare = c(1, 2, 3), electricity = c(0L, 1L, 0L),
    ignored = c(5, 6, 7), weight = c(1, 2, 1), hhsize = c(2, 2, 1)
  )
  policy <- baseline
  policy$electricity <- 1L
  policy$ignored <- 0
  policy[[SP_TRANSFER_COL]] <- c(0.5, 0, 1)

  snapshot <- .policy_diagnostics_snapshot(
    baseline, policy, outcome = "welfare", analysis_unit = "hh",
    candidates = c("electricity", "welfare")
  )
  expect_identical(snapshot$manipulated_vars, c("welfare", "electricity"))
  expect_identical(snapshot$input_summary$variable,
                   c("welfare", "electricity"))
  expect_identical(snapshot$baseline_values$electricity,
                   baseline$electricity)
  expect_identical(snapshot$policy_values$welfare,
                   policy$welfare + policy[[SP_TRANSFER_COL]])
  expect_false("baseline_svy" %in% names(snapshot))
  expect_false("policy_svy" %in% names(snapshot))
  expect_identical(snapshot$analysis_unit, "hh")
  treated <- snapshot$treatment_matrix[snapshot$treatment_matrix$treated_policy, ]
  received <- policy[[SP_TRANSFER_COL]] > 0
  expect_equal(sum(treated$weighted_households),
    sum(baseline$weight[received] / baseline$hhsize[received]))
  sp_component <- snapshot$component_matrix[
    snapshot$component_matrix$component == "Social protection", ]
  expect_equal(sp_component$weighted_households,
    sum(baseline$weight[received] / baseline$hhsize[received]))
  expect_true("Households represented" %in%
    names(.policy_treatment_table_raw(snapshot$treatment_matrix, "hh")))
  component_table <- .policy_component_table_raw(snapshot$component_matrix, "hh")
  expect_true("Households represented" %in% names(component_table))
  expect_true("Sample households affected / covered" %in% names(component_table))

  policy$electricity[] <- 0L
  expect_identical(snapshot$policy_values$electricity, c(1L, 1L, 1L))
})


test_that("diagnostics module reads one published snapshot without rescanning", {
  snapshot <- reactiveVal(list(
    status = NULL,
    manipulated_vars = "electricity",
    baseline_values = list(electricity = c(0L, 1L)),
    policy_values = list(electricity = c(1L, 1L)),
    transfer_sum = 0,
    transfer_pp = 0,
    changed_counts = c(electricity = 1L),
    input_summary = data.frame(
      variable = "electricity", mean_baseline = 0.5, mean_policy = 1,
      delta_mean = 0.5, sd_baseline = sqrt(0.5), sd_policy = 0
    ),
    analysis_unit = "hh",
    treatment_matrix = data.frame(),
    component_matrix = data.frame()
  ))
  run_id <- reactiveVal(1L)

  local_mocked_bindings(
    detect_manipulated_vars = function(...) stop("unexpected rescan"),
    policy_input_diagnostics = function(...) stop("unexpected rescan")
  )
  testServer(
    mod_3_08_diagnostics_server,
    args = list(
      id = "diag", baseline_svy = reactive(stop("unexpected frame read")),
      policy_svy = reactive(stop("unexpected frame read")),
      diagnostic_summary = snapshot, sim_run_id = run_id,
      tabset_id = "tabs"
    ),
    {
      session$flushReact()
      expect_identical(diag_data(), snapshot())
      expect_silent(output$diag_summary_table)
      expect_silent(output$transfer_summary_ui)
    }
  )
})


test_that("failed annual policy runs retain all previously published results", {
  trigger <- reactiveVal(NULL)
  fail_decomp <- FALSE
  svy <- data.frame(
    welfare = c(1, 2), temp = c(20, 21), electricity = c(0L, 1L)
  )
  policy <- transform(svy, electricity = 1L)
  hs <- list(
    svy = svy, so = list(name = "welfare", transform = "none"),
    pipeline = list(y_point = c(1, 2), svy_row_id = 1:2,
                    policy_correction = list(version = "row_aligned_annual_v1")),
    weather_raw = data.frame(temp = 20:21), residuals = "original",
    shared_context = list(), .sig = "step2"
  )
  policy_hs <- hs
  policy_hs$pipeline$y_point <- c(2, 4)
  old_scenarios <- list()
  context_for_run <- function(...) list(
    run_identity = "test-run", adverse_bases = list()
  )

  local_mocked_bindings(
    apply_policy_to_svy = function(...) policy,
    .build_decomposition_context = context_for_run,
    apply_policy_delta_to_baseline = function(...)
      list(hist_sim = policy_hs, saved_scenarios = old_scenarios,
           annual_channels = new.env(parent = emptyenv()),
           decomp_scenarios = data.frame(run = "first"),
           correction_version = "row_aligned_annual_v1"),
    .finalize_decomposition_context = function(context, ...) context,
    .decompose_policy_effect_run = function(...) {
      if (fail_decomp) stop("forced decomposition failure")
      data.frame(delta_total = c(0, 1))
    },
    .policy_diagnostics_snapshot = function(...)
      list(run = "first", status = NULL)
  )

  testServer(
    mod_3_06_policy_sim_server,
    args = list(
      id = "policy", survey_weather = reactive(svy),
      infra_scenario = reactive(list(elec_access_change_pct = 100)),
      selected_model = reactive(list(hh_covariates = "electricity")),
      model_fit = reactive(list(engine = "fixest", weather_terms = "temp")),
      selected_weather = reactive(data.frame(name = "temp")),
      hist_sim = reactive(hs), run_trigger = trigger
    ),
    {
      session$flushReact()
      trigger(1L)
      session$flushReact()
      expect_identical(sim_run_id(), 1L)
      prior <- diagnostic_summary_rv()
      expect_identical(prior$run, "first")
      prior_policy <- policy_hist_sim_rv()
      prior_baseline <- baseline_hist_sim_rv()
      prior_decomp <- decomp_scenarios_rv()

      fail_decomp <<- TRUE
      trigger(2L)
      session$flushReact()
      expect_identical(run_status(), "failure")
      expect_identical(sim_run_id(), 1L)
      expect_identical(diagnostic_summary_rv(), prior)
      expect_identical(policy_hist_sim_rv(), prior_policy)
      expect_identical(baseline_hist_sim_rv(), prior_baseline)
      expect_identical(decomp_scenarios_rv(), prior_decomp)
    }
  )
})
