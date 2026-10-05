library(testthat)
library(shiny)

test_that("baseline survey metadata uses exact code-year selection", {
  ss <- data.frame(code = c("AAA", "AAA", "AAA", "BBB"),
                   year = c(2019L, 2020L, 2020L, 2021L),
                   survname = c("OLD", "MAIN", "ALT", "OTHER"),
                   source = c("old", "main", "alt", "other"),
                   stringsAsFactors = FALSE)
  out <- .step2_filter_baseline_surveys(ss, c("AAA|2020", "BBB|2021"))
  expect_identical(out, ss[c(2L, 3L, 4L), , drop = FALSE])
  expect_identical(out$survname, c("MAIN", "ALT", "OTHER"))
  expect_identical(.step2_filter_baseline_surveys(ss, character(0)), ss)
})

test_that("every Step 2 simulation dependency marks published results stale", {
  survey_version <- reactiveVal(1L)
  model_fit <- reactiveVal(list(engine = "fixest", .sig = list(fit = 1L)))
  survey <- data.frame(hhid = 1:2, code = "AAA", year = 2020L,
                       survname = "SRV", loc_id = "L1", int_month = 1:2,
                       welfare = 1:2, temp = 3:4)
  selected_surveys <- reactiveVal(data.frame(
    code = "AAA", year = 2020L, survname = "SRV", source = "src",
    economy = "A", stringsAsFactors = FALSE
  ))
  testServer(mod_2_01_weathersim_server, args = list(
    id = "sim", connection_params = reactiveVal(list(type = "local", path = tempdir())),
    selected_outcome = reactiveVal(data.frame(name = "welfare")),
    selected_weather = reactiveVal(data.frame(name = "temp")),
    selected_surveys = selected_surveys, survey_weather = reactiveVal(survey),
    model_fit = model_fit, survey_version = survey_version
  ), {
    session$setInputs(hist_years = c(1991, 2020), climate = "ssp3_7_0",
      baseline_survey = "AAA|2020", residuals = "original",
      include_coef_uncertainty = TRUE, propagate_all_covariate_uncertainty = FALSE,
      fut_period_1 = c(2025, 2035), fut_period_2 = c(2015, 2015), fut_period_3 = c(2015, 2015))
    session$flushReact()
    publish <- function() {
      hist_sim(list(.sig = isolate(live_sim_sig())))
      session$flushReact()
      expect_false(sim_stale())
    }
    expect_stale_after <- function(change, label) {
      publish(); before <- isolate(live_sim_sig()); change(); session$flushReact()
      expect_false(identical(isolate(live_sim_sig()), before), info = label)
      expect_true(sim_stale(), info = label)
    }
    expect_stale_after(function() model_fit(list(engine = "fixest", .sig = list(fit = 2L))), "model fit")
    expect_stale_after(function() survey_version(2L), "survey version")
    expect_stale_after(function() { x <- selected_surveys(); x$source <- "replacement_source"; selected_surveys(x) }, "survey metadata source")
    expect_stale_after(function() session$setInputs(hist_years = c(1990, 2020)), "historical years")
    expect_stale_after(function() session$setInputs(climate = "ssp2_4_5"), "climate")
    expect_stale_after(function() session$setInputs(fut_period_1 = c(2025, 2037)), "future periods")
    expect_stale_after(function() session$setInputs(baseline_survey = character(0)), "baseline")
    expect_stale_after(function() session$setInputs(residuals = "resample"), "residuals")
    expect_stale_after(function() session$setInputs(include_coef_uncertainty = FALSE), "coefficient uncertainty")
    expect_stale_after(function() session$setInputs(propagate_all_covariate_uncertainty = TRUE), "covariate uncertainty")
  })
})

test_that("scenario labels follow run order and match worker keys", {
  labels <- .step2_scenario_labels(c("ssp2_4_5", "ssp5_8_5"),
    list(c(2030, 2040), c(2050, 2060)))
  expect_identical(labels, c("SSP2-4.5 / 2030-2040", "SSP2-4.5 / 2050-2060",
    "SSP5-8.5 / 2030-2040", "SSP5-8.5 / 2050-2060"))
  expect_identical(.step2_scenario_labels(character(0), list(c(2030, 2040))),
    character(0))
})

test_that("ETA is NULL until a group completes, then mean per group x remaining", {
  now <- as.POSIXct("2026-01-01 00:10:00", tz = "UTC")
  base <- now - 300
  expect_null(.step2_live_eta(0L, 4L, base, now))
  expect_null(.step2_live_eta(4L, 4L, base, now))
  expect_null(.step2_live_eta(NULL, 4L, base, now))
  expect_equal(.step2_live_eta(2L, 4L, base, now), 300)
  expect_identical(.step2_eta_text(300), "about 5 min remaining")
  expect_identical(.step2_eta_text(20), "under a minute remaining")
  expect_null(.step2_eta_text(NULL))
})

test_that("progress fraction counts historical, finished groups and models", {
  expect_null(.step2_live_pct(list()))
  expect_equal(.step2_live_pct(list(has_hist = TRUE, groups_total = 3L,
    groups_done = 0L)), 1 / 4)
  v <- list(has_hist = TRUE, groups_total = 3L, groups_done = 1L,
    current_label = "B", member_index = 2L, period_members = 4L,
    done_labels = "A")
  expect_equal(.step2_live_pct(v), (1 + 1 + 0.5) / 4)
  v$current_label <- "A"  # a just-finished group does not count twice
  expect_equal(.step2_live_pct(v), 2 / 4)
  html <- as.character(.step2_progress_ui(modifyList(v, list(
    scenario_labels = c("A", "B"), current_label = "B"))))
  expect_match(html, "2 / 4 climate models", fixed = TRUE)
  expect_match(html, "Historical", fixed = TRUE)
})

test_that("live_run lifecycle: submit, partials, adoption, cancel", {
  survey <- data.frame(hhid = 1:2, code = "AAA", year = 2020L,
    survname = "SRV", loc_id = "L1", int_month = 1:2,
    welfare = 1:2, temp = 3:4)
  captured <- new.env()
  local_mocked_bindings(
    .wise_step2_async_enabled = function(...) TRUE,
    .wise_step2_async_submit = function(snapshot, ...) {
      captured$snapshot <- snapshot
      captured$args <- list(...)
      list2env(list(id = "mock-job", generation = list(...)$generation,
        dependency_signature = list(...)$dependency_signature, settled = FALSE),
        parent = emptyenv())
    },
    .wise_step2_async_commit = function(job, commit_fn) isTRUE(commit_fn()),
    .wise_step2_async_read_manifest = function(manifest, job) list(
      hist_sim_result = list(), new_scenarios = list(a = 1), total_runs = 1L,
      weather_store = NULL, failures = list()),
    .wise_step2_async_cancel = function(...) invisible(TRUE)
  )
  testServer(mod_2_01_weathersim_server, args = list(
    id = "sim", connection_params = reactiveVal(list(type = "local", path = tempdir())),
    selected_outcome = reactiveVal(data.frame(name = "welfare")),
    selected_weather = reactiveVal(data.frame(name = "temp", units = "C")),
    selected_surveys = reactiveVal(data.frame(code = "AAA", year = 2020L,
      survname = "SRV", source = "src", economy = "A")),
    survey_weather = reactiveVal(survey),
    model_fit = reactiveVal(list(engine = "fixest", .sig = list(fit = 1L))),
    display_settings = reactive(list(method = "mean", pov_line = 3, bandwidth_p0 = 0.05))
  ), {
    session$setInputs(hist_years = c(1991, 2020), climate = c("ssp2_4_5", "ssp5_8_5"),
      baseline_survey = "AAA|2020", residuals = "original",
      include_coef_uncertainty = TRUE, propagate_all_covariate_uncertainty = FALSE,
      fut_period_1 = c(2030, 2040), fut_period_2 = c(2050, 2060),
      fut_period_3 = c(2015, 2015))
    session$flushReact()
    adopted_partials(list(stale = TRUE))
    submit_step2_async()
    lr <- live_run()
    expect_null(adopted_partials())
    expect_identical(lr$groups_total, 4L)
    expect_identical(lr$scenario_labels[c(1, 2, 3)],
      c("SSP2-4.5 / 2030-2040", "SSP2-4.5 / 2050-2060", "SSP5-8.5 / 2030-2040"))
    expect_identical(lr$display$method, "mean")
    expect_identical(captured$snapshot$input$display$method, "mean")
    expect_length(lr$partials$scenarios, 0L)

    job <- list2env(list(generation = lr$generation,
      dependency_signature = lr$dependency_signature), parent = emptyenv())
    partial <- function(kind, label) list(schema = 1L, kind = kind, label = label,
      ordinal = 0L, groups_total = 4L)
    captured$args$on_partial(partial("historical", "Historical"), job)
    expect_false(is.null(live_run()$partials$historical))
    captured$args$on_progress(list(phase = "group_completed", elapsed = 10,
      groups_done = 1L, groups_total = 4L, current_label = lr$scenario_labels[2],
      member_index = 1L, period_members = 3L), job)
    expect_false(is.null(live_run()$eta_seconds))
    captured$args$on_partial(partial("scenario", lr$scenario_labels[1]), job)
    expect_identical(names(live_run()$partials$scenarios), lr$scenario_labels[1])
    # stale-generation partials are dropped
    stale_job <- list2env(list(generation = -1L,
      dependency_signature = lr$dependency_signature), parent = emptyenv())
    captured$args$on_partial(partial("scenario", "zzz"), stale_job)
    expect_length(live_run()$partials$scenarios, 1L)

    captured$args$on_result(list(status = "succeeded"), job)
    expect_null(live_run())
    expect_identical(adopted_partials()$dependency_signature, lr$dependency_signature)
    expect_false(is.null(adopted_partials()$partials$historical))
    expect_identical(names(adopted_partials()$partials$scenarios), lr$scenario_labels[1])
    expect_identical(saved_scenarios(), list(a = 1))

    # cancel clears the live run
    run_status("idle")
    sim_guard$end()
    submit_step2_async()
    expect_false(is.null(live_run()))
    async_job_id("mock-job")
    session$setInputs(stop_sim = 1L)
    expect_null(live_run())
    expect_identical(run_status(), "cancelled")
  })
})
