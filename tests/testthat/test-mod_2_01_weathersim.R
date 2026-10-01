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

test_that("historical preview is separate from committed results and clears on stop", {
  survey <- data.frame(hhid = 1:2, code = "AAA", year = 2020L,
    survname = "SRV", loc_id = "L1", int_month = 1:2,
    welfare = 1:2, temp = 3:4)
  testServer(mod_2_01_weathersim_server, args = list(
    id = "sim", connection_params = reactiveVal(list(type = "local", path = tempdir())),
    selected_outcome = reactiveVal(data.frame(name = "welfare")),
    selected_weather = reactiveVal(data.frame(name = "temp")),
    selected_surveys = reactiveVal(data.frame(code = "AAA", year = 2020L,
      survname = "SRV", source = "src", economy = "A")),
    survey_weather = reactiveVal(survey),
    model_fit = reactiveVal(list(engine = "fixest", .sig = list(fit = 1L)))
  ), {
    old <- list(.sig = list(previous = TRUE))
    hist_sim(old)
    historical_preview(list(data = data.frame(sim_year = 2000:2001, value = c(2, 3)),
      metadata = list(weighted = TRUE),
      labels = list(historical = "1991-2020", baseline = "AAA 2020")))
    session$flushReact()
    expect_identical(hist_sim(), old)
    expect_identical(saved_scenarios(), list())
    preview_html <- paste(as.character(output$historical_preview_ui$html), collapse = "")
    expect_match(preview_html, "Historical preview", fixed = TRUE)
    expect_match(preview_html, "previous completed run", fixed = TRUE)
    expect_false(grepl("download|Reactable", preview_html))
    ended <- session_callback(function(...) stop("Must not execute after disconnect"))
    callback_env <- parent.env(environment(ended))
    callback_env$session_ended <- TRUE
    expect_identical(ended(), FALSE)
    callback_env$session_ended <- FALSE
    async_job_id("missing-test-job")
    session$setInputs(stop_sim = 1L)
    expect_null(historical_preview())
    expect_identical(hist_sim(), old)
    expect_identical(run_status(), "cancelled")
    callback_env$pending_submission <- TRUE
    before_sequence <- callback_env$submission_sequence
    run_status("queued")
    session$setInputs(stop_sim = 2L)
    expect_false(callback_env$pending_submission)
    expect_gt(callback_env$submission_sequence, before_sequence)
    expect_identical(run_status(), "cancelled")
  })
})
