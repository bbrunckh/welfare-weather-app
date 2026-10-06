library(testthat)
library(shiny)

pipeline_keys <- c("load_survey", "load_weather", "step1", "step2", "step3")

pipeline_fixture <- function() {
  triggers <- stats::setNames(
    lapply(pipeline_keys, function(key) reactiveVal(NULL)), pipeline_keys
  )
  generations <- stats::setNames(
    lapply(pipeline_keys, function(key) reactiveVal(0L)), pipeline_keys
  )
  statuses <- stats::setNames(lapply(pipeline_keys, function(x) reactiveVal("idle")), pipeline_keys)
  list(
    triggers = triggers,
    results = stats::setNames(lapply(pipeline_keys, function(key) list(
      generation = generations[[key]], status = statuses[[key]]
    )), pipeline_keys),
    generations = generations,
    statuses = statuses
  )
}

pipeline_import_file <- function(inputs) {
  file <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(wiseapp_config_version = 1L, inputs = inputs),
    file, auto_unbox = TRUE, pretty = TRUE
  )
  file
}

test_that("pipeline runner executes stages in order", {
  withr::local_options(wiseapp.pipeline_settle = 0)
  testServer(function(input, output, session) {
    f <- pipeline_fixture()
    runner <- pipeline_runner(f$triggers, f$results)
    session$userData$runner <- runner
    session$userData$f <- f
  }, {
    runner <- session$userData$runner
    f <- session$userData$f
    runner$start()
    session$flushReact()
    expect_equal(isolate(runner$state())[["load_survey"]], "running")
    expect_false(is.null(isolate(f$triggers$load_survey())))
    expect_null(isolate(f$triggers$load_weather()))

    for (key in pipeline_keys) {
      f$statuses[[key]]("success")
      f$generations[[key]](isolate(f$generations[[key]]()) + 1L)
      session$flushReact()
      if (key != "step3") {
        next_key <- pipeline_keys[match(key, pipeline_keys) + 1L]
        expect_false(is.null(isolate(f$triggers[[next_key]]())))
      }
    }
    expect_equal(isolate(runner$phase()), "done")
    expect_equal(unname(isolate(runner$state())), rep("done", 5))
  })
})

test_that("runner waits for a generation change, including cached success", {
  withr::local_options(wiseapp.pipeline_settle = 0)
  testServer(function(input, output, session) {
    f <- pipeline_fixture()
    for (key in pipeline_keys) f$statuses[[key]]("success")
    runner <- pipeline_runner(f$triggers, f$results)
    session$userData$runner <- runner
    session$userData$f <- f
  }, {
    runner <- session$userData$runner
    f <- session$userData$f
    runner$start(); session$flushReact()
    expect_equal(isolate(runner$phase()), "running")
    f$generations$load_survey(1L); session$flushReact()
    expect_false(is.null(isolate(f$triggers$load_weather())))
  })
})

test_that("failure skips later stages and cancellation stops advancement", {
  withr::local_options(wiseapp.pipeline_settle = 0)
  testServer(function(input, output, session) {
    f <- pipeline_fixture()
    runner <- pipeline_runner(f$triggers, f$results)
    session$userData$runner <- runner
    session$userData$f <- f
  }, {
    runner <- session$userData$runner
    f <- session$userData$f
    runner$start(); session$flushReact()
    f$generations$load_survey(1L)
    f$statuses$load_survey("failure"); session$flushReact()
    expect_equal(isolate(runner$phase()), "failed")
    expect_equal(unname(isolate(runner$state())),
                 c("failed", rep("skipped", 4)))

    runner$reset(); runner$start(); session$flushReact()
    runner$cancel(); session$flushReact()
    expect_equal(isolate(runner$phase()), "cancelled")
    expect_equal(unname(isolate(runner$state())),
                 rep("skipped", 5))
  })
})

test_that("pipeline progress UI exposes all stages and states", {
  html <- as.character(.pipeline_progress_ui(
    stats::setNames(c("done", "running", "failed", "skipped", "pending"),
                    pipeline_keys)
  ))
  for (text in c("survey sample", "weather variables", "Step 1", "Step 2", "Step 3"))
    expect_match(html, text, fixed = TRUE)
  for (state in c("done", "running", "failed", "skipped", "pending"))
    expect_match(html, paste0("pipeline-stage-", state), fixed = TRUE)
})

test_that("pipeline stage labels describe the automatic replay", {
  expect_equal(
    vapply(.pipeline_stages(), `[[`, character(1), "label"),
    c("Load the survey sample", "Load the weather variables",
      "Step 1 - Fit the welfare model",
      "Step 2 - Run the climate simulation",
      "Step 3 - Run the policy simulation")
  )
})

test_that("staged import starts the wired runner only after Start", {
  withr::local_options(wiseapp.pipeline_settle = 0)
  sent <- new.env(parent = emptyenv())
  server <- function(input, output, session) {
    keys <- pipeline_keys
    triggers <- stats::setNames(
      lapply(keys, function(key) reactiveVal(NULL)), keys
    )
    generations <- stats::setNames(
      lapply(keys, function(key) reactiveVal(0L)), keys
    )
    statuses <- stats::setNames(
      lapply(keys, function(key) reactiveVal("idle")), keys
    )
    proxy <- new.env(parent = emptyenv())
    proxy$userData <- session$userData
    proxy$sendInputMessage <- function(id, message) {
      sent[[id]] <- message$value
      invisible(NULL)
    }
    export_menu_server(
      input, output, proxy,
      provenance = shiny::reactive(list()), seed = 1L,
      run_triggers = triggers,
      step_results = stats::setNames(lapply(keys, function(key) list(
        generation = generations[[key]], status = statuses[[key]]
      )), keys)
    )
    session$userData$triggers <- triggers
    session$userData$generations <- generations
    session$userData$statuses <- statuses
  }

  testServer(server, {
    session$setInputs(real_setting = "old")
    f <- pipeline_import_file(list(real_setting = "new"))
    session$setInputs(import_config_file = list(
      datapath = f, name = "configuration.json", size = 20L,
      type = "application/json"
    ))
    expect_null(sent$real_setting)
    expect_match(session$userData$wise_import_status$text,
                 "Press Start", fixed = TRUE)

    session$setInputs(import_run_config = 1L)
    session$flushReact()
    expect_equal(sent$real_setting, "new")
    expect_false(is.null(isolate(session$userData$triggers$load_survey())))

    for (key in pipeline_keys) {
      session$userData$statuses[[key]]("success")
      g <- session$userData$generations[[key]]
      g(isolate(g()) + 1L)
      session$flushReact()
    }
    expect_equal(isolate(session$userData$triggers$step3()), NULL)
  })
})

test_that("pipeline imports restore controls that appear during settling", {
  withr::local_options(wiseapp.pipeline_settle = 60)
  sent <- new.env(parent = emptyenv())
  server <- function(input, output, session) {
    f <- pipeline_fixture()
    proxy <- new.env(parent = emptyenv())
    proxy$userData <- session$userData
    proxy$sendInputMessage <- function(id, message) {
      sent[[id]] <- message$value
      invisible(NULL)
    }
    export_menu_server(
      input, output, proxy,
      provenance = shiny::reactive(list()), seed = 1L,
      run_triggers = f$triggers, step_results = f$results
    )
  }

  testServer(server, {
    f <- pipeline_import_file(list(late_ctrl = "restored"))
    session$setInputs(import_config_file = list(
      datapath = f, name = "configuration.json", size = 20L,
      type = "application/json"
    ))
    session$setInputs(import_run_config = 1L)
    expect_false("late_ctrl" %in% ls(sent))
    expect_true("late_ctrl" %in%
                  session$userData$wise_import_state$pending$ids)

    session$setInputs(late_ctrl = "placeholder")
    expect_equal(sent$late_ctrl, "restored")
    expect_null(session$userData$wise_import_state$pending)
  })
})

test_that("pipeline prerequisite controls render while their pages are hidden", {
  expected <- list(
    mod_1_01_sample.R = c("unit_ui", "sample_ui", "survey_year_ui"),
    mod_1_03_outcome.R = c("outcome_ui", "currency_ui", "poverty_line_ui"),
    mod_1_04_weather.R = c("weather_selector_ui", "weather_construction_ui"),
    mod_1_06_model.R = c("model_selector_ui", "policy_ui", "model_specs_ui",
                         "covariate_inputs", "lasso_force_ui", "lasso_advanced_ui"),
    mod_2_01_weathersim.R = "baseline_survey_ui",
    mod_3_01_sp.R = c("sp_type_ui", "sp_budget_amount_ui", "sp_targeting_ui",
                      "pmt_variable_ui", "pmt_cutoff_ui", "sp_timing_ui"),
    mod_3_02_infra.R = c("elec_ui", "water_ui", "sanitation_ui", "piped_ui",
                         "piped_to_prem_ui", "imp_wat_san_ui", "health_ui"),
    mod_3_03_digital.R = c("internet_ui", "mobile_ui"),
    mod_3_04_labor.R = c("labor_emp_ui", "labor_sector_ui"),
    mod_3_05_education.R = c("primary_ui", "secondary_ui", "postsec_ui")
  )

  skip_if_not(dir.exists(testthat::test_path("..", "..", "R")),
    "R/ source tree not available (installed package)")
  for (file in names(expected)) {
    text <- paste(readLines(testthat::test_path("..", "..", "R", file),
                            warn = FALSE), collapse = "\n")
    expect_match(text, "suspendWhenHidden = FALSE", fixed = TRUE,
                 info = file)
    for (output_id in expected[[file]]) {
      expect_match(text, paste0('"', output_id, '"'), fixed = TRUE,
                   info = paste(file, output_id))
    }
  }
})
