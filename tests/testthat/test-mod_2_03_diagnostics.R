# ============================================================================ #
# tests/testthat/test-mod_2_03_diagnostics.R                                   #
# INT-07: the Diagnostics tab follows the hist_sim lifecycle - appended on     #
# the first run, removed when the run is cleared, re-appended on a later run.  #
# ============================================================================ #

library(testthat)
library(shiny)

test_that("diagnostics tab is appended, removed on clear, re-appended on rerun", {

  hist_sim <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_2_03_diagnostics_server,
    args = list(
      id               = "diagnostics",
      hist_sim         = hist_sim,
      saved_scenarios  = shiny::reactiveVal(list()),
      survey_weather   = shiny::reactiveVal(NULL),
      selected_weather = shiny::reactiveVal(NULL),
      tabset_id        = "step2_output_tabs"
    ),
    {
      session$flushReact()
      expect_false(diag_tab_added())

      hist_sim(list()); session$flushReact()
      expect_true(diag_tab_added())

      # Clearing the run removes the tab (INT-07)
      hist_sim(NULL); session$flushReact()
      expect_false(diag_tab_added())

      # A later run re-inserts it
      hist_sim(list()); session$flushReact()
      expect_true(diag_tab_added())
    }
  )
})

test_that("diagnostics accepts absent scenarios without eager forcing", {
  hist_sim <- shiny::reactiveVal(NULL)
  shiny::testServer(
    mod_2_03_diagnostics_server,
    args = list(
      id = "diagnostics", hist_sim = hist_sim, saved_scenarios = NULL,
      survey_weather = shiny::reactiveVal(NULL),
      selected_weather = shiny::reactiveVal(NULL), tabset_id = "tabs"
    ),
    {
      session$flushReact()
      expect_false(diag_tab_added())
      hist_sim(list(run = 1L)); session$flushReact()
      expect_true(diag_tab_added())
    }
  )
})

# ============================================================================ #
# Batch 2 UI migration: DT -> reactable, ggplot -> echarts4r (guidelines §6/§7)
# ============================================================================ #

make_diag_weather_fixture <- function() {
  survey <- data.frame(
    loc_id = c("a", "a"),
    int_month = c(1L, 1L),
    timestamp = as.Date(c("2020-01-01", "2021-01-01"))
  )
  weather_raw <- data.frame(
    loc_id = rep("a", 4), int_month = 1L,
    timestamp = as.Date(rep(c("2020-01-01", "2021-01-01"), 2)),
    temp = c(10, 12, 11, 13), rain = c(5, 6, 7, 8)
  )
  scenarios <- list(`SSP2-4.5 / 2030-2040` = transform(weather_raw,
    temp = temp + 1, rain = rain + 0.5
  ))
  list(survey = survey, weather_raw = weather_raw, scenarios = scenarios)
}

test_that("weather density echarts keeps the historical reference and legend", {
  f <- make_diag_weather_fixture()
  ch <- echart_weather_density_panel(
    f$survey, f$weather_raw, "temp",
    scenario_weather = f$scenarios, show_regression = TRUE
  )
  expect_s3_class(ch, "echarts4r")
  names_s <- vapply(ch$x$opts$series, `[[`, character(1), "name")
  expect_match(names_s[[1]], "Historical", fixed = TRUE)
  expect_true("Model support" %in% names_s)
  expect_true(any(grepl("SSP2", names_s)))
  # Legend shows the reference series (the ggplot panel had one).
  expect_true("Historical" %in%
    as.character(unlist(ch$x$opts$legend$data)))
})

test_that("weather density echarts collapses multi-variable panels into one widget", {
  f <- make_diag_weather_fixture()
  ch <- echart_weather_density_panel(
    f$survey, f$weather_raw, c("temp", "rain"),
    scenario_weather = f$scenarios, show_regression = TRUE
  )
  expect_s3_class(ch, "echarts4r")
  # Ridgeline: one category row per variable.
  expect_match(paste(ch$x$opts$yAxis$axisLabel$formatter, collapse = " "),
    "rain", fixed = TRUE)

  # Unknown variables are intersected away, like the ggplot panel.
  blank <- echart_weather_density_panel(f$survey, f$weather_raw, "nope")
  expect_match(blank$x$opts$title[[1]]$text,
    "Selected weather variable(s) not found", fixed = TRUE)
})

test_that("robustness and trajectory charts render from precomputed data", {
  set.seed(3)
  ts <- do.call(rbind, lapply(c("Historical", "SSP2-4.5 / 2030-2040"), function(s) {
    data.frame(
      scenario = s, model_id = paste0("m", 1:3),
      sim_year = rep(2020:2022, 3),
      value = rnorm(9, ifelse(s == "Historical", 5, 5.2)),
      is_historical = s == "Historical"
    )
  }))
  rob <- echart_model_robustness(model_robustness_data(ts), "Mean welfare")
  expect_s3_class(rob, "echarts4r")
  expect_length(rob$x$opts$series, 2L)
  expect_identical(rob$x$opts$yAxis$type, "category")

  sp <- echart_timeseries_spaghetti(ts, "Mean welfare", c(lo = 0.1, hi = 0.9))
  expect_s3_class(sp, "echarts4r")
  types <- vapply(sp$x$opts$series, `[[`, character(1), "type")
  expect_true(all(types %in% c("line", "custom")))
  # One bold median series per scenario with a legend entry.
  expect_true("Historical" %in% as.character(unlist(sp$x$opts$legend$data)))

  blank <- echart_timeseries_spaghetti(NULL, "Mean")
  expect_match(blank$x$opts$title[[1]]$text, "Run a simulation to see model trajectories.",
    fixed = TRUE
  )
})

test_that("diagnostics UI mounts chart outputs without the support table", {
  html <- as.character(htmltools::renderTags(
    mod_2_03_diagnostics_ui("diagnostics")
  )$html)
  expect_match(html, "diagnostics-diag_weather_density", fixed = TRUE)
  expect_match(html, "diagnostics-model_robustness_plot", fixed = TRUE)
  expect_match(html, "diagnostics-timeseries_plot", fixed = TRUE)
  expect_false(grepl("diagnostics-weather_support_table", html, fixed = TRUE))
  expect_false(grepl("Reactable.downloadDataCSV", html, fixed = TRUE))
})
