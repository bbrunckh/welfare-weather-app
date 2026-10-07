test_that("weather_support_from_stored returns the stored rows in variable and scenario order", {
  row <- function(var, scen, share) data.frame(
    weather_variable = var, scenario = scen, n_scenario = 100,
    outside_n = share * 100, outside_share = share, warning = share > 0.05
  )
  scenarios <- list(
    "SSP2 / 2030" = list(weather_support = rbind(row("temp", "SSP2 / 2030", 0.10),
      row("rain", "SSP2 / 2030", 0.01))),
    "SSP5 / 2050" = list(weather_support = rbind(row("temp", "SSP5 / 2050", 0.30),
      row("rain", "SSP5 / 2050", 0.02)))
  )
  out <- weather_support_from_stored(scenarios, vars = c("rain", "temp"))
  expect_identical(out$weather_variable, c("rain", "rain", "temp", "temp"))
  expect_identical(out$scenario, rep(c("SSP2 / 2030", "SSP5 / 2050"), 2L))
  # Selected variables and visible scenarios are honoured.
  one <- weather_support_from_stored(scenarios, vars = "temp", visible = "SSP5 / 2050")
  expect_identical(one$scenario, "SSP5 / 2050")
  expect_equal(one$outside_share, 0.30)
  # A visible scenario without a stored summary (older run) forces the fallback.
  scenarios[["SSP5 / 2050"]]$weather_support <- NULL
  expect_null(weather_support_from_stored(scenarios, vars = "temp"))
  expect_null(weather_support_from_stored(list(), vars = "temp"))
})

test_that("the Diagnostics weather-support table reads the stored summary without weather files", {
  stored <- data.frame(
    weather_variable = "temp", scenario = "SSP2 / 2030", n_reference = 100,
    n_scenario = 200, robust_lo = 1, robust_hi = 99, reference_label = NA_character_,
    is_binned = FALSE, outside_n = 20, outside_share = 0.10, warning = TRUE,
    warning_rule = "Robust 1%-99% reference interval; warn above 5% outside",
    stringsAsFactors = FALSE
  )
  scenarios <- list("SSP2 / 2030" = list(weather_support = stored))
  shiny::testServer(
    mod_2_03_diagnostics_server,
    args = list(
      id = "diagnostics",
      # No weather_raw and no survey_weather: only the stored summary can answer.
      hist_sim = shiny::reactiveVal(list(run = 1L)),
      saved_scenarios = shiny::reactiveVal(scenarios),
      survey_weather = shiny::reactiveVal(NULL),
      selected_weather = shiny::reactiveVal(data.frame(
        name = "temp", label = "Temperature", stringsAsFactors = FALSE
      )),
      tabset_id = "tabs"
    ),
    {
      session$setInputs(diag_weather_vars = "temp", diag_weather_scenario = "all")
      session$flushReact()
      tbl <- weather_support_data()
      expect_equal(tbl$outside_share, 0.10)
      expect_true(tbl$warning)
      disp <- weather_support_display()
      expect_identical(disp$Status, "Review: extrapolation")
      expect_identical(disp$`Weather variable`, "Temperature")
    }
  )
})
