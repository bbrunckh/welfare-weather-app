# ============================================================================ #
# tests/testthat/test-mod_1_07_results.R                                       #
# INT-05: fit results carry a fit-time snapshot of outcome/weather/variable    #
# labels, and engine-conditional headings follow re-fits.                      #
# ============================================================================ #

library(testthat)
library(shiny)

make_outcome <- function(label = "Outcome A", name = "welfare") {
  data.frame(name = name, label = label, type = "numeric",
             stringsAsFactors = FALSE)
}

make_weather_sel <- function(name = "tx") {
  data.frame(name = name, label = paste("Weather", name),
             cont_binned = "Continuous", transformation = "None",
             stringsAsFactors = FALSE)
}

make_vl <- function() {
  data.frame(name  = c("tx", "pr", "welfare"),
             label = c("Max temp", "Precipitation", "Welfare"),
             stringsAsFactors = FALSE)
}

test_that("fit snapshot captures fit-time labels; headings follow re-fit engine", {
  skip_if_not_installed("shiny")

  local_mocked_bindings(
    # The test targets snapshot binding, not the prep/fit internals.
    prepare_outcome_df = function(df, so) df,
    fit_model = function(df, selected_outcome, selected_weather, selected_model) {
      list(
        engine            = selected_model$engine,
        y_var             = selected_outcome$name,
        weather_terms     = selected_weather$name,
        interaction_terms = character(0),
        fit1 = NULL, fit2 = NULL, fit3 = NULL,
        rif_grid = NULL
      )
    },
    # Renderers are inert: elapse() force-executes outputs, and the plot
    # internals need real fitted models which are irrelevant here.
    make_coefplot           = function(...) ggplot2::ggplot(),
    make_weather_effect_plot = function(...) ggplot2::ggplot(),
    make_regtable           = function(...) shiny::tags$p("table"),
    is_logistic_fit         = function(mf) FALSE
  )

  sel_outcome <- shiny::reactiveVal(make_outcome())
  sel_weather <- shiny::reactiveVal(make_weather_sel())
  sel_model   <- shiny::reactiveVal(list(engine = "fixest"))
  run_model   <- shiny::reactiveVal(0L)

  shiny::testServer(
    mod_1_07_results_server,
    args = list(
      id               = "res",
      variable_list    = shiny::reactiveVal(make_vl()),
      selected_surveys = shiny::reactiveVal(data.frame()),
      selected_outcome = sel_outcome,
      selected_weather = sel_weather,
      survey_weather   = shiny::reactiveVal(
        data.frame(tx = 1:4, welfare = 1:4, weight = 1)
      ),
      selected_model   = sel_model,
      model_type       = shiny::reactiveVal("linear"),
      run_model        = run_model,
      tabset_id        = "step1_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      html_of <- function(output_id) {
        paste(as.character(session$output[[output_id]]), collapse = " ")
      }

      # Quirk: inside testServer the first reactiveVal change is treated as
      # the session-init event (like ignoreInit inputs), so prime the fit
      # counter before the real first fit.
      run_model(1L); settle()

      run_model(2L); settle()

      snap <- model_fit_val()$.snap
      expect_s3_class(snap$outcome, "data.frame")
      expect_identical(snap$outcome$label, "Outcome A")
      expect_identical(snap$weather$name, "tx")
      expect_identical(.label_lookup(snap$variable_list)("tx"), "Max temp")

      # Headings describe the fitted engine (fixest wording)
      expect_match(html_of("heading_effect"), "Predicted outcome vs weather",
                   fixed = TRUE)

      # Change the live selections WITHOUT refitting: the snapshot must not
      # move - old results keep their original labels (INT-05) - and the
      # results become stale (INT-08).
      sel_outcome(make_outcome(label = "Outcome B", name = "welf2"))
      sel_weather(make_weather_sel(name = "pr"))
      settle()
      snap <- model_fit_val()$.snap
      expect_identical(snap$outcome$label, "Outcome A")
      expect_identical(snap$weather$name, "tx")
      expect_true(stale())

      # The stale banner renders the warning (INT-08)
      html <- html_of("stale_banner")
      expect_match(html, "Results are out of date", fixed = TRUE)

      # Refit with a different engine: snapshot and headings follow the run,
      # and staleness clears (INT-08).
      sel_model(list(engine = "rif"))
      session$elapse(500); session$flushReact()
      run_model(3L); settle()
      expect_false(stale())
      html <- html_of("stale_banner")
      expect_identical(nchar(html), 0L)

      snap <- model_fit_val()$.snap
      expect_identical(snap$outcome$label, "Outcome B")
      expect_identical(snap$weather$name, "pr")
      expect_match(html_of("heading_effect"),
                   "Weather sensitivity across the distribution", fixed = TRUE)
      expect_match(html_of("heading_coef"),
                   "UQR coefficients by model specification", fixed = TRUE)
      expect_true(model_fit_val()$.snap$outcome$label == "Outcome B")
    }
  )
})

test_that("REACT-14: specification fallbacks render the provenance banner", {
  skip_if_not_installed("shiny")

  local_mocked_bindings(
    prepare_outcome_df = function(df, so) df,
    fit_model = function(df, selected_outcome, selected_weather, selected_model) {
      list(
        engine            = selected_model$engine,
        y_var             = selected_outcome$name,
        weather_terms     = selected_weather$name,
        interaction_terms = character(0),
        fit1 = NULL, fit2 = NULL, fit3 = NULL,
        rif_grid = NULL,
        fallbacks = list(list(
          kind      = "model_family",
          requested = "logistic",
          used      = "linear",
          reason    = "outcome column is not logical (TRUE/FALSE or 0/1)"
        ))
      )
    },
    make_coefplot            = function(...) ggplot2::ggplot(),
    make_weather_effect_plot = function(...) ggplot2::ggplot(),
    make_regtable            = function(...) shiny::tags$p("table"),
    is_logistic_fit          = function(mf) FALSE
  )

  run_model <- shiny::reactiveVal(0L)

  shiny::testServer(
    mod_1_07_results_server,
    args = list(
      id               = "res",
      variable_list    = shiny::reactiveVal(make_vl()),
      selected_surveys = shiny::reactiveVal(data.frame()),
      selected_outcome = shiny::reactiveVal(make_outcome()),
      selected_weather = shiny::reactiveVal(make_weather_sel()),
      survey_weather   = shiny::reactiveVal(
        data.frame(tx = 1:4, welfare = 1:4, weight = 1)
      ),
      selected_model   = shiny::reactiveVal(list(engine = "fixest")),
      model_type       = shiny::reactiveVal("linear"),
      run_model        = run_model,
      tabset_id        = "step1_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      html_of <- function(output_id) {
        paste(as.character(session$output[[output_id]]), collapse = " ")
      }

      # Prime the fit counter (see the quirk note in the test above)
      run_model(1L); settle()
      run_model(2L); settle()

      expect_length(model_fit_val()$fallbacks, 1)
      html <- html_of("fallback_banner")
      expect_match(html, "differs from the requested specification",
                   fixed = TRUE)
      expect_match(html, "requested logistic, fitted linear", fixed = TRUE)
    }
  )
})
