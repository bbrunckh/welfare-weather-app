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
    echart_make_coefplot      = function(...) NULL,
    echart_weather_effect_plot = function(...) NULL,
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
      expect_match(html_of("heading_effect"), "How does weather relate to outcome a?",
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
                   "across the welfare distribution", fixed = TRUE)
      # RIF coefficient stability is available with a tau selector when the
      # fitted model carries a quantile grid. This test fixture has no grid.
      expect_match(html_of("heading_coef"), "stable across specifications")
      expect_identical(nchar(html_of("rif_tau_selector")), 0L)
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
    echart_make_coefplot      = function(...) NULL,
    echart_weather_effect_plot = function(...) NULL,
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

test_that("P16: one fit-signature observer preserves exact stale transitions", {
  local_mocked_bindings(
    prepare_outcome_df = function(df, so) df,
    fit_model = function(df, selected_outcome, selected_weather, selected_model) {
      list(engine = selected_model$engine, y_var = selected_outcome$name,
           weather_terms = selected_weather$name, interaction_terms = character(0),
           fit1 = NULL, fit2 = NULL, fit3 = NULL, rif_grid = NULL)
    },
    echart_make_coefplot = function(...) NULL,
    echart_weather_effect_plot = function(...) NULL,
    is_logistic_fit = function(mf) FALSE
  )
  outcome <- shiny::reactiveVal(make_outcome())
  weather <- shiny::reactiveVal(make_weather_sel())
  model <- shiny::reactiveVal(list(engine = "fixest"))
  swd <- shiny::reactiveVal(data.frame(tx = 1:4, welfare = 1:4, weight = 1))
  version <- shiny::reactiveVal(0L)
  run <- shiny::reactiveVal(0L)
  shiny::testServer(
    mod_1_07_results_server,
    args = list(id = "res", variable_list = shiny::reactiveVal(make_vl()),
                selected_surveys = shiny::reactiveVal(data.frame()),
                selected_outcome = outcome, selected_weather = weather,
                survey_weather = swd, selected_model = model,
                run_model = run,
                survey_version = version, tabset_id = "step1_tabs"),
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      run(1L); settle(); run(2L); settle()
      expect_false(stale())
      outcome(make_outcome()); weather(make_weather_sel())
      model(list(engine = "fixest")); swd(data.frame(tx = 1:4, welfare = 1:4, weight = 1))
      version(0L); settle()
      expect_false(stale())
      version(1L); settle(); expect_true(stale()); stale(FALSE)
      outcome(make_outcome(label = "Outcome B")); settle(); expect_true(stale()); stale(FALSE)
      weather(make_weather_sel("pr")); settle(); expect_true(stale()); stale(FALSE)
      model(list(engine = "rif")); settle(); expect_true(stale()); stale(FALSE)
      swd(data.frame(tx = 1:5, welfare = 1:5, weight = 1)); settle()
      expect_true(stale())
    }
  )
})

test_that("redesigned sections render: who-panel, focused table, RIF suppression", {
  skip_if_not_installed("shiny")

  local_mocked_bindings(
    prepare_outcome_df = function(df, so) df,
    fit_model = function(df, selected_outcome, selected_weather, selected_model) {
      list(
        engine            = selected_model$engine,
        y_var             = selected_outcome$name,
        weather_terms     = selected_weather$name,
        interaction_terms = if (identical(selected_model$engine, "fixest")) "tx:urban" else character(0),
        fit1 = NULL, fit2 = NULL, fit3 = NULL,
        rif_grid = if (identical(selected_model$engine, "rif")) {
          expand.grid(
            term = "tx", tau = c(0.1, 0.5, 0.9), model = 1:3,
            KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE
          ) |>
            transform(estimate = seq_len(9) / 10, std.error = 0.1)
        } else NULL
      )
    },
    echart_make_coefplot      = function(...) NULL,
    echart_weather_effect_plot = function(...) NULL,
    make_regtable_focused_reactable = function(...) {
      reactable::reactable(data.frame(ok = "focused-reactable"))
    },
    make_regtable_specs_reactable = function(...) {
      reactable::reactable(data.frame(ok = "specs-reactable"))
    },
    make_regtable_df_reactable = function(...) {
      reactable::reactable(data.frame(ok = "df-reactable"))
    },
    make_regtable_focused_df = function(...) data.frame(Variable = character(0)),
    step1_scenarios          = function(...) NULL,
    is_logistic_fit          = function(mf) FALSE
  )

  sel_outcome <- shiny::reactiveVal(make_outcome())
  sel_weather <- shiny::reactiveVal(make_weather_sel())
  sel_model   <- shiny::reactiveVal(list(engine = "fixest", cluster = "loc_id_panel"))
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
      run_model        = run_model,
      tabset_id        = "step1_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      html_of <- function(output_id) {
        paste(as.character(session$output[[output_id]]), collapse = " ")
      }

      # Quirk: prime the fit counter (see the note in the first test)
      # before the real first fit.
      run_model(1L); settle()
      run_model(2L); settle()

      # Question-led headings
      expect_match(html_of("heading_effect"),
                   "How does weather relate to outcome a?", fixed = TRUE)
      expect_match(html_of("heading_who"), "Who is most affected?", fixed = TRUE)
      expect_match(html_of("heading_table"), "Full model estimates", fixed = TRUE)

      # Who panel: moderated plot layout + methodological note (interactions)
      expect_match(html_of("who_note_ui"), "moderator level", fixed = TRUE)

      # Focused table + spec comparison render; AER table inside details
      expect_match(html_of("focused_table"), "focused-reactable", fixed = TRUE)
      expect_match(html_of("specs_table"), "specs-reactable", fixed = TRUE)

      # Refit as RIF: coefficient stability gets a tau selector, the who note
      # switches to quantile wording, and specs comparison is hidden.
      sel_model(list(engine = "rif"))
      session$elapse(500); session$flushReact()
      run_model(3L); settle()
      run_model(4L); settle()
      expect_match(html_of("heading_coef"), "stable across specifications")
      expect_match(html_of("rif_tau_selector"), "Welfare quantile")
      expect_match(html_of("rif_tau_selector"), "τ = 0.5")
      expect_match(html_of("who_note_ui"),
                   "welfare distribution", fixed = TRUE)
      # renderReactable(NULL) emits an empty widget payload - no table data.
      expect_match(html_of("specs_table"), '"x":null', fixed = TRUE)
    }
  )
})

test_that("fit-scoped headline inputs preserve values without recomputation", {
  calls <- new.env(parent = emptyenv())
  calls$scenario <- 0L
  calls$tail <- 0L
  calls$heterogeneity <- 0L

  scenario_builder <- function(mf, snap, var) {
    calls$scenario <- calls$scenario + 1L
    list(engine = "rif", scale = "pct", contrast_label = "+1 SD",
         profile_note = NULL,
         scenarios = list(list(tau = 0.5, value = "median", label = "median",
                               estimate = 0.02, se = 0.01, ci = NULL)))
  }
  tail_builder <- function(mf, snap, var, taus) {
    calls$tail <- calls$tail + 1L
    out <- list(lapply(taus, function(tau) {
      list(tau = tau, value = paste0("tau-", tau), label = paste0("tau-", tau),
           estimate = 0.01 + 0.02 * tau, se = 0.01, ci = NULL)
    }))
    attr(out, "contrast_label") <- "+1 SD"
    out
  }
  heterogeneity_builder <- function(...) {
    calls$heterogeneity <- calls$heterogeneity + 1L
    0.042
  }

  mf <- list(engine = "rif", weather_terms = "tx", fit3 = NULL,
             interaction_terms = character(0), train_data = data.frame())
  snap <- list(
    weather = data.frame(name = "tx", label = "Temperature", cont_binned = "Continuous"),
    outcome = data.frame(type = "numeric", transform = "log"),
    model = list(), survey_weather = data.frame(tx = 1:3)
  )

  local_mocked_bindings(
    step1_scenarios = scenario_builder,
    .s1_rif_scenarios = tail_builder,
    step1_rif_heterogeneity_p = heterogeneity_builder
  )

  legacy <- step1_headline_cards(mf, snap)
  legacy_table <- step1_headline_table(result = legacy)
  expect_identical(calls$scenario, 0L)
  expect_identical(calls$tail, 2L)
  expect_identical(calls$heterogeneity, 1L)

  scenarios <- list(tx = scenario_builder(mf, snap, "tx"))
  tails <- list(tx = tail_builder(mf, snap, "tx", c(0.1, 0.9)))
  pvals <- list(tx = heterogeneity_builder(mf, snap, "tx"))
  calls$scenario <- 0L
  calls$tail <- 0L
  calls$heterogeneity <- 0L

  cached <- step1_headline_cards(
    mf, snap, scenarios_list = scenarios,
    rif_scenarios = tails, rif_heterogeneity = pvals
  )

  expect_identical(cached, legacy)
  expect_identical(calls$scenario, 0L)
  expect_identical(calls$tail, 0L)
  expect_identical(calls$heterogeneity, 0L)
  expect_identical(step1_headline_table(result = cached), legacy_table)
  expect_null(step1_headline_table(result = NULL))
})
