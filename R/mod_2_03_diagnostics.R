#' 2_03_diagnostics UI Function
#'
#' @description A shiny Module. Renders the Diagnostics tab content:
#'   weather input density panel, climate-model robustness, and weather-year
#'   trajectory diagnostics. Consolidates the former mod_2_05_sim_diag.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_03_diagnostics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # 0. Stale banner (INT-08) ----
    shiny::uiOutput(ns("stale_banner")),
    shiny::uiOutput(ns("simulation_summary_ui")),

    # 1. Weather inputs panel ----
    shiny::h4(
      "Are simulated weather conditions within model support?",
      info_popover(
        title = "Weather support and overlap",
        shiny::p(
          "The Step 1 regression input is the reference distribution. Future",
          "scenario values are compared with its robust 1st-99th percentile",
          "interval. Values outside that interval require extrapolation of the",
          "estimated weather-outcome relationship."
        ),
        docs = TRUE
      ),
      class = "diagnostic-section-heading"
    ),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      shiny::tags$div(
        style = "display:flex; align-items:center; gap:14px; flex-wrap:wrap; margin-bottom:8px;",
        shiny::uiOutput(ns("diag_weather_vars_ui")),
        shiny::uiOutput(ns("diag_weather_scenario_ui"))
      ),
      wise_chart_output(ns("diag_weather_density"),
        "Weather distribution comparing model support, historical weather and future scenarios",
        height = "340px"
      ),
      shiny::uiOutput(ns("weather_support_warning_ui")),
      shiny::tags$p(
        class = "diagnostic-note",
        "Distributions are normalized separately so samples with different sizes can be compared. Overlap does not by itself establish model validity."
      )
    ),

    # 2. Climate-model robustness (Figure D2-3A default) ----
    shiny::h4(
      "Are expected outcomes consistent across climate models?",
      info_popover(
        title = "Climate-model agreement",
        shiny::p(
          "Each point is one climate model's mean outcome across simulated",
          "weather years for a scenario and projection period. Historical is",
          "shown once as the neutral reference."
        ),
        docs = TRUE
      ),
      class = "diagnostic-section-heading"
    ),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      wise_chart_output(ns("model_robustness_plot"),
        "Climate-model mean outcome by scenario and period",
        height = "420px"
      ),
      shiny::tags$p(
        class = "diagnostic-note",
        "Each scenario-coloured point is one climate model's mean across simulated weather-year draws. The orange point is the median model mean."
      )
    ),

    # 3. Weather-year trajectories (Figure D2-3B advanced) ----
    shiny::h4(
      "How much can outcomes vary across weather-year draws?",
      info_popover(
        title = "Weather-year variation",
        shiny::p(
          "This technical view shows annual outcome variation within each",
          "climate model. It is useful for understanding the inter-annual",
          "component behind the Results summaries."
        ),
        docs = TRUE
      ),
      class = "diagnostic-section-heading"
    ),
    shiny::div(
      class = "results-section-card diagnostic-section-card",
      wise_chart_output(ns("timeseries_plot"),
        "Outcome across simulated weather-year draws",
        height = "380px"
      ),
      shiny::tags$p(
        class = "diagnostic-note",
        "Each line shows a climate model's simulated annual outcome across weather-year draws; the bold line summarizes the across-model median. Historical weather-year draws are the reference, and future scenarios apply a delta-method perturbation to that historical reference. Projection windows are separate regimes, not continuous annual forecasts. See the documentation for details on the simulation and perturbation method."
      )
    )
  )
}


#' 2_03_diagnostics Server Functions
#'
#' Appends a Diagnostics tab to the main tabset once the historical simulation
#' has run. Weather density and simulation diagnostics refresh when their
#' respective Update button is clicked.
#'
#' @param id               Module id.
#' @param hist_sim         ReactiveVal list with preds, so, weather_raw, train_data.
#' @param saved_scenarios  ReactiveVal holding named scenario entries.
#' @param survey_weather   Reactive data frame of merged survey-weather data.
#' @param selected_weather Reactive data frame of selected weather variable metadata.
#' @param stored_breaks    Reactive named list of Step 1 weather bin breaks.
#' @param tabset_id        Character id of the parent tabset panel.
#' @param tabset_session   Shiny session for the tabset.
#'
#' @noRd
mod_2_03_diagnostics_server <- function(id,
                                        hist_sim,
                                        saved_scenarios = NULL,
                                        selected_hist = NULL,
                                        survey_weather,
                                        selected_weather,
                                        timeseries_curves = NULL,
                                        tabset_id,
                                        tabset_session = NULL,
                                        stale = reactive(FALSE),
                                        stored_breaks = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # INT-08: stale banner above the diagnostics pane.
    output$stale_banner <- shiny::renderUI({
      if (isTRUE(stale())) .stale_banner("Step 2 diagnostics") else NULL
    })

    output$simulation_summary_ui <- shiny::renderUI({
      simulation_summary_card(
        hist_sim = hist_sim(),
        saved_scenarios = if (!is.null(saved_scenarios)) saved_scenarios() else list(),
        selected_hist = if (is.function(selected_hist)) selected_hist() else selected_hist,
        selected_weather = if (!is.null(selected_weather)) selected_weather() else NULL
      )
    })

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # Reactive computations ----

    diagnostic_cache_key <- NULL
    diagnostic_cache_value <- NULL
    diagnostic_generation <- reactiveVal(0L)
    if (is.function(hist_sim)) {
      observeEvent(hist_sim(),
        {
          diagnostic_cache_key <<- NULL
          diagnostic_cache_value <<- NULL
          diagnostic_generation(diagnostic_generation() + 1L)
        },
        ignoreInit = FALSE
      )
    }
    # saved_scenarios is intentionally read only when a diagnostic output is
    # requested. This avoids forcing an optional reactive during module setup.
    scenario_weather_data <- reactive({
      generation <- diagnostic_generation()
      sc <- if (!is.null(saved_scenarios)) saved_scenarios() else list()
      if (length(sc) == 0) {
        return(NULL)
      }
      vars <- input$diag_weather_vars %||% character(0)
      active <- active_weather_scenarios()
      visible <- names(sc)
      if (!is.null(active)) visible <- intersect(visible, active)
      scenario_signature <- lapply(sc[visible], function(e) {
        # Only reference descriptors carry file/schema; tibbles are lists and
        # would otherwise warn on $file/$schema access.
        wr <- e$weather_raw
        is_ref <- is.list(wr) && !is.data.frame(wr)
        list(
          signature = e$weather_signature %||% e$signature %||% NULL,
          file = if (is_ref) wr$file else NULL,
          schema = if (is_ref) wr$schema else NULL
        )
      })
      cache_key <- digest::digest(list(
        generation, visible, vars,
        scenario_signature
      ))
      if (identical(cache_key, diagnostic_cache_key)) {
        return(diagnostic_cache_value)
      }
      out <- lapply(sc[visible], function(e) {
        raw <- step2_resolve_weather(e$weather_raw, e)
        if (is.null(raw) || !is.data.frame(raw)) {
          return(NULL)
        }
        keep <- unique(c(
          intersect(STEP2_WEATHER_KEY_COLUMNS, names(raw)),
          intersect(vars, names(raw))
        ))
        raw[, keep, drop = FALSE]
      })
      names(out) <- visible
      out <- Filter(Negate(is.null), out)
      out <- if (length(out)) out else NULL
      diagnostic_cache_key <<- cache_key
      diagnostic_cache_value <<- out
      out
    })

    # renderUI / render* outputs ----

    output$diag_weather_vars_ui <- shiny::renderUI({
      sw <- if (!is.null(selected_weather)) selected_weather() else NULL
      if (is.null(sw) || !"name" %in% names(sw) || !nrow(sw)) {
        return(NULL)
      }
      choices <- if ("label" %in% names(sw)) setNames(sw$name, sw$label) else sw$name
      current <- isolate(input$diag_weather_vars)
      selected <- intersect(current, unname(choices))
      if (!length(selected)) selected <- unname(choices)[[1L]]
      pill_toggle(
        ns("diag_weather_vars"),
        label = NULL,
        aria_label = "Weather variable",
        choices = choices, selected = selected[[1L]],
        layout = "horizontal"
      )
    })

    output$diag_weather_scenario_ui <- shiny::renderUI({
      sc_all <- if (!is.null(saved_scenarios)) names(saved_scenarios()) else character(0)
      if (!length(sc_all)) {
        return(NULL)
      }
      pill_toggle(
        ns("diag_weather_scenario"),
        label = NULL,
        aria_label = "Scenario and period",
        choices = c(
          "All scenarios and periods" = "all",
          stats::setNames(sc_all, sc_all)
        ),
        selected = "all"
      )
    })

    active_weather_scenarios <- reactive({
      selected <- input$diag_weather_scenario %||% "all"
      if (identical(selected, "all")) NULL else selected
    })

    weather_density_chart <- function() {
      req(hist_sim(), survey_weather())
      req(!is.null(hist_sim()$weather_raw))
      vars <- input$diag_weather_vars
      req(length(vars) > 0)

      sw <- if (!is.null(selected_weather)) selected_weather() else NULL
      breaks <- if (is.function(stored_breaks)) stored_breaks() else stored_breaks
      lbl_map <- if (!is.null(sw) && all(c("name", "label") %in% names(sw))) {
        setNames(sw$label, sw$name)
      } else {
        NULL
      }

      ch <- echart_weather_density_panel(
        survey_weather   = survey_weather(),
        weather_raw      = hist_sim()$weather_raw,
        weather_vars     = vars,
        weather_labels   = lbl_map,
        scenario_weather = scenario_weather_data(),
        active_scenarios = active_weather_scenarios(),
        log_x            = rep(FALSE, length(vars)),
        show_regression  = TRUE,
        height           = "340px",
        weather_specs    = sw,
        stored_breaks    = breaks
      )
      req(!is.null(ch))
      ch
    }
    output$diag_weather_density <- echarts4r::renderEcharts4r({
      ch <- weather_density_chart()
      req(!is.null(ch))
      ch
    }) |> shiny::bindEvent(input$diag_weather_vars, input$diag_weather_scenario,
      hist_sim(), survey_weather(), selected_weather(),
      ignoreNULL = TRUE, ignoreInit = FALSE
    )

    weather_support_data <- reactive({
      req(hist_sim(), survey_weather())
      vars <- input$diag_weather_vars
      req(length(vars) > 0L, !is.null(hist_sim()$weather_raw))
      ref <- .filter_hist_weather(hist_sim()$weather_raw, survey_weather())
      scenarios <- scenario_weather_data()
      weather_support_summary(
        ref, scenarios, vars,
        weather_specs = if (!is.null(selected_weather)) selected_weather() else NULL
      )
    })

    # Display frame retained for the lazy bundle export; it is not rendered in
    # the Diagnostics tab.
    weather_support_display <- function() {
      tbl <- weather_support_data()
      if (is.null(tbl) || !nrow(tbl)) {
        return(NULL)
      }
      sw <- if (!is.null(selected_weather)) selected_weather() else NULL
      label_map <- if (!is.null(sw) && all(c("name", "label") %in% names(sw))) {
        setNames(as.character(sw$label), as.character(sw$name))
      } else {
        character(0)
      }
      reference_display <- ifelse(
        tbl$is_binned,
        paste0("Supported bins: ", tbl$reference_label),
        paste0(
          formatC(tbl$robust_lo, format = "fg", digits = 4), " to ",
          formatC(tbl$robust_hi, format = "fg", digits = 4)
        )
      )
      data.frame(
        `Weather variable` = ifelse(tbl$weather_variable %in% names(label_map),
          label_map[tbl$weather_variable], tbl$weather_variable
        ),
        `Scenario / period` = tbl$scenario,
        `Reference support` = reference_display,
        `Scenario values` = tbl$n_scenario,
        `Outside interval` = paste0(tbl$outside_n, " (", round(100 * tbl$outside_share, 1), "%)"),
        Status = ifelse(tbl$warning, "Review: extrapolation", "Within support"),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    }

    output$weather_support_warning_ui <- renderUI({
      tbl <- weather_support_data()
      if (is.null(tbl) || !nrow(tbl) || !any(tbl$warning)) {
        return(NULL)
      }
      sw <- if (!is.null(selected_weather)) selected_weather() else NULL
      label_map <- if (!is.null(sw) && all(c("name", "label") %in% names(sw))) {
        setNames(as.character(sw$label), as.character(sw$name))
      } else {
        character(0)
      }
      bad_vars <- unique(tbl$weather_variable[tbl$warning])
      bad <- ifelse(bad_vars %in% names(label_map), label_map[bad_vars], bad_vars)
      shiny::tags$div(
        class = "alert alert-warning", role = "alert",
        shiny::tags$strong("Weather support warning: "),
        paste(bad, collapse = ", "),
        " has more than 5% of scenario values outside the reference support. Extrapolation may be required."
      )
    })

    # The selected outcome frame has no `method` column, so a bare `$method`
    # on the tibble warns "Unknown or uninitialised column" every time an
    # export artefact is materialised. Read it only when it exists.
    so_method <- function(so) {
      if (is.data.frame(so) && "method" %in% names(so)) {
        out <- as.character(so$method)[[1L]]
        if (nzchar(out)) {
          return(out)
        }
      }
      "mean"
    }

    # The uncertainty-source outputs remain available to the legacy server
    # path, but are not mounted in the current Diagnostics UI and therefore are
    # intentionally not registered as bundle artefacts.
    wise_export_figure(
      key = "simulation_weather_distribution",
      label = "Simulated weather input distribution",
      step = 2L,
      fun = function() {
        req(hist_sim(), survey_weather())
        vars <- input$diag_weather_vars
        req(length(vars) > 0L)
        sw <- if (!is.null(selected_weather)) selected_weather() else NULL
        breaks <- if (is.function(stored_breaks)) stored_breaks() else stored_breaks
        lbl_map <- if (!is.null(sw) && all(c("name", "label") %in% names(sw))) {
          setNames(sw$label, sw$name)
        } else {
          NULL
        }
        ch <- echart_weather_density_panel(
          survey_weather(), hist_sim()$weather_raw, vars,
          weather_labels = lbl_map,
          scenario_weather = scenario_weather_data(),
          active_scenarios = active_weather_scenarios(),
          log_x = rep(FALSE, length(vars)),
          show_regression = TRUE,
          height = "500px",
          weather_specs = sw,
          stored_breaks = breaks
        )
        req(!is.null(ch))
        ch
      },
      description = "Weather input distributions for the selected plot variables across historical and simulated sources.",
      width = 10, height = 6.5
    )
    wise_export_table(
      key = "simulation_weather_distribution_data",
      label = "Simulated weather input data",
      step = 2L,
      fun = function() {
        req(hist_sim(), survey_weather())
        vars <- input$diag_weather_vars
        req(length(vars) > 0L)
        annotate_visualization_export(
          weather_density_data(
            survey_weather(), hist_sim()$weather_raw, vars,
            scenario_weather_data(), active_weather_scenarios(), TRUE
          ),
          so_method(hist_sim()$so), hist_sim()$so,
          observation_unit = "weather input value entering the simulation",
          aggregation_order = "raw weather inputs retained by source and scenario",
          uncertainty = "distributional comparison"
        )
      },
      description = "Underlying tidy weather values used by the selected weather distribution plot."
    )
    wise_export_table(
      key = "simulation_weather_support_summary",
      label = "Weather support summary",
      step = 2L,
      fun = weather_support_display,
      stale = stale,
      description = "Sample sizes, robust Step 1 support intervals, outside-support shares, and warnings."
    )
    robustness_chart <- function() {
      tc <- timeseries_curves()
      req(!is.null(tc$tbl), nrow(tc$tbl) > 0L)
      ch <- echart_model_robustness(model_robustness_data(tc$tbl), tc$x_label,
        height = "420px"
      )
      req(!is.null(ch))
      ch
    }
    wise_export_figure(
      key = "simulation_model_robustness",
      label = "Climate-model robustness",
      step = 2L,
      fun = robustness_chart,
      description = "One point per climate model's mean across weather-year draws with ensemble spread.",
      width = 10, height = 6
    )
    wise_export_table(
      key = "simulation_model_robustness_data",
      label = "Climate-model robustness data",
      step = 2L,
      fun = function() {
        tc <- timeseries_curves()
        req(!is.null(tc$tbl), nrow(tc$tbl) > 0L)
        annotate_visualization_export(model_robustness_data(tc$tbl),
          so_method(hist_sim()$so), hist_sim()$so,
          observation_unit = "climate-model mean across weather-year draws",
          aggregation_order = "annual aggregate by model and weather year, then model mean",
          uncertainty = "ensemble spread"
        )
      },
      description = "Tidy climate-model robustness summaries."
    )
    trajectories_chart <- function() {
      req(timeseries_curves)
      tc <- timeseries_curves()
      req(!is.null(tc$tbl), nrow(tc$tbl) > 0L)
      ch <- echart_timeseries_spaghetti(tc$tbl,
        x_label = tc$x_label,
        ensemble_band_q = tc$ens_q,
        height = "380px"
      )
      req(!is.null(ch))
      ch
    }
    wise_export_figure(
      key = "simulation_model_trajectories",
      label = "Climate-model annual trajectories",
      step = 2L,
      fun = trajectories_chart,
      description = "Advanced climate-model trajectories by discrete simulation window.",
      width = 10, height = 6.5
    )
    wise_export_table(
      key = "simulation_model_trajectories_data",
      label = "Climate-model trajectory data",
      step = 2L,
      fun = function() {
        req(timeseries_curves)
        tc <- timeseries_curves()
        req(!is.null(tc$tbl), nrow(tc$tbl) > 0L)
        annotate_visualization_export(
          tc$tbl, so_method(hist_sim()$so), hist_sim()$so,
          observation_unit = "annual aggregate for one climate model and weather-year draw",
          aggregation_order = "weighted aggregate retained by model, simulation year, and scenario",
          uncertainty = "inter-model spread shown separately from annual draws"
        )
      },
      description = "Tidy data behind the advanced climate-model trajectory view."
    )

    output$timeseries_plot <- echarts4r::renderEcharts4r({
      ch <- trajectories_chart()
      req(!is.null(ch))
      ch
    })

    output$model_robustness_plot <- echarts4r::renderEcharts4r({
      ch <- robustness_chart()
      req(!is.null(ch))
      ch
    })
    outputOptions(output, "model_robustness_plot", suspendWhenHidden = TRUE)


    # Insert Diagnostics tab on first hist_sim; remove when cleared ----

    diag_tab_added <- reactiveVal(FALSE)

    observeEvent(hist_sim(),
      {
        if (is.null(hist_sim())) {
          if (diag_tab_added()) {
            shiny::removeTab(
              inputId = tabset_id,
              target  = "diag_tab",
              session = tabset_session
            )
            diag_tab_added(FALSE)
          }
          return()
        }

        # UI-50: one Diagnostics tab, not one per Step 2 run. The tab's contents
        # are a module UI bound to fixed output ids, so an already-present tab
        # needs no rebuild; the weather-variable pill re-renders itself from
        # selected_weather().
        if (!diag_tab_added()) {
          shiny::appendTab(
            inputId = tabset_id,
            shiny::tabPanel(
              title = "Diagnostics",
              value = "diag_tab",
              mod_2_03_diagnostics_ui(sub("-$", "", session$ns("")))
            ),
            select = FALSE,
            session = tabset_session
          )
          diag_tab_added(TRUE)
        }
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    # Suspend outputs when Results tab is hidden ----
    outputOptions(output, "diag_weather_vars_ui", suspendWhenHidden = TRUE)
    outputOptions(output, "diag_weather_scenario_ui", suspendWhenHidden = TRUE)
    outputOptions(output, "diag_weather_density", suspendWhenHidden = TRUE)
    outputOptions(output, "timeseries_plot", suspendWhenHidden = TRUE)

    # Return API ----
    list(diag_tab_added = diag_tab_added)
  })
}
