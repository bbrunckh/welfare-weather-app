#' 2_05_sim_diag UI Function
#'
#' @description A shiny Module. Renders the Diagnostics tab content:
#'   weather input density panel and welfare output ridge plots.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_05_sim_diag_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- 0. Scenario filters (reactive -- populated by server) -------------
    shiny::uiOutput(ns("scenario_filter_panel")),

    # ---- 1. Weather inputs panel -------------------------------------------
    shiny::wellPanel(
      shiny::h4("Weather input distributions"),
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap; margin-bottom:8px;",
        shiny::tags$div(style = "flex:3; min-width:200px;",
          shiny::selectInput(
            ns("diag_weather_vars"),
            label    = "Weather variables (select one or more)",
            choices  = character(0),
            selected = NULL,
            multiple = TRUE
          )
        )
      ),
      shiny::actionButton(
        ns("diag_update_weather"),
        "Update weather plot",
        class = "btn-sm btn-default",
        style = "margin-bottom:8px;"
      ),
      shiny::plotOutput(ns("diag_weather_density"), height = "340px"),
      # Per-variable log10 checkboxes -- rendered dynamically in server
      shiny::uiOutput(ns("diag_weather_log_ui")),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:4px;",
        shiny::tags$b("Grey fill = Full historical:"),
        " all years at survey locations and months.",
        shiny::tags$br(),
        shiny::tags$b("Black dashed = Regression input"),
        " (shown when 'Include regression output' is selected above).",
        shiny::tags$br(),
        shiny::tags$b("Coloured lines = Future scenarios:"),
        " solid = earliest simulation year, dashed = middle, dotted = latest."
      )
    ),

    # ---- 2. Welfare distributions panel ------------------------------------
    shiny::wellPanel(
      shiny::h4("Welfare output distributions"),

      # -- Primary grouping radio -------------------------------------------
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:16px; flex-wrap:wrap; margin-bottom:4px;",
        shiny::tags$div(style = "flex:2; min-width:260px;",
          shiny::radioButtons(
            ns("diag_ridge_primary_group"),
            label    = "Primary grouping",
            choices  = c(
              "Historical year"                 = "hist_year",
              "Scenario \u00d7 Simulation year" = "scenario",
              "Simulation year \u00d7 Scenario" = "forecast_yr"
            ),
            # NOTE: default to scenario grouping so the tab opens showing
            # climate scenario comparison rather than historical-year detail.
            selected = "scenario",
            inline   = TRUE
          )
        )
      ),

      # -- Welfare output-specific controls ----------------------------------
      shiny::tags$div(
        style = "display:flex; gap:24px; flex-wrap:wrap; margin-bottom:4px; align-items:center;",
        shiny::tags$div(
          shiny::checkboxInput(
            ns("diag_ridge_log"),
            label = "Log\u2081\u2080 x-axis",
            value = FALSE
          )
        )
      ),
      shiny::tags$div(
        style = "max-width:380px; margin-bottom:2px;",
        shiny::sliderInput(
          ns("diag_ridge_scale"),
          label = "Ridge height",
          min   = 0.3,
          max   = 3.0,
          value = 1.5,
          step  = 0.1
        )
      ),
      shiny::tags$div(
        style = "max-width:380px; margin-bottom:8px;",
        shiny::sliderInput(
          ns("diag_ridge_spacing"),
          label = "Row spacing",
          min   = 0.2,
          max   = 3.0,
          value = 1.0,
          step  = 0.1
        )
      ),

      # -- Caption -----------------------------------------------------------
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-bottom:8px;",
        shiny::tags$b("Historical year mode:"),
        " one grey filled ridge per simulation year; coloured lines = scenario perturbations.",
        shiny::tags$br(),
        shiny::tags$b("Scenario / Simulation year mode:"),
        " one row per scenario or simulation year; grey-scale lines = individual historical years",
        " (darkest = most recent).",
        shiny::tags$br(),
        shiny::tags$b("Include regression output:"),
        " overlays predicted (dashed) and actual (dotted) outcome densities from training data.",
        shiny::tags$br(),
        "All ridges share a common global bandwidth. X-axis clipped to P1\u2013P99."
      ),

      shiny::actionButton(
        ns("diag_update_ridge"),
        "Update ridge plot",
        class = "btn-sm btn-default",
        style = "margin-bottom:8px;"
      ),

      # -- Dynamic height plotOutput -----------------------------------------
      shiny::uiOutput(ns("diag_ridge_plot_ui"))
    )
  )
}


#' 2_05_sim_diag Server Functions
#'
#' Appends a Diagnostics tab to the main tabset once the historical simulation
#' has run. Weather density and welfare ridge panels refresh only when their
#' respective Update button is clicked.
#'
#' @param id               Module id.
#' @param hist_sim         Reactive list. Must have $preds, $so, $weather_raw.
#' @param saved_scenarios  ReactiveVal holding named scenario entries.
#' @param survey_weather   Reactive data frame of merged survey-weather data.
#' @param selected_weather Reactive data frame of selected weather variable
#'   metadata.
#' @param tabset_id        Character id of the parent tabset panel.
#' @param tabset_session   Shiny session for the tabset.
#'
#' @noRd
mod_2_05_sim_diag_server <- function(id,
                                      hist_sim         = NULL,
                                      saved_scenarios  = NULL,
                                      survey_weather   = NULL,
                                      selected_weather = NULL,
                                      tabset_id,
                                      tabset_session   = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # ---- Reactive computations ---------------------------------------------

    active_scenarios_data <- reactive({
      sc_all <- if (!is.null(saved_scenarios)) names(saved_scenarios()) else character(0)
      if (length(sc_all) == 0) return(character(0))

      sel_ssps <- input$filter_ssps %||% character(0)
      sel_yrs  <- input$filter_yrs  %||% character(0)

      # When both filters are empty return no scenarios (nothing selected = nothing shown).
      # This is the intentional default: users must opt in to show scenario overlays.
      if (length(sel_ssps) == 0L && length(sel_yrs) == 0L) return(character(0))

      Filter(function(nm) {
        ssp    <- .normalise_ssp(nm)
        yr     <- .parse_year(nm)
        ssp_ok <- length(sel_ssps) == 0L || isTRUE(ssp %in% sel_ssps)
        yr_ok  <- length(sel_yrs)  == 0L || isTRUE(yr  %in% sel_yrs)
        ssp_ok && yr_ok
      }, sc_all)
    })

    scenario_weather_data <- reactive({
      sc <- if (!is.null(saved_scenarios)) saved_scenarios() else list()
      if (length(sc) == 0) return(NULL)
      out <- lapply(sc, function(e) e$weather_raw)
      out <- Filter(Negate(is.null), out)
      if (length(out) == 0) NULL else out
    })

    # Expensive KDE calls -- only re-run when saved_scenarios changes, not on
    # aesthetic input changes (log, ridge_scale, row_gap) or filter ticks.
    ridge_kde_data <- reactive({
      req(hist_sim())

      sc_raw    <- if (!is.null(saved_scenarios)) saved_scenarios() else list()
      scen_list <- lapply(sc_raw, function(e) list(preds = e$preds, so = e$so))

      build_ridge_kde_data(
        hist_preds    = hist_sim()$preds,
        scenario_list = scen_list,
        outcome_name  = hist_sim()$so$name,
        actual_vals   = {
          td <- hist_sim()$train_data
          so <- hist_sim()$so
          nm <- so$name
          if (!is.null(td) && nm %in% names(td)) {
            v <- as.numeric(td[[nm]])
            v <- v[is.finite(v)]
            if (isTRUE(so$transform == "log")) v <- exp(v)
            v
          } else numeric(0)
        }
      )
    })

    # Debounce slider drags: batch rapid ticks into one render after 350ms.
    debounced_ridge_inputs <- shiny::debounce(
      reactive({
        list(
          log_scale       = isTRUE(input$diag_ridge_log),
          ridge_scale     = input$diag_ridge_scale   %||% 1.5,
          row_gap         = input$diag_ridge_spacing %||% 1.0,
          primary_group   = input$diag_ridge_primary_group %||% "scenario",
          show_regression = input$show_regression_input %||% TRUE
        )
      }),
      350
    )

    # ---- renderUI / render* outputs ----------------------------------------

    # Scenario filter panel -- rebuilds as scenarios are added after tab-open.
    output$scenario_filter_panel <- shiny::renderUI({
      sc_all      <- if (!is.null(saved_scenarios)) names(saved_scenarios()) else character(0)
      unique_ssps <- sort(unique(Filter(Negate(is.na),
                                        vapply(sc_all, .normalise_ssp, character(1)))))
      unique_yrs  <- sort(unique(Filter(Negate(is.na),
                                        vapply(sc_all, .parse_year,    character(1)))))

      shiny::wellPanel(
        style = "padding: 10px 16px 8px 16px; background:#f8f8f8; margin-bottom:10px;",
        shiny::tags$div(
          style = "display:flex; align-items:baseline; gap:8px; margin-bottom:10px;",
          shiny::tags$b("Scenario Filters", style = "font-size:13px;"),
          shiny::tags$span(
            style = "font-size:11px; color:#888;",
            "\u2014 applies to all panels below"
          )
        ),
        shiny::tags$div(
          style = "display:flex; flex-wrap:wrap; gap:24px; align-items:flex-start;",
          if (length(unique_ssps) > 0)
            shiny::tags$div(
              shiny::checkboxGroupInput(
                inputId  = ns("filter_ssps"),
                label    = shiny::tags$b("Climate scenario",
                             style = "font-size:11px; font-weight:600;"),
                choices  = setNames(unique_ssps, unique_ssps),
                selected = character(0),
                inline   = TRUE
              )
            ),
          if (length(unique_yrs) > 0)
            shiny::tags$div(
              shiny::checkboxGroupInput(
                inputId  = ns("filter_yrs"),
                label    = shiny::tags$b("Simulation year",
                             style = "font-size:11px; font-weight:600;"),
                choices  = setNames(unique_yrs, unique_yrs),
                selected = character(0),
                inline   = TRUE
              )
            )
        ),
        # Regression output toggle (separate row)
        shiny::tags$div(
          style = "margin-top:8px; border-top:1px solid #e0e0e0; padding-top:6px;",
          shiny::checkboxInput(
            ns("show_regression_input"),
            label = "Include regression output",
            value = TRUE
          )
        )
      )
    })

    # One checkbox per currently selected weather variable, using readable label.
    output$diag_weather_log_ui <- shiny::renderUI({
      vars <- input$diag_weather_vars
      req(length(vars) > 0)
      sw      <- if (!is.null(selected_weather)) selected_weather() else NULL
      lbl_map <- if (!is.null(sw) && all(c("name", "label") %in% names(sw)))
        setNames(sw$label, sw$name) else setNames(vars, vars)

      shiny::tags$div(
        style = "display:flex; flex-wrap:wrap; gap:16px; margin-top:6px;",
        lapply(seq_along(vars), function(i) {
          v   <- vars[[i]]
          lbl <- lbl_map[[v]] %||% v
          shiny::checkboxInput(
            inputId = ns(paste0("diag_log_var_", i)),
            label   = paste0("Log\u2081\u2080: ", lbl),
            value   = FALSE
          )
        })
      )
    })

    # Render only when Update weather plot button is clicked (or on first hist_sim).
    output$diag_weather_density <- renderPlot({
      req(hist_sim(), survey_weather())
      req(!is.null(hist_sim()$weather_raw))
      vars <- input$diag_weather_vars
      req(length(vars) > 0)

      sw      <- if (!is.null(selected_weather)) selected_weather() else NULL
      lbl_map <- if (!is.null(sw) && all(c("name", "label") %in% names(sw)))
        setNames(sw$label, sw$name) else NULL

      log_x_vec <- vapply(seq_along(vars), function(i)
        isTRUE(input[[paste0("diag_log_var_", i)]]), logical(1))

      plot_weather_density_panel(
        survey_weather   = survey_weather(),
        weather_raw      = hist_sim()$weather_raw,
        weather_vars     = vars,
        weather_labels   = lbl_map,
        scenario_weather = scenario_weather_data(),
        active_scenarios = active_scenarios_data(),
        log_x            = log_x_vec,
        show_regression  = input$show_regression_input %||% TRUE
      )
    }) |> shiny::bindEvent(input$diag_update_weather, hist_sim(), ignoreNULL = TRUE, ignoreInit = FALSE)

    # Dynamic height: ~80px per row x row_gap, clamped to [500, 4000]px.
    # Recomputes on button click so height adjusts when filters or grouping change.
    output$diag_ridge_plot_ui <- shiny::renderUI({
      req(hist_sim())
      ri      <- debounced_ridge_inputs()
      hp      <- hist_sim()$preds
      yr_col  <- intersect(c("sim_year", "year"), names(hp))[1]
      n_yrs   <- if (!is.na(yr_col)) length(unique(hp[[yr_col]])) else 20L
      n_scen  <- length(active_scenarios_data())
      # forecast_yr key retained internally; label is "Simulation year x Scenario"
      n_rows  <- if (ri$primary_group %in% c("scenario", "forecast_yr")) max(1L, n_scen) else n_yrs
      plot_ht <- min(4000L, max(500L, as.integer(n_rows * 80L * ri$row_gap + 160L)))
      shiny::plotOutput(ns("diag_ridge"), height = paste0(plot_ht, "px"))
    }) |> shiny::bindEvent(input$diag_update_ridge, hist_sim(), ignoreNULL = TRUE, ignoreInit = FALSE)

    # Render only when Update ridge plot button is clicked (or on first hist_sim).
    output$diag_ridge <- renderPlot({
      req(hist_sim())
      kd <- ridge_kde_data()
      req(!is.null(kd))
      ri <- debounced_ridge_inputs()

      plot_year_anchored_ridge(
        kde_data        = kd,
        x_label         = hist_sim()$so$label %||% hist_sim()$so$name,
        primary_group   = ri$primary_group,
        log_scale       = ri$log_scale,
        ridge_scale     = ri$ridge_scale,
        row_gap         = ri$row_gap,
        show_regression = ri$show_regression,
        scenario_names  = active_scenarios_data()
      )
    }) |> shiny::bindEvent(input$diag_update_ridge, hist_sim(), ignoreNULL = TRUE, ignoreInit = FALSE)

    # ---- observeEvent handlers ---------------------------------------------

    # Insert Diagnostics tab once (first hist_sim only).
    # updateSelectInput is called here too -- this is the only safe moment
    # for first-load population since diag_weather_vars does not exist in
    # the DOM until appendTab has fired.
    # NOTE: session$ns("") returns the full nested namespace string (e.g.
    # "step2-sim_diag-"). Strip trailing dash before passing to
    # mod_2_05_sim_diag_ui() so DOM ids match server session$ns() exactly.
    observeEvent(hist_sim(), {
      req(hist_sim())

      sw      <- if (!is.null(selected_weather)) selected_weather() else NULL
      choices <- if (!is.null(sw) && "name" %in% names(sw)) {
        if ("label" %in% names(sw)) setNames(sw$name, sw$label) else sw$name
      } else character(0)

      shiny::appendTab(
        inputId = tabset_id,
        shiny::tabPanel(
          title = "Diagnostics",
          value = "diag_tab",
          mod_2_05_sim_diag_ui(sub("-$", "", session$ns("")))
        ),
        select  = FALSE,
        session = tabset_session
      )

      shiny::updateSelectInput(session, "diag_weather_vars",
                               choices  = choices,
                               selected = choices[seq_len(min(2, length(choices)))])

    }, ignoreInit = TRUE, once = TRUE)

    # Refresh choices on every model re-run with different weather variables.
    # ignoreInit = TRUE -- first population is handled by the appendTab observer.
    # No req(hist_sim()) guard -- update must land even before the sim fires.
    observeEvent(selected_weather(), {
      sw      <- if (!is.null(selected_weather)) selected_weather() else NULL
      choices <- if (!is.null(sw) && "name" %in% names(sw)) {
        if ("label" %in% names(sw)) setNames(sw$name, sw$label) else sw$name
      } else character(0)
      current <- isolate(input$diag_weather_vars)
      new_sel <- if (length(current) > 0) intersect(current, choices) else character(0)
      if (length(new_sel) == 0) new_sel <- choices[seq_len(min(2, length(choices)))]
      shiny::updateSelectInput(session, "diag_weather_vars",
                               choices  = choices,
                               selected = new_sel)
    }, ignoreInit = TRUE)

    # ---- Return API --------------------------------------------------------

    list()
  })
}
