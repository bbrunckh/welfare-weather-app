#' 2_03_diagnostics UI Function
#'
#' @description A shiny Module. Renders the Diagnostics tab content:
#'   weather input density panel and welfare output ridge plots.
#'   Consolidates the former mod_2_05_sim_diag.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_03_diagnostics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # ---- 0. Scenario filters -----------------------------------------------
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
    )
  )
}


#' 2_03_diagnostics Server Functions
#'
#' Appends a Diagnostics tab to the main tabset once the historical simulation
#' has run. Weather density and welfare ridge panels refresh only when their
#' respective Update button is clicked.
#'
#' @param id               Module id.
#' @param hist_sim         ReactiveVal list with preds, so, weather_raw, train_data.
#' @param saved_scenarios  ReactiveVal holding named scenario entries.
#' @param survey_weather   Reactive data frame of merged survey-weather data.
#' @param selected_weather Reactive data frame of selected weather variable metadata.
#' @param tabset_id        Character id of the parent tabset panel.
#' @param tabset_session   Shiny session for the tabset.
#'
#' @noRd
mod_2_03_diagnostics_server <- function(id,
                                         hist_sim,
                                         saved_scenarios,
                                         survey_weather,
                                         selected_weather,
                                         tabset_id,
                                         tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # ---- Reactive computations ---------------------------------------------

    active_scenarios_data <- reactive({
      sc_all <- if (!is.null(saved_scenarios)) names(saved_scenarios()) else character(0)
      if (length(sc_all) == 0) return(character(0))

      sel_ssps <- input$filter_ssps %||% character(0)
      sel_yrs  <- input$filter_yrs  %||% character(0)

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

    output$weight_status_diag_ui <- shiny::renderUI({
      req(hist_sim())
      # Detect weight column independently of the toggle -- this allows
      # the amber state when the column exists but the toggle is OFF.
      has_w  <- !is.null(hist_sim()$pipeline$weight)
      tog_on <- isTRUE(input$use_weights_diag)
      if (has_w && tog_on)
        shiny::tags$p(
          style = "font-size:11px; color:#2e7d32; margin:2px 0 6px 0;",
          "✅ Survey weights found and applied (",
          shiny::tags$code("weight"), ")")
      else if (has_w && !tog_on)
        shiny::tags$p(
          style = "font-size:11px; color:#e65100; margin:2px 0 6px 0;",
          "⚠ Survey weights available but not applied")
      else
        shiny::tags$p(
          style = "font-size:11px; color:#c62828; margin:2px 0 6px 0;",
          "🔴 No weight column found — unweighted")
    })



    # ---- renderUI / render* outputs ----------------------------------------

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
        shiny::tags$div(
          style = "margin-top:8px; border-top:1px solid #e0e0e0; padding-top:6px;",
          shiny::checkboxInput(
            ns("show_regression_input"),
            label = "Include regression output",
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("use_weights_diag"),
            label = "Use survey weights (if available)",
            value = TRUE
          ),
          shiny::uiOutput(ns("weight_status_diag_ui"))
        )
      )
    })

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
    }) |> shiny::bindEvent(input$diag_update_weather, hist_sim(),
                           ignoreNULL = TRUE, ignoreInit = FALSE)



    # ---- Insert Diagnostics tab once (first hist_sim only) -----------------

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
          mod_2_03_diagnostics_ui(sub("-$", "", session$ns("")))
        ),
        select  = FALSE,
        session = tabset_session
      )

      shiny::updateSelectInput(session, "diag_weather_vars",
                               choices  = choices,
                               selected = choices[seq_len(min(2, length(choices)))])
    }, ignoreInit = TRUE, once = TRUE)

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

    # ---- Suspend outputs when Results tab is hidden ----------------------
    outputOptions(output, "scenario_filter_panel",   suspendWhenHidden = TRUE)
    outputOptions(output, "diag_weather_log_ui",     suspendWhenHidden = TRUE)
    outputOptions(output, "diag_weather_density",    suspendWhenHidden = TRUE)
    outputOptions(output, "weight_status_diag_ui",   suspendWhenHidden = TRUE)

    # ---- Return API --------------------------------------------------------
    list()
  })
}
