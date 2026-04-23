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
    ),

    # ---- 2. Uncertainty decomposition chart ----------------------------------
    shiny::wellPanel(
      shiny::h4("Decomposed uncertainty: annual variability vs. model spread"),
      shiny::plotOutput(ns("uncertainty_decomp_plot"), height = "750px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        shiny::tags$b("Annual variability:"),
        " CI from model-averaged annual outcomes \u2014 captures weather-driven year-to-year variation.",
        shiny::tags$br(),
        shiny::tags$b("Model uncertainty:"),
        " CI from year-averaged per-model outcomes \u2014 captures disagreement across CMIP6 ensemble members.",
        shiny::tags$br(),
        shiny::tags$b("Combined:"),
        " both sources pooled.",
        shiny::tags$br(),
        "Aggregation: mean. Dashed line = historical mean."
      )
    ),

    # ---- 3. Welfare distributions panel ------------------------------------
    shiny::wellPanel(
      shiny::h4("Welfare output distributions"),
      shiny::radioButtons(
        ns("diag_ridge_primary_group"),
        label    = "Primary grouping",
        choices  = c(
          "Historical year"                 = "hist_year",
          "Scenario \u00d7 Simulation year" = "scenario",
          "Simulation year \u00d7 Scenario" = "forecast_yr"
        ),
        selected = "scenario",
        inline   = TRUE
      ),
      shiny::tags$div(
        style = "display:flex; gap:24px; flex-wrap:wrap; margin-bottom:4px; align-items:center;",
        shiny::tags$div(
          shiny::checkboxInput(ns("diag_ridge_log"), label = "Log\u2081\u2080 x-axis", value = FALSE)
        )
      ),
      shiny::tags$div(
        style = "max-width:380px; margin-bottom:2px;",
        shiny::sliderInput(ns("diag_ridge_scale"), label = "Ridge height",
                           min = 0.3, max = 3.0, value = 1.5, step = 0.1)
      ),
      shiny::tags$div(
        style = "max-width:380px; margin-bottom:8px;",
        shiny::sliderInput(ns("diag_ridge_spacing"), label = "Row spacing",
                           min = 0.2, max = 3.0, value = 1.0, step = 0.1)
      ),
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
      shiny::uiOutput(ns("diag_ridge_plot_ui"))
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

    # Detect weight column once -- passed to aggregate_sim_preds() when toggle is on.
    weight_col_diag <- reactive({
      req(hist_sim())
      if (!isTRUE(input$use_weights_diag)) return(NULL)
      w <- grep("^weight$|^hhweight$|^wgt$|^pw$", names(hist_sim()$preds),
                value = TRUE, ignore.case = TRUE)[1]
      if (is.na(w)) NULL else w
    })

    output$weight_status_diag_ui <- shiny::renderUI({
      req(hist_sim())
      # Detect weight column independently of the toggle -- this allows
      # the amber state when the column exists but the toggle is OFF.
      w      <- grep("^weight$|^hhweight$|^wgt$|^pw$",
                     names(hist_sim()$preds),
                     value = TRUE, ignore.case = TRUE)[1]
      has_w  <- length(w) > 0 && !is.na(w)
      tog_on <- isTRUE(input$use_weights_diag)
      if (has_w && tog_on)
        shiny::tags$p(
          style = "font-size:11px; color:#2e7d32; margin:2px 0 6px 0;",
          "✅ Survey weights found and applied (",
          shiny::tags$code(w), ")")
      else if (has_w && !tog_on)
        shiny::tags$p(
          style = "font-size:11px; color:#e65100; margin:2px 0 6px 0;",
          "⚠ Survey weights available but not applied")
      else
        shiny::tags$p(
          style = "font-size:11px; color:#c62828; margin:2px 0 6px 0;",
          "🔴 No weight column found — unweighted")
    })

    # TODO: pending decision on household-level draw storage — unlinked.
    # Future scenarios no longer store raw preds (replaced by pre-aggregated $agg).
    # ridge_kde_data now only uses hist preds; scenario ridges are unavailable.
    ridge_kde_data <- reactive({
      req(hist_sim())
      build_ridge_kde_data(
        hist_preds    = hist_sim()$preds,
        scenario_list = list(),  # scenario preds not retained — see architecture note
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

    # Aggregated series for the uncertainty decomposition chart.
    # Fixed at mean / no deviation — appropriate for a diagnostic view.
    agg_hist_diag <- reactive({
      req(hist_sim())
      aggregate_sim_preds(hist_sim()$preds, hist_sim()$so,
                          "mean", "none", FALSE, NULL, weight_col_diag())
    })

    agg_scenarios_diag <- reactive({
      sc <- if (!is.null(saved_scenarios)) saved_scenarios() else list()
      if (length(sc) == 0) return(list())
      lapply(sc, function(s) {
        tryCatch({
          if (!is.null(s$agg)) {
            # New schema: filter pre-aggregated summary by method = "mean"
            out <- dplyr::filter(s$agg, .data$agg_method == "mean")
            list(out = out, x_label = "Mean welfare")
          } else if (!is.null(s$preds)) {
            # Legacy fallback
            aggregate_sim_preds(s$preds, s$so, "mean", "none", FALSE, NULL, weight_col_diag())
          } else NULL
        }, error = function(e) NULL)
      })
    })
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

    output$uncertainty_decomp_plot <- renderPlot({
      req(agg_hist_diag())
      all_series_diag <- c(
        list(Historical = agg_hist_diag()),
        Filter(Negate(is.null), agg_scenarios_diag())
      )
      plot_uncertainty_decomposition(
        scenarios = all_series_diag,
        hist_agg  = agg_hist_diag()
      )
    }, height = 750)
    outputOptions(output, "uncertainty_decomp_plot", suspendWhenHidden = FALSE)

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

    # DEPRECATED — welfare ridge plot unlinked pending architectural review.
    # Future scenario preds are no longer stored household-level (replaced by $agg).
    # Historical ridge (hist only) still works but is hidden from UI.
    # TODO: revisit if household-level draw storage is reintroduced.
    output$diag_ridge_plot_ui <- shiny::renderUI({
      shiny::tags$p(
        style = "color:#888; font-size:12px; padding:8px;",
        "Welfare ridge plot temporarily unavailable.",
        "Scenario-level household predictions are not retained in the current",
        "memory-efficient architecture. Historical ridge remains available below."
      )
    })

    # output$diag_ridge is unlinked — kept for reference
    # output$diag_ridge <- renderPlot({ ... })

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

    # ---- Return API --------------------------------------------------------
    list()
  })
}
