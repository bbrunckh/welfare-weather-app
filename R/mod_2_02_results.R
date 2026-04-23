#' 2_02_results UI Function
#'
#' @description A shiny Module. Renders the Results tab content: point-range
#'   chart, threshold table/bar, and exceedance curve. Consolidates logic from
#'   the former mod_2_02_historical_sim (tab insertion) and
#'   mod_2_06_sim_compare (all visualisations).
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_02_results_ui <- function(id) {
  # Placeholder — the real content is injected via insertUI in the server.
  tagList()
}


#' Results tab content UI (inserted into the Results tabPanel once).
#' @noRd
.results_content_ui <- function(ns, so) {
  tagList(
    # ---- 1. Results header -------------------------------------------------
    shiny::uiOutput(ns("results_header_ui")),

    # ---- 2. Analysis controls ----------------------------------------------
    shiny::wellPanel(
      style = "padding: 10px 14px 6px 14px;",
      shiny::tags$p("Outcome of interest:",
                    style = "font-weight:600; margin-bottom:4px;"),
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap;",
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::selectInput(
            ns("cmp_agg_method"),
            label    = "Aggregation method",
            choices  = hist_aggregate_choices(so$type, so$name),
            selected = "mean"
          )
        ),
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::selectInput(
            ns("cmp_deviation"),
            label    = "Express as deviation from",
            choices  = c(
              "None (raw value)" = "none",
              "Mean year"        = "mean",
              "Median year"      = "median"
            ),
            selected = "none"
          )
        ),
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::uiOutput(ns("cmp_pov_line_ui"))
        )
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),
      shiny::tags$p("Scenario Filters",
                    style = "font-weight:600; margin-bottom:4px;"),
      shiny::fluidRow(
        shiny::column(12, shiny::uiOutput(ns("scenario_filter_ui")))
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),
      shiny::checkboxInput(
        ns("use_weights"),
        label = "Use survey weights (if available)",
        value = TRUE
      ),
      shiny::uiOutput(ns("weight_status_ui")),
      shiny::radioButtons(
        ns("cmp_group_order"),
        label    = "Group charts and tables by",
        choices  = c(
          "Scenario \u00d7 Year" = "scenario_x_year",
          "Year \u00d7 Scenario" = "year_x_scenario"
        ),
        selected = "scenario_x_year",
        inline   = TRUE
      )
    ),

    # ---- 3. Hero point-range chart -----------------------------------------
    shiny::wellPanel(
      shiny::h4("Distribution of outcomes across climate scenarios and timeframes"),
      shiny::plotOutput(ns("summary_box_plot"), height = "600px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "The central dot shows the mean annual simulated value; the thick",
        "coloured line spans the 90% interval (P5-P95); the thin grey line",
        "spans the 95% interval (P2.5-P97.5).",
        shiny::tags$br(),
        "Future scenarios apply climate-adjusted shifts to the same historical annual base.",
        shiny::tags$br(),
        "Dashed line = historical mean."
      )
    ),

    # ---- 4. Exceedance curve -----------------------------------------------
    shiny::wellPanel(
      shiny::h4("Exceedance probability by climate scenario"),
      shiny::tags$div(
        style = "display:flex; gap:20px; flex-wrap:wrap; margin-bottom:6px;",
        shiny::checkboxInput(
          ns("exceedance_logit_x"),
          "Logit probability axis (emphasise both tails)",
          value = FALSE
        ),
        shiny::checkboxInput(
          ns("show_return_period"),
          "Show return period lines",
          value = TRUE
        )
      ),
      shiny::plotOutput(ns("exceedance_plot"), height = "400px"),
      shiny::uiOutput(ns("exceedance_caption"))
    ),

    # ---- 5. Threshold table ------------------------------------------------
    shiny::wellPanel(
      shiny::uiOutput(ns("threshold_table_header")),
      DT::DTOutput(ns("summary_threshold_table")),
      shiny::uiOutput(ns("threshold_table_footer"))
    )
  )
}


#' 2_02_results Server Functions
#'
#' Appends a Results tab to the main tabset once the historical simulation
#' has run. All comparison outputs update reactively as saved_scenarios change.
#'
#' @param id              Module id.
#' @param hist_sim        ReactiveVal list with preds, so, weather_raw, train_data.
#' @param saved_scenarios ReactiveVal holding named scenario entries.
#' @param selected_hist   Reactive one-row data frame from weathersim.
#' @param tabset_id       Character id of the parent tabset panel.
#' @param tabset_session  Shiny session for the tabset.
#'
#' @noRd
mod_2_02_results_server <- function(id,
                                     hist_sim,
                                     saved_scenarios,
                                     selected_hist,
                                     tabset_id,
                                     tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # ---- Reactive computations (carried over from mod_2_06) ----------------

    hist_label <- reactive({
      nm <- if (!is.null(selected_hist)) selected_hist()$scenario_name else NULL
      if (!is.null(nm) && nzchar(nm)) nm else "Historical"
    })

    all_ssps <- reactive({
      sc <- saved_scenarios()
      if (length(sc) == 0) return(character(0))
      ssps <- unique(.normalise_ssp(names(sc)))
      sort(ssps[!is.na(ssps) & grepl("^SSP", ssps)])
    })

    all_anchor_years <- reactive({
      sc <- saved_scenarios()
      if (length(sc) == 0) return(character(0))
      ranges <- sort(na.omit(unique(.parse_year(names(sc)))))
      setNames(sub("-", "_", ranges), ranges)
    })

    all_models_info <- reactive({
      sc <- saved_scenarios()
      if (length(sc) == 0) return(character(0))
      # Return model counts per scenario for display
      vapply(sc, function(s) s$n_models %||% 1L, integer(1))
    })


    # Detect weight column name once -- passed to aggregate_sim_preds().
    # NULL when no weight column exists in preds or toggle is off.
    weight_col <- reactive({
      req(hist_sim())
      if (!isTRUE(input$use_weights)) return(NULL)

      if ("weight" %in% names(hist_sim()$preds)) "weight" else NULL
    })

    output$weight_status_ui <- shiny::renderUI({
      req(hist_sim())
      # Detect weight column independently of the toggle -- this allows
      # the amber state when the column exists but the toggle is OFF.
      has_w  <- "weight" %in% names(hist_sim()$preds)

      tog_on <- isTRUE(input$use_weights)
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

    pov_line_val <- reactive({
      # Poverty line is baked in at simulation time (input$pov_line_sim).
      # Read it back from hist_sim()$pov_line for the historical fallback path.
      # For future scenarios, pov_line is already embedded in s$agg.
      if (isTRUE(input$cmp_agg_method %in% c("headcount_ratio", "gap", "fgt2"))) {
        if (!is.null(hist_sim())) hist_sim()$pov_line %||% NULL else NULL
      } else {
        NULL
      }
    })

    selected_scenario_names <- reactive({
      sc   <- saved_scenarios()
      if (length(sc) == 0) return(character(0))
      nms  <- names(sc)
      ssps <- if (length(input$filter_ssp)  == 0) all_ssps()                          else input$filter_ssp
      # Decode safe values back to "YYYY-YYYY" ranges for grepl matching
      yr_vals <- if (length(input$filter_year) == 0) names(all_anchor_years())        else sub("_", "-", input$filter_year)
      keep <- vapply(nms, function(nm) {
        is_ssp <- grepl("^SSP", nm)
        if (!is_ssp) return(TRUE)
        ssp_match <- any(vapply(ssps, function(s) startsWith(nm, s), logical(1)))
        yr_match  <- length(yr_vals) == 0 ||
          any(vapply(yr_vals, function(y) grepl(y, nm, fixed = TRUE), logical(1)))
        ssp_match && yr_match
      }, logical(1))
      nms[keep]
    })

    agg_hist <- reactive({
      req(hist_sim())
      method    <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation  %||% "none"
      use_w     <- isTRUE(input$use_weights)

      req(!is.null(hist_sim()$agg))

      out <- dplyr::filter(hist_sim()$agg,
                           .data$agg_method == method,
                           .data$weighted   == use_w)

      # Weighted fallback: if use_weights = TRUE but no weighted rows exist
      # (survey has no weight column or pre-aggregation failed), fall back to
      # unweighted to avoid returning an empty tibble that crashes charts.
      if (nrow(out) == 0L && isTRUE(use_w)) {
        warning("[agg_hist] No weighted rows found — falling back to unweighted")
        out <- dplyr::filter(hist_sim()$agg,
                             .data$agg_method == method,
                             .data$weighted   == FALSE)
      }

      # ⚠️ METHODOLOGICAL FLAG (Option B):
      # Deviation is computed relative to each scenario's OWN mean/median.
      # Option A would centre future scenarios relative to the historical mean —
      # preserving the direction of climate impact but losing within-scenario variance.
      # Option B centres each scenario on itself — useful for comparing spread/variance
      # across scenarios but not for showing directional welfare loss.
      # Pending theoretical review before production use of deviation mode.
      if (!identical(deviation, "none") && nrow(out) > 0) {
        out <- deviation_from_centre(
          df     = out,
          group  = "sim_year",
          centre = deviation,
          loss   = FALSE
        )
        if ("deviation" %in% names(out)) {
          out$value <- out$deviation
          out$deviation <- NULL
        }
      }

      x_label <- if (identical(deviation, "none")) {
        label_agg_method(method)
      } else {
        paste0(label_agg_method(method), " — ", label_deviation(deviation))
      }

      list(out = out, x_label = x_label)
    })

    agg_scenarios <- reactive({
      sc <- saved_scenarios()
      if (length(sc) == 0) return(list())
      method    <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation  %||% "none"
      use_w     <- isTRUE(input$use_weights)

      x_label <- if (identical(deviation, "none")) {
        label_agg_method(method)
      } else {
        paste0(label_agg_method(method), " — ", label_deviation(deviation))
      }

      result <- lapply(sc, function(s) {
        tryCatch({
          if (!is.null(s$agg)) {
            out <- dplyr::filter(s$agg,
                                 .data$agg_method == method,
                                 .data$weighted   == use_w)
            # Weighted fallback: if use_weights = TRUE but no weighted rows
            # exist, fall back to unweighted rather than returning empty tibble.
            if (nrow(out) == 0L && isTRUE(use_w)) {
              out <- dplyr::filter(s$agg,
                                   .data$agg_method == method,
                                   .data$weighted   == FALSE)
            }
            # ⚠️ METHODOLOGICAL FLAG (Option B): see agg_hist above.
            if (!identical(deviation, "none") && nrow(out) > 0) {
              out <- deviation_from_centre(
                df     = out,
                group  = "sim_year",
                centre = deviation,
                loss   = FALSE
              )
              if ("deviation" %in% names(out)) {
                out$value <- out$deviation
                out$deviation <- NULL
              }
            }
            list(out = out, x_label = x_label)
          } else if (!is.null(s$preds)) {
            # Legacy fallback: raw preds still present (e.g. older saved state)
            pov <- if (method %in% c("headcount_ratio", "gap", "fgt2")) pov_line_val() else NULL
            aggregate_sim_preds(s$preds, s$so, method, deviation, FALSE, pov, weight_col())
          } else NULL
        }, error = function(e) {
          message("[agg_scenarios] ERROR: ", conditionMessage(e))
          NULL
        })
      })
      result
    })
    all_series <- reactive({
      sc  <- agg_scenarios()
      sel <- selected_scenario_names()
      c(setNames(list(agg_hist()), hist_label()), sc[intersect(sel, names(sc))])
    })

    table_subtitle <- reactive({
      req(agg_hist(), input$cmp_agg_method, input$cmp_deviation)
      paste0(
        agg_hist()$x_label, " \u2014 ",
        label_agg_method(input$cmp_agg_method), " | ",
        label_deviation(input$cmp_deviation)
      )
    })

    # ---- renderUI / render* outputs ----------------------------------------

    output$results_header_ui <- renderUI({
      req(hist_sim(), input$cmp_agg_method, input$cmp_deviation)
      so <- hist_sim()$so

      agg_label    <- label_agg_method(input$cmp_agg_method)
      dev_label    <- label_deviation(input$cmp_deviation)
      pov_txt      <- if (!is.null(pov_line_val()))
        paste0(" | Poverty line: $", pov_line_val(), "/day") else ""

      notes_txt <- paste0(
        "Showing ", agg_label, " of ", so$label %||% so$name,
        " expressed as ", dev_label, pov_txt, "."
      )

      shiny::div(
        style = paste0(
          "border-left: 4px solid #2166ac; background: #f4f8fd; ",
          "padding: 10px 14px; margin-bottom: 12px; border-radius: 3px;"
        ),
        shiny::tags$strong(style = "font-size:15px;",
                           paste0("Results: ", so$label %||% so$name)),
        shiny::tags$br(),
        shiny::tags$span(style = "color:#555; font-size:12px;", notes_txt)
      )
    })

    output$cmp_pov_line_ui <- renderUI({
      req(input$cmp_agg_method)
      if (input$cmp_agg_method %in% c("headcount_ratio", "gap", "fgt2")) {
        # Poverty line is set at simulation time (input$pov_line_sim in mod_2_01).
        # Post-hoc adjustment requires re-running the simulation.
        pov_val <- if (!is.null(hist_sim()$pov_line)) sprintf("$%.2f", hist_sim()$pov_line) else "not yet set"
        shiny::helpText(
          style = "font-size:11px; color:#555; margin-bottom:6px;",
          paste0("Poverty line: ", pov_val, "/day (set at simulation time). To change, update simulation settings and re-run.")
        )
      }
    })

    output$scenario_filter_ui <- renderUI({
      sc <- saved_scenarios()
      if (length(sc) == 0)
        return(shiny::helpText("Run a simulation."))
      ssps <- all_ssps()
      yrs  <- all_anchor_years()
      mi   <- all_models_info()
      tagList(
        shiny::fluidRow(
          shiny::column(4,
            if (length(yrs) > 0)
              shiny::checkboxGroupInput(
                ns("filter_year"), label = "Projection periods",
                choices = yrs, selected = yrs, inline = TRUE
              )
          ),
          shiny::column(4,
            if (length(ssps) > 0)
              shiny::checkboxGroupInput(
                ns("filter_ssp"), label = "SSPs",
                choices = ssps, selected = ssps, inline = TRUE
              )
          ),
          shiny::column(4,
            if (any(mi > 1L))
              shiny::helpText(
                style = "font-size:11px; color:#555; margin-top:24px;",
                paste0("Each SSP aggregates results from ",
                       max(mi), " ensemble model(s).")
              )
          )
        )
      )
    })

    output$summary_box_plot <- renderPlot({
      req(agg_hist())
      plot_pointrange_climate(
        scenarios   = all_series(),
        hist_agg    = agg_hist(),
        group_order = input$cmp_group_order %||% "scenario_x_year"
      )
    }, height = 600)
    outputOptions(output, "summary_box_plot", suspendWhenHidden = FALSE)

    output$summary_threshold_table <- DT::renderDT({
      req(agg_hist())
      df <- build_threshold_table_df(
        all_series  = all_series(),
        group_order = input$cmp_group_order %||% "scenario_x_year"
      )
      if (is.null(df))
        return(DT::datatable(data.frame(Message = "Insufficient data"),
                             rownames = FALSE, class = "compact stripe",
                             options  = list(dom = "t")))
      DT::datatable(
        df, rownames = FALSE, class = "compact stripe",
        options = list(
          pageLength = 15, dom = "t", ordering = FALSE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      )
    })
    outputOptions(output, "summary_threshold_table", suspendWhenHidden = FALSE)

    output$threshold_table_header <- renderUI({
      req(agg_hist())
      tagList(
        shiny::h4("Outcome value at return-period thresholds (both tails)"),
        shiny::tags$small(class = "text-muted", table_subtitle())
      )
    })

    output$threshold_table_footer <- renderUI({
      req(agg_hist())
      tagList(
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;",
                      "Low odds show the value exceeded in only 1-in-N years."),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
                      "High odds show the value reached in all but 1-in-N years."),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0;",
                      "1:1 shows the median (50th percentile) simulated value.")
      )
    })

    output$exceedance_plot <- renderPlot({
      req(agg_hist())
      enhance_exceedance(
        scenarios     = all_series(),
        hist_agg      = agg_hist(),
        x_label       = agg_hist()$x_label,
        return_period = isTRUE(input$show_return_period),
        n_sim_years   = nrow(agg_hist()$out),
        logit_x       = isTRUE(input$exceedance_logit_x)
      )
    })
    outputOptions(output, "exceedance_plot", suspendWhenHidden = FALSE)

    output$exceedance_caption <- renderUI({
      req(agg_hist())
      axis_txt <- if (isTRUE(input$exceedance_logit_x))
        "Probability axis is logit-scaled, giving equal visual weight to both tails."
      else
        "The curve shows the estimated annual exceedance probability for each outcome value."
      tagList(
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;",
                      axis_txt),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
                      "Low odds show the value exceeded in only 1-in-N years."),
        shiny::tags$p(style = "font-size:11px; color:#666; margin-top:0;",
                      "High odds show the value reached in all but 1-in-N years.")
      )
    })

    # ---- observeEvent handlers ---------------------------------------------

    # Insert Results tab + content once (on first hist_sim).
    observeEvent(hist_sim(), {
      req(hist_sim())

      shiny::appendTab(
        inputId = tabset_id,
        shiny::tabPanel(
          title = "Results",
          value = "sim_tab",
          shiny::div(id = "results_section")
        ),
        select  = TRUE,
        session = tabset_session
      )

      shiny::insertUI(
        selector = "#results_section",
        where    = "afterBegin",
        ui       = .results_content_ui(ns, hist_sim()$so)
      )
    }, ignoreInit = TRUE, once = TRUE)

    # On subsequent runs, just re-select the tab.
    observeEvent(hist_sim(), {
      shiny::updateTabsetPanel(
        session  = tabset_session,
        inputId  = tabset_id,
        selected = "sim_tab"
      )
    }, ignoreInit = TRUE)

    # Keep agg method choices in sync with outcome.
    observeEvent(hist_sim(), {
      req(hist_sim()$so)
      so      <- hist_sim()$so
      choices <- hist_aggregate_choices(so$type, so$name)
      current <- isolate(input$cmp_agg_method)
      new_sel <- if (!is.null(current) && current %in% choices) current else "mean"
      shiny::updateSelectInput(session, "cmp_agg_method",
                               choices  = choices,
                               selected = new_sel)
    }, ignoreInit = TRUE)

    # ---- Return API --------------------------------------------------------
    list()
  })
}
