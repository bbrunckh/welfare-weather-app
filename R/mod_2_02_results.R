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
            label    = "Deviation from historical baseline",
            choices  = c(
              "None (raw value)" = "none",
              "Historical mean"   = "mean",
              "Historical median" = "median"
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

        "The central dot shows the mean annual simulated value; the thick", # TODO: update caption if percentile thresholds change from P5/P95
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
#' @param hist_sim        ReactiveVal. Named list with slots:
#'   \code{$preds} (full prediction data frame), \code{$agg} (pre-aggregated
#'   summary by method x weighted x deviation x sim_year), \code{$so}
#'   (selected outcome metadata), \code{$pov_line} (simulation-time poverty
#'   line), \code{$has_weights} (logical weight flag), \code{$weather_raw},
#'   \code{$train_data}, \code{$n_pre_join}.
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
                                     tabset_session = NULL,
                                     pov_line_sim   = reactive(3.00),
                                     hist_agg_rv     = reactive(NULL),
                                     scenario_agg_rv = reactive(NULL)) {
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



    output$weight_status_ui <- shiny::renderUI({
      req(hist_sim())
      # Detect weight column independently of the toggle -- this allows
      # the amber state when the column exists but the toggle is OFF.
      has_w  <- !is.null(hist_sim()$pipeline$weight)

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
    outputOptions(output, "weight_status_ui", suspendWhenHidden = TRUE)

    pov_line_raw <- reactive({
      if (isTRUE(input$cmp_agg_method %in%
                 c("headcount_ratio", "gap", "fgt2",
                   "prosperity_gap", "avg_poverty"))) {
        as.numeric(pov_line_sim() %||% 3.00)
      } else {
        NULL
      }
    })
    # Debounce — wait 800ms after last change before recomputing
    # Prevents 300 recomputes per keypress when typing a poverty line value
    pov_line_val <- debounce(pov_line_raw, 800)

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
      req(hist_agg_rv())
      method    <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation  %||% "none"
      weight_key <- if (isTRUE(input$use_weights) &&
                  !is.null(hist_sim()) &&
                  isTRUE(hist_sim()$has_weights)) "weighted" else "unweighted"
      out <- hist_agg_rv()[[weight_key]][[method]]
      req(!is.null(out))
      if (!identical(deviation, "none") && nrow(out) > 0) {
        hist_ref <- if (identical(deviation, "mean"))
                      mean(out$value, na.rm = TRUE)
                    else
                      stats::median(out$value, na.rm = TRUE)
        out <- dplyr::mutate(out, value = value - hist_ref)
      }
      x_label <- if (identical(deviation, "none")) label_agg_method(method) else
        paste0(label_agg_method(method), " \u2014 ", label_deviation(deviation))
      list(out = out, x_label = x_label)
    })

    agg_scenarios <- reactive({
      req(scenario_agg_rv())
      sc <- saved_scenarios()
      if (length(sc) == 0) return(list())
      method    <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation  %||% "none"
      hist_ref  <- if (!identical(deviation, "none") && !is.null(agg_hist())) {
        if (identical(deviation, "mean")) mean(agg_hist()$out$value, na.rm = TRUE)
        else stats::median(agg_hist()$out$value, na.rm = TRUE)
      } else NA_real_
      x_label <- if (identical(deviation, "none")) label_agg_method(method) else
        paste0(label_agg_method(method), " \u2014 ", label_deviation(deviation))
        result <- setNames(lapply(names(sc), function(display_key) {
        weight_key <- if (isTRUE(input$use_weights) &&
                  !is.null(hist_sim()) &&
                  isTRUE(hist_sim()$has_weights)) "weighted" else "unweighted"
        out <- scenario_agg_rv()[[display_key]][[weight_key]][[method]]
        if (is.null(out) || nrow(out) == 0L) return(NULL)
        if (!identical(deviation, "none") && !is.na(hist_ref))
          out <- dplyr::mutate(out, value = value - hist_ref)
        list(out = out, x_label = x_label)
      }), names(sc))
      Filter(function(x) !is.null(x) && !is.null(x$out) && nrow(x$out) > 0, result)
    })

    all_series <- reactive({
      sc  <- agg_scenarios()
      sel <- selected_scenario_names()
      hist_entry <- list(agg_hist())
      if (is.null(agg_hist()) || is.null(agg_hist()$out) || nrow(agg_hist()$out) == 0)
        return(list())
      names(hist_entry) <- hist_label()
      sc_filtered <- sc[intersect(sel, names(sc))]
      sc_filtered <- Filter(
        function(x) !is.null(x) && !is.null(x$out) && nrow(x$out) > 0,
        sc_filtered
      )
      c(hist_entry, sc_filtered)
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
        pov_val <- sprintf("$%.2f", pov_line_sim() %||% 3.00)
        shiny::tags$p(
          style = "font-size:11px; color:#555; margin-top:4px;",
          paste0("Poverty line: ", pov_val, "/day (fixed at simulation time)")
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

    # ---- Suspend outputs when Results tab is hidden ----------------------
    outputOptions(output, "summary_box_plot",        suspendWhenHidden = TRUE)
    outputOptions(output, "summary_threshold_table", suspendWhenHidden = TRUE)
    outputOptions(output, "exceedance_plot",         suspendWhenHidden = TRUE)
    outputOptions(output, "results_header_ui",       suspendWhenHidden = TRUE)
    outputOptions(output, "cmp_pov_line_ui",         suspendWhenHidden = TRUE)
    outputOptions(output, "scenario_filter_ui",      suspendWhenHidden = TRUE)
    outputOptions(output, "weight_status_ui",        suspendWhenHidden = TRUE)
    outputOptions(output, "threshold_table_header",  suspendWhenHidden = TRUE)
    outputOptions(output, "threshold_table_footer",  suspendWhenHidden = TRUE)
    outputOptions(output, "exceedance_caption",      suspendWhenHidden = TRUE)
    
    # ---- Return API --------------------------------------------------------
    list()
  })
}
