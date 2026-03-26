#' 2_06_sim_compare UI Function
#'
#' @description A shiny Module. Populates the comparison section within the
#'   existing Results tab via insertUI / removeUI.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_06_sim_compare_ui <- function(id) {
  tagList()
}

# ---------------------------------------------------------------------------- #
# compare_content_ui: inserted once into #compare_section                     #
# Extracted from the insertUI observeEvent to keep the observer readable.     #
# ---------------------------------------------------------------------------- #

compare_content_ui <- function(ns, so) {
  tagList(

    # ---- 1. Results header (outcome name, agg method, baseline period) -----
    shiny::uiOutput(ns("results_header_ui")),

    # ---- 2. Analysis controls (agg method, deviation, poverty line, filters) -
    shiny::wellPanel(
      style = "padding: 10px 14px 6px 14px;",
      shiny::tags$p("Analysis Settings",
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
            choices = c(
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

    # ---- 4. Return-period threshold table (both tails) ---------------------
    shiny::wellPanel(
      shiny::uiOutput(ns("threshold_table_header")),
      DT::DTOutput(ns("summary_threshold_table")),
      shiny::uiOutput(ns("threshold_table_footer"))
    ),

    # ---- 5. Exceedance probability curve -----------------------------------
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
    )
  )
}

#' 2_06_sim_compare Server Functions
#'
#' Populates the comparison section within the Results tab once the
#' historical simulation has run. Uses insertUI/removeUI so no extra
#' tab is created. Updates automatically when future simulations are added.
#'
#' @param id              Module id.
#' @param hist_sim        Reactive list of historical simulation results.
#' @param fut_sim         Reactive list of future simulation results.
#' @param saved_scenarios ReactiveVal holding named scenario entries.
#' @param selected_hist   Reactive one-row data frame from mod_2_01_historical.
#'
#' @noRd
mod_2_06_sim_compare_server <- function(id,
                                        hist_sim        = NULL,
                                        fut_sim         = NULL,
                                        saved_scenarios = NULL,
                                        selected_hist   = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Populate saved_scenarios from fut_sim ----------------------------

    observeEvent(fut_sim(), {
      req(fut_sim())
      fd <- fut_sim()
      req(length(fd$preds_list) > 0)

      current <- saved_scenarios()
      warned  <- FALSE

      for (nm in names(fd$preds_list)) {
        if (length(current) >= 10) {
          if (!warned) {
            shiny::showNotification(
              paste0('\u26a0 Maximum of 10 scenarios reached. ',
                     'Use Clear Scenarios to reset.'),
              type = 'warning', duration = 6
            )
            warned <- TRUE
          }
          break
        }
        current[[nm]] <- list(
          preds       = fd$preds_list[[nm]]$preds,
          weather_raw = fd$preds_list[[nm]]$weather_raw,
          so          = fd$so,
          year_range  = fd$year_range[[nm]]
        )
      }
      saved_scenarios(current)
    }, ignoreInit = TRUE)

    # ---- Historical label ------------------------------------------------

    hist_label <- reactive({
      if (!is.null(selected_hist)) {
        tryCatch({
          nm <- selected_hist()$scenario_name
          if (!is.null(nm) && nzchar(as.character(nm))) return(as.character(nm))
        }, error = function(e) NULL)
      }
      'Historical'
    })

    # ---- SSP / timeframe filter reactives --------------------------------

    all_ssps <- reactive({
      sc <- saved_scenarios()
      if (length(sc) == 0) return(character(0))
      ssps <- unique(sub(' / .*', '', names(sc)))
      sort(ssps[grepl('^SSP', ssps)])
    })

    all_anchor_years <- reactive({
      sc <- saved_scenarios()
      if (length(sc) == 0) return(character(0))
      yrs <- vapply(names(sc), function(nm) {
        m <- regexpr("(?<=/ )[0-9]{4}", nm, perl = TRUE)
        if (m == -1L) NA_character_ else regmatches(nm, m)
      }, character(1))
      sort(unique(yrs[!is.na(yrs)]))
    })

    # ---- pov_line helper -------------------------------------------------

    pov_line_val <- reactive({
      if (isTRUE(input$cmp_agg_method %in% c('headcount_ratio', 'gap', 'fgt2'))) {
        input$cmp_pov_line %||% NULL
      } else {
        NULL
      }
    })

    # ---- Insert compare section into Results tab -------------------------

    compare_inserted <- reactiveVal(FALSE)

    observeEvent(hist_sim(), {
      req(hist_sim())
      if (compare_inserted()) return()

      so <- hist_sim()$so

      shiny::insertUI(
        selector = "#compare_section",
        where    = "afterBegin",
        ui       = compare_content_ui(ns, so)
      )

      compare_inserted(TRUE)
    }, ignoreInit = TRUE)

    # ---- Reactive: all series (historical + filtered scenarios) ----------
    # Single definition used by all render outputs below.

    all_series <- reactive({
      c(setNames(list(agg_hist()), hist_label()), agg_scenarios())
    })

    # ---- Render: Results Header ------------------------------------------
    # Uses label_agg_method() and label_deviation() from fct_sim_compare.R.

    output$results_header_ui <- renderUI({
      req(hist_sim(), input$cmp_agg_method, input$cmp_deviation)
      so  <- hist_sim()$so
      yr  <- tryCatch(unlist(hist_sim()$year_range), error = function(e) NULL)

      agg_label    <- label_agg_method(input$cmp_agg_method)
      dev_label    <- label_deviation(input$cmp_deviation)
      baseline_txt <- if (!is.null(yr) && length(yr) == 2)
        paste0(yr[1], "\u2013", yr[2]) else "historical baseline"
      pov_txt <- if (!is.null(pov_line_val()))
        paste0(" | Poverty line: $", pov_line_val(), "/day") else ""

      notes_txt <- paste0(
        "Showing ", agg_label, " of ", so$label %||% so$name,
        " expressed as ", dev_label,
        " over the ", baseline_txt, " baseline period", pov_txt, "."
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

    # ---- Render: poverty line UI -----------------------------------------

    output$cmp_pov_line_ui <- renderUI({
      req(input$cmp_agg_method)
      if (input$cmp_agg_method %in% c('headcount_ratio', 'gap', 'fgt2')) {
        shiny::numericInput(
          ns('cmp_pov_line'),
          label = 'Poverty line (daily, 2021 PPP USD)',
          value = 3.00, min = 0, step = 0.5
        )
      }
    })

    # ---- Render: scenario filter UI --------------------------------------

    output$scenario_filter_ui <- renderUI({
      sc <- saved_scenarios()
      if (length(sc) == 0)
        return(shiny::helpText('Run a future simulation to add climate scenarios.'))
      ssps <- all_ssps()
      yrs  <- all_anchor_years()
      tagList(
        shiny::fluidRow(
          shiny::column(6,
            if (length(ssps) > 0)
              shiny::checkboxGroupInput(
                ns('filter_ssp'), label = 'Climate scenario',
                choices = ssps, selected = ssps, inline = TRUE
              )
          ),
          shiny::column(6,
            if (length(yrs) > 0)
              shiny::checkboxGroupInput(
                ns('filter_year'), label = 'Timeframe',
                choices = yrs, selected = yrs, inline = TRUE
              )
          )
        )
      )
    })

    # ---- Reactive: selected scenario names from filters ------------------

    selected_scenario_names <- reactive({
      sc  <- saved_scenarios()
      if (length(sc) == 0) return(character(0))
      nms  <- names(sc)
      ssps <- if (length(input$filter_ssp)  == 0) all_ssps()         else input$filter_ssp
      yrs  <- if (length(input$filter_year) == 0) all_anchor_years() else input$filter_year
      keep <- vapply(nms, function(nm) {
        is_ssp <- grepl('^SSP', nm)
        if (!is_ssp) return(TRUE)
        ssp_match <- any(sapply(ssps, function(s) startsWith(nm, s)))
        # Match anchor year after the '/' separator: "SSP2 / 2030 ±10yr"
        yr_match  <- length(yrs) == 0 ||
          any(sapply(yrs, function(y) grepl(paste0("/ ", y, " "), nm, fixed = TRUE)))
        ssp_match && yr_match
      }, logical(1))
      nms[keep]
    })

    # ---- Keep aggregation method choices in sync with the loaded outcome ----
    # insertUI bakes in choices at first render only. This observer refreshes
    # them whenever a new historical sim is run (i.e. when the outcome changes).

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

    # ---- Reactive: aggregated historical series --------------------------

    agg_hist <- reactive({
      req(hist_sim())
      method <- input$cmp_agg_method
      pov    <- pov_line_val()
      if (isTRUE(method %in% c("headcount_ratio", "gap", "fgt2"))) req(pov)
      aggregate_sim_preds(hist_sim()$preds, hist_sim()$so, method, input$cmp_deviation, FALSE, pov)
    })

    # ---- Reactive: aggregated future scenarios (filtered) ----------------

    agg_scenarios <- reactive({
      req(saved_scenarios(), input$cmp_agg_method, input$cmp_deviation)
      pov      <- pov_line_val()
      if (isTRUE(input$cmp_agg_method %in% c("headcount_ratio", "gap", "fgt2"))) req(pov)
      sc       <- saved_scenarios()
      selected <- selected_scenario_names()
      sc       <- sc[intersect(selected, names(sc))]
      if (length(sc) == 0) return(list())
      lapply(sc, function(s) aggregate_sim_preds(s$preds, s$so, input$cmp_agg_method, input$cmp_deviation, FALSE, pov))
    })

    # ---- Reactive: table subtitle ----------------------------------------
    # Uses label_agg_method() and label_deviation() from fct_sim_compare.R.

    table_subtitle <- reactive({
      req(agg_hist(), input$cmp_agg_method, input$cmp_deviation)
      paste0(
        agg_hist()$x_label, " \u2014 ",
        label_agg_method(input$cmp_agg_method), " | ",
        label_deviation(input$cmp_deviation)
      )
    })

    # ---- Render: Hero boxplot --------------------------------------------

    output$summary_box_plot <- renderPlot({
      req(agg_hist())
      plot_pointrange_climate(
        scenarios   = agg_scenarios(),
        hist_agg    = agg_hist(),
        group_order = input$cmp_group_order %||% "scenario_x_year"
      )
    }, height = 600)

    # ---- Render: Return period threshold table ---------------------------
    # Columns: 1:50 ... 1:5 (low tail) | 1:1 (median) | 4:5 ... 49:50 (high tail)
    # Row order follows input$cmp_group_order: scenario_x_year sorts by SSP
    # then year; year_x_scenario sorts by year then SSP.

    output$summary_threshold_table <- DT::renderDT({
      req(agg_hist())
      group_order <- input$cmp_group_order %||% "scenario_x_year"

      # RP_LOW / RP_HIGH constants from fct_simulations.R (both tails)
      rows <- lapply(names(all_series()), function(nm) {
        vals <- all_series()[[nm]]$out$value
        vals <- vals[is.finite(vals)]
        n    <- length(vals)

        keep_low  <- names(RP_LOW)[c(n >= 50, n >= 20, n >= 10, n >= 5)]
        keep_high <- names(RP_HIGH)[c(n >= 5, n >= 10, n >= 20, n >= 50)]
        if (length(keep_low) == 0 && length(keep_high) == 0) return(NULL)

        low_df <- as.data.frame(
          t(sapply(keep_low,  function(th) round(stats::quantile(vals, RP_LOW[th]),  3)))
        )
        high_df <- as.data.frame(
          t(sapply(keep_high, function(th) round(stats::quantile(vals, RP_HIGH[th]), 3)))
        )
        names(low_df)  <- keep_low
        names(high_df) <- keep_high

        if (nrow(low_df)  == 0) low_df  <- data.frame()
        if (nrow(high_df) == 0) high_df <- data.frame()

        median_df <- data.frame("1:1" = round(stats::median(vals), 3),
                                check.names = FALSE)

        cbind(data.frame(Scenario = nm, Obs = n, check.names = FALSE),
              low_df, median_df, high_df)
      })

      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0)
        return(DT::datatable(data.frame(Message = 'Insufficient data'),
                             rownames = FALSE, class = 'compact stripe',
                             options  = list(dom = 't')))

      all_cols <- unique(unlist(lapply(rows, names)))
      rows <- lapply(rows, function(r) {
        for (m in setdiff(all_cols, names(r))) r[[m]] <- NA_real_
        r[all_cols]
      })

      df <- do.call(rbind, rows)

      # ---- Sort rows by group_order ---
      # Extract SSP and year tokens from Scenario name for ordering.
      hist_rows <- df[df$Scenario == "Historical" | !grepl("^SSP", df$Scenario), , drop = FALSE]
      ssp_rows  <- df[grepl("^SSP", df$Scenario), , drop = FALSE]

      if (nrow(ssp_rows) > 0) {
        ssp_rows$ssp_sort <- sub(" /.*", "", ssp_rows$Scenario)
        ssp_rows$yr_sort  <- as.integer(
          regmatches(ssp_rows$Scenario,
                     regexpr("[0-9]{4}", ssp_rows$Scenario))
        )
        if (isTRUE(group_order == "year_x_scenario")) {
          ssp_rows <- ssp_rows[order(ssp_rows$yr_sort, ssp_rows$ssp_sort), ]
        } else {
          ssp_rows <- ssp_rows[order(ssp_rows$ssp_sort, ssp_rows$yr_sort), ]
        }
        ssp_rows$ssp_sort <- NULL
        ssp_rows$yr_sort  <- NULL
      }

      df <- rbind(hist_rows, ssp_rows)

      DT::datatable(
        df,
        rownames  = FALSE,
        class     = 'compact stripe',
        options   = list(
          pageLength = 15, dom = 't', ordering = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = '_all'))
        )
      )
    })

    output$threshold_table_header <- renderUI({
      req(agg_hist())
      tagList(
        shiny::h4('Outcome value at return-period thresholds (both tails)'),
        shiny::tags$small(class = 'text-muted', table_subtitle())
      )
    })

    output$threshold_table_footer <- renderUI({
      req(agg_hist())
      tagList(
        shiny::tags$p(style = 'font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;',
                      'Low odds show the value exceeded in only 1-in-N years.'),
        shiny::tags$p(style = 'font-size:11px; color:#666; margin-top:0; margin-bottom:2px;',
                      'High odds show the value reached in all but 1-in-N years.'),
        shiny::tags$p(style = 'font-size:11px; color:#666; margin-top:0;',
                      '1:1 shows the median (50th percentile) simulated value.')
      )
    })

    # ---- Render: Exceedance curve ----------------------------------------

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
        'Probability axis is logit-scaled, giving equal visual weight to both tails.'
      else
        'The curve shows the estimated annual exceedance probability for each outcome value.'
      tagList(
        shiny::tags$p(style = 'font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;', axis_txt),
        shiny::tags$p(style = 'font-size:11px; color:#666; margin-top:0; margin-bottom:2px;',
                      'Low odds show the value exceeded in only 1-in-N years.'),
        shiny::tags$p(style = 'font-size:11px; color:#666; margin-top:0;',
                      'High odds show the value reached in all but 1-in-N years.')
      )
    })

  })
}
