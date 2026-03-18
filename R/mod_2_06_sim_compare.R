#' 2_06_sim_compare UI Function
#'
#' @description No sidebar UI. Compare outputs are inserted into the
#'   existing Results tab via insertUI / removeUI.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_06_sim_compare_ui <- function(id) {
  tagList()
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
          preds      = fd$preds_list[[nm]],
          so         = fd$so,
          year_range = fd$year_range[[nm]]
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
        m <- regexpr('[0-9]{4}', nm)
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

    # Loss framing is auto-derived from outcome direction:
    # flip sign when higher simulated values = worse outcome (poverty, gini).

    agg_one <- function(preds, so, pov = pov_line_val()) {
      req(input$cmp_agg_method, input$cmp_deviation)
      aggregate_sim_preds(
        preds, so,
        input$cmp_agg_method,
        input$cmp_deviation,
        FALSE,
        pov
      )
    }

    # ---- Insert compare section into Results tab -------------------------

    compare_inserted <- reactiveVal(FALSE)

    observeEvent(hist_sim(), {
      req(hist_sim())
      if (compare_inserted()) return()

      so <- hist_sim()$so

      shiny::insertUI(
        selector = '#compare_section',
        where    = 'afterBegin',
        ui = tagList(

          # ---- 1. Results Header ------------------------------------------
          shiny::uiOutput(ns('results_header_ui')),

          # ---- 2. Controls: two-row compact layout -------------------------
          shiny::wellPanel(
            style = "padding: 10px 14px 6px 14px;",
            # --- Row 1: Analysis Settings ---
            shiny::tags$p("Analysis Settings",
                          style = "font-weight:600; margin-bottom:4px;"),
            shiny::tags$div(
              style = "display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap;",
              shiny::tags$div(style = "flex:1; min-width:160px;",
                shiny::selectInput(
                  ns('cmp_agg_method'),
                  label    = 'Aggregation method',
                  choices  = hist_aggregate_choices(so$type, so$name),
                  selected = 'mean'
                )
              ),
              shiny::tags$div(style = "flex:1; min-width:160px;",
                shiny::selectInput(
                  ns('cmp_deviation'),
                  label    = 'Express as deviation from',
                  choices  = c(
                    'None (raw value)' = 'none',
                    'Mean year'        = 'mean',
                    'Median year'      = 'median'
                  ),
                  selected = 'none'
                )
              ),
              shiny::tags$div(style = "flex:1; min-width:160px;",
                shiny::uiOutput(ns('cmp_pov_line_ui'))
              ),
            ),
            shiny::tags$hr(style = "margin: 6px 0;"),
            # --- Row 2: Scenario Filters ---
            shiny::tags$p("Scenario Filters",
                          style = "font-weight:600; margin-bottom:4px;"),
            shiny::fluidRow(
              shiny::column(12,
                shiny::uiOutput(ns('scenario_filter_ui'))
              )
            )
          ),

          # ---- 3. Hero boxplot --------------------------------------------
          shiny::wellPanel(
            shiny::h4("Distribution of outcomes across climate scenarios and timeframes"),
            shiny::plotOutput(ns('summary_box_plot'), height = '600px'),
            shiny::tags$p(
                        style = "font-size:11px; color:#666; margin-top:6px;",
                        "The central dot shows the mean annual simulated value; the thick coloured ",
                        "line spans the 90% interval (P5–P95); the thin grey line spans the 95% ",
                        "interval (P2.5–P97.5).",
                        shiny::tags$br(),
                        "Future scenarios apply climate-adjusted shifts to the same historical annual base.",
                        shiny::tags$br(),
                        "Dashed line = historical mean."
                      )
          ),

          # ---- 4. Summary distribution statistics table -------------------
          shiny::wellPanel(
            shiny::uiOutput(ns('dist_table_header')),
            DT::DTOutput(ns('summary_dist_table'))
          ),

          # ---- 5. Thresholds for rare adverse years table -----------------
          shiny::wellPanel(
            shiny::uiOutput(ns('threshold_table_header')),
            DT::DTOutput(ns('summary_threshold_table')),
            shiny::uiOutput(ns('threshold_table_footer'))
          ),

          # ---- 6. Exceedance curve ----------------------------------------
          shiny::wellPanel(
            shiny::h4('Exceedance probability by climate scenario'),
            shiny::tags$div(
              style = "display:flex; gap:20px; flex-wrap:wrap; margin-bottom:6px;",
              shiny::checkboxInput(
                ns('exceedance_logit_x'),
                'Logit probability axis (emphasise both tails)',
                value = FALSE
              ),
              shiny::checkboxInput(
                ns('show_return_period'),
                'Show return period lines',
                value = TRUE
              )
            ),
            shiny::plotOutput(ns('exceedance_plot'), height = '400px'),
            shiny::uiOutput(ns('exceedance_caption'))
          )
        )
      )

      compare_inserted(TRUE)
    }, ignoreInit = TRUE)

    # ---- Render: Results Header ------------------------------------------
    # Displays outcome, agg method, deviation, poverty line, baseline period,
    # and an auto-generated notes sentence.

    output$results_header_ui <- renderUI({
      req(hist_sim(), input$cmp_agg_method, input$cmp_deviation)
      so  <- hist_sim()$so
      yr  <- tryCatch(unlist(hist_sim()$year_range), error = function(e) NULL)

      agg_label <- switch(input$cmp_agg_method,
        mean            = 'Mean',
        median          = 'Median',
        headcount_ratio = 'Poverty headcount ratio',
        gap             = 'Poverty gap',
        fgt2            = 'FGT2 severity index',
        gini            = 'Gini coefficient',
        input$cmp_agg_method
      )
      dev_label <- switch(input$cmp_deviation,
        none   = 'raw value',
        mean   = 'deviation from mean year',
        median = 'deviation from median year',
        input$cmp_deviation
      )
      baseline_txt <- if (!is.null(yr) && length(yr) == 2)
        paste0(yr[1], '\u2013', yr[2]) else 'historical baseline'

      pov_txt <- if (!is.null(pov_line_val()))
        paste0(' | Poverty line: $', pov_line_val(), '/day') else ''

      notes_txt <- paste0(
        'Showing ', agg_label, ' of ', so$label %||% so$name,
        ' expressed as ', dev_label,
        ' over the ', baseline_txt, ' baseline period', pov_txt, '.'
      )

      shiny::div(
        style = paste0(
          "border-left: 4px solid #2166ac; ",
          "background: #f4f8fd; ",
          "padding: 10px 14px; ",
          "margin-bottom: 12px; ",
          "border-radius: 3px;"
        ),
        shiny::tags$strong(
          style = "font-size:15px;",
          paste0('Results: ', so$label %||% so$name)
        ),
        shiny::tags$br(),
        shiny::tags$span(
          style = "color:#555; font-size:12px;",
          notes_txt
        )
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

    # ---- Render: direction badge -----------------------------------------

    output$direction_badge_ui <- renderUI({
      req(hist_sim())
      so        <- hist_sim()$so
      direction <- so$direction %||% "higher_is_better"
      if (direction == "lower_is_better") {
        shiny::tags$div(
          style = "padding:6px 10px; background:#fdecea; border:1px solid #f5c6cb;
                   border-radius:4px; font-size:11px; color:#721c24;",
          shiny::tags$b("\u2193 Lower is better"),
          shiny::tags$br(),
          "Sign auto-flipped for display."
        )
      } else {
        shiny::tags$div(
          style = "padding:6px 10px; background:#d4edda; border:1px solid #c3e6cb;
                   border-radius:4px; font-size:11px; color:#155724;",
          shiny::tags$b("\u2191 Higher is better"),
          shiny::tags$br(),
          "No sign adjustment."
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
        yr_match  <- length(yrs) == 0 ||
          any(sapply(yrs, function(y) grepl(paste0('/ ', y, ' '), nm)))
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
      agg_one(hist_sim()$preds, hist_sim()$so, pov)
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
      lapply(sc, function(s) agg_one(s$preds, s$so, pov))
    })

    # ---- Reactive: table subtitle ----------------------------------------

    table_subtitle <- reactive({
      req(agg_hist(), input$cmp_agg_method, input$cmp_deviation)
      agg_label <- switch(input$cmp_agg_method,
        mean            = 'Mean',
        median          = 'Median',
        headcount_ratio = 'Poverty headcount ratio',
        gap             = 'Poverty gap',
        gini            = 'Gini coefficient',
        input$cmp_agg_method
      )
      dev_label <- switch(input$cmp_deviation,
        none   = 'raw value',
        mean   = 'deviation from mean year',
        median = 'deviation from median year',
        input$cmp_deviation
      )
      paste0(agg_hist()$x_label, ' \u2014 ', agg_label, ' | ', dev_label)
    })

    # ---- Render: table headers -------------------------------------------

    output$dist_table_header <- renderUI({
      req(agg_hist())
      tagList(
        shiny::h4('Summary distribution statistics'),
        shiny::tags$small(class = 'text-muted', table_subtitle())
      )
    })

    # ---- Render: Hero boxplot --------------------------------------------

    output$summary_box_plot <- renderPlot({
      req(agg_hist())
      plot_pointrange_climate(
        scenarios = agg_scenarios(),
        hist_agg  = agg_hist()
      )
    }, height = 600)

    # ---- Render: Distribution statistics table ---------------------------

    output$summary_dist_table <- DT::renderDT({
      req(agg_hist())
      all_series <- c(setNames(list(agg_hist()), hist_label()), agg_scenarios())

      rows <- lapply(names(all_series), function(nm) {
        vals <- all_series[[nm]]$out$value
        vals <- vals[is.finite(vals)]
        if (length(vals) == 0) return(NULL)
        data.frame(
          Scenario = nm, N = length(vals),
          Mean   = round(mean(vals),   3),
          Median = round(stats::median(vals), 3),
          P10    = round(as.numeric(stats::quantile(vals, 0.10)), 3),
          P90    = round(as.numeric(stats::quantile(vals, 0.90)), 3),
          Min    = round(min(vals),    3),
          Max    = round(max(vals),    3),
          check.names = FALSE
        )
      })

      df <- do.call(rbind, Filter(Negate(is.null), rows))

      DT::datatable(
        df,
        rownames = FALSE,
        class    = 'compact stripe',
        options  = list(
          pageLength = 15, dom = 't', ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-center', targets = '_all'),
            list(visible = FALSE,
                 targets = which(names(df) %in% c("Min", "Max")) - 1L)
          )
        )
      )
    })

    # ---- Render: Return period threshold table ---------------------------
    # Shows BOTH tails for every available return period.
    # Columns: 1:50, 1:20, 1:10, 1:5 (low tail) | 4:5, 9:10, 19:20, 49:50 (high tail)

    output$summary_threshold_table <- DT::renderDT({
      req(agg_hist())
      all_series <- c(setNames(list(agg_hist()), hist_label()), agg_scenarios())

      # Symmetric return-period map: name -> quantile probability
      rp_low  <- c("1:50" = 0.02, "1:20" = 0.05, "1:10" = 0.10, "1:5" = 0.20)
      rp_high <- c("4:5"  = 0.80, "9:10" = 0.90, "19:20" = 0.95, "49:50" = 0.98)

      rows <- lapply(names(all_series), function(nm) {
        vals <- all_series[[nm]]$out$value
        vals <- vals[is.finite(vals)]
        n    <- length(vals)

        keep_low  <- names(rp_low)[c(n >= 50, n >= 20, n >= 10, n >= 5)]
        keep_high <- names(rp_high)[c(n >= 5, n >= 10, n >= 20, n >= 50)]
        if (length(keep_low) == 0 && length(keep_high) == 0) return(NULL)

        low_df  <- as.data.frame(
          t(sapply(keep_low,  function(th) round(stats::quantile(vals, rp_low[th]),  3)))
        )
        high_df <- as.data.frame(
          t(sapply(keep_high, function(th) round(stats::quantile(vals, rp_high[th]), 3)))
        )
        names(low_df)  <- keep_low
        names(high_df) <- keep_high

        if (nrow(low_df)  == 0) low_df  <- data.frame()
        if (nrow(high_df) == 0) high_df <- data.frame()

        cbind(data.frame(Scenario = nm, Obs = n, check.names = FALSE),
              low_df, high_df)
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

      DT::datatable(
        do.call(rbind, rows),
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
        shiny::tags$p(
          style = 'font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;',
          'Low odds show the value exceeded in only 1-in-N years.'
        ),
        shiny::tags$p(
          style = 'font-size:11px; color:#666; margin-top:0;',
          'High odds show the value reached in all but 1-in-N years.'
        )
      )
    })

    # ---- Render: Exceedance curve ----------------------------------------

    output$exceedance_plot <- renderPlot({
      req(agg_hist())
      all_series <- c(setNames(list(agg_hist()), hist_label()), agg_scenarios())
      enhance_exceedance(
        scenarios     = all_series,
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
        shiny::tags$p(
          style = "font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;",
          axis_txt
        ),
        shiny::tags$p(
          style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
          'Low odds show the value exceeded in only 1-in-N years.'
        ),
        shiny::tags$p(
          style = "font-size:11px; color:#666; margin-top:0;",
          'High odds show the value reached in all but 1-in-N years.'
        )
      )
    })

  })
}
