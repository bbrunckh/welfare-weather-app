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

    # --- Internal State ---
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

    # ---- Helpers: scenario name parsing ----
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

    # --- Helpers: controls ---
    pov_line_val <- reactive({
      if (isTRUE(input$cmp_agg_method %in% c('headcount_ratio', 'gap', 'fgt2'))) {
        input$cmp_pov_line %||% NULL   # no req() — return NULL gracefully if UI not yet rendered
      } else {
        NULL
      }
    })

    # ---- aggregate one series --------------------------------------------

    agg_one <- function(preds, so, pov = pov_line_val()) {
      req(input$cmp_agg_method, input$cmp_deviation)
      aggregate_sim_preds(
        preds, so,
        input$cmp_agg_method,
        input$cmp_deviation,
        isTRUE(input$cmp_loss_frame),
        pov
      )
    }

    # ---- Insert compare section into Results tab -------------------------
    # Fires once when hist_sim() first becomes available.
    # The sentinel div#compare_section is placed at the bottom of the
    # Results tab by mod_2_02_historical_sim_server().

    compare_inserted <- reactiveVal(FALSE)

    observeEvent(hist_sim(), {
      req(hist_sim())
      if (compare_inserted()) return()

      so <- hist_sim()$so

      shiny::insertUI(
        selector = '#compare_section',
        where    = 'afterBegin',
        ui = tagList(

          # --- shared controls ---
          shiny::wellPanel(
            shiny::h4('Scenario comparison controls'),
            shiny::fluidRow(
              shiny::column(3,
                shiny::selectInput(
                  ns('cmp_agg_method'),
                  label    = 'Aggregation method',
                  choices  = hist_aggregate_choices(so$type),
                  selected = if (identical(so$type, 'logical')) 'mean'
                             else 'headcount_ratio'
                )
              ),
              shiny::column(3,
                shiny::selectInput(
                  ns('cmp_deviation'),
                  label    = 'Express as deviation from',
                  choices  = c(
                    'None (raw value)' = 'none',
                    'Mean year'        = 'mean',
                    'Median year'      = 'median'
                  ),
                  selected = 'median'
                )
              ),
              shiny::column(3,
                shiny::uiOutput(ns('cmp_pov_line_ui'))
              ),
              shiny::column(3,
                shiny::checkboxInput(
                  ns('cmp_loss_frame'),
                  label = 'Loss framing (flip sign)',
                  value = FALSE
                )
              )
            )
          ),

          # --- climate scenario / timeframe filters (appear once futures run) ---
          shiny::wellPanel(
            shiny::uiOutput(ns('scenario_filter_ui'))
          ),

          shiny::hr(),

          # --- bar chart ---
          shiny::wellPanel(
            shiny::h4('Distribution by climate scenario and timeframe'),
            shiny::plotOutput(ns('summary_bar_plot'), height = '300px')
          ),

          # --- distribution statistics table ---
          shiny::wellPanel(
            shiny::uiOutput(ns('dist_table_header')),
            DT::DTOutput(ns('summary_dist_table'))
          ),

          # --- return period threshold table ---
          shiny::wellPanel(
            shiny::uiOutput(ns('threshold_table_header')),
            DT::DTOutput(ns('summary_threshold_table'))
          ),

          # --- exceedance curve ---
          shiny::wellPanel(
            shiny::h4('Exceedance probability by climate scenario'),
            shiny::fluidRow(
              shiny::column(6,
                shiny::checkboxInput(ns('show_return_period'),
                  'Show return period axis', value = TRUE)
              )
            ),
            shiny::plotOutput(ns('exceedance_plot'), height = '400px')
          )
        )
      )

      compare_inserted(TRUE)
    }, ignoreInit = TRUE)

    # ---- Render: poverty line UI -------------------------------------------------

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

    # ---- Render: scenario filter UI (climate scenario + timeframe) ---------------

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
                ns('filter_ssp'), label = 'Filter by climate scenario',
                choices = ssps, selected = ssps, inline = TRUE
              )
          ),
          shiny::column(6,
            if (length(yrs) > 0)
              shiny::checkboxGroupInput(
                ns('filter_year'), label = 'Filter by timeframe',
                choices = yrs, selected = yrs, inline = TRUE
              )
          )
        )
      )
    })

    # ---- Reactive: selected scenario names from filters ----------------------------

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

    # ---- Reactive: aggregated series --------------------------------------------------------

    agg_hist <- reactive({
      method <- input$cmp_agg_method
      pov    <- pov_line_val()
      if (isTRUE(method %in% c("headcount_ratio", "gap", "fgt2"))) req(pov)
      agg_one(hist_sim()$preds, hist_sim()$so, pov)
    })

    # ---- agg_scenarios (filtered) ----------------------------------------

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

    # ---- dynamic table subtitles -----------------------------------------
    # Format: "<stat type>  <outcome label> | <agg method> | <deviation>"

    table_subtitle <- reactive({
      req(agg_hist(), input$cmp_agg_method, input$cmp_deviation)
      agg_label <- switch(input$cmp_agg_method,
        mean            = 'Mean',
        median          = 'Median',
        headcount_ratio = 'Poverty headcount ratio',
        gap             = 'Poverty gap',
        input$cmp_agg_method
      )
      dev_label <- switch(input$cmp_deviation,
        none   = 'raw value',
        mean   = 'deviation from mean year',
        median = 'deviation from median year',
        input$cmp_deviation
      )
      outcome_label <- agg_hist()$x_label
      paste0(outcome_label, ' \u2014 ', agg_label, ' | ', dev_label)
    })

    output$dist_table_header <- renderUI({
      req(agg_hist())
      tagList(
        shiny::h4('Distribution statistics'),
        shiny::tags$small(
          class = 'text-muted',
          table_subtitle()
        )
      )
    })

    output$threshold_table_header <- renderUI({
      req(agg_hist())
      tagList(
        shiny::h4('Return period thresholds'),
        shiny::tags$small(
          class = 'text-muted',
          table_subtitle()
        )
      )
    })

    # ---- Bar chart -------------------------------------------------------

    output$summary_bar_plot <- renderPlot({
      req(agg_hist())
      all_series <- c(setNames(list(agg_hist()), hist_label()), agg_scenarios())
      plot_bar_climate(
        scenarios    = all_series,
        hist_agg     = agg_hist(),
        centre_years = NULL,
        show_error   = TRUE
      )
    })

    # ---- Distribution statistics table -----------------------------------

    output$summary_dist_table <- DT::renderDT({
      req(agg_hist())
      all_series <- c(setNames(list(agg_hist()), hist_label()), agg_scenarios())

      rows <- lapply(names(all_series), function(nm) {
        vals <- all_series[[nm]]$out$value
        vals <- vals[is.finite(vals)]
        if (length(vals) == 0) return(NULL)
        data.frame(
          Scenario = nm, N = length(vals),
          Min    = round(min(vals),    3),
          P10    = round(as.numeric(stats::quantile(vals, 0.10)), 3),
          Median = round(stats::median(vals), 3),
          Mean   = round(mean(vals),   3),
          P90    = round(as.numeric(stats::quantile(vals, 0.90)), 3),
          Max    = round(max(vals),    3),
          check.names = FALSE
        )
      })

      DT::datatable(
        do.call(rbind, Filter(Negate(is.null), rows)),
        rownames = FALSE,
        class    = 'compact stripe',
        options  = list(
          pageLength = 15, dom = 't', ordering = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = '_all'))
        )
      )
    })

    # ---- Return period threshold table -----------------------------------

    output$summary_threshold_table <- DT::renderDT({
      req(agg_hist())
      all_series <- c(setNames(list(agg_hist()), hist_label()), agg_scenarios())
      rp_map <- c('1:5' = 0.20, '1:10' = 0.10, '1:20' = 0.05, '1:50' = 0.02)

      rows <- lapply(names(all_series), function(nm) {
        vals <- all_series[[nm]]$out$value
        vals <- vals[is.finite(vals)]
        n    <- length(vals)
        keep <- c(
          if (n >= 5)  '1:5'  else NULL,
          if (n >= 10) '1:10' else NULL,
          if (n >= 20) '1:20' else NULL,
          if (n >= 50) '1:50' else NULL
        )
        if (length(keep) == 0) return(NULL)
        row_vals <- sapply(keep, function(th)
          round(as.numeric(stats::quantile(vals, probs = 1 - rp_map[th])), 3))
        df <- as.data.frame(t(row_vals))
        names(df) <- keep
        cbind(data.frame(Scenario = nm, N = n, check.names = FALSE), df)
      })

      rows <- Filter(Negate(is.null), rows)
      if (length(rows) == 0)
        return(DT::datatable(data.frame(Message = 'Insufficient data'),
                             rownames = FALSE, class = 'compact stripe',
                             options = list(dom = 't')))

      all_cols <- unique(unlist(lapply(rows, names)))
      rows <- lapply(rows, function(r) {
        for (m in setdiff(all_cols, names(r))) r[[m]] <- NA_real_
        r[all_cols]
      })

      DT::datatable(
        do.call(rbind, rows),
        rownames = FALSE,
        class    = 'compact stripe',
        options  = list(
          pageLength = 15, dom = 't', ordering = FALSE,
          columnDefs = list(list(className = 'dt-center', targets = '_all'))
        )
      )
    })

    # ---- Exceedance curve ------------------------------------------------

    output$exceedance_plot <- renderPlot({
      req(agg_hist())
      all_series <- c(setNames(list(agg_hist()), hist_label()), agg_scenarios())
      enhance_exceedance(
        scenarios     = all_series,
        hist_agg      = agg_hist(),
        x_label       = agg_hist()$x_label,
        return_period = isTRUE(input$show_return_period),
        n_sim_years   = nrow(agg_hist()$out)
      )
    })

  })
}
