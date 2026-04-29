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
        shiny::selectInput(
          ns("uncertainty_band"),
          label   = "Uncertainty band",
          choices = c(
            "50% (p25-p75)"    = "p25_p75",
            "60% (p20-p80)"    = "p20_p80",
            "80% (p10-p90)"    = "p10_p90",
            "90% (p05-p95)"    = "p05_p95",
            "95% (p025-p975)"  = "p025_p975",
            "99% (p005-p995)"  = "p005_p995",
            "Max (min-max)"    = "minmax"
          ),
          selected = "p10_p90"
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
      shiny::checkboxInput(
        ns("show_coef_uncertainty"),
        label = "Include coefficient uncertainty",
        value = TRUE
      ),
      shiny::uiOutput(ns("coef_uncertainty_status_ui")), 
      


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
        "Central dot = mean annual welfare across 30 simulation years.",
        shiny::tags$br(),
        "Thick bar = p5\u2013p95 of annual welfare values (weather variation).",
        shiny::tags$br(),
        "Thin line = p10\u2013p90 coefficient uncertainty (updates with uncertainty band selector).",
        shiny::tags$br(),
        "Future scenarios apply climate perturbations to the historical weather base."
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
        ),
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

  output$coef_uncertainty_status_ui <- shiny::renderUI({
      req(hist_sim())
      if (!has_draws()) {
        shiny::tags$p(
          style = "font-size:11px; color:#c62828; margin:2px 0 6px 0;",
          "\U0001f534 Coefficient draws skipped at simulation time"
        )
      } else if (!isTRUE(input$show_coef_uncertainty)) {
        shiny::tags$p(
          style = "font-size:11px; color:#e65100; margin:2px 0 6px 0;",
          "\u26a0 Coefficient uncertainty available but not shown"
        )
      } else {
        shiny::tags$p(
          style = "font-size:11px; color:#2e7d32; margin:2px 0 6px 0;",
          "\u2705 Coefficient uncertainty shown"
        )
      }
    })
    outputOptions(output, "coef_uncertainty_status_ui",
                  suspendWhenHidden = TRUE)

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
    weight_key <- reactive({
      if (isTRUE(input$use_weights) &&
          !is.null(hist_sim()) &&
          isTRUE(hist_sim()$has_weights)) "weighted" else "unweighted"
    })

    # Shared deviation reference — used by all_series_tbl and exceedance_ribbon
        hist_ref_val <- reactive({
          req(hist_agg_rv())
          method    <- input$cmp_agg_method %||% "mean"
          wk        <- weight_key()
          deviation <- input$cmp_deviation %||% "none"
          if (identical(deviation, "none")) return(0)
          raw_vals <- hist_agg_rv()[[wk]][[method]]$value
          if (identical(deviation, "mean"))
            mean(raw_vals, na.rm = TRUE)
          else
            stats::median(raw_vals, na.rm = TRUE)
        })


        # ---- Coefficient draws availability -----------------------------------
    has_draws <- reactive({
      req(hist_sim())
      !is.null(hist_sim()$chol_obj)
    })

    # Auto-uncheck coefficient uncertainty when no draws available
    observeEvent(hist_sim(), {
      req(hist_sim())
      if (!has_draws()) {
        shiny::updateCheckboxInput(
          session,
          "show_coef_uncertainty",
          value = FALSE
        )
      }
    }, ignoreInit = TRUE)





    selected_scenario_names <- reactive({
      sc   <- saved_scenarios()
      if (length(sc) == 0L) return(character(0))
      keys <- names(sc)

      # Read each grid checkbox
      selected <- Filter(Negate(is.null), lapply(keys, function(key) {
        cb_id <- paste0("sc_", gsub("[^a-zA-Z0-9]", "_", key))
        val   <- input[[cb_id]]
        if (isTRUE(val)) key else NULL
      }))

      # Enforce minimum 1 selected
      if (length(selected) == 0L) keys[1L] else unlist(selected)
    })

    agg_hist <- reactive({
      req(hist_agg_rv())
      method    <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation  %||% "none"
      out <- hist_agg_rv()[[weight_key()]][[method]]
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
      hist_ref <- if (!identical(deviation, "none")) hist_ref_val() else NA_real_
      x_label <- if (identical(deviation, "none")) label_agg_method(method) else
        paste0(label_agg_method(method), " \u2014 ", label_deviation(deviation))
        selected <- selected_scenario_names()
        result <- setNames(lapply(names(sc), function(display_key) {
          if (!display_key %in% selected) return(NULL)
          out <- scenario_agg_rv()[[display_key]][[weight_key()]][[method]]
          if (is.null(out) || nrow(out) == 0L) return(NULL)
          if (!identical(deviation, "none") && !is.na(hist_ref))
            out <- dplyr::mutate(out, value = value - hist_ref)
          list(out = out, x_label = x_label)
        }), names(sc))
      Filter(function(x) !is.null(x) && !is.null(x$out) && nrow(x$out) > 0, result)
    })

    exceedance_ribbon <- reactive({
      req(hist_agg_rv())
      method <- input$cmp_agg_method %||% "mean"
      wk     <- weight_key()
      bq <- resolve_band_q(input$uncertainty_band %||% "p10_p90")

      # ADD deviation logic — matches agg_hist() exactly
      hist_ref <- hist_ref_val()

      # Historical ribbon — apply deviation to value AND draw_values
      hist_tbl <- hist_agg_rv()[[wk]][[method]]
      hist_tbl_adj <- if (!is.null(hist_tbl) && nrow(hist_tbl) > 0L) {
        dplyr::mutate(hist_tbl,
          value       = value - hist_ref,
          draw_values = lapply(draw_values, function(dv) dv - hist_ref)
        )
      } else NULL

      hist_ribbon <- if (!is.null(hist_tbl_adj)) {
        r <- compute_exceedance_ribbon(hist_tbl_adj, band_q = bq)
        if (!is.null(r)) dplyr::mutate(r,
          scenario = "Historical",
          ssp_key  = "historical"
        ) else NULL
      } else NULL

      # Scenario ribbons — apply same hist_ref deviation
      sc_ribbons <- if (!is.null(scenario_agg_rv())) {
        Filter(Negate(is.null), lapply(names(scenario_agg_rv()), function(dk) {
          if (!dk %in% selected_scenario_names()) return(NULL)  
          tbl <- scenario_agg_rv()[[dk]][[wk]][[method]]

          # ---- TEMP DEBUG ----
            message("[debug] ensemble bounds for ", dk,
          " model_lo range: ", round(min(tbl$model_lo, na.rm=TRUE), 4),
          " to ", round(max(tbl$model_lo, na.rm=TRUE), 4),
          " model_hi range: ", round(min(tbl$model_hi, na.rm=TRUE), 4),
          " to ", round(max(tbl$model_hi, na.rm=TRUE), 4),
          " value range: ", round(min(tbl$value, na.rm=TRUE), 4),
          " to ", round(max(tbl$value, na.rm=TRUE), 4))
          # ---- END DEBUG ----
          
          if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
          tbl_adj <- dplyr::mutate(tbl,
            value       = value - hist_ref,
            draw_values = lapply(draw_values, function(dv) dv - hist_ref)
          )
          # Extract ensemble bounds — deviate by same hist_ref
          m_lo <- if (!is.null(tbl$model_lo))
            sort(tbl$model_lo - hist_ref, decreasing = TRUE) else NULL
          m_hi <- if (!is.null(tbl$model_hi))
            sort(tbl$model_hi - hist_ref, decreasing = TRUE) else NULL
          r <- compute_exceedance_ribbon(tbl_adj, band_q = bq,
                                          model_lo = m_lo,
                                          model_hi = m_hi)
          if (is.null(r)) return(NULL)
          ssp <- .normalise_ssp(dk) %||% "historical"
          dplyr::mutate(r, scenario = dk, ssp_key = ssp)
        }))
      } else list()

      result <-dplyr::bind_rows(c(list(hist_ribbon),
                         Filter(Negate(is.null), sc_ribbons)))
      if (nrow(result) == 0L) return(NULL)   

      result
                         
    })
    

    all_series <- reactive({
      req(agg_hist())
      hist_list <- list(Historical = agg_hist())
      sc        <- agg_scenarios()
      if (length(sc) == 0L) return(hist_list)
      c(hist_list, sc)
    })

  
    all_series_tbl <- reactive({
      req(agg_hist(), agg_scenarios())
      bq       <- resolve_band_q(input$uncertainty_band %||% "p10_p90")
      hist_ref <- hist_ref_val()
      wk       <- weight_key()
      method   <- input$cmp_agg_method %||% "mean"

      # Helper — compute deviated lo/hi directly from RAW draw_values
      # raw_tbl = scenario_agg_rv()[[dk]][[wk]][[method]] — undeviated
      compute_bands_from_raw <- function(raw_tbl) {
        raw_tbl |>
          dplyr::mutate(
            value_lo = purrr::map_dbl(draw_values,
              ~if (is.null(.x) || length(.x) == 0L) NA_real_
              else quantile(.x, bq["lo"], na.rm = TRUE) - hist_ref),
            value_hi = purrr::map_dbl(draw_values,
              ~if (is.null(.x) || length(.x) == 0L) NA_real_
              else quantile(.x, bq["hi"], na.rm = TRUE) - hist_ref)
          ) |>
          dplyr::select(sim_year, value_lo, value_hi,
              dplyr::any_of(c("model_q10", "model_q90",
                              "model_lo",  "model_hi")))
      }

      # Historical — join deviated value from agg_hist()$out
      # with lo/hi computed from raw hist_agg_rv()
      hist_raw   <- hist_agg_rv()[[wk]][[method]]
      hist_bands <- compute_bands_from_raw(hist_raw)
      hist_out   <- agg_hist()$out |>
        dplyr::select(-dplyr::any_of(c("value_lo", "value_hi"))) |>
        dplyr::left_join(hist_bands, by = "sim_year") |>
        dplyr::mutate(scenario = "Historical")

      # Scenarios — join deviated value from agg_scenarios()[[dk]]$out
      # with lo/hi computed from raw scenario_agg_rv()[[dk]]
      sc_list <- lapply(names(agg_scenarios()), function(dk) {
        sc_out <- agg_scenarios()[[dk]]$out
        if (is.null(sc_out) || nrow(sc_out) == 0L) return(NULL)
        sc_raw   <- scenario_agg_rv()[[dk]][[wk]][[method]]
        if (is.null(sc_raw) || nrow(sc_raw) == 0L) return(NULL)
        sc_bands <- compute_bands_from_raw(sc_raw)
        sc_out |>
          dplyr::select(-dplyr::any_of(c("value_lo", "value_hi",
                                "model_q10", "model_q90",
                                "model_lo",  "model_hi"))) |> #Edited from 'value_lo', 'value_hi'
          dplyr::left_join(sc_bands, by = "sim_year") |>
          dplyr::mutate(scenario = dk)
      })

      dplyr::bind_rows(
        c(list(hist_out), Filter(Negate(is.null), sc_list))
      )
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
      if (length(sc) == 0L)
        return(shiny::helpText("Run a simulation."))

      # Parse scenario keys into SSP × period grid
      keys  <- names(sc)
      ssps  <- sort(unique(sub(" / .*$", "", keys)))
      yrs   <- sort(unique(sub("^.* / ", "", keys)))

      # Build header row
      header <- shiny::tags$tr(
        shiny::tags$th(""),
        lapply(ssps, function(s)
          shiny::tags$th(s,
            style = "text-align:center; font-size:11px;
                    font-weight:600; padding:2px 8px;"))
      )

      # Build one row per period
      period_rows <- lapply(yrs, function(yr) {
        shiny::tags$tr(
          shiny::tags$td(yr,
            style = "font-size:11px; font-weight:600;
                    padding:2px 8px; white-space:nowrap;"),
          lapply(ssps, function(s) {
            key     <- paste0(s, " / ", yr)
            exists  <- key %in% keys
            cb_id   <- ns(paste0("sc_", gsub("[^a-zA-Z0-9]", "_", key)))
            shiny::tags$td(
              style = "text-align:center; padding:2px 4px;",
              if (exists)
                shiny::checkboxInput(
                  cb_id,
                  label = NULL,
                  value = TRUE
                )
              else
                shiny::tags$span(
                  style = "color:#ccc; font-size:11px;",
                  "—"
                )
            )
          })
        )
      })

      shiny::tags$table(
        id    = "scenario-filter-grid",
        style = "border-collapse:collapse; margin-top:4px;",
        # ADD this style tag to tighten checkboxes:
        shiny::tags$style(shiny::HTML("
          #scenario-filter-grid .checkbox { margin: 0; padding: 0; }
          #scenario-filter-grid .checkbox label { 
            padding-left: 0; 
            min-height: 0;
          }
          #scenario-filter-grid .checkbox label span { display: none; }
          #scenario-filter-grid input[type='checkbox'] { 
            width: 16px; height: 16px; 
            margin: 0 auto; 
            display: block;
            position: static;
          }
          #scenario-filter-grid td { padding: 4px 12px; }
          #scenario-filter-grid th { padding: 4px 12px; font-size: 11px; }
        ")),
        shiny::tags$thead(header),
        shiny::tags$tbody(period_rows)
      )
    })

    output$summary_box_plot <- renderPlot({
      req(agg_hist())
      plot_pointrange_climate(
        scenarios   = all_series(),
        hist_agg    = agg_hist(),
        group_order = input$cmp_group_order %||% "scenario_x_year",
        coef_bands_tbl   = if (isTRUE(input$show_coef_uncertainty) &&
                           has_draws()) all_series_tbl() else NULL
      )
    }, height = 600)

    output$summary_threshold_table <- DT::renderDT({
      req(agg_hist())
      df <- build_threshold_table_df(
        all_series  = all_series_tbl(),
        group_order = input$cmp_group_order %||% "scenario_x_year"
      )
      if (!isTRUE(input$show_coef_uncertainty) || !has_draws()) {
        df <- dplyr::filter(df, Estimate == "Central")
      }
      if (is.null(df))
        return(DT::datatable(data.frame(Message = "Insufficient data"),
                             rownames = FALSE, class = "compact stripe",
                             options  = list(dom = "t")))
      DT::datatable(
        df, rownames = FALSE, class = "compact stripe",
        options = list(
          pageLength = 15, dom = "tip", ordering = list(list(2, "desc")),
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        ),
        extensions = "Buttons"
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
        logit_x       = isTRUE(input$exceedance_logit_x),
        ribbon_data   = if (isTRUE(input$show_coef_uncertainty)) exceedance_ribbon() else NULL
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
