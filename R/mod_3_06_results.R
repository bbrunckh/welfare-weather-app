#' 3_06_results UI Function
#'
#' @description A shiny Module. Renders the Step 3 Results and Diagnostics
#'   tabs: side-by-side baseline vs policy-adjusted visualisations and
#'   input-variable summary statistics.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_06_results_ui <- function(id) {
  tagList()
}


#' Results tab content UI (inserted into the Results tabPanel once).
#' @noRd
.results3_content_ui <- function(ns, so) {
  tagList(
    shiny::uiOutput(ns("results_header_ui")),

    # ---- Analysis controls ----------------------------------------------
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

    # ---- Hero comparison plot -------------------------------------------
    shiny::wellPanel(
      shiny::h4("Baseline vs policy-adjusted outcomes across scenarios"),
      shiny::plotOutput(ns("comparison_plot"), height = "600px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "Grey = Step 2 baseline prediction (no policy adjustment). ",
        "Red = Step 3 policy-adjusted prediction. Dot = mean; thick line = ",
        "P5\u2013P95; thin line = P2.5\u2013P97.5. Dashed = baseline historical mean."
      )
    ),

    # ---- Exceedance curves -----------------------------------------------
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
      shiny::tags$p(style = "font-weight:600; margin-bottom:2px;", "Baseline"),
      shiny::plotOutput(ns("exceedance_baseline"), height = "400px"),
      shiny::tags$p(style = "font-weight:600; margin-bottom:2px; margin-top:12px;",
                    "Policy-adjusted"),
      shiny::plotOutput(ns("exceedance_policy"), height = "400px"),
      shiny::uiOutput(ns("exceedance_caption"))
    ),

    # ---- Threshold table ------------------------------------------------
    shiny::wellPanel(
      shiny::h4("Outcome value at return-period thresholds"),
      shiny::tags$small(
        class = "text-muted",
        "Baseline and policy rows shown in sequence per scenario."
      ),
      DT::DTOutput(ns("threshold_table"))
    )
  )
}


#' Diagnostics tab content UI (inserted into the Diagnostics tabPanel once).
#' @noRd
.diag3_content_ui <- function(ns) {
  tagList(
    shiny::wellPanel(
      shiny::h4("Input diagnostics: baseline vs policy-adjusted model covariates"),
      shiny::tags$small(
        class = "text-muted",
        "Summary statistics restricted to variables included in the Step 1 model. ",
        "Any row with a non-zero \u0394 mean indicates the variable was modified."
      ),
      DT::DTOutput(ns("input_diag_table"))
    )
  )
}


#' Extract covariate names from a `selected_model` reactive value.
#' @noRd
.extract_model_covs <- function(sm) {
  if (is.null(sm)) return(character(0))
  take <- function(x) {
    if (is.null(x)) return(character(0))
    nms <- names(x)
    if (!is.null(nms) && any(nzchar(nms))) unique(nms[nzchar(nms)])
    else unique(as.character(unlist(x, use.names = FALSE)))
  }
  unique(c(
    take(sm$individual_covariates),
    take(sm$hh_covariates),
    take(sm$firm_covariates),
    take(sm$area_covariates)
  ))
}


#' 3_06_results Server Functions
#'
#' Renders the Step 3 Results tab and a Diagnostics tab. Combines Step 2
#' baseline predictions with Step 3 policy-adjusted predictions into dodged
#' comparison charts, and summarises pre/post covariate stats for the
#' variables actually used by the fitted model.
#'
#' @param id                       Module id.
#' @param baseline_hist_sim        Reactive Step 2 historical list.
#' @param baseline_saved_scenarios Reactive Step 2 saved scenarios named list.
#' @param policy_hist_sim          Reactive Step 3 policy-adjusted historical list.
#' @param policy_saved_scenarios   Reactive Step 3 policy-adjusted scenarios.
#' @param baseline_svy             Reactive pre-policy survey-weather frame.
#' @param policy_svy               Reactive post-policy survey-weather frame.
#' @param selected_model           Reactive named list describing Step 1 model covariates.
#' @param tabset_id                Parent tabset panel id (character).
#' @param tabset_session           Shiny session that owns the tabset.
#'
#' @noRd
mod_3_06_results_server <- function(id,
                                     baseline_hist_sim,
                                     baseline_saved_scenarios,
                                     policy_hist_sim,
                                     policy_saved_scenarios,
                                     baseline_svy,
                                     policy_svy,
                                     selected_model = reactive(NULL),
                                     tabset_id,
                                     tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (is.null(tabset_session)) tabset_session <- session

    # ---- Reactive helpers ------------------------------------------------

    pov_line_val <- reactive({
      if (isTRUE(input$cmp_agg_method %in% c("headcount_ratio", "gap", "fgt2"))) {
        input$cmp_pov_line %||% NULL
      } else {
        NULL
      }
    })

    agg_one <- function(preds, so) {
      method    <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation  %||% "none"
      pov       <- pov_line_val()
      aggregate_sim_preds(preds, so, method, deviation, FALSE, pov)
    }

    # ---- Filter-related reactives (mirrors mod_2_02_results) ------------

    all_ssps <- reactive({
      sc <- baseline_saved_scenarios() %||% list()
      if (length(sc) == 0) return(character(0))
      ssps <- unique(.normalise_ssp(names(sc)))
      sort(ssps[!is.na(ssps) & grepl("^SSP", ssps)])
    })

    all_anchor_years <- reactive({
      sc <- baseline_saved_scenarios() %||% list()
      if (length(sc) == 0) return(character(0))
      ranges <- sort(na.omit(unique(.parse_year(names(sc)))))
      setNames(sub("-", "_", ranges), ranges)
    })

    all_models_info <- reactive({
      sc <- baseline_saved_scenarios() %||% list()
      if (length(sc) == 0) return(integer(0))
      vapply(sc, function(s) s$n_models %||% 1L, integer(1))
    })

    selected_scenario_names <- reactive({
      sc <- baseline_saved_scenarios() %||% list()
      if (length(sc) == 0) return(character(0))
      nms  <- names(sc)
      ssps <- if (length(input$filter_ssp)  == 0) all_ssps()
              else input$filter_ssp
      yr_vals <- if (length(input$filter_year) == 0) names(all_anchor_years())
                 else sub("_", "-", input$filter_year)
      keep <- vapply(nms, function(nm) {
        is_ssp <- grepl("^SSP", nm)
        if (!is_ssp) return(TRUE)
        ssp_match <- any(vapply(ssps, function(s) startsWith(nm, s),
                                logical(1)))
        yr_match  <- length(yr_vals) == 0 ||
          any(vapply(yr_vals, function(y) grepl(y, nm, fixed = TRUE),
                     logical(1)))
        ssp_match && yr_match
      }, logical(1))
      nms[keep]
    })

    # ---- Aggregates -----------------------------------------------------

    baseline_hist_agg <- reactive({
      req(baseline_hist_sim())
      agg_one(baseline_hist_sim()$preds, baseline_hist_sim()$so)
    })

    baseline_scen_agg <- reactive({
      sc <- baseline_saved_scenarios() %||% list()
      if (length(sc) == 0) return(list())
      lapply(sc, function(s)
        tryCatch(agg_one(s$preds, s$so), error = function(e) NULL))
    })

    policy_hist_agg <- reactive({
      req(policy_hist_sim())
      agg_one(policy_hist_sim()$preds, policy_hist_sim()$so)
    })

    policy_scen_agg <- reactive({
      sc <- policy_saved_scenarios() %||% list()
      if (length(sc) == 0) return(list())
      lapply(sc, function(s)
        tryCatch(agg_one(s$preds, s$so), error = function(e) NULL))
    })

    baseline_series <- reactive({
      req(baseline_hist_agg())
      bs <- baseline_scen_agg()
      bs <- bs[!vapply(bs, is.null, logical(1))]
      sel <- selected_scenario_names()
      bs <- bs[intersect(sel, names(bs))]
      c(setNames(list(baseline_hist_agg()), "Historical"), bs)
    })

    policy_series <- reactive({
      req(policy_hist_agg())
      ps <- policy_scen_agg()
      ps <- ps[!vapply(ps, is.null, logical(1))]
      sel <- selected_scenario_names()
      ps <- ps[intersect(sel, names(ps))]
      c(setNames(list(policy_hist_agg()), "Historical"), ps)
    })

    # ---- Results tab outputs --------------------------------------------

    output$results_header_ui <- renderUI({
      req(policy_hist_sim(), input$cmp_agg_method, input$cmp_deviation)
      so <- policy_hist_sim()$so
      agg_label <- label_agg_method(input$cmp_agg_method)
      dev_label <- label_deviation(input$cmp_deviation)
      pov_txt   <- if (!is.null(pov_line_val()))
        paste0(" | Poverty line: $", pov_line_val(), "/day") else ""
      shiny::div(
        style = paste0(
          "border-left: 4px solid #c0392b; background: #fdf4f3; ",
          "padding: 10px 14px; margin-bottom: 12px; border-radius: 3px;"
        ),
        shiny::tags$strong(
          style = "font-size:15px;",
          paste0("Policy simulation results: ", so$label %||% so$name)
        ),
        shiny::tags$br(),
        shiny::tags$span(
          style = "color:#555; font-size:12px;",
          paste0("Showing ", agg_label, " of ", so$label %||% so$name,
                 " expressed as ", dev_label, pov_txt, ".")
        )
      )
    })

    output$cmp_pov_line_ui <- renderUI({
      req(input$cmp_agg_method)
      if (input$cmp_agg_method %in% c("headcount_ratio", "gap", "fgt2")) {
        shiny::numericInput(
          ns("cmp_pov_line"),
          label = "Poverty line (daily, 2021 PPP USD)",
          value = 3.00, min = 0, step = 0.5
        )
      }
    })

    output$scenario_filter_ui <- renderUI({
      sc <- baseline_saved_scenarios() %||% list()
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
            if (length(mi) > 0 && any(mi > 1L))
              shiny::helpText(
                style = "font-size:11px; color:#555; margin-top:24px;",
                paste0("Each SSP aggregates results from ",
                       max(mi), " ensemble model(s).")
              )
          )
        )
      )
    })

    output$comparison_plot <- renderPlot({
      req(baseline_series(), policy_series())
      plot_policy_comparison(
        baseline_series = baseline_series(),
        policy_series   = policy_series(),
        hist_agg        = baseline_hist_agg(),
        group_order     = input$cmp_group_order %||% "scenario_x_year"
      )
    }, height = 600)
    outputOptions(output, "comparison_plot", suspendWhenHidden = FALSE)

    # ---- Exceedance plots -------------------------------------------------

    output$exceedance_baseline <- renderPlot({
      req(baseline_hist_agg(), baseline_series())
      enhance_exceedance(
        scenarios     = baseline_series(),
        hist_agg      = baseline_hist_agg(),
        x_label       = baseline_hist_agg()$x_label,
        return_period = isTRUE(input$show_return_period),
        n_sim_years   = nrow(baseline_hist_agg()$out),
        logit_x       = isTRUE(input$exceedance_logit_x)
      )
    })
    outputOptions(output, "exceedance_baseline", suspendWhenHidden = FALSE)

    output$exceedance_policy <- renderPlot({
      req(policy_hist_agg(), policy_series())
      enhance_exceedance(
        scenarios     = policy_series(),
        hist_agg      = policy_hist_agg(),
        x_label       = policy_hist_agg()$x_label,
        return_period = isTRUE(input$show_return_period),
        n_sim_years   = nrow(policy_hist_agg()$out),
        logit_x       = isTRUE(input$exceedance_logit_x)
      )
    })
    outputOptions(output, "exceedance_policy", suspendWhenHidden = FALSE)

    output$exceedance_caption <- renderUI({
      req(baseline_hist_agg())
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

    output$threshold_table <- DT::renderDT({
      req(baseline_series(), policy_series())
      go   <- input$cmp_group_order %||% "scenario_x_year"
      df_b <- build_threshold_table_df(baseline_series(), group_order = go)
      df_p <- build_threshold_table_df(policy_series(),   group_order = go)
      if (is.null(df_b) || is.null(df_p))
        return(DT::datatable(
          data.frame(Message = "Insufficient data"),
          rownames = FALSE, options = list(dom = "t")
        ))
      df_b$variant <- "Baseline"
      df_p$variant <- "Policy"
      df <- dplyr::bind_rows(df_b, df_p)
      df <- df[, c("variant", setdiff(names(df), "variant"))]
      DT::datatable(
        df, rownames = FALSE, class = "compact stripe",
        options = list(
          pageLength = 20, dom = "tp", ordering = FALSE,
          columnDefs = list(list(className = "dt-center", targets = "_all"))
        )
      )
    })
    outputOptions(output, "threshold_table", suspendWhenHidden = FALSE)

    # ---- Diagnostics tab output -----------------------------------------

    output$input_diag_table <- DT::renderDT({
      req(baseline_svy(), policy_svy())
      model_covs <- .extract_model_covs(selected_model())
      if (length(model_covs) == 0)
        return(DT::datatable(
          data.frame(Message = "No model covariates detected."),
          rownames = FALSE, options = list(dom = "t")
        ))
      # Restrict to covariates that exist in the survey frame
      vars <- intersect(model_covs, names(baseline_svy()))
      df <- policy_input_diagnostics(baseline_svy(), policy_svy(),
                                     vars = vars)
      if (is.null(df) || nrow(df) == 0)
        return(DT::datatable(
          data.frame(Message = "Model covariates are non-numeric or absent."),
          rownames = FALSE, options = list(dom = "t")
        ))
      num_cols <- setdiff(names(df), "variable")
      df[num_cols] <- lapply(df[num_cols], function(x) signif(x, 4))
      DT::datatable(
        df, rownames = FALSE, class = "compact stripe",
        options = list(pageLength = 25, dom = "tp", ordering = TRUE)
      )
    })
    outputOptions(output, "input_diag_table", suspendWhenHidden = FALSE)

    # ---- Tab insertion --------------------------------------------------

    observeEvent(policy_hist_sim(), {
      req(policy_hist_sim())

      # Results tab
      shiny::appendTab(
        inputId = tabset_id,
        shiny::tabPanel(
          title = "Results",
          value = "policy_results_tab",
          shiny::div(id = ns("results3_section"))
        ),
        select  = TRUE,
        session = tabset_session
      )
      shiny::insertUI(
        selector = paste0("#", ns("results3_section")),
        where    = "afterBegin",
        ui       = .results3_content_ui(ns, policy_hist_sim()$so)
      )

      # Diagnostics tab
      shiny::appendTab(
        inputId = tabset_id,
        shiny::tabPanel(
          title = "Diagnostics",
          value = "policy_diag_tab",
          shiny::div(id = ns("diag3_section"))
        ),
        select  = FALSE,
        session = tabset_session
      )
      shiny::insertUI(
        selector = paste0("#", ns("diag3_section")),
        where    = "afterBegin",
        ui       = .diag3_content_ui(ns)
      )
    }, ignoreInit = TRUE, once = TRUE)

    observeEvent(policy_hist_sim(), {
      shiny::updateTabsetPanel(
        session  = tabset_session,
        inputId  = tabset_id,
        selected = "policy_results_tab"
      )
    }, ignoreInit = TRUE)

    observeEvent(policy_hist_sim(), {
      req(policy_hist_sim()$so)
      so      <- policy_hist_sim()$so
      choices <- hist_aggregate_choices(so$type, so$name)
      current <- isolate(input$cmp_agg_method)
      new_sel <- if (!is.null(current) && current %in% choices) current
                 else "mean"
      shiny::updateSelectInput(session, "cmp_agg_method",
                               choices = choices, selected = new_sel)
    }, ignoreInit = TRUE)

    invisible(NULL)
  })
}
