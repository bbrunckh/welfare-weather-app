#' 1_03_outcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_03_outcome_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      uiOutput(ns("outcome_ui")),
      uiOutput(ns("currency_ui")),
      uiOutput(ns("poverty_line_ui")),
      uiOutput(ns("outcome_info"))
    ),
    uiOutput(ns("outcome_stats_button_ui"))
  )
}

#' 1_03_outcome Server Functions
#'
#' @param id Module id.
#' @param variable_list Reactive data frame — variable metadata from
#'   `mod_0_overview`.
#' @param survey_data Reactive data frame — loaded survey data from
#'   `mod_1_02_surveystats`.
#' @param map_data Reactive GeoJSON FeatureCollection from
#'   `mod_1_02_surveystats` (H3 map data). Used for outcome coverage map.
#' @param tabset_id Character id of the parent tabset panel.
#' @param tabset_session Shiny session for the parent tabset.
#'
#' @noRd
mod_1_03_outcome_server <- function(id, variable_list, survey_data,
                                    map_data       = reactive(NULL),
                                    tabset_id      = NULL,
                                    tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    # ---- Available outcome variables present in the survey data -------------

    available_outcomes <- reactive({
      req(variable_list(), survey_data())
      filter_outcome_vars(variable_list(), colnames(survey_data()))
    })

    # ---- Outcome selector UI ------------------------------------------------

    output$outcome_ui <- renderUI({
      req(available_outcomes())
      outs <- available_outcomes()

      choice_labels <- paste0(outs$label, " (", outs$name, ")")
      choice_map <- stats::setNames(outs$name, choice_labels)

      selectizeInput(
        inputId  = ns("outcome"),
        label    = "Outcome variable",
        choices  = choice_map,
        selected = outs$name[1],
        multiple = FALSE
      )
    })

    # ---- Selected outcome info (single row from available_outcomes) ---------

    selected_outcome_info <- reactive({
      req(input$outcome, available_outcomes())
      outs <- available_outcomes()
      outs[outs$name == input$outcome, , drop = FALSE]
    })

    # ---- Currency selector (monetary outcomes only) -------------------------

    output$currency_ui <- renderUI({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      if (nrow(info) == 0) return(NULL)

      if (!is_monetary_outcome(info$name[1], info$units[1])) return(NULL)

      radioButtons(
        inputId  = ns("currency"),
        label    = "Currency",
        choices  = c("PPP (2021)" = "PPP", "LCU (2021)" = "LCU"),
        selected = "PPP"
      )
    })

    # ---- Poverty line input (poor outcome only) -----------------------------

    output$poverty_line_ui <- renderUI({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      if (nrow(info) == 0 || !identical(as.character(info$name[1]), "poor")) return(NULL)

      currency <- input$currency
      default_line <- if (is.null(currency) || identical(currency, "PPP")) {
        3.00
      } else {
        default_lcu_poverty_line(survey_data())
      }

      numericInput(
        inputId = ns("poverty_line"),
        label   = poverty_line_label(currency),
        value   = default_line,
        min     = 0,
        step    = 0.01
      )
    })

    # ---- Informational message about selected outcome type ------------------

    output$outcome_info <- renderUI({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      if (nrow(info) == 0) return(NULL)
      outcome_info_message(info$type[1])
    })

    # ---- Augmented selected outcome row (with transform/units/povline) ------

    selected_outcome <- reactive({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      if (nrow(info) == 0) return(info)
      build_selected_outcome(
        info         = info,
        currency     = input$currency,
        poverty_line = input$poverty_line
      )
    })

    # ---- Outcome Stats button -----------------------------------------------

    output$outcome_stats_button_ui <- renderUI({
      req(input$outcome, survey_data())
      actionButton(ns("outcome_stats_btn"), "Outcome stats",
                   class = "btn-primary", style = "width: 100%;")
    })

    outcome_tab_added <- reactiveVal(FALSE)

    # ---- Survey data augmented with synthetic "poor" column -------------------

    outcome_data <- reactive({
      req(survey_data(), selected_outcome_info())
      df <- survey_data()
      inf <- selected_outcome_info()
      oname <- as.character(inf$name[1])
      if (identical(oname, "poor") && !"poor" %in% names(df)) {
        so <- selected_outcome()
        pl <- if (!is.null(so) && !is.na(so$povline)) so$povline else 3.00
        if ("welfare" %in% names(df)) {
          df$poor <- as.integer(df$welfare < pl)
        }
      }
      df
    })

    # ---- Outcome Stats tab creation -----------------------------------------

    observeEvent(input$outcome_stats_btn, {
      req(input$outcome, survey_data(), selected_outcome_info())

      # Define outputs (once)
      if (!outcome_tab_added()) {

        output$outcome_dist <- renderPlot({
          req(outcome_data(), selected_outcome_info())
          inf <- selected_outcome_info()
          p <- plot_welfare_dist(
            outcome_data(),
            outcome = as.character(inf$name[1]),
            label   = as.character(inf$label[1]),
            type    = as.character(inf$type[1])
          )
          if (is.null(p)) {
            plot.new()
            title(main = "Distribution unavailable")
            return(invisible(NULL))
          }
          p
        })

        output$outcome_coverage_map <- leaflet::renderLeaflet({
          req(outcome_data(), selected_outcome_info(), map_data())
          inf <- selected_outcome_info()
          m <- plot_outcome_coverage_map(
            map_data(), outcome_data(),
            outcome = as.character(inf$name[1])
          )
          req(!is.null(m))
          m
        })

        output$outcome_summary_stats <- renderTable({
          req(outcome_data(), selected_outcome_info())
          inf <- selected_outcome_info()
          oname_local <- as.character(inf$name[1])
          otype_local <- as.character(inf$type[1])
          vals <- outcome_data()[[oname_local]]
          n_total <- length(vals)
          n_avail <- sum(!is.na(vals))
          n_miss  <- n_total - n_avail
          coverage <- round(100 * n_avail / max(n_total, 1), 1)
          vals <- vals[!is.na(vals)]

          obs_rows <- data.frame(
            Statistic = c("Observations", "Coverage (%)"),
            Value = c(
              format(n_total, big.mark = ","),
              paste0(coverage, "%")
            ),
            stringsAsFactors = FALSE
          )

          if (length(vals) == 0) return(obs_rows)

          if (otype_local == "numeric") {
            vals <- as.numeric(vals)
            stat_rows <- data.frame(
              Statistic = c("Mean", "Median", "Std Dev", "Min",
                            "P10", "P25", "P75", "P90", "Max"),
              Value = as.character(round(c(
                mean(vals), stats::median(vals), stats::sd(vals),
                min(vals),
                stats::quantile(vals, 0.10),
                stats::quantile(vals, 0.25),
                stats::quantile(vals, 0.75),
                stats::quantile(vals, 0.90),
                max(vals)
              ), 3)),
              stringsAsFactors = FALSE
            )
          } else {
            vals <- as.integer(vals)
            n1 <- sum(vals == 1L)
            n0 <- sum(vals == 0L)
            stat_rows <- data.frame(
              Statistic = c("Count = 1 (Yes)", "Count = 0 (No)",
                            "Share = 1"),
              Value = as.character(c(
                format(n1, big.mark = ","),
                format(n0, big.mark = ","),
                round(n1 / max(n1 + n0, 1), 3)
              )),
              stringsAsFactors = FALSE
            )
          }

          rbind(obs_rows, stat_rows)
        }, striped = TRUE, hover = TRUE, bordered = TRUE)

        # Append tab
        tryCatch(
          shiny::appendTab(
            inputId = tabset_id,
            shiny::tabPanel(
              title = "Outcome stats",
              value = "outcome_stats_tab",
              bslib::layout_columns(
                col_widths = c(6, 6),
                bslib::card(
                  shiny::h4("Summary statistics"),
                  shiny::tableOutput(ns("outcome_summary_stats"))
                ),
                bslib::card(
                  shiny::h4("Spatial coverage"),
                  leaflet::leafletOutput(ns("outcome_coverage_map"))
                )
              ),
              shiny::br(),
              bslib::card(
                shiny::h4("Outcome Distribution (by survey wave)"),
                shiny::plotOutput(ns("outcome_dist"), height = "300px")
              ),
              tags$div(style = "height: 40px;")
            ),
            select  = TRUE,
            session = tabset_session
          ),
          error = function(e) {
            shiny::showNotification(
              paste("Failed to add Outcome stats tab:",
                    conditionMessage(e)),
              type = "error"
            )
          }
        )

        outcome_tab_added(TRUE)
      }

      if (outcome_tab_added()) {
        try(shiny::updateTabsetPanel(
          tabset_session, inputId = tabset_id,
          selected = "outcome_stats_tab"
        ), silent = TRUE)
      }

    }, ignoreInit = TRUE)

    # ---- Module return API --------------------------------------------------

    list(
      selected_outcome = selected_outcome
    )
  })
}
