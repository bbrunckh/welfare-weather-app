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
    )
  )
}

#' 1_03_outcome Server Functions
#'
#' @param id Module id.
#' @param variable_list Reactive data frame — variable metadata from
#'   `mod_0_overview`.
#' @param survey_data Reactive data frame — loaded survey data from
#'   `mod_1_02_surveystats`.
#'
#' @noRd
mod_1_03_outcome_server <- function(id, variable_list, survey_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Available outcome variables present in the survey data -------------

    available_outcomes <- reactive({
      req(variable_list(), survey_data())
      filter_outcome_vars(variable_list(), colnames(survey_data()))
    })

    # ---- Outcome selector UI ------------------------------------------------

    output$outcome_ui <- renderUI({
      req(available_outcomes())
      outs <- available_outcomes()
      selectizeInput(
        inputId  = ns("outcome"),
        label    = "Outcome variable",
        choices  = setNames(outs$name, outs$label),
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

    # ---- Module return API --------------------------------------------------

    list(
      selected_outcome = selected_outcome
    )
  })
}