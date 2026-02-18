#' 1_02_outcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_02_outcome_ui <- function(id) {
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
    
#' 1_02_outcome Server Functions
#'
#' @noRd 
mod_1_02_outcome_server <- function(id, varlist, survey_data) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # --------- Available outcome variables in data -----------
    available_outcomes <- reactive({
      req(varlist(), survey_data())

      # Filter varlist for outcomes that are present in survey data
      outs <- varlist() |>
        dplyr::filter(outcome == 1 & name %in% colnames(survey_data())) |>
        dplyr::select(name, label, units, type)
        
      # if any name is "welfare", add new row with name "poor", label "Poor", and type "logical"
      if (any(grepl("welfare", outs$name))) {
        outs <- dplyr::bind_rows(outs, 
          data.frame(
            name = "poor", 
            label = "Poor (welfare < poverty line)",
            units = "Binary",
            type = "logical",
            stringsAsFactors = FALSE)
        )
      }
      # order welfare/poor first if they exist, then by label
      outs <- outs |>
        dplyr::arrange(factor(name, levels = c("welfare", "poor", setdiff(name, c("welfare", "poor")))))
      outs
    })

    # --------- Outcome selector UI -----------
    output$outcome_ui <- renderUI({
      req(available_outcomes())
      
      outs <- available_outcomes()
      
      selectizeInput(
        inputId = ns("outcome"),
        label = "Outcome variable",
        choices = setNames(outs$name, outs$label),  
        selected = outs$name[1],
        multiple = FALSE
      )
    })

    # --------- Get selected outcome info  -----------
    selected_outcome_info <- reactive({
      req(input$outcome, available_outcomes())
      
      outs <- available_outcomes()
      info <- outs[outs$name == input$outcome, , drop = FALSE]
      info
    })

    # --------- Currency selector UI (for monetary outcomes) -----------
    output$currency_ui <- renderUI({
      req(selected_outcome_info())
      info <- selected_outcome_info()

      if (!(info$name %in% c("welfare", "poor") || info$units == "LCU")) return(NULL)
  
      radioButtons(
        inputId = ns("currency"),
        label = "Currency units",
        choices = c(
          "PPP (2021)" = "PPP",
          "LCU (2021)" = "LCU"
        ),
        selected = "PPP"
      )
    })

    # --------- Poverty line input (for "poor" outcome) -----------
    output$poverty_line_ui <- renderUI({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      
      if (nrow(info) == 0 || info$name != "poor") return(NULL)

      current_currency <- input$currency
      
      # Default poverty line based on currency
      default_line <- if (is.null(current_currency) || current_currency == "PPP") {
        3.00
      } else {
        # Calculate 20th percentile of weighted welfare for LCU
        tryCatch({
          df <- survey_data()
          if ("welfare" %in% names(df)) {
            if ("weight" %in% names(df)) {
              p20 <- Hmisc::wtd.quantile(df$welfare, weights = df$weight, probs = 0.2, na.rm = TRUE)
              round(as.numeric(p20), 2)
            } else {
              round(quantile(df$welfare, probs = 0.2, na.rm = TRUE), 2)
            }
          } else {
            1.00
          }
        }, error = function(e) {
          message("Error calculating LCU poverty line: ", e$message)
          1.00
        })
      }
      
      # Label based on currency
      label_text <- if (is.null(current_currency) || current_currency == "PPP") {
        "Poverty line ($/day, 2021 PPP)"
      } else {
        "Poverty line (LCU/day)"
      }
      
      numericInput(
        inputId = ns("poverty_line"),
        label = label_text,
        value = default_line,
        min = 0,
        step = 0.01
      )
    })

    # --------- Informational message about outcome -----------
    output$outcome_info <- renderUI({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      
      if (nrow(info) == 0) return(NULL)
      
      messages <- tagList()  # Start with empty tagList, not list()
      
      # Message about continuous variables (will be log-transformed)
      if (tolower(info$type) == "numeric") {
        messages <- tagList(
          messages,
          tags$div(
            style = "margin-top: 10px; padding: 8px; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; color: #0c5460;",
            "Continuous outcomes will be log-transformed."
          )
        )
      }
      
      # Message about binary outcomes
      if (tolower(info$type) == "logical") {
        messages <- tagList(
          messages,
          tags$div(
            style = "margin-top: 10px; padding: 8px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px; color: #155724;",
            "Binary outcome selected."
          )
        )
      }
      
      messages  # Return the tagList directly, not wrapped again
    })

    # --------- Get selected outcome info  -----------
    selected_outcome <- reactive({
      req(selected_outcome_info())
      info <- selected_outcome_info()  
      
      # Add currency and poverty line to the info
      if (nrow(info) > 0) {
        info$currency <- if (!is.null(input$currency)) input$currency else NA_character_
        info$poverty_line <- if (!is.null(input$poverty_line)) input$poverty_line else NA_real_
      }
      info
    })

    # Module return API
    list(
      # Return selected outcome name, label, type, units, currency, and poverty line as a reactive
      selected_outcome = selected_outcome
    )
  })
}