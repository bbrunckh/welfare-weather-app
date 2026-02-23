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
#' @noRd 
mod_1_03_outcome_server <- function(id, variable_list, survey_data) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # --------- Available outcome variables in data -----------
    available_outcomes <- reactive({
      req(variable_list(), survey_data())

      # Filter variable_list for outcomes that are present in survey data
      outs <- variable_list() |>
        dplyr::filter(outcome == 1 & name %in% colnames(survey_data())) |>
        dplyr::select(name, label, units, type)
        
      # if any name is "welfare", add new row with name "poor", label "Poor", and type "logical"
      if (any(grepl("welfare", outs$name))) {
        outs <- dplyr::bind_rows(outs, 
          data.frame(
            name = "poor", 
            label = "Poor (welfare < poverty line)",
            units = "",
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

      name_match  <- any(info$name %in% c("welfare", "poor"))
      units_match <- any(info$units == "LCU", na.rm = TRUE)

      if (!(name_match || units_match)) return(NULL)

      radioButtons(
        inputId = ns("currency"),
        label = "Currency",
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
      if (nrow(info) == 0) return(NULL)

      # require that at least one row has name == "poor"
      if (!any(info$name == "poor")) return(NULL)

      current_currency <- input$currency

      # Default poverty line based on currency
      default_line <- if (is.null(current_currency) || current_currency == "PPP") {
        3.00
      } else {
        # Calculate 20th percentile of weighted welfare for LCU
        tryCatch({
          df <- survey_data() |>
            dplyr::mutate(welfare_lcu = welfare*ppp2021)
          if ("welfare" %in% names(df)) {
            if ("weight" %in% names(df)) {
              p20 <- Hmisc::wtd.quantile(df$welfare_lcu, weights = df$weight, probs = 0.2, na.rm = TRUE)
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

      messages <- tagList()

      info_type <- tolower(as.character(info$type)[1])

      if (info_type == "numeric") {
        messages <- tagList(
          messages,
          tags$div(
            style = "margin-top: 10px; padding: 8px; background-color: #d1ecf1; border: 1px solid #bee5eb; border-radius: 4px; color: #0c5460;",
            "Continuous outcomes will be log-transformed."
          )
        )
      }

      if (info_type == "logical") {
        messages <- tagList(
          messages,
          tags$div(
            style = "margin-top: 10px; padding: 8px; background-color: #d4edda; border: 1px solid #c3e6cb; border-radius: 4px; color: #155724;",
            "Binary outcome selected."
          )
        )
      }

      messages
    })

    # --------- Get selected outcome info  -----------
    selected_outcome <- reactive({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      if (nrow(info) == 0) return(info)

      # work with first selected row and coerce to character to avoid factor comparisons
      name  <- as.character(info$name[1])
      units <- as.character(info$units[1])
      type  <- as.character(info$type[1])

      # transform: log for numeric, identity otherwise
      if (identical(type, "numeric")) {
        info$transform <- "log"
      } else {
        info$transform <- NA_character_
      }

      # decide whether to show/override currency: only when name == "poor" OR units == "LCU"
      is_poor <- identical(name, "poor")
      is_lcu  <- !is.na(units) && units == "LCU"

      if (is_poor || is_lcu) {
        # safe: input$currency may be NULL during init — guard with req when needed downstream
        info$units <- input$currency
      } else {
        # preserve original (possibly NA) unit value as character
        info$units <- units
      }

      if (is_poor && !is.null(input$poverty_line)) {
        info$povline <- input$poverty_line
      } else {
        info$povline <- NA_real_
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