#' 1_01_sample UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_01_sample_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      uiOutput(ns("unit_ui")),
      uiOutput(ns("sample_ui")),
      uiOutput(ns("survey_year_ui")),
      uiOutput(ns("load_button_ui"))
    )
  )
}

#' 1_01_sample Server Functions
#'
#' @param id Module id.
#' @param survey_list Reactive tibble from mod_0_overview (survey metadata).
#' @param variable_list Reactive tibble from mod_0_overview (variable metadata).
#' @param connection_params Reactive named list from mod_0_overview
#'   (connection type + credentials/path).
#'
#' @noRd
mod_1_01_sample_server <- function(id, connection_params, survey_list, variable_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- List available files at the connection endpoint --------------------

    available_files <- reactive({
      req(connection_params())
      list_available_files(connection_params())
    })

    # ---- Level of analysis selector -----------------------------------------

    output$unit_ui <- renderUI({
      radioButtons(
        inputId  = ns("unit"),
        label    = "Level of analysis",
        choices  = c(
          "Individual" = "ind",
          "Household"  = "hh",
          "Firm"       = "firm"
        ),
        selected = "hh"
      )
    })

    # ---- Surveys available at the endpoint for the selected unit ------------

    surveys <- reactive({
      req(input$unit, survey_list())
      sl <- build_survey_fnames(survey_list(), input$unit, connection_params())
      filter_surveys_to_available(sl, available_files())
    })

    # ---- Economy selector ---------------------------------------------------

    output$sample_ui <- renderUI({
      req(surveys())
      sv <- surveys()
      if (nrow(sv) == 0) {
        return(helpText(
          "No data files found for the selected level of analysis.",
          style = "color: red; font-size: 12px;"
        ))
      }
      choices         <- setNames(sv$code, sv$economy)
      choices         <- choices[!duplicated(choices)]
      selectizeInput(
        inputId  = ns("economy"),
        label    = "Economy",
        choices  = choices,
        # make default "BFA" if it exists, otherwise first in list
        selected = if ("BFA" %in% sv$code) "BFA" else choices[1],
        multiple = TRUE,
        options  = list(maxItems = 2, placeholder = "Select up to 2 economies")
      )
    })

    # ---- Available survey years per selected economy ------------------------

    available_years <- reactive({
      req(input$economy, surveys())
      get_available_years(surveys(), input$economy)
    })

    # ---- Year selector UI ---------------------------------------------------

    output$survey_year_ui <- renderUI({
      req(input$economy, available_years())
      codes   <- input$economy
      all_yrs <- available_years()

      year_inputs <- lapply(codes, function(code) {
        yrs          <- all_yrs[[code]]
        economy_name <- surveys() |>
          dplyr::filter(.data$code == !!code) |>
          dplyr::pull(.data$economy) |>
          head(1)
        selectizeInput(
          inputId  = ns(paste0("year_", code)),
          label    = paste("Survey years —", economy_name),
          choices  = yrs,
          selected = yrs,
          multiple = TRUE,
          options  = list(placeholder = paste("Select years for", economy_name))
        )
      })

      tagList(year_inputs)
    })

    # ---- Collect selected years from dynamic inputs -------------------------

    selected_years_by_code <- reactive({
      req(input$economy)
      years_list <- lapply(
        stats::setNames(input$economy, input$economy),
        function(code) input[[paste0("year_", code)]]
      )
      Filter(Negate(is.null), years_list)
    })

    # ---- Selected surveys ---------------------------------------------------

    selected_surveys <- reactive({
      req(input$economy)
      years_by_code <- selected_years_by_code()
      req(length(years_by_code) > 0)
      build_selected_surveys(surveys(), years_by_code)
    })

    # ---- Return API ---------------------------------------------------------

    list(
      selected_surveys = selected_surveys
    )
  })
}