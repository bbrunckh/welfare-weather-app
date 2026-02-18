#' 1_01_sample UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom dplyr filter pull mutate inner_join select arrange bind_rows
#' @importFrom pins pin_download
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
#' @noRd 
mod_1_01_sample_server <- function(id, survey_list_master, varlist, data_dir) { #pin_prefix, board, survey_metadata, varlist) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    
    # --------- Level of analysis selector UI -----------
    output$unit_ui <- renderUI({
      radioButtons(
        inputId = ns("unit"),
        label   = "Level of analysis",
        choices = c(
          "Individual" = "ind",
          "Household"  = "hh",
          "Firm"       = "firm"
        ),
        selected = "hh"
      )
    })

    # --------- Sample selector UI (economies) -----------
    # surveys with data files in folder for selected level of analysis
    surveys <- reactive({
      req(input$unit)
      slm <- survey_list_master()
      req(slm)
      dir_path <- data_dir()
      req(dir_path)

      # get list of expected file paths
      slm <- slm |>
        mutate(
          fpath = file.path(
            dir_path,
             paste0(code, "_", year, "_", survname, "_", level, ".parquet")
          )) |>
        # filter survey list to files that exist and selected level
        filter(file.exists(fpath), level == input$unit)
      slm
    })

    # --------- Country selector UI -----------
output$sample_ui <- renderUI({
  req(surveys())
  
  # Create named vector: display economy names, but values are codes
  choices <- setNames(surveys()$code, surveys()$economy)
  
  selectizeInput(
    inputId = ns("economy"),
    label = "Economy",
    choices = choices,
    selected = choices[1],
    multiple = TRUE,
    options = list(maxItems = 2, placeholder = "Select countries")
  )
})

  # --------- Available survey years per selected code -----------
  available_years <- reactive({
    req(input$economy)  # Now contains codes, not economy names
    
    yrs <- lapply(input$economy, function(code) {
      surveys() |>
        filter(code == code) |>
        pull(year) |> 
        sort()
    })
    names(yrs) <- input$economy
    yrs
  })

  # --------- Year selector UI -----------
  output$survey_year_ui <- renderUI({
    req(input$economy)
    
    codes <- input$economy
    all_years <- available_years()
    
    # Create a selectizeInput for each selected economy code
    year_inputs <- lapply(codes, function(code) {
      yrs <- all_years[[code]]
      
      # Get the economy name for the label
      economy_name <- surveys() |>
        filter(code == code) |>
        pull(economy) |>
        head(1)
      
      tagList(
        selectizeInput(
          inputId = ns(paste0("survey_year_", code)),
          label = paste("Survey years for", economy_name),
          choices = yrs,
          selected = yrs,
          multiple = TRUE,
          options = list(
            placeholder = paste("Select years for", economy_name)
          )
        )
      )
    })
    
    tagList(year_inputs)
  })

    # --------- Collect selected years from all dynamic inputs -----------
    selected_years_by_code <- reactive({
      req(input$economy)
      
      codes <- input$economy
      years_list <- lapply(codes, function(code) {
        input_name <- paste0("survey_year_", code)
        input[[input_name]]
      })
      names(years_list) <- codes
      
      # Filter out NULL values
      Filter(Negate(is.null), years_list)
    })

    # # --------- Reactive Load Data button -----------
    # output$load_button_ui <- renderUI({
    #   req(input$economy)
      
    #   years <- selected_years_by_code()
      
    #   # Check if at least one economy has years selected
    #   if (length(years) == 0 || all(sapply(years, length) == 0)) {
    #     return(NULL)
    #   }
      
    #   actionButton(ns("load_data"), "Load Data", class = "btn-primary")
    # })

    # --------- Reactive to hold selected surveys -----------
    selected_surveys <- reactive({
      req(input$economy)
      years_by_code <- selected_years_by_code()
      req(length(years_by_code) > 0)
      
      # Create a data frame of all selected code/year combinations
      selected_combos <- do.call(rbind, lapply(names(years_by_code), function(code) {
        years <- years_by_code[[code]]
        if (length(years) == 0) return(NULL)
        data.frame(code = code, year = as.numeric(years), stringsAsFactors = FALSE)
      }))
      # Filter surveys() by the selected combinations 
      selected_surveys <- surveys() |> 
        inner_join(selected_combos, by = c("code", "year")) 
      selected_surveys
    })
    
    
    # --------- Module return API -----------
    list(
      selected_surveys = selected_surveys
    )
  })
}
