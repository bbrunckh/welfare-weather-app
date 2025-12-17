#' 1_01_sample UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom dplyr filter pull
#' @importFrom pins pin_download
mod_1_01_sample_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      uiOutput(ns("sample_ui")),
      uiOutput(ns("survey_year_ui")),
      uiOutput(ns("load_button_ui"))
    )
  )
}
    
#' 1_01_sample Server Functions
#'
#' @noRd 
mod_1_01_sample_server <- function(id, survey_list_master, pin_prefix, board) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- Reactive surveys table filtered to 'external' ---
    surveys <- reactive({
      req(survey_list_master())
      filter(survey_list_master(), external)
    })
    
    # --------- Country selector UI -----------
    output$sample_ui <- renderUI({
      req(surveys())
      selectizeInput(
        inputId = ns("country"),
        label = "Country",
        choices = surveys()$countryname,
        multiple = TRUE,
        options = list(maxItems = 1, placeholder = "Select country")
      )
    })
    
    # --------- Available survey years per selected country -----------
    available_years <- reactive({
      req(input$country)
      yrs <- lapply(input$country, function(country) {
        surveys() %>%
          filter(countryname == country) %>%
          pull(year) %>% sort()
      })
      names(yrs) <- input$country
      yrs
    })
    
    # --------- Render year selectors -----------
    output$survey_year_ui <- renderUI({
      req(input$country)
      
      country_name <- input$country
      yrs <- available_years()[[country_name]]
      
      selectizeInput(
        inputId = ns("survey_year"),
        label   = paste("Survey years for", country_name),
        choices = yrs,
        selected = NULL,
        multiple = TRUE,
        options = list(
          placeholder = paste("Select years for", country_name)
        )
      )
    })
    
    # --------- Reactive Load Data button, only when at least one year is selected -----------
    output$load_button_ui <- renderUI({
      req(input$country)
      
      if (is.null(input$survey_year) || length(input$survey_year) == 0) {
        return(NULL)
      }
      
      actionButton(ns("load_data"), "Load Data", class = "btn-primary")
    })
    
    # --------- Reactive list of pin IDs to download -----------
    survey_data_files <- reactive({
      req(input$country, input$survey_year)
      
      prefix <- pin_prefix() %||% ""
      
      surveys() %>%
        filter(
          countryname == input$country,
          as.integer(year) %in% as.integer(input$survey_year)
        ) %>%
        pull(wiseapp_pin) %>%
        paste0(prefix, .)
    })
    
    # --------- ReactiveVal to hold loaded data (set by observeEvent below) -----------
    survey_data_r <- reactiveVal(NULL)
    
    # --------- Download and read the selected survey files WHEN the user clicks Load Data -----------
    observeEvent(input$load_data, {
      files <- isolate(survey_data_files())
      brd <- board()
      
      busy_id <- showNotification(
        "Downloading files…",
        duration = NULL,
        type = "message"
      )
      
      # download pins → local parquet paths
      paths <- lapply(files, function(pin_id) {
        pin_download(brd, pin_id)
      })
      paths <- Filter(Negate(is.null), paths)
      
      # read with DuckDB helper
      df <- read_parquet_duckdb(unlist(paths))
      
      removeNotification(busy_id)
      
      showNotification(
        paste0("Loaded ", length(paths), " files (", nrow(df), " rows)."),
        type = "message"
      )
      
      survey_data_r(df)
    }, ignoreInit = TRUE)
    
    # --------- Expose survey_data() reactive -----------
    survey_data <- reactive({
      survey_data_r()
    })
    
    # --------- Flag: has data been successfully loaded? -----------
    data_loaded <- reactive({
      df <- survey_data_r()
      !is.null(df) && nrow(df) > 0
    })
    
    # --------- Module return API -----------
    list(
      surveys = surveys,
      survey_data_files = survey_data_files,
      survey_data = survey_data,
      data_loaded = data_loaded
    )
  })
}
