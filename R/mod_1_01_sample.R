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
    
    # --------- Reactive list of pin IDs to download ----------- -> THIS FAILS TO FETCH THE DATA BY BEING IDLE (MOST LIKELY)
    survey_data_files <- reactive({
      req(input$country, input$survey_year, pin_prefix())
      
      surveys() %>%
        filter(
          countryname == input$country,
          year %in% input$survey_year
        ) %>%
        pull(wiseapp_pin) %>%
        paste0(pin_prefix(), .)
      
    })
    
    # --------- ReactiveVal to hold loaded data (set by observeEvent below) -----------
    survey_data_r <- reactiveVal(NULL)
    
    # --------- Download and read the selected survey files WHEN the user clicks Load Data -----------
    observeEvent(input$load_data, {
      # Debug log for button click
      message("DEBUG: Load Data button pressed; input$load_data = ", input$load_data)
      
      try({
        message("DEBUG: files selected: ", isolate(survey_data_files()))
      }, silent = TRUE)
      
      files <- isolate(survey_data_files())
      
      # persistent "busy" notification (remove when finished)
      busy_id <- showNotification("Downloading files...", duration = NULL, type = "message")
      
      # attempt downloads
      paths <- lapply(files, function(pin_id) {
        tryCatch(
          pins::pin_download(board(), pin_id),
          error = function(e) {
            warning("download failed for ", pin_id, ": ", e$message)
            NULL
          }
        )
      })
      paths <- Filter(Negate(is.null), paths)
      
      if (length(paths) == 0) {
        removeNotification(busy_id)
        showNotification("No files could be downloaded for the selected options.", type = "error")
        survey_data_r(tibble::tibble())
        return(invisible())
      }
      
      # read into a single tibble (wrap in tryCatch)
      df <- tryCatch({
        read_parquet_duckdb(unlist(paths), options = list(union_by_name = TRUE), prudence = "lavish")
      }, error = function(e) {
        removeNotification(busy_id)
        showNotification(paste0("Error reading files: ", e$message), type = "error")
        warning(e)
        return(tibble::tibble())
      })
      
      removeNotification(busy_id)
      
      # final notification & set reactiveVal
      showNotification(paste0("Loaded ", length(paths), " files (", nrow(df), " rows)."), type = "message")
      survey_data_r(df)
      
      # Debug print
      message("DEBUG: Loaded df rows = ", ifelse(is.data.frame(df), nrow(df), NA))
    }, ignoreInit = TRUE)
    
    # --------- Expose survey_data() reactive and a flag data_loaded() -----------
    survey_data <- reactive({
      survey_data_r()
    })
    
    data_loaded <- reactive({
      df <- survey_data_r()
      !is.null(df) && nrow(df) > 0
    })
    
    # --------- Module return API -----------
    list(
      surveys = surveys,
      survey_data_files = survey_data_files,
      survey_data = survey_data,   # reactive: call survey_data()
      data_loaded = data_loaded    # reactive: call data_loaded()
    )
  })
}
