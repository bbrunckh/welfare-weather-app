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
  tags$style(".shiny-notification { z-index: 99999 !important; }")
  tagList(
    # container for the country and year selectors
    wellPanel(
      uiOutput(ns("sample_ui")),
      uiOutput(ns("survey_year_ui")),
      # Load button (user must click to trigger download)
      actionButton(ns("load_data"), "Load Data", class = "btn-primary")
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
      dplyr::filter(survey_list_master(), external)
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
          dplyr::filter(countryname == country) %>%
          dplyr::pull(year) %>% sort()
      })
      names(yrs) <- input$country
      yrs
    })
    
    # --------- Render year selectors -----------
    output$survey_year_ui <- renderUI({
      req(input$country)
      yrs <- available_years()
      tagList(
        lapply(input$country, function(country_name) {
          selectizeInput(
            inputId = ns(paste0("survey_year_", gsub(" ", "_", country_name))),
            label = paste("Survey years for", country_name),
            choices = yrs[[country_name]],
            selected = yrs[[country_name]],
            multiple = TRUE,
            options = list(placeholder = paste("Select years for", country_name))
          )
        })
      )
    })
    
    # --------- Reactive list of pin IDs to download -----------
    survey_data_files <- reactive({
      req(input$country)
      all_files <- character(0)
      
      for (country in input$country) {
        year_input_id <- paste0("survey_year_", gsub(" ", "_", country))
        selected_years <- input[[year_input_id]]
        
        if (!is.null(selected_years) && length(selected_years) > 0) {
          country_files <- surveys() %>%
            dplyr::filter(countryname == country, year %in% selected_years) %>%
            dplyr::pull(wiseapp_pin)
          all_files <- c(all_files, country_files)
        }
      }
      
      req(pin_prefix())
      paste0(pin_prefix(), all_files)
    })
    
    # --------- ReactiveVal to hold loaded data (set by observeEvent below) -----------
    survey_data_r <- reactiveVal(NULL)
    
    # --------- Download and read the selected survey files WHEN the user clicks Load Data -----------
    observeEvent(input$load_data, {
      # Debug log for button click
      message("DEBUG: Load Data button pressed; input$load_data = ", input$load_data)
      
      files <- isolate(survey_data_files())
      if (length(files) == 0) {
        showNotification("No survey files selected. Please choose country and years first.", type = "warning")
        survey_data_r(tibble::tibble())  # indicate a load attempt but empty
        return(invisible())
      }
      
      if (is.null(board())) {
        showNotification("No board configured. Cannot download files.", type = "error")
        survey_data_r(tibble::tibble())
        return(invisible())
      }
      
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
    
## To be copied in the UI
# mod_1_01_sample_ui("1_01_sample_1")
    
## To be copied in the server
# mod_1_01_sample_server("1_01_sample_1")
