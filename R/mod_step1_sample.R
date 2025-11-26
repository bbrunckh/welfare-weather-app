# UI function
mod_step1_sample_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # container for the country selector and the (per-country) year selectors
    wellPanel(
      uiOutput(ns("sample_ui")),
      uiOutput(ns("survey_year_ui"))
    )
  )
}

# Server function
mod_step1_sample_server <- function(id, survey_list_master, pin_prefix = "", board = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # surveys reactiveVal initialized from provided master list
    surveys <- reactiveVal({dplyr::filter(survey_list_master, external)})

    # --------- sample UI: country selector -----------
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

    # compute available years for the selected countries
    available_years <- reactive({
      req(input$country)
      selected_countries <- input$country
      years_list <- list()
      for (country in selected_countries) {
        years_list[[country]] <- surveys() %>%
          dplyr::filter(countryname == country) %>%
          dplyr::pull(year) %>% sort()
      }
      years_list
    })

    # render per-country year selector(s)
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

    # reactive giving paths/pin ids to download (uses namespaced inputs)
    survey_data_files <- reactive({
      req(input$country)
      selected_countries <- input$country
      survey_files <- character(0)
      for (country in selected_countries) {
        year_input_id <- paste0("survey_year_", gsub(" ", "_", country))
        selected_years <- input[[year_input_id]]
        if (!is.null(selected_years) && length(selected_years) > 0) {
          country_data <- surveys() %>%
            dplyr::filter(countryname == country & year %in% selected_years) %>%
            dplyr::pull(wiseapp_pin)
          survey_files <- c(survey_files, country_data)
        }
      }
      if (pin_prefix == "") survey_files else paste0(pin_prefix, survey_files)
    })

    # reactive that downloads / reads the selected survey files on demand
    survey_data <- reactive({
      files <- survey_data_files()
      req(length(files) > 0)
      if (is.null(board)) {
        # If no board provided, return a helpful message/empty tibble
        warning("No board provided to module; returning empty tibble")
        return(tibble::tibble())
      }
      # Try downloading each pin; ignore those that fail
      paths <- lapply(files, function(pin_id) {
        tryCatch(
          pins::pin_download(board, pin_id),
          error = function(e) { warning("download failed for ", pin_id); NULL }
        )
      })
      paths <- Filter(Negate(is.null), paths)
      if (length(paths) == 0) return(tibble::tibble())

      # read (using provided helper read_parquet_duckdb in your project)
      df <- read_parquet_duckdb(unlist(paths), options = list(union_by_name = TRUE), prudence = "lavish")
      df
    })

    # return a small API so caller can access the data
    list(
      surveys = surveys,
      survey_data_files = survey_data_files,
      survey_data = survey_data
    )
  })
}