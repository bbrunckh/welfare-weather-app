#' 1_01_sample UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr filter pull mutate inner_join select arrange
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
    #
    # Returns a character vector of filenames (basename only, no path) that
    # exist at the root of the connected data source.
    # For local: uses list.files(). For remote: uses duckdbfs or falls back to
    # attempting open_dataset() per file — not feasible at scale, so for remote
    # sources we trust survey_list and skip the existence filter.

    available_files <- reactive({
      req(connection_params())
      p <- connection_params()

      if (identical(p$type, "local")) {
        if (!dir.exists(p$path)) return(character(0))
        list.files(p$path, recursive = FALSE)
      } else {
        # For remote sources we cannot cheaply list files without S3 ListObjects
        # or equivalent. Return NULL to signal "skip existence filter".
        NULL
      }
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

    # ---- Surveys with data files available at the endpoint ------------------
    #
    # Constructs expected filenames from survey_list metadata, filters to the
    # selected unit level, then cross-checks against available_files().
    # For remote connections where available_files() is NULL, skips the
    # existence check and trusts survey_list.

    surveys <- reactive({
      req(input$unit, survey_list())
      sl    <- survey_list()
      files <- available_files()  # NULL for remote, character vector for local

      sl <- sl |>
        dplyr::mutate(
          fname = paste0(code, "_", year, "_", survname, "_", input$unit, ".parquet"),
          fpath = if (identical(connection_params()$type, "local")) {
            file.path(connection_params()$path, fname)
          } else {
            fname  # bare filename for remote — not used in existence check
          }
        ) |>
        dplyr::filter(level == input$unit)

      # For local connections, filter to files that actually exist on disk
      if (!is.null(files)) {
        sl <- sl |> dplyr::filter(fname %in% files)
      }

      sl
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

      choices <- setNames(sv$code, sv$economy)
      choices <- choices[!duplicated(choices)]  # one entry per country

      selectizeInput(
        inputId  = ns("economy"),
        label    = "Economy",
        choices  = choices,
        selected = choices[1],
        multiple = TRUE,
        options  = list(maxItems = 2, placeholder = "Select up to 2 economies")
      )
    })

    # ---- Available survey years per selected economy ------------------------

    available_years <- reactive({
      req(input$economy, surveys())
      lapply(stats::setNames(input$economy, input$economy), function(code) {
        surveys() |>
          dplyr::filter(code == !!code) |>
          dplyr::pull(year) |>
          sort()
      })
    })

    # ---- Year selector UI ---------------------------------------------------

    output$survey_year_ui <- renderUI({
      req(input$economy, available_years())
      codes    <- input$economy
      all_yrs  <- available_years()

      year_inputs <- lapply(codes, function(code) {
        yrs <- all_yrs[[code]]
        economy_name <- surveys() |>
          dplyr::filter(code == !!code) |>
          dplyr::pull(economy) |>
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

      combos <- do.call(rbind, lapply(names(years_by_code), function(code) {
        yrs <- years_by_code[[code]]
        if (length(yrs) == 0) return(NULL)
        data.frame(code = code, year = as.numeric(yrs), stringsAsFactors = FALSE)
      }))

      surveys() |>
        dplyr::inner_join(combos, by = c("code", "year"))
    })

    # ---- Return API ---------------------------------------------------------

    list(
      selected_surveys = selected_surveys
    )
  })
}