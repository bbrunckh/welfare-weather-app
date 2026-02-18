#' 1_modelling UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS
#' @importFrom bsplus bs_accordion bs_append
#' @importFrom waiter autoWaiter spin_2 transparent
#'
mod_1_modelling_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    autoWaiter(html = spin_2(), color = transparent(.5)),
    h4("How much does weather affect welfare? Who is most affected?"),

    sidebarLayout(

      sidebarPanel(
        bs_accordion(id = ns("accordion")) |>
          bs_append(
            title = "1 Sample",
            content = mod_1_01_sample_ui(ns("sample"))
          ) |>
          bs_append(
            title = "2 Outcome",
            content = tagList(
              mod_1_02_outcome_ui(ns("outcome")),
              mod_1_03_surveystats_ui(ns("surveystats"))
            )
          ) |>
          bs_append(
            title = "3 Weather",
            content = tagList(
              mod_1_04_weather_ui(ns("weather")),
              mod_1_05_weatherstats_ui(ns("weatherstats"))
            )
          ) |>
          bs_append(
            title = "4 Model",
            content = mod_1_06_model_ui(ns("model"))
          )
      ),

      mainPanel(
        tabsetPanel(
          id = ns("step1_output_tabs"),
          tabPanel(
            title = "Overview",
            value = "overview",
            p("Outputs will appear here after you load data and click the relevant buttons in the sidebar."),
            includeMarkdown(system.file("app/www/equation.md", package = "wiseapp"))
          )
        )

      )
    )
  )
}

#' 1_modelling Server Functions
#'
#' @noRd
mod_1_modelling_server <- function(id, survey_list_master, varlist, pov_lines, cpi_ppp, data_dir) { #pin_prefix, board, survey_metadata, varlist, weather_list, pov_lines) {
  moduleServer(id, function(input, output, session) {

    # Pass reactives to sub-modules and get their APIs (return values)
    mod_1_01_sample_api <- mod_1_01_sample_server(
      "sample",
      survey_list_master = survey_list_master,
      varlist = varlist,
      data_dir = data_dir
    )

    mod_1_02_outcome_api <- mod_1_02_outcome_server(
      "outcome",
      varlist = varlist,
      survey_data = mod_1_01_sample_api$survey_data
    )

    # Survey stats module (adds a tab to the main panel after data load + button click)
    mod_1_03_surveystats_server(
      "surveystats",
      varlist = varlist,
      cpi_ppp = cpi_ppp,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_02_outcome_api$selected_outcome,
      survey_data = mod_1_01_sample_api$survey_data,
      survey_geo = mod_1_01_sample_api$survey_geo,
      tabset_id = "step1_output_tabs",
      tabset_session = session
    )

    # Weather selection and configuration module
    mod_1_04_weather_api <- mod_1_04_weather_server(
      "weather",
      varlist = varlist,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      survey_data = mod_1_01_sample_api$survey_data,
      survey_h3 = mod_1_01_sample_api$survey_h3
    )

    # Weather stats module (adds a tab after weather selection + button click)
    mod_1_05_weatherstats_server(
      "weatherstats",
      varlist = varlist,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_02_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      survey_weather = mod_1_04_weather_api$survey_weather,
      survey_geo = mod_1_01_sample_api$survey_geo,
      tabset_id = "step1_output_tabs",
      tabset_session = session
    )

    mod_1_model <- mod_1_06_model_server(
      "model",
      varlist = varlist,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_02_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      survey_weather = mod_1_04_weather_api$survey_weather
    )

    mod_1_07_results_server(
      "results",
      varlist = varlist,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_02_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      # selected_model = mod_1_model$selected_model,
      model_fit = mod_1_model$model_fit,
      tabset_id = "step1_output_tabs",
      tabset_session = session
    )

    mod_1_08_modelfit_server(
      "modelfit",
      varlist = varlist,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_02_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      # selected_model = mod_1_model$selected_model,
      model_fit = mod_1_model$model_fit,
      tabset_id = "step1_output_tabs",
      tabset_session = session
    )

    # Convenience reactive: "final model" (your convention is [[3]])
    final_model <- reactive({
      fits <- mod_1_model$model_fit()
      if (is.null(fits) || !length(fits)) return(NULL)
      fits[[3]] %||% fits[[length(fits)]]
    })

    # NEW: export Step 1 API for Step 2 / Step 3
    list(
      # nested (optional but nice)
      sample_api  = mod_1_01_sample_api,
      outcome_api = mod_1_02_outcome_api,
      weather_api = mod_1_04_weather_api,
      model_api   = mod_1_model,

      # flattened (what downstream modules will actually use)
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_02_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      # selected_model = mod_1_model$selected_model,

      survey_data       = mod_1_01_sample_api$survey_data,
      survey_h3         = mod_1_01_sample_api$survey_h3,
      survey_geo       = mod_1_01_sample_api$survey_geo,
      survey_weather    = mod_1_04_weather_api$survey_weather,

      model_fit         = mod_1_model$model_fit,
      final_model       = final_model
    )
  })
}
