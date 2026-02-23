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
mod_1_modelling_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    autoWaiter(html = spin_2(), color = transparent(.5)),
    h4("How much does weather affect welfare? Who is most affected?"),

    sidebarLayout(

      sidebarPanel(
        bs_accordion(id = ns("accordion")) |>
          bs_append(
            title   = "1 Sample",
            content = tagList(
              mod_1_01_sample_ui(ns("sample")),
              mod_1_02_surveystats_ui(ns("surveystats"))
            )
          ) |>
          bs_append(
            title   = "2 Outcome",
            content = tagList(
              mod_1_03_outcome_ui(ns("outcome"))
            )
          ) |>
          bs_append(
            title   = "3 Weather",
            content = tagList(
              mod_1_04_weather_ui(ns("weather")),
              mod_1_05_weatherstats_ui(ns("weatherstats"))
            )
          ) |>
          bs_append(
            title   = "4 Model",
            content = tagList(
              mod_1_06_model_ui(ns("model")),
              mod_1_07_results_ui(ns("results"))
            )
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
#' @param id Module id.
#' @param connection_params Reactive named list from mod_0_overview.
#' @param survey_list Reactive tibble of survey metadata from mod_0_overview.
#' @param variable_list Reactive tibble of variable metadata from mod_0_overview.
#' @param cpi_ppp Reactive tibble of CPI/PPP conversion factors from mod_0_overview.
#' @param pov_lines Reactive tibble of 2021 PPP poverty lines from mod_0_overview.
#'
#' @noRd
mod_1_modelling_server <- function(
    id,
    connection_params,
    survey_list,
    variable_list,
    cpi_ppp,
    pov_lines
) {
  moduleServer(id, function(input, output, session) {

    # ---- 1. Sample selection ------------------------------------------------

    mod_1_01_sample_api <- mod_1_01_sample_server(
      "sample",
      connection_params = connection_params,
      survey_list       = survey_list,
      variable_list     = variable_list
    )

    # ---- 2. Survey stats (adds tab after button click) -----------------

    mod_1_02_surveystats_api <- mod_1_02_surveystats_server(
      "surveystats",
      connection_params = connection_params,
      variable_list    = variable_list,
      cpi_ppp          = cpi_ppp,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = NULL,
      tabset_id        = "step1_output_tabs",
      tabset_session   = session
    )

    # ---- 3. Outcome selection -----------------------------------------------

    mod_1_03_outcome_api <- mod_1_03_outcome_server(
      "outcome",
      variable_list = variable_list,
      survey_data   = mod_1_02_surveystats_api$survey_data
    )

    # ---- 4. Weather selection and configuration -----------------------------

    mod_1_04_weather_api <- mod_1_04_weather_server(
      "weather",
      variable_list    = variable_list,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      survey_data      = mod_1_02_surveystats_api$survey_data
    )

    # ---- 5. Weather stats (adds tab after button click) ---------------------

    mod_1_05_weatherstats_api <- mod_1_05_weatherstats_server(
      "weatherstats",
      connection_params = connection_params,
      variable_list    = variable_list,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_03_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      survey_data      = mod_1_02_surveystats_api$survey_data,
      tabset_id        = "step1_output_tabs",
      tabset_session   = session
    )

    # ---- 6. Model specification ---------------------------------------------

    mod_1_06_model_api <- mod_1_06_model_server(
      "model",
      variable_list    = variable_list,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_03_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      survey_weather   = mod_1_05_weatherstats_api$survey_weather
    )

    # ---- 7. Results (adds tab after model run) ------------------------------

    mod_1_07_results_api <- mod_1_07_results_server(
      "results",
      variable_list    = variable_list,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_03_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      survey_weather   = mod_1_05_weatherstats_api$survey_weather,
      selected_model   = mod_1_06_model_api$selected_model,
      tabset_id        = "step1_output_tabs",
      tabset_session   = session
    )

    # ---- 8. Model fit diagnostics (adds tab alongside results) --------------

    mod_1_08_modelfit_server(
      "modelfit",
      variable_list    = variable_list,
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_03_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      model_fit        = mod_1_07_results_api$model_fit,
      tabset_id        = "step1_output_tabs",
      tabset_session   = session
    )

    # ---- Return API ---------------------------------------------------------

    list(
      # Nested APIs (for direct access if needed)
      sample_api      = mod_1_01_sample_api,
      surveystats_api = mod_1_02_surveystats_api,
      outcome_api     = mod_1_03_outcome_api,
      weather_api     = mod_1_04_weather_api,
      model_api       = mod_1_06_model_api,
      results_api     = mod_1_07_results_api,

      # Flattened selections (what Step 2 / Step 3 will use)
      selected_surveys = mod_1_01_sample_api$selected_surveys,
      selected_outcome = mod_1_03_outcome_api$selected_outcome,
      selected_weather = mod_1_04_weather_api$selected_weather,
      selected_model   = mod_1_06_model_api$selected_model,

      # Data objects
      survey_data    = mod_1_02_surveystats_api$survey_data,
      survey_h3      = mod_1_02_surveystats_api$survey_h3,
      survey_geo     = mod_1_02_surveystats_api$survey_geo,
      survey_weather = mod_1_05_weatherstats_api$survey_weather,  # fixed: was mod_1_04
      model_fit      = mod_1_07_results_api$model_fit
    )
  })
}