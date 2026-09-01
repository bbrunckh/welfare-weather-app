#' 1_modelling UI Function
#'
#' @description A shiny Module. Orchestrates the Step 1 modelling pipeline:
#'   sample selection, survey stats, outcome, weather, model, and results.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_modelling_ui <- function(id) {
  ns <- NS(id)

  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 360,
      bslib::accordion(
        id       = ns("accordion"),
        multiple = FALSE,
        open     = "Sample",
        bslib::accordion_panel(
          title = "Sample",
          value = "Sample",
          icon  = tags$span("1", class = "step-badge"),
          mod_1_01_sample_ui(ns("sample")),
          mod_1_02_surveystats_ui(ns("surveystats"))
        ),
        bslib::accordion_panel(
          title = "Outcome",
          value = "Outcome",
          icon  = tags$span("2", class = "step-badge"),
          mod_1_03_outcome_ui(ns("outcome"))
        ),
        bslib::accordion_panel(
          title = "Weather",
          value = "Weather",
          icon  = tags$span("3", class = "step-badge"),
          mod_1_04_weather_ui(ns("weather")),
          mod_1_05_weatherstats_ui(ns("weatherstats"))
        ),
        bslib::accordion_panel(
          title = "Model",
          value = "Model",
          icon  = tags$span("4", class = "step-badge"),
          mod_1_06_model_ui(ns("model")),
          mod_1_07_results_ui(ns("results"))
        )
      )
    ),
    h4("How much does weather affect welfare? Who is most affected?",
       class = "step-question"),
    tabsetPanel(
      id = ns("step1_output_tabs"),
      tabPanel(
        title = "Overview",
        value = "overview",
        div(
          class = "empty-state",
          icon("chart-line"),
          h5("No results yet"),
          p(paste(
            "Work through the sidebar: choose your sample, define the outcome,",
            "configure weather variables, then run the model.",
            "Outputs: regression results with coefficient plots, weather-sensitivity",
            "curves, and model-fit diagnostics will appear here as new tabs."
          ))
        ),
        welfare_equation_ui()
      )
    )
  )
}

#' 1_modelling Server Functions
#'
#' Orchestrates sub-modules 01-08. All pure logic is delegated to
#' `fct_modelling.R`. Returns a flat API list consumed by Step 2 / Step 3.
#'
#' @param id              Module id.
#' @param connection_params Reactive named list from mod_0_overview.
#' @param survey_list     Reactive tibble of survey metadata.
#' @param variable_list   Reactive tibble of variable metadata.
#' @param cpi_ppp         Reactive tibble of CPI/PPP conversion factors.
#' @param pov_lines       Reactive tibble of 2021 PPP poverty lines.
#'
#' @noRd
mod_1_modelling_server <- function(id,
                                    connection_params,
                                    survey_list,
                                    variable_list,
                                    cpi_ppp,
                                    pov_lines) {
  moduleServer(id, function(input, output, session) {

    # ---- 1. Sample ----------------------------------------------------------

    s1 <- mod_1_01_sample_server(
      "sample",
      connection_params = connection_params,
      survey_list       = survey_list,
      variable_list     = variable_list
    )

    # ---- 2. Survey stats ----------------------------------------------------

    s2 <- mod_1_02_surveystats_server(
      "surveystats",
      connection_params = connection_params,
      variable_list     = variable_list,
      cpi_ppp           = cpi_ppp,
      selected_surveys  = s1$selected_surveys,
      selected_outcome  = NULL,
      tabset_id         = "step1_output_tabs",
      tabset_session    = session,
      analysis_unit     = s1$analysis_unit
    )

    # ---- 3. Outcome ---------------------------------------------------------

    s3 <- mod_1_03_outcome_server(
      "outcome",
      variable_list  = variable_list,
      survey_data    = s2$survey_data,
      map_data       = s2$map_data,
      cell_data      = s2$cell_data,
      tabset_id      = "step1_output_tabs",
      tabset_session = session
    )

    # ---- 4. Weather ---------------------------------------------------------

    s4 <- mod_1_04_weather_server(
      "weather",
      variable_list    = variable_list,
      selected_surveys = s1$selected_surveys,
      survey_data      = s2$survey_data
    )

    # ---- 5. Weather stats ---------------------------------------------------

    s5 <- mod_1_05_weatherstats_server(
      "weatherstats",
      connection_params = connection_params,
      variable_list     = variable_list,
      selected_surveys  = s1$selected_surveys,
      selected_outcome  = s3$selected_outcome,
      selected_weather  = s4$selected_weather,
      hist_years        = s4$hist_years,
      survey_data       = s2$survey_data,
      map_data          = s2$map_data,
      cell_data         = s2$cell_data,
      tabset_id         = "step1_output_tabs",
      tabset_session    = session
    )

    # ---- 6. Model -----------------------------------------------------------

    s6 <- mod_1_06_model_server(
      "model",
      variable_list    = variable_list,
      selected_surveys = s1$selected_surveys,
      analysis_unit    = s1$analysis_unit,
      selected_outcome = s3$selected_outcome,
      selected_weather = s4$selected_weather,
      survey_weather   = s5$survey_weather
    )

    # ---- 7. Results ---------------------------------------------------------

    s7 <- mod_1_07_results_server(
      "results",
      variable_list    = variable_list,
      selected_surveys = s1$selected_surveys,
      selected_outcome = s3$selected_outcome,
      selected_weather = s4$selected_weather,
      survey_weather   = s5$survey_weather,
      selected_model   = s6$selected_model,
      run_model        = s6$run_model,
      tabset_id        = "step1_output_tabs",
      tabset_session   = session
    )

    # ---- 8. Model fit diagnostics -------------------------------------------

    mod_1_08_modelfit_server(
      "modelfit",
      variable_list    = variable_list,
      selected_outcome = s3$selected_outcome,
      model_fit        = s7$model_fit,
      tabset_id        = "step1_output_tabs",
      survey_weather   = s5$survey_weather,
      tabset_session   = session
    )

    # ---- Return API ---------------------------------------------------------

    list(
      # Selections
      selected_surveys  = s1$selected_surveys,
      analysis_unit     = s1$analysis_unit,
      selected_outcome  = s3$selected_outcome,
      selected_weather  = s4$selected_weather,
      selected_model    = s6$selected_model,
      selected_policies = s6$selected_policies,

      # Data
      survey_data    = s2$survey_data,
      survey_weather = s5$survey_weather,
      model_fit      = s7$model_fit,
      stored_breaks  = s5$stored_breaks
    )
  })
}