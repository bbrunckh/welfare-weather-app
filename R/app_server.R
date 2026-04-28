#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # ---- Step 0: data connection, config, and metadata loading ---------------

  overview_api <- mod_0_overview_server(id = "overview")

  # ---- Step 1: modelling ---------------------------------------------------
  # Pass reactives from overview_api

  step1_api <- mod_1_modelling_server(
    id                = "step1",
    connection_params = overview_api$connection_params,
    survey_list       = overview_api$survey_list,
    variable_list     = overview_api$variable_list,
    cpi_ppp           = overview_api$cpi_ppp,
    pov_lines         = overview_api$pov_lines
  )

  # ---- Step 2: simulation --------------------------------------------------
  # Pass selected Step 1 reactives

  step2_api <- mod_2_simulation_server(
    id                = "step2",
    connection_params = overview_api$connection_params,
    selected_outcome  = step1_api$selected_outcome,
    selected_weather  = step1_api$selected_weather,
    selected_surveys  = step1_api$selected_surveys,
    survey_weather    = step1_api$survey_weather,
    model_fit         = step1_api$model_fit,
    stored_breaks     = step1_api$stored_breaks
  )

  # ---- Step 3: policy scenarios --------------------------------------------
  # Pass selected Step 1 & 2 reactives
  mod_3_scenario_server(
    id                = "step3",
    connection_params = overview_api$connection_params,
    selected_outcome  = step1_api$selected_outcome,
    selected_weather  = step1_api$selected_weather,
    selected_model    = step1_api$selected_model,
    selected_policies = step1_api$selected_policies,
    survey_weather    = step1_api$survey_weather,
    model_fit         = step1_api$model_fit,
    hist_sim          = step2_api$hist_sim,
    saved_scenarios   = step2_api$saved_scenarios,
    selected_hist     = step2_api$selected_hist,
    variable_list     = overview_api$variable_list
  )
}