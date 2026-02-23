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

  # ---- Step 2: simulation (placeholder) ------------------------------------

  mod_2_simulation_server(
    id    = "step2",
    step1 = step1_api
  )

  # ---- Step 3: scenario (placeholder) --------------------------------------

  mod_3_scenario_server(
    id    = "step3",
    step1 = step1_api
  )
  #)
}