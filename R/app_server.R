#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  runtime <- load_runtime_data()

  # Provide runtime data as simple reactives
  survey_list_master_r <- reactive({ runtime$survey_list_master })
  pin_prefix_r         <- reactive({ runtime$pin_prefix })
  board_r              <- reactive({ runtime$board })
  survey_metadata_r    <- reactive({ runtime$survey_metadata })
  varlist_r            <- reactive({ runtime$varlist })
  weather_list_r       <- reactive({ runtime$weather_list })
  pov_lines_r <- reactive({runtime$pov_lines})

  # Now call the step1 module and pass reactives (Now With return value so I can use in STEP 2)
  step1_api <- mod_1_modelling_server(
    id = "step1",
    survey_list_master = survey_list_master_r,
    pin_prefix = pin_prefix_r,
    board = board_r,
    survey_metadata = survey_metadata_r,
    varlist = varlist_r,
    weather_list = weather_list_r,
    pov_lines = pov_lines_r
  )

  #Step 2 (placeholder)
  mod_2_simulation_server(
    id = "step2",
    step1 = step1_api,
    pov_lines = pov_lines_r,
    varlist = varlist_r
    )
}
