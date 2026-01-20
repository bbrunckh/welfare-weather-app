#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  runtime <- load_runtime_data()
  
  # correct reactiveValues creation
  rv <- reactiveValues(
    board = runtime$board,
    pin_prefix = runtime$pin_prefix,
    varlist = runtime$varlist,
    weather_list = runtime$weather_list,
    # welfare = runtime$welfare,
    pov_lines = runtime$pov_lines,
    survey_list_master = runtime$survey_list_master,
    survey_metadata = runtime$survey_metadata
  )
  
  # CREATE THESE BEFORE CALLING THE MODULE
  survey_list_master_r <- reactive({ rv$survey_list_master })
  pin_prefix_r         <- reactive({ rv$pin_prefix })
  board_r              <- reactive({ rv$board })
  survey_metadata_r    <- reactive({ rv$survey_metadata })
  varlist_r            <- reactive({ rv$varlist })
  
  # Now call the step1 module and pass reactives
  mod_1_modelling_server(
    id = "step1",
    survey_list_master = survey_list_master_r,
    pin_prefix = pin_prefix_r,
    board = board_r,
    survey_metadata = survey_metadata_r,
    varlist = varlist_r
  )
}
