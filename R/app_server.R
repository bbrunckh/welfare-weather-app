#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  overview_api <- mod_0_overview_server("overview")  # returns list(data_dir = reactive(...))
  data_dir_r <- overview_api$folder_path

  runtime_r <- reactive({
    req(data_dir_r())
    data_dir <- data_dir_r()
    load_local_data(data_root = data_dir)  # add data_root arg in app_config.R
  })

  # Reactive wrappers for data and config
  survey_list_master_r <- reactive(runtime_r()$survey_list_master)
  varlist_r <- reactive(runtime_r()$varlist)
  cpi_ppp_r <- reactive(runtime_r()$cpi_ppp)
  pov_lines_r <- reactive(runtime_r()$pov_lines)

  # Now call the step1 module and pass reactives (Now With return value so I can use in STEP 2)
  step1_api <- mod_1_modelling_server(
    id = "step1",
    survey_list_master = survey_list_master_r,
    # pin_prefix = pin_prefix_r,
    # board = board_r,
    # survey_metadata = survey_metadata_r,
    varlist = varlist_r,
    pov_lines = pov_lines_r,
    cpi_ppp = cpi_ppp_r,
    data_dir = data_dir_r()
    # weather_list = weather_list_r
  )

  #Step 2 (placeholder)
  mod_2_simulation_server(
    id = "step2",
    step1 = step1_api,
    pov_lines = pov_lines_r,
    varlist = varlist_r,
    # board = board_r
    )
}
