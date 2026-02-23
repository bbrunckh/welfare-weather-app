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


  # Pins board rooted at the selected data directory (deterministic Phase B reads/writes)
  # IMPORTANT: pins::board_local() intentionally stores in a system cache directory.
  # To store pins under your project data directory, use pins::board_folder().
  board_r <- reactive({
    req(data_dir_r())
    pins_dir <- file.path(data_dir_r(), "pins")
    dir.create(pins_dir, recursive = TRUE, showWarnings = FALSE)

    # Prefer board_folder() (folder-backed board). Fall back to legacy APIs if needed.
    if (exists("board_folder", where = asNamespace("pins"), inherits = FALSE)) {
      f <- pins::board_folder
      args <- names(formals(f))
      if ("path" %in% args) {
        f(path = pins_dir)
      } else if ("dir" %in% args) {
        f(dir = pins_dir)
      } else {
        f(pins_dir)  # positional fallback
      }
    } else if (exists("legacy_local", where = asNamespace("pins"), inherits = FALSE)) {
      pins::legacy_local(path = pins_dir)
    } else {
      # Last resort: may still use the system cache on some pins versions.
      pins::board_local(pins_dir)
    }
  })

  # Now call the step1 module and pass reactives (Now With return value so I can use in STEP 2)
  step1_api <- mod_1_modelling_server(
    id = "step1",
    survey_list_master = survey_list_master_r,
    varlist = varlist_r,
    pov_lines = pov_lines_r,
    cpi_ppp = cpi_ppp_r,
    data_dir = data_dir_r
  )

  #Step 2
  step2_api <- mod_2_simulation_server(
    id = "step2",
    step1 = step1_api,
    pov_lines = pov_lines_r,
    varlist = varlist_r,
    board = board_r()
    )
  #Step 3 (Policy scenarios)
  mod_3_scenario_server(
    id = "step3",
    step1 = step1_api,
    step2 = step2_api,
    pov_lines = pov_lines_r,
    varlist = varlist_r
  )
  #)
}
