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
mod_1_modelling_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    waiter::autoWaiter(html = waiter::spin_2(), color = waiter::transparent(.5)),
    h4("How much does weather affect welfare? Who is most affected?"),
    
    sidebarLayout(
      
      sidebarPanel(
        bs_accordion(id = ns("accordion")) |>
          bs_append(
            title = "1 Sample",
            content = mod_1_01_sample_ui(ns("sample"))
          ) |>
          bs_append(
            title = "2 Outcome Variable",
            content = tagList(
              mod_1_02_outcome_ui(ns("outcome")),
              mod_1_03_surveystats_ui(ns("surveystats"))
            )
          )
      ),
      
      mainPanel(
        tabsetPanel(id = ns("step1_output_tabs")),
        includeMarkdown(system.file("app/www/equation.md", package = "wiseapp"))
        
      )
    )
  )
}
    
#' 1_modelling Server Functions
#'
#' @noRd 
mod_1_modelling_server <- function(id, survey_list_master, pin_prefix, board, survey_metadata, varlist) {
  moduleServer(id, function(input, output, session) {
    
    # Pass reactives
    mod_1_01_sample_api <- mod_1_01_sample_server(
      "sample",
      survey_list_master = survey_list_master,
      pin_prefix = pin_prefix,
      board = board,
      survey_metadata = survey_metadata,
      varlist = varlist
    )

    mod_1_02_outcome_api <- mod_1_02_outcome_server(
      "outcome",
      survey_metadata = survey_metadata,
      varlist = varlist,
      country = mod_1_01_sample_api$selected_countries,
      survey_year = mod_1_01_sample_api$selected_years
    )

    # Survey stats module (adds a tab to the main panel after data load + button click)
    mod_1_03_surveystats_server(
      "surveystats",
      survey_data = mod_1_01_sample_api$survey_data,
      data_loaded = mod_1_01_sample_api$data_loaded,
      selected_outcome = mod_1_02_outcome_api$selected_outcome,
      tabset_id = session$ns("step1_output_tabs"),
      varlist = varlist,
      pov_lines = reactive({ golem::get_golem_options("pov_lines") }),
      survey_geo = mod_1_01_sample_api$survey_geo
    )
    
    
  })
}
