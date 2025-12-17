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
          )
      ),
      
      mainPanel(
        
        includeMarkdown(
          system.file("app/www/equation.md", package = "wiseapp")
        )
        
      )
    )
  )
}
    
#' 1_modelling Server Functions
#'
#' @noRd 
mod_1_modelling_server <- function(id, survey_list_master, pin_prefix, board) {
  moduleServer(id, function(input, output, session) {
    
    # Pass reactives
    mod_1_01_sample_api <- mod_1_01_sample_server(
      "sample",
      survey_list_master = survey_list_master,
      pin_prefix = pin_prefix,
      board = board
    )
    
    # React when data is loaded
    observeEvent(mod_1_01_sample_api$data_loaded(), {
      if (mod_1_01_sample_api$data_loaded()) {
        df <- mod_1_01_sample_api$survey_data()
        # do something with df
        message("Parent saw data_loaded; rows = ", nrow(df))
      }
    })
    
  })
}
