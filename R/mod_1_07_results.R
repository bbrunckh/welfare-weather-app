#' 1_07_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_07_results_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 1_07_results Server Functions
#'
#' @noRd 
mod_1_07_results_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_1_07_results_ui("1_07_results_1")
    
## To be copied in the server
# mod_1_07_results_server("1_07_results_1")
