#' 1_02_outcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_02_outcome_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 1_02_outcome Server Functions
#'
#' @noRd 
mod_1_02_outcome_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_1_02_outcome_ui("1_02_outcome_1")
    
## To be copied in the server
# mod_1_02_outcome_server("1_02_outcome_1")
