#' 1_06_model UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_06_model_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 1_06_model Server Functions
#'
#' @noRd 
mod_1_06_model_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_1_06_model_ui("1_06_model_1")
    
## To be copied in the server
# mod_1_06_model_server("1_06_model_1")
