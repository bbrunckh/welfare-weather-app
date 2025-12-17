#' 1_05_weatherstats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_05_weatherstats_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 1_05_weatherstats Server Functions
#'
#' @noRd 
mod_1_05_weatherstats_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_1_05_weatherstats_ui("1_05_weatherstats_1")
    
## To be copied in the server
# mod_1_05_weatherstats_server("1_05_weatherstats_1")
