#' 1_04_weather UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_1_04_weather_ui <- function(id) {
  ns <- NS(id)
  tagList(
 
  )
}
    
#' 1_04_weather Server Functions
#'
#' @noRd 
mod_1_04_weather_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_1_04_weather_ui("1_04_weather_1")
    
## To be copied in the server
# mod_1_04_weather_server("1_04_weather_1")
