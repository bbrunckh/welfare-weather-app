mod_2_02_climate_ui <- function(id) {
  ns <- NS(id)
  content = tagList(
              h4("Yet to be implemented", 
                  style = "color: red; font-size: 12px;"),
              radioButtons("climate", 
                            "Scenario", 
                            choices = c("SSP2-4.5","SSP3-7.0","SSP5-8.5"),
                            selected = "SSP5-8.5"
              ),
              radioButtons("climatemethod", 
                            "Method", 
                            choices = c("Delta"),
                            selected = "Delta"
              ),
              helpText("Adds CMIP6 ensemble 'delta' fields to historical observations.", 
                        style = "font-size: 12px;"),
              sliderInput("yearRange_climate", 
                          "Period defining the distribution of weather in climate change scenario", 
                          min = 2015, 
                          max = 2100, 
                          value = c(2040, 2060),
                          sep = ""
              ),
              hr(),
              actionButton("run_climate_sim", "Run climate simulation",
                            style = "width: 100%;")
            )
}

mod_2_02_climate_server <- function(id, step1 = NULL, pov_lines = NULL, varlist = NULL, board = NULL) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run_climate, {
      if (is.null(step1)) {
        showNotification("Step 1 API missing (wiring).", type = "error")
        return()
      }
      showNotification("Wired OK (climate stub).", type = "message")
    })
  })
}

