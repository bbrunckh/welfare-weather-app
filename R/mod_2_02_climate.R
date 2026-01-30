mod_2_02_climate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("TBD: climate scenario controls"),
    shiny::actionButton(ns("run_climate"), "Run climate simulation", class = "btn-secondary")
  )
}

mod_2_02_climate_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run_climate, {
      showNotification("TBD: climate simulation not implemented yet", type = "message")
    })
  })
}
