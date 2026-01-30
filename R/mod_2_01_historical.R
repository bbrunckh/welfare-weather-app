mod_2_01_historical_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("TBD: Historical weather simulation controls"),
    shiny::actionButton(ns("run_sim"), "Run simulation", class = "btn-primary")
  )
}

mod_2_01_historical_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$run_sim, {
      showNotification("TBD: simulation not implemented yet", type = "message")
    })
  })
}
