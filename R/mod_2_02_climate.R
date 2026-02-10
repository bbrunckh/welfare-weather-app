mod_2_02_climate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("TBD: climate scenario controls"),
    shiny::actionButton(ns("run_climate"), "Run climate simulation", class = "btn-secondary")
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

