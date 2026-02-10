mod_2_01_historical_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("TBD: Historical weather simulation controls"),
    shiny::actionButton(ns("run_sim"), "Run simulation", class = "btn-primary")
  )
}

mod_2_01_historical_server <- function(id, step1 = NULL, pov_lines = NULL, varlist = NULL, board = NULL) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$run_sim, {
      # Phase A: prove wiring is correct (cheap contract check)
      if (is.null(step1)) {
        showNotification("Step 1 API missing (wiring).", type = "error")
        return()
      }
      n_sw <- tryCatch(nrow(step1$survey_weather()), error = function(e) NA_integer_)
      showNotification(paste0("Wired OK. survey_weather rows = ", n_sw), type = "message")
    })
  })
}
