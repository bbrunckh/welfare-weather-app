mod_2_simulation_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    waiter::autoWaiter(html = waiter::spin_2(), color = waiter::transparent(.5)),
    h4("What welfare is expected given historical weather conditions? In future climate scenarios?"),

    sidebarLayout(
      sidebarPanel(
        bsplus::bs_accordion(id = ns("accordion")) |>
          bsplus::bs_append(
            title = "1 Historical weather",
            content = mod_2_01_historical_ui(ns("historical"))
          ) |>
          bsplus::bs_append(
            title = "2 Climate change",
            content = mod_2_02_climate_ui(ns("climate"))
          )
      ),

      mainPanel(
        tabsetPanel(
          id = ns("step2_output_tabs"),
          tabPanel(
            title = "Overview",
            value = "overview",
            p("Step 2 is under development. Outputs will appear here."),
            tags$hr(),
            strong("Status:"),
            textOutput(ns("step2_status"))
          )
        )
      )
    )
  )
}

mod_2_simulation_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Stub submodules (no dependencies yet)
    mod_2_01_historical_server("historical")
    mod_2_02_climate_server("climate")

    # Simple “is this page alive?” output
    output$step2_status <- renderText({
      paste("Loaded OK at", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
    })

    # Later, this server will accept Step 1 exports as arguments.
  })
}
