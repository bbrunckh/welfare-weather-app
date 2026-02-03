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

mod_2_simulation_server <- function(id, step1 = NULL, pov_lines = NULL, varlist = NULL) {
  moduleServer(id, function(input, output, session) {

    # Stub submodules (no dependencies yet)
    mod_2_01_historical_server("historical")
    mod_2_02_climate_server("climate")

    output$step2_status <- renderText({
      if (is.null(step1)) return("Step 1 API not provided (wiring issue).")

      has_model <- !is.null(step1$final_model())  # reactive
      n_sw <- tryCatch(nrow(step1$survey_weather()), error = function(e) NA_integer_)
      n_haz <- tryCatch(length(step1$haz_vars()), error = function(e) NA_integer_)

      paste0(
        "Step 1 linked: YES | ",
        "Model available: ", if (has_model) "YES" else "NO", " | ",
        "survey_weather rows: ", n_sw, " | ",
        "haz vars: ", n_haz
      )
    })


    # Later, this server will accept Step 1 exports as arguments.
  })
}
