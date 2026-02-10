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
          ),
          tabPanel(
            title = "Diagnostics",
            value = "diagnostics",

            h4("Phase D prediction diagnostics"),
            shiny::checkboxInput(ns("historical-show_pred_diag"), "Show prediction diagnostics", TRUE),

            shiny::conditionalPanel(
              condition = paste0("input['", ns("historical-show_pred_diag"), "']"),

              tags$hr(),
              h5("NA predictions summary"),
              tableOutput(ns("historical-pred_na_rate_tbl")),

              h5("NA predictions by simulated year"),
              tableOutput(ns("historical-pred_na_by_year_tbl")),

              h5("NA predictions by interview month"),
              tableOutput(ns("historical-pred_na_by_month_tbl")),

              h5("Top locations by NA count"),
              tableOutput(ns("historical-pred_na_by_loc_tbl")),

              h5("Which predictors are missing among NA predictions?"),
              tableOutput(ns("historical-pred_na_drivers_tbl"))
            )
          )
        )
      )
    )
  )
}

mod_2_simulation_server <- function(id, step1 = NULL, pov_lines = NULL, varlist = NULL, board = NULL) {
  moduleServer(id, function(input, output, session) {

    # Submodules (wired, even if they don't use args yet)
    historical_api <- mod_2_01_historical_server(
      "historical",
      step1 = step1,
      pov_lines = pov_lines,
      varlist = varlist,
      board = board
    )

    mod_2_02_climate_server(
      "climate",
      step1 = step1,
      pov_lines = pov_lines,
      varlist = varlist,
      board = board
    )

    output$step2_status <- renderText({
      if (is.null(step1)) return("Step 1 API not provided (wiring issue).")

      # Phase B status
      lw_n <- tryCatch(nrow(historical_api$loc_weather_sim()), error = function(e) NA_integer_)
      lw_ok <- isTRUE(!is.na(lw_n) && lw_n > 0)

      #Phase C
      sp_n <- tryCatch({
        sp <- historical_api$sim_panel()
        if (is.null(sp)) NA_integer_ else nrow(sp)
      }, error = function(e) NA_integer_)
      sp_ok <- isTRUE(!is.na(sp_n) && sp_n > 0)
      sp_ok <- isTRUE(!is.na(sp_n) && sp_n > 0)

      #Output
      paste0(
        format_step2_status(step1 = step1, board = board),
        "\nPhase B hazards built: ", if (lw_ok) "YES" else "NO",
        " | loc_weather_sim rows: ", lw_n,
        "\nPhase C sim panel built: ", if (sp_ok) "YES" else "NO",
        " | sim_panel rows: ", sp_n
      )



    })

    # Later, this server will accept Step 1 exports as arguments.
  })
}
