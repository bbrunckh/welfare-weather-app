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
          ),

          tabPanel(
            title = "Poverty",
            value = "poverty",
            h4("Phase E: poverty results"),
            tags$hr(),
            h5("Overall"),
            tableOutput(ns("pov_overall_tbl")),
            tags$hr(),
            h5("By simulated year"),
            tableOutput(ns("pov_by_year_tbl")),
            tags$hr(),
            plotOutput(ns("pov_by_year_plot"), height = "320px"),
            bslib::card(
              h4("Prediction + poverty-line diagnostics"),
              tableOutput(ns("pred_summary_tbl")),
              tags$hr(),
              tableOutput(ns("pline_diag_tbl"))
            ),
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

      lw_n <- tryCatch({
        lw <- historical_api$loc_weather_sim()
        if (is.null(lw)) NA_integer_ else nrow(lw)
      }, error = function(e) NA_integer_)
      lw_ok <- isTRUE(!is.na(lw_n) && lw_n > 0)

      sp_n <- tryCatch({
        sp <- historical_api$sim_panel()
        if (is.null(sp)) NA_integer_ else nrow(sp)
      }, error = function(e) NA_integer_)
      sp_ok <- isTRUE(!is.na(sp_n) && sp_n > 0)

      paste0(
        format_step2_status(step1 = step1, board = board),
        "\nPhase B hazards built: ", if (lw_ok) "YES" else "NO",
        " | loc_weather_sim rows: ", lw_n,
        "\nPhase C sim panel built: ", if (sp_ok) "YES" else "NO",
        " | sim_panel rows: ", sp_n
      )
    })
    output$pov_overall_tbl <- renderTable({
      res <- historical_api$poverty_results()

      validate(
        need(!is.null(res), "No poverty results yet. Run 'Compute poverty (Phase E)' in the Historical sidebar.")
      )

      res$overall
    }, rownames = FALSE)

    output$pov_by_year_tbl <- renderTable({
      res <- historical_api$poverty_results()

      validate(
        need(!is.null(res) && !is.null(res$by_year), "No by-year poverty results yet. Run Phase E first.")
      )

      res$by_year
    }, rownames = FALSE)

    output$pov_by_year_plot <- renderPlot({
      res <- historical_api$poverty_results()

      validate(
        need(!is.null(res) && !is.null(res$by_year) && nrow(res$by_year) > 0, "No by-year poverty results to plot yet.")
      )

      d <- res$by_year

      # lm(): expects pline column; glm_binomial(): may not have it
      if ("pline" %in% names(d)) {
        pl0 <- sort(unique(d$pline))[1]
        d <- dplyr::filter(d, .data$pline == pl0)
        ttl <- paste0("Poverty rate (", pl0, ")")
      } else {
        ttl <- "Poverty rate"
      }

      ggplot2::ggplot(d, ggplot2::aes(x = sim_year, y = pov_rate)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Simulated year", y = "Poverty rate", title = ttl)
    })


    output$pred_summary_tbl <- renderTable({
      res <- phase_e_results()
      if (is.null(res) || is.null(res$meta) || is.null(res$meta$pred_summary)) return(NULL)
      res$meta$pred_summary
    }, rownames = FALSE)

    #Poverty Line Plots
    output$pline_diag_tbl <- renderTable({
      res <- phase_e_results()
      if (is.null(res) || is.null(res$meta) || is.null(res$meta$plines_level)) return(NULL)


      pl_level <- res$meta$plines_level
      pl_model <- res$meta$plines_model


      data.frame(
        pline = names(pl_level),
        pline_value_level = as.numeric(pl_level),
        pline_value_model = as.numeric(pl_model),
        stringsAsFactors = FALSE
      )
    }, rownames = FALSE)

    phase_e_results <- reactive({
      if (is.null(historical_api) || !is.list(historical_api) || is.null(historical_api$poverty_results)) {
        return(NULL)
      }
      historical_api$poverty_results()
    })



    # Later, this server will accept Step 1 exports as arguments.
  })
}
