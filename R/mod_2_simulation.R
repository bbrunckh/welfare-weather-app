#DRK Version 20260212 1520

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
              tableOutput(ns("historical-pred_na_drivers_tbl")),

              h5("Overall Prediction Summary Table"),
              tableOutput(ns("historical-pred_na_by_loc_tbl")),

              h5("Predicted welfare (out$pred) summary:"),
              tableOutput(ns("historical-pred_summary_tbl"))

            )
          ),

          tabPanel(
            title = "Visualizations",
            value = "visuals",

            # Controls (dynamic choices once Phase B/D exist)
            uiOutput(ns("viz_haz_ui")),
            uiOutput(ns("viz_plines_ui")),
            tags$hr(),

            h4("Historical weather distribution vs weather used to fit model"),
            plotOutput(ns("viz_haz_overlay_plot"), height = "280px"),

            tags$hr(),
            h4("\u0394 poverty vs exceedance probability"),
            plotOutput(ns("viz_pov_exceed_plot"), height = "320px"),

            tags$hr(),
            h4("Simulated welfare distributions, by median welfare"),
            plotOutput(ns("viz_welfare_ridges_plot"), height = "520px")
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

    # Namespace helper for server-side UI generation
    ns <- session$ns

    # Submodules (wired, even if they don't use args yet)
    historical_api <- mod_2_01_historical_server(
      "historical",
      step1 = step1,
      pov_lines = pov_lines, #DRK Note - I probably don't need this.
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

    #Phase E
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
      #pl_model <- res$meta$plines_model


      data.frame(
        pline = names(pl_level),
        pline_value_level = as.numeric(pl_level),
        #pline_value_model = as.numeric(pl_model),
        stringsAsFactors = FALSE
      )
    }, rownames = FALSE)

    phase_e_results <- reactive({
      if (is.null(historical_api) || !is.list(historical_api) || is.null(historical_api$poverty_results)) {
        return(NULL)
      }
      historical_api$poverty_results()
    })


    #VISUALIZATION
    output$viz_haz_ui <- renderUI({
      lw <- historical_api$loc_weather_sim()
      validate(need(!is.null(lw) && nrow(lw) > 0, "Run Phase B (Build Hazards) first."))

      haz_cols <- grep("^haz_", names(lw), value = TRUE)
      validate(need(length(haz_cols) > 0, "No haz_* columns available yet."))

      shiny::selectInput(
        inputId = ns("viz_haz_var"),
        label   = "Weather / hazard variable",
        choices = haz_cols,
        selected = haz_cols[[1]]
      )
    })

    output$viz_plines_ui <- renderUI({
      res <- historical_api$poverty_results()
      validate(need(!is.null(res) && !is.null(res$by_year), "Run Phase E (Compute poverty) first."))

      d <- res$by_year
      validate(need("pline" %in% names(d), "No pline column found in by_year poverty results."))

      pl_choices <- sort(unique(d$pline))
      shiny::selectInput(
        inputId = ns("viz_plines"),
        label   = "Poverty lines to show",
        choices = pl_choices,
        selected = utils::head(pl_choices, 2),
        multiple = TRUE
      )
    })

    #Hazard Overlay
    output$viz_haz_overlay_plot <- renderPlot({
      h <- input$viz_haz_var
      validate(need(!is.null(h) && nzchar(h), "Select a hazard variable."))

      lw <- historical_api$loc_weather_sim()
      sw <- step1$survey_weather()

      validate(need(!is.null(lw) && nrow(lw) > 0, "No Phase B hazards yet."))
      validate(need(!is.null(sw) && nrow(sw) > 0, "No Step 1 survey_weather available."))
      validate(need(h %in% names(lw), paste0("Missing ", h, " in loc_weather_sim.")))
      validate(need(h %in% names(sw), paste0("Missing ", h, " in survey_weather.")))

      df <- dplyr::bind_rows(
        dplyr::transmute(lw, value = .data[[h]], source = "Historical"),
        dplyr::transmute(sw, value = .data[[h]], source = "Model fit")
      )

      ggplot2::ggplot(df, ggplot2::aes(x = value, fill = source)) +
        ggplot2::geom_density(alpha = 0.45, na.rm = TRUE) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = h, y = "Density", fill = NULL)
    })

    viz_exceed_df <- reactive({
      res <- historical_api$poverty_results()
      pp  <- historical_api$pred_panel()
      h   <- input$viz_haz_var

      validate(need(!is.null(pp) && nrow(pp) > 0, "Run Phase D first (Predict welfare)."))
      validate(need(!is.null(res) && !is.null(res$by_year), "Run Phase E first (Compute poverty)."))
      validate(need(!is.null(h) && h %in% names(pp), "Selected hazard not found on pred_panel."))

      haz_year <- pp |>
        dplyr::group_by(.data$sim_year) |>
        dplyr::summarise(haz_mean = mean(.data[[h]], na.rm = TRUE), .groups = "drop")

      # Exceedance prob = P(H > h) using empirical CDF of yearly means
      ec <- stats::ecdf(haz_year$haz_mean)
      haz_year$exc_prob <- 1 - ec(haz_year$haz_mean)

      d <- res$by_year |>
        dplyr::left_join(haz_year, by = "sim_year") |>
        dplyr::filter(!is.na(.data$exc_prob))

      # Optional filter: only plot selected poverty lines
      if (!is.null(input$viz_plines) && length(input$viz_plines) > 0 && "pline" %in% names(d)) {
        d <- dplyr::filter(d, .data$pline %in% input$viz_plines)
      }

      # Delta poverty (percentage points)
      d |>
        dplyr::group_by(.data$pline) |>
        dplyr::mutate(delta_pp = 100 * (.data$pov_rate - mean(.data$pov_rate, na.rm = TRUE))) |>
        dplyr::ungroup()
    })

    output$viz_pov_exceed_plot <- renderPlot({
      d <- viz_exceed_df()
      validate(need(nrow(d) > 0, "Nothing to plot yet."))

      ggplot2::ggplot(d, ggplot2::aes(x = exc_prob, y = delta_pp)) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          x = "Annual exceedance probability (empirical)",
          y = "Change in poverty rate (pp)"
        ) +
        ggplot2::facet_wrap(~ pline, scales = "free_y")
    })

    output$viz_welfare_ridges_plot <- renderPlot({
      pp <- historical_api$pred_panel()
      h  <- input$viz_haz_var

      validate(need(!is.null(pp) && nrow(pp) > 0, "Run Phase D first (Predict welfare)."))
      validate(need("pred" %in% names(pp), "pred column not found on pred_panel."))
      validate(need(!is.null(h) && h %in% names(pp), "Selected hazard not found on pred_panel."))

      # downsample for speed if needed
      if (nrow(pp) > 400000) pp <- dplyr::slice_sample(pp, n = 400000)

      # yearly stats
      yr <- pp |>
        dplyr::group_by(.data$sim_year) |>
        dplyr::summarise(
          med_pred = stats::median(.data$pred, na.rm = TRUE),
          haz_mean = mean(.data[[h]], na.rm = TRUE),
          .groups = "drop"
        )

      # common x-grid
      rng <- range(pp$pred, na.rm = TRUE)
      xg  <- seq(rng[1], rng[2], length.out = 200)

      # build “ridge” lines
      ridge_df <- lapply(yr$sim_year, function(y) {
        v <- pp$pred[pp$sim_year == y]
        v <- v[is.finite(v)]
        if (length(v) < 50) return(NULL)

        den <- stats::density(v, n = 200, from = rng[1], to = rng[2])
        s <- yr[yr$sim_year == y, ]

        data.frame(
          sim_year = y,
          x = den$x,
          y = s$med_pred + 0.15 * den$y,  # 0.15 is a visual scaling knob
          med_pred = s$med_pred,
          haz_mean = s$haz_mean
        )
      }) |>
        dplyr::bind_rows()

      validate(need(nrow(ridge_df) > 0, "Not enough data to build ridge plot."))

      ggplot2::ggplot(ridge_df, ggplot2::aes(x = x, y = y, group = sim_year, color = haz_mean)) +
        ggplot2::geom_line(alpha = 0.85) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          x = "Welfare (pred scale)",
          y = "Median welfare (baseline + scaled density)",
          color = "Mean hazard"
        )
    })



    # Later, this server will accept Step 1 exports as arguments.
  })
}
