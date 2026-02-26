#' 1_05_weatherstats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_05_weatherstats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("weather_stats_button_ui"))
  )
}


#' 1_05_weatherstats Server Functions
#'
#' @noRd
mod_1_05_weatherstats_server <- function(
    id,
    connection_params,
    variable_list,
    selected_surveys,
    selected_outcome,
    selected_weather,
    survey_data,
    tabset_id,
    tabset_session = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    weather_tab_added <- reactiveVal(FALSE)

    output$weather_stats_button_ui <- renderUI({
      req(selected_weather())
      actionButton(ns("weather_stats"), "Weather stats", class = "btn-primary", style = "width: 100%;")
    })
    shiny::outputOptions(output, "weather_stats_button_ui", suspendWhenHidden = FALSE)

    survey_weather <- reactiveVal(NULL)

    # -------------------------------------------------------------------------
    observeEvent(input$weather_stats, {
      req(selected_weather(), selected_surveys(), survey_data())

      sw <- selected_weather()
      svy <- survey_data()

      # ---- Load, aggregate and construct weather ----------------------------
      notif_load <- showNotification("Loading weather data...", duration = NULL, type = "message")

      loc_wd <- tryCatch({

        dates <- svy |>
          dplyr::filter(!is.na(timestamp)) |>
          dplyr::distinct(timestamp) |>
          dplyr::pull(timestamp)

        get_weather(
          survey_data      = svy,
          selected_weather = sw,
          dates            = dates,
          connection_params = connection_params()
        )

      }, error = function(e) {
        removeNotification(notif_load)
        shiny::showNotification(
          paste("Failed to load weather data:", conditionMessage(e)),
          type = "error", duration = 8
        )
        NULL
      })

      removeNotification(notif_load)
      req(!is.null(loc_wd))

      # ---- Merge with survey data -------------------------------------------
      notif_merge <- showNotification("Merging survey and weather data...", duration = NULL, type = "message")

      survey_wd <- tryCatch({
        svy |>
          dplyr::inner_join(loc_wd,
            by = c("code", "year", "survname", "loc_id", "timestamp")) |>
          dplyr::mutate(year = as.factor(.data$year)) |>
          dplyr::group_by(.data$code, .data$year, .data$survname) |>
          dplyr::mutate(weight = .data$weight / sum(.data$weight, na.rm = TRUE)) |>
          dplyr::ungroup()
      }, error = function(e) {
        removeNotification(notif_merge)
        shiny::showNotification(
          paste("Failed to merge survey and weather data:", conditionMessage(e)),
          type = "error", duration = 8
        )
        NULL
      })

      removeNotification(notif_merge)
      req(!is.null(survey_wd))
      survey_weather(survey_wd)

      showNotification("Weather data ready.", duration = 3, type = "message")

      # ---- Outputs (defined once) ------------------------------------------
      if (!weather_tab_added()) {

        make_weather_dist <- function(idx) {
          renderPlot({
            req(survey_weather())
            df          <- survey_weather() |> dplyr::mutate(countryyear = paste0(economy, ", ", year))
            sw          <- isolate(selected_weather())
            hv          <- sw$name[idx]
            label       <- sw$label[idx]
            cont_binned <- sw$cont_binned[idx]

            if (is.na(hv) || !(hv %in% names(df))) {
              plot.new(); title(main = "Weather variable not configured"); return(invisible(NULL))
            }

            x_label <- stringr::str_wrap(paste0(label, "\n(as configured)"), 40)

            if (!is.na(cont_binned) && cont_binned == "Binned") {
              df_summary <- df |>
                dplyr::filter(!is.na(.data[[hv]])) |>
                dplyr::group_by(countryyear, .data[[hv]]) |>
                dplyr::summarise(n = dplyr::n(), .groups = "drop")

              ggplot2::ggplot(
                  df_summary,
                  ggplot2::aes(x = .data[[hv]], y = n, fill = countryyear)
                ) +
                ggplot2::geom_col(position = ggplot2::position_dodge(preserve = "single"), alpha = 0.85) +
                ggplot2::scale_fill_brewer(palette = "Set2", name = NULL) +
                ggplot2::theme_minimal() +
                ggplot2::labs(title = "Distribution of bins", x = x_label, y = "Count") +
                ggplot2::theme(
                  axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1),
                  legend.position = "top",
                  legend.text     = ggplot2::element_text(size = 9)
                )
            } else {
              ridge_distribution_plot(df, x_var = hv, x_label = x_label, wrap_width = 40)
            }
          })
        }

        output$weather_dist1 <- make_weather_dist(1)
        output$weather_dist2 <- make_weather_dist(2)

        make_binscatter <- function(idx) {
          renderPlot({
            req(survey_weather(), selected_outcome())
            df      <- survey_weather()
            so      <- selected_outcome()
            sw      <- isolate(selected_weather())
            y_var   <- so$name
            y_label <- so$label
            hv      <- sw$name[idx]

            if (is.null(y_var) || !(y_var %in% names(df)) ||
                is.na(hv)      || !(hv %in% names(df))) {
              plot.new()
              title(main = "Weather variable not configured")
              return(invisible(NULL))
            }

            hv_vals <- df[[hv]]
            y_vals  <- df[[y_var]]
            hv_ok   <- if (is.numeric(hv_vals)) is.finite(hv_vals) else !is.na(hv_vals)
            y_ok    <- if (is.numeric(y_vals))  is.finite(y_vals)  else !is.na(y_vals)

            df_plot <- df[hv_ok & y_ok, ]
            if (nrow(df_plot) == 0) {
              plot.new(); title(main = "No data available"); return(invisible(NULL))
            }

            is_binary <- length(unique(df_plot[[y_var]])) <= 2
            p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]], y = .data[[y_var]]))
            if (is_binary) {
              p <- p + ggplot2::stat_summary_bin(fun = "mean", bins = 20,
                                                 color = "orange", linewidth = 1, geom = "line")
            } else {
              p <- p +
                ggplot2::geom_point(alpha = 0.1) +
                ggplot2::stat_summary_bin(fun = "mean", bins = 20,
                                          color = "orange", size = 2, geom = "point")
            }
            p +
              ggplot2::theme_minimal() +
              ggplot2::labs(
                x = stringr::str_wrap(paste0(sw$label[idx], "\n(as configured)"), 40),
                y = stringr::str_wrap(y_label, 40)
              ) +
              if (is_binary) {
                ggplot2::annotate("text", x = Inf, y = Inf,
                  label = "Binary outcome: mean by bin", hjust = 1.05, vjust = 1.2, size = 3)
              }
          })
        }

        output$binscatter1 <- make_binscatter(1)
        output$binscatter2 <- make_binscatter(2)

        output$weather_stats_table <- DT::renderDT({
          req(survey_weather())
          df   <- survey_weather() |> dplyr::mutate(countryyear = paste0(economy, ", ", year))
          vars <- isolate(selected_weather())$name
          if (length(vars) == 0) return(data.frame(Note = "No weather variables found"))
          weighted_summary_long(df, vars = vars)
        }, rownames = FALSE,
          options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
          class = "compact")

        output$selected_weather <- DT::renderDT({
          selected_weather()
        }, rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
        class = "compact")

        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Weather stats",
            value = "weather_desc",
            shiny::h4("Distribution of weather (household survey sample)"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("weather_dist1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("weather_dist2"), height = "300px"))
            ),
            shiny::br(),
            shiny::h4("Outcome vs weather"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("binscatter1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("binscatter2"), height = "300px"))
            ),
            shiny::hr(),
            shiny::h4("Weather summary stats"),
            shiny::helpText(
              "Summary statistics for configured weather variables. Sample weights are used.",
              style = "font-size: 12px;"
            ),
            DT::DTOutput(ns("weather_stats_table")),
            shiny::br(),
            shiny::h4("Selected weather variables"),
            DT::DTOutput(ns("selected_weather"))
          ),
          select  = TRUE,
          session = tabset_session
        )


        weather_tab_added(TRUE)
      }

      if (weather_tab_added()) {
        try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id,
                                     selected = "weather_desc"), silent = TRUE)
      }

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ---- Return API ---------------------------------------------------------
    list(
      survey_weather = survey_weather
    )
  })
}
