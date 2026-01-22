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
    survey_weather,
    weather_vars,
    haz_vars,
    weather_list,
    varlist,
    selected_outcome,
    data_loaded,
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
      if (!isTRUE(data_loaded())) return(NULL)
      if (is.null(weather_vars()) || !length(weather_vars())) return(NULL)
      shiny::actionButton(ns("weather_stats"), "Weather stats", class = "btn-primary", style = "width: 100%;")
    })

    shiny::outputOptions(output, "weather_stats_button_ui", suspendWhenHidden = FALSE)

    observeEvent(input$weather_stats, {
      sw <- survey_weather()
      if (is.null(sw)) {
        shiny::showNotification("Weather stats unavailable: weather data has not joined to survey data.", type = "warning")
        return(invisible(NULL))
      }
      req(length(haz_vars()) > 0)

      if (!weather_tab_added()) {
        output$weather_dist1 <- renderPlot({
          req(survey_weather(), haz_vars())
          df <- survey_weather()
          hv <- haz_vars()[1]

          if (!"countryyear" %in% names(df) && all(c("countryname", "year") %in% names(df))) {
            df <- dplyr::mutate(df, countryyear = paste0(countryname, ", ", year))
          }

          label <- get_label(weather_vars()[1], var_type = "weather", weather_list = weather_list)
          ridge_distribution_plot(
            df,
            x_var = hv,
            x_label = paste0(label, "\n (as configured)")
          )
        })

        output$weather_dist2 <- renderPlot({
          req(length(haz_vars()) > 1, survey_weather(), haz_vars())
          df <- survey_weather()
          hv <- haz_vars()[2]

          if (!"countryyear" %in% names(df) && all(c("countryname", "year") %in% names(df))) {
            df <- dplyr::mutate(df, countryyear = paste0(countryname, ", ", year))
          }

          label <- get_label(weather_vars()[2], var_type = "weather", weather_list = weather_list)
          ridge_distribution_plot(
            df,
            x_var = hv,
            x_label = paste0(label, "\n (as configured)"),
            wrap_width = 40
          )
        })

        output$binscatter1 <- renderPlot({
          req(survey_weather(), haz_vars())
          df <- survey_weather()
          hv <- haz_vars()[1]
          so <- selected_outcome()
          y_var <- so

          if (is.null(y_var)) {
            plot.new(); title(main = "Welfare variable not available")
            return(invisible(NULL))
          }

          x_label <- get_label(weather_vars()[1], var_type = "weather", weather_list = weather_list)
          y_label <- get_label(y_var, var_type = "general", varlist = varlist)

          df_plot <- df |>
            dplyr::filter(is.finite(.data[[hv]]), is.finite(.data[[y_var]]))

          is_binary <- length(unique(df_plot[[y_var]])) <= 2
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]], y = .data[[y_var]]))
          if (is_binary) {
            p <- p + ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", linewidth = 1, geom = "line")
          } else {
            p <- p + ggplot2::geom_point(alpha = 0.1) +
              ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", size = 2, geom = "point")
          }

          p <- p +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = "",
              x = stringr::str_wrap(paste0(x_label, "\n (as configured)"), 40),
              y = stringr::str_wrap(y_label, 40)
            )

          if (is_binary) {
            p <- p + ggplot2::annotate(
              "text",
              x = Inf,
              y = Inf,
              label = "Binary outcome: mean by bin",
              hjust = 1.05,
              vjust = 1.2,
              size = 3
            )
          }

          p
        })

        output$binscatter2 <- renderPlot({
          req(length(haz_vars()) > 1, survey_weather(), haz_vars())
          df <- survey_weather()
          hv <- haz_vars()[2]
          so <- selected_outcome()
          y_var <- if (!is.null(so) && so %in% names(df)) {
            so
          } else {
            dplyr::first(intersect(c("log_welf", "welf_ppp_2021", "welf_lcu_2021"), names(df)))
          }

          if (is.null(y_var)) {
            plot.new(); title(main = "Welfare variable not available")
            return(invisible(NULL))
          }

          x_label <- get_label(weather_vars()[2], var_type = "weather", weather_list = weather_list)
          y_label <- get_label(y_var, var_type = "general", varlist = varlist)

          df_plot <- df |>
            dplyr::filter(is.finite(.data[[hv]]), is.finite(.data[[y_var]]))

          is_binary <- length(unique(df_plot[[y_var]])) <= 2
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]], y = .data[[y_var]]))
          if (is_binary) {
            p <- p + ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", linewidth = 1, geom = "line")
          } else {
            p <- p + ggplot2::geom_point(alpha = 0.1) +
              ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", size = 2, geom = "point")
          }

          p <- p +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = "",
              x = stringr::str_wrap(paste0(x_label, "\n (as configured)"), 40),
              y = stringr::str_wrap(y_label, 40)
            )

          if (is_binary) {
            p <- p + ggplot2::annotate(
              "text",
              x = Inf,
              y = Inf,
              label = "Binary outcome: mean by bin",
              hjust = 1.05,
              vjust = 1.2,
              size = 3
            )
          }

          p
        })

        output$weather_stats <- DT::renderDT({
          req(survey_weather(), haz_vars())
          weighted_summary_long(survey_weather(), vars = haz_vars())
        }, rownames = FALSE)

        output$weather_varlist <- DT::renderDT({
          wl <- if (is.function(weather_list)) weather_list() else weather_list
          req(wl)
          wl
        }, rownames = FALSE)

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
            shiny::h4("Weather over time and space"),
            shiny::helpText("Weather maps to be added...", style = "font-size: 12px;"),
            shiny::br(),
            shiny::h4("Welfare vs weather"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("binscatter1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("binscatter2"), height = "300px"))
            ),
            shiny::br(),
            shiny::h4("Weather summary stats"),
            shiny::helpText("Summary statistics are shown for the configured weather variables. Sample weights are used.", style = "font-size: 12px;"),
            DT::DTOutput(ns("weather_stats")),
            shiny::br(),
            shiny::h4("Weather variable list"),
            DT::DTOutput(ns("weather_varlist"))
          ),
          select = TRUE,
          session = tabset_session
        )

        weather_tab_added(TRUE)
      }

      if (weather_tab_added()) {
        try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id, selected = "weather_desc"), silent = TRUE)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)
  })
}
