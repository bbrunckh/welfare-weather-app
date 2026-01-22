#' 1_03_surveystats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_bar theme_minimal labs theme
mod_1_03_surveystats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("survey_stats_button_ui"))
  )
}
    
#' 1_03_surveystats Server Functions
#'
#' @noRd 
mod_1_03_surveystats_server <- function(
    id,
    survey_data,
    data_loaded,
    selected_outcome,
    tabset_id,
    tabset_session = NULL,
    varlist,
    pov_lines,
    survey_geo = NULL,
    welf_select = NULL
) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    # Selecting a tabsetPanel that lives outside this module's namespace can fail silently.
    # `sendInputMessage()` is a robust fallback that works with fully qualified ids.
    select_tab <- function(value) {
      if (is.null(tabset_id) || !nzchar(tabset_id)) return(invisible(FALSE))
      try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id, selected = value), silent = TRUE)
      try(tabset_session$sendInputMessage(tabset_session$ns(tabset_id), list(value = value)), silent = TRUE)
      invisible(TRUE)
    }

    message_survey_stats <- function(msg, type = c("message", "warning", "error")) {
      type <- match.arg(type)
      if (requireNamespace("shiny", quietly = TRUE)) {
        shiny::showNotification(msg, type = type, duration = 5)
      }
    }

    # Show the button only after data has been loaded AND an outcome has been selected
    output$survey_stats_button_ui <- renderUI({
      dl <- isTRUE(data_loaded())
      so <- selected_outcome()
      if (!dl) return(NULL)
      if (is.null(so) || !nzchar(so)) return(NULL)
      actionButton(ns("survey_stats"), "Survey stats", class = "btn-primary", style = "width: 100%;")
    })

    shiny::outputOptions(output, "survey_stats_button_ui", suspendWhenHidden = FALSE)

    # tracker if the "Survey stats" tab has been added
    survey_tab_added <- reactiveVal(FALSE)

    observeEvent(input$survey_stats, {
      tryCatch({
        req(isTRUE(data_loaded()))
        req(selected_outcome())

        message_survey_stats("Building survey stats…")
        message("[surveystats] click; tabset_id=", tabset_id)

        if (!survey_tab_added()) {
        output$interview_date <- renderPlot({
          req(survey_data())
          df <- survey_data()
          if (!"countryyear" %in% names(df) && all(c("countryname", "year") %in% names(df))) {
            df <- dplyr::mutate(df, countryyear = paste0(countryname, ", ", year))
          }
          req(all(c("countryname", "countryyear", "timestamp") %in% names(df)))

          interview_dates <- df |>
            dplyr::summarise(hh = dplyr::n(), .by = c(countryname, countryyear, timestamp))

          ggplot(interview_dates,
            aes(x = timestamp, y = hh, fill = countryname)
          ) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            labs(title = "", x = "", y = "Number of households", fill = "")
        })

        map_available <- !is.null(survey_geo) && requireNamespace("leaflet", quietly = TRUE)
        if (map_available) {
          output$map <- leaflet::renderLeaflet({
            req(survey_geo())
            polygon_data <- survey_geo()

            gg_color_hue <- scales::hue_pal()
            n_colors <- length(unique(polygon_data$code))
            default_colors <- gg_color_hue(n_colors)
            pal <- leaflet::colorFactor(palette = default_colors, domain = polygon_data$code)

            leaflet::leaflet() %>%
              leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
              leaflet::addPolygons(
                data = polygon_data,
                fillColor = "transparent",
                weight = 1,
                opacity = 0.5,
                color = ~pal(code),
                dashArray = "",
                fillOpacity = 0.5,
                highlight = leaflet::highlightOptions(
                  weight = 1,
                  color = "#FF0000",
                  dashArray = "",
                  fillOpacity = 0.5,
                  bringToFront = TRUE
                ),
                label = ~paste("Interview dates:", int_dates),
                labelOptions = leaflet::labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto"
                )
              ) |>
              leaflet::fitBounds(
                lng1 = min(sf::st_bbox(polygon_data)[1]),
                lat1 = min(sf::st_bbox(polygon_data)[2]),
                lng2 = max(sf::st_bbox(polygon_data)[3]),
                lat2 = max(sf::st_bbox(polygon_data)[4])
              )
          })
        }

        output$welfare_dist <- renderPlot({
          tryCatch({
            df <- survey_data()
            if (is.null(df) || !nrow(df)) {
              plot.new(); title(main = "No survey data loaded")
              return(invisible(NULL))
            }

            if (!"countryyear" %in% names(df) && all(c("countryname", "year") %in% names(df))) {
              df <- dplyr::mutate(df, countryyear = paste0(countryname, ", ", year))
            } else if (!"countryyear" %in% names(df) && all(c("code", "year") %in% names(df))) {
              df <- dplyr::mutate(df, countryyear = paste0(code, ", ", year))
            }

            outcome_var <- selected_outcome()
            x_var <- if (!is.null(outcome_var) && outcome_var %in% names(df)) {
              outcome_var
            } else {
              NULL
            }

            if (is.null(x_var) || !("countryyear" %in% names(df))) {
              plot.new(); title(main = "Welfare distribution unavailable")
              return(invisible(NULL))
            }

            x_label <- if (!is.null(outcome_var) && "label" %in% names(df)) {
              x_label <- df[df$outcome_var == outcome_var, "label"]
              if (length(x_label)) {
                x_label_val <- as.character(x_label[[1]])
                if (!is.na(x_label_val) && nzchar(x_label_val)) {
                  x_label <- x_label_val
                } else {
                  x_label <- NULL
                }
              } else {
                x_label <- NULL
              }
            } else {
              x_label <- NULL
            }

            p <- ridge_distribution_plot(
              df,
              x_var = x_var,
              x_label = x_label,
              wrap_width = 40
            )

            if (is.null(p)) {
              plot.new(); title(main = "Welfare distribution unavailable")
              return(invisible(NULL))
            }

            # Add PPP poverty lines
            if (!is.null(welf_select) && is.function(welf_select) && isTRUE(welf_select()$pre == "$")) {
              pl <- if (is.function(pov_lines)) pov_lines() else pov_lines
              if (!is.null(pl)) {
                povln <- pl[pl$ppp_year == welf_select()$year, "ln"]
                if (length(povln)) {
                  log_povln <- log(povln)
                  p <- p + ggplot2::geom_vline(xintercept = log_povln)
                  for (i in seq_len(min(3, length(log_povln)))) {
                    p <- p + ggplot2::annotate(
                      "text",
                      x = log_povln[i] - 0.1, y = 1,
                      label = paste0("$", sprintf("%.2f", povln[i]), "/day"),
                      angle = 90, size = 4
                    )
                  }
                }
              }
            }

            p
          }, error = function(e) {
            message_survey_stats(paste("welfare_dist failed:", conditionMessage(e)), type = "error")
            message("[surveystats] welfare_dist ERROR: ", conditionMessage(e))
            plot.new(); title(main = "Error rendering welfare distribution")
          })
        })

        output$data_table <- DT::renderDT({
          req(survey_data())
          df <- survey_data()
          if (!"countryyear" %in% names(df) && all(c("countryname", "year") %in% names(df))) {
            df <- dplyr::mutate(df, countryyear = paste0(countryname, ", ", year))
          }
          vars <- intersect(
            c(
              "welf_ppp_2021", "welf_lcu_2021",
              "poor_300ln", "poor_420ln", "poor_830ln",
              "log_welf", "poor"
            ),
            names(df)
          )
          if (!length(vars)) return(data.frame())
          weighted_summary_long(df, vars = vars)
        }, rownames = FALSE)

        output$hh_stats <- DT::renderDT({
          tryCatch({
            df <- survey_data()
            if (is.null(df) || !nrow(df)) return(data.frame(note = "No survey data loaded."))
            if (!"countryyear" %in% names(df) && all(c("countryname", "year") %in% names(df))) {
              df <- dplyr::mutate(df, countryyear = paste0(countryname, ", ", year))
            }

            v <- if (is.function(varlist)) varlist() else varlist
            if (is.null(v) || !"wiseapp" %in% names(v)) {
              num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
              num_vars <- setdiff(num_vars, c("year", "weight"))
              if (!length(num_vars)) return(data.frame(note = "No household variables found."))
              return(weighted_summary_long(df, vars = num_vars))
            }

            v_df <- tryCatch(as.data.frame(v), error = function(e) NULL)
            if (is.null(v_df)) return(data.frame(note = "varlist unavailable"))
            vars <- v_df |>
              dplyr::filter(.data$wiseapp == "HH characteristics", .data$datatype %in% c("Numeric", "Binary", "Integer")) |>
              dplyr::pull(.data$varname)
            vars <- intersect(vars, names(df))
            if (!length(vars)) return(data.frame(note = "No household variables found."))
            weighted_summary_long(df, vars = vars)
          }, error = function(e) {
            message_survey_stats(paste("hh_stats failed:", conditionMessage(e)), type = "error")
            message("[surveystats] hh_stats ERROR: ", conditionMessage(e))
            data.frame()
          })
        }, rownames = FALSE)

        output$area_stats <- DT::renderDT({
          tryCatch({
            df <- survey_data()
            if (is.null(df) || !nrow(df)) return(data.frame(note = "No survey data loaded."))
            if (!"countryyear" %in% names(df) && all(c("countryname", "year") %in% names(df))) {
              df <- dplyr::mutate(df, countryyear = paste0(countryname, ", ", year))
            }

            v <- if (is.function(varlist)) varlist() else varlist
            if (is.null(v) || !"wiseapp" %in% names(v)) {
              area_vars <- grep("area", names(df), value = TRUE, ignore.case = TRUE)
              area_vars <- intersect(area_vars, names(df)[vapply(df, is.numeric, logical(1))])
              if (!length(area_vars)) return(data.frame(note = "No area variables found."))
              return(weighted_summary_long(df, vars = area_vars))
            }

            v_df <- tryCatch(as.data.frame(v), error = function(e) NULL)
            if (is.null(v_df)) return(data.frame(note = "varlist unavailable"))
            vars <- v_df |>
              dplyr::filter(.data$wiseapp == "Area characteristics") |>
              dplyr::pull(.data$varname)
            vars <- intersect(vars, names(df))
            if (!length(vars)) return(data.frame(note = "No area variables found."))
            weighted_summary_long(df, vars = vars)
          }, error = function(e) {
            message_survey_stats(paste("area_stats failed:", conditionMessage(e)), type = "error")
            message("[surveystats] area_stats ERROR: ", conditionMessage(e))
            data.frame()
          })
        }, rownames = FALSE)

          # Render survey stat outputs
          tryCatch(
            {
            map_ui <- if (map_available) {
              leaflet::leafletOutput(ns("map"), height = "300px")
            } else {
              shiny::div(
                class = "text-muted",
                "Map unavailable (missing leaflet and/or survey_geo)."
              )
            }

              shiny::appendTab(
                inputId = tabset_id,
                shiny::tabPanel(
                  title = "Survey stats",
                  value = "desc_stats",
                    shiny::div(
                      style = "margin-bottom: 10px;",
                      shiny::strong("Survey stats loaded."),
                      shiny::tags$div(class = "text-muted", paste0("Rows: ", nrow(survey_data()))),
                      shiny::tags$div(class = "text-muted", paste0("Outcome: ", selected_outcome()))
                    ),
                    bslib::layout_columns(
                      col_widths = c(6, 6),
                      bslib::card(
                        shiny::h4("Timing of interviews"),
                        shiny::plotOutput(ns("interview_date"), height = "300px")
                      ),
                      bslib::card(
                        shiny::h4("Location of interviews"),
                        map_ui
                      )
                    ),
                    shiny::br(),
                    bslib::card(
                      shiny::h4("Welfare distribution"),
                      shiny::plotOutput(ns("welfare_dist"), height = "300px")
                    ),
                    shiny::h4("Welfare summary stats"),
                    DT::DTOutput(ns("data_table")),
                    shiny::h4("Household characteristics"),
                    DT::DTOutput(ns("hh_stats")),
                    shiny::h4("Area characteristics"),
                    DT::DTOutput(ns("area_stats"))
                ),
                select = TRUE,
                menuName = NULL,
                session = tabset_session
              )
            },
            error = function(e) {
              message_survey_stats(paste("Failed to add Survey stats tab:", conditionMessage(e)), type = "error")
              stop(e)
            }
          )

          survey_tab_added(TRUE)
          message_survey_stats("Survey stats tab added.")
        }

        if (survey_tab_added()) {
          select_tab("desc_stats")
        }
      }, error = function(e) {
        # Catch-any: show something even if an output render crashes
        message_survey_stats(paste("Survey stats failed:", conditionMessage(e)), type = "error")
        message("[surveystats] ERROR: ", conditionMessage(e))
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

  })
}
