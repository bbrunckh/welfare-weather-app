#' 1_03_surveystats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
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
    varlist,
    pov_lines,
    survey_geo = NULL,
    welf_select = NULL
) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns

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
          interview_dates <- survey_data() |>
            dplyr::summarise(hh = dplyr::n(), .by = c(countryname, countryyear, timestamp))

          ggplot2::ggplot(interview_dates,
            ggplot2::aes(x = timestamp, y = hh, fill = countryname)
          ) +
            ggplot2::geom_bar(stat = "identity") +
            ggplot2::theme_minimal() +
            ggplot2::labs(title = "", x = "", y = "Number of households", fill = "")
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
          req(survey_data())

          p <- ggplot2::ggplot(survey_data(), ggplot2::aes(
            x = log_welf,
            y = countryyear,
            fill = code
          )) +
            ggridges::geom_density_ridges(alpha = 0.7, scale = 2)

          # Add PPP poverty lines
          if (!is.null(welf_select) && is.function(welf_select) && isTRUE(welf_select()$pre == "$")) {
            povln <- pov_lines[pov_lines$ppp_year == welf_select()$year, "ln"]
            log_povln <- log(povln)
            p <- p + ggplot2::geom_vline(xintercept = log_povln) +
              ggplot2::annotate("text",
                x = log_povln[1] - 0.1, y = 1,
                label = paste0("$", sprintf("%.2f", povln[1]), "/day"),
                angle = 90, size = 4
              ) +
              ggplot2::annotate("text",
                x = log_povln[2] - 0.1, y = 1,
                label = paste0("$", sprintf("%.2f", povln[2]), "/day"),
                angle = 90, size = 4
              ) +
              ggplot2::annotate("text",
                x = log_povln[3] - 0.1, y = 1,
                label = paste0("$", sprintf("%.2f", povln[3]), "/day"),
                angle = 90, size = 4
              )
          }

          p + ggplot2::theme_minimal() +
            ggplot2::labs(
              title = "",
              x = if (!is.null(welf_select) && is.function(welf_select)) {
                paste0("Log ", welf_select()$label)
              } else {
                "Log welfare"
              },
              y = "", fill = ""
            ) +
            ggplot2::theme(legend.position = "none")
        })

        output$data_table <- DT::renderDT({
          req(survey_data())
          desc <- survey_data() |>
            dplyr::select(countryyear, weight, welf_ppp_2021, welf_lcu_2021,
              poor_300ln, poor_420ln, poor_830ln, dplyr::any_of(c("log_welf", "poor"))
            )

          sumtable::sumtable(desc,
            vars = colnames(desc)[-c(1, 2)],
            summ = c("weighted.mean(x, w = wts)", "weighted.sd(x, w = wts)", "min(x)", "max(x)", "notNA(x)"),
            summ.names = c("Mean", "Std. Dev.", "Min", "Max", "N"),
            group = "countryyear",
            group.long = TRUE,
            group.weights = "weight",
            labels = TRUE,
            out = "return"
          )
        }, rownames = FALSE)

        output$hh_stats <- DT::renderDT({
          req(survey_data(), varlist())
          desc <- survey_data() |>
            dplyr::select(countryyear, weight,
              dplyr::any_of(varlist()[varlist()$wiseapp == "HH characteristics" &
                varlist()$datatype %in% c("Numeric", "Binary", "Integer") &
                !is.na(varlist()$wiseapp), "varname"]) 
            )

          sumtable::sumtable(desc,
            vars = colnames(desc)[-c(1, 2)],
            summ = c("weighted.mean(x, w = wts)", "weighted.sd(x, w = wts)", "min(x)", "max(x)", "notNA(x)"),
            summ.names = c("Mean", "Std. Dev.", "Min", "Max", "N"),
            group = "countryyear",
            group.long = TRUE,
            group.weights = "weight",
            labels = TRUE,
            out = "return"
          )
        }, rownames = FALSE)

        output$area_stats <- DT::renderDT({
          req(survey_data(), varlist())
          desc <- survey_data() |>
            dplyr::select(countryyear, weight,
              dplyr::any_of(varlist()[varlist()$wiseapp == "Area characteristics" &
                !is.na(varlist()$wiseapp), "varname"]) 
            )

          sumtable::sumtable(desc,
            vars = colnames(desc)[-c(1, 2)],
            summ = c("weighted.mean(x, w = wts)", "weighted.sd(x, w = wts)", "min(x)", "max(x)", "notNA(x)"),
            summ.names = c("Mean", "Std. Dev.", "Min", "Max", "N"),
            group = "countryyear",
            group.long = TRUE,
            group.weights = "weight",
            labels = TRUE,
            out = "return"
          )
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
                tab = shiny::tabPanel(
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
                select = TRUE
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
          shiny::updateTabsetPanel(session, inputId = tabset_id, selected = "desc_stats")
        }
      }, error = function(e) {
        # Catch-any: show something even if an output render crashes
        message_survey_stats(paste("Survey stats failed:", conditionMessage(e)), type = "error")
        message("[surveystats] ERROR: ", conditionMessage(e))
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

  })
}
