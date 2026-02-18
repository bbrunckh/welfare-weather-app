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
    varlist, 
    selected_surveys,
    selected_outcome,
    survey_data,
    cpi_ppp = cpi_ppp, 
    survey_geo = NULL,
    tabset_id,
    tabset_session = NULL
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
      req(selected_outcome())  
      so <- selected_outcome()
      if (is.null(so)) return(NULL)
      actionButton(ns("survey_stats"), "Survey stats", class = "btn-primary", style = "width: 100%;")
    })

    shiny::outputOptions(output, "survey_stats_button_ui", suspendWhenHidden = FALSE)

    # tracker if the "Survey stats" tab has been added
    survey_tab_added <- reactiveVal(FALSE)

    observeEvent(input$survey_stats, {
      tryCatch({
        req(selected_outcome())

        message_survey_stats("Building survey stats…")
        message("[surveystats] click; tabset_id=", tabset_id)

        if (!survey_tab_added()) {
        output$interview_date <- renderPlot({
          req(survey_data())
          df <- survey_data() |>
            dplyr::mutate(
            timestamp = as.Date(paste0(.data$int_year, "-", .data$int_month, "-01"))
            )

          if (!"countryyear" %in% names(df) && all(c("economy", "year") %in% names(df))) {
            df <- dplyr::mutate(df, countryyear = paste0(economy, ", ", year))
          }
          req(all(c("economy", "countryyear", "timestamp") %in% names(df)))

          interview_dates <- df |>
            dplyr::summarise(hh = dplyr::n(), .by = c(economy, countryyear, timestamp))

          ggplot(interview_dates,
            aes(x = timestamp, y = hh, fill = economy)
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
          
        # welfare distribution plot
        output$welfare_dist <- renderPlot({
          tryCatch({
            df <- survey_data()
            if (is.null(df) || !nrow(df)) {
              plot.new(); title(main = "No survey data loaded")
              return(invisible(NULL))
            }

            if (!"countryyear" %in% names(df) && all(c("economy", "year") %in% names(df))) {
              df <- dplyr::mutate(df, countryyear = paste0(economy, ", ", year))
            } else if (!"countryyear" %in% names(df) && all(c("code", "year") %in% names(df))) {
              df <- dplyr::mutate(df, countryyear = paste0(code, ", ", year))
            }

            # Check if welfare column exists
            if (!("welfare" %in% names(df))) {
              plot.new(); title(main = "Welfare variable not found")
              return(invisible(NULL))
            }
            
            if (!("countryyear" %in% names(df))) {
              plot.new(); title(main = "Country-year variable not found")
              return(invisible(NULL))
            }

            # Convert welfare to 2021 PPP for plotting distribution
            cpi_ppp_data <- cpi_ppp()
            
            df <- df |>
              dplyr::left_join(cpi_ppp_data, by = c("code", "year")) |>
              dplyr::mutate(welfare_ppp = welfare / cpi / ppp2021)
            
            # Check if conversion was successful
            if (!("welfare_ppp" %in% names(df)) || all(is.na(df$welfare_ppp))) {
              plot.new(); title(main = "Could not convert welfare to PPP")
              return(invisible(NULL))
            }

            p <- ridge_distribution_plot(
              df,
              x_var = "welfare_ppp",  # ✅ Pass as string
              x_label = "$ per day (2021 PPP)",
              wrap_width = 40,
              log_transform = TRUE
            )

            if (is.null(p)) {
              plot.new(); title(main = "Welfare distribution unavailable")
              return(invisible(NULL))
            }

            # Add fixed PPP poverty lines at $3.00, $4.20, and $8.30
            poverty_lines <- c(3.00, 4.20, 8.30)
            poverty_labels <- c("$3.00", "$4.20", "$8.30")
              
              # Add vertical lines (ggplot2 handles log transformation automatically)
              for (i in seq_along(poverty_lines)) {
                p <- p + 
                  ggplot2::geom_vline(
                    xintercept = poverty_lines[i], 
                    linetype = "dashed", 
                    color = "red",
                    linewidth = 0.5
                  ) +
                  ggplot2::annotate(
                    "text",
                    x = poverty_lines[i] * 1.15,  # Slight offset to the right
                    y = 0.5,  # Adjust y position as needed
                    label = poverty_labels[i],
                    angle = 90,
                    size = 3,
                    color = "red",
                    hjust = 0
                  )
              }

            p
          }, error = function(e) {
            message_survey_stats(paste("welfare_dist failed:", conditionMessage(e)), type = "error")
            message("[surveystats] welfare_dist ERROR: ", conditionMessage(e))
            plot.new(); title(main = paste("Error:", conditionMessage(e)))
          })
        })
        
        # Summary stats for outcome variables
        output$outcome_stats <- DT::renderDT({
        req(survey_data())
        df <- survey_data()
        df <- dplyr::mutate(df, countryyear = paste0(economy, ", ", year))
        vl <- if (is.function(varlist)) varlist() else varlist
        vars <- intersect(vl$name[vl$outcome == 1], names(df))
        if (length(vars) == 0) return(data.frame(Note = "No outcome variables found"))  
        weighted_summary_long(df, vars = vars)
        }, rownames = FALSE)
          
        # summary stats for individual characteristics (if in data)
        output$ind_stats <- DT::renderDT({
        req(survey_data())
        df <- survey_data()
        df <- dplyr::mutate(df, countryyear = paste0(economy, ", ", year))
        vl <- if (is.function(varlist)) varlist() else varlist
        vars <- intersect(vl$name[vl$ind == 1], names(df))
        if (length(vars) == 0) return(data.frame(Note = "No individual variables found"))  
        weighted_summary_long(df, vars = vars)
        }, rownames = FALSE)
          
        # summary stats for household characteristics (if in data)
        output$hh_stats <- DT::renderDT({
        req(survey_data())
        df <- survey_data()
        df <- dplyr::mutate(df, countryyear = paste0(economy, ", ", year))
        vl <- if (is.function(varlist)) varlist() else varlist
        vars <- intersect(vl$name[vl$hh == 1], names(df))
        if (length(vars) == 0) return(data.frame(Note = "No household variables found"))  
        weighted_summary_long(df, vars = vars)
        }, rownames = FALSE)
          
        # summary stats for firm characteristics (if in data)
        output$firm_stats <- DT::renderDT({
        req(survey_data())
        df <- survey_data()
        df <- dplyr::mutate(df, countryyear = paste0(economy, ", ", year))
        vl <- if (is.function(varlist)) varlist() else varlist
        vars <- intersect(vl$name[vl$firm == 1], names(df))
        if (length(vars) == 0) return(data.frame(Note = "No firm variables found"))  
        weighted_summary_long(df, vars = vars)
        }, rownames = FALSE)
          
        # summary stats for area characteristics (if in data)
        output$area_stats <- DT::renderDT({
        req(survey_data())
        df <- survey_data()
        df <- dplyr::mutate(df, countryyear = paste0(economy, ", ", year))
        vl <- if (is.function(varlist)) varlist() else varlist
        vars <- intersect(vl$name[vl$area == 1], names(df))
        if (length(vars) == 0) return(data.frame(Note = "No area variables found"))  
        weighted_summary_long(df, vars = vars)
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
                    shiny::h4("Outcome stats"),
                    DT::DTOutput(ns("outcome_stats")),
                    shiny::h4("Individual characteristics"),
                    DT::DTOutput(ns("ind_stats")),
                    shiny::h4("Household characteristics"),
                    DT::DTOutput(ns("hh_stats")),
                    shiny::h4("Firm characteristics"),
                    DT::DTOutput(ns("firm_stats")),
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
