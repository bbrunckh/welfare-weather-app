#' 1_02_surveystats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_bar theme_minimal labs theme
mod_1_02_surveystats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("survey_stats_button_ui"))
  )
}

#' 1_02_surveystats Server Functions
#'
#' @noRd
mod_1_02_surveystats_server <- function(
    id,
    varlist, 
    selected_surveys,
    selected_outcome = NULL,
    cpi_ppp = cpi_ppp, 
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

    # Show the button only after if selected_surveys() is not empty
    output$survey_stats_button_ui <- renderUI({
      req(length(selected_surveys()) > 0)
      actionButton(ns("survey_stats"), "Survey stats", class = "btn-primary", style = "width: 100%;")
    })

    shiny::outputOptions(output, "survey_stats_button_ui", suspendWhenHidden = FALSE)

    # tracker if the "Survey stats" tab has been added
    survey_tab_added <- reactiveVal(FALSE)

    # --------- ReactiveVal to hold loaded data -----------
    survey_data_r <- reactiveVal(NULL)
    
    # --------- Expose survey_data() reactive -----------
    survey_data <- reactive({
      survey_data_r()
    })

    # --------- Survey H3 data (used for survey_geo & weather merge) ---
    survey_h3 <- reactive({
      req(survey_data())  
      # Build expected H3 file paths 
      paths <- selected_surveys()$fpath
      paths_h3 <- sub("_[^_]+\\.parquet$", "_h3.parquet", paths)
      read_parquet_duckdb(paths_h3)
    })

    # --------- Survey interview locations (sf polygons/points) -----------
    survey_geo <- reactive({
      req(survey_h3())

      h3 <- survey_h3() 

      # Connect to DuckDB and load extensions
      con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
      DBI::dbExecute(con, "INSTALL spatial;")
      DBI::dbExecute(con, "INSTALL h3;")
      DBI::dbExecute(con, "LOAD spatial;")
      DBI::dbExecute(con, "LOAD h3;")

      # Register the data frame as a DuckDB table
      DBI::dbWriteTable(con, "h3", h3)

      # Run the spatial query
      loc <- DBI::dbGetQuery(con, "
        SELECT 
          code, 
          year, 
          survname, 
          loc_id,
          ST_AsText(st_union_agg(st_geomfromtext(h3_cell_to_boundary_wkt(h3)))) AS geom
        FROM h3
        GROUP BY code, year, survname, loc_id
      ")

      DBI::dbDisconnect(con, shutdown = TRUE)

      # Convert WKT geometry to sf if possible.
      loc_geo <- sf::st_as_sf(loc, wkt = "geom", crs = 4326)
      loc_geo
    })
    
    # --------- Load data and show stats when button is clicked -----------
    observeEvent(input$survey_stats, {
      tryCatch({
        req(length(selected_surveys()) > 0)

        message("[surveystats] click; tabset_id=", tabset_id)
        
        # Load data first
        busy_id <- showNotification(
          "Loading survey data…",
          duration = NULL,
          type = "message"
        )
        
        paths <- isolate(selected_surveys()$fpath)
        df <- read_parquet_duckdb(paths)

        # add timestamp for interview month/year to merge with weather later
        df <- df |>
          dplyr::mutate(
            timestamp = as.Date(paste0(int_year, "-", int_month, "-01")),
            month = lubridate::month(timestamp)
          )
        # store in reactiveVal for reuse
        survey_data_r(df)
        
        removeNotification(busy_id)
        showNotification(
          paste0("Loaded ", length(paths), " files (", nrow(df), " rows)."),
          type = "message",
          duration = 3
        )
        
        message_survey_stats("Building survey stats…")

        if (!survey_tab_added()) {
          # Define all outputs
          output$interview_date <- renderPlot({
            req(survey_data())
            df <- survey_data() 

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
                x_var = "welfare_ppp",
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
                    x = poverty_lines[i] * 1.15,
                    y = 0.5,
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

          # selected surveys table
          output$selected_surveys <- DT::renderDT({
            req(selected_surveys())
            df <- selected_surveys() |>
              dplyr::select(-fpath)
          }, rownames = FALSE,
              options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
              class = "compact")

          # selected outcome variable from mod_1_03_outcome if it exists
          output$selected_outcome_section <- renderUI({
          if (is.null(selected_outcome)) return(NULL)  # selected_outcome not passed at all
          req(!is.null(selected_outcome()))             # selected_outcome() returns NULL
          shiny::tagList(
            shiny::br(),
            shiny::h4("Selected outcome variable"),
            DT::DTOutput(ns("selected_outcome"))
          )
        })

        output$selected_outcome <- DT::renderDT({
          if (is.null(selected_outcome)) return(NULL)
          req(!is.null(selected_outcome()))
          selected_outcome()
        }, rownames = FALSE,
          options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
          class = "compact")

          # Render survey stat outputs
          tryCatch(
            {
              map_ui <- leaflet::leafletOutput(ns("map"), height = "300px")
              
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
                  DT::DTOutput(ns("area_stats")),
                  shiny::br(),
                  shiny::h4("Selected surveys"),
                  DT::DTOutput(ns("selected_surveys")),
                  shiny::br(),
                  shiny::uiOutput(ns("selected_outcome_section"))
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
        message_survey_stats(paste("Survey stats failed:", conditionMessage(e)), type = "error")
        message("[surveystats] ERROR: ", conditionMessage(e))
      })
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------- Return list of reactives for other modules -----------
    return(list(
      survey_data = survey_data,
      survey_h3 = survey_h3,
      survey_geo = survey_geo
    ))

  })
}