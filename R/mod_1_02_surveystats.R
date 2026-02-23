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
    connection_params,
    variable_list,
    selected_surveys,
    selected_outcome = NULL,
    cpi_ppp,
    tabset_id,
    tabset_session = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    select_tab <- function(value) {
      if (is.null(tabset_id) || !nzchar(tabset_id)) return(invisible(FALSE))
      try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id, selected = value), silent = TRUE)
      invisible(TRUE)
    }

    notify <- function(msg, type = "message", duration = 5) {
      shiny::showNotification(msg, type = type, duration = duration)
    }

    # ---- Button (shown once selected_surveys is populated) ------------------

    output$survey_stats_button_ui <- renderUI({
      req(length(selected_surveys()) > 0)
      actionButton(ns("survey_stats"), "Survey stats", class = "btn-primary", style = "width: 100%;")
    })

    shiny::outputOptions(output, "survey_stats_button_ui", suspendWhenHidden = FALSE)

    survey_tab_added <- reactiveVal(FALSE)

    # ---- Data storage -------------------------------------------------------

    survey_data <- reactiveVal(NULL)

    # ---- Load data on button click ------------------------------------------

    observeEvent(input$survey_stats, {
      req(length(selected_surveys()) > 0)

      busy_id <- showNotification("Loading survey data…", duration = NULL, type = "message")
      on.exit(removeNotification(busy_id), add = TRUE)

      ss <- selected_surveys()

      # load_data() resolves bare fnames against connection_params
      df <- tryCatch(
        load_data(ss$fname, connection_params(), collect = TRUE, unify_schemas = TRUE),
        error = function(e) {
          notify(paste("Failed to load survey data:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        }
      )

      req(!is.null(df))

      # Add derived time columns
      df <- df |>
        dplyr::mutate(
          timestamp   = as.Date(paste0(int_year, "-", int_month, "-01")),
          month       = lubridate::month(timestamp),
          countryyear = paste0(economy, ", ", year)
        )

      # Convert LCU monetary variables to 2021 PPP
      cpi_ppp_data <- cpi_ppp()
      lcu_vars <- variable_list() |>
        dplyr::filter(units == "LCU", name %in% colnames(df)) |>
        dplyr::pull(name)

      df <- df |>
        dplyr::mutate(
          data_level = dplyr::case_when(
            code == "CHN" & urban == 1 ~ "urban",
            code == "CHN" & urban == 0 ~ "rural",
            .default = "national"
          )
        ) |>
        dplyr::left_join(cpi_ppp_data, by = c("code", "year", "data_level")) |>
        dplyr::mutate(
          dplyr::across(dplyr::any_of(lcu_vars), ~ .x / cpi / ppp2021)
        )

      survey_data(df)

      notify(
        paste0("Loaded ", nrow(ss), " survey file(s) — ", nrow(df), " rows."),
        type = "message", duration = 3
      )

      map_data <- eventReactive(input$survey_stats, {
        ss <- selected_surveys()
        req(nrow(ss) > 0)
        
        h3_fnames <- sub("_[^_]+\\.parquet$", "_h3.parquet", ss$fname)
        h3_data <- load_data(h3_fnames, connection_params())
        
        duckdbfs::load_spatial()
        duckdbfs::load_h3()

        loc <- h3_data |> 
          dplyr::summarise(
            geom = st_union_agg(st_geomfromtext(h3_cell_to_boundary_wkt(h3))),
            .by = c(code, year, survname, loc_id)
          ) |>
          duckdbfs::to_sf(crs = 4326)
        loc <- loc[!sf::st_is_empty(loc), ]
        loc
      })

      # ---- Outputs (defined once on first click) ---------------------------

      if (!survey_tab_added()) {

        # plot of interview dates by survey
        output$interview_date <- renderPlot({
          df <- survey_data()
          
          # Summarize with error handling
          plot_data <- df |> 
            dplyr::filter(!is.na(timestamp)) |>  # ← Remove invalid dates
            dplyr::summarise(hh = dplyr::n(), .by = c(economy, countryyear, timestamp))
          
          req(!is.null(plot_data), nrow(plot_data) > 0)
          
          ggplot(plot_data, aes(x = timestamp, y = hh, fill = economy)) +
            geom_bar(stat = "identity") +
            theme_minimal() +
            labs(title = "", x = "", y = "Number of households", fill = "") +
            theme(legend.position = "bottom")  
        })

        # map of interview locations 
        output$map <- leaflet::renderLeaflet({
          loc <- map_data() 
          req(nrow(loc) > 0)
          
          pal <- leaflet::colorFactor(scales::hue_pal()(length(unique(loc$code))), loc$code)
          bounds <- sf::st_bbox(loc)

          leaflet::leaflet() |>
            leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
            leaflet::addPolygons(
              data = loc, color = ~pal(code), opacity = 0.5, fillOpacity = 0, weight = 1,
              highlight = leaflet::highlightOptions(weight = 2, color = "#FF0000", bringToFront = TRUE)
            ) |>
            leaflet::fitBounds(
              lng1 = as.numeric(bounds[1]), lat1 = as.numeric(bounds[2]),
              lng2 = as.numeric(bounds[3]), lat2 = as.numeric(bounds[4])
            )
        })

        # Welfare distribution plot (log scale) with poverty lines at $3.00, $4.20, and $8.30 (2021 PPP)
        output$welfare_dist <- renderPlot({
          df <- survey_data()
          if (!("welfare" %in% names(df))) {
            plot.new(); title(main = "Welfare variable not found"); return(invisible(NULL))
          }
          p <- ridge_distribution_plot(df, x_var = "welfare",
            x_label = "$ per day (2021 PPP)", wrap_width = 40, log_transform = TRUE)
          if (is.null(p)) {
            plot.new(); title(main = "Welfare distribution unavailable"); return(invisible(NULL))
          }
          poverty_lines  <- c(3.00, 4.20, 8.30)
          poverty_labels <- c("$3.00", "$4.20", "$8.30")
          for (i in seq_along(poverty_lines)) {
            p <- p +
              ggplot2::geom_vline(xintercept = poverty_lines[i], linetype = "dashed",
                                  color = "red", linewidth = 0.5) +
              ggplot2::annotate("text", x = poverty_lines[i] * 1.15, y = 0.5,
                                label = poverty_labels[i], angle = 90, size = 3,
                                color = "red", hjust = 0)
          }
          p
        })

        # Helper to avoid repeating DT output boilerplate for each variable group
        make_stats_dt <- function(flag_col) {
          DT::renderDT({
            req(survey_data())
            df <- survey_data()
            vl <- if (is.function(variable_list)) variable_list() else variable_list
            vars <- intersect(vl$name[vl[[flag_col]] == 1], names(df))
            if (length(vars) == 0)
              return(data.frame(Note = paste("No", flag_col, "variables found")))
            weighted_summary_long(df, vars = vars)
          }, rownames = FALSE)
        }

        output$outcome_stats <- make_stats_dt("outcome")
        output$ind_stats     <- make_stats_dt("ind")
        output$hh_stats      <- make_stats_dt("hh")
        output$firm_stats    <- make_stats_dt("firm")
        output$area_stats    <- make_stats_dt("area")

        output$selected_surveys <- DT::renderDT({
          req(selected_surveys())
          # drop internal path columns from display
          selected_surveys() |> dplyr::select(-dplyr::any_of(c("fname", "fpath")))
        }, rownames = FALSE,
          options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
          class = "compact")

        output$selected_outcome_section <- renderUI({
          if (is.null(selected_outcome) || !is.function(selected_outcome)) return(NULL)
          sel <- tryCatch(selected_outcome(), error = function(e) NULL)
          if (is.null(sel)) return(NULL)
          tagList(br(), h4("Selected outcome variable"), DT::DTOutput(ns("selected_outcome")))
        })

        output$selected_outcome <- DT::renderDT({
          if (is.null(selected_outcome) || !is.function(selected_outcome)) return(NULL)
          sel <- tryCatch(selected_outcome(), error = function(e) NULL)
          if (is.null(sel) || !is.data.frame(sel) || nrow(sel) == 0)
            return(data.frame(Note = "No outcome selected"))
          sel
        }, rownames = FALSE,
          options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
          class = "compact")

        tryCatch(
          shiny::appendTab(
            inputId = tabset_id,
            shiny::tabPanel(
              title = "Survey stats",
              value = "desc_stats",
              bslib::layout_columns(
                col_widths = c(6, 6),
                bslib::card(h4("Timing of interviews"),
                            plotOutput(ns("interview_date"), height = "300px")),
                bslib::card(h4("Location of interviews"),
                            leaflet::leafletOutput(ns("map"), height = "300px"))
              ),
              br(),
              bslib::card(h4("Welfare distribution"),
                          plotOutput(ns("welfare_dist"), height = "300px")),
              h4("Outcome stats"),             DT::DTOutput(ns("outcome_stats")),
              h4("Individual characteristics"), DT::DTOutput(ns("ind_stats")),
              h4("Household characteristics"),  DT::DTOutput(ns("hh_stats")),
              h4("Firm characteristics"),       DT::DTOutput(ns("firm_stats")),
              h4("Area characteristics"),       DT::DTOutput(ns("area_stats")),
              br(),
              h4("Selected surveys"),           DT::DTOutput(ns("selected_surveys")),
              br(),
              uiOutput(ns("selected_outcome_section"))
            ),
            select  = TRUE,
            session = tabset_session
          ),
          error = function(e) {
            notify(paste("Failed to add Survey stats tab:", conditionMessage(e)), type = "error")
          }
        )

        survey_tab_added(TRUE)
        notify("Survey stats ready.", type = "message", duration = 2)
      }

      if (survey_tab_added()) select_tab("desc_stats")

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ---- Return API ---------------------------------------------------------

    list(
      survey_data = survey_data
    )
  })
}