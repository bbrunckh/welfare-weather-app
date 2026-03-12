#' 1_02_surveystats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom ggplot2 ggplot aes geom_bar theme_minimal labs theme
mod_1_02_surveystats_ui <- function(id) {
  ns <- NS(id)
  tags$style(HTML("
    table.dataTable td.dt-wrap {
      white-space: normal !important;
      word-break: break-word;
    }
  "))
  tagList(
    uiOutput(ns("survey_stats_button_ui"))
  )
}

#' 1_02_surveystats Server Functions
#'
#' @param id Module id.
#' @param connection_params Reactive named list of connection parameters.
#' @param variable_list Reactive data frame of variable metadata.
#' @param selected_surveys Reactive data frame of selected surveys (from mod_1_01_sample).
#' @param selected_outcome Optional reactive returning the selected outcome row.
#' @param cpi_ppp Reactive data frame of CPI/PPP deflators.
#' @param tabset_id Character id of the parent tabset panel to append the tab to.
#' @param tabset_session Shiny session for the parent tabset. Defaults to the parent session.
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
    map_data    <- reactiveVal(NULL)

    # ---- Load and prepare data on button click ------------------------------

    observeEvent(input$survey_stats, {
      req(length(selected_surveys()) > 0)

      busy_id <- showNotification("Loading survey data…", duration = NULL, type = "message")
      on.exit(removeNotification(busy_id), add = TRUE)

      ss <- selected_surveys()

      df <- tryCatch(
        load_data(ss$fname, connection_params(), collect = TRUE, unify_schemas = TRUE),
        error = function(e) {
          notify(paste("Failed to load survey data:", conditionMessage(e)), type = "error", duration = 8)
          NULL
        }
      )

      req(!is.null(df))

      df <- add_time_columns(df)

      lcu_vars <- get_lcu_vars(df, variable_list())
      df       <- df |>
        assign_data_level() |>
        convert_lcu_to_ppp(cpi_ppp(), lcu_vars)

      survey_data(df)

      # ---- H3 map data (computed once per button click) -------------------
      h3_fnames <- df |>
        dplyr::distinct(code, year, survname) |>
        dplyr::mutate(fname = paste0(code, "_", year, "_", survname, "_h3.parquet")) |>
        dplyr::pull(fname)

      h3_df <- tryCatch(
        load_data(h3_fnames, connection_params()),
        error = function(e) {
          notify(paste("Failed to load H3 data:", conditionMessage(e)), type = "warning", duration = 5)
          NULL
        }
      )

      if (!is.null(h3_df)) {
        tryCatch({
          duckdbfs::load_spatial()
          duckdbfs::load_h3()
          loc <- h3_df |>
            dplyr::summarise(
              geom = st_union_agg(st_geomfromtext(h3_cell_to_boundary_wkt(h3))),
              .by  = c(code, year, survname, loc_id)
            ) |>
            duckdbfs::to_sf(crs = 4326)
          map_data(loc[!sf::st_is_empty(loc), ])
        }, error = function(e) {
          notify(paste("Failed to build map data:", conditionMessage(e)), type = "warning", duration = 5)
        })
      }

      notify(
        paste0("Loaded ", nrow(ss), " survey file(s) — ", nrow(df), " rows."),
        type = "message", duration = 3
      )

      # ---- Outputs (defined once on first click) ---------------------------

      if (!survey_tab_added()) {

        # Interview dates bar chart
        output$interview_date <- renderPlot({
          p <- plot_interview_dates(summarise_interview_dates(survey_data()))
          req(!is.null(p))
          p
        })

        # Leaflet map of interview locations
        output$map <- leaflet::renderLeaflet({
          m <- plot_survey_map(map_data())
          req(!is.null(m))
          m
        })

        # Welfare distribution ridge plot with standard poverty line annotations
        output$welfare_dist <- renderPlot({
          p <- plot_welfare_dist(survey_data())
          if (is.null(p)) {
            plot.new(); title(main = "Welfare distribution unavailable"); return(invisible(NULL))
          }
          p
        })

        output$outcome_stats <- make_stats_dt(survey_data, variable_list, "outcome")
        output$ind_stats     <- make_stats_dt(survey_data, variable_list, "ind")
        output$hh_stats      <- make_stats_dt(survey_data, variable_list, "hh")
        output$firm_stats    <- make_stats_dt(survey_data, variable_list, "firm")
        output$area_stats    <- make_stats_dt(survey_data, variable_list, "area")

        output$selected_surveys <- DT::renderDT({
          req(selected_surveys())
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

        # Append Survey stats tab to parent tabset
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
              h4("Outcome stats"),              DT::DTOutput(ns("outcome_stats")),
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