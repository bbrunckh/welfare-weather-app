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
#' @param id               Module id.
#' @param connection_params Reactive named list from mod_0_overview.
#' @param variable_list    Reactive data frame of variable metadata.
#' @param selected_surveys Reactive data frame of selected surveys.
#' @param selected_outcome Reactive data frame row of the selected outcome.
#' @param selected_weather Reactive data frame of selected weather spec.
#' @param survey_data      Reactive data frame of loaded survey observations.
#' @param tabset_id        Character. `inputId` of the parent tabset panel.
#' @param tabset_session   Shiny session for the parent tabset. Defaults to
#'   `session$parent`.
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

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    weather_tab_added <- reactiveVal(FALSE)
    survey_weather    <- reactiveVal(NULL)

    # ---- Weather stats button -----------------------------------------------

    output$weather_stats_button_ui <- renderUI({
      req(selected_weather())
      actionButton(
        ns("weather_stats"), "Weather stats",
        class = "btn-primary", style = "width: 100%;"
      )
    })
    shiny::outputOptions(output, "weather_stats_button_ui", suspendWhenHidden = FALSE)

    # ---- Load and merge weather on button click ------------------------------

    observeEvent(input$weather_stats, {
      req(selected_weather(), selected_surveys(), survey_data())

      sw  <- selected_weather()
      svy <- survey_data()

      # -- Load weather -------------------------------------------------------
      notif_load <- showNotification("Loading weather data...", duration = NULL, type = "message")

      loc_wd <- tryCatch({
        get_weather(
          survey_data       = svy,
          selected_weather  = sw,
          dates             = extract_survey_dates(svy),
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

      # -- Merge with survey data ---------------------------------------------
      notif_merge <- showNotification(
        "Merging survey and weather data...", duration = NULL, type = "message"
      )

      survey_wd <- tryCatch({
        merge_survey_weather(svy, loc_wd)
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

      # ---- Define outputs once then add tab ---------------------------------

      if (!weather_tab_added()) {

        # -- Weather distribution plots (one per variable) -------------------

        make_weather_dist <- function(idx) {
          renderPlot({
            req(survey_weather())
            df          <- survey_weather() |>
              dplyr::mutate(countryyear = paste0(economy, ", ", year))
            sw          <- isolate(selected_weather())
            hv          <- sw$name[idx]
            label       <- sw$label[idx]
            cont_binned <- sw$cont_binned[idx]

            p <- plot_weather_dist(df, hv, label, cont_binned)
            if (is.null(p)) {
              plot.new(); title(main = "Weather variable not configured")
              return(invisible(NULL))
            }
            p
          })
        }

        output$weather_dist1 <- make_weather_dist(1)
        output$weather_dist2 <- make_weather_dist(2)

        # -- Binscatter plots (one per variable) ------------------------------

        make_binscatter <- function(idx) {
          renderPlot({
            req(survey_weather(), selected_outcome())
            so  <- selected_outcome()
            df <- survey_weather() |> prepare_outcome_df(so)
            sw  <- isolate(selected_weather())
            
            # fix so$label for plotting if it has been transformed
            if (!"transform" %in% colnames(so) && so$transform == "log") {
              so$label <- paste0("Log ", so$label)
            }

            p <- plot_binscatter(
              df       = df,
              hv       = sw$name[idx],
              hv_label = paste0(sw$label[idx], "\n(as configured)"),
              y_var    = so$name,
              y_label  = so$label
            )

            if (is.null(p)) {
              plot.new()
              title(main = "Weather variable not configured")
              return(invisible(NULL))
            }
            p
          })
        }

        output$binscatter1 <- make_binscatter(1)
        output$binscatter2 <- make_binscatter(2)

        # -- Summary stats table ----------------------------------------------
        output$weather_stats_table <- make_weather_stats_dt(
          survey_weather   = survey_weather,
          selected_weather = selected_weather
        )

        # -- Selected weather config table ------------------------------------

        output$selected_weather <- DT::renderDT({
          selected_weather()
        },
        rownames = FALSE,
        options  = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
        class    = "compact")

        # -- Append tab -------------------------------------------------------

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
        try(
          shiny::updateTabsetPanel(
            tabset_session, inputId = tabset_id, selected = "weather_desc"
          ),
          silent = TRUE
        )
      }

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ---- Return API ---------------------------------------------------------

    list(survey_weather = survey_weather)
  })
}
