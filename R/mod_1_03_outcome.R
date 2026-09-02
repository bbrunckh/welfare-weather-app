#' 1_03_outcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_03_outcome_ui <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanel(
      uiOutput(ns("outcome_ui")),
      uiOutput(ns("currency_ui")),
      uiOutput(ns("poverty_line_ui")),
      uiOutput(ns("outcome_info"))
    ),
    uiOutput(ns("outcome_stats_button_ui"))
  )
}

#' 1_03_outcome Server Functions
#'
#' @param id Module id.
#' @param variable_list Reactive data frame - variable metadata from
#'   `mod_0_overview`.
#' @param survey_data Reactive data frame - loaded survey data from
#'   `mod_1_02_surveystats`.
#' @param map_data Reactive GeoJSON FeatureCollection from
#'   `mod_1_02_surveystats` (H3 map data). Used for outcome coverage map.
#' @param cell_data Reactive list of `geom` (H3 cell geometry) and `map`
#'   (location-to-cell mapping) from `mod_1_02_surveystats`. When present,
#'   coverage is merged onto non-overlapping cells.
#' @param tabset_id Character id of the parent tabset panel.
#' @param tabset_session Shiny session for the parent tabset.
#'
#' @noRd
mod_1_03_outcome_server <- function(id, variable_list, survey_data,
                                    map_data       = reactive(NULL),
                                    cell_data      = reactive(NULL),
                                    survey_version = reactive(0L),
                                    tabset_id      = NULL,
                                    tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    # INT-08: banner when the survey data behind these statistics was
    # reloaded after the button was last pressed.
    output$outcome_stale_banner <- renderUI({
      spec <- outcome_spec()
      if (!is.null(spec) &&
          !identical(survey_version(), spec$survey_version)) {
        .stale_banner(
          "Outcome stats",
          note = "Survey data was reloaded after these statistics were produced."
        )
      } else NULL
    })

    # ---- Available outcome variables present in the survey data -------------

    available_outcomes <- reactive({
      req(variable_list(), survey_data())
      filter_outcome_vars(variable_list(), colnames(survey_data()))
    })

    # ---- Outcome selector UI ------------------------------------------------

    output$outcome_ui <- renderUI({
      req(available_outcomes())
      outs <- available_outcomes()

      choice_labels <- paste0(outs$label, " (", outs$name, ")")
      choice_map <- stats::setNames(outs$name, choice_labels)

      selectizeInput(
        inputId  = ns("outcome"),
        label    = "Outcome variable",
        choices  = choice_map,
        selected = outs$name[1],
        multiple = FALSE
      )
    })

    # ---- Selected outcome info (single row from available_outcomes) ---------

    selected_outcome_info <- reactive({
      req(input$outcome, available_outcomes())
      outs <- available_outcomes()
      outs[outs$name == input$outcome, , drop = FALSE]
    })

    # ---- Currency selector (monetary outcomes only) -------------------------

    output$currency_ui <- renderUI({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      if (nrow(info) == 0) return(NULL)

      if (!is_monetary_outcome(info$name[1], info$units[1])) return(NULL)

      radioButtons(
        inputId  = ns("currency"),
        label    = "Currency",
        choices  = c("PPP (2021)" = "PPP", "LCU (2021)" = "LCU"),
        selected = "PPP"
      )
    })

    # ---- Poverty line input (poor outcome only) -----------------------------

    output$poverty_line_ui <- renderUI({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      if (nrow(info) == 0 || !identical(as.character(info$name[1]), "poor")) return(NULL)

      currency <- input$currency
      default_line <- if (is.null(currency) || identical(currency, "PPP")) {
        3.00
      } else {
        default_lcu_poverty_line(survey_data())
      }

      numericInput(
        inputId = ns("poverty_line"),
        label   = poverty_line_label(currency),
        value   = default_line,
        min     = 0,
        step    = 0.01
      )
    })

    # ---- Informational message about selected outcome type ------------------

    output$outcome_info <- renderUI({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      if (nrow(info) == 0) return(NULL)
      outcome_info_message(info$type[1])
    })

    # ---- Augmented selected outcome row (with transform/units/povline) ------

    selected_outcome <- reactive({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      if (nrow(info) == 0) return(info)
      build_selected_outcome(
        info         = info,
        currency     = input$currency,
        poverty_line = input$poverty_line
      )
    })

    # ---- Outcome Stats button -----------------------------------------------

    output$outcome_stats_button_ui <- renderUI({
      req(input$outcome, survey_data())
      actionButton(ns("outcome_stats_btn"), "Outcome stats",
                   class = "btn-primary", style = "width: 100%;")
    })

    outcome_tab_added <- reactiveVal(FALSE)

    # ---- Button-time selection snapshot (INT-05 pattern) ----------------------
    # The Outcome stats tab must describe the run the button captured, not the
    # live selector: outputs render from `outcome_spec()` and stay stable
    # until the button is pressed again.
    outcome_spec <- reactiveVal(NULL)

    # ---- Survey data augmented with synthetic "poor" column -------------------

    outcome_data <- reactive({
      spec <- outcome_spec()
      req(survey_data(), spec)
      inf <- spec$info
      df <- survey_data()
      oname <- as.character(inf$name[1])
      if (identical(oname, "poor") && !"poor" %in% names(df)) {
        so <- spec$so
        pl <- if (!is.null(so) && !is.na(so$povline)) so$povline else 3.00
        if ("welfare" %in% names(df)) {
          line <- .povline_to_ppp(
            pl, df,
            !is.null(so) && !is.na(so$povline) &&
              identical(as.character(so$units[1]), "LCU")
          )
          df$poor <- as.integer(df$welfare < line)
        }
      }
      df
    })

    # ---- Outcome Stats tab creation -----------------------------------------

    observeEvent(input$outcome_stats_btn, {
      req(input$outcome, survey_data(), selected_outcome_info())

      # Snapshot the current selection; outputs below bind to it so selector
      # changes do not re-render the tab until the button is pressed again.
      outcome_spec(list(info            = selected_outcome_info(),
                        so              = selected_outcome(),
                        survey_version  = survey_version()))

      # Define outputs (once)
      if (!outcome_tab_added()) {

        output$outcome_dist <- renderPlot({
          spec <- outcome_spec()
          req(outcome_data(), spec)
          inf <- spec$info
          p <- plot_welfare_dist(
            outcome_data(),
            outcome = as.character(inf$name[1]),
            label   = as.character(inf$label[1]),
            type    = as.character(inf$type[1])
          )
          if (is.null(p)) {
            plot.new()
            title(main = "Distribution unavailable")
            return(invisible(NULL))
          }
          p
        })

        cov_view_mem <- map_view_memory(input, session, "outcome_coverage_map")
        cov_view_mem$remember()

        output$outcome_coverage_map <- leaflet::renderLeaflet({
          spec <- outcome_spec()
          req(outcome_data(), spec, map_data())
          inf <- spec$info

          # Prefer H3 cells when Survey stats has supplied them: locations
          # overlap, and stacking translucent fills shows shades that mean
          # nothing on the legend.
          wave  <- input$cov_wave %||% "all"
          cd    <- if (is.function(cell_data)) cell_data() else NULL
          # by_wave = FALSE: a wave selection is applied by filtering the
          # inputs, so whatever is drawn must still paint each cell once.
          cmap  <- if (!is.null(cd)) filter_by_wave(cd$map, wave) else NULL
          feats <- if (!is.null(cmap) && nrow(cmap) > 0) {
            build_cell_features(cd$geom, cmap, by_wave = FALSE)
          } else NULL

          m <- plot_outcome_coverage_map(
            geojson  = feats %||% filter_features_by_wave(map_data(), wave),
            df       = filter_by_wave(outcome_data(), wave),
            outcome  = as.character(inf$name[1]),
            cell_map = if (!is.null(feats)) cmap else NULL
          )
          req(!is.null(m))
          cov_view_mem$restore(m)
        })

        # Wave picker, shown only when there is more than one wave to pick.
        output$cov_wave_ui <- shiny::renderUI({
          w <- survey_wave_list(survey_data())
          if (is.null(w) || nrow(w) < 2) return(NULL)
          shiny::selectInput(
            ns("cov_wave"), NULL,
            choices  = c(stats::setNames("all", "All waves"),
                         stats::setNames(w$key, w$label)),
            selected = shiny::isolate(input$cov_wave) %||% "all",
            width    = "160px"
          ) |>
            htmltools::tagAppendAttributes(
              style = "margin-bottom: 0;", class = "small"
            )
        })

        output$outcome_summary_stats <- renderTable({
          spec <- outcome_spec()
          req(outcome_data(), spec)
          inf <- spec$info
          oname_local <- as.character(inf$name[1])
          otype_local <- as.character(inf$type[1])
          vals <- outcome_data()[[oname_local]]
          n_total <- length(vals)
          n_avail <- sum(!is.na(vals))
          n_miss  <- n_total - n_avail
          coverage <- round(100 * n_avail / max(n_total, 1), 1)
          vals <- vals[!is.na(vals)]

          obs_rows <- data.frame(
            Statistic = c("Observations", "Coverage (%)"),
            Value = c(
              format(n_total, big.mark = ","),
              paste0(coverage, "%")
            ),
            stringsAsFactors = FALSE
          )

          if (length(vals) == 0) return(obs_rows)

          if (otype_local == "numeric") {
            vals <- as.numeric(vals)
            stat_rows <- data.frame(
              Statistic = c("Mean", "Median", "Std Dev", "Min",
                            "P10", "P25", "P75", "P90", "Max"),
              Value = as.character(round(c(
                mean(vals), stats::median(vals), stats::sd(vals),
                min(vals),
                stats::quantile(vals, 0.10),
                stats::quantile(vals, 0.25),
                stats::quantile(vals, 0.75),
                stats::quantile(vals, 0.90),
                max(vals)
              ), 3)),
              stringsAsFactors = FALSE
            )
          } else {
            vals <- as.integer(vals)
            n1 <- sum(vals == 1L)
            n0 <- sum(vals == 0L)
            stat_rows <- data.frame(
              Statistic = c("Count = 1 (Yes)", "Count = 0 (No)",
                            "Share = 1"),
              Value = as.character(c(
                format(n1, big.mark = ","),
                format(n0, big.mark = ","),
                round(n1 / max(n1 + n0, 1), 3)
              )),
              stringsAsFactors = FALSE
            )
          }

          rbind(obs_rows, stat_rows)
        }, striped = TRUE, hover = TRUE, bordered = TRUE)

        # Append tab
        tryCatch(
          shiny::appendTab(
            inputId = tabset_id,
            shiny::tabPanel(
              title = "Outcome stats",
              value = "outcome_stats_tab",
              shiny::uiOutput(ns("outcome_stale_banner")),
              bslib::layout_columns(
                col_widths = c(6, 6),
                bslib::card(
                  shiny::h4("Summary statistics"),
                  shiny::p(
                    class = "text-muted small",
                    paste(
                      "Statistics are computed on the pooled sample across",
                      "the selected countries and survey waves. See the",
                      "Survey stats tab for disaggregated statistics by",
                      "country and wave."
                    )
                  ),
                  shiny::tableOutput(ns("outcome_summary_stats"))
                ),
                # full_screen gives the card bslib's expand control; the map
                # fills the card body in both states (see the height pairing
                # below) and re-fits itself on resize.
                bslib::card(
                  full_screen = TRUE,
                  height      = "470px",   # h4 + the map's original 400px
                  shiny::div(
                    class = paste("d-flex align-items-center",
                                  "justify-content-between flex-wrap gap-2 mb-2"),
                    shiny::h4("Spatial coverage", class = "mb-0"),
                    shiny::uiOutput(ns("cov_wave_ui"), inline = TRUE)
                  ),
                  leaflet::leafletOutput(ns("outcome_coverage_map"),
                                         height = "100%")
                )
              ),
              shiny::br(),
              bslib::card(
                shiny::h4("Outcome Distribution (by survey wave)"),
                wise_plot_output(ns("outcome_dist"),
                                 "Histogram of the selected outcome variable in the selected surveys",
                                 height = "300px")
              ),
              tags$div(style = "height: 40px;")
            ),
            select  = TRUE,
            session = tabset_session
          ),
          error = function(e) {
            shiny::showNotification(
              paste("Failed to add Outcome stats tab:",
                    conditionMessage(e)),
              type = "error"
            )
          }
        )

        outcome_tab_added(TRUE)
      }

      if (outcome_tab_added()) {
        try(shiny::updateTabsetPanel(
          tabset_session, inputId = tabset_id,
          selected = "outcome_stats_tab"
        ), silent = TRUE)
      }

    }, ignoreInit = TRUE)

    # ---- Module return API --------------------------------------------------

    list(
      selected_outcome = selected_outcome
    )
  })
}
