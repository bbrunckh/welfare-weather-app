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
      uiOutput(ns("poverty_line_ui"))
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
#' @param cell_data Reactive list of `geom` (H3 cell geometry) and `map`
#'   (location-to-cell mapping) from `mod_1_02_surveystats`. When present,
#'   coverage is merged onto non-overlapping cells.
#' @param tabset_id Character id of the parent tabset panel.
#' @param tabset_session Shiny session for the parent tabset.
#'
#' @noRd
mod_1_03_outcome_server <- function(id, variable_list, survey_data,
                                    cell_data = reactive(NULL),
                                    survey_version = reactive(0L),
                                    survey_data_generation = survey_version,
                                    tabset_id = NULL,
                                    tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    survey_wave_meta <- shiny::reactive({
      cached_survey_wave_metadata(
        session, shiny::isolate(survey_data()), survey_data_generation()
      )
    })

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
      } else {
        NULL
      }
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

      choice_map <- stats::setNames(outs$name, outs$label)

      selectizeInput(
        inputId = ns("outcome"),
        label = tagList(
          "Outcome variable",
          info_popover(
            shiny::p("Continuous outcomes will be log-transformed.")
          )
        ),
        choices = choice_map,
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
      if (nrow(info) == 0) {
        return(NULL)
      }

      if (!is_monetary_outcome(info$name[1], info$units[1])) {
        return(NULL)
      }

      pill_toggle(
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
      if (nrow(info) == 0 || !identical(as.character(info$name[1]), "poor")) {
        return(NULL)
      }

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

    lapply(c("outcome_ui", "currency_ui", "poverty_line_ui"), function(out_id) {
      shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
    })

    # ---- Augmented selected outcome row (with transform/units/povline) ------

    selected_outcome <- reactive({
      req(selected_outcome_info())
      info <- selected_outcome_info()
      if (nrow(info) == 0) {
        return(info)
      }
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
        class = "btn-primary", style = "width: 100%; margin-top: 0.6rem;"
      )
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

    # ---- Outcome summary statistics (PERF-41) ---------------------------------
    # The summary table covers every wave plus the pooled sample in one
    # grouped pass per captured run; the wave pill re-slices the precomputed
    # matrix instead of re-running the quantiles.
    outcome_summary <- reactive({
      spec <- outcome_spec()
      shiny::req(survey_data(), spec)
      od <- outcome_data()
      inf <- spec$info
      outcome_summary_wide(
        od,
        as.character(inf$name[1]),
        as.character(inf$type[1])
      )
    })

    # ---- Outcome Stats tab creation -----------------------------------------

    observeEvent(input$outcome_stats_btn,
      {
        req(input$outcome, survey_data(), selected_outcome_info())

        # Snapshot the current selection; outputs below bind to it so selector
        # changes do not re-render the tab until the button is pressed again.
        outcome_spec(list(
          info = selected_outcome_info(),
          so = selected_outcome(),
          survey_version = survey_version()
        ))

        # Define outputs (once)
        if (!outcome_tab_added()) {
          # ---- Selection summary card (snapshot, INT-05 pattern) ---------------
          # Describes the outcome the button captured, like every other output
          # on this tab; selector changes do not re-render it until re-press.

          output$selected_outcome_card <- renderUI({
            spec <- outcome_spec()
            req(!is.null(spec), nrow(spec$info) > 0)
            inf <- spec$info
            so <- spec$so
            otype <- tolower(as.character(inf$type[1]))
            oname <- as.character(inf$name[1])
            units <- as.character(so$units[1])

            badge <- if (identical(otype, "numeric")) "Continuous" else "Binary"

            pills <- character(0)
            if (identical(oname, "poor")) {
              pl <- suppressWarnings(as.numeric(so$povline[1]))
              if (length(pl) == 1 && is.finite(pl) && pl > 0) {
                s <- formatC(pl, format = "f", digits = 2, big.mark = ",")
                suffix <- if (identical(units, "LCU")) " LCU/day" else "/day"
                pills <- c(pills, paste0(
                  "Poverty line ",
                  if (identical(units, "LCU")) "" else "$",
                  s, suffix
                ))
              }
            } else if (is_monetary_outcome(oname, units)) {
              pills <- c(pills, paste0(units, " (2021)"))
            }
            if (isTRUE(so$transform[1] == "log")) {
              pills <- c(pills, "log-transformed")
            }

            selection_summary_card(
              title = "Selected outcome",
              badge = badge,
              rows = list(list(
                name  = as.character(inf$label[1]),
                sub   = oname,
                pills = pills,
                note  = outcome_direction_note(so$direction[1])
              ))
            )
          })

          outcome_dist_fig <- function() {
            spec <- outcome_spec()
            od <- outcome_data()
            if (is.null(spec) || is.null(od)) {
              return(NULL)
            }
            inf <- spec$info
            plot_welfare_dist(
              od,
              outcome = as.character(inf$name[1]),
              label = as.character(inf$label[1]),
              type = as.character(inf$type[1]),
              wave_labels = survey_wave_meta()$plot_labels
            )
          }

          wise_export_figure(
            key = "outcome_distribution",
            label = "Outcome distribution",
            step = 1L,
            fun = outcome_dist_fig,
            description = paste(
              "Distribution of the selected welfare outcome over the pooled",
              "sample."
            ),
            width = 9, height = 6
          )

          output$outcome_dist <- renderPlot({
            spec <- outcome_spec()
            req(outcome_data(), spec)
            inf <- spec$info
            p <- plot_welfare_dist(
              outcome_data(),
              outcome = as.character(inf$name[1]),
              label = as.character(inf$label[1]),
              type = as.character(inf$type[1]),
              wave_labels = survey_wave_meta()$plot_labels
            )
            if (is.null(p)) {
              blank_plot("Distribution unavailable")
              return(invisible(NULL))
            }
            p
          })

          # ---- MapLibre coverage payload stream ---------------------------------
          # One reactive observer drives the hex map: survey/outcome loads and
          # wave toggles land here as fresh `set` payloads. The camera is fitted
          # only when the data key changes (PERF-36 view-key semantics), so a
          # wave toggle re-colours in place and the user's pan/zoom survives.
          cov_key <- shiny::reactiveVal(NULL)
          cov_lgd <- shiny::reactiveVal(NULL)

          # The view chosen by the picker above the map: what those sampled
          # units report on average (mean value), or what share of the sample
          # at each location has a non-missing outcome (coverage). Mean value
          # leads the picker and is the default.
          cov_view_val <- shiny::reactiveVal("mean")
          cov_view <- shiny::reactive(cov_view_val())

          observeEvent(input$cov_view,
            {
              v <- input$cov_view
              if (!is.null(v)) cov_view_val(v)
            },
            ignoreInit = TRUE,
            ignoreNULL = TRUE
          )

          observe({
            spec <- outcome_spec()
            cd <- if (is.function(cell_data)) cell_data() else NULL
            wave <- input$cov_wave %||% "all"
            view <- cov_view()

            pl <- NULL
            if (!is.null(spec) && !is.null(cd)) {
              od <- outcome_data()
              cmap <- if (!is.null(cd$map)) filter_by_wave(cd$map, wave) else NULL
              if (!is.null(od) && !is.null(cmap) && nrow(cmap) > 0) {
                if (identical(view, "mean")) {
                  pl <- .outcome_mean_hex_payload(
                    cd$geom, cmap, filter_by_wave(od, wave),
                    as.character(spec$info$name[1]),
                    as.character(spec$info$type[1])
                  )
                } else {
                  pl <- .coverage_hex_payload(
                    cd$geom, cmap, filter_by_wave(od, wave),
                    as.character(spec$info$name[1])
                  )
                }
              }
            }

            if (is.null(pl)) {
              hexmap_clear(session, ns, "coverage_map")
              cov_lgd(NULL)
            } else {
              hexmap_update(session, ns, "coverage_map", pl$payload)
              # Refit only when the selection, view, wave or cell footprint
              # changes; wave re-colours keep pan/zoom.
              key <- digest::digest(list(
                spec, wave, view, sort(unique(cmap$h3))
              ))
              if (!identical(key, shiny::isolate(cov_key()))) {
                hexmap_fit(session, ns, "coverage_map", pl$payload$bounds)
                cov_key(key)
              }
              cov_lgd(pl$legend)
            }
          })

          # R-side legend: same palette state as the payloads, rebuilt per wave
          # and positioned over the map's top-right corner by hexmap_ui().
          output$cov_legend_ui <- shiny::renderUI({
            lgd <- cov_lgd()
            shiny::req(!is.null(lgd))
            htmltools::HTML(.compact_legend_html(
              pal_info = lgd$pal_info,
              binned   = lgd$binned,
              title    = lgd$title,
              info     = lgd$info
            ))
          })

          # Map surface: the MapLibre hex map once cell geography exists, with
          # an explanatory notice while it does not (INT-06: a failed load
          # leaves no map to show).
          output$cov_surface_ui <- shiny::renderUI({
            cd <- if (is.function(cell_data)) cell_data() else NULL
            if (is.null(cd)) {
              shiny::tags$div(
                class = paste(
                  "text-muted small d-flex align-items-center",
                  "justify-content-center text-center"
                ),
                style = "height: 100%;",
                shiny::p(paste(
                  "No H3 cell geography is available for this selection.",
                  "Load surveys in Survey stats to draw the coverage map."
                ))
              )
            } else {
              hexmap_ui(
                ns("coverage_map"),
                height = "100%",
                aria_label = paste(
                  "Map of outcome values: availability of the outcome and mean",
                  "values across sampled units, per hexagonal area cell"
                ),
                legend = shiny::uiOutput(ns("cov_legend_ui"))
              )
            }
          })

          # View picker above the map, applying to it alone. The mean-value
          # view leads the picker; its caveat lives in the legend's hover text
          # (sample statistics, not population-representative).
          output$cov_view_ui <- shiny::renderUI({
            pill_toggle(
              inputId  = ns("cov_view"),
              choices  = c("Mean value" = "mean", "Coverage" = "coverage"),
              selected = cov_view_val()
            )
          })

          # Wave toggle slider, shown only when there is more than one wave to pick.
          output$cov_wave_ui <- shiny::renderUI({
            w <- survey_wave_meta()$waves
            if (is.null(w) || nrow(w) < 2) {
              return(NULL)
            }
            choices <- wave_slider_choices(w, include_all = TRUE)
            selected <- shiny::isolate(input$cov_wave) %||% "all"
            if (!selected %in% choices) selected <- "all"
            wave_toggle_slider(
              ns("cov_wave"),
              choices  = choices,
              selected = selected
            )
          })

          # Wave pill for the summary table, styled like the map's view picker
          # and labelled like the map's wave toggle: "All" plus "CODE year"
          # when several countries are sampled, "year" alone otherwise.
          # Hidden when there is only one wave to pick.
          summary_wave_val <- shiny::reactiveVal("all")

          observeEvent(input$summary_wave,
            {
              v <- input$summary_wave
              if (!is.null(v)) summary_wave_val(v)
            },
            ignoreInit = TRUE,
            ignoreNULL = TRUE
          )

          output$summary_wave_ui <- shiny::renderUI({
            w <- survey_wave_meta()$waves
            if (is.null(w) || nrow(w) < 2) {
              return(NULL)
            }
            choices <- wave_slider_choices(w, include_all = TRUE)
            selected <- shiny::isolate(summary_wave_val())
            if (!selected %in% choices) selected <- "all"
            pill_toggle(
              inputId  = ns("summary_wave"),
              choices  = choices,
              selected = selected
            )
          })

          # Heading names the captured outcome and carries the descriptive
          # note in an (i) popout, like the Survey stats headings.
          output$summary_heading_ui <- shiny::renderUI({
            spec <- outcome_spec()
            shiny::req(spec, nrow(spec$info) > 0)
            inf <- spec$info
            nm <- as.character(inf$label[1])
            if (length(nm) != 1 || is.na(nm) || !nzchar(nm)) {
              nm <- as.character(inf$name[1])
            }
            shiny::h4(
              paste0(nm, " summary stats"),
              class = "mb-0",
              info_popover(
                title = paste0(nm, " summary stats"),
                shiny::p(paste(
                  "Detailed statistics for the selected outcome. Use the",
                  "selector to switch between the pooled sample (All) and",
                  "individual survey waves; deciles are shown for continuous",
                  "outcomes. Observations, Missing and Coverage are raw row",
                  "counts; Mean, Std Dev, deciles and Share = 1 are",
                  "sample-weighted with the survey weight. See the Survey",
                  "stats tab for variable-level statistics by country and",
                  "wave."
                ))
              )
            )
          })

          output$outcome_summary_stats <- renderTable(
            {
              spec <- outcome_spec()
              s <- outcome_summary()
              shiny::req(spec, s)
              .format_outcome_summary(s, summary_wave_val())
            },
            striped = TRUE,
            hover = TRUE,
            bordered = TRUE
          )

          # UI-45/UI-48: export the same precomputed, wave-selectable summary
          # shown on screen rather than re-deriving a live pooled table.
          outcome_summary_df <- function() {
            spec <- outcome_spec()
            s <- outcome_summary()
            req(spec, s)
            .format_outcome_summary(s, summary_wave_val())
          }
          output$outcome_summary_csv <- csv_download_handler(
            "outcome_summary", outcome_summary_df,
            stale = function() FALSE
          )
          wise_export_table(
            key = "outcome_summary",
            label = "Outcome summary statistics",
            step = 1L,
            fun = outcome_summary_df,
            description = paste(
              "Summary statistics for the selected outcome, with the same",
              "wave selection shown in the Outcome stats table."
            )
          )

          # Append tab
          tryCatch(
            shiny::appendTab(
              inputId = tabset_id,
              shiny::tabPanel(
                title = "Outcome stats",
                value = "outcome_stats_tab",
                shiny::uiOutput(ns("outcome_stale_banner")),
                uiOutput(ns("selected_outcome_card")),
                # Keep the visual overview together: the distribution and map
                # share the top row, with pooled summary statistics below.
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  # gap = 0: bslib's default body gap would otherwise put
                  # 24px between the heading and the plot; the heading's own
                  # margin is the spacing that remains.
                  bslib::card(
                    bslib::card_body(
                      gap = 0,
                      shiny::h4(
                        "Outcome distribution",
                        info_popover(
                          title = "Outcome distribution",
                          p(paste(
                            "Distribution of the selected outcome in the",
                            "selected surveys. Continuous outcomes appear as",
                            "densities, one ridge per country, on the log",
                            "scale when all values are positive - for welfare,",
                            "dashed lines mark the $3.00, $4.20 and $8.30",
                            "poverty lines. Binary outcomes appear as No/Yes",
                            "shares per survey wave."
                          ))
                        )
                      ),
                      wise_plot_output(
                        ns("outcome_dist"),
                        "Distribution of the selected outcome variable in the selected surveys",
                        height = "400px"
                      )
                    )
                  ),
                  # full_screen gives the card bslib's expand control; the map
                  # fills the card body in both states and re-fits itself on resize.
                  # The explicit card_body with gap = 0: bslib's default body
                  # gap would otherwise put 24px between the controls row and
                  # the map; the controls' own mb-2 is the spacing that
                  # remains.
                  bslib::card(
                    full_screen = TRUE,
                    height = "470px",
                    bslib::card_body(
                      gap = 0,
                      shiny::div(
                        class = paste(
                          "d-flex align-items-center",
                          "justify-content-between flex-wrap gap-2 mb-2"
                        ),
                        shiny::h4(
                          "Map",
                          info_popover(
                            title = "Map",
                            p(paste(
                              "Geographic distribution of the selected",
                              "outcome. Each hexagon is an H3 cell shaded by",
                              "the share of sampled units reporting the",
                              "outcome (Coverage) or their mean value (Mean",
                              "value); a location's units spread across the",
                              "cells it covers in proportion to each cell's",
                              "2020 population. Pick the view and the survey",
                              "wave on the right."
                            ))
                          )
                        ),
                        shiny::uiOutput(ns("cov_view_ui"), inline = TRUE),
                        shiny::uiOutput(ns("cov_wave_ui"), inline = TRUE)
                      ),
                      # The MapLibre hex map, the Leaflet fallback when the
                      # browser reports WebGL as unavailable, or a notice while
                      # no cell geography exists.
                      shiny::uiOutput(ns("cov_surface_ui")) |>
                        bslib::as_fill_carrier()
                    )
                  )
                ),
                bslib::layout_columns(
                  col_widths = 12,
                  # The explicit card_body with gap = 0: bslib's default body
                  # gap would otherwise put 24px between the controls row and
                  # the table; the controls' own mb-2 is the spacing that
                  # remains.
                  bslib::card(
                    bslib::card_body(
                      gap = 0,
                      shiny::div(
                        class = paste(
                          "d-flex align-items-center",
                          "justify-content-between flex-wrap gap-2 mb-2"
                        ),
                        shiny::uiOutput(ns("summary_heading_ui"), inline = TRUE),
                        shiny::div(
                          class = "d-flex align-items-center gap-2",
                          csv_download_link(ns("outcome_summary_csv")),
                          shiny::uiOutput(ns("summary_wave_ui"), inline = TRUE)
                        )
                      ),
                      shiny::tableOutput(ns("outcome_summary_stats"))
                    )
                  )
                ),
                tags$div(style = "height: 40px;")
              ),
              select = TRUE,
              session = tabset_session
            ),
            error = function(e) {
              shiny::showNotification(
                paste(
                  "Failed to add Outcome stats tab:",
                  conditionMessage(e)
                ),
                type = "error"
              )
            }
          )

          outcome_tab_added(TRUE)
        }

        if (outcome_tab_added()) {
          try(shiny::updateTabsetPanel(
            tabset_session,
            inputId = tabset_id,
            selected = "outcome_stats_tab"
          ), silent = TRUE)
        }
      },
      ignoreInit = TRUE
    )

    # ---- Module return API --------------------------------------------------

    list(
      selected_outcome = selected_outcome
    )
  })
}
