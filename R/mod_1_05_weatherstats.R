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
    stored_breaks     <- reactiveVal(NULL)
    # Slim survey x weather frame holding the pre-binning (continuous) values
    # of binned weather variables. Plot-only; never leaves this module.
    survey_weather_cont <- reactiveVal(NULL)
    # Historical weather over a user-chosen year range, restricted to the
    # sample's location x calendar-month cells. Plot-only.
    hist_cells        <- reactiveVal(NULL)
    hist_cells_years  <- reactiveVal(NULL)

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
      ss  <- selected_surveys()

      # -- Load weather -------------------------------------------------------
      notif_load <- showNotification("Loading weather data...", duration = NULL, type = "message")

      weather_full <- tryCatch({
        get_weather(
          survey_data       = svy,
          selected_surveys  = ss,
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
      req(!is.null(weather_full))

      # Cache bin breaks so Step 2 simulation uses identical factor levels
      brks <- attr(weather_full, "stored_breaks")
      if (!is.null(brks)) stored_breaks(brks)

      loc_wd <- weather_full$historical
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

      # Companion frame with the continuous values behind the bins. Merged
      # from a slim slice of the survey data so it stays cheap and leaves
      # `survey_weather()` (used downstream) untouched.
      cont_wd  <- attr(weather_full, "continuous_weather")
      survey_cont <- NULL
      if (!is.null(cont_wd)) {
        survey_cont <- tryCatch(
          merge_survey_weather(
            svy |> dplyr::select(dplyr::any_of(c(
              "code", "year", "survname", "loc_id", "timestamp",
              "economy", "weight"
            ))),
            cont_wd
          ),
          error = function(e) NULL
        )
      }
      survey_weather_cont(survey_cont)

      # Any historical comparison on screen belongs to the previous weather
      # configuration. Drop the stale cells now, remember the year range, and
      # rebuild it under the new configuration at the end of this observer.
      hist_reload_years <- hist_cells_years()
      hist_cells(NULL)

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

        # -- Continuous distribution behind a binned variable -----------------
        # Only rendered for binned variables (see `weather_dist_layout`); the
        # values are the same transformed series the bins were cut from, so a
        # deviation-from-mean / anomaly configuration carries through.

        make_weather_dist_cont <- function(idx) {
          renderPlot({
            req(survey_weather_cont())
            df    <- survey_weather_cont() |>
              dplyr::mutate(countryyear = paste0(economy, ", ", year))
            sw    <- isolate(selected_weather())
            hv    <- sw$name[idx]
            label <- sw$label[idx]

            p <- plot_weather_dist(df, hv, label, "Continuous")
            if (is.null(p)) {
              plot.new(); title(main = "Continuous distribution unavailable")
              return(invisible(NULL))
            }
            p
          })
        }

        output$weather_dist_cont1 <- make_weather_dist_cont(1)
        output$weather_dist_cont2 <- make_weather_dist_cont(2)

        # -- Historical vs sample weather -------------------------------------
        # Year-range menu lives in the panel itself; defaults to the 20 years
        # ending at the latest year covered by the selected survey waves.

        output$hist_year_ui <- shiny::renderUI({
          shiny::req(survey_data())
          rng <- default_hist_year_range(extract_survey_dates(survey_data()))
          shiny::req(rng)
          this_year <- as.integer(format(Sys.Date(), "%Y"))

          bslib::layout_columns(
            col_widths = c(3, 3, 6),
            shiny::numericInput(
              ns("hist_year_from"), "From year",
              value = rng[["from"]], min = 1950, max = this_year, step = 1
            ),
            shiny::numericInput(
              ns("hist_year_to"), "To year",
              value = rng[["to"]], min = 1950, max = this_year, step = 1
            ),
            shiny::div(
              style = "display: flex; align-items: flex-end; height: 100%;",
              shiny::actionButton(
                ns("hist_load"), "Load historical weather", class = "btn-primary"
              )
            )
          )
        })

        make_hist_vs_sample <- function(idx) {
          renderPlot({
            req(hist_cells(), hist_cells_years())
            sw  <- isolate(selected_weather())
            yrs <- hist_cells_years()

            p <- plot_hist_vs_sample(
              cells_df  = hist_cells(),
              hv        = sw$name[idx],
              label     = sw$label[idx],
              year_from = yrs[["from"]],
              year_to   = yrs[["to"]]
            )
            if (is.null(p)) {
              plot.new(); title(main = "No historical weather to plot")
              return(invisible(NULL))
            }
            p
          })
        }

        output$hist_vs_sample1 <- make_hist_vs_sample(1)
        output$hist_vs_sample2 <- make_hist_vs_sample(2)

        output$hist_vs_sample_layout <- shiny::renderUI({
          cells <- hist_cells()
          if (is.null(cells)) {
            return(shiny::tagList(
              shiny::helpText(
                paste("Pick a year range above and click 'Load historical",
                      "weather' to compare the survey wave against its own",
                      "climate history."),
                style = "font-size: 12px; margin-bottom: 0;"
              ),
              shiny::div(style = "height: 24px;")
            ))
          }

          # Give the facets (one per survey wave) room to breathe.
          n_waves <- length(unique(cells$countryyear))
          n_rows  <- ceiling(n_waves / max(1L, ceiling(sqrt(n_waves))))

          weather_plot_layout(
            ns, nrow(selected_weather() %||% data.frame()),
            ids    = c("hist_vs_sample1", "hist_vs_sample2"),
            height = paste0(max(320, 240 * n_rows), "px")
          )
        })

        # -- Binscatter plots (one per variable) ------------------------------

        make_binscatter <- function(idx) {
          renderPlot({
            req(survey_weather(), selected_outcome())
            so  <- selected_outcome()
            df <- survey_weather() |> prepare_outcome_df(so)
            sw  <- isolate(selected_weather())
            
            # fix so$label for plotting if it has been transformed
            if ("transform" %in% colnames(so) && isTRUE(so$transform == "log")) {
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

        # -- Summary stats tables (continuous + binned) -----------------------
        output$weather_stats_table <- make_weather_stats_dt(
          survey_weather   = survey_weather,
          selected_weather = selected_weather
        )
        output$weather_stats_table_binned <- make_weather_binned_stats_dt(
          survey_weather   = survey_weather,
          selected_weather = selected_weather
        )

        # Single panel that conditionally shows the continuous table, the
        # binned table, or both — based on each selected weather variable's
        # type in the merged survey-weather frame.
        output$weather_stats_layout <- shiny::renderUI({
          shiny::req(survey_weather(), selected_weather())
          df   <- survey_weather()
          sw   <- selected_weather()
          vars <- intersect(sw$name, names(df))
          if (length(vars) == 0) {
            return(shiny::helpText("No weather variables found."))
          }

          is_num <- vapply(df[vars], is.numeric, logical(1))
          has_continuous <- any(is_num)
          has_binned     <- any(!is_num)

          shiny::tagList(
            if (has_continuous) shiny::tagList(
              shiny::helpText(
                "Continuous variables — weighted summary per country-year.",
                style = "font-size: 12px;"
              ),
              DT::DTOutput(ns("weather_stats_table"))
            ),
            if (has_continuous && has_binned) shiny::br(),
            if (has_binned) shiny::tagList(
              shiny::helpText(
                paste("Binned variables — count and share of observations",
                      "in each bin per country-year."),
                style = "font-size: 12px;"
              ),
              DT::DTOutput(ns("weather_stats_table_binned"))
            )
          )
        })

        # -- Selected weather config table ------------------------------------

        output$selected_weather <- DT::renderDT({
          selected_weather()
        },
        rownames = FALSE,
        options  = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
        class    = "compact")

        # -- Append tab -------------------------------------------------------

        # Reactive layouts so panels update when the user toggles between
        # 1 and 2 weather variables without re-creating the tab.
        output$weather_dist_layout <- shiny::renderUI({
          sw     <- selected_weather() %||% data.frame()
          n_vars <- nrow(sw)
          dist_ids <- c("weather_dist1", "weather_dist2")
          cont_ids <- c("weather_dist_cont1", "weather_dist_cont2")
          cont_df  <- survey_weather_cont()

          # A binned variable gets its bar chart supplemented with the
          # continuous distribution underneath; continuous variables are
          # already shown as such and need no supplement.
          is_binned <- function(i) {
            isTRUE(sw$cont_binned[i] == "Binned") &&
              !is.null(cont_df) &&
              isTRUE(sw$name[i] %in% names(cont_df)) &&
              is.numeric(cont_df[[sw$name[i]]])
          }

          if (!any(vapply(seq_len(n_vars), is_binned, logical(1)))) {
            return(weather_plot_layout(
              ns, n_vars, ids = dist_ids, height = "300px"
            ))
          }

          var_panel <- function(i) {
            items <- list(shiny::plotOutput(ns(dist_ids[i]), height = "300px"))
            if (is_binned(i)) {
              items <- c(items, list(
                shiny::helpText(
                  "Above: Binned weather distribution as configured. Below: Continuous distribution the bins were derived from.",
                  style = "font-size: 12px;"
                ),
                shiny::plotOutput(ns(cont_ids[i]), height = "300px")
              ))
            }
            do.call(bslib::card, items)
          }

          if (n_vars >= 2) {
            bslib::layout_columns(
              col_widths = c(6, 6), var_panel(1), var_panel(2)
            )
          } else {
            var_panel(1)
          }
        })
        output$binscatter_layout <- shiny::renderUI({
          weather_plot_layout(
            ns, nrow(selected_weather() %||% data.frame()),
            ids    = c("binscatter1", "binscatter2"),
            height = "300px"
          )
        })

        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Weather stats",
            value = "weather_desc",
            shiny::h4(
              "Distribution of weather (household survey sample)",
              info_popover(
                p(paste(
                  "Distribution of each selected weather variable across the",
                  "survey sample, weighted by survey weights where available.",
                  "Note that they need not be of the same time of year or location, ",
                  "so the distributions need not match each other.",
                  "Use below Distribution of Weather (historical versus sample) to ",
                  "compare the sample against its own climate history."
                ))
              )
            ),
            shiny::uiOutput(ns("weather_dist_layout")),
            shiny::br(),
            shiny::h4(
              "Distribution of weather (historical versus sample)",
              info_popover(
                p(paste(
                  "Weather over a longer run of years for the same locations",
                  "and the same calendar months the survey was fielded in,",
                  "overlaid with the weather the sample actually experienced.",
                  "Each variable is shown on its configured scale (raw,",
                  "deviation from mean, standardised anomaly) and always as a",
                  "continuous distribution, even when the variable is binned",
                  "for modelling. Cells are weighted by the number of sampled",
                  "households behind them, so both distributions are composed",
                  "the same way."
                ))
              )
            ),
            shiny::uiOutput(ns("hist_year_ui")),
            shiny::uiOutput(ns("hist_vs_sample_layout")),
            shiny::br(),
            shiny::h4(
              "Outcome vs weather",
              info_popover(
                p(paste(
                  "Binned scatter of the outcome variable against each",
                  "selected weather variable, useful for spotting non-linear",
                  "relationships before modelling."
                ))
              )
            ),
            shiny::uiOutput(ns("binscatter_layout")),
            shiny::hr(),
            shiny::h4(
              "Weather summary stats",
              info_popover(
                p(paste(
                  "Weighted summary statistics for each weather variable,",
                  "aggregated per country-year."
                ))
              )
            ),
            shiny::uiOutput(ns("weather_stats_layout")),
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

      # Rebuild the historical comparison for the year range already in use,
      # so a re-configuration refreshes every panel on the tab rather than
      # sending the user back to the "Load historical weather" button.
      if (!is.null(hist_reload_years)) {
        load_hist_weather(hist_reload_years[["from"]],
                          hist_reload_years[["to"]])
      }

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ---- Historical weather over a user-chosen year range --------------------

    # Loads historical weather for [yf, yt] and rebuilds the comparison cells.
    # Shared by the "Load historical weather" button and by the automatic
    # refresh that follows a change of weather configuration.
    #
    # This is a second, full `get_weather()` pass: the survey-period load only
    # covers the wave's own months, so the extra years have to be aggregated,
    # rolled and transformed here regardless of what was loaded before.
    load_hist_weather <- function(yf, yt) {
      svy <- survey_data()
      ss  <- selected_surveys()
      sw  <- selected_weather()
      swd <- survey_weather()
      if (is.null(svy) || is.null(ss) || is.null(sw) || is.null(swd)) {
        return(invisible(FALSE))
      }

      # Same months and locations as the survey, more years. The temporal
      # aggregation window configured for each variable is applied by
      # get_weather() relative to each of these timestamps.
      dates <- expand_hist_dates(extract_survey_dates(svy), yf, yt)
      if (length(dates) == 0) return(invisible(FALSE))

      # Always continuous here — binning is a modelling choice, this section
      # compares distributions. Everything else (temporal aggregation,
      # deviation from mean, standardised anomaly) is left as configured.
      sw_cont <- sw
      if ("cont_binned" %in% names(sw_cont)) sw_cont$cont_binned <- "Continuous"

      notif <- showNotification(
        sprintf("Loading historical weather %d-%d...", yf, yt),
        duration = NULL, type = "message"
      )

      hist_res <- tryCatch({
        get_weather(
          survey_data       = svy,
          selected_surveys  = ss,
          selected_weather  = sw_cont,
          dates             = dates,
          connection_params = connection_params()
        )
      }, error = function(e) {
        shiny::showNotification(
          paste("Failed to load historical weather:", conditionMessage(e)),
          type = "error", duration = 8
        )
        NULL
      })

      removeNotification(notif)
      if (is.null(hist_res$historical)) return(invisible(FALSE))

      cells <- join_hist_sample_cells(hist_res$historical, swd)
      if (is.null(cells) || nrow(cells) == 0) {
        showNotification(
          paste("No historical weather matched the survey's locations and",
                "months for the selected years."),
          type = "warning", duration = 8
        )
        return(invisible(FALSE))
      }

      hist_cells(cells)
      hist_cells_years(c(from = yf, to = yt))
      showNotification("Historical weather ready.", duration = 3,
                       type = "message")
      invisible(TRUE)
    }

    observeEvent(input$hist_load, {
      req(selected_weather(), selected_surveys(), survey_data(),
          survey_weather())

      yf <- suppressWarnings(as.integer(input$hist_year_from))
      yt <- suppressWarnings(as.integer(input$hist_year_to))
      if (is.na(yf) || is.na(yt)) {
        showNotification("Enter both a start and an end year.",
                         type = "warning", duration = 5)
        return()
      }
      if (yf > yt) {
        tmp <- yf
        yf  <- yt
        yt  <- tmp
      }

      load_hist_weather(yf, yt)

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ---- Return API ---------------------------------------------------------

    list(survey_weather = survey_weather,
         stored_breaks  = stored_breaks)
  })
}
