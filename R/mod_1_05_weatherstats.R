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
#' @param hist_years       Reactive named integer vector `c(from = , to = )`
#'   from `mod_1_04_weather_server()`, bounding the historical comparison.
#'   Defaults to 1991-2020 when not supplied.
#' @param survey_data      Reactive data frame of loaded survey observations.
#' @param map_data         Reactive GeoJSON FeatureCollection of survey
#'   locations from `mod_1_02_surveystats_server()`. Optional - the
#'   weather-by-location maps are skipped when it is `NULL`.
#' @param cell_data        Reactive list of `geom` (H3 cell geometry) and
#'   `map` (location-to-cell mapping) from `mod_1_02_surveystats_server()`.
#'   When present, values are merged onto H3 cells so overlapping survey
#'   locations no longer stack translucent fills on top of each other.
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
    hist_years = NULL,
    survey_data,
    map_data = NULL,
    cell_data = NULL,
    survey_version = reactive(0L),
    tabset_id,
    tabset_session = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session
    if (is.null(hist_years)) {
      hist_years <- shiny::reactive(c(from = 1991L, to = 2020L))
    }

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

    # REACT-02: double-click guard - one weather load at a time.
    load_guard <- .busy_guard(session, weather_stats)

    # ---- Button-time selection snapshot (INT-05 pattern) ----------------------
    # Every panel on the Weather stats tab renders from `wx_spec()`, captured
    # when the button is pressed: changing the weather-variable or outcome
    # selectors afterwards re-renders nothing until the button is used again.
    wx_spec     <- reactiveVal(NULL)
    wx_spec_sw  <- shiny::reactive({ req(wx_spec()); wx_spec()$sw })
    wx_spec_so  <- shiny::reactive({ req(wx_spec()); wx_spec()$so })
    # REACT-03: digest of the last successfully completed weather load.
    last_wx_load_sig <- reactiveVal(NULL)

    # INT-08: banner when the survey data behind the weather load was
    # reloaded after the button was last pressed.
    output$wx_stale_banner <- shiny::renderUI({
      spec <- wx_spec()
      if (!is.null(spec) &&
          !identical(survey_version(), spec$survey_version)) {
        .stale_banner(
          "Weather stats",
          note = "Survey data was reloaded after these statistics were produced."
        )
      } else NULL
    })

    # ---- Load and merge weather on button click ------------------------------

    observeEvent(input$weather_stats, {
      req(selected_weather(), selected_surveys(), survey_data())
      if (!load_guard$begin()) return(invisible(NULL))
      on.exit(load_guard$end(), add = TRUE)

      sw  <- selected_weather()
      svy <- survey_data()
      ss  <- selected_surveys()

      # REACT-03: an identical request to the last completed load is served
      # from state instead of re-running the weather I/O. `survey_version()`
      # stands in for the (large) survey frame itself; it is stored only when
      # the load finishes, so a failed load always retries.
      sig <- digest::digest(list(sw, ss, survey_version(), connection_params()))
      if (identical(sig, last_wx_load_sig())) {
        showNotification("Weather data is already loaded for this selection.",
                         duration = 3, type = "message")
        return(invisible(NULL))
      }

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
      # configuration. Drop the stale cells now; they are rebuilt under the new
      # configuration at the end of this observer.
      hist_cells(NULL)

      # INT-05 pattern: snapshot the selection this load was built from. All
      # tab outputs bind to this spec, so selector changes stay inert until
      # the button is pressed again.
      wx_spec(list(sw = sw, so = selected_outcome(),
                   survey_version = survey_version()))

      last_wx_load_sig(sig)

      showNotification("Weather data ready.", duration = 3, type = "message")

      # ---- Define outputs once then add tab ---------------------------------

      if (!weather_tab_added()) {

        # -- Weather distribution plots (one per variable) -------------------
        # Each plot carries the sample and, once the historical years have
        # loaded, the same locations' own climate history alongside it. The
        # historical series is loaded continuous, so the bar chart cuts it with
        # the breaks the sample was binned on (`stored_breaks`) to land in the
        # same bins.

        make_weather_dist <- function(idx) {
          renderPlot({
            req(survey_weather(), wx_spec())
            df          <- survey_weather() |>
              dplyr::mutate(countryyear = paste0(economy, ", ", year))
            sw          <- wx_spec()$sw
            hv          <- sw$name[idx]
            label       <- sw$label[idx]
            cont_binned <- sw$cont_binned[idx]
            yrs         <- hist_cells_years()
            brks        <- stored_breaks()

            p <- plot_weather_dist(
              df, hv, label, cont_binned,
              hist_df   = hist_cells(),
              breaks    = if (is.null(brks)) NULL else brks[[hv]],
              year_from = if (is.null(yrs)) NULL else yrs[["from"]],
              year_to   = if (is.null(yrs)) NULL else yrs[["to"]]
            )
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
            req(survey_weather_cont(), wx_spec())
            df    <- survey_weather_cont() |>
              dplyr::mutate(countryyear = paste0(economy, ", ", year))
            sw    <- wx_spec()$sw
            hv    <- sw$name[idx]
            label <- sw$label[idx]
            yrs   <- hist_cells_years()

            p <- plot_weather_ridges_compare(
              df, hv, label,
              hist_df   = hist_cells(),
              year_from = if (is.null(yrs)) NULL else yrs[["from"]],
              year_to   = if (is.null(yrs)) NULL else yrs[["to"]]
            )
            if (is.null(p)) {
              plot.new(); title(main = "Continuous distribution unavailable")
              return(invisible(NULL))
            }
            p
          })
        }

        output$weather_dist_cont1 <- make_weather_dist_cont(1)
        output$weather_dist_cont2 <- make_weather_dist_cont(2)


        # -- Binscatter plots (one per variable) ------------------------------

        make_binscatter <- function(idx) {
          renderPlot({
            so  <- wx_spec_so()
            req(survey_weather(), so)
            df <- survey_weather() |> prepare_outcome_df(so)
            sw  <- wx_spec_sw()
            
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
          selected_weather = wx_spec_sw
        )
        output$weather_stats_table_binned <- make_weather_binned_stats_dt(
          survey_weather   = survey_weather,
          selected_weather = wx_spec_sw
        )

        # Single panel that conditionally shows the continuous table, the
        # binned table, or both - based on each selected weather variable's
        # type in the merged survey-weather frame.
        output$weather_stats_layout <- shiny::renderUI({
          shiny::req(survey_weather(), wx_spec_sw())
          df   <- survey_weather()
          sw   <- wx_spec_sw()
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
                "Continuous variables - weighted summary per country-year.",
                style = "font-size: 12px;"
              ),
              DT::DTOutput(ns("weather_stats_table"))
            ),
            if (has_continuous && has_binned) shiny::br(),
            if (has_binned) shiny::tagList(
              shiny::helpText(
                paste("Binned variables - count and share of observations",
                      "in each bin per country-year."),
                style = "font-size: 12px;"
              ),
              DT::DTOutput(ns("weather_stats_table_binned"))
            )
          )
        })

        # -- Selected weather config table ------------------------------------

        output$selected_weather <- DT::renderDT({
          wx_spec_sw()
        },
        rownames = FALSE,
        options  = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
        class    = "compact")

        # -- Append tab -------------------------------------------------------

        # Reactive layouts so panels update when the user toggles between
        # 1 and 2 weather variables without re-creating the tab.
        output$weather_dist_layout <- shiny::renderUI({
          sw     <- wx_spec_sw() %||% data.frame()
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
              ns, n_vars, ids = dist_ids, height = "300px",
              alts = paste("Distribution of", sw$label,
                           "in the selected surveys and their climate history")
            ))
          }

          var_panel <- function(i) {
            items <- list(wise_plot_output(
              ns(dist_ids[i]),
              paste("Distribution of", sw$label[i],
                    "in the selected surveys and their climate history"),
              height = "300px"
            ))
            if (is_binned(i)) {
              items <- c(items, list(
                shiny::helpText(
                  paste("Above: binned weather distribution as configured.",
                        "Below: the continuous distribution the bins were",
                        "derived from."),
                  style = "font-size: 12px;"
                ),
                wise_plot_output(
                  ns(cont_ids[i]),
                  paste("Continuous distribution of", sw$label[i],
                        "underlying the binned configuration"),
                  height = "300px"
                )
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
          wx <- wx_spec_sw() %||% data.frame()
          weather_plot_layout(
            ns, nrow(wx),
            ids    = c("binscatter1", "binscatter2"),
            height = "300px",
            alts   = if (nrow(wx)) {
              paste("Binscatter of the outcome against", wx$label)
            } else NULL
          )
        })

        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Weather stats",
            value = "weather_desc",
            shiny::uiOutput(ns("wx_stale_banner")),
            shiny::h4(
              "Distribution of weather (sample and its own history)",
              info_popover(
                shiny::tagList(
                  p(paste(
                    "Distribution of each selected weather variable across the",
                    "survey sample, with each wave drawn in its own colour.",
                    "Waves need not cover the same time of year or the same",
                    "locations, so their distributions need not match each",
                    "other."
                  )),
                  p(paste(
                    "Alongside each wave sits that wave's own climate history:",
                    "weather over the configured year range for the same",
                    "locations and the same calendar months the wave was",
                    "fielded in, weighted by the number of sampled households",
                    "behind each location-month, so both series are composed",
                    "the same way. The year range is set under 'Historical",
                    "comparison' in the weather sidebar."
                  )),
                  p(paste(
                    "For binned variables the bars show the share of",
                    "observations in each bin - not counts, since the",
                    "historical series spans decades and would otherwise dwarf",
                    "the single wave behind it. The historical values are cut",
                    "with the same bin breaks as the sample. The continuous",
                    "panel below the bars shows the series the bins were",
                    "derived from, on its configured scale (raw, deviation",
                    "from mean, standardised anomaly)."
                  ))
                )
              )
            ),
            shiny::uiOutput(ns("weather_dist_layout")),
            shiny::br(),
            shiny::h4(
              "Weather by location",
              info_popover(
                shiny::tagList(
                  p(paste(
                    "Survey locations shaded by the weather the sample",
                    "experienced there - the bin for binned variables, the",
                    "configured value (raw, deviation from mean, standardised",
                    "anomaly) for continuous ones. One map per weather",
                    "variable; pick the survey wave above. The colour scale is",
                    "built across all waves and does not move when the wave",
                    "changes, so switching between them compares like with",
                    "like."
                  )),
                  p(paste(
                    "The shaded value is the same quantity that enters the",
                    "model as a regressor: each household is assigned the",
                    "weather of its own location in its own interview month,",
                    "already aggregated over the configured window (e.g. the",
                    "mean of the months preceding the interview) and already",
                    "transformed. Where a location was surveyed in a single",
                    "interview month, every household there shares one value",
                    "and the colour is exactly that regressor."
                  )),
                  p(paste(
                    "Locations surveyed across several interview months hold",
                    "several different values, so their colour is the",
                    "household-weighted mean (continuous) or the most common",
                    "bin (binned) across that location's own household-month",
                    "observations. They are drawn with a dotted outline: the",
                    "colour there summarises the households rather than",
                    "reproducing any one household's regressor. Click a",
                    "location for its value, household count and number of",
                    "interview months."
                  )),
                  p(shiny::tags$b("Views.")),
                  shiny::tags$ul(
                    shiny::tags$li(paste(
                      "Wave value - cross-sectional: what the sample",
                      "experienced, so locations are compared with each other.",
                      "This is the regressor itself."
                    )),
                    shiny::tags$li(paste(
                      "Difference from own historical mean - within-location:",
                      "the wave's value minus what the same location normally",
                      "gets in the same calendar months over the loaded",
                      "historical years, in the variable's units. Blue is",
                      "below that location's own normal, red above."
                    )),
                    shiny::tags$li(paste(
                      "Percentile in own history - within-location: where the",
                      "wave's value falls in that location's own historical",
                      "distribution, 0-100, with 50 a typical year. This is",
                      "the map equivalent of where the sample curve sits",
                      "inside the historical distribution above."
                    ))
                  ),
                  p(paste(
                    "Both within-location views need the historical years,",
                    "loaded with the weather data over the range set under",
                    "'Historical comparison' in the weather sidebar, and both",
                    "use the continuous series even when the variable is",
                    "binned for modelling - a difference between bins would",
                    "not be meaningful."
                  ))
                )
              )
            ),
            shiny::uiOutput(ns("wxmap_view_ui")),
            shiny::uiOutput(ns("weather_map_layout")),
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

      # Every panel on this tab compares a wave with its own climate history,
      # so the historical years are loaded as part of the weather load rather
      # than behind a separate button.
      hy <- hist_years()
      load_hist_weather(hy[["from"]], hy[["to"]])

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ---- Weather by location maps -------------------------------------------

  # -- Weather by location, one map per wave ----------------------------
  # The merged frame holds one value per location *and interview month*,
  # so `summarise_weather_by_loc()` collapses to one value per location
  # (mean, or modal bin) before mapping. The palette is built once per
  # variable across all waves so the maps stay comparable.

  # The view chosen above the maps. "value" is cross-sectional - how the
  # locations compare with each other in this wave. "anomaly" and "pctile" are
  # within-location - how each location's wave compares with its own history,
  # so they need the historical years loaded in the section above.
  # The view chosen by the single picker above the maps.
  wxmap_view_val <- reactiveVal("value")

  wxmap_view <- reactive(wxmap_view_val())

  wxmap_view_picker <- function(id) {
    has_hist <- !is.null(hist_cells()) && !is.null(hist_cells_years())
    yrs      <- hist_cells_years()
    span     <- if (is.null(yrs)) "" else
      paste0(" (", yrs[["from"]], "-", yrs[["to"]], ")")

    selected <- wxmap_view_val()
    if (!has_hist) selected <- "value"

    radios <- shiny::radioButtons(
      ns(id), NULL, inline = TRUE,
      selected    = selected,
      choiceNames = list(
        "Wave value",
        paste0("Difference from own historical mean", span),
        paste0("Percentile in own history", span)
      ),
      choiceValues = list("value", "anomaly", "pctile")
    )

    if (!has_hist) {
      radios <- htmltools::tagQuery(radios)$
        find("input")$
        filter(function(x, i) i %in% c(2L, 3L))$
        addAttrs(
          disabled = NA,
          title    = paste("Historical weather is not available for this",
                           "sample, so a location cannot be compared with",
                           "its own history")
        )$
        parent()$
        addAttrs(style = "opacity: 0.45; cursor: not-allowed;")$
        allTags()
    }

    htmltools::tagAppendAttributes(radios, style = "margin-bottom: 0;")
  }

  # Whether the current view can actually be drawn.
  wxmap_view_ready <- reactive({
    wxmap_view() == "value" ||
      (!is.null(hist_cells()) && !is.null(hist_cells_years()))
  })

  # Cell geometry to draw on, when Survey stats has supplied it. Values are
  # merged onto these cells so overlapping survey locations stop stacking
  # translucent fills - which invented colours that were in neither the data
  # nor the legend.
  wxmap_cells <- reactive({
    cd <- if (is.null(cell_data)) NULL else cell_data()
    if (is.null(cd) || is.null(cd$geom) || is.null(cd$map)) return(NULL)
    cd
  })

  wxmap_features <- reactive({
    cd <- wxmap_cells()
    if (is.null(cd)) return(if (is.null(map_data)) NULL else map_data())
    build_cell_features(cd$geom, cd$map) %||%
      (if (is.null(map_data)) NULL else map_data())
  })

  # Per-location values, merged onto cells when cell geometry is in use.
  to_cells <- function(lv) {
    cd <- wxmap_cells()
    if (is.null(cd) || is.null(lv)) return(lv)
    merge_loc_values_to_cells(cd$map, lv) %||% lv
  }

  weather_loc_vals <- reactive({
    req(survey_weather(), wx_spec_sw())
    sw   <- wx_spec_sw()
    view <- wxmap_view()

    if (view == "value") {
      # Grouping is identical for every variable - build it once (PERF-25).
      prep <- .summarise_loc_prep(survey_weather())
      return(lapply(seq_len(nrow(sw)), function(i) {
        to_cells(summarise_weather_by_loc(prep$df, sw$name[i], prep = prep))
      }))
    }

    cells <- hist_cells()
    yrs   <- hist_cells_years()
    if (is.null(cells) || is.null(yrs)) return(NULL)
    lapply(seq_len(nrow(sw)), function(i) {
      to_cells(summarise_weather_anomaly_by_loc(
        cells_df  = cells,
        hv        = sw$name[i],
        year_from = yrs[["from"]],
        year_to   = yrs[["to"]],
        measure   = if (view == "pctile") "percentile" else "anomaly"
      ))
    })
  })

  # Legend title and colour scale both follow the view. The maps are small, so
  # the legend carries a short heading and the full sentence goes into the
  # info marker's hover text (and the popups).
  wxmap_label <- function(base_label) {
    yrs  <- hist_cells_years()
    span <- if (is.null(yrs)) "the historical years" else
      paste0(yrs[["from"]], "-", yrs[["to"]])
    switch(
      wxmap_view(),
      anomaly = paste0(base_label, " - difference from the location's own ",
                       span, " mean, in the variable's units. Negative means",
                       " the wave was below that location's normal."),
      pctile  = paste0(base_label, " - where the wave falls in the location's",
                       " own ", span, " distribution (0-100; 50 is a typical",
                       " year)."),
      paste0(base_label, " - the value the sample experienced, as configured.",
             " This is the variable that enters the model.")
    )
  }

  wxmap_short <- function(base_label) {
    short <- if (nchar(base_label) > 20) {
      paste0(substr(base_label, 1, 19), "...")
    } else {
      base_label
    }
    switch(
      wxmap_view(),
      anomaly = paste0(short, " \u0394"),
      pctile  = paste0(short, " %ile"),
      short
    )
  }

  wave_list <- reactive({
    lv <- weather_loc_vals()
    if (is.null(lv)) return(NULL)
    lv <- Filter(Negate(is.null), lv)
    if (length(lv) == 0) return(NULL)
    w <- unique(lv[[1]][, c("code", "year", "survname", "economy")])
    w$key   <- paste(w$code, as.character(w$year), w$survname, sep = "|")
    w$label <- paste0(w$economy, ", ", w$year)
    w[order(w$label), , drop = FALSE]
  })

  # One map per weather variable, not per variable x wave. A wave is picked
  # above the maps instead: a country with several waves and two variables
  # would otherwise stand up a leaflet widget for every combination, and they
  # all render whether or not anyone looks at them. This is the same
  # arrangement the survey-stats and outcome-coverage maps already use.
  wxmap_id <- function(i) paste0("wxmap_", i)

  # Which wave the maps are showing. Unlike the other tabs' maps there is no
  # "All waves" option: a location carries a different weather value in each
  # wave, so drawing them together would just hide one behind the other.
  wxmap_wave_val <- reactiveVal(NULL)

  wxmap_wave <- reactive({
    w <- wave_list()
    if (is.null(w) || nrow(w) == 0) return(NULL)
    sel <- wxmap_wave_val()
    if (is.null(sel) || !(sel %in% w$key)) w$key[1] else sel
  })

  observeEvent(input$wxmap_wave, {
    v <- input$wxmap_wave
    if (!is.null(v)) wxmap_wave_val(v)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  output$wxmap_wave_ui <- shiny::renderUI({
    w <- wave_list()
    if (is.null(w) || nrow(w) < 2) return(NULL)
    choices <- wave_slider_choices(w, include_all = FALSE)
    wave_toggle_slider(
      ns("wxmap_wave"),
      choices  = choices,
      selected = wxmap_wave()
    )
  })

  # Every picker copy (the one above the maps plus one per card) writes to the
  # shared value; a change in any of them updates the rest.
  observeEvent(input$wxmap_view, {
    v <- input$wxmap_view
    if (!is.null(v)) wxmap_view_val(v)
  }, ignoreInit = TRUE, ignoreNULL = TRUE)

  # The within-location views need historical weather; drop back to the wave
  # value if it goes away (a re-configuration clears it before reloading).
  observeEvent(hist_cells(), {
    if (is.null(hist_cells()) && !identical(wxmap_view_val(), "value")) {
      wxmap_view_val("value")
      shiny::updateRadioButtons(session, "wxmap_view", selected = "value")
    }
  }, ignoreNULL = FALSE)

  # Per-map view memory, so switching view keeps the pan and zoom.
  wxmap_view_mem <- new.env(parent = emptyenv())

  # One output per weather variable. The palette is still built across *all*
  # waves, so the colour scale does not shift under the user when they change
  # wave - the whole point of the picker is to compare them.
  observe({
    lv_list <- weather_loc_vals()
    req(lv_list)
    sw   <- wx_spec_sw()
    view <- wxmap_view()

    for (i in seq_along(lv_list)) {
      lv <- lv_list[[i]]
      if (is.null(lv)) next

      tf <- if ("transformation" %in% names(sw)) sw$transformation[i] else "None"

      # A difference is diverging around zero whatever the variable's own
      # configuration; a percentile is a fixed 0-100 scale.
      pal <- switch(
        view,
        anomaly = .weather_map_palette(lv$value, FALSE, NULL, tf,
                                       force = "diverging"),
        pctile  = .weather_map_palette(lv$value, FALSE, NULL, tf,
                                       force = "sequential",
                                       domain = c(0, 100)),
        .weather_map_palette(lv$value, isTRUE(attr(lv, "binned")),
                             attr(lv, "levels"), tf)
      )

      local({
        .lv    <- lv
        .pal   <- pal
        .tf    <- tf
        .label <- sw$label[i]
        .info  <- wxmap_label(sw$label[i])
        .short <- wxmap_short(sw$label[i])
        .id    <- wxmap_id(i)

        if (is.null(wxmap_view_mem[[.id]])) {
          wxmap_view_mem[[.id]] <- map_view_memory(
            input, session, .id,
            # A weather reload can change the geography on screen; a wave or
            # view switch within one load keeps the user's view.
            key = shiny::reactive(digest::digest(list(wx_spec(), .id)))
          )
          wxmap_view_mem[[.id]]$remember()
        }
        .mem <- wxmap_view_mem[[.id]]

        output[[.id]] <- mapgl::renderMaplibre({
          gj <- wxmap_features()
          req(gj)
          key <- wxmap_wave()
          req(key)

          sub <- .lv[paste(.lv$code, as.character(.lv$year), .lv$survname,
                           sep = "|") == key, , drop = FALSE]
          req(nrow(sub) > 0)
          attr(sub, "binned") <- attr(.lv, "binned")
          attr(sub, "levels") <- attr(.lv, "levels")

          m <- plot_weather_loc_map(
            geojson        = gj,
            loc_vals       = sub,
            label          = .label,
            transformation = .tf,
            pal_info       = .pal,
            legend_title   = .short,
            legend_info    = .info
          )
          req(!is.null(m))
          .mem$restore(m)
        })
      })
    }
  })

  # Wave and view pickers. Both sit above the maps and apply to all of them, so
  # the two weather variables are always shown for the same wave on the same
  # scale. The within-location views need the historical years, which load with
  # the weather data.
  output$wxmap_view_ui <- shiny::renderUI({
    # Without historical weather there is nothing to compare a location with,
    # so the two within-location views are greyed out and unselectable rather
    # than silently showing something else (see the observer above for the
    # matching reset of the shared value).
    has_hist <- !is.null(hist_cells()) && !is.null(hist_cells_years())

    shiny::tagList(
      shiny::div(
        class = "d-flex align-items-center gap-3 flex-wrap",
        shiny::uiOutput(ns("wxmap_wave_ui"), inline = TRUE),
        wxmap_view_picker("wxmap_view")
      ),
      if (!has_hist) shiny::helpText(
        paste("The two within-location views compare each location with its",
              "own history, which is still loading or unavailable for this",
              "sample."),
        style = "font-size: 12px; margin-top: -6px;"
      )
    )
  })

  output$weather_map_layout <- shiny::renderUI({
    if (is.null(wxmap_features())) {
      return(shiny::helpText(
        "Location map data is not available for this sample.",
        style = "font-size: 12px;"
      ))
    }
    if (!wxmap_view_ready()) {
      return(shiny::helpText(
        paste("This view compares each location with its own history, which",
              "is still loading or unavailable for this sample."),
        style = "font-size: 12px;"
      ))
    }

    waves   <- wave_list()
    lv_list <- weather_loc_vals()
    sw      <- wx_spec_sw()
    if (is.null(waves) || is.null(lv_list) || is.null(sw)) {
      return(shiny::helpText("No locations to map.",
                             style = "font-size: 12px;"))
    }

    # One card per weather variable, side by side when there are two. The wave
    # is named in the card header as well as in the picker, so a screenshot of
    # a single card still says which wave it is.
    wave_label <- waves$label[match(wxmap_wave(), waves$key)]
    idx   <- Filter(function(i) !is.null(lv_list[[i]]), seq_len(nrow(sw)))
    if (length(idx) == 0) {
      return(shiny::helpText("No locations to map.",
                             style = "font-size: 12px;"))
    }

    cards <- lapply(idx, function(i) {
      # full_screen adds bslib's expand control: the card fans out over the
      # results area, with a close button and Esc to come back.
      bslib::card(
        full_screen = TRUE,
        height      = "430px",
        bslib::card_header(
          if (is.na(wave_label)) sw$label[i] else
            paste0(sw$label[i], " - ", wave_label)
        ),
        mapgl::maplibreOutput(ns(wxmap_id(i)), height = "100%")
      )
    })

    n_col <- if (length(cards) == 1) 1L else 2L
    do.call(
      bslib::layout_columns,
      c(list(col_widths = rep(12L / n_col, n_col)), cards)
    )
  })

    # ---- Historical weather over the configured year range -------------------

    # Loads historical weather for [yf, yt] and rebuilds the comparison cells.
    # Called once per weather load, and again whenever the year range under
    # "Historical comparison" in the weather sidebar changes.
    #
    # This is a second, full `get_weather()` pass: the survey-period load only
    # covers the wave's own months, so the extra years have to be aggregated,
    # rolled and transformed here regardless of what was loaded before.
    load_hist_weather <- function(yf, yt) {
      svy <- survey_data()
      ss  <- selected_surveys()
      # The historical comparison backs what is on screen, so it loads the
      # button-time snapshot, not the live selector (INT-05 pattern).
      sw  <- wx_spec_sw()
      swd <- survey_weather()
      if (is.null(svy) || is.null(ss) || is.null(sw) || is.null(swd)) {
        return(invisible(FALSE))
      }

      # Same months and locations as the survey, more years. The temporal
      # aggregation window configured for each variable is applied by
      # get_weather() relative to each of these timestamps.
      dates <- expand_hist_dates(extract_survey_dates(svy), yf, yt)
      if (length(dates) == 0) return(invisible(FALSE))

      # Always continuous here - binning is a modelling choice, this section
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

    # Changing the year range in the sidebar reloads the comparison in place,
    # but only once weather is on screen - before that the initial load will
    # pick the range up anyway. Debounced so typing "2015" into a year box does
    # not fire a load per keystroke.
    hist_years_d <- shiny::debounce(hist_years, 1500)

    observeEvent(hist_years_d(), {
      req(wx_spec_sw(), selected_surveys(), survey_data(),
          survey_weather())

      hy  <- hist_years_d()
      cur <- hist_cells_years()
      if (!is.null(cur) && identical(as.integer(cur), as.integer(hy))) return()

      load_hist_weather(hy[["from"]], hy[["to"]])

    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # ---- Return API ---------------------------------------------------------

    list(survey_weather = survey_weather,
         stored_breaks  = stored_breaks)
  })
}
