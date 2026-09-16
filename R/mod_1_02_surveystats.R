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
  # The `dt-wrap` column style lives in custom.css. It used to be built here
  # as a bare tags$style() that was evaluated and then thrown away - the
  # tagList below is what the function returns - so the rule never reached the
  # page. It now also serves the weather stats tables (fct_weatherstats.R).
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
#' @param run_trigger Optional reactive trigger for a programmatic load.
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
  tabset_session = NULL,
  analysis_unit = NULL,
  run_trigger = shiny::reactive(NULL)
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    select_tab <- function(value) {
      if (is.null(tabset_id) || !nzchar(tabset_id)) {
        return(invisible(FALSE))
      }
      try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id, selected = value), silent = TRUE)
      invisible(TRUE)
    }

    notify <- function(msg, type = "message", duration = 5) {
      shiny::showNotification(msg, type = type, duration = duration)
    }

    # ---- Button (shown once selected_surveys is populated) ------------------

    output$survey_stats_button_ui <- renderUI({
      req(nrow(selected_surveys()) > 0)
      actionButton(ns("survey_stats"), "Survey stats", class = "btn-primary", style = "width: 100%;")
    })

    shiny::outputOptions(output, "survey_stats_button_ui", suspendWhenHidden = FALSE)

    # REACT-02: double-click guard - one load at a time, button disabled
    # while running.
    load_guard <- .busy_guard(session, survey_stats)

    survey_tab_added <- reactiveVal(FALSE)

    # ---- Data storage -------------------------------------------------------

    survey_data <- reactiveVal(NULL)
    # INT-08: bumped on every successful survey load; downstream run
    # signatures include it so a reload invalidates fit/sim/policy results
    # even when the selection string is unchanged.
    survey_version <- shiny::reactiveVal(0L)
    # Internal generation for every newly loaded survey frame. Unlike
    # survey_version, this also advances when downstream H3/panel work fails.
    survey_data_generation <- shiny::reactiveVal(0L)
    publish_new_survey_data <- function(value) {
      survey_data(value)
      survey_data_generation(
        shiny::isolate(survey_data_generation()) + 1L
      )
    }
    survey_wave_meta <- shiny::reactive({
      cached_survey_wave_metadata(
        session, shiny::isolate(survey_data()), survey_data_generation()
      )
    })
    # Completion generation used by the automatic configuration pipeline.
    # Cached requests count as successful completions; failed requests do not.
    load_done <- shiny::reactiveVal(0L)
    load_status <- shiny::reactiveVal("idle")
    # Per-H3-cell counts and mapped-location count behind the density map,
    # recomputed together for whichever wave the picker is on.
    density_cells <- function(wave = "all") {
      cd <- cell_data()
      df <- survey_data()
      if (is.null(cd) || is.null(df)) {
        return(NULL)
      }

      density <- .density_cell_summary(
        filter_by_wave(cd$map, wave), filter_by_wave(df, wave)
      )
      if (is.null(density)) {
        return(NULL)
      }

      density$cells <- dplyr::inner_join(cd$geom, density$cells, by = "h3") |>
        dplyr::filter(!is.na(geom), nchar(geom) > 2)
      density
    }
    # Cell geometry plus the location-to-cell mapping, shared with the outcome
    # and weather maps so they can merge overlapping locations onto cells.
    cell_data <- reactiveVal(NULL)
    # REACT-03: digest of the last successfully completed load request.
    last_load_sig <- reactiveVal(NULL)
    # PERF-36: bumped whenever a new map dataset lands in cell_data(); the
    # density observer fits the camera only when this changes. The fit must
    # not key on digest(selected_surveys()): that reactive is read inside the
    # observer, so changing the sample picker fired it against the old map
    # and stamped the new selection's key onto stale payloads - the refit
    # after the real load was then skipped and the camera kept the previous
    # sample's extent.
    map_data_version <- shiny::reactiveVal(0L)

    # ---- Shared stats base (PERF-40) ----------------------------------------
    # Every summary-stats table on the tab summarises the same survey frame;
    # six independent renderDT pipelines each re-subset it and re-run the
    # grouped collapse passes over their own var set on every load (~5s at
    # Iran scale). One pass over the union of table variables feeds every
    # table by row-filtering via make_stats_dt(base = ).
    policy_vars <- unique(unlist(lapply(POLICY_DEFINITIONS, `[[`, "vars")))

    stats_base <- reactive({
      sd <- survey_data()
      shiny::req(sd)
      vl <- if (is.function(variable_list)) variable_list() else variable_list

      targets <- if (is.null(vl)) {
        character(0)
      } else {
        unlist(lapply(c("outcome", "ind", "hh", "firm", "area"), function(fc) {
          if (fc %in% names(vl)) vl$name[vl[[fc]] == 1] else character(0)
        }), use.names = FALSE)
      }

      union_vars <- intersect(unique(c(targets, policy_vars)), names(sd))
      if (!length(union_vars)) {
        return(NULL)
      }
      list(
        vars = union_vars,
        summary = weighted_summary_long(sd, vars = union_vars),
        missing = if ("countryyear" %in% names(sd)) {
          survey_missingness_long(sd, vars = union_vars)
        } else {
          NULL
        }
      )
    })

    # ---- Load and prepare data on button click ------------------------------

    survey_stats_event <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$survey_stats,
      {
        if (shiny::isTruthy(input$survey_stats)) {
          survey_stats_event(list(source = "manual", value = input$survey_stats))
        }
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )
    shiny::observeEvent(run_trigger(),
      {
        ext <- run_trigger()
        if (!is.null(ext)) survey_stats_event(list(source = "pipeline", value = ext))
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    observeEvent(survey_stats_event(),
      {
        if (!load_guard$begin()) {
          return(invisible(NULL))
        }
        on.exit(load_guard$end(), add = TRUE)
        load_done(load_done() + 1L)
        load_status("running")
        completed <- FALSE
        on.exit(
          {
            if (!completed) load_status("failure")
          },
          add = TRUE
        )
        req(nrow(selected_surveys()) > 0)

        # REACT-03: an identical request to the last completed load is served
        # from state instead of re-running the full I/O pipeline. The signature
        # covers everything this handler consumes; it is stored only when no
        # inner stage warned, so a partially failed load always retries on the
        # next click.
        sig <- digest::digest(list(
          selected_surveys(), connection_params(), variable_list(), cpi_ppp()
        ))
        load_ok <- TRUE
        if (identical(sig, last_load_sig())) {
          showNotification("Survey data is already loaded for this selection.",
            duration = 3, type = "message"
          )
          load_status("success")
          completed <- TRUE
          return(invisible(NULL))
        }

        busy_id <- showNotification("Loading survey data...", duration = NULL, type = "message")
        on.exit(removeNotification(busy_id), add = TRUE)

        # INT-06: drop the previous survey's map/cell state as soon as a reload
        # starts, and on every inner failure below. The hex-map payload
        # observer reacts by sending `clear`, so the previous survey's
        # geography can never outlive its microdata - a failure now leaves a
        # blank map instead of a misleading one.
        cell_data(NULL)

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
        df <- df |>
          assign_data_level() |>
          convert_lcu_to_ppp(cpi_ppp(), lcu_vars) |>
          bottom_code_welfare(0.28) |>
          apply_policy_derivations()

        publish_new_survey_data(df)

        # ---- H3 map data (computed once per button click) -------------------
        h3_fnames <- ss |>
          dplyr::distinct(code, year, survname, source) |>
          dplyr::mutate(fname = paste0(
            "microdata/h3/", code, "/",
            code, "_", year, "_", survname, "_", source, "_h3.parquet"
          )) |>
          dplyr::pull(fname)

        h3_df <- tryCatch(
          load_data(h3_fnames, connection_params()),
          error = function(e) {
            notify(paste(
              "Some map data could not be loaded. Survey results are still available:",
              conditionMessage(e)
            ), type = "warning", duration = 5)
            NULL
          }
        )

        if (!is.null(h3_df)) {
          # PERF-23: the h3 lazy relation is a view over remote parquet files.
          # Every downstream scan (map GeoJSON, cell geometry, cell map,
          # loc_panel's multiple passes, loc keys) would otherwise re-read the
          # files over the network. Materialise once into a local temp table;
          # all consumers in this block then read locally. The table is dropped
          # when the block ends (everything downstream collects eagerly).
          h3_local <- tryCatch(
            {
              nm <- basename(tempfile(pattern = "ss_h3_"))
              local_h3 <- dplyr::compute(h3_df, name = nm, temporary = TRUE)
              on.exit(
                try(DBI::dbRemoveTable(dbplyr::remote_con(local_h3), nm), silent = TRUE),
                add = TRUE
              )
              local_h3
            },
            error = function(e) {
              notify(paste(
                "Map data could not be prepared locally. Survey results are still available:",
                conditionMessage(e)
              ), type = "warning", duration = 5)
              h3_df
            }
          )

          tryCatch(
            {
              con <- dbplyr::remote_con(h3_local)
              .duck_load_ext("spatial")
              .duck_load_ext("h3")

              # -- Location of interviews map ------------------------------------
              # One row per H3 cell: a GeoJSON geometry string (the Leaflet
              # fallback artifact; MapLibre decodes geometry in the browser from
              # cell ids, so it is never sent when WebGL is available) plus the
              # per-cell bbox that the payload's fit bounds come from, and the
              # location-to-cell mapping shared with the outcome and weather maps.
              cell_geo <- h3_local |>
                dplyr::distinct(h3) |>
                dplyr::mutate(g = st_geomfromtext(h3_cell_to_boundary_wkt(h3))) |>
                dplyr::mutate(
                  geom = st_asgeojson(g),
                  # PERF-36: per-cell bbox beside the geometry string.
                  env  = st_extent(g)
                ) |>
                dplyr::mutate(
                  xmin = st_xmin(env), ymin = st_ymin(env),
                  xmax = st_xmax(env), ymax = st_ymax(env)
                ) |>
                dplyr::select(-g, -env) |>
                collect_deterministic("h3")

              cell_map <- h3_local |>
                dplyr::select(code, year, survname, loc_id, h3, pop_2020) |>
                collect_deterministic(c("code", "year", "survname", "loc_id", "h3"))

              cell_data(list(geom = cell_geo, map = cell_map))
              map_data_version(map_data_version() + 1L)
            },
            error = function(e) {
              load_ok <<- FALSE
              notify(
                paste(
                  "The sample coverage map could not be prepared. Other survey results are still available:",
                  conditionMessage(e)
                ),
                type = "warning", duration = 5
              )
            }
          )

          tryCatch(
            {
              panel_map <- loc_panel(h3_local,
                id_col = loc_id, h3_col = h3, weight_col = pop_2020,
                group_cols = c("code", "year", "survname")
              )

              loc_keys <- h3_local |>
                dplyr::distinct(code, year, survname, loc_id) |>
                collect_deterministic(c("code", "year", "survname", "loc_id"))

              df <- df |>
                dplyr::left_join(
                  dplyr::left_join(loc_keys, panel_map, by = c("code", "year", "survname", "loc_id")),
                  by = c("code", "year", "survname", "loc_id")
                )
              publish_new_survey_data(df)
              survey_version(survey_version() + 1L)
            },
            error = function(e) {
              # INT-06: loc_id_panel is not a cosmetic join - downstream VCV
              # estimation falls back when it is missing, which changes inference.
              load_ok <<- FALSE
              notify(paste0(
                "Location-level uncertainty estimates are unavailable, so standard ",
                "survey-design uncertainty is being used. Details: ", conditionMessage(e)
              ), type = "warning", duration = 8)
            }
          )
        } else {
          load_ok <- FALSE
        }

        if (load_ok) last_load_sig(sig)
        # H3 geometry or panel metadata can fail independently of the survey
        # frame. Preserve the existing fallback behavior and publish the usable
        # survey load so downstream stages can continue.
        load_status("success")
        completed <- TRUE

        notify(
          "Survey data loaded. Summary statistics and maps are ready.",
          type = "message", duration = 3
        )

        # ---- Outputs (defined once on first click) ---------------------------

        if (!survey_tab_added()) {
          # Interview dates bar chart. Grouped columns keep wave totals directly
          # comparable; plot_interview_dates() also exposes faceted and heatmap
          # variants for static outputs and design comparisons.
          interview_date_fig <- function() {
            unit <- if (is.function(analysis_unit)) analysis_unit() else NULL
            unit_label <- switch(unit %||% "hh",
              ind = "Individuals",
              firm = "Firms",
              "Households"
            )
            plot_interview_dates(
              summarise_interview_dates(survey_data()),
              unit_label = unit_label,
              palette = "sequential",
              wave_labels = survey_wave_meta()$plot_labels
            )
          }

          output$interview_date <- renderPlot({
            p <- interview_date_fig()
            req(!is.null(p))
            p
          })

          wise_export_figure(
            key = "interview_dates",
            label = "Interview dates",
            step = 1L,
            fun = interview_date_fig,
            description = paste(
              "When the selected surveys were fielded, by month - the calendar",
              "window the weather aggregation is matched against."
            ),
            width = 9, height = 5
          )

          # Unit label for legend/tooltip text ("households", "individuals",
          # "firms"), resolved live so an analysis-unit switch re-labels.
          unit_label <- function() {
            unit <- if (is.function(analysis_unit)) analysis_unit() else NULL
            switch(unit %||% "hh",
              ind = "individuals",
              firm = "firms",
              "households"
            )
          }

          # ---- Location of interviews payload stream ---------------------------
          # One reactive observer drives the hex map: data loads and wave
          # toggles both land here as fresh `set` payloads (cheap - cell ids
          # and values only, no geometry serialization). The camera is fitted
          # only when the data key changes (PERF-36 view-key semantics), so a
          # wave toggle re-colours in place and the user's pan/zoom survives.
          density_key <- reactiveVal(NULL)
          density_lgd <- reactiveVal(NULL)
          density_nloc <- reactiveVal(NULL)
          observe({
            cd <- cell_data()
            wave <- input$map_wave %||% "all"

            density <- if (is.null(cd)) NULL else density_cells(wave)
            pl <- if (is.null(density)) {
              NULL
            } else {
              .density_hex_payload(density$cells, unit_label())
            }
            if (is.null(pl)) {
              hexmap_clear(session, ns, "density_map")
              density_key(NULL)
              density_lgd(NULL)
              density_nloc(NULL)
            } else {
              hexmap_update(session, ns, "density_map", pl$payload)
              # PERF-36: refit only when a new map dataset has landed; wave
              # re-colours keep pan/zoom.
              if (!identical(map_data_version(), shiny::isolate(density_key()))) {
                hexmap_fit(session, ns, "density_map", pl$payload$bounds)
                density_key(map_data_version())
              }
              density_lgd(pl$legend)

              density_nloc(density$n_locations)
            }
          })

          # R-side legend: same palette state as the payloads, rebuilt per wave
          # and positioned over the map's top-right corner by hexmap_ui().
          output$map_legend_ui <- shiny::renderUI({
            lgd <- density_lgd()
            shiny::req(!is.null(lgd))
            html <- .compact_legend_html(
              pal_info = lgd$pal_info,
              binned   = lgd$binned,
              levels   = lgd$levels,
              title    = lgd$title,
              info     = lgd$info
            )
            n <- density_nloc()
            note <- if (is.null(n)) {
              ""
            } else {
              paste0(
                '<div style="white-space: nowrap; font-size: 10px; color: #333; ',
                "background: rgba(255,255,255,0.88); border-radius: 4px; ",
                'padding: 2px 5px; margin-top: 2px;">',
                "Number of unique locations: ",
                format(n, big.mark = ",", scientific = FALSE), "</div>"
              )
            }
            htmltools::HTML(paste0(html, note))
          })

          # Wave toggle slider, shown only when there is more than one wave to pick.
          output$map_wave_ui <- shiny::renderUI({
            w <- survey_wave_meta()$waves
            if (is.null(w) || nrow(w) < 2) {
              return(NULL)
            }
            choices <- wave_slider_choices(w, include_all = TRUE)
            selected <- shiny::isolate(input$map_wave) %||% "all"
            if (!selected %in% choices) selected <- "all"
            wave_toggle_slider(
              ns("map_wave"),
              choices  = choices,
              selected = selected
            )
          })

          output$outcome_stats <- make_stats_dt(survey_data, variable_list, "outcome",
            base = stats_base
          )
          output$ind_stats <- make_stats_dt(survey_data, variable_list, "ind",
            base = stats_base
          )
          output$hh_stats <- make_stats_dt(survey_data, variable_list, "hh",
            base = stats_base
          )
          output$firm_stats <- make_stats_dt(survey_data, variable_list, "firm",
            base = stats_base
          )
          output$area_stats <- make_stats_dt(survey_data, variable_list, "area",
            base = stats_base
          )

          # UI-48: register the same builders with the export bundle. Nothing is
          # computed here - the functions are called only if a bundle is asked
          # for, and return NULL for a level the survey has no variables at.
          local({
            levels <- list(
              outcome = "Outcome variables",
              ind     = "Individual-level variables",
              hh      = "Household-level variables",
              firm    = "Firm-level variables",
              area    = "Area-level variables"
            )
            for (flag in names(levels)) {
              local({
                f <- flag
                wise_export_table(
                  key = paste0("survey_summary_", f),
                  label = paste("Survey summary -", tolower(levels[[f]])),
                  step = 1L,
                  fun = function() {
                    build_stats_table(survey_data, variable_list, f,
                      base = stats_base
                    )
                  },
                  description = paste0(
                    "Weighted summary statistics (N, mean, SD, percentiles, % ",
                    "missing) for ", tolower(levels[[f]]),
                    ", by country and survey wave."
                  )
                )
              })
            }
          })

          # Only show characteristic tables relevant to the selected level of
          # analysis: individual level implies household + area also apply;
          # household level implies area also applies; firm level is separate.
          output$characteristic_tables_ui <- renderUI({
            unit <- if (is.function(analysis_unit)) analysis_unit() else NULL
            show_ind <- is.null(unit) || unit == "ind"
            show_hh <- is.null(unit) || unit %in% c("ind", "hh")
            show_firm <- is.null(unit) || unit == "firm"

            tagList(
              if (show_ind) {
                tagList(
                  h4("Individual characteristics"),
                  p(class = "text-muted small", "Summary statistics for individual-level variables"),
                  DT::DTOutput(ns("ind_stats"))
                )
              },
              if (show_hh) {
                tagList(
                  h4("Household characteristics"),
                  p(class = "text-muted small", "Summary statistics for household-level variables"),
                  DT::DTOutput(ns("hh_stats"))
                )
              },
              if (show_firm) {
                tagList(
                  h4("Firm characteristics"),
                  p(class = "text-muted small", "Summary statistics for firm-level variables"),
                  DT::DTOutput(ns("firm_stats"))
                )
              },
              h4("Area characteristics"),
              p(class = "text-muted small", "Summary statistics for area-level variables"),
              DT::DTOutput(ns("area_stats"))
            )
          })

          # ---- Selection summary card (replaces the old DT table) -------------
          # Binds live to selected_surveys()/analysis_unit() so the card follows
          # the sidebar selection, like the DT table it replaces.

          wise_export_table(
            key = "survey_summary_policy",
            label = "Survey summary - policy-relevant variables",
            step = 1L,
            fun = function() {
              build_stats_table(survey_data, variable_list,
                vars = policy_vars,
                base = stats_base
              )
            },
            description = paste(
              "Weighted summary statistics for the variables that Step 3's",
              "policy levers act on, by country and survey wave."
            )
          )

          selected_surveys_df <- function() {
            sel <- selected_surveys()
            if (is.null(sel)) {
              return(NULL)
            }
            sel |> dplyr::select(-dplyr::any_of(c("fname", "fpath")))
          }

          wise_export_table(
            key = "selected_surveys",
            label = "Selected surveys",
            step = 1L,
            fun = selected_surveys_df,
            description = paste(
              "The survey rounds included in the analysis: country, year,",
              "survey name and sample metadata."
            )
          )

          output$selected_surveys <- DT::renderDT(
            {
              req(selected_surveys())
              selected_surveys_df()
            },
            rownames = FALSE,
            extensions = "Buttons",
            options = list(
              dom = wise_csv_dom("t"), paging = FALSE,
              searching = FALSE, info = FALSE,
              buttons = wise_csv_button("selected_surveys")
            ),
            class = "compact"
          )

          output$selected_surveys_card <- renderUI({
            ss <- selected_surveys()
            req(nrow(ss) > 0)
            sd <- survey_data()
            req(!is.null(sd))

            unit <- if (is.function(analysis_unit)) analysis_unit() else NULL
            badge <- analysis_unit_label(unit) %||%
              analysis_unit_label(unique(ss$level)[1])
            sample_n <- paste0("N = ", format(nrow(sd), big.mark = ",", scientific = FALSE))
            badge <- paste(c(badge, sample_n), collapse = " | ")

            econ_rows <- lapply(
              sort(unique(ss$code)),
              function(code) {
                s <- ss[ss$code == code, ]
                programs <- sort(unique(s$survname))
                selection_card_row(
                  name  = as.character(s$economy[1]),
                  sub   = if (length(programs)) paste(programs, collapse = " / "),
                  pills = as.character(sort(unique(s$year)))
                )
              }
            )

            selection_summary_card(
              title = "Selected sample",
              rows  = econ_rows,
              badge = badge
            )
          })

          output$policy_stats <- make_stats_dt(survey_data, variable_list,
            vars = policy_vars,
            base = stats_base
          )

          # ---- Outcome summary moved to the Outcome stats tab's selection card --

          # Append Survey stats tab to parent tabset
          tryCatch(
            shiny::appendTab(
              inputId = tabset_id,
              shiny::tabPanel(
                title = "Survey stats",
                value = "desc_stats",
                uiOutput(ns("selected_surveys_card")),
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  # gap = 0: bslib's default body gap would otherwise put
                  # 24px between the heading and the plot; the heading's own
                  # margin is the spacing that remains.
                  bslib::card(
                    bslib::card_body(
                      gap = 0,
                      h4(
                        "Timing of interviews",
                        class = "mb-2",
                        info_popover(
                          title = "Timing of interviews",
                          p("Monthly breakdown of interview waves.")
                        )
                      ),
                      wise_plot_output(ns("interview_date"),
                        "Bar plot of the distribution of interview dates across the selected surveys",
                        height = "300px"
                      )
                    )
                  ),
                  # Pairing a definite card height with a 100%-height map is what
                  # lets the map fill the card in both the normal and the
                  # expanded state; a fixed pixel height would stay small when
                  # the card fans out. The title shares one row with the wave
                  # toggle so the map keeps as much of the card as possible,
                  # expanded or not.
                  bslib::card(
                    full_screen = TRUE,
                    height = "400px",
                    # The explicit card_body with gap = 0: bslib's default body
                    # gap would otherwise put 24px between the controls row and
                    # the map; the controls' own mb-2 is the spacing that
                    # remains.
                    bslib::card_body(
                      gap = 0,
                      shiny::div(
                        class = paste(
                          "d-flex align-items-center",
                          "justify-content-between flex-wrap gap-2 mb-2"
                        ),
                        h4(
                          "Location of interviews",
                          class = "mb-0",
                          info_popover(
                            title = "Location of interviews",
                            p(paste(
                              "Geographic distribution of sampled interviews.",
                              "Each hexagon is an H3 cell shaded by how many",
                              "sampled units fall in it; cells tile without",
                              "overlapping, so dense areas read directly off the",
                              "colour. Pick the survey wave on the right."
                            ))
                          )
                        ),
                        shiny::uiOutput(ns("map_wave_ui"), inline = TRUE)
                      ),
                      # The MapLibre hex map. hexmap_ui() is placed directly in
                      # the card (no renderUI): the container persists for the
                      # session and the payload observer drives everything.
                      hexmap_ui(
                        ns("density_map"),
                        height = "100%",
                        aria_label = paste0(
                          "Map of sample density: number of sampled units ",
                          "per hexagonal area cell"
                        ),
                        legend = shiny::uiOutput(ns("map_legend_ui"))
                      ) |>
                        bslib::as_fill_carrier()
                    )
                  )
                ),
                h4(
                  "Outcome stats",
                  info_popover(
                    title = "Outcome stats",
                    p(paste(
                      "Candidate outcome variables available for welfare",
                      "analysis in Step 1. Check the missingness column",
                      "before selecting an outcome - high missingness can",
                      "limit sample size after listwise deletion."
                    ))
                  )
                ),
                p(class = "text-muted small", "Candidate outcome variables for welfare analysis"),
                DT::DTOutput(ns("outcome_stats")),
                h4("Policy variables"),
                p(class = "text-muted small", "Variables that can be adjusted in Step 3 policy scenarios"),
                DT::DTOutput(ns("policy_stats")),
                uiOutput(ns("characteristic_tables_ui"))
              ),
              select = TRUE,
              session = tabset_session
            ),
            error = function(e) {
              notify(paste("Failed to add Survey stats tab:", conditionMessage(e)), type = "error")
            }
          )

          survey_tab_added(TRUE)
        }

        if (survey_tab_added()) select_tab("desc_stats")
      },
      ignoreInit = TRUE,
      ignoreNULL = TRUE
    )

    # ---- Return API ---------------------------------------------------------

    list(
      survey_data = survey_data,
      cell_data = cell_data,
      survey_version = survey_version,
      survey_data_generation = survey_data_generation,
      load_done = load_done,
      load_status = load_status
    )
  })
}
