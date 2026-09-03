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
    tabset_session = NULL,
    analysis_unit  = NULL
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
      req(nrow(selected_surveys()) > 0)
      actionButton(ns("survey_stats"), "Survey stats", class = "btn-primary", style = "width: 100%;")
    })

    shiny::outputOptions(output, "survey_stats_button_ui", suspendWhenHidden = FALSE)

    # REACT-02: double-click guard - one load at a time, button disabled
    # while running.
    load_guard <- .busy_guard(session, survey_stats)

    survey_tab_added <- reactiveVal(FALSE)

    # ---- Data storage -------------------------------------------------------

    survey_data  <- reactiveVal(NULL)
    # INT-08: bumped on every successful survey load; downstream run
    # signatures include it so a reload invalidates fit/sim/policy results
    # even when the selection string is unchanged.
    survey_version <- shiny::reactiveVal(0L)
    # Per-H3-cell counts behind the density map, recomputed for whichever
    # wave the picker is on. Cheap: it is a regrouping of data already in
    # memory, no round trip to the store.
    density_cells <- function(wave = "all") {
      cd <- cell_data()
      df <- survey_data()
      if (is.null(cd) || is.null(df)) return(NULL)

      alloc <- allocate_units_to_cells(
        filter_by_wave(cd$map, wave), filter_by_wave(df, wave)
      )
      if (is.null(alloc)) return(NULL)

      dplyr::inner_join(cd$geom, alloc, by = "h3") |>
        dplyr::filter(!is.na(geom), nchar(geom) > 2)
    }
    # Cell geometry plus the location-to-cell mapping, shared with the outcome
    # and weather maps so they can merge overlapping locations onto cells.
    cell_data    <- reactiveVal(NULL)
    # REACT-03: digest of the last successfully completed load request.
    last_load_sig <- reactiveVal(NULL)

    # ---- Load and prepare data on button click ------------------------------

    observeEvent(input$survey_stats, {
      req(nrow(selected_surveys()) > 0)
      if (!load_guard$begin()) return(invisible(NULL))
      on.exit(load_guard$end(), add = TRUE)

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
                         duration = 3, type = "message")
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
      df       <- df |>
        assign_data_level() |>
        convert_lcu_to_ppp(cpi_ppp(), lcu_vars) |>
        bottom_code_welfare(0.28) |>
        apply_policy_derivations()

      survey_data(df)

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
          notify(paste("Failed to load H3 data:", conditionMessage(e)), type = "warning", duration = 5)
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
        h3_local <- tryCatch({
          nm <- basename(tempfile(pattern = "ss_h3_"))
          local_h3 <- dplyr::compute(h3_df, name = nm, temporary = TRUE)
          on.exit(
            try(DBI::dbRemoveTable(dbplyr::remote_con(local_h3), nm), silent = TRUE),
            add = TRUE
          )
          local_h3
        }, error = function(e) {
          notify(paste("Could not cache H3 data locally; continuing remote:",
                       conditionMessage(e)), type = "warning", duration = 5)
          h3_df
        })

        tryCatch({
          con <- dbplyr::remote_con(h3_local)
            .duck_load_ext("spatial")
            .duck_load_ext("h3")

          # -- Sample density map -------------------------------------------
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
        }, error = function(e) {
          load_ok <<- FALSE
          notify(paste("Failed to build sample density map:", conditionMessage(e)),
                 type = "warning", duration = 5)
        })

        tryCatch({
          panel_map <- loc_panel(h3_local, id_col = loc_id, h3_col = h3, weight_col = pop_2020,
                                    group_cols = c("code", "year", "survname"))

          loc_keys <- h3_local |>
            dplyr::distinct(code, year, survname, loc_id) |>
            collect_deterministic(c("code", "year", "survname", "loc_id"))

          df <- df |>
            dplyr::left_join(
              dplyr::left_join(loc_keys, panel_map, by = c("code", "year", "survname", "loc_id")),
              by = c("code", "year", "survname", "loc_id")
            )
          survey_data(df)
          survey_version(survey_version() + 1L)
        }, error = function(e) {
          # INT-06: loc_id_panel is not a cosmetic join - downstream VCV
          # estimation falls back when it is missing, which changes inference.
          load_ok <<- FALSE
          notify(paste0(
            "Failed to compute loc_id_panel: ", conditionMessage(e), "\n",
            "Location-level panels are unavailable, so variance estimation ",
            "will fall back to survey-design defaults. Treat inference ",
            "accordingly."
          ), type = "warning", duration = 8)
        })
      } else {
        load_ok <- FALSE
      }

      if (load_ok) last_load_sig(sig)

      notify(
        paste0("Loaded ", nrow(ss), " survey file(s) - ", nrow(df), " rows."),
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

        # Unit label for legend/tooltip text ("households", "individuals",
        # "firms"), resolved live so an analysis-unit switch re-labels.
        unit_label <- function() {
          unit <- if (is.function(analysis_unit)) analysis_unit() else NULL
          switch(unit %||% "hh", ind = "individuals", firm = "firms",
                 "households")
        }

        # ---- Leaflet fallback (WebGL unavailable) ---------------------------
        # The widget only rebuilds on data loads. Wave changes swap the
        # GeoJSON layer in place (observer below) - no widget rebuild, no
        # basemap re-fetch, and the user's pan/zoom survives without any
        # view-memory machinery. map_view_memory stays Leaflet-only: the
        # MapLibre container persists across payloads, so it keeps pan/zoom
        # by construction.
        map_view_mem <- map_view_memory(
          input, session, "map",
          key = shiny::reactive(digest::digest(selected_surveys()))
        )
        map_view_mem$remember()

        output$map <- leaflet::renderLeaflet({
          req(isFALSE(input$density_map_webgl))

          wave <- shiny::isolate(input$map_wave %||% "all")
          m <- plot_sample_density_map(density_cells(wave),
                                       unit_label = unit_label())
          req(!is.null(m))
          map_view_mem$restore(m)
        })

        # ---- MapLibre density payload stream ---------------------------------
        # One reactive observer drives the hex map: data loads and wave
        # toggles both land here as fresh `set` payloads (cheap - cell ids
        # and values only, no geometry serialization). The camera is fitted
        # only when the data key changes (PERF-36 view-key semantics), so a
        # wave toggle re-colours in place and the user's pan/zoom survives.
        density_key <- reactiveVal(NULL)
        density_lgd <- reactiveVal(NULL)
        observe({
          shiny::req(!isFALSE(input$density_map_webgl))  # fallback drives itself

          cd   <- cell_data()
          wave <- input$map_wave %||% "all"

          pl <- if (is.null(cd)) NULL else {
            .density_hex_payload(density_cells(wave), unit_label())
          }
          if (is.null(pl)) {
            hexmap_clear(session, ns, "density_map")
            density_lgd(NULL)
          } else {
            hexmap_update(session, ns, "density_map", pl$payload)
            key <- digest::digest(selected_surveys())
            if (!identical(key, density_key())) {
              hexmap_fit(session, ns, "density_map", pl$payload$bounds)
              density_key(key)
            }
            density_lgd(pl$legend)
          }
        })

        # R-side legend: same palette state as the payloads, rebuilt per wave
        # and positioned over the map's bottom-right corner by hexmap_ui().
        output$map_legend_ui <- shiny::renderUI({
          lgd <- density_lgd()
          shiny::req(!is.null(lgd))
          htmltools::HTML(.compact_legend_html(
            pal_info = lgd$pal_info,
            binned   = lgd$binned,
            title    = lgd$title,
            info     = lgd$info
          ))
        })

        # Surface switch: MapLibre container (default, optimistic) or the
        # Leaflet fallback once the browser reports WebGL as unavailable.
        output$map_surface_ui <- shiny::renderUI({
          if (isFALSE(input$density_map_webgl)) {
            shiny::tags$div(
              style = "position: relative; height: 100%;",
              leaflet::leafletOutput(ns("map"), height = "100%")
            )
          } else {
            hexmap_ui(
              ns("density_map"),
              height     = "100%",
              aria_label = paste0(
                "Map of sample density: number of sampled ",
                unit_label(), " per hexagonal area cell"
              ),
              legend = shiny::uiOutput(ns("map_legend_ui"))
            )
          }
        })

        # Wave-only changes: swap the fallback layer in place. The MapLibre
        # path needs nothing here - its payload observer tracks map_wave.
        observeEvent(input$map_wave,
          {
            if (!isFALSE(input$density_map_webgl)) return()
            wave <- input$map_wave %||% "all"
            fc   <- .sample_density_fc(density_cells(wave))
            px   <- leaflet::leafletProxy(ns("map"), session)
            leaflet::removeGeoJSON(px, "density-cells")
            if (!is.null(fc)) {
              leaflet::addGeoJSON(
                px,
                geojson     = fc$fc,
                layerId     = "density-cells",
                stroke      = FALSE,
                color       = "#000000",
                weight      = 1,
                opacity     = 0.5,
                fill        = TRUE,
                fillOpacity = 0.75
              )
            }
          },
          ignoreInit = TRUE
        )

        # Wave toggle slider, shown only when there is more than one wave to pick.
        output$map_wave_ui <- shiny::renderUI({
          w <- survey_wave_list(survey_data())
          if (is.null(w) || nrow(w) < 2) return(NULL)
          choices <- wave_slider_choices(w, include_all = TRUE)
          selected <- shiny::isolate(input$map_wave) %||% "all"
          if (!selected %in% choices) selected <- "all"
          wave_toggle_slider(
            ns("map_wave"),
            choices  = choices,
            selected = selected
          )
        })

        output$outcome_stats <- make_stats_dt(survey_data, variable_list, "outcome")
        output$ind_stats     <- make_stats_dt(survey_data, variable_list, "ind")
        output$hh_stats      <- make_stats_dt(survey_data, variable_list, "hh")
        output$firm_stats    <- make_stats_dt(survey_data, variable_list, "firm")
        output$area_stats    <- make_stats_dt(survey_data, variable_list, "area")

        # Only show characteristic tables relevant to the selected level of
        # analysis: individual level implies household + area also apply;
        # household level implies area also applies; firm level is separate.
        output$characteristic_tables_ui <- renderUI({
          unit <- if (is.function(analysis_unit)) analysis_unit() else NULL
          show_ind  <- is.null(unit) || unit == "ind"
          show_hh   <- is.null(unit) || unit %in% c("ind", "hh")
          show_firm <- is.null(unit) || unit == "firm"

          tagList(
            if (show_ind) tagList(
              h4("Individual characteristics"),
              p(class = "text-muted small", "Summary statistics for individual-level variables"),
              DT::DTOutput(ns("ind_stats"))
            ),
            if (show_hh) tagList(
              h4("Household characteristics"),
              p(class = "text-muted small", "Summary statistics for household-level variables"),
              DT::DTOutput(ns("hh_stats"))
            ),
            if (show_firm) tagList(
              h4("Firm characteristics"),
              p(class = "text-muted small", "Summary statistics for firm-level variables"),
              DT::DTOutput(ns("firm_stats"))
            ),
            h4("Area characteristics"),
            p(class = "text-muted small", "Summary statistics for area-level variables"),
            DT::DTOutput(ns("area_stats"))
          )
        })

        policy_vars <- unique(unlist(lapply(POLICY_DEFINITIONS, `[[`, "vars")))
        output$policy_stats  <- make_stats_dt(survey_data, variable_list,
                                              vars = policy_vars)

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
                bslib::card(
                  h4(
                    "Timing of interviews", class = "mb-2",
                    info_popover(
                      title = "Timing of interviews",
                      p("Monthly breakdown of interview waves.")
                    )
                  ),
                  wise_plot_output(ns("interview_date"),
                                   "Bar plot of the distribution of interview dates across the selected surveys",
                                   height = "300px")
                ),
                # Pairing a definite card height with a 100%-height map is what
                # lets the map fill the card in both the normal and the
                # expanded state; a fixed pixel height would stay small when
                # the card fans out. The title shares one row with the wave
                # toggle so the map keeps as much of the card as possible,
                # expanded or not.
                bslib::card(
                  full_screen = TRUE,
                  height      = "400px",
                  shiny::div(
                    class = paste("d-flex align-items-center",
                                  "justify-content-between flex-wrap gap-2 mb-2"),
                    h4(
                      "Sample density", class = "mb-0",
                      info_popover(
                        title = "Sample density",
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
                  # The MapLibre hex map, or the Leaflet fallback when the
                  # browser reports WebGL as unavailable.
                  shiny::uiOutput(ns("map_surface_ui")) |>
                    bslib::as_fill_carrier()
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
              uiOutput(ns("characteristic_tables_ui")),
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
      survey_data    = survey_data,
      cell_data      = cell_data,
      survey_version = survey_version
    )
  })
}
