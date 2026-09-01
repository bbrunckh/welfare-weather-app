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
    map_data     <- reactiveVal(NULL)
    # Per-H3-cell counts behind the "Sample density" view of the same map,
    # recomputed for whichever wave the picker is on. Cheap: it is a regrouping
    # of data already in memory, no round trip to the store.
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

    # ---- Load and prepare data on button click ------------------------------

    observeEvent(input$survey_stats, {
      req(nrow(selected_surveys()) > 0)
      if (!load_guard$begin()) return(invisible(NULL))
      on.exit(load_guard$end(), add = TRUE)

      busy_id <- showNotification("Loading survey data...", duration = NULL, type = "message")
      on.exit(removeNotification(busy_id), add = TRUE)

      # INT-06: drop the previous survey's map/cell state as soon as a reload
      # starts, and on every inner failure below. The map output re-renders
      # reactively, so the previous survey's geography can never outlive its
      # microdata - a failure now leaves a blank map instead of a misleading
      # one.
      map_data(NULL)
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

          loc_df <- h3_local |>
            dplyr::summarise(
              # Emit GeoJSON string directly from DuckDB - no WKB or sf needed
              geom = st_asgeojson(st_union_agg(st_geomfromtext(h3_cell_to_boundary_wkt(h3)))),
              .by  = c(code, year, survname, loc_id)
            ) |>
            collect_deterministic(c("code", "year", "survname", "loc_id")) |>
            dplyr::filter(!is.na(geom), nchar(geom) > 2)   # drop NULLs and empty "{}"

          # Assemble a GeoJSON FeatureCollection
          features <- lapply(seq_len(nrow(loc_df)), function(i) {
            row <- loc_df[i, ]
            list(
              type      = "Feature",
              geom_json = row$geom,                     # raw string for addGeoJSON
              properties = list(
                code     = row$code,
                year     = row$year,
                survname = row$survname,
                loc_id   = row$loc_id
              )
            )
          })

          geojson <- list(type = "FeatureCollection", features = features)
          map_data(geojson)

        }, error = function(e) {
          notify(paste("Failed to build map data:", conditionMessage(e)), type = "warning", duration = 5)
        })

        # -- Sample density heatmap -------------------------------------------
        # One hexagon per H3 cell rather than one polygon per location: cells
        # tile without overlapping, so the sample's density reads directly off
        # the colour instead of a pile of outlines.
        tryCatch({
          cell_geo <- h3_local |>
            dplyr::distinct(h3) |>
            dplyr::mutate(
              geom = st_asgeojson(st_geomfromtext(h3_cell_to_boundary_wkt(h3)))
            ) |>
            collect_deterministic("h3")

          cell_map <- h3_local |>
            dplyr::select(code, year, survname, loc_id, h3, pop_2020) |>
            collect_deterministic(c("code", "year", "survname", "loc_id", "h3"))

          cell_data(list(geom = cell_geo, map = cell_map))
        }, error = function(e) {
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
        }, error = function(e) {
          # INT-06: loc_id_panel is not a cosmetic join - downstream VCV
          # estimation falls back when it is missing, which changes inference.
          notify(paste0(
            "Failed to compute loc_id_panel: ", conditionMessage(e), "\n",
            "Location-level panels are unavailable, so variance estimation ",
            "will fall back to survey-design defaults. Treat inference ",
            "accordingly."
          ), type = "warning", duration = 8)
        })
      }

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

        # Leaflet map of interview locations.
        # Keep the view across a switch between Locations and Sample density,
        # and across a reload - rebuilding the widget would otherwise snap
        # back to the full extent.
        map_view_mem <- map_view_memory(input, session, "map")
        map_view_mem$remember()

        output$map <- leaflet::renderLeaflet({
          wave <- input$map_wave %||% "all"

          m <- if (identical(input$map_view, "density")) {
            unit <- if (is.function(analysis_unit)) analysis_unit() else NULL
            plot_sample_density_map(
              density_cells(wave),
              unit_label = switch(unit %||% "hh",
                                  ind = "individuals", firm = "firms",
                                  "households")
            )
          } else {
            plot_survey_map(filter_features_by_wave(map_data(), wave))
          }
          req(!is.null(m))
          map_view_mem$restore(m)
        })

        # Wave picker, shown only when there is more than one wave to pick.
        output$map_wave_ui <- shiny::renderUI({
          w <- survey_wave_list(survey_data())
          if (is.null(w) || nrow(w) < 2) return(NULL)
          shiny::selectInput(
            ns("map_wave"), NULL,
            choices  = c(stats::setNames("all", "All waves"),
                         stats::setNames(w$key, w$label)),
            selected = shiny::isolate(input$map_wave) %||% "all",
            width    = "160px"
          ) |>
            htmltools::tagAppendAttributes(
              style = "margin-bottom: 0;", class = "small"
            )
        })

        # Toggle between outlined locations and the per-cell heatmap.
        output$map_view_ui <- shiny::renderUI({
          shiny::radioButtons(
            ns("map_view"), NULL, inline = TRUE,
            selected = shiny::isolate(input$map_view) %||% "locations",
            choiceNames  = list("Locations", "Sample density"),
            choiceValues = list("locations", "density")
          ) |>
            htmltools::tagAppendAttributes(
              style = "margin-bottom: 0;", class = "small"
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
                  plotOutput(ns("interview_date"), height = "300px")
                ),
                # Pairing a definite card height with a 100%-height map is what
                # lets the map fill the card in both the normal and the
                # expanded state; a fixed pixel height would stay small when
                # the card fans out.
                # Title and view toggle share one row so the map keeps as much
                # of the card as possible, expanded or not.
                bslib::card(
                  full_screen = TRUE,
                  height      = "400px",
                  shiny::div(
                    class = paste("d-flex align-items-center",
                                  "justify-content-between flex-wrap gap-2 mb-2"),
                    h4(
                      "Location of interviews", class = "mb-0",
                      info_popover(
                        title = "Location of interviews",
                        p(paste(
                          "Geographic distribution of sampled interviews.",
                          "'Locations' outlines each survey location;",
                          "'Sample density' shades H3 cells by how many",
                          "sampled units fall in them."
                        ))
                      )
                    ),
                    shiny::div(
                      class = "d-flex align-items-center gap-2 flex-wrap",
                      shiny::uiOutput(ns("map_wave_ui"), inline = TRUE),
                      shiny::uiOutput(ns("map_view_ui"), inline = TRUE)
                    )
                  ),
                  leaflet::leafletOutput(ns("map"), height = "100%")
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
      survey_data = survey_data,
      map_data    = map_data,
      cell_data   = cell_data
    )
  })
}
