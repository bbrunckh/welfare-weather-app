#' 2_01_weathersim UI Function
#'
#' @description A shiny Module. Unified sidebar for configuring and running
#'   both historical and future welfare simulations.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_01_weathersim_ui <- function(id) {

  ns <- NS(id)

  tagList(
    # ---- Settings summary banner (always visible) --------------------------
    shiny::htmlOutput(ns("settings_summary")),

    # ---- Collapsible simulation settings -----------------------------------
    shiny::tags$details(
      id = ns("settings_details"),
      shiny::tags$summary(
        style = "cursor:pointer; font-weight:600; font-size:13px; margin-bottom:8px;",
        "Simulation settings \u25bc"
      ),

      # -- Baseline survey ------------------------------------------------
      shiny::tags$h6("Baseline survey",
                     style = "font-weight:600; margin-top:8px; margin-bottom:4px;"),
      shiny::uiOutput(ns("baseline_survey_ui")),
      shiny::uiOutput(ns("baseline_warning_ui")),
      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Historical period --------------------------------------------------
      shiny::tags$h6("Historical weather distribution period",
                     style = "font-weight:600; margin-top:8px; margin-bottom:4px;"),
      shiny::sliderInput(
        inputId = ns("hist_years"),
        label   = NULL,
        min     = 1950,
        max     = 2024,
        value   = c(1991, 2020),
        sep     = ""
      ),
      shiny::uiOutput(ns("hist_years_warning")),
      shiny::helpText(
        tags$b("Note:"), " Historical weather informs the underlying variability,",
        " which is then perturbed with climate scenario forecasts.",
        tags$b(" 30 years is the recommended default."),
        style = "font-size: 11px; color: #555; margin-top: 2px; margin-bottom: 8px;"
      ),

      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Future periods (up to 3) -------------------------------------------
      shiny::tags$h6("Projection periods",
                     style = "font-weight:600; margin-bottom:4px;"),

      # Period 1
      shiny::tags$div(
        style = "display:flex; gap:8px; align-items:center; margin-bottom:4px;",
        shiny::tags$span("Period 1:", style = "min-width:60px; font-weight:500;"),
        shiny::numericInput(ns("fut_start_1"), label = NULL, value = 2025,
                            min = 2015, max = 2100, step = 1, width = "90px"),
        shiny::tags$span("\u2013"),
        shiny::numericInput(ns("fut_end_1"), label = NULL, value = 2035,
                            min = 2015, max = 2100, step = 1, width = "90px")
      ),
      # Period 2 (optional)
      shiny::tags$div(
        style = "display:flex; gap:8px; align-items:center; margin-bottom:4px;",
        shiny::tags$span("Period 2:", style = "min-width:60px; font-weight:500;"),
        shiny::numericInput(ns("fut_start_2"), label = NULL, value = NA,
                            min = 2015, max = 2100, step = 1, width = "90px"),
        shiny::tags$span("\u2013"),
        shiny::numericInput(ns("fut_end_2"), label = NULL, value = NA,
                            min = 2015, max = 2100, step = 1, width = "90px")
      ),
      # Period 3 (optional)
      shiny::tags$div(
        style = "display:flex; gap:8px; align-items:center; margin-bottom:4px;",
        shiny::tags$span("Period 3:", style = "min-width:60px; font-weight:500;"),
        shiny::numericInput(ns("fut_start_3"), label = NULL, value = NA,
                            min = 2015, max = 2100, step = 1, width = "90px"),
        shiny::tags$span("\u2013"),
        shiny::numericInput(ns("fut_end_3"), label = NULL, value = NA,
                            min = 2015, max = 2100, step = 1, width = "90px")
      ),

      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Climate scenarios ---------------------------------------------------
      shiny::tags$h6("Climate scenarios",
                     style = "font-weight:600; margin-bottom:4px;"),
      shiny::checkboxGroupInput(
        inputId  = ns("climate"),
        label    = NULL,
        choices  = c(
          "SSP2-4.5" = "ssp2_4_5",
          "SSP3-7.0" = "ssp3_7_0",
          "SSP5-8.5" = "ssp5_8_5"
        ),
        selected = "ssp3_7_0"
      ),

      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Residual method ----------------------------------------------------
      shiny::tags$h6("Simulation residuals",
                     style = "font-weight:600; margin-bottom:4px;"),
      shiny::radioButtons(
        inputId  = ns("residuals"),
        label    = NULL,
        choices  = residual_choices(),
        selected = "original"
      ),
      shiny::helpText(
        shiny::tags$b("original:"), " Recommended Default: match each observation\u2019s own residual -  assumes no changes due to changing hazards ",
        shiny::tags$br(),
        shiny::tags$b("resample:"), " Secondary Recommendation: randomly resample residuals from the model.",
        shiny::tags$br(),
        shiny::tags$b("normal:"), " Caution: draw residuals from N(0, \u03c3) which assumes normal tails and no heteroskedasticity",
        shiny::tags$br(),
        shiny::tags$b("none:"), " DIAGNOSTIC: return fitted values only. not including residuals understates variance and for any log-transformed variable will understate mean",
        shiny::tags$br(),
        style = "font-size:11px;"
      )
    ),

    shiny::tags$hr(style = "margin: 10px 0;"),

    # ---- Run simulation button (hidden for RIF engine) ---------------------
    shiny::uiOutput(ns("run_sim_ui"))
  )
}


#' 2_01_weathersim Server Functions
#'
#' Handles the unified simulation sidebar: validates settings, runs historical
#' and future simulations on button click, and returns reactive results.
#'
#' @param id               Module id.
#' @param connection_params Reactive named list from mod_0_overview.
#' @param selected_outcome Reactive one-row data frame of selected outcome.
#' @param selected_weather Reactive data frame of selected weather variables.
#' @param selected_surveys Reactive data frame from the survey list.
#' @param survey_weather   Reactive data frame of merged survey-weather data.
#' @param model_fit        Reactive list with fit3, engine, train_data.
#'
#' @noRd
mod_2_01_weathersim_server <- function(id,
                                        connection_params,
                                        selected_outcome,
                                        selected_weather,
                                        selected_surveys,
                                        survey_weather,
                                        model_fit,
                                        stored_breaks = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Internal state ----------------------------------------------------
    hist_sim        <- reactiveVal(NULL)
    saved_scenarios <- reactiveVal(list())

    # ---- Baseline survey reactives ----------------------------------------

    # Derive available survey x year choices from survey_weather.
    # Returns a named character vector: label -> "survname|year" value.
    # ---- Baseline survey reactives -----------------------------------------

    baseline_survey_choices <- reactive({
      req(survey_weather())
      svy  <- survey_weather()
      if (!all(c('code', 'survname', 'year') %in% names(svy))) return(character(0))
      combos <- unique(svy[, c('code', 'survname', 'year')])
      combos <- combos[order(combos$code, combos$year), ]
      # Join economy (country) name from selected_surveys via code column.
      # NOTE: code (e.g. TGO, GNB) is unique per country; survname (e.g. EHCVM)
      # is shared across countries in the same survey programme and must NOT
      # be used as the join key -- this was the bug causing Togo to disappear.
      ss <- tryCatch(selected_surveys(), error = function(e) NULL)
      if (!is.null(ss) && all(c('code', 'economy') %in% names(ss))) {
        lbl_map <- unique(ss[, c('code', 'economy')])
        combos  <- merge(combos, lbl_map, by = 'code', all.x = TRUE)
        combos$economy[is.na(combos$economy)] <- combos$code[is.na(combos$economy)]
      } else {
        combos$economy <- combos$code
      }
      vals <- paste0(combos$code, '|', combos$year)
      lbls <- paste0(combos$economy, ' ', combos$year)
      setNames(vals, lbls)
    })

    # Default selection: latest year per unique economy (code), not survname.
    baseline_default <- reactive({
      ch <- baseline_survey_choices()
      if (length(ch) == 0) return(character(0))
      df <- data.frame(
        val      = ch,
        code = sub("^(.*?)\\|.*$", "\\1", ch),
        year = as.integer(sub("^.*\\|", "", ch)),
        stringsAsFactors = FALSE
      )

      latest <- tapply(df$year, df$code, max)
      keep   <- mapply(function(c, y) latest[c] == y, df$code, df$year)
      unname(ch[keep])
    })


    # ---- Settings summary banner -------------------------------------------

    output$baseline_survey_ui <- shiny::renderUI({
      ch  <- baseline_survey_choices()
      def <- baseline_default()
      if (length(ch) == 0)
        return(shiny::helpText("No survey data loaded.", style = "font-size:11px;"))
      shiny::selectInput(
        ns("baseline_survey"),
        label    = NULL,
        choices  = ch,
        selected = def,
        multiple = TRUE,
        selectize = TRUE
      )
    })

    output$baseline_warning_ui <- shiny::renderUI({
      sel <- input$baseline_survey %||% baseline_default()
      if (length(sel) <= 1) return(NULL)
      # Multiple economies or years selected -- show warning
      n_economies <- length(unique(sub("\\|.*$", "", sel)))
      n_years     <- length(unique(sub("^.*\\|", "", sel)))
      if (n_economies > 1 || n_years > 1) {
        shiny::helpText(
          shiny::tags$b("\u26a0 Warning:"),
           "Using multiple survey years or economies is not recommended. Requires normalizing weights based on sample design, which is not currently implemented — verify before interpreting results.",
          style = "color: #c0392b; font-size: 11px; margin-top: 2px;"
        )
      }
    })

    output$settings_summary <- shiny::renderUI({
      hist_yr <- input$hist_years %||% c(1991, 2020)

      # Collect future periods
      fut_parts <- character(0)
      for (i in 1:3) {
        s <- input[[paste0("fut_start_", i)]]
        e <- input[[paste0("fut_end_", i)]]
        if (!is.null(s) && !is.na(s) && !is.null(e) && !is.na(e)) {
          fut_parts <- c(fut_parts, paste0(s, "\u2013", e))
        }
      }
      fut_txt <- if (length(fut_parts) > 0) paste(fut_parts, collapse = ", ") else "None"

      ssp_map <- c(
        "ssp2_4_5" = "SSP2-4.5",
        "ssp3_7_0" = "SSP3-7.0",
        "ssp5_8_5" = "SSP5-8.5"
      )
      ssp_sel <- input$climate %||% character(0)
      ssp_txt <- if (length(ssp_sel) > 0)
        paste(ssp_map[ssp_sel], collapse = ", ") else "None"

      ens_txt <- "All models"

      res_labels <- c(
        "original"  = "Original",
        "resample" = "Resample",
        "none"      = "None",
        "normal"    = "Normal distribution"
      )
      res_txt <- res_labels[input$residuals %||% "normal"] %||% input$residuals

      shiny::tags$div(
        style = paste0(
          "border-left: 3px solid #2166ac; background: #f4f8fd; ",
          "padding: 8px 12px; margin-bottom: 10px; border-radius: 3px; ",
          "font-size: 12px; color: #444;"
        ),
        shiny::tags$b("Historical baseline:", style = "color:#333;"),
        paste0(hist_yr[1], "\u2013", hist_yr[2]),
        shiny::tags$br(),
        shiny::tags$b("Projection periods:", style = "color:#333;"), fut_txt,
        shiny::tags$b(" \u00b7 SSPs:", style = "color:#333;"), ssp_txt,
        shiny::tags$b(" \u00b7 Ensemble result:", style = "color:#333;"), ens_txt,
        shiny::tags$br(),
        shiny::tags$b("Simulation residuals:", style = "color:#333;"), res_txt,
        shiny::tags$br(),
        shiny::tags$b("Baseline survey:", style = "color:#333;"), {
          sel  <- input$baseline_survey %||% baseline_default()
          ch   <- baseline_survey_choices()
          nms  <- names(ch)[ch %in% sel]
          if (length(nms) == 0) "None" else paste(nms, collapse = ", ")
        }
      )
    })

    # ---- 30-year minimum window warning ------------------------------------

    output$hist_years_warning <- shiny::renderUI({
      req(input$hist_years)
      if (length(input$hist_years[1]:input$hist_years[2]) < 30) {
        shiny::helpText(
          "\u26a0\ufe0f Window is less than 30 years. This may not capture the full range of weather variability, which could lead to underestimation of future risks.",
          style = "color: #c0392b; font-size: 12px;"
        )
      }
    })

    # ---- Derived config reactives ------------------------------------------

    # survey_weather filtered to the selected baseline rows.
    # Used in place of survey_weather() inside observeEvent(run_sim).
    baseline_svy <- reactive({
      sel <- input$baseline_survey %||% baseline_default()
      if (length(sel) == 0) return(survey_weather())
      svy <- survey_weather()
      vals <- paste0(svy$code, "|", as.character(svy$year))
      svy[vals %in% sel, , drop = FALSE]
    })

    selected_hist <- reactive({
      req(input$hist_years)
      data.frame(
        type          = "historical",
        year_range    = I(list(input$hist_years)),
        residuals     = input$residuals %||% "normal",
        scenario_name = paste0("Historical / ",
                               input$hist_years[1], "-", input$hist_years[2]),
        stringsAsFactors = FALSE
      )
    })

    future_periods <- reactive({
      periods <- list()
      for (i in 1:3) {
        s <- input[[paste0("fut_start_", i)]]
        e <- input[[paste0("fut_end_", i)]]
        if (!is.null(s) && !is.na(s) && !is.null(e) && !is.na(e) && e > s) {
          periods[[length(periods) + 1L]] <- c(s, e)
        }
      }
      periods
    })

    selected_fut <- reactive({
      req(input$climate)
      fp <- future_periods()
      if (length(fp) == 0) return(NULL)

      ssp_choices <- c(
        "ssp2_4_5" = "SSP2-4.5",
        "ssp3_7_0" = "SSP3-7.0",
        "ssp5_8_5" = "SSP5-8.5"
      )

      rows <- lapply(input$climate, function(ssp) {
        prefix <- ssp_choices[[ssp]]
        lapply(fp, function(yr) {
          scene_name <- paste0(prefix, " / ", yr[1], "-", yr[2])
          data.frame(
            type          = "future",
            year_range    = I(list(yr)),
            ssp           = ssp,
            method        = "delta",
            residuals     = input$residuals %||% "normal",
            scenario_name = scene_name,
            stringsAsFactors = FALSE
          )
        })
      })

      do.call(rbind, do.call(c, rows))
    })

    # ---- Run simulation button (hidden for RIF engine) --------------------

    output$run_sim_ui <- shiny::renderUI({
      mf <- model_fit()
      if (!is.null(mf) && identical(mf$engine, "rif")) {
        shiny::div(
          class = "alert alert-warning",
          style = "font-size: 13px; margin-top: 4px;",
          shiny::tags$b("\u26a0 Simulations are not yet implemented for Quantile Regression (RIF)."),
          " Please select a different model engine to run simulations."
        )
      } else {
        shiny::actionButton(
          ns("run_sim"),
          label = "Run simulation",
          class = "btn-primary",
          icon  = shiny::icon("play"),
          style = "width: 100%; margin-top: 4px;"
        )
      }
    })

    # ---- Run simulation on button click ------------------------------------

    observeEvent(input$run_sim, {
      req(selected_weather(), selected_outcome(),
          survey_weather(), selected_hist(), model_fit())

      sw  <- selected_weather()
      so  <- selected_outcome()
      sh  <- selected_hist()
      svy <- survey_weather()
      ss  <- selected_surveys()
      mf  <- model_fit()
      cp  <- connection_params()

      model      <- mf$fit3
      engine     <- mf$engine
      train_data <- mf$train_data
      residuals  <- sh$residuals

      sim_dates <- build_hist_sim_dates(svy, unlist(sh$year_range))

      # ---- Build get_weather() arguments ------------------------------------
      fut_periods <- future_periods()
      sf          <- selected_fut()
      has_future  <- !is.null(sf) && length(fut_periods) > 0
      ssps        <- if (has_future) unique(sf$ssp) else character(0)
      perturbation_method <- if (has_future) build_perturbation_method(sw) else NULL

      # Build the list of future_period vectors (or NULL for historical-only)
      fp_list <- if (has_future) {
        lapply(fut_periods, function(yr) {
          c(paste0(yr[1], "-01-01"), paste0(yr[2], "-12-31"))
        })
      } else {
        NULL
      }

      ssp_labels <- c(
        "ssp2_4_5" = "SSP2-4.5",
        "ssp3_7_0" = "SSP3-7.0",
        "ssp5_8_5" = "SSP5-8.5"
      )

      shiny::withProgress(
        message = "Running simulation...",
        value   = 0,
        {
          new_scenarios    <- list()
          hist_sim_result  <- NULL
          cached_breaks    <- stored_breaks()

          # Single get_weather() call — handles all SSPs × all periods
          shiny::setProgress(value = 0.15, detail = "Loading weather data...")

          weather_result <- tryCatch(
            get_weather(
              survey_data          = svy,
              selected_surveys     = ss,
              selected_weather     = sw,
              dates                = sim_dates,
              connection_params    = cp,
              ssp                  = if (has_future) ssps else NULL,
              future_period        = fp_list,
              perturbation_method  = perturbation_method,
              stored_breaks        = cached_breaks
            ),
            error = function(e) {
              shiny::showNotification(
                paste0("Weather load failed: ", conditionMessage(e)),
                type = "error", duration = 8
              )
              NULL
            }
          )
          req(!is.null(weather_result))

          if (is.null(cached_breaks)) {
            cached_breaks <- attr(weather_result, "stored_breaks")
          }

          shiny::setProgress(value = 0.5, detail = "Running simulations...")

          # Run sim pipeline on every key returned by get_weather().
          # Keys are "historical" or "<ssp>_<start>_<end>_<model>".
          # Future results are grouped by SSP + period.  To limit peak memory,
          # predictions are trimmed (slim = TRUE) and incrementally bound into
          # a running data frame per group instead of accumulating a list.

          # group_key -> running combined data frames
          group_preds   <- list()
          group_weather <- list()
          group_meta    <- list()
          group_n       <- list()

          future_keys <- setdiff(names(weather_result), "historical")
          all_keys    <- c("historical", future_keys)
          n_keys      <- length(all_keys)

          for (ki in seq_along(all_keys)) {
            key <- all_keys[[ki]]
            is_hist <- identical(key, "historical")

            if (ki %% 5L == 0L || is_hist) {
              shiny::setProgress(
                value  = 0.5 + 0.45 * (ki / n_keys),
                detail = if (is_hist) "Historical simulation..."
                         else sprintf("Model %d / %d", ki - 1L, length(future_keys))
              )
            }

            # Process historical with full output; future with slim output
            out <- run_sim_pipeline(
              weather_raw = weather_result[[key]],
              svy         = svy,
              sw          = sw,
              so          = so,
              model       = model,
              residuals   = residuals,
              train_data  = train_data,
              engine      = engine,
              slim        = !is_hist
            )

            # Free the raw weather for this model immediately
            weather_result[[key]] <- NULL

            if (is.null(out)) next

            if (is_hist && is.null(hist_sim_result)) {
              hist_sim_result <- list(
                preds       = out$preds,
                so          = so,
                n_pre_join  = out$n_pre_join,
                weather_raw = out$weather_raw,
                train_data  = train_data
              )
            } else if (!is_hist) {
              # Key is "<ssp>_<start>_<end>_<model>"
              ssp_code   <- sub("^(ssp[0-9]_[0-9]_[0-9])_.*$", "\\1", key)
              rest       <- sub(paste0("^", ssp_code, "_"), "", key)
              period_str <- sub("^([0-9]{4}_[0-9]{4})_.*$", "\\1", rest)
              model_name <- sub(paste0("^", period_str, "_"), "", rest)
              yr_parts   <- as.integer(strsplit(period_str, "_")[[1]])
              gk         <- paste0(ssp_code, "_", period_str)

              # Tag each model's predictions so aggregation can group by
              # (model, sim_year) — producing N_models × N_years values
              # for the CI in the point-range chart.
              out$preds$model       <- model_name
              out$weather_raw$model <- model_name

              # Incrementally bind instead of accumulating a list of dfs.
              # Peak memory = existing combined df + one new model df.
              group_preds[[gk]]   <- dplyr::bind_rows(group_preds[[gk]],   out$preds)
              group_weather[[gk]] <- dplyr::bind_rows(group_weather[[gk]], out$weather_raw)
              group_n[[gk]]       <- (group_n[[gk]] %||% 0L) + 1L

              if (is.null(group_meta[[gk]])) {
                group_meta[[gk]] <- list(
                  ssp_code   = ssp_code,
                  year_range = yr_parts
                )
              }
            }
            rm(out)

            # Periodic garbage collection to release freed model weather
            if (ki %% 10L == 0L) gc(verbose = FALSE)
          }

          rm(weather_result); gc(verbose = FALSE)

          # Build display entries: one per SSP + period
          for (gk in names(group_preds)) {
            meta        <- group_meta[[gk]]
            ssp_pretty  <- ssp_labels[meta$ssp_code] %||% meta$ssp_code
            period_lbl  <- paste0(meta$year_range[1], "-", meta$year_range[2])
            display_key <- paste0(ssp_pretty, " / ", period_lbl)
            new_scenarios[[display_key]] <- list(
              preds       = group_preds[[gk]],
              weather_raw = group_weather[[gk]],
              so          = so,
              year_range  = meta$year_range,
              n_models    = group_n[[gk]]
            )
          }
          rm(group_preds, group_weather, group_meta, group_n)
          gc(verbose = FALSE)

          req(!is.null(hist_sim_result))
          hist_sim(hist_sim_result)
          saved_scenarios(new_scenarios)
          shiny::setProgress(value = 1, detail = "Complete")
        }
      )

      n_scen <- length(saved_scenarios())
      shiny::showNotification(
        paste0("\u2713 Simulation complete.",
               if (n_scen > 0) paste0(" ", n_scen, " future scenario(s).") else ""),
        type = "message", duration = 5
      )
    }, ignoreInit = TRUE)

    # ---- Return API --------------------------------------------------------

    list(
      hist_sim        = hist_sim,
      saved_scenarios = saved_scenarios,
      selected_hist   = selected_hist,
      selected_fut    = selected_fut
    )
  })
}
