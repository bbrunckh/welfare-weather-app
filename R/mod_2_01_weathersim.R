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
      shiny::tags$h6("Future climate periods",
                     style = "font-weight:600; margin-bottom:4px;"),

      # Period 1
      shiny::tags$div(
        style = "display:flex; gap:8px; align-items:center; margin-bottom:4px;",
        shiny::tags$span("Period 1:", style = "min-width:60px; font-weight:500;"),
        shiny::numericInput(ns("fut_start_1"), label = NULL, value = 2020,
                            min = 2015, max = 2100, step = 1, width = "90px"),
        shiny::tags$span("\u2013"),
        shiny::numericInput(ns("fut_end_1"), label = NULL, value = 2040,
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

      # -- Ensemble percentiles -----------------------------------------------
      shiny::tags$h6("Ensemble model spread",
                     style = "font-weight:600; margin-bottom:4px;"),
      shiny::radioButtons(
        inputId  = ns("ensemble_choice"),
        label    = NULL,
        choices  = c(
          "All members"                    = "all",
          "Percentiles (P10, P50, P90)"    = "percentiles"
        ),
        selected = "percentiles"
      ),

      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Residual method ----------------------------------------------------
      shiny::tags$h6("Residual method",
                     style = "font-weight:600; margin-bottom:4px;"),
      shiny::radioButtons(
        inputId  = ns("residuals"),
        label    = NULL,
        choices  = residual_choices(),
        selected = "normal"
      ),
      shiny::helpText(
        shiny::tags$b("none:"), " return fitted values only.", shiny::tags$br(),
        shiny::tags$b("original:"), " match each observation\u2019s own training residual.",
        shiny::tags$br(),
        shiny::tags$b("empirical:"), " resample residuals from the training distribution.",
        shiny::tags$br(),
        shiny::tags$b("normal:"), " draw residuals from N(0, \u03c3).",
        style = "font-size:11px;"
      )
    ),

    shiny::tags$hr(style = "margin: 10px 0;"),

    # ---- Run simulation button (always visible) ----------------------------
    shiny::actionButton(
      ns("run_sim"),
      label = "Run simulation",
      class = "btn-primary",
      icon  = shiny::icon("play"),
      style = "width: 100%; margin-top: 4px;"
    )
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
                                        model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Internal state ----------------------------------------------------
    hist_sim        <- reactiveVal(NULL)
    saved_scenarios <- reactiveVal(list())

    # ---- Settings summary banner -------------------------------------------

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

      ens_txt <- if (identical(input$ensemble_choice, "all"))
        "All models" else "P10, P50, P90"

      res_labels <- c(
        "none"      = "None",
        "original"  = "Original",
        "empirical" = "Empirical resample",
        "normal"    = "Normal distribution"
      )
      res_txt <- res_labels[input$residuals %||% "normal"] %||% input$residuals

      shiny::tags$div(
        style = paste0(
          "border-left: 3px solid #2166ac; background: #f4f8fd; ",
          "padding: 8px 12px; margin-bottom: 10px; border-radius: 3px; ",
          "font-size: 12px; color: #444;"
        ),
        shiny::tags$b("Historical period:", style = "color:#333;"),
        paste0(hist_yr[1], "\u2013", hist_yr[2]),
        shiny::tags$br(),
        shiny::tags$b("Future climate:", style = "color:#333;"), fut_txt,
        shiny::tags$b(" \u00b7 SSP:", style = "color:#333;"), ssp_txt,
        shiny::tags$b(" \u00b7 Ensemble:", style = "color:#333;"), ens_txt,
        shiny::tags$br(),
        shiny::tags$b("Simulation residuals:", style = "color:#333;"), res_txt
      )
    })

    # ---- 30-year minimum window warning ------------------------------------

    output$hist_years_warning <- shiny::renderUI({
      req(input$hist_years)
      if (length(input$hist_years[1]:input$hist_years[2]) < 30) {
        shiny::helpText(
          "\u26a0\ufe0f Window is less than 30 years. Results may be unreliable.",
          style = "color: #c0392b; font-size: 12px;"
        )
      }
    })

    # ---- Derived config reactives ------------------------------------------

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

    ensemble_percentiles <- reactive({
      if (identical(input$ensemble_choice, "all")) NULL else c(10, 50, 90)
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
      ens_pct   <- ensemble_percentiles()

      # ---- Group future scenarios by period --------------------------------
      # get_weather() always returns a "historical" key alongside any SSP
      # results, so we never need a separate historical-only call when future
      # periods are configured. One call per unique period, all SSPs batched.
      fut_periods <- future_periods()
      sf          <- selected_fut()
      has_future  <- !is.null(sf) && length(fut_periods) > 0
      ssps        <- if (has_future) unique(sf$ssp) else character(0)
      perturbation_method <- if (has_future) build_perturbation_method(sw) else NULL

      # Build ordered list of get_weather() calls.
      # Historical-only when no future periods; otherwise one per period.
      if (has_future) {
        weather_calls <- lapply(fut_periods, function(yr) {
          list(
            ssp           = ssps,
            future_period = c(paste0(yr[1], "-01-01"),
                              paste0(yr[2], "-12-31")),
            period_label  = paste0(yr[1], "-", yr[2]),
            year_range    = yr
          )
        })
      } else {
        weather_calls <- list(
          list(ssp = NULL, future_period = NULL,
               period_label = "historical", year_range = NULL)
        )
      }

      n_calls <- length(weather_calls)

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

          for (i in seq_along(weather_calls)) {
            wc <- weather_calls[[i]]
            shiny::setProgress(
              value  = i / n_calls,
              detail = if (is.null(wc$ssp))
                "Loading historical weather..."
              else
                paste("Loading weather:", wc$period_label)
            )

            weather_result <- tryCatch(
              get_weather(
                survey_data          = svy,
                selected_surveys     = ss,
                selected_weather     = sw,
                dates                = sim_dates,
                connection_params    = cp,
                ssp                  = wc$ssp,
                future_period        = wc$future_period,
                perturbation_method  = perturbation_method,
                ensemble_percentiles = ens_pct
              ),
              error = function(e) {
                shiny::showNotification(
                  paste0("Weather load failed (",
                         wc$period_label, "): ",
                         conditionMessage(e)),
                  type = "error", duration = 8
                )
                NULL
              }
            )
            if (is.null(weather_result)) next

            # Run sim pipeline on every key returned by get_weather()
            for (key in names(weather_result)) {
              out <- run_sim_pipeline(
                weather_raw = weather_result[[key]],
                svy         = svy,
                sw          = sw,
                so          = so,
                model       = model,
                residuals   = residuals,
                train_data  = train_data,
                engine      = engine
              )
              if (is.null(out)) next

              if (key == "historical" && is.null(hist_sim_result)) {
                # First historical result — store as baseline
                hist_sim_result <- list(
                  preds       = out$preds,
                  so          = so,
                  n_pre_join  = out$n_pre_join,
                  weather_raw = out$weather_raw,
                  train_data  = train_data
                )
              } else if (key != "historical") {
                # Future scenario
                ssp_code    <- sub("_p\\d+$|_[A-Za-z].*$", "", key)
                ssp_pretty  <- ssp_labels[ssp_code] %||% ssp_code

                # Extract percentile label (e.g. "P50") from key suffix
                pct_m <- regexpr("p(\\d+)$", key)
                pct_label <- if (pct_m > 0) {
                  paste0("P", regmatches(key, pct_m) |>
                           sub(pattern = "^p", replacement = ""))
                } else {
                  # Individual model member — use model name as label
                  sub(paste0("^", ssp_code, "_"), "", key)
                }

                display_key <- paste0(ssp_pretty, " / ",
                                      wc$period_label, " / ", pct_label)

                new_scenarios[[display_key]] <- list(
                  preds       = out$preds,
                  weather_raw = out$weather_raw,
                  so          = so,
                  year_range  = wc$year_range
                )
              }
            }
          }

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
