#' 2_01_weathersim UI Function
#'
#' @description A shiny Module. Unified sidebar for configuring and running
#'   both historical and future welfare simulations.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @param sim_n Integer. Number of coefficient draws (S) for VCV uncertainty
#'   propagation. Default 50. Range 10-1000. Higher values give more stable
#'   tail percentiles but increase computation time linearly.
#'
#' @param pov_line_sim Numeric. Poverty line in daily 2021 PPP USD used for
#'   headcount ratio (FGT0), poverty gap (FGT1), and FGT2 calculations.
#'   Fixed at simulation time — re-run simulation to change.
#'
#' @param coef_draws Matrix (S x K). Pre-computed coefficient draw matrix from
#'   \code{draw_coefs()}. NULL falls back to point-estimate predictions.
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
      ),


      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Simulation parameters -------------------------------------------
      shiny::tags$h6("Simulation parameters",
                     style = "font-weight:600; margin-bottom:4px;"),
      shiny::numericInput(
        inputId = ns("sim_n"),
        label   = "Coefficient draws (S)",
        value   = 50, min = 10, max = 1000, step = 10
      ),
      shiny::helpText(
        "200-500 recommended for final runs; 50 for speed. Upper bound 1,000.",
        style = "font-size:11px; color:#555; margin-top:2px; margin-bottom:8px;"
      ),
      shiny::numericInput(
        inputId = ns("pov_line_sim"),
        label   = "Poverty line (daily, 2021 PPP USD)",
        value   = 3.00, min = 0, step = 0.5
      ),
      shiny::helpText(
        "Used for headcount / FGT calculations. Poverty line is fixed at simulation time.",
        style = "font-size:11px; color:#555; margin-top:2px; margin-bottom:8px;"
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),
      shiny::checkboxInput(
        inputId = ns("dev_mode"),
        label   = shiny::tags$span(
          style = "font-size:11px; font-weight:600; color:#b45309;",
          "⚠ Dev mode: 1 ensemble model only"
        ),
        value   = FALSE
      ),
      shiny::helpText(
        "When checked, only the first CMIP6 ensemble member per SSP/period is used.",
        " Speeds up testing. Disable for final runs.",
        style = "font-size:11px; color:#b45309; margin-top:2px; margin-bottom:8px;"
      ),
      shiny::checkboxInput(
        inputId = ns("skip_coef_draws"),
        label   = shiny::tags$span(
          style = "font-size:11px; font-weight:600; color:#555;",
          "Skip coefficient draws (point estimates only)"
        ),
        value   = FALSE
      ),
      shiny::helpText(
        "When checked, S draws are skipped and point estimates are used.",
        " Faster for testing. Coefficient uncertainty bands will not appear.",
        style = "font-size:11px; color:#555; margin-top:2px; margin-bottom:8px;"
      ),
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

          # Pre-compute coefficient draws once — reused across ALL weather keys
          # (historical + every SSP/period/model combination). The draw matrix is
          # a property of the fitted model, not of any climate scenario.
          # Pre-compute S coefficient draws once before the weather-key loop.
          # Reused across all keys (historical + future).
          # Set skip_coef_draws = TRUE in UI to suppress and use point estimates only.
          coef_draws <- if (isTRUE(input$skip_coef_draws)) {
            message("[wiseapp] Coefficient draws skipped (point estimates only)")
            NULL
          } else {
            tryCatch(
              draw_coefs(
                fit       = model,
                S         = as.integer(input$sim_n %||% 50L),
                method    = "vcov",
                vcov_spec = COEF_VCOV_SPEC,
                seed      = 42L
              ),
              error = function(e) {
                warning("[mod_2_01] draw_coefs() failed, falling back to point estimates: ",
                        conditionMessage(e))
                NULL
              }
            )
          }
          shiny::setProgress(value = 0.5, detail = "Running simulations...")

          # Detect weight column from svy before the loop — used in aggregate_sim_preds().
          weight_col_sim <- grep("^weight$|^hhweight$|^wgt$|^pw$",
                                 names(svy), value = TRUE,
                                 ignore.case = TRUE)[1]
          if (is.na(weight_col_sim %||% NA)) weight_col_sim <- NULL
          wt_detected <- grep("^weight$|^hhweight$|^wgt$|^pw$",
                              names(svy), value = TRUE, ignore.case = TRUE)

          if (length(wt_detected) > 1L) {
            warning(sprintf(
              "[wiseapp] Multiple weight columns detected: %s. Using '%s'.",
              paste(wt_detected, collapse = ", "), weight_col_sim
            ))
          }

          # Poverty line for FGT methods — set before simulation runs.
          pov_line_sim_val <- as.numeric(input$pov_line_sim %||% 3.0)

          # All aggregation methods to pre-compute.
          # User can switch between them post-hoc in Results without re-running.
          # --------------------------------------------------------------------
          # Aggregation methods pre-computed at simulation time.
          # All 9 methods x weighted/unweighted x deviation combinations are
          # computed once so the user can switch between them post-hoc in the
          # Results tab without re-running the simulation.
          # NOTE: poverty line (pov_line_sim_val) is baked in here — changing
          # the poverty line requires a full re-run.
          # --------------------------------------------------------------------
          agg_methods_all <- c("mean", "median", "total",
                                  "headcount_ratio", "gap", "fgt2",
                                  "gini", "prosperity_gap", "avg_poverty")
          # Pre-aggregate immediately after each future key to avoid accumulating
          # N_households x S_draws x N_keys rows in memory.
          # group_agg stores ~(S_draws x 5_methods x N_years) rows per group.
          group_agg         <- list()
          group_weather_rep <- list()
          group_meta        <- list()
          group_n           <- list()

          future_keys <- setdiff(names(weather_result), "historical")
          if (isTRUE(input$dev_mode)) future_keys <- future_keys[!duplicated(stringr::str_extract(future_keys, "^(?:[^_]+_){4}[^_]+"))]
          all_keys    <- c("historical", future_keys)
          n_keys      <- length(all_keys)

          # Wall-clock timer — updated every key to show elapsed + estimated remaining.
          # Option E progress: benchmark 1 draw to estimate total runtime,
          # then message() to console in real time (fires even while Shiny is blocked).
          # setProgress() only updates between keys — message() is the only real-time feedback.
          S_val      <- if (!is.null(coef_draws)) nrow(coef_draws) else 1L
          n_hist_yrs <- if (!is.null(weather_result[["historical"]])) {
            length(unique(format(weather_result[["historical"]]$timestamp, "%Y")))
          } else 30L
          n_future_keys <- length(future_keys)

          total_runs <- S_val * n_hist_yrs * (1L + n_future_keys)
          hist_runs   <- S_val * n_hist_yrs
          future_runs <- S_val * n_hist_yrs * n_future_keys
          message(sprintf(
            "[wiseapp] Simulation starting: %d keys | S=%d draws",
            length(all_keys), S_val
          ))
          message(sprintf(
            "[wiseapp]   Historical : %d yrs x %d draws = %d runs",
            n_hist_yrs, S_val, hist_runs
          ))
          if (n_future_keys > 0L) message(sprintf(
            "[wiseapp]   Future     : %d keys x %d yrs x %d draws = %d runs",
            n_future_keys, n_hist_yrs, S_val, future_runs
          ))
          message(sprintf(
            "[wiseapp]   Total      : ~%d prediction runs across all keys",
            total_runs
          ))
          # t_start defined outside withProgress so it captures the full
          # simulation wall-clock time including progress setup overhead.
          t_start <- proc.time()[["elapsed"]]

          for (ki in seq_along(all_keys)) {
            key     <- all_keys[[ki]]
            is_hist <- identical(key, "historical")

            t_elapsed <- proc.time()[["elapsed"]] - t_start
            t_remain  <- if (ki > 1L) (t_elapsed / (ki - 1L)) * (n_keys - ki + 1L) else NA_real_

            # message() fires to R console in real time even while Shiny UI is blocked.
            # message() fires to R console in real time even while Shiny UI is blocked.
            # setProgress() only updates between blocking R calls (i.e. between keys).
            key_runs <- S_val * n_hist_yrs
            message(sprintf(
              "[wiseapp] Key %d/%d: %s | %d yrs x %d draws = %d runs | %s elapsed%s",
              ki, n_keys,
              if (is_hist) "Historical"
              else sub("^(ssp[^_]+_[0-9]+_[0-9]+)_.*$", "\\1", key),
              n_hist_yrs, S_val, key_runs,
              format_elapsed(t_elapsed),
              if (!is.na(t_remain))
                paste0(" | ~", format_elapsed(t_remain), " remaining")
              else " | estimating..."
            ))
            out <- run_sim_pipeline(
              weather_raw = weather_result[[key]],
              svy         = svy,
              sw          = sw,
              so          = so,
              model       = model,
              residuals   = residuals,
              train_data  = train_data,
              engine      = engine,
              coef_draws  = coef_draws,
              slim        = !is_hist
            )

            # Free raw weather for this key immediately
            weather_result[[key]] <- NULL

            # Update progress bar after key completes (Shiny unblocks here).
            t_elapsed_post <- proc.time()[["elapsed"]] - t_start
            t_remain_post  <- if (ki >= 1L) (t_elapsed_post / ki) * (n_keys - ki) else NA_real_
            shiny::setProgress(
              value  = 0.5 + 0.45 * (ki / n_keys),
              detail = sprintf(
                "%s | Key %d/%d | %s elapsed%s",
                if (is_hist) "Historical"
                else sub("^(ssp[^_]+_[0-9]+_[0-9]+)_.*$", "\\1", key),
                ki, n_keys,
                format_elapsed(t_elapsed_post),
                if (!is.na(t_remain_post) && t_remain_post > 0)
                  paste0(" | ~", format_elapsed(t_remain_post), " remaining")
                else if (ki == n_keys) " | finalising..."
                else ""
              )
            )

            if (is.null(out)) next

            if (is_hist && is.null(hist_sim_result)) {
              # Historical: keep full preds for weather density plot, but also
              # pre-aggregate all methods so agg_hist reactive can filter cheaply
              # without re-processing S_draws x N_households rows per UI interaction.
              # Pre-aggregate all methods x weighted/unweighted combinations.
              # Running both at simulation time avoids re-processing S_draws x N_households
              # rows reactively when user toggles weights in the Results tab.
              # ⚠️ METHODOLOGICAL FLAG — Option B deviation (pending review):
              # Deviation is computed relative to each scenario's OWN mean/median.
              # This shows internal spread but suppresses the absolute climate
              # signal (direction of welfare change). Option A would centre future
              # scenarios relative to the historical baseline mean — preserving
              # direction. Confirm with supervisor before production use.
              # See: https://github.com/bbrunckh/welfare-weather-app/issues/22
              hist_agg <- dplyr::bind_rows(lapply(agg_methods_all, function(meth) {
                dplyr::bind_rows(lapply(c(TRUE, FALSE), function(use_w) {
                  wt  <- if (use_w) weight_col_sim else NULL
                  if (use_w && is.null(wt)) return(NULL)
                  tryCatch({
                    pov <- if (meth %in% c("headcount_ratio", "gap", "fgt2")) pov_line_sim_val else NULL
                    res <- aggregate_sim_preds(out$preds, so, meth, "none", FALSE, pov, wt)
                    if (!is.null(res$out)) dplyr::mutate(res$out, agg_method = meth, weighted = use_w)
                    else NULL
                  }, error = function(e) NULL)
                }))
              }))
              # hist_sim_result schema:
              #   $preds       — full household x sim_year predictions (S draws x N rows)
              #   $agg         — pre-aggregated (method x weighted x deviation x sim_year)
              #   $so          — selected outcome metadata (single-row data frame)
              #   $pov_line    — poverty line used at simulation time (numeric)
              #   $n_pre_join  — row count before weather join (diagnostics)
              #   $weather_raw — representative historical weather series
              #   $train_data  — training data (for residual resampling)
              #   $has_weights — logical: weight column detected in survey data
              hist_sim_result <- list(
                preds       = out$preds,
                agg         = hist_agg,
                so          = so,
                n_pre_join  = out$n_pre_join,
                pov_line    = pov_line_sim_val,
                weather_raw = out$weather_raw,
                train_data  = train_data,
                has_weights = !is.null(weight_col_sim)
              )
            } else if (!is_hist) {
              # Key is "<ssp>_<start>_<end>_<model>"
              ssp_code   <- sub("^(ssp[0-9]_[0-9]_[0-9])_.*$", "\\1", key)
              rest       <- sub(paste0("^", ssp_code, "_"), "", key)
              period_str <- sub("^([0-9]{4}_[0-9]{4})_.*$", "\\1", rest)
              model_name <- sub(paste0("^", period_str, "_"), "", rest)
              yr_parts   <- as.integer(strsplit(period_str, "_")[[1]])
              gk         <- paste0(ssp_code, "_", period_str)

              # Tag model before aggregating so agg can group by model
              out$preds$model <- model_name

              # Aggregate immediately across all methods — free raw preds after.
              # IMPORTANT order of operations:
              # Stage 1: aggregate within (draw_id, model, sim_year) -> scalar per group
              # Stage 2: percentiles across draw_id -> p05/p50/p95 bands
              # Taking percentiles before aggregating would give quantiles of the
              # individual welfare distribution, not the uncertainty band on the
              # aggregate statistic — completely different quantities.
              # Pre-aggregate all methods x weighted/unweighted combinations.
              key_agg <- dplyr::bind_rows(lapply(agg_methods_all, function(meth) {
                dplyr::bind_rows(lapply(c(TRUE, FALSE), function(use_w) {
                  wt  <- if (use_w) weight_col_sim else NULL
                  if (use_w && is.null(wt)) return(NULL)
                  tryCatch({
                    pov <- if (meth %in% c("headcount_ratio", "gap", "fgt2")) pov_line_sim_val else NULL
                    res <- aggregate_sim_preds(out$preds, so, meth, "none", FALSE, pov, wt)
                    if (!is.null(res$out)) dplyr::mutate(res$out, agg_method = meth, weighted = use_w)
                    else NULL
                  }, error = function(e) {
                    message("[loop] aggregate_sim_preds failed for method ", meth,
                            " weighted=", use_w, ": ", conditionMessage(e))
                    NULL
                  })
                }))
              }))

              # Retain representative weather (first model only) for diagnostics
              if (is.null(group_weather_rep[[gk]])) {
                group_weather_rep[[gk]] <- out$weather_raw
              }

              # Free household-level predictions immediately
              rm(out); gc(verbose = FALSE)

              group_agg[[gk]] <- dplyr::bind_rows(group_agg[[gk]], key_agg)
              group_n[[gk]]   <- (group_n[[gk]] %||% 0L) + 1L

              if (is.null(group_meta[[gk]])) {
                group_meta[[gk]] <- list(
                  ssp_code   = ssp_code,
                  year_range = yr_parts
                )
              }
              next  # skip rm(out) below — already freed
            }
            rm(out)

            # Periodic garbage collection
            if (ki %% 10L == 0L) gc(verbose = FALSE)
          }

          rm(weather_result); gc(verbose = FALSE)

          # Build display entries: one per SSP + period.
          # saved_scenarios stores pre-aggregated summaries (agg) + representative
          # weather — NOT raw preds. This keeps memory flat regardless of S or
          # number of CMIP6 ensemble members.
          for (gk in names(group_agg)) {
            meta        <- group_meta[[gk]]
            ssp_pretty  <- ssp_labels[meta$ssp_code] %||% meta$ssp_code
            period_lbl  <- paste0(meta$year_range[1], "-", meta$year_range[2])
            display_key <- paste0(ssp_pretty, " / ", period_lbl)
            new_scenarios[[display_key]] <- list(
              agg         = group_agg[[gk]],
              weather_raw = group_weather_rep[[gk]],
              so          = so,
              year_range  = meta$year_range,
              n_models    = group_n[[gk]]
            )
          }
          rm(group_agg, group_weather_rep, group_meta, group_n)
          gc(verbose = FALSE)

          # Push results into reactiveVals — triggers all downstream observers.
          # hist_sim() must be set before saved_scenarios() so the
          # observeEvent(hist_sim(), once = TRUE) in mod_2_02_results fires
          # and inserts the Results tab before agg_scenarios tries to render.
          hist_sim(hist_sim_result)
          saved_scenarios(new_scenarios)

          shiny::setProgress(value = 1, detail = "Complete")
        }
      )

      t_total <- proc.time()[["elapsed"]] - t_start
      n_scen  <- length(saved_scenarios())
      message(sprintf(
        "[wiseapp] Simulation complete in %s | %d key(s) | S=%d draws | ~%d total runs",
        format_elapsed(t_total), n_keys, S_val, total_runs
      ))
      shiny::showNotification(
        ui = tagList(
          tags$b("\u2713 Simulation complete"),
          tags$br(),
          sprintf("%s | %d key(s) | S=%d draws | ~%d runs",
                  format_elapsed(t_total), n_keys, S_val, total_runs),
          if (n_scen > 0) tagList(tags$br(), paste0(n_scen, " future scenario(s)")) else NULL
        ),
        type = "message", duration = 8
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
