#' 2_01_weathersim UI Function
#'
#' @description A shiny Module. Unified sidebar for configuring and running
#'   both historical and future welfare simulations.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @section Simulation inputs (configured in server via input$):
#'   \describe{
#'     \item{\code{sim_n}}{Integer. Number of VCV coefficient draws (S).}
#'     \item{\code{pov_line_sim}}{Numeric. Poverty line in daily 2021 PPP USD.
#'       Fixed at simulation time.}
#'     \item{\code{skip_coef_draws}}{Logical. If TRUE, bypasses VCV draws
#'       and uses point estimates only. Default TRUE.}
#'     \item{\code{dev_mode}}{Logical. If TRUE, limits to 1 ensemble model
#'       per SSP/period for fast testing.}
#'   }
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
        value   = 30, min = 10, max = 1000, step = 10
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
        value   = TRUE
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
        value   = TRUE
      ),
      shiny::helpText(
        "When checked, S draws are skipped and point estimates are used.",
        " Faster for testing. Coefficient uncertainty bands will not appear.",
        style = "font-size:11px; color:#555; margin-top:2px; margin-bottom:8px;"
      ),
      shiny::checkboxInput(
        ns("exclude_gini"),
        label = "Exclude Gini coefficient (faster computation)",
        value = TRUE  # default ON — gini excluded
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
#' @param stored_breaks Reactive returning a named list of pre-computed
#'   histogram break points for the weather density plot. Defaults to
#'   \code{reactive(NULL)} — breaks computed on demand when not supplied.
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
    hist_agg_rv    <- reactiveVal(NULL)
    scenario_agg_rv <- reactiveVal(NULL)

    # ---- Baseline survey reactives ----------------------------------------

    # Derive available survey x year choices from survey_weather.
    # Returns a named character vector: label -> "survname|year" value.

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

      # ---- Gather inputs ---------------------------------------------------
      sw  <- selected_weather()
      so  <- selected_outcome()
      sh  <- selected_hist()
      svy <- baseline_svy()
      ss  <- selected_surveys()
      req(ss)
      mf  <- model_fit()
      cp  <- connection_params()

      sim_dates           <- build_hist_sim_dates(svy, unlist(sh$year_range))
      fut_periods         <- future_periods()
      sf                  <- selected_fut()
      has_future          <- !is.null(sf) && length(fut_periods) > 0
      ssps                <- if (has_future) unique(sf$ssp) else character(0)
      perturbation_method <- if (has_future) build_perturbation_method(sw) else NULL
      fp_list             <- if (has_future) lapply(fut_periods, function(yr)
                               c(paste0(yr[1], "-01-01"), paste0(yr[2], "-12-31")))
                             else list()
      # Fixed at p10/p90 — input$ensemble_band_width not yet in UI
      ensemble_band_q     <- c(lo = 0.10, hi = 0.90)


      shiny::withProgress(message = "Running simulation...", value = 0, {

        # ---- Run simulation ------------------------------------------------
        result <- tryCatch(
          fct_run_simulation(
            sw                  = sw,
            so                  = so,
            svy                 = svy,
            ss                  = ss,
            mf                  = mf,
            cp                  = cp,
            fp_list             = fp_list,
            ssps                = ssps,
            residuals           = sh$residuals,
            dev_mode            = isTRUE(input$dev_mode),
            skip_coef_draws     = isTRUE(input$skip_coef_draws),
            sim_dates           = sim_dates,
            perturbation_method = perturbation_method,
            stored_breaks       = stored_breaks(),
            ensemble_band_q     = ensemble_band_q,
             full_ensemble       = FALSE,   # input$full_ensemble not yet in UI
            progress_fn         = function(value, detail)
                                    shiny::setProgress(value = value,
                                                       detail = detail)
          ),
          error = function(e) {
            shiny::showNotification(
              paste0("Simulation failed: ", conditionMessage(e)),
              type = "error", duration = 8
            )
            NULL
          }
        )
        req(!is.null(result))

        # ---- Store results (reactive side effects) -------------------------
        hist_sim(result$hist_sim_result)
        saved_scenarios(result$new_scenarios)

        # ---- Pre-aggregate at simulation time ------------------------------
        agg_methods_all <- {
          m <- unname(hist_aggregate_choices(so$type, so$name))
          if (isTRUE(input$exclude_gini)) setdiff(m, "gini") else m
        }
        S_sim   <- as.integer(input$sim_n %||% 150L)
        bq_sim  <- c(lo = 0.10, hi = 0.90)
        res_sim <- sh$residuals %||% "none"
        pov_sim <- as.numeric(input$pov_line_sim)

        shiny::showNotification(
          ui       = paste0("Aggregating results across ",
                            length(result$new_scenarios) + 1L,
                            " key(s) — please wait..."),
          id       = "agg_notify", duration = NULL, type = "message"
        )

        # Generate shared Z matrix once — paired draws for historical + future
        # Same z vectors applied to both → ribbon gap stable, common random numbers variance reduction
        K_global     <- if (!is.null(result$hist_sim_result$pipeline$F_loading))
                          ncol(result$hist_sim_result$pipeline$F_loading) else 0L
        Z_global     <- if (K_global > 0L && S_sim > 0L)
                          matrix(stats::rnorm(S_sim * K_global), nrow = S_sim, ncol = K_global)
                        else NULL

        shiny::isolate({
          hist_agg_rv(
            compute_hist_agg(
              pipeline  = result$hist_sim_result$pipeline,
              chol_obj  = result$chol_obj,
              methods   = agg_methods_all,
              S         = S_sim,
              band_q    = bq_sim,
              residuals = res_sim,
              pov_line  = pov_sim,
              is_log    = isTRUE(so$transform == "log"),
              Z_global = Z_global
            )
          )
          scenario_agg_rv(
            compute_scenario_agg(
              scenarios = result$new_scenarios,
              methods   = agg_methods_all,
              S         = S_sim,
              band_q    = bq_sim,
              residuals = res_sim,
              pov_line  = pov_sim,
              Z_global = Z_global
            )
          )
        })

        shiny::removeNotification("agg_notify")
        shiny::setProgress(value = 1, detail = "Complete")
      })

      # ---- Dev fixture saving (dev mode only) ------------------------------
      if (!isTRUE(getOption("golem.app.prod"))) {
        tryCatch({
          dir.create("dev/fixtures", showWarnings = FALSE, recursive = TRUE)
          saveRDS(list(
            hist_sim     = result$hist_sim_result[[1L]],
            scenario_sim = result$new_scenarios[[1L]],
            chol_obj     = result$chol_obj,
            pov_line     = as.numeric(input$pov_line_sim)
          ), "dev/fixtures/sim_inputs.rds")
          message("[dev] Fixtures saved to dev/fixtures/sim_inputs.rds")
        }, error = function(e) {
          message("[dev] Fixture save failed: ", conditionMessage(e))
        })
      }

      # ---- Completion notification -----------------------------------------
      message(sprintf(
        "[wiseapp] Simulation complete in %s | %d key(s) | S=%d | ~%d total runs",
        format_elapsed(result$t_elapsed),
        result$n_keys, S_sim, result$total_runs
      ))
      shiny::showNotification(
        ui = tagList(
          tags$b("\u2713 Simulation complete"), tags$br(),
          sprintf("%s | %d key(s) | ~%d runs",
                  format_elapsed(result$t_elapsed),
                  result$n_keys, result$total_runs),
          if (length(result$new_scenarios) > 0)
            tagList(tags$br(),
                    paste0(length(result$new_scenarios),
                           " future scenario(s)"))
          else NULL
        ),
        type = "message", duration = 8
      )
    }, ignoreInit = TRUE)

    # ---- Return API --------------------------------------------------------

    list(
      hist_sim        = hist_sim,
      saved_scenarios = saved_scenarios,
      selected_hist   = selected_hist,
      selected_fut    = selected_fut,
      pov_line_sim    = reactive(input$pov_line_sim),
      hist_agg_rv    = hist_agg_rv,
      scenario_agg_rv = scenario_agg_rv
    )
  })
}
