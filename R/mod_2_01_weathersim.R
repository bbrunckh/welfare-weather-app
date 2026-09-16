#' 2_01_weathersim UI Function
#'
#' @description A shiny Module. Unified sidebar for configuring and running
#'   both historical and future welfare simulations.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @section Simulation inputs (configured in server via input$):
#'   \describe{
#'     \item{\code{pov_line_sim}}{Numeric. Poverty line in daily 2021 PPP USD.
#'       Fixed at simulation time.}
#'     \item{\code{skip_coef_draws}}{Logical. If TRUE, bypasses VCV draws
#'       and uses point estimates only. Default TRUE.}
#'   }
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_01_weathersim_ui <- function(id) {
  ns <- NS(id)
  step2_with_grid_num <- function(slider_tag, n) {
    for (i in seq_along(slider_tag$children)) {
      ch <- slider_tag$children[[i]]
      if (is.list(ch) && !is.null(ch$attribs) &&
        "data-grid-num" %in% names(ch$attribs)) {
        ch$attribs[["data-grid-num"]] <- n
        slider_tag$children[[i]] <- ch
        break
      }
    }
    slider_tag
  }

  tags$div(
    class = "step2-sidebar",
    # ---- Settings summary banner (always visible) --------------------------
    shiny::uiOutput(ns("settings_summary")),

    # ---- Simulation settings flyout (same pattern as Step 1 'Configure') ---
    # UI-02: shared flyout block - anchored to its toggle, one-open state,
    # aria-expanded, focus management, Escape to close (see custom.js).
    config_flyout_block(
      ns("settings_toggle"),
      "Simulation settings",
      toggle_label = "Simulation settings",

      # -- Baseline survey ----------------------------------------------------
      shiny::tags$div(
        class = "step2-section-label",
        "Baseline survey"
      ),
      shiny::uiOutput(ns("baseline_survey_ui")),
      shiny::uiOutput(ns("baseline_warning_ui")),
      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Climate scenarios --------------------------------------------------
      shiny::tags$div(
        class = "step2-section-label",
        "Climate scenarios"
      ),
      shiny::checkboxGroupInput(
        inputId = ns("climate"),
        label = shiny::tags$span(class = "visually-hidden", "Climate scenarios"),
        choices = c(
          "SSP2" = "ssp2_4_5",
          "SSP3" = "ssp3_7_0",
          "SSP5" = "ssp5_8_5"
        ),
        selected = "ssp3_7_0",
        inline = TRUE
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Projection period -------------------------------------------------
      shiny::tags$div(
        class = "step2-section-label",
        "Projection period"
      ),
      shiny::tags$div(
        class = "step2-projection-period",
        step2_with_grid_num(
          shiny::sliderInput(
            ns("fut_period_1"),
            label = shiny::tags$span(class = "visually-hidden", "Projection period 1"),
            min = 2010, max = 2100, value = c(2025, 2035), step = 1, sep = ""
          ),
          9
        )
      ),
      shiny::uiOutput(ns("fut_years_warning")),
      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Historical period --------------------------------------------------
      shiny::tags$h6(
        "Historical weather distribution period",
        info_popover(
          title = "Historical weather distribution period",
          shiny::p(
            "Historical weather informs the underlying variability, which is then",
            "perturbed with climate scenario projections."
          )
        ),
        style = "font-weight:600; margin-top:8px; margin-bottom:4px;"
      ),
      shiny::sliderInput(
        inputId = ns("hist_years"),
        label = shiny::tags$span(
          class = "visually-hidden",
          "Historical weather distribution period"
        ),
        min = 1950,
        max = 2024,
        value = c(1991, 2020),
        sep = ""
      ),
      shiny::uiOutput(ns("hist_years_warning")),
      shiny::helpText(
        tags$b("30 years is the recommended default."),
        style = "font-size: 11px; color: #555; margin-top: 2px; margin-bottom: 8px;"
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Additional future periods ------------------------------------------
      shiny::tags$h6("Additional projection periods",
        style = "font-weight:600; margin-bottom:4px;"
      ),

      # Period 2 (optional)
      shiny::tags$div(
        class = "step2-projection-period",
        shiny::tags$span("Period 2", class = "step2-projection-label"),
        step2_with_grid_num(
          shiny::sliderInput(
            ns("fut_period_2"),
            label = shiny::tags$span(class = "visually-hidden", "Projection period 2"),
            min = 2010, max = 2100, value = c(2015, 2015), step = 1, sep = ""
          ),
          9
        )
      ),
      # Period 3 (optional)
      shiny::tags$div(
        class = "step2-projection-period",
        shiny::tags$span("Period 3", class = "step2-projection-label"),
        step2_with_grid_num(
          shiny::sliderInput(
            ns("fut_period_3"),
            label = shiny::tags$span(class = "visually-hidden", "Projection period 3"),
            min = 2010, max = 2100, value = c(2015, 2015), step = 1, sep = ""
          ),
          9
        )
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Residual method ----------------------------------------------------
      shiny::tags$h6(
        "Simulation residuals",
        info_popover(
          title = "Simulation residuals",
          shiny::p(shiny::tags$b("original:"), " Recommended default: match each observation's own residual - assumes no changes due to changing hazards."),
          shiny::p(shiny::tags$b("resample:"), " Secondary recommendation: randomly resample residuals from the model."),
          docs = TRUE
        ),
        style = "font-weight:600; margin-bottom:4px;"
      ),
      pill_toggle(
        inputId = ns("residuals"),
        label = shiny::tags$span(
          class = "visually-hidden",
          "Simulation residuals"
        ),
        choices = residual_choices(),
        selected = "original"
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),

      # -- Coefficient uncertainty -------------------------------------------
      shiny::tags$h6(
        "Model coefficient uncertainty",
        info_popover(
          title = "Model coefficient uncertainty",
          shiny::p(
            "Checking this box incorporates uncertainty around the model",
            "definition process itself, via the analytic delta method.",
            "Disable to use point estimates only."
          ),
          shiny::p(
            "By default, only coefficients on variables that change between",
            "baseline and counterfactual contribute to the reported SE:",
            "weather variables and their interactions in Step 2; weather plus",
            "the policy-modified variables and their interactions in Step 3.",
            "Under 'original' residuals, uncertainty on the unchanged covariates",
            "cancels through the held-fixed residual term (additive-decomposition",
            "SE). 'Include uncertainty on all covariates' propagates uncertainty",
            "from all covariates instead - more conservative, but inconsistent",
            "with the model's own additive-separability assumption. Has no",
            "effect when residuals are not 'original'."
          ),
          shiny::p(
            "Coefficient uncertainty is propagated analytically via the",
            "delta method for all aggregates (mean, total, headcount,",
            "poverty gap, FGT2, Gini, 'avg_poverty'). There is no Monte Carlo",
            "draw fallback in the current implementation."
          ),
          docs = TRUE
        ),
        style = "font-weight:600; margin-bottom:4px;"
      ),
      shiny::checkboxInput(
        inputId = ns("include_coef_uncertainty"),
        label   = "Include coefficient uncertainty",
        value   = TRUE
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("include_coef_uncertainty")),
        shiny::checkboxInput(
          inputId = ns("propagate_all_covariate_uncertainty"),
          label   = "Include uncertainty on all covariates",
          value   = FALSE
        )
      )
    ),
    shiny::tags$hr(style = "margin: 10px 0;"),

    # ---- Run simulation button (hidden for RIF engine) ---------------------
    shiny::uiOutput(ns("run_sim_ui"))
  )
}

.step2_filter_baseline_surveys <- function(selected_surveys,
                                           baseline_selection) {
  if (is.null(selected_surveys) || length(baseline_selection) == 0L) {
    return(selected_surveys)
  }
  required <- c("code", "year")
  if (!all(required %in% names(selected_surveys))) {
    return(selected_surveys)
  }
  wave_key <- paste0(
    selected_surveys$code, "|",
    as.character(selected_surveys$year)
  )
  selected_surveys[wave_key %in% baseline_selection, , drop = FALSE]
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
#'   \code{reactive(NULL)} - breaks computed on demand when not supplied.
#' @param run_trigger Optional reactive trigger for a programmatic run.
#'
#' @noRd
mod_2_01_weathersim_server <- function(id,
                                       connection_params,
                                       selected_outcome,
                                       selected_weather,
                                       selected_surveys,
                                       survey_weather,
                                       model_fit,
                                       stored_breaks = reactive(NULL),
                                       survey_version = reactive(0L),
                                       run_trigger = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Internal state ----------------------------------------------------
    hist_sim <- reactiveVal(NULL)
    saved_scenarios <- reactiveVal(list())
    # INT-08: TRUE while the stored simulation's run signature no longer
    # matches the current fit/climate inputs.
    sim_stale <- reactiveVal(FALSE)
    run_generation <- reactiveVal(0L)
    run_status <- reactiveVal("idle")
    weather_store_lease <- reactiveVal(NULL)

    cleanup_weather_stores <- function() {
      # Session-end callbacks are not reactive consumers. Isolate the final
      # state read so cleanup does not try to register a dependency after the
      # session's reactive graph has been torn down.
      step2_weather_store_release(shiny::isolate(weather_store_lease()))
      weather_store_lease(NULL)
    }
    session$onSessionEnded(cleanup_weather_stores)

    # ---- Baseline survey reactives ----------------------------------------

    # Derive available survey x year choices from survey_weather.
    # Returns a named character vector: label -> "survname|year" value.

    baseline_survey_choices <- reactive({
      req(survey_weather())
      svy <- survey_weather()
      if (!all(c("code", "survname", "year") %in% names(svy))) {
        return(character(0))
      }
      combos <- unique(svy[, c("code", "survname", "year")])
      combos <- combos[order(combos$code, combos$year), ]

      # Join economy (country) name from selected_surveys via code column.
      # NOTE: code (e.g. TGO, GNB) is unique per country; survname (e.g. EHCVM)
      # is shared across countries in the same survey programme and must NOT
      # be used as the join key -- this was the bug causing Togo to disappear.

      ss <- tryCatch(selected_surveys(), error = function(e) NULL)
      if (!is.null(ss) && all(c("code", "economy") %in% names(ss))) {
        lbl_map <- unique(ss[, c("code", "economy")])
        combos <- merge(combos, lbl_map, by = "code", all.x = TRUE)
        combos$economy[is.na(combos$economy)] <- combos$code[is.na(combos$economy)]
      } else {
        combos$economy <- combos$code
      }
      vals <- paste0(combos$code, "|", combos$year)
      lbls <- paste0(combos$economy, " ", combos$year)
      setNames(vals, lbls)
    })

    # Default selection: latest year per unique economy (code), not survname.
    baseline_default <- reactive({
      ch <- baseline_survey_choices()
      if (length(ch) == 0) {
        return(character(0))
      }
      df <- data.frame(
        val = ch,
        code = sub("^(.*?)\\|.*$", "\\1", ch),
        year = as.integer(sub("^.*\\|", "", ch)),
        stringsAsFactors = FALSE
      )

      latest <- tapply(df$year, df$code, max)
      keep <- mapply(function(c, y) latest[c] == y, df$code, df$year)
      unname(ch[keep])
    })


    # ---- Settings summary banner -------------------------------------------

    output$baseline_survey_ui <- shiny::renderUI({
      ch <- baseline_survey_choices()
      def <- baseline_default()
      if (length(ch) == 0) {
        return(shiny::helpText("No survey data loaded.", style = "font-size:11px;"))
      }
      # INT-01: keep the user's baseline selection across rebuilds (e.g. the
      # survey list changing after a Step 1 reload); only invalid values are
      # dropped, and the default applies only when nothing survives.
      prev_bs <- shiny::isolate(input$baseline_survey)
      shiny::selectInput(
        ns("baseline_survey"),
        label = shiny::tags$span(
          class = "visually-hidden",
          "Baseline survey"
        ),
        choices = ch,
        selected = .restore_selection(prev_bs, ch, fallback = def),
        multiple = TRUE,
        selectize = TRUE
      )
    })
    shiny::outputOptions(output, "baseline_survey_ui",
      suspendWhenHidden = FALSE
    )

    output$baseline_warning_ui <- shiny::renderUI({
      sel <- input$baseline_survey %||% baseline_default()
      if (length(sel) <= 1) {
        return(NULL)
      }
      # Multiple economies or years selected -- show warning
      n_economies <- length(unique(sub("\\|.*$", "", sel)))
      n_years <- length(unique(sub("^.*\\|", "", sel)))
      if (n_economies > 1 || n_years > 1) {
        shiny::helpText(
          shiny::tags$b("\u26a0 Warning:"),
          "Using multiple survey years or economies is not recommended. Requires normalizing weights based on sample design, which is not currently implemented - verify before interpreting results.",
          style = "color: #c0392b; font-size: 11px; margin-top: 2px;"
        )
      }
    })

    output$settings_summary <- shiny::renderUI({
      hist_yr <- input$hist_years %||% c(1991, 2020)
      ssp_map <- c(
        "ssp2_4_5" = "SSP2-4.5",
        "ssp3_7_0" = "SSP3-7.0",
        "ssp5_8_5" = "SSP5-8.5"
      )
      ssp_sel <- input$climate %||% character(0)
      sel <- input$baseline_survey %||% baseline_default()
      ch <- baseline_survey_choices()
      survey_txt <- {
        nms <- names(ch)[ch %in% sel]
        if (length(nms) == 0) "None" else paste(nms, collapse = ", ")
      }

      period_values <- lapply(seq_len(3), function(i) {
        period <- input[[paste0("fut_period_", i)]]
        if (length(period) >= 2 && all(is.finite(period)) && period[2] > period[1]) {
          as.integer(period[1:2])
        } else {
          NULL
        }
      })
      period_values <- Filter(Negate(is.null), period_values)

      item <- function(label, value, pill = TRUE) {
        shiny::tags$span(
          class = "step2-summary-item",
          shiny::tags$span(class = "step2-summary-label", label),
          if (isTRUE(pill)) {
            shiny::tags$span(class = "selection-card-pill", value)
          } else {
            shiny::tags$span(class = "step2-summary-value", value)
          }
        )
      }
      separator <- shiny::tags$span(class = "step2-summary-separator", "·")
      scenario_items <- unlist(lapply(ssp_sel, function(ssp) {
        lapply(period_values, function(period) {
          shiny::tags$span(
            class = "step2-summary-item",
            shiny::tags$span(
              class = "step2-summary-label step2-summary-scenario-label",
              unname(ssp_map[[ssp]])
            ),
            shiny::tags$span(
              class = "step2-summary-value",
              paste0(period[1], "\u2013", period[2])
            )
          )
        })
      }), recursive = FALSE)
      climate_items <- if (length(scenario_items)) {
        do.call(c, lapply(seq_along(scenario_items), function(i) {
          c(
            if (i > 1L) list(separator) else list(),
            list(scenario_items[[i]])
          )
        }))
      } else {
        list(item("Climate scenarios", "None", pill = FALSE))
      }

      summary_row <- c(
        climate_items,
        list(separator),
        list(item("Reference climate", paste0(hist_yr[1], "\u2013", hist_yr[2]))),
        list(separator),
        list(item("Baseline survey", survey_txt))
      )

      selection_summary_card(
        title = NULL,
        badge = NULL,
        rows = list(shiny::tags$div(
          class = "selection-card-row step2-summary-row",
          summary_row
        )),
        compact = TRUE
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

    # Display-only check for fully supplied future periods with end <= start.
    # Mirrors future_periods() below, which silently excludes such periods;
    # Run is not disabled.
    output$fut_years_warning <- shiny::renderUI({
      issues <- character(0)
      for (i in 1:3) {
        period <- input[[paste0("fut_period_", i)]]
        if (length(period) >= 2 && all(is.finite(period)) && period[2] < period[1]) {
          issues <- c(issues, paste0(
            "Period ", i, " (", period[1], "-", period[2],
            "): end year is not after start year."
          ))
        }
      }
      if (length(issues) == 0) {
        return(NULL)
      }
      shiny::helpText(
        shiny::tags$b("Warning:"),
        " invalid projection period(s) will be excluded from the simulation: ",
        paste(issues, collapse = " "),
        style = "color: #c0392b; font-size: 11px; margin-top: 2px;"
      )
    })

    # ---- Derived config reactives ------------------------------------------

    # survey_weather filtered to the selected baseline rows.
    # Used in place of survey_weather() inside observeEvent(run_sim).
    baseline_svy <- reactive({
      sel <- input$baseline_survey %||% baseline_default()
      if (length(sel) == 0) {
        return(survey_weather())
      }
      svy <- survey_weather()
      vals <- paste0(svy$code, "|", as.character(svy$year))
      svy[vals %in% sel, , drop = FALSE]
    })

    # Keep weather retrieval on the same code/year waves as the baseline
    # population. Retain every matching metadata row so same-year survey rounds
    # and duplicate file references keep their existing filename semantics.
    baseline_surveys <- reactive({
      ss <- selected_surveys()
      req(ss)
      sel <- input$baseline_survey %||% baseline_default()
      .step2_filter_baseline_surveys(ss, sel)
    })

    selected_hist <- reactive({
      req(input$hist_years)
      data.frame(
        type = "historical",
        year_range = I(list(input$hist_years)),
        residuals = input$residuals %||% "original",
        scenario_name = paste0(
          "Historical / ",
          input$hist_years[1], "-", input$hist_years[2]
        ),
        stringsAsFactors = FALSE
      )
    })

    future_periods <- reactive({
      periods <- list()
      for (i in 1:3) {
        period <- input[[paste0("fut_period_", i)]]
        if (length(period) >= 2 && all(is.finite(period)) && period[2] > period[1]) {
          periods[[length(periods) + 1L]] <- period[1:2]
        }
      }
      periods
    })

    selected_fut <- reactive({
      req(input$climate)
      fp <- future_periods()
      if (length(fp) == 0) {
        return(NULL)
      }

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
            type = "future",
            year_range = I(list(yr)),
            ssp = ssp,
            method = "delta",
            residuals = input$residuals %||% "original",
            scenario_name = scene_name,
            stringsAsFactors = FALSE
          )
        })
      })

      do.call(rbind, do.call(c, rows))
    })

    # ---- Run simulation button (hidden for non-linear or RIF engine) --------------------

    output$run_sim_ui <- shiny::renderUI({
      mf <- model_fit()
      engine <- if (!is.null(mf)) mf$engine %||% "fixest" else "fixest"

      # Block only unsupported engines - linear (fixest) and RIF both supported
      unsupported <- !is.null(mf) &&
        !engine %in% c("fixest", "rif")

      # UI-29: name the missing prerequisites before the click instead of
      # letting the button silently no-op (the click observer req()s on all
      # of these).
      missing <- character(0)
      swd <- tryCatch(selected_weather(), error = function(e) NULL)
      so <- tryCatch(selected_outcome(), error = function(e) NULL)
      svy <- tryCatch(survey_weather(), error = function(e) NULL)
      ss <- tryCatch(selected_surveys(), error = function(e) NULL)
      hist_ok <- tryCatch(
        {
          selected_hist()
          TRUE
        },
        error = function(e) FALSE
      )
      if (is.null(so) || nrow(as.data.frame(so)) == 0) {
        missing <- c(missing, "an outcome")
      }
      if (is.null(swd) || nrow(as.data.frame(swd)) == 0) {
        missing <- c(missing, "weather variables")
      }
      if (is.null(svy) || nrow(as.data.frame(svy)) == 0) {
        missing <- c(missing, "survey and weather data")
      }
      if (is.null(ss) || nrow(as.data.frame(ss)) == 0) {
        missing <- c(missing, "a baseline survey")
      }
      if (!hist_ok) {
        missing <- c(missing, "a historical period")
      }
      if (is.null(mf)) {
        missing <- c(missing, "a fitted Step 1 model (run the Step 1 model first)")
      }

      if (unsupported) {
        shiny::div(
          class = "alert alert-warning",
          style = "font-size: 13px; margin-top: 4px;",
          shiny::tags$b(
            "\u26a0 Simulations are not yet implemented for ",
            engine, " models."
          ),
          " Please select a linear or RIF model engine to run simulations."
        )
      } else {
        shiny::tagList(
          if (length(missing)) {
            shiny::div(
              class = "alert alert-warning warning-message",
              role = "alert",
              style = "font-size: 13px; margin-top: 4px;",
              shiny::tags$b("To run the simulation, first select "),
              paste(missing, collapse = ", "), "."
            )
          },
          shiny::actionButton(
            ns("run_sim"),
            label = "Run simulation",
            class = "btn-primary",
            icon = shiny::icon("play"),
            style = "width: 100%; margin-top: 4px;",
            disabled = length(missing) > 0
          )
        )
      }
    })

    # ---- Run simulation on button click ------------------------------------

    # REACT-02: one simulation at a time - double-clicks are ignored and the
    # button is disabled for the duration of the run.
    sim_guard <- .busy_guard(session, run_sim)

    # ---- Run signature (INT-08) ----------------------------------------------
    # Everything the simulation depends on, captured at run time into the
    # result and recomputed from live inputs for the staleness comparison.

    .sim_sig_from_live <- function(fit_sig) {
      list(
        step = "sim",
        fit_sig = fit_sig,
        survey_version = survey_version(),
        selected_surveys = .sig_plain(selected_surveys()),
        hist_years = input$hist_years,
        climate = input$climate,
        future_periods = future_periods(),
        fut_sel = .sig_plain(selected_fut()),
        baseline_survey = input$baseline_survey,
        residuals = input$residuals,
        skip_coef_draws = isTRUE(input$include_coef_uncertainty),
        propagate_all_covariate_uncertainty =
          isTRUE(input$propagate_all_covariate_uncertainty)
      )
    }

    live_sim_sig <- reactive({
      mf <- model_fit()
      .sim_sig_from_live(mf$.sig %||% NULL)
    })

    stale_dependencies <- reactive(list(
      model_fit(), survey_version(), selected_surveys(),
      input$hist_years, input$climate,
      input$baseline_survey, input$residuals, input$include_coef_uncertainty,
      input$propagate_all_covariate_uncertainty,
      input$fut_period_1, input$fut_period_2, input$fut_period_3
    ))

    observeEvent(stale_dependencies(),
      {
        hs <- hist_sim()
        if (!is.null(hs) && !identical(live_sim_sig(), hs$.sig)) {
          sim_stale(TRUE)
        }
      },
      ignoreInit = TRUE
    )

    observeEvent(hist_sim(), sim_stale(FALSE))

    sim_run_event <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$run_sim,
      {
        if (shiny::isTruthy(input$run_sim)) {
          sim_run_event(list(source = "manual", value = input$run_sim))
        }
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )
    shiny::observeEvent(run_trigger(),
      {
        ext <- run_trigger()
        if (!is.null(ext)) sim_run_event(list(source = "pipeline", value = ext))
      },
      ignoreInit = FALSE,
      ignoreNULL = TRUE
    )

    observeEvent(sim_run_event(),
      {
        run_generation(run_generation() + 1L)
        run_status("running")
        completed <- FALSE
        on.exit(
          {
            if (!completed) run_status("failure")
          },
          add = TRUE
        )
        req(
          selected_weather(), selected_outcome(),
          survey_weather(), selected_hist(), model_fit()
        )
        if (!sim_guard$begin()) {
          return(invisible(NULL))
        }
        on.exit(sim_guard$end(), add = TRUE)

        # ---- Gather inputs ---------------------------------------------------
        sw <- selected_weather()
        so <- selected_outcome()
        sh <- selected_hist()
        svy <- baseline_svy()
        ss <- baseline_surveys()
        req(ss)
        mf <- model_fit()
        cp <- connection_params()

        sim_dates <- build_hist_sim_dates(svy, unlist(sh$year_range))
        fut_periods <- future_periods()
        sf <- selected_fut()
        has_future <- !is.null(sf) && length(fut_periods) > 0
        ssps <- if (has_future) unique(sf$ssp) else character(0)
        perturbation_method <- if (has_future) build_perturbation_method(sw) else NULL
        fp_list <- if (has_future) {
          lapply(fut_periods, function(yr) {
            c(paste0(yr[1], "-01-01"), paste0(yr[2], "-12-31"))
          })
        } else {
          list()
        }

        # ---- RIF-specific params -------------------------------------------
        engine <- mf$engine %||% "fixest"
        is_rif <- identical(engine, "rif")
        fit_multi <- if (is_rif) mf$fit3 else NULL
        rif_taus <- if (is_rif) mf$taus else NULL
        rif_weather <- if (is_rif) mf$weather_terms else NULL

        # Force residuals = "none" for RIF (delta method, no residual draw)
        sh_residuals <- if (is_rif) "none" else sh$residuals


        shiny::withProgress(message = "Running climate simulation...", value = 0, {
          # ---- Run simulation ------------------------------------------------
          result <- tryCatch(
            fct_run_simulation(
              sw = sw,
              so = so,
              svy = svy,
              ss = ss,
              mf = mf,
              cp = cp,
              fp_list = fp_list,
              ssps = ssps,
              residuals = sh_residuals, # sh$residuals,
              skip_coef_draws = !isTRUE(input$include_coef_uncertainty),
              propagate_all_covariate_uncertainty =
                isTRUE(input$propagate_all_covariate_uncertainty),
              sim_dates = sim_dates,
              perturbation_method = perturbation_method,
              stored_breaks = stored_breaks(),
              fit_multi = fit_multi,
              taus = rif_taus,
              weather_cols = rif_weather,
              weather_storage = match.arg(
                Sys.getenv("WISEAPP_STEP2_WEATHER_STORAGE", "memory"),
                c("memory", "reference")
              ),
              weather_collect = match.arg(
                Sys.getenv("WISEAPP_STEP2_WEATHER_COLLECT", "fast"),
                c("fast", "bounded")
              ),
              weather_threads = match.arg(
                Sys.getenv("WISEAPP_STEP2_WEATHER_THREADS", "auto"),
                c("auto", "1", "2")
              ),
              direct_rif_predictions = TRUE,
              seed = wise_current_seed(),
              payload_mode = "compact",
              progress_fn = function(value, detail) {
                shiny::setProgress(
                  value = value,
                  detail = detail
                )
              }
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
          # Aggregation now happens lazily in mod_2_02_results.R via the analytic
          # delta method - no pre-aggregation step here.
          # INT-05: bind the historical scenario label into the result so the
          # Step 3 pane describes the simulated run, not the live selection.
          result$hist_sim_result$hist_label <- sh$scenario_name
          model_spec <- mf$.snap$model %||% list()
          result$hist_sim_result$sim_summary <- list(
            weather = sw,
            historical_years = unlist(sh$year_range[[1]], use.names = FALSE),
            baseline_survey = {
              ch <- baseline_survey_choices()
              sel <- input$baseline_survey %||% baseline_default()
              nms <- names(ch)[ch %in% sel]
              if (length(nms)) paste(nms, collapse = ", ") else "Selected baseline survey"
            },
            baseline_n = nrow(svy),
            model = list(
              label = if (length(model_spec)) model_badge(model_spec) else "Fitted model",
              weather_terms = length(mf$weather_terms %||% character(0)),
              fixed_effects = length(model_spec$fixedeffects %||% mf$fe_terms %||% character(0)),
              covariates = if (length(model_spec)) model_covariate_total(model_spec) else NA_integer_
            ),
            total_runs = result$total_runs
          )
          # INT-08: the immutable run signature travels with the result so
          # Step 3 can detect that it is consuming a superseded simulation.
          result$hist_sim_result$.sig <- .sim_sig_from_live(mf$.sig %||% NULL)
          sim_stale(FALSE)
          old_lease <- weather_store_lease()
          weather_store_lease(result$weather_store_lease %||% NULL)
          result$hist_sim_result$weather_store_lease <-
            result$weather_store_lease %||% NULL
          hist_sim(result$hist_sim_result)
          saved_scenarios(result$new_scenarios)
          step2_weather_store_release(old_lease)
          run_status("success")
          completed <- TRUE

          shiny::setProgress(value = 1, detail = "Results ready")
        })

        # ---- REACT-12: partial failures get a prominent persistent warning ----
        # (Historical or whole-group failures throw inside fct_run_simulation,
        # so reaching this point means results are publishable.)
        sim_failures <- result$failures %||% list()
        if (length(sim_failures) > 0L) {
          fail_txt <- paste(vapply(sim_failures, function(f) {
            sprintf("%s: %s", f$key, f$error)
          }, character(1)), collapse = "\n")
          shiny::showNotification(
            ui = tagList(
              tags$b("Simulation completed with some scenarios unavailable."),
              tags$br(),
              tags$details(
                tags$summary("Show details"),
                tags$div(
                  style = "font-size: 12px; white-space: pre-wrap;",
                  fail_txt
                )
              )
            ),
            type = "warning", duration = NULL
          )
        }

        # ---- Completion notification -----------------------------------------
        message(sprintf(
          "[wiseapp] TOTAL wall time: %s | weather: %s | pipelines: %s | %d/%d key(s)",
          format_elapsed(result$t_elapsed),
          format_elapsed(result$t_weather %||% 0),
          format_elapsed(result$t_elapsed - (result$t_weather %||% 0)),
          result$n_keys_ok %||% result$n_keys, result$n_keys
        ))

        if (!length(sim_failures)) {
          shiny::showNotification(
            "Climate scenario results are ready.",
            type = "message", duration = 3
          )
        }
      },
      ignoreInit = TRUE
    )

    # ---- Return API --------------------------------------------------------

    list(
      hist_sim = hist_sim,
      saved_scenarios = saved_scenarios,
      selected_hist = selected_hist,
      selected_fut = selected_fut,
      residuals = reactive(input$residuals %||% "original"),
      skip_coef_draws = reactive(!isTRUE(input$include_coef_uncertainty)),
      propagate_all_covariate_uncertainty =
        reactive(isTRUE(input$propagate_all_covariate_uncertainty)),
      stale = sim_stale,
      run_generation = run_generation,
      run_status = run_status,
      clear_weather_stores = cleanup_weather_stores
    )
  })
}
