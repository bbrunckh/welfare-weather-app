# ---------------------------------------------------------------------------- #
# diag_content_ui: inserted once into the Diagnostics tab                     #
# Extracted from the insertUI observeEvent to keep the observer readable.     #
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# diag_content_ui: inserted once into the Diagnostics tab                     #
# Extracted from the insertUI observeEvent to keep the observer readable.     #
# ---------------------------------------------------------------------------- #

diag_content_ui <- function(ns, so, weather_var_choices, scenario_names = character(0)) {

  tagList(

    # ---- 0. Scenario filters (reactive -- populated by server) -------------
    shiny::uiOutput(ns("scenario_filter_panel")),

    # ---- 1. Weather inputs panel -------------------------------------------
    shiny::wellPanel(
      shiny::h4("Weather input distributions"),
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap; margin-bottom:8px;",
        shiny::tags$div(style = "flex:3; min-width:200px;",
          shiny::selectInput(
            ns("diag_weather_vars"),
            label    = "Weather variables (select one or more)",
            choices  = weather_var_choices,
            selected = weather_var_choices[seq_len(min(2, length(weather_var_choices)))],
            multiple = TRUE
          )
        )
      ),
      shiny::plotOutput(ns("diag_weather_density"), height = "340px"),
      # Per-variable log10 checkboxes -- rendered dynamically in server
      shiny::uiOutput(ns("diag_weather_log_ui")),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:4px;",
        shiny::tags$b("Grey fill = Full historical:"),
        " all years at survey locations and months.",
        shiny::tags$br(),
        shiny::tags$b("Black dashed = Regression input"),
        " (shown when \"Include regression input\" is selected above).",
        shiny::tags$br(),
        shiny::tags$b("Coloured lines = Future scenarios:"),
        " solid = earliest forecast year, dashed = middle, dotted = latest."
      )
    ),

    # ---- 2. Welfare distributions panel -----------------------------------
    shiny::wellPanel(
      shiny::h4("Welfare output distributions"),

      # -- Primary grouping radio -------------------------------------------
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:16px; flex-wrap:wrap; margin-bottom:4px;",
        shiny::tags$div(style = "flex:2; min-width:260px;",
          shiny::radioButtons(
            ns("diag_ridge_primary_group"),
            label    = "Primary grouping",
            choices  = c(
              "Historical year"               = "hist_year",
              "Scenario × Forecast year" = "scenario",
              "Forecast year × Scenario" = "forecast_yr"
            ),
            selected = "hist_year",
            inline   = TRUE
          )
        )
      ),

      # -- Welfare output-specific controls ---------------------------------
      # Row 1: checkboxes
      shiny::tags$div(
        style = "display:flex; gap:24px; flex-wrap:wrap; margin-bottom:4px; align-items:center;",
        shiny::tags$div(
          shiny::checkboxInput(
            ns("diag_ridge_log"),
            label = "Log₁₀ x-axis",
            value = FALSE
          )
        )
      ),
      # Row 2: ridge height slider
      shiny::tags$div(
        style = "max-width:380px; margin-bottom:2px;",
        shiny::sliderInput(
          ns("diag_ridge_scale"),
          label = "Ridge height",
          min   = 0.3,
          max   = 3.0,
          value = 1.5,
          step  = 0.1
        )
      ),
      # Row 3: row spacing slider
      shiny::tags$div(
        style = "max-width:380px; margin-bottom:8px;",
        shiny::sliderInput(
          ns("diag_ridge_spacing"),
          label = "Row spacing",
          min   = 0.2,
          max   = 3.0,
          value = 1.0,
          step  = 0.1
        )
      ),

      # -- Caption ----------------------------------------------------------
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-bottom:8px;",
        shiny::tags$b("Historical year mode:"),
        " one grey filled ridge per simulation year; coloured lines = scenario perturbations.",
        shiny::tags$br(),
        shiny::tags$b("Scenario / Forecast year mode:"),
        " one row per scenario or forecast year; grey-scale lines = individual historical years",
        " (darkest = most recent).",
        shiny::tags$br(),
        shiny::tags$b("Include regression output:"),
        " overlays predicted (dashed) and actual (dotted) outcome densities from training data.",
        shiny::tags$br(),
        "All ridges share a common global bandwidth. X-axis clipped to P1–P99."
      ),

      # -- Dynamic height plotOutput ----------------------------------------
      shiny::uiOutput(ns("diag_ridge_plot_ui"))
    )
  )
}

#' 2_05_sim_diag Server Functions
#'
#' Appends a Diagnostics tab to the main tabset once the historical simulation
#' has run. Weather density and welfare ridge panels auto-refresh when
#' saved_scenarios is updated or input controls change.
#'
#' @param id               Module id.
#' @param hist_sim         Reactive list. Must have $preds, $so, $weather_raw.
#' @param fut_sim          Reactive list of future simulation phase results.
#' @param saved_scenarios  ReactiveVal holding named scenario entries.
#' @param survey_weather   Reactive data frame of merged survey-weather data.
#' @param selected_weather Reactive data frame of selected weather variable
#'   metadata.
#' @param tabset_id        Character id of the parent tabset panel.
#' @param tabset_session   Shiny session for the tabset.
#'
#' @noRd
mod_2_05_sim_diag_server <- function(id,
                                      hist_sim         = NULL,
                                      fut_sim          = NULL,
                                      saved_scenarios  = NULL,
                                      survey_weather   = NULL,
                                      selected_weather = NULL,
                                      tabset_id,
                                      tabset_session   = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    diag_tab_added <- reactiveVal(FALSE)

    # ---- Insert Diagnostics tab once historical sim runs ------------------

    observeEvent(hist_sim(), {
      req(hist_sim())
      if (diag_tab_added()) return()

      so  <- hist_sim()$so
      sw  <- if (!is.null(selected_weather)) selected_weather() else NULL
      # Use readable labels as display names; raw column names as values
      weather_var_choices <- if (!is.null(sw) && "name" %in% names(sw)) {
        if ("label" %in% names(sw)) setNames(sw$name, sw$label) else sw$name
      } else character(0)
      sc_nms <- if (!is.null(saved_scenarios)) names(isolate(saved_scenarios())) else character(0)

      shiny::appendTab(
        inputId = tabset_id,
        shiny::tabPanel(
          title = "Diagnostics",
          value = "diag_tab",
          diag_content_ui(ns, so, weather_var_choices, scenario_names = sc_nms)
        ),
        select  = FALSE,
        session = tabset_session
      )

      diag_tab_added(TRUE)
    }, ignoreInit = TRUE)

    # ---- Update weather variable choices when selected_weather changes ----

    observeEvent(selected_weather(), {
      req(diag_tab_added())
      sw      <- selected_weather()
      choices <- if (!is.null(sw) && "name" %in% names(sw)) {
        if ("label" %in% names(sw)) setNames(sw$name, sw$label) else sw$name
      } else character(0)
      current <- isolate(input$diag_weather_vars)
      new_sel <- if (!is.null(current)) intersect(current, choices) else
        choices[seq_len(min(2, length(choices)))]
      if (length(new_sel) == 0) new_sel <- choices[1]
      shiny::updateSelectInput(session, "diag_weather_vars",
                               choices  = choices,
                               selected = new_sel)
    }, ignoreInit = TRUE)

    # ---- Render: Scenario filter panel ------------------------------------
    # Reactive renderUI -- rebuilds as scenarios are saved after tab-open.

    output$scenario_filter_panel <- shiny::renderUI({
      sc_all <- if (!is.null(saved_scenarios)) names(saved_scenarios()) else character(0)

      ssp_of <- function(nm) {
        m_full <- regmatches(nm, regexpr("SSP[0-9]-[0-9.]+", nm))
        if (length(m_full) > 0) return(m_full)
        m_short <- regmatches(nm, regexpr("SSP([2-9])", nm))
        if (length(m_short) == 0) return(NA_character_)
        digit  <- sub("SSP", "", m_short)
        lookup <- c("2" = "SSP2-4.5", "3" = "SSP3-7.0", "5" = "SSP5-8.5")
        if (digit %in% names(lookup)) lookup[[digit]] else NA_character_
      }
      yr_of <- function(nm) {
        m <- regmatches(nm, regexpr("[0-9]{4}", nm))
        if (length(m) == 0L) NA_character_ else m
      }

      unique_ssps <- sort(unique(Filter(Negate(is.na), vapply(sc_all, ssp_of, character(1)))))
      unique_yrs  <- sort(unique(Filter(Negate(is.na), vapply(sc_all, yr_of,  character(1)))))

      shiny::wellPanel(
        style = "padding: 10px 16px 8px 16px; background:#f8f8f8; margin-bottom:10px;",
        shiny::tags$div(
          style = "display:flex; align-items:baseline; gap:8px; margin-bottom:10px;",
          shiny::tags$b("Scenario Filters", style = "font-size:13px;"),
          shiny::tags$span(
            style = "font-size:11px; color:#888;",
            "\u2014 applies to all panels below"
          )
        ),
        shiny::tags$div(
          style = "display:flex; flex-wrap:wrap; gap:24px; align-items:flex-start;",
          if (length(unique_ssps) > 0)
            shiny::tags$div(
              shiny::checkboxGroupInput(
                inputId  = ns("filter_ssps"),
                label    = shiny::tags$b("Climate scenario",
                             style = "font-size:11px; font-weight:600;"),
                choices  = setNames(unique_ssps, unique_ssps),
                selected = unique_ssps,
                inline   = TRUE
              )
            ),
          if (length(unique_yrs) > 0)
            shiny::tags$div(
              shiny::checkboxGroupInput(
                inputId  = ns("filter_yrs"),
                label    = shiny::tags$b("Timeframe",
                             style = "font-size:11px; font-weight:600;"),
                choices  = setNames(unique_yrs, unique_yrs),
                selected = unique_yrs,
                inline   = TRUE
              )
            )
        ),
        # Regression input toggle (separate row)
        shiny::tags$div(
          style = "margin-top:8px; border-top:1px solid #e0e0e0; padding-top:6px;",
          shiny::checkboxInput(
            ns("show_regression_input"),
            label = "Include regression input",
            value = FALSE
          )
        )
      )
    })

    # ---- Reactive: active scenario filter -----------------------------------

    active_scenarios_data <- reactive({
      sc_all <- if (!is.null(saved_scenarios)) names(saved_scenarios()) else character(0)
      if (length(sc_all) == 0) return(character(0))

      ssp_of <- function(nm) {
        m_full <- regmatches(nm, regexpr("SSP[0-9]-[0-9.]+", nm))
        if (length(m_full) > 0) return(m_full)
        m_short <- regmatches(nm, regexpr("SSP([2-9])", nm))
        if (length(m_short) == 0) return(NA_character_)
        digit  <- sub("SSP", "", m_short)
        lookup <- c("2" = "SSP2-4.5", "3" = "SSP3-7.0", "5" = "SSP5-8.5")
        if (digit %in% names(lookup)) lookup[[digit]] else NA_character_
      }
      yr_of <- function(nm) {
        m <- regmatches(nm, regexpr("[0-9]{4}", nm))
        if (length(m) == 0L) NA_character_ else m
      }

      sel_ssps <- input$filter_ssps %||% character(0)
      sel_yrs  <- input$filter_yrs  %||% character(0)

      Filter(function(nm) {
        ssp    <- ssp_of(nm)
        yr     <- yr_of(nm)
        ssp_ok <- length(sel_ssps) == 0L || isTRUE(ssp %in% sel_ssps)
        yr_ok  <- length(sel_yrs)  == 0L || isTRUE(yr  %in% sel_yrs)
        ssp_ok && yr_ok
      }, sc_all)
    })

    # ---- Reactive: perturbed weather per saved scenario ------------------

    scenario_weather_data <- reactive({
      sc <- if (!is.null(saved_scenarios)) saved_scenarios() else list()
      if (length(sc) == 0) return(NULL)
      out <- lapply(sc, function(e) e$weather_raw)
      out <- Filter(Negate(is.null), out)
      if (length(out) == 0) NULL else out
    })

    # ---- Reactive: pre-computed KDE data ----------------------------------
    # Expensive density() calls live here and only re-run when preds change.
    # Aesthetic/scale inputs (log, ridge_scale, row_gap) are applied later
    # in renderPlot and do NOT invalidate this reactive.

    ridge_kde_data <- reactive({
      req(hist_sim(), diag_tab_added())

      sc_raw    <- if (!is.null(saved_scenarios)) saved_scenarios() else list()
      active_nm <- active_scenarios_data()
      sc_raw    <- sc_raw[intersect(active_nm, names(sc_raw))]
      scen_list <- lapply(sc_raw, function(e) list(preds = e$preds, so = e$so))

      build_ridge_kde_data(
        hist_preds    = hist_sim()$preds,
        scenario_list = scen_list,
        outcome_name  = hist_sim()$so$name,
        actual_vals   = {
          td <- hist_sim()$train_data
          so <- hist_sim()$so
          nm <- so$name
          if (!is.null(td) && nm %in% names(td)) {
            v <- as.numeric(td[[nm]])
            v <- v[is.finite(v)]
            # train_data stores raw survey values already in linear space.
            # hist_preds$.fitted is back-transformed by apply_log_backtransform().
            # Both are on the same scale -- no further transform needed.
            # (Previous exp() call here was incorrect and has been removed.)
            v
          } else numeric(0)
        }
      )
    })

    # ---- Reactive: debounced aesthetic inputs ----------------------------
    # Slider drags fire on every tick; debounce batches them to one render
    # after 350ms of inactivity so renderPlot is not called 20x per drag.

    debounced_ridge_inputs <- shiny::debounce(
      reactive({
        list(
          log_scale     = isTRUE(input$diag_ridge_log),
          ridge_scale   = input$diag_ridge_scale   %||% 1.5,
          row_gap       = input$diag_ridge_spacing %||% 1.0,
            primary_group   = input$diag_ridge_primary_group %||% "hist_year",
            show_regression = isTRUE(input$show_regression_input)
        )
      }),
      350
    )

    # ---- Render: per-variable log10 checkboxes ---------------------------
    # One checkbox per currently selected weather variable, using readable label.

    output$diag_weather_log_ui <- shiny::renderUI({
      vars <- input$diag_weather_vars
      req(length(vars) > 0)
      sw  <- if (!is.null(selected_weather)) selected_weather() else NULL
      lbl_map <- if (!is.null(sw) && all(c("name", "label") %in% names(sw)))
        setNames(sw$label, sw$name) else setNames(vars, vars)

      shiny::tags$div(
        style = "display:flex; flex-wrap:wrap; gap:16px; margin-top:6px;",
        lapply(seq_along(vars), function(i) {
          v   <- vars[[i]]
          lbl <- lbl_map[[v]] %||% v
          shiny::checkboxInput(
            inputId = ns(paste0("diag_log_var_", i)),
            label   = paste0("Log\u2081\u2080: ", lbl),
            value   = FALSE
          )
        })
      )
    })

    # ---- Render: Weather density plot ------------------------------------

    output$diag_weather_density <- renderPlot({
      req(hist_sim(), survey_weather(), diag_tab_added())
      req(!is.null(hist_sim()$weather_raw))
      vars <- input$diag_weather_vars
      req(length(vars) > 0)

      sw      <- if (!is.null(selected_weather)) selected_weather() else NULL
      lbl_map <- if (!is.null(sw) && all(c("name", "label") %in% names(sw)))
        setNames(sw$label, sw$name) else NULL

      # Build per-variable log vector
      log_x_vec <- vapply(seq_along(vars), function(i)
        isTRUE(input[[paste0("diag_log_var_", i)]]), logical(1))

      plot_weather_density_panel(
        survey_weather   = survey_weather(),
        weather_raw      = hist_sim()$weather_raw,
        weather_vars     = vars,
        weather_labels   = lbl_map,
        scenario_weather = scenario_weather_data(),
        active_scenarios = active_scenarios_data(),
        log_x            = log_x_vec,
        show_regression  = isTRUE(input$show_regression_input)
      )
    })

    # ---- Render: dynamic plot height for year-anchored ridge -------------
    # hist_year mode: ~80px per year x row_gap.
    # scenario mode:  ~80px per scenario row x row_gap (max 9 rows).

    output$diag_ridge_plot_ui <- shiny::renderUI({
      req(hist_sim())
      ri       <- debounced_ridge_inputs()
      hp       <- hist_sim()$preds
      yr_col   <- intersect(c("sim_year", "year"), names(hp))[1]
      n_yrs    <- if (!is.na(yr_col)) length(unique(hp[[yr_col]])) else 20L
      n_scen   <- length(active_scenarios_data())
      n_rows   <- if (ri$primary_group %in% c("scenario", "forecast_yr")) max(1L, n_scen) else n_yrs
      plot_ht  <- min(4000L, max(500L, as.integer(n_rows * 80L * ri$row_gap + 160L)))
      shiny::plotOutput(ns("diag_ridge"), height = paste0(plot_ht, "px"))
    })

    # ---- Render: year-anchored welfare ridge plot ------------------------

    output$diag_ridge <- renderPlot({
      req(diag_tab_added())
      kd <- ridge_kde_data()
      req(!is.null(kd))
      ri <- debounced_ridge_inputs()

      plot_year_anchored_ridge(
        kde_data      = kd,
        x_label       = hist_sim()$so$label %||% hist_sim()$so$name,
        primary_group = ri$primary_group,
        log_scale     = ri$log_scale,
        ridge_scale   = ri$ridge_scale,
        row_gap         = ri$row_gap,
        show_regression = ri$show_regression
      )
    })

    # ---- Return API ---------------------------------------------------------

    list()
  })
}
