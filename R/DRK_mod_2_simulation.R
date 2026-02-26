#DRK Version 20260220 1900

mod_2_simulation_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    tags$head(
      tags$style(HTML("
        .plot-wrap { position: relative; }
        .plot-overlay {
          position: absolute; top: 0; left: 0; right: 0; bottom: 0;
          background: rgba(255,255,255,0.65);
          display: flex; align-items: center; justify-content: center;
          gap: 12px;
          z-index: 10;
        }
        .spin-circle {
          width: 44px; height: 44px;
          border: 6px solid rgba(0,0,0,0.15);
          border-top-color: rgba(0,0,0,0.65);
          border-radius: 50%;
          animation: drk_spin 1s linear infinite;
        }
        @keyframes drk_spin { to { transform: rotate(360deg); } }
        .plot-wrap { overflow-x: hidden; }      /* prevents parent horizontal scroll */
        table.shiny-table { width: 100%; }      /* forces the Shiny table to fit */
      "))
    ),

    h4("What welfare is expected given historical weather conditions? In future climate scenarios?"),

    sidebarLayout(
      sidebarPanel(
        bsplus::bs_accordion(id = ns("accordion")) |>
          bsplus::bs_append(
            title = "1 Historical weather",
            content = mod_2_01_historical_ui(ns("historical"))
          ) |>
          bsplus::bs_append(
            title = "2 Climate change",
            content = mod_2_02_climate_ui(ns("climate"))
          )
      ),

      mainPanel(
        tabsetPanel(
          id = ns("step2_output_tabs"),

          tabPanel(
            title = "Run",
            value = "run",

            uiOutput(ns("run_banner_ui")),

            bslib::card(
              bslib::card_header(
                fluidRow(
                  column(4, uiOutput(ns("viz_haz_ui"))),
                  column(4, uiOutput(ns("viz_metric_ui"))),
                  column(4, uiOutput(ns("povline_label_ui")))
                )
              ),

              div(
                class = "plot-wrap",

                fluidRow(
                  plotOutput(ns("viz_pov_exceed_plot"), height = "420px", width = "100%"),
                ),

                tags$hr(),
                h4("Read-off at common return periods"),

                tableOutput(ns("viz_return_readoff_tbl")),

                tags$p(
                  class = "small text-muted",
                  "Interpretation: This exceedance curve summarizes the distribution of simulated annual values of the selected welfare metric (FGT0/1/2). ",
                  "A return period of 10 years means that outcomes at least that severe occur with about a 10% chance in any given year ",
                  "(about 1-in-10). The left axis shows the change in the selected metric (percentage points) relative to the expected poverty; ",
                  "the right axis converts the same values to the absolute metric level."
                ),

                uiOutput(ns("run_overlay_ui"))
              ))
          ),


          tabPanel(
            title = "Compare",
            value = "compare",

            h4("Compare scenarios"),
            uiOutput(ns("compare_notice_ui")),

            fluidRow(
              column(6, uiOutput(ns("compare_base_ui"))),
              column(6, uiOutput(ns("compare_alt_ui")))
            ),

            tags$hr(),
            plotOutput(ns("compare_exceed_plot"), height = "420px", width = "100%"),

            tags$hr(),
            h4("Differences at common return periods"),
            tableOutput(ns("compare_readoff_tbl")),

            tags$p(
              class = "small text-muted",
              "Each successful run is saved as a scenario (by its Scenario ID). ",
              "Select two scenarios to compare their poverty exceedance curves (poverty rate vs return period). ",
              "The table reports the difference in poverty rates at common return periods (comparison minus baseline)."
            ),

            actionButton(ns("compare_clear_btn"), "Clear saved scenarios", class = "btn btn-outline-secondary btn-sm")
          ),

          tabPanel(
            title = "Weather & Inputs",
            value = "inputs",

            h4("Historical weather distribution vs weather used to fit model"),
            plotOutput(ns("viz_haz_overlay_plot"), height = "280px"),

            tags$hr(),
            h4("Simulated welfare distributions, by median welfare"),
            plotOutput(ns("viz_welfare_ridges_plot"), height = "520px")
          ),

          tabPanel(
            title = "Tables",
            value = "tables",

            h4("Poverty results (Phase E)"),
            tags$hr(),
            h5("Overall"),
            tableOutput(ns("pov_overall_tbl")),
            tags$hr(),
            h5("By simulated year"),
            tableOutput(ns("pov_by_year_tbl")),
            tags$hr(),
            plotOutput(ns("pov_by_year_plot"), height = "320px"),

            tags$hr(),
            h4("Prediction diagnostics (Phase D)"),
            h5("NA predictions summary"),
            tableOutput(ns("historical-pred_na_rate_tbl")),

            h5("NA predictions by simulated year"),
            tableOutput(ns("historical-pred_na_by_year_tbl")),

            h5("NA predictions by interview month"),
            tableOutput(ns("historical-pred_na_by_month_tbl")),

            h5("Top locations by NA count"),
            tableOutput(ns("historical-pred_na_by_loc_tbl")),

            h5("Which predictors are missing among NA predictions?"),
            tableOutput(ns("historical-pred_na_drivers_tbl")),

            h5("Overall prediction summary"),
            tableOutput(ns("historical-pred_summary_tbl")),

            tags$hr(),
            bslib::card(
              bslib::card_header("Prediction + poverty-line diagnostics"),
              tableOutput(ns("pred_summary_tbl")),
              tags$hr(),
              tableOutput(ns("pline_diag_tbl"))
            )
          ),

          tabPanel(
            title = "Diagnostics",
            value = "diagnostics",

            h4("Pipeline status"),
            verbatimTextOutput(ns("step2_status")),

            tags$hr(),
            h5("Smoke tests"),
            tableOutput(ns("smoke_tbl")),


            tags$hr(),
            h5("Historical module phase status"),
            h6("Phase B"),
            verbatimTextOutput(ns("historical-phase_b_status")),
            h6("Phase C"),
            verbatimTextOutput(ns("historical-phase_c_status")),
            h6("Phase D"),
            verbatimTextOutput(ns("historical-phase_d_status")),
            h6("Phase E"),
            verbatimTextOutput(ns("historical-phase_e_status")),

            tags$hr(),
            h5("Artifact checks"),
            tableOutput(ns("historical-checks_tbl")),
            tags$hr(),
            h6("Check details"),
            uiOutput(ns("historical-checks_select_ui")),
            verbatimTextOutput(ns("historical-checks_detail_txt"))
          )
        )
      )
    )
  )
}



mod_2_simulation_server <- function(id, step1 = NULL, pov_lines = NULL, varlist = NULL, board = NULL) {
  moduleServer(id, function(input, output, session) {

    # Namespace helper for server-side UI generation
    ns <- session$ns

    # Step 1 -> Step 2 shim + contract check (v0)
    step1_s2 <- adapt_step1_to_step2_v0(step1, require_weather_api = TRUE)
    step1_contract <- check_step1_contract_step2(step1_s2, require_weather_api = TRUE)

    # Poverty line label: prefer the current run's recorded label (so UI does not drift if Step 1 inputs change),
    # fall back to Step 1 selection when no run exists yet.
    output$povline_label_ui <- renderUI({
      r <- tryCatch(hist_run(), error = function(e) NULL)
      lbl <- NULL
      if (!is.null(r) && is.list(r) && !is.null(r$meta) && !is.null(r$meta$povline_label)) {
        cand <- as.character(r$meta$povline_label)
        if (length(cand) && nzchar(cand) && !is.na(cand)) lbl <- cand[[1]]
      }
      if (is.null(lbl)) {
        pl <- tryCatch(get_active_poverty_line(step1_s2), error = function(e) NULL)
        lbl <- if (is.null(pl) || is.null(pl$label) || !nzchar(pl$label)) "NA" else pl$label
      }
      tags$div(tags$strong("Poverty line:"), paste0(" ", lbl))
    })


    # Keep selector outputs active even when their tab is hidden (so downstream plots keep working).
    # NOTE: outputOptions() errors if called before the output binding exists, so register after first flush.
    session$onFlushed(function() {
      outputOptions(output, "viz_haz_ui", suspendWhenHidden = FALSE)
      outputOptions(output, "viz_metric_ui", suspendWhenHidden = FALSE)
    }, once = TRUE)


    # Submodules (wired, even if they don't use args yet)
    historical_api <- mod_2_01_historical_server(
      "historical",
      step1 = step1_s2,
      pov_lines = pov_lines, #DRK Note - I probably don't need this.
      varlist = varlist,
      board = board
    )


    `%||%` <- function(a, b) if (!is.null(a)) a else b

    # --- One-run-object wiring: prefer the run bundle as the primary input ---
    hist_run <- reactive({
      if (is.null(historical_api) || !is.list(historical_api) || is.null(historical_api$run)) return(NULL)
      tryCatch(historical_api$run(), error = function(e) NULL)
    })
    hist_checks <- reactive({
      r <- hist_run()
      if (is.null(r) || is.null(r$checks)) return(NULL)
      r$checks
    })
    hist_scenario <- reactive({
      r <- hist_run()
      if (is.null(r) || is.null(r$meta) || is.null(r$meta$scenario_spec)) return(NULL)
      r$meta$scenario_spec
    })


    # Smoke tests: fast regression snapshot (contract + board + phase artifacts)
    output$smoke_tbl <- renderTable({
      r <- hist_run()
      smoke_step2_snapshot(step1 = step1, board = board, run = r)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    get_phase <- function(r, nm) {
      if (is.null(r) || is.null(r[[nm]])) return(NULL)
      r[[nm]]
    }
    phase_err <- function(r, nm) {
      p <- get_phase(r, nm)
      if (is.null(p)) return("")
      p$error %||% ""
    }

    # --- Scenario registry (saved runs for comparison) ---
    runs_r <- reactiveVal(list())

    scenario_id_from_run <- function(r) {
      sc <- NULL
      try(sc <- r$meta$scenario_spec, silent = TRUE)
      id <- NULL
      if (!is.null(sc) && is.list(sc) && "id" %in% names(sc)) id <- sc$id
      if (is.null(id) || !nzchar(id)) {
        # Fallback (should be rare once ScenarioSpec is always set)
        id <- paste0("scenario_", format(Sys.time(), "%Y%m%d%H%M%S"))
      }
      id
    }

    observeEvent(hist_run(), {
      r <- hist_run()
      if (is.null(r)) return()

      pe <- get_phase(r, "phase_e")
      if (is.null(pe) || !isTRUE(pe$ok) || is.null(pe$data)) return()

      id <- scenario_id_from_run(r)

      runs <- runs_r() %||% list()
      runs[[id]] <- r
      runs_r(runs)
    }, ignoreInit = TRUE)

    scenario_ids <- reactive({
      ids <- names(runs_r() %||% list())
      sort(unique(ids))
    })

    observeEvent(input$compare_clear_btn, {
      runs_r(list())
    }, ignoreInit = TRUE)

    output$compare_notice_ui <- renderUI({
      ids <- scenario_ids()
      if (!length(ids)) {
        return(tags$div(class = "alert alert-info",
                        "No saved scenarios yet. Run at least one scenario in the Historical sidebar first."))
      }
      tags$div(
        class = "alert alert-secondary",
        paste0(length(ids), " saved scenario(s): ", paste(ids, collapse = ", "))
      )
    })

    output$compare_base_ui <- renderUI({
      ids <- scenario_ids()
      if (!length(ids)) return(NULL)
      sel <- if (length(ids) >= 2) ids[[length(ids) - 1]] else ids[[1]]
      shiny::selectInput(
        inputId = ns("compare_base_id"),
        label   = "Baseline scenario",
        choices = ids,
        selected = sel
      )
    })

    output$compare_alt_ui <- renderUI({
      ids <- scenario_ids()
      if (!length(ids)) return(NULL)
      sel <- ids[[length(ids)]]
      shiny::selectInput(
        inputId = ns("compare_alt_id"),
        label   = "Comparison scenario",
        choices = ids,
        selected = sel
      )
    })

    extract_pov_by_year_from_run <- function(r) {
      pe <- get_phase(r, "phase_e")
      if (is.null(pe) || !isTRUE(pe$ok) || is.null(pe$data) || is.null(pe$data$by_year)) return(NULL)
      d <- pe$data$by_year

      # If multiple poverty lines exist, keep the first.
      if ("pline" %in% names(d)) {
        pl0 <- d$pline[[which(!is.na(d$pline))[1]]]
        d <- dplyr::filter(d, .data$pline == pl0)
      }

      d <- dplyr::filter(d, is.finite(.data$pov_rate))
      d
    }

    build_exceed_curve_from_by_year <- function(d, max_T = 100, n_grid = 200) {
      expected <- mean(d$pov_rate, na.rm = TRUE)

      T_grid <- exp(seq(log(1), log(max_T), length.out = n_grid))
      probs  <- 1 - 1 / T_grid
      probs  <- pmax(pmin(probs, 0.999999), 0)

      pov_q <- as.numeric(stats::quantile(d$pov_rate, probs = probs, na.rm = TRUE, names = FALSE, type = 7))

      data.frame(
        return_period    = T_grid,
        pov_pct          = 100 * pov_q,
        expected_pov_pct = 100 * expected,
        stringsAsFactors = FALSE
      )
    }

    output$compare_exceed_plot <- renderPlot({
      ids <- scenario_ids()
      validate(need(length(ids) >= 1, "No saved scenarios yet."))

      base_id <- input$compare_base_id
      alt_id  <- input$compare_alt_id
      validate(need(!is.null(base_id) && base_id %in% ids, "Select a baseline scenario."))
      validate(need(!is.null(alt_id)  && alt_id  %in% ids, "Select a comparison scenario."))

      runs <- runs_r()
      r_base <- runs[[base_id]]
      r_alt  <- runs[[alt_id]]

      d_base <- extract_pov_by_year_from_run(r_base)
      d_alt  <- extract_pov_by_year_from_run(r_alt)

      validate(need(!is.null(d_base) && nrow(d_base) > 0, "Baseline scenario has no Phase E by-year results."))
      validate(need(!is.null(d_alt)  && nrow(d_alt)  > 0, "Comparison scenario has no Phase E by-year results."))

      c_base <- build_exceed_curve_from_by_year(d_base); c_base$scenario <- base_id
      c_alt  <- build_exceed_curve_from_by_year(d_alt);  c_alt$scenario  <- alt_id

      df <- dplyr::bind_rows(c_base, c_alt)
      ref_T <- c(5, 10, 20, 25, 50, 100)

      ggplot2::ggplot(df, ggplot2::aes(x = .data$return_period, y = .data$pov_pct, color = .data$scenario)) +
        ggplot2::geom_vline(xintercept = ref_T, linetype = "dotted", color = "grey70") +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::scale_x_log10(breaks = c(1, 2, ref_T)) +
        ggplot2::labs(
          title = "Poverty exceedance curves by scenario",
          x = "Return period (years, log scale)",
          y = "Poverty rate (%)",
          color = "Scenario"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"))
    })

    output$compare_readoff_tbl <- renderTable({
      ids <- scenario_ids()
      validate(need(length(ids) >= 1, "No saved scenarios yet."))

      base_id <- input$compare_base_id
      alt_id  <- input$compare_alt_id
      validate(need(!is.null(base_id) && base_id %in% ids, "Select a baseline scenario."))
      validate(need(!is.null(alt_id)  && alt_id  %in% ids, "Select a comparison scenario."))

      runs <- runs_r()
      r_base <- runs[[base_id]]
      r_alt  <- runs[[alt_id]]

      d_base <- extract_pov_by_year_from_run(r_base)
      d_alt  <- extract_pov_by_year_from_run(r_alt)

      validate(need(!is.null(d_base) && nrow(d_base) > 0, "Baseline scenario has no Phase E by-year results."))
      validate(need(!is.null(d_alt)  && nrow(d_alt)  > 0, "Comparison scenario has no Phase E by-year results."))

      T <- c(5, 10, 20, 25, 50, 100)
      probs <- 1 - 1 / T
      probs <- pmax(pmin(probs, 0.999999), 0)

      q_base <- as.numeric(stats::quantile(d_base$pov_rate, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
      q_alt  <- as.numeric(stats::quantile(d_alt$pov_rate,  probs = probs, na.rm = TRUE, names = FALSE, type = 7))

      data.frame(
        return_period_years   = T,
        exceed_prob           = round(1 / T, 4),
        baseline_poverty_pct  = round(100 * q_base, 1),
        comparison_poverty_pct= round(100 * q_alt, 1),
        diff_pp               = round(100 * (q_alt - q_base), 1),
        stringsAsFactors      = FALSE
      )
    }, rownames = FALSE)


# --- Run tab UX: show running overlay + contextual banner ---
    output$run_overlay_ui <- renderUI({
      if (is.null(historical_api) || !is.list(historical_api) || is.null(historical_api$is_running)) return(NULL)
      if (!isTRUE(historical_api$is_running())) return(NULL)

      div(
        class = "plot-overlay",
        div(class = "spin-circle"),
        div(
          tags$strong("Running…"),
          tags$div(style = "font-size: 0.9em;", "Building hazards → panel → predictions → poverty")
        )
      )
    })
    output$run_banner_ui <- renderUI({
      if (!isTRUE(step1_contract$ok)) {
        return(tags$div(
          class = "alert alert-warning",
          paste0("Step 1 contract not satisfied (Step 2 v0). Missing: ", paste(step1_contract$missing, collapse = ", "))
        ))
      }

      r <- hist_run()
      if (is.null(r)) {
        return(tags$div(class = "alert alert-info",
                        "Choose a weather-year window and click “Run simulations” in the left sidebar. Results will appear here."))
      }

      # Prefer explicit phase errors when present (E → D → C → B)
      e <- NULL
      e_e <- phase_err(r, "phase_e"); if (nzchar(e_e)) e <- e_e
      if (is.null(e)) { e_d <- phase_err(r, "phase_d"); if (nzchar(e_d)) e <- e_d }
      if (is.null(e)) { e_c <- phase_err(r, "phase_c"); if (nzchar(e_c)) e <- e_c }
      if (is.null(e)) { e_b <- phase_err(r, "phase_b"); if (nzchar(e_b)) e <- e_b }

      if (!is.null(e) && nzchar(e)) {
        return(tags$div(class = "alert alert-danger", paste0("Run failed: ", e)))
      }

      pe <- get_phase(r, "phase_e")
      if (is.null(pe) || !isTRUE(pe$ok) || is.null(pe$data)) {
        return(tags$div(class = "alert alert-info",
                        "Choose a weather-year window and click “Run simulations” in the left sidebar. Results will appear here."))
      }

      last <- pe$last_run
      yrs  <- NULL
      pb <- get_phase(r, "phase_b")
      if (!is.null(pb) && isTRUE(pb$ok) && !is.null(pb$data$sim_year_range)) yrs <- pb$data$sim_year_range
      if (is.null(yrs) && !is.null(r$meta$hist_years)) yrs <- r$meta$hist_years
      yrs_txt <- if (!is.null(yrs) && length(yrs) == 2) paste0(yrs[[1]], "-", yrs[[2]]) else "(unknown)"

      chk <- hist_checks()
      chk_txt <- format_checks_short(chk)

      sc_txt <- format_scenario_spec_short(hist_scenario())

      tags$div(
        class = "alert alert-success",
        paste0("Last run: ", as.character(last),
               " | Scenario: ", sc_txt,
               " | Weather-year window: ", yrs_txt,
               " | Checks: ", chk_txt)
      )
    })


    mod_2_02_climate_server(
      "climate",
      step1 = step1_s2,
      pov_lines = pov_lines,
      varlist = varlist,
      board = board
    )


    phase_e_data <- reactive({
      r <- hist_run()
      pe <- get_phase(r, "phase_e")
      if (is.null(pe) || !isTRUE(pe$ok)) return(NULL)
      pe$data
    })
    phase_d_panel <- reactive({
      r <- hist_run()
      pd <- get_phase(r, "phase_d")
      if (is.null(pd) || !isTRUE(pd$ok)) return(NULL)
      pd$data$pred_panel
    })
    phase_b_lw <- reactive({
      r <- hist_run()
      pb <- get_phase(r, "phase_b")
      if (is.null(pb) || !isTRUE(pb$ok)) return(NULL)
      pb$data$loc_weather_sim
    })
    output$step2_status <- renderText({
      if (is.null(step1_s2)) return("Step 1 API not provided (wiring issue).")

      r <- hist_run()

      lw_n <- tryCatch({
        pb <- get_phase(r, "phase_b")
        lw <- if (!is.null(pb) && isTRUE(pb$ok)) pb$data$loc_weather_sim else NULL
        if (is.null(lw)) NA_integer_ else nrow(lw)
      }, error = function(e) NA_integer_)
      lw_ok <- isTRUE(!is.na(lw_n) && lw_n > 0)

      sp_n <- tryCatch({
        pc <- get_phase(r, "phase_c")
        sp <- if (!is.null(pc) && isTRUE(pc$ok)) pc$data$sim_panel else NULL
        if (is.null(sp)) NA_integer_ else nrow(sp)
      }, error = function(e) NA_integer_)
      sp_ok <- isTRUE(!is.na(sp_n) && sp_n > 0)

      chk <- hist_checks()
      sc_txt <- format_scenario_spec_short(hist_scenario())

      paste0(
        format_step2_status(step1 = step1_s2, board = board),
        "\nScenario: ", sc_txt,
        "\nChecks: ", format_checks_short(chk),
        "\nPhase B hazards built: ", if (lw_ok) "YES" else "NO",
        " | loc_weather_sim rows: ", lw_n,
        "\nPhase C sim panel built: ", if (sp_ok) "YES" else "NO",
        " | sim_panel rows: ", sp_n
      )
    })


    # --- Historical phase status outputs (shown in Diagnostics tab) ---
    output[["historical-phase_b_status"]] <- renderText({
      r <- hist_run()
      pb <- get_phase(r, "phase_b")
      if (is.null(pb)) return("Not run yet.")
      if (!isTRUE(pb$ok)) return(paste0("ERROR: ", pb$error %||% ""))
      lw <- pb$data$loc_weather_sim
      haz_cols <- grep("^haz_", names(lw), value = TRUE)
      paste0("OK | years=", paste(pb$data$sim_year_range, collapse = "-"),
             " | rows=", nrow(lw),
             " | haz_cols=", paste(haz_cols, collapse = ", "),
             " | last_run=", as.character(pb$last_run))
    })

    output[["historical-phase_c_status"]] <- renderText({
      r <- hist_run()
      pc <- get_phase(r, "phase_c")
      if (is.null(pc)) return("Not run yet.")
      if (!isTRUE(pc$ok)) return(paste0("ERROR: ", pc$error %||% ""))
      sp <- pc$data$sim_panel
      haz_cols <- grep("^haz_", names(sp), value = TRUE)
      paste0("OK | sim_panel rows=", nrow(sp),
             " | haz_cols=", paste(haz_cols, collapse = ", "),
             " | last_run=", as.character(pc$last_run))
    })

    output[["historical-phase_d_status"]] <- renderText({
      r <- hist_run()
      pd <- get_phase(r, "phase_d")
      if (is.null(pd)) return("Not run yet.")
      if (!isTRUE(pd$ok)) return(paste0("ERROR: ", pd$error %||% ""))
      pp <- pd$data$pred_panel
      paste0("OK | rows=", nrow(pp),
             " | NA preds=", sum(is.na(pp$pred)),
             " (", round(mean(is.na(pp$pred)), 4), ")",
             " | last_run=", as.character(pd$last_run))
    })

    output[["historical-phase_e_status"]] <- renderText({
      r <- hist_run()
      pe <- get_phase(r, "phase_e")
      if (is.null(pe)) return("Not run yet.")
      if (!isTRUE(pe$ok)) return(paste0("ERROR: ", pe$error %||% ""))
      ov <- pe$data$overall
      pov <- if ("pov_rate" %in% names(ov)) ov$pov_rate[[1]] else NA_real_
      paste0("OK | pov_rate=", round(pov, 4),
             " | last_run=", as.character(pe$last_run))
    })

    # --- Prediction diagnostics tables (Phase D) derived from the run object ---
    pred_diag <- reactive({
      pp <- phase_d_panel()
      if (is.null(pp) || !nrow(pp) || !"pred" %in% names(pp)) return(NULL)

      is_na <- is.na(pp$pred)
      r <- hist_run()
      pred_vars <- character()
      if (!is.null(r) && is.list(r) && !is.null(r$meta) && !is.null(r$meta$pred_vars)) {
        pred_vars <- as.character(r$meta$pred_vars)
        pred_vars <- intersect(pred_vars, names(pp))
      } else {
        # Pre-run fallback: infer from current Step 1 model
        mod <- tryCatch(step1_s2$final_model(), error = function(e) NULL)
        if (!is.null(mod)) {
          tt <- stats::terms(mod)
          pred_vars <- all.vars(stats::delete.response(tt))
          pred_vars <- intersect(pred_vars, names(pp))
        }
      }

      na_rate <- data.frame(
        n = nrow(pp),
        na = sum(is_na),
        na_rate = mean(is_na),
        stringsAsFactors = FALSE
      )

      # Summary stats for predictions
      is_bad <- is.na(pp$pred) | !is.finite(pp$pred)
      pred_ok <- pp$pred[!is_bad]
      has_ok <- length(pred_ok) > 0

      pred_summary <- data.frame(
        n        = nrow(pp),
        n_ok     = sum(!is_bad),
        bad      = sum(is_bad),
        bad_rate = mean(is_bad),
        mean   = if (has_ok) mean(pred_ok) else NA_real_,
        median = if (has_ok) stats::median(pred_ok) else NA_real_,
        sd     = if (has_ok) stats::sd(pred_ok) else NA_real_,
        iqr    = if (has_ok) stats::IQR(pred_ok) else NA_real_,
        min    = if (has_ok) min(pred_ok) else NA_real_,
        p01    = if (has_ok) as.numeric(stats::quantile(pred_ok, 0.01, na.rm = TRUE)) else NA_real_,
        p05    = if (has_ok) as.numeric(stats::quantile(pred_ok, 0.05, na.rm = TRUE)) else NA_real_,
        p95    = if (has_ok) as.numeric(stats::quantile(pred_ok, 0.95, na.rm = TRUE)) else NA_real_,
        p99    = if (has_ok) as.numeric(stats::quantile(pred_ok, 0.99, na.rm = TRUE)) else NA_real_,
        max    = if (has_ok) max(pred_ok) else NA_real_,
        stringsAsFactors = FALSE
      )

      na_by_year <- if ("sim_year" %in% names(pp)) {
        pp |>
          dplyr::summarise(
            n       = dplyr::n(),
            na      = sum(is.na(.data$pred)),
            na_rate = mean(is.na(.data$pred)),
            .by     = "sim_year"
          ) |>
          dplyr::arrange(dplyr::desc(.data$na_rate))
      } else NULL

      na_by_month <- if ("month" %in% names(pp)) {
        pp |>
          dplyr::summarise(
            n       = dplyr::n(),
            na      = sum(is.na(.data$pred)),
            na_rate = mean(is.na(.data$pred)),
            .by     = "month"
          )  |>
          dplyr::arrange(dplyr::desc(.data$na_rate))
      } else NULL

      na_by_loc <- if ("loc_id" %in% names(pp)) {
        pp |>
          dplyr::summarise(
            n       = dplyr::n(),
            na      = sum(is.na(.data$pred)),
            na_rate = mean(is.na(.data$pred)),
            .by     = "loc_id"
          )  |>
          dplyr::arrange(dplyr::desc(.data$na)) |>
          dplyr::slice_head(n = 20)
      } else NULL

      na_drivers <- NULL
      if (sum(is_na) > 0 && length(pred_vars) > 0) {
        na_drivers <- data.frame(
          var = pred_vars,
          share_na_among_na_preds = vapply(pred_vars, function(v) mean(is.na(pp[[v]][is_na])), numeric(1)),
          share_na_overall = vapply(pred_vars, function(v) mean(is.na(pp[[v]])), numeric(1)),
          stringsAsFactors = FALSE
        ) |>
          dplyr::arrange(dplyr::desc(.data$share_na_among_na_preds))
      }

      list(
        na_rate = na_rate,
        na_by_year = na_by_year,
        na_by_month = na_by_month,
        na_by_loc = na_by_loc,
        na_drivers = na_drivers,
        pred_summary = pred_summary
      )
    })

    output[["historical-pred_na_rate_tbl"]] <- renderTable({ d <- pred_diag(); if (is.null(d)) return(NULL); d$na_rate }, rownames = FALSE)
    output[["historical-pred_na_by_year_tbl"]] <- renderTable({ d <- pred_diag(); if (is.null(d)) return(NULL); d$na_by_year }, rownames = FALSE)
    output[["historical-pred_na_by_month_tbl"]] <- renderTable({ d <- pred_diag(); if (is.null(d)) return(NULL); d$na_by_month }, rownames = FALSE)
    output[["historical-pred_na_by_loc_tbl"]] <- renderTable({ d <- pred_diag(); if (is.null(d)) return(NULL); d$na_by_loc }, rownames = FALSE)
    output[["historical-pred_na_drivers_tbl"]] <- renderTable({ d <- pred_diag(); if (is.null(d)) return(NULL); d$na_drivers }, rownames = FALSE)
    output[["historical-pred_summary_tbl"]] <- renderTable({ d <- pred_diag(); if (is.null(d)) return(NULL); d$pred_summary }, rownames = FALSE)

    # --- Checks table (Phase artifacts) ---
    output[["historical-checks_tbl"]] <- renderTable({
      ck <- hist_checks()
      if (is.null(ck) || !length(ck)) return(NULL)
      data.frame(
        phase = names(ck),
        ok = vapply(ck, function(x) isTRUE(x$ok), logical(1)),
        msg = vapply(ck, function(x) x$msg %||% "", character(1)),
        stringsAsFactors = FALSE
      )
    }, rownames = FALSE)

    # --- Checks details UI ---
    output[["historical-checks_select_ui"]] <- renderUI({
      ck <- hist_checks()
      if (is.null(ck) || !length(ck)) return(NULL)
      shiny::selectInput(
        inputId = ns("checks_phase"),
        label = NULL,
        choices = names(ck),
        selected = names(ck)[[1]]
      )
    })

    output[["historical-checks_detail_txt"]] <- renderText({
      ck <- hist_checks()
      if (is.null(ck) || !length(ck)) return("No checks yet.")

      ph <- input$checks_phase
      if (is.null(ph) || !nzchar(ph) || !(ph %in% names(ck))) ph <- names(ck)[[1]]
      item <- ck[[ph]]
      if (is.null(item)) return(paste0(ph, ": (no data)"))

      ok <- isTRUE(item$ok)
      msg <- item$msg %||% ""
      det <- item$details

      det_txt <- if (is.null(det)) {
        "(no details)"
      } else {
        paste(utils::capture.output(str(det, max.level = 4)), collapse = "\n")
      }

      paste0(
        "phase: ", ph,
        "\nok: ", ok,
        "\nmsg: ", msg,
        "\n\ndetails:\n", det_txt
      )
    })

    #Phase E
    output$pov_overall_tbl <- renderTable({
      res <- phase_e_data()
      validate(need(!is.null(res), "No poverty results yet. Run simulations first."))
      res$overall
    }, rownames = FALSE)

    output$pov_by_year_tbl <- renderTable({
      res <- phase_e_data()
      validate(need(!is.null(res) && !is.null(res$by_year), "No by-year poverty results yet. Run simulations first."))
      res$by_year
    }, rownames = FALSE)

    output$pov_by_year_plot <- renderPlot({
      res <- phase_e_data()
      validate(need(!is.null(res) && !is.null(res$by_year) && nrow(res$by_year) > 0, "No by-year poverty results to plot yet."))

      d <- res$by_year
      ttl <- "Poverty rate ($3.00/day, 2021 PPP)"

      ggplot2::ggplot(d, ggplot2::aes(x = sim_year, y = pov_rate)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = "Simulated year", y = "Poverty rate", title = ttl)
    })


    output$pred_summary_tbl <- renderTable({
      d <- pred_diag()
      if (is.null(d)) return(NULL)
      d$pred_summary
    }, rownames = FALSE)

    #Poverty Line Plots
    output$pline_diag_tbl <- renderTable({
      res <- phase_e_data()
      if (is.null(res) || is.null(res$meta)) return(NULL)
      data.frame(
        pline_value = res$meta$pline_value %||% NA_real_,
        weight_col  = res$meta$weight_col %||% NA_character_,
        stringsAsFactors = FALSE
      )
    }, rownames = FALSE)

    phase_e_results <- phase_e_data


    #VISUALIZATION
    output$viz_haz_ui <- renderUI({
      lw <- phase_b_lw()
      validate(need(!is.null(lw) && nrow(lw) > 0, "Run Phase B (Build Hazards) first."))

      haz_cols <- grep("^haz_", names(lw), value = TRUE)
      validate(need(length(haz_cols) > 0, "No haz_* columns available yet."))

      shiny::selectInput(
        inputId = ns("viz_haz_var"),
        label   = "Weather / hazard variable",
        choices = haz_cols,
        selected = haz_cols[[1]]
      )
    })

    output$viz_metric_ui <- renderUI({
      res <- phase_e_data()
      choices <- c(
        "FGT0 (poverty rate)"   = "pov_rate",
        "FGT1 (poverty gap)"    = "fgt1",
        "FGT2 (poverty severity)" = "fgt2"
      )
      if (!is.null(res) && !is.null(res$by_year)) {
        keep <- unname(choices) %in% names(res$by_year)
        choices <- choices[keep]
      }
      if (!length(choices)) return(NULL)

      sel <- if ("pov_rate" %in% unname(choices)) "pov_rate" else unname(choices)[[1]]

      shiny::selectInput(
        inputId = ns("viz_metric_var"),
        label   = "Metric",
        choices = choices,
        selected = sel
      )
    })


    #Hazard Overlay
    output$viz_haz_overlay_plot <- renderPlot({
      h <- input$viz_haz_var
      validate(need(!is.null(h) && nzchar(h), "Select a hazard variable."))

      lw <- phase_b_lw()
      validate(need(!is.null(lw) && nrow(lw) > 0, "No Phase B hazards yet."))
      validate(need(h %in% names(lw), paste0("Missing ", h, " in loc_weather_sim.")))

      # Prefer run-tied model-fit hazard values when available (prevents UI drift after a run).
      fit_vals <- NULL
      r <- hist_run()
      if (!is.null(r) && is.list(r) && !is.null(r$meta) && !is.null(r$meta$fit_haz_values) &&
          is.list(r$meta$fit_haz_values) && (h %in% names(r$meta$fit_haz_values))) {
        fit_vals <- r$meta$fit_haz_values[[h]]
      } else {
        # Pre-run fallback: Step 1 unit-level survey_weather when available; otherwise model frame.
        sw <- tryCatch(step1_s2$survey_weather(), error = function(e) NULL)
        if (!is.null(sw) && nrow(sw) > 0 && (h %in% names(sw))) {
          fit_vals <- sw[[h]]
        } else {
          mod <- tryCatch(step1_s2$final_model(), error = function(e) NULL)
          mf  <- tryCatch(if (!is.null(mod)) stats::model.frame(mod) else NULL, error = function(e) NULL)
          if (!is.null(mf) && (h %in% names(mf))) {
            fit_vals <- mf[[h]]
          }
        }
      }

      if (!is.null(fit_vals)) {
        df <- dplyr::bind_rows(
          dplyr::transmute(lw, value = .data[[h]], source = "Historical"),
          tibble::tibble(value = fit_vals, source = "Model fit")
        )
      } else {
        df <- dplyr::transmute(lw, value = .data[[h]], source = "Historical")
      }

      ggplot2::ggplot(df, ggplot2::aes(x = value, fill = source)) +
        ggplot2::geom_density(alpha = 0.45, na.rm = TRUE) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = h, y = "Density", fill = NULL)
    })



    
    # --- Exceedance metric helpers (D2) ---
    current_metric <- reactive({
      m <- input$viz_metric_var
      if (is.null(m) || !nzchar(m)) "pov_rate" else m
    })

    metric_meta <- function(metric) {
      switch(
        metric,
        pov_rate = list(code = "pov_rate", short = "Poverty rate",   title = "Poverty risk curve vs return period"),
        fgt1     = list(code = "fgt1",     short = "Poverty gap",    title = "Poverty gap curve vs return period"),
        fgt2     = list(code = "fgt2",     short = "Poverty severity", title = "Poverty severity curve vs return period"),
        list(code = metric, short = metric, title = paste0(metric, " curve vs return period"))
      )
    }

    current_povline_label <- function() {
      r <- tryCatch(hist_run(), error = function(e) NULL)
      lbl <- NULL
      if (!is.null(r) && is.list(r) && !is.null(r$meta) && !is.null(r$meta$povline_label)) {
        cand <- as.character(r$meta$povline_label)
        if (length(cand) && !is.na(cand[[1]]) && nzchar(cand[[1]])) lbl <- cand[[1]]
      }
      if (is.null(lbl)) {
        pl <- tryCatch(get_active_poverty_line(step1_s2), error = function(e) NULL)
        if (!is.null(pl) && !is.null(pl$label) && nzchar(pl$label)) lbl <- pl$label
      }
      lbl %||% "NA"
    }

# Exceedance curve inputs: poverty rate by simulated year (single poverty line: $3/day 2021 PPP)
    viz_exceed_df <- reactive({
      res <- phase_e_data()
      validate(need(!is.null(res) && !is.null(res$by_year), "Run simulations first (Phase E results missing)."))

      d <- res$by_year

      # If a pline column exists (legacy), keep the first.
      if ("pline" %in% names(d)) {
        first_pl <- d$pline[[which(!is.na(d$pline))[1]]]
        d <- dplyr::filter(d, .data$pline == first_pl)
      }

      metric <- current_metric()
      validate(need(metric %in% names(d), paste0("Selected metric not available in Phase E by-year: ", metric)))

      d <- dplyr::filter(d, is.finite(.data[[metric]]))
      d
    })


    # Build smooth exceedance curves on a return-period axis (T = 1 / exceedance probability)
    viz_exceed_curve_df <- reactive({
      d <- viz_exceed_df()
      validate(need(nrow(d) > 0, "Nothing to plot yet."))

      metric <- current_metric()
      meta <- metric_meta(metric)

      expected <- mean(d[[metric]], na.rm = TRUE)

      # Return-period grid (log-spaced)
      max_T <- 100
      T_grid <- exp(seq(log(1), log(max_T), length.out = 200))
      probs  <- 1 - 1 / T_grid
      probs  <- pmax(pmin(probs, 0.999999), 0)

      q <- as.numeric(stats::quantile(d[[metric]], probs = probs, na.rm = TRUE, names = FALSE, type = 7))

      data.frame(
        return_period     = T_grid,
        exceed_prob       = 1 / T_grid,
        metric_code       = metric,
        metric            = q,
        metric_pct        = 100 * q,
        delta_pp          = 100 * (q - expected),
        expected_metric   = expected,
        expected_metric_pct = 100 * expected,
        stringsAsFactors  = FALSE
      )
    })


    output$viz_pov_exceed_plot <- renderPlot({
      df <- viz_exceed_curve_df()
      validate(need(nrow(df) > 0, "Nothing to plot yet."))

      metric <- current_metric()
      meta <- metric_meta(metric)

      ref_T <- c(5, 10, 20, 25, 50, 100)

      expected0 <- df$expected_metric_pct[[1]]  # scalar (percent)
      h <- input$viz_haz_var
      pl_lbl <- current_povline_label()

      haz_txt <- if (!is.null(h) && nzchar(h)) h else "(none)"
      subtitle_txt <- paste0("Metric: ", meta$short, " | Poverty line: ", pl_lbl, " | Hazard: ", haz_txt)

      ggplot2::ggplot(df, ggplot2::aes(x = .data$return_period, y = .data$delta_pp)) +
        ggplot2::geom_vline(xintercept = ref_T, linetype = "dotted", color = "grey70") +
        ggplot2::geom_hline(yintercept = 0, linetype = "dotted", color = "black") +
        ggplot2::geom_point(color = "lightblue", size = 0.6) +
        ggplot2::geom_line(color = "red", linewidth = 1) +
        ggplot2::scale_x_log10(breaks = c(1, 2, ref_T)) +
        ggplot2::scale_y_continuous(
          name = paste0("Change in ", meta$short, " (pp.)"),
          sec.axis = ggplot2::sec_axis(
            trans = ~ . + expected0,
            name  = paste0(meta$short, " (%)")
          )
        ) +
        ggplot2::labs(
          title = meta$title,
          subtitle = subtitle_txt,
          x = "Return period (years, log scale)",
          caption = paste0(
            "Left axis shows change relative to expected. Right axis converts to levels. ",
            "Expected = ", round(expected0, 1), "%."
          )
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.margin = ggplot2::margin(t = 10, r = 60, b = 10, l = 60),
          axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 8)),
          axis.title.y.right = ggplot2::element_text(margin = ggplot2::margin(l = 8)),
          plot.title    = ggplot2::element_text(face = "bold"),
          plot.subtitle = ggplot2::element_text(margin = ggplot2::margin(b = 6))
        )
    })


    output$viz_return_readoff_tbl <- renderTable({
      d <- viz_exceed_df()
      validate(need(nrow(d) > 0, "Run simulations first."))

      metric <- current_metric()
      meta <- metric_meta(metric)

      expected <- mean(d[[metric]], na.rm = TRUE)

      T <- c(5, 10, 20, 25, 50, 100)
      probs <- 1 - 1 / T
      probs <- pmax(pmin(probs, 0.999999), 0)

      q <- as.numeric(stats::quantile(d[[metric]], probs = probs, na.rm = TRUE, names = FALSE, type = 7))

      out <- data.frame(
        return_period_years = T,
        exceed_prob         = round(1 / T, 4),
        metric_pct          = round(100 * q, 1),
        delta_pp            = round(100 * (q - expected), 1),
        stringsAsFactors    = FALSE
      )
      names(out)[3] <- paste0(meta$short, " (%)")
      names(out)[4] <- paste0("Δ ", meta$short, " (pp.)")
      out
    }, rownames = FALSE, striped = TRUE, bordered = FALSE, spacing = "s")


################


    output$viz_welfare_ridges_plot <- renderPlot({
      pp <- phase_d_panel()
      h  <- input$viz_haz_var

      validate(need(!is.null(pp) && nrow(pp) > 0, "Run Phase D first (Predict welfare)."))
      validate(need("pred" %in% names(pp), "pred column not found on pred_panel."))
      validate(need(!is.null(h) && h %in% names(pp), "Selected hazard not found on pred_panel."))

      # downsample for speed if needed
      if (nrow(pp) > 400000) pp <- dplyr::slice_sample(pp, n = 400000)

      # yearly stats
      yr <- pp |>
        dplyr::group_by(.data$sim_year) |>
        dplyr::summarise(
          med_pred = stats::median(.data$pred, na.rm = TRUE),
          haz_mean = mean(.data[[h]], na.rm = TRUE),
          .groups = "drop"
        )

      # common x-grid
      rng <- range(pp$pred, na.rm = TRUE)
      xg  <- seq(rng[1], rng[2], length.out = 200)

      # build “ridge” lines
      ridge_df <- lapply(yr$sim_year, function(y) {
        v <- pp$pred[pp$sim_year == y]
        v <- v[is.finite(v)]
        if (length(v) < 50) return(NULL)

        den <- stats::density(v, n = 200, from = rng[1], to = rng[2])
        s <- yr[yr$sim_year == y, ]

        data.frame(
          sim_year = y,
          x = den$x,
          y = s$med_pred + 0.15 * den$y,  # 0.15 is a visual scaling knob
          med_pred = s$med_pred,
          haz_mean = s$haz_mean
        )
      }) |>
        dplyr::bind_rows()

      validate(need(nrow(ridge_df) > 0, "Not enough data to build ridge plot."))

      ggplot2::ggplot(ridge_df, ggplot2::aes(x = x, y = y, group = sim_year, color = haz_mean)) +
        ggplot2::geom_line(alpha = 0.85) +
        ggplot2::theme_minimal() +
        ggplot2::labs(
          x = "Welfare (pred scale)",
          y = "Median welfare (baseline + scaled density)",
          color = "Mean hazard"
        )
    })



    

# --- Step 2 API (for Step 3 Policy module) ---
# Expose the latest baseline run bundle and scenario registry so downstream modules can
# consume results without reaching back into Step 1 reactives.
list(
  run = hist_run,            # reactive() -> latest run bundle (or NULL)
  runs = runs_r,             # reactiveVal() -> named list of saved scenarios
  scenario_ids = scenario_ids # reactive() -> sorted scenario IDs
)

# Later, this server will accept Step 1 exports as arguments.
  })
}
