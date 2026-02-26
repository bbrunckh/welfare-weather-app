#DRK Version 20260220 1900

mod_2_01_historical_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Historical simulation: choose a historical weather-year window, then run phases B–E."),

    shiny::sliderInput(
      inputId = ns("hist_years"),
      label   = "Period defining the distribution of weather (years)",
      min     = 1950,
      max     = 2024,
      value   = c(1990, 2024),
      sep     = ""
    ),

    shiny::actionButton(ns("run_all"), "Run simulations", class = "btn-primary"),

    tags$hr(),
    strong("Status:"),
    verbatimTextOutput(ns("run_status_short"))
  )
}


mod_2_01_historical_server <- function(id, step1 = NULL, pov_lines = NULL, varlist = NULL, board = NULL) {
  moduleServer(id, function(input, output, session) {

    # ---- Run state ----
is_running_r <- reactiveVal(FALSE)

# ---- Step 2 run object (single source of truth) ----
run_r <- reactiveVal(NULL)

`%||%` <- function(a, b) if (!is.null(a)) a else b

phase_get <- function(name) {
  r <- run_r()
  if (is.null(r)) return(NULL)
  r[[name]]
}
phase_err <- function(name) {
  p <- phase_get(name)
  if (is.null(p)) return("")
  p$error %||% ""
}
phase_last <- function(name) {
  p <- phase_get(name)
  if (is.null(p)) return(NULL)
  p$last_run
}

# ---- Phase read-only "state" (derived from run_r) ----
phase_b_error_r   <- reactive(phase_err("phase_b"))
phase_b_last_run  <- reactive(phase_last("phase_b"))
built_haz_r       <- reactive({ p <- phase_get("phase_b"); if (is.null(p) || !isTRUE(p$ok)) NULL else p$data })

phase_c_error_r   <- reactive(phase_err("phase_c"))
phase_c_last_run  <- reactive(phase_last("phase_c"))
sim_panel_r       <- reactive({ p <- phase_get("phase_c"); if (is.null(p) || !isTRUE(p$ok)) NULL else p$data$sim_panel })

phase_d_error_r   <- reactive(phase_err("phase_d"))
phase_d_last_run  <- reactive(phase_last("phase_d"))
pred_panel_r      <- reactive({ p <- phase_get("phase_d"); if (is.null(p) || !isTRUE(p$ok)) NULL else p$data$pred_panel })

phase_e_error_r   <- reactive(phase_err("phase_e"))
phase_e_last_run  <- reactive(phase_last("phase_e"))
poverty_r         <- reactive({ p <- phase_get("phase_e"); if (is.null(p) || !isTRUE(p$ok)) NULL else p$data })

# Local accessors for Phase B outputs (needed inside this module)
loc_weather_sim <- reactive({
  x <- built_haz_r()
  if (is.null(x)) NULL else x$loc_weather_sim
})

sim_year_range <- reactive({
  x <- built_haz_r()
  if (is.null(x)) NULL else x$sim_year_range
})

    # Initial (Phase B) - Output (Small status readout shown in sidebar)
    output$phase_b_status <- renderText({
      if (!is.null(phase_b_error_r()) && nzchar(phase_b_error_r())) {
        return(paste0("ERROR: ", phase_b_error_r()))
      }
      obj <- built_haz_r()
      if (is.null(obj)) return("Not run yet.")
      paste0(
        "OK | years=", obj$sim_year_range[1], "-", obj$sim_year_range[2],
        " | loc_month rows=", nrow(obj$loc_weather_sim),
        " | haz cols=", paste(grep("^haz_", names(obj$loc_weather_sim), value = TRUE), collapse = ", "),
        " | last_run=", as.character(phase_b_last_run())
      )
    })

    #Phase B Event
    do_phase_b <- function(notify = TRUE) {
  tryCatch({
    if (is.null(board)) stop("Phase B: board is NULL. Pass a pins board from app_server (board_folder(file.path(data_dir, 'pins'))).")
    brd <- board
    res_b <- phase_b_historical_core(
      step1 = step1,
      board = brd,
      hist_years = input$hist_years
    )

    run <- run_r() %||% list(meta = list(), checks = list())
    run$meta$hist_years <- input$hist_years
    # Record boundary spec + board path for reproducibility/diagnostics
    run$meta$step1_spec_summary <- tryCatch(format_step1_spec_summary(step1), error = function(e) NA_character_)
    run$meta$board_path <- tryCatch(board_local_path(brd), error = function(e) NA_character_)

    # Store full Step1Spec snapshot + poverty line label in run meta (so UI does not drift if Step 1 inputs change)
    run$meta$step1_spec <- tryCatch(step1$step1_spec(), error = function(e) NULL)
    pl <- tryCatch(get_active_poverty_line(step1), error = function(e) NULL)
    run$meta$povline_label <- if (is.null(pl)) NA_character_ else (pl$label %||% NA_character_)
    run$meta$povline_level <- if (is.null(pl)) NA_real_ else (pl$level %||% NA_real_)

    # Minimal ScenarioSpec (stored in meta; used for later comparisons)
    run$meta$scenario_spec <- make_scenario_spec(
      type = "historical",
      id = paste0("hist_", input$hist_years[[1]], "_", input$hist_years[[2]]),
      label = "Historical weather",
      hazard_source = "pins_folder",
      years = input$hist_years,
      weather_pins = res_b$weather_pins %||% NULL
    )

    run$phase_b <- list(ok = TRUE, data = res_b, error = "", last_run = Sys.time())
    run$checks$phase_b <- validate_phase_b_artifact(res_b)

    # Reset downstream phases whenever hazards change
    run$phase_c <- NULL
    run$phase_d <- NULL
    run$phase_e <- NULL
    run$checks$phase_c <- NULL
    run$checks$phase_d <- NULL
    run$checks$phase_e <- NULL

    run_r(run)

    if (notify) shiny::showNotification("Phase B complete: hazards built.", type = "message")
    TRUE
  }, error = function(e) {
    run <- run_r() %||% list(meta = list(), checks = list())
    run$meta$hist_years <- input$hist_years

    # Minimal ScenarioSpec (still store so diagnostics can show intended run)
    run$meta$scenario_spec <- make_scenario_spec(
      type = "historical",
      id = paste0("hist_", input$hist_years[[1]], "_", input$hist_years[[2]]),
      label = "Historical weather",
      hazard_source = "pins_folder",
      years = input$hist_years
    )

    run$phase_b <- list(ok = FALSE, data = NULL, error = e$message, last_run = Sys.time())
    run$checks$phase_b <- mk_check(FALSE, paste0("FAIL | ", e$message))

    run$phase_c <- NULL
    run$phase_d <- NULL
    run$phase_e <- NULL
    run$checks$phase_c <- NULL
    run$checks$phase_d <- NULL
    run$checks$phase_e <- NULL

    run_r(run)

    if (notify) shiny::showNotification(paste0("Phase B failed: ", e$message), type = "error", duration = NULL)
    FALSE
  })
    }


    #Phase C - Output
    output$phase_c_status <- renderText({
      if (nzchar(phase_c_error_r())) return(paste0("ERROR: ", phase_c_error_r()))
      sp <- sim_panel_r()
      if (is.null(sp)) return("Not run yet.")
      haz_cols <- grep("^haz_", names(sp), value = TRUE)
      miss <- if (length(haz_cols)) mean(is.na(sp[[haz_cols[1]]])) else NA_real_
      paste0(
        "OK | sim_panel rows=", nrow(sp),
        " | haz cols=", paste(haz_cols, collapse = ", "),
        " | NA rate (first haz)=", round(miss, 4),
        " | last_run=", as.character(phase_c_last_run())
      )
    })

    #Phase C - Event
    do_phase_c <- function(notify = TRUE) {
  tryCatch({
    b <- built_haz_r()
        if (is.null(b)) stop("Phase B must be completed before Phase C.")
    res_c <- phase_c_historical_core(step1 = step1, built_haz = b)

    run <- run_r() %||% list(meta = list(), checks = list())
    run$phase_c <- list(ok = TRUE, data = res_c, error = "", last_run = Sys.time())
    mdl <- tryCatch(step1$final_model(), error = function(e) NULL)
    run$checks$phase_c <- validate_phase_c_artifact(res_c, model = mdl)
    # Snapshot model variables used (for run-tied diagnostics/plots; avoids reaching back into Step 1 after a run)
    run$meta$pred_vars <- tryCatch({
      if (is.null(mdl)) character()
      else {
        tt <- stats::terms(mdl)
        all.vars(stats::delete.response(tt))
      }
    }, error = function(e) character())

    # Snapshot hazard values used to fit the model (for the hazard overlay plot)
    run$meta$fit_haz_values <- tryCatch({
      hv <- list()
      # Prefer unit-level survey_weather if available (may include haz cols)
      sw_fit <- tryCatch(step1$survey_weather(), error = function(e) NULL)
      if (!is.null(sw_fit) && nrow(sw_fit) > 0) {
        haz_cols <- grep("^haz_", names(sw_fit), value = TRUE)
        for (hc in haz_cols) hv[[hc]] <- sw_fit[[hc]]
      } else {
        mf <- tryCatch(if (!is.null(mdl)) stats::model.frame(mdl) else NULL, error = function(e) NULL)
        if (!is.null(mf) && nrow(mf) > 0) {
          haz_cols <- grep("^haz_", names(mf), value = TRUE)
          for (hc in haz_cols) hv[[hc]] <- mf[[hc]]
        }
      }
      hv
    }, error = function(e) list())


    # Reset downstream phases whenever the sim panel changes
    run$phase_d <- NULL
    run$phase_e <- NULL
    run$checks$phase_d <- NULL
    run$checks$phase_e <- NULL

    run_r(run)

    if (notify) shiny::showNotification("Phase C complete: simulation panel built.", type = "message")
    TRUE
  }, error = function(e) {
    run <- run_r() %||% list(meta = list(), checks = list())
    run$phase_c <- list(ok = FALSE, data = NULL, error = e$message, last_run = Sys.time())
    run$checks$phase_c <- mk_check(FALSE, paste0("FAIL | ", e$message))

    run$phase_d <- NULL
    run$phase_e <- NULL
    run$checks$phase_d <- NULL
    run$checks$phase_e <- NULL
    run_r(run)

    if (notify) shiny::showNotification(paste0("Phase C failed: ", e$message), type = "error", duration = NULL)
    FALSE
  })
    }


    #Phase D
    output$phase_d_status <- renderText({
      if (nzchar(phase_d_error_r())) return(paste0("ERROR: ", phase_d_error_r()))
      pp <- pred_panel_r()
      if (is.null(pp)) return("Not run yet.")
      paste0(
        "OK | rows=", nrow(pp),
        " | NA preds=", sum(is.na(pp$pred)),
        " (", round(mean(is.na(pp$pred)), 4), ")",
        " | last_run=", as.character(phase_d_last_run())
      )
    })

    #Output Subgroup - NA table analysis (From Phase D)
    output$pred_na_rate_tbl <- renderTable({
      d <- pred_diag()
      if (is.null(d)) return(NULL)
      d$na_rate
    }, rownames = FALSE)

    output$pred_na_by_year_tbl <- renderTable({
      d <- pred_diag()
      if (is.null(d)) return(NULL)
      d$na_by_year
    }, rownames = FALSE)

    output$pred_na_by_month_tbl <- renderTable({
      d <- pred_diag()
      if (is.null(d)) return(NULL)
      d$na_by_month
    }, rownames = FALSE)

    output$pred_na_by_loc_tbl <- renderTable({
      d <- pred_diag()
      if (is.null(d)) return(NULL)
      d$na_by_loc
    }, rownames = FALSE)

    output$pred_na_drivers_tbl <- renderTable({
      d <- pred_diag()
      if (is.null(d)) return(NULL)
      d$na_drivers
    }, rownames = FALSE)

    #My addition
    output$pred_summary_tbl <- renderTable({
      d <- pred_diag()
      if (is.null(d)) return(NULL)
      d$pred_summary
    }, rownames = FALSE)



    #Phase D Event
    do_phase_d <- function(notify = TRUE) {
  tryCatch({
    sp <- sim_panel_r()
        if (is.null(sp)) stop("Phase C must be completed before Phase D.")
    res_d <- phase_d_historical_core(step1 = step1, sim_panel = sp)

    run <- run_r() %||% list(meta = list(), checks = list())
    run$phase_d <- list(ok = TRUE, data = res_d, error = "", last_run = Sys.time())
    run$checks$phase_d <- validate_phase_d_artifact(res_d)

    # Reset downstream phase E whenever predictions change
    run$phase_e <- NULL
    run$checks$phase_e <- NULL
    run_r(run)

    if (notify) shiny::showNotification("Phase D complete: predictions built.", type = "message")
    TRUE
  }, error = function(e) {
    run <- run_r() %||% list(meta = list(), checks = list())
    run$phase_d <- list(ok = FALSE, data = NULL, error = e$message, last_run = Sys.time())
    run$checks$phase_d <- mk_check(FALSE, paste0("FAIL | ", e$message))

    run$phase_e <- NULL
    run$checks$phase_e <- NULL
    run_r(run)

    if (notify) shiny::showNotification(paste0("Phase D failed: ", e$message), type = "error", duration = NULL)
    FALSE
  })
}

    #Diagnostics for Prediction/NA
    pred_diag <- reactive({
      pp <- pred_panel_r()
      if (is.null(pp) || !nrow(pp) || !"pred" %in% names(pp)) return(NULL)

      is_na <- is.na(pp$pred)

      # Find required predictors from model
      mod <- step1$final_model()
      tt <- stats::terms(mod)
      pred_vars <- all.vars(stats::delete.response(tt))
      pred_vars <- intersect(pred_vars, names(pp))

      # NA rate table
      na_rate <- data.frame(
        n = nrow(pp),
        na = sum(is_na),
        na_rate = mean(is_na),
        stringsAsFactors = FALSE
      )

      #Summary rate table
      # Summary stats for predictions (ignore NA / non-finite)
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

      # By sim year
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

      # By interview month (if present)
      na_by_month <- if ("int_month" %in% names(pp)) {
        pp |>
          dplyr::summarise(
            n       = dplyr::n(),
            na      = sum(is.na(.data$pred)),
            na_rate = mean(is.na(.data$pred)),
            .by     = "int_month"
          )  |>
          dplyr::arrange(dplyr::desc(.data$na_rate))
      } else NULL

      # By location
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

      # What’s missing among NA prediction rows?
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


      #Overall


      list(
        na_rate = na_rate,
        na_by_year = na_by_year,
        na_by_month = na_by_month,
        na_by_loc = na_by_loc,
        na_drivers = na_drivers,

        pred_summary = pred_summary
      )
    })

    plines_levels_r <- reactiveVal(NULL)


    # Output - Phase E:
    output$run_status_short <- renderText({
      if (isTRUE(is_running_r())) return("Running…")
      # Prefer the most recent error if present
      if (nzchar(phase_e_error_r())) return(paste0("ERROR (Phase E): ", phase_e_error_r()))
      if (nzchar(phase_d_error_r())) return(paste0("ERROR (Phase D): ", phase_d_error_r()))
      if (nzchar(phase_c_error_r())) return(paste0("ERROR (Phase C): ", phase_c_error_r()))
      if (nzchar(phase_b_error_r())) return(paste0("ERROR (Phase B): ", phase_b_error_r()))

      last <- phase_e_last_run()
      if (!is.null(last)) {
        yrs <- sim_year_range()
        yrs_txt <- if (!is.null(yrs) && length(yrs) == 2) paste0(yrs[[1]], "-", yrs[[2]]) else "(years not set)"
        return(paste0("OK | years=", yrs_txt, " | last_run=", as.character(last)))
      }
      "Not run yet."
    })

    #Output
    output$phase_e_status <- renderText({
      if (nzchar(phase_e_error_r())) return(paste0("ERROR: ", phase_e_error_r()))
      res <- poverty_r()
      if (is.null(res)) return("Not run yet.")
      pl_now <- plines_levels_r()
      paste0(
        "OK | kind=", res$meta$model_kind,
        " | coverage_w=", round(res$meta$coverage_weight_overall, 4),
        " | last_run=", as.character(phase_e_last_run()),
        " | Pline values = ", as.character(pl_now)
      )
    })

    #Phase E - Event
    do_phase_e <- function(notify = TRUE) {
  tryCatch({
    pp <- pred_panel_r()
        if (is.null(pp)) stop("Phase D must be completed before Phase E.")
    pline <- get_active_poverty_line(step1)
    res_e <- phase_e_historical_core(step1 = step1, pred_panel = pp, pline_value = pline$model, pline_level = pline$level, pred_scale = pline$pred_scale)
    # Enrich meta with the level poverty line (for display) and model scale info
    res_e$meta$pline_level <- pline$level
    res_e$meta$pline_label <- pline$label
    res_e$meta$pred_scale  <- pline$pred_scale

    run <- run_r() %||% list(meta = list(), checks = list())
    run$phase_e <- list(ok = TRUE, data = res_e, error = "", last_run = Sys.time())
    run$checks$phase_e <- validate_phase_e_artifact(res_e)
    run_r(run)

    if (notify) shiny::showNotification("Phase E complete: poverty computed.", type = "message")
    TRUE
  }, error = function(e) {
    run <- run_r() %||% list(meta = list(), checks = list())
    run$phase_e <- list(ok = FALSE, data = NULL, error = e$message, last_run = Sys.time())
    run$checks$phase_e <- mk_check(FALSE, paste0("FAIL | ", e$message))
    run_r(run)

    if (notify) shiny::showNotification(paste0("Phase E failed: ", e$message), type = "error", duration = NULL)
    FALSE
  })
}


    #Run All
    observeEvent(input$run_all, {
      # Reset run object (clears errors + downstream state)
      run_r(NULL)

      is_running_r(TRUE)
      on.exit(is_running_r(FALSE), add = TRUE)

      withProgress(message = "Running historical simulation (B–E)", value = 0, {
        incProgress(0.05, detail = "Phase B: building hazards")
        ok <- do_phase_b(notify = FALSE)
        if (!isTRUE(ok)) return(invisible(NULL))

        incProgress(0.35, detail = "Phase C: building simulation panel")
        ok <- do_phase_c(notify = FALSE)
        if (!isTRUE(ok)) return(invisible(NULL))

        incProgress(0.65, detail = "Phase D: predicting welfare")
        ok <- do_phase_d(notify = FALSE)
        if (!isTRUE(ok)) return(invisible(NULL))

        incProgress(0.85, detail = "Phase E: computing poverty")
        ok <- do_phase_e(notify = FALSE)
        if (!isTRUE(ok)) return(invisible(NULL))

        incProgress(1)
      })

      shiny::showNotification("Run complete: phases B–E finished.", type = "message", duration = 6)
    }, ignoreInit = TRUE)



# ---- Module API (so Step 2 can consume Phase outputs) ----
list(

  # Whole run object (useful for diagnostics/comparisons later)
  run = reactive(run_r()),
  checks = reactive({ r <- run_r(); if (is.null(r)) NULL else r$checks }),

  #Phase B (building Hazards)
  built_haz        = built_haz_r,
  weather_data_sim = reactive({ x <- built_haz_r(); if (is.null(x)) NULL else x$weather_data_sim }),
  loc_weather_sim  = loc_weather_sim,
  sim_year_range   = sim_year_range,
  phase_b_error    = phase_b_error_r,
  phase_b_last_run = phase_b_last_run,

  #Phase C (Building Simulations)
  sim_panel        = sim_panel_r,
  phase_c_error    = phase_c_error_r,
  phase_c_last_run = phase_c_last_run,

  #Phase D (Predicting Outcome)
  pred_panel       = pred_panel_r,
  phase_d_error    = phase_d_error_r,
  phase_d_last_run = phase_d_last_run,

  #Phase E (Outcome)
  poverty_results  = poverty_r,
  phase_e_error    = phase_e_error_r,
  phase_e_last_run = phase_e_last_run,

  # Run state (for UI spinners/overlays)
  is_running       = reactive(is_running_r())
)
  })
}
