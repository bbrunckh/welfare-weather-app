#DRK Version 20260212 1500

mod_2_01_historical_ui <- function(id) {
  ns <- NS(id)
  tagList(

    #Phase B Block
    p("Phase B: Build loc-month hazards over a historical window (for simulation)."),

    shiny::sliderInput(
      inputId = ns("hist_years"),
      label   = "Period defining the distribution of weather (years)",
      min     = 1950,
      max     = 2024,
      value   = c(1990, 2024),
      sep     = ""
    ),

    shiny::actionButton(ns("build_hazards"), "Build Hazards (Phase B)", class = "btn-primary"),

    tags$hr(),
    strong("Phase B status:"),
    verbatimTextOutput(ns("phase_b_status")),

    #Phase C Black
    tags$hr(),
    shiny::actionButton(ns("build_panel"), "Build simulation panel (Phase C)", class = "btn-primary"),
    tags$br(), tags$br(),
    strong("Phase C status:"),
    verbatimTextOutput(ns("phase_c_status")),

    #Phase D Block (Simulate Welfare with Hazards)
    tags$hr(),
    shiny::actionButton(ns("predict_welfare"), "Predict welfare (Phase D)", class = "btn-primary"),
    tags$br(), tags$br(),
    strong("Phase D status:"),
    verbatimTextOutput(ns("phase_d_status")),

    #Phase E Block (Output)
    tags$hr(),
    shiny::actionButton(ns("compute_poverty"), "Compute poverty (Phase E)", class = "btn-primary"),
    tags$br(), tags$br(),
    strong("Phase E status:"),
    verbatimTextOutput(ns("phase_e_status"))

  )
}

mod_2_01_historical_server <- function(id, step1 = NULL, pov_lines = NULL, varlist = NULL, board = NULL) {
  moduleServer(id, function(input, output, session) {

    # ---- Phase B state ----
    phase_b_error_r   <- reactiveVal("")
    phase_b_last_run  <- reactiveVal(NULL)
    built_haz_r       <- reactiveVal(NULL)

    # Local accessors for Phase B outputs (needed inside this module)
    loc_weather_sim <- reactive({
      x <- built_haz_r()
      if (is.null(x)) NULL else x$loc_weather_sim
    })

    sim_year_range <- reactive({
      x <- built_haz_r()
      if (is.null(x)) NULL else x$sim_year_range
    })


    # Small status readout (shown in sidebar)
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
    observeEvent(input$build_hazards, {
      phase_b_error_r("")
      built_haz_r(NULL)

      tryCatch({
        # --- Basic contract checks ---
        if (is.null(step1)) stop("Step 1 API missing (wiring).")

        # Required Step 1 exports for Phase B
        files <- step1$survey_data_files()
        if (is.null(files) || !length(files)) stop("step1$survey_data_files() returned 0 files.")

        survey_h3 <- step1$weather_api$survey_h3()
        if (is.null(survey_h3) || !nrow(survey_h3)) stop("step1$weather_api$survey_h3() is NULL/empty.")

        haz_spec <- step1$haz_spec()
        if (is.null(haz_spec) || !nrow(haz_spec)) stop("step1$haz_spec() is NULL/empty.")

        # Board (may be a reactive)
        brd <- if (is.function(board)) board() else board
        if (is.null(brd)) stop("board is NULL (wiring).")

        # Historical window
        yrs <- input$hist_years
        if (length(yrs) != 2) stop("hist_years not set.")
        y0 <- as.integer(yrs[1])
        y1 <- as.integer(yrs[2])
        if (anyNA(c(y0, y1)) || y0 > y1) stop("Invalid year range in hist_years.")

        # --- Load historical weather pins (same naming logic as Step 1) ---
        pin_names <- derive_weather_pin_names(files)
        if (!length(pin_names)) stop("derive_weather_pin_names() produced 0 pin names.")

        local_paths <- lapply(pin_names, function(pin) {
          tryCatch(pins::pin_download(brd, pin), error = function(e) NULL)
        })
        local_paths <- Filter(Negate(is.null), local_paths)
        if (!length(local_paths)) stop("Could not download any weather pins (check board + pin names).")

        weather <- read_parquet_duckdb(unlist(local_paths))
        if (is.null(weather) || !nrow(weather)) stop("Downloaded weather data is empty.")

        # Standardize timestamp
        if (!inherits(weather$timestamp, "Date")) {
          weather$timestamp <- as.Date(weather$timestamp)
        }

        # Use haz_spec varnames as required weather columns
        wvars <- unique(haz_spec$varname)
        miss_wvars <- setdiff(wvars, names(weather))
        if (length(miss_wvars)) {
          stop(paste0("Weather data missing vars needed by haz_spec: ", paste(miss_wvars, collapse = ", ")))
        }

        # Filter to relevant H3s + year range, keep only necessary columns
        weather <- weather |>
          dplyr::select(.data$h3_6, .data$timestamp, dplyr::all_of(wvars)) |>
          dplyr::filter(.data$h3_6 %in% survey_h3$h3_6) |>
          dplyr::filter(lubridate::year(.data$timestamp) >= y0,
                        lubridate::year(.data$timestamp) <= y1) |>
          dplyr::distinct()

        if (!nrow(weather)) stop("Weather is empty after filtering to survey H3s + selected years.")

        # --- Build hazards at H3 level, then aggregate to loc_id ---
        h3_haz <- build_h3_hazards(weather, haz_spec)
        if (is.null(h3_haz) || !nrow(h3_haz)) stop("build_h3_hazards() returned 0 rows.")

        loc_haz <- aggregate_h3_to_loc(survey_h3, h3_haz)
        if (is.null(loc_haz) || !nrow(loc_haz)) stop("aggregate_h3_to_loc() returned 0 rows.")

        keys <- intersect(c("code", "loc_id", "timestamp"), names(loc_haz))
        key_check <- check_unique_key(loc_haz, keys = keys, context = "Phase B loc_weather_sim")
        if (!isTRUE(key_check$ok)) stop(paste0("Phase B failed unique key check: ", key_check$msg))
        #NOTE:if I change for loc_weather_sim to use year/mon instead of timestamp, need to change keys to that.

        #NA table for missing columns
        haz_cols <- grep("^haz_", names(loc_haz), value = TRUE)
        cov <- summarize_missingness(loc_haz, cols = haz_cols)
        if (wise_debug_enabled()) print(cov)

        built_haz_r(list(
          weather_data_sim = weather,
          h3_weather_sim   = h3_haz,
          loc_weather_sim  = loc_haz,
          sim_year_range   = c(y0, y1),
          weather_pins     = pin_names
        ))

        phase_b_last_run(Sys.time())
        shiny::showNotification("Phase B complete: historical hazards built.", type = "message", duration = 4)

      }, error = function(e) {
        msg <- conditionMessage(e)
        if (is.null(msg) || !nzchar(msg)) msg <- "Phase B failed (silent error)."
        phase_b_error_r(msg)
        shiny::showNotification(paste0("Phase B failed: ", msg), type = "error", duration = 12)
      })
    }, ignoreInit = TRUE)



    #Phase C state
    phase_c_error_r  <- reactiveVal("")
    phase_c_last_run <- reactiveVal(NULL)
    sim_panel_r      <- reactiveVal(NULL)

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

    observeEvent(input$build_panel, {
      phase_c_error_r("")
      sim_panel_r(NULL)

      tryCatch({
        # Require wiring + Phase B hazards
        if (is.null(step1)) stop("Step 1 API missing (wiring).")
        lw <- loc_weather_sim()
        if (is.null(lw) || !nrow(lw)) stop("Phase B hazards missing. Run Phase B first.")

        yrs <- sim_year_range()
        if (is.null(yrs) || length(yrs) != 2) stop("sim_year_range() missing.")
        sim_years <- seq.int(as.integer(yrs[1]), as.integer(yrs[2]))

        sw <- step1$survey_weather()
        if (is.null(sw) || !nrow(sw)) stop("step1$survey_weather() is empty.")

        # --- Identify key columns robustly ---
        # household key
        hh_candidates <- c("hh_key", "hhid", "household_id", "hh_id", "case_id")
        hh_col <- hh_candidates[hh_candidates %in% names(sw)][1]
        if (is.na(hh_col) || !nzchar(hh_col)) {
          sw$hh_key__tmp <- seq_len(nrow(sw))
          hh_col <- "hh_key__tmp"
        }

        # location id
        if (!"loc_id" %in% names(sw)) stop("survey_weather missing loc_id.")
        sw$loc_id <- as.character(sw$loc_id)

        # interview timestamp (monthly)
        if (!"timestamp" %in% names(sw)) stop("survey_weather missing timestamp.")
        sw$timestamp <- as.Date(sw$timestamp)

        mod <- step1$final_model()
        if (is.null(mod)) stop("final_model() is NULL.")

        tt <- stats::terms(mod)
        pred_vars <- all.vars(stats::delete.response(tt))  # predictors only

        # Do NOT carry haz_* from sw; we join simulated haz_* from lw
        haz_cols_sw <- grep("^haz_", names(sw), value = TRUE)
        pred_vars   <- setdiff(pred_vars, haz_cols_sw)

        # Keep only predictors that exist in sw
        pred_vars <- intersect(pred_vars, names(sw))

        # Always keep core keys
        vars_to_keep <- unique(c(hh_col, "loc_id", "timestamp", pred_vars))


        base <- sw |>
          dplyr::arrange(.data$timestamp) |>

          dplyr::mutate(.miss = rowSums(is.na(dplyr::across(dplyr::all_of(vars_to_keep))))) |>
          dplyr::group_by(.data[[hh_col]]) |>
          dplyr::slice_min(.data$.miss, with_ties = FALSE) |>
          dplyr::ungroup() |>
          dplyr::select(-.data$.miss) |>

          dplyr::mutate(int_month = lubridate::month(.data$timestamp)) |>
          dplyr::select(dplyr::all_of(vars_to_keep), .data$int_month)

        # Expand households across simulated years, month-matched
        panel <- tidyr::crossing(base, sim_year = sim_years) |>
          dplyr::mutate(
            sim_timestamp = as.Date(sprintf("%04d-%02d-01", .data$sim_year, .data$int_month))
          )

        # Join hazards (loc_id + timestamp)
        lw2 <- lw |>
          dplyr::mutate(loc_id = as.character(.data$loc_id),
                        timestamp = as.Date(.data$timestamp))

        panel <- dplyr::left_join(
          panel,
          lw2,
          by = c("loc_id" = "loc_id", "sim_timestamp" = "timestamp")
        )

        # Diagnostics: unique key + missingness
        key_check <- check_unique_key(panel, keys = c(hh_col, "sim_year"), context = "Phase C sim_panel")
        if (!isTRUE(key_check$ok)) stop(paste0("Phase C failed unique key check: ", key_check$msg))

        haz_cols <- grep("^haz_", names(panel), value = TRUE)
        if (!length(haz_cols)) stop("Phase C: no haz_* columns present after join.")
        # If NA rate is crazy high, flag loudly
        na_rate <- mean(is.na(panel[[haz_cols[1]]]))
        if (is.nan(na_rate) || na_rate > 0.25) {
          warning(paste0("High NA rate after hazard join (", round(na_rate, 3), "). Check loc_id/month alignment."))
        }

        sim_panel_r(panel)
        phase_c_last_run(Sys.time())
        showNotification("Phase C complete: simulation panel built.", type = "message", duration = 4)

      }, error = function(e) {
        phase_c_error_r(conditionMessage(e))
        showNotification(paste0("Phase C failed: ", conditionMessage(e)), type = "error", duration = 12)
      })
    }, ignoreInit = TRUE)


    #Phase D state (predicting with simulated hazards)

    #State
    phase_d_error_r  <- reactiveVal("")
    phase_d_last_run <- reactiveVal(NULL)
    pred_panel_r     <- reactiveVal(NULL)



    #Output
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

    #Output Subgroup - NA table analysis
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



    #Event
    observeEvent(input$predict_welfare, {
      phase_d_error_r("")
      pred_panel_r(NULL)

      tryCatch({
        if (is.null(step1)) stop("Step 1 API missing.")
        sp <- sim_panel_r()
        if (is.null(sp) || !nrow(sp)) stop("Phase C sim_panel missing. Run Phase C first.")

        mod <- step1$final_model()
        if (is.null(mod)) stop("final_model() is NULL.")

        # Figure out required predictors from model terms
        tt <- stats::terms(mod)
        pred_vars <- all.vars(stats::delete.response(tt))
        missing_vars <- setdiff(pred_vars, names(sp))
        if (length(missing_vars)) {
          stop(paste0(
            "sim_panel is missing predictors required for prediction: ",
            paste(missing_vars, collapse = ", "),
            " | Fix by carrying these from survey_weather into Phase C base."
          ))
        }

        # Coerce factor levels to match training (xlevels) if present
        if (!is.null(mod$xlevels) && length(mod$xlevels)) {
          for (nm in names(mod$xlevels)) {
            if (nm %in% names(sp)) {
              if (is.character(sp[[nm]])) sp[[nm]] <- as.factor(sp[[nm]])
              if (is.factor(sp[[nm]])) sp[[nm]] <- factor(sp[[nm]], levels = mod$xlevels[[nm]])
            }
          }
        }

        # Predict
        is_glm_binom <- inherits(mod, "glm") &&
          !is.null(mod$family) &&
          identical(mod$family$family, "binomial")

        pred <- if (is_glm_binom) {
          stats::predict(mod, newdata = sp, type = "response")   # probability
        } else {
          stats::predict(mod, newdata = sp)                      # model scale
        }

        out <- sp
        out$pred <- as.numeric(pred) #Predictions are here.

        pred_panel_r(out)
        phase_d_last_run(Sys.time())
        showNotification("Phase D complete: predictions built.", type = "message", duration = 4)

      }, error = function(e) {
        phase_d_error_r(conditionMessage(e))
        showNotification(paste0("Phase D failed: ", conditionMessage(e)), type = "error", duration = 12)
      })
    }, ignoreInit = TRUE)






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


    #Phase E State
    phase_e_error_r  <- reactiveVal("")
    phase_e_last_run <- reactiveVal(NULL)
    poverty_r        <- reactiveVal(NULL)
    plines_levels_r <- reactiveVal(NULL)

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






    #Event
    observeEvent(input$compute_poverty, {
      phase_e_error_r("")
      poverty_r(NULL)

      tryCatch({
        if (is.null(step1)) stop("Step 1 API missing.")
        mod <- step1$final_model()
        if (is.null(mod)) stop("final_model() is NULL.")

        pp <- pred_panel_r()
        if (is.null(pp) || !nrow(pp) || !"pred" %in% names(pp)) {
          stop("Phase D predictions missing. Run Phase D first.")
        }
        if (!"sim_year" %in% names(pp)) stop("pred_panel missing sim_year.")

        model_kind <- infer_model_kind(mod)

        # weights (optional)
        wt_col <- detect_weight_col(pp, varlist = varlist)
        w <- if (is.null(wt_col)) rep(1, nrow(pp)) else pp[[wt_col]]
        if (anyNA(w)) w[is.na(w)] <- 0

        used <- !is.na(pp$pred)
        cov_n_overall <- mean(used)
        cov_w_overall <- if (sum(w) > 0) sum(w[used]) / sum(w) else NA_real_




        if (model_kind == "glm_binomial") {
          # pred is poverty probability; poverty rate is mean(pred) among usable rows
          by_year <- tidyr::crossing(sim_year = sort(unique(cc$sim_year)), pl) |>
            dplyr::group_by(.data$sim_year, .data$pline_name, .data$pline_level, .data$pline_model) |>
            dplyr::summarise(
              pov_rate = {
                d <- dplyr::cur_data_all()
                # (cur_data_all() includes pl columns too; instead filter cc explicitly)
                dd <- dplyr::filter(cc, .data$sim_year == dplyr::first(.data$sim_year))
                ww <- if ("weight" %in% names(dd)) dd$weight else rep(1, nrow(dd))
                th <- dplyr::first(.data$pline_model)
                poor <- if (pr$pred_scale == "log") dd$pred_lin < th else dd$pred_level < th
                weight_mean(poor, ww)
              },
              .groups = "drop"
            ) |>
            dplyr::transmute(
              sim_year,
              pline = .data$pline_name,
              pline_value = .data$pline_level,
              pline_model = .data$pline_model,
              pov_rate
            )

          overall <- data.frame(
            pov_rate = wmean_safe(pp$pred[used], w[used]),
            coverage_n = cov_n_overall,
            coverage_w = cov_w_overall,
            stringsAsFactors = FALSE
          )

          poverty_r(list(
            overall = overall,
            by_year = by_year,
            meta = list(
              model_kind = model_kind,
              poverty_line_mode = "implicit_in_logit_model",
              coverage_weight_overall = cov_w_overall,
              weight_col = wt_col
            )
          ))

        } else if (model_kind == "lm") {

          # PovLine Section
          p_year <- 2021L

          pv_safe <- if (is.null(pov_lines)) NULL else if (is.function(pov_lines)) pov_lines() else pov_lines
          if (is.null(pv_safe) || !is.data.frame(pv_safe)) {
            stop("pov_lines must be a data.frame OR a reactive/function returning a data.frame.")
          }
          if (!all(c("ppp_year", "ln") %in% names(pv_safe))) {
            stop("pov_lines is missing required columns: ppp_year and ln.")
          }

          plines_levels <- pv_safe |>
            dplyr::filter(.data$ppp_year == p_year) |>
            dplyr::pull(.data$ln)

          plines_levels_r(plines_levels)

          plines_levels[1] #dplyr #Can iterate, and need to be reactive

# # FROM HERE REDUNDANT
#           # Then ensure BOTH calls use ppp_year:
#           #plines_level <- coerce_pov_lines_modelscale(pov_lines, pred_scale = "level", ppp_year = ppp_year)
#
#
#           plines_model <- coerce_pov_lines_modelscale(
#             pov_lines,
#             pred_scale = pred_scale,
#             ppp_year = ppp_year
#           )
#
#
#           # 1) Always coerce poverty lines in *LEVEL* (USD PPP/day)
#           pl_level <- coerce_pov_lines_modelscale(
#             pov_lines,
#             pred_scale = "level",
#             ppp_year = ppp
#           )
#           if (is.null(pl_level) || !length(pl_level)) {
#             stop("pov_lines is required for lm(). Provide a named numeric vector or a data.frame.")
#           }
#           if (is.null(names(pl_level)) || any(!nzchar(names(pl_level)))) {
#             names(pl_level) <- paste0("pline_", seq_along(pl_level))
#           }
#
#
#           # 2) Guess model prediction scale
#           pred_scale_guess <- infer_pred_scale_lm(mod, pred_sample = pp$pred)
#
#
#           # Helper to get the model-scale threshold for a LEVEL poverty line
#           to_model_scale <- function(x_level, scale) {
#             if (scale == "log") log(x_level) else x_level
#           }
#
#
#           # 3) Sanity-check the guessed scale (avoid the "everything is poor" trap)
#           pred_scale <- pred_scale_guess
#           used_pred <- pp$pred[used]
#
#
#           ####### JANK
#           ###########
#
#           # Use the LOWEST line for a simple diagnostic
#           lowest_level <- min(plines_levels, na.rm = TRUE)
#
#           pov_level_low <- wmean_safe(as.numeric(used_pred < lowest_level), w[used])
#           pov_log_low <- wmean_safe(as.numeric(used_pred < log(lowest_level)), w[used])
#
# ########################### JANK
#           if (isTRUE(pred_scale_guess == "level") && isTRUE(pov_level_low > 0.995) && isTRUE(pov_log_low < 0.995)) {
#             pred_scale <- "log"
#             showNotification(
#               "Phase E: predictions look like LOG welfare; switching poverty-line comparison to log scale.",
#               type = "warning", duration = 10
# #            )
# #          }
# #          if (isTRUE(pred_scale_guess == "log") && isTRUE(pov_log_low > 0.995) && isTRUE(pov_level_low < 0.995)) {
# #            pred_scale <- "level"
# #            showNotification(
# #              "Phase E: predictions look like LEVEL welfare; switching poverty-line comparison to level scale.",
# #              type = "warning", duration = 10
# #            )
# #          }
# ###################### JANK

          # --- ASSUMPTION for simplified Phase E ---
          # pp$pred is already in the same LEVEL units as plines_levels (e.g., USD PPP/day).
          # plines_levels is a named numeric vector, e.g. c(pline_1=3, pline_2=4.2, pline_3=8.3)

          # 4) Build results for each poverty line (LEVEL-on-LEVEL comparison)
          if (is.null(plines_levels) || !length(plines_levels)) {
            stop("plines_levels is NULL/empty. Provide a named numeric vector of poverty lines in PPP/day.")
          }
          if (!is.numeric(plines_levels)) {
            stop("plines_levels must be numeric.")
          }
          if (is.null(names(plines_levels)) || any(!nzchar(names(plines_levels)))) {
            names(plines_levels) <- paste0("pline_", seq_along(plines_levels))
          }

          # Optional sanity check: are preds wildly below lines?
          if (any(used, na.rm = TRUE)) {
            q_pred <- stats::quantile(pp$pred[used], probs = c(.01, .05, .5, .95, .99), na.rm = TRUE)
            # If 99% of predictions are below the minimum poverty line, warn loudly
            if (is.finite(q_pred[["99%"]]) && q_pred[["99%"]] < min(plines_levels, na.rm = TRUE)) {
              showNotification(
                "Phase E warning: nearly all predictions are below the lowest poverty line. Check welfare units (per day vs per month, PPP vs LCU, log vs level).",
                type = "warning", duration = 10
              )
            }
          }

          out_by_year  <- list()
          out_overall  <- list()

          for (nm in names(plines_levels)) {
            pl <- as.numeric(plines_levels[[nm]])

            # poverty indicator per row (NA where prediction missing)
            pov_ind <- pp$pred < pl
            pov_ind[!used] <- NA

            by_year <- pp |>
              dplyr::mutate(w = w, used = used, pov = pov_ind) |>
              dplyr::group_by(.data$sim_year) |>
              dplyr::summarise(
                n_total    = dplyr::n(),
                n_used     = sum(.data$used),
                coverage_n = mean(.data$used),
                w_total    = sum(.data$w),
                w_used     = sum(.data$w[.data$used]),
                coverage_w = ifelse(.data$w_total > 0, .data$w_used / .data$w_total, NA_real_),
                pov_rate   = wmean_safe(as.numeric(.data$pov[.data$used]), .data$w[.data$used]),
                .groups = "drop"
              ) |>
              dplyr::mutate(
                pline = nm,
                pline_value = pl
              ) |>
              dplyr::arrange(.data$sim_year)

            overall <- data.frame(
              pline = nm,
              pline_value = pl,
              pov_rate = wmean_safe(as.numeric(pov_ind[used]), w[used]),
              coverage_n = cov_n_overall,
              coverage_w = cov_w_overall,
              stringsAsFactors = FALSE
            )

            out_by_year[[nm]] <- by_year
            out_overall[[nm]] <- overall
          }

          # Prediction quantiles (LEVEL) for debugging
          q <- stats::quantile(pp$pred[used], probs = c(0, .05, .25, .5, .75, .95, 1), na.rm = TRUE)
          pred_summary <- data.frame(
            stat = names(q),
            pred_level = as.numeric(q),
            stringsAsFactors = FALSE
          )

          poverty_r(list(
            overall = dplyr::bind_rows(out_overall),
            by_year = dplyr::bind_rows(out_by_year),
            meta = list(
              model_kind = model_kind,
              coverage_weight_overall = cov_w_overall,
              weight_col = wt_col,
              plines_levels = plines_levels,
              pred_summary = pred_summary
            )
          ))


        } else {
          stop(paste0("Unsupported model kind for Phase E: ", model_kind))
        }

        phase_e_last_run(Sys.time())
        showNotification("Phase E complete: poverty metrics computed.", type = "message", duration = 4)

      }, error = function(e) {
        phase_e_error_r(conditionMessage(e))
        showNotification(paste0("Phase E failed: ", conditionMessage(e)), type = "error", duration = 12)
      })
    }, ignoreInit = TRUE)













    # ---- Module API (so Step 2 can consume Phase B outputs later) ----
    list(

      #Phase B (building Hazards)
      built_haz       = reactive(built_haz_r()),
      weather_data_sim = reactive({ x <- built_haz_r(); if (is.null(x)) NULL else x$weather_data_sim }),
      loc_weather_sim = loc_weather_sim,
      sim_year_range  = sim_year_range,
      phase_b_error    = reactive(phase_b_error_r()),
      phase_b_last_run = reactive(phase_b_last_run),

      #Phase C (Building Simulations)
      sim_panel = reactive(sim_panel_r()),
      phase_c_error = reactive(phase_c_error_r()),
      phase_c_last_run = reactive(phase_c_last_run),

      #Phase D (Predicting Outcome)
      pred_panel = reactive(pred_panel_r()),
      phase_d_error = reactive(phase_d_error_r()),
      phase_d_last_run = reactive(phase_d_last_run),

      #Phase E (Outcome)
      poverty_results = reactive(poverty_r()),
      phase_e_error   = reactive(phase_e_error_r()),
      phase_e_last_run = reactive(phase_e_last_run)
    )
  })
}
