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
    verbatimTextOutput(ns("phase_c_status"))

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

        base <- sw |>
          dplyr::arrange(.data$timestamp) |>
          dplyr::group_by(.data[[hh_col]]) |>
          dplyr::slice(1) |>
          dplyr::ungroup() |>
          dplyr::mutate(int_month = lubridate::month(.data$timestamp)) |>
          dplyr::select(dplyr::all_of(hh_col), .data$loc_id, .data$int_month)

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












    # ---- Module API (so Step 2 can consume Phase B outputs later) ----
    list(

      #Phase B (building Hazards)
      built_haz       = reactive(built_haz_r()),
      weather_data_sim = reactive({ x <- built_haz_r(); if (is.null(x)) NULL else x$weather_data_sim }),
      loc_weather_sim = loc_weather_sim,
      sim_year_range  = sim_year_range,
      sim_year_range   = reactive({ x <- built_haz_r(); if (is.null(x)) NULL else x$sim_year_range }),
      phase_b_error    = reactive(phase_b_error_r()),
      phase_b_last_run = reactive(phase_b_last_run),

      #Phase C (Building Simulations)
      sim_panel = reactive(sim_panel_r()),
      phase_c_error = reactive(phase_c_error_r()),
      phase_c_last_run = reactive(phase_c_last_run)
    )
  })
}
