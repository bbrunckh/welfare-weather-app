#' 1_05_weatherstats UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_05_weatherstats_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("weather_stats_button_ui"))
  )
}

#' 1_05_weatherstats Server Functions
#'
#' @noRd
mod_1_05_weatherstats_server <- function(
    id,
    varlist = varlist,
    selected_surveys = selected_surveys,
    selected_outcome = selected_outcome,
    selected_weather = selected_weather,
    survey_data = survey_data,
    survey_h3 = survey_h3,
    tabset_id,
    tabset_session = NULL
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    `%||%` <- function(a, b) if (!is.null(a)) a else b

    # Helper: map raw weather varname -> haz_* column name
    to_haz <- function(x) {
      x <- trimws(as.character(x))
      if (is.na(x) || !nzchar(x)) return(NA_character_)
      if (startsWith(x, "haz_")) return(x)
      paste0("haz_", x)
    }

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    weather_tab_added <- reactiveVal(FALSE)


    has_w2 <- reactive({
      sw <- selected_weather()
      is.data.frame(sw) && nrow(sw) >= 2 && !is.na(sw$name[2]) && nzchar(as.character(sw$name[2]))
    })

    output$weather_stats_button_ui <- renderUI({
      req(selected_weather())
      actionButton(ns("weather_stats"), "Weather stats", class = "btn-primary", style = "width: 100%;")
    })
    shiny::outputOptions(output, "weather_stats_button_ui", suspendWhenHidden = FALSE)

    # ---- Cached weather objects (computed lazily; NOT dependent on a UI click) ----
    weather_data_rv   <- reactiveVal(NULL)  # loc-month base (raw variables aggregated to loc_id)
    loc_weather_rv    <- reactiveVal(NULL)  # loc-month configured hazards (haz_*)
    survey_weather_rv <- reactiveVal(NULL)  # unit-level survey joined to loc_weather
    last_key_rv       <- reactiveVal(NULL)

    # helper: shift Date by N months (negative allowed)
    shift_months <- function(d, n) {
      if (is.na(d)) return(as.Date(NA))
      as.Date(seq.Date(as.Date(d), by = paste0(n, ' months'), length.out = 2)[2])
    }


    # Key for cache invalidation (keep simple & stable)
    make_key <- function() {
      ss <- selected_surveys()
      sw <- selected_weather()
      sd <- survey_data()





      ss_key <- ""
      if (!is.null(ss) && is.data.frame(ss) && nrow(ss)) {
        cols <- intersect(c("code","year","survname","fpath"), names(ss))
        ss_key <- paste0(apply(unique(ss[, cols, drop = FALSE]), 1, paste, collapse = ":"), collapse = "|")
      }

      sw_key <- ""
      if (!is.null(sw) && is.data.frame(sw) && nrow(sw)) {
        cols <- intersect(
          c("name","ref_start","ref_end","temporalAgg","transformation","cont_binned","num_bins","binning_method","polynomial"),
          names(sw)
        )
        sw_key <- paste0(apply(sw[, cols, drop = FALSE], 1, paste, collapse = ":"), collapse = "|")
      }

      # include a small fingerprint of survey timestamps so a new sample triggers rebuild
      sd_key <- ""
      if (!is.null(sd) && is.data.frame(sd) && nrow(sd) && "timestamp" %in% names(sd)) {
        ts <- unique(sd$timestamp)
        ts <- ts[!is.na(ts)]
        if (length(ts)) {
          sd_key <- paste0(as.character(min(ts)), "_", as.character(max(ts)), "_", length(ts))
        }
      }

      paste(ss_key, sw_key, sd_key, sep = " :: ")
    }

    # Compute and cache weather objects; called lazily (e.g., by model fit) and by the Weather stats button
    ensure_built <- function(notify = FALSE) {
      req(selected_weather(), selected_surveys(), survey_data(), survey_h3())

      key <- make_key()
      already <- !is.null(last_key_rv()) && identical(last_key_rv(), key) &&
        !is.null(survey_weather_rv()) && is.data.frame(survey_weather_rv()) && nrow(survey_weather_rv()) > 0

      if (already) return(invisible(TRUE))

      notif_id <- NULL
      if (isTRUE(notify)) {
        notif_id <- showNotification("Building weather data…", duration = NULL, type = "message")
      }

      # ---- 1) Load + aggregate base weather data to loc_id-month ----
      ss <- selected_surveys()
      sw <- selected_weather()
      sd <- survey_data()

      # date bounds (survey window + simulation window)
      survey_dates <- sd |>
        dplyr::distinct(timestamp) |>
        dplyr::filter(!is.na(timestamp))

      if (length(survey_dates$timestamp) > 0) {
        survey_date_min <- shift_months(min(survey_dates$timestamp), -12)
        survey_date_max <- max(survey_dates$timestamp)
      } else {
        survey_date_min <- as.Date(NA)
        survey_date_max <- as.Date(NA)
      }

      sim_date_min <- shift_months(as.Date("1950-01-01"), -12)
      sim_date_max <- as.Date("2024-12-01")

      start_date <- min(c(survey_date_min, sim_date_min), na.rm = TRUE)
      end_date   <- max(c(survey_date_max, sim_date_max), na.rm = TRUE)
      weather_dates <- seq.Date(start_date, end_date, by = "month")

      codes <- unique(ss$code)
      root <- unique(dirname(ss$fpath))
      paths_weather <- file.path(root, paste0(codes, "_weather.parquet"))

      wd <- read_parquet_duckdb(paths_weather) |>
        dplyr::filter(timestamp %in% !!weather_dates) |>
        dplyr::inner_join(
          survey_h3() |> dplyr::select(code, year, survname, h3, loc_id, pop_2020) |> dplyr::distinct(),
          by = "h3"
        ) |>
        dplyr::group_by(code, year, survname, loc_id, timestamp) |>
        dplyr::summarise(
          across(all_of(sw$name), ~ sum(.x * pop_2020, na.rm = TRUE) / sum(pop_2020, na.rm = TRUE)),
          .groups = "drop"
        )

      weather_data_rv(wd)

      # ---- 2) Configure hazards (haz_*) at loc_id-month ----
      out <- NULL
      for (idx in seq_len(nrow(sw))) {

        var_name      <- sw$name[idx]
        ref_start     <- sw$ref_start[idx]
        ref_end       <- sw$ref_end[idx]
        temporal_agg  <- sw$temporalAgg[idx]
        transformation <- sw$transformation[idx]
        cont_binned   <- sw$cont_binned[idx]
        num_bins      <- sw$num_bins[idx]
        binning_method <- sw$binning_method[idx]

        weather <- wd
        for (l in seq(ref_start, ref_end)) {
          colname <- paste0(var_name, "_", l)
          weather <- weather |>
            dplyr::group_by(loc_id) |>
            dplyr::mutate("{colname}" := dplyr::lag(.data[[var_name]], n = l, order_by = timestamp)) |>
            dplyr::ungroup()
        }

        lag_cols <- paste0(var_name, "_", seq(ref_start, ref_end))
        lag_mat <- as.matrix(weather[, lag_cols])

        weather <- weather |>
          dplyr::mutate(
            haz = switch(temporal_agg,
              "Mean"   = rowMeans(lag_mat, na.rm = TRUE),
              "Median" = matrixStats::rowMedians(lag_mat, na.rm = TRUE),
              "Min"    = matrixStats::rowMins(lag_mat, na.rm = TRUE),
              "Max"    = matrixStats::rowMaxs(lag_mat, na.rm = TRUE),
              "Sum"    = rowSums(lag_mat,    na.rm = TRUE)
            )
          ) |>
          dplyr::select(-dplyr::all_of(lag_cols))

        # apply transformation if specified
        if (!(transformation == "None" || var_name %in% c("spi6", "spei6"))) {

          climate_ref <- weather |>
            dplyr::filter(timestamp >= as.Date("1991-01-01"), timestamp <= as.Date("2020-12-31")) |>
            dplyr::mutate(month = lubridate::month(timestamp)) |>
            dplyr::group_by(loc_id, month) |>
            dplyr::summarise(mean = mean(haz, na.rm = TRUE), sd = sd(haz, na.rm = TRUE), .groups = "drop")

          if (transformation == "Deviation from mean") {
            weather <- weather |>
              dplyr::mutate(month = lubridate::month(timestamp)) |>
              dplyr::left_join(climate_ref, by = c("loc_id", "month")) |>
              dplyr::mutate(haz = (haz - mean)) |>
              dplyr::select(-month, -mean, -sd)
          } else if (transformation == "Standardized anomaly") {
            weather <- weather |>
              dplyr::mutate(month = lubridate::month(timestamp)) |>
              dplyr::left_join(climate_ref, by = c("loc_id", "month")) |>
              dplyr::mutate(haz = (haz - mean) / sd) |>
              dplyr::select(-month, -mean, -sd)
          }
        }

        # Binning
        if (!is.na(cont_binned) && cont_binned == "Binned") {

          weather_svy <- weather |>
            dplyr::inner_join(
              sd |> dplyr::select(loc_id, timestamp) |> dplyr::distinct(),
              by = c("loc_id", "timestamp")
            ) |>
            dplyr::filter(!is.na(haz)) |>
            dplyr::pull(haz)

          if (length(weather_svy)) {
            if (binning_method == "Equal frequency") {
              cutoffs <- unique(quantile(weather_svy, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE))
              if (length(cutoffs) > 1) {
                breaks_ext <- c(-Inf, cutoffs[-1], Inf)
                weather <- weather |> dplyr::mutate(haz = cut(.data$haz, breaks = breaks_ext, include.lowest = TRUE))
              }
            } else if (binning_method == "Equal width") {
              cutoffs <- unique(seq(min(weather_svy, na.rm = TRUE), max(weather_svy, na.rm = TRUE), length.out = num_bins + 1))
              if (length(cutoffs) > 1) {
                breaks_ext <- c(-Inf, cutoffs[-1], Inf)
                weather <- weather |> dplyr::mutate(haz = cut(.data$haz, breaks = breaks_ext, include.lowest = TRUE))
              }
            } else if (binning_method == "K-means") {
              tryCatch({
                if (length(unique(weather_svy)) >= num_bins) {
                  set.seed(123)
                  km <- kmeans(weather_svy, centers = num_bins)
                  centers <- sort(as.numeric(km$centers))
                  cutoffs <- unique(c(min(weather_svy, na.rm = TRUE), (centers[-length(centers)] + centers[-1]) / 2, max(weather_svy, na.rm = TRUE)))
                  if (length(cutoffs) > 1) {
                    breaks_ext <- c(-Inf, cutoffs[-1], Inf)
                    weather <- weather |> dplyr::mutate(haz = cut(.data$haz, breaks = breaks_ext, include.lowest = TRUE))
                  }
                }
              }, error = function(e) NULL)
            }
          }
        }

        # Rename haz -> haz_{var_name} and merge
        weather <- weather |> dplyr::rename(!!paste0("haz_", var_name) := haz)

        if (is.null(out)) {
          out <- weather
        } else {
          out <- dplyr::full_join(out, weather, by = c("code", "year", "survname", "loc_id", "timestamp"))
        }
      } # end weather variable loop

      loc_weather_rv(out)

      # ---- 3) Join to unit-level survey ----
      survey_wd <- sd |>
        dplyr::left_join(out, by = c("code", "year", "survname", "loc_id", "timestamp")) |>
        dplyr::mutate(year = as.factor(.data$year)) |>
        dplyr::group_by(.data$code, .data$year, .data$survname) |>
        dplyr::mutate(weight = .data$weight / sum(.data$weight, na.rm = TRUE)) |>
        dplyr::ungroup()

      survey_weather_rv(survey_wd)
      last_key_rv(key)

      if (!is.null(notif_id)) removeNotification(notif_id)
      if (isTRUE(notify)) showNotification("Weather data ready.", duration = 2, type = "message")

      invisible(TRUE)
    }

    # Public accessors (reactive) used by other modules.
    weather_data <- reactive({ ensure_built(FALSE); weather_data_rv() })
    loc_weather  <- reactive({ ensure_built(FALSE); loc_weather_rv() })
    survey_weather <- reactive({ ensure_built(FALSE); survey_weather_rv() })

    # ---- Weather stats UI click: ensure built + add/open tab ----
    observeEvent(input$weather_stats, {
      ensure_built(TRUE)

      if (!weather_tab_added()) {

        # Weather distribution plots and binned scatterplots
        output$weather_dist1 <- renderPlot({
          req(survey_weather(), selected_weather())
          df <- survey_weather() |> dplyr::mutate(countryyear = paste0(economy, ", ", year))

          hv_raw <- isolate(selected_weather()$name[1])
          hv <- to_haz(hv_raw)
          if (is.na(hv) || !(hv %in% names(df))) {
            plot.new(); title(main = "Weather variable not configured"); return(invisible(NULL))
          }

          label <- isolate(selected_weather()$label[1])
          cont_binned <- isolate(selected_weather()$cont_binned[1])

          if (!is.na(cont_binned) && cont_binned == "Binned") {
            df_plot <- df |> dplyr::filter(!is.na(.data[[hv]]))
            ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]])) +
              ggplot2::geom_bar(alpha = 0.7) +
              ggplot2::theme_minimal() +
              ggplot2::labs(title = "Distribution of bins", x = stringr::str_wrap(paste0(label, "\n(as configured)"), 40), y = "Count") +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          } else {
            ridge_distribution_plot(df, x_var = hv, x_label = paste0(label, "\n(as configured)"))
          }
        })

        output$weather_dist2 <- renderPlot({
          req(survey_weather(), selected_weather())
          df <- survey_weather() |> dplyr::mutate(countryyear = paste0(economy, ", ", year))

          hv_raw <- isolate(selected_weather()$name[2])
          hv <- to_haz(hv_raw)
          if (is.na(hv) || !(hv %in% names(df))) {
            plot.new(); title(main = "Weather variable not configured"); return(invisible(NULL))
          }

          label <- isolate(selected_weather()$label[2])
          cont_binned <- isolate(selected_weather()$cont_binned[2])

          if (!is.na(cont_binned) && cont_binned == "Binned") {
            df_plot <- df |> dplyr::filter(!is.na(.data[[hv]]))
            ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]])) +
              ggplot2::geom_bar(alpha = 0.7) +
              ggplot2::theme_minimal() +
              ggplot2::labs(title = "Distribution of bins", x = stringr::str_wrap(paste0(label, "\n(as configured)"), 40), y = "Count") +
              ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          } else {
            ridge_distribution_plot(df, x_var = hv, x_label = paste0(label, "\n(as configured)"), wrap_width = 40)
          }
        })

        output$binscatter1 <- renderPlot({
          req(survey_weather(), selected_outcome(), selected_weather())
          df <- survey_weather()
          so <- selected_outcome()
          sw <- isolate(selected_weather())

          y_var <- so$name
          y_label <- so$label

          hv_raw <- isolate(selected_weather()$name[1])
          hv <- to_haz(hv_raw)

          if (is.null(y_var) || !(y_var %in% names(df)) || is.na(hv) || !(hv %in% names(df))) {
            plot.new(); title(main = "Outcome or weather variable not available"); return(invisible(NULL))
          }

          hv_vals <- df[[hv]]
          y_vals  <- df[[y_var]]
          hv_ok <- if (is.numeric(hv_vals)) is.finite(hv_vals) else !is.na(hv_vals)
          y_ok  <- if (is.numeric(y_vals)) is.finite(y_vals) else !is.na(y_vals)

          df_plot <- df[hv_ok & y_ok, ]
          if (!nrow(df_plot)) { plot.new(); title(main = "No data available"); return(invisible(NULL)) }

          is_binary <- length(unique(df_plot[[y_var]])) <= 2
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]], y = .data[[y_var]]))
          if (is_binary) {
            p <- p + ggplot2::stat_summary_bin(fun = "mean", bins = 20, linewidth = 1, geom = "line")
          } else {
            p <- p + ggplot2::geom_point(alpha = 0.1) +
              ggplot2::stat_summary_bin(fun = "mean", bins = 20, size = 2, geom = "point")
          }

          p + ggplot2::theme_minimal() +
            ggplot2::labs(
              x = stringr::str_wrap(paste0(sw$label[1], "\n(as configured)"), 40),
              y = stringr::str_wrap(y_label, 40)
            )
        })

        output$binscatter2 <- renderPlot({
          req(survey_weather(), selected_outcome(), selected_weather())
          df <- survey_weather()
          so <- selected_outcome()
          sw <- isolate(selected_weather())

          y_var <- so$name
          y_label <- so$label

          hv_raw <- isolate(selected_weather()$name[2])
          hv <- to_haz(hv_raw)

          if (is.null(y_var) || !(y_var %in% names(df)) || is.na(hv) || !(hv %in% names(df))) {
            plot.new(); title(main = "Outcome or weather variable not available"); return(invisible(NULL))
          }

          hv_vals <- df[[hv]]
          y_vals  <- df[[y_var]]
          hv_ok <- if (is.numeric(hv_vals)) is.finite(hv_vals) else !is.na(hv_vals)
          y_ok  <- if (is.numeric(y_vals)) is.finite(y_vals) else !is.na(y_vals)

          df_plot <- df[hv_ok & y_ok, ]
          if (!nrow(df_plot)) { plot.new(); title(main = "No data available"); return(invisible(NULL)) }

          is_binary <- length(unique(df_plot[[y_var]])) <= 2
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]], y = .data[[y_var]]))
          if (is_binary) {
            p <- p + ggplot2::stat_summary_bin(fun = "mean", bins = 20, linewidth = 1, geom = "line")
          } else {
            p <- p + ggplot2::geom_point(alpha = 0.1) +
              ggplot2::stat_summary_bin(fun = "mean", bins = 20, size = 2, geom = "point")
          }

          p + ggplot2::theme_minimal() +
            ggplot2::labs(
              x = stringr::str_wrap(paste0(sw$label[2], "\n(as configured)"), 40),
              y = stringr::str_wrap(y_label, 40)
            )
        })


        output$weather_dist2_ui <- renderUI({
          if (!has_w2()) {
            return(shiny::helpText("Select a second weather variable to populate this panel."))
          }
          shiny::plotOutput(ns("weather_dist2"), height = "250px")
        })
        shiny::outputOptions(output, "weather_dist2_ui", suspendWhenHidden = FALSE)

        output$binscatter2_ui <- renderUI({
          if (!has_w2()) {
            return(shiny::helpText("Select a second weather variable to populate this panel."))
          }
          shiny::plotOutput(ns("binscatter2"), height = "250px")
        })
        shiny::outputOptions(output, "binscatter2_ui", suspendWhenHidden = FALSE)

        output$weather_stats_table <- DT::renderDT({
          req(survey_weather())
          df <- survey_weather()
          df <- dplyr::mutate(df, countryyear = paste0(economy, ", ", year))
          vars <- names(df)[grepl("^haz_", names(df))]
          if (!length(vars)) return(data.frame(Note = "No weather variables found"))
          weighted_summary_long(df, vars = vars)
        }, rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
        class = "compact")

        output$selected_weather <- DT::renderDT({
          selected_weather()
        }, rownames = FALSE,
        options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
        class = "compact")

        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Weather stats",
            value = "weather_desc",
            shiny::fluidRow(
              shiny::column(6, shiny::h4("Weather distribution"), plotOutput(ns("weather_dist1"), height = "250px")),
              shiny::column(6, shiny::h4("Weather distribution (2)"), uiOutput(ns("weather_dist2_ui")))
            ),
            shiny::fluidRow(
              shiny::column(6, shiny::h4("Binscatter"), plotOutput(ns("binscatter1"), height = "250px")),
              shiny::column(6, shiny::h4("Binscatter (2)"), uiOutput(ns("binscatter2_ui")))
            ),
            shiny::hr(),
            shiny::h4("Weather summary stats"),
            shiny::helpText("Summary statistics are shown for the configured weather variables. Sample weights are used.", style = "font-size: 12px;"),
            DT::DTOutput(ns("weather_stats_table")),
            shiny::br(),
            shiny::h4("Selected weather variables"),
            DT::DTOutput(ns("selected_weather"))
          ),
          select = TRUE,
          session = tabset_session
        )

        weather_tab_added(TRUE)
      }

      if (weather_tab_added()) {
        try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id, selected = "weather_desc"), silent = TRUE)
      }
    }, ignoreInit = TRUE, ignoreNULL = TRUE)

    # --------- Module return API -----------
    list(
      weather_data   = weather_data,
      loc_weather    = loc_weather,
      survey_weather = survey_weather
    )
  })
}
