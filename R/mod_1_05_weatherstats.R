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

    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }

    weather_tab_added <- reactiveVal(FALSE)

    output$weather_stats_button_ui <- renderUI({
      req(selected_weather())
      actionButton(ns("weather_stats"), "Weather stats", class = "btn-primary", style = "width: 100%;")
    })

    shiny::outputOptions(output, "weather_stats_button_ui", suspendWhenHidden = FALSE)

    # Reactive placeholders for weather data and bin cutoffs
    
    weather_data <- reactiveVal(NULL)
    loc_weather  <- reactiveVal(NULL)
    survey_weather  <- reactiveVal(NULL)

observeEvent(input$weather_stats, {
  req(selected_weather(), selected_surveys(), survey_data(), survey_h3())
    
    # Reactives inside observeEvent
  
    # notification for loading weather data
    notif_load <- showNotification(
      "Loading weather data...", 
      duration = NULL, type = "message"
    )

    # weather data for selected surveys and variables at loc_id level
    wd <- {
      req(selected_surveys(), selected_weather(), survey_data(), survey_h3())
      ss <- selected_surveys()
      sw <- selected_weather()
      sd <- survey_data()
      h3 <- survey_h3()

      # get date range for weather data
      
      survey_dates <- sd |>
        dplyr::distinct(timestamp) |>
        dplyr::filter(!is.na(timestamp))

      if (length(survey_dates$timestamp) > 0) {
        survey_date_min <- min(survey_dates$timestamp) - months(12) # ensure at least 12 months prior
        survey_date_max <- max(survey_dates$timestamp)
      } else {
        survey_date_min <- as.Date(NA)
        survey_date_max <- as.Date(NA)
      }

      # max required simulation bounds (fallback)
      sim_date_min <- as.Date("1950-01-01") - months(12)
      sim_date_max <- as.Date("2024-12-01")

      # combine bounds safely; prefer real survey bounds but fall back to simulation bounds
      start_date <- min(c(survey_date_min, sim_date_min), na.rm = TRUE)
      end_date   <- max(c(survey_date_max, sim_date_max), na.rm = TRUE)

      weather_dates <- seq(start_date, end_date, by = "1 month")

      # load weather data for countries with selected surveys
      codes <- unique(ss$code)
      root <- unique(dirname(ss$fpath))
      paths_weather <- file.path(root, paste0(codes, "_weather.parquet"))
      
      weather <- read_parquet_duckdb(paths_weather) |>
        # filter to date range needed for surveys and simulation years
        dplyr::filter(timestamp %in% !!weather_dates) |>
        # filter to h3 cells that are in survey_h3
        dplyr::inner_join( 
          survey_h3() |> dplyr::select(code, year, survname, h3, loc_id, pop_2020) |> dplyr::distinct(),
          by = "h3"
        ) |> 
        # aggregate to loc_id level using h3 mapping in survey_h3
        dplyr::group_by(code, year, survname, loc_id, timestamp) |>
        dplyr::summarise(
          across(all_of(sw$name), ~ sum(.x * pop_2020, na.rm = TRUE) / sum(pop_2020, na.rm = TRUE)),
          .groups = "drop"
        )
      
      weather
    }
    weather_data(wd) # assign to reactiveVal so it can be accessed by other reactives and outputs

    # remove loading notification, add notification for configuring weather variables
    removeNotification(notif_load)
    notif_config <- showNotification(
      "Configuring weather variables...", 
      duration = NULL, type = "message"
    )

    # configure weather variables according to user specifications and merge with survey data at loc_id level
    loc_wd <- {
      req(weather_data(), selected_weather(), survey_data(), survey_h3())
      wd <- weather_data() 
      sw <- selected_weather()
      sd <- survey_data() 
      h3 <- survey_h3()

      out <- NULL
      
      # loop over each weather variable
      for (idx in seq_len(nrow(sw))) {

        var_name <- sw$name[idx]
        ref_start <- sw$ref_start[idx]
        ref_end <- sw$ref_end[idx]
        temporal_agg <- sw$temporalAgg[idx]
        transformation <- sw$transformation[idx]
        cont_binned <- sw$cont_binned[idx]
        num_bins <- sw$num_bins[idx]
        binning_method <- sw$binning_method[idx]

        # get variable for each month in selected reference period
        weather <- wd  # start with full data
        for (l in seq(ref_start, ref_end)) {
          colname <- paste0(var_name, "_", l)
          weather <- weather |>
            dplyr::group_by(loc_id) |>
            dplyr::mutate("{colname}" := dplyr::lag(.data[[var_name]], n = l, order_by = timestamp)) |>
            dplyr::ungroup()
        }

        # temporal aggregation over reference period
        lag_cols <- paste0(var_name, "_", seq(ref_start, ref_end))

        # Extract lag columns as a matrix once — avoids repeated across()/pick() calls
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
          dplyr::select(-dplyr::all_of(lag_cols)) # clean up lag columns after aggregation


        # apply transformation if specified
        if (!(transformation == "None" || var_name %in% c("spi6", "spei6"))) {

          # calculate mean and sd for reference climate by loc_id and month
          climate_ref <- weather |>
            dplyr::filter(timestamp >= as.Date("1991-01-01"), timestamp <= as.Date("2020-12-31")) |>
            dplyr::mutate(month = lubridate::month(timestamp)) |>
            dplyr::group_by(loc_id, month) |>
            dplyr::summarise(mean = mean(haz, na.rm = TRUE), sd = sd(haz, na.rm = TRUE), .groups = "drop")
          
          # standardize weather variable using reference climate stats
          if (transformation == "Deviation from mean") {
          weather <- weather |>
            dplyr::mutate(month = lubridate::month(timestamp)) |>
            dplyr::left_join(climate_ref, by = c("loc_id", "month")) |>
            dplyr::mutate(haz = (haz - mean)) |>
            dplyr::select(-month, -mean, -sd)         # clean up temp columns
            
          } else if (transformation == "Standardized anomaly") {
            weather <- weather |>
            dplyr::mutate(month = lubridate::month(timestamp)) |>
            dplyr::left_join(climate_ref, by = c("loc_id", "month")) |>
            dplyr::mutate(haz = (haz - mean)/sd) |>
            dplyr::select(-month, -mean, -sd)         # clean up temp columns
          }
        }

        # Bin variable if specified
        if (!is.na(cont_binned) && cont_binned == "Binned") {

          # define cutoffs for bins from weather in survey data (to ensure bins are meaningful for survey sample)
          weather_svy <- weather |>
            dplyr::inner_join(
              sd |> dplyr::select(loc_id, timestamp) |> dplyr::distinct(),
              by = c("loc_id", "timestamp")) |>
            dplyr::filter(!is.na(haz)) |>
            dplyr::pull(haz)
          
            # equal frequency binning
            if (binning_method == "Equal frequency") {

              cutoffs <- unique(quantile(weather_svy, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE))
              
              if (length(cutoffs) > 1) {
                # extend breaks so outer bins capture values outside the survey-derived range
                breaks_ext <- c(-Inf, cutoffs[-1], Inf)
                weather <- weather |>
                  dplyr::mutate(haz = cut(.data$haz, breaks = breaks_ext, include.lowest = TRUE))
                message("Equal frequency cutoffs for ", var_name, ": ", paste(round(cutoffs, 3), collapse = ", "))
              } else {
                message("Insufficient variation in ", var_name, " for Equal frequency binning. Keeping continuous.")
              }
              # equal width binning
            } else if (binning_method == "Equal width") {

              cutoffs <- unique(seq(min(weather_svy, na.rm = TRUE), max(weather_svy, na.rm = TRUE), length.out = num_bins + 1))

              if (length(cutoffs) > 1) {
                # extend breaks so outer bins capture values outside the survey-derived range
                breaks_ext <- c(-Inf, cutoffs[-1], Inf)
                weather <- weather |>
                  dplyr::mutate(haz = cut(.data$haz, breaks = breaks_ext, include.lowest = TRUE))
                message("Equal width cutoffs for ", var_name, ": ", paste(round(cutoffs, 3), collapse = ", "))
              } else {
                message("Insufficient variation in ", var_name, " for Equal width binning. Keeping continuous.")
              }
              # k-means binning
            } else if (binning_method == "K-means") {
              tryCatch({
                if (length(unique(weather_svy)) >= num_bins) {
                  set.seed(123)

                  km <- kmeans(weather_svy, centers = num_bins)
                  centers <- sort(as.numeric(km$centers))
                  cutoffs <- unique(c(min(weather_svy, na.rm = TRUE), (centers[-length(centers)] + centers[-1]) / 2, max(weather_svy, na.rm = TRUE)))

                  if (length(cutoffs) > 1) {
                    # extend breaks so outer bins capture values outside the survey-derived range
                    breaks_ext <- c(-Inf, cutoffs[-1], Inf)
                    weather <- weather |>
                      dplyr::mutate(haz = cut(.data$haz, breaks = breaks_ext, include.lowest = TRUE))
                    message("K-means cutoffs for ", var_name, ": ", paste(round(cutoffs, 3), collapse = ", "))
                  } else {
                    message("Insufficient variation in ", var_name, " for K-means binning. Keeping continuous.")
                  }
                } else {
                  message("Not enough unique values in ", var_name, " for K-means (need >= num_bins). Keeping continuous.")
                }
              }, error = function(e) {
                message("K-means clustering failed for ", var_name, ": ", e$message, ". Keeping continuous.")
              })
          }
        }
          # Rename haz column to haz_{var_name}
          weather <- weather |>
            dplyr::rename(!!paste0("haz_", var_name) := haz)

          # Combine with other weather variables
          if (is.null(out)) { 
            out <- weather
            } else { 
            out <- dplyr::full_join(out, weather, by = c("code", "year", "survname", "loc_id", "timestamp"))
          }
      } # end loop over weather variables


      # return final data frame with all weather variables configured and merged at loc_id level
      out
    }
    loc_weather(loc_wd) # assign to reactiveVal so it can be accessed by other reactives and outputs

    # remove loading notification, add notification for merging data
    removeNotification(notif_config)
    notif_merge <- showNotification(
      "Merging survey and weather data...", 
      duration = NULL, type = "message"
    )

    # merge survey and weather data (unit level)
    survey_wd <- {
      req(loc_weather(), survey_data())
      sd <- survey_data() 
        
      survey_weather <- sd |>
        dplyr::left_join(loc_weather(), by = c("code", "year", "survname", "loc_id", "timestamp")) |>
        # ensure year is a factor for plotting
        dplyr::mutate(year = as.factor(.data$year)) |>
        # re-normalize weights after filtering to non-missing weather data
        dplyr::group_by(.data$code, .data$year, .data$survname) |>
        dplyr::mutate(weight = .data$weight / sum(.data$weight, na.rm = TRUE)) |>
        dplyr::ungroup()

      # add labels (NOT IMPLEMENTED)
      
      survey_weather
    }
    survey_weather(survey_wd) # assign to reactiveVal so it can be accessed by other reactives and outputs

    removeNotification(notif_merge)
    showNotification("Weather data ready.", duration = 3, type = "message")

    if (!weather_tab_added()) {
    
    # -------- UI Outputs -----------
    # Output: Weather distributions and stats
      
      output$weather_dist1 <- renderPlot({
        req(survey_weather())
        df <- survey_weather() |> dplyr::mutate(countryyear = paste0(economy, ", ", year))
        hv_vec <- isolate(selected_weather()$name)
        hv <- hv_vec[1]
        if (is.na(hv) || !(hv %in% names(df))) {
          plot.new(); title(main = "Weather variable not configured"); return(invisible(NULL))
        }
        label <- isolate(selected_weather()$label[1])
        cont_binned <- isolate(selected_weather()$cont_binned[1])

        if (!is.na(cont_binned) && cont_binned == "Binned") {
          df_plot <- df |> dplyr::filter(!is.na(.data[[hv]]))
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]])) +
            ggplot2::geom_bar(fill = "steelblue", alpha = 0.7) +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = "Distribution of bins",
              x = stringr::str_wrap(paste0(label, "\n(as configured)"), 40),
              y = "Count"
            ) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          p
        } else {
          ridge_distribution_plot(
            df,
            x_var = hv,
            x_label = paste0(label, "\n(as configured)")
          )
        }
      })

      # safer weather_dist2
      output$weather_dist2 <- renderPlot({
        req(survey_weather())
        df <- survey_weather() |> dplyr::mutate(countryyear = paste0(economy, ", ", year))
        hv_vec <- isolate(selected_weather()$name)
        hv <- hv_vec[2]
        if (is.na(hv) || !(hv %in% names(df))) {
          plot.new(); title(main = "Weather variable not configured"); return(invisible(NULL))
        }
        label <- isolate(selected_weather()$label[2])
        cont_binned <- isolate(selected_weather()$cont_binned[2])

        if (!is.na(cont_binned) && cont_binned == "Binned") {
          df_plot <- df |> dplyr::filter(!is.na(.data[[hv]]))
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]])) +
            ggplot2::geom_bar(fill = "steelblue", alpha = 0.7) +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = "Distribution of bins",
              x = stringr::str_wrap(paste0(label, "\n(as configured)"), 40),
              y = "Count"
            ) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          p
        } else {
          ridge_distribution_plot(
            df,
            x_var = hv,
            x_label = paste0(label, "\n(as configured)"),
            wrap_width = 40
          )
        }
      })

        # Binned scatterplots of outcome vs weather
        output$binscatter1 <- renderPlot({
          req(survey_weather(), selected_outcome())
          df <- survey_weather()
          so <- selected_outcome()
          sw <- isolate(selected_weather())
          y_var <- so$name
          y_label <- so$label
          hv_vec <- isolate(selected_weather()$name)
          hv <- hv_vec[1]

          if (is.null(y_var) || !(y_var %in% names(df)) || is.na(hv) || !(hv %in% names(df))) {
            plot.new(); title(main = "Outcome or weather variable not available"); return(invisible(NULL))
          }

          # build logical masks safely (call is.finite only on numeric vectors)
          hv_vec_vals <- df[[hv]]
          y_vec_vals  <- df[[y_var]]
          hv_ok <- if (is.numeric(hv_vec_vals)) is.finite(hv_vec_vals) else !is.na(hv_vec_vals)
          y_ok  <- if (is.numeric(y_vec_vals)) is.finite(y_vec_vals) else !is.na(y_vec_vals)

          df_plot <- df[hv_ok & y_ok, ]
          if (nrow(df_plot) == 0) {
            plot.new(); title(main = "No data available"); return(invisible(NULL))
          }

          is_binary <- length(unique(df_plot[[y_var]])) <= 2
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]], y = .data[[y_var]]))
          if (is_binary) {
            p <- p + ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", linewidth = 1, geom = "line")
          } else {
            p <- p + ggplot2::geom_point(alpha = 0.1) +
              ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", size = 2, geom = "point")
          }

          p +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = "",
              x = stringr::str_wrap(paste0(sw$label[1], "\n(as configured)"), 40),
              y = stringr::str_wrap(y_label, 40)
            ) +
            { if (is_binary) ggplot2::annotate("text", x = Inf, y = Inf, label = "Binary outcome: mean by bin", hjust = 1.05, vjust = 1.2, size = 3) else NULL }
        })

        output$binscatter2 <- renderPlot({
          req(survey_weather(), selected_outcome())
          df <- survey_weather()
          so <- selected_outcome()
          sw <- isolate(selected_weather())
          y_var <- so$name
          y_label <- so$label
          hv_vec <- isolate(selected_weather()$name)
          hv <- hv_vec[2]

          if (is.null(y_var) || !(y_var %in% names(df)) || is.na(hv) || !(hv %in% names(df))) {
            plot.new(); title(main = "Outcome or weather variable not available"); return(invisible(NULL))
          }

          hv_vec_vals <- df[[hv]]
          y_vec_vals  <- df[[y_var]]
          hv_ok <- if (is.numeric(hv_vec_vals)) is.finite(hv_vec_vals) else !is.na(hv_vec_vals)
          y_ok  <- if (is.numeric(y_vec_vals)) is.finite(y_vec_vals) else !is.na(y_vec_vals)

          df_plot <- df[hv_ok & y_ok, ]
          if (nrow(df_plot) == 0) {
            plot.new(); title(main = "No data available"); return(invisible(NULL))
          }

          is_binary <- length(unique(df_plot[[y_var]])) <= 2
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]], y = .data[[y_var]]))
          if (is_binary) {
            p <- p + ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", linewidth = 1, geom = "line")
          } else {
            p <- p + ggplot2::geom_point(alpha = 0.1) +
              ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", size = 2, geom = "point")
          }

          p +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = "",
              x = stringr::str_wrap(paste0(sw$label[2], "\n(as configured)"), 40),
              y = stringr::str_wrap(y_label, 40)
            ) +
            { if (is_binary) ggplot2::annotate("text", x = Inf, y = Inf, label = "Binary outcome: mean by bin", hjust = 1.05, vjust = 1.2, size = 3) else NULL }
        })

        # summary stats for weather variables
        output$weather_stats_table <- DT::renderDT({
          req(survey_weather())
          df <- survey_weather()
          # weather variables start with haz_
          df <- dplyr::mutate(df, countryyear = paste0(economy, ", ", year))
          vars <- names(df)[grepl("^haz_", names(df))]
          if (length(vars) == 0) return(data.frame(Note = "No weather variables found"))  
          weighted_summary_long(df, vars = vars)
        }, rownames = FALSE,
          options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
          class = "compact")
    
        # selected weather data
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
            shiny::h4("Distribution of weather (household survey sample)"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("weather_dist1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("weather_dist2"), height = "300px"))
            ),
            shiny::br(),
            shiny::h4("Weather over time and space"),
            shiny::helpText("Weather maps to be added...", style = "font-size: 12px;"),
            shiny::br(),
            shiny::h4("Outcome vs weather"),
            bslib::layout_columns(
              col_widths = c(6, 6),
              bslib::card(shiny::plotOutput(ns("binscatter1"), height = "300px")),
              bslib::card(shiny::plotOutput(ns("binscatter2"), height = "300px"))
            ),
            shiny::br(),
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
      weather_data = weather_data,
      loc_weather = loc_weather,
      survey_weather = survey_weather
    )
      })
}