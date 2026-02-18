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

    # Reactive to store bin cutoffs
    bin_cutoffs <- reactiveVal(list())

    observeEvent(input$weather_stats, {
      req(selected_weather(), selected_surveys(), survey_data(), survey_h3())

      if (!weather_tab_added()) {
        
        # Load weather data for selected surveys
        weather_data <- reactive({
          req(selected_surveys(), selected_weather(), survey_data(), survey_h3())

          # Get selected weather variables
          sw <- selected_weather()
          
          # Get survey dates from survey data 
          df <- survey_data()

          # Create timestamp from int_year and int_month
          survey_dates <- df |>
            dplyr::mutate(
              int_year = as.integer(.data$int_year),
              int_month = as.integer(.data$int_month),
              timestamp = as.Date(paste0(.data$int_year, "-", .data$int_month, "-01"))
            ) |>
            dplyr::pull(.data$timestamp) |>
            stats::na.omit() |>
            as.Date()

          if (!length(survey_dates) || all(!is.finite(survey_dates))) return(NULL)

          survey_date_min <- min(survey_dates, na.rm = TRUE)
          survey_date_max <- max(survey_dates, na.rm = TRUE)
          if (!is.finite(survey_date_min) || !is.finite(survey_date_max)) return(NULL)
          
          weather_dates <- seq.Date(from = survey_date_min, to = survey_date_max, by = "1 month")
          if (length(weather_dates)) {
            weather_dates <- seq.Date(
              from = min(weather_dates) - 365,
              to = max(weather_dates),
              by = "1 month"
            )
          }
          
          # Load weather data for selected surveys
          codes <- unique(selected_surveys()$code)
          root <- unique(dirname(selected_surveys()$fpath))
          paths_weather <- file.path(root, paste0(codes, "_weather.parquet"))
          
          # Only read files that exist
          paths_weather <- paths_weather[file.exists(paths_weather)]
          req(length(paths_weather) > 0)
          
          weather <- read_parquet_duckdb(paths_weather)

          weather |>
            # Keep h3 index, timestamp and selected weather variables
            dplyr::select(h3, timestamp, dplyr::all_of(sw$name)) |>
            # Filter to survey dates and h3 in survey, and distinct
            dplyr::filter(h3 %in% survey_h3()$h3) |>
            dplyr::filter(timestamp %in% weather_dates) |>
            dplyr::distinct()
        })

        h3_weather <- reactive({
          req(weather_data(), selected_weather())
          
          sw <- selected_weather()
          out <- NULL
          cutoffs_list <- list()

          for (idx in seq_len(nrow(sw))) {
            var_name <- sw$name[idx]
            ref_start <- sw$ref_start[idx]
            ref_end <- sw$ref_end[idx]
            temporal_agg <- sw$temporalAgg[idx]
            transformation <- sw$transformation[idx]
            cont_binned <- sw$cont_binned[idx]
            num_bins <- sw$num_bins[idx]
            binning_method <- sw$binning_method[idx]

            weather <- weather_data() |>
              dplyr::group_by(.data$h3)

            # Temporal aggregation - create lagged columns
            if (is.finite(ref_start) && is.finite(ref_end)) {
              for (l in seq(ref_start, ref_end)) {
                colname <- paste0(var_name, "_", l)
                weather <- weather |>
                  dplyr::mutate(!!rlang::sym(colname) := dplyr::lag(.data[[var_name]], n = l, order_by = .data$timestamp))
              }
            }

            # Filter out rows with missing data in the reference period
            if (is.finite(ref_start) && is.finite(ref_end)) {
              end_col <- paste0(var_name, "_", max(ref_start, ref_end))
              if (end_col %in% names(weather)) {
                weather <- weather |>
                  dplyr::filter(!is.na(.data[[end_col]])) |>
                  dplyr::ungroup()
              } else {
                weather <- weather |>
                  dplyr::ungroup()
              }
            } else {
              weather <- weather |>
                dplyr::ungroup()
            }

            # Apply temporal aggregation function
            if (temporal_agg == "Mean") {
              weather <- weather |>
                dplyr::mutate(haz = rowMeans(dplyr::across(dplyr::starts_with(paste0(var_name, "_"))), na.rm = TRUE))
            } else if (temporal_agg == "Median") {
              lag_cols <- names(weather)[grepl(paste0("^", var_name, "_"), names(weather))]
              weather <- weather |>
                dplyr::rowwise() |>
                dplyr::mutate(haz = stats::median(dplyr::c_across(dplyr::all_of(lag_cols)), na.rm = TRUE)) |>
                dplyr::ungroup()
            } else if (temporal_agg == "Min") {
              weather <- weather |>
                dplyr::mutate(haz = do.call(pmin, c(dplyr::across(dplyr::starts_with(paste0(var_name, "_"))), na.rm = TRUE)))
            } else if (temporal_agg == "Max") {
              weather <- weather |>
                dplyr::mutate(haz = do.call(pmax, c(dplyr::across(dplyr::starts_with(paste0(var_name, "_"))), na.rm = TRUE)))
            } else if (temporal_agg == "Sum") {
              weather <- weather |>
                dplyr::mutate(haz = rowSums(dplyr::across(dplyr::starts_with(paste0(var_name, "_"))), na.rm = TRUE))
            }

            if (!"haz" %in% names(weather)) {
              weather <- weather |>
                dplyr::mutate(haz = NA_real_)
            }

            # Transform weather variable (standardize, anomaly, etc.)
            if (!is.na(transformation) && 
                !(transformation == "None" || var_name %in% c("spi6", "spei6")) && 
                "haz" %in% names(weather)) {
              
              weather <- weather |>
                dplyr::mutate(
                  year = lubridate::year(.data$timestamp),
                  month = lubridate::month(.data$timestamp)
                )

              climate_ref <- weather |>
                dplyr::filter(.data$year >= 1991 & .data$year <= 2020) |>
                dplyr::summarise(
                  mean = mean(.data$haz, na.rm = TRUE),
                  sd = stats::sd(.data$haz, na.rm = TRUE),
                  .by = c(.data$h3, .data$month)
                )

              if (transformation == "Deviation from mean") {
                weather <- weather |>
                  dplyr::left_join(climate_ref, by = c("h3", "month")) |>
                  dplyr::mutate(haz = .data$haz - .data$mean)
              } else if (transformation == "Standardized anomaly") {
                weather <- weather |>
                  dplyr::left_join(climate_ref, by = c("h3", "month")) |>
                  dplyr::mutate(haz = (.data$haz - .data$mean) / .data$sd)
              }
            }

            # Bin weather variable (non-linear effects)
            if (!is.na(cont_binned) && cont_binned == "Binned") {
              haz_clean <- weather$haz[!is.na(weather$haz)]
              
              if (length(haz_clean) > 0 && !is.na(num_bins) && !is.na(binning_method)) {
                
                if (binning_method == "Equal frequency") {
                  cutoffs <- quantile(haz_clean, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE)
                  weather <- weather |>
                    dplyr::mutate(haz = cut(.data$haz, breaks = cutoffs, include.lowest = TRUE))
                  
                  cutoffs_list[[var_name]] <- cutoffs
                  message("Equal frequency cutoffs for ", var_name, ": ", paste(round(cutoffs, 3), collapse = ", "))
                  
                } else if (binning_method == "Equal width") {
                  cutoffs <- seq(min(haz_clean, na.rm = TRUE), max(haz_clean, na.rm = TRUE), length.out = num_bins + 1)
                  weather <- weather |>
                    dplyr::mutate(haz = cut(.data$haz, breaks = cutoffs, include.lowest = TRUE))
                  
                  cutoffs_list[[var_name]] <- cutoffs
                  message("Equal width cutoffs for ", var_name, ": ", paste(round(cutoffs, 3), collapse = ", "))
                  
                } else if (binning_method == "K-means") {
                  set.seed(123) # for reproducibility
                  km <- kmeans(haz_clean, centers = num_bins)
                  
                  # Create cutoffs from cluster centers
                  centers <- sort(km$centers[, 1])
                  cutoffs <- c(min(haz_clean), (centers[-length(centers)] + centers[-1]) / 2, max(haz_clean))
                  
                  weather <- weather |>
                    dplyr::mutate(haz = cut(.data$haz, breaks = cutoffs, include.lowest = TRUE, labels = FALSE))
                  
                  cutoffs_list[[var_name]] <- cutoffs
                  message("K-means cutoffs for ", var_name, ": ", paste(round(cutoffs, 3), collapse = ", "))
                }
              }
            }

            weather <- weather |>
              dplyr::select(.data$h3, .data$timestamp, .data$haz) |>
              dplyr::rename_with(~ paste0("haz_", var_name), .cols = dplyr::starts_with("haz")) |>
              dplyr::arrange(.data$h3, .data$timestamp)

            if (is.null(out)) {
              out <- weather
            } else {
              out <- dplyr::full_join(out, weather, by = c("h3", "timestamp"))
            }
          }

          # Store cutoffs
          bin_cutoffs(cutoffs_list)
          
          out
        })

        # Get haz variable names
        haz_vars <- reactive({
          req(selected_weather())
          paste0("haz_", selected_weather()$name)
        })

        loc_weather <- reactive({
          req(h3_weather(), survey_h3())
          h3 <- survey_h3()
          hw <- h3_weather()
          join_cols <- intersect(c("h3", "timestamp"), intersect(names(h3), names(hw)))
          if (!length(join_cols)) {
            return(NULL)
          }

          data <- dplyr::left_join(h3, hw, by = join_cols)

          by_cols <- intersect(
            c("code", "year", "survname", "loc_id", "timestamp"),
            names(data)
          )
          if (!length(by_cols)) {
            return(NULL)
          }

          if ("pop_2020" %in% names(data)) {
            data <- data |>
              dplyr::summarise(
                dplyr::across(dplyr::starts_with("haz"), ~ sum(.x * .data$pop_2020, na.rm = TRUE) / sum(.data$pop_2020, na.rm = TRUE)),
                .by = dplyr::all_of(by_cols)
              )
          } else {
            data <- data |>
              dplyr::summarise(
                dplyr::across(dplyr::starts_with("haz"), ~ mean(.x, na.rm = TRUE)),
                .by = dplyr::all_of(by_cols)
              )
          }

          data |>
            dplyr::mutate(loc_id = as.character(.data$loc_id))
        })

        survey_weather <- reactive({
          req(loc_weather(), survey_data())
          sd <- survey_data()
          lw <- loc_weather()
          join_cols <- intersect(c("code", "year", "survname", "loc_id", "timestamp"), intersect(names(sd), names(lw)))
          if (!length(join_cols)) {
            return(NULL)
          }

          sw <- sd |>
            dplyr::left_join(lw, by = join_cols) |>
            dplyr::mutate(year = as.factor(.data$year)) |>
            dplyr::group_by(.data$code, .data$year, .data$survname) |>
            dplyr::mutate(weight = .data$weight / sum(.data$weight, na.rm = TRUE)) |>
            dplyr::ungroup()

          sw
        })

        # Output: Weather distributions and stats
        output$weather_dist1 <- renderPlot({
          req(survey_weather(), haz_vars())
          df <- survey_weather()
          hv <- haz_vars()[1]

          if (!"countryyear" %in% names(df) && all(c("economy", "year") %in% names(df))) {
            df <- dplyr::mutate(df, countryyear = paste0(economy, ", ", year))
          }

          label <- selected_weather()$label[1]
          ridge_distribution_plot(
            df,
            x_var = hv,
            x_label = paste0(label, "\n(as configured)")
          )
        })

        output$weather_dist2 <- renderPlot({
          req(length(haz_vars()) > 1, survey_weather(), haz_vars())
          df <- survey_weather()
          hv <- haz_vars()[2]

          if (!"countryyear" %in% names(df) && all(c("economy", "year") %in% names(df))) {
            df <- dplyr::mutate(df, countryyear = paste0(economy, ", ", year))
          }

          label <- selected_weather()$label[2]
          ridge_distribution_plot(
            df,
            x_var = hv,
            x_label = paste0(label, "\n(as configured)"),
            wrap_width = 40
          )
        })

        output$binscatter1 <- renderPlot({
          req(survey_weather(), haz_vars(), selected_outcome())
          df <- survey_weather()
          hv <- haz_vars()[1]
          so <- selected_outcome()
          y_var <- so$name[1]

          if (is.null(y_var) || !(y_var %in% names(df))) {
            plot.new(); title(main = "Outcome variable not available")
            return(invisible(NULL))
          }

          x_label <- selected_weather()$label[1]
          y_label <- so$label[1]

          df_plot <- df |>
            dplyr::filter(is.finite(.data[[hv]]), is.finite(.data[[y_var]]))

          if (nrow(df_plot) == 0) {
            plot.new(); title(main = "No data available")
            return(invisible(NULL))
          }

          is_binary <- length(unique(df_plot[[y_var]])) <= 2
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]], y = .data[[y_var]]))
          if (is_binary) {
            p <- p + ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", linewidth = 1, geom = "line")
          } else {
            p <- p + ggplot2::geom_point(alpha = 0.1) +
              ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", size = 2, geom = "point")
          }

          p <- p +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = "",
              x = stringr::str_wrap(paste0(x_label, "\n(as configured)"), 40),
              y = stringr::str_wrap(y_label, 40)
            )

          if (is_binary) {
            p <- p + ggplot2::annotate(
              "text",
              x = Inf,
              y = Inf,
              label = "Binary outcome: mean by bin",
              hjust = 1.05,
              vjust = 1.2,
              size = 3
            )
          }

          p
        })

        output$binscatter2 <- renderPlot({
          req(length(haz_vars()) > 1, survey_weather(), haz_vars(), selected_outcome())
          df <- survey_weather()
          hv <- haz_vars()[2]
          so <- selected_outcome()
          y_var <- so$name[1]

          if (is.null(y_var) || !(y_var %in% names(df))) {
            plot.new(); title(main = "Outcome variable not available")
            return(invisible(NULL))
          }

          x_label <- selected_weather()$label[2]
          y_label <- so$label[1]

          df_plot <- df |>
            dplyr::filter(is.finite(.data[[hv]]), is.finite(.data[[y_var]]))

          if (nrow(df_plot) == 0) {
            plot.new(); title(main = "No data available")
            return(invisible(NULL))
          }

          is_binary <- length(unique(df_plot[[y_var]])) <= 2
          p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]], y = .data[[y_var]]))
          if (is_binary) {
            p <- p + ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", linewidth = 1, geom = "line")
          } else {
            p <- p + ggplot2::geom_point(alpha = 0.1) +
              ggplot2::stat_summary_bin(fun = "mean", bins = 20, color = "orange", size = 2, geom = "point")
          }

          p <- p +
            ggplot2::theme_minimal() +
            ggplot2::labs(
              title = "",
              x = stringr::str_wrap(paste0(x_label, "\n(as configured)"), 40),
              y = stringr::str_wrap(y_label, 40)
            )

          if (is_binary) {
            p <- p + ggplot2::annotate(
              "text",
              x = Inf,
              y = Inf,
              label = "Binary outcome: mean by bin",
              hjust = 1.05,
              vjust = 1.2,
              size = 3
            )
          }

          p
        })

        output$weather_stats <- DT::renderDT({
          req(survey_weather(), haz_vars())
          weighted_summary_long(survey_weather(), vars = haz_vars())
        }, rownames = FALSE)

        output$weather_varlist <- DT::renderDT({
          req(selected_weather())
          selected_weather()
        }, rownames = FALSE)

        output$bin_cutoffs_display <- renderText({
          cutoffs <- bin_cutoffs()
          if (length(cutoffs) == 0) {
            "No binned variables"
          } else {
            paste(
              sapply(names(cutoffs), function(var) {
                paste0(var, ": ", paste(round(cutoffs[[var]], 3), collapse = ", "))
              }),
              collapse = "\n"
            )
          }
        })

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
            DT::DTOutput(ns("weather_stats")),
            shiny::br(),
            shiny::h4("Bin cutoffs (if applicable)"),
            shiny::verbatimTextOutput(ns("bin_cutoffs_display")),
            shiny::br(),
            shiny::h4("Weather variable configuration"),
            DT::DTOutput(ns("weather_varlist"))
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
    
    # Return API
    list(
      bin_cutoffs = bin_cutoffs
    )
  })
}