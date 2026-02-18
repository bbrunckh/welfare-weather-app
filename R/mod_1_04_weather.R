#' 1_04_weather UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_1_04_weather_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("weather_selector_ui")),
    uiOutput(ns("weather_construction_ui"))
  )
}

#' 1_04_weather Server Functions
#'
#' @noRd
mod_1_04_weather_server <- function(id, varlist, selected_surveys, survey_data, survey_h3 
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$weather_selector_ui <- renderUI({

      # get weather variables from varlist
      wl <- varlist() |> 
        dplyr::filter(weather == 1) |> 
        dplyr::select(name, label, units)

      shiny::selectizeInput(
        inputId = ns("weather_variable_selector"),
        label = "Weather variables",
        choices = setNames(wl$name, wl$label),
        selected = c(wl$name[1], wl$name[10]),
        multiple = TRUE,
        options = list(
          placeholder = "Select up to 2 weather variables",
          maxItems = 2
        )
      )
    })

    # Dynamically render weather variable constructor based on variable(s) chosen
    output$weather_construction_ui <- renderUI({
      req(input$weather_variable_selector)

      # get weather variables from varlist
      wl <- varlist() |> 
        dplyr::filter(weather == 1) |> 
        dplyr::pull(name, label, units)

      ui_list <- lapply(seq_along(input$weather_variable_selector), function(i) {
        current_var_name <- input$weather_variable_selector[i]
        id_prefix <- paste0(wl[wl$name == current_var_name, "name"], "_")

        tagList(
          shiny::p(paste0(current_var_name, ":")),
          shiny::actionButton(ns(paste0(id_prefix, "toggle")), "Configure"),
          shiny::conditionalPanel(
            condition = paste0("input['", ns(paste0(id_prefix, "toggle")), "'] % 2 == 1"),
            tagList(
              if (i > 1) shiny::hr(),
              shiny::h5("Reference period"),
              shiny::sliderInput(
                ns(paste0(id_prefix, "relativePeriod")),
                "Months before interview",
                min = 0,
                max = 12,
                value = c(1, 1)
              ),
              if (wl[wl$name == current_var_name, ]$units %in% c("days", "mm")) {
                shiny::selectInput(
                  ns(paste0(id_prefix, "temporalAgg")),
                  "Aggregation over reference period:",
                  choices = c("Mean", "Median", "Min", "Max", "Sum"),
                  selected = "Sum"
                )
              } else {
                shiny::selectInput(
                  ns(paste0(id_prefix, "temporalAgg")),
                  "Aggregation over reference period:",
                  choices = c("Mean", "Median", "Min", "Max"),
                  selected = "Mean"
                )
              },
              if (wl[wl$name == current_var_name, ]$units %in% c("")) {
                shiny::radioButtons(
                  ns(paste0(id_prefix, "varConstruction")),
                  "Transformation",
                  choices = c("Standardized anomaly"),
                  selected = "Standardized anomaly"
                )
              } else {
                shiny::radioButtons(
                  ns(paste0(id_prefix, "varConstruction")),
                  "Transformation",
                  choices = c("None", "Deviation from mean", "Standardized anomaly"),
                  selected = "None"
                )
              },
              shiny::radioButtons(
                ns(paste0(id_prefix, "contOrBinned")),
                "Continuous or binned",
                choices = c("Continuous", "Binned")
              ),
              shiny::helpText("Binned option is yet to be implemented.", style = "color: red; font-size: 12px;"),
              shiny::conditionalPanel(
                condition = paste0("input['", ns(paste0(id_prefix, "contOrBinned")), "'] == 'Binned'"),
                tagList(
                  shiny::sliderInput(
                    ns(paste0(id_prefix, "numBins")),
                    "Number of bins:",
                    min = 2,
                    max = 10,
                    value = 5
                  ),
                  shiny::radioButtons(
                    ns(paste0(id_prefix, "binningMethod")),
                    "Binning method:",
                    choices = c("Equal frequency", "Equal size", "K-means clustering")
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = paste0("input['", ns(paste0(id_prefix, "contOrBinned")), "'] == 'Continuous'"),
                shiny::checkboxGroupInput(
                  inputId = ns(paste0(id_prefix, "polynomial")),
                  label = "Include polynomial terms",
                  choices = c("Quadratic" = "a", "Cubic" = "b")
                )
              )
            )
          )
        )
      })

      tagList(
        do.call(tagList, ui_list),
        shiny::hr()
      )
    })

selected_weather <- reactive({
  req(input$weather_variable_selector)
  
  # Get basic variable info from varlist
  var_info <- varlist() |> 
    dplyr::filter(weather == 1) |>
    dplyr::filter(name %in% input$weather_variable_selector) |>
    dplyr::select(name, label, units)
  
  # Get hazard specifications for each variable
  vars <- input$weather_variable_selector
  specs <- lapply(vars, function(v) {
    id_prefix <- paste0(v, "_")
    
    # Get variable units
    units <- var_info[var_info$name == v, "units"]
    
    # Get input values with defaults
    ref_period <- input[[paste0(id_prefix, "relativePeriod")]] %||% c(1, 1)
    ref_start <- as.integer(ref_period[1])
    ref_end <- as.integer(ref_period[2])
    
    temporal_default <- if (!is.na(units) && units %in% c("days", "mm")) "Sum" else "Mean"
    temporal_agg <- input[[paste0(id_prefix, "temporalAgg")]] %||% temporal_default
    
    trans_default <- if (!is.na(units) && units %in% c("")) "Standardized anomaly" else "None"
    transformation <- input[[paste0(id_prefix, "varConstruction")]] %||% trans_default
    
    cont_binned <- input[[paste0(id_prefix, "contOrBinned")]] %||% "Continuous"
    
    poly <- input[[paste0(id_prefix, "polynomial")]] %||% character(0)
    poly_str <- if (length(poly) > 0) paste(poly, collapse = ", ") else ""
    
    tibble::tibble(
      name = v,
      ref_start = ref_start,
      ref_end = ref_end,
      temporalAgg = temporal_agg,
      transformation = transformation,
      cont_binned = cont_binned,
      polynomial = poly_str
    )
  })
  
  specs_df <- dplyr::bind_rows(specs)
  
  # Join variable info with specifications
  var_info |>
    dplyr::left_join(specs_df, by = "name")
})

    weather_data <- reactive({
      req(selected_surveys(), selected_weather(), survey_data(), survey_h3())
      
      # get survey dates from survey data 
      df <- survey_data()

      # int_year and int_month variables, create timestamp date variable as first day of interview month
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
      
      # load weather data for selected surveys
      codes <- unique(selected_surveys()$code)
      root <- unique(dirname(selected_surveys()$fpath))
      paths_weather <- file.path(root, paste0(codes, "_weather.parquet"))
      weather <- read_parquet_duckdb(paths_weather)

      weather |>
        # keep h3 index, timestamp and selected weather variables
        dplyr::select(h3, timestamp, input$weather_variable_selector ) |>
        # filter to survey dates and h3 in survey, and distinct
        dplyr::filter(h3 %in% survey_h3()$h3) |>
        dplyr::filter(timestamp %in% weather_dates) |>
        dplyr::distinct()
    })

    h3_weather <- reactive({
      req(weather_data(), survey_h3())
      out <- NULL

      for (i in input$weather_variable_selector) {
        id_prefix <- paste0(i, "_")
        ref_period <- input[[paste0(id_prefix, "relativePeriod")]]
        if (is.null(ref_period) || length(ref_period) < 2 || anyNA(ref_period)) {
          ref_period <- c(1, 1)
        }
        ref_start <- as.numeric(ref_period[1])
        ref_end <- as.numeric(ref_period[2])

        temporal_agg <- input[[paste0(id_prefix, "temporalAgg")]] %||% ""
        transformation <- input[[paste0(id_prefix, "varConstruction")]] %||% ""
        cont_binned <- input[[paste0(id_prefix, "contOrBinned")]] %||% ""

        weather <- weather_data() |>
          dplyr::group_by(.data$h3)

        if (is.finite(ref_start) && is.finite(ref_end)) {
          for (l in seq(ref_start, ref_end)) {
          colname <- paste0(i, "_", l)
          weather <- weather |>
            dplyr::mutate(!!rlang::sym(colname) := dplyr::lag(.data[[i]], n = l, order_by = .data$timestamp))
          }
        }

        if (is.finite(ref_start) && is.finite(ref_end)) {
          end_col <- paste0(i, "_", max(ref_start, ref_end))
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

        if (temporal_agg == "Mean") {
          weather <- weather |>
            dplyr::mutate(haz = rowMeans(dplyr::across(dplyr::starts_with(paste0(i, "_")))))
        }
        if (temporal_agg == "Median") {
          weather <- weather |>
            dplyr::mutate(haz = apply(dplyr::select(., dplyr::starts_with(paste0(i, "_"))), 1, stats::median, na.rm = TRUE))
        }
        if (temporal_agg == "Min") {
          weather <- weather |>
            dplyr::mutate(haz = do.call(pmin, c(dplyr::across(dplyr::starts_with(paste0(i, "_"))), na.rm = TRUE)))
        }
        if (temporal_agg == "Max") {
          weather <- weather |>
            dplyr::mutate(haz = do.call(pmax, c(dplyr::across(dplyr::starts_with(paste0(i, "_"))), na.rm = TRUE)))
        }
        if (temporal_agg == "Sum") {
          weather <- weather |>
            dplyr::mutate(haz = rowSums(dplyr::across(dplyr::starts_with(paste0(i, "_")))))
        }

        if (!"haz" %in% names(weather)) {
          weather <- weather |>
            dplyr::mutate(haz = NA_real_)
        }

        if (!(transformation == "None" || i %in% c("spi6", "spei6")) && "haz" %in% names(weather)) {
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

        if (cont_binned == "Binned") {
          # not implemented yet
        }

        weather <- weather |>
          dplyr::select(.data$h3, .data$timestamp, .data$haz) |>
          dplyr::rename_with(~ paste0("haz_", i), .cols = dplyr::starts_with("haz")) |>
          dplyr::arrange(.data$h3, .data$timestamp)

        if (is.null(out)) {
          out <- weather
        } else {
          out <- dplyr::full_join(out, weather, by = c("h3", "timestamp"))
        }
      }

      out
    })

    loc_weather <- reactive({
      req(h3_weather())
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
      req(loc_weather())
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

    list(
      selected_weather = selected_weather,
      h3_weather = h3_weather,
      loc_weather = loc_weather,
      survey_weather = survey_weather
    )
  })
}
