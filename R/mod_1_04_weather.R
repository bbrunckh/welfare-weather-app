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
mod_1_04_weather_server <- function(
    id,
    survey_data,
    survey_data_files,
    board,
    weather_list,
    varlist,
    data_loaded
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$weather_selector_ui <- renderUI({
      wl <- if (is.function(weather_list)) weather_list() else weather_list
      req(wl)
      shiny::selectizeInput(
        inputId = ns("weather_variable_selector"),
        label = "Weather variables",
        choices = wl$name,
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
      wl <- if (is.function(weather_list)) weather_list() else weather_list
      req(wl)

      ui_list <- lapply(seq_along(input$weather_variable_selector), function(i) {
        current_var_name <- input$weather_variable_selector[i]
        id_prefix <- paste0(wl[wl$name == current_var_name, "varname"], "_")

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
              if (wl[wl$name == current_var_name, ]$units %in% c("Dimensionless")) {
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

    survey_h3 <- reactive({
      req(data_loaded())
      files <- survey_data_files()
      brd <- board()
      req(files)

      pin_names <- paste0(files, "_H3")
      local_paths <- lapply(pin_names, function(pin) pins::pin_download(brd, pin))
      read_parquet_duckdb(unlist(local_paths))
    })

    weather_vars <- reactive({
      req(input$weather_variable_selector)
      wl <- if (is.function(weather_list)) weather_list() else weather_list
      wl |>
        dplyr::filter(.data$name %in% input$weather_variable_selector) |>
        dplyr::pull(.data$varname)
    })

    weather_settings <- reactive({
      req(input$weather_variable_selector)
      wl <- if (is.function(weather_list)) weather_list() else weather_list
      req(wl)

      vars <- wl |>
        dplyr::filter(.data$name %in% input$weather_variable_selector) |>
        dplyr::pull(.data$varname)

      polynomials <- lapply(vars, function(v) {
        input[[paste0(v, "_polynomial")]] %||% character(0)
      })

      data.frame(
        varname = vars,
        polynomial = I(polynomials),
        stringsAsFactors = FALSE
      )
    })

    weather_data <- reactive({
      req(data_loaded(), weather_vars())
      df <- survey_data()
      req(df)

      if (!"timestamp" %in% names(df)) {
        return(NULL)
      }
      survey_dates <- df |>
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

      files <- survey_data_files()
      brd <- board()
      pin_names <- paste0(unique(substr(files, 1, nchar(files) - 4)), "weather")
      local_paths <- lapply(pin_names, function(pin) pins::pin_download(brd, pin))
  weather <- read_parquet_duckdb(unlist(local_paths))

      weather |>
        dplyr::select(.data$h3_6, .data$timestamp, dplyr::all_of(weather_vars())) |>
        dplyr::filter(.data$h3_6 %in% survey_h3()$h3_6) |>
        dplyr::filter(.data$timestamp %in% weather_dates) |>
        dplyr::distinct()
    })

    h3_weather <- reactive({
      req(weather_vars(), weather_data())
      out <- NULL

      for (i in weather_vars()) {
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
          dplyr::group_by(.data$h3_6)

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
              .by = c(.data$h3_6, .data$month)
            )

          if (transformation == "Deviation from mean") {
            weather <- weather |>
              dplyr::left_join(climate_ref, by = c("h3_6", "month")) |>
              dplyr::mutate(haz = .data$haz - .data$mean)
          } else if (transformation == "Standardized anomaly") {
            weather <- weather |>
              dplyr::left_join(climate_ref, by = c("h3_6", "month")) |>
              dplyr::mutate(haz = (.data$haz - .data$mean) / .data$sd)
          }
        }

        if (cont_binned == "Binned") {
          # not implemented yet
        }

        weather <- weather |>
          dplyr::select(.data$h3_6, .data$timestamp, .data$haz) |>
          dplyr::rename_with(~ paste0("haz_", i), .cols = dplyr::starts_with("haz")) |>
          dplyr::arrange(.data$h3_6, .data$timestamp)

        if (is.null(out)) {
          out <- weather
        } else {
          out <- dplyr::full_join(out, weather, by = c("h3_6", "timestamp"))
        }
      }

      out
    })

    loc_weather <- reactive({
      req(weather_vars(), survey_h3(), h3_weather())
      h3 <- survey_h3()
      hw <- h3_weather()
      join_cols <- intersect(c("h3_6", "timestamp"), intersect(names(h3), names(hw)))
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
      req(weather_vars(), loc_weather())
      sw <- survey_data()
      lw <- loc_weather()
      join_cols <- intersect(c("code", "year", "survname", "loc_id", "timestamp"), intersect(names(sw), names(lw)))
      if (!length(join_cols)) {
        return(NULL)
      }

      sw <- sw |>
        dplyr::left_join(lw, by = join_cols) |>
        dplyr::mutate(year = as.factor(.data$year)) |>
        dplyr::group_by(.data$code, .data$year, .data$survname) |>
        dplyr::mutate(weight = .data$weight / sum(.data$weight, na.rm = TRUE)) |>
        dplyr::ungroup()

      vl <- if (is.function(varlist)) varlist() else varlist
      if (!is.null(vl)) {
        for (i in seq_len(nrow(vl))) {
          var_name <- vl$varname[i]
          var_label <- vl$label[i]
          if (var_name %in% names(sw)) {
            attr(sw[[var_name]], "label") <- var_label
          }
        }
      }

      sw
    })

    haz_vars <- reactive({
      paste0("haz_", weather_vars())
    })

    list(
      weather_vars = weather_vars,
      weather_settings = weather_settings,
      survey_h3 = survey_h3,
      weather_data = weather_data,
      h3_weather = h3_weather,
      loc_weather = loc_weather,
      survey_weather = survey_weather,
      haz_vars = haz_vars,
      weather_list = reactive({ if (is.function(weather_list)) weather_list() else weather_list })
    )
  })
}
