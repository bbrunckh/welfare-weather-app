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
mod_1_04_weather_server <- function(id, variable_list, selected_surveys, survey_data
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$weather_selector_ui <- renderUI({

      # get weather variables from variable_list
      wl <- variable_list() |> 
        dplyr::filter(weather == 1) |> 
        dplyr::select(name, label, units)

      shiny::selectizeInput(
        inputId = ns("weather_variable_selector"),
        label = "Weather variables",
        choices = setNames(wl$name, wl$label),
        selected = c(wl$name[1]),
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

      # get weather variables from variable_list
      wl <- variable_list() |> 
        dplyr::filter(weather == 1) |> 
        dplyr::select(name, label, units)

      ui_list <- lapply(seq_along(input$weather_variable_selector), function(i) {
        current_var_name <- input$weather_variable_selector[i]
        current_var_info <- wl[wl$name == current_var_name, ]
        id_prefix <- paste0(current_var_info$name, "_")

        tagList(
          shiny::p(paste0(current_var_info$label, ":")),
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
              if (current_var_info$units %in% c("days", "mm")) {
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
              if (current_var_info$units %in% c("")) {
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
                    choices = c("Equal frequency", "Equal width", "K-means")
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = paste0("input['", ns(paste0(id_prefix, "contOrBinned")), "'] == 'Continuous'"),
                shiny::checkboxGroupInput(
                  inputId = ns(paste0(id_prefix, "polynomial")),
                  label = "Include polynomial terms",
                  choices = c("Quadratic" = "2", "Cubic" = "3")
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
  
  # Get basic variable info from variable_list
  var_info <- variable_list() |> 
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
    num_bins <- if (cont_binned == "Binned") input[[paste0(id_prefix, "numBins")]] %||% 5 else NA_integer_
    binning_method <- if (cont_binned == "Binned") input[[paste0(id_prefix, "binningMethod")]] %||% "Equal frequency" else NA_character_

    poly <- input[[paste0(id_prefix, "polynomial")]] %||% character(0)

    tibble::tibble(
      name = v,
      ref_start = ref_start,
      ref_end = ref_end,
      temporalAgg = temporal_agg,
      transformation = transformation,
      cont_binned = cont_binned,
      num_bins = num_bins,
      binning_method = binning_method,
      polynomial = list(poly)
    )
  })
  
  specs_df <- dplyr::bind_rows(specs)
  
  # Join variable info with specifications
  var_info |>
    dplyr::left_join(specs_df, by = "name")
})
    # --------- Module return API -----------
    list(
      selected_weather = selected_weather
    )
  })
}
