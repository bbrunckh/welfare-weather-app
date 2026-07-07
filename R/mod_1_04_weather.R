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
    uiOutput(ns("weather_summary_ui")),
    wellPanel(
      uiOutput(ns("weather_selector_ui")),
      uiOutput(ns("weather_construction_ui"))
    )
  )
}

#' 1_04_weather Server Functions
#'
#' @param id              Module id.
#' @param variable_list   Reactive data frame of variable metadata.
#' @param selected_surveys Reactive data frame of selected surveys.
#' @param survey_data     Reactive data frame of loaded survey data.
#'
#' @noRd
mod_1_04_weather_server <- function(id, variable_list, selected_surveys, survey_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Weather variable choices -------------------------------------------

    weather_vars <- reactive({
      req(variable_list())
      get_weather_vars(variable_list())
    })

    # ---- Variable selector --------------------------------------------------

    output$weather_selector_ui <- renderUI({
      wl <- weather_vars()

      choice_labels <- paste0(wl$label, " (", wl$name, ")")
      choice_map <- stats::setNames(wl$name, choice_labels)

      shiny::selectizeInput(
        inputId  = ns("weather_variable_selector"),
        label    = "Weather variables",
        choices  = choice_map,
        selected = wl$name[1],
        multiple = TRUE,
        options  = list(
          placeholder = "Select up to 2 weather variables",
          maxItems    = 2
        )
      )
    })

    # ---- Per-variable configuration UI --------------------------------------

    output$weather_construction_ui <- renderUI({
      req(input$weather_variable_selector)
      wl <- weather_vars()

      n_vars <- length(input$weather_variable_selector)
      ui_list <- lapply(seq_along(input$weather_variable_selector), function(i) {
        v        <- input$weather_variable_selector[i]
        var_info <- wl[wl$name == v, ]
        units    <- as.character(var_info$units[1])
        prefix   <- paste0(v, "_")

        tagList(
          if (i > 1 && n_vars > 1) hr(),
          tags$p(tags$strong(paste0(var_info$label, ":")),
                 style = "font-size: 15px;"),
          shiny::actionButton(ns(paste0(prefix, "toggle")), "Configure",
                 icon  = shiny::icon("sliders"),
                 class = "btn-outline-primary btn-sm",
                 style = "margin-bottom: 10px;"),
          # Options render in a floating panel beside the sidebar
          # (.config-flyout in custom.css) so they are visible without
          # scrolling. Content stays in the DOM at all times, so input
          # defaults register immediately.
          shiny::conditionalPanel(
            condition = paste0("input['", ns(paste0(prefix, "toggle")), "'] % 2 == 1"),
            class     = "config-flyout",
            div(
              class = "config-flyout-header",
              tags$h6(paste0(var_info$label, " settings")),
              tags$button(
                type    = "button",
                class   = "btn-close",
                `aria-label` = "Close",
                onclick = sprintf(
                  "document.getElementById('%s').click();",
                  ns(paste0(prefix, "toggle"))
                )
              )
            ),
            tagList(

              shiny::sliderInput(
                ns(paste0(prefix, "relativePeriod")),
                "Months before interview",
                min   = 0, max = 12,
                value = c(1, 1)
              ),
              shiny::selectInput(
                ns(paste0(prefix, "temporalAgg")),
                "Aggregation over reference period:",
                choices  = temporal_agg_choices(units),
                selected = temporal_agg_default(units)
              ),
              shiny::radioButtons(
                ns(paste0(prefix, "varConstruction")),
                "Transformation",
                choices  = transformation_choices(units),
                selected = transformation_default(units)
              ),
              shiny::radioButtons(
                ns(paste0(prefix, "contOrBinned")),
                "Continuous or binned",
                choices = c("Binned", "Continuous")
              ),
              shiny::conditionalPanel(
                condition = paste0("input['", ns(paste0(prefix, "contOrBinned")), "'] == 'Binned'"),
                tagList(
                  shiny::sliderInput(
                    ns(paste0(prefix, "numBins")),
                    "Number of bins:",
                    min = 2, max = 10, value = 5
                  ),
                  shiny::radioButtons(
                    ns(paste0(prefix, "binningMethod")),
                    "Binning method:",
                    choices = c("Equal frequency", "Equal width", "K-means", "Custom")
                  ),
                  shiny::conditionalPanel(
                    condition = paste0("input['", ns(paste0(prefix, "binningMethod")), "'] == 'Custom'"),
                    tagList(
                      shiny::textInput(
                        ns(paste0(prefix, "customBreaks")),
                        label = "Breaks (comma-separated)",
                        value = "",
                        placeholder = "e.g. 20, 25, 30, 35"
                      ),
                      shiny::helpText(
                        paste0(
                          "Provide N-1 numeric values for N bins, in the variable's units. ",
                          "Values bound the interior bin edges; the outermost bins are open-ended ",
                          "(-Inf and +Inf) so out-of-range values still map to the extreme bins."
                        ),
                        style = "font-size: 12px;"
                      )
                    )
                  ),
                  shiny::helpText(
                    "Binning keeps only unique bins, duplicates are dropped. This can lead to fewer bins than specified.",
                    style = "color: red; font-size: 12px;"
                  )
                )
              ),
              shiny::conditionalPanel(
                condition = paste0("input['", ns(paste0(prefix, "contOrBinned")), "'] == 'Continuous'"),
                shiny::checkboxGroupInput(
                  inputId = ns(paste0(prefix, "polynomial")),
                  label   = "Include polynomial terms",
                  choices = c("Quadratic" = "2", "Cubic" = "3")
                )
              )
            )
          )
        )
      })

      tagList(do.call(tagList, ui_list))
    })

    # ---- Selected weather spec ----------------------------------------------

    selected_weather <- reactive({
      req(input$weather_variable_selector)
      wl   <- weather_vars()
      vars <- input$weather_variable_selector

      # Collect only the specific per-variable spec inputs.
      # Use isolate() on everything EXCEPT the spec inputs themselves so that
      # clicking the "Configure" toggle button (an actionButton whose count
      # increments on each click) does NOT invalidate this reactive and cause
      # the Weather stats tab to continuously reload.
      spec_keys <- c(
        "relativePeriod", "temporalAgg", "varConstruction",
        "contOrBinned", "numBins", "binningMethod", "customBreaks",
        "polynomial"
      )
      spec_input_names <- unlist(lapply(vars, function(v) paste0(v, "_", spec_keys)))

      # Read only the spec inputs reactively; toggle counts are NOT observed.
      spec_inputs <- lapply(
        setNames(spec_input_names, spec_input_names),
        function(k) input[[k]]
      )

      build_selected_weather(
        selected_vars = vars,
        var_info      = wl,
        spec_inputs   = spec_inputs
      )
    })

    # ---- Settings summary banner --------------------------------------------

    output$weather_summary_ui <- renderUI({
      sw <- tryCatch(selected_weather(), error = function(e) NULL)
      if (is.null(sw) || nrow(sw) == 0) return(NULL)

      rows <- lapply(seq_len(nrow(sw)), function(i) {
        r <- sw[i, ]

        ref_txt <- if (identical(r$ref_start, r$ref_end)) {
          paste0(r$ref_start, if (r$ref_start == 1) " month" else " months",
                 " before interview")
        } else {
          paste0(r$ref_start, "–", r$ref_end, " months before interview")
        }

        form_txt <- if (identical(r$cont_binned, "Binned")) {
          paste0("binned × ", r$num_bins, " (", tolower(r$binning_method), ")")
        } else {
          poly <- unlist(r$polynomial)
          if (length(poly) > 0) {
            paste0("continuous, polynomial ", paste(poly, collapse = "+"))
          } else "continuous"
        }

        parts <- c(
          paste0(r$temporalAgg, " over ", ref_txt),
          if (!identical(r$transformation, "None")) tolower(r$transformation),
          form_txt
        )

        tagList(
          tags$b(paste0(r$label, ":")),
          paste0(" ", paste(parts, collapse = " · ")),
          tags$br()
        )
      })

      div(class = "settings-summary", rows)
    })

    # ---- Module return API --------------------------------------------------

    list(
      selected_weather = selected_weather
    )
  })
}