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
      uiOutput(ns("weather_construction_ui")),
      uiOutput(ns("hist_config_ui"))
    )
  )
}

#' 1_04_weather Server Functions
#'
#' @param id              Module id.
#' @param variable_list   Reactive data frame of variable metadata.
#'
#' @noRd
mod_1_04_weather_server <- function(id, variable_list) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Weather variable choices ----

    weather_vars <- reactive({
      req(variable_list())
      get_weather_vars(variable_list())
    })

    # Variable selector ----

    output$weather_selector_ui <- renderUI({
      wl <- weather_vars()

      choice_labels <- wl$label
      choice_map <- stats::setNames(wl$name, choice_labels)

      # INT-01: keep the user's variable selection when the choice set is
      # rebuilt; fall back to the first variable only when nothing survives.
      prev_sel <- shiny::isolate(input$weather_variable_selector)
      shiny::selectizeInput(
        inputId = ns("weather_variable_selector"),
        label = "Weather variables",
        choices = choice_map,
        selected = .restore_selection(prev_sel, wl$name, fallback = wl$name[1]),
        multiple = TRUE,
        options = list(
          placeholder = "Select up to 2 weather variables",
          maxItems = 2,
          # Selectize otherwise sorts the displayed labels alphabetically.
          sortField = list(field = "$order", direction = "asc")
        )
      )
    })

    # Per-variable configuration UI ----

    output$weather_construction_ui <- renderUI({
      req(input$weather_variable_selector)
      wl <- weather_vars()

      n_vars <- length(input$weather_variable_selector)
      ui_list <- lapply(seq_along(input$weather_variable_selector), function(i) {
        v <- input$weather_variable_selector[i]
        var_info <- wl[wl$name == v, ]
        units <- as.character(var_info$units[1])
        display_label <- wise_label_short(as.character(var_info$label[1]))
        prefix <- paste0(v, "_")

        tagList(
          if (i > 1 && n_vars > 1) hr(),
          # Options render in a floating panel beside the sidebar
          # (.config-flyout in custom.css) so they are visible without
          # scrolling. Content stays in the DOM at all times, so input
          # defaults register immediately. UI-02: anchored to its toggle,
          # one-open state, aria-expanded, focus management, Escape to close.
          config_flyout_block(
            ns(paste0(prefix, "toggle")),
            paste0(var_info$label, " settings"),
            tagList(
              shiny::sliderInput(
                ns(paste0(prefix, "relativePeriod")),
                "Months before interview",
                min = 0, max = 12,
                value = c(1, 1)
              ),
              # Aggregation is only meaningful when the window spans more
              # than one month; for a single month the value is that month's
              # own reading, so the selector is hidden.
              shiny::conditionalPanel(
                condition = paste0(
                  "input['", ns(paste0(prefix, "relativePeriod")),
                  "'][0] !== input['", ns(paste0(prefix, "relativePeriod")),
                  "'][1]"
                ),
                pill_toggle(
                  ns(paste0(prefix, "temporalAgg")),
                  "Aggregation over reference period",
                  choices  = temporal_agg_choices(units),
                  selected = temporal_agg_default(units)
                )
              ),
              pill_toggle(
                ns(paste0(prefix, "varConstruction")),
                label = "Transformation",
                choices = transformation_choices(units),
                selected = transformation_default(units),
                layout = "vertical"
              ),
              pill_toggle(
                ns(paste0(prefix, "contOrBinned")),
                label = "Continuous or binned",
                choices = c("Binned", "Continuous")
              ),
              shiny::conditionalPanel(
                condition = paste0("input['", ns(paste0(prefix, "contOrBinned")), "'] == 'Binned'"),
                tagList(
                  shiny::sliderInput(
                    ns(paste0(prefix, "numBins")),
                    "Number of bins",
                    min = 2, max = 10, value = 5
                  ),
                  pill_toggle(
                    ns(paste0(prefix, "binningMethod")),
                    label = shiny::tagList(
                      "Binning method",
                      info_popover(
                        title = "Binning",
                        shiny::p(
                          "Binning keeps only unique bins, so duplicate values are",
                          "dropped. This can result in fewer bins than specified."
                        )
                      )
                    ),
                    choices = c("Equal frequency", "Equal width", "K-means", "Custom"),
                    layout = "vertical"
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
            ),
            display_label = display_label
          )
        )
      })

      tagList(do.call(tagList, ui_list))
    })
    shiny::outputOptions(output, "weather_selector_ui",
      suspendWhenHidden = FALSE
    )
    shiny::outputOptions(output, "weather_construction_ui",
      suspendWhenHidden = FALSE
    )

    # Historical comparison config ----
    # The weather stats tab always draws each wave against its own climate
    # history, so the year range is a setting of the weather configuration
    # rather than a control on the results panel. 1991-2020 is the same
    # reference period the deviation / anomaly transformations use.

    output$hist_config_ui <- renderUI({
      req(input$weather_variable_selector)
      this_year <- as.integer(format(Sys.Date(), "%Y"))

      # Same shape as a weather variable's block above: heading, "Configure"
      # button, flyout with the settings - so the sidebar reads as one list of
      # configurable sections rather than a list plus an odd one out.
      tagList(
        hr(),
        config_flyout_block(
          ns("hist_toggle"),
          "Historical comparison settings",
          tagList(
            shiny::sliderInput(
              inputId = ns("hist_years"),
              label = shiny::tagList(
                "Historical comparison period",
                info_popover(
                  title = "Historical comparison period",
                  shiny::p(
                    paste(
                      "Weather over these years for the same locations and the same",
                      "calendar months each wave was fielded in. It is drawn",
                      "alongside the sample in the weather distribution plots and",
                      "backs the within-location map views. Widening the range",
                      "means loading more years, which takes longer."
                    )
                  )
                )
              ),
              min = 1950,
              max = this_year,
              value = c(1991, 2020),
              sep = ""
            )
          ),
          display_label = "Historical comparison"
        )
      )
    })
    shiny::outputOptions(output, "hist_config_ui", suspendWhenHidden = FALSE)

    # Falls back to 1991-2020 before the range slider has registered, and
    # orders the two years so a reversed range still loads.
    hist_years <- reactive({
      years <- suppressWarnings(as.integer(input$hist_years))
      if (length(years) != 2L || anyNA(years)) years <- c(1991L, 2020L)
      yf <- years[1]
      yt <- years[2]
      if (yf > yt) {
        tmp <- yf
        yf <- yt
        yt <- tmp
      }
      c(from = yf, to = yt)
    })

    # Selected weather spec ----

    selected_weather <- reactive({
      req(input$weather_variable_selector)
      wl <- weather_vars()
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

    # Live weather configuration card ----
    # Same pipeline card as on the Weather stats tab, but bound to the live
    # selection so it doubles as instant config feedback in the sidebar.
    # Headerless and stripped back: the variable row + stages speak for
    # themselves; the history range rides in the badge.

    output$weather_summary_ui <- renderUI({
      sw <- tryCatch(selected_weather(), error = function(e) NULL)
      if (is.null(sw) || nrow(sw) == 0) {
        return(NULL)
      }

      hy <- hist_years()
      single_weather <- nrow(sw) == 1L
      weather_title <- if (single_weather) {
        label <- wise_label_short(as.character(sw$label[1]))
        tags$span(
          class = "weather-sidebar-title",
          paste0(toupper(substr(label, 1, 1)), substr(label, 2, nchar(label)))
        )
      } else {
        NULL
      }
      weather_rows <- weather_pipeline_rows(sw)
      if (single_weather) {
        # Move the variable name into the card header, keeping its units in
        # the body beside the pipeline stages.
        weather_rows[[1]]$children[[1]]$children[[1]] <- NULL
      }
      tags$div(
        class = "weather-sidebar-summary",
        selection_summary_card(
          title   = weather_title,
          badge   = paste0("History ", hy[["from"]], "-", hy[["to"]]),
          rows    = weather_rows,
          compact = TRUE
        )
      )
    })

    # Module return API ----

    list(
      selected_weather = selected_weather,
      hist_years       = hist_years
    )
  })
}
