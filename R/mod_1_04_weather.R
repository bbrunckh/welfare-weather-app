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

      choice_labels <- wl$label
      choice_map <- stats::setNames(wl$name, choice_labels)

      # INT-01: keep the user's variable selection when the choice set is
      # rebuilt; fall back to the first variable only when nothing survives.
      prev_sel <- shiny::isolate(input$weather_variable_selector)
      shiny::selectizeInput(
        inputId  = ns("weather_variable_selector"),
        label    = "Weather variables",
        choices  = choice_map,
        selected = .restore_selection(prev_sel, wl$name, fallback = wl$name[1]),
        multiple = TRUE,
        options  = list(
          placeholder = "Select up to 2 weather variables",
          maxItems    = 2,
          # Selectize otherwise sorts the displayed labels alphabetically.
          sortField  = list(field = "$order", direction = "asc")
        )
      )
    })

    # ---- Per-variable configuration UI --------------------------------------

    output$weather_construction_ui <- renderUI({
      req(input$weather_variable_selector)
      wl <- weather_vars()

      n_vars <- length(input$weather_variable_selector)
      # UI-54: one flyout for the whole selection rather than one per
      # variable. Two adjacent "Configure" buttons distinguished only by a
      # variable name read as a list of unrelated settings; a single
      # "Weather variable(s)" control with a section per selected variable
      # says what it configures without depending on the labels beside it.
      ui_list <- lapply(seq_along(input$weather_variable_selector), function(i) {
        v        <- input$weather_variable_selector[i]
        var_info <- wl[wl$name == v, ]
        units    <- as.character(var_info$units[1])
        display_label <- sub("^Monthly\\s+", "", as.character(var_info$label[1]))
        prefix   <- paste0(v, "_")

        tagList(
          # Section heading inside the shared flyout; with one variable
          # selected it is redundant with the flyout title, so it is dropped.
          if (n_vars > 1) tagList(
            if (i > 1) hr(class = "wx-flyout-sep"),
            tags$div(class = "wx-flyout-section", display_label)
          ),
          tagList(

              shiny::sliderInput(
                ns(paste0(prefix, "relativePeriod")),
                "Months before interview",
                min   = 0, max = 12,
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
                shiny::selectInput(
                  ns(paste0(prefix, "temporalAgg")),
                  "Aggregation over reference period:",
                  choices  = temporal_agg_choices(units),
                  selected = temporal_agg_default(units)
                )
              ),
              pill_toggle(
                ns(paste0(prefix, "varConstruction")),
                label = "Transformation",
                choices  = transformation_choices(units),
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
                    "Number of bins:",
                    min = 2, max = 10, value = 5
                  ),
                  pill_toggle(
                    ns(paste0(prefix, "binningMethod")),
                    label = "Binning method:",
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
      })

      # Options render in a floating panel beside the sidebar (.config-flyout
      # in custom.css) so they are visible without scrolling. Content stays in
      # the DOM at all times, so input defaults register immediately. UI-02:
      # anchored to its toggle, one-open state, aria-expanded, focus
      # management, Escape to close.
      config_flyout_block(
        ns("wx_settings_toggle"),
        if (n_vars > 1) "Weather variable settings"
        else paste0(sub("^Monthly\\s+", "",
                        as.character(wl$label[wl$name ==
                          input$weather_variable_selector[1]][1])),
                    " settings"),
        do.call(tagList, ui_list),
        display_label = "Weather variable(s)"
      )
    })

    # ---- Historical comparison config ---------------------------------------
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
              label = shiny::tags$span(
                class = "visually-hidden",
                "Historical comparison period"
              ),
              min = 1950,
              max = this_year,
              value = c(1991, 2020),
              sep = ""
            ),
            shiny::helpText(
              paste(
                "Weather over these years for the same locations and the same",
                "calendar months each wave was fielded in. It is drawn",
                "alongside the sample in the weather distribution plots and",
                "backs the within-location map views. Widening the range",
                "means loading more years, which takes longer."
              ),
              style = "font-size: 12px;"
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
        tmp <- yf; yf <- yt; yt <- tmp
      }
      c(from = yf, to = yt)
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

    # ---- Live weather configuration card ------------------------------------
    # Same pipeline card as on the Weather stats tab, but bound to the live
    # selection so it doubles as instant config feedback in the sidebar.
    # Headerless and stripped back: the variable row + stages speak for
    # themselves; the history range rides in the badge.

    output$weather_summary_ui <- renderUI({
      sw <- tryCatch(selected_weather(), error = function(e) NULL)
      if (is.null(sw) || nrow(sw) == 0) return(NULL)

      hy <- hist_years()
      selection_summary_card(
        title   = NULL,
        rows    = weather_pipeline_rows(sw, hist_years = hy),
        info    = paste(
          "Each row reads left to right: the reference window (months before",
          "each interview), how those months are aggregated into one value",
          "(shown only when the window spans several months), any",
          "transformation against the historical mean, and the form the",
          "variable takes in the model (bins or continuous curve). The",
          "history badge is the comparison period: same locations and",
          "calendar months, per survey wave."
        ),
        compact = TRUE
      )
    })

    # ---- Module return API --------------------------------------------------

    # UI-73: rendered eagerly so the controls exist before their accordion
    # panel is opened. Sidebars open one panel at a time and hidden outputs
    # are suspended, so a setting restored from a configuration had nothing to
    # land on until the user happened to visit that panel - which is why an
    # import reported dozens of controls that "never appeared".
    lapply(c("weather_selector_ui", "weather_construction_ui"), function(out_id) {
      shiny::outputOptions(output, out_id, suspendWhenHidden = FALSE)
    })

    list(
      selected_weather = selected_weather,
      hist_years       = hist_years
    )
  })
}
