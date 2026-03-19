#' 2_01_historical UI Function
#'
#' @description A shiny Module. Selects the historical climate configuration
#'   (SSP baseline, reference period, residual handling).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_01_historical_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("hist_years_select")),
    uiOutput(ns("hist_sim_specs_button")),
    uiOutput(ns("hist_sim_specs"))
  )
}
#' 2_01_historical Server Functions
#'
#' Manages historical climate variable selection and reference period
#' configuration. Returns selected_hist for use by mod_2_02_historical_sim.
#'
#' @param id Module id.
#' @noRd
mod_2_01_historical_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Climate years selector ---------------------------------------------
    # Rendered as plain UI (not reactive renderUI) since it never needs
    # to change dynamically after mount.

    output$hist_years_select <- renderUI({
      tagList(
        shiny::sliderInput(
          inputId = ns("hist_years"),
          label   = "Period defining the historical weather distribution",
          min     = 1950,
          max     = 2024,
          value   = c(1994, 2024),
          sep     = ""
        ),
        uiOutput(ns("hist_years_warning")),
        helpText(
          tags$b("Note:"), " Historical weather data is used to inform the",
          " underlying variability of weather, which is then perturbed with",
          " climate scenario forecasts to estimate outcomes given this",
          " perturbation and historical variability. There is therefore a",
          " balance between additional years (which can encompass rarer events)",
          " and the representativeness of this underlying weather data, which",
          " may shift due to climate change.",
          tags$b(" 30 years (e.g. 1994-2024) is the recommended default."),
          style = "font-size: 11px; color: #555; margin-top: 4px;"
        ),
        textInput(
          inputId = ns("scenario_name"),
          label   = "Scenario name",
          value   = "Historical / 1994-2024"
        )
      )
    })

    # ---- 20-year minimum window warning ------------------------------------

    output$hist_years_warning <- renderUI({
      req(input$hist_years)
      if ((input$hist_years[2] - input$hist_years[1]) < 20) {
        helpText(
          "\u26a0\ufe0f Window is less than 20 years. Results may be unreliable.",
          style = "color: #c0392b; font-size: 12px;"
        )
      }
    })

    # ---- Auto-update scenario name when years change -----------------------

    prev_hist_years <- reactiveVal(c(1994L, 2024L))

    observeEvent(input$hist_years, {
      req(input$hist_years)
      yr  <- input$hist_years
      old <- paste0("Historical / ", prev_hist_years()[1], "-", prev_hist_years()[2])
      if (!is.null(input$scenario_name) &&
          (input$scenario_name == old || input$scenario_name == "Historical")) {
        updateTextInput(session, "scenario_name",
                        value = paste0("Historical / ", yr[1], "-", yr[2]))
      }
      prev_hist_years(yr)
    }, ignoreInit = TRUE)

    # ---- Simulation parameters toggle ---------------------------------------

    hist_sim_specs_open <- reactiveVal(FALSE)

    output$hist_sim_specs_button <- renderUI({
      shiny::actionButton(ns("hist_sim_specs_toggle"),
                          "Simulation parameters",
                          style = "margin-bottom:12px;")
    })

    observeEvent(input$hist_sim_specs_toggle, {
      hist_sim_specs_open(!isTRUE(hist_sim_specs_open()))
    })

    # ---- Simulation parameters panel ----------------------------------------
    # Uses shared residual_method_ui() from fct_simulations.R

    output$hist_sim_specs <- renderUI({
      if (!isTRUE(hist_sim_specs_open())) return(NULL)
      residual_method_ui(ns, "hist_sim_residuals")
    })

    # ---- Selected historical weather simulation configuration ---------------

    selected_hist <- reactive({
      req(input$hist_years)
      data.frame(
        type          = "historical",
        year_range    = I(list(input$hist_years)),
        residuals     = input$hist_sim_residuals  %||% "original",
        scenario_name = as.character(input$scenario_name %||% "Historical"),
        stringsAsFactors = FALSE
      )
    })

    # ---- Module return API --------------------------------------------------

    list(selected_hist = selected_hist)
  })
}

