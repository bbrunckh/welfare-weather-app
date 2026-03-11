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
#' @param id               Module id.
#'
#' @noRd
mod_2_01_historical_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Climate years selector ---------------------------------------------

    output$hist_years_select <- renderUI({
      tagList(
        shiny::sliderInput(
          inputId = ns("hist_years"),
          label   = "Period defining the historical weather distribution",
          min     = 1950,
          max     = 2024,
          value   = c(1990, 2024),
          sep     = ""
        ),
        uiOutput(ns("hist_years_warning")),
        textInput(
          inputId = ns("scenario_name"),
          label   = "Scenario name",
          value   = "Historical / 1990–2024"
        )
      )
    })

    # ---- 20-year minimum window warning ------------------------------------

    output$hist_years_warning <- renderUI({
      req(input$hist_years)
      if ((input$hist_years[2] - input$hist_years[1]) < 20) {
        helpText(
          "⚠️ Window is less than 20 years. Results may be unreliable.",
          style = "color: #c0392b; font-size: 12px;"
        )
      }
    })

    # ---- Auto-update scenario name when years change -----------------------

    prev_hist_years <- reactiveVal(c(1990L, 2024L))

    observeEvent(input$hist_years, {
      req(input$hist_years)
      yr  <- input$hist_years
      old <- paste0("Historical / ", prev_hist_years()[1], "–", prev_hist_years()[2])
      if (!is.null(input$scenario_name) &&
          (input$scenario_name == old || input$scenario_name == "Historical")) {
        updateTextInput(session, "scenario_name",
                        value = paste0("Historical / ", yr[1], "–", yr[2]))
      }
      prev_hist_years(yr)
    }, ignoreInit = TRUE)

    # ---- Simulation parameters toggle ---------------------------------------

    hist_sim_specs_open <- reactiveVal(FALSE)

    output$hist_sim_specs_button <- renderUI({
      shiny::actionButton(ns("hist_sim_specs"), "Simulation parameters", style = "margin-bottom:12px;")
    })

    observeEvent(input$hist_sim_specs, {
      hist_sim_specs_open(!isTRUE(hist_sim_specs_open()))
    })

    # ---- Simulation parameters panel ----------------------------------------

    output$hist_sim_specs <- renderUI({
      if (!isTRUE(hist_sim_specs_open())) return(NULL)

      tagList(
        radioButtons(
          inputId  = ns("hist_sim_residuals"),
          label    = "Residuals method",
          choices  = residual_choices(),
          selected = "original"
        ),
        helpText(
          tags$b("none:"), " return fitted values only.", tags$br(),
          tags$b("original:"), " match each observation's own training residual by ID,",
          " preserving individual-level heterogeneity across simulation years.", tags$br(),
          tags$b("empirical:"), " resample residuals from the training distribution",
          " (non-parametric bootstrap).", tags$br(),
          tags$b("normal:"), " draw residuals from N(0, \u03c3) where \u03c3 is the",
          " training residual SD.",
          style = "font-size: 11px;"
        )
      )
    })

    # ---- Selected historical weather simulation configuration ---------------

    selected_hist <- reactive({
      data.frame(
        type          = "historical",
        year_range    = I(list(input$hist_years         %||% c(1990, 2024))),
        residuals     = input$hist_sim_residuals         %||% "original",
        scenario_name = as.character(input$scenario_name %||% "Historical"),
        stringsAsFactors = FALSE
      )
    })

    # ---- Module return API --------------------------------------------------

    list(
      selected_hist = selected_hist
    )
  })
}
