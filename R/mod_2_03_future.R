#' 2_03_future UI Function
#'
#' @description A shiny Module. Selects the future climate configuration
#'   (SSP baseline, reference period, residual handling).
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_03_future_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("fut_years_select")),
    uiOutput(ns("climate_scenario")),
    uiOutput(ns("fut_sim_specs_button")),
    uiOutput(ns("fut_sim_specs"))
  )
}
#' 2_03future Server Functions
#'
#' Manages future climate variable selection and reference period
#' configuration. Returns selected_fut for use by mod_2_04_future_sim.
#'
#' @param id               Module id.
#'
#' @noRd
mod_2_03_future_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  # ---- Climate years selector ---------------------------------------------

    output$fut_years_select <- renderUI({
      shiny::sliderInput(
        inputId = ns("fut_years"),
        label   = "Period defining the future weather distribution",
        min     = 2015,
        max     = 2100,
        value   = c(2040, 2060),
        sep     = ""
      )
    })

    # ---- Climate scenario selector ------------------------------------------

    output$climate_scenario <- renderUI({

      ssp_choices <- c(
        "SSP2-4.5" = "ssp2_4_5",
        "SSP3-7.0" = "ssp3_7_0",
        "SSP5-8.5" = "ssp5_8_5"
      )
      cc_method_choices <- c("Delta" = "delta")

      tagList(
        radioButtons(
          inputId  = ns("climate"),
          label    = "Scenario",
          choices  = ssp_choices,
          selected = unname(ssp_choices)[length(ssp_choices)]
        ),
        # radioButtons(
        #   inputId  = ns("climatemethod"),
        #   label    = "Method",
        #   choices  = cc_method_choices,
        #   selected = unname(cc_method_choices)[1]
        # ),
        helpText(
          "CMIP6 ensemble 'delta' fields are used to perturb historical observations.",
          style = "font-size: 12px;"
        )
      )
    })

    # ---- Simulation parameters toggle ---------------------------------------

    fut_sim_specs_open <- reactiveVal(FALSE)

    output$fut_sim_specs_button <- renderUI({
      shiny::actionButton(ns("fut_sim_specs"), "Simulation parameters", style = "margin-bottom:12px;")
    })

    observeEvent(input$fut_sim_specs, {
      fut_sim_specs_open(!isTRUE(fut_sim_specs_open()))
    })

    # ---- Simulation parameters panel ----------------------------------------

    output$fut_sim_specs <- renderUI({
      if (!isTRUE(fut_sim_specs_open())) return(NULL)

      tagList(
        radioButtons(
          inputId  = ns("fut_sim_residuals"),
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

    # ---- Selected future weather simulation configuration -----------------------------

    selected_fut <- reactive({

      data.frame(
        type       = "future",
        year_range = input$fut_years          %||% c(2040, 2060),
        ssp        = input$climate            %||% "ssp5_8_5",
        method     = input$climatemethod      %||% "delta",
        residuals  = input$fut_sim_residuals  %||% "original",
        stringsAsFactors = FALSE)
    })

    # ---- Module return API --------------------------------------------------

    list(
      selected_fut = selected_fut
    )
  })
}
