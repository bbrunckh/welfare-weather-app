#' 2_02_climate UI Function
#'
#' @param id Module id.
#' @noRd
#' @importFrom shiny NS tagList
mod_2_02_climate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("future_years_select")),
    uiOutput(ns("cc_scenario")),
    uiOutput(ns("future_sim_specs_button")),
    uiOutput(ns("future_sim_specs"))
  )
}

#' 2_02_climate Server Function
#'
#' @param id              Module id.
#' @param variable_list   Reactive data frame of variable metadata.
#' @param selected_surveys Reactive data frame of selected surveys.
#' @param selected_outcome Reactive data frame of selected outcome info.
#' @param selected_weather Reactive data frame of selected weather specs.
#' @param survey_weather  Reactive data frame of merged survey-weather data.
#' @param model_fit       Reactive fitted model object.
#' @param step1           Accepted but unused (for call-site compatibility).
#' @param pov_lines       Accepted but unused (for call-site compatibility).
#' @param varlist         Accepted but unused (for call-site compatibility).
#' @param board           Accepted but unused (for call-site compatibility).
#'
#' @return A named list with one reactive element:
#'   \describe{
#'     \item{`selected_cc`}{A reactive returning a one-row `data.frame` as
#'       produced by `build_selected_cc()`, with columns `ssp`, `method`,
#'       `year_start`, `year_end`, and `residuals`.}
#'   }
#'
#' @noRd
mod_2_02_climate_server <- function(
    id,
    variable_list    = NULL,
    selected_surveys = NULL,
    selected_outcome = NULL,
    selected_weather = NULL,
    survey_weather   = NULL,
    model_fit        = NULL,
    step1            = NULL,   # accepted but unused
    pov_lines        = NULL,   # accepted but unused
    varlist          = NULL,   # accepted but unused
    board            = NULL    # accepted but unused
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Climate years selector ---------------------------------------------

    output$climate_years_select <- renderUI({
      shiny::sliderInput(
        inputId = ns("cc_years"),
        label   = "Period defining the projected weather distribution",
        min     = 2015,
        max     = 2100,
        value   = c(2040, 2060),
        sep     = ""
      )
    })

    # ---- Climate scenario selector ------------------------------------------

    output$climate_scenario <- renderUI({
      tagList(
        radioButtons(
          inputId  = ns("climate"),
          label    = "Scenario",
          choices  = ssp_choices(),
          selected = unname(ssp_choices())[length(ssp_choices())]
        ),
        radioButtons(
          inputId  = ns("climatemethod"),
          label    = "Method",
          choices  = cc_method_choices(),
          selected = unname(cc_method_choices())[1]
        ),
        helpText(
          "CMIP6 ensemble 'delta' fields are used to perturb historical observations.",
          style = "font-size: 12px;"
        )
      )
    })

    # ---- Simulation parameters toggle ---------------------------------------

    cc_sim_specs_open <- reactiveVal(FALSE)

    output$climate_sim_specs_button <- renderUI({
      shiny::actionButton(ns("cc_sim_specs"), "Simulation parameters",
                          style = "margin-bottom:12px;")
    })

    observeEvent(input$cc_sim_specs, {
      cc_sim_specs_open(!isTRUE(cc_sim_specs_open()))
    })

    # ---- Simulation parameters panel ----------------------------------------

    output$climate_sim_specs <- renderUI({
      if (!isTRUE(cc_sim_specs_open())) return(NULL)

      tagList(
        radioButtons(
          inputId  = ns("cc_sim_residuals"),
          label    = "Residuals method",
          choices  = cc_residual_choices(),
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

    # ---- selected_cc: assembled spec data frame -----------------------------

    selected_cc <- reactive({
      # Read only what is needed; isolate toggle-button state to avoid
      # rerunning on every actionButton click.
      ssp        <- input$climate        %||% unname(ssp_choices())[length(ssp_choices())]
      method     <- input$climatemethod  %||% unname(cc_method_choices())[1]
      year_range <- isolate(input$cc_years) %||% c(2040, 2060)
      residuals  <- input$cc_sim_residuals  %||% "original"

      tryCatch(
        build_selected_cc(
          ssp        = ssp,
          method     = method,
          year_start = as.integer(year_range[1]),
          year_end   = as.integer(year_range[2]),
          residuals  = residuals
        ),
        error = function(e) {
          message("[climate] build_selected_cc() error: ", conditionMessage(e))
          NULL
        }
      )
    })

    # ---- Module return API --------------------------------------------------

    list(
      selected_cc = selected_cc
    )
  })
}
