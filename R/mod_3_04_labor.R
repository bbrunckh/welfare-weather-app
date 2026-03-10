#' 3_04_labor UI Function
#'
#' @description A shiny Module. Labor market scenario configuration —
#'   allows the user to simulate changes in labor force participation,
#'   employment rate, and sectoral composition.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_04_labor_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("labor_lfp_ui")),
    uiOutput(ns("labor_emp_ui")),
    uiOutput(ns("labor_sector_ui"))
  )
}

#' 3_04_labor Server Functions
#'
#' Manages labor market scenario inputs. Returns reactive scenario parameters
#' to be consumed by the policy simulation sub-module.
#'
#' @param id Module id.
#' @param selected_outcome Reactive one-row data frame of selected outcome
#'   from \code{mod_1_modelling_server()}.
#' @param survey_weather Reactive data frame of merged survey-weather data
#'   from \code{mod_1_modelling_server()}.
#'
#' @return A named list of reactives:
#'   \describe{
#'     \item{labor_scenario}{Named list of labor market scenario parameters.}
#'     \item{fut_sim}{Reactive — placeholder for future simulation results.}
#'   }
#'
#' @noRd
mod_3_04_labor_server <- function(id,
                                   selected_outcome = reactive(NULL),
                                   survey_weather   = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Labor force participation change -------------------------------

    output$labor_lfp_ui <- renderUI({
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-person-walking me-1"),
          "Change in labor force participation (pp)"
        ),
        sliderInput(
          inputId = ns("labor_lfp"),
          label   = NULL,
          min     = -20,
          max     = 20,
          value   = 0,
          step    = 1,
          post    = "pp"
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # ---- Employment rate change -----------------------------------------

    output$labor_emp_ui <- renderUI({
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-briefcase me-1"),
          "Change in employment rate (pp)"
        ),
        sliderInput(
          inputId = ns("labor_emp"),
          label   = NULL,
          min     = -20,
          max     = 20,
          value   = 0,
          step    = 1,
          post    = "pp"
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # ---- Sectoral composition ------------------------------------------
    # Three sliders sum to 100pp — agriculture anchors, manufacturing and
    # services are user-controlled, agriculture = 100 - mfg - services.

    output$labor_sector_ui <- renderUI({
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-chart-pie me-1"),
          "Sectoral composition (% of employment)"
        ),
        tags$small(
          class = "text-muted d-block mb-2",
          "Agriculture adjusts automatically to sum to 100%."
        ),
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-industry me-1"),
          "Manufacturing (%)"
        ),
        sliderInput(
          inputId = ns("sector_manufacturing"),
          label   = NULL,
          min     = 0,
          max     = 100,
          value   = 20,
          step    = 1,
          post    = "%"
        ),
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-shop me-1"),
          "Services (%)"
        ),
        sliderInput(
          inputId = ns("sector_services"),
          label   = NULL,
          min     = 0,
          max     = 100,
          value   = 40,
          step    = 1,
          post    = "%"
        ),
        uiOutput(ns("sector_agri_display"))
      )
    })

    # ---- Agriculture share derived from mfg + services -----------------

    sector_agri <- reactive({
      mfg      <- input$sector_manufacturing %||% 20
      services <- input$sector_services      %||% 40
      max(0L, 100L - mfg - services)
    })

    output$sector_agri_display <- renderUI({
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-wheat-awn me-1"),
          "Agriculture (%) — derived"
        ),
        tags$div(
          class = "well well-sm text-center",
          style = "padding: 6px; background: #f5f5f5; border-radius: 4px;",
          tags$strong(paste0(sector_agri(), "%"))
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    })

    # ---- Return API -----------------------------------------------------

    list(
      labor_scenario = reactive({
        list(
          lfp_change_pp         = input$labor_lfp           %||% 0,
          employment_change_pp  = input$labor_emp           %||% 0,
          sector_manufacturing  = input$sector_manufacturing %||% 20,
          sector_services       = input$sector_services      %||% 40,
          sector_agriculture    = sector_agri()
        )
      }),
      fut_sim = reactive(NULL)
    )
  })
}
