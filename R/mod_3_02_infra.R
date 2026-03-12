#' 3_02_infra UI Function
#'
#' @description A shiny Module. Access to infrastructure scenario
#'   configuration — allows the user to define changes in infrastructure
#'   access rates and travel times to be applied in the policy simulation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_02_infra_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("elec_ui")),
    uiOutput(ns("water_ui")),
    uiOutput(ns("sanitation_ui")),
    uiOutput(ns("health_ui"))
  )
}

#' 3_02_infra Server Functions
#'
#' Manages infrastructure access scenario inputs. Returns reactive scenario
#' parameters to be consumed by the policy simulation sub-module.
#'
#' @param id Module id.
#' @param selected_outcome Reactive one-row data frame of selected outcome
#'   from \code{mod_1_modelling_server()}.
#' @param survey_weather Reactive data frame of merged survey-weather data
#'   from \code{mod_1_modelling_server()}.
#'
#' @return A named list of reactives:
#'   \describe{
#'     \item{infra_scenario}{Named list of infrastructure scenario parameters.}
#'     \item{hist_sim}{Reactive — placeholder for historical simulation results.}
#'   }
#'
#' @noRd
mod_3_02_infra_server <- function(id,
                                   selected_outcome = reactive(NULL),
                                   survey_weather   = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Helper: slider + universal access toggle -----------------------
    # Renders a sliderInput with a checkbox for universal access.
    # When universal access is checked the slider is hidden/ignored.

    infra_access_ui <- function(input_id, label, icon_class = "fa-house") {
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = paste("fa", icon_class, "me-1")),
          label
        ),
        checkboxInput(
          inputId = ns(paste0(input_id, "_universal")),
          label   = "Universal",
          value   = FALSE
        ),
        conditionalPanel(
          condition = paste0("!input['", ns(paste0(input_id, "_universal")), "']"),
          sliderInput(
            inputId = ns(paste0(input_id, "_pct")),
            label   = "Change access by (%)",
            min     = -20,
            max     = 100,
            value   = 0,
            step    = 5,
            post    = "%"
          )
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    }

    # ---- Electricity access ---------------------------------------------

    output$elec_ui <- renderUI({
      infra_access_ui("elec", "Access to electricity", "fa-bolt")
    })

    # ---- Improved water access ------------------------------------------

    output$water_ui <- renderUI({
      infra_access_ui("water", "Access to improved water", "fa-droplet")
    })

    # ---- Improved sanitation access -------------------------------------

    output$sanitation_ui <- renderUI({
      infra_access_ui("sanitation", "Access to improved sanitation", "fa-toilet")
    })

    # ---- Health facility access -----------------------------------------
    # Two modes: reduce travel time by % OR cap at maximum minutes.

    output$health_ui <- renderUI({
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = "fa fa-hospital me-1"),
          "Access to health facility"
        ),
        radioButtons(
          inputId  = ns("health_mode"),
          label    = NULL,
          choices  = c(
            "Reduce travel time by (%)" = "pct",
            "Set maximum travel time (min)" = "max"
          ),
          selected = "pct",
          inline   = TRUE
        ),
        conditionalPanel(
          condition = paste0("input['", ns("health_mode"), "'] === 'pct'"),
          sliderInput(
            inputId = ns("health_travel_pct"),
            label   = "Change travel time by (%)",
            min     = -100,
            max     = 20,
            value   = 0,
            step    = 5,
            post    = "%"
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("health_mode"), "'] === 'max'"),
          sliderInput(
            inputId = ns("health_travel_max"),
            label   = "Maximum travel time (minutes)",
            min     = 15,
            max     = 240,
            value   = 60,
            step    = 15,
            post    = " min"
          )
        )
      )
    })

    # ---- Return API -----------------------------------------------------

    list(
      infra_scenario = reactive({
        list(
          # Electricity: TRUE = set all to 1, FALSE = increase by pct
          elec_universal        = isTRUE(input$elec_universal),
          elec_access_change_pct = if (isTRUE(input$elec_universal)) 100L
                                   else input$elec_pct %||% 0L,

          # Water
          water_universal        = isTRUE(input$water_universal),
          water_access_change_pct = if (isTRUE(input$water_universal)) 100L
                                    else input$water_pct %||% 0L,

          # Sanitation
          sanitation_universal        = isTRUE(input$sanitation_universal),
          sanitation_access_change_pct = if (isTRUE(input$sanitation_universal)) 100L
                                         else input$sanitation_pct %||% 0L,

          # Health facility travel time
          health_mode        = input$health_mode %||% "pct",
          health_travel_pct  = input$health_travel_pct  %||% 0L,
          health_travel_max  = input$health_travel_max  %||% 60L
        )
      }),
      hist_sim = reactive(NULL)
    )
  })
}
