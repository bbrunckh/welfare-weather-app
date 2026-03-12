#' 3_03_digital UI Function
#'
#' @description A shiny Module. Digital inclusion scenario configuration —
#'   allows the user to define changes in internet access and mobile phone
#'   ownership to be applied in the policy simulation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_03_digital_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("internet_ui")),
    uiOutput(ns("mobile_ui"))
  )
}

#' 3_03_digital Server Functions
#'
#' Manages digital inclusion scenario inputs. Returns reactive scenario
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
#'     \item{digital_scenario}{Named list of digital inclusion scenario parameters.}
#'     \item{selected_fut}{Reactive — placeholder for selected future period.}
#'   }
#'
#' @noRd
mod_3_03_digital_server <- function(id,
                                    selected_outcome = reactive(NULL),
                                    survey_weather   = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Helper: slider + universal access toggle -----------------------
    # Mirrors the same pattern used in mod_3_02_infra.R.

    digital_access_ui <- function(input_id, label, icon_class) {
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

    # ---- Internet access ------------------------------------------------

    output$internet_ui <- renderUI({
      digital_access_ui("internet", "Access to internet", "fa-wifi")
    })

    # ---- Mobile phone ownership -----------------------------------------

    output$mobile_ui <- renderUI({
      digital_access_ui("mobile", "Mobile phone ownership", "fa-mobile-screen")
    })

    # ---- Return API -----------------------------------------------------

    list(
      digital_scenario = reactive({
        list(
          # Internet access
          internet_universal        = isTRUE(input$internet_universal),
          internet_access_change_pct = if (isTRUE(input$internet_universal)) 100L
                                       else input$internet_pct %||% 0L,

          # Mobile phone ownership
          mobile_universal        = isTRUE(input$mobile_universal),
          mobile_access_change_pct = if (isTRUE(input$mobile_universal)) 100L
                                     else input$mobile_pct %||% 0L
        )
      }),
      selected_fut = reactive(NULL)
    )
  })
}
