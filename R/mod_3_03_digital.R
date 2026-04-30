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
    uiOutput(ns("placeholder_ui")),
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
#' @param selected_model Reactive named list of the selected model's parameters
#'   from Step 1. Used to determine which digital variables are in the model
#'   and should be shown in the UI.
#'
#' @return A named list of reactives:
#'   \describe{
#'     \item{digital_scenario}{Named list of digital inclusion scenario parameters.}
#'     \item{selected_fut}{Reactive — placeholder for selected future period.}
#'   }
#'
#' @noRd
mod_3_03_digital_server <- function(id,
                                    selected_model = reactive(NULL),
                                    survey_data = reactive(NULL),
                                    variable_list  = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Get model coefficients ------------------------------------------
    sm <- reactive({
      req(selected_model())
      selected_model()
    })

    extract_cov_names <- function(x) {
      if (is.null(x)) return(character(0))
      nms <- names(x)
      if (!is.null(nms) && any(nzchar(nms))) {
        unique(nms[nzchar(nms)])
      } else {
        unique(as.character(unlist(x, use.names = FALSE)))
      }
    }

    ind_coeff <- reactive({
      s <- sm()
      extract_cov_names(s$individual_covariates)
    })
    hh_coeff <- reactive({
      s <- sm()
      extract_cov_names(s$hh_covariates)
    })
    firm_coeff <- reactive({
      s <- sm()
      extract_cov_names(s$firm_covariates)
    })
    area_coeff <- reactive({
      s <- sm()
      extract_cov_names(s$area_covariates)
    })

    interaction_names <- reactive({
      s <- sm()
      extract_cov_names(s$interactions)
    })

    coeffs <- reactive({
      unique(c(ind_coeff(), hh_coeff(), firm_coeff(), area_coeff(), interaction_names()))
    })

    # ---- Candidate variables for this category --------------------------
    digital_patterns <- c("internet", "cellphone")

    any_selected <- reactive({
      any(tolower(digital_patterns) %in% tolower(coeffs()))
    })

    # Digital variables available in the selected survey
    digital_vars_available <- reactive({
      svy <- survey_data()
      if (is.null(svy)) return(character(0))
      # Check which digital variables are actually in the selected survey
      intersect(digital_patterns, names(svy))
    })

    output$placeholder_ui <- renderUI({
      if (isTRUE(any_selected())) return(NULL)
      # Only show placeholder if there are digital variables in the survey
      if (length(digital_vars_available()) == 0) {
        return(div(
          class = "alert alert-warning",
          "No digital inclusion variables found in the selected survey (or level of analysis)."
        ))
      }
      # Show candidate variables for digital inclusion if none are in the model
      cand <- policy_candidate_info(variable_list(), digital_patterns)
      policy_placeholder_tag("digital inclusion", cand)
    })

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

    show_internet <- reactive({
      any(grepl("internet", coeffs(), ignore.case = TRUE))
    })

    output$internet_ui <- renderUI({
      req(show_internet())
      digital_access_ui("internet", "Access to internet", "fa-wifi")
    })

    # ---- Mobile phone ownership -----------------------------------------

    show_mobile <- reactive({
      any(grepl("cellphone", coeffs(), ignore.case = TRUE))
    })

    output$mobile_ui <- renderUI({
      req(show_mobile())
      digital_access_ui("mobile", "Mobile phone ownership", "fa-mobile-screen")
    })

    # ---- Return API -----------------------------------------------------

    list(
      digital_scenario = reactive({
        list(
          internet_universal        = isTRUE(input$internet_universal),
          internet_access_change_pct = if (isTRUE(input$internet_universal)) 100L
                                       else input$internet_pct %||% 0L,

          mobile_universal        = isTRUE(input$mobile_universal),
          mobile_access_change_pct = if (isTRUE(input$mobile_universal)) 100L
                                     else input$mobile_pct %||% 0L
        )
      })
    )
  })
}
