#' 3_05_education UI Function
#'
#' @description A shiny Module. Education scenario configuration — allows the
#'   user to define changes in educational attainment (primary, secondary and
#'   post-secondary completion) to be applied in the policy simulation.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_05_education_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("placeholder_ui")),
    uiOutput(ns("primary_ui")),
    uiOutput(ns("secondary_ui")),
    uiOutput(ns("postsec_ui"))
  )
}

#' 3_05_education Server Functions
#'
#' Manages education scenario inputs. Returns reactive scenario parameters to be
#' consumed by the policy simulation sub-module.
#'
#' @param id Module id.
#' @param selected_model Reactive named list of the selected model's parameters
#'   from Step 1. Used to determine which education variables are in the model
#'   and should be shown in the UI.
#' @param survey_data Reactive survey data frame, used to detect which education
#'   variables are present in the selected survey.
#' @param variable_list Reactive variable-list metadata data frame.
#'
#' @return A named list of reactives:
#'   \describe{
#'     \item{education_scenario}{Named list of education scenario parameters.}
#'   }
#'
#' @noRd
mod_3_05_education_server <- function(id,
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
    education_patterns <- c("educ_com1_hh", "educ_com2_hh", "educ_com3_hh")

    any_selected <- reactive({
      any(tolower(education_patterns) %in% tolower(coeffs()))
    })

    # Education variables available in the selected survey
    education_vars_available <- reactive({
      svy <- survey_data()
      if (is.null(svy)) return(character(0))
      # Check which education variables are actually in the selected survey
      intersect(education_patterns, names(svy))
    })

    output$placeholder_ui <- renderUI({
      if (isTRUE(any_selected())) return(NULL)
      # Only show placeholder if there are education variables in the survey
      if (length(education_vars_available()) == 0) {
        return(div(
          class = "alert alert-warning",
          "No education variables found in the selected survey (or level of analysis)."
        ))
      }
      # Show candidate variables for education if none are in the model
      cand <- policy_candidate_info(variable_list(), education_patterns)
      policy_placeholder_tag("education", cand)
    })

    # ---- Helper: slider + universal access toggle -----------------------
    # Mirrors the same pattern used in mod_3_03_digital.R.

    education_access_ui <- function(input_id, label, icon_class) {
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
            label   = "Change attainment by (%)",
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

    # ---- Primary attainment ---------------------------------------------

    show_primary <- reactive({
      any(grepl("educ_com1_hh", coeffs(), ignore.case = TRUE))
    })

    output$primary_ui <- renderUI({
      req(show_primary())
      education_access_ui("primary", "Primary completion", "fa-graduation-cap")
    })

    # ---- Secondary attainment -------------------------------------------

    show_secondary <- reactive({
      any(grepl("educ_com2_hh", coeffs(), ignore.case = TRUE))
    })

    output$secondary_ui <- renderUI({
      req(show_secondary())
      education_access_ui("secondary", "Secondary completion", "fa-graduation-cap")
    })

    # ---- Post-secondary attainment --------------------------------------

    show_postsec <- reactive({
      any(grepl("educ_com3_hh", coeffs(), ignore.case = TRUE))
    })

    output$postsec_ui <- renderUI({
      req(show_postsec())
      education_access_ui("postsec", "Post-secondary completion", "fa-graduation-cap")
    })

    # ---- Return API -----------------------------------------------------

    list(
      education_scenario = reactive({
        list(
          primary_universal        = isTRUE(input$primary_universal),
          primary_access_change_pct = if (isTRUE(input$primary_universal)) 100L
                                      else input$primary_pct %||% 0L,

          secondary_universal        = isTRUE(input$secondary_universal),
          secondary_access_change_pct = if (isTRUE(input$secondary_universal)) 100L
                                        else input$secondary_pct %||% 0L,

          postsec_universal        = isTRUE(input$postsec_universal),
          postsec_access_change_pct = if (isTRUE(input$postsec_universal)) 100L
                                      else input$postsec_pct %||% 0L
        )
      })
    )
  })
}
