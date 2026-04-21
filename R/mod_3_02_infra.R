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
    uiOutput(ns("placeholder_ui")),
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
#' @param selected_model Reactive named list of the selected model's parameters from Step 1. Used to determine which infrastructure variables are in the model and should be shown in the UI.
#'
#' @return A named list of reactives:
#'   \describe{
#'     \item{infra_scenario}{Named list of infrastructure scenario parameters.}
#'     \item{hist_sim}{Reactive — placeholder for historical simulation results.}
#'   }
#'
#' @noRd
mod_3_02_infra_server <- function(id,
                                  selected_model = reactive(NULL),
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
      # if named list/vector, use names; otherwise use values
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
    infra_patterns <- c("electricity", "imp_wat_rec", "imp_san_rec", "ttime_health")

    any_selected <- reactive({
      any(vapply(infra_patterns, function(p) {
        any(grepl(p, coeffs(), ignore.case = TRUE))
      }, logical(1)))
    })

    output$placeholder_ui <- renderUI({
      if (isTRUE(any_selected())) return(NULL)
      cand <- policy_candidate_info(variable_list(), infra_patterns)
      policy_placeholder_tag("infrastructure", cand)
    })

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

    show_elec <- reactive({
      any(grepl("electricity", coeffs(), ignore.case = TRUE))
    })

    output$elec_ui <- renderUI({
      req(show_elec())
      infra_access_ui("elec", "Access to electricity", "fa-bolt")
    })

    # ---- Improved water access ------------------------------------------

    show_water <- reactive({
      any(grepl("imp_wat_rec", coeffs(), ignore.case = TRUE))
    })

    output$water_ui <- renderUI({
      req(show_water())
      infra_access_ui("water", "Access to improved water", "fa-droplet")
    })

    # ---- Improved sanitation access -------------------------------------

    show_sanitation <- reactive({
      any(grepl("imp_san_rec", coeffs(), ignore.case = TRUE))
    })

    output$sanitation_ui <- renderUI({
      req(show_sanitation())
      infra_access_ui("sanitation", "Access to improved sanitation", "fa-toilet")
    })

    # ---- Health facility access -----------------------------------------
    # Two modes: reduce travel time by % OR cap at maximum minutes.

    show_health <- reactive({
      any(grepl("ttime_health", coeffs(), ignore.case = TRUE))
    })

    output$health_ui <- renderUI({
      req(show_health())
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
