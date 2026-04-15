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
    uiOutput(ns("placeholder_ui")),
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
#' @param selected_model Reactive named list of the selected model's parameters
#'   from Step 1. Used to determine which labor variables are in the model
#'   and should be shown in the UI.
#'
#' @return A named list of reactives:
#'   \describe{
#'     \item{labor_scenario}{Named list of labor market scenario parameters.}
#'     \item{fut_sim}{Reactive — placeholder for future simulation results.}
#'   }
#'
#' @noRd
mod_3_04_labor_server <- function(id,
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

    coeffs <- reactive({
      unique(c(ind_coeff(), hh_coeff(), firm_coeff(), area_coeff()))
    })

    # ---- Candidate variables for this category --------------------------
    labor_patterns <- c("lstatus", "empstat", "agriculture", "industry", "services")

    any_selected <- reactive({
      any(vapply(labor_patterns, function(p) {
        any(grepl(p, coeffs(), ignore.case = TRUE))
      }, logical(1)))
    })

    output$placeholder_ui <- renderUI({
      if (isTRUE(any_selected())) return(NULL)
      cand <- policy_candidate_info(variable_list(), labor_patterns)
      policy_placeholder_tag("labor market", cand)
    })

    # ---- Helper: pp slider ---------------------------------------------
    # Renders a simple percentage-point slider with icon label.

    labor_pp_ui <- function(input_id, label, icon_class,
                            min = -20, max = 20, value = 0, step = 1,
                            post = "pp") {
      tagList(
        tags$label(
          class = "control-label",
          tags$i(class = paste("fa", icon_class, "me-1")),
          label
        ),
        sliderInput(
          inputId = ns(input_id),
          label   = NULL,
          min     = min,
          max     = max,
          value   = value,
          step    = step,
          post    = post
        ),
        tags$hr(style = "margin: 8px 0;")
      )
    }

    # ---- Labor force participation change -------------------------------

    show_lfp <- reactive({
      any(grepl("lstatus", coeffs(), ignore.case = TRUE))
    })

    output$labor_lfp_ui <- renderUI({
      req(show_lfp())
      labor_pp_ui("labor_lfp",
                  "Change in labor force participation (pp)",
                  "fa-person-walking")
    })

    # ---- Employment rate change -----------------------------------------

    show_emp <- reactive({
      any(grepl("empstat", coeffs(), ignore.case = TRUE))
    })

    output$labor_emp_ui <- renderUI({
      req(show_emp())
      labor_pp_ui("labor_emp",
                  "Change in employment rate (pp)",
                  "fa-briefcase")
    })

    # ---- Sectoral composition ------------------------------------------
    # Three sliders sum to 100pp — agriculture anchors, manufacturing and
    # services are user-controlled, agriculture = 100 - mfg - services.

    show_sector <- reactive({
      cv <- coeffs()
      any(grepl("agriculture", cv, ignore.case = TRUE)) &&
      any(grepl("industry",    cv, ignore.case = TRUE)) &&
      any(grepl("services",    cv, ignore.case = TRUE))
    })

    output$labor_sector_ui <- renderUI({
      req(show_sector())
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
