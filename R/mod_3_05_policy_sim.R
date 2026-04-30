#' 3_05_policy_sim UI Function
#'
#' @description A shiny Module. Renders status banner for policy adjustments.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_05_policy_sim_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sim_status_ui"))
  )
}

#' 3_05_policy_sim Server Functions
#'
#' Applies user-defined policy adjustments to survey covariates from the
#' policy scenario modules (mod_3_01 through mod_3_04). Exposes baseline
#' and policy-adjusted survey frames for downstream diagnostics.
#'
#' @param id                Module id.
#' @param survey_weather    Reactive survey-weather df to be adjusted.
#' @param sp_scenario       Reactive named list from mod_3_01_sp_server().
#' @param infra_scenario    Reactive named list from mod_3_02_infra_server().
#' @param digital_scenario  Reactive named list from mod_3_03_digital_server().
#' @param labor_scenario    Reactive named list from mod_3_04_labor_server().
#'
#' @return Named list exposing baseline_svy, policy_svy, and sim_run_id.
#'
#' @noRd
mod_3_05_policy_sim_server <- function(id,
                                        survey_weather,
                                        sp_scenario        = reactive(NULL),
                                        infra_scenario     = reactive(NULL),
                                        digital_scenario   = reactive(NULL),
                                        labor_scenario     = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    baseline_svy_rv <- reactiveVal(NULL)
    policy_svy_rv   <- reactiveVal(NULL)
    sim_error       <- reactiveVal(NULL)
    sim_run_id      <- reactiveVal(0L)

    output$sim_status_ui <- renderUI({
      err <- sim_error()
      if (!is.null(err)) {
        return(div(class = "alert alert-danger", conditionMessage(err)))
      }
      if (!is.null(policy_svy_rv())) {
        return(div(
          class = "alert alert-success",
          "Policy adjustments applied successfully."
        ))
      }
      NULL
    })

    run <- function() {
      sim_error(NULL)

      svy <- survey_weather()
      if (is.null(svy)) {
        sim_error(simpleError(
          "Survey data not available."
        ))
        return(invisible(NULL))
      }

      result <- tryCatch(
        {
          baseline_svy_rv(svy)

          svy_mod <- apply_policy_to_svy(
            svy,
            infra   = infra_scenario(),
            sp      = sp_scenario(),
            digital = digital_scenario(),
            labor   = labor_scenario()
          )

          policy_svy_rv(svy_mod)
          sim_run_id(isolate(sim_run_id()) + 1L)

          shiny::showNotification(
            "Policy adjustments applied.",
            type = "message", duration = 3
          )
          invisible(NULL)
        },
        error = function(e) {
          sim_error(e)
          baseline_svy_rv(NULL)
          policy_svy_rv(NULL)
          shiny::showNotification(
            paste0("Policy adjustment failed: ", conditionMessage(e)),
            type = "error", duration = 8
          )
          return(invisible(NULL))
        }
      )
    }

    # RE-SIMULATE BASED ON POLICY SCENARIO CHANGES FROM HERE

    list(
      run            = run,
      baseline_svy   = baseline_svy_rv,
      policy_svy     = policy_svy_rv,
      sim_run_id     = sim_run_id
    )
  })
}
