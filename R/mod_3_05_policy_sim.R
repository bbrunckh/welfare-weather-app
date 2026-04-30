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
#' policy scenario modules (mod_3_01 through mod_3_04), then re-runs the
#' Step 2 simulation pipeline against both the baseline and policy-adjusted
#' survey frames using the cached Step 2 weather, model fit and draws.
#'
#' @param id                Module id.
#' @param survey_weather    Reactive survey-weather df to be adjusted.
#' @param sp_scenario       Reactive named list from mod_3_01_sp_server().
#' @param infra_scenario    Reactive named list from mod_3_02_infra_server().
#' @param digital_scenario  Reactive named list from mod_3_03_digital_server().
#' @param labor_scenario    Reactive named list from mod_3_04_labor_server().
#' @param model_fit         Reactive list from mod_1 model fit.
#' @param selected_weather  Reactive selected-weather metadata.
#' @param hist_sim          Reactive Step 2 hist_sim list.
#' @param saved_scenarios   Reactive Step 2 named scenario list.
#'
#' @return Named list with baseline_svy, policy_svy, sim_run_id, plus
#'   re-simulated baseline_hist_sim/baseline_saved_scenarios and
#'   policy_hist_sim/policy_saved_scenarios.
#'
#' @noRd
mod_3_05_policy_sim_server <- function(id,
                                        survey_weather,
                                        sp_scenario        = reactive(NULL),
                                        infra_scenario     = reactive(NULL),
                                        digital_scenario   = reactive(NULL),
                                        labor_scenario     = reactive(NULL),
                                        model_fit          = reactive(NULL),
                                        selected_weather   = reactive(NULL),
                                        hist_sim           = reactive(NULL),
                                        saved_scenarios    = reactive(list())) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    baseline_svy_rv <- reactiveVal(NULL)
    policy_svy_rv   <- reactiveVal(NULL)
    sim_error       <- reactiveVal(NULL)
    sim_run_id      <- reactiveVal(0L)

    baseline_hist_sim_rv        <- reactiveVal(NULL)
    baseline_saved_scenarios_rv <- reactiveVal(list())
    policy_hist_sim_rv          <- reactiveVal(NULL)
    policy_saved_scenarios_rv   <- reactiveVal(list())

    run <- function() {
      sim_error(NULL)

      svy <- survey_weather()
      mf  <- model_fit()
      sw  <- selected_weather()
      hs  <- hist_sim()
      ss  <- saved_scenarios()

      if (is.null(svy)) {
        sim_error(simpleError("Survey data not available."))
        return(invisible(NULL))
      }
      if (is.null(mf) || is.null(hs)) {
        sim_error(simpleError(
          "Step 2 simulation must be run before policy simulation."
        ))
        return(invisible(NULL))
      }

      tryCatch(
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

          shiny::withProgress(
            message = "Re-running simulations for baseline and policy...",
            value   = 0.1,
            {
              shiny::setProgress(value = 0.2, detail = "Baseline...")
              base_out <- resimulate_with_svy(svy, sw, hs$so, mf, hs, ss)
              if (!is.null(base_out)) {
                baseline_hist_sim_rv(base_out$hist_sim)
                baseline_saved_scenarios_rv(base_out$saved_scenarios)
              }

              # SP transfer is applied via below addition to run_sim_pipeline.
              # It may need re-introduction when merging branches from revamped Step 2.
                # Apply SP direct transfer if pre-computed by apply_policy_to_svy
                # (no-op for Step 2 baseline simulations — no ._sp_transfer column)
                # if ("._sp_transfer" %in% names(preds)) {
                #   preds[[so$name]] <- preds[[so$name]] + preds[["._sp_transfer"]]
                # }
              shiny::setProgress(value = 0.6, detail = "Policy...")
              pol_out <- resimulate_with_svy(svy_mod, sw, hs$so, mf, hs, ss)
              if (!is.null(pol_out)) {
                policy_hist_sim_rv(pol_out$hist_sim)
                policy_saved_scenarios_rv(pol_out$saved_scenarios)
              }

              shiny::setProgress(value = 1, detail = "Complete")
            }
          )

          sim_run_id(isolate(sim_run_id()) + 1L)
          shiny::showNotification(
            "Policy adjustments applied and simulation re-run.",
            type = "message", duration = 3
          )
        },
        error = function(e) {
          sim_error(e)
          shiny::showNotification(
            paste0("Policy simulation failed: ", conditionMessage(e)),
            type = "error", duration = 8
          )
        }
      )
      invisible(NULL)
    }

    list(
      run                      = run,
      baseline_svy             = baseline_svy_rv,
      policy_svy               = policy_svy_rv,
      sim_run_id               = sim_run_id,
      baseline_hist_sim        = baseline_hist_sim_rv,
      baseline_saved_scenarios = baseline_saved_scenarios_rv,
      policy_hist_sim          = policy_hist_sim_rv,
      policy_saved_scenarios   = policy_saved_scenarios_rv
    )
  })
}
