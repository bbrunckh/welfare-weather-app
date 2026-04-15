#' 3_05_policy_sim UI Function
#'
#' @description A shiny Module. Renders status banner for the Step 3 policy
#'   simulation. Visualisations live in a separate Results tab populated by
#'   \code{mod_2_02_results_server()} (reused from Step 2).
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
#' Orchestrates the Step 3 policy simulation. On \code{run()}:
#' \enumerate{
#'   \item Applies policy-scenario adjustments to the survey covariates via
#'     \code{apply_policy_to_svy()}.
#'   \item Re-runs the prediction pipeline across the Step 2 historical and
#'     future scenarios (reusing their \code{weather_raw}) via
#'     \code{run_policy_pipeline()}.
#' }
#' Exposes reactives with the same shape as the Step 2 sidebar output so that
#' the Step 2 Results module can render them directly.
#'
#' @param id                Module id.
#' @param connection_params Reactive named list from \code{mod_0_overview_server()}.
#' @param selected_outcome  Reactive one-row data frame of selected outcome.
#' @param selected_weather  Reactive data frame of selected weather variables.
#' @param survey_weather    Reactive data frame of merged survey-weather data.
#' @param model_fit         Reactive list of fitted model objects.
#' @param hist_sim          Reactive Step 2 historical simulation list.
#' @param saved_scenarios   Reactive Step 2 saved scenarios named list.
#' @param sp_scenario       Reactive named list from \code{mod_3_01_sp_server()}.
#' @param infra_scenario    Reactive named list from \code{mod_3_02_infra_server()}.
#' @param digital_scenario  Reactive named list from \code{mod_3_03_digital_server()}.
#' @param labor_scenario    Reactive named list from \code{mod_3_04_labor_server()}.
#'
#' @return Named list exposing \code{run}, \code{policy_hist_sim},
#'   \code{policy_saved_scenarios}, \code{policy_selected_hist}.
#'
#' @noRd
mod_3_05_policy_sim_server <- function(id,
                                        connection_params  = reactive(NULL),
                                        selected_outcome   = reactive(NULL),
                                        selected_weather   = reactive(NULL),
                                        survey_weather     = reactive(NULL),
                                        model_fit          = reactive(NULL),
                                        hist_sim           = reactive(NULL),
                                        saved_scenarios    = reactive(list()),
                                        sp_scenario        = reactive(NULL),
                                        infra_scenario     = reactive(NULL),
                                        digital_scenario   = reactive(NULL),
                                        labor_scenario     = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Reactive result stores ----------------------------------------
    policy_hist_sim        <- reactiveVal(NULL)
    policy_saved_scenarios <- reactiveVal(list())
    baseline_svy_rv        <- reactiveVal(NULL)
    policy_svy_rv          <- reactiveVal(NULL)
    sim_error              <- reactiveVal(NULL)

    # Synthetic selected_hist so mod_2_02_results can label the baseline row.
    policy_selected_hist <- reactive({
      data.frame(
        scenario_name = "Policy baseline (historical)",
        stringsAsFactors = FALSE
      )
    })

    # ---- Status banner -------------------------------------------------
    output$sim_status_ui <- renderUI({
      err <- sim_error()
      if (!is.null(err)) {
        return(div(class = "alert alert-danger", conditionMessage(err)))
      }
      if (!is.null(policy_hist_sim())) {
        n <- length(policy_saved_scenarios())
        return(div(
          class = "alert alert-success",
          paste0(
            "Policy simulation complete.",
            if (n > 0) paste0(" ", n, " future scenario(s) recomputed.") else ""
          )
        ))
      }
      NULL
    })

    # ---- Runner invoked externally -------------------------------------
    run <- function() {
      sim_error(NULL)

      hs <- hist_sim()
      if (is.null(hs)) {
        sim_error(simpleError(
          "Run the Step 2 simulation first — no historical baseline available."
        ))
        return(invisible(NULL))
      }

      svy <- survey_weather()
      mf  <- model_fit()
      so  <- selected_outcome()
      sw  <- selected_weather()
      if (is.null(svy) || is.null(mf) || is.null(so) || is.null(sw)) {
        sim_error(simpleError(
          "Missing Step 1 inputs (survey, model, outcome, or weather selection)."
        ))
        return(invisible(NULL))
      }

      result <- tryCatch(
        {
          svy_mod <- apply_policy_to_svy(
            svy,
            infra   = infra_scenario(),
            sp      = sp_scenario(),
            digital = digital_scenario(),
            labor   = labor_scenario()
          )
          baseline_svy_rv(svy)
          policy_svy_rv(svy_mod)

          residuals <- "normal"

          shiny::withProgress(
            message = "Running policy simulation...",
            value   = 0.1,
            {
              shiny::setProgress(value = 0.3, detail = "Re-predicting outcomes...")
              res <- run_policy_pipeline(
                hist_sim        = hs,
                saved_scenarios = saved_scenarios(),
                svy_mod         = svy_mod,
                sw              = sw,
                so              = so,
                model           = mf$fit3,
                residuals       = residuals,
                train_data      = mf$train_data,
                engine          = mf$engine
              )
              shiny::setProgress(value = 1, detail = "Complete")
              res
            }
          )
        },
        error = function(e) e
      )

      if (inherits(result, "error")) {
        sim_error(result)
        policy_hist_sim(NULL)
        policy_saved_scenarios(list())
        shiny::showNotification(
          paste0("Policy simulation failed: ", conditionMessage(result)),
          type = "error", duration = 8
        )
        return(invisible(NULL))
      }

      policy_hist_sim(result$hist_sim)
      policy_saved_scenarios(result$saved_scenarios)

      shiny::showNotification(
        paste0(
          "\u2713 Policy simulation complete.",
          if (length(result$saved_scenarios) > 0)
            paste0(" ", length(result$saved_scenarios), " future scenario(s).")
          else ""
        ),
        type = "message", duration = 5
      )
      invisible(NULL)
    }

    list(
      run                    = run,
      policy_hist_sim        = policy_hist_sim,
      policy_saved_scenarios = policy_saved_scenarios,
      policy_selected_hist   = policy_selected_hist,
      baseline_svy           = baseline_svy_rv,
      policy_svy             = policy_svy_rv
    )
  })
}
