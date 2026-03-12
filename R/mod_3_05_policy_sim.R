#' 3_05_policy_sim UI Function
#'
#' @description A shiny Module. Renders the policy simulation outputs —
#'   welfare impact tables and distributional charts — in the main panel of
#'   \code{mod_3_scenario}.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_05_policy_sim_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sim_status_ui")),
    uiOutput(ns("sim_results_ui"))
  )
}

#' 3_05_policy_sim Server Functions
#'
#' Runs the combined policy simulation when triggered from
#' \code{mod_3_scenario_server()}. Combines scenario parameters from the four
#' upstream sub-modules with the weather-welfare model and climate simulation
#' results to produce counterfactual welfare predictions.
#'
#' @param id               Module id.
#' @param connection_params Reactive named list from \code{mod_0_overview_server()}.
#' @param selected_outcome Reactive one-row data frame of selected outcome
#'   from \code{mod_1_modelling_server()}.
#' @param selected_weather Reactive data frame of selected weather variables
#'   from \code{mod_1_modelling_server()}.
#' @param survey_weather   Reactive data frame of merged survey-weather data
#'   from \code{mod_1_modelling_server()}.
#' @param model_fit        Reactive list of fitted model objects from
#'   \code{mod_1_modelling_server()}.
#' @param hist_sim         Reactive returning the historical simulation raw
#'   predictions list from \code{mod_2_simulation_server()}.
#' @param fut_sim          Reactive returning the future simulation raw
#'   predictions list from \code{mod_2_simulation_server()}. May be
#'   \code{NULL} if the future simulation has not yet been run.
#' @param sp_scenario      Reactive named list from \code{mod_3_01_sp_server()}.
#' @param infra_scenario   Reactive named list from \code{mod_3_02_infra_server()}.
#' @param digital_scenario Reactive named list from \code{mod_3_03_digital_server()}.
#' @param labor_scenario   Reactive named list from \code{mod_3_04_labor_server()}.
#'
#' @return \code{NULL} invisibly. All outputs are rendered directly into the
#'   session via \code{output$}.
#'
#' @noRd
mod_3_05_policy_sim_server <- function(id,
                                        connection_params  = reactive(NULL),
                                        selected_outcome   = reactive(NULL),
                                        selected_weather   = reactive(NULL),
                                        survey_weather     = reactive(NULL),
                                        model_fit          = reactive(NULL),
                                        hist_sim           = reactive(NULL),
                                        fut_sim            = reactive(NULL),
                                        sp_scenario        = reactive(NULL),
                                        infra_scenario     = reactive(NULL),
                                        digital_scenario   = reactive(NULL),
                                        labor_scenario     = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---- Simulation result storage --------------------------------------

    sim_result <- reactiveVal(NULL)

    # ---- Status banner --------------------------------------------------

    output$sim_status_ui <- renderUI({
      res <- sim_result()
      if (is.null(res)) return(NULL)
      if (inherits(res, "error")) {
        div(class = "alert alert-danger", conditionMessage(res))
      } else {
        div(class = "alert alert-success", "Simulation complete.")
      }
    })

    # ---- Results placeholder --------------------------------------------
    # Replace with tables/charts once fct_policy_sim.R is implemented.

    output$sim_results_ui <- renderUI({
      req(sim_result())
      res <- sim_result()
      if (inherits(res, "error")) return(NULL)
      tagList(
        h5("Policy simulation results"),
        p("Output charts and tables will appear here.")
      )
    })

    # ---- Run triggered externally via observeEvent in mod_3_scenario ---

    run <- function() {
      sim_result(NULL)
      result <- tryCatch(
        {
          # TODO: replace with call to fct_policy_sim() once implemented
          infra <- infra_scenario()
          list(
            sp_scenario      = sp_scenario(),
            infra_scenario   = infra,
            digital_scenario = digital_scenario(),
            labor_scenario   = labor_scenario(),
            # Pre-resolved infra policy flags for fct_predict_outcomes()
            infra_policy = list(
              electricity       = infra$elec_universal       || infra$elec_access_change_pct > 0,
              imp_wat_rec       = infra$water_universal      || infra$water_access_change_pct > 0,
              imp_san_rec       = infra$sanitation_universal || infra$sanitation_access_change_pct > 0,
              health_mode       = infra$health_mode,
              health_travel_pct = infra$health_travel_pct,
              health_travel_max = infra$health_travel_max
            )
          )
        },
        error = function(e) e
      )
      sim_result(result)
    }

    # Expose run() so mod_3_scenario can call it inside observeEvent
    list(run = run)
  })
}
