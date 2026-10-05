#' 3_07_results UI Function
#'
#' @description A shiny Module. The Baseline and Policy results tabs are
#'   inserted into the parent tabset on the first successful policy
#'   simulation run, so this UI returns nothing.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_3_07_results_ui <- function(id) {
  tagList()
}

#' 3_07_results Server Functions
#'
#' Renders Baseline and Policy results tabs in the Step 3 tabset, each a
#' faithful copy of Step 2's Results UI bound to its own re-simulated
#' \code{hist_sim} / \code{saved_scenarios} reactives. Tabs are appended on
#' the first successful policy simulation run.
#'
#' @param id                       Module id.
#' @param baseline_hist_sim        Reactive (Step 2 schema) for baseline.
#' @param baseline_saved_scenarios Reactive named scenario list (baseline).
#' @param policy_hist_sim          Reactive (Step 2 schema) for policy.
#' @param policy_saved_scenarios   Reactive named scenario list (policy).
#' @param selected_hist            Reactive one-row historical metadata.
#' @param selected_policies        Reactive selected policy scenario keys.
#' @param sim_run_id               Reactive trigger; tabs appended once
#'   this is > 0.
#' @param tabset_id                Char id of the parent tabset.
#' @param tabset_session           Shiny session for the parent tabset.
#'
#' @noRd
mod_3_07_results_server <- function(id,
                                    baseline_hist_sim,
                                    baseline_saved_scenarios,
                                    policy_hist_sim,
                                    policy_saved_scenarios,
                                    selected_hist = reactive(NULL),
                                    sim_run_id = reactive(0L),
                                    tabset_id,
                                    tabset_session = NULL,
                                    selected_policies = reactive(NULL),
                                    policy_scenarios = reactive(list()),
                                    sp_scenario = reactive(NULL),
                                    infra_scenario = reactive(NULL),
                                    digital_scenario = reactive(NULL),
                                    labor_scenario = reactive(NULL),
                                    education_scenario = reactive(NULL),
                                    residuals = reactive("original"),
                                    stale = reactive(FALSE),
                                    decomp_scenarios = reactive(list()),
                                    decomp_context = reactive(NULL),
                                    baseline_svy = reactive(NULL),
                                    policy_svy = reactive(NULL),
                                    aggregation_cache = NULL,
                                     analysis_unit = reactive(NULL),
                                     annual_channels = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session$userData$wise_step3_stale <- stale
    if (is.null(tabset_session)) {
      tabset_session <- session$parent %||% session
    }
    select_tab <- function(value) {
      if (is.null(tabset_id) || !nzchar(tabset_id)) {
        return(invisible(FALSE))
      }
      try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id, selected = value), silent = TRUE)
      invisible(TRUE)
    }

    tabs_added <- reactiveVal(FALSE)

    results_api <- .wire_results_pane(
      input, output, session,
      baseline_hist_sim = baseline_hist_sim,
      baseline_saved_scenarios = baseline_saved_scenarios,
      policy_hist_sim = policy_hist_sim,
      policy_saved_scenarios = policy_saved_scenarios,
      selected_hist = selected_hist,
      selected_policies = selected_policies,
      policy_scenarios = policy_scenarios,
      sp_scenario = sp_scenario,
      infra_scenario = infra_scenario,
      digital_scenario = digital_scenario,
      labor_scenario = labor_scenario,
      education_scenario = education_scenario,
      residuals = residuals,
      stale = stale,
      decomp_scenarios = decomp_scenarios,
      decomp_context = decomp_context,
      baseline_svy = baseline_svy,
      policy_svy = policy_svy,
      aggregation_cache = aggregation_cache,
      analysis_unit = analysis_unit,
      annual_channels = annual_channels
    )

    observeEvent(sim_run_id(),
      {
        req(sim_run_id() > 0)
        # sim_run_id is incremented inside the policy_sim run() after the
        # reactiveVals are set, but if a downstream step (e.g. decomposition)
        # silently fails the increment can still fire with NULL hist_sim values.
        # Guard explicitly so we never try to read $so on NULL.
        req(baseline_hist_sim(), policy_hist_sim())

        if (!tabs_added()) {
          bs <- baseline_hist_sim()
          if (is.null(bs) || is.null(bs$so)) {
            return()
          }
          shiny::appendTab(
            inputId = tabset_id,
            shiny::tabPanel(
              title = "Results",
              value = "results_tab",
              # Insert the module UI after the tab exists in the browser. This
              # mirrors Step 2 and lets Shiny bind the nested outputs/inputs.
              shiny::div(id = ns("results_section"))
            ),
            select = TRUE,
            session = tabset_session
          )
          w_var <- bs$weather_var %||% bs$sim_summary$weather %||% NULL
          shiny::insertUI(
            selector = paste0("#", ns("results_section")),
            where = "afterBegin",
            ui = .results_pane_ui(ns, bs$so, weather_var = w_var),
            session = session
          )
          tabs_added(TRUE)
        }

        if (tabs_added()) select_tab("results_tab")
      },
      ignoreInit = TRUE
    )

    # Expose the current uncertainty settings to sibling tabs.
    list(
      show_coef_uncertainty = reactive(isTRUE(input$show_coef_uncertainty)),
      aggregation_method = results_api$aggregation_method,
      poverty_line = results_api$poverty_line,
      focus_scenario = results_api$focus_scenario,
      metric_context = results_api$metric_context,
      metric_decomposition = results_api$metric_decomposition,
      analysis_unit = analysis_unit
    )
  })
}
