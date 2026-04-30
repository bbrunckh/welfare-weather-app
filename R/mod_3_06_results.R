#' 3_06_results UI Function
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
mod_3_06_results_ui <- function(id) {
  tagList()
}

#' 3_06_results Server Functions
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
#' @param sim_run_id               Reactive trigger; tabs appended once
#'   this is > 0.
#' @param tabset_id                Char id of the parent tabset.
#' @param tabset_session           Shiny session for the parent tabset.
#'
#' @noRd
mod_3_06_results_server <- function(id,
                                     baseline_hist_sim,
                                     baseline_saved_scenarios,
                                     policy_hist_sim,
                                     policy_saved_scenarios,
                                     selected_hist  = reactive(NULL),
                                     sim_run_id     = reactive(0L),
                                     tabset_id,
                                     tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    if (is.null(tabset_session))
      tabset_session <- session$parent %||% session

    tabs_added <- reactiveVal(FALSE)

    .wire_results_pane(input, output, session, "base",
                       hist_sim        = baseline_hist_sim,
                       saved_scenarios = baseline_saved_scenarios,
                       selected_hist   = selected_hist)
    .wire_results_pane(input, output, session, "pol",
                       hist_sim        = policy_hist_sim,
                       saved_scenarios = policy_saved_scenarios,
                       selected_hist   = selected_hist)

    observeEvent(sim_run_id(), {
      req(sim_run_id() > 0)

      if (!tabs_added()) {
        bs <- baseline_hist_sim()
        if (is.null(bs) || is.null(bs$so)) return()

        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Baseline",
            value = "baseline_tab",
            .results_pane_ui(ns, "base", bs$so)
          ),
          select  = TRUE,
          session = tabset_session
        )
        shiny::appendTab(
          inputId = tabset_id,
          shiny::tabPanel(
            title = "Policy",
            value = "policy_tab",
            .results_pane_ui(ns, "pol", bs$so)
          ),
          session = tabset_session
        )
        tabs_added(TRUE)
      }

      try(shiny::updateTabsetPanel(tabset_session, inputId = tabset_id,
                                    selected = "baseline_tab"), silent = TRUE)
    }, ignoreInit = TRUE)

    invisible(NULL)
  })
}
