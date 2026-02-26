#' 2_05_sim_diag UI Function
#'
#' @description A shiny Module. Displays simulation diagnostics.
#'   No sidebar UI — outputs appended directly to the main tabset.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_05_sim_diag_ui <- function(id) {
  # No sidebar UI for this module — outputs are appended to the tabset
  tagList()
}

#' 2_05_sim_diag Server Functions
#'
#' Appends a Diagnostics tab to the main tabset showing simulation
#' phase status and prediction diagnostics.
#'
#' @param id             Module id.
#' @param hist_sim       Reactive list of historical simulation phase results.
#' @param fut_sim        Reactive list of future simulation phase results.
#' @param tabset_id      Character id of the parent tabset panel.
#' @param tabset_session Shiny session for the tabset.
#'
#' @noRd
mod_2_05_sim_diag_server <- function(id,
                                      hist_sim = NULL,
                                      fut_sim = NULL,
                                      tabset_id,
                                      tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # TODO: implement diagnostics tab (phase status, NA rates, pred summaries)

  })
}