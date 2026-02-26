#' 2_06_sim_compare UI Function
#'
#' @description A shiny Module. Compares historical and future simulation
#'   results. No sidebar UI — outputs appended directly to the main tabset.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_06_sim_compare_ui <- function(id) {
  # No sidebar UI for this module — outputs are appended to the tabset
  tagList()
}

#' 2_06_sim_compare Server Functions
#'
#' Appends a Compare tab to the main tabset showing exceedance curves
#' and poverty rate comparisons across historical and future scenarios.
#'
#' @param id             Module id.
#' @param hist_sim       Reactive list of historical simulation phase results.
#' @param selected_fut    Reactive one-row data frame from mod_2_03_future.
#' @param fut_sim        Reactive list of future simulation phase results.
#' @param tabset_id      Character id of the parent tabset panel.
#' @param tabset_session Shiny session for the tabset.
#'
#' @noRd
mod_2_06_sim_compare_server <- function(id,
                                        selected_hist = NULL,
                                        selected_fut = NULL,
                                        hist_sim = NULL,
                                        fut_sim = NULL,
                                        tabset_id,
                                        tabset_session = NULL) {
  moduleServer(id, function(input, output, session) {

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # TODO: implement comparison tab (exceedance curves, poverty rate plots)

  })
}