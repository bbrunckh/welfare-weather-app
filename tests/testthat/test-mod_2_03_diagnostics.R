# ============================================================================ #
# tests/testthat/test-mod_2_03_diagnostics.R                                   #
# INT-07: the Diagnostics tab follows the hist_sim lifecycle - appended on     #
# the first run, removed when the run is cleared, re-appended on a later run.  #
# ============================================================================ #

library(testthat)
library(shiny)

test_that("diagnostics tab is appended, removed on clear, re-appended on rerun", {
  skip_if_not_installed("shiny")

  hist_sim <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_2_03_diagnostics_server,
    args = list(
      id               = "diagnostics",
      hist_sim         = hist_sim,
      saved_scenarios  = shiny::reactiveVal(list()),
      survey_weather   = shiny::reactiveVal(NULL),
      selected_weather = shiny::reactiveVal(NULL),
      tabset_id        = "step2_output_tabs"
    ),
    {
      session$flushReact()
      expect_false(diag_tab_added())

      hist_sim(list()); session$flushReact()
      expect_true(diag_tab_added())

      # Clearing the run removes the tab (INT-07)
      hist_sim(NULL); session$flushReact()
      expect_false(diag_tab_added())

      # A later run re-inserts it
      hist_sim(list()); session$flushReact()
      expect_true(diag_tab_added())
    }
  )
})
