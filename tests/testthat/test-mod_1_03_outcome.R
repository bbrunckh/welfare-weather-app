# ============================================================================ #
# tests/testthat/test-mod_1_03_outcome.R                                       #
# Outcome stats renders only on button press: changing the outcome selector    #
# afterwards must not re-render the tab (snapshot binding, INT-05 pattern).    #
# ============================================================================ #

library(testthat)
library(shiny)

make_vl_outcome <- function() {
  data.frame(
    name    = c("welfare", "welf2"),
    label   = c("Welfare", "Welfare 2"),
    units   = c("", ""),
    type    = c("numeric", "numeric"),
    outcome = c(1L, 1L),
    stringsAsFactors = FALSE
  )
}

make_survey_df <- function() {
  set.seed(3)
  data.frame(
    welfare = rnorm(50, 5, 1),
    welf2   = rnorm(50, 5, 1),
    weight  = 1,
    stringsAsFactors = FALSE
  )
}

test_that("outcome stats tab re-renders only on button press", {
  skip_if_not_installed("shiny")

  plot_calls <- 0L
  real_plot <- plot_welfare_dist
  local_mocked_bindings(
    plot_welfare_dist = function(...) {
      plot_calls <<- plot_calls + 1L
      ggplot2::ggplot()
    }
  )

  shiny::testServer(
    mod_1_03_outcome_server,
    args = list(
      id             = "outcome",
      variable_list  = shiny::reactiveVal(make_vl_outcome()),
      survey_data    = shiny::reactiveVal(make_survey_df()),
      survey_version = shiny::reactiveVal(0L),
      tabset_id      = "step1_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      banner <- function() {
        paste(as.character(session$output$outcome_stale_banner), collapse = " ")
      }

      session$setInputs(outcome = "welfare")

      # ignoreInit quirk: prime the button counter, then press.
      session$setInputs(outcome_stats_btn = 0L)
      session$setInputs(outcome_stats_btn = 1L); settle()
      expect_equal(plot_calls, 1L)

      # Selector change without re-press: no re-render (the fix).
      session$setInputs(outcome = "welf2"); settle()
      expect_equal(plot_calls, 1L)
      # ...while the module API keeps publishing the live selection.
      expect_equal(selected_outcome()$name, "welf2")

      # Re-press: snapshot updates and the tab re-renders.
      session$setInputs(outcome_stats_btn = 2L); settle()
      expect_equal(plot_calls, 2L)
      # The rendered plot now describes the new outcome.
      spec <- outcome_spec()
      expect_equal(spec$info$name, "welf2")

      # Survey reload after the run: the stale banner appears (INT-08) and
      # clears again once the button is re-pressed.
      survey_version(1L); settle()
      expect_match(banner(), "Results are out of date", fixed = TRUE)
      expect_match(banner(), "Survey data was reloaded", fixed = TRUE)
      session$setInputs(outcome_stats_btn = 3L); settle()
      expect_identical(nchar(banner()), 0L)
    }
  )
})
