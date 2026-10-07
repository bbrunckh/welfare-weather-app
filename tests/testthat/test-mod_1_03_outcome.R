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

  plot_calls <- 0L
  local_mocked_bindings(
    # The distribution chart renders through the echarts builder (guidelines
    # §7); the mock counts builder invocations, which is what the button-press
    # contract is about.
    echart_welfare_dist = function(...) {
      plot_calls <<- plot_calls + 1L
      NULL
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
      card <- function() {
        paste(as.character(session$output$selected_outcome_card), collapse = " ")
      }

      session$setInputs(outcome = "welfare")

      # ignoreInit quirk: prime the button counter, then press.
      session$setInputs(outcome_stats_btn = 0L)
      session$setInputs(outcome_stats_btn = 1L); settle()
      expect_equal(plot_calls, 1L)

      # ignoreInit quirk: prime the button counter, then press.
      session$setInputs(outcome_stats_btn = 0L)
      session$setInputs(outcome_stats_btn = 1L); settle()
      expect_equal(plot_calls, 1L)

      # Selection card describes the snapshot: label, raw name, direction
      # note; no raw "div" leak.
      expect_match(card(), "Welfare", fixed = TRUE)
      expect_match(card(), "welfare", fixed = TRUE)
      expect_match(card(),
        "Higher values indicate better outcomes", fixed = TRUE)
      expect_false(grepl("^\\s*div\\s*$", card()))

      # Selector change without re-press: no re-render (the fix).
      session$setInputs(outcome = "welf2"); settle()
      expect_equal(plot_calls, 1L)
      # ...while the module API keeps publishing the live selection.
      expect_equal(selected_outcome()$name, "welf2")
      # The card still describes the button-time snapshot (welfare).
      expect_match(card(), "welfare", fixed = TRUE)

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

test_that("outcome map switches between coverage and mean-value views", {

  geo <- data.frame(
    h3   = c("8975492ffffffff", "8975493ffffffff"),
    geom = rep('{"type":"Polygon"}', 2),
    xmin = c(-19.6, -19.2), ymin = c(27, 27),
    xmax = c(-19.2, -18.8), ymax = c(27.4, 27.4),
    stringsAsFactors = FALSE
  )
  cmap <- data.frame(
    code = "TST", year = "2021", survname = "SRV",
    loc_id = paste0("L", 1:2), h3 = geo$h3, pop_2020 = c(10, 20),
    stringsAsFactors = FALSE
  )
  cd <- shiny::reactive(list(geom = geo, map = cmap))

  fake_pl <- list(
    payload = list(action = "set", bounds = c(0, 0, 1, 1)),
    legend = list(
      pal_info = list(pal = function(v) rep("#000000", length(v)),
                      domain = c(0, 100)),
      binned = FALSE, title = "t", info = "i"
    )
  )
  cov_calls <- 0L
  mean_calls <- 0L
  local_mocked_bindings(
    .coverage_hex_payload = function(...) {
      cov_calls <<- cov_calls + 1L
      fake_pl
    },
    .outcome_mean_hex_payload = function(...) {
      mean_calls <<- mean_calls + 1L
      fake_pl
    }
  )

  shiny::testServer(
    mod_1_03_outcome_server,
    args = list(
      id             = "outcome",
      variable_list  = shiny::reactiveVal(make_vl_outcome()),
      survey_data    = shiny::reactiveVal(make_survey_df()),
      survey_version = shiny::reactiveVal(0L),
      cell_data      = cd,
      tabset_id      = "step1_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }

      session$setInputs(outcome = "welfare")
      # ignoreInit quirk: prime the button counter, then press.
      session$setInputs(outcome_stats_btn = 0L)
      session$setInputs(outcome_stats_btn = 1L); settle()

      # Default view is mean value. The isolated fit-key read prevents the
      # observer from running a second time after storing that key.
      cov0 <- cov_calls; mean0 <- mean_calls
      expect_equal(mean0, 1L)
      expect_equal(cov0, 0L)
      settle()
      expect_equal(mean_calls, mean0)

      # Switch to coverage: the coverage payload builder takes over.
      session$setInputs(cov_view = "coverage"); settle()
      expect_gt(cov_calls - cov0, 0L)
      expect_equal(mean_calls - mean0, 0L)

      # Back to mean value: no further coverage payloads.
      cov1 <- cov_calls; mean1 <- mean_calls
      session$setInputs(cov_view = "mean"); settle()
      expect_gt(mean_calls - mean1, 0L)
      expect_equal(cov_calls - cov1, 0L)
    }
  )
})

make_wave_survey_df <- function() {
  set.seed(7)
  data.frame(
    code     = "TST",
    economy  = "Testland",
    year     = rep(c("2020", "2021"), each = 20),
    survname = "SRV",
    welfare  = c(rnorm(20, 5, 1), rnorm(20, 8, 1)),
    weight   = 1,
    stringsAsFactors = FALSE
  )
}

test_that("PERF-41: summary table shows deciles and switches waves via the pill", {

  local_mocked_bindings(
    echart_welfare_dist = function(...) NULL
  )

  shiny::testServer(
    mod_1_03_outcome_server,
    args = list(
      id             = "outcome",
      variable_list  = shiny::reactiveVal(make_vl_outcome()),
      survey_data    = shiny::reactiveVal(make_wave_survey_df()),
      survey_version = shiny::reactiveVal(0L),
      tabset_id      = "step1_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }

      session$setInputs(outcome = "welfare")
      session$setInputs(outcome_stats_btn = 0L)
      session$setInputs(outcome_stats_btn = 1L); settle()

      # The pill renders with All + one entry per wave, labelled like the
      # map's wave toggle (years only for a single-country sample).
      pill <- session$output$summary_wave_ui
      expect_false(is.null(pill))
      pill_html <- paste(as.character(pill), collapse = " ")
      expect_match(pill_html, "All", fixed = TRUE)
      expect_match(pill_html, "2021", fixed = TRUE)
      expect_false(grepl("Testland, 2021", pill_html, fixed = TRUE))

      # The heading names the captured outcome and moves the descriptive
      # note into the (i) popout.
      heading_html <- paste(as.character(session$output$summary_heading_ui),
                            collapse = " ")
      expect_match(heading_html, "Welfare summary stats", fixed = TRUE)
      expect_match(heading_html, "sample-weighted", fixed = TRUE)
      expect_match(heading_html, "circle-info", fixed = TRUE)

      # Pooled table: 40 observations, deciles present. The summary is a
      # Reactable widget; its JSON payload surfaces in testServer.
      html_all <- paste(session$output$outcome_summary_stats, collapse = " ")
      expect_match(html_all, "Median (P50)", fixed = TRUE)
      expect_match(html_all, "P90", fixed = TRUE)
      expect_match(html_all, "[\"40\"", fixed = TRUE)

      # Switching the pill re-slices without recomputation: the wave table
      # describes 20 observations and a different mean.
      session$setInputs(summary_wave = "TST|2021|SRV"); settle()
      html_21 <- paste(session$output$outcome_summary_stats, collapse = " ")
      expect_match(html_21, "[\"20\"", fixed = TRUE)
      expect_false(identical(html_all, html_21))
    }
  )
})

test_that("PERF-41: summary wave pill is hidden for single-wave data", {

  local_mocked_bindings(
    echart_welfare_dist = function(...) NULL
  )

  single <- make_wave_survey_df()
  single <- single[single$year == "2020", ]

  shiny::testServer(
    mod_1_03_outcome_server,
    args = list(
      id             = "outcome",
      variable_list  = shiny::reactiveVal(make_vl_outcome()),
      survey_data    = shiny::reactiveVal(single),
      survey_version = shiny::reactiveVal(0L),
      tabset_id      = "step1_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      session$setInputs(outcome = "welfare")
      session$setInputs(outcome_stats_btn = 0L)
      session$setInputs(outcome_stats_btn = 1L); settle()

      expect_null(session$output$summary_wave_ui)
      html <- paste(session$output$outcome_summary_stats, collapse = " ")
      expect_match(html, "[\"20\"", fixed = TRUE)
    }
  )
})
