# ============================================================================ #
# tests/testthat/test-mod_1_05_weatherstats.R (selector-inert tabs)            #
# The Weather stats tab renders from the button-time selection snapshot:       #
# changing the weather-variable or outcome selections afterwards must not      #
# recompute or re-render anything until the button is pressed again.           #
# ============================================================================ #

library(testthat)
library(shiny)

# Weather rows in the shape get_weather()$historical takes before
# merge_survey_weather() joins them onto the survey frame.
make_wx_rows <- function(var = "tx") {
  data.frame(
    code      = "TST",
    year      = "2021",
    survname  = "SRV",
    loc_id    = "L1",
    timestamp = as.Date("2021-06-01"),
    value     = c(25, 30),
    check.names = FALSE
  ) |> (\(d) { names(d)[6] <- var; d })()
}

make_wx_survey <- function() {
  data.frame(
    code      = "TST",
    year      = "2021",
    survname  = "SRV",
    loc_id    = "L1",
    timestamp = as.Date("2021-06-01"),
    economy   = "Testland",
    weight    = 1,
    welfare   = c(1, 2),
    stringsAsFactors = FALSE
  )
}

make_wx_selected <- function(name = "tx", label = "Max temp") {
  data.frame(
    name           = name,
    label          = label,
    units          = "C",
    cont_binned    = "Continuous",
    transformation = "None",
    stringsAsFactors = FALSE
  )
}

test_that("weather stats tab is inert to selector changes until re-pressed", {
  skip_if_not_installed("shiny")

  loc_calls <- 0L
  real_swl  <- summarise_weather_by_loc
  local_mocked_bindings(
    get_weather = function(survey_data, selected_surveys, selected_weather,
                           dates, connection_params) {
      rows <- make_wx_rows(selected_weather$name[1])
      structure(
        list(historical = rows),
        stored_breaks       = NULL,
        continuous_weather  = rows
      )
    },
    summarise_weather_by_loc = function(...) {
      loc_calls <<- loc_calls + 1L
      real_swl(...)
    }
  )

  sel_weather_rv <- shiny::reactiveVal(make_wx_selected())
  sel_outcome_rv <- shiny::reactiveVal(data.frame(
    name = "welfare", label = "Welfare", type = "numeric",
    transform = "none", stringsAsFactors = FALSE
  ))

  shiny::testServer(
    mod_1_05_weatherstats_server,
    args = list(
      id                = "weatherstats",
      connection_params = shiny::reactiveVal(list()),
      variable_list     = shiny::reactiveVal(data.frame()),
      selected_surveys  = shiny::reactiveVal(data.frame(
        fname = "microdata/TST/TST_2021_SRv_NAT.parquet")),
      selected_outcome  = sel_outcome_rv,
      selected_weather  = sel_weather_rv,
      survey_data       = shiny::reactiveVal(make_wx_survey()),
      tabset_id         = "step1_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      press  <- function(n) {
        # join_hist_sample_cells warns many-to-many on the tiny two-row
        # fixture; the warning is fixture-only, not app behaviour.
        suppressWarnings(session$setInputs(weather_stats = n))
      }

      # ignoreInit quirk: prime the button counter, then press.
      press(0L)
      press(1L); settle()
      expect_false(is.null(wx_spec()))
      expect_equal(loc_calls, 1L)  # one selected variable, summarised once

      # Weather-variable selector change (mod_1_04 republishes) without a
      # re-press: nothing recomputes.
      sel_weather_rv(make_wx_selected(label = "Max temp reconfigured")); settle()
      expect_equal(loc_calls, 1L)
      # The on-screen configuration is still the one the button captured.
      expect_equal(wx_spec()$sw$label, "Max temp")

      # Outcome selector change: no recomputation either.
      sel_outcome_rv(data.frame(name = "welfare", label = "Welfare 2",
                                type = "numeric", transform = "none",
                                stringsAsFactors = FALSE)); settle()
      expect_equal(loc_calls, 1L)

      # Re-press: the new selection is captured and recomputed.
      press(2L); settle()
      expect_equal(loc_calls, 2L)
      expect_equal(wx_spec()$sw$label, "Max temp reconfigured")
    }
  )
})
