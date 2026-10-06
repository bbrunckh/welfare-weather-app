# ============================================================================ #
# tests/testthat/test-mod_1_04_weather.R                                       #
# Sidebar weather configuration card: pipeline stages render from the live     #
# selection (window -> aggregation -> transformation -> form), no "div" leak.  #
# ============================================================================ #

library(testthat)
library(shiny)

make_vl_weather <- function() {
  data.frame(
    name   = c("tx", "pr"),
    label  = c("Monthly max temperature", "Monthly precipitation"),
    units  = c("degC", "mm"),
    hazard = c(1L, 1L),
    stringsAsFactors = FALSE
  )
}

test_that("weather configuration card renders pipeline stages live", {
  skip_if_not_installed("shiny")

  shiny::testServer(
    mod_1_04_weather_server,
    args = list(
      id               = "weather",
      variable_list    = shiny::reactiveVal(make_vl_weather())
    ),
    {
      session$setInputs(weather_variable_selector = "tx")
      # per-variable spec inputs left unset -> weather_spec_defaults()
      html <- paste(as.character(output$weather_summary_ui), collapse = " ")

      # Headerless, compact card: no title, badge carries the history range
      expect_false(grepl("Weather configuration", html))
      expect_match(html, "selection-card compact", fixed = TRUE)
      expect_match(html, "History 1991-2020", fixed = TRUE)
      expect_match(html, "Max temperature", fixed = TRUE)  # "Monthly" stripped
      expect_match(html, "1 month before interview", fixed = TRUE)
      # Single-month window: no aggregation stage
      expect_false(grepl("selection-card-symbol", html))
      # degC default transformation is "None" -> stage skipped
      expect_false(grepl("Deviation from mean", html))
      expect_match(html, "5 equal-frequency bins", fixed = TRUE)  # degC default
      expect_match(html, "selection-card-stage", fixed = TRUE)
      expect_false(grepl(">div<", html))
      # No separate history row
      expect_false(grepl("Historical comparison</span>", html, fixed = TRUE))

      # A second variable appends its own row.
      session$setInputs(weather_variable_selector = c("tx", "pr"))
      html2 <- paste(as.character(output$weather_summary_ui), collapse = " ")
      expect_match(html2, "Precipitation", fixed = TRUE)
    }
  )
})
