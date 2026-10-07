# ============================================================================ #
# tests/testthat/test-mod_1_01_sample.R                                        #
# Sample panel: no-data warning renders with icon, and year pickers are        #
# gated on the filtered survey list (stale economy selections must not         #
# render empty "Survey years -" inputs).                                       #
# ============================================================================ #

library(testthat)
library(shiny)

make_survey_list <- function() {
  data.frame(
    code     = c("bfa", "bfa", "sen"),
    economy  = c("Burkina Faso", "Burkina Faso", "Senegal"),
    year     = c(2018, 2021, 2022),
    survname = c("eaho", "eaho", "esopes"),
    level    = c("hh", "hh", "firm"),
    source   = "v1",
    stringsAsFactors = FALSE
  )
}

test_that("year picker renders with data; warning shows and picker hides without data", {

  data_dir <- tempfile("wise-data-")
  dir.create(file.path(data_dir, "microdata", "hh", "bfa"), recursive = TRUE)
  file.create(file.path(data_dir, "microdata", "hh", "bfa", "bfa_2018_eaho_v1_hh.parquet"))
  file.create(file.path(data_dir, "microdata", "hh", "bfa", "bfa_2021_eaho_v1_hh.parquet"))
  withr::defer(unlink(data_dir, recursive = TRUE))

  shiny::testServer(
    mod_1_01_sample_server,
    args = list(
      id                = "sample",
      connection_params = shiny::reactive(list(type = "local", path = data_dir)),
      survey_list       = shiny::reactive(make_survey_list()),
      variable_list     = shiny::reactive(NULL)
    ),
    {
      # Household with available files: economy + year pickers render
      session$setInputs(unit = "hh")
      expect_match(
        paste(as.character(output$sample_ui$html), collapse = " "),
        "Burkina Faso", fixed = TRUE
      )
      session$setInputs(economy = "bfa")
      years_html <- paste(as.character(output$survey_year_ui$html), collapse = " ")
      expect_match(years_html, "Survey years - Burkina Faso", fixed = TRUE)
      expect_match(years_html, "2018", fixed = TRUE)

      # Switch to Firm (no files available): warning with icon shows,
      # year picker must not render even though input$economy keeps its
      # stale "bfa" value (removed inputs retain their last value).
      session$setInputs(unit = "firm")
      warning_html <- paste(as.character(output$sample_ui$html), collapse = " ")
      expect_match(warning_html, "No data files found", fixed = TRUE)
      expect_match(warning_html, "triangle-exclamation", fixed = TRUE)
      yr_html <- tryCatch(
        paste(as.character(output$survey_year_ui$html), collapse = " "),
        error = function(e) ""
      )
      expect_false(nzchar(yr_html))
    }
  )
})
