# Static contract for the export affordances embedded in module UI/server code.
# This deliberately checks literal keys only; dynamic keys have an explicit
# expected family below so the contract remains readable and intentional.

library(testthat)

.export_wiring_files <- function() {
  list.files(testthat::test_path("..", "..", "R"),
             pattern = "\\.R$", full.names = TRUE)
}

.export_wiring_text <- function() {
  skip_if(!length(.export_wiring_files()),
    "R/ source tree not available (installed package)")
  paste(vapply(.export_wiring_files(), function(path) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  }, character(1)),
        collapse = "\n")
}

.literal_keys <- function(text, pattern) {
  hits <- gregexpr(pattern, text, perl = TRUE)
  m <- regmatches(text, hits)[[1L]]
  if (!length(m)) return(character(0))
  sub(".*['\"]([^'\"]+)['\"].*", "\\1", m)
}

test_that("every literal CSV export key is registered", {
  text <- .export_wiring_text()
  csv_keys <- unique(c(
    .literal_keys(text, "wise_csv_button\\(\\s*['\"][^'\"]+['\"]"),
    .literal_keys(text, "csv_download_handler\\(\\s*['\"][^'\"]+['\"]")
  ))
  registered <- .literal_keys(
    text,
    "wise_export_(?:table|figure)\\(\\s*key\\s*=\\s*['\"][^'\"]+['\"]"
  )
  expect_equal(
    setdiff(csv_keys, registered),
    character(0),
    info = "Every visible/download CSV key must have a bundle export registration."
  )
})

test_that("all explicit export registrations use unique keys", {
  text <- .export_wiring_text()
  keys <- .literal_keys(
    text,
    "wise_export_(?:table|figure)\\(\\s*key\\s*=\\s*['\"][^'\"]+['\"]"
  )
  expect_length(keys, length(unique(keys)))
})

test_that("dynamic export families have explicit coverage", {
  text <- .export_wiring_text()
  expect_match(text, "paste0\\(\"weather_distribution_\", idx", fixed = FALSE)
  expect_match(text, "paste0\\(\"weather_distribution_continuous_\", idx", fixed = FALSE)
  expect_match(text, "paste0\\(\"binscatter_\", idx", fixed = FALSE)
  expect_match(text, "paste0\\(\"policy_before_after_\", var_name", fixed = FALSE)
  expect_match(text, "paste0\\(\"policy_rif_weather_curve_\", i", fixed = FALSE)

  # The corresponding render loops must remain present beside the registered
  # families, preventing a family from becoming a dead export-only surface.
  expect_match(text, "output\\[\\[paste0\\(\"hist_\", var_name\\)\\]\\]", fixed = FALSE)
  expect_match(text, "output\\$coefplot1", fixed = FALSE)
  expect_match(text, "output\\$weather_stats_table", fixed = FALSE)
})

test_that("known UI-only outputs are not mistaken for exportable artefacts", {
  text <- .export_wiring_text()
  ui_only <- c(
    "selected_weather_card", "map_legend_ui", "hist_plots_ui",
    "treatment_explanation_ui", "policy_summary_ui", "model_summary_ui"
  )
  for (id in ui_only) {
    expect_match(text, paste0("output\\$", id), fixed = FALSE)
  }
})
