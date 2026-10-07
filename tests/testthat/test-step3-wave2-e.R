library(testthat)
library(shiny)

test_that("Step 3 stale indicators are present on diagnostics and decomposition panes", {
  diag_html <- as.character(htmltools::renderTags(.diagnostics_content_ui(shiny::NS("diagnostics")))$html)
  decomp_html <- as.character(htmltools::renderTags(mod_3_09_decomposition_ui("decomposition"))$html)
  expect_match(diag_html, "diagnostics-stale_banner_ui", fixed = TRUE)
  expect_match(decomp_html, "decomposition-stale_banner_ui", fixed = TRUE)
})

test_that("stale Step 3 export items are skipped without materialising old data", {
  item <- list(step = 3L, stale = function() TRUE,
               fun = function() stop("stale export must not be evaluated"))
  result <- .export_write_item(item, tempdir(), tempfile())
  expect_identical(result$status, "skipped")
  expect_match(result$note, "stale", ignore.case = TRUE)
})

test_that("stale bundle items are recorded as skipped, not exported", {
  zipfile <- tempfile(fileext = ".zip")
  item <- list(key = "stale_table", label = "Stale table", step = 3L,
               kind = "table", description = "suppressed", width = 10,
               height = 6, stale = function() TRUE,
               fun = function() stop("must not materialise"))
  manifest <- wise_export_bundle(zipfile, list(item), include = "tables")
  skipped <- attr(manifest, "skipped")
  expect_length(skipped, 1L)
  expect_identical(skipped[[1L]]$label, "Stale table")
  expect_false(any(grepl("stale_table", manifest$file)))
})

test_that("Step 3 preview labels and calculations use one debounced snapshot", {
  svy <- data.frame(welfare = 1:4, weight = 1)
  hs <- shiny::reactiveVal(list(svy = svy, so = list(name = "welfare")))
  testServer(mod_3_01_sp_server, args = list(
    id = "sp_preview", survey_weather = shiny::reactiveVal(svy),
    variable_list = shiny::reactiveVal(data.frame()),
    analysis_unit = shiny::reactiveVal("hh"), hist_sim = hs
  ), {
    session$userData$wise_analysis_seed <- 12345L
    session$setInputs(sp_type = "shock", transfer_amount_usd = 10)
    session$elapse(300); session$flushReact()
    snap <- sp_preview_inputs(); reach <- sp_reach()
    expect_identical(snap$display_type, "shock")
    expect_identical(reach$on_baseline, TRUE)
    expect_identical(
      reach[names(reach) != "on_baseline"],
      .sp_scenario_reach(svy, sp_scenario_spec(), "hh", seed = 12345L)
    )
  })
})

test_that("paired matrix transforms do not mutate aggregate inputs", {
  baseline <- data.frame(sim_year = c(2020L, 2021L), model_id = c("m1", "m1"),
                         value_all = I(list(c(1, 2), c(3, 4))),
                         value_all_sd = I(list(c(.1, .1), c(.1, .1))))
  policy <- baseline
  before <- baseline
  paired_model_year_effects(baseline, policy)
  expect_identical(baseline, before)
  expect_identical(policy, before)
})
