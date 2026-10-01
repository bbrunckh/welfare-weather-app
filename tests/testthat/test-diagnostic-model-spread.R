test_that("model spread identifies members and uses scenario colours and orange median", {
  ts <- expand.grid(sim_year = 2001:2006,
    model_id = c("GFDL-ESM4/r1i1p1f1", "MPI-ESM1-2-HR/r2i1p1f1"),
    scenario = c("Historical", "SSP3-7.0 / 2025-2035"), stringsAsFactors = FALSE)
  ts$value <- .4 + sin(seq_len(nrow(ts))) / 100
  chart <- echart_model_robustness(model_robustness_data(ts), "Poverty rate (percent)")
  models <- chart$x$opts$series[[1L]]
  expect_identical(models$data[[1L]]$model, ts$model_id[[1L]])
  expect_equal(models$data[[1L]]$n_years, 6)
  expect_identical(models$data[[1L]]$itemStyle$color, .wise_history)
  future <- Filter(function(p) grepl("SSP3", p$scenario), models$data)
  expect_identical(future[[1L]]$itemStyle$color, unname(.ssp_colours[["SSP3-7.0"]]))
  expect_identical(chart$x$opts$series[[2L]]$itemStyle$color, .wise_policy)
  expect_identical(chart$x$opts$xAxis$nameLocation, "middle")
  expect_gte(chart$x$opts$grid$bottom, 60)
  expect_match(as.character(chart$x$opts$tooltip$formatter), "Model/member", fixed = TRUE)
  expect_match(as.character(chart$x$opts$tooltip$formatter), "percent = true", fixed = TRUE)
  expect_match(as.character(chart$x$opts$xAxis$axisLabel$formatter), "v*100", fixed = TRUE)
})
