# Batch 2 UI migration contracts for the Step 3 surfaces:
# mod_3_08_diagnostics, mod_3_09_decomposition, and the policy-sim results
# pane in fct_policy_sim_compare.R (guidelines §6/§7). The ggplot builders
# stay intact; these tests assert the new echarts/reactable structures.

library(testthat)
library(shiny)

# ---------------------------------------------------------------------------
# mod_3_08 before/after histogram builder
# ---------------------------------------------------------------------------

test_that("echart_before_after_hist draws grouped bars for binary variables", {
  w <- echart_before_after_hist(
    c(0, 1, 1, 0, 1), c(1, 1, 0, 0, 0), "electricity"
  )
  expect_s3_class(w, "echarts4r")
  expect_length(w$x$opts$series, 2L)
  expect_true(all(vapply(w$x$opts$series, function(s)
    identical(s$type, "bar"), logical(1L))))
  # echarts4r wraps the single x axis in a length-1 list.
  expect_match(w$x$opts$xAxis[[1]]$name, "electricity", fixed = TRUE)
  # ggplot showed a legend at the top for the binary case.
  expect_identical(w$x$opts$legend$top, 0)
})

test_that("echart_before_after_hist draws ridge polygons for continuous variables", {
  set.seed(1)
  b <- rlnorm(400)
  p <- rlnorm(400, meanlog = 0.2)
  w <- echart_before_after_hist(b, p, "welfare")
  expect_s3_class(w, "echarts4r")
  expect_length(w$x$opts$series, 2L)
  expect_true(all(vapply(w$x$opts$series, function(s)
    identical(s$type, "line") && !is.null(s$areaStyle), logical(1L))))
  # One ridge polygon per group; the polygon's lower edge sits at the
  # group's ridge row (Baseline = 1 bottom, policy = 2 on top) and the
  # series data carries the polygon as an n x 2 matrix.
  mins <- vapply(w$x$opts$series, function(s) min(s$data[, 2]), numeric(1L))
  expect_setequal(floor(mins), c(1, 2))
})

test_that("echart_before_after_hist keeps the empty-state message", {
  w <- echart_before_after_hist(numeric(0), numeric(0), "x")
  expect_s3_class(w, "echarts4r")
  expect_match(w$x$opts$title[[1]]$text, "No data available", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# mod_3_09 decomposition builders
# ---------------------------------------------------------------------------

test_that("echart_decomposition_headline draws one bar series per scenario", {
  df <- decomposition_summary_data(
    data.frame(
      delta_total = c(0.10, 0.10), delta_main = c(0.07, 0.07),
      delta_sp = c(0.04, 0.04), delta_res1 = c(0.01, 0.01),
      delta_res2 = c(0.02, 0.02), weight = c(1, 1)
    ),
    is_rif = TRUE
  )
  df <- rbind(
    transform(df, scenario = "Historical"),
    transform(df, scenario = "SSP2-4.5 / 2030")
  )
  w <- echart_decomposition_headline(df)
  expect_s3_class(w, "echarts4r")
  expect_length(w$x$opts$series, 2L)
  expect_true(all(vapply(w$x$opts$series, function(s)
    identical(s$type, "bar"), logical(1L))))
  # Dashed zero reference line rides on the first series.
  expect_identical(w$x$opts$series[[1]]$markLine$data[[1]]$yAxis, 0)
  # Scenario legend sits at the bottom-left.
  expect_identical(w$x$opts$legend$top, "bottom")
  expect_identical(w$x$opts$legend$left, 0)
})

test_that("echart_decomposition_headline keeps the empty-state message", {
  w <- echart_decomposition_headline(NULL)
  expect_s3_class(w, "echarts4r")
  expect_match(w$x$opts$title[[1]]$text, "Decomposition is unavailable.", fixed = TRUE)
})

test_that("echart_decomposition_channels_by_decile stacks channels and marks totals", {
  set.seed(3)
  rows <- data.frame(
    id = 1:100, decile = rep(1:10, each = 10),
    delta_main = rnorm(100, 0.05), delta_sp = rnorm(100, 0.03),
    delta_res1 = rnorm(100, 0.01), delta_res2 = rnorm(100, 0.01),
    delta_total = rnorm(100, 0.07), weight = rep(1, 100)
  )
  tbl <- decomposition_channels_by_decile(rows, svy = NULL)
  w <- echart_decomposition_channels_by_decile(tbl, is_rif = TRUE)
  expect_s3_class(w, "echarts4r")
  types <- vapply(w$x$opts$series, function(s) s$type, character(1L))
  expect_true(all(types == "bar" | types == "scatter"))
  expect_identical(sum(types == "scatter"), 1L) # total-effect marker series
  expect_identical(w$x$opts$series[[1]]$stack, "channels")
  expect_true(all(vapply(w$x$opts$series[types == "bar"], function(s)
    identical(s$stack, "channels"), logical(1L))))
})

test_that("echart_decomposition_channels_by_decile keeps the empty-state message", {
  w <- echart_decomposition_channels_by_decile(NULL)
  expect_s3_class(w, "echarts4r")
  expect_match(
    w$x$opts$title[[1]]$text,
    "Decile decomposition is unavailable for this run.",
    fixed = TRUE
  )
})

# ---------------------------------------------------------------------------
# mod_3_09 RIF beta curves
# ---------------------------------------------------------------------------

test_that("echart_rif_weather_curve draws ribbon plus curve for a single term", {
  rg <- expand.grid(model = 3L, term = "temp", tau = seq(0.1, 0.9, by = 0.1))
  rg$estimate <- rnorm(nrow(rg))
  rg$std.error <- 0.05
  rg$conf.low <- rg$estimate - 1.96 * rg$std.error
  rg$conf.high <- rg$estimate + 1.96 * rg$std.error
  w <- echart_rif_weather_curve(rg, "temp", label_fun = identity)
  expect_s3_class(w, "echarts4r")
  # CI ribbon (transparent line + area) and the beta curve itself.
  expect_length(w$x$opts$series, 2L)
  expect_null(w$x$opts$legend)
  expect_match(w$x$opts$yAxis$name, "Effect", fixed = TRUE)
  expect_match(w$x$opts$xAxis$name, "Welfare quantile", fixed = TRUE)
  # Zero reference line rides on the first series.
  expect_identical(w$x$opts$series[[1]]$markLine$data[[1]]$yAxis, 0)
})

test_that("echart_rif_weather_curve draws one series per moderator level", {
  rg <- expand.grid(
    model = 3L, term = c("temp", "temp:electricity"),
    tau = seq(0.1, 0.9, by = 0.1)
  )
  rg$estimate <- rnorm(nrow(rg))
  rg$std.error <- 0.05
  rg$conf.low <- rg$estimate - 1.96 * rg$std.error
  rg$conf.high <- rg$estimate + 1.96 * rg$std.error
  w <- echart_rif_weather_curve(
    rg, "temp",
    interaction_terms = "temp:electricity", label_fun = identity
  )
  expect_s3_class(w, "echarts4r")
  # Two curves + two ribbons, with a bottom-left legend naming both levels.
  expect_length(w$x$opts$series, 4L)
  expect_identical(w$x$opts$legend$top, "bottom")
  labs <- vapply(w$x$opts$legend$data, as.character, character(1L))
  expect_setequal(labs, c("electricity: no", "electricity: yes"))
})

test_that("echart_rif_weather_curve keeps the missing-term message", {
  rg <- expand.grid(model = 3L, term = "temp", tau = c(0.25, 0.5, 0.75))
  rg$estimate <- c(1, 2, 3)
  rg$std.error <- 0.1
  rg$conf.low <- rg$estimate - 0.2
  rg$conf.high <- rg$estimate + 0.2
  w <- echart_rif_weather_curve(rg, "rain", label_fun = identity)
  expect_s3_class(w, "echarts4r")
  expect_match(w$x$opts$title[[1]]$text, "No RIF terms found for 'rain'.", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# mod_3_08 reactable tables
# ---------------------------------------------------------------------------

test_that("policy input table raw variant carries unrounded numerics", {
  inputs <- data.frame(
    variable = "electricity", mean_baseline = 0.5, mean_policy = 1,
    delta_mean = 0.5, sd_baseline = sqrt(0.5), sd_policy = 0
  )
  raw <- .policy_input_table_raw(inputs)
  expect_s3_class(raw, "data.frame")
  expect_true(all(vapply(raw[-1], is.numeric, logical(1L))))
  expect_identical(names(raw)[1], "Variable")
  # The export formatter keeps its rounded-string display behaviour.
  fmt <- .format_policy_input_table(inputs)
  expect_identical(fmt$`Baseline mean`, fmt_num(0.5, digits = 2))
})

test_that("diagnostics reactable styling follows the mod_1_02 pattern", {
  w <- .wise_diag_reactable(data.frame(
    Variable = "electricity",
    `Baseline mean` = 0.5,
    check.names = FALSE
  ))
  expect_s3_class(w, "reactable")
  opts <- w$x$tag$attribs
  expect_true(isTRUE(opts$compact))
  expect_true(isTRUE(opts$searchable))
  expect_true(isTRUE(opts$highlight))
  expect_equal(opts$defaultPageSize, 10)
  expect_true(all(c(10, 25, 50, 100) %in% unlist(opts$pageSizeOptions)))

  note <- .wise_diag_reactable(data.frame(Note = "No transfer data available."))
  expect_s3_class(note, "reactable")
})

test_that("threshold reactable keeps raw values and wraps text columns", {
  df <- data.frame(
    `Scenario / Period` = "Historical",
    Source = "Baseline",
    Estimate = "Central (P50)",
    Expected = 1.23456,
    check.names = FALSE
  )
  w <- .wise_threshold_reactable(df)
  expect_s3_class(w, "reactable")
  cols <- w$x$tag$attribs$columns
  fmt <- cols[[which(vapply(cols, function(cl)
    identical(cl$id, "Expected"), logical(1L)))]]$format
  expect_true(!is.null(fmt))
})

test_that("mod_3_08 diagnostics tables render as reactable widgets", {
  snapshot <- reactiveVal(list(
    status = NULL,
    manipulated_vars = "electricity",
    baseline_values = list(electricity = c(0L, 1L)),
    policy_values = list(electricity = c(1L, 1L)),
    transfer_sum = 0,
    transfer_pp = 0,
    changed_counts = c(electricity = 1L),
    input_summary = data.frame(
      variable = "electricity", mean_baseline = 0.5, mean_policy = 1,
      delta_mean = 0.5, sd_baseline = sqrt(0.5), sd_policy = 0
    ),
    analysis_unit = "hh",
    treatment_matrix = data.frame(
      status = "Eligible and treated", n = 1L,
      weighted_n = 2, weighted_share = 1
    ),
    component_matrix = data.frame(
      component = "infra_elec", n_affected = 1L, weighted_affected = 2,
      population_share = 1, realized_cost = 10
    )
  ))
  run_id <- reactiveVal(1L)

  testServer(
    mod_3_08_diagnostics_server,
    args = list(
      id = "diag", baseline_svy = reactive(stop("unexpected frame read")),
      policy_svy = reactive(stop("unexpected frame read")),
      diagnostic_summary = snapshot, sim_run_id = run_id,
      tabset_id = "tabs"
    ),
    {
      session$flushReact()
      # renderReactable emits the widget payload as JSON in testServer.
      expect_s3_class(output$diag_summary_table, "json")
      expect_s3_class(output$transfer_summary_ui, "json")
      expect_s3_class(output$treatment_table, "json")
      expect_s3_class(output$policy_component_table, "json")
    }
  )
})

# ---------------------------------------------------------------------------
# mod_3_09 module renders
# ---------------------------------------------------------------------------

test_that("mod_3_09 headline chart and table render echarts/reactable widgets", {
  # Same fixture shape as the W3-B characterization tests (self-contained
  # here so this file runs standalone).
  base <- data.frame(
    id = 1:4,
    weight = c(1, 2, 1, 2),
    delta_sp = c(0.05, 0.10, 0.15, 0.20),
    delta_main_covar = c(0.05, 0.10, 0.15, 0.20),
    delta_main = c(0.10, 0.20, 0.30, 0.40),
    delta_res1 = c(0.01, 0.02, 0.03, 0.04),
    delta_res2 = c(0.02, 0.01, 0.02, 0.01),
    sd_main = c(0.01, 0.02, 0.03, 0.04),
    sd_res1 = c(0.02, 0.01, 0.02, 0.01),
    sd_res2 = c(0.01, 0.02, 0.01, 0.02),
    sd_total = c(0.02, 0.03, 0.04, 0.05),
    stringsAsFactors = FALSE
  ) |>
    transform(delta_total = delta_main + delta_res1 + delta_res2)
  survey <- data.frame(welfare = c(1, 2, 3, 4), weight = c(1, 2, 1, 2))
  so <- list(name = "welfare", type = "numeric", transform = "log")

  testServer(
    mod_3_09_decomposition_server,
    args = list(
      id = "decomposition",
      decomp_result = reactiveVal(base),
      decomp_scenarios = reactiveVal(list()),
      model_fit = reactiveVal(list(engine = "fixest", rif_grid = data.frame())),
      so = reactiveVal(so),
      baseline_svy = reactiveVal(survey),
      policy_svy = reactiveVal(survey)
    ),
    {
      session$flushReact()
      # Zero-arg closures shared by the render and the export bundle.
      expect_s3_class(headline_decomp_chart(), "echarts4r")
      expect_s3_class(decomp_bar_chart(), "echarts4r")
      expect_s3_class(output$headline_decomp_plot, "json")
      expect_s3_class(output$headline_decomp_table, "json")
      expect_s3_class(output$decomp_summary_table, "json")
    }
  )
})
