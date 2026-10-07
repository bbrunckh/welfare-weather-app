library(testthat)

.s3p1_fixture <- function() {
  data.frame(
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
}

.s3p1_future <- function() {
  base <- .s3p1_fixture()
  dplyr::bind_rows(lapply(c("Scenario A", "Scenario B"), function(scenario) {
    dplyr::bind_rows(lapply(2030:2031, function(year) {
      out <- base
      out$scenario <- scenario
      out$sim_year <- year
      out$year_start <- 2030L
      out$year_end <- 2040L
      out
    }))
  }))
}

test_that("future channel-by-decile summaries use fixed baseline deciles", {
  future <- .s3p1_future()
  one_year <- future[future$scenario == "Scenario A" & future$sim_year == 2030, , drop = FALSE]
  survey <- data.frame(
    welfare = c(1, 2, 3, 4),
    weight = c(1, 2, 1, 2)
  )
  fixed_deciles <- c(2L, 5L, 7L, 10L)
  summary <- wiseapp:::decomposition_channels_by_decile(
    one_year, survey, "welfare", is_rif = TRUE,
    baseline_deciles = fixed_deciles
  )

  expect_identical(summary$decile, c(2L, 5L, 7L, 10L))
  expect_equal(summary$level_log, c(0.1, 0.2, 0.3, 0.4), tolerance = 1e-12)
  expect_equal(summary$resilience_log, c(0.03, 0.03, 0.05, 0.05), tolerance = 1e-12)
  expect_equal(summary$total_log, c(0.13, 0.23, 0.35, 0.45), tolerance = 1e-12)
  expect_equal(summary$weighted_population, c(1, 2, 1, 2))
  expect_equal(summary$n_households, c(1L, 1L, 1L, 1L))
})

# Compact per-year channel rows: one row per scenario-model-year, with the
# baseline level the adverse-year rule ranks on.
.adverse_compact <- function(models, years = 2001:2020, scenario = "Scenario A") {
  channels <- c("delta_total", "delta_main", "delta_sp", "delta_main_covar",
    "delta_res", "delta_res1", "delta_res2", "baseline_level",
    "lvl_main", "lvl_res1", "lvl_res2", "lvl_total")
  rows <- dplyr::bind_rows(lapply(names(models), function(m) {
    out <- data.frame(scenario = scenario, member = m, sim_year = years,
      year_start = min(years), year_end = max(years))
    for (ch in channels) {
      out[[paste0("sum_", ch)]] <- switch(ch,
        baseline_level = models[[m]](years),
        delta_total = , lvl_total = models[[m]](years) * 10, 0)
      out[[paste0("weight_", ch)]] <- 1
    }
    out$decile <- 1L
    out$n_households <- 1L
    out$weighted_population <- 1
    out
  }))
  structure(list(channel_summary = rows, decile_summary = rows, engine = "fixest",
    is_rif = FALSE, scenario_order = scenario),
    class = c("wise_compact_decomp_scenarios", "list"))
}

test_that("adverse weather year follows the Step 2 rank-interpolation rule", {
  welfare <- list(name = "welfare", type = "numeric")
  poor <- list(name = "poor", type = "logical", direction = "lower_is_better")
  compact <- .adverse_compact(list(a = function(y) y - 2000))
  # Higher welfare is better: the low tail is adverse, ranks 1 and 2 at p = 0.05.
  res <- .compact_future_decomp(compact, "Scenario A", "adverse_20", welfare)
  expect_equal(res$baseline_annual, 1.5)
  expect_equal(res$delta_total, 15)
  expect_equal(res$lvl_total, 15)
  # p = 0.10 -> k = 2.5; p = 0.20 -> k = 4.5.
  expect_equal(.compact_future_decomp(compact, "Scenario A", "adverse_10", welfare)$baseline_annual, 2.5)
  expect_equal(.compact_future_decomp(compact, "Scenario A", "adverse_5", welfare)$baseline_annual, 4.5)
  # Lower is better (poverty): the high tail is adverse.
  expect_equal(.compact_future_decomp(compact, "Scenario A", "adverse_20", poor)$baseline_annual, 19.5)
  expect_equal(.compact_future_decomp(compact, "Scenario A", "adverse_5", poor)$baseline_annual, 16.5)
})

test_that("adverse weather year is chosen within each model, then averaged equally", {
  welfare <- list(name = "welfare", type = "numeric")
  compact <- .adverse_compact(list(a = function(y) y - 2000, b = function(y) 2021 - y))
  res <- .compact_future_decomp(compact, "Scenario A", "adverse_20", welfare)
  # Model a: years 2001/2002 (delta 10 * baseline 1, 2); model b: years 2020/2019.
  expect_equal(res$baseline_annual, 1.5)
  expect_equal(res$delta_total, 15)
  deciles <- .compact_future_decile_summary(compact, "Scenario A", "adverse_20", welfare)
  expect_equal(deciles$total, 15)
})

test_that("adverse basis is unavailable without enough simulated years", {
  welfare <- list(name = "welfare", type = "numeric")
  compact <- .adverse_compact(list(a = function(y) y - 2000), years = 2001:2010)
  expect_equal(nrow(.compact_future_decomp(compact, "Scenario A", "adverse_20", welfare)), 0L)
  expect_equal(nrow(.compact_future_decile_summary(compact, "Scenario A", "adverse_20", welfare)), 0L)
  expect_equal(nrow(.compact_future_decomp(compact, "Scenario A", "adverse_10", welfare)), 1L)
  expect_equal(nrow(.compact_future_decomp(compact, "Scenario A", "mean", welfare)), 1L)
})

test_that("decomposition export contracts expose current future and historical products", {
  compact <- .adverse_compact(list(a = function(y) y - 2000))
  compact$engine <- "rif"
  compact$is_rif <- TRUE
  model <- list(engine = "rif", rif_grid = data.frame())
  survey <- data.frame(welfare = c(1, 2, 3, 4), weight = c(1, 2, 1, 2))
  so <- list(name = "welfare", label = "Welfare", units = "PPP",
    time_basis = "day", welfare_denominator = "person",
    type = "numeric", transform = "log")

  check_module <- function(scenarios) {
    captured <- new.env(parent = emptyenv())
    shiny::testServer(
      wiseapp:::mod_3_09_decomposition_server,
      args = list(
        id = "decomposition",
        decomp_scenarios = shiny::reactiveVal(scenarios),
        model_fit = shiny::reactiveVal(model),
        so = shiny::reactiveVal(so),
        baseline_svy = shiny::reactiveVal(survey)
      ),
      {
      session$flushReact()
      session$setInputs(headline_scenario = "Scenario A", decile_scenario = "Scenario A",
        headline_weather_basis = "mean", decile_weather_basis = "mean")
      session$flushReact()
      items <- wiseapp:::wise_export_items(session)
      keys <- c("policy_decomposition_headline", "policy_decomposition_headline_data",
        "policy_decomposition_channels_by_decile",
        "policy_decomposition_channels_by_decile_plot")
      expect_true(all(keys %in% names(items)))

      headline <- items[["policy_decomposition_headline_data"]]$fun()
      expect_identical(names(headline), c("Scenario", "Metric", "Outcome", "Change unit",
        "Effect component", "Weighted average change"))
      expect_true("Scenario A" %in% headline$Scenario)

      decile <- items[["policy_decomposition_channels_by_decile"]]$fun()
      expect_true("Baseline welfare decile" %in% names(decile))
      expect_equal(ncol(decile), 5L)

      # Batch 2: on-screen decomposition figures are echarts4r widgets (their
      # registry funs switched); export-only figures stay ggplot. The
      # legacy-vs-compact characterization compares widget opts below.
      expect_s3_class(items[["policy_decomposition_headline"]]$fun(), "echarts4r")
      expect_s3_class(items[["policy_decomposition_channels_by_decile_plot"]]$fun(), "echarts4r")
      captured$headline_data <- items[["policy_decomposition_headline_data"]]$fun()
      captured$headline_plot <- items[["policy_decomposition_headline"]]$fun()
      captured$selected_plot <- items[["policy_decomposition_channels_by_decile_plot"]]$fun()
      }
    )
    captured
  }

  compact_exports <- check_module(compact)
  expect_true(is.list(compact_exports$headline_plot$x$opts))
  expect_true(is.list(compact_exports$selected_plot$x$opts))
})
