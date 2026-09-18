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

test_that("future channel summaries preserve weighted current output", {
  future <- .s3p1_future()
  one_year <- future[future$scenario == "Scenario A" & future$sim_year == 2030, , drop = FALSE]
  summary <- wiseapp:::decomposition_summary_data(one_year, is_rif = TRUE)

  expect_identical(
    summary$channel_id,
    c("total", "level", "cash_transfer", "covariate_shift",
      "resilience", "repositioning", "interaction")
  )
  expect_equal(
    summary$model_value,
    c(23 / 75, 4 / 15, 2 / 15, 2 / 15, 1 / 25, 2 / 75, 1 / 75),
    tolerance = 1e-10
  )
  expect_equal(summary$percent, (exp(summary$model_value) - 1) * 100)

  by_year <- lapply(split(future, list(future$scenario, future$sim_year)), function(x) {
    wiseapp:::decomposition_summary_data(x, is_rif = TRUE)
  })
  expect_length(by_year, 4L)
  expect_true(all(vapply(by_year, nrow, integer(1L)) == 7L))
  expect_true(all(vapply(by_year, function(x) all(is.finite(x$model_value)), logical(1L)))
  )
})

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

test_that("compact and legacy future summaries preserve adverse selection", {
  future <- dplyr::bind_rows(lapply(c("Scenario A", "Scenario B"), function(scenario) {
    dplyr::bind_rows(lapply(2001:2020, function(year) {
      out <- .s3p1_fixture()
      out$scenario <- scenario
      out$sim_year <- year
      out$year_start <- 2001L
      out$year_end <- 2020L
      out$delta_total <- if (scenario == "Scenario A") year - 2000 else 2021 - year
      out
    }))
  }))
  baseline_deciles <- c(2L, 5L, 7L, 10L)
  parts <- lapply(split(future, list(future$scenario, future$sim_year)), function(x) {
    .compact_future_decomposition(
      x, x$scenario[[1L]], x$sim_year[[1L]], 2001L, 2020L,
      baseline_deciles, TRUE, "rif"
    )
  })
  compact <- .bind_compact_future_decompositions(parts, "rif", TRUE)
  for (outcome in list(
    list(name = "welfare", type = "numeric"),
    list(name = "poor", type = "numeric")
  )) {
    for (basis in c("adverse_10", "adverse_20")) {
      for (scenario in c("Scenario A", "Scenario B")) {
        legacy <- select_decomp_weather_basis(
          future[future$scenario == scenario, , drop = FALSE], basis, outcome
        )
        compact_year <- .compact_future_year(compact, scenario, basis, outcome)
        expect_identical(compact_year$sim_year, unique(legacy$sim_year))
        expect_equal(
          .compact_future_summary(compact, scenario, basis, outcome, TRUE),
          decomposition_summary_data(legacy, TRUE),
          tolerance = 0
        )
      }
    }
  }
})

test_that("adverse 1-in-10 and 1-in-20 selection preserves ranking direction", {
  future <- data.frame(
    scenario = "Scenario A",
    sim_year = 2001:2020,
    delta_total = seq(0.01, 0.20, by = 0.01),
    weight = 1
  )
  welfare <- list(name = "welfare", type = "numeric")
  poor <- list(name = "poor", type = "numeric")

  welfare_10 <- wiseapp:::select_decomp_weather_basis(future, "adverse_10", welfare)
  welfare_20 <- wiseapp:::select_decomp_weather_basis(future, "adverse_20", welfare)
  poor_10 <- wiseapp:::select_decomp_weather_basis(future, "adverse_10", poor)
  poor_20 <- wiseapp:::select_decomp_weather_basis(future, "adverse_20", poor)

  expect_identical(welfare_10$sim_year, 2002L)
  expect_identical(welfare_20$sim_year, 2001L)
  expect_identical(poor_10$sim_year, 2019L)
  expect_identical(poor_20$sim_year, 2020L)
})

test_that("compact future summaries match the full future frame for OLS and RIF", {
  future <- .s3p1_future()
  baseline_deciles <- c(2L, 5L, 7L, 10L)
  so <- list(name = "welfare", type = "numeric")
  for (engine in c("fixest", "rif")) {
    is_rif <- identical(engine, "rif")
    full <- .compact_future_summary(
      .bind_compact_future_decompositions(lapply(split(future, list(
        future$scenario, future$sim_year
      )), function(x) .compact_future_decomposition(
        x, x$scenario[[1L]], x$sim_year[[1L]], 2030L, 2040L,
        baseline_deciles, is_rif, engine
      )), engine, is_rif),
      "Scenario A", "mean", so, is_rif
    )
    raw <- wiseapp:::decomposition_summary_data(
      future[future$scenario == "Scenario A" & future$sim_year == 2030, , drop = FALSE],
      is_rif = is_rif
    )
    expect_equal(full, raw, tolerance = 0)

    compact <- .bind_compact_future_decompositions(lapply(split(future, list(
      future$scenario, future$sim_year
    )), function(x) .compact_future_decomposition(
      x, x$scenario[[1L]], x$sim_year[[1L]], 2030L, 2040L,
      baseline_deciles, is_rif, engine
    )), engine, is_rif)
    compact_decile <- .compact_future_decile_summary(
      compact, "Scenario A", "mean", so, is_rif
    )
    raw_decile <- wiseapp:::decomposition_channels_by_decile(
      future[future$scenario == "Scenario A", , drop = FALSE],
      data.frame(welfare = 1:4, weight = c(1, 2, 1, 2)),
      "welfare", is_rif, baseline_deciles
    )
    expect_equal(compact_decile, raw_decile, tolerance = 0)
  }
})

test_that("decomposition export contracts expose current future and historical products", {
  base <- .s3p1_fixture()
  future <- .s3p1_future()
  compact <- .bind_compact_future_decompositions(lapply(split(
    future, list(future$scenario, future$sim_year)
  ), function(x) .compact_future_decomposition(
    x, x$scenario[[1L]], x$sim_year[[1L]], 2030L, 2040L,
        c(2L, 5L, 7L, 10L), TRUE, "rif"
  )), "rif", TRUE)
  model <- list(engine = "rif", rif_grid = data.frame())
  survey <- data.frame(welfare = c(1, 2, 3, 4), weight = c(1, 2, 1, 2))
  so <- list(name = "welfare", type = "numeric", transform = "log")

  check_module <- function(scenarios) {
    captured <- new.env(parent = emptyenv())
    shiny::testServer(
      wiseapp:::mod_3_09_decomposition_server,
      args = list(
        id = "decomposition",
        decomp_result = shiny::reactiveVal(base),
        decomp_scenarios = shiny::reactiveVal(scenarios),
        model_fit = shiny::reactiveVal(model),
        so = shiny::reactiveVal(so),
        baseline_svy = shiny::reactiveVal(survey),
        policy_svy = shiny::reactiveVal(survey)
      ),
      {
      session$flushReact()
      session$setInputs(decile_scenario = "Scenario A", decile_weather_basis = "mean")
      session$flushReact()
      items <- wiseapp:::wise_export_items(session)
      keys <- c(
        "policy_decomposition_headline",
        "policy_decomposition_headline_data",
        "policy_decomposition_channels",
        "policy_decomposition_channels_by_decile",
        "policy_decomposition_channels_selected",
        "policy_decomposition_summary"
      )
      expect_true(all(keys %in% names(items)))

      headline <- items[["policy_decomposition_headline_data"]]$fun()
      expect_identical(
        names(headline),
        c("Scenario", "Effect component", "Mean effect (%)", "Share of total (%)")
      )
      expect_true(all(c("Historical", "Scenario A", "Scenario B") %in% headline$Scenario))

      decile <- items[["policy_decomposition_channels_by_decile"]]$fun()
      expect_true(all(c("Baseline welfare decile", "Total policy effect (%)",
                        "Sample units", "Population represented") %in% names(decile)))
      expect_true(all(c("Mean weather (%)", "Adverse 1-in-5 (%)",
                        "Adverse 1-in-10 (%)", "Adverse 1-in-20 (%)") %in%
                        names(items[["policy_decomposition_summary"]]$fun())))

      # Batch 2: on-screen decomposition figures are echarts4r widgets (their
      # registry funs switched); export-only figures stay ggplot. The
      # legacy-vs-compact characterization compares widget opts below.
      expect_s3_class(items[["policy_decomposition_headline"]]$fun(), "echarts4r")
      expect_s3_class(items[["policy_decomposition_channels"]]$fun(), "ggplot")
      expect_s3_class(items[["policy_decomposition_channels_selected"]]$fun(), "echarts4r")
      captured$headline_data <- items[["policy_decomposition_headline_data"]]$fun()
      captured$headline_plot <- items[["policy_decomposition_headline"]]$fun()
      captured$selected_plot <- items[["policy_decomposition_channels_selected"]]$fun()
      }
    )
    captured
  }

  legacy_exports <- check_module(future)
  compact_exports <- check_module(compact)
  expect_equal(legacy_exports$headline_data, compact_exports$headline_data, tolerance = 0)
  # The figure contracts compare the echarts option trees: identical data prep
  # must produce identical series/axis/tooltip opts (formatters are plain JS
  # strings, so the comparison is exact).
  expect_equal(
    legacy_exports$headline_plot$x$opts,
    compact_exports$headline_plot$x$opts,
    tolerance = 0
  )
  expect_equal(
    legacy_exports$selected_plot$x$opts,
    compact_exports$selected_plot$x$opts,
    tolerance = 0
  )
})
