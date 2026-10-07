# ============================================================================ #
# tests/testthat/test-mod_2_02_results.R                                       #
# Regression tests for the Step 2 results module: aggregation-cache keying     #
# (PERF-30) and the resolve_band_q contract (DUP-01).                          #
# ============================================================================ #

library(testthat)
library(shiny)

# ---- DUP-01: single authoritative resolve_band_q ----------------------------

test_that("resolve_band_q maps every UI band key to its quantile pair", {
  expect_identical(resolve_band_q("p25_p75"),   c(lo = 0.25,  hi = 0.75))
  expect_identical(resolve_band_q("p20_p80"),   c(lo = 0.20,  hi = 0.80))
  expect_identical(resolve_band_q("p10_p90"),   c(lo = 0.10,  hi = 0.90))
  expect_identical(resolve_band_q("p05_p95"),   c(lo = 0.05,  hi = 0.95))
  expect_identical(resolve_band_q("p025_p975"), c(lo = 0.025, hi = 0.975))
  expect_identical(resolve_band_q("p005_p995"), c(lo = 0.005, hi = 0.995))
  # minmax is the full observed range, not a winsorised pair (the deleted
  # fct_aggregation.R duplicate winsorised to 0.001/0.999 - DUP-01).
  expect_identical(resolve_band_q("minmax"),    c(lo = 0.00,  hi = 1.00))
})

test_that("resolve_band_q falls back to p10_p90 for unknown keys", {
  expect_identical(resolve_band_q("bogus"), c(lo = 0.10, hi = 0.90))
})

# ---- PERF-30: aggregation cache keyed only by value-affecting inputs -------

make_hist_sim_fixture <- function() {
  n <- 400
  set.seed(7)
  pl <- data.frame(
    sim_year = rep(2020:2021, each = n / 2),
    y_point  = rnorm(n, 1.2, 0.4),
    weight   = rep(c(1, 2), length.out = n)
  )
  list(
    so          = list(type = "numeric", name = "welfare", transform = "log"),
    residuals   = "none",
    has_weights = TRUE,
    pipeline    = list(
      sim_year  = pl$sim_year,
      y_point   = pl$y_point,
      weight    = pl$weight,
      # Tiny loadings keep the auto-tuned kernel bandwidth below the user
      # bandwidth, so bandwidth_p0 changes must visibly alter headcount SEs.
      F_loading = matrix(rnorm(2 * n) * 0.01, nrow = n),
      train_aug = NULL, id_vec = NULL, id_col = NULL
    )
  )
}

test_that("only Step 2 expected headlines use equal-model means and align with Step 3", {
  pipe <- function(values, years) list(
    y_point = rep(values, each = 4) + rep(c(-.03, -.01, .01, .03), length(values)),
    sim_year = rep(years, each = 4),
    F_loading = matrix(0, nrow = 4 * length(values), ncol = 1L)
  )
  so <- list(type = "numeric", name = "welfare", transform = "identity")
  hist <- list(so = so, residuals = "none", has_weights = FALSE,
               pipeline = pipe(seq_len(20), 2000:2019))
  scenario <- "SSP2-4.5 / 2030-2050"
  pipes <- list(m1 = pipe(rep(1, 20), 2030:2049),
                m2 = pipe(rep(10, 21), 2030:2050),
                m3 = pipe(rep(100, 22), 2030:2051))
  saved <- setNames(list(list(so = so, pipelines = pipes)), scenario)
  testServer(mod_2_02_results_server, args = list(
    id = "results", hist_sim = reactiveVal(hist),
    saved_scenarios = reactiveVal(saved), selected_hist = reactiveVal(NULL),
    tabset_id = "step2_output_tabs"
  ), {
    session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none")
    session$flushReact()
    old_bands <- pointrange_bands_rv()
    old_tail <- threshold_table_rv()
    old_curves <- timeseries_curves_rv()
    old_uncertainty <- variance_breakdown_rv()
    expect_equal(old_bands$value[old_bands$scenario == scenario], 10)
    headline <- headline_bands_rv()
    expect_equal(headline$value[headline$scenario == scenario], 37)
    expect_equal(headline_cards_data_rv()[[1]]$value_native[[2]], 37)
    expect_identical(pointrange_bands_rv(), old_bands)
    expect_identical(threshold_table_rv(), old_tail)
    expect_identical(timeseries_curves_rv(), old_curves)
    expect_identical(variance_breakdown_rv(), old_uncertainty)

    baseline <- scenario_agg_rv()[[scenario]]$unweighted$mean
    paired <- paired_model_year_effects(baseline, baseline)
    summary <- paired_effect_summary(paired, center = "equal_model_mean")
    expect_equal(summary$baseline, headline$value[headline$scenario == scenario])
    expect_equal(summary$policy, summary$baseline)
    expect_equal(summary$value, 0)
    before <- saved_scenarios()[[scenario]]$pipelines
    for (deviation in c("mean", "median")) {
      session$setInputs(cmp_deviation = deviation)
      session$flushReact()
      expect_equal(headline_bands_rv()$value[headline_bands_rv()$scenario == scenario],
                   37 - hist_ref_val())
      expect_identical(saved_scenarios()[[scenario]]$pipelines, before)
      expect_identical(scenario_agg_rv()[[scenario]]$unweighted$mean, baseline)
      expect_equal(paired_effect_summary(
        paired_model_year_effects(baseline, baseline), center = "equal_model_mean"
      )$baseline, 37)
    }
  })
})

test_that("Step 2 outcome metadata tolerates a missing optional level column", {
  so <- tibble::tibble(
    type = "numeric", name = "welfare", label = "Welfare", units = "PPP"
  )
  expect_no_warning({
    analysis_unit <- wiseapp:::.metric_context_value(so, "level")
    metadata <- metric_metadata("mean", so, analysis_unit = analysis_unit,
      weighted = TRUE)
  })
  expect_null(analysis_unit)
  expect_identical(metadata$analysis_unit, NULL)
})

test_that("simulation prediction counts are compact and identify their row unit", {
  expect_identical(format_prediction_count(4500000, "hh"), "4.5M household-years")
  expect_identical(format_prediction_count(1250, "ind"), "1.2K individual-years")
  expect_identical(format_prediction_count(42, NULL), "42 observation-years")
})

test_that("Results frame is immutable and scoped to method/deviation", {
  skip_if_not_installed("shiny")
  hist_sim <- shiny::reactiveVal(make_hist_sim_fixture())
  stale <- shiny::reactiveVal(FALSE)
  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id = "results", hist_sim = hist_sim,
      saved_scenarios = shiny::reactiveVal(list()),
      selected_hist = shiny::reactiveVal(NULL),
      tabset_id = "step2_output_tabs", stale = stale
    ),
    {
      session$flushReact()
      frame <- derived_results_frame_rv()
      expect_true(is.environment(frame))
      expect_false("entries" %in% ls(frame, all.names = TRUE))
      expect_identical(frame$method, "mean")
      expect_identical(frame$deviation, "none")
      entry <- .results_frame_entry(frame, "Historical")
      expect_true(is.list(entry$matrix))
      key <- entry$key
      copy <- .results_frame_matrix(frame, key)
      original <- .results_frame_matrix(frame, key)$vals[1L, 1L]
      copy$vals[1L, 1L] <- copy$vals[1L, 1L] + 100
      expect_identical(.results_frame_matrix(frame, key)$vals[1L, 1L], original)
    }
  )
})

test_that("agg cache: display-only controls do not invalidate unaffected methods", {
  skip_if_not_installed("shiny")

  hist_sim <- shiny::reactiveVal(make_hist_sim_fixture())

  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id              = "results",
      hist_sim        = hist_sim,
      saved_scenarios = shiny::reactiveVal(list()),
      selected_hist   = shiny::reactiveVal(NULL),
      tabset_id       = "step2_output_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      ws <- function() agg_workspace()

      session$flushReact()
      h1 <- .get_hist_agg("mean")
      stopifnot(length(ls(envir = ws()$cache)) == 1L)

      # Poverty-line move: mean does not read it -> entry survives untouched
      session$setInputs(pov_line = 5.50); settle()
      expect_true(isTRUE(all.equal(pov_line_val(), 5.5)))
      h2 <- .get_hist_agg("mean")
      expect_identical(h1, h2)
      expect_length(ls(envir = ws()$cache), 1L)

      # gap at first line, then at a new one: new key, recompute, old kept
      g1 <- .get_hist_agg("gap")
      expect_length(ls(envir = ws()$cache), 2L)
      session$setInputs(pov_line = 7.25); settle()
      g2 <- .get_hist_agg("gap")
      expect_false(identical(g1, g2))
      expect_length(ls(envir = ws()$cache), 3L)
      m2 <- .get_hist_agg("mean")
      expect_identical(h1, m2)
      expect_true(all(g2$unweighted$gap$value > g1$unweighted$gap$value))
      # headcount reads pl and the fixed bandwidth; gap ignores bandwidth
      session$setInputs(bandwidth_p0 = 0.10); settle()
      hc1 <- .get_hist_agg("headcount_ratio")
      expect_length(ls(envir = ws()$cache), 4L)
      hc2 <- .get_hist_agg("headcount_ratio")
      expect_identical(hc1, hc2)
      g3 <- .get_hist_agg("gap")
      expect_identical(g2, g3)
      m3 <- .get_hist_agg("mean")
      expect_identical(h1, m3)
      expect_length(ls(envir = ws()$cache), 4L)

      # bandwidth_p0 has no UI control (CR-BUG-11): a stray input value is
      # ignored, so headcount stays cached at the fixed default bandwidth.
      session$setInputs(bandwidth_p0 = 0.20); settle()
      hc3 <- .get_hist_agg("headcount_ratio")
      expect_identical(hc2, hc3)
      expect_length(ls(envir = ws()$cache), 4L)
      g4 <- .get_hist_agg("gap")
      expect_identical(g2, g4)
      m4 <- .get_hist_agg("mean")
      expect_identical(h1, m4)
    }
  )
})

test_that("agg cache is bounded, observable, and recomputes evicted values", {
  skip_if_not_installed("shiny")

  hist_sim <- shiny::reactiveVal(make_hist_sim_fixture())

  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id              = "results",
      hist_sim        = hist_sim,
      saved_scenarios = shiny::reactiveVal(list()),
      selected_hist   = shiny::reactiveVal(NULL),
      tabset_id       = "step2_output_tabs"
    ),
    {
      session$flushReact()
      mean_before <- .get_hist_agg("mean")
      methods <- unname(hist_aggregate_choices("numeric", "welfare"))
      for (method in methods) .get_hist_agg(method)

      state <- aggregation_cache()
      expect_equal(state$max_entries, 8L)
      expect_lte(state$n_entries, state$max_entries)
      expect_gt(length(state$evictions), 0L)
      expect_equal(state$n_entries, length(state$keys))
      expect_gt(state$object_bytes, 0)
      expect_gt(state$serialized_bytes, 0)

      # The oldest entry is evicted, but recomputation remains byte-identical.
      mean_after <- .get_hist_agg("mean")
      expect_identical(mean_after$weighted$mean, mean_before$weighted$mean)
      expect_lte(aggregation_cache()$n_entries, 8L)

      # Export builders resolve the active method again after eviction rather
      # than depending on an unbounded retained result entry.
      exported <- threshold_table_df()
      expect_s3_class(exported, "data.frame")
      expect_gt(nrow(exported), 0L)
    }
  )
})

test_that("agg cache clears with the published Results lifecycle", {
  skip_if_not_installed("shiny")

  hist_sim <- shiny::reactiveVal(make_hist_sim_fixture())

  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id              = "results",
      hist_sim        = hist_sim,
      saved_scenarios = shiny::reactiveVal(list()),
      selected_hist   = shiny::reactiveVal(NULL),
      tabset_id       = "step2_output_tabs"
    ),
    {
      session$flushReact()
      .get_hist_agg("mean")
      expect_gt(aggregation_cache()$n_entries, 0L)

      hist_sim(NULL)
      session$flushReact()
      state <- aggregation_cache()
      expect_equal(state$n_entries, 0L)
      expect_length(state$keys, 0L)
      expect_equal(state$object_bytes, 0)
      expect_equal(state$serialized_bytes, 0)
    }
  )
})

test_that("agg cache preserves the active result while reruns become stale", {
  skip_if_not_installed("shiny")

  first <- make_hist_sim_fixture()
  second <- make_hist_sim_fixture()
  second$pipeline$y_point <- second$pipeline$y_point + 0.25
  hist_sim <- shiny::reactiveVal(first)
  stale <- shiny::reactiveVal(FALSE)

  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id              = "results",
      hist_sim        = hist_sim,
      saved_scenarios = shiny::reactiveVal(list()),
      selected_hist   = shiny::reactiveVal(NULL),
      tabset_id       = "step2_output_tabs",
      stale           = stale
    ),
    {
      session$flushReact()
      old <- .get_hist_agg("mean")
      stale(TRUE)
      session$flushReact()

      # Staleness is presentation state; it must not discard the last valid
      # result or make its cache entry disappear.
      expect_true(stale())
      expect_identical(.get_hist_agg("mean"), old)
      expect_gt(aggregation_cache()$n_entries, 0L)

      # A completed rerun publishes a new workspace and releases the old one.
      hist_sim(second)
      stale(FALSE)
      session$flushReact()
      new <- .get_hist_agg("mean")
      expect_false(identical(new$weighted$mean, old$weighted$mean))
      expect_equal(aggregation_cache()$n_entries, 1L)
      expect_equal(aggregation_cache()$evictions, character(0))
    }
  )
})

test_that("agg cache records hits, misses, and value-affecting key changes", {
  skip_if_not_installed("shiny")

  hist_sim <- shiny::reactiveVal(make_hist_sim_fixture())

  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id              = "results",
      hist_sim        = hist_sim,
      saved_scenarios = shiny::reactiveVal(list()),
      selected_hist   = shiny::reactiveVal(NULL),
      tabset_id       = "step2_output_tabs"
    ),
    {
      session$flushReact()
      baseline <- aggregation_cache()
      .get_hist_agg("gap")
      first <- aggregation_cache()
      expect_equal(first$misses - baseline$misses, 1L)

      .get_hist_agg("gap")
      second <- aggregation_cache()
      expect_equal(second$hits, 1L)
      expect_equal(second$n_entries, first$n_entries)

      session$setInputs(pov_line = 7.25)
      session$elapse(500)
      session$flushReact()
      .get_hist_agg("gap")
      third <- aggregation_cache()
      expect_equal(third$misses - second$misses, 1L)
      expect_equal(third$n_entries, second$n_entries + 1L)
      expect_length(third$entry_object_bytes, third$n_entries)
    }
  )
})

test_that("session end releases aggregation cache entries", {
  skip_if_not_installed("shiny")

  hist_sim <- shiny::reactiveVal(make_hist_sim_fixture())
  cache_state <- NULL

  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id              = "results",
      hist_sim        = hist_sim,
      saved_scenarios = shiny::reactiveVal(list()),
      selected_hist   = shiny::reactiveVal(NULL),
      tabset_id       = "step2_output_tabs"
    ),
    {
      session$flushReact()
      .get_hist_agg("mean")
      cache_state <<- agg_workspace()
      expect_length(ls(cache_state$cache, all.names = TRUE), 1L)
      session$close()
    }
  )

  expect_length(ls(cache_state$cache, all.names = TRUE), 0L)
  expect_length(cache_state$cache_order$keys, 0L)
})

# ---- INT-08: stale banner on the Step 2 results pane ------------------------

test_that("Step 2 results pane shows the stale banner while stale", {
  skip_if_not_installed("shiny")

  hist_sim <- shiny::reactiveVal(make_hist_sim_fixture())
  stale    <- shiny::reactiveVal(FALSE)

  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id              = "results",
      hist_sim        = hist_sim,
      saved_scenarios = shiny::reactiveVal(list()),
      selected_hist   = shiny::reactiveVal(NULL),
      tabset_id       = "step2_output_tabs",
      stale           = stale
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }

      stale(TRUE); settle()
      html <- paste(as.character(session$output$stale_banner), collapse = " ")
      expect_match(html, "Results are out of date", fixed = TRUE)
      expect_match(html, "Step 2 simulation results", fixed = TRUE)

      stale(FALSE); settle()
      html <- paste(as.character(session$output$stale_banner), collapse = " ")
      expect_identical(nchar(html), 0L)
    }
  )
})

# ---- INT-07: results tab follows the hist_sim lifecycle ---------------------

test_that("results tab is appended, removed on clear, re-appended on rerun", {
  skip_if_not_installed("shiny")

  hist_sim <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id              = "results",
      hist_sim        = hist_sim,
      saved_scenarios = shiny::reactiveVal(list()),
      selected_hist   = shiny::reactiveVal(NULL),
      tabset_id       = "step2_output_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }

      session$flushReact()
      expect_false(results_tab_added())

      hist_sim(make_hist_sim_fixture()); settle()
      expect_true(results_tab_added())

      # Clearing the run removes the tab so the empty state returns (INT-07)
      hist_sim(NULL); settle()
      expect_false(results_tab_added())

      # ...and a later run re-inserts it (never fired again under once = TRUE)
      hist_sim(make_hist_sim_fixture()); settle()
      expect_true(results_tab_added())
    }
  )
})

# ---- Scenario coverage ------------------------------------------------------

test_that("all saved scenarios feed results when no scenario filter is shown", {
  skip_if_not_installed("shiny")

  hist_sim <- shiny::reactiveVal(make_hist_sim_fixture())
  saved    <- shiny::reactiveVal(list(
    "SSP2-4.5 / 2030" = list(scenario_name = "SSP2-4.5 / 2030"),
    "SSP5-8.5 / 2030" = list(scenario_name = "SSP5-8.5 / 2030")
  ))

  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id              = "results",
      hist_sim        = hist_sim,
      saved_scenarios = saved,
      selected_hist   = shiny::reactiveVal(NULL),
      tabset_id       = "step2_output_tabs"
    ),
    {
      settle <- function() { session$elapse(500); session$flushReact() }
      settle()
      expect_setequal(
        selected_scenario_names(),
        c("SSP2-4.5 / 2030", "SSP5-8.5 / 2030")
      )
    }
  )
})

test_that("all Module 2 summaries use the same complete scenario set", {
  skip_if_not_installed("shiny")

  hist <- make_hist_sim_fixture()
  shifted_pipeline <- function(shift) {
    pipe <- hist$pipeline
    pipe$y_point <- pipe$y_point + shift
    pipe
  }
  scenario_entry <- function(shift) {
    list(
      so = hist$so,
      pipelines = list(model_1 = shifted_pipeline(shift)),
      n_models = 1L
    )
  }
  scenario_names <- c("SSP2-4.5 / 2030", "SSP5-8.5 / 2050")
  saved <- stats::setNames(
    list(scenario_entry(0.10), scenario_entry(0.30)),
    scenario_names
  )

  shiny::testServer(
    mod_2_02_results_server,
    args = list(
      id              = "results",
      hist_sim        = shiny::reactiveVal(hist),
      saved_scenarios = shiny::reactiveVal(saved),
      selected_hist   = shiny::reactiveVal(NULL),
      tabset_id       = "step2_output_tabs"
    ),
    {
      session$setInputs(
        cmp_agg_method = "mean",
        cmp_deviation = "none",
        ensemble_band = "minmax",
        uncertainty_band = "p10_p90"
      )
      session$flushReact()

      expected <- c("Historical", scenario_names)
      annual <- annual_distribution_curves_rv()
      bands <- pointrange_bands_rv()
      thresholds <- threshold_table_rv()
      exceedance <- exceedance_curves_rv()

      expect_setequal(unique(annual$scenario), expected)
      expect_setequal(unique(bands$scenario), expected)
      expect_setequal(unique(thresholds$scenario), expected)
      expect_setequal(unique(exceedance$scenario), expected)
      expect_equal(dplyr::n_distinct(round(bands$value, 8)), 3L)

      central <- thresholds[thresholds$Estimate == "Equal-model mean" &
                              thresholds$rp_name == "1:1", , drop = FALSE]
      expect_equal(dplyr::n_distinct(round(central$value, 8)), 2L)
    }
  )
})

test_that("formatted threshold table preserves output across repeated builds", {
  skip_if_not_installed("shiny")
  testServer(
    mod_2_02_results_server,
    args = list(
      id = "results", hist_sim = reactiveVal(make_hist_sim_fixture()),
      saved_scenarios = reactiveVal(list()), selected_hist = reactiveVal(NULL),
      tabset_id = "step2_output_tabs"
    ),
    {
      session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none",
                        cmp_group_order = "scenario_x_year")
      session$flushReact()
      first <- threshold_table_df()
      second <- threshold_table_df()
      expect_identical(second, first)
      expect_s3_class(first, "data.frame")
      expect_gt(nrow(first), 0L)
    }
  )
})

# ---- Step 2 Headline Cards -------------------------------------------------

test_that("step2_headline_cards returns 4 cards plus a basis card with mod_1 styling", {
  bands <- tibble::tibble(
    scenario      = c("Historical", "SSP3-7.0 / 2025-2035"),
    value         = c(4.50, 4.52),
    coef_lo       = c(4.45, 4.47),
    coef_hi       = c(4.55, 4.57),
    interann_lo   = c(4.20, 4.25),
    interann_hi   = c(4.80, 4.85),
    intermod_lo   = c(4.50, 4.48),
    intermod_hi   = c(4.50, 4.56),
    total_lo      = c(NA_real_, 4.40),
    total_hi      = c(NA_real_, 4.64),
    is_historical = c(TRUE, FALSE),
    n_models      = c(1L, 22L)
  )

  thresh_tbl <- tibble::tibble(
    scenario = c(rep("Historical", 4L), rep("SSP3-7.0 / 2025-2035", 4L)),
    Estimate = rep("Central (P50)", 8L),
    rp_name  = rep(c("1:1", "1:5", "1:10", "1:20"), 2L),
    value    = c(4.50, 4.30, 4.20, 4.05, 4.52, 4.38, 4.25, 4.10)
  )

  hist_sim <- list(
    so = list(type = "numeric", name = "welfare", label = "Consumption", units = "$/day"),
    svy = data.frame(welfare = rep(1, 100)),
    sim_summary = list(
      total_runs = 690L,
      historical_years = c(1991L, 2020L)
    )
  )

  saved <- list("SSP3-7.0 / 2025-2035" = list(n_models = 22L))
  timeseries <- tibble::tibble(
    scenario = rep("SSP3-7.0 / 2025-2035", 6L),
    model_id = rep(c("m1", "m2"), each = 3L),
    sim_year = rep(2025:2027, 2L),
    value = c(4.20, 4.70, 4.45, 4.50, 4.90, 4.55),
    is_historical = FALSE
  )

  cards <- step2_headline_cards(
    bands            = bands,
    threshold_tbl    = thresh_tbl,
    hist_sim         = hist_sim,
    saved_scenarios  = saved,
    method           = "mean",
    timeseries_curves = timeseries
  )

  expect_length(cards, 5L)

  # Check labels
  labels <- vapply(cards, function(c) c$label, character(1L))
  expect_identical(
    labels,
    c("Expected change", "Adverse weather years", "Signal vs noise",
      "Year-to-year range", "Simulation years")
  )

  # Every card has non-empty fields
  for (card in cards) {
    expect_true(nzchar(card$label))
    expect_true(nzchar(card$value))
    expect_true(nzchar(card$note))
    expect_s3_class(card$note_html, "shiny.tag.list")
    expect_true(nzchar(card$info))
  }

  # Card 1: Expected change (delta headline, levels in the context line)
  expect_identical(cards[[1]]$value, "+0.02 $/day")
  expect_match(cards[[1]]$note, "4.50 \u2192 4.52", fixed = TRUE)
  expect_match(cards[[1]]$note, "Historical \u2192 SSP", fixed = TRUE)
  expect_match(cards[[1]]$note, "average year", fixed = TRUE)
  expect_false(grepl("Outcome level", cards[[1]]$note, fixed = TRUE))
  expect_match(cards[[1]]$info, "mean across weather years and climate models", fixed = TRUE)
  expect_match(cards[[1]]$info, "adaptation is not included", fixed = TRUE)
  expect_match(cards[[1]]$basis_text, "adaptation not included", fixed = TRUE)
  expect_identical(cards[[1]]$status$kind, "favourable")

  median_cards <- step2_headline_cards(
    bands            = bands,
    threshold_tbl    = thresh_tbl,
    hist_sim         = hist_sim,
    saved_scenarios  = saved,
    method           = "median",
    timeseries_curves = timeseries
  )
  expect_match(median_cards[[1]]$note, "average year", fixed = TRUE)

  # Card 2: Adverse weather years (1-in-20 year), change headline
  expect_identical(cards[[2]]$value, "+0.05 $/day")
  expect_match(cards[[2]]$note, "4.05 \u2192 4.10", fixed = TRUE)
  expect_match(cards[[2]]$note, "1-in-20 year", fixed = TRUE)
  # Six simulated years cannot resolve a 1-in-20 frequency.
  expect_false(grepl("now ", cards[[2]]$note, fixed = TRUE))

  # Card 3: Signal vs noise (m1 -0.05, m2 +0.15 against the +0.02 ensemble change)
  expect_identical(cards[[3]]$value, "1 of 2 models")
  expect_match(cards[[3]]$note, "agree on direction", fixed = TRUE)
  expect_identical(cards[[3]]$status$kind, "uncertain")
  expect_identical(cards[[3]]$status$text, "Models disagree")
  expect_match(cards[[3]]$info, "Range of model averages: 4.45 to 4.65", fixed = TRUE)
  expect_match(cards[[3]]$info, "coefficient", fixed = TRUE)

  # Card 4: Year-to-year range (people card does not apply to a mean)
  expect_identical(cards[[4]]$value, "4.35 to 4.80")
  expect_match(cards[[4]]$note, "Hist: 4.20 to 4.80", fixed = TRUE)
  expect_match(cards[[4]]$note, "SSP3-7.0 / 2025-2035", fixed = TRUE)

  # Card 5: Simulation years (basis strip, not a card)
  expect_true(cards[[5]]$basis_only)
  expect_identical(cards[[5]]$value, "6")
  expect_identical(cards[[5]]$prediction_count_note, "600 observation-years")
  expect_identical(cards[[5]]$prediction_count_native, 600)
  expect_match(cards[[5]]$note, "600 observation-years", fixed = TRUE)
  expect_match(cards[[5]]$basis_text, "600 observation-years", fixed = TRUE)
  expect_match(cards[[5]]$note, "(1 scenario \u00d7 2 models) \u00d7 3 yrs", fixed = TRUE)
  expect_identical(cards[[5]]$class, "neutral")

  # Table conversion
  df <- step2_headline_df(cards)
  expect_equal(nrow(df), 5L)
  expect_identical(names(df)[1:3], c("Metric", "Value", "Note"))
  expect_identical(df$Metric, labels)
  expect_equal(df$Historical_native[[1]], 4.5)
  expect_equal(df$Focus_native[[1]], 4.52)
  expect_identical(df$Summary_method[[1]], "equal_model_mean")
  expect_match(df$Note[[5]], "600 observation-years", fixed = TRUE)
  expect_identical(df$Prediction_count_native[[5]], 600)
  expect_identical(df$Prediction_sample_rows[[5]], 100)

  total_cards <- step2_headline_cards(
    bands = bands, threshold_tbl = thresh_tbl, hist_sim = hist_sim,
    saved_scenarios = saved, method = "total", timeseries_curves = timeseries,
    metadata = metric_metadata("total", hist_sim$so, weighted = TRUE)
  )
  expect_match(total_cards[[1]]$value, "^\\+0")
})

test_that("headline levels and deviation changes use metric-native display units", {
  bands <- tibble::tibble(
    scenario = c("Historical", "SSP2-4.5 / 2030"),
    value = c(0.32, 0.28),
    interann_lo = c(0.25, 0.22), interann_hi = c(0.40, 0.35),
    n_models = c(1L, 2L), is_historical = c(TRUE, FALSE)
  )
  so <- list(type = "numeric", name = "welfare", label = "Consumption", units = "PPP")
  hist_sim <- list(so = so)
  timeseries <- tibble::tibble(
    scenario = "SSP2-4.5 / 2030", model_id = c("m1", "m1"), sim_year = 2030:2031,
    value = c(0.22, 0.35)
  )
  metadata <- metric_metadata("headcount_ratio", so, pov_line = 0.3, weighted = TRUE)
  cards <- step2_headline_cards(
    bands, hist_sim = hist_sim, method = "headcount_ratio", timeseries_curves = timeseries,
    deviation = "mean", metadata = metadata
  )
  expect_identical(cards[[1]]$value, "-4.0 pp")
  expect_match(cards[[1]]$note, "Difference from historical mean", fixed = TRUE)
  expect_match(cards[[1]]$info, "Poverty line 0.3", fixed = TRUE)
  expect_match(cards[[4]]$value, "+22.0 pp to +35.0 pp", fixed = TRUE)
  df <- step2_headline_df(cards)
  expect_equal(df$Historical_native[[1]], 0.32)
  expect_equal(df$Focus_native[[1]], 0.28)
  expect_equal(df$Range_lower_native[[4]], 0.22)
  expect_equal(df$Range_upper_native[[4]], 0.35)
  expect_identical(df$Native_unit[[1]], "fraction")
  expect_identical(df$Threshold_value[[1]], 0.3)
  expect_identical(df$Deviation[[1]], "mean")
  expect_identical(df$Display_unit[[1]], "pp")
  expect_identical(df$Summary_method[[2]], "median_across_climate_models")

  change <- format_metric_value(0.28 - 0.32, metadata, change = TRUE)
  expect_identical(change, "-4.00 pp")
})

test_that("prosperity gap uses fixed threshold context without editable poverty line", {
  so <- list(
    name = "welfare", type = "numeric", label = "Consumption", units = "PPP",
    povline = 3
  )
  ui <- wiseapp:::.results_content_ui(shiny::NS("results"), so)
  html <- as.character(htmltools::renderTags(ui)$html)
  expect_match(html, "fixed threshold of 28", fixed = TRUE)
  expect_match(html, "prosperity_gap", fixed = TRUE)

  metadata <- metric_metadata("prosperity_gap", so, pov_line = 3, weighted = TRUE)
  expect_false(metadata$uses_poverty_line)
  expect_identical(metadata$threshold_value, 28)
  expect_match(metric_context_note(metadata), "Fixed prosperity threshold 28", fixed = TRUE)

  # The selected prosperity aggregation key has no editable threshold suffix.
  expect_false(metadata$uses_poverty_line)
})

test_that("step2_headline_cards handles historical-only simulation gracefully", {
  bands <- tibble::tibble(
    scenario      = "Historical",
    value         = 4.50,
    coef_lo       = 4.45,
    coef_hi       = 4.55,
    interann_lo   = 4.20,
    interann_hi   = 4.80,
    intermod_lo   = 4.50,
    intermod_hi   = 4.50,
    total_lo      = NA_real_,
    total_hi      = NA_real_,
    is_historical = TRUE,
    n_models      = 1L
  )

  hist_sim <- list(
    so = list(type = "numeric", name = "welfare", label = "Consumption"),
    sim_summary = list(
      total_runs = 30L,
      historical_years = c(1991L, 2020L)
    )
  )

  cards <- step2_headline_cards(
    bands           = bands,
    threshold_tbl   = NULL,
    hist_sim        = hist_sim,
    saved_scenarios = list()
  )

  expect_length(cards, 5L)
  expect_match(cards[[1]]$value, "4.50", fixed = TRUE)
  expect_identical(cards[[3]]$value, "Not applicable")
  expect_match(cards[[5]]$note, "1 historical \u00d7 30 yrs", fixed = TRUE)
})

test_that("results content UI produces clear aggregation panel with question and pill selector", {
  so <- list(
    name    = "welfare",
    type    = "numeric",
    label   = "Consumption",
    level   = "hh",
    units   = "$/day, 2021 PPP",
    povline = 3.00
  )
  ui <- wiseapp:::.results_content_ui(shiny::NS("results"), so)
  html <- as.character(htmltools::renderTags(ui)$html)

  expect_match(html, "results-aggregation-panel", fixed = TRUE)
  expect_match(html, "How to summarise consumption across households?", fixed = TRUE)
  expect_match(html, "results-cmp_agg_method", fixed = TRUE)
  expect_match(html, "results-pov_line", fixed = TRUE)
  expect_match(html, "Poverty line ($/day, 2021 PPP):", fixed = TRUE)
  expect_match(html, "toggle-slider pill-toggle", fixed = TRUE)

  # Shared outcome/deviation controls belong in the aggregation panel.
  agg_panel_html <- as.character(htmltools::renderTags(ui[[3]])$html)
  expect_match(agg_panel_html, "results-aggregation-panel", fixed = TRUE)
  expect_false(grepl("results-controls", agg_panel_html, fixed = TRUE))
  expect_match(agg_panel_html, "results-cmp_deviation", fixed = TRUE)
  expect_false(grepl("results-uncertainty_band", agg_panel_html, fixed = TRUE))
  expect_false(grepl("results-ensemble_band", agg_panel_html, fixed = TRUE))
  expect_false(grepl("results-show_coef_uncertainty", agg_panel_html, fixed = TRUE))
  expect_false(grepl("results-show_model_spread", agg_panel_html, fixed = TRUE))
  expect_false(grepl("results-bandwidth_p0", agg_panel_html, fixed = TRUE))
  expect_false(grepl("results-cmp_group_order", agg_panel_html, fixed = TRUE))

  # Verify the 5 sections in the overall page
  expect_match(html, "How is consumption predicted to vary across climate scenarios and weather years?", fixed = TRUE)
  expect_match(html, "What outcomes are predicted in adverse weather years?", fixed = TRUE)
  expect_match(html, "What is the probability of severe outcomes occurring?", fixed = TRUE)
  expect_match(html, "results-exceedance_model_spread", fixed = TRUE)
  expect_match(html, "Climate model spread", fixed = TRUE)
  expect_match(html, "Full ensemble spread", fixed = TRUE)
  expect_match(html, "results-ensemble_band", fixed = TRUE)
  expect_match(html, "value=\"none\"", fixed = TRUE)
  expect_match(html, "What drives the uncertainty in these predictions?", fixed = TRUE)
  expect_match(html, "Detailed return-period outcomes and uncertainty", fixed = TRUE)

  # Verify distributional incidence is removed from this page
  expect_false(grepl("results-incidence_plot", html, fixed = TRUE))
  expect_false(grepl("results-incidence_table", html, fixed = TRUE))
})

test_that("annual distribution UI includes a plot type selector", {
  so <- list(name = "welfare", type = "numeric", label = "Welfare")
  html <- as.character(htmltools::renderTags(
    wiseapp:::.results_content_ui(shiny::NS("results"), so)
  )$html)

  expect_match(html, "results-annual_distribution_type", fixed = TRUE)
  expect_match(html, "Violin", fixed = TRUE)
  expect_match(html, "Boxplot", fixed = TRUE)
})

test_that("threshold-table direction is always defined from the selected metric", {
  expect_identical(
    wiseapp:::metric_metadata("headcount_ratio", list(direction = "higher_is_better"))$adverse_tail,
    "high"
  )
  expect_identical(
    wiseapp:::metric_metadata("mean", list(direction = "lower_is_better"))$adverse_tail,
    "low"
  )
})

test_that("adverse dot data uses the selected climate-model spread", {
  threshold_tbl <- tibble::tibble(
    scenario = rep("SSP2-4.5 / 2030", 6L),
    Estimate = c("Central (P50)", "Central (P50)",
                 "Ensemble min", "Ensemble min", "Ensemble max", "Ensemble max"),
    rp_name = c("1:1", "1:5", "1:1", "1:5", "1:1", "1:5"),
    value = c(5, 5, 4, 3, 6, 7),
    is_historical = FALSE,
    n_obs = 30L
  )
  threshold_tbl <- dplyr::bind_rows(threshold_tbl, tibble::tibble(
    scenario = "Historical", Estimate = "Single historical estimate",
    rp_name = "1:1", value = 0, is_historical = TRUE, n_obs = 30L
  ))

  dot <- step2_adverse_dot_data(threshold_tbl, method = "mean")
  expect_equal(dot$intermod_lo[!dot$is_historical], c(4, 3))
  expect_equal(dot$intermod_hi[!dot$is_historical], c(6, 7))
})

# Batch 2 UI migration: DT -> reactable, ggplot -> echarts4r (guidelines §6/§7)
# ============================================================================ #

test_that("echart_pointrange_climate draws bands, dots and empty states", {
  bands <- tibble::tibble(
    scenario      = c("Historical", "SSP2-4.5 / 2030-2040", "SSP3-7.0 / 2030-2040"),
    value         = c(4.50, 4.60, 4.70),
    coef_lo       = c(4.45, 4.55, 4.65),
    coef_hi       = c(4.55, 4.65, 4.75),
    interann_lo   = c(4.20, 4.30, 4.40),
    interann_hi   = c(4.80, 4.90, 5.00),
    intermod_lo   = c(4.50, 4.55, 4.60),
    intermod_hi   = c(4.50, 4.65, 4.75),
    total_lo      = NA_real_, total_hi = NA_real_,
    is_historical = c(TRUE, FALSE, FALSE),
    n_models      = 1L
  )
  ch <- echart_pointrange_climate(bands, "Mean welfare", show_coef = TRUE)
  expect_s3_class(ch, "echarts4r")
  types <- vapply(ch$x$opts$series, `[[`, character(1), "type")
  expect_true("bar" %in% types)
  expect_true("scatter" %in% types)
  # One open central marker per scenario row plus its bands.
  expect_equal(sum(types == "scatter"), 3L)
  expect_identical(ch$x$opts$yAxis$name, "Mean welfare")

  blank <- echart_pointrange_climate(NULL)
  expect_s3_class(blank, "echarts4r")
  expect_match(blank$x$opts$title[[1]]$text, "Run a simulation to see results.",
    fixed = TRUE
  )
})

test_that("echart_annual_distribution keeps violin and boxplot modes", {
  set.seed(2)
  curves <- do.call(rbind, lapply(
    c("Historical", "SSP2-4.5 / 2030-2040"),
    function(s) {
      data.frame(
        scenario = s, model_id = paste0("m", 1:3),
        sim_year = rep(2020:2022, 3),
        value = rnorm(9, ifelse(s == "Historical", 5, 5.2)),
        is_historical = s == "Historical"
      )
    }
  ))
  violin <- echart_annual_distribution(curves, "Mean", "violin")
  expect_s3_class(violin, "echarts4r")
  # Historical-mean reference markLine survives the migration.
  ml <- violin$x$opts$series[[which(vapply(violin$x$opts$series, function(s)
    !is.null(s$markLine), logical(1)))[1]]]$markLine
  expect_equal(ml$data[[1]]$xAxis, mean(curves$value[curves$scenario == "Historical"]))

  box <- echart_annual_distribution(curves, "Mean", "boxplot")
  expect_s3_class(box, "echarts4r")
  types <- vapply(box$x$opts$series, `[[`, character(1), "type")
  expect_true(all(types %in% c("custom", "scatter", "line")))

  blank <- echart_annual_distribution(NULL, "Mean", "violin")
  expect_match(blank$x$opts$title[[1]]$text, "No annual simulation results available.",
    fixed = TRUE
  )
})

test_that("echart_step2_adverse_dot labels scenarios on the top row", {
  tbl <- data.frame(
    scenario = rep(c("SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040"), each = 2),
    Estimate = "Central (P50)",
    rp_label = rep(factor(c("Adverse 1-in-10", "Expected"),
      levels = rev(c("Expected", "Adverse 1-in-5", "Adverse 1-in-10",
        "Adverse 1-in-20", "Adverse 1-in-50")
    )), 2),
    value = c(12, 13, 12.5, 13.5),
    is_historical = FALSE,
    intermod_lo = c(11, 12, 11.5, 12.5),
    intermod_hi = c(13, 14, 13.5, 14.5)
  )
  ch <- echart_step2_adverse_dot(tbl, "Outcome")
  expect_s3_class(ch, "echarts4r")
  dots <- Filter(function(s) identical(s$type, "scatter"), ch$x$opts$series)
  expect_length(dots, 2L)
  # One bold endpoint label per scenario (direct labels, no legend).
  labs <- unlist(lapply(dots, function(s) {
    vapply(s$data, function(pt) {
      if (!is.null(pt$label)) pt$label$formatter else NA_character_
    }, character(1))
  }))
  expect_setequal(stats::na.omit(labs), c("SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040"))

  blank <- echart_step2_adverse_dot(NULL)
  expect_match(blank$x$opts$title[[1]]$text, "Return-period outcomes are unavailable.",
    fixed = TRUE
  )
})

test_that("echart_exceedance mirrors the median curves and empty state", {
  curves <- data.frame(
    scenario = rep(c("Historical", "SSP2 / 2030"), each = 30),
    model_id = rep(rep(c("m1", "m2"), each = 15), 2),
    rank = rep(seq(15), 4),
    welfare_val = c(seq(1, 30), seq(2, 31)),
    coef_sd = 0.1,
    exceed_prob = rep((seq(15) - 0.5) / 30, 4),
    is_historical = rep(c(TRUE, FALSE), each = 30)
  )
  ch <- echart_exceedance(curves, "Mean", n_sim_years = 30,
    logit_x = TRUE, band_q = NULL, ensemble_band_q = c(lo = 0.25, hi = 0.75)
  )
  expect_s3_class(ch, "echarts4r")
  expect_identical(ch$x$opts$xAxis$type, "log")
  expect_match(ch$x$opts$xAxis$name, "Annual adverse exceedance probability",
    fixed = TRUE
  )
  # One labelled median line per scenario plus two ensemble boundary lines
  # per future scenario.
  median_lines <- Filter(function(s) {
    identical(s$type, "line") && !is.null(s$name) &&
      !grepl("__", s$name, fixed = TRUE) && !identical(s$name, "Historical mean")
  }, ch$x$opts$series)
  expect_setequal(
    vapply(median_lines, `[[`, character(1), "name"),
    c("Historical", "SSP2 / 2030")
  )

  blank <- echart_exceedance(NULL, "Mean")
  expect_match(blank$x$opts$title[[1]]$text,
    "Run a simulation to see exceedance probabilities.", fixed = TRUE
  )
})

test_that("echart_exceedance uses explicit stacked uncertainty bands", {
  curves <- data.frame(
    scenario = rep(c("Historical", "SSP2 / 2030"), each = 20),
    model_id = rep(rep(c("m1", "m2"), each = 10), 2),
    rank = rep(seq_len(10), 4),
    welfare_val = c(seq(1, 10), seq(2, 11)),
    coef_sd = 0.1,
    exceed_prob = rep((seq(10) - 0.5) / 20, 4),
    is_historical = rep(c(TRUE, FALSE), each = 20)
  )
  chart <- echart_exceedance(
    curves, "Mean", n_sim_years = 20,
    band_q = c(lo = 0.10, hi = 0.90),
    ensemble_band_q = c(lo = 0.10, hi = 0.90)
  )
  bands <- Filter(function(s) grepl("__(ensemble|coefficient)$", s$name), chart$x$opts$series)
  expect_true(length(bands) >= 3L)
  band_shapes <- vapply(bands, function(s) {
    !is.null(s$lineStyle) && !is.null(s$areaStyle) &&
      is.matrix(s$data) && nrow(s$data) >= 4L
  }, logical(1))
  expect_true(all(band_shapes))
  expect_true(all(vapply(bands, function(s) is.null(s$stack), logical(1))))
  expect_gt(length(unique(vapply(bands, `[[`, character(1), "name"))), 1L)
})

test_that("echart_variance_contribution draws one bar series per source", {
  vb <- data.frame(
    scenario = c("Historical", "SSP2 / 2030"),
    var_coef = c(1, 2), var_within = c(4, 3), var_across = c(0, 9),
    is_historical = c(TRUE, FALSE)
  )
  ch <- echart_variance_contribution(vb)
  expect_s3_class(ch, "echarts4r")
  expect_length(ch$x$opts$series, 3L)
  expect_true(all(vapply(ch$x$opts$series, function(s)
    identical(s$type, "bar"), logical(1))))
  blank <- echart_variance_contribution(NULL)
  expect_match(blank$x$opts$title[[1]]$text, "Run a simulation to see SD contributions.",
    fixed = TRUE
  )
})

test_that("results module renders echarts charts and a reactable threshold table", {
  skip_if_not_installed("shiny")
  testServer(
    mod_2_02_results_server,
    args = list(
      id = "results", hist_sim = reactiveVal(make_hist_sim_fixture()),
      saved_scenarios = reactiveVal(list()), selected_hist = reactiveVal(NULL),
      tabset_id = "step2_output_tabs"
    ),
    {
      session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none",
                        cmp_group_order = "scenario_x_year")
      session$flushReact()

      # Zero-arg closures return echarts widgets (render + export share them).
      expect_s3_class(pointrange_chart(), "echarts4r")
      expect_s3_class(annual_distribution_chart(), "echarts4r")
      expect_s3_class(adverse_dot_chart(), "echarts4r")
      expect_s3_class(exceedance_chart(), "echarts4r")
      expect_s3_class(uncertainty_chart(), "echarts4r")

      # Threshold table is a reactable widget carrying the raw rows.
      tbl <- threshold_table_df()
      expect_gt(nrow(tbl), 0L)
      rt <- threshold_reactable()
      expect_s3_class(rt, "reactable")
      payload <- jsonlite::fromJSON(rt$x$tag$attribs$data)
      expect_identical(as.character(payload[["Scenario / Period"]][[1]]),
        "Historical")
    }
  )
})

test_that("results content UI mounts chart outputs and the reactable CSV button", {
  so <- list(name = "welfare", type = "numeric", label = "Welfare")
  html <- as.character(htmltools::renderTags(
    wiseapp:::.results_content_ui(shiny::NS("results"), so)
  )$html)

  expect_match(html, "results-annual_distribution_plot", fixed = TRUE)
  expect_match(html, "results-adverse_dot_plot", fixed = TRUE)
  expect_match(html, "results-exceedance_plot", fixed = TRUE)
  expect_match(html, "results-uncertainty_sources_plot", fixed = TRUE)
  expect_match(html, "results-summary_threshold_table", fixed = TRUE)
  # Client-side CSV button bound to the reactable widget id.
  expect_match(html, "Reactable.downloadDataCSV", fixed = TRUE)
  expect_match(html, "climate_outcome_thresholds.csv", fixed = TRUE)
  expect_false(grepl("threshold_csv", html, fixed = TRUE))
})

test_that("results_section container id is namespaced (CR-BUG-12)", {
  src <- testthat::test_path("..", "..", "R", "mod_2_02_results.R")
  skip_if(!file.exists(src), "R/ source tree not available (installed package)")
  text <- readLines(src, warn = FALSE)
  code <- text[!grepl("^\\s*#", text)]
  # Every reference goes through ns(); no bare id or selector remains.
  expect_false(any(grepl("\"#results_section|id = \"results_section", code)))
  expect_true(any(grepl("ns(\"results_section\")", code, fixed = TRUE)))
})

test_that("lazy aggregation S3 methods live at package top level (CR-BUG-15)", {
  ns <- asNamespace("wiseapp")
  for (m in c("[[.wise_lazy_aggregation_method_list", "$.wise_lazy_aggregation_method_list",
              "names.wise_lazy_aggregation_method_list", "length.wise_lazy_aggregation_method_list",
              ".force_lazy_aggregation_method")) {
    expect_true(exists(m, envir = ns, inherits = FALSE), info = m)
  }
  calls <- 0L
  state <- new.env(parent = emptyenv())
  state$builder <- function() {
    calls <<- calls + 1L
    list(mean = data.frame(v = 1))
  }
  state$built <- FALSE
  x <- structure(list(mean = NULL), class = c("wise_lazy_aggregation_method_list", "list"),
    state = state)
  expect_identical(calls, 0L)
  expect_identical(length(x), 1L)
  expect_identical(names(x), "mean")
  expect_identical(calls, 0L)
  expect_identical(x[["mean"]]$v, 1)
  expect_identical(x$mean$v, 1)
  expect_identical(calls, 1L)
})

test_that("step2_adverse_return_period inverts the adverse exceedance share", {
  v <- c(rep(1, 90), rep(5, 10))
  expect_equal(step2_adverse_return_period(v, 5, "high"), 10)
  expect_equal(step2_adverse_return_period(-v, -5, "low"), 10)
  expect_identical(step2_adverse_return_period(v, 9, "high"), Inf)
  expect_true(is.na(step2_adverse_return_period(numeric(0), 5)))
  expect_true(is.na(step2_adverse_return_period(v, NA_real_)))
})

test_that("step2 headline cards report return-period shift, model agreement and people", {
  bands <- tibble::tibble(
    scenario = c("Historical", "SSP3-7.0 / 2025-2035"),
    value = c(0.30, 0.34), interann_lo = c(0.2, 0.2), interann_hi = c(0.4, 0.4),
    is_historical = c(TRUE, FALSE), n_models = c(1L, 4L)
  )
  hist_vals <- seq(0.20, 0.40, length.out = 40)
  ts <- dplyr::bind_rows(
    tibble::tibble(scenario = "Historical", model_id = "h", sim_year = 1:40,
      value = hist_vals, is_historical = TRUE),
    do.call(rbind, lapply(1:4, function(m) tibble::tibble(
      scenario = "SSP3-7.0 / 2025-2035", model_id = paste0("m", m), sim_year = 1:40,
      value = hist_vals + 0.04 + 0.01 * m, is_historical = FALSE)))
  )
  thresh <- tibble::tibble(
    scenario = c(rep("Historical", 2L), rep("SSP3-7.0 / 2025-2035", 2L)),
    Estimate = "Central (P50)", rp_name = rep(c("1:1", "1:20"), 2L),
    value = c(0.30, 0.38, 0.35, 0.41)
  )
  so <- list(type = "numeric", name = "welfare", label = "Consumption", units = "PPP")
  hist_sim <- list(so = so, svy = data.frame(weight = rep(1000, 50)))
  metadata <- metric_metadata("headcount_ratio", so, pov_line = 3, weighted = TRUE)
  cards <- step2_headline_cards(bands, thresh, hist_sim,
    saved_scenarios = list("SSP3-7.0 / 2025-2035" = list()),
    method = "headcount_ratio", timeseries_curves = ts, metadata = metadata)

  # Poverty rate is lower-is-better: a rise is adverse.
  expect_identical(cards[[1]]$status$kind, "adverse")
  expect_match(cards[[1]]$value, "^\\+4\\.0 pp")
  # Return-period shift: more than 1 in 20 of the scenario years now reach 0.38.
  expect_match(cards[[2]]$note, "Historical 1-in-20 year \u2192 about 1-in-", fixed = TRUE)
  expect_match(cards[[2]]$note, "in SSP3-7.0 / 2025-2035", fixed = TRUE)
  expect_lt(cards[[2]]$return_period_native, 20)
  # All four models shift upward and the shift is large relative to variability.
  expect_identical(cards[[3]]$value, "4 of 4 models")
  expect_identical(cards[[3]]$status$text, "Robust: worsens")
  expect_gt(cards[[3]]$snr_native, 0.3)
  # Number of poor on the first two cards: +4 pp x 50,000 weighted people = 2.0K,
  # and +3 pp at the 1-in-20 level = 1.5K.
  expect_match(cards[[1]]$note, "\u2248 2.0K more poor", fixed = TRUE)
  expect_match(cards[[2]]$note, "\u2248 1.5K more poor", fixed = TRUE)
  # The fourth card stays the year-to-year range for every metric.
  expect_identical(cards[[4]]$label, "Year-to-year range")
})

test_that("step2_baseline_check compares the observed survey with the simulated baseline", {
  set.seed(1)
  svy <- data.frame(welfare = c(rep(2, 40), rep(6, 60)), weight = 1)
  so <- list(name = "welfare", type = "numeric", transform = "none", units = "PPP")
  rate <- metric_metadata("headcount_ratio", so, pov_line = 3, weighted = TRUE)
  # Observed poverty rate: 40% below $3.
  ok <- step2_baseline_check(svy, so, "headcount_ratio", 3, TRUE, 0.41, rate)
  expect_equal(ok$observed, 0.40)
  expect_equal(ok$gap, 0.01, tolerance = 1e-8)
  expect_false(ok$flag)
  expect_identical(ok$tolerance_text, "2 pp")
  # A 5 pp miss exceeds the 2 pp tolerance for rates.
  bad <- step2_baseline_check(svy, so, "headcount_ratio", 3, TRUE, 0.45, rate)
  expect_true(bad$flag)
  # Levels use a relative tolerance: observed mean 4.4, 10% = 0.44.
  lvl <- metric_metadata("mean", so)
  expect_false(step2_baseline_check(svy, so, "mean", NULL, TRUE, 4.8, lvl)$flag)
  expect_true(step2_baseline_check(svy, so, "mean", NULL, TRUE, 5.0, lvl)$flag)
  # Log outcomes are compared on the level scale.
  svy_log <- data.frame(welfare = exp(c(rep(log(2), 40), rep(log(6), 60))), weight = 1)
  so_log <- list(name = "welfare", type = "numeric", transform = "log", units = "PPP")
  expect_equal(step2_baseline_check(svy_log, so_log, "mean", NULL, TRUE, 4.4, lvl)$observed, 4.4)
  # Not available: no outcome column, binary "poor" outcome, LCU log without the PPP factor.
  expect_null(step2_baseline_check(svy[, "weight", drop = FALSE], so, "mean", NULL, TRUE, 4, lvl))
  expect_null(step2_baseline_check(svy, list(name = "poor"), "mean", NULL, TRUE, 4, lvl))
  so_lcu <- list(name = "welfare", type = "numeric", transform = "log", units = "LCU")
  expect_null(step2_baseline_check(svy_log, so_lcu, "mean", NULL, TRUE, 4, lvl))
  expect_null(step2_baseline_check(svy, so, "mean", NULL, TRUE, NA_real_, lvl))
})

test_that("a baseline check gap above tolerance is flagged in the basis strip", {
  bands <- tibble::tibble(
    scenario = c("Historical", "SSP3-7.0 / 2025-2035"), value = c(0.40, 0.42),
    interann_lo = c(0.3, 0.3), interann_hi = c(0.5, 0.5),
    is_historical = c(TRUE, FALSE), n_models = c(1L, 2L)
  )
  so <- list(type = "numeric", name = "welfare", units = "PPP")
  hist_sim <- list(so = so)
  meta <- metric_metadata("headcount_ratio", so, pov_line = 3, weighted = TRUE)
  chk <- list(observed = 0.30, simulated = 0.40, gap = 0.10, flag = TRUE,
    tolerance_text = "2 pp")
  cards <- step2_headline_cards(bands, hist_sim = hist_sim, method = "headcount_ratio",
    metadata = meta, baseline_check = chk)
  expect_match(cards[[1]]$info, "Baseline check: simulated 40.0% vs survey 30.0%", fixed = TRUE)
  expect_match(cards[[1]]$basis_text, "exceeds 2 pp", fixed = TRUE)
  chk$flag <- FALSE
  quiet <- step2_headline_cards(bands, hist_sim = hist_sim, method = "headcount_ratio",
    metadata = meta, baseline_check = chk)
  expect_match(quiet[[1]]$info, "Baseline check", fixed = TRUE)
  expect_false(grepl("Baseline check", quiet[[1]]$basis_text, fixed = TRUE))
})

test_that("Results module feeds the observed-survey baseline check into the headline cards", {
  pipe <- function(values, years) list(
    y_point = rep(values, each = 4) + rep(c(-.03, -.01, .01, .03), length(values)),
    sim_year = rep(years, each = 4),
    F_loading = matrix(0, nrow = 4 * length(values), ncol = 1L)
  )
  so <- list(type = "numeric", name = "welfare", transform = "identity", units = "PPP")
  hist <- list(
    so = so, residuals = "none", has_weights = FALSE,
    pipeline = pipe(rep(5, 20), 2000:2019),
    svy = data.frame(welfare = c(4, 5, 6, 5), weight = 1)
  )
  scenario <- "SSP2-4.5 / 2030-2050"
  saved <- setNames(list(list(so = so, pipelines = list(
    m1 = pipe(rep(5.5, 20), 2030:2049)
  ))), scenario)
  testServer(mod_2_02_results_server, args = list(
    id = "results", hist_sim = reactiveVal(hist),
    saved_scenarios = reactiveVal(saved), selected_hist = reactiveVal(NULL),
    tabset_id = "step2_output_tabs"
  ), {
    session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none")
    session$flushReact()
    chk <- baseline_check_rv()
    expect_equal(chk$observed, 5)
    expect_equal(chk$simulated, 5, tolerance = 0.01)
    expect_false(chk$flag)
    card1 <- headline_cards_data_rv()[[1]]
    expect_match(card1$info, "Baseline check", fixed = TRUE)
    expect_equal(card1$baseline_check$observed, 5)
  })
})

test_that("simulation-years note uses the scenarios, models and years actually simulated", {
  bands <- tibble::tibble(
    scenario = c("Historical", "SSP2-4.5 / 2030", "SSP3-7.0 / 2030"),
    value = c(1, 1.1, 1.2), interann_lo = 0.9, interann_hi = 1.3,
    is_historical = c(TRUE, FALSE, FALSE), n_models = c(1L, 22L, 22L)
  )
  so <- list(type = "numeric", name = "welfare", units = "PPP")
  mk <- function(scenario, models, years, hist = FALSE) tibble::tibble(
    scenario = scenario,
    model_id = rep(models, each = length(years)),
    sim_year = rep(years, length(models)),
    value = 1, is_historical = hist
  )
  # Uniform: 2 scenarios x 3 models x 4 years + 4 historical years.
  uniform <- dplyr::bind_rows(
    mk("Historical", "h", 1:4, TRUE),
    mk("SSP2-4.5 / 2030", c("a", "b", "c"), 1:4),
    mk("SSP3-7.0 / 2030", c("a", "b", "c"), 1:4)
  )
  cards <- step2_headline_cards(bands, hist_sim = list(so = so),
    saved_scenarios = list(a = list(), b = list(), c = list()),
    method = "mean", timeseries_curves = uniform)
  expect_match(cards[[5]]$basis_text, "(2 scenarios \u00d7 3 models + 1 historical) \u00d7 4 yrs", fixed = TRUE)
  expect_identical(cards[[5]]$value, as.character(nrow(uniform)))
  # Models differ across scenarios (and one saved scenario was not selected):
  # state the range rather than a product that does not add up.
  uneven <- dplyr::bind_rows(
    mk("Historical", "h", 1:4, TRUE),
    mk("SSP2-4.5 / 2030", c("a", "b", "c"), 1:4),
    mk("SSP3-7.0 / 2030", c("a", "b"), 1:4)
  )
  cards2 <- step2_headline_cards(bands, hist_sim = list(so = so),
    saved_scenarios = list(a = list(), b = list(), c = list()),
    method = "mean", timeseries_curves = uneven)
  expect_match(cards2[[5]]$basis_text, "2 scenarios, 2\u20133 models and 4 yrs each", fixed = TRUE)
  expect_identical(cards2[[5]]$value, as.character(nrow(uneven)))
})

test_that("step2_delta_ci is the norm of the difference of mean gradients", {
  hist_tbl <- tibble::tibble(F_agg_all = list(
    matrix(c(1, 0), nrow = 1), matrix(c(3, 0), nrow = 1)
  ))
  scen_tbl <- tibble::tibble(F_agg_all = list(
    matrix(c(4, 2, 4, 2), nrow = 2, byrow = TRUE),
    matrix(c(6, 2, 6, 2), nrow = 2, byrow = TRUE)
  ))
  # Mean historical gradient (2, 0); mean scenario gradient (5, 2); diff (3, 2).
  expect_equal(step2_delta_ci(hist_tbl, scen_tbl)$sd, sqrt(13))
  # Same gradients for both: the coefficients move them together, no uncertainty.
  expect_null(step2_delta_ci(hist_tbl, hist_tbl))
  expect_null(step2_delta_ci(hist_tbl, tibble::tibble(value = 1)))
  expect_null(step2_delta_ci(NULL, scen_tbl))
})

test_that("Expected change card prints the 95% CI from estimation uncertainty", {
  bands <- tibble::tibble(
    scenario = c("Historical", "SSP3-7.0 / 2025-2035"), value = c(4.50, 4.40),
    interann_lo = 4, interann_hi = 5, is_historical = c(TRUE, FALSE),
    n_models = c(1L, 2L)
  )
  so <- list(type = "numeric", name = "welfare", label = "Consumption", units = "$/day")
  cards <- step2_headline_cards(bands, hist_sim = list(so = so), method = "mean",
    delta_ci = list(sd = 0.025))
  # -0.10 +/- 1.96 * 0.025 = -0.149 to -0.051.
  expect_match(cards[[1]]$note, "(95% CI: -0.15 to -0.05 $/day)", fixed = TRUE)
  expect_equal(cards[[1]]$change_ci_native, -0.10 + c(-1, 1) * stats::qnorm(0.975) * 0.025)
  expect_match(cards[[1]]$info, "weather coefficients only", fixed = TRUE)
  # No interval when gradients are unavailable.
  none <- step2_headline_cards(bands, hist_sim = list(so = so), method = "mean")
  expect_false(grepl("95% CI", none[[1]]$note, fixed = TRUE))
  expect_true(all(is.na(none[[1]]$change_ci_native)))
})

test_that("Results module derives the climate-shift interval from the aggregation gradients", {
  set.seed(3)
  pipe <- function(values, years) {
    n <- 4 * length(values)
    list(
      y_point = rep(values, each = 4) + rep(c(-.03, -.01, .01, .03), length(values)),
      sim_year = rep(years, each = 4),
      F_loading = matrix(stats::rnorm(n * 2) * 0.05, nrow = n)
    )
  }
  so <- list(type = "numeric", name = "welfare", transform = "identity", units = "PPP")
  hist <- list(so = so, residuals = "none", has_weights = FALSE,
    pipeline = pipe(rep(5, 20), 2000:2019))
  scenario <- "SSP2-4.5 / 2030-2050"
  saved <- setNames(list(list(so = so, pipelines = list(
    m1 = pipe(rep(5.5, 20), 2030:2049), m2 = pipe(rep(5.7, 20), 2030:2049)
  ))), scenario)
  testServer(mod_2_02_results_server, args = list(
    id = "results", hist_sim = reactiveVal(hist),
    saved_scenarios = reactiveVal(saved), selected_hist = reactiveVal(NULL),
    tabset_id = "step2_output_tabs"
  ), {
    session$setInputs(cmp_agg_method = "mean", cmp_deviation = "none")
    session$flushReact()
    ci <- delta_ci_rv()
    expect_false(is.null(ci))
    expect_gt(ci$sd, 0)
    expect_match(headline_cards_data_rv()[[1]]$note, "95% CI", fixed = TRUE)
  })
})

test_that("weather support is stated in the popover and warned about in the basis strip", {
  bands <- tibble::tibble(
    scenario = c("Historical", "SSP3-7.0 / 2025-2035"), value = c(4.5, 4.4),
    interann_lo = 4, interann_hi = 5, is_historical = c(TRUE, FALSE),
    n_models = c(1L, 2L)
  )
  so <- list(type = "numeric", name = "welfare", units = "$/day")
  support <- data.frame(
    weather_variable = c("temp", "rain"), weather_label = c("Temperature", NA),
    outside_share = c(0.123, 0.01), warning = c(TRUE, FALSE)
  )
  cards <- step2_headline_cards(bands, hist_sim = list(so = so), method = "mean",
    weather_support = support)
  expect_match(cards[[1]]$info, "Temperature 12.3%; rain 1.0%", fixed = TRUE)
  expect_match(cards[[1]]$basis_text,
    "Weather outside historical range: Temperature 12.3% (see Diagnostics)", fixed = TRUE)
  support$warning <- FALSE
  quiet <- step2_headline_cards(bands, hist_sim = list(so = so), method = "mean",
    weather_support = support)
  expect_match(quiet[[1]]$info, "Weather support", fixed = TRUE)
  expect_false(grepl("Weather outside", quiet[[1]]$basis_text, fixed = TRUE))
})

test_that("weather_support_pool adds member counts and recomputes the share", {
  rows <- list(
    data.frame(weather_variable = "temp", scenario = "m1", n_scenario = 100,
      outside_n = 10, outside_share = 0.10, warning = TRUE),
    data.frame(weather_variable = "temp", scenario = "m2", n_scenario = 100,
      outside_n = 0, outside_share = 0, warning = FALSE)
  )
  pooled <- weather_support_pool(rows, "SSP2 / 2030")
  expect_equal(pooled$outside_n, 10)
  expect_equal(pooled$n_scenario, 200)
  expect_equal(pooled$outside_share, 0.05)
  expect_false(pooled$warning)
  expect_equal(pooled$max_member_share, 0.10)
  expect_identical(pooled$scenario, "SSP2 / 2030")
  expect_null(weather_support_pool(list(NULL), "x"))
})
