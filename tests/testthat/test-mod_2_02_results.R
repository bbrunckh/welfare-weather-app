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

      # headcount reads both pl and bandwidth; gap ignores bandwidth
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

      # New bandwidth invalidates only headcount
      session$setInputs(bandwidth_p0 = 0.20); settle()
      hc3 <- .get_hist_agg("headcount_ratio")
      expect_false(identical(hc2, hc3))
      expect_length(ls(envir = ws()$cache), 5L)
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

      central <- thresholds[thresholds$Estimate == "Central (P50)" &
                              thresholds$rp_name == "1:1", , drop = FALSE]
      expect_equal(dplyr::n_distinct(round(central$value, 8)), 3L)
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

test_that("step2_headline_cards returns 5 cards with mod_1 styling", {
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
    c("Expected outcome", "Adverse weather years", "Range across years",
      "Climate-model spread", "Simulation years")
  )

  # Every card has non-empty fields
  for (card in cards) {
    expect_true(nzchar(card$label))
    expect_true(nzchar(card$value))
    expect_true(nzchar(card$note))
    expect_s3_class(card$note_html, "shiny.tag.list")
    expect_true(nzchar(card$info))
  }

  # Card 1: Typical outcome
  expect_identical(cards[[1]]$value, "4.50 vs 4.52")
  expect_match(cards[[1]]$note, "Historical vs SSP", fixed = TRUE)
  expect_match(cards[[1]]$note, "Mean weather year", fixed = TRUE)

  median_cards <- step2_headline_cards(
    bands            = bands,
    threshold_tbl    = thresh_tbl,
    hist_sim         = hist_sim,
    saved_scenarios  = saved,
    method           = "median",
    timeseries_curves = timeseries
  )
  expect_match(median_cards[[1]]$note, "Mean weather year", fixed = TRUE)
  expect_false(grepl("Median weather year", median_cards[[1]]$note, fixed = TRUE))

  # Card 2: Adverse weather years (1-in-20 year)
  expect_identical(cards[[2]]$value, "4.05 vs 4.10")
  expect_match(cards[[2]]$note, "Historical vs SSP", fixed = TRUE)
  expect_match(cards[[2]]$note, "1-in-20 year", fixed = TRUE)

  # Card 3: Range across years
  expect_identical(cards[[3]]$value, "4.35 to 4.80")
  expect_match(cards[[3]]$note, "Hist: 4.20 to 4.80", fixed = TRUE)
  expect_match(cards[[3]]$note, "Inter-annual weather variability", fixed = TRUE)

  # Card 4: Climate-model spread
  expect_identical(cards[[4]]$value, "4.45 to 4.65")
  expect_match(cards[[4]]$note, "Full range across 2 models", fixed = TRUE)
  expect_match(cards[[4]]$note, "CMIP6 model disagreement", fixed = TRUE)
  expect_false(grepl("Coef", cards[[4]]$note, fixed = TRUE))

  # Card 5: Simulation years
  expect_identical(cards[[5]]$value, "6")
  expect_match(cards[[5]]$note, "(1 SSP \u00d7 22 models + 1 historical) \u00d7 3 yrs", fixed = TRUE)
  expect_identical(cards[[5]]$class, "neutral")

  # Table conversion
  df <- step2_headline_df(cards)
  expect_equal(nrow(df), 5L)
  expect_identical(names(df), c("Metric", "Value", "Note"))
  expect_identical(df$Metric, labels)
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
  expect_identical(cards[[1]]$value, "4.50")
  expect_identical(cards[[4]]$value, "Not applicable")
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

test_that("adverse plot uses the selected climate-model spread", {
  threshold_tbl <- tibble::tibble(
    scenario = rep("SSP2-4.5 / 2030", 6L),
    Estimate = c("Central (P50)", "Central (P50)",
                 "Ensemble min", "Ensemble min", "Ensemble max", "Ensemble max"),
    rp_name = c("1:1", "1:5", "1:1", "1:5", "1:1", "1:5"),
    value = c(5, 5, 4, 3, 6, 7),
    is_historical = FALSE,
    n_obs = 30L
  )

  dot <- step2_adverse_dot_data(threshold_tbl, method = "mean")
  expect_equal(dot$intermod_lo, c(4, 3))
  expect_equal(dot$intermod_hi, c(6, 7))

  plot <- plot_step2_adverse_dot(dot)
  expect_equal(plot$data$intermod_lo, c(4, 3))
  expect_equal(plot$data$intermod_hi, c(6, 7))
})

test_that("adverse plot labels scenarios directly on one shared x-axis", {
  threshold_tbl <- tibble::tibble(
    scenario = c(rep("SSP2-4.5 / 2030-2040", 6L),
                 rep("SSP2-4.5 / 2050-2060", 6L)),
    Estimate = rep(c("Central (P50)", "Central (P50)",
                     "Ensemble min", "Ensemble min",
                     "Ensemble max", "Ensemble max"), 2L),
    rp_name = rep(c("1:1", "1:5", "1:1", "1:5", "1:1", "1:5"), 2L),
    value = rep(c(5, 5, 4, 3, 6, 7), 2L),
    is_historical = FALSE,
    n_obs = 30L
  )

  dot <- step2_adverse_dot_data(threshold_tbl, method = "mean")
  plot <- plot_step2_adverse_dot(dot)
  colour_scale <- plot$scales$get_scales("colour")

  # Design D: the colour legend is replaced by direct scenario labels on the
  # top row, so the colour scale carries no guide.
  expect_identical(colour_scale$guide, "none")
  # Multiple future periods share one x-axis: no facet is added, each period
  # variant is its own labelled row instead.
  expect_true(inherits(plot$facet, "FacetNull"))
  # One bold text layer per scenario on the Expected row.
  text_layers <- Filter(function(l) inherits(l$geom, "GeomText"), plot$layers)
  expect_equal(length(text_layers), 1L)
  expect_setequal(text_layers[[1L]]$data$scenario_key,
                  c("SSP2-4.5 / 2030-2040", "SSP2-4.5 / 2050-2060"))
})

test_that("adverse dot plot offsets scenario dumbbells vertically", {
  # Two future scenarios at the same return-period rows: without vertical
  # dodging their dumbbells overlap and only one is readable.
  threshold_tbl <- tibble::tibble(
    scenario = rep(c("SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2030-2040"), each = 4L),
    Estimate = rep(c("Central (P50)", "Central (P50)",
                     "Ensemble min", "Ensemble max"), 2L),
    rp_name = rep(c("1:1", "1:5", "1:1", "1:5"), 2L),
    value = rep(c(4.6, 4.4, 3.8, 5.0), 2L),
    is_historical = FALSE,
    n_obs = 30L
  )
  dot <- step2_adverse_dot_data(threshold_tbl, method = "mean")
  plot <- plot_step2_adverse_dot(dot)

  # The y aesthetic must be scenario-dependent within a return period, so
  # dumbbells at the same rp_label get distinct vertical offsets.
  seg_idx <- which(vapply(plot$layers, function(l) inherits(l$geom, "GeomSegment"),
                          logical(1)))[1]
  seg <- ggplot2::layer_data(plot, seg_idx)
  expect_equal(nrow(seg), nrow(plot$data))

  # Match rendered segment rows back to data rows via the rp_label y position
  # pattern: currently both scenarios at the same rp_label share one y, so
  # y values repeat across scenarios. With vertical dodging, scenarios at the
  # same rp_label must have different rendered y values.
  y_expected <- as.integer(plot$data$rp_label)
  y1 <- seg$y[plot$data$scenario_key == "SSP2-4.5 / 2030-2040"]
  y2 <- seg$y[plot$data$scenario_key == "SSP5-8.5 / 2030-2040"]
  expect_false(isTRUE(all.equal(y1, y2)))
})

test_that("exceedance plot omits unsupported return-period warning annotation", {
  curves <- tibble::tibble(
    scenario = rep("SSP2-4.5 / 2030-2040", 30L),
    model_id = rep(c("m1", "m2"), each = 15L),
    rank = rep(seq_len(15L), 2L),
    welfare_val = seq_len(30L),
    coef_sd = 0,
    exceed_prob = rep((seq_len(15L) - 0.5) / 30, 2L),
    is_historical = FALSE
  )

  plot <- enhance_exceedance(
    curves, x_label = "Outcome", n_sim_years = 30L,
    logit_x = TRUE, band_q = NULL, ensemble_band_q = c(lo = 0, hi = 1)
  )
  labels <- vapply(plot$layers, function(layer) {
    if (!inherits(layer$geom, "GeomText")) return("")
    as.character(layer$stat_params$label %||% "")
  }, character(1L))
  expect_false(any(grepl("unreliable", labels, fixed = TRUE)))
  expect_false(any(grepl("1:50", labels, fixed = TRUE)))
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
  expect_true(all(types == "bar" | types == "scatter" | types == "line"))

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

test_that("echart_incidence_by_decile draws scenario bars with a zero line", {
  inc <- data.frame(
    decile = rep(1:10, 2),
    scenario = rep(c("Historical", "SSP2 / 2030"), each = 10),
    effect = rnorm(20)
  )
  ch <- echart_incidence_by_decile(inc)
  expect_s3_class(ch, "echarts4r")
  expect_length(ch$x$opts$series, 2L)
  ml <- ch$x$opts$series[[1]]$markLine
  expect_equal(ml$data[[1]]$yAxis, 0)
  blank <- echart_incidence_by_decile(NULL)
  expect_match(blank$x$opts$title[[1]]$text, "Distributional incidence is unavailable.",
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
      expect_s3_class(incidence_chart(), "echarts4r")
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
