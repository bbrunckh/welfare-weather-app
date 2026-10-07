# ============================================================================ #
# tests/testthat/test-mod_3_07_results.R                                       #
# Step 3 Results visualization and UI redesign unit tests                      #
# ============================================================================ #

library(testthat)
library(shiny)

test_that("technical decomposition cannot export logistic policy effects", {
  testServer(mod_3_09_decomposition_server, args = list(id = "decomp",
    model_fit = reactiveVal(list(engine = "fixest", model_type = "logistic")),
    so = reactiveVal(list(name = "indicator", type = "numeric", transform = "none"))), {
    session$flushReact()
    expect_length(decomp_scenarios(), 0L)
    expect_identical(policy_method_status()$status, "unsupported")
    expect_match(policy_method_status()$reason, "response-scale", fixed = TRUE)
    expect_equal(nrow(headline_decomp_data()), 0)
    expect_equal(nrow(decile_decomp_data()), 0)
    exported <- session$userData$wise_exports$items$policy_decomposition_headline_data$fun()
    expect_identical(names(exported), "Message")
  })
})

test_that("Step 3 cards format rate levels and absolute changes without benefit claims", {
  summary <- tibble::tibble(scenario = "Historical", baseline = .32, policy = .28,
    value = -.04, intermod_lo = -.04, intermod_hi = -.04, n_models = 1L)
  spec <- metric_metadata("headcount_ratio", list(name = "welfare", type = "numeric", units = "PPP"),
    pov_line = 3, weighted = TRUE, analysis_unit = "hh")
  cards <- step3_headline_cards(summary, method = "headcount_ratio",
    so = list(name = "welfare", type = "numeric", units = "PPP"), metric_context = spec)
  expect_match(cards[[1]]$value, "-4.00 pp", fixed = TRUE)
  expect_match(cards[[1]]$info, "28.00%", fixed = TRUE)
  expect_match(cards[[1]]$info, "32.00%", fixed = TRUE)
  expect_match(cards[[1]]$info, "3", fixed = TRUE)
  expect_match(cards[[1]]$info, "not necessarily a benefit", fixed = TRUE)
  binary <- step3_headline_cards(summary, so = list(name = "indicator", type = "binary"))
  expect_match(binary[[1]]$value, "-4.00 pp", fixed = TRUE)
  expect_match(binary[[1]]$info, "indicator", fixed = TRUE)
  summary$intermod_lo <- -.2
  summary$intermod_hi <- .7
  summary$n_models <- 3L
  binary <- step3_headline_cards(summary, so = list(name = "indicator", type = "binary"))
  expect_identical(binary[[4]]$value, "Models disagree")
  expect_identical(binary[[4]]$status$kind, "uncertain")
  expect_match(binary[[4]]$note, "Model range: -20.00 to +70.00 pp", fixed = TRUE)
})

test_that("resilience and adverse headline cards use metric-aware shared results", {
  summary <- tibble::tibble(scenario = "SSP2-4.5 / 2030-2040", baseline = .32,
    policy = .28, value = -.04, intermod_lo = -.04, intermod_hi = -.04, n_models = 2L)
  metric <- list(
    status = "ok", reason = NULL,
    metadata = list(method = "headcount_ratio", label = "Poverty rate", format = "percent",
      display_multiplier = 100, change_unit = "pp", level_unit = "percent",
      repositioning_modeled = TRUE, interaction_included = TRUE),
    scenarios = list("SSP2-4.5 / 2030-2040" = list(status = "ok", summary = data.frame(
      scenario = "SSP2-4.5 / 2030-2040", baseline = .32, after_main = .30,
      after_repositioning = .29, policy = .28, main = -.02,
      repositioning = -.01, interaction = -.01, resilience = -.02, total = -.04))),
    return_period = data.frame(scenario = "SSP2-4.5 / 2030-2040", return_period = 20,
      scope = "baseline_anchored", status = "ok", total = -.08,
      main = -.02, resilience = -.06, repositioning = -.01, interaction = -.05),
    mechanisms = list(repositioning_status = "modeled", interaction_status = "included")
  )
  cards <- step3_headline_cards(summary, method = "headcount_ratio", metric_decomposition = metric)
  expect_length(cards, 4L)
  expect_identical(cards[[2]]$label, "Resilience effect")
  expect_identical(cards[[2]]$value, "-6.00 pp")
  # Resilience = repositioning + interaction = -1 + -5 pp.
  expect_equal(-1 + -5, -6)
  # The total and main effects are not repeated on the card; they are in the popover.
  expect_false(grepl("Total", cards[[2]]$note, fixed = TRUE))
  expect_match(cards[[2]]$info, "Total adverse-year effect: -8.00 pp", fixed = TRUE)
  expect_match(cards[[2]]$info, "Main effect: -2.00 pp", fixed = TRUE)
  expect_match(cards[[2]]$note, "SSP2-4.5 / 2030-2040", fixed = TRUE)
  expect_match(cards[[2]]$note, "1-in-20 year", fixed = TRUE)
  expect_match(cards[[2]]$info, "Repositioning: -1.00 pp", fixed = TRUE)
  expect_match(cards[[2]]$info, "Interaction: -5.00 pp", fixed = TRUE)
  expect_match(as.character(cards[[2]]$note_html[[2]]), "font-weight: 600", fixed = TRUE)
  expect_equal(cards[[2]]$adverse_effect_native, -.08)
  # Poverty rate is lower-is-better: a negative resilience value reduces
  # weather sensitivity.
  expect_identical(cards[[2]]$status$kind, "favourable")
  expect_identical(cards[[2]]$status$text, "Reduces weather sensitivity")

  adverse <- metric
  adverse$return_period$resilience <- 0.0023
  adverse_cards <- step3_headline_cards(summary, method = "headcount_ratio",
    metric_decomposition = adverse)
  expect_identical(adverse_cards[[2]]$status$kind, "adverse")
  expect_identical(adverse_cards[[2]]$status$text, "Increases weather sensitivity")

  metric$metadata$repositioning_modeled <- FALSE
  metric$metadata$interaction_included <- FALSE
  metric$return_period$resilience <- NA_real_
  unavailable_resilience <- step3_headline_cards(summary, method = "headcount_ratio",
    metric_decomposition = metric)
  expect_identical(unavailable_resilience[[2]]$value, "Unavailable")
  expect_null(unavailable_resilience[[2]]$status)
  expect_match(unavailable_resilience[[2]]$note, "not modeled", fixed = TRUE)
  expect_match(unavailable_resilience[[2]]$note, "not included in fitted model", fixed = TRUE)
  expect_match(unavailable_resilience[[2]]$info, "Not included in fitted model", fixed = TRUE)
})

test_that("Results module returns selection API while preserving uncertainty reactive", {
  hs <- list(so = list(name = "welfare", type = "numeric", transform = "none"),
    pipeline = list(y_point = c(1, 2, 3, 4), sim_year = c(2020, 2020, 2021, 2021),
      F_loading = matrix(0, 4, 1)), residuals = "none")
  testServer(mod_3_07_results_server, args = list(id = "results",
    baseline_hist_sim = reactiveVal(hs), policy_hist_sim = reactiveVal(hs),
    baseline_saved_scenarios = reactiveVal(list()), policy_saved_scenarios = reactiveVal(list()),
    tabset_id = "tabs", analysis_unit = reactiveVal("ind")), {
    session$flushReact()
    api <- session$returned
    expect_true(all(c("aggregation_method", "poverty_line", "focus_scenario", "metric_context",
                      "metric_decomposition", "show_coef_uncertainty") %in% names(api)))
    expect_identical(api$aggregation_method(), "mean")
    expect_identical(api$metric_context()$analysis_unit, "ind")
    session$setInputs(show_coef_uncertainty = TRUE)
    expect_true(api$show_coef_uncertainty())
  })
})

test_that("step3_headline_cards builds 4 concise policy cards", {
  paired_sum <- tibble::tibble(
    scenario    = c("Historical", "SSP2-4.5 / 2030-2040"),
    value       = c(0.00, 0.45),
    baseline    = c(3, 3.2),
    policy      = c(3, 3.65),
    intermod_lo = c(0.00, 0.32),
    intermod_hi = c(0.00, 0.58),
    n_models    = c(1L, 4L),
    n_years     = c(30L, 30L)
  )

  thresh <- tibble::tibble(
    scenario      = rep(c("Historical", "SSP2-4.5 / 2030-2040"), each = 4),
    source        = rep(c("Baseline", "Baseline", "Policy", "Policy"), 2),
    Estimate      = rep("Central (P50)", 8),
    rp_name       = rep(c("1:1", "1:10"), 4),
    value         = c(3.0, 2.0, 3.0, 2.0,
                      3.2, 2.1, 3.65, 2.68),
    n_obs         = 30L,
    is_historical = rep(c(TRUE, FALSE), each = 4)
  )

  cards <- step3_headline_cards(
    paired_summary    = paired_sum,
    threshold_tbl     = thresh,
    baseline_agg      = list("SSP2-4.5 / 2030-2040" = list(out = data.frame(value = 3.2))),
    policy_agg        = list("SSP2-4.5 / 2030-2040" = list(out = data.frame(value = 3.65))),
    policy_svy        = NULL,
    sp_scenario       = list(budget_fixed = 12500000),
    timeseries_curves = data.frame(scenario = "SSP2-4.5 / 2030-2040", source = "Policy", sim_year = 2030:2031),
    method            = "mean",
    so                = list(type = "numeric", name = "welfare")
  )

  expect_length(cards, 4L)

  # Card 1: Expected policy effect
  expect_identical(cards[[1]]$label, "Expected policy effect")
  expect_identical(cards[[1]]$value, "+0.45 outcome units")
  expect_match(cards[[1]]$note, "Policy vs baseline", fixed = TRUE)
  expect_match(cards[[1]]$note, "SSP2-4.5 / 2030-2040", fixed = TRUE)
  expect_false(grepl("Equal-model mean", cards[[1]]$note, fixed = TRUE))
  expect_false(grepl("Analysis unit:", cards[[1]]$note, fixed = TRUE))
  expect_match(cards[[1]]$info, "Years averaged within model; climate models weighted equally", fixed = TRUE)
  total_summary <- tibble::tibble(scenario = "Historical", baseline = 100.4,
    policy = 90.6, value = -9.8, intermod_lo = -9.8, intermod_hi = -9.8,
    n_models = 1L)
  total_summary$baseline <- 100.4
  total_summary$policy <- 90.6
  total_summary$value <- -9.8
  total_cards <- step3_headline_cards(total_summary, method = "total",
    so = list(type = "numeric", name = "welfare", units = "PPP"),
    metric_context = metric_metadata("total",
      list(type = "numeric", name = "welfare", units = "PPP"), weighted = TRUE))
  expect_match(total_cards[[1]]$value, "-10", fixed = TRUE)
  expect_match(total_cards[[1]]$info, "baseline: 100 $ per day", fixed = TRUE)

  # Card 2: Resilience effect (also carries the adverse-year total)
  expect_identical(cards[[2]]$label, "Resilience effect")
  expect_identical(cards[[2]]$value, "Unavailable")

  # Card 3: Program scale & reach
  expect_identical(cards[[3]]$label, "Program reach")
  expect_identical(cards[[3]]$value, "Unavailable")
  expect_match(cards[[3]]$note, "People covered or affected", fixed = TRUE)

  # Card 4: Policy robustness (every model range is above zero)
  expect_identical(cards[[4]]$label, "Policy robustness")
  expect_identical(cards[[4]]$value, "All 4 models agree")
  expect_identical(cards[[4]]$status$text, "Raises mean")
  expect_identical(cards[[4]]$status$kind, "favourable")
  expect_match(cards[[4]]$note, "Model range: +0.32 to +0.58 outcome units", fixed = TRUE)

  # Serializer
  df <- step3_headline_df(cards)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 4L)
  expect_identical(df$label, c("Expected policy effect", "Resilience effect",
                               "Program reach", "Policy robustness"))
})

test_that("Program scale & reach counts units affected by all implemented policies", {
  paired_sum <- tibble::tibble(
    scenario    = c("Historical", "SSP2-4.5 / 2030-2040"),
    value       = c(0.00, 0.45),
    intermod_lo = c(0.00, 0.32),
    intermod_hi = c(0.00, 0.58),
    n_models    = c(1L, 4L),
    n_years     = c(30L, 30L)
  )

  base <- data.frame(
    welfare = c(1.0, 2.0, 3.0, 4.0),
    road_access = c(1, 1, 0, 0),
    weight = c(100, 200, 300, 400),
    hhsize = c(4, 3, 2, 1),
    stringsAsFactors = FALSE
  )
  # Row 1: SP transfer only; row 2: infra lever only; row 3: both; row 4: untouched.
  pol <- base
  pol$.wiseapp_sp_transfer <- c(0.5, 0, 0.25, 0)
  pol$road_access <- c(1, 0, 0, 0)

  cards <- step3_headline_cards(
    paired_summary = paired_sum,
    policy_svy     = pol,
    baseline_svy   = base,
    analysis_unit  = "hh"
  )

  # Touched rows: 1, 2, 3 -> represented population = 100 + 200 + 300 = 600
  expect_identical(cards[[3]]$value, "600")
  expect_equal(cards[[3]]$households_represented,
    100 / 4 + 200 / 3 + 300 / 2)
  expect_identical(cards[[4]]$prediction_count_native, 960)
  expect_identical(cards[[4]]$prediction_count_note, "960 household-years")
  expect_match(cards[[4]]$note, "960 household-years", fixed = TRUE)
  # Counts are provenance: in the basis strip, not the card body.
  expect_no_match(as.character(cards[[4]]$note_html), "household-years", fixed = TRUE)
  expect_match(cards[[4]]$basis_text, "960 household-years", fixed = TRUE)
  expect_identical(step3_headline_df(cards)$prediction_count_native[[4]], 960)
  expect_identical(step3_headline_df(cards)$prediction_sample_rows[[4]], 4)
  expect_match(cards[[3]]$info, "another policy lever", fixed = TRUE)

  # Without a baseline frame, falls back to SP recipients only (rows 1 and 3).
  cards_sp <- step3_headline_cards(
    paired_summary = paired_sum,
    policy_svy     = pol,
    analysis_unit  = "hh"
  )
  expect_identical(cards_sp[[3]]$value, "400")
  expect_equal(cards_sp[[3]]$households_represented, 100 / 4 + 300 / 2)
  expect_match(cards_sp[[3]]$note, "175 households", fixed = TRUE)
  expect_match(cards_sp[[3]]$note, "% of population", fixed = TRUE)

  # No touched units at all -> Unavailable.
  pol_none <- base
  cards_none <- step3_headline_cards(
    paired_summary = paired_sum,
    policy_svy     = pol_none,
    baseline_svy   = base
  )
  expect_identical(cards_none[[3]]$value, "Unavailable")
})

test_that("step3_adverse_dot_data works correctly", {
  thresh <- tibble::tibble(
    scenario      = rep(c("Historical", "SSP2-4.5 / 2030-2040"), each = 8),
    source        = rep(rep(c("Baseline", "Policy"), each = 4), 2),
    Estimate      = rep(c("Central (P50)", "Ensemble 0%", "Ensemble 100%", "Central (P50)"), 4),
    rp_name       = rep(c("1:1", "1:10", "1:10", "1:10"), 4),
    value         = c(3.0, 2.0, 2.0, 2.0, 3.0, 2.0, 2.0, 2.0,
                      3.2, 2.1, 2.1, 2.1, 3.65, 2.4, 2.9, 2.68),
    n_obs         = c(rep(30L, 8), rep(5L, 8)),
    is_historical = rep(c(TRUE, FALSE), each = 8)
  )

  dot_df <- step3_adverse_dot_data(thresh, method = "mean", so = list(type = "numeric", name = "welfare"))
  expect_s3_class(dot_df, "data.frame")
  expect_true(nrow(dot_df) > 0L)
  expect_true(all(c("scenario", "rp_label", "baseline_val", "policy_val", "effect") %in% names(dot_df)))
})

test_that("step3 adverse dot data carries model spread for baseline and policy", {
  # Ensemble rows exist for both sources; the dot data must expose a spread
  # band for each so all future scenarios can show model disagreement.
  thresh <- tibble::tibble(
    scenario      = rep("SSP2-4.5 / 2030-2040", each = 8),
    source        = rep(rep(c("Baseline", "Policy"), each = 4), 1),
    Estimate      = rep(c("Central (P50)", "Ensemble 0%", "Ensemble 100%", "Central (P50)"), 2),
    rp_name       = rep(c("1:1", "1:10", "1:10", "1:10"), 2),
    value         = c(3.2, 2.9, 3.1, 3.2, 3.65, 2.4, 2.9, 2.68),
    n_obs         = c(30L, 5L, 5L, 30L, 30L, 5L, 5L, 30L),
    is_historical = FALSE
  )
  thresh <- dplyr::bind_rows(thresh, tibble::tibble(
    scenario = "Historical", source = "Baseline", Estimate = "Single historical estimate",
    rp_name = "1:1", value = 3.1, n_obs = 30L, is_historical = TRUE
  ))
  dot_df <- step3_adverse_dot_data(thresh, method = "mean", so = list(type = "numeric", name = "welfare"))
  rp10 <- dot_df$rp_name == "1:10" & dot_df$scenario != "Historical"
  expect_true(all(c("base_lo", "base_hi", "policy_lo", "policy_hi") %in% names(dot_df)))
  expect_equal(dot_df$base_lo[rp10], 2.9)
  expect_equal(dot_df$base_hi[rp10], 3.1)
  expect_equal(dot_df$policy_lo[rp10], 2.4)
  expect_equal(dot_df$policy_hi[rp10], 2.9)
  # Historical-free data: no baseline band on the 1:1 row is fine, but the
  # RP with ensemble rows must carry finite bands for both series.
  expect_true(is.finite(dot_df$base_lo[rp10]) && is.finite(dot_df$base_hi[rp10]))
  expect_true(is.finite(dot_df$policy_lo[rp10]) && is.finite(dot_df$policy_hi[rp10]))
})

test_that("Step 3 adverse dot data uses Step 2 periods and historical support", {
  periods <- c("1:1", "1:5", "1:10", "1:20", "1:50")
  labels <- c("Equal-model mean", rep("Equal-model mean", 4))
  tbl <- tibble::tibble(
    scenario = rep(c("Historical", "SSP2-4.5 / 2030-2040"), each = length(periods) * 2),
    source = rep(rep(c("Baseline", "Policy"), each = length(periods)), 2),
    Estimate = rep(labels, 4),
    rp_name = rep(periods, 4),
    value = seq_len(20),
    n_obs = rep(c(30L, 30L, 30L, 30L, 30L), 4),
    is_historical = rep(c(TRUE, FALSE), each = length(periods) * 2)
  )
  dot <- step3_adverse_dot_data(tbl, method = "mean", so = list(type = "numeric", name = "welfare"))
  expect_setequal(as.character(dot$rp_label), c("Expected", "Adverse 1-in-5", "Adverse 1-in-10", "Adverse 1-in-20"))
  expect_false(any(dot$rp_label == "Adverse 1-in-50"))
  expect_equal(dot$effect, dot$policy_val - dot$baseline_val)
  expect_true(all(is.finite(dot$baseline_val) & is.finite(dot$policy_val)))

  tbl$n_obs <- 50L
  dot_50 <- step3_adverse_dot_data(tbl, method = "mean", so = list(type = "numeric", name = "welfare"))
  expect_true(any(dot_50$rp_label == "Adverse 1-in-50"))
})

test_that(".results_pane_ui renders aggregation panel and results sections", {
  so <- list(name = "welfare", type = "numeric", label = "Consumption", level = "hh", units = "$/day")
  ui <- .results_pane_ui(shiny::NS("results3"), so)
  html <- as.character(htmltools::renderTags(ui)$html)

  # Aggregation panel
  expect_match(html, "results-aggregation-panel", fixed = TRUE)
  expect_match(html, "How to summarise consumption across households?", fixed = TRUE)
  expect_match(html, "results3-cmp_agg_method", fixed = TRUE)
  expect_match(html, "results3-cmp_pov_line", fixed = TRUE)
  expect_match(html, "results3-cmp_deviation", fixed = TRUE)

  # Five question-based section cards
  expect_match(html, "How does the policy shift consumption across climate scenarios and weather years?", fixed = TRUE)
  expect_match(html, "Does the policy protect against adverse weather years?", fixed = TRUE)
  expect_match(html, "How does the policy change the probability of severe outcomes?", fixed = TRUE)
  expect_false(grepl("What drives uncertainty, and does the policy reduce outcome variance?", html, fixed = TRUE))
  expect_match(html, "Detailed baseline, policy, and return-period outcomes", fixed = TRUE)

  # Plot and table outputs
  expect_match(html, "results3-annual_distribution_plot", fixed = TRUE)
  expect_match(html, "results3-annual_distribution_type", fixed = TRUE)
  expect_match(html, "Violin", fixed = TRUE)
  expect_match(html, "Boxplot", fixed = TRUE)
  expect_match(html, "results3-adverse_dot_plot", fixed = TRUE)
  expect_match(html, "Climate model spread", fixed = TRUE)
  expect_match(html, "Full ensemble spread", fixed = TRUE)
  expect_match(html, "results3-ensemble_band", fixed = TRUE)
  expect_match(html, "value=\"none\"", fixed = TRUE)
  expect_match(html, "results3-exceedance_plot", fixed = TRUE)
  expect_false(grepl("results3-uncertainty_sources_plot", html, fixed = TRUE))
  expect_match(html, "results3-summary_threshold_table", fixed = TRUE)
  # The threshold table's CSV affordance is a client-side reactable CSV
  # button bound to the table id (guidelines §6); the R-side download
  # handler it replaced is gone.
  expect_match(html, "Reactable.downloadDataCSV", fixed = TRUE)
  expect_match(html, "policy_outcome_thresholds.csv", fixed = TRUE)
  expect_false(grepl("threshold_csv", html, fixed = TRUE))
})

test_that("format_weather_heading_phrase handles scalar, vector, data.frame, and length 12 safely", {
  expect_identical(format_weather_heading_phrase(NULL), "")
  expect_identical(format_weather_heading_phrase("Precipitation"), "precipitation")
  expect_identical(format_weather_heading_phrase(c("Temperature", "Precipitation")), "temperature and precipitation")
  expect_identical(format_weather_heading_phrase("Temperature, Precipitation"), "temperature and precipitation")

  # 3+ variables / 12 months should yield empty string (no awkward 12-variable heading)
  twelve_vars <- paste0("month_", 1:12)
  expect_identical(format_weather_heading_phrase(twelve_vars), "")

  # Comma-separated list of 12 variables
  expect_identical(format_weather_heading_phrase(paste(twelve_vars, collapse = ", ")), "")

  # Data frames (single row, two rows, 12 rows)
  df_one <- data.frame(name = "temp", label = "Temperature", stringsAsFactors = FALSE)
  expect_identical(format_weather_heading_phrase(df_one), "temperature")

  df_two <- data.frame(name = c("temp", "precip"), label = c("Temperature", "Precipitation"), stringsAsFactors = FALSE)
  expect_identical(format_weather_heading_phrase(df_two), "temperature and precipitation")

  df_twelve <- data.frame(name = twelve_vars, label = paste("Month", 1:12), stringsAsFactors = FALSE)
  expect_identical(format_weather_heading_phrase(df_twelve), "")
})

test_that(".results_pane_ui handles tibble so without level and 12-element weather_var without warnings or errors", {
  # tibble without level column - must NOT emit 'Unknown or uninitialised column: level'
  so_tbl <- tibble::tibble(
    name  = "welfare",
    type  = "numeric",
    label = "Welfare",
    units = "$/day"
  )

  # weather_var of length 12 (as from a 12-month simulation)
  twelve_vars <- paste0("month_var_", 1:12)

  # Run without warnings or errors
  expect_no_warning({
    ui <- .results_pane_ui(shiny::NS("results3"), so_tbl, weather_var = twelve_vars)
  })

  html <- as.character(htmltools::renderTags(ui)$html)
  expect_match(html, "How does the policy shift welfare across climate scenarios and weather years?", fixed = TRUE)

  # Also test with 12-row data frame
  df_twelve <- data.frame(name = twelve_vars, label = paste("Month", 1:12), stringsAsFactors = FALSE)
  expect_no_warning({
    ui_df <- .results_pane_ui(shiny::NS("results3"), so_tbl, weather_var = df_twelve)
  })
  html_df <- as.character(htmltools::renderTags(ui_df)$html)
  expect_match(html_df, "How does the policy shift welfare across climate scenarios and weather years?", fixed = TRUE)
})

test_that("step3_offset_share reports the share of climate loss offset by the policy", {
  # Poverty rate (lower is better): climate +2 pp, policy -1 pp.
  expect_equal(step3_offset_share(0.30, 0.32, 0.31, "lower_is_better"), 0.5)
  # Policy more than offsets the climate loss.
  expect_equal(step3_offset_share(0.30, 0.32, 0.28, "lower_is_better"), 2)
  # Higher is better: climate -2, policy +1.
  expect_equal(step3_offset_share(10, 8, 9, "higher_is_better"), 0.5)
  # Climate improves the metric, the policy adds to the loss, or direction unknown.
  expect_true(is.na(step3_offset_share(0.30, 0.28, 0.27, "lower_is_better")))
  expect_true(is.na(step3_offset_share(0.30, 0.32, 0.33, "lower_is_better")))
  expect_true(is.na(step3_offset_share(0.30, 0.32, 0.28, "unknown")))
  expect_true(is.na(step3_offset_share(NA_real_, 0.32, 0.28, "lower_is_better")))
})

test_that("expected policy effect card states the offset share when climate worsens the metric", {
  summary <- tibble::tibble(
    scenario = c("Historical", "SSP2-4.5 / 2030-2040"),
    baseline = c(0.30, 0.32), policy = c(0.30, 0.28), value = c(0, -0.04),
    intermod_lo = c(0, -0.04), intermod_hi = c(0, -0.04), n_models = c(1L, 2L)
  )
  spec <- metric_metadata("headcount_ratio",
    list(name = "welfare", type = "numeric", units = "PPP"),
    pov_line = 3, weighted = TRUE, analysis_unit = "hh")
  cards <- step3_headline_cards(summary, method = "headcount_ratio",
    so = list(name = "welfare", type = "numeric", units = "PPP"), metric_context = spec)
  expect_equal(cards[[1]]$offset_share, 2)
  expect_match(cards[[1]]$note, "more than offsets climate change (2.0×)", fixed = TRUE)
  expect_match(cards[[1]]$info, "Offset share", fixed = TRUE)
})

test_that("Step 3 first two cards translate rate changes into the number of poor", {
  summary <- tibble::tibble(
    scenario = c("Historical", "SSP2-4.5 / 2030-2040"),
    baseline = c(0.30, 0.32), policy = c(0.30, 0.28), value = c(0, -0.04),
    intermod_lo = c(0, -0.04), intermod_hi = c(0, -0.04), n_models = c(1L, 2L)
  )
  metric <- list(
    status = "ok", reason = NULL,
    metadata = list(method = "headcount_ratio", label = "Poverty rate", format = "percent",
      display_multiplier = 100, change_unit = "pp", level_unit = "percent",
      repositioning_modeled = TRUE, interaction_included = TRUE),
    scenarios = list("SSP2-4.5 / 2030-2040" = list(status = "ok", summary = data.frame(
      scenario = "SSP2-4.5 / 2030-2040", baseline = .32, after_main = .30,
      after_repositioning = .29, policy = .28, main = -.02,
      repositioning = -.01, interaction = -.01, resilience = -.02, total = -.04))),
    return_period = data.frame(scenario = "SSP2-4.5 / 2030-2040", return_period = 20,
      scope = "baseline_anchored", status = "ok", total = -.08,
      main = -.02, resilience = -.06, repositioning = -.01, interaction = -.05),
    mechanisms = list(repositioning_status = "modeled", interaction_status = "included")
  )
  svy <- data.frame(weight = rep(1000, 100))
  cards <- step3_headline_cards(summary, method = "headcount_ratio",
    metric_decomposition = metric, baseline_svy = svy)
  # 100,000 people: -4 pp = 4.0K fewer poor; resilience -6 pp = 6.0K fewer poor.
  expect_match(cards[[1]]$note, "\u2248 4.0K fewer poor", fixed = TRUE)
  expect_match(cards[[2]]$note, "\u2248 6.0K fewer poor", fixed = TRUE)
  # Not for other metrics, and not without weights.
  mean_cards <- step3_headline_cards(summary, method = "mean", baseline_svy = svy)
  expect_false(grepl("poor", mean_cards[[1]]$note, fixed = TRUE))
  none <- step3_headline_cards(summary, method = "headcount_ratio", metric_decomposition = metric)
  expect_false(grepl("poor", none[[1]]$note, fixed = TRUE))
})

test_that("poor_change_text formats and suppresses sensibly", {
  expect_identical(poor_change_text(0.04, 50000), "\u2248 2.0K more poor")
  expect_identical(poor_change_text(-0.0123, 8e6), "\u2248 98.4K fewer poor")
  expect_null(poor_change_text(0.04, NA_real_))
  expect_null(poor_change_text(NA_real_, 1000))
  expect_null(poor_change_text(1e-9, 1000))
})

test_that("Step 3 expected effect card prints the 95% CI from the paired coefficient SD", {
  summary <- tibble::tibble(
    scenario = c("Historical", "SSP2-4.5 / 2030-2040"),
    baseline = c(0.30, 0.32), policy = c(0.30, 0.28), value = c(0, -0.04),
    intermod_lo = c(0, -0.04), intermod_hi = c(0, -0.04), n_models = c(1L, 2L),
    coef_sd = c(0, 0.01)
  )
  spec <- metric_metadata("headcount_ratio",
    list(name = "welfare", type = "numeric", units = "PPP"),
    pov_line = 3, weighted = TRUE, analysis_unit = "hh")
  cards <- step3_headline_cards(summary, method = "headcount_ratio",
    so = list(name = "welfare", type = "numeric", units = "PPP"), metric_context = spec)
  # -4.00 pp +/- 1.96 * 1.00 pp.
  expect_match(cards[[1]]$note, "(95% CI: -5.96 to -2.04 pp)", fixed = TRUE)
  expect_equal(cards[[1]]$effect_ci_native, -0.04 + c(-1, 1) * stats::qnorm(0.975) * 0.01)
  expect_match(cards[[1]]$info, "weather coefficients only", fixed = TRUE)
  # Without a coefficient SD the generic line stays.
  summary$coef_sd <- NULL
  plain <- step3_headline_cards(summary, method = "headcount_ratio",
    so = list(name = "welfare", type = "numeric", units = "PPP"), metric_context = spec)
  expect_match(plain[[1]]$note, "Policy vs baseline", fixed = TRUE)
  expect_false(grepl("95% CI", plain[[1]]$note, fixed = TRUE))
})
