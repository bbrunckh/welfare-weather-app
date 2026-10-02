library(testthat)

test_that("selected binary outcome does not gain direction from its variable name", {
  for (name in c("poor", "arbitrary_indicator")) {
    outcome <- build_selected_outcome(data.frame(name = name, type = "logical", units = ""))
    expect_identical(outcome$direction, "unknown")
    expect_false(metric_metadata("mean", outcome)$direction_known)
  }
})

test_that("export metadata consumes one effective context and preserves empty exports", {
  context <- metric_metadata("headcount_ratio", list(name = "welfare", units = "PPP"),
    pov_line = 7, analysis_unit = "hh", weighted = TRUE)
  export <- annotate_visualization_export(data.frame(value = .2), "headcount_ratio",
    observation_unit = "annual", aggregation_order = "matched", context = context)
  expect_equal(export$threshold_value, 7)
  expect_identical(export$analysis_unit, "hh")
  expect_equal(nrow(annotate_visualization_export(data.frame(value = numeric(0)),
    observation_unit = "annual", aggregation_order = "matched", context = context)), 0)
  expect_match(metric_axis_label("headcount_ratio", deviation = "mean"), "pp", fixed = TRUE)
  expect_identical(metric_metadata("mean", list(type = "binary", direction = "lower_is_better"))$direction,
    "lower_is_better")
  expect_match(metric_metadata("mean", list(units = "score"))$level_unit, "score", fixed = TRUE)
})

test_that("metric metadata carries direction and adverse tail", {
  low <- wiseapp:::metric_metadata("headcount_ratio")
  high <- wiseapp:::metric_metadata("mean")
  expect_identical(low$direction, "lower_is_better")
  expect_identical(low$adverse_tail, "high")
  expect_identical(high$direction, "higher_is_better")
  expect_identical(high$adverse_tail, "low")
})

test_that("selected built-in metric direction overrides stale outcome metadata", {
  spec <- wiseapp:::metric_metadata(
    "headcount_ratio",
    so = list(direction = "higher_is_better")
  )
  expect_identical(spec$direction, "lower_is_better")
  expect_identical(spec$adverse_tail, "high")
})

test_that("metric metadata covers native units, changes, and thresholds", {
  expected <- c(
    mean = "outcome units", median = "outcome units",
    total = "outcome units", headcount_ratio = "fraction",
    gap = "fraction", fgt2 = "fraction",
    gini = "native Gini index", prosperity_gap = "ratio",
    avg_poverty = "inverse outcome units"
  )
  specs <- lapply(names(expected), wiseapp:::metric_metadata)
  names(specs) <- names(expected)
  expect_identical(vapply(specs, `[[`, character(1), "native_unit"), expected)
  expect_true(all(vapply(specs[c("headcount_ratio", "gap", "fgt2")],
                         `[[`, logical(1), "uses_poverty_line")))
  expect_false(specs$prosperity_gap$uses_poverty_line)
  expect_true(specs$prosperity_gap$poverty_line)
  expect_equal(specs$prosperity_gap$threshold_value, 28)
  expect_identical(specs$headcount_ratio$change_unit, "pp")
  expect_identical(specs$gap$change_unit, "pp")
  expect_identical(specs$fgt2$change_unit, "pp")
  expect_identical(specs$gini$change_unit, "index points")
  expect_match(specs$avg_poverty$caveat, "positive-welfare", fixed = TRUE)
})

test_that("metadata only claims the confirmed PPP or LCU basis", {
  ppp <- wiseapp:::metric_metadata(
    "mean", list(name = "welfare", label = "Welfare", units = "PPP", type = "numeric"),
    analysis_unit = "hh"
  )
  lcu <- wiseapp:::metric_metadata(
    "median", list(name = "welfare", label = "Welfare", units = "LCU", type = "numeric")
  )
  unknown <- wiseapp:::metric_metadata(
    "mean", list(name = "consumption", label = "Consumption", units = "", type = "numeric")
  )
  expect_identical(ppp$currency_basis, "PPP 2021")
  expect_identical(lcu$currency_basis, "LCU 2021")
  expect_identical(ppp$level_unit, "$ per day")
  expect_identical(lcu$level_unit, "LCU per day")
  expect_identical(ppp$level_unit, "$ per day")
  expect_true("welfare/time basis unavailable" %in% ppp$missing_context)
  expect_true(any(grepl("currency/unit basis unavailable", unknown$missing_context)))
  expect_identical(ppp$analysis_unit, "hh")
})

test_that("population weighted sum and average-poverty units are concise", {
  total <- wiseapp:::metric_metadata(
    "total",
    list(name = "welfare", label = "Welfare", units = "PPP", type = "numeric"),
    analysis_unit = "hh", weighted = TRUE
  )
  total_unknown <- wiseapp:::metric_metadata("total", weighted = NULL)
  inverse <- wiseapp:::metric_metadata("avg_poverty", weighted = TRUE)
  expect_identical(total$label, "Population weighted sum")
  expect_identical(total$level_unit, "$ per day")
  expect_match(total$weight_interpretation, "expansion semantics unknown", fixed = TRUE)
  expect_identical(total_unknown$level_unit, "outcome units")
  expect_identical(metric_axis_label("total", list(units = "LCU")),
    "Population weighted sum (LCU per day)")
  expect_identical(metric_axis_label("total", list(units = "PPP"), deviation = "mean"),
    "Population weighted sum - deviation from mean year ($ per day)")
  expect_identical(wiseapp:::metric_metadata("avg_poverty", list(units = "PPP"))$level_unit,
    "days per $")
  expect_identical(wiseapp:::metric_metadata("avg_poverty", list(units = "LCU"))$level_unit,
    "days per LCU")
  expect_identical(metric_axis_label("prosperity_gap"), "Prosperity gap")
  expect_identical(wiseapp:::metric_metadata("prosperity_gap")$native_unit, "ratio")
  expect_identical(wiseapp:::metric_metadata("prosperity_gap")$level_unit, "")
  expect_identical(wiseapp:::format_metric_value(1.2,
    wiseapp:::metric_metadata("prosperity_gap")), "1.20")
  expect_identical(inverse$level_unit, "inverse outcome units")
  expect_true(any(grepl("positive welfare", inverse$missing_context, fixed = TRUE)))
})

test_that("binary means use percent and pp without inferring direction", {
  meta <- wiseapp:::metric_metadata(
    "mean", list(name = "employed", label = "Employment", type = "binary", units = "")
  )
  expect_identical(meta$format, "percent")
  expect_identical(meta$change_unit, "pp")
  expect_identical(meta$direction, "unknown")
  expect_false(meta$direction_known)
  expect_identical(meta$adverse_tail, "low")
  expect_match(meta$adverse_note, "Direction unknown", fixed = TRUE)
  expect_match(wiseapp:::metric_context_note(meta), "without a benefit claim", fixed = TRUE)
})

test_that("metric formatter applies scale once and shows change units", {
  rate <- wiseapp:::metric_metadata("headcount_ratio", pov_line = 3)
  welfare <- wiseapp:::metric_metadata(
    "mean", list(name = "welfare", label = "Welfare", units = "PPP", type = "numeric")
  )
  gini <- wiseapp:::metric_metadata("gini")
  expect_identical(wiseapp:::format_metric_value(0.32, rate), "32.00%")
  expect_identical(wiseapp:::format_metric_value(-0.04, rate, change = TRUE), "-4.00 pp")
  expect_identical(wiseapp:::format_metric_value(0.32, welfare),
                   "0.32 $ per day")
  expect_identical(wiseapp:::format_metric_value(0.02, welfare, change = TRUE),
                   "+0.02 $ per day")
  expect_identical(wiseapp:::format_metric_value(0.02, gini, change = TRUE),
                   "+0.02 index points")
  total <- metric_metadata("total", list(type = "numeric", units = "PPP"), weighted = TRUE)
  expect_identical(wiseapp:::format_metric_value(1234.6, total, digits = 0), "1,235 $ per day")
  expect_identical(wiseapp:::format_metric_value(-1234.6, total, change = TRUE, digits = 0), "-1,235 $ per day")
  expect_identical(wiseapp:::format_metric_value(NA_real_, rate), "Not available")
})

test_that("metric context identifies outcome, threshold, and missing basis", {
  meta <- wiseapp:::metric_metadata(
    "headcount_ratio",
    list(name = "welfare", label = "Welfare", units = "PPP", type = "numeric"),
    pov_line = 3, analysis_unit = "hh", weighted = TRUE
  )
  note <- wiseapp:::metric_context_note(meta)
  expect_match(note, "Welfare: Poverty rate", fixed = TRUE)
  expect_match(note, "Poverty line 3", fixed = TRUE)
  expect_match(note, "Analysis unit: hh", fixed = TRUE)
  expect_match(note, "poverty-line welfare/time compatibility unavailable", fixed = TRUE)
})

test_that("metric export context arguments are optional and appended", {
  legacy <- wiseapp:::annotate_visualization_export(
    data.frame(value = 1), "mean", NULL, "annual", "weighted", "none"
  )
  extended <- wiseapp:::annotate_visualization_export(
    data.frame(value = 1), method = "prosperity_gap",
    observation_unit = "annual", aggregation_order = "weighted",
    pov_line = 3, analysis_unit = "hh", weighted = TRUE,
    context = "Focus scenario"
  )
  expect_true(is.logical(legacy$poverty_line))
  expect_false(extended$uses_poverty_line)
  expect_equal(extended$threshold_value, 28)
  expect_match(extended$metric_context, "Focus scenario", fixed = TRUE)
})

test_that("return-period interpolation selects the adverse tail", {
  vals <- matrix(seq_len(20), nrow = 1)
  sds <- matrix(0, nrow = 1, ncol = 20)
  high <- wiseapp:::by_model_rp_matrix(vals, sds, c(`1:20` = 0.05), "high")$rp[1, 1]
  low <- wiseapp:::by_model_rp_matrix(vals, sds, c(`1:20` = 0.05), "low")$rp[1, 1]
  expect_equal(high, 19.5)
  expect_equal(low, 1.5)
})

test_that("paired effects are zero for identical baseline and policy tables", {
  tbl <- tibble::tibble(
    sim_year = c(2020L, 2021L),
    model_id = list(c("m1"), c("m1")),
    value_all = list(1, 2),
    value_all_sd = list(0.1, 0.1),
    F_agg_all = list(matrix(1, nrow = 1L), matrix(1, nrow = 1L))
  )
  effects <- wiseapp:::paired_model_year_effects(tbl, tbl)
  expect_true(all(effects$effect == 0))
  expect_true(all(effects$effect_sd == 0))
  summary <- wiseapp:::paired_effect_summary(effects, scenario = "SSP2-4.5 / 2030-2040")
  expect_equal(summary$value, 0)
  expect_equal(summary$coef_lo, 0)
  expect_equal(summary$coef_hi, 0)
})

test_that("paired adverse effects use equal-probability arm quantiles", {
  effects <- tibble::tibble(
    model_id = rep(c("m1", "m2"), each = 4L),
    sim_year = rep(2018:2021, 2L),
    baseline = rep(c(1, 2, 3, 4), 2L),
    policy = rep(c(2, 3, 4, 5), 2L),
    effect = 1,
    effect_sd = 0
  )
  out <- wiseapp:::paired_equal_probability_effects(
    effects, c(`1-in-10` = 0.1, `1-in-5` = 0.2)
  )
  expect_true(all(abs(out$effect - 1) < 1e-12))
  expect_equal(sort(unique(out$probability)), c(0.1, 0.2))
})

test_that("variance display uses separate aligned bars", {
  p <- wiseapp:::plot_variance_contribution(tibble::tibble(
    scenario = "SSP2-4.5 / 2030-2040", var_coef = 1,
    var_within = 4, var_across = 9, is_historical = FALSE
  ))
  expect_s3_class(p, "ggplot")
  expect_true(any(vapply(p$layers, function(x) inherits(x$position, "PositionDodge"),
                         logical(1L))))
})

test_that("visualization exports carry metric and observation metadata", {
  out <- wiseapp:::annotate_visualization_export(
    data.frame(value = 1:2),
    method = "headcount_ratio",
    observation_unit = "annual aggregate",
    aggregation_order = "weighted household aggregate by model and year",
    uncertainty = "inter-model spread"
  )
  expect_identical(out$metric_id, c("headcount_ratio", "headcount_ratio"))
  expect_identical(out$direction, c("lower_is_better", "lower_is_better"))
  expect_identical(out$adverse_tail, c("high", "high"))
  expect_identical(out$observation_unit, rep("annual aggregate", 2L))
  expect_identical(out$uncertainty_type, rep("inter-model spread", 2L))
})

test_that("weather support export preserves source and variable", {
  survey <- data.frame(
    loc_id = c("a", "a"), int_month = c(1L, 1L),
    timestamp = as.Date(c("2020-01-01", "2021-01-01"))
  )
  weather <- data.frame(
    loc_id = c("a", "a"), int_month = c(1L, 1L),
    timestamp = as.Date(c("2020-01-01", "2021-01-01")),
    temp = c(10, 20)
  )
  out <- wiseapp:::weather_density_data(
    survey, weather, "temp",
    scenario_weather = list(`SSP2 / 2030` = transform(weather, temp = temp + 1))
  )
  expect_true(all(c("weather_variable", "source", "value") %in% names(out)))
  expect_setequal(unique(out$source), c("Model support", "Full historical archive", "SSP2 / 2030"))
  expect_true(all(is.finite(out$value)))
})

test_that("weighted baseline deciles are fixed and cover supported households", {
  svy <- data.frame(welfare = 1:10, weight = c(rep(1, 9), 9))
  d <- wiseapp:::weighted_baseline_deciles(svy, "welfare", "weight")
  expect_equal(d[[1]], 1L)
  expect_equal(d[[10]], 10L)
  expect_true(all(d >= 1L & d <= 10L))
})

test_that("paired adverse table includes expected supported periods", {
  x <- tibble::tibble(
    model_id = "m1", baseline = 1:20, policy = 2:21,
    effect = 1, effect_sd = 0
  )
  out <- wiseapp:::paired_adverse_effect_table(x, "mean")
  expect_setequal(out$period, c("Expected", "Adverse 1-in-5", "Adverse 1-in-10", "Adverse 1-in-20"))
  expect_true(all(abs(out$effect - 1) < 1e-12))
})

test_that("unsupported adverse return periods are omitted", {
  x <- tibble::tibble(
    model_id = "m1", baseline = 1:10, policy = 2:11,
    effect = 1, effect_sd = 0
  )
  out <- wiseapp:::paired_adverse_effect_table(x, "mean")
  expect_false("Adverse 1-in-20" %in% out$period)
})

test_that("variance shares are opt-in and explicitly approximate", {
  x <- data.frame(scenario = "SSP2 / 2030", var_coef = 1,
                  var_within = 4, var_across = 9)
  hidden <- wiseapp:::variance_component_data(x, FALSE)
  shown <- wiseapp:::variance_component_data(x, TRUE)
  expect_true(all(is.na(hidden$share_approx)))
  expect_true(all(is.finite(shown$share_approx)))
  expect_true(all(grepl("zero-covariance", shown$share_warning)))
})

test_that("weather support uses robust interval and warning share", {
  ref <- data.frame(temp = 1:100)
  sc <- list(`SSP2 / 2030` = data.frame(temp = c(rep(1, 90), rep(1000, 10))))
  out <- wiseapp:::weather_support_summary(ref, sc, "temp")
  expect_equal(out$n_reference, 100)
  expect_true(out$warning)
  expect_equal(out$warning_rule, "Robust 1%-99% reference interval; warn above 5% outside")
})

test_that("policy covariate support flags range and rare categories", {
  train <- data.frame(x = 1:10, sector = rep(c("a", "b"), 5))
  policy <- data.frame(x = c(1, 20), sector = c("a", "new"))
  out <- wiseapp:::policy_covariate_support(train, policy)
  expect_true(out$warning[out$variable == "x"])
  expect_true(out$warning[out$variable == "sector"])
})

test_that("policy covariate support tolerates unmatched and missing categories", {
  train <- data.frame(sector = factor(c("a", "b", "a")), x = c(1, 2, NA))
  policy <- data.frame(sector = c("a", NA, ""), x = c(1, 3, NA))
  out <- wiseapp:::policy_covariate_support(
    train, policy, vars = c("sector", "x", "missing_from_both")
  )

  expect_setequal(out$variable, c("sector", "x"))
  expect_true(out$warning[out$variable == "sector"])
  expect_true(out$warning[out$variable == "x"])
})

test_that("log effects convert to percent without losing model-scale additivity", {
  level <- 0.10
  resilience <- -0.04
  total <- level + resilience
  expect_equal(wiseapp:::log_effect_to_percent(total),
               100 * (exp(total) - 1))
  expect_equal(wiseapp:::percent_to_log_effect(
    wiseapp:::log_effect_to_percent(total)), total)
  expect_false(isTRUE(all.equal(
    wiseapp:::log_effect_to_percent(level) +
      wiseapp:::log_effect_to_percent(resilience),
    wiseapp:::log_effect_to_percent(total)
  )))
})

test_that("model robustness summarizes one point per model", {
  x <- data.frame(
    scenario = rep("SSP2 / 2030", 6), model_id = rep(c("a", "b"), each = 3),
    sim_year = rep(1:3, 2), value = c(1, 2, 3, 2, 3, 4)
  )
  out <- wiseapp:::model_robustness_data(x)
  expect_equal(nrow(out), 2L)
  expect_equal(unique(out$center), 2.5)
  expect_true(all(out$n_weather_years == 3L))
})

test_that("decision return periods follow metric adverse direction", {
  high <- wiseapp:::metric_decision_return_periods("mean")
  low <- wiseapp:::metric_decision_return_periods("headcount_ratio")
  expect_identical(unname(high[["Adverse 1-in-10"]]), "1:10")
  expect_identical(unname(low[["Adverse 1-in-10"]]), "1:10")
})

test_that("adverse threshold interpolation follows metric tail direction", {
  vals <- matrix(seq_len(20), nrow = 1L)
  sds <- matrix(0, nrow = 1L, ncol = 20L)
  rp <- c("1:20" = 0.05)

  high <- wiseapp:::by_model_rp_matrix(vals, sds, rp, adverse_tail = "high")$rp[1, 1]
  low  <- wiseapp:::by_model_rp_matrix(vals, sds, rp, adverse_tail = "low")$rp[1, 1]

  expect_gt(high, low)
  expect_equal(high, 19.5)
  expect_equal(low, 1.5)
})

test_that("step2 adverse dot data extracts supported periods and ensemble bounds", {
  tbl <- data.frame(
    scenario = c("Historical", "Historical", "SSP2-4.5 / 2030", "SSP2-4.5 / 2030", "SSP2-4.5 / 2030"),
    Estimate = c("Central (P50)", "Central (P50)", "Central (P50)", "Ensemble 0%", "Ensemble 100%"),
    rp_name = c("1:1", "9:10", "1:1", "1:1", "1:1"),
    value = c(10, 8, 12, 11, 14),
    is_historical = c(TRUE, TRUE, FALSE, FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  out <- wiseapp:::step2_adverse_dot_data(tbl, method = "mean")
  expect_true(nrow(out) >= 1)
  expect_true("rp_label" %in% names(out))
  fut <- out[!out$is_historical, ]
  if (nrow(fut)) {
    expect_equal(fut$intermod_lo[[1]], 11)
    expect_equal(fut$intermod_hi[[1]], 14)
  }
})

test_that("new export keys return valid figures or data frames", {
  # 1. climate_adverse_return_periods
  tbl <- data.frame(
    scenario = "SSP2 / 2030", Estimate = "Central (P50)",
    rp_name = "1:1", value = 10, is_historical = FALSE, stringsAsFactors = FALSE
  )
  dot_data <- wiseapp:::step2_adverse_dot_data(tbl, "mean")
  p_dot <- wiseapp:::plot_step2_adverse_dot(dot_data)
  expect_s3_class(p_dot, "ggplot")

  # 2. policy_distributional_incidence
  inc <- data.frame(decile = 1:10, effect = rep(1, 10))
  p_inc <- wiseapp:::plot_incidence_by_decile(inc)
  expect_s3_class(p_inc, "ggplot")

  # 4. policy_construction_summary & treatment_matrix & covariate_support
  df1 <- data.frame(welfare = 1:5, x = 1:5)
  df2 <- data.frame(welfare = 2:6, x = 2:6)
  expect_s3_class(wiseapp:::policy_construction_summary(df1, df2), "data.frame")
  expect_s3_class(wiseapp:::policy_treatment_matrix(df1, df2), "data.frame")
  expect_s3_class(wiseapp:::policy_covariate_support(df1, df2), "data.frame")
})

test_that("treatment diagnostics distinguish eligibility from realized treatment", {
  baseline <- data.frame(weight = c(1, 1, 1, 1))
  policy <- data.frame(
    weight = baseline$weight,
    .sp_transfer = c(0, 10, 0, 10)
  )
  eligible <- c(TRUE, FALSE, TRUE, FALSE)

  out <- wiseapp:::policy_treatment_matrix(baseline, policy, eligible)
  expect_identical(
    out$status,
    c(
      "Not eligible, not treated",
      "Inclusion error: not eligible, treated",
      "Exclusion error: eligible, not treated",
      "Eligible and treated"
    )
  )

  no_sp <- wiseapp:::policy_treatment_matrix(
    baseline,
    transform(policy, .wiseapp_sp_transfer = rep(0, 4)),
    eligible
  )
  expect_equal(no_sp$n, c(4L, 0L, 0L, 0L))

  note <- wiseapp:::.policy_treatment_explanation(list(
    targeting = "exante_poor", targeting_threshold = 20,
    inclusion_error_pct = 10, exclusion_error_pct = 5
  ))
  expect_match(note, "bottom 20%", fixed = TRUE)
  expect_match(note, "10% inclusion error", fixed = TRUE)
  expect_match(note, "5% exclusion error", fixed = TRUE)
  expect_match(note, "counterfactual assignment", fixed = TRUE)
})

test_that("selected eligibility omits inclusion and exclusion errors", {
  svy <- data.frame(welfare = 1:10)
  sp <- list(
    targeting = "exante_poor", targeting_threshold = 20,
    inclusion_error_pct = 100, exclusion_error_pct = 100
  )

  ideal <- wiseapp:::.determine_sp_eligibility(svy, sp, apply_errors = FALSE)
  realized <- withr::with_seed(
    1L, wiseapp:::.determine_sp_eligibility(svy, sp, apply_errors = TRUE)
  )
  expect_equal(sum(ideal), 2L)
  expect_equal(sum(realized), 8L)
})
