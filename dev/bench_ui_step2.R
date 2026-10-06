# Development-only Step 2 UI-migration benchmark (Batch 2).
#
# Measures the R-side builder cost old (ggplot/DT) vs new (echarts4r/reactable)
# for representative migrated Step 2 surfaces:
#   - exceedance curves           (echart_exceedance vs enhance_exceedance)
#   - annual outcome distribution (echart_annual_distribution vs
#                                   plot_annual_distribution, violin + boxplot)
#   - weather-density ridgeline   (echart_weather_density_panel vs
#                                   plot_weather_density_panel, 3 variables)
#   - threshold table widget      (.step2_reactable)
# Payloads are synthetic frames shaped like the module reactives. Statistics
# are identical by construction in each pair, so the comparison isolates the
# rendering layer (guidelines §7 / §9). Medians of 10 iterations via
# bench::mark (memory = FALSE: htmlwidget assembly trips bench's memory
# profiler on this platform, as in dev/bench_surveystats.R).
#
# Usage:
#   Rscript dev/bench_ui_step2.R

options(golem.app.prod = FALSE)
devtools::load_all(quiet = TRUE)

set.seed(21)

fmt_bench <- function(bm) {
  bm$expression <- vapply(bm$expression, function(e) deparse(e)[1], character(1))
  bm[, c("expression", "min", "median", "itr/sec", "n_itr")]
}

print_pair <- function(label, bm) {
  cat("\n==", label, "==\n")
  print(fmt_bench(bm), row.names = FALSE)
}

# ---------------------------------------------------------------------------
# Payload 1: exceedance curves (enhance_exceedance-shaped; 3 scenarios x 30
# ranks x 5 climate models, no source column - the Step 2 shape)
# ---------------------------------------------------------------------------

n_models <- 5L
n_ranks <- 30L
scen <- c("Historical", "SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2040-2050")
ex_curves <- do.call(rbind, lapply(scen, function(s) {
  is_hist <- s == "Historical"
  do.call(rbind, lapply(seq_len(n_ranks), function(r) {
    data.frame(
      scenario = s,
      model_id = paste0("m", seq_len(n_models)),
      rank = r,
      exceed_prob = 1 - r / (n_ranks + 1),
      welfare_val = 10 + r * 0.15 + rnorm(n_models, 0, 0.4),
      coef_sd = runif(n_models, 0.02, 0.08),
      is_historical = is_hist,
      stringsAsFactors = FALSE
    )
  }))
}))

print_pair(
  "exceedance: ggplot enhance_exceedance",
  bench::mark(
    old = enhance_exceedance(
      curves_tbl = ex_curves, x_label = "Mean welfare ($/day, 2021 PPP)",
      return_period = TRUE, n_sim_years = 30L, logit_x = TRUE,
      band_q = NULL, ensemble_band_q = c(lo = 0.25, hi = 0.75)
    ),
    new = echart_exceedance(
      curves_tbl = ex_curves, x_label = "Mean welfare ($/day, 2021 PPP)",
      n_sim_years = 30L, logit_x = TRUE,
      band_q = NULL, ensemble_band_q = c(lo = 0.25, hi = 0.75)
    ),
    iterations = 10L, memory = FALSE, check = FALSE
  )
)

# ---------------------------------------------------------------------------
# Payload 2: annual outcome distribution (plot_annual_distribution-shaped;
# 2 scenarios x 3 models x 30 weather-year draws)
# ---------------------------------------------------------------------------

n_years <- 30L
ann_curves <- do.call(rbind, lapply(scen[1:2], function(s) {
  data.frame(
    scenario = s,
    model_id = paste0("m", rep(seq_len(3L), each = n_years)),
    sim_year = rep(2020:2021, times = 15L),
    value = rnorm(3L * n_years, 12 + ifelse(s == "Historical", 0, 0.4), 1.1),
    is_historical = s == "Historical",
    stringsAsFactors = FALSE
  )
}))

print_pair(
  "annual distribution (violin): ggplot vs echarts",
  bench::mark(
    old = plot_annual_distribution(ann_curves, x_label = "Mean welfare",
                                   plot_type = "violin"),
    new = echart_annual_distribution(ann_curves, x_label = "Mean welfare",
                                     plot_type = "violin", height = "470px"),
    iterations = 10L, memory = FALSE, check = FALSE
  )
)

print_pair(
  "annual distribution (boxplot): ggplot vs echarts",
  bench::mark(
    old = plot_annual_distribution(ann_curves, x_label = "Mean welfare",
                                   plot_type = "boxplot"),
    new = echart_annual_distribution(ann_curves, x_label = "Mean welfare",
                                     plot_type = "boxplot", height = "470px"),
    iterations = 10L, memory = FALSE, check = FALSE
  )
)

# ---------------------------------------------------------------------------
# Payload 3: weather-density ridgeline (plot_weather_density_panel-shaped;
# 3 continuous variables, 5k historical values, 2 scenarios)
# ---------------------------------------------------------------------------

n_obs <- 5000L
n_locs <- 500L
survey_weather <- data.frame(
  loc_id = paste0("l", rep(seq_len(n_locs), each = n_years)),
  int_month = rep(seq_len(12L), length.out = n_locs * n_years),
  timestamp = as.Date(paste0("2020-", rep(seq_len(12L), length.out = n_locs * n_years),
    "-15"
  ), format = "%Y-%m-%d"),
  stringsAsFactors = FALSE
)
weather_vars <- c("temperature", "precipitation", "wind_speed")
weather_raw <- do.call(cbind, lapply(weather_vars, function(v) {
  data.frame(rnorm(n_obs, if (v == "temperature") 25 else if (v == "precipitation") 40 else 3, 2))
}))
names(weather_raw) <- weather_vars
weather_raw$loc_id <- paste0("l", rep(seq_len(n_locs), each = n_obs / n_locs))
weather_raw$int_month <- rep(seq_len(12L), length.out = n_obs)
weather_raw$timestamp <- as.Date(paste0(
  rep(1990:2020, length.out = n_obs), "-",
  rep(seq_len(12L), length.out = n_obs), "-15"
), format = "%Y-%m-%d")

diag_scenarios <- list(
  `SSP2-4.5 / 2030-2040` = transform(weather_raw,
    temperature = temperature + 1.2, precipitation = precipitation * 1.1),
  `SSP5-8.5 / 2040-2050` = transform(weather_raw,
    temperature = temperature + 2.4, precipitation = precipitation * 0.9)
)

print_pair(
  "weather density ridgeline (3 vars): ggplot patchwork vs one echarts widget",
  bench::mark(
    old = plot_weather_density_panel(
      survey_weather = survey_weather, weather_raw = weather_raw,
      weather_vars = weather_vars, scenario_weather = diag_scenarios,
      log_x = rep(FALSE, 3L), show_regression = TRUE
    ),
    new = echart_weather_density_panel(
      survey_weather = survey_weather, weather_raw = weather_raw,
      weather_vars = weather_vars, scenario_weather = diag_scenarios,
      log_x = rep(FALSE, 3L), show_regression = TRUE
    ),
    iterations = 10L, memory = FALSE, check = FALSE
  )
)

# ---------------------------------------------------------------------------
# Payload 4: threshold table widget (build_threshold_table_df-shaped; 3
# scenarios x 7 estimate rows x 5 RP columns)
# ---------------------------------------------------------------------------

thr_tbl <- build_threshold_table_df(do.call(rbind, lapply(scen, function(s) {
  do.call(rbind, lapply(
    c("Pooled P05", "Coef P10", "Ensemble P25", "Central (P50)",
      "Ensemble P75", "Coef P90", "Pooled P95"),
    function(est) {
      data.frame(
        scenario = s,
        Estimate = est,
        rp_name = rep(c("1:20", "1:10", "1:1", "9:10", "19:20"), each = 1L),
        rp_label = c("1:20", "1:10", "1:1", "9:10", "19:20"),
        value = 12 + rnorm(1, 0, 0.2) + 0.1 * seq_len(5L),
        n_obs = 30L,
        is_historical = s == "Historical",
        stringsAsFactors = FALSE
      )
    }
  ))
})), group_order = "scenario_x_year", show_coef = TRUE, adverse_only = FALSE,
method = "mean")

cat("\n== threshold table: .step2_reactable ==\n")
print(bench::mark(
  reactable = .step2_reactable(thr_tbl),
  iterations = 10L, memory = FALSE, check = FALSE
))

cat("\nbench_ui_step2: done\n")
