

library(tidyverse)
library(patchwork)
library(tidytext)

OUT_DIR <- Sys.getenv("WISEAPP_RESULTS_PATH")
SIM_DIR <- file.path(OUT_DIR, "simulations")

FOCUS_REF   <- "1to3m"
POLICY_VARS <- c("electricity", "imp_wat_san_rec", "ttime_health", "urban")

# Load ALL estimates (no filter on estimate)
rps <- read_csv(file.path(SIM_DIR, "return_periods.csv")) |>
  filter(
    grepl("t_1to3m_binn_None", spec_label),
    policy_label == "no_policy"
  )


focus_rps <- c("1:20", "1:10", "1:1", "9:10", "19:20")
focus_agg <- c("headcount_ratio_300", "headcount_ratio_830", "median", "gini")

# Return periods differ by metric direction:
# poverty/gini: extreme hot year = 1:20, 1:10, median temperature = 1:1
# median welfare: extreme hot year reduces welfare → use 19:20, 9:10, median = 1:1
metric_rps <- list(
  headcount_ratio_300 = c("1:1", "1:10", "1:20"),
  headcount_ratio_830 = c("1:1", "1:10", "1:20"),
  gini                = c("1:1", "1:10", "1:20"),
  median              = c("1:1", "9:10", "19:20")
)

agg_labels <- c(
  "headcount_ratio_300" = "Poverty ($3.00 2021 PPP)",
  "headcount_ratio_830" = "Poverty ($8.30 2021 PPP)",
  "gini"                = "Gini coefficient",
  "median"              = "Median welfare"
)

rp_labels <- c(
  "1:1"   = "Typical \n (median) \n temperature",
  "1:10"  = "1-in-10 \n year \n temperature",
  "1:20"  = "1-in-20 \n year \n temperature",
  "9:10"  = "1-in-10 \n year \n temperature",
  "19:20" = "1-in-20 \n year \n temperature"
)

# Compute diff_pct for every estimate type
# Note: Ensemble min/max only exist for SSP3 (observation-based historical has no ensemble)
# so we use the historical Central (P50) as the baseline for ALL estimate types

rp_base <- rps |>
  mutate(country = str_extract(spec_label, "^[A-Z]+")) |>
  pivot_longer(cols = `1:50`:`49:50`, names_to = "return_period", values_to = "value") |>
  filter(!is.na(value), return_period %in% focus_rps, agg_method %in% focus_agg) |>
  summarise(value = mean(value), .by = c(country, agg_method, scenario_id, return_period, estimate))

# Historical central baseline
hist_central <- rp_base |>
  filter(scenario_id == "historical", estimate == "Central (P50)") |>
  select(country, agg_method, return_period, hist_value = value)

# SSP3 estimates only, joined to historical baseline
rp_diff <- rp_base |>
  filter(scenario_id != "historical") |>
  left_join(hist_central, by = c("country", "agg_method", "return_period")) |>
  mutate(diff_pct = (value - hist_value) / abs(hist_value) * 100) |>
  filter(!is.na(diff_pct)) |>
  select(country, agg_method, return_period, estimate, diff_pct) |>
  pivot_wider(names_from = estimate, values_from = diff_pct) |>
  rename(
    central     = `Central (P50)`,
    coef_lo     = `Coef P10`,
    coef_hi     = `Coef P90`,
    ensemble_lo = `Ensemble min`,
    ensemble_hi = `Ensemble max`
  ) |>
  mutate(worse = if_else(agg_method == "median", central < 0, central > 0))

order_1_1 <- rp_diff |>
  filter(return_period == "1:1") |>
  select(country, agg_method, order_val = central)

plot_metric <- function(metric_key) {
  label   <- agg_labels[[metric_key]]
  rps_use <- metric_rps[[metric_key]]

  rp_diff |>
    filter(agg_method == metric_key, return_period %in% rps_use) |>
    left_join(order_1_1 |> filter(agg_method == metric_key), by = c("country", "agg_method")) |>
    mutate(
      country_f     = reorder_within(country, order_val, agg_method),
      return_period = factor(rp_labels[as.character(return_period)], levels = unique(rp_labels[rps_use]))
    ) |>
    ggplot(aes(y = country_f, colour = worse)) +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = "dashed", colour = "grey50") +
    # outer whiskers: ensemble range
    geom_errorbarh(aes(xmin = ensemble_lo, xmax = ensemble_hi),
                   height = 0, linewidth = 0.4, alpha = 0.5) +
    # central estimate
    geom_point(aes(x = central), size = 2) +
    facet_wrap(~return_period, nrow = 1, scales = "free_x") +
    scale_y_reordered() +
    scale_colour_manual(
      values = c("TRUE" = "#d6604d", "FALSE" = "#2166ac"),
      labels = c("TRUE" = "Worse", "FALSE" = "Better"),
      name   = ""
    ) +
    labs(
      title    = label,
      subtitle = NULL,
      x        = "Relative difference in SSP3 2030 climate scenario (%)",
      y        = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      strip.text       = element_text(size = 9)
    )
}

iwalk(agg_labels, \(label, key) {
  p <- plot_metric(key)
  ggsave(
    file.path(SIM_DIR, paste0("rp_diff_", key, ".png")),
    plot = p, width = 6.5, height = 4, dpi = 300
  )
})

# ── Policy comparison figures ─────────────────────────────────────────────────
# Compare no_policy vs each other policy under SSP3

policy_labels <- c(
  "sp_p10_bottom40"      = "Cash transfer (bottom 40%, P10)",
  "elec_universal"       = "Universal electricity access",
  "health15min"          = "Health facility within 15 min",
  "imp_wat_san_universal" = "Universal water & sanitation"
)

rps_policy <- read_csv(file.path(SIM_DIR, "return_periods.csv")) |>
  filter(
    grepl("t_1to3m_binn_None", spec_label),
    policy_label %in% c("no_policy", names(policy_labels)),
    scenario_id  != "historical"
  )

rp_policy_base <- rps_policy |>
  mutate(country = str_extract(spec_label, "^[A-Z]+")) |>
  pivot_longer(cols = `1:50`:`49:50`, names_to = "return_period", values_to = "value") |>
  filter(!is.na(value), return_period %in% focus_rps, agg_method %in% focus_agg) |>
  summarise(value = mean(value), .by = c(country, agg_method, scenario_id, return_period, estimate, policy_label))

# Express all policy scenarios relative to the same historical central baseline
rp_policy_diff <- rp_policy_base |>
  left_join(hist_central, by = c("country", "agg_method", "return_period")) |>
  mutate(diff_pct = (value - hist_value) / abs(hist_value) * 100) |>
  filter(!is.na(diff_pct)) |>
  select(country, agg_method, return_period, estimate, policy_label, diff_pct) |>
  pivot_wider(names_from = estimate, values_from = diff_pct) |>
  rename(
    central     = `Central (P50)`,
    coef_lo     = `Coef P10`,
    coef_hi     = `Coef P90`,
    ensemble_lo = `Ensemble min`,
    ensemble_hi = `Ensemble max`
  ) |>
  mutate(worse = if_else(agg_method == "median", central < 0, central > 0))

order_no_policy <- rp_policy_diff |>
  filter(return_period == "1:1", policy_label == "no_policy") |>
  select(country, agg_method, order_val = central)

plot_metric_policy <- function(metric_key, policy_key) {
  metric_label     <- agg_labels[[metric_key]]
  policy_label_str <- policy_labels[[policy_key]]
  rps_use          <- metric_rps[[metric_key]]

  plot_data <- rp_policy_diff |>
    filter(agg_method == metric_key, policy_label %in% c("no_policy", policy_key),
           return_period %in% rps_use) |>
    left_join(order_no_policy |> filter(agg_method == metric_key), by = c("country", "agg_method")) |>
    mutate(
      country_f     = reorder_within(country, order_val, agg_method),
      return_period = factor(rp_labels[as.character(return_period)], levels = unique(rp_labels[rps_use])),
      policy_label  = factor(
        policy_label,
        levels = c("no_policy", policy_key),
        labels = c("No policy", policy_label_str)
      )
    )

  ggplot(plot_data, aes(y = country_f, x = central, colour = policy_label, shape = policy_label)) +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(
      aes(xmin = ensemble_lo, xmax = ensemble_hi),
      height = 0, linewidth = 0.4, alpha = 0.5,
      position = position_dodge2(width = 0.6)
    ) +
    geom_point(
      size = 2,
      position = position_dodge2(width = 0.6)
    ) +
    facet_wrap(~return_period, nrow = 1, scales = "free_x") +
    scale_y_reordered() +
    scale_colour_manual(
      values = c("No policy" = "#d6604d", setNames("#1a9850", policy_label_str)),
      name   = ""
    ) +
    scale_shape_manual(
      values = c("No policy" = 16, setNames(17, policy_label_str)),
      name   = ""
    ) +
    labs(
      title    = metric_label,
      subtitle = paste0(policy_label_str, " vs No policy"),
      x        = "Relative difference from historical (%)",
      y        = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      strip.text       = element_text(size = 9)
    )
}

# Save one plot per policy × metric combination
walk(names(policy_labels), \(policy_key) {
  iwalk(agg_labels, \(label, metric_key) {
    p <- plot_metric_policy(metric_key, policy_key)
    ggsave(
      file.path(SIM_DIR, paste0("rp_policy_", policy_key, "_", metric_key, ".png")),
      plot = p, width = 6.5, height = 4, dpi = 300
    )
  })
})