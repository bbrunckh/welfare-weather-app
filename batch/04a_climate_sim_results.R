

library(tidyverse)
library(patchwork)
library(tidytext)

OUT_DIR <- Sys.getenv("WISEAPP_RESULTS_PATH")
OUT_DIR <- "dev/outputs"
SIM_DIR <- file.path(OUT_DIR, "simulations")

FOCUS_REF   <- "1to3m"
POLICY_VARS <- c("electricity", "imp_wat_san_rec", "ttime_health", "urban")

# -- Scenarios to display (excluding "historical" which is always the baseline) --
# Edit this vector to control which scenario_ids appear as separate lines
FOCUS_SCENARIOS <- c("SSP3-7.0 / 2025-2035", "SSP3-7.0 / 2040-2060")

SCENARIO_LABELS  <- c("SSP3-7.0 / 2025-2035" = "SSP3 2030", "SSP3-7.0 / 2040-2060" = "SSP3 2050")
SCENARIO_COLOURS <- c("SSP3-7.0 / 2025-2035" = "#189fe7",   "SSP3-7.0 / 2040-2060" = "#e7480f")

active_scenario_labels  <- SCENARIO_LABELS[FOCUS_SCENARIOS]
active_scenario_colours <- SCENARIO_COLOURS[FOCUS_SCENARIOS]

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

# Future scenarios only (filtered to FOCUS_SCENARIOS), joined to historical baseline
rp_diff <- rp_base |>
  filter(scenario_id %in% FOCUS_SCENARIOS) |>
  left_join(hist_central, by = c("country", "agg_method", "return_period")) |>
  mutate(diff_pct = (value - hist_value) / abs(hist_value) * 100) |>
  filter(!is.na(diff_pct)) |>
  select(country, agg_method, return_period, scenario_id, estimate, diff_pct) |>
  pivot_wider(names_from = estimate, values_from = diff_pct) |>
  rename(
    central     = `Central (P50)`,
    coef_lo     = `Coef P10`,
    coef_hi     = `Coef P90`,
    ensemble_lo = `Ensemble min`,
    ensemble_hi = `Ensemble max`
  ) |>
  mutate(
    scenario_label = factor(SCENARIO_LABELS[scenario_id], levels = active_scenario_labels)
  )

# Order countries by the first FOCUS_SCENARIO central estimate at 1:1
order_1_1 <- rp_diff |>
  filter(return_period == "1:1", scenario_id == FOCUS_SCENARIOS[[1]]) |>
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
    ggplot(aes(y = country_f, colour = scenario_label, shape = scenario_label)) +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(
      aes(xmin = ensemble_lo, xmax = ensemble_hi),
      height = 0, linewidth = 0.4, alpha = 0.5,
      position = position_dodge2(width = 0.6)
    ) +
    geom_point(aes(x = central), size = 2, position = position_dodge2(width = 0.6)) +
    facet_wrap(~return_period, nrow = 1, scales = "free_x") +
    scale_y_reordered() +
    scale_colour_manual(values = active_scenario_colours, labels = active_scenario_labels, name = "") +
    scale_shape_manual(
      values = setNames(c(16, 17, 15, 18)[seq_along(FOCUS_SCENARIOS)], active_scenario_labels),
      name   = ""
    ) +
    labs(
      title = label,
      x     = "Relative difference from historical (%)",
      y        = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position  = "right",
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
  "sp_p5_bottom40"      = "Cash transfer (bottom 40%, P5)",
  "elec_universal"       = "Universal electricity access",
  "health15min"          = "Health facility within 15 min",
  "imp_wat_san_universal" = "Universal water & sanitation"
)

rps_policy <- read_csv(file.path(SIM_DIR, "return_periods.csv")) |>
  filter(
    grepl("t_1to3m_binn_None", spec_label),
    policy_label %in% c("no_policy", names(policy_labels)),
    scenario_id  %in% FOCUS_SCENARIOS
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
  select(country, agg_method, return_period, scenario_id, estimate, policy_label, diff_pct) |>
  pivot_wider(names_from = estimate, values_from = diff_pct) |>
  rename(
    central     = `Central (P50)`,
    coef_lo     = `Coef P10`,
    coef_hi     = `Coef P90`,
    ensemble_lo = `Ensemble min`,
    ensemble_hi = `Ensemble max`
  ) |>
  mutate(
    scenario_label = factor(SCENARIO_LABELS[scenario_id], levels = active_scenario_labels)
  )

order_no_policy <- rp_policy_diff |>
  filter(return_period == "1:1", policy_label == "no_policy", scenario_id == FOCUS_SCENARIOS[[1]]) |>
  select(country, agg_method, order_val = central)

plot_metric_policy <- function(metric_key, policy_key) {
  metric_label     <- agg_labels[[metric_key]]
  policy_label_str <- policy_labels[[policy_key]]
  rps_use          <- metric_rps[[metric_key]]

  plot_data <- rp_policy_diff |>
    filter(
      agg_method   == metric_key,
      policy_label %in% c("no_policy", policy_key),
      return_period %in% rps_use
    ) |>
    left_join(order_no_policy |> filter(agg_method == metric_key), by = c("country", "agg_method")) |>
    mutate(
      country_f     = reorder_within(country, order_val, agg_method),
      return_period = factor(rp_labels[as.character(return_period)], levels = unique(rp_labels[rps_use])),
      policy_lab    = if_else(policy_label == "no_policy", "No policy", policy_label_str),
      grp_label     = paste0(SCENARIO_LABELS[scenario_id], " - ", policy_lab)
    )

  grp_levels  <- unique(plot_data$grp_label)
  grp_colours <- setNames(
    active_scenario_colours[
      plot_data |> distinct(grp_label, scenario_id) |>
        arrange(match(grp_label, grp_levels)) |> pull(scenario_id)
    ],
    grp_levels
  )
  grp_shapes <- setNames(ifelse(grepl("No policy", grp_levels), 16L, 17L), grp_levels)

  ggplot(plot_data, aes(y = country_f, x = central, colour = grp_label, shape = grp_label)) +
    geom_vline(xintercept = 0, linewidth = 0.4, linetype = "dashed", colour = "grey50") +
    geom_errorbarh(
      aes(xmin = ensemble_lo, xmax = ensemble_hi),
      height = 0, linewidth = 0.4, alpha = 0.5,
      position = position_dodge2(width = 0.8)
    ) +
    geom_point(size = 2, position = position_dodge2(width = 0.8)) +
    facet_wrap(~return_period, nrow = 1, scales = "free_x") +
    scale_y_reordered() +
    scale_colour_manual(values = grp_colours, name = "") +
    scale_shape_manual(values = grp_shapes, name = "") +
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
# ── Exceedance probability curves ─────────────────────────────────────────────
# Historical (no_policy baseline) vs SSP3 per policy scenario (median + ribbon)

library(arrow)

OUT_DIR <- "dev/outputs"
SIM_DIR <- file.path(OUT_DIR, "simulations")


EXCEED_DIR <- file.path(SIM_DIR, "exceedance_plots")
dir.create(EXCEED_DIR, showWarnings = FALSE, recursive = TRUE)

all_policy_labels <- c("no_policy", names(policy_labels)[names(policy_labels) != "health15min"])

outcomes_ep <- read_parquet(file.path(SIM_DIR, "outcomes.parquet")) |>
  filter(
    wx_name      == "t_r_1to3m_binn",
    policy_label %in% all_policy_labels,
    agg_method   == "headcount_ratio_300",
    scenario_id  %in% c("historical", FOCUS_SCENARIOS)
  )

# Helper: empirical exceedance probabilities (Weibull plotting position)
exceedance_curve <- function(x) {
  x_sorted <- sort(x, decreasing = TRUE)
  n        <- length(x_sorted)
  tibble(value = x_sorted, exceed_prob = seq_len(n) / (n + 1))
}

# ── Historical curve (no_policy only) ────────────────────────────────────────
hist_curve <- outcomes_ep |>
  filter(scenario_id == "historical", policy_label == "no_policy") |>
  summarise(value = mean(value), .by = c(code, sim_year)) |>
  reframe(exceedance_curve(value), .by = code)

# ── Future curves: per scenario × policy × ensemble member, then summarise ──
common_probs <- seq(0.02, 0.98, by = 0.02)

fut_per_member <- outcomes_ep |>
  filter(scenario_id %in% FOCUS_SCENARIOS) |>
  summarise(value = mean(value), .by = c(code, scenario_id, policy_label, ensemble_member, sim_year)) |>
  reframe(exceedance_curve(value), .by = c(code, scenario_id, policy_label, ensemble_member))

fut_interp <- fut_per_member |>
  group_by(code, scenario_id, policy_label, ensemble_member) |>
  group_modify(\(df, .key) {
    tibble(
      exceed_prob = common_probs,
      value       = approx(df$exceed_prob, df$value, xout = common_probs, rule = 2)$y
    )
  }) |>
  ungroup()

fut_curve <- fut_interp |>
  summarise(
    value_median = median(value),
    value_lo     = min(value),
    value_hi     = max(value),
    .by          = c(code, scenario_id, policy_label, exceed_prob)
  )

# ── Colours per policy (red tones for interventions, grey for no_policy) ─────
exceed_policy_colours <- c(
  "no_policy"             = "#1d2f73",
  "sp_p5_bottom40"        = "#8e1a0e",
  "elec_universal"        = "#8e1a0e",
  "imp_wat_san_universal" = "#8e1a0e"
)

exceed_policy_names <- c(
  "no_policy"             = "No policy",
  "sp_p5_bottom40"        = "Cash transfer",
  "elec_universal"        = "Universal electricity",
  "imp_wat_san_universal" = "Water & sanitation"
)

# ── Linetypes per scenario ────────────────────────────────────────────────────
exceed_scenario_lty <- setNames(
  c("solid", "dashed", "dotdash", "dotted")[seq_along(FOCUS_SCENARIOS)],
  FOCUS_SCENARIOS
)

# ── Plot function ─────────────────────────────────────────────────────────────
plot_exceedance <- function(country_code) {
  h      <- hist_curve |> filter(code == country_code)
  s      <- fut_curve  |> filter(code == country_code)
  combos <- s |> distinct(scenario_id, policy_label)

  # Lines: colour = policy, linetype = scenario
  line_layers <- pmap(combos, \(scenario_id, policy_label) {
    sd  <- filter(s, .data$scenario_id == .env$scenario_id, .data$policy_label == .env$policy_label)
    col <- exceed_policy_colours[[policy_label]]
    lty <- exceed_scenario_lty[[scenario_id]]
    lbl <- paste0(exceed_policy_names[[policy_label]], " - ", SCENARIO_LABELS[[scenario_id]])
    if (nrow(sd) == 0 || is.null(col)) return(NULL)
    geom_line(
      data = sd,
      aes(x = exceed_prob, y = value_median, colour = lbl, linetype = lbl),
      linewidth = 0.8, inherit.aes = FALSE
    )
  })

  # Build scale vectors
  combo_labels  <- paste0(exceed_policy_names[combos$policy_label], " - ", SCENARIO_LABELS[combos$scenario_id])
  colour_values <- c("Historical (no policy)" = "#000000",
                     setNames(exceed_policy_colours[combos$policy_label], combo_labels))
  lty_values    <- c("Historical (no policy)" = "solid",
                     setNames(exceed_scenario_lty[combos$scenario_id], combo_labels))

  # ── Build label data: endpoint of each line (right edge) ─────────────────
  right_edge_prob <- max(common_probs)

  fut_labels <- s |>
    filter(exceed_prob == right_edge_prob) |>
    mutate(
      lbl   = paste0(exceed_policy_names[policy_label], " - ", SCENARIO_LABELS[scenario_id]),
      colour = exceed_policy_colours[policy_label]
    ) |>
    select(exceed_prob, y = value_median, lbl, colour)

  hist_label <- h |>
    filter(exceed_prob == max(exceed_prob)) |>
    summarise(y = mean(value)) |>
    mutate(exceed_prob = max(h$exceed_prob), lbl = "Historical (no policy)", colour = "#000000")

  label_data <- bind_rows(fut_labels, hist_label)

  ggplot() +
    line_layers +
    geom_line(
      data = h,
      aes(x = exceed_prob, y = value,
          colour = "Historical (no policy)", linetype = "Historical (no policy)"),
      linewidth = 0.9, inherit.aes = FALSE
    ) +
    ggrepel::geom_text_repel(
      data          = label_data,
      aes(x = exceed_prob, y = y, label = lbl, colour = lbl),
      direction     = "y",
      hjust         = 0,
      nudge_x       = 0.02,
      segment.size  = 0.3,
      segment.color = "grey60",
      size          = 3,
      xlim          = c(right_edge_prob + 0.01, Inf),
      inherit.aes   = FALSE,
      show.legend   = FALSE
    ) +
    scale_x_continuous(
      name   = "Annual exceedance probability",
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(0, 1, 0.1),
      expand = expansion(mult = c(0.01, 0.45))
    ) +
    scale_y_continuous(
      name   = "Poverty rate ($3.00/day, 2021 PPP)",
      labels = scales::percent_format(accuracy = 0.1)
    ) +
    scale_colour_manual(name = NULL, values = colour_values) +
    scale_linetype_manual(name = NULL, values = lty_values) +
    coord_cartesian(clip = "off") +
    labs(title = paste0(country_code, ": Exceedance probability — Poverty ($3.00/day)")) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position  = "none",
      panel.grid.minor = element_blank(),
      plot.margin      = margin(5, 120, 5, 5)
    )
}

# Save all countries to exceedance_plots/
walk(unique(outcomes_ep$code), \(cc) {
  p <- plot_exceedance(cc)
  ggsave(
    file.path(EXCEED_DIR, paste0("exceedance_hrc300_", cc, ".png")),
    plot = p, width = 8, height = 5, dpi = 300
  )
})

walk(names(policy_labels), \(policy_key) {
  iwalk(agg_labels, \(label, metric_key) {
    p <- plot_metric_policy(metric_key, policy_key)
    ggsave(
      file.path(SIM_DIR, paste0("rp_policy_", policy_key, "_", metric_key, ".png")),
      plot = p, width = 6.5, height = 4, dpi = 300
    )
  })
})

