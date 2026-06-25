
# ── 04b_decile_decomposition.R ────────────────────────────────────────────────
# Visualise policy effect decomposition across welfare deciles.
# Channels (mutually exclusive, sum to delta_total):
#   delta_sp          = SP direct transfer effect (constant across weather years)
#   delta_main_covar  = covariate-shift channel (infrastructure/health policies)
#   delta_res1        = weather-sensitive residual channel 1
#   delta_res2        = weather-sensitive residual channel 2
#
# Two plot types per country:
#   1. Stacked bar: median year vs worst year, per decile + overall
#      – SP bar is visually identical between year types; residual varies
#   2. Year-sensitivity: delta_total by sim_year per decile
#
# "Worst year" = sim_year with the lowest total delta_total summed across
# deciles, i.e. the year in which the policy provides least overall protection.
#
# Output: dev/outputs/simulations/decile_decomp/
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(arrow)

OUT_DIR    <- "dev/outputs/case_studies"
SIM_DIR    <- file.path(OUT_DIR, "simulations")
DECILE_DIR <- file.path(SIM_DIR, "decile_decomp")
dir.create(DECILE_DIR, showWarnings = FALSE, recursive = TRUE)

WX_SPEC <- "t_1to3m_binn_None"

# ── Labels ────────────────────────────────────────────────────────────────────
policy_names <- c(
  "sp_p5_bottom40"        = "Cash transfer\n(5% GDP, bottom 40%)",
  "sp_p10_bottom40"       = "Cash transfer\n(10% GDP, bottom 40%)",
  "elec_universal"        = "Universal\nelectricity",
  "imp_wat_san_universal" = "Water &\nsanitation",
  "health15min"           = "Health\n(15-min access)"
)

scenario_names <- c(
  "historical"            = "Historical (1991–2020)",
  "SSP3-7.0 / 2025-2035" = "SSP3 near-term (2025–2035)",
  "SSP3-7.0 / 2040-2060" = "SSP3 long-term (2040–2060)"
)

# Four channels; colours chosen to be interpretable together
channel_colours <- c(
  "SP direct effect"  = "#2166ac",
  "Covariate shift"   = "#4dac26",
  "Weather effect 1"  = "#d6604d",
  "Weather effect 2"  = "#f4a582"
)

year_type_levels <- c("Median year", "Worst year")

# ── Load & prep ───────────────────────────────────────────────────────────────
dd_raw <- read_parquet(
  file.path(SIM_DIR, "decile_decomposition_by_year.parquet")
) |>
  filter(grepl(WX_SPEC, spec_label)) |>
  mutate(country = sub("_.*", "", spec_label))

# Median across ensemble members → year × decile level
dd <- dd_raw |>
  summarise(
    across(c(delta_total, delta_sp, delta_main_covar, delta_res1, delta_res2), median),
    .by = c(country, scenario_id, policy_label, decile, sim_year)
  )

# ── Identify median year and worst year per country × scenario × policy ───────
# Worst year = sim_year with minimum sum of delta_total across deciles
# (year where the policy provides the least aggregate protection)
year_ranks <- dd |>
  summarise(total_effect = sum(delta_total), .by = c(country, scenario_id, policy_label, sim_year)) |>
  mutate(yr_rank = percent_rank(total_effect), .by = c(country, scenario_id, policy_label)) |>
  summarise(
    median_year = sim_year[which.min(abs(yr_rank - 0.5))],
    worst_year  = sim_year[which.min(total_effect)],
    .by = c(country, scenario_id, policy_label)
  ) |>
  pivot_longer(
    cols      = c(median_year, worst_year),
    names_to  = "year_type",
    values_to = "sim_year"
  ) |>
  mutate(year_type = recode(year_type,
    median_year = "Median year",
    worst_year  = "Worst year"
  ))

# Join year type back to decile-level data
dd_typed <- dd |>
  inner_join(year_ranks, by = c("country", "scenario_id", "policy_label", "sim_year"))

# Add an "Overall" row by summing channels across deciles (not averaging —
# total poverty effect = sum of decile-level effects for a given year)
dd_overall <- dd_typed |>
  summarise(
    across(c(delta_total, delta_sp, delta_main_covar, delta_res1, delta_res2), sum),
    .by = c(country, scenario_id, policy_label, year_type)
  ) |>
  mutate(decile = 0L)   # 0 = "Overall" sentinel

dd_plot <- bind_rows(dd_typed, dd_overall) |>
  mutate(
    decile_label = if_else(decile == 0L, "Overall", as.character(decile)),
    decile_label = factor(
      decile_label,
      levels = c(as.character(1:9), "Overall")
    ),
    year_type = factor(year_type, levels = year_type_levels),
    scenario_label = coalesce(scenario_names[scenario_id], scenario_id),
    scenario_label = factor(scenario_label, levels = scenario_names),
    policy_name    = coalesce(policy_names[policy_label], policy_label)
  )

# ── Plot 1: Stacked bar, median vs worst year ─────────────────────────────────
plot_stacked_bars <- function(ctry) {
  ctry_dat <- filter(dd_plot, country == ctry)

  active_channels <- ctry_dat |>
    pivot_longer(cols = c(delta_sp, delta_main_covar, delta_res1, delta_res2)) |>
    summarise(max_abs = max(abs(value)), .by = name) |>
    filter(max_abs > 1e-6) |>
    pull(name)

  channel_map <- c(
    delta_sp         = "SP direct effect",
    delta_main_covar = "Covariate shift",
    delta_res1       = "Weather effect 1",
    delta_res2       = "Weather effect 2"
  )

  dat <- ctry_dat |>
    pivot_longer(
      cols      = c(delta_sp, delta_main_covar, delta_res1, delta_res2),
      names_to  = "channel",
      values_to = "delta"
    ) |>
    filter(channel %in% active_channels) |>
    mutate(channel = factor(channel_map[channel], levels = names(channel_colours)))

  # Compute per-channel position within the stack for stacked white overlay
  # (We need cumulative sums to place the faded "lost" portion correctly)
  dat_med <- filter(dat, year_type == "Median year") |>
    arrange(decile_label, scenario_label, policy_name, channel) |>
    mutate(
      ymax = cumsum(delta),
      ymin = ymax - delta,
      .by = c(decile_label, scenario_label, policy_name)
    )

  dat_worst <- filter(dat, year_type == "Worst year") |>
    arrange(decile_label, scenario_label, policy_name, channel) |>
    mutate(
      ymax_w = cumsum(delta),
      ymin_w = ymax_w - delta,
      .by = c(decile_label, scenario_label, policy_name)
    ) |>
    select(decile_label, scenario_label, policy_name, channel, ymax_w, ymin_w, delta_worst = delta)

  dat_combined <- left_join(
    dat_med, dat_worst,
    by = c("decile_label", "scenario_label", "policy_name", "channel")
  ) |>
    # The "lost" portion for each channel: top of worst_bar to top of median_bar
    # Only shown where worst < median (i.e. weather residual shrinks)
    mutate(lost_top = ymax, lost_bot = pmax(ymax_w, ymin))

  worst_totals <- dat_worst |>
    summarise(total_worst = sum(delta_worst), .by = c(decile_label, scenario_label, policy_name))

  n_pol <- n_distinct(dat$policy_name)

  ggplot() +
    # Stacked median-year bars (coloured by channel)
    geom_rect(
      data = dat_med,
      aes(xmin = as.integer(decile_label) - 0.35,
          xmax = as.integer(decile_label) + 0.35,
          ymin = ymin, ymax = ymax, fill = channel)
    ) +
    # White fade overlay from worst-year top to median-year top (lost protection)
    geom_rect(
      data = dat_combined,
      aes(xmin = as.integer(decile_label) - 0.35,
          xmax = as.integer(decile_label) + 0.35,
          ymin = lost_bot, ymax = lost_top),
      fill  = "white", alpha = 0.6, inherit.aes = FALSE
    ) +
    # Outline showing worst-year total height
    geom_segment(
      data = worst_totals,
      aes(x    = as.integer(decile_label) - 0.35,
          xend = as.integer(decile_label) + 0.35,
          y    = total_worst, yend = total_worst),
      colour = "grey25", linewidth = 0.5, linetype = "dashed",
      inherit.aes = FALSE
    ) +
    { if (n_pol > 1) facet_grid(scenario_label ~ policy_name) else facet_wrap(~scenario_label, ncol = 1) } +
    scale_fill_manual(
      values = channel_colours[names(channel_colours) %in% levels(dat$channel)],
      name   = "Effect channel"
    ) +
    scale_x_continuous(
      name   = "Welfare decile (1 = poorest)",
      breaks = seq_along(levels(dat$decile_label)),
      labels = levels(dat$decile_label)
    ) +
    scale_y_continuous(
      name   = "Δ Poverty rate (p.p.)",
      labels = scales::label_number(scale = 100, suffix = "pp", accuracy = 0.1)
    ) +
    labs(
      title    = paste0(ctry, ": Policy effect decomposition — median vs worst weather year"),
      subtitle = paste0(
        "Full bar = median year; faded portion = protection lost in the worst weather year (dashed line).\n",
        "SP/covariate channels are constant; weather residual channels vary."
      )
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position    = "bottom",
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text         = element_text(size = 9, face = "bold"),
      axis.text.x        = element_text(size = 8),
      plot.title         = element_text(face = "bold", size = 11),
      plot.subtitle      = element_text(size = 8, colour = "grey40")
    )
}

# ── Plot 2: Year-sensitivity lines ────────────────────────────────────────────
plot_year_sensitivity <- function(ctry) {
  dat <- dd |>
    filter(country == ctry, scenario_id != "historical") |>
    mutate(
      scenario_label = coalesce(scenario_names[scenario_id], scenario_id),
      scenario_label = factor(scenario_label, levels = scenario_names),
      policy_name    = coalesce(policy_names[policy_label], policy_label)
    )

  # Mark median and worst year
  markers <- year_ranks |>
    filter(country == ctry, scenario_id != "historical") |>
    left_join(
      dd |> filter(country == ctry, scenario_id != "historical"),
      by = c("country", "scenario_id", "policy_label", "sim_year")
    ) |>
    mutate(
      scenario_label = coalesce(scenario_names[scenario_id], scenario_id),
      scenario_label = factor(scenario_label, levels = scenario_names),
      policy_name    = coalesce(policy_names[policy_label], policy_label),
      year_type      = factor(year_type, levels = year_type_levels)
    )

  ggplot(dat, aes(
    x      = sim_year,
    y      = delta_total,
    colour = factor(decile),
    group  = factor(decile)
  )) +
    geom_line(alpha = 0.7, linewidth = 0.5) +
    geom_point(
      data = markers,
      aes(x = sim_year, y = delta_total, shape = year_type),
      size = 2.5, stroke = 0.8, colour = "grey20",
      inherit.aes = FALSE
    ) +
    facet_grid(scenario_label ~ policy_name, scales = "free_x") +
    scale_colour_viridis_d(
      name = "Decile\n(1 = poorest)", option = "plasma", end = 0.9, direction = -1
    ) +
    scale_shape_manual(
      values = c("Median year" = 16, "Worst year" = 4),
      name   = "Reference year"
    ) +
    scale_y_continuous(
      name   = "Δ Poverty rate (p.p.)",
      labels = scales::label_number(scale = 100, suffix = "pp", accuracy = 0.1)
    ) +
    scale_x_continuous(name = "Weather analogue year") +
    labs(
      title    = paste0(ctry, ": Weather-year sensitivity by decile"),
      subtitle = "Dot = median year  ✕ = worst year (lowest aggregate policy benefit)"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position  = "right",
      panel.grid.minor = element_blank(),
      strip.text       = element_text(size = 8),
      plot.title       = element_text(face = "bold", size = 11)
    )
}

# ── Save all countries ────────────────────────────────────────────────────────
all_countries <- sort(unique(dd_raw$country))

walk(all_countries, \(ctry) {
  n_pol <- n_distinct(filter(dd_raw, country == ctry)$policy_label)
  n_scn <- n_distinct(filter(dd_raw, country == ctry)$scenario_id)

  p_bar <- plot_stacked_bars(ctry)
  ggsave(
    file.path(DECILE_DIR, paste0("decile_decomp_bar_", ctry, ".png")),
    plot   = p_bar,
    # 10 x-positions (deciles 1–9 + Overall) × 2 bars each, per policy column
    width  = 2 + n_pol * 5.5,
    height = 2 + n_scn * 2.2,
    dpi    = 300
  )

  p_yr <- plot_year_sensitivity(ctry)
  ggsave(
    file.path(DECILE_DIR, paste0("decile_year_sensitivity_", ctry, ".png")),
    plot   = p_yr,
    width  = 3 + n_pol * 2.2,
    height = 2 + (n_scn - 1) * 2.5,
    dpi    = 300
  )

  message("Saved: ", ctry, " (", n_pol, " policies, ", n_scn, " scenarios)")
})

message("\nDone — ", length(all_countries), " countries in: ", DECILE_DIR)
