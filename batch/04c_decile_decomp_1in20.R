# ── 04c_decile_decomp_1in20.R ─────────────────────────────────────────────────
# Stacked bar chart: decile decomposition of policy effects in the 1-in-20 year
# bad weather event.
#
# "1-in-20 bad weather year" = the sim_year whose total policy effect
# (sum of delta_total across deciles, median across ensemble members) sits at
# the 95th percentile across years -> the year the policy works hardest.
#
# Effect channels (stacked bars):
#   SP direct effect    - delta_sp
#   Main effect        - delta_main_covar
#   Repositioning effect  - delta_res1
#   Interaction effect  - delta_res2
#
# Facets: Historical  vs  SSP3 2050 (SSP3-7.0 / 2040-2060)
# One PNG per country x policy combination.
#
# Output: dev/outputs/simulations/decile_decomp_1in20/
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(arrow)

OUT_DIR      <- "dev/outputs/case_studies"
SIM_DIR      <- file.path(OUT_DIR, "simulations")
OUT_PLOT_DIR <- file.path(SIM_DIR, "decile_decomp_1in20")
dir.create(OUT_PLOT_DIR, showWarnings = FALSE, recursive = TRUE)

# ── Config ────────────────────────────────────────────────────────────────────
FOCUS_SCENARIOS <- c("historical", "SSP3-7.0 / 2040-2060")

scenario_labels <- c(
  "historical"            = "Historical (1991-2020)",
  "SSP3-7.0 / 2040-2060" = "SSP3 2050 (2040-2060)"
)

policy_names <- c(
  "sp_p5_bottom40"        = "Cash transfer (5% GDP, bottom 40%)",
  "elec_universal"        = "Universal electricity access",
  "imp_wat_san_universal" = "Water & sanitation (universal)",
  "secondary_universal"     = "Universal secondary education"
)

channel_labels <- c(
  delta_sp         = "SP direct effect",
  delta_main_covar = "Main effect (covariate shift)",
  delta_res1       = "Resilience - Repositioning effect",
  delta_res2       = "Resilience - Interaction effect"
)

channel_colours <- c(
  "SP direct effect"   = "#2166ac",
  "Main effect (covariate shift)"    = "#2166ac",
  "Resilience - Repositioning effect" = "#d6604d",
  "Resilience - Interaction effect" = "#f4a582"
)

# ── Load ──────────────────────────────────────────────────────────────────────
# Re-use dd_raw if already in environment, otherwise load from parquet
if (!exists("dd_raw")) {
  dd_raw <- read_parquet(file.path(SIM_DIR, "decile_decomposition_by_year.parquet"))
}

# Restrict to temperature 1-3 month binned spec only (exclude rx5day etc.)
dd <- dd_raw |>
  filter(grepl("_1to3m_binn", spec_label)) |>
  mutate(country = str_extract(spec_label, "^[A-Z]+")) |>
  filter(scenario_id %in% FOCUS_SCENARIOS)

# ── Median across ensemble members ────────────────────────────────────────────
dd_med <- dd |>
  summarise(
    across(c(delta_total, delta_sp, delta_main_covar, delta_res1, delta_res2), median),
    .by = c(country, scenario_id, policy_label, decile, sim_year)
  )

# ── Identify 1-in-20 bad weather year ─────────────────────────────────────────
# Sum delta_total across deciles per year -> 95th percentile year per group
year_1in20 <- dd_med |>
  summarise(year_total = sum(delta_total), .by = c(country, scenario_id, policy_label, sim_year)) |>
  summarise(
    sim_year_1in20 = sim_year[which.min(abs(year_total - quantile(year_total, 0.95)))],
    .by = c(country, scenario_id, policy_label)
  )

dd_1in20 <- dd_med |>
  inner_join(year_1in20, by = c("country", "scenario_id", "policy_label")) |>
  filter(sim_year == sim_year_1in20) |>
  mutate(
    scenario_label = factor(scenario_labels[scenario_id], levels = scenario_labels),
    decile_label   = factor(paste0(decile * 10, "%"), levels = paste0(1:9 * 10, "%"))
  )

# ── Plot function (one country x policy) ─────────────────────────────────────
plot_1in20 <- function(ctry, pol) {
  dat <- dd_1in20 |>
    filter(country == ctry, policy_label == pol)

  if (nrow(dat) == 0) return(NULL)

  # Year labels for subtitle
  year_info <- year_1in20 |>
    filter(country == ctry, policy_label == pol, scenario_id %in% FOCUS_SCENARIOS) |>
    mutate(lbl = paste0(scenario_labels[scenario_id], ": year ", sim_year_1in20)) |>
    pull(lbl) |>
    paste(collapse = "  |  ")

  pol_name <- coalesce(policy_names[pol], pol)

  # Drop channels that are entirely zero / negligible
  active_channels <- dat |>
    pivot_longer(c(delta_sp, delta_main_covar, delta_res1, delta_res2)) |>
    summarise(max_abs = max(abs(value)), .by = name) |>
    filter(max_abs > 1e-6) |>
    pull(name)

  dat_long <- dat |>
    pivot_longer(
      cols      = all_of(active_channels),
      names_to  = "channel",
      values_to = "delta"
    ) |>
    mutate(
      channel = factor(channel_labels[channel], levels = names(channel_colours))
    )

  active_colours <- channel_colours[names(channel_colours) %in% levels(dat_long$channel)]

  ggplot(dat_long, aes(x = decile_label, y = (exp(delta) - 1) * 100, fill = channel)) +
    geom_col(width = 0.72, colour = "white", linewidth = 0.2) +
    geom_hline(yintercept = 0, linewidth = 0.4, colour = "grey30") +
    facet_wrap(~scenario_label, ncol = 2) +
    scale_fill_manual(values = active_colours, name = NULL) +
    scale_y_continuous(
      labels = scales::label_number(suffix = "%", accuracy = 0.1)
    ) +
    labs(
      title    = paste0(ctry, "  -  ", pol_name),
      subtitle = NULL,
      x = "Welfare percentile",
      y = "% change in welfare"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x        = element_text(size = 9),
      legend.position    = "bottom",
      legend.text        = element_text(size = 9),
      panel.grid.minor   = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.text         = element_text(face = "bold", size = 10),
      strip.background   = element_rect(fill = "grey93", colour = NA),
      plot.title         = element_text(face = "bold", size = 12),
      plot.subtitle      = element_text(size = 8.5, colour = "grey40"),
      plot.margin        = margin(10, 12, 6, 10)
    )
}

# ── Save all country x policy combinations ────────────────────────────────────
combos <- dd_1in20 |>
  distinct(country, policy_label) |>
  arrange(country, policy_label)

pwalk(combos, \(country, policy_label) {
  p <- plot_1in20(country, policy_label)
  if (is.null(p)) return(invisible(NULL))

  fname <- file.path(
    OUT_PLOT_DIR,
    paste0("1in20_decile_decomp_", country, "_", policy_label, ".png")
  )
  ggsave(fname, plot = p, width = 10, height = 5, dpi = 300)
  message("Saved: ", basename(fname))
})

message("\nDone - ", nrow(combos), " plots saved to: ", OUT_PLOT_DIR)

