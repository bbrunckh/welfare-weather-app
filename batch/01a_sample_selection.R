# =============================================================================
# batch/01a_sample_selection.R
#
# Quick analysis of survey_stats.csv and welfare_aggregates.csv produced by
# batch/01_survey_stats.R.  Generates plots and a markdown summary to inform
# sample selection for subsequent batch runs (02–04).
#
# Outputs (all under OUT_DIR/sample_selection/):
#   01a_1_survey_coverage.png          countries × survey waves
#   01a_2_geocoding_diagnostic.png     % missing geocodes per survey
#   01a_3_spatiotemporal_variation.png scatter: interview months vs locations
#   01a_4_spatial_unit_size.png        avg H3-7 cell area by survey
#   01a_5_welfare_comparison.png       faceted welfare metrics by country
#   01a_6_policy_coverage.png          heatmap of policy variable availability
#   sample_selection_summary.md     narrative summary of findings
#
# Usage: source("batch/01a_sample_selection.R")
# =============================================================================

library(tidyverse)
library(patchwork)

# =============================================================================
# SECTION 1 — CONFIGURATION
# =============================================================================

OUT_DIR <- Sys.getenv("WISEAPP_RESULTS_PATH")
STATS_DIR <- file.path(OUT_DIR, "survey_stats")
OUT_SAMPLE <- file.path(OUT_DIR, "sample_selection")
dir.create(OUT_SAMPLE, showWarnings = FALSE, recursive = TRUE)

survey_stats <- read_csv(file.path(STATS_DIR, "survey_stats.csv"), show_col_types = FALSE)
welfare_agg  <- read_csv(file.path(STATS_DIR, "welfare_aggregates.csv"), show_col_types = FALSE)

welfare_agg <- welfare_agg |>
  mutate(label = paste0(code, " ", year))

# =============================================================================
# SECTION 2 — COUNTRIES WITH MULTIPLE SURVEYS
# =============================================================================

survey_counts <- welfare_agg |>
  group_by(code, economy) |>
  summarise(n_surveys = n(), years = paste(year, collapse = ", "), .groups = "drop") |>
  arrange(desc(n_surveys), code)

multi_survey <- filter(survey_counts, n_surveys >= 2)
multi_codes  <- multi_survey$code

cat("=== Countries with 2+ surveys ===\n")
print(multi_survey, n = Inf)

# Plot: survey coverage bar chart
p_coverage <- survey_counts |>
  mutate(economy = fct_reorder(economy, n_surveys)) |>
  ggplot(aes(x = economy, y = n_surveys)) +
  geom_col(aes(fill = n_surveys >= 2), width = 0.7) +
  geom_text(aes(label = years), hjust = -0.05, size = 2.5, color = "grey30") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey70"), guide = "none") +
  coord_flip() +
  labs(title = "Survey coverage by country",
       subtitle = paste0(nrow(multi_survey), " countries with 2+ surveys (blue)"),
       x = NULL, y = "Number of survey waves") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(OUT_SAMPLE, "01a_1_survey_coverage.png"), p_coverage,
       width = 10, height = max(6, nrow(survey_counts) * 0.25), dpi = 150,
       bg = "white")

# =============================================================================
# SECTION 2B — GEOCODING DIAGNOSTIC
# =============================================================================

geocoding <- survey_stats |>
  filter(variable == "welfare") |>
  select(code, economy, year, n, with_loc_n) |>
  mutate(
    pct_geocoded = with_loc_n / n * 100,
    pct_missing_geo = 100 - pct_geocoded,
    label = paste0(code, " ", year)
  )

p_geocode <- ggplot(geocoding, aes(x = fct_reorder(label, pct_missing_geo),
                                    y = pct_missing_geo)) +
  geom_col(aes(fill = pct_missing_geo), width = 0.7) +
  geom_text(aes(label = sprintf("%.1f%%  (n=%s, geo=%s)",
                                 pct_missing_geo,
                                 format(n, big.mark = ","),
                                 format(with_loc_n, big.mark = ","))),
            hjust = -0.03, size = 2.2, color = "grey30") +
  geom_hline(yintercept = 10, linetype = "dashed", color = "red", alpha = 0.6) +
  annotate("text", x = 1, y = 11, label = "10% threshold", hjust = 0,
           size = 2.8, color = "red", fontface = "italic") +
  scale_fill_gradient(low = "steelblue", high = "firebrick", guide = "none") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(title = "Missing geocodes by survey",
       subtitle = "% of observations without loc_id / area_h3 spatial identifiers",
       x = NULL, y = "% missing geocode") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(OUT_SAMPLE, "01a_2_geocoding_diagnostic.png"), p_geocode,
       width = 10, height = max(6, nrow(geocoding) * 0.25), dpi = 150, bg = "white")

cat(sprintf("\n=== Geocoding summary ===\n"))
cat(sprintf("  Fully geocoded (0%% missing): %d surveys\n",
            sum(geocoding$pct_missing_geo == 0)))
cat(sprintf("  >10%% missing geocodes: %d surveys\n",
            sum(geocoding$pct_missing_geo > 10)))
cat(sprintf("  Range: %.1f%% – %.1f%% missing\n\n",
            min(geocoding$pct_missing_geo), max(geocoding$pct_missing_geo)))

# =============================================================================
# SECTION 3 — SPATIOTEMPORAL VARIATION (scatter)
# =============================================================================

# n_dates = number of distinct interview months; n_unique of area_h3_7 = locations
temporal <- survey_stats |>
  filter(variable == "welfare") |>
  select(code, economy, survname, year, n_dates)

spatial <- survey_stats |>
  filter(variable == "area_h3_7") |>
  select(code, economy, survname, year, n_locations = n_unique)

spatiotemporal <- inner_join(temporal, spatial, by = c("code", "economy", "survname", "year")) |>
  mutate(label = paste0(code, " ", year)) |>
  left_join(survey_counts |> select(code, n_surveys), by = "code") |>
  mutate(multi = n_surveys >= 2) |>
  left_join(geocoding |> select(code, year, pct_geocoded), by = c("code", "year"))

p_scatter <- spatiotemporal |>
  filter(multi) |>
  ggplot(aes(x = n_locations, y = n_dates)) +
  geom_point(aes(colour = pct_geocoded), size = 3, alpha = 0.85) +
  ggrepel::geom_text_repel(aes(label = label), size = 3, max.overlaps = 20,
                            color = "grey30", segment.color = "grey70") +
  scale_colour_gradient(
    low = "#a8c8e8", high = "#0a2a4a",
    name = "% observations geocoded",
    labels = scales::label_number(suffix = "%"),
    guide = guide_colourbar(barwidth = 30, barheight = 0.6,
                             title.position = "top", title.hjust = 0.5)
  ) +
  scale_x_log10() +
  labs(title = NULL,
       subtitle = NULL,
       x = "Number of geocoded locations (log scale)", y = "Number of interview months") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom")

ggsave(file.path(OUT_SAMPLE, "01a_3_spatiotemporal_variation.png"), p_scatter,
       width = 9, height = 7, dpi = 150, bg = "white")

# =============================================================================
# SECTION 4 — AVERAGE SPATIAL UNIT SIZE (H3-7)
# =============================================================================

h3_stats <- survey_stats |>
  filter(variable == "area_h3_7") |>
  select(code, economy, year, avg_area_km2 = mean, n_locations = n_unique) |>
  mutate(label = paste0(code, " ", year))

p_h3 <- ggplot(h3_stats, aes(x = fct_reorder(label, avg_area_km2), y = avg_area_km2)) +
  geom_col(aes(fill = avg_area_km2), width = 0.7) +
  geom_text(aes(label = sprintf("%.0f km²  (n=%d)", avg_area_km2, n_locations)),
            hjust = -0.05, size = 2.3, color = "grey30") +
  scale_fill_viridis_c(option = "mako", direction = -1, guide = "none",
                        trans = "log10") +
  coord_flip() +
  scale_y_log10(labels = scales::label_comma(),
                expand = expansion(mult = c(0, 0.35))) +
  labs(title = "Average H3-7 cell area by survey",
       subtitle = "Smaller cells = finer spatial resolution for weather matching (log scale)",
       x = NULL, y = "Average area (km²)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

ggsave(file.path(OUT_SAMPLE, "01a_4_spatial_unit_size.png"), p_h3,
       width = 10, height = max(6, nrow(h3_stats) * 0.25), dpi = 150, bg = "white")

# =============================================================================
# SECTION 5 — WELFARE METRICS COMPARISON
# =============================================================================

metric_levels <- c(
  "Mean welfare\n(PPP/day)", "Median welfare\n(PPP/day)", "Gini",
  "Poverty $3.00", "Poverty $8.30"
)

# Wide form for sorting, long form for plotting
welfare_wide <- welfare_agg |>
  filter(code %in% multi_survey$code) |>
  select(code, economy, year,
         `Mean welfare\n(PPP/day)`   = welfare_mean,
         `Median welfare\n(PPP/day)` = welfare_median,
         `Gini`                      = welfare_gini,
         `Poverty $3.00`             = headcount_ratio_300,
         `Poverty $8.30`             = headcount_ratio_830)

# Country order: mean welfare in the LATEST available survey wave per country
country_order <- welfare_wide |>
  summarise(latest_year = max(year), .by = economy) |>
  left_join(
    welfare_wide |> select(economy, year, latest_mean = `Mean welfare\n(PPP/day)`),
    by = c("economy", "latest_year" = "year")
  ) |>
  arrange(latest_mean) |>
  pull(economy)

welfare_long <- welfare_wide |>
  pivot_longer(-c(code, economy, year), names_to = "metric", values_to = "value") |>
  mutate(
    metric  = factor(metric, levels = metric_levels),
    economy = factor(economy, levels = country_order)
  )

# Build a light-to-dark navy gradient across survey years
all_years <- sort(unique(welfare_long$year))

p_welfare <- ggplot(welfare_long, aes(x = value, y = economy)) +
  geom_line(aes(group = economy), colour = "grey70", linewidth = 0.4) +
  geom_point(aes(colour = year), size = 3, alpha = 0.9) +
  facet_wrap(~ metric, nrow = 1, scales = "free_x") +
  scale_colour_gradient(
    low = "#a8c8e8", high = "#0a2a4a",
    name = "Survey year",
    breaks = all_years,
    labels = all_years,
    guide = guide_colourbar(barwidth = 70, barheight = 0.7, title.position = "top",
                             title.hjust = 0.5)
  ) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal(base_size = 18) +
  theme(
    strip.text         = element_text(size = 16, face = "bold"),
    axis.text.y        = element_text(size = 16),
    axis.text.x        = element_text(size = 16, angle = 30, hjust = 1),
    panel.grid.major.y = element_line(colour = "grey92"),
    panel.grid.major.x = element_line(colour = "grey88"),
    panel.grid.minor   = element_blank(),
    legend.position    = "bottom",
    panel.spacing.x    = unit(1.2, "lines")
  )

n_countries <- length(country_order)

ggsave(file.path(OUT_SAMPLE, "01a_5_welfare_comparison.png"), p_welfare,
       width = 22, height = max(6, n_countries * 0.28 + 2), dpi = 150,
       bg = "white", limitsize = FALSE)

# =============================================================================
# SECTION 6 — POLICY VARIABLE COVERAGE
# =============================================================================

policy_vars <- c("electricity", "imp_wat_san_rec", "educ_com2_hh")

policy_coverage <- survey_stats |>
  filter(variable %in% policy_vars) |>
  select(code, economy, year, variable, mean, pct_missing, n,
         with_loc_n, with_loc_pct_missing) |>
  mutate(
    loc_coverage = ifelse(with_loc_n > 0, 1 - with_loc_pct_missing / 100, 0),
    available = loc_coverage >= 0.90,
    low_coverage = pct_missing > 10,
    label = paste0(code, " ", year)
  )

# Dot plot — multi-survey countries only, one facet per policy variable
policy_var_labels <- c(
  electricity    = "Electricity access",
  imp_wat_san_rec = "Improved water & sanitation",
  educ_com2_hh   = "Secondary education"
)

# Country order: mean electricity (first variable) in latest survey wave
policy_country_order <- policy_coverage |>
  filter(code %in% multi_codes, variable == "electricity") |>
  summarise(latest_year = max(year), .by = economy) |>
  left_join(
    policy_coverage |> filter(variable == "electricity") |>
      select(economy, year, latest_mean = mean),
    by = c("economy", "latest_year" = "year")
  ) |>
  arrange(latest_mean) |>
  pull(economy)

# Fallback: countries present in multi_codes but missing electricity data
all_multi_economies <- welfare_agg |>
  filter(code %in% multi_codes) |>
  distinct(economy) |>
  pull(economy)

policy_country_order <- c(
  setdiff(all_multi_economies, policy_country_order),
  policy_country_order
)

policy_plot_df <- policy_coverage |>
  filter(code %in% multi_codes) |>
  mutate(
    variable = factor(variable, levels = names(policy_var_labels),
                      labels = policy_var_labels),
    economy  = factor(economy, levels = policy_country_order)
  )

all_policy_years <- sort(unique(policy_plot_df$year))

p_policy <- ggplot(policy_plot_df, aes(x = mean, y = economy)) +
  geom_line(aes(group = economy), colour = "grey70", linewidth = 0.4) +
  # all dots filled by year gradient
  geom_point(aes(fill = year), colour = "transparent", size = 3.5, alpha = 0.9, shape = 21) +
  # red outline only for low-coverage points
  geom_point(
    data = filter(policy_plot_df, low_coverage),
    colour = "red", fill = NA, size = 3.5, shape = 21, stroke = 1
  ) +
  facet_wrap(~ variable, nrow = 1, scales = "free_x") +
  scale_fill_gradient(
    low = "#a8c8e8", high = "#0a2a4a",
    name = "Survey year",
    breaks = all_policy_years,
    labels = all_policy_years,
    guide = guide_colourbar(barwidth = 50, barheight = 0.7,
                             title.position = "top", title.hjust = 0.5)
  ) +
  labs(
    title = NULL, x = NULL, y = NULL,
    caption = "Red outline = >10% observations missing values"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    strip.text          = element_text(size = 14, face = "bold"),
    axis.text.y         = element_text(size = 14),
    axis.text.x         = element_text(size = 14, angle = 30, hjust = 1),
    panel.grid.major.y  = element_line(colour = "grey92"),
    panel.grid.major.x  = element_line(colour = "grey88"),
    panel.grid.minor    = element_blank(),
    legend.position     = "bottom",
    panel.spacing.x     = unit(1.2, "lines"),
    plot.caption        = element_text(size = 10, colour = "grey40", hjust = 0)
  )

n_policy_countries <- length(policy_country_order)

ggsave(file.path(OUT_SAMPLE, "01a_6_policy_coverage.png"), p_policy,
       width = 16, height = max(6, n_policy_countries * 0.28 + 2), dpi = 150,
       bg = "white", limitsize = FALSE)

# =============================================================================
# SECTION 7 — WEATHER VARIABLES COMPARISON
# =============================================================================

wx_stats <- read_csv(file.path(OUT_DIR, "weather_stats/weather_stats.csv"), show_col_types = FALSE)

wx_var_groups <- list(
  temp = c(
    t     = "Temperature mean (°C)",
    tr    = "Tropical nights (Tmin >20°C)",
    tx35  = "Days max >35°C"
  ),
  wet = c(
    r      = "Precipitation (mm)",
    rx5day = "Max 5-day precip (mm)",
    spei6  = "SPEI-6 (drought index)"
  )
)

wx_period_labels <- c(
  "1to12m" = "12 month reference period",
  "1to3m"  = "3 month reference period"
)

# Helper: build and save one weather dot plot
make_wx_plot <- function(group_name, ref_per, var_labels) {

  wx_survey <- wx_stats |>
    filter(
      ref_period     == ref_per,
      temporal_agg   == "Mean",
      transformation == "None",
      !is.na(year),
      code           %in% multi_codes,
      variable       %in% names(var_labels)
    ) |>
    select(code, economy, year, variable, mean)

  wx_ref <- wx_stats |>
    filter(
      ref_period     == ref_per,
      temporal_agg   == "Mean",
      transformation == "None",
      is.na(year),
      code           %in% multi_codes,
      variable       %in% names(var_labels)
    ) |>
    select(code, economy, variable, ref_mean = mean)

  # Country order: first variable in the group, latest wave
  order_var <- names(var_labels)[1]

  wx_country_order <- wx_survey |>
    filter(variable == order_var) |>
    summarise(latest_year = max(year), .by = economy) |>
    left_join(
      wx_survey |> filter(variable == order_var) |> select(economy, year, latest_mean = mean),
      by = c("economy", "latest_year" = "year")
    ) |>
    arrange(latest_mean) |>
    pull(economy)

  # Fallback countries missing the anchor variable
  wx_country_order <- c(
    setdiff(
      welfare_agg |> filter(code %in% multi_codes) |> distinct(economy) |> pull(economy),
      wx_country_order
    ),
    wx_country_order
  )

  all_wx_years <- sort(unique(wx_survey$year))

  wx_plot_df <- wx_survey |>
    mutate(
      variable = factor(variable, levels = names(var_labels), labels = var_labels),
      economy  = factor(economy, levels = wx_country_order)
    )

  wx_ref_df <- wx_ref |>
    mutate(
      variable = factor(variable, levels = names(var_labels), labels = var_labels),
      economy  = factor(economy, levels = wx_country_order)
    )

  n_countries <- length(wx_country_order)

  p <- ggplot(wx_plot_df, aes(x = mean, y = economy)) +
    geom_line(aes(group = economy), colour = "grey70", linewidth = 0.4) +
    geom_point(aes(fill = year), colour = "transparent", size = 3.5, alpha = 0.9, shape = 21) +
    geom_point(
      data = wx_ref_df,
      aes(x = ref_mean), shape = "|", size = 6, colour = "grey30", alpha = 0.8
    ) +
    facet_wrap(~ variable, nrow = 1, scales = "free_x") +
    scale_fill_gradient(
      low = "#a8c8e8", high = "#0a2a4a",
      name = "Survey year",
      breaks = all_wx_years,
      labels = all_wx_years,
      guide = guide_colourbar(barwidth = 50, barheight = 0.7,
                               title.position = "top", title.hjust = 0.5)
    ) +
    labs(
      title    = paste0(wx_period_labels[ref_per]),
      x = NULL, y = NULL,
      caption  = "Filled circles = survey wave mean  |  Grey tick ( | ) = climate reference period mean (1991–2020)"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      strip.text         = element_text(size = 16, face = "bold"),
      axis.text.y        = element_text(size = 16),
      axis.text.x        = element_text(size = 16, angle = 30, hjust = 1),
      panel.grid.major.y = element_line(colour = "grey92"),
      panel.grid.major.x = element_line(colour = "grey88"),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      panel.spacing.x    = unit(1.0, "lines"),
      plot.caption       = element_text(size = 16, colour = "grey40", hjust = 0),
      plot.title         = element_text(size = 16, face = "bold")
    )

  fname <- sprintf("01a_7_weather_%s_%s.png", group_name,
                   if (ref_per == "1to12m") "annual" else "seasonal")

  ggsave(file.path(OUT_SAMPLE, fname), p,
         width  = 3.5 * length(var_labels) + 10,
         height = max(6, n_countries * 0.28 + 2),
         dpi    = 150, bg = "white", limitsize = FALSE)
}

# Generate all 4 plots
for (grp in names(wx_var_groups)) {
  for (per in names(wx_period_labels)) {
    make_wx_plot(grp, per, wx_var_groups[[grp]])
  }
}

# =============================================================================
# SECTION 8 — WEATHER DISTRIBUTION: SURVEY vs REFERENCE PERIOD
# =============================================================================

# Variables to show (same selections as Section 7)
wx_dist_groups <- list(
  temp = c(
    t     = "Temperature mean (°C)",
    tr    = "Tropical nights (Tmin >20°C)",
    tx35  = "Days max >35°C"
  ),
  wet = c(
    r      = "Precipitation (mm)",
    rx5day = "Max 5-day precip (mm)",
    spei6  = "SPEI-6 (drought index)"
  )
)

pct_cols <- c("p10", "p20", "p30", "p40", "p50", "p60", "p70", "p80", "p90")

make_wx_dist_plot <- function(group_name, ref_per, var_labels) {

  # Reference period rows (year is NA)
  dist_ref <- wx_stats |>
    filter(
      ref_period     == ref_per,
      temporal_agg   == "Mean",
      transformation == "None",
      is.na(year),
      code           %in% multi_codes,
      variable       %in% names(var_labels)
    ) |>
    select(code, economy, variable, all_of(pct_cols)) |>
    mutate(source = "Reference period (1991–2020)")

  # Survey rows: pool across waves per country by averaging percentiles
  dist_survey <- wx_stats |>
    filter(
      ref_period     == ref_per,
      temporal_agg   == "Mean",
      transformation == "None",
      !is.na(year),
      code           %in% multi_codes,
      variable       %in% names(var_labels)
    ) |>
    summarise(
      across(all_of(pct_cols), \(x) mean(x, na.rm = TRUE)),
      .by = c(code, economy, variable)
    ) |>
    mutate(source = "Survey (pooled waves)")

  dist_all <- bind_rows(dist_ref, dist_survey)

  # Country order: survey p50 of first variable, ascending
  order_var <- names(var_labels)[1]
  country_order <- dist_survey |>
    filter(variable == order_var) |>
    arrange(p50) |>
    pull(economy)

  country_order <- c(
    setdiff(
      welfare_agg |> filter(code %in% multi_codes) |> distinct(economy) |> pull(economy),
      country_order
    ),
    country_order
  )

  dist_plot <- dist_all |>
    mutate(
      variable = factor(variable, levels = names(var_labels), labels = var_labels),
      economy  = factor(economy, levels = country_order),
      # small y-offset so reference and survey don't sit exactly on top
      y_offset = if_else(source == "Reference period (1991–2020)", -0.18, 0.18)
    )

  n_countries <- length(country_order)

  p <- ggplot(dist_plot, aes(y = as.numeric(economy) + y_offset, colour = source)) +
    # thin line: p10–p90
    geom_segment(
      aes(x = p10, xend = p90,
          yend = as.numeric(economy) + y_offset),
      linewidth = 0.6, alpha = 0.6
    ) +
    # thick line: p30–p70
    geom_segment(
      aes(x = p30, xend = p70,
          yend = as.numeric(economy) + y_offset),
      linewidth = 2.2, alpha = 0.75
    ) +
    # median dot
    geom_point(aes(x = p50), size = 2.2, alpha = 0.95) +
    facet_wrap(~ variable, nrow = 1, scales = "free_x") +
    scale_y_continuous(
      breaks = seq_along(country_order),
      labels = country_order,
      expand = expansion(add = 0.8)
    ) +
    scale_colour_manual(
      values = c(
        "Survey (pooled waves)"        = "#0a2a4a",
        "Reference period (1991–2020)" = "#b0b0b0"
      ),
      name = NULL
    ) +
    labs(
      title   = wx_period_labels[ref_per],
      x = NULL, y = NULL,
      caption = "Dot = median (p50)  |  Thick bar = p30–p70  |  Thin line = p10–p90  |  Navy = survey (pooled)  |  Grey = reference period"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      strip.text         = element_text(size = 16, face = "bold"),
      axis.text.y        = element_text(size = 16),
      axis.text.x        = element_text(size = 16, angle = 30, hjust = 1),
      panel.grid.major.y = element_line(colour = "grey92"),
      panel.grid.major.x = element_line(colour = "grey88"),
      panel.grid.minor   = element_blank(),
      legend.position    = "bottom",
      legend.text        = element_text(size = 16),
      panel.spacing.x    = unit(1.0, "lines"),
      plot.caption       = element_text(size = 16, colour = "grey40", hjust = 0),
      plot.title         = element_text(size = 16, face = "bold")
    )

  fname <- sprintf("01a_8_wx_dist_%s_%s.png", group_name,
                   if (ref_per == "1to12m") "annual" else "seasonal")

  ggsave(file.path(OUT_SAMPLE, fname), p,
         width  = 3.5 * length(var_labels) + 10,
         height = max(6, n_countries * 0.35 + 2),
         dpi    = 150, bg = "white", limitsize = FALSE)
}

for (grp in names(wx_dist_groups)) {
  for (per in names(wx_period_labels)) {
    make_wx_dist_plot(grp, per, wx_dist_groups[[grp]])
  }
}

# =============================================================================
# SECTION 9 — POLICY SCENARIOS SUMMARY TABLE
# =============================================================================

policy_scenario_table <- tribble(
  ~scenario,                ~policy_key, ~description,                                   ~variable_required,
  "elec_universal",         "A",         "Universal electricity access",                  "electricity",
  "imp_wat_universal",      "B",         "Universal improved water access",               "imp_wat_rec",
  "imp_san_universal",      "C",         "Universal improved sanitation access",           "imp_san_rec",
  "imp_wat_san_universal",  "I",         "Universal improved water + sanitation",          "imp_wat_san_rec",
  "health30min",            "D",         "Max 30-min travel to health facility",           "ttime_health",
  "educ_sec_universal",     "E",         "Universal secondary education completion",        "educ_com2_hh",
  "sp_p20_bottom40",        "—",         "Social protection: P20 transfer, bottom 40%",   "(none — SP only)"
)

# Check which scenarios are feasible per country
feasibility <- policy_coverage |>
  filter(available) |>
  distinct(code, variable) |>
  left_join(
    policy_scenario_table |> select(scenario, variable_required),
    by = c("variable" = "variable_required"),
    relationship = "many-to-many"
  ) |>
  filter(!is.na(scenario)) |>
  bind_rows(
    distinct(welfare_agg, code) |> mutate(scenario = "sp_p20_bottom40", variable = "(none)")
  ) |>
  group_by(code, scenario) |>
  summarise(feasible = TRUE, .groups = "drop")

feasibility_wide <- feasibility |>
  pivot_wider(names_from = scenario, values_from = feasible, values_fill = FALSE)

cat("\n=== Policy scenario feasibility by country ===\n")
print(feasibility_wide, n = Inf)

# =============================================================================
# SECTION 9 — GENERATE MARKDOWN SUMMARY
# =============================================================================

# Collect summary stats for the markdown
n_countries <- nrow(survey_counts)
n_surveys <- sum(survey_counts$n_surveys)
n_multi <- nrow(multi_survey)

top_spatial <- spatiotemporal |> arrange(desc(n_locations)) |> head(10)
top_temporal <- spatiotemporal |> arrange(desc(n_dates)) |> head(10)

welfare_multi_agg <- welfare_agg |> filter(code %in% multi_codes)
poorest <- welfare_multi_agg |> arrange(desc(headcount_ratio_300)) |> head(5)
richest <- welfare_multi_agg |> arrange(headcount_ratio_300) |> head(5)
most_unequal <- welfare_multi_agg |> arrange(desc(welfare_gini)) |> head(5)

# Policy variable available in how many surveys
policy_avail_summary <- policy_coverage |>
  filter(available) |>
  group_by(variable) |>
  summarise(n_surveys = n(), n_countries = n_distinct(code),
            mean_value = mean(mean, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(n_surveys))

n_fully_geo <- sum(geocoding$pct_missing_geo == 0)
n_high_miss <- sum(geocoding$pct_missing_geo > 10)
geo_worst <- geocoding |> arrange(desc(pct_missing_geo)) |> head(5)

md <- character()
md_line <- function(...) md <<- c(md, paste0(...))

md_line("# Sample Selection Analysis")
md_line("")
md_line("Analysis of `survey_stats.csv` and `welfare_aggregates.csv` from `batch/01_survey_stats.R`.")
md_line("Generated: ", Sys.Date())
md_line("")

# --- Section 1 ---------------------------------------------------------------
md_line("## 1. Survey coverage")
md_line("")
md_line("- **", n_countries, "** countries, **", n_surveys, "** total survey waves")
md_line("- **", n_multi, "** countries with 2+ surveys (needed for panel/temporal variation)")
md_line("")
md_line("| Country | Economy | # Surveys | Years |")
md_line("|---------|---------|-----------|-------|")
for (i in seq_len(nrow(survey_counts))) {
  r <- survey_counts[i, ]
  md_line("| ", r$code, " | ", r$economy, " | ", r$n_surveys, " | ", r$years, " |")
}
md_line("")
md_line("Countries with the most survey waves: **",
        paste(head(survey_counts$code, 5), collapse = ", "), "**")
md_line("")
md_line("![Survey coverage](01a_1_survey_coverage.png)")
md_line("")

# --- Section 2 ---------------------------------------------------------------
md_line("## 2. Geocoding coverage")
md_line("")
md_line("Weather matching requires geocoded observations (non-NA `loc_id` / `area_h3`). ",
        "Surveys with high missing geocode rates lose usable sample for weather-welfare estimation.")
md_line("")
md_line("- **", n_fully_geo, "** surveys are fully geocoded (0% missing)")
md_line("- **", n_high_miss, "** surveys have >10% missing geocodes")
md_line("")
if (n_high_miss > 0) {
  md_line("**Surveys with highest geocode missingness:**")
  md_line("")
  md_line("| Survey | % missing | Total n | Geocoded n |")
  md_line("|--------|-----------|---------|------------|")
  for (i in seq_len(nrow(geo_worst))) {
    r <- geo_worst[i, ]
    md_line("| ", r$label, " | ", sprintf("%.1f%%", r$pct_missing_geo),
            " | ", format(r$n, big.mark = ","), " | ", format(r$with_loc_n, big.mark = ","), " |")
  }
  md_line("")
}
md_line("![Geocoding diagnostic](01a_2_geocoding_diagnostic.png)")
md_line("")

# --- Section 3 ---------------------------------------------------------------
md_line("## 3. Spatiotemporal variation")
md_line("")
md_line("Key for weather-welfare identification: more interview months and more locations ",
        "provide greater variation in weather exposure.")
md_line("")
md_line("**Top 10 by spatial coverage (H3-7 locations):**")
md_line("")
md_line("| Survey | Locations | Interview months |")
md_line("|--------|-----------|-----------------|")
for (i in seq_len(nrow(top_spatial))) {
  r <- top_spatial[i, ]
  md_line("| ", r$label, " | ", format(r$n_locations, big.mark = ","), " | ", r$n_dates, " |")
}
md_line("")
md_line("**Top 10 by temporal coverage (interview months):**")
md_line("")
md_line("| Survey | Interview months | Locations |")
md_line("|--------|-----------------|-----------|")
for (i in seq_len(nrow(top_temporal))) {
  r <- top_temporal[i, ]
  md_line("| ", r$label, " | ", r$n_dates, " | ", format(r$n_locations, big.mark = ","), " |")
}
md_line("")
md_line("![Spatiotemporal variation](01a_3_spatiotemporal_variation.png)")
md_line("")

# --- Section 4 ---------------------------------------------------------------
md_line("## 4. Spatial unit size")
md_line("")
md_line("Average H3-7 cell area (km²) determines the spatial resolution of weather matching. ",
        "Smaller cells = finer-grained weather exposure.")
md_line("")
md_line("![Spatial unit size](01a_4_spatial_unit_size.png)")
md_line("")

# --- Section 5 ---------------------------------------------------------------
md_line("## 5. Welfare metrics comparison")
md_line("")
md_line("Comparison of key welfare indicators across survey waves for countries with 2+ surveys.")
md_line("")
md_line("**Most poverty ($3.00/day):** ",
        paste(paste0(poorest$label, " (", poorest$headcount_ratio_300 * 100, "%)"), collapse = ", "))
md_line("")
md_line("**Least poverty ($3.00/day):** ",
        paste(paste0(richest$label, " (", richest$headcount_ratio_300 * 100, "%)"), collapse = ", "))
md_line("")
md_line("**Most unequal (Gini):** ",
        paste(paste0(most_unequal$label, " (", most_unequal$welfare_gini, ")"), collapse = ", "))
md_line("")
md_line("![Welfare comparison](01a_5_welfare_comparison.png)")
md_line("")

# --- Section 6 ---------------------------------------------------------------
md_line("## 6. Policy variable coverage")
md_line("")
md_line("Availability of policy-relevant variables across surveys. A variable is marked ",
        "unavailable (×) when <90% of geocoded observations have a non-missing value.")
md_line("")
md_line("| Variable | # Surveys | # Countries | Mean (where available) |")
md_line("|----------|-----------|-------------|------------------------|")
for (i in seq_len(nrow(policy_avail_summary))) {
  r <- policy_avail_summary[i, ]
  fmt_mean <- if (r$mean_value <= 1) sprintf("%.0f%%", r$mean_value * 100) else sprintf("%.0f", round(r$mean_value))
  md_line("| ", r$variable, " | ", r$n_surveys, " | ", r$n_countries, " | ", fmt_mean, " |")
}
md_line("")
md_line("![Policy coverage](01a_6_policy_coverage.png)")
md_line("")

# --- Section 7 ---------------------------------------------------------------
md_line("## 7. Policy scenarios for simulation")
md_line("")
md_line("These are the policy scenarios configured in `batch/04_run_sim.R`:")
md_line("")
md_line("| Scenario | Key | Description | Required variable |")
md_line("|----------|-----|-------------|-------------------|")
for (i in seq_len(nrow(policy_scenario_table))) {
  r <- policy_scenario_table[i, ]
  md_line("| `", r$scenario, "` | ", r$policy_key, " | ", r$description, " | ", r$variable_required, " |")
}
md_line("")
md_line("**Feasibility by country** (based on geocoded variable availability):")
md_line("")

# Print feasibility as markdown table
feas_cols <- setdiff(names(feasibility_wide), "code")
md_line("| Country | ", paste(feas_cols, collapse = " | "), " |")
md_line("|---------|", paste(rep("---", length(feas_cols)), collapse = "|"), "|")
for (i in seq_len(nrow(feasibility_wide))) {
  vals <- sapply(feas_cols, function(col) {
    v <- feasibility_wide[[col]][i]
    if (isTRUE(v)) "✓" else "—"
  })
  md_line("| ", feasibility_wide$code[i], " | ", paste(vals, collapse = " | "), " |")
}
md_line("")

# --- Section 8 ---------------------------------------------------------------
md_line("## 8. Recommendations for sample selection")
md_line("")
md_line("### Prioritisation criteria")
md_line("")
md_line("1. **Multiple survey waves** — enables temporal variation in weather exposure")
md_line("2. **Geocoding coverage** — >90% geocoded preferred; surveys with >10% missing lose sample")
md_line("3. **High spatial coverage** — more H3-7 locations = finer weather variation")
md_line("4. **Extended interview periods** — more months = more weather variation within a wave")
md_line("5. **Policy variable availability** — required for infrastructure/digital policy scenarios")
md_line("6. **Reasonable spatial unit size** — very large cells dilute weather signal")
md_line("")

# Programmatic tier assignment (spatiotemporal already has n_surveys from earlier join)
tier1 <- spatiotemporal |>
  filter(n_surveys >= 2, n_locations >= 200, n_dates >= 4) |>
  distinct(code) |>
  pull(code) |>
  sort()

tier2 <- spatiotemporal |>
  filter(n_surveys >= 2, !code %in% tier1) |>
  distinct(code) |>
  pull(code) |>
  sort()

tier3 <- survey_counts |>
  filter(n_surveys == 1) |>
  pull(code) |>
  sort()

md_line("### Suggested tiers")
md_line("")
md_line("**Tier 1** (2+ waves, ≥ 200 locations, ≥ 4 interview months): ",
        paste0("`", tier1, "`", collapse = ", "))
md_line("")
md_line("**Tier 2** (2+ waves, lower spatial/temporal coverage): ",
        paste0("`", tier2, "`", collapse = ", "))
md_line("")
md_line("**Tier 3** (single wave only): ",
        paste0("`", tier3, "`", collapse = ", "))
md_line("")

writeLines(md, file.path(OUT_SAMPLE, "sample_selection_summary.md"))
cat("\n=== Saved summary to", file.path(OUT_SAMPLE, "sample_selection_summary.md"), "===\n")
cat("=== Plots saved to", OUT_SAMPLE, "===\n")
