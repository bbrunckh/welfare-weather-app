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
  mutate(multi = n_surveys >= 2)

p_scatter <- ggplot(spatiotemporal, aes(x = n_locations, y = n_dates)) +
  geom_point(aes(size = n_locations, color = multi), alpha = 0.6) +
  ggrepel::geom_text_repel(aes(label = label), size = 2.5, max.overlaps = 20,
                            color = "grey30", segment.color = "grey70") +
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey60"),
                     labels = c("TRUE" = "2+ waves", "FALSE" = "1 wave"),
                     name = NULL) +
  scale_x_log10() +
  labs(title = "Spatial vs temporal variation across surveys",
       subtitle = "Higher = more interview months; further right = more H3-7 locations",
       x = "Number of H3-7 locations (log scale)", y = "Number of interview months") +
  theme_minimal(base_size = 11) +
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
  "Mean welfare (PPP/day)", "Median welfare (PPP/day)", "Gini",
  "Poverty $3.00", "Poverty $4.20", "Poverty $8.30",
  "Poverty gap $3.00", "Prosperity gap"
)
metric_colors <- c(
  "Mean welfare (PPP/day)" = "#2166ac", "Median welfare (PPP/day)" = "#67a9cf",
  "Gini" = "#b2182b",
  "Poverty $3.00" = "#d6604d", "Poverty $4.20" = "#f4a582", "Poverty $8.30" = "#fddbc7",
  "Poverty gap $3.00" = "#e08214", "Prosperity gap" = "#8073ac"
)

welfare_long <- welfare_agg |>
  select(code, economy, year, label,
         `Mean welfare (PPP/day)` = welfare_mean,
         `Median welfare (PPP/day)` = welfare_median,
         `Gini` = welfare_gini,
         `Poverty $3.00` = headcount_ratio_300,
         `Poverty $4.20` = headcount_ratio_420,
         `Poverty $8.30` = headcount_ratio_830,
         `Poverty gap $3.00` = gap_300,
         `Prosperity gap` = welfare_prosperity_gap) |>
  pivot_longer(-c(code, economy, year, label), names_to = "metric", values_to = "value") |>
  mutate(metric = factor(metric, levels = metric_levels))

multi_codes <- multi_survey$code

welfare_multi <- welfare_long |>
  filter(code %in% multi_codes)

p_welfare <- ggplot(welfare_multi, aes(x = factor(year), y = value, fill = metric)) +
  geom_col(width = 0.6, alpha = 0.85) +
  facet_grid(metric ~ economy, scales = "free", switch = "y") +
  scale_fill_manual(values = metric_colors, guide = "none") +
  labs(title = "Welfare metrics across surveys — countries with 2+ waves",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(
    strip.text.y.left = element_text(angle = 0, hjust = 1, size = 8),
    strip.text.x = element_text(size = 8),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    panel.grid.major.x = element_blank()
  )

ggsave(file.path(OUT_SAMPLE, "01a_5_welfare_comparison.png"), p_welfare,
       width = min(20, 2.5 * length(multi_codes)), height = 12, dpi = 150,
       bg = "white", limitsize = FALSE)

# =============================================================================
# SECTION 6 — POLICY VARIABLE COVERAGE
# =============================================================================

policy_vars <- c("electricity", "imp_wat_rec", "imp_san_rec", "imp_wat_san_rec",
                 "ttime_health", "internet", "cellphone", "piped", "piped_to_prem")

policy_coverage <- survey_stats |>
  filter(variable %in% policy_vars) |>
  select(code, economy, year, variable, mean, pct_missing, n,
         with_loc_n, with_loc_pct_missing) |>
  mutate(
    loc_coverage = ifelse(with_loc_n > 0, 1 - with_loc_pct_missing / 100, 0),
    available = loc_coverage >= 0.90,
    label = paste0(code, " ", year)
  )

# Color = mean normalized per variable; text = mean value; × = <90% coverage among located obs
policy_plot_df <- policy_coverage |>
  mutate(value = ifelse(available, mean, NA_real_)) |>
  group_by(variable) |>
  mutate(
    var_min = min(value, na.rm = TRUE),
    var_max = max(value, na.rm = TRUE),
    value_norm = ifelse(is.na(value), NA_real_,
                        (value - var_min) / pmax(var_max - var_min, 1e-9))
  ) |>
  ungroup() |>
  mutate(text_label = case_when(
    !available  ~ "×",
    mean <= 1   ~ sprintf("%.0f%%", mean * 100),
    TRUE        ~ sprintf("%.0f", round(mean))
  ))

p_policy <- ggplot(policy_plot_df, aes(x = variable, y = fct_rev(label), fill = value_norm)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = text_label), size = 2.3, color = "grey20") +
  scale_fill_viridis_c(option = "viridis", na.value = "grey90",
                        name = "Mean\n(per-variable\nscale)",
                        labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Policy variables",
       subtitle = "× = <90% coverage among geocoded obs",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

ggsave(file.path(OUT_SAMPLE, "01a_6_policy_coverage.png"), p_policy,
       width = 10, height = max(6, nrow(welfare_agg) * 0.2 + 2), dpi = 150,
       bg = "white")

# =============================================================================
# SECTION 7 — POLICY SCENARIOS SUMMARY TABLE
# =============================================================================

policy_scenario_table <- tribble(
  ~scenario,                ~policy_key, ~description,                                   ~variable_required,
  "elec_universal",         "A",         "Universal electricity access",                  "electricity",
  "imp_wat_universal",      "B",         "Universal improved water access",               "imp_wat_rec",
  "imp_san_universal",      "C",         "Universal improved sanitation access",           "imp_san_rec",
  "imp_wat_san_universal",  "I",         "Universal improved water + sanitation",          "imp_wat_san_rec",
  "health30min",            "D",         "Max 30-min travel to health facility",           "ttime_health",
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
# SECTION 8 — GENERATE MARKDOWN SUMMARY
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
