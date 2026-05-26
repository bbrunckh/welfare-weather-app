# =============================================================================
# batch/02a_weather_selection.R
#
# Analysis of weather_stats.csv from batch/02_weather_stats.R to inform
# weather variable, reference period, and binning choices for simulations.
#
# Outputs (all under OUT_DIR/sample_selection/):
#   02a_1_variation_by_spec.png        SD heatmap by variable × ref_period
#   02a_2_within_loc_variation.png     within-location SD (temporal signal)
#   02a_3_survey_vs_climate.png        overlap of survey vs climate distributions
#   02a_4_bin_breaks.png               suggested absolute bin breaks across countries
#   02a_5_country_within_sd.png        within-loc SD by country × variable
#   02a_6_country_overlap.png          climate overlap by country × variable
#   weather_selection_summary.md     narrative summary
#
# Usage: source("batch/02a_weather_selection.R")
# =============================================================================

library(tidyverse)

# =============================================================================
# SECTION 1 — CONFIGURATION
# =============================================================================

OUT_DIR   <- Sys.getenv("WISEAPP_RESULTS_PATH")
WX_CSV    <- file.path(OUT_DIR, "weather_stats", "weather_stats.csv")
OUT_SAMPLE <- file.path(OUT_DIR, "sample_selection")
dir.create(OUT_SAMPLE, showWarnings = FALSE, recursive = TRUE)

wx <- read_csv(WX_CSV, show_col_types = FALSE)

# Flag survey vs climate reference rows
wx <- wx |>
  mutate(
    is_climate = grepl("Climate reference", survname),
    base_var = sub("_.*", "", variable)
  )

# Split
wx_svy  <- wx |> filter(!is_climate, !is.na(year))
wx_clim <- wx |> filter(is_climate)

# Base weather variables (from the variable column, not wx_spec)
base_vars <- sort(unique(wx_svy$base_var))
ref_periods <- c("1to1m", "1to3m", "1to6m", "1to12m")
transformations <- c("None", "Deviation from mean")

cat(sprintf("Weather stats: %d rows (%d survey, %d climate reference)\n",
            nrow(wx), nrow(wx_svy), nrow(wx_clim)))
cat(sprintf("Countries: %d | Variables: %s\n",
            n_distinct(wx_svy$code), paste(base_vars, collapse = ", ")))
cat(sprintf("Reference periods: %s\n", paste(ref_periods, collapse = ", ")))
cat(sprintf("Transformations: %s\n\n", paste(transformations, collapse = ", ")))

# Which countries have climate reference data
clim_codes <- unique(wx_clim$code)
svy_codes  <- unique(wx_svy$code)
no_clim    <- setdiff(svy_codes, clim_codes)
cat(sprintf("Countries with climate reference: %d / %d\n", length(clim_codes), length(svy_codes)))
if (length(no_clim) > 0)
  cat(sprintf("  Missing climate reference: %s\n\n", paste(no_clim, collapse = ", ")))

# =============================================================================
# SECTION 2 — VARIATION BY SPEC (SD heatmap)
# =============================================================================

# For each country × base_var × ref_period × transformation: get the SD
# Average SD across surveys within a country (for countries with multiple waves)
variation <- wx_svy |>
  filter(ref_period %in% ref_periods, transformation %in% transformations) |>
  group_by(code, base_var, ref_period, transformation) |>
  summarise(
    mean_sd = mean(sd, na.rm = TRUE),
    mean_within_sd = mean(within_loc_sd, na.rm = TRUE),
    mean_unique_per_loc = mean(n_unique_per_loc, na.rm = TRUE),
    .groups = "drop"
  )

# Rank specs by average cross-country SD (higher = more variation = better for identification)
spec_ranking <- variation |>
  group_by(base_var, ref_period, transformation) |>
  summarise(
    grand_sd = mean(mean_sd, na.rm = TRUE),
    grand_within_sd = mean(mean_within_sd, na.rm = TRUE),
    n_countries = n(),
    .groups = "drop"
  ) |>
  mutate(
    ref_period = factor(ref_period, levels = ref_periods),
    spec_label = paste0(base_var, " ", ref_period)
  ) |>
  arrange(base_var, ref_period)

# Heatmap: SD by variable × ref_period, faceted by transformation
p_var <- ggplot(spec_ranking, aes(x = ref_period, y = fct_rev(base_var), fill = grand_sd)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", grand_sd)), size = 3, color = "grey20") +
  facet_wrap(~transformation) +
  scale_fill_viridis_c(option = "mako", direction = -1, name = "Mean SD\n(across countries)") +
  labs(title = "Weather variation by variable and reference period",
       subtitle = "Higher SD = more variation in the survey sample for identification",
       x = "Reference period (months before interview)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

ggsave(file.path(OUT_SAMPLE, "02a_1_variation_by_spec.png"), p_var,
       width = 12, height = 6, dpi = 150, bg = "white")

# =============================================================================
# SECTION 3 — WITHIN-LOCATION VARIATION (temporal signal)
# =============================================================================

# within_loc_sd captures variation at the same location across time —
# this is the identifying variation for FE models with location fixed effects

within_ranking <- variation |>
  group_by(base_var, ref_period, transformation) |>
  summarise(
    grand_within_sd = mean(mean_within_sd, na.rm = TRUE),
    ratio = mean(mean_within_sd / mean_sd, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(ref_period = factor(ref_period, levels = ref_periods))

p_within <- ggplot(within_ranking, aes(x = ref_period, y = fct_rev(base_var), fill = grand_within_sd)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", grand_within_sd)), size = 3, color = "grey20") +
  facet_wrap(~transformation) +
  scale_fill_viridis_c(option = "rocket", direction = -1, name = "Mean within-\nlocation SD") +
  labs(title = "Within-location weather variation (temporal signal)",
       subtitle = "Variation at the same location across time — key for location-FE identification",
       x = "Reference period (months before interview)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

ggsave(file.path(OUT_SAMPLE, "02a_2_within_loc_variation.png"), p_within,
       width = 12, height = 6, dpi = 150, bg = "white")

# =============================================================================
# SECTION 4 — SURVEY vs CLIMATE OVERLAP
# =============================================================================

# Compare survey-period quantiles to climate reference quantiles
# Focus on "None" transformation (absolute values) for interpretability
overlap <- wx_svy |>
  filter(transformation == "None", ref_period %in% ref_periods) |>
  select(code, base_var, ref_period, year,
         svy_p10 = p10, svy_p50 = p50, svy_p90 = p90,
         svy_mean = mean, svy_sd = sd)

clim_match <- wx_clim |>
  filter(transformation == "None", ref_period %in% ref_periods) |>
  select(code, base_var, ref_period,
         clim_p10 = p10, clim_p50 = p50, clim_p90 = p90,
         clim_mean = mean, clim_sd = sd)

overlap_df <- inner_join(overlap, clim_match, by = c("code", "base_var", "ref_period")) |>
  mutate(
    # Overlap metric: how much of the survey p10-p90 range falls within the climate p10-p90 range
    overlap_lo = pmax(svy_p10, clim_p10),
    overlap_hi = pmin(svy_p90, clim_p90),
    svy_range = svy_p90 - svy_p10,
    overlap_pct = ifelse(svy_range > 0,
                          pmax(0, overlap_hi - overlap_lo) / svy_range * 100, NA_real_),
    ref_period = factor(ref_period, levels = ref_periods)
  )

# Summarise overlap by spec
overlap_summary <- overlap_df |>
  group_by(base_var, ref_period) |>
  summarise(
    mean_overlap = mean(overlap_pct, na.rm = TRUE),
    n_countries = n_distinct(code),
    .groups = "drop"
  )

p_overlap <- ggplot(overlap_summary, aes(x = ref_period, y = fct_rev(base_var), fill = mean_overlap)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", mean_overlap)), size = 3, color = "grey20") +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                        midpoint = 80, name = "Overlap %",
                        limits = c(50, 100)) +
  labs(title = "Survey vs climate reference distribution overlap",
       subtitle = paste0("% of survey p10–p90 range within climate p10–p90 (n=", n_distinct(overlap_df$code), " countries with climate ref)"),
       x = "Reference period", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

ggsave(file.path(OUT_SAMPLE, "02a_3_survey_vs_climate.png"), p_overlap,
       width = 8, height = 6, dpi = 150, bg = "white")

# =============================================================================
# SECTION 5 — BIN BREAK ANALYSIS
# =============================================================================

# For "None" transformation, examine distribution of values across countries
# to suggest reasonable absolute bin breaks that work across countries
# Focus on 12m ref period (most smoothed, most used for simulation)

bin_data <- wx_svy |>
  filter(transformation == "None", ref_period == "1to12m") |>
  select(code, base_var, year, p10, p20, p30, p40, p50, p60, p70, p80, p90, mean, sd, min, max)

# Compute cross-country quantile summary for each variable
bin_summary <- bin_data |>
  group_by(base_var) |>
  summarise(
    across(c(p10, p20, p30, p40, p50, p60, p70, p80, p90, mean, sd, min, max),
           list(med = ~median(.x, na.rm = TRUE),
                lo = ~quantile(.x, 0.1, na.rm = TRUE),
                hi = ~quantile(.x, 0.9, na.rm = TRUE)),
           .names = "{.col}_{.fn}"),
    n_surveys = n(),
    .groups = "drop"
  )

# Plot: cross-country range of percentiles for each variable (12m, None)
bin_long <- bin_data |>
  pivot_longer(cols = c(p10, p20, p30, p40, p50, p60, p70, p80, p90),
               names_to = "percentile", values_to = "value") |>
  mutate(
    pct_num = as.numeric(sub("p", "", percentile)),
    label = paste0(code, " ", year)
  )

p_bins <- ggplot(bin_long, aes(x = factor(pct_num), y = value)) +
  geom_boxplot(aes(fill = factor(pct_num)), outlier.size = 0.8, alpha = 0.7) +
  facet_wrap(~base_var, scales = "free_y", ncol = 3) +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(title = "Distribution of percentiles across countries (12-month, absolute values)",
       subtitle = "Each box = spread of that percentile across all country-surveys",
       x = "Percentile", y = "Value") +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold"))

ggsave(file.path(OUT_SAMPLE, "02a_4_bin_breaks.png"), p_bins,
       width = 14, height = max(5, length(base_vars) * 1.5), dpi = 150, bg = "white")

# Compute suggested 5-bin breaks per variable using pooled cross-country quantiles
suggested_breaks <- bin_data |>
  group_by(base_var) |>
  summarise(
    b1 = round(quantile(p20, 0.5, na.rm = TRUE), 1),
    b2 = round(quantile(p40, 0.5, na.rm = TRUE), 1),
    b3 = round(quantile(p60, 0.5, na.rm = TRUE), 1),
    b4 = round(quantile(p80, 0.5, na.rm = TRUE), 1),
    global_min = round(min(min, na.rm = TRUE), 1),
    global_max = round(max(max, na.rm = TRUE), 1),
    .groups = "drop"
  ) |>
  mutate(breaks = sprintf("[%.1f, %.1f, %.1f, %.1f, %.1f, %.1f]",
                           global_min, b1, b2, b3, b4, global_max))

cat("\n=== Suggested 5-bin breaks (12m, absolute values) ===\n")
print(suggested_breaks, n = Inf)

# =============================================================================
# SECTION 6 — COUNTRY-LEVEL ANALYSIS
# =============================================================================

# Variable ranking (used for plot axis ordering and markdown)
rec_var <- spec_ranking |>
  filter(transformation == "None") |>
  group_by(base_var) |>
  summarise(best_sd = max(grand_sd), best_within = max(grand_within_sd), .groups = "drop") |>
  arrange(desc(best_sd))

var_order <- rec_var$base_var

# Country-level metrics for recommended spec: 12-month, absolute
ctry_within <- variation |>
  filter(ref_period == "1to12m", transformation == "None") |>
  select(code, base_var, within_sd = mean_within_sd, total_sd = mean_sd)

ctry_overlap_12m <- overlap_df |>
  filter(ref_period == "1to12m") |>
  group_by(code, base_var) |>
  summarise(overlap = mean(overlap_pct, na.rm = TRUE), .groups = "drop") |>
  mutate(overlap = ifelse(is.nan(overlap), NA_real_, overlap))

# Country order: strongest overall signal first
ctry_order <- ctry_within |>
  group_by(code) |>
  summarise(mean_within = mean(within_sd, na.rm = TRUE)) |>
  arrange(desc(mean_within)) |>
  pull(code)

# --- Zero-inflation check ---
# Variables with many zero values (e.g. tx35, tr in cool countries) have inflated
# SD that doesn't reflect usable variation for identification
ctry_zero <- wx_svy |>
  filter(transformation == "None", ref_period == "1to12m") |>
  mutate(base_var = sub("_.*", "", variable)) |>
  group_by(code, base_var) |>
  summarise(med_p20 = median(p20, na.rm = TRUE),
            med_p50 = median(p50, na.rm = TRUE), .groups = "drop") |>
  mutate(zero_inflated = med_p20 <= 0)

# --- Plot 02e: within-loc SD by country × variable ---
ctry_within_norm <- ctry_within |>
  group_by(base_var) |>
  mutate(norm = (within_sd - min(within_sd, na.rm = TRUE)) /
           max(1e-6, max(within_sd, na.rm = TRUE) - min(within_sd, na.rm = TRUE))) |>
  ungroup() |>
  mutate(code = factor(code, levels = ctry_order),
         base_var = factor(base_var, levels = rev(var_order)))

# Zero-inflation markers for the heatmap (computed early, factors applied to match)
zero_marks <- ctry_zero |>
  filter(zero_inflated) |>
  mutate(code = factor(code, levels = ctry_order),
         base_var = factor(base_var, levels = rev(var_order))) |>
  filter(!is.na(code))

p_ctry_sd <- ggplot(ctry_within_norm, aes(x = code, y = base_var, fill = norm)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f", within_sd)), size = 2, color = "grey20") +
  geom_point(data = zero_marks, shape = 4, size = 3, color = "#d62728",
             stroke = 1.2, inherit.aes = FALSE,
             aes(x = code, y = base_var)) +
  scale_fill_viridis_c(option = "rocket", direction = -1,
                        name = "Within-loc SD\n(per-variable\nnormalized)") +
  labs(title = "Within-location SD by country and variable (12-month, absolute)",
       subtitle = "Color normalized per variable; text = actual SD. Red × = zero-inflated (median p20 ≤ 0).",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))

ggsave(file.path(OUT_SAMPLE, "02a_5_country_within_sd.png"), p_ctry_sd,
       width = 16, height = 6, dpi = 150, bg = "white")

# --- Plot 02f: climate overlap by country × variable ---
ctry_overlap_plot <- ctry_overlap_12m |>
  mutate(code = factor(code, levels = ctry_order[ctry_order %in% code]),
         base_var = factor(base_var, levels = rev(var_order)))

p_ctry_ov <- ggplot(ctry_overlap_plot, aes(x = code, y = base_var, fill = overlap)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = ifelse(is.na(overlap), "", sprintf("%.0f", overlap))),
            size = 2, color = "grey20") +
  scale_fill_gradient2(low = "#d73027", mid = "#fee08b", high = "#1a9850",
                        midpoint = 85, name = "Overlap %",
                        limits = c(50, 100), oob = scales::squish) +
  labs(title = "Survey vs climate overlap by country and variable (12-month, absolute)",
       subtitle = "% of survey p10–p90 within climate p10–p90. Countries without climate reference excluded.",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 7))

ggsave(file.path(OUT_SAMPLE, "02a_6_country_overlap.png"), p_ctry_ov,
       width = 14, height = 6, dpi = 150, bg = "white")

cat(sprintf("\n=== Zero-inflated combos (median p20 <= 0, 12m absolute): %d ===\n",
            sum(ctry_zero$zero_inflated)))

# --- Combined viability assessment ---
# Viable: within-loc SD >= p25, overlap >= 85%, not zero-inflated
sd_thresholds <- ctry_within |>
  group_by(base_var) |>
  summarise(sd_p25 = quantile(within_sd, 0.25, na.rm = TRUE), .groups = "drop")

ctry_combined <- ctry_within |>
  left_join(ctry_overlap_12m, by = c("code", "base_var")) |>
  left_join(sd_thresholds, by = "base_var") |>
  left_join(ctry_zero |> select(code, base_var, zero_inflated), by = c("code", "base_var")) |>
  mutate(
    has_climate = code %in% clim_codes,
    zero_inflated = replace_na(zero_inflated, FALSE),
    good_signal = within_sd >= sd_p25,
    poor_overlap = !is.na(overlap) & overlap < 85,
    viable = good_signal & !poor_overlap & !zero_inflated
  )

best_viable <- ctry_combined |>
  filter(viable) |>
  group_by(code) |>
  arrange(desc(within_sd)) |>
  slice_head(n = 3) |>
  summarise(best_vars = paste(base_var, collapse = ", "),
            top_sd = within_sd[1], .groups = "drop")

avoid_vars <- ctry_combined |>
  filter(poor_overlap | zero_inflated) |>
  mutate(reason = case_when(
    poor_overlap & zero_inflated ~ sprintf("%s (%.0f%% ov., zero-infl.)", base_var, overlap),
    poor_overlap ~ sprintf("%s (%.0f%% ov.)", base_var, overlap),
    zero_inflated ~ sprintf("%s (zero-infl.)", base_var)
  )) |>
  group_by(code) |>
  summarise(avoid = paste(reason, collapse = ", "), .groups = "drop")

# Regional groupings
region_map <- c(
  AGO = "Sub-Saharan Africa", BEN = "West Africa", BFA = "West Africa",
  BRA = "Latin America", CIV = "West Africa", COL = "Latin America",
  DOM = "Latin America", FJI = "Pacific", GAB = "Central Africa",
  GHA = "West Africa", GMB = "West Africa", GNB = "West Africa",
  GTM = "Latin America", IND = "South Asia", IRN = "Central/West Asia",
  LAO = "Southeast Asia", LKA = "South Asia", MLI = "West Africa",
  MMR = "Southeast Asia", MNG = "East Asia", MRT = "West Africa",
  MWI = "East/Southern Africa", NER = "West Africa", NGA = "West Africa",
  NPL = "South Asia", PER = "Latin America", SEN = "West Africa",
  TCD = "Central Africa", TGO = "West Africa", TJK = "Central/West Asia",
  TLS = "Southeast Asia", VNM = "Southeast Asia", ZMB = "East/Southern Africa"
)

ctry_recs <- tibble(code = ctry_order) |>
  left_join(best_viable, by = "code") |>
  left_join(avoid_vars, by = "code") |>
  mutate(
    has_climate = code %in% clim_codes,
    best_vars = replace_na(best_vars, "—"),
    avoid = replace_na(avoid, "—"),
    region = region_map[code]
  )

region_best <- ctry_combined |>
  filter(viable) |>
  mutate(region = region_map[code]) |>
  group_by(region, base_var) |>
  summarise(
    mean_sd = mean(within_sd, na.rm = TRUE),
    n_viable = n_distinct(code),
    mean_overlap = mean(overlap, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(region) |>
  arrange(desc(n_viable), desc(mean_sd)) |>
  slice_head(n = 3)

region_countries <- ctry_recs |>
  group_by(region) |>
  summarise(codes = paste(code, collapse = ", "), .groups = "drop")

cat("\n=== Country-level recommendations ===\n")
print(ctry_recs, n = Inf, width = 120)

# =============================================================================
# SECTION 7 — GENERATE MARKDOWN SUMMARY
# =============================================================================

md <- character()
md_line <- function(...) md <<- c(md, paste0(...))

md_line("# Weather Variable Selection")
md_line("")
md_line("Analysis of `weather_stats.csv` from `batch/02_weather_stats.R` to inform weather ",
        "variable, reference period, transformation, and binning choices for `batch/04_run_sim.R`.")
md_line("")
md_line("Builds on the sample selection analysis (see `sample_selection_summary.md`).")
md_line("")
md_line("Generated: ", Sys.Date())
md_line("")

# --- Data summary ---
md_line("## 1. Data summary")
md_line("")
md_line("- **", n_distinct(wx_svy$code), "** countries with survey weather data")
md_line("- **", length(clim_codes), "** countries with climate reference (1991–2020)")
md_line("- **", length(base_vars), "** weather variables: ", paste0("`", base_vars, "`", collapse = ", "))
md_line("- **Reference periods**: 1, 3, 6, 12 months before interview")
md_line("- **Transformations**: None (absolute), Deviation from mean")
md_line("")
if (length(no_clim) > 0) {
  md_line("**Countries missing climate reference**: ",
          paste0("`", no_clim, "`", collapse = ", "),
          " — these cannot be used for climate overlap assessment or simulation.")
  md_line("")
}

# --- Variation analysis ---
md_line("## 2. Weather variation by variable and reference period")
md_line("")
md_line("Higher SD = more variation in the survey sample, which is better for statistical ",
        "identification of weather effects on welfare.")
md_line("")

# Top specs by SD
top_specs_none <- spec_ranking |>
  filter(transformation == "None") |>
  arrange(desc(grand_sd)) |>
  head(10)
top_specs_devi <- spec_ranking |>
  filter(transformation == "Deviation from mean") |>
  arrange(desc(grand_sd)) |>
  head(10)

md_line("**Top 10 specs by SD (absolute values):**")
md_line("")
md_line("| Variable | Ref period | Mean SD | Within-loc SD |")
md_line("|----------|-----------|---------|---------------|")
for (i in seq_len(nrow(top_specs_none))) {
  r <- top_specs_none[i, ]
  w <- within_ranking |> filter(base_var == r$base_var, ref_period == r$ref_period, transformation == "None")
  md_line("| ", r$base_var, " | ", as.character(r$ref_period), " | ",
          sprintf("%.2f", r$grand_sd), " | ", sprintf("%.2f", w$grand_within_sd), " |")
}
md_line("")
md_line("**Top 10 specs by SD (deviation from mean):**")
md_line("")
md_line("| Variable | Ref period | Mean SD | Within-loc SD |")
md_line("|----------|-----------|---------|---------------|")
for (i in seq_len(nrow(top_specs_devi))) {
  r <- top_specs_devi[i, ]
  w <- within_ranking |> filter(base_var == r$base_var, ref_period == r$ref_period, transformation == "Deviation from mean")
  md_line("| ", r$base_var, " | ", as.character(r$ref_period), " | ",
          sprintf("%.2f", r$grand_sd), " | ", sprintf("%.2f", w$grand_within_sd), " |")
}
md_line("")
md_line("![Variation by spec](02a_1_variation_by_spec.png)")
md_line("")

# --- Within-location ---
md_line("## 3. Within-location variation (temporal signal)")
md_line("")
md_line("When using location fixed effects, only within-location (temporal) variation ",
        "identifies the weather effect. Higher within-location SD means more usable signal.")
md_line("")

# Best within-loc specs
best_within <- within_ranking |>
  filter(transformation == "None") |>
  arrange(desc(grand_within_sd)) |>
  head(5)
md_line("**Best temporal signal (absolute, top 5):**")
md_line("")
md_line("| Variable | Ref period | Within-loc SD | Within/Total ratio |")
md_line("|----------|-----------|---------------|-------------------|")
for (i in seq_len(nrow(best_within))) {
  r <- best_within[i, ]
  md_line("| ", r$base_var, " | ", as.character(r$ref_period), " | ",
          sprintf("%.2f", r$grand_within_sd), " | ", sprintf("%.0f%%", r$ratio * 100), " |")
}
md_line("")
md_line("![Within-location variation](02a_2_within_loc_variation.png)")
md_line("")

# --- Overlap ---
md_line("## 4. Survey vs climate reference overlap")
md_line("")
md_line("For simulation, survey-period weather should overlap well with the historical ",
        "climate distribution — otherwise the model extrapolates outside its training range.")
md_line("")
md_line("Overlap = % of the survey p10–p90 range that falls within the climate p10–p90 range. ",
        "Higher is better (100% = survey weather is fully within the historical climate envelope).")
md_line("")

# Best/worst overlap
best_overlap <- overlap_summary |> arrange(desc(mean_overlap)) |> head(5)
worst_overlap <- overlap_summary |> arrange(mean_overlap) |> head(5)

md_line("**Best overlap:**")
md_line("")
md_line("| Variable | Ref period | Mean overlap |")
md_line("|----------|-----------|-------------|")
for (i in seq_len(nrow(best_overlap))) {
  r <- best_overlap[i, ]
  md_line("| ", r$base_var, " | ", as.character(r$ref_period), " | ",
          sprintf("%.0f%%", r$mean_overlap), " |")
}
md_line("")
md_line("**Worst overlap (risk of extrapolation):**")
md_line("")
md_line("| Variable | Ref period | Mean overlap |")
md_line("|----------|-----------|-------------|")
for (i in seq_len(nrow(worst_overlap))) {
  r <- worst_overlap[i, ]
  md_line("| ", r$base_var, " | ", as.character(r$ref_period), " | ",
          sprintf("%.0f%%", r$mean_overlap), " |")
}
md_line("")
md_line("![Survey vs climate overlap](02a_3_survey_vs_climate.png)")
md_line("")

# --- Bin breaks ---
md_line("## 5. Suggested bin breaks")
md_line("")
md_line("For binned weather specifications, absolute breaks that work across countries. ",
        "Based on the cross-country median of survey percentiles (12-month reference period, absolute values).")
md_line("")
md_line("| Variable | Breaks (5 bins) | Global range |")
md_line("|----------|----------------|--------------|")
for (i in seq_len(nrow(suggested_breaks))) {
  r <- suggested_breaks[i, ]
  md_line("| ", r$base_var, " | ", r$breaks, " | ",
          sprintf("%.1f – %.1f", r$global_min, r$global_max), " |")
}
md_line("")
md_line("![Bin breaks](02a_4_bin_breaks.png)")
md_line("")

# --- Country-level analysis ---
md_line("## 6. Country-level weather signal")
md_line("")
md_line("The global rankings above average across all countries. This section shows which ",
        "variables work best in each country, using the recommended specification ",
        "(12-month reference period, absolute values).")
md_line("")
md_line("### Within-location SD by country")
md_line("")
md_line("Higher within-location SD = stronger temporal signal for fixed-effects estimation. ",
        "Countries sorted left to right by mean signal strength across all variables.")
md_line("")
md_line("![Country within-loc SD](02a_5_country_within_sd.png)")
md_line("")
md_line("### Zero-inflation diagnostic")
md_line("")
md_line("Variables like `tx35`, `tr`, and `r20` can have many zero values (no extreme events), ",
        "inflating SD without adding usable variation. A red × in the heatmap above marks ",
        "country–variable combinations where the median 20th percentile is ≤ 0 ",
        "(substantial mass at zero).")
md_line("")

# Global zero-inflation summary
zi_global <- ctry_zero |>
  group_by(base_var) |>
  summarise(n_zero = sum(zero_inflated),
            n_total = n(), .groups = "drop") |>
  filter(n_zero > 0) |>
  arrange(desc(n_zero))

md_line("**Variables affected (12-month, absolute):**")
md_line("")
md_line("| Variable | Countries with zero-inflation | Countries affected |")
md_line("|----------|------------------------------|-------------------|")
for (i in seq_len(nrow(zi_global))) {
  r <- zi_global[i, ]
  affected <- ctry_zero |> filter(base_var == r$base_var, zero_inflated) |> pull(code)
  md_line("| ", r$base_var, " | ", r$n_zero, "/", r$n_total, " | ",
          paste(affected, collapse = ", "), " |")
}
md_line("")
md_line("These combinations are excluded from the viability assessment below. ",
        "The top-ranked variables (`rx5day`, `mrsos`) are **not** affected by zero-inflation ",
        "at the 12-month reference period.")
md_line("")
md_line("### Climate overlap by country")
md_line("")
md_line("For simulation, survey weather must overlap well with the climate reference (1991–2020). ",
        "Values below 85% (orange/red) indicate risk of extrapolation beyond the training range.")
md_line("")
md_line("![Country climate overlap](02a_6_country_overlap.png)")
md_line("")

# --- Country-specific recommendations ---
md_line("## 7. Country-specific recommendations")
md_line("")
md_line("A variable is **viable** for a country if: (1) within-location SD ≥ 25th percentile ",
        "(adequate signal), (2) survey–climate overlap ≥ 85% (safe for simulation), and ",
        "(3) not zero-inflated (median p20 > 0). Best 3 viable variables per country, ",
        "ranked by within-location SD.")
md_line("")
md_line("| Country | Region | Best variables | Top SD | Avoid | Climate ref |")
md_line("|---------|--------|---------------|--------|-------|-------------|")
for (i in seq_len(nrow(ctry_recs))) {
  r <- ctry_recs[i, ]
  clim_flag <- ifelse(r$has_climate, "✓", "—")
  top_str <- ifelse(is.na(r$top_sd), "—", sprintf("%.2f", r$top_sd))
  md_line("| ", r$code, " | ", r$region, " | ", r$best_vars, " | ",
          top_str, " | ", r$avoid, " | ", clim_flag, " |")
}
md_line("")

md_line("### Regional patterns")
md_line("")
for (rgn in unique(region_best$region)) {
  rc <- region_countries |> filter(region == rgn)
  rb <- region_best |> filter(region == rgn)
  md_line("**", rgn, "** (", rc$codes, "):")
  md_line("")
  for (j in seq_len(nrow(rb))) {
    v <- rb[j, ]
    ov_str <- ifelse(is.na(v$mean_overlap), "no climate ref",
                     sprintf("%.0f%% overlap", v$mean_overlap))
    md_line(sprintf("- **%s**: viable in %d/%d countries, mean within-loc SD = %.2f, %s",
                    v$base_var, v$n_viable,
                    sum(ctry_recs$region == rgn, na.rm = TRUE),
                    v$mean_sd, ov_str))
  }
  md_line("")
}

if (length(no_clim) > 0) {
  md_line("### Countries without climate reference")
  md_line("")
  md_line("These countries (", paste0("`", no_clim, "`", collapse = ", "),
          ") cannot be assessed for climate overlap. Variable selection is based on ",
          "within-location SD only; simulation feasibility should be verified with caution.")
  md_line("")
}

# --- General Recommendations ---
md_line("## 8. General recommendations")
md_line("")

md_line("### Variable selection")
md_line("")
md_line("Ranked by overall variation (SD) in absolute values:")
md_line("")
for (i in seq_len(nrow(rec_var))) {
  r <- rec_var[i, ]
  md_line(sprintf("%d. **%s** — SD: %.2f, within-loc SD: %.2f",
                  i, r$base_var, r$best_sd, r$best_within))
}
md_line("")

md_line("### Reference period")
md_line("")
ref_rank <- spec_ranking |>
  filter(transformation == "None") |>
  group_by(ref_period) |>
  summarise(mean_sd = mean(grand_sd), mean_within = mean(grand_within_sd), .groups = "drop") |>
  arrange(desc(mean_sd))

md_line("| Ref period | Mean SD | Mean within-loc SD |")
md_line("|-----------|---------|-------------------|")
for (i in seq_len(nrow(ref_rank))) {
  r <- ref_rank[i, ]
  md_line("| ", as.character(r$ref_period), " | ", sprintf("%.2f", r$mean_sd),
          " | ", sprintf("%.2f", r$mean_within), " |")
}
md_line("")
md_line("Shorter reference periods (1m, 3m) have more variation but are noisier. ",
        "Longer periods (6m, 12m) smooth out noise and overlap better with climate projections. ",
        "**12-month** is recommended for simulation; **1–3 month** for robustness checks.")
md_line("")

md_line("### Transformation")
md_line("")
trans_rank <- spec_ranking |>
  group_by(transformation) |>
  summarise(mean_sd = mean(grand_sd), mean_within = mean(grand_within_sd), .groups = "drop")

md_line("| Transformation | Mean SD | Mean within-loc SD |")
md_line("|---------------|---------|-------------------|")
for (i in seq_len(nrow(trans_rank))) {
  r <- trans_rank[i, ]
  md_line("| ", r$transformation, " | ", sprintf("%.2f", r$mean_sd),
          " | ", sprintf("%.2f", r$mean_within), " |")
}
md_line("")
md_line("\"None\" (absolute values) preserves cross-sectional variation and enables interpretable ",
        "bin breaks. \"Deviation from mean\" isolates temporal anomalies but requires a reference period. ",
        "For simulation with binned specifications, **absolute values (None)** are recommended. ",
        "Deviation is useful for continuous specifications and robustness checks.")
md_line("")

writeLines(md, file.path(OUT_SAMPLE, "weather_selection_summary.md"))
cat("\n=== Saved summary to", file.path(OUT_SAMPLE, "weather_selection_summary.md"), "===\n")
cat("=== Plots saved to", OUT_SAMPLE, "===\n")
