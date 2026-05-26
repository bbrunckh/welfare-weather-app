# =============================================================================
# batch/03a_model_selection.R
#
# Analysis of model_coefficients.csv and model_fit_stats.csv from
# batch/03_run_mod1.R (temperature variable only).  Evaluates which weather
# construction, reference period, FE profile, covariate selection, and
# interaction terms produce the most significant and precise estimates, to
# inform simulation configuration in batch/04_run_sim.R.
#
# Outputs (all under OUT_DIR/sample_selection/):
#   03a_1_coef_significance.png      significance heatmap by spec × country
#   03a_2_fe_cov_comparison.png      FE and covariate profile comparison
#   03a_3_ref_period_ranking.png     coefficient precision by reference period
#   03a_4_interaction_heatmap.png    interaction significance by country
#   03a_5_rif_distributional.png     RIF quantile coefficient profiles
#   03a_6_model_fit_quality.png      R² and within-R² comparison
#   03a_7_lasso_selection.png        LASSO-selected covariate frequency
#   model_selection_summary.md       narrative summary
#
# Usage: source("batch/03a_model_selection.R")
# =============================================================================

library(tidyverse)
library(patchwork)

# =============================================================================
# SECTION 1 — CONFIGURATION
# =============================================================================

OUT_DIR    <- Sys.getenv("WISEAPP_RESULTS_PATH")
MOD_DIR    <- file.path(OUT_DIR, "model_fit")
OUT_SAMPLE <- file.path(OUT_DIR, "sample_selection")
dir.create(OUT_SAMPLE, showWarnings = FALSE, recursive = TRUE)

coef  <- read_csv(file.path(MOD_DIR, "model_coefficients.csv"), show_col_types = FALSE)
stats <- read_csv(file.path(MOD_DIR, "model_fit_stats.csv"), show_col_types = FALSE)

int_na <- tryCatch(
  read_csv(file.path(MOD_DIR, "_interactions_not_available.csv"), show_col_types = FALSE),
  error = function(e) tibble(spec_label = character(), reason = character(),
                             sample = character(), interaction = character())
)

# Parse weather spec into components
parse_wx <- function(df) {
  df |>
    mutate(
      ref_period = str_extract(weather, "\\d+to\\d+m"),
      wx_type = ifelse(grepl("_binn_", weather), "binned", "continuous")
    )
}

coef  <- parse_wx(coef)
stats <- parse_wx(stats)

ref_levels <- c("1to1m", "1to3m", "1to6m", "1to12m")

# Weather main effect: "t" for continuous, "t_bin*" for binned
wx_main <- coef |> filter(term == "t" | grepl("^t_bin", term))
wx_cont <- coef |> filter(term == "t")
wx_int  <- coef |> filter(grepl("^t:", term))

n_countries <- n_distinct(coef$code)
countries   <- sort(unique(coef$code))
n_wx_specs  <- n_distinct(coef$weather)

cat(sprintf("Model results: %d countries, %d weather specs\n", n_countries, n_wx_specs))
cat(sprintf("Coefficients: %s rows | Fit stats: %s rows\n",
            format(nrow(coef), big.mark = ","), format(nrow(stats), big.mark = ",")))
cat(sprintf("Countries: %s\n\n", paste(countries, collapse = ", ")))

# Missing interactions summary
if (nrow(int_na) > 0) {
  int_na_summary <- int_na |> distinct(sample, interaction) |> count(sample, name = "n_missing")
  cat("Countries with unavailable interactions:\n")
  for (i in seq_len(nrow(int_na_summary))) {
    r <- int_na_summary[i, ]
    vars <- int_na |> filter(sample == r$sample) |> distinct(interaction) |> pull(interaction)
    cat(sprintf("  %s: %s\n", r$sample, paste(vars, collapse = ", ")))
  }
  cat("\n")
}

# =============================================================================
# SECTION 2 — COEFFICIENT SIGNIFICANCE BY SPEC × COUNTRY
# =============================================================================

# Focus on continuous weather (term == "t"), fit3 (full model), Mean estimand
cont_fit3 <- wx_cont |>
  filter(model == "fit3", estimand == "Mean")

# Best p-value per country × weather spec (across FE/cov profiles, no interaction)
best_by_spec <- cont_fit3 |>
  filter(is.na(interaction)) |>
  group_by(code, weather, ref_period) |>
  summarise(
    best_p = min(p_value, na.rm = TRUE),
    best_est = estimate[which.min(p_value)],
    best_t = statistic[which.min(p_value)],
    best_fe = fe_profile[which.min(p_value)],
    best_cov = cov_profile[which.min(p_value)],
    .groups = "drop"
  ) |>
  mutate(
    sig_level = case_when(
      best_p < 0.001 ~ "***",
      best_p < 0.01  ~ "**",
      best_p < 0.05  ~ "*",
      best_p < 0.10  ~ "†",
      TRUE           ~ ""
    ),
    ref_period = factor(ref_period, levels = ref_levels)
  )

# Country ordering: by overall best significance
ctry_order <- best_by_spec |>
  group_by(code) |>
  summarise(min_p = min(best_p)) |>
  arrange(min_p) |>
  pull(code)

best_by_spec <- best_by_spec |>
  mutate(code = factor(code, levels = ctry_order))

# Heatmap: significance level with t-stat text
p_sig <- ggplot(best_by_spec,
                aes(x = ref_period, y = code, fill = -log10(best_p))) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.1f%s", best_t, sig_level)),
            size = 2.3, color = "grey15") +
  scale_fill_viridis_c(option = "magma", direction = -1,
                        name = "-log10(p)",
                        limits = c(0, max(-log10(best_by_spec$best_p), na.rm = TRUE))) +
  labs(title = "Temperature coefficient significance by country and reference period",
       subtitle = "Continuous specs, fit3 (FE + covariates), Mean estimand. Best p across FE/cov profiles.\n*** p<0.001  ** p<0.01  * p<0.05  † p<0.10",
       x = "Reference period", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

ggsave(file.path(OUT_SAMPLE, "03a_1_coef_significance.png"), p_sig,
       width = 8, height = max(6, n_countries * 0.3), dpi = 150, bg = "white")

# =============================================================================
# SECTION 3 — FE AND COVARIATE PROFILE COMPARISON
# =============================================================================

# Compare all FE × cov profile combinations across models
fe_cov_summary <- wx_cont |>
  filter(estimand == "Mean", is.na(interaction)) |>
  mutate(
    spec = paste(fe_profile, cov_profile, sep = " + ") |>
      str_replace("NA \\+ NA", "No FE / No cov") |>
      str_replace("NA", "—"),
    model_label = case_when(
      model == "fit1" ~ "Weather only",
      model == "fit2" ~ "Weather + FE",
      model == "fit3" ~ "Weather + FE + Cov"
    )
  ) |>
  group_by(model, model_label, fe_profile, cov_profile, spec) |>
  summarise(
    n = n(),
    n_sig05 = sum(p_value < 0.05, na.rm = TRUE),
    pct_sig = n_sig05 / n * 100,
    med_t = median(abs(statistic), na.rm = TRUE),
    med_se = median(std_error, na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(!is.na(fe_profile) | model == "fit1")

# Bar chart: % significant and median |t| by profile
p_fe_sig <- fe_cov_summary |>
  filter(model == "fit3") |>
  ggplot(aes(x = reorder(spec, pct_sig), y = pct_sig)) +
  geom_col(aes(fill = med_t), width = 0.6) +
  geom_text(aes(label = sprintf("%.0f%% (|t|=%.2f)", pct_sig, med_t)),
            hjust = -0.05, size = 3) +
  scale_fill_viridis_c(option = "mako", direction = -1, name = "Median |t|") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.35))) +
  labs(title = "FE + covariate profile comparison (fit3, Mean, no interaction)",
       subtitle = sprintf("Across %d country × weather spec combinations", max(fe_cov_summary$n)),
       x = NULL, y = "% of specs significant at p<0.05") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank())

# Comparison across fit1/fit2/fit3
p_fe_model <- fe_cov_summary |>
  ggplot(aes(x = reorder(spec, pct_sig), y = pct_sig, fill = model_label)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2", name = "Model") +
  labs(title = "Significance rate by model complexity",
       x = NULL, y = "% significant (p<0.05)") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(), legend.position = "bottom")

p_fe_combined <- p_fe_sig / p_fe_model + plot_layout(heights = c(1, 1.2))

ggsave(file.path(OUT_SAMPLE, "03a_2_fe_cov_comparison.png"), p_fe_combined,
       width = 10, height = 10, dpi = 150, bg = "white")

# =============================================================================
# SECTION 4 — REFERENCE PERIOD RANKING
# =============================================================================

# Country-level best FE/cov per ref_period (no interaction)
ref_ctry <- cont_fit3 |>
  filter(is.na(interaction)) |>
  group_by(code, ref_period) |>
  slice_min(p_value, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(ref_period = factor(ref_period, levels = ref_levels))

# Coefficient forest plot by ref_period
p_ref <- ref_ctry |>
  mutate(code = factor(code, levels = rev(ctry_order))) |>
  ggplot(aes(x = estimate, y = code, color = ref_period)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.6), size = 1.5) +
  geom_errorbarh(aes(xmin = estimate - 1.96 * std_error,
                      xmax = estimate + 1.96 * std_error),
                  height = 0, position = position_dodge(width = 0.6), linewidth = 0.4) +
  scale_color_brewer(palette = "Set1", name = "Ref period") +
  labs(title = "Temperature coefficient by country and reference period",
       subtitle = "Point estimate ± 95% CI. Best FE/cov profile per country × ref_period (fit3, Mean).",
       x = "Coefficient on temperature", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())

ggsave(file.path(OUT_SAMPLE, "03a_3_ref_period_ranking.png"), p_ref,
       width = 10, height = max(6, n_countries * 0.35), dpi = 150, bg = "white")

# =============================================================================
# SECTION 5 — INTERACTION EFFECTS
# =============================================================================

# Interaction coefficients (t:X terms), fit3, Mean
int_fit3 <- wx_int |>
  filter(model == "fit3", estimand == "Mean") |>
  mutate(int_var = sub("^t:", "", term))

# Best interaction per country × int_var (across weather specs, FE/cov)
int_best <- int_fit3 |>
  group_by(code, int_var) |>
  summarise(
    best_p = min(p_value, na.rm = TRUE),
    best_est = estimate[which.min(p_value)],
    best_t = statistic[which.min(p_value)],
    n_sig05 = sum(p_value < 0.05),
    n_total = n(),
    .groups = "drop"
  ) |>
  mutate(
    sig_flag = case_when(
      best_p < 0.001 ~ "***",
      best_p < 0.01  ~ "**",
      best_p < 0.05  ~ "*",
      TRUE           ~ ""
    ),
    code = factor(code, levels = ctry_order)
  )

# Heatmap: interaction significance
int_order <- int_best |>
  group_by(int_var) |>
  summarise(n_sig = sum(best_p < 0.05)) |>
  arrange(desc(n_sig)) |>
  pull(int_var)

p_int <- int_best |>
  mutate(int_var = factor(int_var, levels = rev(int_order))) |>
  ggplot(aes(x = code, y = int_var, fill = -log10(best_p))) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(sig_flag != "",
                                 paste0(sprintf("%.3f", best_est), sig_flag),
                                 "")),
            size = 2, color = "grey15") +
  scale_fill_viridis_c(option = "inferno", direction = -1,
                        name = "-log10(p)",
                        limits = c(0, NA)) +
  labs(title = "Interaction effects: temperature × policy/demographic variables",
       subtitle = "Coefficient on t:X (fit3, Mean). Best p across all weather specs and FE/cov profiles.\nText = coefficient value. *** p<0.001  ** p<0.01  * p<0.05",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUT_SAMPLE, "03a_4_interaction_heatmap.png"), p_int,
       width = 12, height = 5, dpi = 150, bg = "white")

# Interaction direction summary (for markdown)
int_direction <- int_fit3 |>
  filter(p_value < 0.05) |>
  mutate(int_var = sub("^t:", "", term)) |>
  group_by(int_var) |>
  summarise(
    n_positive = sum(estimate > 0),
    n_negative = sum(estimate < 0),
    n_total = n(),
    med_est = median(estimate),
    .groups = "drop"
  ) |>
  arrange(desc(n_total))

# =============================================================================
# SECTION 6 — RIF DISTRIBUTIONAL EFFECTS
# =============================================================================

# RIF quantile coefficients (no interaction, fit3)
rif_coefs <- wx_cont |>
  filter(engine == "rif", model == "fit3", is.na(interaction)) |>
  mutate(
    quantile = tau,
    ref_period = factor(ref_period, levels = ref_levels)
  )

# Best p-value per country × quantile (across weather specs, FE/cov)
rif_best <- rif_coefs |>
  group_by(code, quantile) |>
  slice_min(p_value, n = 1, with_ties = FALSE) |>
  ungroup()

# Quantile coefficient profile: estimate by quantile, faceted by country
rif_profile <- rif_best |>
  mutate(code = factor(code, levels = ctry_order))

p_rif <- ggplot(rif_profile, aes(x = quantile, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.3) +
  geom_ribbon(aes(ymin = estimate - 1.96 * std_error,
                   ymax = estimate + 1.96 * std_error),
              fill = "steelblue", alpha = 0.2) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_point(aes(shape = ifelse(p_value < 0.05, "sig", "ns")),
             color = "steelblue", size = 1.5) +
  scale_shape_manual(values = c("sig" = 16, "ns" = 1), guide = "none") +
  facet_wrap(~code, scales = "free_y", ncol = 5) +
  scale_x_continuous(breaks = seq(0.1, 0.9, 0.2),
                     labels = paste0("p", seq(10, 90, 20))) +
  labs(title = "RIF quantile regression: temperature effect across the welfare distribution",
       subtitle = "Best specification per country × quantile (fit3, no interaction). Filled = p<0.05, hollow = n.s.",
       x = "Quantile", y = "UQR coefficient") +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 8))

ggsave(file.path(OUT_SAMPLE, "03a_5_rif_distributional.png"), p_rif,
       width = 14, height = max(6, ceiling(n_countries / 5) * 2.5), dpi = 150, bg = "white")

# Differential effect summary: is the effect stronger at lower quantiles?
rif_gradient <- rif_best |>
  group_by(code) |>
  summarise(
    est_p10 = estimate[quantile == 0.1],
    est_p50 = estimate[quantile == 0.5],
    est_p90 = estimate[quantile == 0.9],
    gradient = est_p10 - est_p90,
    n_sig = sum(p_value < 0.05),
    .groups = "drop"
  ) |>
  mutate(
    stronger_at = case_when(
      abs(est_p10) > abs(est_p90) * 1.5 ~ "bottom",
      abs(est_p90) > abs(est_p10) * 1.5 ~ "top",
      TRUE ~ "uniform"
    )
  ) |>
  arrange(desc(abs(gradient)))

# =============================================================================
# SECTION 7 — MODEL FIT QUALITY
# =============================================================================

# R² comparison: fit1 vs fit2 vs fit3 for fixest Mean
fit_quality <- stats |>
  filter(engine == "fixest", estimand == "Mean", is.na(interaction)) |>
  select(code, weather, ref_period, model, fe_profile, cov_profile,
         r2, r2_adj, r2_within, aic, n) |>
  mutate(
    ref_period = factor(ref_period, levels = ref_levels),
    model_label = case_when(
      model == "fit1" ~ "Weather only",
      model == "fit2" ~ "Weather + FE",
      model == "fit3" ~ "Weather + FE + Cov"
    ),
    model_label = factor(model_label,
                          levels = c("Weather only", "Weather + FE", "Weather + FE + Cov"))
  )

# Best fit3 per country (highest r2_within)
best_fit <- fit_quality |>
  filter(model == "fit3") |>
  group_by(code) |>
  slice_max(r2_within, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(code = factor(code, levels = ctry_order))

p_r2 <- fit_quality |>
  group_by(code, model_label) |>
  summarise(med_r2 = median(r2, na.rm = TRUE),
            med_r2w = median(r2_within, na.rm = TRUE),
            .groups = "drop") |>
  mutate(code = factor(code, levels = ctry_order)) |>
  ggplot(aes(x = code, y = med_r2, fill = model_label)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_brewer(palette = "Blues", name = "Model") +
  labs(title = "Model R² by country and model complexity",
       subtitle = "Median across weather specs (fixest, Mean, no interaction)",
       x = NULL, y = "R²") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

p_r2w <- best_fit |>
  ggplot(aes(x = code, y = r2_within)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", r2_within)), vjust = -0.3, size = 2.5) +
  labs(title = "Within-R² (best fit3 per country)",
       subtitle = "Variation explained by weather + covariates after absorbing fixed effects",
       x = NULL, y = "Within R²") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_fit_combined <- p_r2 / p_r2w
ggsave(file.path(OUT_SAMPLE, "03a_6_model_fit_quality.png"), p_fit_combined,
       width = 12, height = 10, dpi = 150, bg = "white")

# =============================================================================
# SECTION 8 — LASSO COVARIATE SELECTION
# =============================================================================

# Parse lasso_selected pipe-separated strings
lasso_data <- stats |>
  filter(!is.na(lasso_selected), lasso_selected != "",
         engine == "fixest", estimand == "Mean") |>
  select(code, weather, interaction, lasso_selected)

if (nrow(lasso_data) > 0) {
  lasso_vars <- lasso_data |>
    mutate(vars = str_split(lasso_selected, "\\|")) |>
    unnest(vars) |>
    mutate(vars = str_trim(vars)) |>
    filter(vars != "")

  # Frequency across all models
  lasso_freq <- lasso_vars |>
    count(vars, sort = TRUE) |>
    mutate(pct = n / n_distinct(lasso_data |> unite("id", code, weather, interaction)) * 100)

  # Top 25
  top_lasso <- head(lasso_freq, 25)

  p_lasso <- top_lasso |>
    mutate(vars = fct_reorder(vars, pct)) |>
    ggplot(aes(x = vars, y = pct)) +
    geom_col(aes(fill = pct), width = 0.6) +
    geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.1, size = 2.8) +
    coord_flip() +
    scale_fill_viridis_c(option = "viridis", guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = "LASSO-selected covariates (top 25)",
         subtitle = sprintf("Frequency across %d fixest models with LASSO covariate selection",
                            n_distinct(lasso_data |> unite("id", code, weather, interaction))),
         x = NULL, y = "% of models selecting this variable") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank())

  ggsave(file.path(OUT_SAMPLE, "03a_7_lasso_selection.png"), p_lasso,
         width = 10, height = 8, dpi = 150, bg = "white")

  # Country-level LASSO patterns
  lasso_ctry <- lasso_vars |>
    count(code, vars) |>
    group_by(code) |>
    mutate(pct = n / sum(n) * 100) |>
    ungroup()
}

# =============================================================================
# SECTION 9 — BINNED VS CONTINUOUS COMPARISON
# =============================================================================

# For binned specs, the coefficients are t_bin2..t_bin5 (relative to bin1)
# Compare joint significance and R² between binned and continuous
bin_fit <- stats |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean", is.na(interaction)) |>
  group_by(code, ref_period, wx_type) |>
  summarise(
    med_r2w = median(r2_within, na.rm = TRUE),
    med_aic = median(aic, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(ref_period = factor(ref_period, levels = ref_levels))

bin_compare <- bin_fit |>
  pivot_wider(names_from = wx_type, values_from = c(med_r2w, med_aic)) |>
  mutate(
    r2w_diff = med_r2w_binned - med_r2w_continuous,
    aic_diff = med_aic_binned - med_aic_continuous
  )

# =============================================================================
# SECTION 10 — GENERATE MARKDOWN SUMMARY
# =============================================================================

md <- character()
md_line <- function(...) md <<- c(md, paste0(...))

md_line("# Model Selection Analysis")
md_line("")
md_line("Analysis of `model_coefficients.csv` and `model_fit_stats.csv` from ",
        "`batch/03_run_mod1.R` to evaluate which specifications produce the ",
        "most significant and precise weather–welfare estimates for simulation.")
md_line("")
md_line("Builds on the sample selection analysis (`sample_selection_summary.md`) and ",
        "weather selection analysis (`weather_selection_summary.md`).")
md_line("")
md_line("Generated: ", Sys.Date())
md_line("")

# --- Section 1: Data summary ---
md_line("## 1. Data summary")
md_line("")
md_line("- **", n_countries, "** countries")
md_line("- **", n_wx_specs, "** weather specifications (temperature only)")
md_line("- **Weather specs**: ", paste0("`", sort(unique(coef$weather)), "`", collapse = ", "))
md_line("- **Engines**: fixest (OLS with FE), RIF (unconditional quantile regression)")
md_line("- **Models**: fit1 (weather only), fit2 (weather + FE), fit3 (weather + FE + covariates)")
md_line("- **FE profiles**: year_admin1, year_loc")
md_line("- **Covariate profiles**: hhsize_urban (user-defined), lasso (data-driven)")
md_line("- **Interactions**: urban, electricity, imp_wat_rec, imp_san_rec, ttime_health")
md_line("")

if (nrow(int_na) > 0) {
  md_line("**Unavailable interactions** (variable not in survey data):")
  md_line("")
  int_na_tbl <- int_na |> distinct(sample, interaction)
  for (ctry in unique(int_na_tbl$sample)) {
    vars <- int_na_tbl |> filter(sample == ctry) |> pull(interaction)
    md_line("- **", ctry, "**: ", paste(vars, collapse = ", "))
  }
  md_line("")
}

# --- Section 2: Coefficient significance ---
md_line("## 2. Coefficient significance by weather construction")
md_line("")
md_line("For each country and reference period, we select the best-performing FE/covariate ",
        "profile (lowest p-value on the temperature coefficient). Continuous specifications only, ",
        "fit3 (full model), Mean estimand.")
md_line("")

# Summary: best ref_period per country
best_per_ctry <- best_by_spec |>
  group_by(code) |>
  slice_min(best_p, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(best_p)

md_line("**Best specification per country** (lowest p-value):")
md_line("")
md_line("| Country | Best ref period | Coefficient | t-stat | p-value | FE profile | Covariates |")
md_line("|---------|----------------|-------------|--------|---------|------------|------------|")
for (i in seq_len(nrow(best_per_ctry))) {
  r <- best_per_ctry[i, ]
  md_line("| ", as.character(r$code), " | ", as.character(r$ref_period), " | ",
          sprintf("%.4f", r$best_est), " | ", sprintf("%.2f", r$best_t), " | ",
          sprintf("%.2e", r$best_p), " | ", r$best_fe, " | ", r$best_cov, " |")
}
md_line("")

# Count significance by ref_period
ref_sig <- best_by_spec |>
  group_by(ref_period) |>
  summarise(
    n_sig001 = sum(best_p < 0.001),
    n_sig01 = sum(best_p < 0.01),
    n_sig05 = sum(best_p < 0.05),
    med_t = median(abs(best_t), na.rm = TRUE),
    .groups = "drop"
  )

md_line("**Significance summary by reference period:**")
md_line("")
md_line("| Ref period | p<0.001 | p<0.01 | p<0.05 | Median |t| |")
md_line("|-----------|---------|--------|--------|-----------|")
for (i in seq_len(nrow(ref_sig))) {
  r <- ref_sig[i, ]
  md_line("| ", as.character(r$ref_period), " | ", r$n_sig001, "/", n_countries,
          " | ", r$n_sig01, "/", n_countries, " | ", r$n_sig05, "/", n_countries,
          " | ", sprintf("%.2f", r$med_t), " |")
}
md_line("")

# Best ref_period count
ref_wins <- best_per_ctry |> count(ref_period, name = "n_best") |> arrange(desc(n_best))
md_line("**Reference period ranking** (most often best per country): ",
        paste(sprintf("%s (%d)", ref_wins$ref_period, ref_wins$n_best), collapse = ", "))
md_line("")
md_line("![Coefficient significance](03a_1_coef_significance.png)")
md_line("")

# --- Section 3: FE and covariate profiles ---
md_line("## 3. Fixed effects and covariate selection")
md_line("")
md_line("Comparing FE + covariate profile combinations on the rate of statistically significant ",
        "temperature coefficients and median t-statistic.")
md_line("")

fe_cov_md <- fe_cov_summary |>
  filter(model == "fit3") |>
  arrange(desc(pct_sig))

md_line("| Profile | % significant (p<0.05) | Median |t| | Median SE |")
md_line("|---------|----------------------|-----------|-----------|")
for (i in seq_len(nrow(fe_cov_md))) {
  r <- fe_cov_md[i, ]
  md_line("| ", r$spec, " | ", sprintf("%.1f%%", r$pct_sig),
          " | ", sprintf("%.2f", r$med_t), " | ", sprintf("%.4f", r$med_se), " |")
}
md_line("")

# Which profile wins per country
fe_per_ctry <- best_per_ctry |> count(best_fe, best_cov) |> arrange(desc(n))
md_line("**Best FE/cov profile per country** (most significant model): ",
        paste(sprintf("%s + %s (%d countries)", fe_per_ctry$best_fe, fe_per_ctry$best_cov, fe_per_ctry$n),
              collapse = "; "))
md_line("")
md_line("![FE comparison](03a_2_fe_cov_comparison.png)")
md_line("")

# --- Section 4: Coefficient precision ---
md_line("## 4. Coefficient precision and direction")
md_line("")
md_line("Forest plot showing point estimates and 95% confidence intervals for the temperature ",
        "coefficient by country and reference period (best FE/cov profile per combination).")
md_line("")

# Direction summary
n_negative <- sum(best_per_ctry$best_est < 0, na.rm = TRUE)
n_positive <- sum(best_per_ctry$best_est > 0, na.rm = TRUE)
md_line(paste0("**Direction**: ", n_negative, "/", n_countries,
               " countries show negative temperature-welfare relationship ",
               "(higher temperature = lower welfare); ", n_positive, "/", n_countries,
               " show positive."))
md_line("")

pos_countries <- best_per_ctry |> filter(best_est > 0) |> pull(code) |> as.character()
if (length(pos_countries) > 0) {
  md_line("Countries with positive coefficients: ", paste0("`", pos_countries, "`", collapse = ", "),
          " — likely reflecting cold-climate contexts where warming benefits welfare, ",
          "or compositional effects in the data.")
  md_line("")
}

md_line("![Coefficient forest plot](03a_3_ref_period_ranking.png)")
md_line("")

# --- Section 5: Interactions ---
md_line("## 5. Interaction effects (policy-relevant heterogeneity)")
md_line("")
md_line("Interaction terms (temperature × policy variable) capture whether policy interventions ",
        "modify the weather–welfare relationship. A positive interaction with a protective factor ",
        "(e.g., electricity) means the policy buffers the negative temperature effect.")
md_line("")

# Overall interaction ranking
int_rank <- int_best |>
  group_by(int_var) |>
  summarise(
    n_countries = n_distinct(code),
    n_sig05 = sum(best_p < 0.05),
    n_sig01 = sum(best_p < 0.01),
    med_est = median(best_est, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(n_sig05))

md_line("**Interaction ranking** (countries with at least one p<0.05 across all specs):")
md_line("")
md_line("| Interaction | Countries with data | Significant (p<0.05) | Significant (p<0.01) | Median coefficient |")
md_line("|-------------|--------------------|--------------------|--------------------|--------------------|")
for (i in seq_len(nrow(int_rank))) {
  r <- int_rank[i, ]
  md_line("| ", r$int_var, " | ", r$n_countries, " | ", r$n_sig05, " | ", r$n_sig01,
          " | ", sprintf("%.4f", r$med_est), " |")
}
md_line("")

# Country-specific interaction table
md_line("**Significant interactions by country** (p<0.05, showing best coefficient):")
md_line("")
md_line("| Country | Interaction | Coefficient | p-value | Best weather spec |")
md_line("|---------|-------------|-------------|---------|-------------------|")
int_sig <- int_best |> filter(best_p < 0.05) |> arrange(code, int_var)
for (i in seq_len(nrow(int_sig))) {
  r <- int_sig[i, ]
  wx_str <- int_fit3 |>
    filter(code == as.character(r$code), sub("^t:", "", term) == r$int_var) |>
    slice_min(p_value, n = 1) |>
    pull(weather)
  md_line("| ", as.character(r$code), " | ", r$int_var, " | ",
          sprintf("%.4f", r$best_est), " | ", sprintf("%.2e", r$best_p),
          " | ", wx_str[1], " |")
}
md_line("")

# Direction of interactions
if (nrow(int_direction) > 0) {
  md_line("**Interaction direction** (among significant results):")
  md_line("")
  for (i in seq_len(nrow(int_direction))) {
    r <- int_direction[i, ]
    dir_str <- ifelse(r$n_positive > r$n_negative,
                       sprintf("predominantly positive (%d+, %d−)", r$n_positive, r$n_negative),
                       sprintf("predominantly negative (%d+, %d−)", r$n_positive, r$n_negative))
    md_line(sprintf("- **%s**: %s, median = %.4f", r$int_var, dir_str, r$med_est))
  }
  md_line("")
}

md_line("![Interaction heatmap](03a_4_interaction_heatmap.png)")
md_line("")

# --- Section 6: RIF distributional effects ---
md_line("## 6. Distributional effects (RIF quantile regression)")
md_line("")
md_line("RIF (recentered influence function) regressions estimate the effect of temperature ",
        "at each quantile of the welfare distribution. This reveals whether weather shocks ",
        "disproportionately affect poorer households (lower quantiles).")
md_line("")

# Gradient summary
md_line("**Differential impact across the distribution:**")
md_line("")
md_line("| Country | Coef p10 | Coef p50 | Coef p90 | Gradient (p10−p90) | Pattern | # Sig quantiles |")
md_line("|---------|----------|----------|----------|--------------------|---------|-----------------|")
for (i in seq_len(nrow(rif_gradient))) {
  r <- rif_gradient[i, ]
  md_line("| ", r$code, " | ", sprintf("%.4f", r$est_p10), " | ",
          sprintf("%.4f", r$est_p50), " | ", sprintf("%.4f", r$est_p90), " | ",
          sprintf("%.4f", r$gradient), " | ", r$stronger_at, " | ", r$n_sig, "/9 |")
}
md_line("")

# Count patterns
n_bottom <- sum(rif_gradient$stronger_at == "bottom")
n_top <- sum(rif_gradient$stronger_at == "top")
n_uniform <- sum(rif_gradient$stronger_at == "uniform")
md_line(paste0("**Pattern summary**: ", n_bottom, " countries show stronger effects at the bottom of the ",
               "distribution, ", n_top, " at the top, ", n_uniform, " roughly uniform."))
md_line("")

bottom_countries <- rif_gradient |> filter(stronger_at == "bottom") |> pull(code)
if (length(bottom_countries) > 0) {
  md_line("Countries where poorer households are more affected: ",
          paste0("`", bottom_countries, "`", collapse = ", "))
  md_line("")
}

md_line("![RIF distributional effects](03a_5_rif_distributional.png)")
md_line("")

# --- Section 7: Model fit ---
md_line("## 7. Model fit quality")
md_line("")
md_line("R² and within-R² for the best fit3 model per country (fixest, Mean, no interaction).")
md_line("")

md_line("| Country | R² | Within R² | FE | Covariates | Weather spec | N |")
md_line("|---------|-----|-----------|-----|------------|-------------|---|")
for (i in seq_len(nrow(best_fit))) {
  r <- best_fit[i, ]
  md_line("| ", as.character(r$code), " | ", sprintf("%.3f", r$r2), " | ",
          sprintf("%.3f", r$r2_within), " | ", r$fe_profile, " | ", r$cov_profile,
          " | ", r$weather, " | ", format(r$n, big.mark = ","), " |")
}
md_line("")
md_line("![Model fit quality](03a_6_model_fit_quality.png)")
md_line("")

# --- Section 8: LASSO selection ---
if (nrow(lasso_data) > 0) {
  md_line("## 8. LASSO covariate selection")
  md_line("")
  md_line("Variables most frequently selected by LASSO across all country × weather spec × ",
          "interaction combinations (fixest models).")
  md_line("")
  md_line("| Variable | % of models | Count |")
  md_line("|----------|------------|-------|")
  for (i in seq_len(min(15, nrow(lasso_freq)))) {
    r <- lasso_freq[i, ]
    md_line("| ", r$vars, " | ", sprintf("%.0f%%", r$pct), " | ", r$n, " |")
  }
  md_line("")
  md_line("![LASSO selection](03a_7_lasso_selection.png)")
  md_line("")
}

# --- Section 9: Binned vs continuous ---
md_line("## 9. Binned vs continuous specifications")
md_line("")
md_line("Comparing within-R² and AIC between binned and continuous temperature specifications ",
        "(fit3, no interaction).")
md_line("")

bin_summary_md <- bin_compare |>
  group_by(ref_period) |>
  summarise(
    med_diff_r2w = median(r2w_diff, na.rm = TRUE),
    n_binned_better = sum(r2w_diff > 0, na.rm = TRUE),
    n_cont_better = sum(r2w_diff < 0, na.rm = TRUE),
    .groups = "drop"
  )

md_line("| Ref period | Median Δ within-R² (binned − continuous) | Binned better | Continuous better |")
md_line("|-----------|------------------------------------------|---------------|-------------------|")
for (i in seq_len(nrow(bin_summary_md))) {
  r <- bin_summary_md[i, ]
  md_line("| ", as.character(r$ref_period), " | ", sprintf("%.4f", r$med_diff_r2w),
          " | ", r$n_binned_better, "/", n_countries, " | ", r$n_cont_better, "/", n_countries, " |")
}
md_line("")

# --- Section 10: Recommendations ---
md_line("## 10. Recommendations for simulation")
md_line("")

md_line("### Weather construction")
md_line("")
best_ref <- ref_wins$ref_period[1]
md_line(paste0("**Recommended reference period: `", as.character(best_ref),
               "`** -- most often the best-performing specification (",
               ref_wins$n_best[1], "/", n_countries,
               " countries). Short reference periods (1m, 3m) capture acute shocks; longer ",
               "periods (6m, 12m) capture cumulative exposure. Both are informative."))
md_line("")
md_line("Multiple reference periods should be run in simulation for robustness. The continuous ",
        "specification is preferred for simulation (simpler extrapolation); binned specifications ",
        "are useful for checking non-linearity.")
md_line("")

md_line("### Fixed effects")
md_line("")
best_fe_str <- paste(fe_per_ctry$best_fe[1], "+", fe_per_ctry$best_cov[1])
md_line(paste0("**Most common best profile: `", best_fe_str, "`** (",
               fe_per_ctry$n[1], "/", n_countries, " countries). "))
md_line("Both `year_admin1` and `year_loc` FE profiles produce significant results. ",
        "`year_admin1` is more common as the best choice, likely because `year_loc` absorbs ",
        "too much of the weather signal in countries with less spatial variation.")
md_line("")

md_line("### Covariate selection")
md_line("")
lasso_wins <- fe_per_ctry |> filter(best_cov == "lasso") |> pull(n)
hhsize_wins <- fe_per_ctry |> filter(best_cov == "hhsize_urban") |> pull(n)
lasso_wins <- if (length(lasso_wins) == 0) 0L else sum(lasso_wins)
hhsize_wins <- if (length(hhsize_wins) == 0) 0L else sum(hhsize_wins)
md_line(paste0("LASSO covariate selection is best in ", lasso_wins,
               " countries; user-defined (hhsize + urban) in ", hhsize_wins,
               ". LASSO is recommended as default -- it selects relevant controls data-adaptively ",
               "and avoids over-controlling on variables correlated with the weather channel."))
md_line("")

md_line("### Policy interactions")
md_line("")
md_line("Key interactions for policy scenarios, ranked by cross-country significance:")
md_line("")
for (i in seq_len(nrow(int_rank))) {
  r <- int_rank[i, ]
  ctry_list <- int_sig |> filter(int_var == r$int_var) |> pull(code) |> as.character()
  md_line(sprintf("1. **%s** — significant in %d/%d countries: %s",
                  r$int_var, r$n_sig05, r$n_countries,
                  paste(ctry_list, collapse = ", ")))
}
md_line("")

md_line("### Distributional targeting")
md_line("")
if (n_bottom > 0) {
  md_line(paste0("RIF regressions show that in ", n_bottom, "/", n_countries,
                 " countries, poorer households (lower quantiles) ",
                 "are more affected by temperature shocks than wealthier ones. ",
                 "This supports targeting social protection to the poorest."))
  md_line("")
  md_line("Countries with pro-poor vulnerability: ",
          paste0("`", bottom_countries, "`", collapse = ", "))
  md_line("")
}

md_line("### Country simulation readiness")
md_line("")
md_line("Based on coefficient significance and model fit, countries are grouped by simulation readiness:")
md_line("")

# Tier assignment
tier_a <- best_per_ctry |> filter(best_p < 0.01) |> pull(code) |> as.character() |> sort()
tier_b <- best_per_ctry |> filter(best_p >= 0.01, best_p < 0.05) |> pull(code) |> as.character() |> sort()
tier_c <- best_per_ctry |> filter(best_p >= 0.05) |> pull(code) |> as.character() |> sort()

md_line("**Tier A** (p<0.01, strong weather signal): ",
        paste0("`", tier_a, "`", collapse = ", "),
        sprintf(" (%d countries)", length(tier_a)))
md_line("")
md_line("**Tier B** (p<0.05, moderate signal): ",
        if (length(tier_b) > 0) paste0("`", tier_b, "`", collapse = ", ") else "none",
        sprintf(" (%d countries)", length(tier_b)))
md_line("")
md_line("**Tier C** (p≥0.05, weak/no signal): ",
        if (length(tier_c) > 0) paste0("`", tier_c, "`", collapse = ", ") else "none",
        sprintf(" (%d countries)", length(tier_c)))
md_line("")

writeLines(md, file.path(OUT_SAMPLE, "model_selection_summary.md"))
cat(sprintf("\n=== Saved: %s ===\n", file.path(OUT_SAMPLE, "model_selection_summary.md")))
cat(sprintf("=== Plots saved to: %s ===\n", OUT_SAMPLE))
