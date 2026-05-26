# =============================================================================
# batch/03a_model_selection.R
#
# Analysis of model_coefficients.csv and model_fit_stats.csv from
# batch/03_run_mod1.R (temperature variable only).  Evaluates which weather
# construction, reference period, covariate selection, and interaction terms
# produce the most significant and precise estimates under location-year FE,
# to inform simulation configuration in batch/04_run_sim.R.
#
# Identification strategy: year_loc FE is the preferred specification because
# it absorbs all time-invariant location characteristics, identifying the
# temperature effect from within-location temporal variation only.  Countries
# where this signal is too weak are flagged as "admin1-only".
#
# Reference period: 1to1m (1-month pre-interview average) is locked in as the
# primary specification (best power under year_loc).  1to3m is retained as a
# robustness check.  All downstream sections (3-9) use the primary ref period
# for consistent cross-country interpretation.
#
# Outputs (all under OUT_DIR/sample_selection/):
#   03a_1_coef_significance.png      year_loc significance + admin1 fallback
#   03a_2_covariate_stability.png    LASSO vs user-defined: stability + fit
#   03a_3_ref_period_ranking.png     coefficient precision by reference period
#   03a_4a_interaction_continuous.png continuous interaction heatmap
#   03a_4b_binned_highest.png        highest bin coefficients + interactions
#   03a_5_rif_distributional.png     RIF quantile coefficient profiles
#   03a_6_model_fit_quality.png      R-squared and within-R-squared
#   03a_7_lasso_selection.png        LASSO-selected covariate frequency
#   model_selection_summary.md       narrative summary
#
# Usage: source("batch/03a_model_selection.R")
# =============================================================================

library(tidyverse)
library(patchwork)

# =============================================================================
# SECTION 1 — CONFIGURATION & FE DECISION
# =============================================================================

OUT_DIR    <- Sys.getenv("WISEAPP_RESULTS_PATH")
MOD_DIR    <- file.path(OUT_DIR, "model_fit")
OUT_SAMPLE <- file.path(OUT_DIR, "sample_selection")
dir.create(OUT_SAMPLE, showWarnings = FALSE, recursive = TRUE)

coef  <- read_csv(file.path(MOD_DIR, "model_coefficients.csv"), show_col_types = FALSE)
stats <- read_csv(file.path(MOD_DIR, "model_fit_stats.csv"), show_col_types = FALSE)

# Restrict to countries with >= 2 survey waves (consistent with 01a_sample_selection.R)
welfare_agg      <- read_csv(file.path(OUT_DIR, "survey_stats", "welfare_aggregates.csv"), show_col_types = FALSE)
multi_wave_codes <- welfare_agg |>
  dplyr::count(code) |>
  dplyr::filter(n >= 2) |>
  dplyr::pull(code)
coef  <- coef  |> dplyr::filter(code %in% multi_wave_codes)
stats <- stats |> dplyr::filter(code %in% multi_wave_codes)
cat(sprintf("Restricting to %d countries with 2+ survey waves\n\n", length(multi_wave_codes)))

int_na <- tryCatch(
  read_csv(file.path(MOD_DIR, "_interactions_not_available.csv"), show_col_types = FALSE),
  error = function(e) tibble(spec_label = character(), reason = character(),
                             sample = character(), interaction = character())
)

# --- Parse weather spec ---
parse_wx <- function(df) {
  df |> mutate(
    ref_period = str_extract(weather, "\\d+to\\d+m"),
    wx_type    = ifelse(grepl("_binn_", weather), "binned", "continuous")
  )
}
coef  <- parse_wx(coef)
stats <- parse_wx(stats)

ref_levels <- c("1to1m", "1to3m", "1to6m", "1to12m")

# --- Default FE decision ---
# year_loc absorbs all time-invariant location characteristics; identification
# comes from within-location temporal variation in weather only.
DEFAULT_FE <- "year_loc"

# --- Reference period decision ---
# 1to1m has strongest statistical power under year_loc (highest sig rate,
# highest median |t|).  Lock it in as the primary specification for all
# downstream sections to keep interpretation consistent across countries.
# 1to3m retained as robustness check.
DEFAULT_REF <- "1to3m"
ROBUST_REF  <- "1to1m"

wx_cont <- coef |> filter(term == "t")
wx_int  <- coef |> filter(grepl("^t:", term))

n_countries <- n_distinct(coef$code)
countries   <- sort(unique(coef$code))
n_wx_specs  <- n_distinct(coef$weather)

cat(sprintf("Model results: %d countries, %d weather specs\n", n_countries, n_wx_specs))
cat(sprintf("Default FE: %s (within-location temporal identification)\n", DEFAULT_FE))
cat(sprintf("Countries: %s\n\n", paste(countries, collapse = ", ")))

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
# SECTION 2 — COEFFICIENT SIGNIFICANCE UNDER year_loc FE
# =============================================================================

# Continuous weather coef, fit3 (full model), Mean estimand, year_loc FE
loc_fit3 <- wx_cont |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean",
         fe_profile == DEFAULT_FE, is.na(interaction))

# Best covariate profile per country × ref_period (under year_loc)
loc_best <- loc_fit3 |>
  group_by(code, weather, ref_period) |>
  slice_min(p_value, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ "†",
      TRUE            ~ ""
    ),
    ref_period = factor(ref_period, levels = ref_levels)
  )

# Best spec per country (under year_loc)
loc_best_ctry <- loc_best |>
  group_by(code) |>
  slice_min(p_value, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(p_value)

# --- Compare to year_admin1 to flag admin1-only countries ---
adm_fit3 <- wx_cont |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean",
         fe_profile == "year_admin1", is.na(interaction))

adm_best_ctry <- adm_fit3 |>
  group_by(code) |>
  slice_min(p_value, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(code, adm_est = estimate, adm_se = std_error, adm_t = statistic,
         adm_p = p_value, adm_cov = cov_profile, adm_wx = weather)

fe_compare <- loc_best_ctry |>
  select(code, loc_est = estimate, loc_se = std_error, loc_t = statistic,
         loc_p = p_value, loc_cov = cov_profile, loc_wx = weather) |>
  left_join(adm_best_ctry, by = "code") |>
  mutate(
    loc_sig = loc_p < 0.05,
    adm_sig = adm_p < 0.05,
    status = case_when(
      loc_sig            ~ "year_loc sig",
      !loc_sig & adm_sig ~ "admin1 only",
      TRUE               ~ "not significant"
    )
  )

n_loc_sig   <- sum(fe_compare$status == "year_loc sig")
n_adm_only  <- sum(fe_compare$status == "admin1 only")
n_neither   <- sum(fe_compare$status == "not significant")
adm_only    <- fe_compare |> filter(status == "admin1 only") |> pull(code) |> sort()
neither     <- fe_compare |> filter(status == "not significant") |> pull(code) |> sort()

cat(sprintf("year_loc significant: %d | admin1-only: %d | neither: %d\n",
            n_loc_sig, n_adm_only, n_neither))
if (length(adm_only) > 0)
  cat(sprintf("  admin1-only countries: %s\n", paste(adm_only, collapse = ", ")))

# Country ordering: year_loc p-value, admin1-only at bottom
ctry_order <- c(
  fe_compare |> filter(status == "year_loc sig") |> arrange(loc_p) |> pull(code),
  fe_compare |> filter(status == "admin1 only") |> arrange(adm_p) |> pull(code),
  fe_compare |> filter(status == "not significant") |> arrange(loc_p) |> pull(code)
)

# --- Plot: dual heatmap (year_loc left, year_admin1 right for comparison) ---
loc_plot <- loc_best |>
  mutate(code = factor(code, levels = ctry_order), fe_label = "year_loc (preferred)")

adm_best <- adm_fit3 |>
  group_by(code, weather, ref_period) |>
  slice_min(p_value, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01  ~ "**",
      p_value < 0.05  ~ "*",
      p_value < 0.10  ~ "†",
      TRUE            ~ ""
    ),
    ref_period = factor(ref_period, levels = ref_levels),
    code = factor(code, levels = ctry_order),
    fe_label = "year_admin1 (comparison)"
  )

dual_plot <- bind_rows(loc_plot, adm_best)

p_sig <- ggplot(dual_plot, aes(x = ref_period, y = code, fill = -log10(p_value))) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = paste0(sprintf("%.1f", statistic), sig)),
            size = 2, color = "grey15") +
  facet_wrap(~fe_label) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "-log10(p)") +
  labs(title = "Temperature coefficient significance: year_loc vs year_admin1",
       subtitle = "t-stat shown. *** p<0.001  ** p<0.01  * p<0.05  † p<0.10. Best cov profile per cell.",
       x = "Reference period", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

ggsave(file.path(OUT_SAMPLE, "03a_1_coef_significance.png"), p_sig,
       width = 12, height = max(6, n_countries * 0.3), dpi = 150, bg = "white")

# =============================================================================
# SECTION 3 — COVARIATE SELECTION: STABILITY + FIT
# =============================================================================

# Under year_loc FE: compare hhsize_urban vs lasso on (a) weather coef
# stability and (b) within-R2 improvement.  Decision rule: use lasso if it
# improves fit AND the weather coefficient is stable (ratio near 1).

cov_pair <- wx_cont |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean",
         fe_profile == DEFAULT_FE, is.na(interaction),
         wx_type == "continuous", ref_period == DEFAULT_REF) |>
  select(code, weather, ref_period, cov_profile, estimate, std_error, p_value)

cov_wide <- cov_pair |>
  pivot_wider(names_from = cov_profile, values_from = c(estimate, std_error, p_value),
              names_sep = "_") |>
  drop_na(estimate_lasso, `estimate_hhsize_urban`)

# Stability metric: ratio of lasso coef to hhsize_urban coef
cov_wide <- cov_wide |>
  mutate(
    coef_ratio = estimate_lasso / `estimate_hhsize_urban`,
    coef_diff  = estimate_lasso - `estimate_hhsize_urban`,
    stable     = abs(coef_ratio - 1) < 0.25
  )

# Within-R2 comparison
r2_pair <- stats |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean",
         fe_profile == DEFAULT_FE, is.na(interaction), wx_type == "continuous",
         ref_period == DEFAULT_REF) |>
  select(code, weather, ref_period, cov_profile, r2_within, aic)

r2_wide <- r2_pair |>
  pivot_wider(names_from = cov_profile, values_from = c(r2_within, aic),
              names_sep = "_") |>
  drop_na(r2_within_lasso, `r2_within_hhsize_urban`) |>
  mutate(
    r2w_gain = r2_within_lasso - `r2_within_hhsize_urban`,
    aic_gain = aic_lasso - `aic_hhsize_urban`,
    lasso_better_r2 = r2w_gain > 0,
    lasso_better_aic = aic_gain < 0
  )

# Country-level summary
cov_ctry <- cov_wide |>
  group_by(code) |>
  summarise(
    med_ratio = median(coef_ratio, na.rm = TRUE),
    pct_stable = mean(stable, na.rm = TRUE) * 100,
    med_lasso_est  = median(estimate_lasso, na.rm = TRUE),
    med_user_est   = median(`estimate_hhsize_urban`, na.rm = TRUE),
    .groups = "drop"
  )

r2_ctry <- r2_wide |>
  group_by(code) |>
  summarise(
    med_r2w_gain = median(r2w_gain, na.rm = TRUE),
    pct_lasso_better = mean(lasso_better_r2, na.rm = TRUE) * 100,
    med_aic_gain = median(aic_gain, na.rm = TRUE),
    .groups = "drop"
  )

cov_decision <- cov_ctry |>
  left_join(r2_ctry, by = "code") |>
  mutate(
    use_lasso = pct_stable >= 75 & med_r2w_gain > 0,
    code = factor(code, levels = ctry_order)
  )

# --- Plot: scatter of stability vs fit gain ---
p_cov_scatter <- cov_decision |>
  ggplot(aes(x = med_r2w_gain * 100, y = med_ratio)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = c(0.75, 1.25), linetype = "dotted", color = "grey70") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_point(aes(color = use_lasso), size = 3) +
  ggrepel::geom_text_repel(aes(label = code), size = 2.8, max.overlaps = 25) +
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "#d62728"),
                     labels = c("TRUE" = "Use LASSO", "FALSE" = "Keep user-defined"),
                     name = NULL) +
  labs(title = paste0("Covariate selection: LASSO vs user-defined (", DEFAULT_REF, ", ", DEFAULT_FE, ")"),
       subtitle = paste0("Stable = coef ratio within 0.75-1.25. ",
                          "Right of zero = LASSO improves within-R²."),
       x = "Median within-R² gain from LASSO (percentage points)",
       y = "Median coefficient ratio (LASSO / user-defined)") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

# --- Plot: country-level bar of stability + gain ---
p_cov_bars <- cov_decision |>
  select(code, `Coef stability (%)` = pct_stable,
         `Specs where LASSO R² better (%)` = pct_lasso_better) |>
  pivot_longer(-code, names_to = "metric", values_to = "value") |>
  ggplot(aes(x = code, y = value, fill = metric)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_hline(yintercept = 75, linetype = "dashed", color = "grey40") +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  labs(title = "LASSO adoption criteria by country",
       subtitle = "Dashed line = 75% threshold for adoption",
       x = NULL, y = "%") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

p_cov_combined <- p_cov_scatter / p_cov_bars + plot_layout(heights = c(1.2, 1))
ggsave(file.path(OUT_SAMPLE, "03a_2_covariate_stability.png"), p_cov_combined,
       width = 12, height = 12, dpi = 150, bg = "white")

# =============================================================================
# SECTION 4 — COEFFICIENT FOREST PLOT (faceted by ref period)
# =============================================================================

# Continuous OLS, LASSO covariates, year_loc FE, all ref periods
forest_data <- wx_cont |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean",
         fe_profile == DEFAULT_FE, cov_profile == "lasso",
         wx_type == "continuous", is.na(interaction)) |>
  mutate(
    ref_period = factor(ref_period, levels = ref_levels),
    sig = p_value < 0.05
  )

coef_order <- forest_data |>
  filter(ref_period == DEFAULT_REF) |>
  arrange(estimate) |>
  pull(code) |>
  unique()

forest_data <- forest_data |>
  mutate(code = factor(code, levels = rev(coef_order)))

p_ref <- ggplot(forest_data, aes(x = estimate, y = code)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = estimate - 1.96 * std_error,
                     xmax = estimate + 1.96 * std_error),
                width = 0, linewidth = 0.4, color = "grey60",
                orientation = "y") +
  geom_point(aes(color = sig), size = 2) +
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey70"),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "n.s."),
                     name = NULL) +
  facet_wrap(~ref_period, nrow = 1, scales = "free_x") +
  labs(title = paste0("Temperature coefficient by country and reference period (",
                      DEFAULT_FE, ", LASSO, OLS)"),
       subtitle = paste0("Point estimate +/- 95% CI. Continuous spec, fit3, Mean, no interactions. ",
                         "Sorted by ", DEFAULT_REF, " coefficient."),
       x = "Coefficient on temperature", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave(file.path(OUT_SAMPLE, "03a_3_ref_period_ranking.png"), p_ref,
       width = 16, height = max(6, n_countries * 0.35), dpi = 150, bg = "white")

# =============================================================================
# SECTION 5a — INTERACTION EFFECTS: CONTINUOUS (year_loc)
# =============================================================================

int_fit3 <- wx_int |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean",
         fe_profile == DEFAULT_FE, ref_period == DEFAULT_REF,
         wx_type == "continuous") |>
  mutate(int_var = sub("^t:", "", term))

int_best <- int_fit3 |>
  group_by(code, int_var) |>
  summarise(
    best_p   = min(p_value, na.rm = TRUE),
    best_est = estimate[which.min(p_value)],
    best_t   = statistic[which.min(p_value)],
    best_wx  = weather[which.min(p_value)],
    n_sig05  = sum(p_value < 0.05),
    n_total  = n(),
    .groups  = "drop"
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

int_order <- int_best |>
  group_by(int_var) |>
  summarise(n_sig = sum(best_p < 0.05)) |>
  arrange(desc(n_sig)) |>
  pull(int_var)

int_plot_data <- int_best |>
  mutate(
    int_var = factor(int_var, levels = rev(int_order)),
    signed_logp = sign(best_est) * -log10(best_p)
  )

logp_max <- max(abs(int_plot_data$signed_logp), na.rm = TRUE)

p_int <- int_plot_data |>
  ggplot(aes(x = code, y = int_var, fill = signed_logp)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(sig_flag != "",
                                paste0(sprintf("%.3f", best_est), sig_flag), "")),
            size = 2, color = "grey15") +
  scale_fill_gradient2(low = "#b2182b", mid = "grey95", high = "#2166ac",
                       midpoint = 0, limits = c(-logp_max, logp_max),
                       name = "signed\n-log10(p)") +
  labs(title = paste0("Continuous: interaction effects (", DEFAULT_FE, ", ", DEFAULT_REF, ")"),
       subtitle = paste0("Blue = positive (protective), Red = negative (amplifying). ",
                         "Coef on t:X shown for sig cells.\n",
                         "*** p<0.001  ** p<0.01  * p<0.05"),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(OUT_SAMPLE, "03a_4a_interaction_continuous.png"), p_int,
       width = 12, height = 5, dpi = 150, bg = "white")

int_direction <- int_fit3 |>
  filter(p_value < 0.05) |>
  group_by(int_var) |>
  summarise(
    n_positive = sum(estimate > 0),
    n_negative = sum(estimate < 0),
    n_total    = n(),
    med_est    = median(estimate),
    .groups    = "drop"
  ) |>
  arrange(desc(n_total))

# =============================================================================
# SECTION 5b — BINNED: HIGHEST BIN COEFFICIENTS + INTERACTIONS (year_loc)
# =============================================================================

# Highest temperature bin = term containing "Inf]"
binn_all <- coef |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean",
         fe_profile == DEFAULT_FE, ref_period == DEFAULT_REF,
         wx_type == "binned", grepl("Inf]", term))

# Main effect: no ":" in term
binn_main <- binn_all |>
  filter(!grepl(":", term), is.na(interaction)) |>
  mutate(
    bin_label = term,
    sig = p_value < 0.05
  )

# Best cov profile per country
binn_best <- binn_main |>
  group_by(code) |>
  slice_min(p_value, n = 1, with_ties = FALSE) |>
  ungroup()

binn_coef_order <- binn_best |> arrange(estimate) |> pull(code)

binn_best <- binn_best |>
  mutate(code = factor(code, levels = rev(binn_coef_order)))

p_binn_coef <- ggplot(binn_best, aes(x = estimate, y = code)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = estimate - 1.96 * std_error,
                     xmax = estimate + 1.96 * std_error),
                width = 0, linewidth = 0.4, color = "grey60",
                orientation = "y") +
  geom_point(aes(color = sig), size = 2.5) +
  geom_text(aes(label = bin_label), hjust = -0.15, size = 2.2, color = "grey40") +
  scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey70"),
                     labels = c("TRUE" = "p < 0.05", "FALSE" = "n.s."),
                     name = NULL) +
  labs(title = paste0("Binned: highest temperature bin coefficient (",
                      DEFAULT_FE, ", ", DEFAULT_REF, ")"),
       subtitle = "Point estimate +/- 95% CI. Best cov profile per country (fit3, Mean, no interaction). Bin label shown.",
       x = "Coefficient on highest temperature bin", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", panel.grid.major.y = element_blank())

# Interaction effects for highest bin
binn_int <- binn_all |>
  filter(grepl(":", term)) |>
  mutate(int_var = sub(".*:", "", term))

binn_int_best <- binn_int |>
  group_by(code, int_var) |>
  summarise(
    best_p   = min(p_value, na.rm = TRUE),
    best_est = estimate[which.min(p_value)],
    .groups  = "drop"
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

binn_int_order <- binn_int_best |>
  group_by(int_var) |>
  summarise(n_sig = sum(best_p < 0.05)) |>
  arrange(desc(n_sig)) |>
  pull(int_var)

binn_int_plot <- binn_int_best |>
  mutate(
    int_var = factor(int_var, levels = rev(binn_int_order)),
    signed_logp = sign(best_est) * -log10(best_p)
  )

binn_logp_max <- max(abs(binn_int_plot$signed_logp), na.rm = TRUE)

p_binn_int <- binn_int_plot |>
  ggplot(aes(x = code, y = int_var, fill = signed_logp)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(sig_flag != "",
                                paste0(sprintf("%.3f", best_est), sig_flag), "")),
            size = 2, color = "grey15") +
  scale_fill_gradient2(low = "#b2182b", mid = "grey95", high = "#2166ac",
                       midpoint = 0, limits = c(-binn_logp_max, binn_logp_max),
                       name = "signed\n-log10(p)") +
  labs(title = paste0("Binned: highest bin x policy interactions (",
                      DEFAULT_FE, ", ", DEFAULT_REF, ")"),
       subtitle = paste0("Blue = positive (protective), Red = negative (amplifying). ",
                         "Coef shown for sig cells.\n",
                         "*** p<0.001  ** p<0.01  * p<0.05"),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

p_binn_combined <- p_binn_coef / p_binn_int + plot_layout(heights = c(1, 0.6))
ggsave(file.path(OUT_SAMPLE, "03a_4b_binned_highest.png"), p_binn_combined,
       width = 12, height = max(10, n_countries * 0.35 + 5), dpi = 150, bg = "white")

# =============================================================================
# SECTION 6 — RIF DISTRIBUTIONAL EFFECTS (year_loc)
# =============================================================================

rif_coefs <- wx_cont |>
  filter(engine == "rif", model == "fit3", fe_profile == DEFAULT_FE, is.na(interaction),
         ref_period == DEFAULT_REF, wx_type == "continuous") |>
  mutate(quantile = tau)

rif_best <- rif_coefs |>
  group_by(code, quantile) |>
  slice_min(p_value, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(code = factor(code, levels = ctry_order))

p_rif <- ggplot(rif_best, aes(x = quantile, y = estimate)) +
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
  labs(title = paste0("RIF quantile regression: temperature effect across welfare distribution (",
                      DEFAULT_FE, ", ", DEFAULT_REF, ")"),
       subtitle = "Best cov profile per country x quantile (fit3, no interaction). Filled = p<0.05.",
       x = "Quantile", y = "UQR coefficient") +
  theme_minimal(base_size = 10) +
  theme(strip.text = element_text(face = "bold", size = 8))

ggsave(file.path(OUT_SAMPLE, "03a_5_rif_distributional.png"), p_rif,
       width = 14, height = max(6, ceiling(n_countries / 5) * 2.5), dpi = 150, bg = "white")

rif_gradient <- rif_best |>
  group_by(code) |>
  summarise(
    est_p10  = estimate[quantile == 0.1],
    est_p50  = estimate[quantile == 0.5],
    est_p90  = estimate[quantile == 0.9],
    gradient = est_p10 - est_p90,
    n_sig    = sum(p_value < 0.05),
    .groups  = "drop"
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
# SECTION 7 — MODEL FIT QUALITY (year_loc)
# =============================================================================

fit_quality <- stats |>
  filter(engine == "fixest", estimand == "Mean", fe_profile == DEFAULT_FE,
         is.na(interaction), ref_period == DEFAULT_REF) |>
  select(code, weather, ref_period, wx_type, model, cov_profile,
         r2, r2_adj, r2_within, aic, n) |>
  mutate(
    model_label = factor(
      case_when(model == "fit1" ~ "Weather only",
                model == "fit2" ~ "Weather + FE",
                model == "fit3" ~ "Weather + FE + Cov"),
      levels = c("Weather only", "Weather + FE", "Weather + FE + Cov")
    )
  )

best_fit <- fit_quality |>
  filter(model == "fit3") |>
  group_by(code) |>
  slice_max(r2_within, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(code = factor(code, levels = ctry_order))

p_r2 <- fit_quality |>
  group_by(code, model_label) |>
  summarise(med_r2 = median(r2, na.rm = TRUE), .groups = "drop") |>
  mutate(code = factor(code, levels = ctry_order)) |>
  ggplot(aes(x = code, y = med_r2, fill = model_label)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_brewer(palette = "Blues", name = "Model") +
  labs(title = paste0("Model R² by country and model complexity (", DEFAULT_FE, ", ", DEFAULT_REF, ")"),
       subtitle = "Median across weather specs (fixest, Mean, no interaction)",
       x = NULL, y = "R²") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

p_r2w <- best_fit |>
  ggplot(aes(x = code, y = r2_within)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_text(aes(label = sprintf("%.2f", r2_within)), vjust = -0.3, size = 2.5) +
  labs(title = paste0("Within-R² (best fit3 per country, ", DEFAULT_FE, ", ", DEFAULT_REF, ")"),
       subtitle = "Variation explained by weather + covariates after absorbing fixed effects",
       x = NULL, y = "Within R²") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_fit_combined <- p_r2 / p_r2w
ggsave(file.path(OUT_SAMPLE, "03a_6_model_fit_quality.png"), p_fit_combined,
       width = 12, height = 10, dpi = 150, bg = "white")

# =============================================================================
# SECTION 8 — LASSO COVARIATE SELECTION (year_loc)
# =============================================================================

lasso_data <- stats |>
  filter(!is.na(lasso_selected), lasso_selected != "",
         engine == "fixest", estimand == "Mean", fe_profile == DEFAULT_FE,
         ref_period == DEFAULT_REF) |>
  select(code, weather, interaction, lasso_selected)

lasso_freq <- tibble()
if (nrow(lasso_data) > 0) {
  lasso_vars <- lasso_data |>
    mutate(vars = str_split(lasso_selected, "\\|")) |>
    unnest(vars) |>
    mutate(vars = str_trim(vars)) |>
    filter(vars != "")

  n_lasso_models <- n_distinct(lasso_data |> unite("id", code, weather, interaction))
  lasso_freq <- lasso_vars |>
    count(vars, sort = TRUE) |>
    mutate(pct = n / n_lasso_models * 100)

  top_lasso <- head(lasso_freq, 25)

  p_lasso <- top_lasso |>
    mutate(vars = fct_reorder(vars, pct)) |>
    ggplot(aes(x = vars, y = pct)) +
    geom_col(aes(fill = pct), width = 0.6) +
    geom_text(aes(label = sprintf("%.0f%%", pct)), hjust = -0.1, size = 2.8) +
    coord_flip() +
    scale_fill_viridis_c(option = "viridis", guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = paste0("LASSO-selected covariates (", DEFAULT_FE, ", ", DEFAULT_REF, ", top 25)"),
         subtitle = sprintf("Frequency across %d fixest models", n_lasso_models),
         x = NULL, y = "% of models selecting this variable") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.major.y = element_blank())

  ggsave(file.path(OUT_SAMPLE, "03a_7_lasso_selection.png"), p_lasso,
         width = 10, height = 8, dpi = 150, bg = "white")
}

# =============================================================================
# SECTION 9 — BINNED VS CONTINUOUS (year_loc)
# =============================================================================

bin_fit <- stats |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean",
         fe_profile == DEFAULT_FE, is.na(interaction),
         ref_period == DEFAULT_REF) |>
  group_by(code, wx_type) |>
  summarise(med_r2w = median(r2_within, na.rm = TRUE),
            med_aic = median(aic, na.rm = TRUE), .groups = "drop")

bin_compare <- bin_fit |>
  pivot_wider(names_from = wx_type, values_from = c(med_r2w, med_aic)) |>
  mutate(r2w_diff = med_r2w_binned - med_r2w_continuous,
         aic_diff = med_aic_binned - med_aic_continuous)

# =============================================================================
# SECTION 10 — ROBUSTNESS: 1to1m vs 1to3m COMPARISON
# =============================================================================

# Compare primary (1to1m) to robustness (1to3m) reference period: sign
# consistency, magnitude stability, significance agreement.
robust_coefs <- wx_cont |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean",
         fe_profile == DEFAULT_FE, is.na(interaction),
         wx_type == "continuous",
         ref_period %in% c(DEFAULT_REF, ROBUST_REF)) |>
  group_by(code, ref_period) |>
  slice_min(p_value, n = 1, with_ties = FALSE) |>
  ungroup()

robust_wide <- robust_coefs |>
  select(code, ref_period, estimate, std_error, p_value) |>
  pivot_wider(names_from = ref_period, values_from = c(estimate, std_error, p_value),
              names_sep = "_")

# Defensive column naming — use backticks for constructed names
est_primary <- paste0("estimate_", DEFAULT_REF)
est_robust  <- paste0("estimate_", ROBUST_REF)
p_primary   <- paste0("p_value_", DEFAULT_REF)
p_robust    <- paste0("p_value_", ROBUST_REF)

robust_wide <- robust_wide |>
  mutate(
    same_sign   = sign(.data[[est_primary]]) == sign(.data[[est_robust]]),
    coef_ratio  = .data[[est_robust]] / .data[[est_primary]],
    both_sig    = .data[[p_primary]] < 0.05 & .data[[p_robust]] < 0.05,
    primary_sig = .data[[p_primary]] < 0.05,
    robust_sig  = .data[[p_robust]] < 0.05
  )

n_same_sign   <- sum(robust_wide$same_sign, na.rm = TRUE)
n_both_sig    <- sum(robust_wide$both_sig, na.rm = TRUE)
n_primary_only <- sum(robust_wide$primary_sig & !robust_wide$robust_sig, na.rm = TRUE)
n_robust_only  <- sum(!robust_wide$primary_sig & robust_wide$robust_sig, na.rm = TRUE)
med_coef_ratio <- median(robust_wide$coef_ratio, na.rm = TRUE)

cat(sprintf("Robustness: %d/%d same sign, %d both sig, median coef ratio %.2f\n",
            n_same_sign, nrow(robust_wide), n_both_sig, med_coef_ratio))

# =============================================================================
# SECTION 11 — GENERATE MARKDOWN SUMMARY
# =============================================================================

md <- character()
md_line <- function(...) md <<- c(md, paste0(...))

md_line("# Model Selection Analysis")
md_line("")
md_line("Analysis of `model_coefficients.csv` and `model_fit_stats.csv` from ",
        "`batch/03_run_mod1.R` to evaluate which specifications produce the ",
        "most significant and precise weather-welfare estimates for simulation.")
md_line("")
md_line("Builds on the sample selection analysis (`sample_selection_summary.md`) and ",
        "weather selection analysis (`weather_selection_summary.md`).")
md_line("")
md_line("Generated: ", Sys.Date())
md_line("")
md_line("> **Note:** Restricted to **", length(multi_wave_codes), "** countries with ≥ 2 survey ",
        "waves (from `01a_sample_selection.R`): ",
        paste0("`", sort(multi_wave_codes), "`", collapse = ", "), ". ",
        "Single-wave countries are excluded to ensure temporal variation in weather-welfare estimation.")
md_line("")

# --- 1: Data summary + FE decision ---
md_line("## 1. Data summary and identification strategy")
md_line("")
md_line("- **", n_countries, "** countries, **", n_wx_specs, "** weather specifications (temperature only)")
md_line("- **Weather specs**: ", paste0("`", sort(unique(coef$weather)), "`", collapse = ", "))
md_line("- **Engines**: fixest (OLS with FE), RIF (unconditional quantile regression)")
md_line("- **Models**: fit1 (weather only), fit2 (weather + FE), fit3 (weather + FE + covariates)")
md_line("- **Interactions**: urban, electricity, imp_wat_rec, imp_san_rec, ttime_health")
md_line("")
md_line("### Fixed effects decision")
md_line("")
md_line("**Default FE: `year_loc`** (location x year fixed effects). This is the more rigorous ",
        "specification because it absorbs all time-invariant location characteristics, identifying ",
        "the temperature effect from within-location temporal variation only. The alternative ",
        "`year_admin1` (admin1 x year) preserves cross-location variation within a region-year, ",
        "which inflates significance but may capture confounded spatial differences rather than ",
        "causal weather effects.")
md_line("")
md_line("Countries where `year_loc` fails to produce a significant result but `year_admin1` does ",
        "are flagged as **admin1-only** -- the temperature signal in those countries may partly ",
        "reflect spatial confounding rather than pure weather effects.")
md_line("")

md_line("### Reference period decision")
md_line("")
md_line(paste0("**Primary reference period: `", DEFAULT_REF, "`** (1-month pre-interview average). ",
        "This has the strongest statistical power under `year_loc` FE across countries ",
        "(highest significance rate, highest median |t|). All downstream analysis (covariate selection, ",
        "interactions, RIF, model fit) uses `", DEFAULT_REF, "` to keep interpretation consistent ",
        "across countries. `", ROBUST_REF, "` is retained as a robustness check."))
md_line("")

if (nrow(int_na) > 0) {
  md_line("### Unavailable interactions")
  md_line("")
  int_na_tbl <- int_na |> distinct(sample, interaction)
  for (ctry in unique(int_na_tbl$sample)) {
    vars <- int_na_tbl |> filter(sample == ctry) |> pull(interaction)
    md_line("- **", ctry, "**: ", paste(vars, collapse = ", "))
  }
  md_line("")
}

# --- 2: Coefficient significance ---
md_line("## 2. Coefficient significance under year_loc FE")
md_line("")
md_line("For each country and reference period, we select the best covariate profile ",
        "(lowest p-value) under `year_loc` FE. Continuous specs only, fit3, Mean estimand.")
md_line("")

md_line("### FE comparison summary")
md_line("")
md_line("| Status | Count | Countries |")
md_line("|--------|-------|-----------|")
loc_sig_codes <- fe_compare |> filter(status == "year_loc sig") |> arrange(loc_p) |> pull(code)
md_line("| year_loc significant (p<0.05) | ", n_loc_sig, " | ",
        paste(loc_sig_codes, collapse = ", "), " |")
md_line("| admin1-only (year_loc n.s., admin1 p<0.05) | ", n_adm_only, " | ",
        if (length(adm_only) > 0) paste(adm_only, collapse = ", ") else "--", " |")
md_line("| Not significant in either | ", n_neither, " | ",
        if (length(neither) > 0) paste(neither, collapse = ", ") else "--", " |")
md_line("")

md_line("### Best specification per country (year_loc FE)")
md_line("")
md_line("| Country | Status | Ref period | Coefficient | t-stat | p-value | Covariates |")
md_line("|---------|--------|-----------|-------------|--------|---------|------------|")
for (i in seq_len(nrow(fe_compare))) {
  r <- fe_compare[i, ]
  if (r$status == "year_loc sig") {
    md_line("| ", r$code, " | ", r$status, " | ",
            str_extract(r$loc_wx, "\\d+to\\d+m"), " | ",
            sprintf("%.4f", r$loc_est), " | ", sprintf("%.2f", r$loc_t), " | ",
            sprintf("%.2e", r$loc_p), " | ", r$loc_cov, " |")
  } else {
    adm_ref <- str_extract(r$adm_wx, "\\d+to\\d+m")
    md_line("| ", r$code, " | ", r$status, " | ",
            adm_ref, " (admin1) | ",
            sprintf("%.4f", r$adm_est), " | ", sprintf("%.2f", r$adm_t), " | ",
            sprintf("%.2e", r$adm_p), " | ", r$adm_cov, " |")
  }
}
md_line("")

# Significance by ref_period under year_loc
ref_sig <- loc_best |>
  group_by(ref_period) |>
  summarise(
    n_sig001 = sum(p_value < 0.001),
    n_sig01  = sum(p_value < 0.01),
    n_sig05  = sum(p_value < 0.05),
    med_t    = median(abs(statistic), na.rm = TRUE),
    .groups  = "drop"
  )

md_line("### Significance by reference period (year_loc)")
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

ref_wins <- loc_best_ctry |>
  filter(p_value < 0.05) |>
  count(ref_period = as.character(ref_period), name = "n_best") |>
  arrange(desc(n_best))
if (nrow(ref_wins) > 0) {
  md_line("**Best ref period among year_loc-significant countries**: ",
          paste(sprintf("%s (%d)", ref_wins$ref_period, ref_wins$n_best), collapse = ", "))
  md_line("")
}

md_line("![Coefficient significance](03a_1_coef_significance.png)")
md_line("")

# --- 3: Covariate selection ---
md_line("## 3. Covariate selection: stability and fit")
md_line("")
md_line(paste0("Under `year_loc` FE and `", DEFAULT_REF, "` reference period, we compare LASSO ",
        "(data-driven) vs user-defined (hhsize + urban) covariates on two criteria:"))
md_line("")
md_line("1. **Coefficient stability**: does the temperature coefficient change when switching ",
        "from user-defined to LASSO? A ratio near 1.0 means the weather estimate is robust to ",
        "covariate choice -- the covariates are controlling for confounders without absorbing ",
        "the weather signal.")
md_line("2. **Fit improvement**: does LASSO improve within-R-squared? If yes, LASSO is picking ",
        "up relevant variation that the parsimonious set misses.")
md_line("")
md_line("**Decision rule**: use LASSO when it improves fit AND the weather coefficient is stable ",
        "(>=75% of specs have coefficient ratio within 0.75-1.25).")
md_line("")

md_line("| Country | Coef ratio (LASSO/user) | % stable | Median R-squared gain | % LASSO better | Use LASSO? |")
md_line("|---------|------------------------|----------|---------------------|----------------|------------|")
for (i in seq_len(nrow(cov_decision))) {
  r <- cov_decision[i, ]
  md_line("| ", as.character(r$code), " | ", sprintf("%.2f", r$med_ratio),
          " | ", sprintf("%.0f%%", r$pct_stable),
          " | ", sprintf("%.4f", r$med_r2w_gain),
          " | ", sprintf("%.0f%%", r$pct_lasso_better),
          " | ", ifelse(r$use_lasso, "Yes", "No"), " |")
}
md_line("")

n_use_lasso <- sum(cov_decision$use_lasso, na.rm = TRUE)
n_use_user  <- sum(!cov_decision$use_lasso, na.rm = TRUE)
md_line(paste0("**Summary**: LASSO recommended for ", n_use_lasso, "/", n_countries,
               " countries; user-defined for ", n_use_user, "."))

lasso_no_codes <- cov_decision |> filter(!use_lasso) |> pull(code) |> as.character()
if (length(lasso_no_codes) > 0) {
  md_line(" Countries where LASSO is not recommended (unstable coef or no fit gain): ",
          paste0("`", lasso_no_codes, "`", collapse = ", "), ".")
}
md_line("")
md_line("![Covariate stability](03a_2_covariate_stability.png)")
md_line("")

# --- 4: Coefficient precision and direction ---
md_line("## 4. Coefficient precision and direction")
md_line("")
md_line(paste0("Faceted forest plot showing point estimates and 95% CI for the temperature ",
        "coefficient under `year_loc` FE, LASSO covariates, OLS -- one panel per reference period. ",
        "Countries sorted by `", DEFAULT_REF, "` coefficient value."))
md_line("")

n_negative <- sum(loc_best_ctry$estimate < 0 & loc_best_ctry$p_value < 0.05, na.rm = TRUE)
n_positive <- sum(loc_best_ctry$estimate > 0 & loc_best_ctry$p_value < 0.05, na.rm = TRUE)
md_line(paste0("**Direction** (among year_loc-significant countries): ", n_negative,
               " negative (higher temp = lower welfare), ", n_positive, " positive."))
md_line("")

pos_countries <- loc_best_ctry |>
  filter(estimate > 0, p_value < 0.05) |> pull(code)
if (length(pos_countries) > 0) {
  md_line("Positive coefficients: ", paste0("`", pos_countries, "`", collapse = ", "),
          " -- likely cold-climate contexts or compositional effects.")
  md_line("")
}
md_line("![Coefficient forest plot](03a_3_ref_period_ranking.png)")
md_line("")

# --- 5a: Continuous interactions ---
md_line(paste0("## 5. Interaction effects (", DEFAULT_REF, ", ", DEFAULT_FE, ")"))
md_line("")
md_line(paste0("Interaction terms (temperature x policy variable) capture whether policy ",
        "interventions modify the weather-welfare relationship. Continuous and binned ",
        "specifications are shown separately -- continuous gives a marginal effect per degree, ",
        "binned captures the non-linear effect of extreme heat."))
md_line("")

md_line("### 5a. Continuous specification")
md_line("")

int_rank <- int_best |>
  group_by(int_var) |>
  summarise(
    n_countries = n_distinct(code),
    n_sig05     = sum(best_p < 0.05),
    n_sig01     = sum(best_p < 0.01),
    med_est     = median(best_est, na.rm = TRUE),
    .groups     = "drop"
  ) |>
  arrange(desc(n_sig05))

md_line("| Interaction | Countries with data | Sig (p<0.05) | Sig (p<0.01) | Median coef |")
md_line("|-------------|--------------------|--------------|--------------|--------------------|")
for (i in seq_len(nrow(int_rank))) {
  r <- int_rank[i, ]
  md_line("| ", r$int_var, " | ", r$n_countries, " | ", r$n_sig05, " | ", r$n_sig01,
          " | ", sprintf("%.4f", r$med_est), " |")
}
md_line("")

int_sig <- int_best |> filter(best_p < 0.05) |> arrange(code, int_var)
md_line("**Significant interactions by country** (p<0.05):")
md_line("")
md_line("| Country | Interaction | Coefficient | p-value |")
md_line("|---------|-------------|-------------|---------|")
for (i in seq_len(nrow(int_sig))) {
  r <- int_sig[i, ]
  md_line("| ", as.character(r$code), " | ", r$int_var, " | ",
          sprintf("%.4f", r$best_est), " | ", sprintf("%.2e", r$best_p), " |")
}
md_line("")

if (nrow(int_direction) > 0) {
  md_line("**Direction summary** (among significant results):")
  md_line("")
  for (i in seq_len(nrow(int_direction))) {
    r <- int_direction[i, ]
    dir_str <- ifelse(r$n_positive > r$n_negative,
                       sprintf("predominantly positive (%d+, %d-)", r$n_positive, r$n_negative),
                       sprintf("predominantly negative (%d+, %d-)", r$n_positive, r$n_negative))
    md_line(sprintf("- **%s**: %s, median = %.4f", r$int_var, dir_str, r$med_est))
  }
  md_line("")
}
md_line("![Continuous interactions](03a_4a_interaction_continuous.png)")
md_line("")

# --- 5b: Binned highest bin ---
md_line("### 5b. Binned specification (highest temperature bin)")
md_line("")
md_line(paste0("The highest temperature bin captures extreme heat effects relative to the ",
        "omitted (moderate) bin. Country-specific bin cutpoints reflect the local temperature ",
        "distribution."))
md_line("")

md_line("**Highest bin coefficient by country:**")
md_line("")
md_line("| Country | Bin | Coefficient | SE | p-value | Covariates |")
md_line("|---------|-----|-------------|-----|---------|------------|")
binn_best_md <- binn_best |> arrange(estimate)
for (i in seq_len(nrow(binn_best_md))) {
  r <- binn_best_md[i, ]
  md_line("| ", as.character(r$code), " | ", r$bin_label, " | ",
          sprintf("%.4f", r$estimate), " | ", sprintf("%.4f", r$std_error), " | ",
          sprintf("%.2e", r$p_value), " | ", r$cov_profile, " |")
}
md_line("")

if (nrow(binn_int_best) > 0) {
  binn_int_rank <- binn_int_best |>
    group_by(int_var) |>
    summarise(
      n_countries = n_distinct(code),
      n_sig05     = sum(best_p < 0.05),
      med_est     = median(best_est, na.rm = TRUE),
      .groups     = "drop"
    ) |>
    arrange(desc(n_sig05))

  md_line("**Highest bin interaction ranking:**")
  md_line("")
  md_line("| Interaction | Countries | Sig (p<0.05) | Median coef |")
  md_line("|-------------|-----------|--------------|-------------|")
  for (i in seq_len(nrow(binn_int_rank))) {
    r <- binn_int_rank[i, ]
    md_line("| ", r$int_var, " | ", r$n_countries, " | ", r$n_sig05,
            " | ", sprintf("%.4f", r$med_est), " |")
  }
  md_line("")
}

md_line("![Binned highest bin](03a_4b_binned_highest.png)")
md_line("")

# --- 6: RIF distributional ---
md_line(paste0("## 6. Distributional effects (RIF, year_loc FE, ", DEFAULT_REF, ")"))
md_line("")
md_line(paste0("RIF regressions estimate the temperature effect at each quantile of the welfare ",
        "distribution under `year_loc` FE and `", DEFAULT_REF, "` reference period."))
md_line("")

md_line("| Country | Coef p10 | Coef p50 | Coef p90 | Gradient (p10-p90) | Pattern | # Sig |")
md_line("|---------|----------|----------|----------|--------------------|---------|-----------| ")
for (i in seq_len(nrow(rif_gradient))) {
  r <- rif_gradient[i, ]
  md_line("| ", r$code, " | ", sprintf("%.4f", r$est_p10), " | ",
          sprintf("%.4f", r$est_p50), " | ", sprintf("%.4f", r$est_p90), " | ",
          sprintf("%.4f", r$gradient), " | ", r$stronger_at, " | ", r$n_sig, "/9 |")
}
md_line("")

n_bottom  <- sum(rif_gradient$stronger_at == "bottom")
n_top     <- sum(rif_gradient$stronger_at == "top")
n_uniform <- sum(rif_gradient$stronger_at == "uniform")
md_line(paste0("**Pattern**: ", n_bottom, " bottom-heavy, ", n_top, " top-heavy, ",
               n_uniform, " roughly uniform."))

bottom_countries <- rif_gradient |> filter(stronger_at == "bottom") |> pull(code)
if (length(bottom_countries) > 0) {
  md_line(" Pro-poor vulnerability: ", paste0("`", bottom_countries, "`", collapse = ", "))
}
md_line("")
md_line("![RIF distributional effects](03a_5_rif_distributional.png)")
md_line("")

# --- 7: Model fit ---
md_line(paste0("## 7. Model fit quality (year_loc FE, ", DEFAULT_REF, ")"))
md_line("")
md_line("| Country | R-squared | Within R-squared | Covariates | Weather spec | N |")
md_line("|---------|-----------|------------------|------------|-------------|---|")
for (i in seq_len(nrow(best_fit))) {
  r <- best_fit[i, ]
  md_line("| ", as.character(r$code), " | ", sprintf("%.3f", r$r2), " | ",
          sprintf("%.3f", r$r2_within), " | ", r$cov_profile,
          " | ", r$weather, " | ", format(r$n, big.mark = ","), " |")
}
md_line("")
md_line("![Model fit quality](03a_6_model_fit_quality.png)")
md_line("")

# --- 8: LASSO selection ---
if (nrow(lasso_freq) > 0) {
  md_line(paste0("## 8. LASSO covariate selection (year_loc FE, ", DEFAULT_REF, ")"))
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

# --- 9: Binned vs continuous ---
md_line(paste0("## 9. Binned vs continuous (year_loc FE, ", DEFAULT_REF, ")"))
md_line("")
bin_summary_md <- bin_compare |>
  summarise(
    med_diff_r2w     = median(r2w_diff, na.rm = TRUE),
    n_binned_better  = sum(r2w_diff > 0, na.rm = TRUE),
    n_cont_better    = sum(r2w_diff < 0, na.rm = TRUE)
  )

md_line(paste0("Under `", DEFAULT_REF, "` reference period:"))
md_line("")
md_line("| Metric | Value |")
md_line("|--------|-------|")
md_line("| Median delta within-R-squared (binned - continuous) | ",
        sprintf("%.4f", bin_summary_md$med_diff_r2w), " |")
md_line("| Binned better | ", bin_summary_md$n_binned_better, "/", n_countries, " |")
md_line("| Continuous better | ", bin_summary_md$n_cont_better, "/", n_countries, " |")
md_line("")

# --- 10: Robustness ---
md_line(paste0("## 10. Robustness: ", DEFAULT_REF, " vs ", ROBUST_REF))
md_line("")
md_line(paste0("Comparison of primary (`", DEFAULT_REF, "`) and robustness (`", ROBUST_REF,
        "`) reference periods under `year_loc` FE. Consistency supports the locked-in ",
        "specification; divergence flags countries where results are sensitive to the ",
        "weather averaging window."))
md_line("")

md_line("| Country | Coef (", DEFAULT_REF, ") | Coef (", ROBUST_REF, ") | Same sign | ",
        "Coef ratio | Both sig | Primary sig | Robust sig |")
md_line("|---------|", paste(rep("------|", 7), collapse = ""))
for (i in seq_len(nrow(robust_wide))) {
  r <- robust_wide[i, ]
  md_line("| ", r$code,
          " | ", sprintf("%.4f", r[[est_primary]]),
          " | ", sprintf("%.4f", r[[est_robust]]),
          " | ", ifelse(r$same_sign, "Yes", "**No**"),
          " | ", sprintf("%.2f", r$coef_ratio),
          " | ", ifelse(r$both_sig, "Yes", "No"),
          " | ", ifelse(r$primary_sig, "Yes", "No"),
          " | ", ifelse(r$robust_sig, "Yes", "No"), " |")
}
md_line("")

md_line(paste0("**Summary**: ", n_same_sign, "/", nrow(robust_wide), " countries have same sign, ",
        n_both_sig, " both significant, median coefficient ratio = ", sprintf("%.2f", med_coef_ratio), "."))
if (n_primary_only > 0) {
  primary_only_codes <- robust_wide |> filter(primary_sig & !robust_sig) |> pull(code)
  md_line(paste0(" Significant only under `", DEFAULT_REF, "`: ",
          paste0("`", primary_only_codes, "`", collapse = ", "), "."))
}
if (n_robust_only > 0) {
  robust_only_codes <- robust_wide |> filter(!primary_sig & robust_sig) |> pull(code)
  md_line(paste0(" Significant only under `", ROBUST_REF, "`: ",
          paste0("`", robust_only_codes, "`", collapse = ", "), "."))
}
sign_flip_codes <- robust_wide |> filter(!same_sign) |> pull(code)
if (length(sign_flip_codes) > 0) {
  md_line(paste0(" **Sign flips**: ", paste0("`", sign_flip_codes, "`", collapse = ", "),
          " -- interpret with caution."))
}
md_line("")

# --- 11: Recommendations ---
md_line("## 11. Recommendations for simulation")
md_line("")

md_line("### Fixed effects")
md_line("")
md_line(paste0("**Use `year_loc` as default** -- produces clean within-location identification in ",
               n_loc_sig, "/", n_countries, " countries."))
if (length(adm_only) > 0) {
  md_line(paste0(" Fall back to `year_admin1` for admin1-only countries (",
                 paste0("`", adm_only, "`", collapse = ", "),
                 ") -- flag these results as potentially confounded by spatial heterogeneity."))
}
if (length(neither) > 0) {
  md_line(paste0(" Countries with no significant temperature signal under either FE: ",
                 paste0("`", neither, "`", collapse = ", "), "."))
}
md_line("")

md_line("### Covariate selection")
md_line("")
md_line(paste0("**Use LASSO where it improves fit and preserves the weather coefficient** (",
               n_use_lasso, "/", n_countries, " countries). ",
               "For the remaining ", n_use_user, " countries, stick with user-defined (hhsize + urban) ",
               "-- LASSO either destabilises the weather coefficient or does not improve fit."))
md_line("")

md_line("### Weather construction")
md_line("")
md_line(paste0("**Primary reference period: `", DEFAULT_REF, "`** (best statistical power under ",
               "year_loc). Run `", ROBUST_REF, "` as robustness check (",
               n_same_sign, "/", nrow(robust_wide), " countries show consistent sign, ",
               "median coefficient ratio ", sprintf("%.2f", med_coef_ratio), ")."))
md_line(" Continuous specs preferred for simulation; binned useful for non-linearity checks.")
md_line("")

md_line("### Policy interactions")
md_line("")
md_line("Under year_loc FE, ranked by cross-country significance:")
md_line("")
for (i in seq_len(nrow(int_rank))) {
  r <- int_rank[i, ]
  ctry_list <- int_sig |> filter(int_var == r$int_var) |> pull(code) |> as.character()
  md_line(sprintf("1. **%s** -- significant in %d/%d countries: %s",
                  r$int_var, r$n_sig05, r$n_countries,
                  paste(ctry_list, collapse = ", ")))
}
md_line("")

md_line("### Distributional targeting")
md_line("")
if (n_bottom > 0) {
  md_line(paste0("RIF regressions (year_loc FE) show that in ", n_bottom, "/", n_countries,
                 " countries poorer households are disproportionately affected. ",
                 "Countries: ", paste0("`", bottom_countries, "`", collapse = ", "), "."))
  md_line("")
}

md_line("### Country simulation readiness")
md_line("")

tier_a <- fe_compare |> filter(loc_p < 0.01) |> pull(code) |> sort()
tier_b <- fe_compare |> filter(loc_p >= 0.01, loc_p < 0.05) |> pull(code) |> sort()
tier_c_loc <- fe_compare |> filter(loc_p >= 0.05, adm_p < 0.05) |> pull(code) |> sort()
tier_d <- fe_compare |> filter(loc_p >= 0.05, adm_p >= 0.05) |> pull(code) |> sort()

md_line("**Tier A** (year_loc p<0.01, strong causal identification): ",
        if (length(tier_a) > 0) paste0("`", tier_a, "`", collapse = ", ") else "none",
        sprintf(" (%d)", length(tier_a)))
md_line("")
md_line("**Tier B** (year_loc p<0.05, adequate identification): ",
        if (length(tier_b) > 0) paste0("`", tier_b, "`", collapse = ", ") else "none",
        sprintf(" (%d)", length(tier_b)))
md_line("")
md_line("**Tier C** (admin1-only, use with caution): ",
        if (length(tier_c_loc) > 0) paste0("`", tier_c_loc, "`", collapse = ", ") else "none",
        sprintf(" (%d)", length(tier_c_loc)))
md_line("")
md_line("**Tier D** (no significant signal): ",
        if (length(tier_d) > 0) paste0("`", tier_d, "`", collapse = ", ") else "none",
        sprintf(" (%d)", length(tier_d)))
md_line("")

writeLines(md, file.path(OUT_SAMPLE, "model_selection_summary.md"))
cat(sprintf("\n=== Saved: %s ===\n", file.path(OUT_SAMPLE, "model_selection_summary.md")))
cat(sprintf("=== Plots saved to: %s ===\n", OUT_SAMPLE))
