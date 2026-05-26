# =============================================================================
# batch/03a_model_selection.R
#
# Analysis of model_coefficients.csv and model_fit_stats.csv from
# batch/03_run_mod1.R.  Compares weather constructions (continuous, binned
# equal-frequency, binned custom) across reference periods and engines
# (fixest OLS vs RIF), then focuses on 1to3m to evaluate model fit, top-bin
# effects, distributional heterogeneity, and policy interactions.
#
# Locked-in decisions (from prior analysis):
#   - Covariates: LASSO
#   - Fixed effects: year_loc
#
# Key questions answered:
#   1. Which reference period performs best? (then focus on 1to3m)
#   2. Continuous, equal-frequency binned, or custom (absolute) binned?
#   3. Where are distributional effects important (RIF vs OLS)?
#   4. Which policy x extreme-weather interactions are significant?
#   5. What simulations to prioritise per country?
#
# Policy variables considered:
#   electricity, imp_wat_rec, imp_san_rec, imp_wat_san_rec, ttime_health
#
# Outputs (all under OUT_DIR/model_fit/):
#   03a_1_ref_period_significance.png      significance by reference period
#   03a_1b_ref_coefs_{constr}_{wxvar}.png   coef ranking by ref period per construction/var
#   03a_2_construction_fit.png             within-R2 by weather construction
#   03a_3_top_bin_coefficients.png         extreme effects: heat (top) & drought (bottom)
#   03a_4_rif_distributional.png           RIF quantile profiles
#   03a_5_policy_interactions.png          policy x extreme weather interactions
#   03a_6_rif_policy_interactions.png      RIF: sig quantiles for policy interactions
#   03a_7_rif_profiles_{policy_var}.png    RIF quantile profiles per policy variable
#   model_selection_summary.md             narrative summary + recommendations
#
# Dynamic: handles any number of countries and weather variables.
# Usage: source("batch/03a_model_selection.R")
# =============================================================================

library(tidyverse)
library(patchwork)

# =============================================================================
# SECTION 1 -- CONFIGURATION & DATA
# =============================================================================

OUT_DIR <- Sys.getenv("WISEAPP_RESULTS_PATH")
if (!nzchar(OUT_DIR)) OUT_DIR <- "dev/output"
MOD_DIR <- file.path(OUT_DIR, "model_fit")

coef  <- read_csv(file.path(MOD_DIR, "model_coefficients.csv"), show_col_types = FALSE)
stats <- read_csv(file.path(MOD_DIR, "model_fit_stats.csv"),    show_col_types = FALSE)

int_na <- tryCatch(
  read_csv(file.path(MOD_DIR, "_interactions_not_available.csv"), show_col_types = FALSE),
  error = function(e) tibble(spec_label = character(), reason = character(),
                             sample = character(), interaction = character())
)

parse_wx <- function(df) {
  df |> mutate(
    wx_var       = str_extract(weather, "^.+(?=_\\d+to)"),
    ref_period   = str_extract(weather, "\\d+to\\d+m"),
    construction = case_when(
      grepl("_cont_", weather)     ~ "continuous",
      grepl("_binn_cust", weather) ~ "binned_custom",
      grepl("_binn_", weather)     ~ "binned_equal",
      TRUE                         ~ "other"
    )
  )
}

coef  <- parse_wx(coef)
stats <- parse_wx(stats)

REF_LEVELS <- c("1to1m", "1to3m", "1to6m", "1to12m")
CONSTR_LEVELS <- c("continuous", "binned_equal", "binned_custom")
CONSTR_LABELS <- c("Continuous", "Binned (equal freq)", "Binned (custom)")

FOCUS_REF    <- "1to3m"
FOCUS_CONSTR <- "binned_equal"
FOCUS_CONSTR_LABEL <- "Binned (equal freq)"
POLICY_VARS <- c("electricity", "imp_wat_rec", "imp_san_rec",
                 "imp_wat_san_rec", "ttime_health")

# For SPEI the bottom bin (drought) is the omitted reference category, so all
# SPEI bin coefficients measure "welfare relative to drought."  To present
# both variables on the same scale — negative = the extreme hurts welfare,
# positive interaction = protective — we negate SPEI binned coefficients.
# Temperature top bin (heat) needs no adjustment.
SPEI_VARS <- grep("^spei", unique(coef$wx_var), value = TRUE)

countries   <- sort(unique(coef$code))
n_countries <- length(countries)
wx_vars     <- sort(unique(coef$wx_var))

cat(sprintf("Countries (%d): %s\n", n_countries, paste(countries, collapse = ", ")))
cat(sprintf("Weather variables: %s\n", paste(wx_vars, collapse = ", ")))
cat(sprintf("Focus reference period: %s\n\n", FOCUS_REF))

if (nrow(int_na) > 0) {
  cat("Unavailable interactions:\n")
  for (ctry in sort(unique(int_na$sample))) {
    v <- int_na |> filter(sample == ctry) |> distinct(interaction) |> pull(interaction)
    cat(sprintf("  %s: %s\n", ctry, paste(v, collapse = ", ")))
  }
  cat("\n")
}

# =============================================================================
# SECTION 2 -- REFERENCE PERIOD COMPARISON
# =============================================================================

# Continuous main effect: fixest OLS, fit3, Mean, no interaction
cont_main <- coef |>
  filter(construction == "continuous", engine == "fixest", model == "fit3",
         estimand == "Mean", is.na(interaction), term %in% wx_vars) |>
  mutate(
    sig = case_when(p_value < 0.001 ~ "***", p_value < 0.01  ~ "**",
                    p_value < 0.05  ~ "*",   p_value < 0.10  ~ "†",
                    TRUE ~ ""),
    ref_period = factor(ref_period, levels = REF_LEVELS)
  )

p_ref <- cont_main |>
  ggplot(aes(x = ref_period, y = code, fill = -log10(p_value))) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = paste0(sprintf("%.1f", statistic), sig)),
            size = 2.2, color = "grey15") +
  facet_wrap(~wx_var, scales = "free") +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "-log10(p)") +
  labs(title = "Continuous weather effect: significance by reference period",
       subtitle = "fixest OLS, fit3, LASSO, year_loc FE, no interaction. t-stat shown.",
       x = "Reference period", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank())

ggsave(file.path(MOD_DIR, "03a_1_ref_period_significance.png"), p_ref,
       width = max(10, length(wx_vars) * 5.5),
       height = max(4, n_countries * 0.45 + 1.5), dpi = 150, bg = "white")

ref_sig_summary <- cont_main |>
  group_by(wx_var, ref_period) |>
  summarise(n_sig05 = sum(p_value < 0.05), n_sig10 = sum(p_value < 0.10),
            med_abs_t = median(abs(statistic), na.rm = TRUE), .groups = "drop")

ref_fit_summary <- stats |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean",
         is.na(interaction), construction == "continuous") |>
  group_by(wx_var, ref_period) |>
  summarise(med_r2w = median(r2_within, na.rm = TRUE), .groups = "drop")

cat("=== Reference period significance ===\n")
print(as.data.frame(ref_sig_summary), row.names = FALSE)
cat("\n=== Median within-R2 by reference period (continuous) ===\n")
print(as.data.frame(ref_fit_summary), row.names = FALSE)
cat("\n")

# --- Reference period coefficient ranking: one plot per construction × wx_var ---
for (constr in CONSTR_LEVELS) {
  constr_label <- CONSTR_LABELS[match(constr, CONSTR_LEVELS)]

  for (wv in wx_vars) {
    if (constr == "continuous") {
      ref_wv <- coef |>
        filter(construction == "continuous", engine == "fixest", model == "fit3",
               estimand == "Mean", is.na(interaction), term == wv, wx_var == wv)
    } else {
      ref_wv <- coef |>
        filter(construction == constr, engine == "fixest", model == "fit3",
               estimand == "Mean", is.na(interaction), wx_var == wv,
               grepl("Inf\\]$", term), !grepl(":", term)) |>
        mutate(estimate = if (wv %in% SPEI_VARS) -estimate else estimate)
    }

    if (nrow(ref_wv) == 0) next

    ref_wv <- ref_wv |>
      mutate(ref_period = factor(ref_period, levels = REF_LEVELS),
             sig = p_value < 0.05)

    sort_order <- ref_wv |>
      filter(ref_period == FOCUS_REF) |>
      select(code, sort_est = estimate)

    ref_wv <- ref_wv |>
      left_join(sort_order, by = "code") |>
      mutate(code = fct_reorder(code, -sort_est))

    wv_label <- if (constr != "continuous" && wv %in% SPEI_VARS) {
      paste0(wv, " (drought)")
    } else if (constr != "continuous") {
      paste0(wv, " (heat)")
    } else {
      wv
    }
    spei_note <- if (constr != "continuous" && wv %in% SPEI_VARS) " SPEI negated (drought focus)." else ""

    p_ref_rank <- ref_wv |>
      ggplot(aes(x = estimate, y = code, color = sig)) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
      geom_errorbar(aes(xmin = estimate - 1.96 * std_error,
                         xmax = estimate + 1.96 * std_error),
                    width = 0, linewidth = 0.3) +
      geom_point(size = 1.8) +
      scale_color_manual(values = c("TRUE" = "steelblue", "FALSE" = "grey60"),
                         labels = c("TRUE" = "p < 0.05", "FALSE" = "n.s."),
                         name = NULL) +
      facet_wrap(~ref_period, nrow = 1, scales = "free_x") +
      labs(title = paste0(wv_label, " coefficient by country and reference period (", constr_label, ")"),
           subtitle = paste0("Point estimate +/- 95% CI. fixest OLS, fit3, LASSO, year_loc, no interaction. ",
                             "Sorted by ", FOCUS_REF, " coefficient.", spei_note),
           x = paste0("Coefficient on ", wv_label), y = NULL) +
      theme_minimal(base_size = 10) +
      theme(legend.position = "bottom",
            panel.grid.major.y = element_blank(),
            strip.text = element_text(face = "bold"))

    suffix <- c("continuous" = "cont", "binned_equal" = "binn_equal",
                "binned_custom" = "binn_custom")[constr]

    ggsave(file.path(MOD_DIR, paste0("03a_1b_ref_coefs_", suffix, "_", wv, ".png")),
           p_ref_rank,
           width = max(10, length(REF_LEVELS) * 3),
           height = max(4, n_countries * 0.45 + 1.5),
           dpi = 150, bg = "white")
  }
}

# =============================================================================
# SECTION 3 -- WEATHER CONSTRUCTION COMPARISON (1to3m)
# =============================================================================

fit_by_constr <- stats |>
  filter(engine == "fixest", model == "fit3", estimand == "Mean",
         is.na(interaction), ref_period == FOCUS_REF) |>
  mutate(construction = factor(construction, levels = CONSTR_LEVELS,
                               labels = CONSTR_LABELS))

fit_summary <- fit_by_constr |>
  group_by(wx_var, construction, code) |>
  summarise(r2_within = mean(r2_within, na.rm = TRUE),
            aic = mean(aic, na.rm = TRUE), .groups = "drop")

p_constr_r2 <- fit_summary |>
  ggplot(aes(x = code, y = r2_within, fill = construction)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~wx_var, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  labs(title = paste0("Within-R² by weather construction (", FOCUS_REF, ")"),
       subtitle = "fixest OLS, fit3, LASSO, year_loc, no interaction.",
       x = NULL, y = "Within R²") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

p_constr_aic <- fit_summary |>
  ggplot(aes(x = code, y = aic, fill = construction)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~wx_var, scales = "free_y") +
  scale_fill_brewer(palette = "Set2", name = NULL) +
  labs(title = paste0("AIC by weather construction (", FOCUS_REF, ")"),
       subtitle = "Lower = better fit with parsimony penalty.",
       x = NULL, y = "AIC") +
  theme_minimal(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

p_constr <- p_constr_r2 / p_constr_aic + plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(file.path(MOD_DIR, "03a_2_construction_fit.png"), p_constr,
       width = max(8, n_countries * 1.2 + length(wx_vars) * 2),
       height = 10, dpi = 150, bg = "white")

# Delta within-R2: binned minus continuous
fit_wide <- fit_summary |>
  mutate(construction = factor(construction, labels = CONSTR_LEVELS)) |>
  pivot_wider(names_from = construction, values_from = c(r2_within, aic))

fit_wide <- fit_wide |>
  mutate(
    r2w_diff_equal  = r2_within_binned_equal  - r2_within_continuous,
    r2w_diff_custom = r2_within_binned_custom - r2_within_continuous,
    aic_diff_equal  = aic_binned_equal  - aic_continuous,
    aic_diff_custom = aic_binned_custom - aic_continuous
  )

constr_winner <- fit_summary |>
  group_by(wx_var, code) |>
  slice_max(r2_within, n = 1, with_ties = FALSE) |>
  ungroup()

cat("=== Best construction by within-R2 ===\n")
print(as.data.frame(constr_winner |> select(wx_var, code, construction, r2_within)),
      row.names = FALSE)
cat("\n")

# =============================================================================
# SECTION 4 -- TOP BIN COEFFICIENTS (1to3m)
# =============================================================================

# For temperature: extract top bin (Inf]) = extreme heat effect.
# For SPEI: extract top bin (Inf]) = extreme wet vs drought (reference).
#   Negate SPEI so the coefficient reads as "drought effect on welfare"
#   (negative = drought hurts, consistent with temperature where negative = heat hurts).
top_bin_main <- coef |>
  filter(ref_period == FOCUS_REF, engine == "fixest", model == "fit3",
         estimand == "Mean", is.na(interaction),
         grepl("Inf\\]$", term), !grepl(":", term),
         construction == FOCUS_CONSTR) |>
  mutate(
    raw_estimate = estimate,
    estimate = ifelse(wx_var %in% SPEI_VARS, -estimate, estimate),
    sig = p_value < 0.05,
    extreme_label = ifelse(wx_var %in% SPEI_VARS,
                           paste0(wx_var, " (drought effect)"),
                           paste0(wx_var, " (heat effect)"))
  )

if (nrow(top_bin_main) > 0) {
  p_top_bin <- top_bin_main |>
    ggplot(aes(x = estimate, y = code, shape = sig)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(xmin = estimate - 1.96 * std_error,
                       xmax = estimate + 1.96 * std_error),
                  width = 0, linewidth = 0.4, color = "steelblue") +
    geom_point(size = 2.5, color = "steelblue") +
    geom_text(aes(label = term), hjust = -0.1, size = 1.8, color = "grey40",
              show.legend = FALSE) +
    facet_wrap(~extreme_label, scales = "free_x") +
    scale_shape_manual(values = c("TRUE" = 16, "FALSE" = 1),
                       labels = c("TRUE" = "p < 0.05", "FALSE" = "n.s."),
                       name = NULL) +
    labs(title = paste0("Extreme weather effects: heat & drought (", FOCUS_REF, ", ", FOCUS_CONSTR_LABEL, ")"),
         subtitle = paste0("fixest OLS, fit3, LASSO, year_loc, no interaction. ",
                           "SPEI negated: drought (bottom bin) is reference; shown as drought penalty. ",
                           "Negative = extreme hurts welfare."),
         x = "Coefficient (negative = extreme hurts welfare)", y = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", panel.grid.major.y = element_blank())

  ggsave(file.path(MOD_DIR, "03a_3_top_bin_coefficients.png"), p_top_bin,
         width = max(10, length(wx_vars) * 6),
         height = max(4, n_countries * 0.5 + 2), dpi = 150, bg = "white")
}

# =============================================================================
# SECTION 5 -- FIXEST OLS VS RIF DISTRIBUTIONAL (1to3m, binned_equal top bin)
# =============================================================================

rif_coefs <- coef |>
  filter(ref_period == FOCUS_REF, construction == FOCUS_CONSTR,
         engine == "rif", model == "fit3", is.na(interaction),
         grepl("Inf\\]$", term), !grepl(":", term)) |>
  mutate(
    quantile = tau,
    estimate = ifelse(wx_var %in% SPEI_VARS, -estimate, estimate)
  )

ols_mean <- coef |>
  filter(ref_period == FOCUS_REF, construction == FOCUS_CONSTR,
         engine == "fixest", model == "fit3", estimand == "Mean",
         is.na(interaction),
         grepl("Inf\\]$", term), !grepl(":", term)) |>
  mutate(estimate = ifelse(wx_var %in% SPEI_VARS, -estimate, estimate))

if (nrow(rif_coefs) > 0) {
  # Build per-wx_var plots and combine
  rif_plots <- list()
  for (wv in wx_vars) {
    rif_wv <- rif_coefs |> filter(wx_var == wv)
    ols_wv <- ols_mean  |> filter(wx_var == wv) |> select(code, ols_est = estimate)
    if (nrow(rif_wv) == 0) next

    rif_plots[[wv]] <- rif_wv |>
      left_join(ols_wv, by = "code") |>
      ggplot(aes(x = quantile, y = estimate)) +
      geom_hline(aes(yintercept = ols_est), linetype = "dotted",
                 color = "orange", linewidth = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50",
                 linewidth = 0.3) +
      geom_ribbon(aes(ymin = estimate - 1.96 * std_error,
                       ymax = estimate + 1.96 * std_error),
                  fill = "steelblue", alpha = 0.2) +
      geom_line(color = "steelblue", linewidth = 0.6) +
      geom_point(aes(shape = ifelse(p_value < 0.05, "sig", "ns")),
                 color = "steelblue", size = 1.5) +
      scale_shape_manual(values = c("sig" = 16, "ns" = 1), guide = "none") +
      facet_wrap(~code, scales = "free_y",
                 ncol = min(5, n_countries)) +
      scale_x_continuous(breaks = seq(0.1, 0.9, 0.2),
                         labels = paste0("p", seq(10, 90, 20))) +
      labs(title = paste0(wv, ": RIF quantile profile — top bin (", FOCUS_REF, ", ", FOCUS_CONSTR_LABEL, ")"),
           subtitle = "Blue = UQR top-bin coefficient +/- 95% CI. Orange dotted = OLS mean. SPEI negated. Filled = p<0.05.",
           x = "Quantile", y = "UQR coefficient") +
      theme_minimal(base_size = 10) +
      theme(strip.text = element_text(face = "bold", size = 8))
  }

  if (length(rif_plots) > 0) {
    p_rif <- wrap_plots(rif_plots, ncol = 1)
    ggsave(file.path(MOD_DIR, "03a_4_rif_distributional.png"), p_rif,
           width = max(10, min(5, n_countries) * 2.8),
           height = max(4, ceiling(n_countries / 5) * 2.5) * length(wx_vars),
           dpi = 150, bg = "white")
  }
}

# Distributional gradient: p10 vs p90
rif_n_sig <- rif_coefs |>
  filter(p_value < 0.05) |>
  count(code, wx_var, name = "n_sig_all")

rif_gradient <- rif_coefs |>
  filter(quantile %in% c(0.1, 0.5, 0.9)) |>
  select(code, wx_var, quantile, estimate, p_value) |>
  pivot_wider(names_from = quantile, values_from = c(estimate, p_value),
              names_sep = "_q") |>
  left_join(rif_n_sig, by = c("code", "wx_var")) |>
  mutate(
    n_sig_all = replace_na(n_sig_all, 0L),
    gradient = estimate_q0.1 - estimate_q0.9,
    pattern = case_when(
      abs(estimate_q0.1) > abs(estimate_q0.9) * 1.5 ~ "bottom-heavy",
      abs(estimate_q0.9) > abs(estimate_q0.1) * 1.5 ~ "top-heavy",
      TRUE ~ "uniform"
    )
  )

cat("=== RIF distributional gradient ===\n")
print(as.data.frame(rif_gradient |> select(code, wx_var, estimate_q0.1,
       estimate_q0.5, estimate_q0.9, gradient, pattern, n_sig_all)),
      row.names = FALSE)
cat("\n")

# =============================================================================
# SECTION 6 -- POLICY INTERACTIONS x TOP BIN (1to3m)
# =============================================================================

# Negate SPEI interaction coefficients so positive = protective against the
# extreme of interest (heat for t, drought for SPEI) across both variables.
top_bin_int <- coef |>
  filter(ref_period == FOCUS_REF, engine == "fixest", model == "fit3",
         estimand == "Mean", interaction %in% POLICY_VARS,
         grepl("Inf\\]:", term),
         construction == FOCUS_CONSTR) |>
  mutate(
    raw_estimate = estimate,
    estimate  = ifelse(wx_var %in% SPEI_VARS, -estimate, estimate),
    statistic = ifelse(wx_var %in% SPEI_VARS, -statistic, statistic)
  )

int_summary <- top_bin_int |>
  group_by(code, wx_var, interaction) |>
  summarise(best_p   = min(p_value, na.rm = TRUE),
            best_est = estimate[which.min(p_value)],
            best_t   = statistic[which.min(p_value)],
            .groups  = "drop") |>
  mutate(
    sig_flag = case_when(best_p < 0.001 ~ "***", best_p < 0.01 ~ "**",
                         best_p < 0.05  ~ "*",   TRUE ~ ""),
    signed_logp = sign(best_est) * -log10(pmax(best_p, 1e-20)),
    extreme_label = ifelse(wx_var %in% SPEI_VARS,
                           paste0(wx_var, "\n(drought)"),
                           paste0(wx_var, "\n(heat)"))
  )

if (nrow(int_summary) > 0) {
  logp_lim <- max(abs(int_summary$signed_logp), na.rm = TRUE)
  if (!is.finite(logp_lim)) logp_lim <- 3

  p_int <- int_summary |>
    ggplot(aes(x = code, y = interaction, fill = signed_logp)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(sig_flag != "",
                    paste0(sprintf("%.3f", best_est), sig_flag), "")),
              size = 2, color = "grey15") +
    facet_wrap(~extreme_label, ncol = 1) +
    scale_fill_gradient2(low = "#b2182b", mid = "grey95", high = "#2166ac",
                         midpoint = 0, limits = c(-logp_lim, logp_lim),
                         name = "signed\n-log10(p)") +
    labs(title = paste0("Policy x extreme weather interactions (", FOCUS_REF, ", ", FOCUS_CONSTR_LABEL, ")"),
         subtitle = paste0("Blue = protective, Red = amplifying. SPEI negated (drought focus). ",
                           "*** p<0.001 ** p<0.01 * p<0.05"),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(file.path(MOD_DIR, "03a_5_policy_interactions.png"), p_int,
         width = max(8, n_countries * 1.2 + 4),
         height = max(5, length(POLICY_VARS) * length(wx_vars) * 0.35 + 3),
         dpi = 150, bg = "white")
}

sig_interactions <- int_summary |> filter(best_p < 0.05) |>
  arrange(code, wx_var, interaction)

cat("=== Significant policy x top bin interactions (p<0.05) ===\n")
if (nrow(sig_interactions) > 0) {
  print(as.data.frame(sig_interactions |>
    select(code, wx_var, interaction, best_est, best_p)),
    row.names = FALSE)
} else {
  cat("  None.\n")
}
cat("\n")


# =============================================================================
# SECTION 6b -- RIF DISTRIBUTIONAL POLICY INTERACTIONS (1to3m)
# =============================================================================

# RIF top-bin x policy interactions across quantiles.
# Even if the OLS (mean) interaction is weak, the interaction may be strong at
# specific quantiles — especially at the bottom, where policy interventions
# targeting the poorest are most valuable.

rif_topbin_int <- coef |>
  filter(ref_period == FOCUS_REF, engine == "rif", model == "fit3",
         interaction %in% POLICY_VARS,
         grepl("Inf\\]:", term),
         construction == FOCUS_CONSTR) |>
  mutate(
    quantile = tau,
    estimate = ifelse(wx_var %in% SPEI_VARS, -estimate, estimate)
  )

if (nrow(rif_topbin_int) > 0) {
  # Per country x wx_var x policy x bin_type: count sig quantiles & gradient
  rif_int_profile <- rif_topbin_int |>
    group_by(code, wx_var, interaction) |>
    summarise(
      n_sig      = sum(p_value < 0.05),
      n_quantiles = n(),
      est_p10 = estimate[quantile == 0.1],
      est_p50 = estimate[quantile == 0.5],
      est_p90 = estimate[quantile == 0.9],
      gradient    = est_p10 - est_p90,
      max_abs_est = estimate[which.max(abs(estimate))],
      best_q_p    = min(p_value),
      .groups     = "drop"
    ) |>
    mutate(
      pattern = case_when(
        abs(est_p10) > abs(est_p90) * 1.5 ~ "bottom-heavy",
        abs(est_p90) > abs(est_p10) * 1.5 ~ "top-heavy",
        TRUE ~ "uniform"
      )
    )

  # Compare with OLS: flag hidden distributional effects
  rif_int_vs_ols <- rif_int_profile |>
    left_join(
      int_summary |>
        select(code, wx_var, interaction,
               ols_est = best_est, ols_p = best_p),
      by = c("code", "wx_var", "interaction")
    ) |>
    mutate(
      ols_sig  = !is.na(ols_p) & ols_p < 0.05,
      rif_any  = n_sig > 0,
      rif_strong = n_sig >= 3,
      hidden   = !ols_sig & rif_any
    )

  hidden_effects <- rif_int_vs_ols |> filter(hidden) |>
    arrange(code, wx_var, interaction)

  cat("=== RIF distributional policy interactions ===\n")
  cat(sprintf("Total combinations: %d\n", nrow(rif_int_vs_ols)))
  cat(sprintf("OLS significant: %d | RIF any sig quantile: %d | Hidden (RIF only): %d\n",
              sum(rif_int_vs_ols$ols_sig), sum(rif_int_vs_ols$rif_any),
              nrow(hidden_effects)))
  if (nrow(hidden_effects) > 0) {
    cat("\nHidden distributional effects (OLS n.s., RIF sig at some quantiles):\n")
    print(as.data.frame(hidden_effects |>
      select(code, wx_var, interaction, ols_p, n_sig, gradient,
             pattern, est_p10, est_p50, est_p90)), row.names = FALSE)
  }
  cat("\n")

  # --- Heatmap: n_sig RIF quantiles per combination ---
  rif_int_heatmap <- rif_int_vs_ols |>
    mutate(
      label = case_when(
        hidden ~ paste0(n_sig, "q *"),
        rif_any ~ paste0(n_sig, "q"),
        TRUE ~ ""
      ),
      extreme_label = ifelse(wx_var %in% SPEI_VARS,
                             paste0(wx_var, "\n(drought)"),
                             paste0(wx_var, "\n(heat)"))
    )

  p_rif_int <- rif_int_heatmap |>
    ggplot(aes(x = code, y = interaction, fill = n_sig)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = label), size = 2.2, color = "grey15") +
    geom_point(data = rif_int_heatmap |> filter(hidden),
               shape = 8, size = 2, color = "red") +
    facet_wrap(~extreme_label, ncol = 1) +
    scale_fill_gradient(low = "grey95", high = "steelblue", name = "# sig\nquantiles",
                        limits = c(0, 9), breaks = c(0, 3, 6, 9)) +
    labs(title = paste0("RIF: policy x extreme weather across quantiles (", FOCUS_REF, ", ", FOCUS_CONSTR_LABEL, ")"),
         subtitle = paste0("Number of quantiles (of 9) with p<0.05 interaction. ",
                           "Red star = hidden effect (OLS n.s. but RIF significant)."),
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(file.path(MOD_DIR, "03a_6_rif_policy_interactions.png"), p_rif_int,
         width = max(8, n_countries * 1.2 + 4),
         height = max(5, length(POLICY_VARS) * length(wx_vars) * 0.35 + 3),
         dpi = 150, bg = "white")

  # --- Per-policy-variable RIF quantile profiles (all available data) ---
  ols_overlay_all <- int_summary |>
    select(code, wx_var, interaction, ols_est = best_est)

  all_profile_data <- rif_topbin_int |>
    left_join(ols_overlay_all, by = c("code", "wx_var", "interaction")) |>
    mutate(
      extreme_label = ifelse(wx_var %in% SPEI_VARS,
                             paste0(wx_var, " (drought)"),
                             paste0(wx_var, " (heat)"))
    )

  for (pol_var in sort(unique(all_profile_data$interaction))) {
    pol_data <- all_profile_data |> filter(interaction == pol_var)
    if (nrow(pol_data) == 0) next
    n_ctry_pol <- n_distinct(pol_data$code)
    n_wx_pol   <- n_distinct(pol_data$wx_var)

    p_pol <- pol_data |>
      ggplot(aes(x = quantile, y = estimate)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "grey50",
                 linewidth = 0.3) +
      geom_hline(aes(yintercept = ols_est), linetype = "dotted",
                 color = "orange", linewidth = 0.5) +
      geom_ribbon(aes(ymin = estimate - 1.96 * std_error,
                       ymax = estimate + 1.96 * std_error),
                  fill = "steelblue", alpha = 0.2) +
      geom_line(color = "steelblue", linewidth = 0.6) +
      geom_point(aes(shape = ifelse(p_value < 0.05, "sig", "ns")),
                 color = "steelblue", size = 1.5) +
      scale_shape_manual(values = c("sig" = 16, "ns" = 1), guide = "none") +
      facet_grid(code ~ extreme_label, scales = "free_y") +
      scale_x_continuous(breaks = seq(0.1, 0.9, 0.2),
                         labels = paste0("p", seq(10, 90, 20))) +
      labs(title = paste0("RIF: ", pol_var, " x extreme weather (", FOCUS_REF, ", ", FOCUS_CONSTR_LABEL, ")"),
           subtitle = "Blue = UQR interaction +/- 95% CI. Orange dotted = OLS mean. SPEI negated.",
           x = "Quantile", y = "Interaction coefficient") +
      theme_minimal(base_size = 10) +
      theme(strip.text = element_text(face = "bold", size = 9))

    ggsave(file.path(MOD_DIR, paste0("03a_7_rif_profiles_", pol_var, ".png")),
           p_pol,
           width = max(6, n_wx_pol * 3.5),
           height = max(4, n_ctry_pol * 2.2 + 1.5),
           dpi = 150, bg = "white")
  }
}

# =============================================================================
# SECTION 7 -- RECOMMENDATION MATRIX
# =============================================================================

# --- Continuous significance per country x wx_var ---
cont_sig <- cont_main |>
  filter(ref_period == FOCUS_REF) |>
  select(code, wx_var, cont_est = estimate, cont_se = std_error,
         cont_p = p_value, cont_t = statistic)

# --- Top bin significance per country x wx_var ---
topbin_sig <- top_bin_main |>
  select(code, wx_var, topbin_est = estimate, topbin_se = std_error,
         topbin_p = p_value)

# --- RIF gradient ---
rif_rec <- rif_gradient |>
  select(code, wx_var, gradient, pattern, n_sig_all) |>
  mutate(rif_matters = abs(gradient) > 0 & n_sig_all >= 3)

# --- Policy interactions per country x wx_var (OLS + RIF hidden) ---
# OLS-significant interactions
ols_policy_rec <- sig_interactions |>
  group_by(code, wx_var) |>
  summarise(ols_policies = paste(unique(interaction), collapse = ", "),
            .groups = "drop")

# RIF-only (hidden) interactions: OLS n.s. but RIF significant at some quantiles
rif_policy_rec <- if (exists("hidden_effects") && nrow(hidden_effects) > 0) {
  hidden_effects |>
    group_by(code, wx_var) |>
    summarise(rif_only_policies = paste(unique(interaction), collapse = ", "),
              .groups = "drop")
} else {
  tibble(code = character(), wx_var = character(), rif_only_policies = character())
}

policy_rec <- ols_policy_rec |>
  full_join(rif_policy_rec, by = c("code", "wx_var")) |>
  mutate(
    ols_policies = replace_na(ols_policies, ""),
    rif_only_policies = replace_na(rif_only_policies, ""),
    sig_policies = ifelse(
      nzchar(rif_only_policies) & nzchar(ols_policies),
      paste0(ols_policies, "; RIF-only: ", rif_only_policies),
      ifelse(nzchar(rif_only_policies),
             paste0("RIF-only: ", rif_only_policies),
             ols_policies)
    )
  )

# Build recommendation table
rec <- cont_sig |>
  left_join(rif_rec |> select(code, wx_var, pattern, rif_matters),
            by = c("code", "wx_var")) |>
  left_join(policy_rec, by = c("code", "wx_var")) |>
  mutate(
    signal = case_when(
      cont_p < 0.01  ~ "strong",
      cont_p < 0.05  ~ "moderate",
      cont_p < 0.10  ~ "weak",
      TRUE           ~ "none"
    ),
    sig_policies = replace_na(sig_policies, ""),
    rif_matters  = replace_na(rif_matters, FALSE),
    pattern      = replace_na(pattern, "unknown")
  ) |>
  arrange(wx_var, cont_p)

cat("=== Recommendation matrix ===\n")
print(as.data.frame(rec |> select(code, wx_var, signal, cont_est, cont_p,
      rif_matters, pattern, sig_policies)), row.names = FALSE)
cat("\n")

# =============================================================================
# SECTION 8 -- MARKDOWN SUMMARY
# =============================================================================

md <- character()
L <- function(...) md <<- c(md, paste0(...))

L("# Model Selection Analysis")
L("")
L("Comparison of weather constructions, reference periods, model types, and ",
  "policy interactions from `batch/03_run_mod1.R` outputs. Informs which ",
  "specifications to prioritise for climate and policy simulations.")
L("")
L("**Locked-in**: Covariates = LASSO, Fixed effects = year_loc, ",
  "Weather construction = ", FOCUS_CONSTR_LABEL, ".")
L("")
L("Generated: ", Sys.Date())
L("")

# --- 1: Data summary ---
L("## 1. Data summary")
L("")
L("- **", n_countries, "** countries: ", paste0("`", countries, "`", collapse = ", "))
L("- **Weather variables**: ", paste0("`", wx_vars, "`", collapse = ", "))
L("- **Reference periods**: ", paste(REF_LEVELS, collapse = ", "),
  " (focus: **", FOCUS_REF, "**)")
L("- **Constructions compared** (Sections 2-3): continuous, binned (equal-frequency), binned (custom absolute breaks)")
L("- **Construction locked-in** (Sections 4-7): **", FOCUS_CONSTR_LABEL, "**")
L("- **Engines**: fixest (OLS with FE), RIF (unconditional quantile regression)")
L("- **Policy variables**: ", paste0("`", POLICY_VARS, "`", collapse = ", "))
L("")

if (nrow(int_na) > 0) {
  L("### Unavailable policy interactions")
  L("")
  int_na_policy <- int_na |> filter(interaction %in% POLICY_VARS) |>
    distinct(sample, interaction)
  if (nrow(int_na_policy) > 0) {
    for (ctry in sort(unique(int_na_policy$sample))) {
      v <- int_na_policy |> filter(sample == ctry) |> pull(interaction)
      L("- **", ctry, "**: ", paste(v, collapse = ", "))
    }
  } else {
    L("All policy variables available in all countries.")
  }
  L("")
}

# --- 2: Reference period comparison ---
L("## 2. Reference period comparison")
L("")
L("Continuous main effect, fixest OLS, fit3, LASSO, year_loc, no interaction. ",
  "Evaluates which reference period produces the strongest weather-welfare signal.")
L("")

for (wv in wx_vars) {
  rs <- ref_sig_summary |> filter(wx_var == wv)
  rf <- ref_fit_summary |> filter(wx_var == wv)
  L("### ", wv)
  L("")
  L("| Ref period | Sig (p<0.05) | Sig (p<0.10) | Median |t| | Median within-R² |")
  L("|-----------|-------------|-------------|-----------|-------------------|")
  for (i in seq_len(nrow(rs))) {
    r <- rs[i, ]
    r2 <- rf |> filter(ref_period == r$ref_period)
    r2v <- if (nrow(r2) > 0) sprintf("%.4f", r2$med_r2w) else "N/A"
    L("| ", as.character(r$ref_period), " | ", r$n_sig05, "/", n_countries,
      " | ", r$n_sig10, "/", n_countries,
      " | ", sprintf("%.2f", r$med_abs_t), " | ", r2v, " |")
  }
  L("")
}

best_ref <- ref_sig_summary |> filter(ref_period == FOCUS_REF)
L("**Decision**: Focus on **`", FOCUS_REF, "`** for all downstream analysis.")
L("")
L("![Reference period significance](03a_1_ref_period_significance.png)")
L("")
L("### Coefficient ranking by reference period")
L("")
L("Countries sorted by ", FOCUS_REF, " coefficient. ",
  "Separate plots per weather variable and construction type.")
L("")
for (suffix in c("cont", "binn_equal", "binn_custom")) {
  for (wv in wx_vars) {
    L("![", suffix, " ", wv, "](03a_1b_ref_coefs_", suffix, "_", wv, ".png)")
    L("")
  }
}

# --- 3: Weather construction comparison ---
L("## 3. Weather construction comparison (", FOCUS_REF, ")")
L("")
L("Compares continuous, equal-frequency binned, and custom (absolute) binned ",
  "specifications on within-R² (variation explained after absorbing FE) and AIC.")
L("")
L("Custom bins use absolute thresholds identical across countries ",
  "(temperature: 25/26/27/28°C; SPEI6: -1/-0.5/0/0.5). ",
  "Equal-frequency bins use country-specific quantile-based cutpoints.")
L("")

L("### Within-R² by construction")
L("")
L("| Country | Wx var | Continuous | Equal freq | Custom | Best |")
L("|---------|--------|-----------|-----------|--------|------|")
for (i in seq_len(nrow(fit_wide))) {
  r <- fit_wide[i, ]
  best <- constr_winner |>
    filter(code == r$code, wx_var == r$wx_var) |> pull(construction)
  L("| ", r$code, " | ", r$wx_var,
    " | ", sprintf("%.4f", r$r2_within_continuous),
    " | ", sprintf("%.4f", r$r2_within_binned_equal),
    " | ", sprintf("%.4f", r$r2_within_binned_custom),
    " | ", as.character(best), " |")
}
L("")

L("### Delta within-R² (binned minus continuous)")
L("")
L("| Country | Wx var | Equal freq - Cont | Custom - Cont |")
L("|---------|--------|-------------------|---------------|")
for (i in seq_len(nrow(fit_wide))) {
  r <- fit_wide[i, ]
  L("| ", r$code, " | ", r$wx_var,
    " | ", sprintf("%+.4f", r$r2w_diff_equal),
    " | ", sprintf("%+.4f", r$r2w_diff_custom), " |")
}
L("")

constr_counts <- constr_winner |> count(construction, name = "n")
L("**Summary**: Best construction across all country-variable pairs: ",
  paste(sprintf("%s (%d)", constr_counts$construction, constr_counts$n), collapse = ", "),
  ". **Decision**: use **", FOCUS_CONSTR_LABEL, "** for all downstream analysis.")
L("")
L("![Construction fit](03a_2_construction_fit.png)")
L("")

# --- 4: Extreme weather effects ---
L("## 4. Extreme weather effects: heat and drought (", FOCUS_REF, ")")
L("")
L("For **temperature**, the top bin (containing +Inf) captures extreme heat relative ",
  "to the omitted moderate bin. For **SPEI6**, the bottom bin (drought) is the omitted ",
  "reference, so all SPEI coefficients represent welfare relative to drought. ",
  "To present both variables on a consistent scale, SPEI coefficients are **negated**: ",
  "the value shown is the drought penalty (negative = drought hurts welfare). ",
  "This means for both variables, **negative = the extreme hurts welfare**.")
L("")

if (nrow(top_bin_main) > 0) {
  L("### Extreme effect coefficients (", FOCUS_CONSTR_LABEL, ")")
  L("")
  L("| Country | Wx var | Focus | Bin range | Coefficient | SE | p-value |")
  L("|---------|--------|-------|-----------|-------------|-----|---------|")
  for (i in seq_len(nrow(top_bin_main |> arrange(wx_var, code)))) {
    r <- (top_bin_main |> arrange(wx_var, code))[i, ]
    focus <- ifelse(r$wx_var %in% SPEI_VARS, "drought", "heat")
    L("| ", r$code, " | ", r$wx_var, " | ", focus,
      " | ", r$term, " | ", sprintf("%.4f", r$estimate),
      " | ", sprintf("%.4f", r$std_error),
      " | ", sprintf("%.2e", r$p_value), " |")
  }
  L("")
  if (length(SPEI_VARS) > 0) {
    L("> *SPEI coefficients are negated from the raw top-bin (wet) coefficient. ",
      "A negative value means drought reduces welfare relative to wet conditions.*")
    L("")
  }

  top_bin_sig_count <- top_bin_main |> filter(sig) |>
    count(wx_var, name = "n_sig")
  if (nrow(top_bin_sig_count) > 0) {
    L("**Significant extreme effects (p<0.05)**: ",
      paste(sprintf("%s (%d/%d countries)", top_bin_sig_count$wx_var,
                    top_bin_sig_count$n_sig,
                    n_countries), collapse = "; "), ".")
    L("")
  }
}

L("![Extreme weather effects](03a_3_top_bin_coefficients.png)")
L("")

# --- 5: Distributional effects ---
L("## 5. Distributional effects: OLS vs RIF (", FOCUS_REF, ", ", FOCUS_CONSTR_LABEL, ", top bin)")
L("")
L("RIF (unconditional quantile regression) estimates the extreme weather (top bin) effect ",
  "at each decile of the welfare distribution. SPEI coefficients are negated (drought focus). ",
  "The OLS mean effect (orange dotted line in plots) ",
  "may mask important heterogeneity -- if the effect is stronger at the bottom of the ",
  "distribution, poorer households bear a disproportionate burden.")
L("")

if (nrow(rif_gradient) > 0) {
  L("### Distributional gradient")
  L("")
  L("| Country | Wx var | Coef p10 | Coef p50 | Coef p90 | Gradient (p10-p90) | Pattern | # Sig quantiles |")
  L("|---------|--------|----------|----------|----------|--------------------|---------|-----------------|")
  for (i in seq_len(nrow(rif_gradient |> arrange(wx_var, code)))) {
    r <- (rif_gradient |> arrange(wx_var, code))[i, ]
    L("| ", r$code, " | ", r$wx_var,
      " | ", sprintf("%.4f", r$estimate_q0.1),
      " | ", sprintf("%.4f", r$estimate_q0.5),
      " | ", sprintf("%.4f", r$estimate_q0.9),
      " | ", sprintf("%+.4f", r$gradient),
      " | ", r$pattern, " | ", r$n_sig_all, "/9 |")
  }
  L("")

  pattern_counts <- rif_gradient |> count(pattern, name = "n")
  L("**Pattern summary**: ",
    paste(sprintf("%s (%d)", pattern_counts$pattern, pattern_counts$n), collapse = ", "), ".")
  L("")

  bottom_heavy <- rif_gradient |> filter(pattern == "bottom-heavy")
  if (nrow(bottom_heavy) > 0) {
    L("**Pro-poor vulnerability**: In ",
      paste0(sprintf("`%s` (%s)", bottom_heavy$code, bottom_heavy$wx_var), collapse = ", "),
      ", the weather effect is stronger at lower welfare quantiles -- poorer households ",
      "are disproportionately affected. RIF models are recommended for these country-variable pairs ",
      "to capture distributional targeting.")
    L("")
  }
}

L("![RIF distributional](03a_4_rif_distributional.png)")
L("")

# --- 6: Policy interactions ---
L("## 6. Policy interactions x extreme weather (", FOCUS_REF, ", ", FOCUS_CONSTR_LABEL, ")")
L("")
L("Interaction terms between extreme weather bins and policy variables. ",
  "For temperature: interaction with the top bin (heat). ",
  "For SPEI: interaction with the top bin (wet vs drought reference), **negated** so that ",
  "the interpretation is consistent — a **positive** coefficient means the policy is ",
  "**protective** against the extreme of interest (heat or drought); ",
  "a **negative** coefficient means **amplifying**.")
L("")

if (nrow(sig_interactions) > 0) {
  L("### Significant interactions (p<0.05)")
  L("")
  L("| Country | Wx var | Policy variable | Coefficient | p-value | Direction |")
  L("|---------|--------|----------------|-------------|---------|-----------|")
  for (i in seq_len(nrow(sig_interactions))) {
    r <- sig_interactions[i, ]
    dir <- ifelse(r$best_est > 0, "protective", "amplifying")
    L("| ", r$code, " | ", r$wx_var, " | ", r$interaction,
      " | ", sprintf("%.4f", r$best_est),
      " | ", sprintf("%.2e", r$best_p),
      " | ", dir, " |")
  }
  L("")
} else {
  L("No significant policy x top bin interactions found at p<0.05.")
  L("")
}

# Interaction ranking across countries
int_ranking <- int_summary |>
  group_by(wx_var, interaction) |>
  summarise(n_ctry = n_distinct(code),
            n_sig = sum(best_p < 0.05),
            med_est = median(best_est, na.rm = TRUE),
            .groups = "drop") |>
  arrange(wx_var, desc(n_sig))

if (nrow(int_ranking) > 0) {
  L("### Interaction ranking (all countries)")
  L("")
  L("| Wx var | Policy variable | Countries with data | Sig (p<0.05) | Median coef |")
  L("|--------|----------------|--------------------|--------------|--------------------|")
  for (i in seq_len(nrow(int_ranking))) {
    r <- int_ranking[i, ]
    L("| ", r$wx_var, " | ", r$interaction,
      " | ", r$n_ctry, " | ", r$n_sig,
      " | ", sprintf("%.4f", r$med_est), " |")
  }
  L("")
}


L("![Policy interactions](03a_5_policy_interactions.png)")
L("")

# --- 6b: RIF distributional policy interactions ---
if (exists("rif_int_vs_ols") && nrow(rif_int_vs_ols) > 0) {
  L("### Distributional analysis of policy interactions (RIF)")
  L("")
  L("Even if the mean (OLS) policy x weather interaction is weak, the interaction ",
    "may be strong at specific quantiles of the welfare distribution. A **hidden ",
    "distributional effect** occurs when the OLS interaction is not significant ",
    "(p>=0.05) but RIF detects significant interactions at one or more quantiles ",
    "-- indicating the policy targets a specific part of the distribution.")
  L("")

  n_ols_sig <- sum(rif_int_vs_ols$ols_sig)
  n_rif_any <- sum(rif_int_vs_ols$rif_any)
  n_hidden  <- if (exists("hidden_effects")) nrow(hidden_effects) else 0L
  L("| Metric | Count |")
  L("|--------|-------|")
  L("| Total policy x extreme x country combinations | ", nrow(rif_int_vs_ols), " |")
  L("| OLS significant (p<0.05) | ", n_ols_sig, " |")
  L("| RIF significant at any quantile | ", n_rif_any, " |")
  L("| **Hidden distributional effects** (OLS n.s., RIF sig) | ", n_hidden, " |")
  L("")

  if (n_hidden > 0) {
    L("#### Hidden distributional effects")
    L("")
    L("These policy interactions are **not significant in the OLS mean** but show ",
      "significant effects at specific quantiles. They may warrant policy simulations ",
      "targeting specific parts of the welfare distribution.")
    L("")
    L("| Country | Wx var | Policy | OLS p | # Sig quantiles | Coef p10 | Coef p50 | Coef p90 | Pattern |")
    L("|---------|--------|--------|-------|-----------------|----------|----------|----------|---------|")
    for (i in seq_len(nrow(hidden_effects))) {
      r <- hidden_effects[i, ]
      L("| ", r$code, " | ", r$wx_var, " | ", r$interaction,
        " | ", sprintf("%.3f", r$ols_p),
        " | ", r$n_sig, "/9",
        " | ", sprintf("%.4f", r$est_p10),
        " | ", sprintf("%.4f", r$est_p50),
        " | ", sprintf("%.4f", r$est_p90),
        " | ", r$pattern, " |")
    }
    L("")

    bottom_hidden <- hidden_effects |> filter(pattern == "bottom-heavy")
    if (nrow(bottom_hidden) > 0) {
      L("**Pro-poor hidden interactions**: ",
        paste0(sprintf("`%s` %s x %s", bottom_hidden$code, bottom_hidden$wx_var,
                       bottom_hidden$interaction),
               collapse = "; "),
        " -- the policy interaction is strongest at the bottom of the welfare ",
        "distribution, suggesting potential for targeted interventions even though ",
        "the average effect is not significant.")
      L("")
    }
  }

  L("![RIF policy interactions](03a_6_rif_policy_interactions.png)")
  L("")
  if (exists("all_profile_data") && nrow(all_profile_data) > 0) {
    profile_pols <- sort(unique(all_profile_data$interaction))
    for (pol_var in profile_pols) {
      L("![RIF profile: ", pol_var, "](03a_7_rif_profiles_", pol_var, ".png)")
      L("")
    }
  }
}

# --- 7: Recommendations ---
L("## 7. Recommendations for simulation")
L("")

# 7a: Weather construction (locked in)
L("### Weather construction")
L("")
L("**Locked-in**: **", FOCUS_CONSTR_LABEL, "** — selected based on within-R² ",
  "comparison in Section 3. All downstream analysis (Sections 4-7) uses this construction.")
L("")
topbin_any_sig <- n_distinct(top_bin_main$code[top_bin_main$sig])
L("Top bin is significant (p<0.05) in **", topbin_any_sig, "/", n_countries,
  "** countries (across any wx_var).")
L("")

# 7b: OLS vs RIF
n_rif_matters <- sum(rif_rec$rif_matters, na.rm = TRUE)
L("### Model type: OLS vs RIF")
L("")
L("RIF distributional analysis shows meaningful heterogeneity (|gradient| > 0, ",
  "3+ quantiles significant) in **", n_rif_matters, "/",
  nrow(rif_rec), "** country-variable pairs.")
L("")
if (n_rif_matters > 0) {
  rif_yes <- rif_rec |> filter(rif_matters)
  L("Countries/variables where RIF adds value: ",
    paste0(sprintf("`%s` (%s, %s)", rif_yes$code, rif_yes$wx_var, rif_yes$pattern),
           collapse = ", "), ".")
  L("")
}
L("For simulation: use **fixest OLS as default** (mean welfare effect). ",
  "Add RIF for countries/variables with distributional heterogeneity to capture ",
  "differential impacts across the welfare distribution.")
L("")

# 7c: Policy simulations per country
n_hidden_total <- if (exists("hidden_effects")) nrow(hidden_effects) else 0L
L("### Policy simulations by country")
L("")
L("Policy variables with significant interactions (p<0.05) should be ",
  "included in simulation scenarios. **OLS-significant** interactions affect mean welfare; ",
  "**RIF-only** interactions (marked below) are not significant on average but show ",
  "significant distributional effects -- they warrant simulation if targeting specific ",
  "parts of the welfare distribution.")
if (n_hidden_total > 0) {
  L(" There are **", n_hidden_total, "** hidden distributional effects ",
    "where the OLS mean is not significant but RIF detects quantile-specific effects.")
}
L("")

for (ctry in countries) {
  ctry_int <- sig_interactions |> filter(code == ctry)
  if (nrow(ctry_int) > 0) {
    policies <- ctry_int |>
      mutate(label = paste0(interaction, " (", wx_var, ")")) |>
      pull(label)
    L("- **", ctry, "**: ", paste(policies, collapse = "; "))
  } else {
    L("- **", ctry, "**: no significant policy x top bin interactions")
  }
}
L("")

# 7d: Simulation priority matrix
L("### Simulation priority matrix")
L("")
L("Per-country recommendations combining signal strength, engine, and policy interactions. ",
  "Construction locked-in as ", FOCUS_CONSTR_LABEL, ".")
L("")
L("| Country | Wx var | Signal | Use RIF? | Policy interactions |")
L("|---------|--------|--------|----------|---------------------|")
for (i in seq_len(nrow(rec))) {
  r <- rec[i, ]
  rif_str <- ifelse(r$rif_matters, paste0("Yes (", r$pattern, ")"), "No")
  pol_str <- ifelse(nzchar(r$sig_policies), r$sig_policies, "--")
  L("| ", r$code, " | ", r$wx_var, " | ", r$signal,
    " | ", rif_str, " | ", pol_str, " |")
}
L("")

# Signal summary
signal_counts <- rec |> count(signal) |> arrange(match(signal, c("strong","moderate","weak","none")))
L("**Signal summary**: ",
  paste(sprintf("%s (%d)", signal_counts$signal, signal_counts$n), collapse = ", "), ".")
L("")

# Prioritisation advice
strong_pairs <- rec |> filter(signal %in% c("strong", "moderate"))
if (nrow(strong_pairs) > 0) {
  L("**Prioritise**: Countries/variables with strong or moderate signals ",
    "for climate simulation. Those with significant policy interactions ",
    "should also run policy scenarios (intervention counterfactuals).")
  L("")
}

weak_or_none <- rec |> filter(signal %in% c("weak", "none"))
if (nrow(weak_or_none) > 0) {
  L("**Deprioritise**: ",
    paste0(sprintf("`%s` (%s)", weak_or_none$code, weak_or_none$wx_var), collapse = ", "),
    " -- weak or no weather-welfare signal under year_loc FE. ",
    "Simulation results for these pairs should be interpreted with caution.")
  L("")
}

# --- Write ---
writeLines(md, file.path(MOD_DIR, "model_selection_summary.md"))
cat(sprintf("\n=== Saved: %s ===\n", file.path(MOD_DIR, "model_selection_summary.md")))
cat(sprintf("=== Plots saved to: %s ===\n", MOD_DIR))
