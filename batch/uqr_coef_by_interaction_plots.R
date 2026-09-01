
library(tidyverse)
library(arrow)
library(ggplot2)

OUT_DIR <- "dev/outputs/case_studies"
MODEL_DIR <- file.path(OUT_DIR, "model_fit")

COEF_PLOT_DIR <- file.path(MODEL_DIR, "coef_plots")
dir.create(COEF_PLOT_DIR, showWarnings = FALSE, recursive = TRUE)

FOCUS_WX <- "t_1to3m_binn_None"

# Human-readable labels for each interaction variable (value == 1 → label[1], value == 0 → label[2])
interaction_meta <- list(
  electricity = list(
    label_1 = "Access to electricity",
    label_0 = "No access to electricity",
    colors  = c("Access to electricity" = "#2166ac", "No access to electricity" = "#d6604d")
  ),
  educ_com2_hh = list(
    label_1 = "Completed secondary education",
    label_0 = "Not completed secondary education",
    colors  = c("Completed secondary education" = "#2166ac", "Not completed secondary education" = "#d6604d")
  ),
  urban = list(
    label_1 = "Urban",
    label_0 = "Rural",
    colors  = c("Urban" = "#2166ac", "Rural" = "#d6604d")
  )
)

# ── Load data ────────────────────────────────────────────────────────────────
outcomes_all <- read_parquet(file.path(MODEL_DIR, "model_coefficients.parquet")) |>
  filter(
    weather    == FOCUS_WX,
    model      == "fit3",
    engine     == "rif",
    !is.na(interaction)
  )

# ── Helper: build plot-ready data for one interaction variable ────────────────
# Each temperature-bin term (e.g. "t(29.6, Inf]") represents the effect for
# the reference group (interaction var == 0).  The companion term
# "t(29.6, Inf]:interaction_var" is the *additional* effect for group == 1,
# so group-1 effect = main + interaction.
build_interaction_data <- function(df, interact_var) {
  meta <- interaction_meta[[interact_var]]

  # Temperature-bin terms (no colon → main effect, with colon → interaction delta)
  wx_pattern   <- "^t[(]"
  inter_suffix <- paste0(":", interact_var)

  df_sub <- df |> filter(interaction == interact_var)

  main_terms <- df_sub |>
    filter(grepl(wx_pattern, term), !grepl(":", term)) |>
    select(code, term, tau, estimate, std_error)

  inter_terms <- df_sub |>
    filter(grepl(wx_pattern, term), grepl(inter_suffix, term)) |>
    mutate(base_term = str_remove(term, fixed(inter_suffix))) |>
    select(code, base_term, tau, inter_est = estimate, inter_se = std_error)

  combined <- main_terms |>
    left_join(inter_terms, by = c("code", "term" = "base_term", "tau")) |>
    mutate(
      # group 0 = main effect
      est_0 = estimate,
      se_0  = std_error,
      # group 1 = main + interaction delta; SE combined via quadrature
      est_1 = estimate + inter_est,
      se_1  = sqrt(std_error^2 + inter_se^2),
      label_0 = meta$label_0,
      label_1 = meta$label_1
    ) |>
    select(code, term, tau, est_0, se_0, est_1, se_1, label_0, label_1)

  # Pivot to long form, one row per (group × tau)
  bind_rows(
    combined |> transmute(code, term, tau,
                          group = label_0, estimate = est_0, std_error = se_0),
    combined |> transmute(code, term, tau,
                          group = label_1, estimate = est_1, std_error = se_1)
  ) |>
    mutate(
      ci_lo = estimate - 1.96 * std_error,
      ci_hi = estimate + 1.96 * std_error,
      pct_label = paste0(tau * 100, "%"),
      interact_var = interact_var
    )
}

# ── Build plot data for all available interactions ────────────────────────────
plot_data <- outcomes_all |>
  pull(interaction) |>
  unique() |>
  setdiff(NA_character_) |>
  map(\(iv) build_interaction_data(outcomes_all, iv)) |>
  bind_rows()

# ── Plotting function: one panel per (country × temperature bin) ──────────────
make_interaction_plot <- function(data, interact_var, country, bin_term) {
  meta <- interaction_meta[[interact_var]]

  pdata <- data |>
    filter(interact_var == !!interact_var, code == country, term == bin_term) |>
    mutate(
      group    = factor(group, levels = c(meta$label_1, meta$label_0)),
      pct_label = factor(pct_label, levels = paste0(seq(10, 90, 10), "%"))
    )

  ggplot(pdata, aes(x = pct_label, y = estimate, colour = group, fill = group, group = group)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50", linewidth = 0.4) +
    geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.15, colour = NA) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 2.5) +
    scale_colour_manual(values = meta$colors) +
    scale_fill_manual(values = meta$colors) +
    labs(
      x      = "Welfare quantile",
      y      = "UQR coefficient",
      colour = NULL,
      fill   = NULL
    ) +
    theme_bw(base_size = 13) +
    theme(
      panel.border      = element_blank(),
      panel.grid.minor  = element_blank(),
      legend.position   = "bottom",
      legend.text       = element_text(size = 10)
    )
}

# ── Save one PNG per (interaction × country) for the top temperature bin ──────
plot_data |>
  filter(grepl("Inf]", term)) |>
  distinct(interact_var, code, term) |>
  pwalk(\(interact_var, code, term) {
    p <- make_interaction_plot(plot_data, interact_var, code, term)

    safe_term <- term |>
      str_replace_all("[(),]", "") |>
      str_replace_all("\\s+", "_") |>
      str_replace_all("[^[:alnum:]_]", "")

    filename <- file.path(
      COEF_PLOT_DIR,
      paste0("uqr_", code, "_", interact_var, "_", safe_term, ".png")
    )

    ggsave(filename, p, width = 7, height = 5, dpi = 150)
    message("Saved: ", filename)
  })
