# =============================================================================
# dev/summarize_mod1_batch.R
#
# Summarize results from mod1 batch runs. See dev/run_mod1_batch_BB.R
#
# Usage: source("dev/summarize_mod1_batch.R")
# =============================================================================

rm(list = ls())
pkgload::load_all(quiet = TRUE)
source("dev/spec_curve.R") # For the spec curve plotting code

OUT_DIR         <- "dev/outputs/pooled"

# coefficient summary
coefs <- read.csv(file.path(OUT_DIR, "_summary_coefficients.csv"))

# model fit summary
fit_stats <- read.csv(file.path(OUT_DIR, "_summary_fit_stats.csv"))

# =============================================================================
# SPECIFICATION CURVES
# =============================================================================

spec_vars_all <- c(
  "wx_var", "wx_ref_period", "wx_binned", "engine",
  "fe_profile", "cov_profile",
  "interaction", "code"
)

spec_configs <- list(
  # --- Continuous -------------------------------------------------------------
  list(
    term = "t", term_regex = NULL,
    estimands = c("Mean", "UQR p10", "UQR p50", "UQR p90"),
    colour_by = "estimand",
    title     = "Specification curve: temperature (continuous)",
    filename  = "spec_curve_t.png"
  ),
  list(
    term = "t:urban", term_regex = NULL,
    estimands = c("Mean", "UQR p10", "UQR p50", "UQR p90"),
    colour_by = "estimand",
    title     = "Specification curve: temperature × urban (continuous)",
    filename  = "spec_curve_t_urban.png"
  ),
  list(
    term = "t:electricity", term_regex = NULL,
    estimands = c("Mean", "UQR p10", "UQR p50", "UQR p90"),
    colour_by = "estimand",
    title     = "Specification curve: temperature × electricity (continuous)",
    filename  = "spec_curve_t_electricity.png"
  ),
  # --- Binned -----------------------------------------------------------------
  list(
    term = NULL, term_regex = "^t\\([^:]+\\]$",
    estimands = "Mean",
    colour_by = "wx_bin",
    title     = "Specification curve: temperature bins",
    filename  = "spec_curve_t_bins.png"
  ),
  list(
    term = NULL, term_regex = "^t\\(.*\\]:urban$",
    estimands = "Mean",
    colour_by = "wx_bin",
    title     = "Specification curve: temperature bins × urban",
    filename  = "spec_curve_t_bins_urban.png"
  ),
  list(
    term = NULL, term_regex = "^t\\(.*\\]:electricity$",
    estimands = "Mean",
    colour_by = "wx_bin",
    title     = "Specification curve: temperature bins × electricity",
    filename  = "spec_curve_t_bins_electricity.png"
  )
)

for (cfg in spec_configs) {
  message("Generating: ", cfg$filename)
  p <- plot_spec_curve(
    data         = coefs,
    term         = cfg$term,
    term_regex   = cfg$term_regex,
    estimands    = cfg$estimands,
    model_filter = NULL,
    spec_vars    = spec_vars_all,
    rank_by      = cfg$estimands[[1]],
    colour_by    = cfg$colour_by,
    title        = cfg$title,
    subtitle     = "Outcome: Log welfare",
    y_label      = "Coefficient estimate (log welfare)"
  )
  ggplot2::ggsave(
    file.path(OUT_DIR, cfg$filename),
    plot = p, width = 14, height = 10, dpi = 300
  )
}

# =============================================================================
# HEATPLOTS
# =============================================================================
