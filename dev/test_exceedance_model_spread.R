# =============================================================================
# Test: Exceedance plot model spread band
# =============================================================================

devtools::load_all()
library(ggplot2)

set.seed(42)
N_YEARS  <- 30
N_MODELS <- 10

make_scenario_out <- function(n_models, mean_shift = 0, sd_between = 5) {
  per_model_vals <- lapply(seq_len(n_models), function(m) {
    model_mean <- mean_shift + rnorm(1, 0, sd_between)
    rnorm(N_YEARS, mean = model_mean, sd = 10)
  })
  per_year_vals <- lapply(seq_len(N_YEARS), function(yr) {
    vapply(per_model_vals, `[`, numeric(1), yr)
  })
  tibble::tibble(
    sim_year     = seq_len(N_YEARS),
    value        = vapply(per_year_vals, median, numeric(1)),
    value_p05    = NA_real_,
    value_p95    = NA_real_,
    model_values = per_year_vals
  )
}

hist_agg <- list(
  out     = make_scenario_out(n_models = 1, mean_shift = 0, sd_between = 0),
  x_label = "Simulated welfare (test)"
)

future_multi  <- make_scenario_out(n_models = N_MODELS, mean_shift = -15, sd_between = 8)
future_single <- make_scenario_out(n_models = 1,        mean_shift = -15, sd_between = 0)

scenarios_multi  <- list("Historical"       = hist_agg,
                         "ssp370_2041_2070" = list(out = future_multi,  x_label = "test"))
scenarios_single <- list("Historical"       = hist_agg,
                         "ssp370_2041_2070" = list(out = future_single, x_label = "test"))

# Helper: exceedance ribbons use xmin/xmax (horizontal spread), not ymin/ymax
ribbon_spread <- function(built) {
  Filter(function(d) all(c("xmin", "xmax") %in% names(d)), built$data)
}

# =============================================================================
# Test 1: multi-model → ribbon present with non-zero width
# =============================================================================
p_multi <- enhance_exceedance(
  scenarios     = scenarios_multi,
  hist_agg      = hist_agg,
  x_label       = "Simulated welfare (test)",
  return_period = FALSE,
  show_bands    = TRUE
)

built_m       <- ggplot_build(p_multi)
ribbon_data_m <- ribbon_spread(built_m)
spread_widths <- vapply(ribbon_data_m, function(d) max(d$xmax - d$xmin, na.rm = TRUE), numeric(1))

cat("Test 1 — ribbon layers found      :", length(ribbon_data_m), "\n")
cat("Test 1 — max spread widths        :", round(spread_widths, 3), "\n")
stopifnot("No ribbon layers in multi-model plot" = length(ribbon_data_m) >= 1)
stopifnot("Model spread ribbon has zero width"   = any(spread_widths > 0.01))
cat("PASS: multi-model spread ribbon has non-zero width\n\n")

# =============================================================================
# Test 2: single model → no spread ribbon (or zero width)
# =============================================================================
p_single <- enhance_exceedance(
  scenarios     = scenarios_single,
  hist_agg      = hist_agg,
  x_label       = "Simulated welfare (test)",
  return_period = FALSE,
  show_bands    = TRUE
)

built_s         <- ggplot_build(p_single)
ribbon_data_s   <- ribbon_spread(built_s)
spread_widths_s <- if (length(ribbon_data_s) > 0) {
  vapply(ribbon_data_s, function(d) max(d$xmax - d$xmin, na.rm = TRUE), numeric(1))
} else {
  numeric(0)
}

cat("Test 2 — single-model spread widths:", round(spread_widths_s, 3), "\n")
stopifnot("Single-model scenario should have zero-width spread" =
            length(spread_widths_s) == 0 || all(spread_widths_s < 0.01))
cat("PASS: single-model scenario has no model spread\n\n")

# =============================================================================
# Test 3: envelope ordering — model_lo <= model_hi everywhere
# =============================================================================
set.seed(99)
controlled_vals <- lapply(seq_len(N_YEARS), function(yr)
  c(rnorm(1, 80, 2), rnorm(1, 100, 2), rnorm(1, 120, 2))
)
controlled_out <- tibble::tibble(
  sim_year     = seq_len(N_YEARS),
  value        = vapply(controlled_vals, median, numeric(1)),
  value_p05    = NA_real_,
  value_p95    = NA_real_,
  model_values = controlled_vals
)
sc_ctrl <- list("Historical"       = hist_agg,
                "ssp370_2041_2070" = list(out = controlled_out, x_label = "test"))

p_ctrl   <- enhance_exceedance(sc_ctrl, hist_agg, "test",
                                return_period = FALSE, show_bands = TRUE)
built_c  <- ggplot_build(p_ctrl)
ribbon_c <- ribbon_spread(built_c)

if (length(ribbon_c) > 0) {
  widths   <- vapply(ribbon_c, function(d) max(d$xmax - d$xmin, na.rm = TRUE), numeric(1))
  spread_d <- ribbon_c[[which.max(widths)]]
  bad      <- sum(spread_d$xmin > spread_d$xmax + 1e-9, na.rm = TRUE)
  cat("Test 3 — rows where model_lo > model_hi:", bad, "(expect 0)\n")
  stopifnot("model_lo > model_hi in some rows" = bad == 0)
  cat("PASS: envelope well-ordered (model_lo <= model_hi)\n\n")
} else {
  stop("Test 3: no ribbon data found for controlled scenario")
}

# =============================================================================
# Visual — display plots side by side
# =============================================================================
gridExtra::grid.arrange(
  p_multi  + ggplot2::ggtitle("Multi-model (10 GCMs) — spread band expected"),
  p_single + ggplot2::ggtitle("Single model — no spread band"),
  ncol = 2
)

cat("=== All tests passed ===\n")
