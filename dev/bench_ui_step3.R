# Development-only Step 3 UI-migration benchmark (Batch 2).
#
# Measures the R-side builder cost old (ggplot/DT) vs new (echarts4r/reactable)
# for representative migrated surfaces:
#   - exceedance curves          (fct_sim_compare echart_exceedance vs
#                                 enhance_exceedance)
#   - decomposition decile bars  (echart_decomposition_channels_by_decile vs
#                                 plot_decomposition_channels_by_decile)
#   - adverse return-period dots (echart_step3_adverse_dot vs
#                                 plot_step3_adverse_dot)
#   - threshold table widget     (.wise_threshold_reactable)
# Payloads are synthetic frames shaped like the module reactives (draws per
# model/year for exceedance, decile channel summaries, return-period rows).
# Medians of 10 iterations via bench::mark (memory = FALSE: htmlwidget
# assembly trips bench's memory profiler on this platform, as in
# dev/bench_surveystats.R).
#
# Usage:
#   Rscript dev/bench_ui_step3.R

options(golem.app.prod = FALSE)
devtools::load_all(quiet = TRUE)

set.seed(11)

fmt_bench <- function(bm) {
  bm$expression <- vapply(bm$expression, function(e) deparse(e)[1], character(1))
  bm[, c("expression", "min", "median", "itr/sec", "mem_alloc", "n_itr")]
}

# ---------------------------------------------------------------------------
# Payload 1: exceedance curves (build_exceedance_rows-shaped; 2 scenarios x
# baseline/policy x 30 ranks x 5 climate models)
# ---------------------------------------------------------------------------

n_models <- 5L
n_ranks <- 30L
scen <- c("Historical", "SSP2-4.5 / 2030-2040", "SSP5-8.5 / 2040-2050")
curves <- do.call(rbind, lapply(scen, function(s) {
  do.call(rbind, lapply(c("Baseline", "Policy"), function(src) {
    if (s == "Historical" && src == "Policy") {
      return(NULL)
    }
    is_hist <- s == "Historical"
    do.call(rbind, lapply(seq_len(n_ranks), function(r) {
      data.frame(
        scenario = s,
        source = src,
        model_id = paste0("m", seq_len(n_models)),
        rank = r,
        exceed_prob = 1 - r / (n_ranks + 1),
        welfare_val = 10 + r * 0.15 + rnorm(n_models, 0, 0.4) +
          (if (src == "Policy") 1.2 else 0),
        coef_sd = runif(n_models, 0.02, 0.08),
        is_historical = is_hist,
        stringsAsFactors = FALSE
      )
    }))
  }))
}))
cat(sprintf("exceedance payload: %d rows (%d scenarios x 2 sources x %d ranks x %d models)\n",
            nrow(curves), length(scen), n_ranks, n_models))

bm_exceed <- bench::mark(
  ggplot   = enhance_exceedance(
    curves_tbl = curves, x_label = "Outcome", return_period = TRUE,
    n_sim_years = 30, logit_x = TRUE, band_q = NULL,
    ensemble_band_q = c(lo = 0, hi = 1)
  ),
  echarts  = echart_exceedance(
    curves_tbl = curves, x_label = "Outcome",
    n_sim_years = 30, logit_x = TRUE, band_q = NULL,
    ensemble_band_q = c(lo = 0, hi = 1)
  ),
  min_time = 1, iterations = 10, check = FALSE, memory = FALSE
)
cat("\n== exceedance chart builder (R-side) ==\n")
print(fmt_bench(bm_exceed))

# ---------------------------------------------------------------------------
# Payload 2: decomposition channels by decile (10 deciles, RIF channels)
# ---------------------------------------------------------------------------

decile_tbl <- do.call(rbind, lapply(1:10, function(d) {
  data.frame(
    decile = d,
    cash_transfer_percent = rnorm(1, 1.5, 0.3),
    covariate_shift_percent = rnorm(1, 2.5, 0.4),
    repositioning_percent = rnorm(1, 0.4, 0.1),
    interaction_percent = rnorm(1, 0.2, 0.1),
    total_percent = rnorm(1, 4.6, 0.5),
    stringsAsFactors = FALSE
  )
}))
bm_decile <- bench::mark(
  ggplot  = plot_decomposition_channels_by_decile(decile_tbl, is_rif = TRUE),
  echarts = echart_decomposition_channels_by_decile(decile_tbl, is_rif = TRUE),
  min_time = 1, iterations = 10, check = FALSE, memory = FALSE
)
cat("\n== decomposition decile bar builder (R-side) ==\n")
print(fmt_bench(bm_decile))

# ---------------------------------------------------------------------------
# Payload 3: adverse return-period dots (2 scenarios x 2 sources x 4 RPs)
# ---------------------------------------------------------------------------

dot_tbl <- do.call(rbind, lapply(c("Historical", "SSP2-4.5 / 2030-2040"), function(s) {
  do.call(rbind, lapply(names(RP_LOW), function(rp) {
    data.frame(
      scenario = s,
      rp_name = rp,
      rp_label = factor(paste0("1-in-", sub(".*:", "", rp)),
        levels = paste0("1-in-", sub(".*:", "", RP_LOW))),
      baseline_val = 10 + runif(1),
      policy_val = 12 + runif(1),
      base_lo = 9.7, base_hi = 10.3,
      policy_lo = 11.7, policy_hi = 12.3,
      is_historical = s == "Historical",
      stringsAsFactors = FALSE
    )
  }))
}))
bm_dot <- bench::mark(
  ggplot  = plot_step3_adverse_dot(dot_tbl, x_label = "Outcome"),
  echarts = echart_step3_adverse_dot(dot_tbl, x_label = "Outcome"),
  min_time = 1, iterations = 10, check = FALSE, memory = FALSE
)
cat("\n== adverse dot builder (R-side) ==\n")
print(fmt_bench(bm_dot))

# ---------------------------------------------------------------------------
# Payload 4: threshold table widget (200 rows x 8 RP columns)
# ---------------------------------------------------------------------------

rp_cols <- names(RP_LOW)
thresh <- do.call(rbind, lapply(seq_len(20), function(i) {
  data.frame(
    Scenario = scen[[1 + i %% 3]],
    Source = if (i %% 2) "Baseline" else "Policy",
    Estimate = c("Central (P50)", "Ensemble 10%", "Ensemble 90%",
      "Coef 10%", "Coef 90%", "Pooled 10%", "Pooled 90%",
      "Min (P0)", "Max (P100)", "Central (P50)")[1 + i %% 10],
    Obs = 30L,
    vapply(rp_cols, function(rp) round(runif(1, 5, 20), 2), numeric(1)),
    stringsAsFactors = FALSE, check.names = FALSE
  )
}))
bm_table <- bench::mark(
  reactable = .wise_threshold_reactable(thresh),
  min_time = 1, iterations = 10, check = FALSE, memory = FALSE
)
cat(sprintf("\n== threshold table widget (%d rows x %d cols) ==\n",
            nrow(thresh), ncol(thresh)))
print(fmt_bench(bm_table))

cat("\nDone.\n")
