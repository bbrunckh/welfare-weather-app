# =============================================================================
# dev/reprex_module2_debug.R
#
# Minimal reprex for Module 2 future simulation display bug.
#
# Tests the pure functions (no Shiny session) in this order:
#   1. aggregate_sim_preds()   -- does aggregation work for hist + future?
#   2. selected_scenario_names() regex -- does the year/pct filter match keys?
#   3. agg_scenarios() req() timing -- does NULL input$cmp_agg_method abort?
#   4. all_series construction -- are future entries present?
#   5. plot_pointrange_climate() -- do future series appear on the chart?
#   6. build_threshold_table_df() -- are future rows in the table?
#   7. plot_threshold_points() -- are future series on the threshold chart?
#
# Run with: source("dev/reprex_module2_debug.R")
# =============================================================================

pkgload::load_all()   # expose all exported + internal package functions

cat("\n========== Module 2 future simulation display reprex ==========\n\n")

# ---------------------------------------------------------------------------
# Shared helpers
# ---------------------------------------------------------------------------

.check <- function(label, expr) {
  result <- tryCatch(expr, error = function(e) {
    cat("  [FAIL]", label, "\n        ERROR:", conditionMessage(e), "\n")
    invisible(NULL)
  })
  if (!is.null(result)) cat("  [PASS]", label, "\n")
  invisible(result)
}

# ---------------------------------------------------------------------------
# Synthetic data: mimics what the simulation pipeline produces
# ---------------------------------------------------------------------------

# so -- minimal outcome metadata data frame (continuous, no log transform)
so_cont <- data.frame(
  name      = "welfare",
  label     = "Household welfare",
  type      = "numeric",
  transform = NA_character_,
  stringsAsFactors = FALSE
)

make_preds <- function(n_years = 30, n_obs = 500, seed = 42, shift = 0) {
  set.seed(seed)
  data.frame(
    sim_year = rep(seq_len(n_years), each = n_obs),
    welfare  = exp(rnorm(n_years * n_obs, mean = log(3.5) + shift, sd = 0.6))
  )
}

# Historical: 30 years x 500 obs
hist_preds <- make_preds(n_years = 30, seed = 1)

# Future: 3 SSP x 2 years x 3 percentiles = 18 saved scenarios
# Keys use the format produced by mod_2_04_future_sim:
#   "{SSP} / {anchor_year} / P{percentile}"
future_scenarios <- list(
  "SSP2-4.5 / 2030 / P10" = list(preds = make_preds(shift = -0.05, seed = 10), so = so_cont),
  "SSP2-4.5 / 2030 / P50" = list(preds = make_preds(shift =  0.00, seed = 11), so = so_cont),
  "SSP2-4.5 / 2030 / P90" = list(preds = make_preds(shift =  0.05, seed = 12), so = so_cont),
  "SSP2-4.5 / 2050 / P10" = list(preds = make_preds(shift = -0.10, seed = 13), so = so_cont),
  "SSP2-4.5 / 2050 / P50" = list(preds = make_preds(shift =  0.00, seed = 14), so = so_cont),
  "SSP2-4.5 / 2050 / P90" = list(preds = make_preds(shift =  0.10, seed = 15), so = so_cont),
  "SSP3-7.0 / 2030 / P10" = list(preds = make_preds(shift = -0.08, seed = 20), so = so_cont),
  "SSP3-7.0 / 2030 / P50" = list(preds = make_preds(shift =  0.02, seed = 21), so = so_cont),
  "SSP3-7.0 / 2030 / P90" = list(preds = make_preds(shift =  0.12, seed = 22), so = so_cont),
  "SSP3-7.0 / 2050 / P10" = list(preds = make_preds(shift = -0.15, seed = 23), so = so_cont),
  "SSP3-7.0 / 2050 / P50" = list(preds = make_preds(shift =  0.05, seed = 24), so = so_cont),
  "SSP3-7.0 / 2050 / P90" = list(preds = make_preds(shift =  0.20, seed = 25), so = so_cont),
  "SSP5-8.5 / 2030 / P10" = list(preds = make_preds(shift = -0.10, seed = 30), so = so_cont),
  "SSP5-8.5 / 2030 / P50" = list(preds = make_preds(shift =  0.05, seed = 31), so = so_cont),
  "SSP5-8.5 / 2030 / P90" = list(preds = make_preds(shift =  0.15, seed = 32), so = so_cont),
  "SSP5-8.5 / 2050 / P10" = list(preds = make_preds(shift = -0.20, seed = 33), so = so_cont),
  "SSP5-8.5 / 2050 / P50" = list(preds = make_preds(shift =  0.10, seed = 34), so = so_cont),
  "SSP5-8.5 / 2050 / P90" = list(preds = make_preds(shift =  0.30, seed = 35), so = so_cont)
)

cat("--- Scenario keys in saved_scenarios ---\n")
cat(paste0("  ", names(future_scenarios)), sep = "\n")
cat("\n")

# ---------------------------------------------------------------------------
# 1. aggregate_sim_preds() -- basic smoke test
# ---------------------------------------------------------------------------
cat("=== 1. aggregate_sim_preds() ===\n")

agg_hist <- .check(
  "aggregate_sim_preds() works for historical preds",
  aggregate_sim_preds(hist_preds, so_cont, "mean", "none", FALSE, NULL)
)
cat("    hist out rows:", nrow(agg_hist$out), " | x_label:", agg_hist$x_label, "\n")

agg_ssp2_2050_p50 <- .check(
  "aggregate_sim_preds() works for 'SSP2-4.5 / 2050 / P50'",
  aggregate_sim_preds(
    future_scenarios[["SSP2-4.5 / 2050 / P50"]]$preds,
    so_cont, "mean", "none", FALSE, NULL
  )
)
cat("    future out rows:", nrow(agg_ssp2_2050_p50$out), "\n\n")

# ---------------------------------------------------------------------------
# 2. selected_scenario_names() regex -- the year-filter match
# ---------------------------------------------------------------------------
cat("=== 2. Year / percentile regex filter ===\n")

sc_names  <- names(future_scenarios)

# Use the package's internal helpers directly
all_ssps  <- sort(unique(na.omit(vapply(sc_names, wiseapp:::.normalise_ssp,   character(1)))))
all_years <- sort(unique(na.omit(vapply(sc_names, wiseapp:::.parse_year,       character(1)))))
all_pcts  <- sort(unique(na.omit(vapply(sc_names, wiseapp:::.parse_percentile, character(1)))))

cat("  Detected SSPs: ", paste(all_ssps,  collapse = ", "), "\n")
cat("  Detected years:", paste(all_years, collapse = ", "), "\n")
cat("  Detected pcts: ", paste(all_pcts,  collapse = ", "), "\n\n")

# Replicate selected_scenario_names() logic from mod_2_06_sim_compare_server()
filter_names <- function(sc_nms, ssps = all_ssps, yrs = all_years, pcts = all_pcts) {
  keep <- vapply(sc_nms, function(nm) {
    is_ssp    <- grepl("^SSP", nm)
    if (!is_ssp) return(TRUE)
    ssp_match <- any(vapply(ssps, function(s) startsWith(nm, s), logical(1)))
    yr_match  <- length(yrs) == 0 ||
      any(vapply(yrs, function(y) grepl(paste0("/ ", y, "[ /]"), nm), logical(1)))
    pct_match <- length(pcts) == 0 ||
      any(vapply(pcts, function(p) grepl(paste0("/ ", p, "$"), nm), logical(1)))
    ssp_match && yr_match && pct_match
  }, logical(1))
  sc_nms[keep]
}

all_selected <- filter_names(sc_names)
cat("  filter_names() with all filters -> keeps", length(all_selected), "of", length(sc_names), "scenarios\n")
if (length(all_selected) != length(sc_names)) {
  cat("  [WARN] Some scenarios dropped by filter! Missing:\n")
  cat(paste0("    ", setdiff(sc_names, all_selected)), sep = "\n")
} else {
  cat("  [PASS] All scenarios pass the full filter.\n")
}

# Test year filter specifically: filter to 2050 only
selected_2050 <- filter_names(sc_names, yrs = "2050")
cat("\n  filter_names(yrs='2050') -> expects 6, got:", length(selected_2050), "\n")
if (length(selected_2050) != 6) {
  cat("  [WARN] Year filter is broken. Matched:\n")
  cat(paste0("    ", selected_2050), sep = "\n")
} else {
  cat("  [PASS] Year filter returns correct 6 scenarios.\n")
}

# Test pct filter: P50 only
selected_p50 <- filter_names(sc_names, pcts = "P50")
cat("\n  filter_names(pcts='P50') -> expects 6, got:", length(selected_p50), "\n")
if (length(selected_p50) != 6) {
  cat("  [WARN] Pct filter is broken. Matched:\n")
  cat(paste0("    ", selected_p50), sep = "\n")
} else {
  cat("  [PASS] Pct filter returns correct 6 scenarios.\n")
}
cat("\n")

# ---------------------------------------------------------------------------
# 3. agg_scenarios() req() timing -- NULL input simulation
# ---------------------------------------------------------------------------
cat("=== 3. req() timing with NULL input$cmp_agg_method ===\n")

# Simulate what happens when insertUI has fired but inputs haven't registered:
# input$cmp_agg_method = NULL, input$cmp_deviation = NULL
simulate_agg_scenarios_with_req <- function(method, deviation) {
  tryCatch({
    shiny::req(method, deviation)   # aborts silently on NULL in a reactive
    "proceeded"
  }, error = function(e) {
    paste0("ABORTED (req silenced): ", class(e)[1])
  })
}

cat("  method=NULL, deviation=NULL ->",
    simulate_agg_scenarios_with_req(NULL, NULL), "\n")
cat("  method='mean', deviation=NULL ->",
    simulate_agg_scenarios_with_req("mean", NULL), "\n")
cat("  method='mean', deviation='none' ->",
    simulate_agg_scenarios_with_req("mean", "none"), "\n")

# Proposed fix: %||% defaults bypass the NULL abort
simulate_agg_scenarios_with_fallback <- function(method, deviation) {
  method    <- method    %||% "mean"
  deviation <- deviation %||% "none"
  paste0("proceeded with method=", method, ", deviation=", deviation)
}

cat("\n  With %||% fallback:\n")
cat("  method=NULL, deviation=NULL ->",
    simulate_agg_scenarios_with_fallback(NULL, NULL), "\n")
cat("  method='mean', deviation=NULL ->",
    simulate_agg_scenarios_with_fallback("mean", NULL), "\n\n")

# ---------------------------------------------------------------------------
# 4. all_series construction -- are future entries included?
# ---------------------------------------------------------------------------
cat("=== 4. all_series construction ===\n")

agg_scenarios_list <- lapply(future_scenarios, function(s)
  aggregate_sim_preds(s$preds, s$so, "mean", "none", FALSE, NULL)
)

selected_nms <- filter_names(names(future_scenarios))
all_series <- c(
  list(Historical = agg_hist),
  agg_scenarios_list[intersect(selected_nms, names(agg_scenarios_list))]
)

cat("  all_series length:", length(all_series), "(expect 19: 1 hist + 18 future)\n")
if (length(all_series) == 19) {
  cat("  [PASS] all_series contains all scenarios.\n")
} else {
  cat("  [WARN] all_series is missing entries. Names present:\n")
  cat(paste0("    ", names(all_series)), sep = "\n")
}
cat("\n")

# ---------------------------------------------------------------------------
# 5. plot_pointrange_climate()
# ---------------------------------------------------------------------------
cat("=== 5. plot_pointrange_climate() ===\n")

p_pointrange <- .check(
  "plot_pointrange_climate() runs without error",
  plot_pointrange_climate(
    scenarios = agg_scenarios_list[selected_nms],
    hist_agg  = agg_hist
  )
)

if (!is.null(p_pointrange)) {
  built      <- ggplot2::ggplot_build(p_pointrange)
  n_pts      <- sum(vapply(built$data, function(d) if ("x" %in% names(d)) nrow(d) else 0L, integer(1)))
  cat("    Total point/range rows across all layers:", n_pts, "\n")
  if (n_pts > 5) {
    cat("    [PASS] Multiple series present on the pointrange chart.\n")
  } else {
    cat("    [WARN] Only", n_pts, "row(s) -- future results likely missing.\n")
  }
}
cat("\n")

# ---------------------------------------------------------------------------
# 6. build_threshold_table_df()
# ---------------------------------------------------------------------------
cat("=== 6. build_threshold_table_df() ===\n")

thresh_df <- .check(
  "build_threshold_table_df() runs without error",
  build_threshold_table_df(all_series)
)

if (!is.null(thresh_df)) {
  cat("    Rows:", nrow(thresh_df), "(expect 19)\n")
  if (any(grepl("^SSP", thresh_df$Scenario))) {
    cat("    [PASS] Future SSP rows present in threshold table.\n")
  } else {
    cat("    [WARN] No SSP rows in threshold table -- future results missing.\n")
    cat("    Scenario column:\n")
    cat(paste0("      ", thresh_df$Scenario), sep = "\n")
  }
}
cat("\n")

# ---------------------------------------------------------------------------
# 7. plot_threshold_points()
# ---------------------------------------------------------------------------
cat("=== 7. plot_threshold_points() ===\n")

p_thresh <- .check(
  "plot_threshold_points() runs without error",
  plot_threshold_points(all_series)
)

if (!is.null(p_thresh)) {
  built2             <- ggplot2::ggplot_build(p_thresh)
  colours_on_plot    <- unique(unlist(lapply(built2$data, function(d) {
    if ("colour" %in% names(d)) d$colour else character(0)
  })))
  cat("    Distinct colours on threshold plot:", length(colours_on_plot), "\n")
  if (length(colours_on_plot) > 1) {
    cat("    [PASS] Multiple scenario series on threshold chart.\n")
  } else {
    cat("    [WARN] Only one colour -- future results missing from threshold chart.\n")
  }
}
cat("\n")

cat("========== Reprex complete ==========\n")
