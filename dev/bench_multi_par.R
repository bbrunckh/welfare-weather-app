#!/usr/bin/env Rscript
# Corrected apples-to-apples: the app's W3-A multi path (shared prep,
# per-method resolver) vs the new kernel suite path, same prepared state.
suppressPackageStartupMessages({library(duckdb); options(golem.app.prod = FALSE)})
pkgload::load_all(quiet = TRUE)

data_dir <- "/Users/bbrunckhorst/Library/CloudStorage/OneDrive-WBG/wiseapp - Documents/microdata/hh"

make_pipe <- function(file, n_years, k, seed = 42L) {
  con <- DBI::dbConnect(duckdb::duckdb(), read_only = TRUE, shared_home = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  d <- DBI::dbGetQuery(con, sprintf(
    "SELECT hhid, weight, welfare FROM read_parquet('%s/%s') WHERE welfare IS NOT NULL AND weight IS NOT NULL AND weight > 0",
    data_dir, file))
  n <- nrow(d); nt <- n * n_years; set.seed(seed)
  list(sim_year = rep(seq_len(n_years), each = n),
       y_point = log(rep(d$welfare, length.out = nt)),
       weight = rep(d$weight, length.out = nt),
       F_loading = matrix(rnorm(nt * k, sd = 0.01), nrow = nt),
       id_vec = rep(seq_len(n), length.out = nt), id_col = "hhid",
       train_aug = data.frame(hhid = seq_len(n), .resid = rnorm(n, 0, 0.15)))
}

methods <- c("mean", "median", "total", "headcount_ratio", "gap", "fgt2",
             "gini", "prosperity_gap", "avg_poverty")
pov_lines <- setNames(rep(3, length(methods)), methods)

old_multi <- function(pipe, cache) {
  prep <- .aggregation_prepare_pipeline(pipe, pipe$train_aug, pipe$id_col,
    "original", 42, TRUE, TRUE, cache)
  out <- setNames(vector("list", length(methods)), methods)
  for (i in seq_along(prep$years)) {
    for (m in methods) {
      out[[m]][[i]] <- aggregate_with_uncertainty_delta(
        pipe$y_point[prep$rows[[i]][prep$valid[[i]]]],
        prep$factor_blocks[[i]], m, prep$weights[[i]], 3,
        "original", pipe$train_aug, NULL, pipe$id_col, TRUE,
        seed = 42, prepared_mu = prep$mu[[i]])
      out[[m]][[i]]$sim_year <- prep$years[[i]]
    }
  }
  out
}

new_multi <- function(pipe, cache) {
  aggregate_pipeline_per_year_multi(pipe, methods, weighted = TRUE,
    pov_lines = pov_lines, residuals = "original", is_log = TRUE,
    skip_coef = FALSE, seed = 42, preparation_cache = cache)
}

for (spec in list(
    list(f = "BFA/BFA_2021_EHCVM_GMD_hh.parquet", y = 30L, k = 25L, lbl = "BFA 7.2k x 30y"),
    list(f = "IRN/IRN_2020_HEIS_GMD_hh.parquet", y = 53L, k = 25L, lbl = "IRN 37.6k x 53y"))) {
  pipe <- make_pipe(spec$f, spec$y, spec$k)
  c1 <- .new_aggregation_preparation_cache()
  c2 <- .new_aggregation_preparation_cache()
  o <- old_multi(pipe, c1)
  nw <- new_multi(pipe, c2)
  worst <- 0
  for (m in methods) {
    for (i in seq_along(o[[m]])) {
      worst <- max(worst, abs(o[[m]][[i]]$value - nw[[m]][[i]]$value) /
        max(abs(o[[m]][[i]]$value), 1e-12))
    }
  }
  cat(sprintf("\n=== %s === parity worst rel diff: %.2g\n", spec$lbl, worst))
  print(bench::mark(old_multi_resolver = old_multi(pipe, c1),
    new_kernel_suite = new_multi(pipe, c2), check = FALSE,
    iterations = 3, min_time = 0.3)[, c("expression", "median", "mem_alloc")])
}
