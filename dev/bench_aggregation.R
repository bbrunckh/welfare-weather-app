#!/usr/bin/env Rscript
# Focused benchmark: multi-method aggregation suite, old per-method resolver
# path vs single-pass welfare kernel path (the new tables_multi behaviour).
#
# Payloads mirror the app's simulation pipelines: per-year mu vectors sized
# from real BFA (small) and IRN (large) survey welfare/weight columns.
#
# Usage: Rscript dev/bench_aggregation.R [bfa|irn|both]

suppressPackageStartupMessages({
  library(duckdb)
  options(golem.app.prod = FALSE)
})
pkgload::load_all(quiet = TRUE)

if (!nzchar(Sys.getenv("WISEAPP_DATA_PATH"))) {
  stop("Set WISEAPP_DATA_PATH to the local data directory.", call. = FALSE)
}
data_dir <- Sys.getenv("WISEAPP_DATA_PATH")

payloads <- list(
  bfa = list(file = "BFA/BFA_2021_EHCVM_GMD_hh.parquet", n_years = 30L,
             k_coef = 25L, label = "BFA 7.2k x 30y"),
  irn = list(file = "IRN/IRN_2020_HEIS_GMD_hh.parquet", n_years = 53L,
             k_coef = 25L, label = "IRN 37.6k x 53y")
)

make_pipe <- function(spec, seed = 42L) {
  con <- DBI::dbConnect(duckdb::duckdb(), read_only = TRUE, shared_home = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  d <- DBI::dbGetQuery(con, sprintf(
    "SELECT hhid, weight, welfare FROM read_parquet('%s') WHERE welfare IS NOT NULL AND weight IS NOT NULL AND weight > 0",
    file.path(data_dir, "microdata/hh", spec$file)))
  n <- nrow(d)
  n_total <- n * spec$n_years
  set.seed(seed)
  list(
    sim_year = rep(seq_len(spec$n_years), each = n),
    y_point = log(rep(d$welfare, length.out = n_total)),
    weight = rep(d$weight, length.out = n_total),
    F_loading = matrix(rnorm(n_total * spec$k_coef, sd = 0.01), nrow = n_total),
    id_vec = rep(seq_len(n), length.out = n_total),
    id_col = "hhid",
    train_aug = data.frame(hhid = seq_len(n), .resid = rnorm(n, 0, 0.15))
  )
}

methods <- c(
  "mean", "median", "total", "headcount_ratio", "gap", "fgt2",
  "gini", "prosperity_gap", "avg_poverty"
)
pov_lines <- setNames(rep(3, length(methods)), methods)

# Old path shape: per-method per-year delta calls sharing preparation only
# (what tables_multi did before the kernel; replicate by calling the oracle
# pipeline aggregator once per method).
old_path <- function(pipe) {
  lapply(methods, function(m) {
    aggregate_pipeline_per_year(
      pipe, m, weighted = TRUE, pov_line = 3, residuals = "original",
      is_log = TRUE, skip_coef = FALSE, seed = 42
    )
  })
}

new_path <- function(pipe) {
  aggregate_pipeline_tables_multi(
    pipelines = pipe, methods = methods, weighted = TRUE,
    pov_lines = pov_lines, residuals = "original", is_log = TRUE,
    skip_coef = FALSE, seed = 42
  )
}

# Parity gate before timing: kernel path must match the oracle path to
# determinism tolerance. tables_multi returns per-method tibbles (one row per
# year, models pooled); the oracle path returns per-year result lists.
check_parity <- function(old, new) {
  worst <- 0
  for (j in seq_along(methods)) {
    tbl <- new[[methods[[j]]]]
    vals_new <- tbl$value
    vals_old <- vapply(old[[j]], `[[`, numeric(1L), "value")
    worst <- max(worst, max(abs(vals_new - vals_old) /
      pmax(abs(vals_old), 1e-12)))
    sds_new <- vapply(tbl$value_all_sd, `[[`, numeric(1L), 1L)
    sds_old <- vapply(old[[j]], function(x) {
      sqrt(max((x$var_coef %||% 0) + (x$var_resid %||% 0), 0))
    }, numeric(1L))
    worst <- max(worst, max(abs(sds_new - sds_old) / pmax(sds_old, 1e-12)))
  }
  worst
}

run_case <- function(key, spec) {
  cat("\n===", spec$label, "===\n")
  pipe <- make_pipe(spec)
  cache <- .new_aggregation_preparation_cache()
  # Warm prep once so both paths pay only their own compute.
  invisible(aggregate_pipeline_per_year_multi(
    pipe, methods[1], weighted = TRUE, pov_line = 3, residuals = "original",
    is_log = TRUE, skip_coef = FALSE, seed = 42, preparation_cache = cache))
  old <- old_path(pipe) # builds its own cache
  new <- new_path(pipe)
  worst <- check_parity(old, new)
  cat(sprintf("parity worst rel diff: %.3g\n", worst))
  stopifnot(worst < 1e-12)
  bench::mark(
    old_per_method = old_path(pipe),
    new_kernel_suite = new_path(pipe),
    check = FALSE, iterations = 5, min_time = 0.5
  )
}

args <- commandArgs(trailingOnly = TRUE)
which_payloads <- if (length(args)) args[[1]] else "both"
keys <- if (which_payloads == "both") names(payloads) else which_payloads
results <- lapply(keys, function(k) {
  out <- run_case(k, payloads[[k]])
  print(out[, c("expression", "median", "mem_alloc", "n_itr")])
  out
})
names(results) <- keys
saveRDS(results, file.path(tempdir(), "bench_aggregation.rds"))
