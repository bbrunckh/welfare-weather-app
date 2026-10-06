#!/usr/bin/env Rscript

# Profiling + parity harness for the welfare aggregation hot path
# (`aggregate_pipeline_per_year_multi` -> `aggregate_with_uncertainty_delta`).
# Uses real BFA/IRN welfare + weight columns as payloads, shaped like the app's
# simulation pipelines (per-year mu vectors over the baseline wave).

suppressPackageStartupMessages({
  library(duckdb)
  options(golem.app.prod = FALSE)
})
pkgload::load_all(".", quiet = TRUE)

if (!nzchar(Sys.getenv("WISEAPP_DATA_PATH"))) {
  stop("Set WISEAPP_DATA_PATH to the local data directory.", call. = FALSE)
}
data_dir <- file.path(Sys.getenv("WISEAPP_DATA_PATH"), "microdata", "hh")

make_pipe_fixture <- function(country_file, n_years, k_coef, seed = 42L,
                              years = NULL) {
  con <- DBI::dbConnect(duckdb::duckdb(), read_only = TRUE, shared_home = FALSE)
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- file.path(data_dir, country_file)
  q <- sprintf(
    "SELECT hhid, weight, welfare FROM read_parquet('%s') WHERE welfare IS NOT NULL AND weight IS NOT NULL AND weight > 0",
    path
  )
  d <- DBI::dbGetQuery(con, q)
  set.seed(seed)
  n <- nrow(d)
  years_vec <- if (is.null(years)) rep(seq_len(n_years), each = n) else years
  n_total <- length(years_vec)
  list(
    sim_year = years_vec,
    y_point = log(rep(d$welfare, length.out = n_total)),
    weight = rep(d$weight, length.out = n_total),
    F_loading = matrix(0, nrow = 0, ncol = k_coef),
    id_vec = rep(d$hhid, length.out = n_total),
    id_col = "hhid",
    train_aug = data.frame(hhid = d$hhid, .resid = rnorm(n, 0, 0.15)),
    n_per_year = n,
    years = n_years,
    n_rows = n_total
  )
}

# Fixture F_loading must match row count; build it separately after the fact.
set_F_loading <- function(pipe, k_coef, seed = 7L) {
  set.seed(seed)
  n_total <- length(pipe$y_point)
  pipe$F_loading <- matrix(rnorm(n_total * k_coef, sd = 0.01), nrow = n_total)
  pipe
}

profile_case <- function(label, pipe, skip_coef = FALSE) {
  cat("\n===", label, "===\n")
  cat(sprintf("rows=%d per_year=%d years=%d K=%d\n",
              pipe$n_rows, pipe$n_per_year, pipe$years, ncol(pipe$F_loading)))
  methods <- c(
    "mean", "median", "total", "headcount_ratio", "gap", "fgt2",
    "gini", "prosperity_gap", "avg_poverty"
  )
  pov_lines <- setNames(rep(3, length(methods)), methods)

  # Warm the preparation cache so prep is not attributed to the profiled loop.
  cache <- .new_aggregation_preparation_cache()
  invisible(aggregate_pipeline_per_year_multi(
    pipe, methods, weighted = TRUE, pov_lines = pov_lines,
    residuals = "none", is_log = TRUE, skip_coef = skip_coef,
    preparation_cache = cache
  ))

  tmp <- tempfile(fileext = ".out")
  Rprof(tmp, line.profiling = FALSE, memory.profiling = TRUE)
  out <- aggregate_pipeline_per_year_multi(
    pipe, methods, weighted = TRUE, pov_lines = pov_lines,
    residuals = "none", is_log = TRUE, skip_coef = skip_coef,
    preparation_cache = cache
  )
  Rprof(NULL)
  prof <- summaryRprof(tmp)
  elapsed <- prof$walkingtime[["total.time"]]
  unlink(tmp)
  cat(sprintf("elapsed: %.3f s\n", elapsed))
  print(head(prof$by.total, 25))
  invisible(out)
}

args <- commandArgs(trailingOnly = TRUE)
case <- if (length(args)) args[[1]] else "bfa"

if (case == "bfa") {
  pipe <- set_F_loading(make_pipe_fixture("BFA/BFA_2021_EHCVM_GMD_hh.parquet", 30, 25L), 25L)
} else {
  pipe <- set_F_loading(make_pipe_fixture("IRN/IRN_2020_HEIS_GMD_hh.parquet", 53, 25L), 25L)
}

profile_case(paste0(toupper(case), " skip_coef=FALSE"), pipe, skip_coef = FALSE)
profile_case(paste0(toupper(case), " skip_coef=TRUE"), pipe, skip_coef = TRUE)
