# Development-only benchmark for the RIF computation kernels on real
# Section 9 payloads (BFA = smaller, IRN = larger).
#
# Benchmarks compute_rif_multi() against the pre-optimization reference
# implementation side by side, stage by stage, on the survey welfare payloads
# used by the app. Run from the repository root:
#
#   Rscript dev/bench_rif_kernels.R
#
# For external peak RSS, run:
#   /usr/bin/time -l Rscript dev/bench_rif_kernels.R
#
# Payload rows come from the local dataset folder (optimization guidelines
# Section 9). Override with WISEAPP_RIF_BENCH_DATA if needed.

pkgload::load_all(quiet = TRUE)

data_dir <- Sys.getenv(
  "WISEAPP_RIF_BENCH_DATA",
  "~/Library/CloudStorage/OneDrive-WBG/wiseapp - Documents/microdata/hh"
)
iterations <- as.integer(Sys.getenv("WISEAPP_RIF_BENCH_ITERATIONS", "20"))
taus <- seq(0.1, 0.9, by = 0.1)

# Legacy (pre-optimization) kernels kept verbatim for side-by-side parity
# and timing comparison. Do not use in production code.

legacy_compute_rif_multi <- function(y, taus, bw = NULL, dens = NULL) {
  na_mask <- !is.finite(y)
  y_obs <- y[!na_mask]
  q_taus <- stats::quantile(y_obs, probs = taus, names = FALSE, type = 7)
  if (is.null(dens)) {
    bw_use <- bw
    if (is.null(bw_use)) {
      bw_use <- tryCatch(stats::bw.SJ(y_obs), error = function(e) stats::bw.nrd0(y_obs))
    }
    dens <- stats::density(y_obs, bw = bw_use, n = 1024)
  }
  f_taus <- stats::approx(dens$x, dens$y, xout = q_taus)$y
  dens_max <- max(dens$y)
  lapply(seq_along(taus), function(i) {
    f_q <- f_taus[i]
    if (is.na(f_q) || f_q <= 0) {
      f_q <- dens_max * 0.01
      warning(sprintf("Density near zero at quantile %.2f; using floor.", taus[i]))
    }
    f_q <- max(f_q, dens_max * 0.001)
    rif <- rep(NA_real_, length(y))
    rif[!na_mask] <- q_taus[i] +
      (taus[i] - as.numeric(y_obs <= q_taus[i])) / f_q
    rif
  })
}

load_payload <- function(country) {
  dir <- file.path(data_dir, country)
  files <- file.path(dir, list.files(dir, pattern = "parquet$"))
  con <- DBI::dbConnect(duckdb::duckdb(), read_only = TRUE)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  d <- DBI::dbGetQuery(con, sprintf(
    "SELECT welfare FROM read_parquet([%s]) WHERE welfare IS NOT NULL",
    paste0("\"", files, "\"", collapse = ", ")
  ))
  d$welfare
}

for (country in c("BFA", "IRN")) {
  y <- load_payload(country)
  df <- data.frame(welfare = y)
  y_obs <- y[is.finite(y)]

  cat(sprintf("\n=== %s (n = %d) ===\n", country, length(y_obs)))

  # Parity gate: new kernel must match the legacy one on this payload
  # (bit-identical on the log model scale; last-ulp tolerance on raw scale).
  bw <- tryCatch(stats::bw.SJ(y_obs), error = function(e) stats::bw.nrd0(y_obs))
  dens <- stats::density(y_obs, bw = bw, n = 1024)
  ref <- suppressWarnings(legacy_compute_rif_multi(y, taus, dens = dens))
  new <- suppressWarnings(compute_rif_multi(y, taus, dens = dens))
  max_abs <- max(abs(unlist(ref) - unlist(new)))
  max_rel <- max(abs(unlist(ref) - unlist(new)) / pmax(abs(unlist(ref)), 1e-300))
  ref_log <- suppressWarnings(legacy_compute_rif_multi(log(y), taus, dens = dens))
  new_log <- suppressWarnings(compute_rif_multi(log(y), taus, dens = dens))
  cat(sprintf(
    "parity raw: identical=%s max_abs=%.3g max_rel=%.3g\n",
    identical(ref, new), max_abs, max_rel
  ))
  cat(sprintf(
    "parity log: identical=%s max_abs=%.3g\n",
    identical(ref_log, new_log), max(abs(unlist(ref_log) - unlist(new_log)))
  ))

  # bw.SJ parity gate for the rejected subsampling idea, for the record.
  stride <- max(1L, as.integer(length(y_obs) %/% 100000L))
  bw_sub <- stats::bw.SJ(y_obs[seq(1L, length(y_obs), by = stride)])
  cat(sprintf(
    "bw.SJ stride-subsample n=%d: %.6f vs full %.6f (rel %.2f%%)\n",
    length(y_obs[seq(1L, length(y_obs), by = stride)]), bw_sub, bw,
    100 * abs(bw_sub - bw) / bw
  ))

  legacy_quantile <- function(x, probs, type) {
    stats::quantile(x, probs = probs, names = FALSE, type = 7)
  }
  new_quantile <- function(x, probs, type) {
    collapse::fquantile(x, probs = probs, type = 7, names = FALSE)
  }

  mk <- function(rif_fn, quantile_fn, full_label) {
    suppressWarnings(bench::mark(
      bw_sj = {
        invisible(tryCatch(stats::bw.SJ(y_obs),
          error = function(e) stats::bw.nrd0(y_obs)
        ))
      },
      kde = {
        invisible(stats::density(y_obs, bw = bw, n = 1024))
      },
      quantile = {
        invisible(quantile_fn(y_obs, probs = taus, type = 7))
      },
      rif_multi_shared_dens = {
        invisible(rif_fn(y, taus, dens = dens))
      },
      prepare_outcome_full = if (full_label == "legacy") {
        {
          y_obs <- y[is.finite(y)]
          bw_use <- tryCatch(stats::bw.SJ(y_obs), error = function(e) stats::bw.nrd0(y_obs))
          dens_use <- stats::density(y_obs, bw = bw_use, n = 1024)
          rv <- legacy_compute_rif_multi(y, taus, dens = dens_use)
          dfl <- df
          rif_cols <- paste0("rif_", formatC(taus * 100, format = "d"))
          for (i in seq_along(taus)) dfl[[rif_cols[i]]] <- rv[[i]]
          attr(dfl, "rif_taus") <- taus
          attr(dfl, "rif_cols") <- rif_cols
          invisible(dfl)
        }
      } else {
        {
          invisible(ENGINE_REGISTRY$rif$prepare_outcome(df, "welfare", FALSE))
        }
      },
      iterations = iterations,
      check = FALSE,
      memory = TRUE
    ))
  }

  res_legacy <- suppressWarnings(mk(legacy_compute_rif_multi, legacy_quantile, "legacy"))
  res_new <- suppressWarnings(mk(compute_rif_multi, new_quantile, "new"))

  cat("\n-- legacy --\n")
  print(res_legacy[, c("expression", "min", "median", "mem_alloc")])
  cat("\n-- new --\n")
  print(res_new[, c("expression", "min", "median", "mem_alloc")])

  pick <- function(res, expr) {
    as.numeric(res$median[res$expression == expr]) * 1e3
  }
  cat(sprintf(
    paste0(
      "\nsummary (median, ms):\n",
      "  quantile        %6.1f -> %6.1f\n",
      "  rif_multi       %6.1f -> %6.1f\n",
      "  prepare_outcome %6.1f -> %6.1f\n"
    ),
    pick(res_legacy, "quantile"), pick(res_new, "quantile"),
    pick(res_legacy, "rif_multi_shared_dens"), pick(res_new, "rif_multi_shared_dens"),
    pick(res_legacy, "prepare_outcome_full"), pick(res_new, "prepare_outcome_full")
  ))
}
