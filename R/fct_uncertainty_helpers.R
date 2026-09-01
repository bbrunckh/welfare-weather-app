# ============================================================================ #
# fct_uncertainty_helpers.R
#
# Small pure helpers shared by Module 2 (climate sim results) and Module 3
# (policy simulation results) for the three-source uncertainty decomposition
# (coefficient / inter-annual / inter-model). All functions are package-internal
# (not exported) and take plain R data; they have no Shiny dependencies.
# ============================================================================ #

#' Reshape a per-sim_year tibble with list-cols into a (model * year) matrix.
#'
#' Input `tbl` carries list-columns `model_id`, `value_all`, `value_all_sd`,
#' one row per simulation year. Output reshapes them into parallel matrices
#' keyed by model_id (rows) * sim_year (cols), recycling a scalar SD across
#' members when only one (historical) is present.
#'
#' @param tbl A data frame with `sim_year`, `model_id`, `value_all`,
#'   `value_all_sd` columns (list-cols for the last three).
#' @return A list with `vals` (matrix), `sds` (matrix), `model_ids` (chr),
#'   `sim_years` (numeric/int), or NULL if `tbl` is empty/NULL.
#' @noRd
by_model_matrix <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
  all_ids <- unique(unlist(tbl$model_id, use.names = FALSE))
  n_yrs   <- nrow(tbl)
  vals_mat <- matrix(NA_real_, nrow = length(all_ids), ncol = n_yrs,
                     dimnames = list(all_ids, tbl$sim_year))
  sds_mat  <- matrix(NA_real_, nrow = length(all_ids), ncol = n_yrs,
                     dimnames = list(all_ids, tbl$sim_year))
  for (k in seq_len(n_yrs)) {
    ids  <- as.character(tbl$model_id[[k]])
    vals <- tbl$value_all[[k]]
    sds  <- tbl$value_all_sd[[k]]
    if (length(sds) == 1L && length(vals) > 1L) sds <- rep(sds, length(vals))
    vals_mat[ids, k] <- vals
    sds_mat[ids,  k] <- sds
  }
  list(vals = vals_mat, sds = sds_mat, model_ids = all_ids,
       sim_years = tbl$sim_year)
}

#' Per-model rank-interpolated values (and coefficient SDs) at each kept
#' return probability.
#'
#' Builds the [models x RPs] matrices explicitly with a loop. The previous
#' `t(apply(...))` formulation relied on `apply()` returning a matrix; with a
#' single admissible RP (two simulation years) and several ensemble members it
#' simplified to a vector and `t()` transposed it, silently recycling every
#' downstream row (duplicated threshold-table entries).
#'
#' @param vals    Numeric matrix [models x sim years] of point values.
#' @param sds     Numeric matrix [models x sim years] of coefficient SDs.
#' @param RPs_keep Named numeric vector of admissible exceedance probabilities.
#' @return List with `rp` and `sd`, both [models x RPs].
#' @noRd
by_model_rp_matrix <- function(vals, sds, RPs_keep) {
  n_m <- nrow(vals)
  n_r <- length(RPs_keep)
  rp    <- matrix(NA_real_, nrow = n_m, ncol = n_r)
  sd_at <- matrix(NA_real_, nrow = n_m, ncol = n_r)
  for (i in seq_len(n_m)) {
    v  <- vals[i, ]
    ok <- is.finite(v)
    if (sum(ok) < 2L) next
    sv <- sort(v[ok])
    rp[i, ] <- vapply(RPs_keep, function(p) rank_interp(sv, p), numeric(1L))
    s <- sds[i, ]
    if (all(is.na(s))) next
    s_sorted <- s[ok][order(v[ok])]
    sd_at[i, ] <- vapply(RPs_keep, function(p) rank_interp(s_sorted, p),
                         numeric(1L))
  }
  list(rp = rp, sd = sd_at)
}

#' Format a quantile probability as "P05" / "P50" / "min" / "max".
#'
#' @param q  Numeric probability between 0 and 1.
#' @param use_minmax Logical; if TRUE map q <= 0.001 to "min" and q >= 0.999
#'   to "max" instead of "P00"/"P100".
#' @return A character scalar.
#' @noRd
pct_label <- function(q, use_minmax = FALSE) {
  if (isTRUE(use_minmax) && q <= 0.001) return("min")
  if (isTRUE(use_minmax) && q >= 0.999) return("max")
  paste0("P", formatC(round(q * 100), width = 2, flag = "0"))
}

#' Rank-position interpolation aligned with the exceedance plot.
#'
#' Computes `k = n * (1 - p) + 0.5` and linearly interpolates the sorted
#' vector at position k. Returns NA (no clamping) when the requested
#' probability falls outside the empirical range supported by n.
#'
#' @param sorted_vals Numeric vector sorted ascending.
#' @param p Numeric exceedance probability in (0, 1).
#' @return Interpolated value or NA_real_.
#' @noRd
rank_interp <- function(sorted_vals, p) {
  n <- length(sorted_vals)
  if (n == 0L) return(NA_real_)
  k <- n * (1 - p) + 0.5
  if (k < 1 || k > n) return(NA_real_)
  lo <- floor(k); hi <- ceiling(k)
  if (lo == hi) sorted_vals[lo]
  else sorted_vals[lo] + (k - lo) * (sorted_vals[hi] - sorted_vals[lo])
}
