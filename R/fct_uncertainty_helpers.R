# Uncertainty decomposition helpers ----
#
# Small pure helpers shared by Module 2 (climate sim results) and Module 3
# (policy simulation results) for the three-source uncertainty decomposition
# (coefficient / inter-annual / inter-model). All functions are package-internal
# (not exported) and take plain R data; they have no Shiny dependencies.

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
  if (is.null(tbl) || nrow(tbl) == 0L) {
    return(NULL)
  }
  required <- c("sim_year", "model_id", "value_all", "value_all_sd")
  if (!all(required %in% names(tbl))) {
    return(NULL)
  }

  cell <- function(x, k) if (is.list(x)) x[[k]] else x[[k]]
  ids_by_year <- lapply(seq_len(nrow(tbl)), function(k) {
    as.character(cell(tbl$model_id, k))
  })
  vals_by_year <- lapply(seq_len(nrow(tbl)), function(k) {
    as.numeric(cell(tbl$value_all, k))
  })
  sds_by_year <- lapply(seq_len(nrow(tbl)), function(k) {
    as.numeric(cell(tbl$value_all_sd, k))
  })
  valid_rows <- vapply(seq_along(ids_by_year), function(k) {
    length(ids_by_year[[k]]) > 0L &&
      length(vals_by_year[[k]]) == length(ids_by_year[[k]])
  }, logical(1L))
  if (!any(valid_rows)) {
    return(NULL)
  }

  all_ids <- unique(unlist(ids_by_year[valid_rows], use.names = FALSE))
  n_yrs <- nrow(tbl)
  vals_mat <- matrix(NA_real_,
    nrow = length(all_ids), ncol = n_yrs,
    dimnames = list(all_ids, tbl$sim_year)
  )
  sds_mat <- matrix(NA_real_,
    nrow = length(all_ids), ncol = n_yrs,
    dimnames = list(all_ids, tbl$sim_year)
  )
  for (k in seq_len(n_yrs)) {
    if (!valid_rows[[k]]) next
    ids <- ids_by_year[[k]]
    vals <- vals_by_year[[k]]
    sds <- sds_by_year[[k]]
    if (length(sds) == 1L && length(vals) > 1L) sds <- rep(sds, length(vals))
    if (length(sds) != length(vals)) sds <- rep(NA_real_, length(vals))
    keep <- !is.na(ids) & nzchar(ids) & ids %in% all_ids
    vals_mat[ids[keep], k] <- vals[keep]
    sds_mat[ids[keep], k] <- sds[keep]
  }
  list(
    vals = vals_mat, sds = sds_mat, model_ids = all_ids,
    sim_years = tbl$sim_year
  )
}

#' Per-model rank-interpolated values (and coefficient SDs) at each kept
#' return probability.
#'
#' Builds the `models x RPs` matrices explicitly with a loop. The previous
#' `t(apply(...))` formulation relied on `apply()` returning a matrix; with a
#' single admissible RP (two simulation years) and several ensemble members it
#' simplified to a vector and `t()` transposed it, silently recycling every
#' downstream row (duplicated threshold-table entries).
#'
#' @param vals    Numeric matrix (`models x sim years`) of point values.
#' @param sds     Numeric matrix (`models x sim years`) of coefficient SDs.
#' @param RPs_keep Named numeric vector of admissible adverse-tail probabilities.
#' @param adverse_tail Whether adverse outcomes are in the upper or lower tail.
#' @return List with `rp` and `sd`, both `models x RPs`.
#' @noRd
by_model_rp_matrix <- function(vals, sds, RPs_keep,
                               adverse_tail = c("high", "low")) {
  adverse_tail <- match.arg(adverse_tail)
  n_m <- nrow(vals)
  n_r <- length(RPs_keep)
  rp <- matrix(NA_real_, nrow = n_m, ncol = n_r)
  sd_at <- matrix(NA_real_, nrow = n_m, ncol = n_r)
  for (i in seq_len(n_m)) {
    v <- vals[i, ]
    ok <- is.finite(v)
    if (sum(ok) < 2L) next
    sv <- sort(v[ok])
    rp[i, ] <- vapply(RPs_keep, function(p) {
      rank_interp(sv, if (identical(adverse_tail, "high")) p else 1 - p)
    }, numeric(1L))
    s <- sds[i, ]
    if (all(is.na(s))) next
    s_sorted <- s[ok][order(v[ok])]
    sd_at[i, ] <- vapply(RPs_keep, function(p) {
      rank_interp(s_sorted, if (identical(adverse_tail, "high")) p else 1 - p)
    }, numeric(1L))
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
  if (isTRUE(use_minmax) && q <= 0.001) {
    return("min")
  }
  if (isTRUE(use_minmax) && q >= 0.999) {
    return("max")
  }
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
  if (n == 0L) {
    return(NA_real_)
  }
  k <- n * (1 - p) + 0.5
  if (k < 1 || k > n) {
    return(NA_real_)
  }
  lo <- floor(k)
  hi <- ceiling(k)
  if (lo == hi) {
    sorted_vals[lo]
  } else {
    sorted_vals[lo] + (k - lo) * (sorted_vals[hi] - sorted_vals[lo])
  }
}

# Baseline-selected annual support shared by metric-aware and technical tails.
# Year keys are retained explicitly so later states cannot accidentally apply
# the ranks to a different row order.
adverse_year_support <- function(values, sim_years, probability,
                                 adverse_tail = c("high", "low")) {
  adverse_tail <- tryCatch(match.arg(adverse_tail), error = function(e) NA_character_)
  empty <- list(status = "unavailable", reason = NULL,
    baseline_value = NA_real_, probability = NA_real_, return_period = NA_real_,
    adverse_tail = adverse_tail, n_years_total = length(values),
    n_years_finite = 0L, n_years_excluded = length(values), min_years = NA_integer_,
    rank_lo = NA_integer_, rank_hi = NA_integer_, year_lo = NA_real_, year_hi = NA_real_,
    weight_lo = NA_real_, weight_hi = NA_real_,
    quantile_method = "rank_interp_n_p_plus_half",
    tie_method = "value_then_sim_year_ascending")
  fail <- function(reason) { empty$reason <- reason; empty }
  if (is.na(adverse_tail)) return(fail("Unknown adverse-tail direction."))
  if (!is.numeric(values) || !is.numeric(sim_years) || length(values) != length(sim_years)) {
    return(fail("Annual values and numeric year keys must have matching lengths."))
  }
  if (any(!is.finite(sim_years)) || anyDuplicated(sim_years)) {
    return(fail("Annual year keys must be finite and unique."))
  }
  if (length(probability) != 1L || !is.finite(probability) || probability <= 0 || probability >= 1) {
    return(fail("Adverse probability must be a finite scalar between zero and one."))
  }
  empty$probability <- probability
  empty$return_period <- 1 / probability
  empty$min_years <- max(2L, as.integer(ceiling(1 / min(probability, 1 - probability))))
  finite <- is.finite(values)
  empty$n_years_finite <- sum(finite)
  empty$n_years_excluded <- sum(!finite)
  if (sum(finite) < empty$min_years) {
    return(fail("Insufficient finite baseline years for the requested return period."))
  }
  ord <- order(values[finite], sim_years[finite])
  sorted_values <- values[finite][ord]
  sorted_years <- sim_years[finite][ord]
  q_exceed <- if (identical(adverse_tail, "high")) probability else 1 - probability
  k <- length(sorted_values) * (1 - q_exceed) + 0.5
  if (!is.finite(k) || k < 1 || k > length(sorted_values)) {
    return(fail("Requested adverse rank is outside retained baseline-year support."))
  }
  lo <- as.integer(floor(k)); hi <- as.integer(ceiling(k))
  whi <- if (lo == hi) 0 else k - lo
  wlo <- 1 - whi
  empty$status <- "ok"
  empty$reason <- ""
  empty$baseline_value <- wlo * sorted_values[lo] + whi * sorted_values[hi]
  empty$rank_lo <- lo; empty$rank_hi <- hi
  empty$year_lo <- sorted_years[lo]; empty$year_hi <- sorted_years[hi]
  empty$weight_lo <- wlo; empty$weight_hi <- whi
  empty
}

apply_adverse_year_support <- function(values, sim_years, support) {
  fail <- function(reason) list(status = "unavailable", reason = reason, value = NA_real_)
  if (is.data.frame(support) && nrow(support)) {
    row <- support[1L, , drop = FALSE]
    support <- lapply(names(row), function(name) row[[name]][[1L]])
    names(support) <- names(row)
  }
  if (!is.list(support) || !identical(support$status, "ok")) {
    return(fail(support$reason %||% "Baseline adverse support is unavailable."))
  }
  if (!is.numeric(values) || !is.numeric(sim_years) || length(values) != length(sim_years) ||
      any(!is.finite(sim_years)) || anyDuplicated(sim_years)) {
    return(fail("Annual state values require unique finite numeric year keys."))
  }
  use_lo <- support$weight_lo != 0
  use_hi <- support$weight_hi != 0
  idx_lo <- if (use_lo) which(sim_years == support$year_lo) else integer()
  idx_hi <- if (use_hi) which(sim_years == support$year_hi) else integer()
  if ((use_lo && length(idx_lo) != 1L) || (use_hi && length(idx_hi) != 1L)) {
    return(fail("State does not contain each nonzero-weight baseline support year exactly once."))
  }
  selected <- c(if (use_lo) values[idx_lo], if (use_hi) values[idx_hi])
  if (any(!is.finite(selected))) return(fail("State is nonfinite at a selected baseline support year."))
  value <- 0
  if (use_lo) value <- value + support$weight_lo * values[idx_lo]
  if (use_hi) value <- value + support$weight_hi * values[idx_hi]
  list(status = "ok", reason = "", value = value)
}
