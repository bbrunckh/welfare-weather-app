# Welfare aggregation kernel (single-pass) ----
# Compiled kernel behind the multi-method aggregation suite. Computes all
# nine statistics of `resolve_agg_fn()` in one scan plus one stable sort per
# group (simulation year), mirroring each method's exact NA/Inf/weight rules.
# Parity with `resolve_agg_fn()` is mandatory and enforced to float tolerance
# in test-fct-aggregation-kernel.R; `resolve_agg_fn()` remains the single-
# method oracle and the fallback for methods the kernel does not cover.

#' @useDynLib wiseapp, .registration = TRUE
NULL

#' Compute the Full Welfare Aggregation Suite for One Group
#'
#' R wrapper around the compiled \code{welfare_stats_all()} kernel. Returns
#' the nine statistics in the fixed kernel order with the stable sort order
#' attached as attribute \code{"order"} (1-based, matching \code{order()}).
#'
#' @param welfare Numeric vector of welfare values for one group.
#' @param weights Optional numeric weights (same length), or \code{NULL}.
#' @param pov_line Headcount threshold. \code{NA} yields \code{NA} for
#'   \code{headcount_ratio}; other methods ignore it, exactly like
#'   \code{resolve_agg_fn()}.
#' @param prosp_line Prosperity threshold; the app hardcodes $28/day.
#'
#' @return Named numeric vector: mean, median, total, headcount_ratio,
#'   poverty_gap, poverty_severity (fgt2), gini, prosperity_gap,
#'   avg_poverty; attribute \code{"order"} carries the stable sort index.
#'
#' @noRd
welfare_stats_suite <- function(welfare, weights = NULL,
                                pov_line = NA_real_, prosp_line = 28) {
  if (!is.numeric(welfare)) {
    stop("[welfare_stats_suite] welfare must be numeric.", call. = FALSE)
  }
  w <- if (is.null(weights)) {
    numeric(0)
  } else {
    if (!is.numeric(weights)) {
      stop("[welfare_stats_suite] weights must be numeric.", call. = FALSE)
    }
    weights
  }
  welfare_stats_all(welfare, w, pov_line, pov_line, pov_line, prosp_line)
}

#' Extract Kernel Statistics and Sort Order
#'
#' Internal helper: pulls the named statistic (or NULL when absent) and the
#' reusable stable sort order out of a \code{welfare_stats_suite()} result.
#'
#' @param suite Named numeric vector from \code{welfare_stats_suite()}.
#' @param method Aggregation method name.
#' @return List with elements \code{value} (numeric(1) or NULL) and
#'   \code{order} (integer vector, or NULL).
#' @noRd
.welfare_kernel_pick <- function(suite, method) {
  if (is.null(suite)) {
    return(list(value = NULL, order = NULL))
  }
  idx <- match(method, names(suite))
  list(
    value = if (is.na(idx)) NULL else as.numeric(suite[[idx]]),
    order = .welfare_kernel_order(suite)
  )
}

.welfare_kernel_order <- function(suite) {
  ord <- attr(suite, "order", exact = TRUE)
  if (is.null(ord)) NULL else ord
}
