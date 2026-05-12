# fct_aggregation.R
# -----------------
# Welfare aggregation functions — all called at simulation time.
# Stateless and testable without Shiny.
#
# Called by:
#   - mod_2_01_weathersim.R (via fct_run_simulation)
#   - mod_2_02_results.R    (hist_aggregate_choices, combine_ensemble_results)
#   - mod_3_06_results.R    (apply_deviation)
#
# Exports:
#   resolve_agg_fn, resolve_band_q
#   combine_ensemble_results
#   hist_aggregate_choices
#   aggregate_outcome, deviation_from_centre
#   aggregate_sim_preds, apply_deviation
#
# Internal:
#   draw_residuals_vec
#
# The closed-form delta-method aggregator (aggregate_with_uncertainty_delta)
# lives in fct_aggregation_delta.R.

#' Resolve Aggregation Function from Method String
#'
#' Maps an aggregation method string to a function of the form
#' \code{function(welfare, weights, pov_line) -> scalar}. Used by
#' \code{aggregate_with_uncertainty_delta()} to compute the headline statistic
#' for each Monte Carlo draw.
#'
#' @param method Character. One of \code{"mean"}, \code{"median"},
#'   \code{"headcount_ratio"}, \code{"gap"}, \code{"fgt2"},
#'   \code{"gini"}, \code{"prosperity_gap"}, \code{"avg_poverty"},
#'   \code{"total"}.
#'
#' @return A function \code{function(welfare, weights, pov_line)} returning
#'   a single numeric scalar.
#'
#' @export
resolve_agg_fn <- function(method) {
  switch(method,

    mean = function(welfare, weights, pov_line) {
      if (!is.null(weights))
        stats::weighted.mean(welfare, weights, na.rm = TRUE)
      else
        mean(welfare, na.rm = TRUE)
    },

    median = function(welfare, weights, pov_line) {
      if (!is.null(weights)) {
        # Weighted median via cumulative weight
        ord  <- order(welfare)
        w    <- weights[ord] / sum(weights, na.rm = TRUE)
        cumw <- cumsum(w)
        welfare[ord][which(cumw >= 0.5)[1L]]
      } else {
        stats::median(welfare, na.rm = TRUE)
      }
    },

    total = function(welfare, weights, pov_line) {
      if (!is.null(weights))
        sum(welfare * weights, na.rm = TRUE)
      else
        sum(welfare, na.rm = TRUE)
    },

    headcount_ratio = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for headcount_ratio" = !is.null(pov_line))
      poor <- as.numeric(welfare < pov_line)
      if (!is.null(weights))
        stats::weighted.mean(poor, weights, na.rm = TRUE)
      else
        mean(poor, na.rm = TRUE)
    },

    gap = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for gap" = !is.null(pov_line))
      gaps <- pmax(pov_line - welfare, 0) / pov_line
      if (!is.null(weights))
        stats::weighted.mean(gaps, weights, na.rm = TRUE)
      else
        mean(gaps, na.rm = TRUE)
    },

    fgt2 = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for fgt2" = !is.null(pov_line))
      sq <- (pmax(pov_line - welfare, 0) / pov_line)^2
      if (!is.null(weights))
        stats::weighted.mean(sq, weights, na.rm = TRUE)
      else
        mean(sq, na.rm = TRUE)
    },

    gini = function(welfare, weights, pov_line) {
      # Weighted Gini via the covariance formula — NA guard first
      valid   <- !is.na(welfare)
      welfare <- welfare[valid]
      if (!is.null(weights)) weights <- weights[valid]
      n <- length(welfare)
      if (n < 2L) return(NA_real_)
      if (!is.null(weights)) {
        ord  <- order(welfare)
        w    <- weights[ord] / sum(weights, na.rm = TRUE)
        y    <- welfare[ord]
        F_i  <- cumsum(w) - w / 2
        2 * sum(w * y * F_i) / sum(w * y) - 1
      } else {
        ord <- order(welfare)
        y   <- welfare[ord]
        n   <- length(y)
        2 * sum((seq_len(n) / n - 0.5) * y) / (n * mean(y))
      }
    },

    prosperity_gap = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for prosperity_gap" = !is.null(pov_line))
      gaps <- pmax(pov_line - welfare, 0)
      if (!is.null(weights))
        stats::weighted.mean(gaps, weights, na.rm = TRUE)
      else
        mean(gaps, na.rm = TRUE)
    },

    avg_poverty = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for avg_poverty" = !is.null(pov_line))
      poor <- welfare < pov_line
      if (sum(poor, na.rm = TRUE) == 0L) return(NA_real_)
      if (!is.null(weights))
        stats::weighted.mean(welfare[poor], weights[poor], na.rm = TRUE)
      else
        mean(welfare[poor], na.rm = TRUE)
    },

    stop(sprintf("[resolve_agg_fn] Unknown method: '%s'", method))
  )
}


#' Resolve Band Quantiles from Band Width Selection
#'
#' Maps the user-facing band width UI selection to a named numeric vector
#' of lower and upper quantile values for percentile band computation.
#'
#' @param band_width Character. One of \code{"p10_p90"}, \code{"p025_p975"},
#'   \code{"p005_p995"}, \code{"minmax"}, \code{"none"}.
#'
#' @return Named numeric vector \code{c(lo = ..., hi = ...)} or \code{NULL}
#'   when \code{band_width = "none"}.
#'
#' @export
resolve_band_q <- function(band_width) {
  switch(band_width %||% "p10_p90",
    p25_p75   = c(lo = 0.25,  hi = 0.75),
    p20_p80   = c(lo = 0.20,  hi = 0.80),
    p10_p90   = c(lo = 0.10,  hi = 0.90),
    p05_p95   = c(lo = 0.05,  hi = 0.95),
    p025_p975 = c(lo = 0.025, hi = 0.975),
    p005_p995 = c(lo = 0.005, hi = 0.995),
    minmax    = c(lo = 0.001, hi = 0.999),
    none      = NULL,
               c(lo = 0.10,  hi = 0.90)   # default
  )
}

# ---------------------------------------------------------------------------- #
# Residual drawing helper                                                       #
# ---------------------------------------------------------------------------- #

#' Draw a Residual Vector for Welfare Simulation
#'
#' Draws a length-N residual vector for addition to log-scale welfare
#' predictions. Called once per \code{aggregate_with_uncertainty_delta()}
#' invocation. Residuals are independent of coefficient uncertainty (Option A).
#'
#' @param residuals Character. One of \code{"none"}, \code{"original"},
#'   \code{"normal"}, \code{"resample"}.
#' @param train_aug Data frame. Augmented training data with \code{.resid}
#'   column and optionally an id column for household matching. Returned by
#'   \code{run_sim_pipeline()} as \code{$train_aug}. Required for all
#'   \code{residuals} options except \code{"none"}.
#' @param N Integer. Number of households in the simulation newdata.
#' @param id_vec Optional character/integer vector of length N. Household IDs
#'   in simulation newdata for \code{residuals = "original"} matching.
#' @param id_col Optional character. Name of the ID column in \code{train_aug}.
#'
#' @return Numeric vector of length N. Zero vector when
#'   \code{residuals = "none"}.
#'
#' @noRd
draw_residuals_vec <- function(residuals,
                               train_aug = NULL,
                               N,
                               id_vec  = NULL,
                               id_col  = NULL) {
  switch(residuals,

    none = rep(0, N),

    original = {
      if (is.null(train_aug) || !".resid" %in% names(train_aug))
        stop("[draw_residuals_vec] train_aug with .resid required for residuals = 'original'.")
      if (is.null(id_vec) || is.null(id_col))
        stop("[draw_residuals_vec] id_vec and id_col required for residuals = 'original'.")
      if (!id_col %in% names(train_aug))
        stop(sprintf("[draw_residuals_vec] id_col '%s' not found in train_aug.", id_col))

      # Match simulation households to their training residuals by ID.
      # Unmatched households fall back to resampled residual.
      resid_lookup <- stats::setNames(train_aug$.resid, train_aug[[id_col]])
      matched      <- as.character(id_vec) %in% names(resid_lookup)

      out <- numeric(N)
      out[matched]  <- resid_lookup[as.character(id_vec[matched])]
      out[!matched] <- sample(train_aug$.resid, sum(!matched), replace = TRUE)
      out
    },

    normal = {
      if (is.null(train_aug) || !".resid" %in% names(train_aug))
        stop("[draw_residuals_vec] train_aug with .resid required for residuals = 'normal'.")
      sigma_hat <- stats::sd(train_aug$.resid, na.rm = TRUE)
      stats::rnorm(N, mean = 0, sd = sigma_hat)
    },

    resample = {
      if (is.null(train_aug) || !".resid" %in% names(train_aug))
        stop("[draw_residuals_vec] train_aug with .resid required for residuals = 'resample'.")
      sample(train_aug$.resid, N, replace = TRUE)
    },

    stop(sprintf("[draw_residuals_vec] Unknown residuals option: '%s'", residuals))
  )
}

#' Combine Per-Model Aggregation Results
#'
#' Given M tibbles (one per GCM/ensemble model), each with per-year
#' aggregate values and optional coefficient-uncertainty bands, produce
#' a single tibble with multi-model median as central value and both
#' parameter uncertainty and model spread bands.
#'
#' @param member_results List of M lists, each from
#'   `aggregate_with_uncertainty_delta()` (one per CMIP6 ensemble member).
#' @return A tibble with columns: `sim_year`, `value`, `value_p05`,
#'   `value_p95`, `model_values`.
#' @export
combine_ensemble_results <- function(member_results,
                                     band_q = c(lo = 0.10, hi = 0.90)) {
  member_results <- Filter(Negate(is.null), member_results)
  if (length(member_results) == 0L) return(NULL)

  values <- vapply(member_results, `[[`, numeric(1L), "value")
  value  <- mean(values, na.rm = TRUE)

  # Analytic pooling via law of total variance:
  #   Var(T) = E[Var(T|m)] + Var(E[T|m])
  #          = mean(var_coef + var_resid) + var(values across members)
  var_coef_vec <- vapply(member_results,
                         function(x) x$var_coef  %||% NA_real_, numeric(1L))
  var_res_vec  <- vapply(member_results,
                         function(x) x$var_resid %||% NA_real_, numeric(1L))
  var_within <- mean(var_coef_vec + var_res_vec, na.rm = TRUE)
  if (!is.finite(var_within)) var_within <- 0
  var_across <- if (length(values) > 1L) stats::var(values, na.rm = TRUE) else 0
  if (!is.finite(var_across)) var_across <- 0
  var_pool   <- var_within + var_across
  se_pool    <- sqrt(max(var_pool, 0))

  # Per-member SDs propagated separately so callers can build per-outcome
  # bands without recovering var_coef/var_resid from the member list.
  per_member_sd <- sqrt(pmax(var_coef_vec + var_res_vec, 0, na.rm = FALSE))

  z_lo <- stats::qnorm(band_q[["lo"]])
  z_hi <- stats::qnorm(band_q[["hi"]])

  list(
    value     = value,
    value_all = values,

    # Thick band — point estimates only (weather + model spread)
    value_lo  = unname(stats::quantile(values, band_q[["lo"]], na.rm = TRUE)),
    value_hi  = unname(stats::quantile(values, band_q[["hi"]], na.rm = TRUE)),

    # Thin line — analytic pooled SE (coefficient + residual + model spread)
    coef_lo   = value + z_lo * se_pool,
    coef_hi   = value + z_hi * se_pool,

    model_lo  = min(values,  na.rm = TRUE),
    model_q10 = unname(stats::quantile(values, 0.10, na.rm = TRUE)),
    model_q25 = unname(stats::quantile(values, 0.25, na.rm = TRUE)),
    model_med = unname(stats::quantile(values, 0.50, na.rm = TRUE)),
    model_q75 = unname(stats::quantile(values, 0.75, na.rm = TRUE)),
    model_q90 = unname(stats::quantile(values, 0.90, na.rm = TRUE)),
    model_hi  = max(values,  na.rm = TRUE),
    draw_values = NULL,
    var_pool    = var_pool,
    var_within  = var_within,
    var_across  = var_across,
    per_member_sd = per_member_sd,
    n_members   = length(member_results)
  )
}

# ---------------------------------------------------------------------------- #
# Aggregation choices                                                          #
# ---------------------------------------------------------------------------- #

#' Aggregation Method Choices for Historical Simulation
#'
#' Returns the named character vector of aggregation method choices used to
#' populate the `selectInput` in the simulation results tab. Binary outcomes
#' are restricted to rate (mean) only.
#'
#' @param outcome_type A character string; `"logical"` for binary outcomes,
#'   any other value for continuous outcomes.
#'
#' @return A named character vector suitable for use in `shiny::selectInput()`.
#'
#' @export
hist_aggregate_choices <- function(outcome_type, outcome_name = NULL) {
  if (identical(outcome_type, "logical")) {
    # Binary outcomes: Mean
    c("Mean" = "mean")
  } else if (identical(outcome_type, "numeric") &&
             identical(outcome_name, "welfare")) {
    # Welfare outcomes: full suite including FGT, Gini, prosperity gap, and average poverty
    c(
      "Mean"                     = "mean",
      "Median"                   = "median",
      "Total"                    = "total",
      "Poverty rate"             = "headcount_ratio",
      "Poverty gap"              = "gap",
      "Poverty severity"         = "fgt2",
      "Gini"                     = "gini",
      "Prosperity gap"           = "prosperity_gap",
      "Average poverty (days/$)" = "avg_poverty"
    )
  } else {
    # All other numeric outcomes (wage, hours, employment, etc.)
    c(
      "Mean"                    = "mean",
      "Median"                  = "median",
      "Total"                   = "total",
      "Outcome headcount ratio" = "headcount_ratio",
      "Outcome gap"             = "gap",
      "Outcome severity"        = "fgt2",
      "Gini"                    = "gini"
    )
  }
}



# ---------------------------------------------------------------------------- #
# Aggregate predictions for plotting
# ---------------------------------------------------------------------------- #

#' Aggregate a Predicted Outcome Across Observations Within Groups
#'
#' Aggregates a predicted individual-level outcome across observations within
#' each group (e.g. simulation year), producing a single summary statistic per
#' group. This is typically used to collapse individual-level predictions from
#' a simulation into group-level indicators before plotting.
#'
#' For binary outcomes the aggregate is always the population share with a value
#' of 1 (i.e. the mean). For continuous outcomes a range of aggregates are
#' available, including poverty and inequality measures.
#'
#' @param df A data frame containing individual-level predictions.
#' @param outcome A character string giving the name of the outcome column in
#'   `df`
#' @param group A character string giving the name of the grouping column.
#'   Defaults to `sim_year`.
#' @param type Either `continuous` (default) or `binary`. For
#'   binary outcomes the aggregate is always the population share with outcome
#'   equal to 1, and the `aggregate` and `pov_line` arguments are
#'   ignored.
#' @param aggregate The summary statistic to compute when `type =
#'   "continuous`. One of `mean` (default), `sum`,
#'   `median`, `headcount_ratio` (population share with outcome
#'   below `pov_line`), `gap` (average normalised shortfall below
#'   `pov_line` across the whole population, i.e. the Foster-Greer-Thorbecke
#'   P1 index), or `gini` (Gini coefficient). Ignored when
#'   `type = "binary`.
#' @param pov_line A numeric poverty line required when `aggregate` is
#'   `headcount_ratio` or `gap`. Ignored otherwise.
#' @param weights  An optional character string giving the name of a survey
#'   weight column in `preds`. When supplied, passed to `aggregate_outcome()`;
#'   `NULL` (default) weights all observations equally.
#'
#' @return A tibble with one row per group containing the grouping column
#'   and a column named `value` holding the computed aggregate.
#'
#' @examples
#' library(dplyr)
#'
#' # Simulate 50 simulation years, 1000 individuals each
#' set.seed(42)
#' sim_data <- data.frame(
#'   sim_year = rep(1:50, each = 1000),
#'   welfare  = exp(rnorm(50000, mean = log(3.50), sd = 0.8))
#' )
#'
#' # Headcount poverty rate at $3.00/day
#' pov_by_year <- aggregate_outcome(
#'   df        = sim_data,
#'   outcome   = "welfare",
#'   type      = "continuous",
#'   aggregate = "headcount_ratio",
#'   pov_line  = 3.00
#' )
#'
#' head(pov_by_year)
#'
#' @importFrom dplyr group_by summarise
#' @importFrom rlang sym
#' @export
aggregate_outcome <- function(df,
                              outcome,
                              group     = "sim_year",
                              type      = c("continuous", "binary"),
                              aggregate = c("mean", "sum", "total", "median",
                                            "headcount_ratio", "gap", "fgt2", "gini",
                                            "prosperity_gap", "avg_poverty"),
                              pov_line  = NULL,
                              weights   = NULL) {

  type      <- match.arg(type)
  # For binary outcomes the aggregate argument is ignored (always population
  # share). Skip match.arg so UI values like "binary" don't cause an error.
  if (type != "binary") {
    aggregate <- match.arg(aggregate)
    # 'total' is a user-facing alias for 'sum'
    if (aggregate == "total") aggregate <- "sum"
  }

  gini_coef <- function(x, w) {
    # Remove NA / non-finite values (present in future sim predictions)
    ok <- is.finite(x)
    if (!is.null(w)) ok <- ok & is.finite(w)
    x  <- x[ok]
    w  <- if (!is.null(w)) w[ok] else NULL

    if (length(x) < 2) return(NA_real_)

    ord    <- order(x)
    x      <- x[ord]
    w      <- if (is.null(w)) rep(1, length(x)) else w[ord]
    w      <- w / sum(w)
    lorenz <- cumsum(w * x) / sum(w * x)
    lorenz <- c(0, lorenz)
    cx     <- c(0, cumsum(w))
    B      <- sum(diff(cx) * (lorenz[-length(lorenz)] + lorenz[-1]) / 2)
    1 - 2 * B
  }

  compute <- function(x, w) {
    if (type == "binary") {
      # multiply by 100 to express as percentage points
      if (is.null(w)) 100*mean(as.numeric(x), na.rm = TRUE)
      else 100*sum(as.numeric(x) * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
    } else {
      switch(aggregate,
        mean            = if (is.null(w)) mean(x, na.rm = TRUE)
                          else sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE),
        sum             = if (is.null(w)) sum(x, na.rm = TRUE)
                          else sum(x * w, na.rm = TRUE),
        median          = if (is.null(w)) median(x, na.rm = TRUE) else {
                            valid <- is.finite(x) & is.finite(w) & w > 0
                            if (sum(valid) == 0L) return(NA_real_)
                            x <- x[valid]; w <- w[valid]
                            ord  <- order(x); x <- x[ord]; w <- w[ord]
                            cumw <- cumsum(w) / sum(w)
                            x[which(cumw >= 0.5)[1]]
                          },
        headcount_ratio = {
          if (is.null(pov_line)) stop("`pov_line` must be supplied for headcount_ratio")
          poor <- as.numeric(x < pov_line)
          if (is.null(w)) mean(poor, na.rm = TRUE)
          else sum(poor * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        },
        gap             = {
          if (is.null(pov_line)) stop("`pov_line` must be supplied for gap")
          shortfall <- pmax(pov_line - x, 0) / pov_line
          if (is.null(w)) mean(shortfall, na.rm = TRUE)
          else sum(shortfall * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        },
        fgt2 = {
          if (is.null(pov_line)) stop("`pov_line` must be supplied for fgt2")
          shortfall <- pmax(pov_line - x, 0) / pov_line
          if (is.null(w)) mean(shortfall^2, na.rm = TRUE)
          else sum((shortfall^2) * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        },
        gini            = gini_coef(x, w),
        prosperity_gap  = {
          # Average factor by which incomes must be multiplied to reach $28/day.
          # For incomes already >= 28 the gap factor is 1 (no gap).
          pg <- pmax(28 / x, 1)
          if (is.null(w)) mean(pg, na.rm = TRUE)
          else sum(pg * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        },
        avg_poverty     = {
          # Average poverty = mean(1 / x): days needed to earn $1.
          # Non-positive incomes are excluded to avoid Inf / NaN.
          ok <- is.finite(x) & x > 0
          xp <- x[ok]
          wp <- if (!is.null(w)) w[ok] else NULL
          if (length(xp) == 0) return(NA_real_)
          if (is.null(wp)) mean(1 / xp, na.rm = TRUE)
          else sum((1 / xp) * wp, na.rm = TRUE) / sum(wp, na.rm = TRUE)
        }
      )
    }
  }

  df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group))) |>
    summarise(
      value = compute(
        x = .data[[outcome]],
        w = if (!is.null(weights)) .data[[weights]] else NULL
      ),
      .groups = "drop"
    )
}

# ---------------------------------------------------------------------------- #
# Convert to deviation from centre for plotting
# ---------------------------------------------------------------------------- #

#' Express Aggregate Values as Deviation from a Central Tendency
#'
#' Takes the output of `aggregate_outcome()` and expresses each group's
#' value as a deviation from either the mean or median across all groups.
#' Optionally flips the sign of the result for loss-framed outcomes, where a
#' positive deviation is undesirable (e.g. poverty rates, deficits).
#'
#' @param df A data frame with at least a grouping column and a `value`
#'   column, as returned by `aggregate_outcome()`.
#' @param group A character string giving the name of the grouping column.
#'   Defaults to `"sim_year"`.
#' @param centre A character string, either `"mean"` (default) or
#'   `"median"`, specifying the central tendency to deviate from.
#' @param loss Logical. If `TRUE`, the sign of the deviation is flipped so
#'   that positive values represent outcomes worse than the centre (e.g. higher
#'   poverty than expected). Defaults to `FALSE`.
#'
#' @return A tibble with the same columns as `df` and `value`
#'   replaced by the deviation from the chosen centre, optionally sign-flipped.
#'
#' @examples
#' library(dplyr)
#'
#' set.seed(42)
#' sim_data <- data.frame(
#'   sim_year = rep(1:50, each = 1000),
#'   welfare  = exp(rnorm(50000, mean = log(3.50), sd = 0.8))
#' )
#'
#' pov_by_year <- aggregate_outcome(
#'   df        = sim_data,
#'   outcome   = "welfare",
#'   type      = "continuous",
#'   aggregate = "headcount_ratio",
#'   pov_line  = 3.00
#' )
#'
#' # Deviation from mean, welfare framing (higher welfare = good)
#' deviation_from_centre(pov_by_year)
#'
#' # Deviation from median, loss framing (higher poverty = bad)
#' deviation_from_centre(pov_by_year, centre = "median", loss = TRUE)
#'
#' @importFrom dplyr mutate
#' @export
deviation_from_centre <- function(df,
                                  group  = "sim_year",
                                  centre = c("mean", "median"),
                                  loss   = FALSE) {

  centre <- match.arg(centre)

  ref <- switch(centre,
    mean   = mean(df$value,   na.rm = TRUE),
    median = median(df$value, na.rm = TRUE)
  )

  sign <- if (loss) -1 else 1

  df |>
    mutate(value = sign * (value - ref))
}

# NOTE: plot_exceedance() and plot_hist_sim() have been archived to
# dev/archived_fct/plot_exceedance_archived.R. They are superseded by
# enhance_exceedance() in fct_sim_compare.R and have no active call sites.

# ---------------------------------------------------------------------------- #
# Shared aggregation helper                                                    #
# ---------------------------------------------------------------------------- #

#' Aggregate Simulation Predictions for Exceedance Plotting
#'
#' Aggregates individual-level predictions by sim_year, optionally applies
#' deviation_from_centre(), and returns the aggregated data frame with x_label.
#' @param preds      Data frame of individual-level predictions with a
#'   `sim_year` column and the outcome column named by `so$name`.
#' @param so         One-row outcome metadata data frame (columns `name`,
#'   `label`, `type`).
#' @param agg_method Character. One of `"mean"`, `"median"`,
#'   `"headcount_ratio"`, `"gap"`, `"gini"`.
#' @param deviation  Character. One of `"none"`, `"mean"`, `"median"`.
#' @param loss_frame Logical. Passed to `deviation_from_centre()` as `loss`.
#' @param pov_line   Numeric or `NULL`. Required for `"headcount_ratio"` /
#'   `"gap"`.
#' @param weights    Character or `NULL`. Name of the survey weight column in
#'   `preds`. When non-`NULL` and present in `preds`, passed to
#'   `aggregate_outcome()`. Default `NULL` (unweighted).
#'
#' @return A list with elements `out` (aggregated data frame with `sim_year`
#'   and `value` columns) and `x_label` (character).
#'
#' @importFrom rlang abort
#' @export
aggregate_sim_preds <- function(preds, so, agg_method, deviation, loss_frame,
                                pov_line = NULL, weights = NULL) {

  if (agg_method %in% c("headcount_ratio", "gap", "fgt2") && is.null(pov_line)) {
    rlang::abort(
      "`pov_line` must be supplied when `agg_method` is 'headcount_ratio','gap', or 'fgt2'."
    )
  }

  # Group by (model, sim_year) when a model column is present (future scenarios
  # with all ensemble members pooled), so the CI reflects model × year variation.
  # --- Two-stage aggregation ----------------------------------------------- #
  # IMPORTANT: order of operations matters for coefficient uncertainty bands.  #
  # Stage 1: aggregate within each (draw_id, model, sim_year) to a scalar.    #
  #          This gives one aggregate statistic per coefficient draw.           #
  # Stage 2: take percentiles of that scalar across draw_id.                   #
  # Taking percentiles BEFORE aggregating gives quantiles of the individual    #
  # welfare distribution — a completely different quantity. Don't mix these up. #
  # --------------------------------------------------------------------------- #

  has_draws <- "draw_id" %in% names(preds) && !all(is.na(preds$draw_id))

  # Stage 1 grouping: always include draw_id when present so each draw
  # produces its own aggregate scalar before we summarise across draws.
  grp_cols <- c(
    if (has_draws)                          "draw_id",
    if ("model" %in% names(preds))          "model",
    "sim_year"
  )

  out <- aggregate_outcome(
    df        = preds,
    outcome   = so$name,
    group     = grp_cols,
    type      = if (identical(so$type, "logical")) "binary" else "continuous",
    aggregate = agg_method,
    pov_line  = pov_line,
    weights   = if (!is.null(weights) && weights %in% names(preds)) weights else NULL
  )

  # Stage 2: collapse across draw_id to get coefficient-uncertainty percentiles.
  # IMPORTANT: percentiles are taken ACROSS draw_id (one scalar per draw),
  # NOT across household-level predictions. These are uncertainty bands on the
  # aggregate statistic, not quantiles of the individual welfare distribution.
  if (has_draws) {
    out <- out |>
      dplyr::group_by(dplyr::across(dplyr::any_of(c("model", "sim_year")))) |>
      dplyr::summarise(
        value_p05 = stats::quantile(value, 0.05, na.rm = TRUE),
        value_p50 = stats::quantile(value, 0.50, na.rm = TRUE),
        value_p95 = stats::quantile(value, 0.95, na.rm = TRUE),
        value     = value_p50,
        .groups   = "drop"
      )
  }

  if (!identical(deviation, "none")) {
    out <- deviation_from_centre(out, "sim_year", deviation, loss_frame)
    names(out)[names(out) == "value"] <- "deviation"
  }

  list(
    out     = out,
    x_label = "Simulation year"
  )
}

#' Apply Deviation from Historical Reference Value
#'
#' Subtracts a historical reference value from a data frame's \code{value}
#' column to express results as deviations from a baseline. Used in Module 2
#' results and Module 3 comparison to centre scenario values against the
#' historical mean or median.
#'
#' @param d          Data frame with a \code{value} column. Returns \code{d}
#'   unchanged if \code{deviation = "none"} or \code{d} is \code{NULL}.
#' @param deviation  Character. One of \code{"none"}, \code{"mean"},
#'   \code{"median"}. Determines how the reference value is computed when
#'   \code{hist_ref} is not supplied.
#' @param hist_ref   Numeric. Pre-computed reference value (e.g. historical
#'   mean). When \code{NA} (default), computed from \code{d$value} using
#'   \code{deviation}.
#'
#' @return Data frame with \code{value} column replaced by
#'   \code{value - hist_ref}.
#'
#' @export
apply_deviation <- function(d, deviation, hist_ref = NA_real_) {
  if (identical(deviation, "none") || is.null(d)) return(d)
  if (is.na(hist_ref)) {
    hist_ref <- if (identical(deviation, "mean"))
      mean(d$value, na.rm = TRUE)
    else
      stats::median(d$value, na.rm = TRUE)
  }
  dplyr::mutate(d, value = value - hist_ref)
}

#' Compute exceedance curve ribbon from Cholesky draw values
#'
#' For each of S coefficient draws, computes the exceedance probability
#' (1 - ECDF) at a grid of welfare values across all N simulation years.
#' Returns p10/p90 envelope across S draws — the coefficient uncertainty
#' ribbon for the exceedance plot.
#'
#' @param agg_tbl  Tibble. Output of compute_hist_agg() or
#'   compute_scenario_agg() for one method. Must have columns:
#'   value (point estimate per year) and draw_values (list column,
#'   S draws per year).
#' @param band_q   Named numeric(2). Quantile bounds for ribbon.
#'   Default c(lo = 0.10, hi = 0.90).
#'
#' @return Tibble with columns: exceed_prob (y-axis exceedance probability),
#'   welfare_mid (point estimate welfare), welfare_lo, welfare_hi
#'   (p10/p90 coefficient uncertainty bounds). NULL if draw_values unavailable.
#' @noRd
compute_exceedance_ribbon <- function(agg_tbl,
                                      band_q = c(lo = 0.10, hi = 0.90),
                                      model_lo = NULL, model_hi = NULL) {
  N_years <- nrow(agg_tbl)
  if (N_years == 0L) return(NULL)

  rank_order     <- order(agg_tbl$value, decreasing = TRUE)
  probs          <- (seq_len(N_years) - 0.5) / N_years
  welfare_sorted <- sort(agg_tbl$value, decreasing = TRUE)

  draw_list <- agg_tbl$draw_values
  has_draws <- !is.null(draw_list) && length(draw_list) > 0L &&
               !is.null(draw_list[[1L]]) && length(draw_list[[1L]]) >= 2L

  if (has_draws) {
    S <- length(draw_list[[1L]])
    # Build N_years × S matrix — each column = one draw across all years
    draw_mat <- matrix(
      unlist(draw_list, use.names = FALSE),
      nrow  = N_years,
      ncol  = S,
      byrow = TRUE
    )
    ordered_mat <- draw_mat[rank_order, ]
    coef_lo <- matrixStats::rowQuantiles(
               ordered_mat, probs = band_q[["lo"]], na.rm = TRUE)
    coef_hi <- matrixStats::rowQuantiles(
                ordered_mat, probs = band_q[["hi"]], na.rm = TRUE)
  } else if (all(c("coef_lo", "coef_hi") %in% names(agg_tbl))) {
    # Delta-method path — use analytic band columns directly.
    # Re-order to match the descending welfare ranking.
    coef_lo <- agg_tbl$coef_lo[rank_order]
    coef_hi <- agg_tbl$coef_hi[rank_order]
  } else {
    return(NULL)
  }

  # Option A approximation — apply coef width to ensemble bounds
  # Ensemble uncertainty bands use coefficient uncertainty width from
  # the mean ensemble member applied to lo/hi members.
  # This approximates the joint distribution. Error is small for linear
  # aggregates (<5% of band width) and conservative for poverty measures
  # (understates true joint uncertainty by ~10-20%).
  # Option B (per-member Cholesky draws) available if tighter bounds needed.
  # See known_issues.md #18 and methodology workplan for full discussion.
  coef_width_lo <- welfare_sorted - coef_lo   # half-width below central
  coef_width_hi <- coef_hi - welfare_sorted   # half-width above central

  final_lo <- if (!is.null(model_lo))
    pmin(coef_lo, model_lo - coef_width_lo)
  else coef_lo

  final_hi <- if (!is.null(model_hi))
    pmax(coef_hi, model_hi + coef_width_hi)
  else coef_hi

  tibble::tibble(
    exceed_prob  = probs,
    welfare_mid  = welfare_sorted,
    welfare_lo   = final_lo,
    welfare_hi   = final_hi
  )
}