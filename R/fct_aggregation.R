# Aggregation helpers ----
# Welfare aggregation functions - all called at simulation time.
# Stateless and testable without Shiny.
#
# Called by:
#   - mod_2_01_weathersim.R (via fct_run_simulation)
#   - mod_2_02_results.R    (hist_aggregate_choices, combine_ensemble_results)
#
# Exports:
#   resolve_agg_fn
#   combine_ensemble_results
#   hist_aggregate_choices
#   aggregate_outcome, deviation_from_centre
#   aggregate_sim_preds
#
# Internal:
#   draw_residuals_vec
#
# resolve_band_q() is defined once, in fct_sim_compare.R (a duplicate here
# with divergent "minmax" semantics was removed - DUP-01).
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
      if (!is.null(weights)) {
        stats::weighted.mean(welfare, weights, na.rm = TRUE)
      } else {
        mean(welfare, na.rm = TRUE)
      }
    },
    median = function(welfare, weights, pov_line) {
      if (!is.null(weights)) {
        # Weighted median via cumulative weight. Missing welfare rows are
        # dropped first so their weight does not shift the 0.5 crossing.
        valid <- !is.na(welfare)
        welfare <- welfare[valid]
        weights <- weights[valid]
        ord <- order(welfare)
        w <- weights[ord] / sum(weights, na.rm = TRUE)
        cumw <- cumsum(w)
        welfare[ord][which(cumw >= 0.5)[1L]]
      } else {
        stats::median(welfare, na.rm = TRUE)
      }
    },
    total = function(welfare, weights, pov_line) {
      if (!is.null(weights)) {
        sum(welfare * weights, na.rm = TRUE)
      } else {
        sum(welfare, na.rm = TRUE)
      }
    },
    headcount_ratio = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for headcount_ratio" = !is.null(pov_line))
      poor <- as.numeric(welfare < pov_line)
      if (!is.null(weights)) {
        stats::weighted.mean(poor, weights, na.rm = TRUE)
      } else {
        mean(poor, na.rm = TRUE)
      }
    },
    gap = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for gap" = !is.null(pov_line))
      gaps <- pmax(pov_line - welfare, 0) / pov_line
      if (!is.null(weights)) {
        stats::weighted.mean(gaps, weights, na.rm = TRUE)
      } else {
        mean(gaps, na.rm = TRUE)
      }
    },
    fgt2 = function(welfare, weights, pov_line) {
      stopifnot("pov_line required for fgt2" = !is.null(pov_line))
      sq <- (pmax(pov_line - welfare, 0) / pov_line)^2
      if (!is.null(weights)) {
        stats::weighted.mean(sq, weights, na.rm = TRUE)
      } else {
        mean(sq, na.rm = TRUE)
      }
    },
    gini = function(welfare, weights, pov_line) {
      # The app's single Gini definition (CR-BUG-10): weighted covariance
      # form, equal to the weighted Lorenz-curve area. No weights means
      # equal weights, which gives the standard sample Gini.
      valid <- !is.na(welfare)
      welfare <- welfare[valid]
      if (!is.null(weights)) weights <- weights[valid]
      n <- length(welfare)
      if (n < 2L) {
        return(NA_real_)
      }
      if (is.null(weights)) weights <- rep(1, n)
      ord <- order(welfare)
      w <- weights[ord] / sum(weights, na.rm = TRUE)
      y <- welfare[ord]
      F_i <- cumsum(w) - w / 2
      2 * sum(w * y * F_i) / sum(w * y) - 1
    },
    prosperity_gap = function(welfare, weights, pov_line) {
      # Average factor by which incomes must be multiplied to reach $28/day.
      # pov_line ignored - threshold is always hardcoded to $28/day.
      pg <- pmax(28 / welfare, 1)
      if (!is.null(weights)) {
        stats::weighted.mean(pg, weights, na.rm = TRUE)
      } else {
        mean(pg, na.rm = TRUE)
      }
    },
    avg_poverty = function(welfare, weights, pov_line) {
      # Average poverty = mean(1 / welfare): days needed to earn $1.
      # pov_line ignored.
      ok <- is.finite(welfare) & welfare > 0
      wp <- welfare[ok]
      wts <- if (!is.null(weights)) weights[ok] else NULL
      if (length(wp) == 0L) {
        return(NA_real_)
      }
      if (!is.null(wts)) {
        stats::weighted.mean(1 / wp, wts, na.rm = TRUE)
      } else {
        mean(1 / wp, na.rm = TRUE)
      }
    },
    stop(sprintf("[resolve_agg_fn] Unknown method: '%s'", method))
  )
}


# Residual drawing helper ----

#' Build the ID-to-residual named lookup for the "original" residual mode
#'
#' PERF-34: the lookup is identical for every year/method/weighting
#' combination over the same pipeline, so callers can build it once per
#' pipeline and pass it through instead of rebuilding the named vector and
#' character keys on every `draw_residuals_vec()` call. Returns NULL when
#' the inputs cannot support it (callers then compute internally).
#'
#' @param train_aug Data frame with `.resid` column.
#' @param id_col    Character name of the ID column.
#' @return Named numeric vector, or NULL.
#' @noRd
.residual_lookup <- function(train_aug, id_col) {
  if (is.null(train_aug) || !".resid" %in% names(train_aug) ||
    is.null(id_col) || !id_col %in% names(train_aug)) {
    NULL
  } else {
    stats::setNames(train_aug$.resid, train_aug[[id_col]])
  }
}

#' Residual variance of the training augmentation
#'
#' PERF-34 companion: `var(train_aug$.resid)` is rebuilt by every aggregate
#' call; build it once per pipeline alongside `.residual_lookup()`. NULL
#' when unavailable.
#'
#' @param train_aug Data frame with `.resid` column.
#' @return Numeric scalar, or NULL.
#' @noRd
.residual_sigma2 <- function(train_aug) {
  if (is.null(train_aug) || !".resid" %in% names(train_aug)) {
    NULL
  } else {
    stats::var(train_aug$.resid, na.rm = TRUE)
  }
}

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
#' @param seed Integer seed for stochastic residual modes.
#' @param resid_lookup Optional pre-built named residual vector from
#'   \code{.residual_lookup()} (PERF-34). When NULL it is built here.
#' @param resid_sigma2 Optional pre-built residual variance from
#'   \code{.residual_sigma2()} (PERF-34). When NULL it is computed here.
#'
#' @return Numeric vector of length N. Zero vector when
#'   \code{residuals = "none"}.
#'
#' @noRd
draw_residuals_vec <- function(residuals,
                               train_aug = NULL,
                               N,
                               id_vec = NULL,
                               id_col = NULL,
                               seed = WISEAPP_DEFAULT_SEED,
                               resid_lookup = NULL,
                               resid_sigma2 = NULL) {
  switch(residuals,
    none = rep(0, N),
    original = {
      if (is.null(train_aug) || !".resid" %in% names(train_aug)) {
        stop("[draw_residuals_vec] train_aug with .resid required for residuals = 'original'.")
      }
      if (is.null(id_vec) || is.null(id_col)) {
        stop("[draw_residuals_vec] id_vec and id_col required for residuals = 'original'.")
      }
      if (!id_col %in% names(train_aug)) {
        stop(sprintf("[draw_residuals_vec] id_col '%s' not found in train_aug.", id_col))
      }

      # Match simulation households to their training residuals by ID.
      # Unmatched households use a stable hash-indexed lookup so the fallback
      # neither depends on caller RNG state nor consumes the global stream.
      # PERF-34: the lookup may be supplied pre-built by the pipeline owner.
      resid_lookup <- resid_lookup %||% .residual_lookup(train_aug, id_col)
      matched <- as.character(id_vec) %in% names(resid_lookup)

      out <- numeric(N)
      out[matched] <- resid_lookup[as.character(id_vec[matched])]
      out[!matched] <- deterministic_values_by_key(
        train_aug$.resid,
        id_vec[!matched],
        seed = wise_seed(seed, "original-unmatched")
      )
      out
    },
    normal = {
      if (is.null(train_aug) || !".resid" %in% names(train_aug)) {
        stop("[draw_residuals_vec] train_aug with .resid required for residuals = 'normal'.")
      }
      # sd(x) is sqrt(var(x)) in R, so sqrt of a cached var() is bit-identical
      # to the per-call stats::sd() this replaces (PERF-34).
      sigma_hat <- if (!is.null(resid_sigma2)) {
        sqrt(resid_sigma2)
      } else {
        stats::sd(train_aug$.resid, na.rm = TRUE)
      }
      withr::with_seed(seed, stats::rnorm(N, mean = 0, sd = sigma_hat))
    },
    resample = {
      if (is.null(train_aug) || !".resid" %in% names(train_aug)) {
        stop("[draw_residuals_vec] train_aug with .resid required for residuals = 'resample'.")
      }
      withr::with_seed(seed, sample(train_aug$.resid, N, replace = TRUE))
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
#' @param band_q Named numeric vector \code{c(lo =, hi =)} of ensemble
#'   quantiles for the thick model-spread band. Default \code{c(0.10, 0.90)}.
#' @return A tibble with columns: `sim_year`, `value`, `value_p05`,
#'   `value_p95`, `model_values`.
#' @export
combine_ensemble_results <- function(member_results,
                                     band_q = c(lo = 0.10, hi = 0.90)) {
  member_results <- Filter(Negate(is.null), member_results)
  if (length(member_results) == 0L) {
    return(NULL)
  }

  values <- vapply(member_results, `[[`, numeric(1L), "value")
  value <- mean(values, na.rm = TRUE)

  # Analytic pooling via law of total variance:
  #   Var(T) = E[Var(T|m)] + Var(E[T|m])
  #          = mean(var_coef + var_resid) + var(values across members)
  var_coef_vec <- vapply(
    member_results,
    function(x) x$var_coef %||% NA_real_, numeric(1L)
  )
  var_res_vec <- vapply(
    member_results,
    function(x) x$var_resid %||% NA_real_, numeric(1L)
  )
  var_within <- mean(var_coef_vec + var_res_vec, na.rm = TRUE)
  if (!is.finite(var_within)) var_within <- 0
  var_across <- if (length(values) > 1L) stats::var(values, na.rm = TRUE) else 0
  if (!is.finite(var_across)) var_across <- 0
  var_pool <- var_within + var_across
  se_pool <- sqrt(max(var_pool, 0))

  # Per-member SDs propagated separately so callers can build per-outcome
  # bands without recovering var_coef/var_resid from the member list.
  per_member_sd <- sqrt(pmax(var_coef_vec + var_res_vec, 0, na.rm = FALSE))

  z_lo <- stats::qnorm(band_q[["lo"]])
  z_hi <- stats::qnorm(band_q[["hi"]])

  list(
    value = value,
    value_all = values,

    # Thick band - point estimates only (weather + model spread)
    value_lo = unname(stats::quantile(values, band_q[["lo"]], na.rm = TRUE)),
    value_hi = unname(stats::quantile(values, band_q[["hi"]], na.rm = TRUE)),

    # Thin line - analytic pooled SE (coefficient + residual + model spread)
    coef_lo = value + z_lo * se_pool,
    coef_hi = value + z_hi * se_pool,
    model_lo = min(values, na.rm = TRUE),
    model_q10 = unname(stats::quantile(values, 0.10, na.rm = TRUE)),
    model_q25 = unname(stats::quantile(values, 0.25, na.rm = TRUE)),
    model_med = unname(stats::quantile(values, 0.50, na.rm = TRUE)),
    model_q75 = unname(stats::quantile(values, 0.75, na.rm = TRUE)),
    model_q90 = unname(stats::quantile(values, 0.90, na.rm = TRUE)),
    model_hi = max(values, na.rm = TRUE),
    draw_values = NULL,
    var_pool = var_pool,
    var_within = var_within,
    var_across = var_across,
    per_member_sd = per_member_sd,
    n_members = length(member_results)
  )
}

# Aggregation choices ----

#' Aggregation Method Choices for Historical Simulation
#'
#' Returns the named character vector of aggregation method choices used to
#' populate the `selectInput` in the simulation results tab. Binary outcomes
#' are restricted to rate (mean) only.
#'
#' @param outcome_type A character string; `"logical"` for binary outcomes,
#'   any other value for continuous outcomes.
#' @param outcome_name Optional character scalar. Outcome variable name; when
#'   `"welfare"` (with a numeric outcome type), the full welfare-specific
#'   method suite is offered.
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


# Aggregate predictions for plotting ----

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
                              group = "sim_year",
                              type = c("continuous", "binary"),
                              aggregate = c(
                                "mean", "sum", "total", "median",
                                "headcount_ratio", "gap", "fgt2", "gini",
                                "prosperity_gap", "avg_poverty"
                              ),
                              pov_line = NULL,
                              weights = NULL) {
  type <- match.arg(type)
  # For binary outcomes the aggregate argument is ignored (always population
  # share). Skip match.arg so UI values like "binary" don't cause an error.
  if (type != "binary") {
    aggregate <- match.arg(aggregate)
    # 'total' is a user-facing alias for 'sum'
    if (aggregate == "total") aggregate <- "sum"
  }

  gini_coef <- function(x, w) {
    # Remove NA / non-finite values (present in future sim predictions),
    # then use the app's single Gini definition in resolve_agg_fn().
    ok <- is.finite(x)
    if (!is.null(w)) ok <- ok & is.finite(w)
    resolve_agg_fn("gini")(x[ok], if (!is.null(w)) w[ok], NULL)
  }

  compute <- function(x, w) {
    if (type == "binary") {
      # multiply by 100 to express as percentage points
      if (is.null(w)) {
        100 * mean(as.numeric(x), na.rm = TRUE)
      } else {
        100 * sum(as.numeric(x) * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
      }
    } else {
      switch(aggregate,
        mean = if (is.null(w)) {
          mean(x, na.rm = TRUE)
        } else {
          sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
        },
        sum = if (is.null(w)) {
          sum(x, na.rm = TRUE)
        } else {
          sum(x * w, na.rm = TRUE)
        },
        median = if (is.null(w)) {
          stats::median(x, na.rm = TRUE)
        } else {
          valid <- is.finite(x) & is.finite(w) & w > 0
          if (sum(valid) == 0L) {
            return(NA_real_)
          }
          x <- x[valid]
          w <- w[valid]
          ord <- order(x)
          x <- x[ord]
          w <- w[ord]
          cumw <- cumsum(w) / sum(w)
          x[which(cumw >= 0.5)[1]]
        },
        headcount_ratio = {
          if (is.null(pov_line)) stop("`pov_line` must be supplied for headcount_ratio")
          poor <- as.numeric(x < pov_line)
          if (is.null(w)) {
            mean(poor, na.rm = TRUE)
          } else {
            sum(poor * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
          }
        },
        gap = {
          if (is.null(pov_line)) stop("`pov_line` must be supplied for gap")
          shortfall <- pmax(pov_line - x, 0) / pov_line
          if (is.null(w)) {
            mean(shortfall, na.rm = TRUE)
          } else {
            sum(shortfall * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
          }
        },
        fgt2 = {
          if (is.null(pov_line)) stop("`pov_line` must be supplied for fgt2")
          shortfall <- pmax(pov_line - x, 0) / pov_line
          if (is.null(w)) {
            mean(shortfall^2, na.rm = TRUE)
          } else {
            sum((shortfall^2) * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
          }
        },
        gini = gini_coef(x, w),
        prosperity_gap = {
          # Average factor by which incomes must be multiplied to reach $28/day.
          # For incomes already >= 28 the gap factor is 1 (no gap).
          pg <- pmax(28 / x, 1)
          if (is.null(w)) {
            mean(pg, na.rm = TRUE)
          } else {
            sum(pg * w, na.rm = TRUE) / sum(w, na.rm = TRUE)
          }
        },
        avg_poverty = {
          # Average poverty = mean(1 / x): days needed to earn $1.
          # Non-positive incomes are excluded to avoid Inf / NaN.
          ok <- is.finite(x) & x > 0
          xp <- x[ok]
          wp <- if (!is.null(w)) w[ok] else NULL
          if (length(xp) == 0) {
            return(NA_real_)
          }
          if (is.null(wp)) {
            mean(1 / xp, na.rm = TRUE)
          } else {
            sum((1 / xp) * wp, na.rm = TRUE) / sum(wp, na.rm = TRUE)
          }
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

# Convert to deviation from centre for plotting ----

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
                                  group = "sim_year",
                                  centre = c("mean", "median"),
                                  loss = FALSE) {
  centre <- match.arg(centre)

  ref <- switch(centre,
    mean   = mean(df$value, na.rm = TRUE),
    median = stats::median(df$value, na.rm = TRUE)
  )

  sign <- if (loss) -1 else 1

  df |>
    mutate(value = sign * (value - ref))
}

# NOTE: plot_exceedance() and plot_hist_sim() had no active call sites and
# were removed (formerly archived under
# dev/archived_fct/plot_exceedance_archived.R). They are superseded by
# archived enhance_exceedance() renderer in dev/static plots/sim_compare_static.R.

# Shared aggregation helper ----

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
  # with all ensemble members pooled), so the CI reflects model * year variation.
  # Two-stage aggregation ----
  # IMPORTANT: order of operations matters for coefficient uncertainty bands.  #
  # Stage 1: aggregate within each (draw_id, model, sim_year) to a scalar.    #
  #          This gives one aggregate statistic per coefficient draw.           #
  # Stage 2: take percentiles of that scalar across draw_id.                   #
  # Taking percentiles BEFORE aggregating gives quantiles of the individual    #
  # welfare distribution - a completely different quantity. Don't mix these up. #

  has_draws <- "draw_id" %in% names(preds) && !all(is.na(preds$draw_id))

  # Stage 1 grouping: always include draw_id when present so each draw
  # produces its own aggregate scalar before we summarise across draws.
  grp_cols <- c(
    if (has_draws) "draw_id",
    if ("model" %in% names(preds)) "model",
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

# Shared pipeline aggregation table ----

#' Build the canonical per-year aggregation table for one or more pipelines
#'
#' Step 2 and Step 3 both aggregate a historical pipeline or a set of ensemble
#' pipelines into the same list-column schema. Keeping that schema construction
#' here prevents the two result panes from drifting in their model IDs,
#' uncertainty fields, or variance decomposition.
#'
#' @param pipelines A pipeline-like list, or a named list of such pipelines.
#' @param method Aggregation method.
#' @param weighted Logical. Use each pipeline's survey weights when available.
#' @param pov_line Numeric poverty line, when required by the method.
#' @param residuals Residual draw mode.
#' @param is_log Logical. Whether pipeline predictions are on the log scale.
#' @param band_q Named numeric vector of coefficient-band quantiles.
#' @param skip_coef Logical. Drop coefficient loadings when TRUE.
#' @param bandwidth_p0 Numeric bandwidth for the headcount-ratio kernel.
#' @param seed Integer base seed for deterministic residual streams.
#' @param model_ids Optional character vector naming the pipelines.
#' @param method_label Optional method value stored in output metadata.
#' @param scenario Optional scenario label stored in output metadata.
#' @return A tibble with one row per simulation year and list-columns for the
#'   per-model values, standard errors, and coefficient gradients.
#' @noRd
aggregate_pipeline_table <- function(pipelines,
                                     method,
                                     weighted = TRUE,
                                     pov_line = NULL,
                                     residuals = "original",
                                     is_log = TRUE,
                                     band_q = c(lo = 0.10, hi = 0.90),
                                     skip_coef = FALSE,
                                     bandwidth_p0 = 0.05,
                                     seed = WISEAPP_DEFAULT_SEED,
                                     model_ids = NULL,
                                     method_label = method,
                                     scenario = NULL,
                                     shared_context = NULL,
                                     preparation_cache = NULL) {
  if (is.null(pipelines)) {
    return(tibble::tibble())
  }
  if (!is.null(pipelines$y_point)) pipelines <- list(pipelines)
  if (!is.list(pipelines) || length(pipelines) == 0L) {
    return(tibble::tibble())
  }

  if (is.null(model_ids)) model_ids <- names(pipelines)
  if (is.null(model_ids)) model_ids <- character(0)
  if (length(model_ids) != length(pipelines) || any(!nzchar(model_ids))) {
    model_ids <- paste0("model_", seq_along(pipelines))
  }

  if (is.null(preparation_cache)) {
    preparation_cache <- .new_aggregation_preparation_cache()
  }

  per_model <- lapply(pipelines, function(pipe) {
    aggregate_pipeline_per_year(
      pipe = pipe,
      method = method,
      weighted = weighted,
      pov_line = pov_line,
      residuals = residuals,
      is_log = is_log,
      band_q = band_q,
      skip_coef = skip_coef,
      bandwidth_p0 = bandwidth_p0,
      seed = seed,
      shared_context = shared_context,
      preparation_cache = preparation_cache
    )
  })
  per_model_by_year <- lapply(per_model, .index_aggregation_results_by_year)

  # The first pipeline defines the simulation-year grid, matching the
  # simulation contract used by both result panes.
  years <- sort(unique(pipelines[[1L]]$sim_year))
  if (length(years) == 0L) {
    return(tibble::tibble())
  }

  rows <- lapply(years, function(year) {
    per_year <- lapply(per_model_by_year, function(results) results[[as.character(year)]] %||% NULL)
    keep <- !vapply(per_year, is.null, logical(1L))
    per_year <- per_year[keep]
    ids <- model_ids[keep]
    if (length(per_year) == 0L) {
      return(NULL)
    }

    values <- vapply(per_year, `[[`, numeric(1L), "value")
    sds <- sqrt(pmax(vapply(
      per_year,
      function(x) (x$var_coef %||% 0) + (x$var_resid %||% 0),
      numeric(1L)
    ), 0))

    gradients <- lapply(per_year, `[[`, "F_agg")
    F_mat <- if (all(vapply(gradients, is.null, logical(1L)))) {
      NULL
    } else {
      first_gradient <- which(!vapply(gradients, is.null, logical(1L)))[1L]
      n_factors <- length(gradients[[first_gradient]])
      do.call(rbind, lapply(gradients, function(value) {
        if (is.null(value)) rep(NA_real_, n_factors) else as.numeric(value)
      }))
    }

    if (length(per_year) == 1L) {
      var_within <- sds[[1L]]^2
      var_across <- 0
    } else {
      combined <- combine_ensemble_results(per_year, band_q = band_q)
      var_within <- combined$var_within %||% mean(sds^2, na.rm = TRUE)
      var_across <- combined$var_across %||% stats::var(values, na.rm = TRUE)
    }

    row <- tibble::tibble(
      sim_year     = year,
      value        = mean(values, na.rm = TRUE),
      model_id     = list(ids),
      value_all    = list(values),
      value_all_sd = list(sds),
      F_agg_all    = list(F_mat),
      var_within   = var_within,
      var_across   = var_across,
      agg_method   = method_label,
      weighted     = weighted
    )
    if (!is.null(scenario)) row$scenario <- scenario
    row
  })

  dplyr::bind_rows(Filter(Negate(is.null), rows))
}

.index_aggregation_results_by_year <- function(results) {
  if (is.null(results) || !length(results)) return(list())
  out <- list()
  for (result in results) {
    year <- result$sim_year %||% NA_integer_
    key <- as.character(year)
    if (!identical(key, "NA") && is.null(out[[key]])) out[[key]] <- result
  }
  out
}

#' Build canonical aggregation tables for several methods in one pass
#'
#' The weighted path shares per-pipeline preparation across all methods. The
#' returned object is keyed by method and each value has the same schema as
#' \\code{aggregate_pipeline_table()}.
#'
#' @param pipelines A pipeline-like list, or a named list of such pipelines.
#' @param methods Character vector of aggregation methods.
#' @param weighted Logical. Use each pipeline's survey weights when available.
#' @param pov_lines Optional named list of method-specific poverty lines.
#' @param pov_line Fallback poverty line.
#' @param residuals Residual draw mode.
#' @param is_log Logical. Whether pipeline predictions are on the log scale.
#' @param band_q Named numeric vector of coefficient-band quantiles.
#' @param skip_coef Logical. Drop coefficient loadings when TRUE.
#' @param bandwidth_p0 Numeric bandwidth for the headcount-ratio kernel.
#' @param seed Integer base seed for deterministic residual streams.
#' @param model_ids Optional character vector naming the pipelines.
#' @param scenario Optional scenario label stored in output metadata.
#' @param shared_context Optional shared pipeline context.
#' @param preparation_cache Optional bounded preparation cache.
#' @return Named list of canonical aggregation tibbles.
#' @noRd
aggregate_pipeline_tables_multi <- function(pipelines,
                                             methods,
                                             weighted = TRUE,
                                             pov_lines = NULL,
                                             pov_line = NULL,
                                             residuals = "original",
                                             is_log = TRUE,
                                             band_q = c(lo = 0.10, hi = 0.90),
                                             skip_coef = FALSE,
                                             bandwidth_p0 = 0.05,
                                             seed = WISEAPP_DEFAULT_SEED,
                                             model_ids = NULL,
                                             scenario = NULL,
                                             shared_context = NULL,
                                             preparation_cache = NULL) {
  if (is.null(pipelines)) return(setNames(vector("list", length(methods)), methods))
  if (!is.null(pipelines$y_point)) pipelines <- list(pipelines)
  if (!is.list(pipelines) || length(pipelines) == 0L || !length(methods)) {
    return(setNames(vector("list", length(methods)), methods))
  }
  if (is.null(model_ids)) model_ids <- names(pipelines)
  if (is.null(model_ids)) model_ids <- character(0)
  if (length(model_ids) != length(pipelines) || any(!nzchar(model_ids))) {
    model_ids <- paste0("model_", seq_along(pipelines))
  }
  if (is.null(preparation_cache)) {
    preparation_cache <- .new_aggregation_preparation_cache()
  }

  per_model <- lapply(pipelines, function(pipe) {
    aggregate_pipeline_per_year_multi(
      pipe = pipe,
      methods = methods,
      weighted = weighted,
      pov_lines = pov_lines,
      pov_line = pov_line,
      residuals = residuals,
      is_log = is_log,
      band_q = band_q,
      skip_coef = skip_coef,
      bandwidth_p0 = bandwidth_p0,
      seed = seed,
      shared_context = shared_context,
      preparation_cache = preparation_cache
    )
  })
  per_model_by_year <- lapply(per_model, function(results_by_method) {
    lapply(results_by_method, .index_aggregation_results_by_year)
  })
  years <- sort(unique(pipelines[[1L]]$sim_year))
  if (!length(years)) return(setNames(vector("list", length(methods)), methods))

  build_table <- function(method) {
    per_method <- lapply(per_model_by_year, `[[`, method)
    rows <- lapply(years, function(year) {
      per_year <- lapply(per_method, function(results) results[[as.character(year)]] %||% NULL)
      keep <- !vapply(per_year, is.null, logical(1L))
      per_year <- per_year[keep]
      ids <- model_ids[keep]
      if (!length(per_year)) return(NULL)
      values <- vapply(per_year, `[[`, numeric(1L), "value")
      sds <- sqrt(pmax(vapply(
        per_year,
        function(x) (x$var_coef %||% 0) + (x$var_resid %||% 0),
        numeric(1L)
      ), 0))
      gradients <- lapply(per_year, `[[`, "F_agg")
      F_mat <- if (all(vapply(gradients, is.null, logical(1L)))) {
        NULL
      } else {
        first_gradient <- which(!vapply(gradients, is.null, logical(1L)))[1L]
        n_factors <- length(gradients[[first_gradient]])
        do.call(rbind, lapply(gradients, function(value) {
          if (is.null(value)) rep(NA_real_, n_factors) else as.numeric(value)
        }))
      }
      if (length(per_year) == 1L) {
        var_within <- sds[[1L]]^2
        var_across <- 0
      } else {
        combined <- combine_ensemble_results(per_year, band_q = band_q)
        var_within <- combined$var_within %||% mean(sds^2, na.rm = TRUE)
        var_across <- combined$var_across %||% stats::var(values, na.rm = TRUE)
      }
      row <- tibble::tibble(
        sim_year = year,
        value = mean(values, na.rm = TRUE),
        model_id = list(ids),
        value_all = list(values),
        value_all_sd = list(sds),
        F_agg_all = list(F_mat),
        var_within = var_within,
        var_across = var_across,
        agg_method = method,
        weighted = weighted
      )
      if (!is.null(scenario)) row$scenario <- scenario
      row
    })
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }
  setNames(lapply(methods, build_table), methods)
}

# The single table the Results tab displays for one method.
#
# Faithful extraction of the argument sets in mod_2_02_results.R
# (.build_hist_for_method() / .build_scn_for_method(), inner build_for()). The
# worker calls this to compute streamed partial tables; the module must call
# this helper too (Phase 5 switches it) so the two sides cannot drift.
#
# Branches: weighted when the pipelines carry weights, otherwise unweighted;
# prosperity_gap goes through aggregate_pipeline_table(); every other method
# is read from the full-suite aggregate_pipeline_tables_multi() call. With
# `suite = FALSE` only the requested method is computed (same values, checked
# by tests; not the default until parity is established on real runs).
#
# @param pipelines One historical pipeline (`historical = TRUE`) or a named
#   list of scenario member pipelines.
# @param so Outcome metadata (type, name, transform).
# @return list(table, weight_key, weighted): `weight_key` is "weighted" or
#   "unweighted", the slot the module displays.
# @noRd
step2_display_aggregation <- function(pipelines,
                                      method,
                                      so,
                                      residuals,
                                      skip_coef,
                                      pov_line,
                                      bandwidth_p0 = 0.05,
                                      shared_context = NULL,
                                      historical = FALSE,
                                      preparation_cache = NULL,
                                      suite = TRUE) {
  first <- if (isTRUE(historical) || !is.null(pipelines$y_point)) {
    pipelines
  } else {
    pipelines[[1L]]
  }
  has_w <- !is.null(first$weight)
  weighted <- has_w
  is_log <- isTRUE(so$transform == "log")
  band_q <- c(lo = 0.10, hi = 0.90)
  model_ids <- if (isTRUE(historical)) {
    "Historical"
  } else {
    names(pipelines) %||% paste0("m", seq_along(pipelines))
  }
  scenario <- if (isTRUE(historical)) "Historical" else NULL

  if (!identical(method, "prosperity_gap")) {
    suite_pov <- pov_line %||% 3
    methods <- if (isTRUE(suite)) {
      unname(hist_aggregate_choices(so$type, so$name))
    } else {
      method
    }
    out <- aggregate_pipeline_tables_multi(
      pipelines = pipelines,
      methods = methods,
      weighted = weighted,
      pov_lines = setNames(lapply(methods, function(x) suite_pov), methods),
      residuals = residuals,
      is_log = is_log,
      band_q = band_q,
      skip_coef = skip_coef,
      bandwidth_p0 = bandwidth_p0,
      model_ids = model_ids,
      scenario = scenario,
      shared_context = shared_context,
      preparation_cache = preparation_cache
    )[[method]]
  } else {
    out <- aggregate_pipeline_table(
      pipelines = pipelines,
      method = method,
      weighted = weighted,
      pov_line = pov_line,
      residuals = residuals,
      is_log = is_log,
      band_q = band_q,
      skip_coef = skip_coef,
      bandwidth_p0 = bandwidth_p0,
      model_ids = model_ids,
      scenario = scenario,
      shared_context = shared_context,
      preparation_cache = preparation_cache
    )
  }
  list(
    table = out,
    weight_key = if (has_w) "weighted" else "unweighted",
    weighted = weighted
  )
}

# The full method suite the Results tab can display, for one group.
#
# Computes aggregate_pipeline_tables_multi() ONCE over every method the
# outcome supports except prosperity_gap, with the same arguments as the
# module's suite branch (the captured pov_line for every method), so each
# table is identical() to what the module displays for that method.
# prosperity_gap is not part of the module's suite path (it uses
# aggregate_pipeline_table()); it is added only when it is the captured
# display `method`, via step2_display_aggregation().
#
# @inheritParams step2_display_aggregation
# @return list(tables, weight_key, weighted): `tables` is a named list
#   method -> table, in hist_aggregate_choices() order.
# @noRd
step2_display_aggregation_suite <- function(pipelines,
                                            method,
                                            so,
                                            residuals,
                                            skip_coef,
                                            pov_line,
                                            bandwidth_p0 = 0.05,
                                            shared_context = NULL,
                                            historical = FALSE,
                                            preparation_cache = NULL) {
  first <- if (isTRUE(historical) || !is.null(pipelines$y_point)) {
    pipelines
  } else {
    pipelines[[1L]]
  }
  weighted <- !is.null(first$weight)
  methods <- setdiff(
    unname(hist_aggregate_choices(so$type, so$name)), "prosperity_gap"
  )
  if (is.null(preparation_cache)) {
    preparation_cache <- .new_aggregation_preparation_cache()
  }
  suite_pov <- pov_line %||% 3
  tables <- aggregate_pipeline_tables_multi(
    pipelines = pipelines,
    methods = methods,
    weighted = weighted,
    pov_lines = setNames(lapply(methods, function(x) suite_pov), methods),
    residuals = residuals,
    is_log = isTRUE(so$transform == "log"),
    band_q = c(lo = 0.10, hi = 0.90),
    skip_coef = skip_coef,
    bandwidth_p0 = bandwidth_p0,
    model_ids = if (isTRUE(historical)) {
      "Historical"
    } else {
      names(pipelines) %||% paste0("m", seq_along(pipelines))
    },
    scenario = if (isTRUE(historical)) "Historical" else NULL,
    shared_context = shared_context,
    preparation_cache = preparation_cache
  )
  if (identical(method, "prosperity_gap")) {
    tables[["prosperity_gap"]] <- step2_display_aggregation(
      pipelines = pipelines, method = method, so = so, residuals = residuals,
      skip_coef = skip_coef, pov_line = pov_line,
      bandwidth_p0 = bandwidth_p0, shared_context = shared_context,
      historical = historical, preparation_cache = preparation_cache
    )$table
  }
  list(
    tables = tables,
    weight_key = if (weighted) "weighted" else "unweighted",
    weighted = weighted
  )
}

# Session-scoped cache for compact aggregation suites shared by Results tabs.
new_shared_aggregation_cache <- function(max_entries = 16L) {
  cache <- new.env(parent = emptyenv())
  cache$keys <- character(0)
  cache$max_entries <- as.integer(max_entries)
  cache
}

shared_aggregation_cache_get <- function(cache, key) {
  if (is.null(cache) || !is.environment(cache) || !nzchar(key)) return(NULL)
  value <- get0(key, envir = cache)
  if (!is.null(value)) cache$keys <- c(setdiff(cache$keys, key), key)
  value
}

shared_aggregation_cache_put <- function(cache, key, value) {
  if (is.null(cache) || !is.environment(cache) || !nzchar(key)) {
    return(invisible(value))
  }
  assign(key, value, envir = cache)
  cache$keys <- c(setdiff(cache$keys, key), key)
  while (length(cache$keys) > cache$max_entries) {
    evict <- cache$keys[[1L]]
    cache$keys <- cache$keys[-1L]
    if (exists(evict, envir = cache, inherits = FALSE)) rm(list = evict, envir = cache)
  }
  invisible(value)
}

shared_aggregation_cache_key <- function(run_signature, poverty_line,
                                         bandwidth_p0, weighted, residuals,
                                         skip_coef, is_log, methods) {
  digest::digest(list(
    schema = 1L, run_signature = run_signature,
    poverty_line = poverty_line, bandwidth_p0 = bandwidth_p0,
    weighted = isTRUE(weighted), residuals = residuals,
    skip_coef = isTRUE(skip_coef), is_log = isTRUE(is_log), methods = methods
  ), algo = "xxhash64")
}
