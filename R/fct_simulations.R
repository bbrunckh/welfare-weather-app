# ============================================================================ #
# Used by mod_2_02_historical_sim_server() and mod_2_04_future_sim_server(),
# and 2_06_sim_compare_server().
#
# Stateless and testable without Shiny.

# ============================================================================ #

# ---- Internal colour / style helpers ---------------------------------------
# Used by plot_bar_climate() and enhance_exceedance(). Not exported.

# Canonical SSP keys  must match what .normalise_ssp() returns
.ssp_colours <- c(
  "SSP2-4.5" = "#4dac26",   # green  (SSP2 = lower emissions = better)
  "SSP3-7.0" = "#2166ac",   # blue   (SSP3 = mid)
  "SSP5-8.5" = "#c0392b"    # red    (SSP5 = high emissions = worse)
)



# Normalise any SSP token found in a scenario name to a canonical key
# e.g. "SSP2" -> "SSP2-4.5", "SSP3-7.0" -> "SSP3-7.0", NA -> NA
.normalise_ssp <- function(nm) {
  # Try full form first: SSP2-4.5 / SSP3-7.0 / SSP5-8.5
  m_full <- regmatches(nm, regexpr("SSP[0-9]-[0-9.]+", nm))
  if (length(m_full) > 0) return(m_full)
  # Try short form: SSP2 / SSP3 / SSP5
  m_short <- regmatches(nm, regexpr("SSP([2-9])", nm))
  if (length(m_short) == 0) return(NA_character_)
  digit <- sub("SSP", "", m_short)
  lookup <- c("2" = "SSP2-4.5", "3" = "SSP3-7.0", "5" = "SSP5-8.5")
  if (digit %in% names(lookup)) lookup[[digit]] else NA_character_
}

.parse_year <- function(nm) {
  m <- regmatches(nm, regexpr("[0-9]{4}(?= \u00b1)", nm, perl = TRUE))
  if (length(m) == 0) NA_character_ else m
}



# ---------------------------------------------------------------------------- #
# Simulation date grid                                                         #
# ---------------------------------------------------------------------------- #

#' Build the Date Grid for the Historical Simulation
#'
#' Constructs a vector of first-of-month dates covering every combination of
#' interview month present in `survey_weather` and every year in the requested
#' historical period. These dates are passed directly to `get_weather()` as the
#' `dates` argument.
#'
#' @param survey_weather A data frame containing at least an `int_month` column
#'   (integer, 1-12) representing the interview month of each survey
#'   observation.
#' @param year_range A length-2 integer vector `c(start_year, end_year)`
#'   defining the historical period to simulate over.
#'
#' @return A `Date` vector of first-of-month dates (one per month x year
#'   combination).
#'
#' @examples
#' svy <- data.frame(int_month = c(3L, 6L, 9L))
#' build_hist_sim_dates(svy, c(2000L, 2002L))
#'
#' @export
build_hist_sim_dates <- function(survey_weather, year_range) {
  months <- unique(survey_weather$int_month)
  years  <- seq(year_range[1], year_range[2])

  with(
    expand.grid(int_month = months, int_year = years),
    as.Date(paste(int_year, int_month, "01", sep = "-"))
  )
}

# ---------------------------------------------------------------------------- #
# Residual choice helpers                                                      #
# ---------------------------------------------------------------------------- #

#' Available residual handling choices
#'
#' @return A named character vector suitable for use in `radioButtons()`.
#' @export
residual_choices <- function() {
  c(
    "None (fitted values only)"         = "none",
    "Original (match by individual ID)" = "original",
    "Resample from training residuals"  = "empirical",
    "Draw from normal distribution"     = "normal"
  )
}


# ---------------------------------------------------------------------------- #
# Perturbation method helper                                                   #
# ---------------------------------------------------------------------------- #

#' Build a Perturbation Method Vector for Climate Simulations
#'
#' Derives the named `perturbation_method` vector required by `get_weather()`
#' when an SSP scenario is active. Precipitation and similar accumulation
#' variables (units `"mm"` or `"days"`) use `"multiplicative"` scaling;
#' all other variables (e.g. temperature in `"°C"`) use `"additive"` delta.
#'
#' @param selected_weather A data frame with columns `name` and `units`, as
#'   returned by `build_selected_weather()`.
#'
#' @return A named character vector with one entry per row in
#'   `selected_weather`, values being `"additive"` or `"multiplicative"`.
#'
#' @export
build_perturbation_method <- function(selected_weather) {
  method <- ifelse(
    selected_weather$units %in% c("mm", "days"),
    "multiplicative",
    "additive"
  )
  stats::setNames(method, selected_weather$name)
}


# ---------------------------------------------------------------------------- #
# Weather preparation for simulation                                           #
# ---------------------------------------------------------------------------- #

#' Prepare Historical Weather Data for Simulation
#'
#' Takes raw output from `get_weather()` and joins it back to the survey frame,
#' adding `sim_year` and ensuring `year` is a factor consistent with the
#' training data. Weather and outcome columns from the survey frame are dropped
#' before the join to avoid duplication.
#'
#' @param weather_raw A data frame returned by `get_weather()`, containing at
#'   least columns `code`, `year`, `survname`, `loc_id`, `int_month`, and
#'   `timestamp`.
#' @param survey_weather A data frame of the merged survey-weather training
#'   data. Must contain columns `code`, `year`, `survname`, `loc_id`, and
#'   `int_month`.
#' @param selected_weather A data frame of selected weather variable metadata
#'   with at least a `name` column.
#' @param outcome_name A character string giving the name of the outcome column
#'   in `survey_weather` to exclude before the join.
#'
#' @return A tibble with the weather variables from `weather_raw` joined to the
#'   survey covariate columns from `survey_weather`, with additional columns
#'   `sim_year` (integer) and `year` (factor).
#'
#' @importFrom dplyr mutate select inner_join any_of
#' @export
prepare_hist_weather <- function(weather_raw,
                                 survey_weather,
                                 selected_weather,
                                 outcome_name) {
  drop_cols <- c(selected_weather$name, outcome_name)

  weather_raw |>
    dplyr::mutate(
      year      = as.factor(year),
      int_month = as.integer(format(timestamp, "%m")),
      sim_year  = as.integer(format(timestamp, "%Y"))
    ) |>
    dplyr::select(-timestamp) |>
    dplyr::inner_join(
      survey_weather |> dplyr::select(-dplyr::any_of(drop_cols)),
      by = c("code", "year", "survname", "loc_id", "int_month")
    )
}


# ---------------------------------------------------------------------------- #
# Back-transformation                                                          #
# ---------------------------------------------------------------------------- #

#' Back-Transform a Log-Transformed Outcome Column
#'
#' Exponentiates the named outcome column in `preds` when `so$transform` is
#' `"log"`. Returns `preds` unchanged for any other transformation or when
#' `so$transform` is `NULL` / `NA`.
#'
#' @param preds A data frame of predictions from `predict_outcome()`.
#' @param so A one-row data frame of outcome metadata as returned by
#'   `build_selected_outcome()`. Must contain columns `name` and `transform`.
#'
#' @return `preds` with the outcome column exponentiated if applicable.
#'
#' @importFrom dplyr mutate
#' @importFrom rlang sym .data
#' @export
apply_log_backtransform <- function(preds, so) {
  if (!isTRUE(so$transform == "log")) return(preds)

  preds |>
    dplyr::mutate(!!rlang::sym(so$name) := exp(.data[[so$name]]))
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
hist_aggregate_choices <- function(outcome_type) {
  if (identical(outcome_type, "logical")) {
    c("Rate" = "mean")
  } else {
    c(
      "Mean"            = "mean",
      "Median"          = "median",
      "Headcount Ratio (FGT0)" = "headcount_ratio",
      "Outcome Gap (FGT1)"     = "gap",
      "Outcome Severity (FGT2)"     = "fgt2",
      "Gini coefficient"            = "gini"
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
#' @param weights An optional character string giving the name of a survey
#'   weight column in `df`. If `NULL` (default) all observations are
#'   weighted equally.
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
                              aggregate = c("mean", "sum", "median",
                                            "headcount_ratio", "gap", "fgt2", "gini"),
                              pov_line  = NULL,
                              weights   = NULL) {

  type      <- match.arg(type)
  # For binary outcomes the aggregate argument is ignored (always population
  # share). Skip match.arg so UI values like "binary" don't cause an error.
  if (type != "binary") {
    aggregate <- match.arg(aggregate)
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
        gini            = gini_coef(x, w)
      )
    }
  }

  df |>
    group_by(!!sym(group)) |>
    summarise(
      value = compute(
        x = .data[[outcome]],
        w = if (!is.null(weights)) .data[[weights]] else NULL
      ),
      .groups = "drop"
    )
}

#---------------------------------------------------------------------------- #
# Convert to deviation from centre for plotting
#---------------------------------------------------------------------------- #

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

#---------------------------------------------------------------------------- #
# Plot exceedance curve for one or more variables
#---------------------------------------------------------------------------- #

#' Plot Exceedance Probability Curve
#'
#' Creates an exceedance probability curve (1 - ECDF) for one or more variables,
#' with the probability axis on the y-axis and the variable axis on the x-axis
#' (via `coord_flip`). Useful for visualizing the probability that a
#' variable exceeds a given threshold. When multiple variables are provided,
#' each is drawn in a distinct color with a legend.
#'
#' @param df A data frame containing the variables to plot.
#' @param variables A character vector of column names in `df` to plot.
#'   Each variable will be drawn as a separate exceedance curve.
#' @param x_label A character string for the axis label of the plotted variable
#'   (displayed on the y-axis after `coord_flip`).
#' @param labels An optional character vector of labels corresponding to each
#'   entry in `variables`, used in the legend. Must be the same length as
#'   `variables`. If `NULL` (default), the column names are used.
#'
#' @return A `ggplot` object.
#'
#' @examples
#' library(ggplot2)
#'
#' # Simulate poverty rate changes across 500 scenarios
#' set.seed(42)
#' sim_data <- data.frame(
#'   pov300d_base   = rnorm(500, mean = 0.02, sd = 0.05),
#'   pov300d_policy = rnorm(500, mean = -0.01, sd = 0.04)
#' )
#'
#' # Single variable (no legend)
#' plot_exceedance(sim_data, "pov300d_base", "Change in $3.00 poverty rate (pp.)")
#'
#' # Multiple variables with custom labels
#' plot_exceedance(
#'   sim_data,
#'   variables = c("pov300d_base", "pov300d_policy"),
#'   x_label   = "Change in $3.00 poverty rate (pp.)",
#'   labels    = c("Baseline", "Policy")
#' )
#'
#' @importFrom ggplot2 ggplot aes stat_ecdf geom_vline labs theme_minimal
#'   coord_flip after_stat scale_color_brewer
#' @importFrom dplyr bind_rows mutate
#' @export
plot_exceedance <- function(df, variables, x_label, labels = NULL) {

  if (is.null(labels)) labels <- variables
  stopifnot(length(labels) == length(variables))

  # Reshape to long format so ggplot can map color to group
  long_df <- do.call(bind_rows, lapply(seq_along(variables), function(i) {
    data.frame(
      value = df[[variables[i]]],
      group = labels[i]
    )
  }))

  # Preserve label order in the legend
  long_df$group <- factor(long_df$group, levels = labels)

  ggplot(long_df, aes(x = value, color = group)) +
    stat_ecdf(geom = "point", aes(y = 1 - after_stat(y)), alpha = 0.5) +
    stat_ecdf(geom = "line",  aes(y = 1 - after_stat(y)), linewidth = 1) +
    geom_vline(xintercept = 0, color = "black", linetype = "dotted") +
    labs(
      x     = x_label,
      y     = "Annual Exceedance Probability (P(X > x))",
      color = NULL
    ) +
    scale_color_brewer(palette = "Set1") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    coord_flip()
}



# ---------------------------------------------------------------------------- #
# Shared aggregation helper                                                    #
# ---------------------------------------------------------------------------- #

#' Aggregate Simulation Predictions for Exceedance Plotting
#'
#' Helper shared by `plot_hist_sim()` and `plot_fut_sim()`. Aggregates
#' individual-level predictions by `sim_year`, optionally applies
#' `deviation_from_centre()`, and returns the aggregated data frame together
#' with a resolved x-axis label.
#'
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
#'
#' @return A list with elements `out` (aggregated data frame with `sim_year`
#'   and `value` columns) and `x_label` (character).
#'
#' @importFrom rlang abort
#' @export
aggregate_sim_preds <- function(preds, so, agg_method, deviation, loss_frame,
                                pov_line = NULL) {

  if (agg_method %in% c("headcount_ratio", "gap", "fgt2") && is.null(pov_line)) {
    rlang::abort(
      "`pov_line` must be supplied when `agg_method` is 'headcount_ratio','gap', or 'fgt2'."
    )
  }

  out <- aggregate_outcome(
    df        = preds,
    outcome   = so$name,
    group     = "sim_year",
    type      = if (identical(so$type, "logical")) "binary" else "continuous",
    aggregate = agg_method,
    pov_line  = pov_line
  )

  if (!identical(deviation, "none")) {
    out <- deviation_from_centre(
      out,
      centre = deviation,
      loss   = isTRUE(loss_frame)
    )
    if (!identical(so$type, "logical")) {
      x_label <- paste0(
        "Change in ", so$label,
        " relative to typical year (", deviation, ")"
      )
    } else {
      x_label <- paste0(
        "Change in ", so$label,
        " relative to typical year (pp.)"
      )
    }
  } else {
    if (!identical(so$type, "logical")) {
      x_label <- paste0(
        "Simulated ", so$label,
        " [", agg_method, "]"
      )
    } else {
      x_label <- paste0(
        "Simulated ", so$label,
        " rate (pp.)"
      )
    }
  }

  list(out = out, x_label = x_label)
}


# ---------------------------------------------------------------------------- #
# Plot builders                                                                #
# ---------------------------------------------------------------------------- #

#' Build the Historical Simulation Exceedance Plot
#'
#' Pure function that aggregates individual-level predictions and returns a
#' `plot_exceedance()` ggplot object. When `fut_preds` is also supplied the
#' future climate series is overlaid as a second curve labelled
#' `"Future climate"`.
#'
#' @param preds      A data frame of individual-level predictions from
#'   `predict_outcome()`, containing at least a `sim_year` column and the
#'   outcome column named by `so$name`.
#' @param so         A one-row data frame of outcome metadata (must contain
#'   `name`, `label`, and `type`).
#' @param agg_method Character. One of `"mean"`, `"median"`,
#'   `"headcount_ratio"`, `"gap"`, or `"gini"`.
#' @param deviation  Character. One of `"none"`, `"mean"`, or `"median"`.
#' @param loss_frame Logical. Passed to `deviation_from_centre()` as `loss`.
#' @param pov_line   Numeric or `NULL`. Required when `agg_method` is
#'   `"headcount_ratio"` or `"gap"`.
#' @param fut_preds  Optional data frame of future-climate individual-level
#'   predictions from `predict_outcome()`. When supplied a second exceedance
#'   curve labelled `"Future climate"` is overlaid on the same plot.
#'
#' @return A `ggplot` object.
#'
#' @importFrom rlang abort
#' @export
plot_hist_sim <- function(preds, so, agg_method, deviation, loss_frame,
                          pov_line = NULL, fut_preds = NULL) {

  agg <- aggregate_sim_preds(preds, so, agg_method, deviation, loss_frame,
                             pov_line)

  if (!is.null(fut_preds)) {
    agg_fut <- aggregate_sim_preds(fut_preds, so, agg_method, deviation,
                                   loss_frame, pov_line)

    plot_df <- data.frame(
      historical = agg$out$value,
      future     = agg_fut$out$value
    )

    plot_exceedance(
      df        = plot_df,
      variables = c("historical", "future"),
      x_label   = agg$x_label,
      labels    = c("Historical weather", "Future climate")
    )
  } else {
    plot_exceedance(
      df        = agg$out,
      variables = "value",
      x_label   = agg$x_label,
      labels    = "Historical weather"
    )
  }
}


