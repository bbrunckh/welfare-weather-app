# ============================================================================ #
# Pure functions used by mod_2_01.
# Stateless and testable without Shiny.
#
# Shared constants (used across multiple modules):
#   RP_LOW            -- low-tail return period map  (name -> quantile prob)
#   RP_HIGH           -- high-tail return period map (name -> quantile prob)
#   SSP_SHORT_LABELS  -- canonical SSP key -> short display label
#
# Shared UI helpers (called inside module server functions):
#   residual_method_ui(ns, input_id)  -- radio buttons + helpText for residuals
#
# Simulation pipeline helper:
#   run_sim_pipeline()  -- weather join -> predict -> back-transform in one call

# ============================================================================ #

# ---- Internal colour / style helpers ---------------------------------------
# Used by enhance_exceedance() and plot_pointrange_climate(). Not exported.

# Canonical SSP keys must match what .normalise_ssp() returns
.ssp_colours <- c(
  "SSP2-4.5" = "#4dac26",   # green  (lower emissions)
  "SSP3-7.0" = "#2166ac",   # blue   (mid emissions)
  "SSP5-8.5" = "#c0392b"    # red    (high emissions)
)



# Normalise any SSP token found in a scenario name to a canonical key
# Strips trailing " / P{pct}" suffix first so keys like "SSP3-7.0 / 2030 / P50"
# are handled identically to the old "SSP3-7.0 / 2030" format.
# e.g. "SSP2" -> "SSP2-4.5", "SSP3-7.0" -> "SSP3-7.0", NA -> NA
.normalise_ssp <- function(nm) {
  nm_clean <- sub(" / P[0-9]+$", "", nm)
  m_full <- regmatches(nm_clean, regexpr("SSP[0-9]-[0-9.]+", nm_clean))
  if (length(m_full) > 0) return(m_full)
  m_short <- regmatches(nm_clean, regexpr("SSP([2-9])", nm_clean))
  if (length(m_short) == 0) return(NA_character_)
  digit  <- sub("SSP", "", m_short)
  lookup <- c("2" = "SSP2-4.5", "3" = "SSP3-7.0", "5" = "SSP5-8.5")
  if (digit %in% names(lookup)) lookup[[digit]] else NA_character_
}

.parse_year <- function(nm) {
  if (length(nm) == 1L) {
    m <- regexpr("\\d{4}-\\d{4}", nm)
    if (m == -1L) return(NA_character_)
    return(regmatches(nm, m))
  }
  # Vectorised path
  m <- gregexpr("\\d{4}-\\d{4}", nm)
  vapply(seq_along(nm), function(i) {
    matches <- regmatches(nm[i], m[i])[[1]]
    if (length(matches) == 0) NA_character_ else matches[1]
  }, character(1))
}

# ---------------------------------------------------------------------------- #
# Shared constants                                                             #
# ---------------------------------------------------------------------------- #

#' Symmetric Return-Period Probability Maps
#'
#' Named numeric vectors mapping return-period labels to exceedance
#' probabilities. Used consistently across the threshold table and exceedance
#' curve in mod_2_06 and fct_sim_compare.
#'
#' \describe{
#'   \item{RP_LOW}{Rare low-outcome tail: 1:50, 1:20, 1:10, 1:5}
#'   \item{RP_HIGH}{Rare high-outcome tail: 4:5, 9:10, 19:20, 49:50}
#' }
#' @export
RP_LOW  <- c("1:50" = 0.02, "1:20" = 0.05, "1:10" = 0.10, "1:5" = 0.20)

#' @rdname RP_LOW
#' @export
RP_HIGH <- c("4:5" = 0.80, "9:10" = 0.90, "19:20" = 0.95, "49:50" = 0.98)

#' Short Display Labels for SSP Scenarios
#'
#' Maps canonical SSP keys (as returned by `.normalise_ssp()`) to short labels
#' used in UI checkboxes and plot legends.
#'
#' @export
SSP_SHORT_LABELS <- c(
  "SSP2-4.5" = "SSP2",
  "SSP3-7.0" = "SSP3",
  "SSP5-8.5" = "SSP5"
)

# ---------------------------------------------------------------------------- #
# Shared UI helper                                                             #
# ---------------------------------------------------------------------------- #

#' Residual Method Radio Buttons UI
#'
#' Produces a consistent `radioButtons` widget + `helpText` block for choosing
#' how prediction residuals are handled. Used in both `mod_2_01_historical` and
#' `mod_2_03_future` to avoid duplicating the same 24-line block.
#'
#' @param ns       The module namespace function (from `session$ns`).
#' @param input_id The input id for the radio buttons (unnamespaced).
#'
#' @return A `tagList` containing `radioButtons` and `helpText`.
#' @export
residual_method_ui <- function(ns, input_id) {
  shiny::tagList(
    shiny::radioButtons(
      inputId  = ns(input_id),
      label    = "Residuals method",
      choices  = residual_choices(),
      selected = "original"
    ),
    shiny::helpText(
      shiny::tags$b("none:"), " return fitted values only.", shiny::tags$br(),
      shiny::tags$b("original:"), " match each observation's own training residual",
      " by ID, preserving individual-level heterogeneity across simulation years.",
      shiny::tags$br(),
      shiny::tags$b("empirical:"), " resample residuals from the training",
      " distribution (non-parametric bootstrap).", shiny::tags$br(),
      shiny::tags$b("normal:"), " draw residuals from N(0, \u03c3) where \u03c3",
      " is the training residual SD.",
      style = "font-size:11px;"
    )
  )
}

# ---------------------------------------------------------------------------- #
# Simulation pipeline helper                                                   #
# ---------------------------------------------------------------------------- #

#' Resolve the ID Column for Residual Matching
#'
#' Returns the first of `c("pid", "hhid", "fid")` that exists in both data
#' frames, or `NULL` if none match.  Used by `run_sim_pipeline()` to enable
#' ID-based residual matching when `residuals = "original"`.
#' @noRd
resolve_id_col <- function(a, b) {
  candidates <- c("pid", "hhid", "fid")
  shared     <- intersect(names(a), names(b))
  match      <- candidates[candidates %in% shared]
  if (length(match) == 0L) NULL else match[[1L]]
}

#' Run the Full Simulation Pipeline for One Scenario Row
#'
#' Combines the three steps that are identical between historical and future
#' simulation:
#' \enumerate{
#'   \item Join raw weather output back to survey covariates via
#'     `prepare_hist_weather()`.
#'   \item Generate outcome predictions via `predict_outcome()`.
#'   \item Back-transform log-scale outcomes via `apply_log_backtransform()`.
#' }
#'
#' @param weather_raw  Data frame returned by `get_weather()$result`.
#' @param svy          Survey-weather data frame (from `survey_weather()`).
#' @param sw           Selected weather metadata (from `selected_weather()`).
#' @param so           Selected outcome metadata (from `selected_outcome()`).
#' @param model        Fitted model object (`mf$fit3`).
#' @param residuals    Character. Residual method, e.g. `"original"`.
#' @param train_data   Data frame used at fit time (`mf$train_data`).
#' @param engine       Character. Model engine, e.g. `"fixest"`.
#' @param slim         Logical. When `TRUE`, trim `preds` to only the columns
#'   required by downstream aggregation and diagnostics (`sim_year`, outcome,
#'   `.fitted`, `.residual`, and any `weight` column) and trim `weather_raw`
#'   to location/time keys and weather variable columns.  Dramatically reduces
#'   memory when processing many ensemble models.  Default `FALSE`.
#'
#' @return A named list with three elements:
#'   \describe{
#'     \item{preds}{Data frame of individual-level predictions with the outcome
#'       column back-transformed where applicable, or \code{NULL} on failure.
#'       When \code{slim = TRUE}, only essential columns are retained.}
#'     \item{n_pre_join}{Integer. Number of rows in \code{svy} before the
#'       weather join. Used by the Diagnostics tab to compute the drop rate.}
#'     \item{weather_raw}{The raw weather data frame passed in as
#'       \code{weather_raw}. Stored so the Diagnostics tab can construct the
#'       full historical weather distribution without a second DB query.
#'       When \code{slim = TRUE}, only key + weather columns are kept.}
#'   }
#'
#' @export
run_sim_pipeline <- function(weather_raw, svy, sw, so,
                             model, residuals, train_data, engine,
                             slim = FALSE) {
  n_pre_join    <- nrow(svy)
  survey_wd_sim <- prepare_hist_weather(weather_raw, svy, sw, so$name)
  id_col        <- if (residuals == "original") resolve_id_col(train_data, survey_wd_sim) else NULL

  preds <- tryCatch(
    predict_outcome(
      model      = model,
      newdata    = survey_wd_sim,
      residuals  = residuals,
      outcome    = so$name,
      id         = id_col,
      train_data = train_data,
      engine     = engine
    ),
    error = function(e) {
      warning("[run_sim_pipeline] predict_outcome() failed: ", conditionMessage(e))
      NULL
    }
  )
  rm(survey_wd_sim)

  if (is.null(preds)) return(NULL)
  preds <- apply_log_backtransform(preds, so)

  if (slim) {
    # Keep only columns needed by aggregate_sim_preds() and
    # build_ridge_kde_data(): sim_year, year, the outcome column,
    # .fitted, .residual, and any weight column for aggregation.
    keep <- c("sim_year", "year", so$name, ".fitted", ".residual")
    if ("weight" %in% names(preds)) keep <- c(keep, "weight")
    preds <- preds[, intersect(keep, names(preds)), drop = FALSE]

    # Keep only key + weather columns from weather_raw for diagnostics.
    weather_keep <- c("code", "year", "survname", "loc_id", "timestamp", sw$name)
    weather_raw  <- weather_raw[, intersect(weather_keep, names(weather_raw)), drop = FALSE]
  }

  list(
    preds       = preds,
    n_pre_join  = n_pre_join,
    weather_raw = weather_raw
  )
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
    "Original" = "original",
    "Resample"  = "resample",
    "Normal"     = "normal",
    "None"      = "none"
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
  grp_cols <- if ("model" %in% names(preds)) c("model", "sim_year") else "sim_year"

  out <- aggregate_outcome(
    df        = preds,
    outcome   = so$name,
    group     = grp_cols,
    type      = if (identical(so$type, "logical")) "binary" else "continuous",
    aggregate = agg_method,
    pov_line  = pov_line,
    weights  = if (!is.null(weights) && weights %in% names(preds)) weights else NULL
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

