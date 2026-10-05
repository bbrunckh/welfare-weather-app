# Metric metadata ----

.WISE_METRIC_REGISTRY <- list(
  mean = list(label = "Mean", native_unit = "outcome units", format = "number", direction = "higher_is_better", change_kind = "absolute", percent_change = TRUE, poverty_line = FALSE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Weighted annual aggregate for the fixed survey population."),
  median = list(label = "Median", native_unit = "outcome units", format = "number", direction = "higher_is_better", change_kind = "absolute", percent_change = TRUE, poverty_line = FALSE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Weighted median aggregate; not a household distribution."),
  total = list(label = "Population weighted sum", native_unit = "outcome units", format = "number", direction = "higher_is_better", change_kind = "absolute", percent_change = TRUE, poverty_line = FALSE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Sum of the selected outcome using survey weights when available."),
  headcount_ratio = list(label = "Poverty rate", native_unit = "fraction", format = "percent", direction = "lower_is_better", change_kind = "percentage_points", poverty_line = TRUE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Share below the selected poverty line; changes are percentage points."),
  gap = list(label = "Poverty gap", native_unit = "fraction", format = "percent", direction = "lower_is_better", change_kind = "percentage_points", poverty_line = TRUE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Average normalized shortfall below the selected poverty line."),
  fgt2 = list(label = "Poverty severity", native_unit = "fraction", format = "percent", direction = "lower_is_better", change_kind = "percentage_points", poverty_line = TRUE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Squared normalized poverty shortfall."),
  gini = list(label = "Gini coefficient", native_unit = "native Gini index", format = "number", direction = "lower_is_better", change_kind = "index_points", poverty_line = FALSE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Relative inequality index on its native scale."),
  prosperity_gap = list(label = "Prosperity gap", native_unit = "ratio", format = "number", direction = "lower_is_better", change_kind = "absolute", poverty_line = TRUE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Gap relative to the fixed threshold of 28; the selected poverty line is not used."),
  avg_poverty = list(label = "Average poverty", native_unit = "inverse outcome units", format = "number", direction = "lower_is_better", change_kind = "absolute", poverty_line = FALSE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Mean inverse welfare over eligible positive-welfare rows only.")
)

.metric_context_value <- function(so, field) {
  if (is.null(so)) return(NULL)
  if (is.data.frame(so) && field %in% names(so)) return(so[[field]][1L])
  if (is.list(so)) return(so[[field]] %||% NULL)
  NULL
}

.metric_scalar_text <- function(x) {
  if (is.null(x) || !length(x) || is.na(x[1L])) return(NULL)
  value <- trimws(as.character(x[1L]))
  if (!nzchar(value)) NULL else value
}

#' Return the metadata contract for a displayed metric.
#'
#' Direction comes from selected outcome metadata whenever available. This
#' prevents plot code from inferring whether a larger value is adverse.
#' @noRd
metric_metadata <- function(method = "mean", so = NULL, pov_line = NULL,
                            analysis_unit = NULL, weighted = NULL) {
  method <- as.character(method %||% "mean")[1]
  fallback_labels <- c(
    mean = "Mean", median = "Median", total = "Population weighted sum",
    headcount_ratio = "Poverty rate", gap = "Poverty gap",
    fgt2 = "Poverty severity", gini = "Gini coefficient",
    prosperity_gap = "Prosperity gap", avg_poverty = "Average poverty"
  )
  out <- .WISE_METRIC_REGISTRY[[method]] %||% list(
    label = unname(fallback_labels[[method]] %||% method),
    native_unit = "outcome units", format = "number",
    change_kind = "absolute"
  )
  direction <- out$direction %||% NULL
  outcome_type <- tolower(.metric_scalar_text(
    .metric_context_value(so, "type")
  ) %||% "")
  binary_mean <- identical(method, "mean") &&
    outcome_type %in% c("logical", "binary", "boolean")
  # Built-in aggregation methods have an explicit direction contract. Do not
  # let stale outcome metadata from an earlier selection reverse their tail.
  if (!is.null(so) && !method %in% names(.WISE_METRIC_REGISTRY)) {
    so_direction <- .metric_context_value(so, "direction")
    if (!is.null(so_direction) && nzchar(as.character(so_direction))) {
      direction <- as.character(so_direction)[1]
    }
  }
  if (binary_mean) {
    explicit_direction <- .metric_scalar_text(.metric_context_value(so, "direction"))
    direction <- if (!is.null(explicit_direction) && explicit_direction %in%
      c("higher_is_better", "lower_is_better")) explicit_direction else "unknown"
  } else if (is.null(direction) || !direction %in% c("higher_is_better", "lower_is_better")) {
    direction <- "higher_is_better"
  }
  out$method <- method
  if (binary_mean) {
    out$format <- "percent"
    out$native_unit <- "probability fraction"
    out$change_kind <- "percentage_points"
  }
  outcome_name <- .metric_scalar_text(.metric_context_value(so, "name"))
  outcome_label <- .metric_scalar_text(.metric_context_value(so, "label"))
  selected_units <- toupper(.metric_scalar_text(
    .metric_context_value(so, "units")
  ) %||% "")
  currency_basis <- switch(selected_units,
    PPP = "PPP 2021",
    LCU = "LCU 2021",
    NULL
  )
  display_currency_unit <- switch(selected_units,
    PPP = "$ per day",
    LCU = "LCU per day",
    NULL
  )
  time_basis <- .metric_scalar_text(.metric_context_value(so, "time_basis"))
  welfare_denominator <- .metric_scalar_text(
    .metric_context_value(so, "welfare_denominator")
  )
  native_unit <- out$native_unit %||% out$unit %||% "outcome units"
  level_unit <- native_unit
  input_unit <- display_currency_unit %||% currency_basis %||%
    .metric_scalar_text(.metric_context_value(so, "units")) %||% "outcome units"
  if (method %in% c("mean", "median") && !binary_mean) {
    level_unit <- input_unit
  } else if (identical(method, "total")) {
    level_unit <- input_unit
  } else if (identical(method, "prosperity_gap")) {
    level_unit <- ""
  } else if (identical(method, "avg_poverty")) {
    level_unit <- switch(selected_units,
      PPP = "days per $",
      LCU = "days per LCU",
      paste("inverse", .metric_scalar_text(.metric_context_value(so, "units")) %||% "outcome units")
    )
  }
  if (identical(out$format, "percent")) {
    level_unit <- "percent"
  }
  uses_poverty_line <- isTRUE(out$poverty_line) &&
    !identical(method, "prosperity_gap")
  missing_context <- character(0)
  threshold_value <- NULL
  threshold_kind <- "none"
  threshold_unit <- NULL
  if (identical(method, "prosperity_gap")) {
    threshold_value <- 28
    threshold_kind <- "fixed prosperity threshold"
    threshold_unit <- if (!is.null(time_basis) && !is.null(welfare_denominator)) {
      paste(currency_basis %||% "currency basis unknown", "per",
            welfare_denominator, "per", time_basis)
    } else {
      "currency/time applicability unknown"
    }
  } else if (uses_poverty_line) {
    threshold_value <- suppressWarnings(as.numeric(pov_line)[1L])
    if (!length(threshold_value) || !is.finite(threshold_value)) {
      threshold_value <- suppressWarnings(as.numeric(
        .metric_context_value(so, "povline")
      )[1L])
    }
    if (!length(threshold_value) || !is.finite(threshold_value)) {
      threshold_value <- NA_real_
      missing_context <- c(missing_context, "selected poverty-line value unavailable")
    }
    threshold_kind <- "selected poverty line"
    threshold_unit <- input_unit
  }
  if (method %in% c("mean", "median") && !binary_mean &&
      is.null(display_currency_unit) &&
      !is.null(time_basis) && nzchar(time_basis) && !is.null(welfare_denominator)) {
    level_unit <- paste(level_unit, "per", welfare_denominator, "per", time_basis)
  }
  change_unit <- switch(out$change_kind,
    percentage_points = "pp",
    index_points = "index points",
    level_unit
  )
  if (identical(method, "total")) change_unit <- level_unit
  if (!binary_mean && method %in% c("mean", "median", "total", "avg_poverty") &&
      identical(selected_units %||% "", "")) {
    missing_context <- c(missing_context, "currency/unit basis unavailable")
  }
  if (!binary_mean && method %in% c("mean", "median", "total", "avg_poverty") &&
      (is.null(time_basis) || is.null(welfare_denominator))) {
    missing_context <- c(missing_context, "welfare/time basis unavailable")
  }
  if (identical(method, "prosperity_gap") && is.null(currency_basis)) {
    missing_context <- c(missing_context, "threshold currency applicability unavailable")
  }
  if (identical(method, "prosperity_gap") &&
      (identical(selected_units, "LCU") || is.null(time_basis) || is.null(welfare_denominator))) {
    missing_context <- c(missing_context, "fixed 28 threshold currency/welfare/time applicability unconfirmed; no currency conversion applied")
  }
  if (uses_poverty_line &&
      (is.null(time_basis) || is.null(welfare_denominator))) {
    missing_context <- c(missing_context, "poverty-line welfare/time compatibility unavailable")
  }
  if (identical(method, "avg_poverty")) {
    missing_context <- c(missing_context, "eligibility restricted to positive welfare")
  }
  analysis_unit <- .metric_scalar_text(analysis_unit)
  if (is.null(weighted)) {
    weight_interpretation <- "survey-weight interpretation unknown"
  } else if (isTRUE(weighted)) {
    weight_interpretation <- "survey weights applied; expansion semantics unknown"
  } else {
    weight_interpretation <- "unweighted"
  }
  out$label <- out$label %||% unname(fallback_labels[[method]] %||% method)
  if (identical(method, "total") && identical(weighted, FALSE)) {
    out$label <- "Population sum"
  }
  out$direction <- direction
  out$direction_known <- !identical(direction, "unknown")
  out$adverse_tail <- if (identical(direction, "lower_is_better")) "high" else "low"
  out$adverse_note <- if (!out$direction_known) {
    "Direction unknown; low tail retained as legacy default"
  } else if (identical(out$adverse_tail, "high")) {
    "Higher values are adverse"
  } else {
    "Lower values are adverse"
  }
  out$valid_transform <- if (identical(out$change_kind, "percentage_points")) {
    "percentage points"
  } else if (isTRUE(out$percent_change)) {
    "percent change for log effects"
  } else {
    "native absolute units"
  }
  out$native_unit <- if (method %in% c("mean", "median") && !binary_mean) input_unit else native_unit
  out$unit <- if (identical(out$format, "percent")) "percent" else level_unit
  out$level_unit <- level_unit
  out$change_unit <- change_unit
  out$display_multiplier <- if (identical(out$format, "percent")) 100 else 1
  out$change_kind <- out$change_kind %||% "absolute"
  out$currency_basis <- currency_basis
  out$time_basis <- time_basis
  out$welfare_denominator <- welfare_denominator
  out$analysis_unit <- analysis_unit
  out$weight_interpretation <- weight_interpretation
  out$weighted <- weighted
  out$outcome_name <- outcome_name
  out$outcome_label <- outcome_label
  out$outcome_type <- if (nzchar(outcome_type)) outcome_type else NULL
  out$uses_poverty_line <- uses_poverty_line
  out$threshold_kind <- threshold_kind
  out$threshold_value <- threshold_value
  out$threshold_unit <- threshold_unit
  out$missing_context <- unique(missing_context)
  out$poverty_line <- isTRUE(out$poverty_line)
  out$engines <- out$engines %||% c("ols", "rif")
  out$uncertainty <- out$uncertainty %||% c("coefficient", "weather", "ensemble")
  out$caveat <- out$caveat %||% "Metric definition supplied by the selected outcome metadata."
  out
}

#' Format a native metric value for display
#' @noRd
format_metric_value <- function(value, metadata, change = FALSE, digits = 2) {
  value <- suppressWarnings(as.numeric(value))[1L]
  if (!length(value) || !is.finite(value)) return("Not available")
  digits <- max(0L, as.integer(digits[1L] %||% 2L))
  multiplier <- suppressWarnings(as.numeric(metadata$display_multiplier))[1L]
  if (!length(multiplier) || !is.finite(multiplier)) multiplier <- 1
  shown <- value * multiplier
  number <- formatC(
    shown, format = "f", digits = digits, big.mark = ",", decimal.mark = "."
  )
  if (isTRUE(change) && shown >= 0) number <- paste0("+", number)
  if (identical(metadata$format, "percent")) {
    return(paste0(number, if (isTRUE(change)) " pp" else "%"))
  }
  unit <- if (isTRUE(change)) metadata$change_unit else metadata$level_unit
  if (is.null(unit) || !nzchar(unit)) return(number)
  paste(number, unit)
}

#' Build a concise interpretation note for a metric
#' @noRd
metric_context_note <- function(metadata) {
  outcome <- metadata$outcome_label %||% metadata$outcome_name
  metric <- metadata$label %||% metadata$method %||% "Selected metric"
  title <- if (!is.null(outcome) && nzchar(outcome)) {
    paste0(outcome, ": ", metric)
  } else {
    metric
  }
  pieces <- title
  if (!is.null(metadata$threshold_value) && length(metadata$threshold_value) &&
      isTRUE(is.finite(metadata$threshold_value))) {
    threshold_label <- if (identical(metadata$method, "prosperity_gap")) {
      "Fixed prosperity threshold"
    } else {
      "Poverty line"
    }
    pieces <- c(pieces, paste0(
      threshold_label, " ", format(metadata$threshold_value, trim = TRUE),
      " (", metadata$threshold_unit %||% "unit basis unknown", ")"
    ))
  }
  if (length(metadata$missing_context)) {
    pieces <- c(pieces, paste(metadata$missing_context, collapse = "; "))
  }
  if (!is.null(metadata$analysis_unit)) {
    pieces <- c(pieces, paste("Analysis unit:", metadata$analysis_unit))
  }
  if (!is.null(metadata$weight_interpretation)) {
    pieces <- c(pieces, metadata$weight_interpretation)
  }
  if (identical(metadata$direction, "unknown")) {
    pieces <- c(pieces, "Outcome direction unknown; interpret signed changes without a benefit claim")
  }
  paste(pieces, collapse = ". ")
}

metric_axis_label <- function(method = "mean", so = NULL, deviation = "none") {
  spec <- metric_metadata(method, so)
  label <- if (identical(deviation, "none")) {
    spec$label
  } else {
    paste0(spec$label, " - ", label_deviation(deviation))
  }
  unit <- if (!identical(deviation, "none")) spec$change_unit else spec$level_unit
  if (identical(deviation, "none") && method == "prosperity_gap") {
    return(label)
  }
  if (is.null(unit) || !nzchar(unit)) return(label)
  if (identical(deviation, "none") &&
      (method %in% c("headcount_ratio", "gap", "fgt2") ||
       identical(method, "gini"))) {
    return(label)
  }
  paste0(label, " (", unit, ")")
}

metric_decision_return_periods <- function(method = "mean", so = NULL) {
  spec <- metric_metadata(method, so)
  tail_names <- if (identical(spec$adverse_tail, "high")) {
    c(
      "Adverse 1-in-5" = "1:5", "Adverse 1-in-10" = "1:10",
      "Adverse 1-in-20" = "1:20", "Adverse 1-in-50" = "1:50"
    )
  } else {
    c(
      "Adverse 1-in-5" = "1:5", "Adverse 1-in-10" = "1:10",
      "Adverse 1-in-20" = "1:20", "Adverse 1-in-50" = "1:50"
    )
  }
  c("Expected" = "1:1", tail_names)
}

# Keep adverse comparison rows only when the historical baseline has enough
# finite annual aggregates to support their requested return period.
filter_historically_supported_return_periods <- function(central, rp_map,
                                                         historical_scenario = "Historical",
                                                         source_col = NULL) {
  if (is.null(central) || !nrow(central) || !"rp_label" %in% names(central)) {
    return(central)
  }
  hist <- central$scenario == historical_scenario
  if (!is.null(source_col) && source_col %in% names(central)) {
    hist <- hist & central[[source_col]] == "Baseline"
  }
  expected_id <- unname(rp_map[["Expected"]])
  hist <- hist & central$rp_name == expected_id
  n_years <- if ("n_obs" %in% names(central) && any(hist)) {
    suppressWarnings(max(as.numeric(central$n_obs[hist]), na.rm = TRUE))
  } else NA_real_
  supported <- names(rp_map)[names(rp_map) == "Expected"]
  if (is.finite(n_years)) {
    supported <- c(supported, names(rp_map)[vapply(names(rp_map), function(label) {
      if (identical(label, "Expected")) return(FALSE)
      period <- suppressWarnings(as.numeric(sub("^Adverse 1-in-", "", label)))
      is.finite(period) && period >= 1 && n_years >= ceiling(period)
    }, logical(1))])
  } else {
    supported <- character()
  }
  central[central$rp_label %in% supported, , drop = FALSE]
}

# Add the definitions needed to interpret a chart-data export. Keeping these
# fields beside the values makes a CSV useful outside the live Shiny session.
visualization_export_metadata <- function(method = "mean", so = NULL,
                                           observation_unit,
                                           aggregation_order,
                                           uncertainty = "none",
                                           pov_line = NULL,
                                           analysis_unit = NULL,
                                           weighted = NULL,
                                           context = NULL) {
  spec <- if (is.list(context)) context else metric_metadata(method, so, pov_line, analysis_unit, weighted)
  context_note <- metric_context_note(spec)
  if (!is.null(context) && !is.list(context) && nzchar(as.character(context)[1L])) {
    context_note <- paste(context_note, as.character(context)[1L], sep = ". ")
  }
  data.frame(
    metric_id = spec$method,
    metric_label = spec$label,
    unit = spec$unit,
    native_unit = spec$native_unit,
    level_unit = spec$level_unit,
    change_unit = spec$change_unit,
    display_multiplier = spec$display_multiplier,
    number_format = spec$format,
    direction = spec$direction,
    direction_known = spec$direction_known,
    adverse_tail = spec$adverse_tail,
    valid_transform = spec$valid_transform,
    poverty_line = isTRUE(spec$poverty_line),
    uses_poverty_line = spec$uses_poverty_line,
    threshold_kind = spec$threshold_kind,
    threshold_value = if (length(spec$threshold_value)) spec$threshold_value else NA_real_,
    threshold_unit = spec$threshold_unit %||% NA_character_,
    currency_basis = spec$currency_basis %||% NA_character_,
    time_basis = spec$time_basis %||% NA_character_,
    welfare_denominator = spec$welfare_denominator %||% NA_character_,
    outcome_name = spec$outcome_name %||% NA_character_,
    outcome_label = spec$outcome_label %||% NA_character_,
    outcome_type = spec$outcome_type %||% NA_character_,
    outcome_transform = .metric_context_value(so, "transform") %||% NA_character_,
    missing_context_note = paste(spec$missing_context, collapse = "; "),
    analysis_unit = spec$analysis_unit %||% NA_character_,
    weight_interpretation = spec$weight_interpretation,
    metric_context = context_note,
    supported_engines = paste(spec$engines, collapse = ", "),
    supported_uncertainty = paste(spec$uncertainty, collapse = ", "),
    caveat = spec$caveat,
    observation_unit = observation_unit,
    aggregation_order = aggregation_order,
    uncertainty_type = uncertainty,
    stringsAsFactors = FALSE
  )
}

annotate_visualization_export <- function(data, method = "mean", so = NULL,
                                          observation_unit,
                                          aggregation_order,
                                          uncertainty = "none",
                                          pov_line = NULL,
                                          analysis_unit = NULL,
                                          weighted = NULL,
                                          context = NULL) {
  if (is.null(data) || !is.data.frame(data)) {
    return(data)
  }
  meta <- visualization_export_metadata(
    method, so, observation_unit, aggregation_order, uncertainty,
    pov_line, analysis_unit, weighted, context
  )
  for (nm in names(meta)) data[[nm]] <- rep(meta[[nm]][[1L]], nrow(data))
  data
}

log_effect_to_percent <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  100 * (exp(x) - 1)
}

percent_to_log_effect <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  log1p(x / 100)
}
