# Metric metadata ----

.WISE_METRIC_REGISTRY <- list(
  mean = list(label = "Mean", unit = "outcome units", format = "number", direction = "higher_is_better", percent_change = TRUE, poverty_line = FALSE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Weighted annual aggregate for the fixed survey population."),
  median = list(label = "Median", unit = "outcome units", format = "number", direction = "higher_is_better", percent_change = TRUE, poverty_line = FALSE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Weighted median aggregate; not a household distribution."),
  total = list(label = "Total", unit = "outcome units", format = "number", direction = "higher_is_better", percent_change = TRUE, poverty_line = FALSE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Population aggregate; survey population is fixed."),
  headcount_ratio = list(label = "Poverty rate", unit = "percent", format = "percent", direction = "lower_is_better", percent_change = FALSE, poverty_line = TRUE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Share below the selected poverty line; changes are percentage points."),
  gap = list(label = "Poverty gap", unit = "percent", format = "percent", direction = "lower_is_better", percent_change = FALSE, poverty_line = TRUE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Average normalized shortfall below the selected poverty line."),
  fgt2 = list(label = "Poverty severity", unit = "percent", format = "percent", direction = "lower_is_better", percent_change = FALSE, poverty_line = TRUE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Squared normalized poverty shortfall."),
  gini = list(label = "Gini coefficient", unit = "index", format = "number", direction = "lower_is_better", percent_change = FALSE, poverty_line = FALSE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Relative inequality index."),
  prosperity_gap = list(label = "Prosperity gap", unit = "ratio", format = "number", direction = "lower_is_better", percent_change = FALSE, poverty_line = TRUE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Gap relative to the prosperity threshold."),
  avg_poverty = list(label = "Average poverty", unit = "days per dollar", format = "number", direction = "lower_is_better", percent_change = FALSE, poverty_line = FALSE, engines = c("ols", "rif"), uncertainty = c("coefficient", "weather", "ensemble"), caveat = "Average poverty burden across the fixed population.")
)

#' Return the metadata contract for a displayed metric.
#'
#' Direction comes from selected outcome metadata whenever available. This
#' prevents plot code from inferring whether a larger value is adverse.
#' @noRd
metric_metadata <- function(method = "mean", so = NULL) {
  method <- as.character(method %||% "mean")[1]
  fallback_labels <- c(
    mean = "Mean", median = "Median", total = "Total",
    headcount_ratio = "Poverty rate", gap = "Poverty gap",
    fgt2 = "Poverty severity", gini = "Gini coefficient",
    prosperity_gap = "Prosperity gap", avg_poverty = "Average poverty"
  )
  out <- .WISE_METRIC_REGISTRY[[method]] %||% list(
    label = unname(fallback_labels[[method]] %||% method),
    unit = "outcome units", format = "number"
  )
  direction <- out$direction %||% NULL
  # Built-in aggregation methods have an explicit direction contract. Do not
  # let stale outcome metadata from an earlier selection reverse their tail.
  if (!is.null(so) && !method %in% names(.WISE_METRIC_REGISTRY)) {
    so_direction <- if (is.data.frame(so) && "direction" %in% names(so)) {
      so$direction[[1]]
    } else if (is.list(so)) {
      so$direction %||% NULL
    } else {
      NULL
    }
    if (!is.null(so_direction) && nzchar(as.character(so_direction))) {
      direction <- as.character(so_direction)[1]
    }
  }
  if (is.null(direction) || !direction %in% c("higher_is_better", "lower_is_better")) {
    direction <- "higher_is_better"
  }
  out$method <- method
  if (identical(method, "mean") && !is.null(so) &&
      tolower(as.character(so$type %||% ""))[1L] %in% c("logical", "binary", "boolean")) {
    out$format <- "percent"
    out$unit <- "percent"
  }
  out$label <- out$label %||% unname(fallback_labels[[method]] %||% method)
  out$direction <- direction
  out$adverse_tail <- if (identical(direction, "lower_is_better")) "high" else "low"
  out$adverse_note <- if (identical(out$adverse_tail, "high")) {
    "Higher values are adverse"
  } else {
    "Lower values are adverse"
  }
  out$valid_transform <- if (isTRUE(out$percent_change)) "percent change for log effects" else "percentage points or native units"
  out$poverty_line <- isTRUE(out$poverty_line)
  out$engines <- out$engines %||% c("ols", "rif")
  out$uncertainty <- out$uncertainty %||% c("coefficient", "weather", "ensemble")
  out$caveat <- out$caveat %||% "Metric definition supplied by the selected outcome metadata."
  out
}

metric_axis_label <- function(method = "mean", so = NULL, deviation = "none") {
  spec <- metric_metadata(method, so)
  label <- if (identical(deviation, "none")) {
    spec$label
  } else {
    paste0(spec$label, " - ", label_deviation(deviation))
  }
  unit <- switch(spec$format,
    percent = "percent",
    spec$unit %||% "outcome units"
  )
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

# Add the definitions needed to interpret a chart-data export. Keeping these
# fields beside the values makes a CSV useful outside the live Shiny session.
visualization_export_metadata <- function(method = "mean", so = NULL,
                                          observation_unit,
                                          aggregation_order,
                                          uncertainty = "none") {
  spec <- metric_metadata(method, so)
  data.frame(
    metric_id = spec$method,
    metric_label = spec$label,
    unit = spec$unit,
    number_format = spec$format,
    direction = spec$direction,
    adverse_tail = spec$adverse_tail,
    valid_transform = spec$valid_transform,
    poverty_line = isTRUE(spec$poverty_line),
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
                                          uncertainty = "none") {
  if (is.null(data) || !is.data.frame(data)) {
    return(data)
  }
  meta <- visualization_export_metadata(
    method, so, observation_unit, aggregation_order, uncertainty
  )
  for (nm in names(meta)) data[[nm]] <- meta[[nm]][[1L]]
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
