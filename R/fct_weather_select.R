# ============================================================================ #
# Pure functions for weather variable selection and specification logic.       #
# Used by mod_1_04_weather_server(). Stateless and testable without Shiny.    #
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Variable list helpers                                                         #
# ---------------------------------------------------------------------------- #

#' Filter a variable list to weather variables
#'
#' @param variable_list A data frame with columns `hazard` (0/1), `name`,
#'   `label`, and `units`.
#'
#' @return A data frame with columns `name`, `label`, `units` filtered to rows
#'   where `hazard == 1`.
#'
#' @export
get_weather_vars <- function(variable_list) {
  if (is.null(variable_list) || nrow(variable_list) == 0) {
    return(data.frame(name = character(), label = character(),
                      units = character(), stringsAsFactors = FALSE))
  }
  variable_list |>
    dplyr::filter(.data$hazard == 1) |>
    dplyr::select("name", "label", "units")
}


# ---------------------------------------------------------------------------- #
# UI choice helpers                                                             #
# ---------------------------------------------------------------------------- #

#' Temporal aggregation choices for a weather variable
#'
#' Returns aggregation choices based on units. Variables with `"days"` or
#' `"mm"` units include `"Sum"`; all others do not.
#'
#' @param units Scalar character string of the variable's units.
#'
#' @return A character vector of aggregation choices.
#'
#' @export
temporal_agg_choices <- function(units) {
  base <- c("Mean", "Median", "Min", "Max")
  if (!is.na(units) && units %in% c("days", "mm")) c(base, "Sum") else base
}

#' Default temporal aggregation for a weather variable
#'
#' @param units Scalar character string of the variable's units.
#'
#' @return `"Sum"` for `"days"`/`"mm"` units, otherwise `"Mean"`.
#'
#' @export
temporal_agg_default <- function(units) {
  if (!is.na(units) && units %in% c("days", "mm")) "Sum" else "Mean"
}

#' Transformation choices for a weather variable
#'
#' Dimensionless variables (empty-string units) are restricted to
#' `"Standardized anomaly"`. All others include `"None"` and
#' `"Deviation from mean"` as well.
#'
#' @param units Scalar character string of the variable's units.
#'
#' @return A character vector of transformation choices.
#'
#' @export
transformation_choices <- function(units) {
  if (!is.na(units) && identical(units, "")) {
    "Standardized anomaly"
  } else {
    c("None", "Deviation from mean", "Standardized anomaly")
  }
}

#' Default transformation for a weather variable
#'
#' @param units Scalar character string of the variable's units.
#'
#' @return `"Standardized anomaly"` for dimensionless units, `"None"`
#'   otherwise.
#'
#' @export
transformation_default <- function(units) {
  if (!is.na(units) && identical(units, "")) "Standardized anomaly" else "None"
}


# ---------------------------------------------------------------------------- #
# Spec defaults                                                                 #
# ---------------------------------------------------------------------------- #

#' Safe defaults for a weather variable specification
#'
#' Returns a named list of defaults for all weather configuration parameters.
#' Used when Shiny inputs have not yet been rendered to avoid `NULL`
#' propagation into `build_weather_spec()`.
#'
#' @param units Scalar character string of the variable's units.
#'
#' @return A named list with elements `ref_start`, `ref_end`, `temporal_agg`,
#'   `transformation`, `cont_binned`, `num_bins`, `binning_method`, and
#'   `polynomial`.
#'
#' @export
weather_spec_defaults <- function(units) {
  list(
    ref_start      = 1L,
    ref_end        = 1L,
    temporal_agg   = temporal_agg_default(units),
    transformation = transformation_default(units),
    cont_binned    = "Binned",
    num_bins       = 5L,
    binning_method = "Equal frequency",
    polynomial     = character(0)
  )
}


# ---------------------------------------------------------------------------- #
# Spec assembly                                                                 #
# ---------------------------------------------------------------------------- #

#' Build a single weather variable specification tibble row
#'
#' Assembles a one-row `tibble` describing the full specification for one
#' weather variable. All parameters fall back to `weather_spec_defaults()`
#' when `NULL` is supplied.
#'
#' @param name          Scalar character. Variable name (e.g. `"tx"`).
#' @param units         Scalar character. Variable units (e.g. `"°C"`).
#' @param ref_period    Integer vector of length 2: `c(start, end)` months
#'   before interview. Defaults to `c(1L, 1L)`.
#' @param temporal_agg  Scalar character. Temporal aggregation method.
#' @param transformation Scalar character. Transformation name.
#' @param cont_binned   One of `"Continuous"` or `"Binned"`.
#' @param num_bins      Integer. Number of bins when `cont_binned == "Binned"`.
#' @param binning_method Scalar character. Binning method name.
#' @param polynomial    Character vector of polynomial degrees (e.g.
#'   `c("2", "3")`).
#'
#' @return A one-row `tibble` with columns `name`, `ref_start`, `ref_end`,
#'   `temporalAgg`, `transformation`, `cont_binned`, `num_bins`,
#'   `binning_method`, and `polynomial` (list column).
#'
#' @export
build_weather_spec <- function(name,
                                units,
                                ref_period     = NULL,
                                temporal_agg   = NULL,
                                transformation = NULL,
                                cont_binned    = NULL,
                                num_bins       = NULL,
                                binning_method = NULL,
                                polynomial     = NULL) {
  defs           <- weather_spec_defaults(units)
  ref_period     <- ref_period     %||% c(defs$ref_start, defs$ref_end)
  temporal_agg   <- temporal_agg   %||% defs$temporal_agg
  transformation <- transformation %||% defs$transformation
  cont_binned    <- cont_binned    %||% defs$cont_binned
  polynomial     <- polynomial     %||% defs$polynomial

  is_binned      <- identical(cont_binned, "Binned")
  num_bins       <- if (is_binned) as.integer(num_bins %||% 5L) else NA_integer_
  binning_method <- if (is_binned) (binning_method %||% "Equal frequency") else NA_character_

  tibble::tibble(
    name           = name,
    ref_start      = as.integer(ref_period[1]),
    ref_end        = as.integer(ref_period[2]),
    temporalAgg    = temporal_agg,
    transformation = transformation,
    cont_binned    = cont_binned,
    num_bins       = num_bins,
    binning_method = binning_method,
    polynomial     = list(polynomial)
  )
}


# ---------------------------------------------------------------------------- #
# Selected weather assembly                                                     #
# ---------------------------------------------------------------------------- #

#' Build the full selected weather specification data frame
#'
#' Loops over `selected_vars`, calls `build_weather_spec()` for each, and
#' joins the result to `var_info` to attach `label` and `units`. This is the
#' pure equivalent of the `selected_weather` reactive in
#' `mod_1_04_weather_server()`.
#'
#' @param selected_vars Character vector of selected variable names.
#' @param var_info      Data frame with columns `name`, `label`, `units` as
#'   returned by `get_weather_vars()`.
#' @param spec_inputs   A named list of raw input values keyed by
#'   `<varname>_<param>` (e.g. `tx_relativePeriod`, `tx_temporalAgg`).
#'   Absent or `NULL` entries will use `weather_spec_defaults()`.
#'
#' @return A `tibble` with one row per selected variable and columns `name`,
#'   `label`, `units`, `ref_start`, `ref_end`, `temporalAgg`,
#'   `transformation`, `cont_binned`, `num_bins`, `binning_method`,
#'   `polynomial`.
#'
#' @export
build_selected_weather <- function(selected_vars, var_info, spec_inputs = list()) {
  if (length(selected_vars) == 0 || nrow(var_info) == 0) {
    return(tibble::tibble(
      name = character(), label = character(), units = character(),
      ref_start = integer(), ref_end = integer(), temporalAgg = character(),
      transformation = character(), cont_binned = character(),
      num_bins = integer(), binning_method = character(),
      polynomial = list()
    ))
  }

  specs <- lapply(selected_vars, function(v) {
    units  <- var_info$units[var_info$name == v]
    units  <- if (length(units) == 0 || is.na(units[1])) "" else as.character(units[1])
    prefix <- paste0(v, "_")

    build_weather_spec(
      name           = v,
      units          = units,
      ref_period     = spec_inputs[[paste0(prefix, "relativePeriod")]],
      temporal_agg   = spec_inputs[[paste0(prefix, "temporalAgg")]],
      transformation = spec_inputs[[paste0(prefix, "varConstruction")]],
      cont_binned    = spec_inputs[[paste0(prefix, "contOrBinned")]],
      num_bins       = spec_inputs[[paste0(prefix, "numBins")]],
      binning_method = spec_inputs[[paste0(prefix, "binningMethod")]],
      polynomial     = spec_inputs[[paste0(prefix, "polynomial")]]
    )
  })

  var_info |>
    dplyr::filter(.data$name %in% selected_vars) |>
    dplyr::left_join(dplyr::bind_rows(specs), by = "name")
}