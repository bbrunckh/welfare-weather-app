# ============================================================================ #
# Pure functions for survey sample selection logic.                            #
# Used by mod_1_01_sample_server(). 
# All functions are stateless and testable without a Shiny session.                                                     #
# ============================================================================ #

# ---------------------------------------------------------------------------- #
# File listing                                                                  #
# ---------------------------------------------------------------------------- #

#' List available data files at a connection endpoint
#'
#' For local connections lists files at the root of the path. For remote
#' connections where cheap file listing is not available returns `NULL` to
#' signal that the existence filter should be skipped and `survey_list` trusted.
#'
#' @param connection_params A named list as returned by
#'   `build_connection_params()`. Must contain at least `type` and, when
#'   `type = "local"`, a non-empty `path` element.
#'
#' @return A character vector of filenames (basenames, no path) for local
#'   connections, or `NULL` for remote connections.
#'
#' @export
list_available_files <- function(connection_params) {
  if (is.null(connection_params) || !is.list(connection_params)) return(NULL)

  if (identical(connection_params$type, "local")) {
    path <- connection_params$path %||% ""
    if (!nzchar(path) || !dir.exists(path)) return(character(0))
    list.files(path, recursive = FALSE)
  } else {
    # Remote sources (S3, GCS, Azure, HF) cannot cheaply list files without
    # provider-specific ListObjects calls. Return NULL to skip existence filter.
    NULL
  }
}


# ---------------------------------------------------------------------------- #
# Survey file construction                                                      #
# ---------------------------------------------------------------------------- #

#' Construct expected parquet filenames from survey metadata
#'
#' Builds the expected filename and full path for each survey row based on the
#' naming convention `<code>_<year>_<survname>_<unit>.parquet`.
#'
#' @param survey_list A data frame of survey metadata containing at minimum
#'   columns `code`, `year`, `survname`, and `level`.
#' @param unit A character string; the unit of analysis. One of `"ind"`,
#'   `"hh"`, or `"firm"`.
#' @param connection_params A named list as returned by
#'   `build_connection_params()`. Used to construct the full path for local
#'   connections.
#'
#' @return `survey_list` filtered to `level == unit` and augmented with columns
#'   `fname` (basename) and `fpath` (full path for local; bare name for remote).
#'
#' @export
build_survey_fnames <- function(survey_list, unit, connection_params) {
  if (is.null(survey_list) || nrow(survey_list) == 0) return(survey_list)

  is_local  <- identical(connection_params$type, "local")
  base_path <- if (is_local) connection_params$path %||% "" else ""

  survey_list |>
    dplyr::filter(.data$level == unit) |>
    dplyr::mutate(
      fname = paste0(.data$code, "_", .data$year, "_", .data$survname, "_", unit, ".parquet"),
      fpath = if (is_local) file.path(base_path, .data$fname) else .data$fname
    )
}


# ---------------------------------------------------------------------------- #
# Survey filtering                                                              #
# ---------------------------------------------------------------------------- #

#' Filter a survey list to files available at the endpoint
#'
#' Cross-checks survey filenames against the vector returned by
#' `list_available_files()`. When `available_files` is `NULL` (remote
#' connection) no filtering is applied and the full survey list is returned.
#'
#' @param surveys A data frame with at minimum a column `fname`.
#' @param available_files A character vector of available filenames (basenames),
#'   or `NULL` to skip filtering.
#'
#' @return The filtered (or unfiltered) data frame.
#'
#' @export
filter_surveys_to_available <- function(surveys, available_files) {
  if (is.null(available_files)) return(surveys)
  surveys |> dplyr::filter(.data$fname %in% available_files)
}


# ---------------------------------------------------------------------------- #
# Year helpers                                                                  #
# ---------------------------------------------------------------------------- #

#' Get available survey years per economy code
#'
#' Returns a named list where each element is a sorted numeric vector of
#' available survey years for that economy code.
#'
#' @param surveys A data frame with columns `code` and `year`.
#' @param codes A character vector of economy codes to extract years for.
#'
#' @return A named list of numeric vectors, one entry per element of `codes`.
#'   Codes with no matching rows return `integer(0)`.
#'
#' @export
get_available_years <- function(surveys, codes) {
  if (is.null(surveys) || nrow(surveys) == 0) {
    return(stats::setNames(vector("list", length(codes)), codes))
  }
  lapply(stats::setNames(codes, codes), function(code) {
    surveys |>
      dplyr::filter(.data$code == !!code) |>
      dplyr::pull(.data$year) |>
      sort()
  })
}


# ---------------------------------------------------------------------------- #
# Selected survey assembly                                                      #
# ---------------------------------------------------------------------------- #

#' Build the selected surveys data frame from economy/year selections
#'
#' Joins a named list of selected years per economy code back to the full
#' survey metadata table to produce the final selected surveys data frame.
#'
#' @param surveys A data frame with columns `code` and `year` (and any further
#'   metadata columns), as produced by `build_survey_fnames()`.
#' @param years_by_code A named list where names are economy codes and values
#'   are character or numeric vectors of selected years. `NULL` or length-0
#'   elements are silently dropped.
#'
#' @return A data frame of selected surveys (inner join of `surveys` with the
#'   supplied code/year combinations). Returns a zero-row data frame when no
#'   valid combinations are supplied.
#'
#' @export
build_selected_surveys <- function(surveys, years_by_code) {
  years_by_code <- Filter(function(x) !is.null(x) && length(x) > 0, years_by_code)

  if (length(years_by_code) == 0) return(surveys[0L, ])

  combos <- do.call(rbind, lapply(names(years_by_code), function(code) {
    data.frame(
      code = code,
      year = as.numeric(years_by_code[[code]]),
      stringsAsFactors = FALSE
    )
  }))

  surveys |> dplyr::inner_join(combos, by = c("code", "year"))
}