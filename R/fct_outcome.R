# ============================================================================ #
# Pure functions for outcome variable selection logic.                         #
# Used by mod_1_03_outcome_server(). 
# All functions are stateless and testable without a Shiny session.                                                     #
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Available outcomes                                                            #
# ---------------------------------------------------------------------------- #

#' Filter variable list to available outcome variables
#'
#' Filters `variable_list` to rows flagged as outcomes (`outcome == 1`) that
#' are present in `survey_colnames`. If `"welfare"` is present, a synthetic
#' `"poor"` row is appended. Results are ordered welfare -> poor -> all others.
#'
#' @param variable_list A data frame with columns `name`, `label`, `units`,
#'   `type`, and `outcome` (integer 0/1).
#' @param survey_colnames A character vector of column names present in the
#'   loaded survey data.
#'
#' @return A data frame with columns `name`, `label`, `units`, `type`.
#'
#' @export
filter_outcome_vars <- function(variable_list, survey_colnames) {
  outs <- variable_list |>
    dplyr::filter(.data$outcome == 1 & .data$name %in% survey_colnames) |>
    dplyr::select("name", "label", "units", "type") |>
    dplyr::mutate(
      name  = as.character(.data$name),
      label = as.character(.data$label),
      units = as.character(.data$units),
      type  = as.character(.data$type)
    )

  if (any(grepl("welfare", outs$name, fixed = TRUE))) {
    outs <- dplyr::bind_rows(
      outs,
      data.frame(
        name  = "poor",
        label = "Poor (welfare < poverty line)",
        units = "",
        type  = "logical",
        stringsAsFactors = FALSE
      )
    )
  }

  priority <- c("welfare", "poor")
  rest     <- setdiff(outs$name, priority)
  outs |> dplyr::arrange(factor(.data$name, levels = c(priority, rest)))
}


# ---------------------------------------------------------------------------- #
# Monetary outcome guard                                                        #
# ---------------------------------------------------------------------------- #

#' Test whether an outcome row represents a monetary variable
#'
#' Returns `TRUE` when `name` is `"welfare"` or `"poor"`, or when `units` is
#' `"LCU"`. All comparisons are scalar-safe.
#'
#' @param name  A single character string — the outcome variable name.
#' @param units A single character string — the outcome units (may be `NA`).
#'
#' @return A single logical value.
#'
#' @export
is_monetary_outcome <- function(name, units) {
  name  <- as.character(name[1])
  units <- as.character(units[1])
  name %in% c("welfare", "poor") || (!is.na(units) && units == "LCU")
}


# ---------------------------------------------------------------------------- #
# Default LCU poverty line                                                      #
# ---------------------------------------------------------------------------- #

#' Compute the default LCU poverty line from survey welfare data
#'
#' Returns the 20th percentile of LCU welfare (welfare x ppp2021). Falls back
#' to `1.00` on error or when required columns are absent.
#'
#' @param df A data frame with numeric columns `welfare` and `ppp2021`; an
#'   optional `weight` column is used for weighted quantile computation.
#'
#' @return A single numeric value.
#'
#' @export
default_lcu_poverty_line <- function(df) {
  tryCatch({
    if (!all(c("welfare", "ppp2021") %in% names(df))) return(1.00)
    df_lcu <- df |> dplyr::mutate(welfare_lcu = .data$welfare * .data$ppp2021)
    if ("weight" %in% names(df_lcu)) {
      p20 <- Hmisc::wtd.quantile(df_lcu$welfare_lcu, weights = df_lcu$weight,
                                  probs = 0.2, na.rm = TRUE)
    } else {
      p20 <- quantile(df_lcu$welfare_lcu, probs = 0.2, na.rm = TRUE)
    }
    round(as.numeric(p20), 2)
  }, error = function(e) {
    message("default_lcu_poverty_line() error: ", e$message)
    1.00
  })
}


# ---------------------------------------------------------------------------- #
# Poverty line label                                                            #
# ---------------------------------------------------------------------------- #

#' Return the appropriate poverty line input label for the selected currency
#'
#' @param currency A single character string: `"PPP"` or `"LCU"`. Defaults to
#'   `"PPP"` when `NULL`.
#'
#' @return A single character string for use as a `numericInput` label.
#'
#' @export
poverty_line_label <- function(currency) {
  currency <- currency[1]
  if (is.null(currency) || is.na(currency) || identical(currency, "PPP")) {
    "Poverty line ($/day, 2021 PPP)"
  } else {
    "Poverty line (LCU/day)"
  }
}


# ---------------------------------------------------------------------------- #
# Outcome transform classification                                              #
# ---------------------------------------------------------------------------- #

#' Classify the transformation to apply to an outcome variable
#'
#' Returns `"log"` for numeric/continuous outcomes and `NA_character_` for all
#' others (binary, logical, etc.).
#'
#' @param type A single character string — the outcome type as stored in the
#'   variable list (e.g. `"numeric"`, `"logical"`, `"binary"`).
#'
#' @return `"log"` or `NA_character_`.
#'
#' @export
outcome_transform <- function(type) {
  type <- tolower(as.character(type[1]))
  if (identical(type, "numeric")) "log" else NA_character_
}


# ---------------------------------------------------------------------------- #
# Build selected outcome row                                                    #
# ---------------------------------------------------------------------------- #

#' Augment an outcome info row with transform, units, and poverty-line metadata
#'
#' Takes a single-row data frame from `filter_outcome_vars()` and attaches
#' three additional columns: `transform`, `units`, and `povline`.
#'
#' @param info         A single-row data frame with columns `name`, `units`,
#'   `type`.
#' @param currency     A single character string: `"PPP"` or `"LCU"`. May be
#'   `NULL` (treated as `"PPP"`).
#' @param poverty_line A single numeric value. May be `NULL`.
#'
#' @return `info` augmented with columns `transform`, `units`, `povline`.
#'
#' @export
build_selected_outcome <- function(info, currency = NULL, poverty_line = NULL) {
  if (is.null(info) || nrow(info) == 0) return(info)

  name  <- as.character(info$name[1])
  units <- as.character(info$units[1])
  type  <- as.character(info$type[1])

  info$transform <- outcome_transform(type)

  if (is_monetary_outcome(name, units)) {
    info$units <- if (!is.null(currency) && nzchar(currency)) currency else units
    pl <- suppressWarnings(as.numeric(poverty_line))
    info$povline <- if (!is.null(pl) && length(pl) == 1 && is.finite(pl) && pl > 0) {
      pl
    } else if (is.null(currency) || identical(currency, "PPP")) {
      3.00
    } else {
      NA_real_
    }
  } else {
    info$povline <- NA_real_
  }

  info
}


# ---------------------------------------------------------------------------- #
# Outcome info message UI                                                       #
# ---------------------------------------------------------------------------- #

#' Build the informational tagList shown below the outcome selector
#'
#' Returns a `shiny::tagList` with styled divs describing the selected outcome
#' type. Returns an empty `tagList` for unrecognised types.
#'
#' @param type A single character string — the outcome type (e.g. `"numeric"`,
#'   `"logical"`).
#'
#' @return A `shiny.tag.list`.
#'
#' @export
outcome_info_message <- function(type) {
  type     <- tolower(as.character(type[1]))
  messages <- shiny::tagList()

  if (identical(type, "numeric")) {
    messages <- shiny::tagList(messages, shiny::tags$div(
      style = "margin-top:10px;padding:8px;background-color:#d1ecf1;border:1px solid #bee5eb;border-radius:4px;color:#0c5460;",
      "Continuous outcomes will be log-transformed."
    ))
  }

  if (identical(type, "logical")) {
    messages <- shiny::tagList(messages, shiny::tags$div(
      style = "margin-top:10px;padding:8px;background-color:#d4edda;border:1px solid #c3e6cb;border-radius:4px;color:#155724;",
      "Binary outcome selected."
    ))
  }

  messages
}
