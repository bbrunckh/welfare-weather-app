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
  info$direction <- outcome_direction(name, type)

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


# ---------------------------------------------------------------------------- #
# Outcome missingness summary                                                   #
# ---------------------------------------------------------------------------- #

#' Compute missingness summary for the selected outcome variable
#'
#' @param df      Survey data frame.
#' @param outcome Single character string — the outcome variable name.
#'
#' @return A one-row data frame with columns \code{variable}, \code{n_total},
#'   \code{n_available}, \code{n_missing}, \code{pct_available}.
#' @export
outcome_missing_summary <- function(df, outcome) {
  if (is.null(df) || !outcome %in% names(df))
    return(data.frame(variable = outcome, n_total = NA_integer_,
                      n_available = NA_integer_, n_missing = NA_integer_,
                      pct_available = NA_real_, stringsAsFactors = FALSE))
  x     <- df[[outcome]]
  n     <- length(x)
  n_ok  <- sum(!is.na(x))
  data.frame(
    variable      = outcome,
    n_total       = n,
    n_available   = n_ok,
    n_missing     = n - n_ok,
    pct_available = round(100 * n_ok / max(n, 1), 1),
    stringsAsFactors = FALSE
  )
}


# # ---------------------------------------------------------------------------- #
# # Outcome histogram / density                                                   #
# # ---------------------------------------------------------------------------- #

# #' Plot a histogram of the selected outcome variable
# #'
# #' For numeric outcomes, draws a histogram with an optional log-x scale. For
# #' binary/logical outcomes, draws a bar chart of 0/1 counts.
# #'
# #' @param df      Survey data frame.
# #' @param outcome Single character string — the outcome variable name.
# #' @param label   Human-readable label for the x axis.
# #' @param type    Outcome type: \code{"numeric"} or \code{"logical"}.
# #'
# #' @return A \code{ggplot} object, or \code{NULL} invisibly.
# #' @export
# plot_outcome_histogram <- function(df, outcome, label = outcome,
#                                    type = "numeric") {
#   if (is.null(df) || !outcome %in% names(df)) return(invisible(NULL))
#   vals <- df[[outcome]]
#   vals <- vals[!is.na(vals)]
#   if (length(vals) == 0) return(invisible(NULL))

#   if (identical(type, "logical") || all(vals %in% c(0L, 1L, TRUE, FALSE))) {
#     plot_df <- data.frame(value = factor(as.integer(vals),
#                                          levels = c(0L, 1L),
#                                          labels = c("0 (No)", "1 (Yes)")))
#     ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$value)) +
#       ggplot2::geom_bar(fill = "#3498db", width = 0.5) +
#       ggplot2::labs(x = label, y = "Count") +
#       ggplot2::theme_minimal(base_size = 13)
#   } else {
#     plot_df <- data.frame(value = as.numeric(vals))
#     p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$value)) +
#       ggplot2::geom_histogram(bins = 50, fill = "#3498db", colour = "white",
#                               linewidth = 0.2) +
#       ggplot2::labs(x = label, y = "Count") +
#       ggplot2::theme_minimal(base_size = 13)
#     if (all(plot_df$value > 0, na.rm = TRUE)) {
#       p <- p + ggplot2::scale_x_log10()
#     }
#     p
#   }
# }

# ---------------------------------------------------------------------------- #
# Outcome distribution ridge plot (by survey wave)                              #
# ---------------------------------------------------------------------------- #

#' Plot outcome distribution by survey wave
#'
#' Calls `ridge_distribution_plot()` on the specified outcome column.
#' For welfare, overlays dashed poverty line references. Numeric outcomes use
#' log scale when all values are positive.
#'
#' @param df A data frame with a `countryyear` column for ridge grouping.
#' @param outcome Single character string — outcome variable name. Defaults to
#'   `"welfare"`.
#' @param label Human-readable label for the x axis.
#' @param type Outcome type: `"numeric"` or `"logical"`.
#' @param poverty_lines A data frame with columns `value` and `label`.
#'   Only used when `outcome == "welfare"`.
#'
#' @return A `ggplot` object, or `NULL` invisibly.
#'
#' @export
plot_welfare_dist <- function(df,
                              outcome = "welfare",
                              label = NULL,
                              type = "numeric",
                              poverty_lines = welfare_poverty_lines()) {
  if (is.null(df) || !(outcome %in% names(df))) return(invisible(NULL))

  use_log <- identical(type, "numeric") &&
    all(df[[outcome]][!is.na(df[[outcome]])] > 0)

  x_label <- label %||% outcome
  if (identical(outcome, "welfare")) x_label <- "$ per day (2021 PPP)"

  p <- ridge_distribution_plot(
    df,
    x_var         = outcome,
    x_label       = x_label,
    wrap_width    = 40,
    log_transform = use_log
  )

  if (is.null(p)) return(invisible(NULL))

  if (identical(outcome, "welfare") && !is.null(poverty_lines)) {
    for (i in seq_len(nrow(poverty_lines))) {
      p <- p +
        ggplot2::geom_vline(
          xintercept = poverty_lines$value[i],
          linetype   = "dashed",
          color      = "red",
          linewidth  = 0.5
        ) +
        ggplot2::annotate(
          "text",
          x     = poverty_lines$value[i] * 1.15,
          y     = 0.5,
          label = poverty_lines$label[i],
          angle = 90,
          size  = 3,
          color = "red",
          hjust = 0
        )
    }
  }

  p
}


# ---------------------------------------------------------------------------- #
# Outcome spatial coverage map                                                  #
# ---------------------------------------------------------------------------- #

#' Build a leaflet map highlighting locations with outcome data
#'
#' Colours H3 polygons by whether the outcome variable is available (non-NA)
#' at each location. Uses the same GeoJSON FeatureCollection as the survey
#' map but overlays availability shading.
#'
#' @param geojson  GeoJSON FeatureCollection (from Survey Stats H3 pipeline).
#' @param df       Survey data frame with \code{loc_id} and the outcome column.
#' @param outcome  Single character string — the outcome variable name.
#'
#' @return A \code{leaflet} widget, or \code{NULL}.
#' @export
plot_outcome_coverage_map <- function(geojson, df, outcome) {
  if (is.null(geojson) || length(geojson$features) == 0) return(invisible(NULL))
  if (is.null(df) || !outcome %in% names(df) || !"loc_id" %in% names(df))
    return(invisible(NULL))

  loc_avail <- df |>
    dplyr::mutate(.has = !is.na(.data[[outcome]])) |>
    dplyr::summarise(pct = mean(.data$.has) * 100, .by = "loc_id")

  avail_map <- stats::setNames(loc_avail$pct, as.character(loc_avail$loc_id))

  pal <- leaflet::colorNumeric(
    palette = c("#e74c3c", "#f1c40f", "#2ecc71"),
    domain  = c(0, 100)
  )

  bounds <- .geojson_bounds(geojson)

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

  for (f in geojson$features) {
    lid <- as.character(f$properties$loc_id)
    pct <- avail_map[lid]
    if (is.na(pct)) pct <- 0
    col <- pal(pct)
    sub_gc <- list(type = "FeatureCollection", features = list(f))
    m <- m |>
      leaflet::addGeoJSON(
        geojson     = sub_gc,
        color       = col,
        fillColor   = col,
        fillOpacity = 0.5,
        weight      = 1
      )
  }

  m <- m |>
    leaflet::fitBounds(bounds$lng1, bounds$lat1, bounds$lng2, bounds$lat2) |>
    leaflet::addLegend(
      position = "bottomright", pal = pal,
      values = c(0, 100), title = "% available"
    )
  m
}


# ---------------------------------------------------------------------------- #
# Outcome directionality                                                        #
# ---------------------------------------------------------------------------- #

#' Classify whether larger simulated values are better or worse for an outcome
#'
#' Returns `"lower_is_better"` for poverty/inequality measures where a higher
#' simulated value represents a worse outcome. Returns `"higher_is_better"` for
#' all other outcomes (welfare, employment, hours worked, etc.).
#'
#' @param name A single character string — the outcome variable name.
#' @param type A single character string — the outcome type.
#'
#' @return `"lower_is_better"` or `"higher_is_better"`.
#'
#' @export
outcome_direction <- function(name, type) {
  name <- tolower(as.character(name[1]))
  lower_is_better_names <- c("poor", "headcount_ratio", "gap", "fgt2", "gini")
  if (name %in% lower_is_better_names) "lower_is_better" else "higher_is_better"
}
