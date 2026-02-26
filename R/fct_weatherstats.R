# ============================================================================ #
# fct_weatherstats.R                                                           #
# Pure functions for weather statistics logic.                                 #
# Used by mod_1_05_weatherstats_server(). Stateless and testable without Shiny.#
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Date helpers                                                                  #
# ---------------------------------------------------------------------------- #

#' Extract unique non-NA survey timestamps from survey data
#'
#' @param survey_data A data frame with a `timestamp` (Date) column.
#'
#' @return A sorted Date vector of unique non-NA timestamps.
#'
#' @export
extract_survey_dates <- function(survey_data) {
  if (is.null(survey_data) || !("timestamp" %in% names(survey_data))) {
    return(as.Date(character(0)))
  }
  survey_data |>
    dplyr::filter(!is.na(.data$timestamp)) |>
    dplyr::distinct(.data$timestamp) |>
    dplyr::pull(.data$timestamp) |>
    sort()
}


# ---------------------------------------------------------------------------- #
# Survey-weather merge                                                          #
# ---------------------------------------------------------------------------- #

#' Merge survey data with weather data and normalise within-survey weights
#'
#' Performs an `inner_join` on `code`, `year`, `survname`, `loc_id`, and
#' `timestamp`, converts `year` to a factor for plotting, and normalises
#' `weight` to sum to 1 within each `code` / `year` / `survname` group.
#'
#' @param survey_data  A data frame of survey observations with at minimum
#'   columns `code`, `year`, `survname`, `loc_id`, `timestamp`, and `weight`.
#' @param weather_data A data frame of weather observations at the loc-month
#'   level with at minimum columns `code`, `year`, `survname`, `loc_id`, and
#'   `timestamp`.
#'
#' @return A merged data frame with `year` as factor and `weight` normalised
#'   within group. Returns `NULL` when either input is `NULL` or the join
#'   produces zero rows.
#'
#' @export
merge_survey_weather <- function(survey_data, weather_data) {
  if (is.null(survey_data) || is.null(weather_data)) return(NULL)

  joined <- survey_data |>
    dplyr::inner_join(
      weather_data,
      by = c("code", "year", "survname", "loc_id", "timestamp")
    ) |>
    dplyr::mutate(year = as.factor(.data$year)) |>
    dplyr::group_by(.data$code, .data$year, .data$survname) |>
    dplyr::mutate(weight = .data$weight / sum(.data$weight, na.rm = TRUE)) |>
    dplyr::ungroup()

  if (nrow(joined) == 0) return(NULL)
  joined
}


# ---------------------------------------------------------------------------- #
# Weather distribution plot                                                     #
# ---------------------------------------------------------------------------- #

#' Plot the distribution of a weather variable
#'
#' For binned variables renders a dodged bar chart of bin counts by
#' `countryyear`. For continuous variables renders a ridge density plot.
#'
#' @param df          A data frame with a `countryyear` column and a column
#'   named `hv`.
#' @param hv          Scalar character. Name of the weather variable column.
#' @param label       Scalar character. Human-readable label for the x-axis.
#' @param cont_binned One of `"Binned"` or `"Continuous"` (or `NA`).
#'
#' @return A `ggplot` object, or `NULL` invisibly when `hv` is absent or `NA`.
#'
#' @export
plot_weather_dist <- function(df, hv, label, cont_binned) {
  if (is.null(df) || is.na(hv) || !(hv %in% names(df))) return(invisible(NULL))

  x_label <- stringr::str_wrap(paste0(label, "\n(as configured)"), 40)

  if (!is.na(cont_binned) && cont_binned == "Binned") {
    df_summary <- df |>
      dplyr::filter(!is.na(.data[[hv]])) |>
      dplyr::group_by(.data$countryyear, .data[[hv]]) |>
      dplyr::summarise(n = dplyr::n(), .groups = "drop")

    ggplot2::ggplot(
      df_summary,
      ggplot2::aes(x = .data[[hv]], y = n, fill = .data$countryyear)
    ) +
      ggplot2::geom_col(
        position = ggplot2::position_dodge(preserve = "single"),
        alpha    = 0.85
      ) +
      ggplot2::scale_fill_brewer(palette = "Set2", name = NULL) +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = "Distribution of bins", x = x_label, y = "Count") +
      ggplot2::theme(
        axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "top",
        legend.text     = ggplot2::element_text(size = 9)
      )
  } else {
    ridge_distribution_plot(df, x_var = hv, x_label = x_label, wrap_width = 40)
  }
}


# ---------------------------------------------------------------------------- #
# Binscatter plot                                                               #
# ---------------------------------------------------------------------------- #

#' Plot a binscatter of an outcome against a weather variable
#'
#' For binary outcomes plots the conditional mean by bin as a line. For
#' continuous outcomes overlays raw points with a binned mean overlay.
#'
#' @param df       A data frame containing both `hv` and `y_var` columns.
#' @param hv       Scalar character. Name of the weather variable column.
#' @param hv_label Scalar character. x-axis label.
#' @param y_var    Scalar character. Name of the outcome variable column.
#' @param y_label  Scalar character. y-axis label.
#'
#' @return A `ggplot` object, or `NULL` invisibly when inputs are missing or
#'   no finite data remain after filtering.
#'
#' @export
plot_binscatter <- function(df, hv, hv_label, y_var, y_label) {
  if (is.null(df) || is.null(y_var) || is.na(hv)) return(invisible(NULL))
  if (!(hv %in% names(df)) || !(y_var %in% names(df))) return(invisible(NULL))

  hv_vals <- df[[hv]]
  y_vals  <- df[[y_var]]
  hv_ok   <- if (is.numeric(hv_vals)) is.finite(hv_vals) else !is.na(hv_vals)
  y_ok    <- if (is.numeric(y_vals))  is.finite(y_vals)  else !is.na(y_vals)

  df_plot <- df[hv_ok & y_ok, ]
  if (nrow(df_plot) == 0) return(invisible(NULL))

  is_binary <- length(unique(df_plot[[y_var]])) <= 2

  if (is_binary) {
    df_plot[[y_var]] <- as.numeric(as.character(df_plot[[y_var]]))
  }

  p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = .data[[hv]], y = .data[[y_var]]))

  if (is_binary) {
    p <- p +
      ggplot2::stat_summary_bin(
        fun = "mean", bins = 10, color = "orange", size = 2, geom = "point"
      ) 
  } else {
    p <- p +
      ggplot2::geom_point(alpha = 0.1) +
      ggplot2::stat_summary_bin(
        fun = "mean", bins = 10, color = "orange", size = 2, geom = "point"
      )
  }

  p <- p +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = stringr::str_wrap(hv_label, 40),
      y = stringr::str_wrap(y_label,  40)
    )

  if (is_binary) {
    p <- p + ggplot2::annotate(
      "text", x = Inf, y = Inf,
      label = "Binary outcome: mean by bin",
      hjust = 1.05, vjust = 1.2, size = 3
    )
  }

  p
}
