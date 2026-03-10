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
plot_binscatter <- function(df, hv, hv_label = hv, y_var, y_label = y_var) {
  if (is.null(df) || !all(c(hv, y_var) %in% names(df))) return(NULL)

  d <- df[, c(hv, y_var), drop = FALSE]
  names(d) <- c("x", "y")
  d <- d[stats::complete.cases(d), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)

  # Outcome: detect binary vs continuous
  y_raw <- d$y
  y_num <- suppressWarnings(as.numeric(as.character(y_raw)))
  is_binary_y <- FALSE

  if (!all(is.na(y_num))) {
    uy <- sort(unique(y_num[!is.na(y_num)]))
    is_binary_y <- length(uy) <= 2 && all(uy %in% c(0, 1))
  }

  if (!is_binary_y && (is.logical(y_raw) || is.factor(y_raw))) {
    y_fac <- as.factor(y_raw)
    if (nlevels(y_fac) == 2) {
      is_binary_y <- TRUE
      y_num <- as.integer(y_fac) - 1
    }
  }

  if (is_binary_y) {
    d$y <- y_num
  } else {
    d$y <- suppressWarnings(as.numeric(d$y))
    d <- d[!is.na(d$y), , drop = FALSE]
    if (nrow(d) == 0) return(NULL)
  }

  # X: detect binned/categorical vs continuous
  is_binned_x <- is.factor(d$x) || is.character(d$x)

  if (is_binned_x) {
    d$x <- as.factor(d$x)

    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5)
      ) +
      ggplot2::labs(
        x = stringr::str_wrap(hv_label, 40),
        y = stringr::str_wrap(y_label, 40)
      )

    if (is_binary_y) {
      p <- p +
        ggplot2::geom_jitter(width = 0.15, height = 0.03, alpha = 0.10) +
        ggplot2::stat_summary(fun = mean, geom = "point", color = "orange", size = 2.5) +
        ggplot2::scale_y_continuous(limits = c(0, 1))
    } else {
      p <- p +
        ggplot2::geom_jitter(width = 0.15, alpha = 0.10) +
        ggplot2::stat_summary(fun = mean, geom = "point", color = "orange", size = 2.5)
    }

    return(p)
  }

  # Continuous x
  d$x <- suppressWarnings(as.numeric(d$x))
  d <- d[!is.na(d$x), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)

  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y)) +
    ggplot2::geom_point(alpha = 0.10) +
    ggplot2::stat_summary_bin(fun = mean, bins = 20, color = "orange", size = 2, geom = "point") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(
      x = stringr::str_wrap(hv_label, 40),
      y = stringr::str_wrap(y_label, 40)
    )

  if (is_binary_y) {
    p <- p + ggplot2::scale_y_continuous(limits = c(0, 1))
  }

  p
}

# ---------------------------------------------------------------------------- #
# Summary stats table                                                          #
# ---------------------------------------------------------------------------- #


#' Weather summary stats DT renderer
#'
#' @param survey_weather Reactive returning merged survey-weather data.
#' @param selected_weather Reactive returning selected weather rows (needs name/label).
#'
#' @return A DT render function.
#' @export
make_weather_stats_dt <- function(survey_weather, selected_weather) {
  DT::renderDT({
    shiny::req(survey_weather(), selected_weather())

    df <- survey_weather() |>
      dplyr::mutate(countryyear = paste0(.data$economy, ", ", .data$year))

    sw <- selected_weather()
    vars <- intersect(sw$name, names(df))
    if (length(vars) == 0) return(data.frame(Note = "No weather variables found"))

    tab <- weighted_summary_long(df, vars = vars)

    # Add wave-specific missingness (% Missing) by countryyear and variable
    if ("countryyear" %in% names(tab) && "variable" %in% names(tab)) {
      miss_list <- lapply(vars, function(v) {
        df |>
          dplyr::group_by(.data$countryyear) |>
          dplyr::summarise(`% Missing` = 100 * mean(is.na(.data[[v]]), na.rm = TRUE), .groups = "drop") |>
          dplyr::mutate(variable = v)
      })
      miss_df <- dplyr::bind_rows(miss_list)
      tab <- dplyr::left_join(tab, miss_df, by = c("countryyear", "variable"))
    }

    # Add variable label
    if ("variable" %in% names(tab)) {
      lab_map <- sw |>
        dplyr::select(name, label) |>
        dplyr::distinct()
      tab <- tab |>
        dplyr::left_join(lab_map, by = c("variable" = "name")) |>
        dplyr::mutate(variable_label = dplyr::coalesce(.data$label, .data$variable)) |>
        dplyr::select(variable, variable_label, dplyr::everything(), -dplyr::any_of("label"))
    }

    # Rename key columns
    if ("variable" %in% names(tab))       names(tab)[names(tab) == "variable"] <- "Variable"
    if ("variable_label" %in% names(tab)) names(tab)[names(tab) == "variable_label"] <- "Variable Label"
    if ("countryyear" %in% names(tab))    names(tab)[names(tab) == "countryyear"] <- "County, Year"

    # Capitalize first letter of all column names
    names(tab) <- vapply(names(tab), function(nm) {
      if (!nzchar(nm)) return(nm)
      paste0(toupper(substr(nm, 1, 1)), substr(nm, 2, nchar(nm)))
    }, character(1))

    dt <- DT::datatable(
      tab,
      rownames = FALSE,
      options = list(dom = "t", paging = FALSE, searching = FALSE, info = FALSE),
      class = "compact"
    )

    # Formatting: N no decimals, others numeric 2 decimals
    num_cols <- names(tab)[vapply(tab, is.numeric, logical(1))]
    num_cols <- setdiff(num_cols, "N")
    if (length(num_cols) > 0) dt <- DT::formatRound(dt, columns = num_cols, digits = 2)

    dt
  })
}