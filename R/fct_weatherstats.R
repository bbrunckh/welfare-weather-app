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
#' OUTDATED: `weight` to sum to 1 within each `code` / `year` / `survname` group.
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
    # dplyr::mutate(weight = .data$weight / sum(.data$weight, na.rm = TRUE)) |>
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
      theme_wise() +
      ggplot2::labs(x = x_label, y = "Count") +
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
      theme_wise() +
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
    theme_wise() +
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
# Historical vs sample weather comparison                                       #
# ---------------------------------------------------------------------------- #

#' Default year range for the historical weather comparison
#'
#' The range ends at the latest calendar year covered by the survey
#' timestamps and spans `n_years` calendar years (inclusive).
#'
#' @param survey_dates Date vector of survey timestamps.
#' @param n_years      Integer. Length of the range. Default 20.
#'
#' @return A named integer vector `c(from = , to = )`, or `NULL` when
#'   `survey_dates` is empty.
#'
#' @export
default_hist_year_range <- function(survey_dates, n_years = 20L) {
  survey_dates <- as.Date(survey_dates)
  survey_dates <- survey_dates[!is.na(survey_dates)]
  if (length(survey_dates) == 0) return(NULL)

  to <- as.integer(format(max(survey_dates), "%Y"))
  c(from = to - as.integer(n_years) + 1L, to = to)
}


#' Expand survey timestamps across a range of calendar years
#'
#' Repeats each survey month-day across every year in `[year_from, year_to]`
#' so the historical series covers exactly the same part of the calendar as
#' the survey waves (e.g. August-October only, if that is when the survey was
#' fielded). The original survey timestamps are always retained so the sample
#' can be plotted alongside the historical distribution even when the survey
#' year falls outside the requested range.
#'
#' Any preceding months pulled in by a variable's temporal aggregation window
#' are handled by `get_weather()` itself — the rolling window is applied
#' relative to each returned timestamp.
#'
#' @param survey_dates Date vector of survey timestamps.
#' @param year_from,year_to Integer calendar years (inclusive).
#'
#' @return A sorted Date vector of unique timestamps.
#'
#' @export
expand_hist_dates <- function(survey_dates, year_from, year_to) {
  survey_dates <- as.Date(survey_dates)
  survey_dates <- survey_dates[!is.na(survey_dates)]
  if (length(survey_dates) == 0) return(as.Date(character(0)))

  year_from <- as.integer(year_from)
  year_to   <- as.integer(year_to)
  if (is.na(year_from) || is.na(year_to)) return(sort(unique(survey_dates)))
  if (year_from > year_to) {
    tmp <- year_from; year_from <- year_to; year_to <- tmp
  }

  month_day <- unique(format(survey_dates, "%m-%d"))
  grid      <- expand.grid(
    year = seq.int(year_from, year_to), md = month_day,
    stringsAsFactors = FALSE
  )
  expanded <- as.Date(paste0(grid$year, "-", grid$md), format = "%Y-%m-%d")

  sort(unique(c(survey_dates, expanded[!is.na(expanded)])))
}


#' Restrict historical weather to the survey's location-month cells
#'
#' Joins a historical weather frame (loc x timestamp, as returned by
#' `get_weather()$historical`) onto the `loc_id` x calendar-month cells that
#' the survey sample actually occupies, and attaches the number of sampled
#' households per cell. This does three things at once:
#'
#' * drops locations that are not in the sample,
#' * keeps only the calendar months the wave was fielded in — per wave, so
#'   two waves fielded in different seasons stay separate,
#' * gives each cell the weight of the households behind it, so the
#'   historical and sample distributions are composed the same way.
#'
#' Rows falling on a wave's own survey timestamps are flagged `is_sample`.
#'
#' @param hist_df        Data frame with `code`, `year`, `survname`, `loc_id`,
#'   `timestamp` and one column per weather variable.
#' @param survey_weather Merged survey-weather frame (household level).
#'
#' @return `hist_df` with added columns `int_month`, `cal_year`, `n_hh`,
#'   `is_sample`, `economy` and `countryyear`; `NULL` when the inputs cannot
#'   be joined or nothing survives the join.
#'
#' @export
join_hist_sample_cells <- function(hist_df, survey_weather) {
  keys <- c("code", "year", "survname", "loc_id", "timestamp")
  if (is.null(hist_df) || is.null(survey_weather)) return(NULL)
  if (!all(keys %in% names(hist_df)) || !all(keys %in% names(survey_weather))) {
    return(NULL)
  }

  sw <- survey_weather
  sw$year      <- as.character(sw$year)
  sw$timestamp <- as.Date(sw$timestamp)
  sw$int_month <- as.integer(format(sw$timestamp, "%m"))
  if (!"economy" %in% names(sw)) sw$economy <- sw$code

  # One row per wave x location x calendar month, weighted by the households
  # sampled there.
  cells <- sw |>
    dplyr::count(
      .data$code, .data$year, .data$survname, .data$loc_id, .data$int_month,
      name = "n_hh"
    )

  waves <- sw |>
    dplyr::distinct(.data$code, .data$year, .data$survname, .data$economy)

  wave_dates <- sw |>
    dplyr::distinct(.data$code, .data$year, .data$survname, .data$timestamp) |>
    dplyr::mutate(is_sample = TRUE)

  h <- hist_df
  h$year      <- as.character(h$year)
  h$timestamp <- as.Date(h$timestamp)
  h$int_month <- as.integer(format(h$timestamp, "%m"))
  h$cal_year  <- as.integer(format(h$timestamp, "%Y"))

  h <- h |>
    dplyr::inner_join(
      cells, by = c("code", "year", "survname", "loc_id", "int_month")
    ) |>
    dplyr::left_join(waves, by = c("code", "year", "survname")) |>
    dplyr::left_join(
      wave_dates, by = c("code", "year", "survname", "timestamp")
    )

  if (nrow(h) == 0) return(NULL)

  h$is_sample   <- !is.na(h$is_sample)
  h$countryyear <- paste0(h$economy, ", ", h$year)
  h
}


#' Overlay the historical weather distribution with the survey sample
#'
#' Draws a household-weighted histogram of the chosen historical years and
#' overlays the survey wave's own weather as a density curve on the same
#' density scale, so the two remain comparable despite very different numbers
#' of observations (the sample covers a handful of location-months, the
#' historical series covers the same cells across many years). One facet per
#' survey wave.
#'
#' @param cells_df  Data frame from `join_hist_sample_cells()`.
#' @param hv        Scalar character. Name of the weather variable column.
#' @param label     Scalar character. Human-readable variable label.
#' @param year_from,year_to Integer calendar years bounding the historical
#'   series (inclusive).
#' @param bins      Integer. Number of histogram bins. Default 30.
#'
#' @return A `ggplot` object, or `NULL` when there is nothing to plot.
#'
#' @export
plot_hist_vs_sample <- function(cells_df, hv, label, year_from, year_to,
                                bins = 30) {
  if (is.null(cells_df) || is.na(hv) || !(hv %in% names(cells_df))) return(NULL)

  vals <- suppressWarnings(as.numeric(cells_df[[hv]]))
  d    <- cells_df[is.finite(vals), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  d$value <- vals[is.finite(vals)]

  hist_lab <- paste0("Historical ", year_from, "–", year_to)
  samp_lab <- "Survey sample"

  hist_part <- d[d$cal_year >= as.integer(year_from) &
                   d$cal_year <= as.integer(year_to), , drop = FALSE]
  samp_part <- d[d$is_sample, , drop = FALSE]
  if (nrow(hist_part) == 0) return(NULL)

  hist_part$source <- hist_lab
  samp_part$source <- samp_lab

  p <- ggplot2::ggplot() +
    ggplot2::geom_histogram(
      data    = hist_part,
      mapping = ggplot2::aes(
        x      = .data$value,
        y      = ggplot2::after_stat(density),
        weight = .data$n_hh,
        fill   = .data$source
      ),
      colour = NA, alpha = 0.7, bins = bins
    )

  if (nrow(samp_part) > 0) {
    p <- p + ggplot2::geom_density(
      data    = samp_part,
      mapping = ggplot2::aes(
        x      = .data$value,
        y      = ggplot2::after_stat(density),
        weight = .data$n_hh,
        colour = .data$source
      ),
      fill = NA, linewidth = 0.9, key_glyph = ggplot2::draw_key_path
    )
  }

  p +
    ggplot2::facet_wrap(ggplot2::vars(.data$countryyear), scales = "free_y") +
    ggplot2::scale_fill_manual(
      values = stats::setNames("#808080", hist_lab), name = NULL
    ) +
    ggplot2::scale_colour_manual(
      values = stats::setNames("#1f78b4", samp_lab), name = NULL
    ) +
    theme_wise() +
    ggplot2::labs(
      x       = stringr::str_wrap(paste0(label, "\n(as configured)"), 40),
      y       = "Density",
      caption = paste(
        "Same locations and calendar months as the survey wave, weighted by",
        "the number of sampled households per location-month."
      )
    ) +
    ggplot2::theme(legend.position = "top")
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
    if (!is.data.frame(tab) || nrow(tab) == 0) {
      return(data.frame(
        Note = "No continuous weather variables to summarise (binned variables are shown below)."
      ))
    }

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


#' Weather binned-variable level-distribution DT renderer
#'
#' Builds a DT for binned (factor / character) weather variables, showing
#' count and share of observations in each bin per `countryyear`. Numeric
#' weather variables are skipped (handled by `make_weather_stats_dt`).
#'
#' @param survey_weather   Reactive returning merged survey-weather data.
#' @param selected_weather Reactive returning selected weather rows
#'   (needs `name` and `label`).
#'
#' @return A DT render function.
#' @export
make_weather_binned_stats_dt <- function(survey_weather, selected_weather) {
  DT::renderDT({
    shiny::req(survey_weather(), selected_weather())

    df <- survey_weather() |>
      dplyr::mutate(countryyear = paste0(.data$economy, ", ", .data$year))

    sw <- selected_weather()
    vars <- intersect(sw$name, names(df))
    if (length(vars) == 0) {
      return(data.frame(Note = "No weather variables found"))
    }

    binned_vars <- vars[vapply(df[vars],
                               function(x) !is.numeric(x), logical(1))]
    if (length(binned_vars) == 0) {
      return(data.frame(
        Note = "No binned weather variables to summarise."
      ))
    }

    rows_list <- lapply(binned_vars, function(v) {
      counts <- df |>
        dplyr::filter(!is.na(.data[[v]])) |>
        dplyr::group_by(.data$countryyear, .data[[v]]) |>
        dplyr::summarise(N = dplyr::n(), .groups = "drop") |>
        dplyr::group_by(.data$countryyear) |>
        dplyr::mutate(share = 100 * .data$N / sum(.data$N)) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          variable = v,
          level    = as.character(.data[[v]])
        ) |>
        dplyr::select(.data$variable, .data$countryyear, .data$level,
                      .data$N, .data$share)

      miss_df <- df |>
        dplyr::group_by(.data$countryyear) |>
        dplyr::summarise(`% Missing` = 100 * mean(is.na(.data[[v]])),
                         .groups = "drop")

      counts |> dplyr::left_join(miss_df, by = "countryyear")
    })

    tab <- dplyr::bind_rows(rows_list)
    if (nrow(tab) == 0) {
      return(data.frame(Note = "No binned weather observations found."))
    }

    if ("variable" %in% names(tab) &&
        all(c("name", "label") %in% names(sw))) {
      lab_map <- sw |>
        dplyr::select(.data$name, .data$label) |>
        dplyr::distinct()
      tab <- tab |>
        dplyr::left_join(lab_map, by = c("variable" = "name")) |>
        dplyr::mutate(
          variable_label = dplyr::coalesce(.data$label, .data$variable)
        ) |>
        dplyr::select(.data$variable, .data$variable_label,
                      .data$countryyear, .data$level, .data$N,
                      .data$share, .data$`% Missing`)
    }

    # Sort: by variable, country-year, then by level (factor order if available)
    tab <- tab |>
      dplyr::arrange(.data$variable, .data$countryyear, .data$level)

    if ("variable" %in% names(tab))
      names(tab)[names(tab) == "variable"] <- "Variable"
    if ("variable_label" %in% names(tab))
      names(tab)[names(tab) == "variable_label"] <- "Variable Label"
    if ("countryyear" %in% names(tab))
      names(tab)[names(tab) == "countryyear"] <- "Country, Year"
    if ("level" %in% names(tab))
      names(tab)[names(tab) == "level"] <- "Level"
    if ("share" %in% names(tab))
      names(tab)[names(tab) == "share"] <- "Share (%)"

    dt <- DT::datatable(
      tab,
      rownames = FALSE,
      options  = list(dom = "t", paging = FALSE,
                      searching = FALSE, info = FALSE),
      class    = "compact"
    )

    num_cols <- intersect(c("Share (%)", "% Missing"), names(tab))
    if (length(num_cols) > 0) {
      dt <- DT::formatRound(dt, columns = num_cols, digits = 2)
    }

    dt
  })
}

#' Per-weather-variable plot layout (full panel for 1 var, two for >= 2)
#'
#' Returns a `bslib::card` for a single weather variable, or a two-column
#' `bslib::layout_columns` for two. Used to keep panel layouts consistent
#' across the app (Step 1 weather stats, Step 1 results, Step 3
#' decomposition).
#'
#' @param ns       The module's `NS` function (from `session$ns`).
#' @param n_vars   Integer. Number of selected weather variables.
#' @param ids      Character vector of length 2 — output IDs for plot 1
#'                 and plot 2. Only `ids[1]` is used when `n_vars < 2`.
#' @param height   CSS height passed to `shiny::plotOutput`.
#'
#' @return A Shiny tag.
#' @noRd
weather_plot_layout <- function(ns, n_vars, ids, height = "500px") {
  if (isTRUE(n_vars >= 2)) {
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(shiny::plotOutput(ns(ids[1]), height = height)),
      bslib::card(shiny::plotOutput(ns(ids[2]), height = height))
    )
  } else {
    bslib::card(shiny::plotOutput(ns(ids[1]), height = height))
  }
}