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

#' Merge survey data with weather data
#'
#' Performs an `inner_join` on `code`, `year`, `survname`, `loc_id`, and
#' `timestamp`, and converts `year` to a factor for plotting.
#'
#' Weights are carried through unmodified: raw household survey weights are the
#' statistical contract for all downstream weather statistics, so no per-wave
#' normalisation is applied even when multiple survey waves are combined.
#'
#' @param survey_data  A data frame of survey observations with at minimum
#'   columns `code`, `year`, `survname`, `loc_id`, `timestamp`, and `weight`.
#' @param weather_data A data frame of weather observations at the loc-month
#'   level with at minimum columns `code`, `year`, `survname`, `loc_id`, and
#'   `timestamp`.
#'
#' @return A merged data frame with `year` as factor and raw `weight` values
#'   preserved. Returns `NULL` when either input is `NULL` or the join
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
    dplyr::ungroup()

  if (nrow(joined) == 0) return(NULL)
  joined
}


# ---------------------------------------------------------------------------- #
# Weather distribution plots                                                    #
# ---------------------------------------------------------------------------- #
# Both the binned bar chart and the continuous ridge plot draw the survey wave
# and that wave's own climate history in the same panel, so the comparison the
# user cares about ("was this wave unusual?") is a within-panel one. Historical
# weather is loaded for the same locations and calendar months as each wave and
# weighted by the households behind them, so the two series are composed the
# same way - see `join_hist_sample_cells()`.

# Label used for the sample series across both plots.
#' @noRd
.wx_sample_lab <- "Survey sample"

# Label used for the historical series across both plots.
#' @noRd
.wx_hist_lab <- function(year_from, year_to) {
  paste0("Historical ", year_from, "-", year_to)
}


#' One colour per survey wave
#'
#' Shared by the binned bar chart and the continuous ridge plot so a wave keeps
#' its colour across both panels.
#'
#' @param waves Character vector of wave labels (`countryyear`).
#'
#' @return A named character vector of colours.
#' @noRd
.wave_palette <- function(waves) {
  n <- length(waves)
  if (n == 0) return(character(0))
  base <- scales::brewer_pal(palette = "Set2")(max(3L, min(n, 8L)))
  if (n > length(base)) base <- grDevices::colorRampPalette(base)(n)
  stats::setNames(base[seq_len(n)], waves)
}


#' Blend colours towards another colour
#'
#' Used to derive the historical series' colours from the wave's own colour:
#' a lighter fill for the bars, a darker outline for the ridges.
#'
#' @param col     Character vector of colours.
#' @param towards Colour to blend towards.
#' @param amount  Numeric in `[0, 1]`. How far to move.
#'
#' @return A character vector of hex colours.
#' @noRd
.blend_colour <- function(col, towards = "white", amount = 0.55) {
  from <- grDevices::col2rgb(col) / 255
  to   <- as.numeric(grDevices::col2rgb(towards)) / 255
  out  <- from * (1 - amount) + to * amount
  grDevices::rgb(out[1, ], out[2, ], out[3, ])
}


#' Bin historical weather with the survey's own bin breaks
#'
#' The historical series is always loaded continuous (binning is a modelling
#' choice, not a property of the weather), so it has to be cut here with the
#' breaks the survey sample was binned on. Both sides then share one set of bin
#' labels and the bars line up.
#'
#' @param hist_df   Data frame from `join_hist_sample_cells()`.
#' @param hv        Scalar character. Name of the weather variable column.
#' @param breaks    Numeric break vector for `hv`, from `stored_breaks`.
#' @param year_from,year_to Integer calendar years bounding the historical
#'   series (inclusive). `NULL` keeps every year present.
#'
#' @return A data frame of `countryyear`, `bin` and `w` (household weight), or
#'   `NULL` when the historical series cannot be binned.
#' @noRd
.hist_bin_counts <- function(hist_df, hv, breaks, year_from = NULL,
                             year_to = NULL) {
  if (is.null(hist_df) || is.null(breaks) || length(breaks) < 2) return(NULL)
  if (is.na(hv) || !(hv %in% names(hist_df))) return(NULL)
  if (!all(c("n_hh", "countryyear") %in% names(hist_df))) return(NULL)

  v    <- suppressWarnings(as.numeric(hist_df[[hv]]))
  keep <- is.finite(v)
  if (!is.null(year_from) && !is.null(year_to) &&
      "cal_year" %in% names(hist_df)) {
    keep <- keep &
      hist_df$cal_year >= as.integer(year_from) &
      hist_df$cal_year <= as.integer(year_to)
  }
  if (!any(keep)) return(NULL)

  data.frame(
    countryyear = as.character(hist_df$countryyear[keep]),
    bin         = as.character(cut(v[keep], breaks = breaks,
                                   include.lowest = TRUE)),
    w           = as.numeric(hist_df$n_hh[keep]),
    stringsAsFactors = FALSE
  ) |>
    dplyr::group_by(.data$countryyear, .data$bin) |>
    dplyr::summarise(w = sum(.data$w, na.rm = TRUE), .groups = "drop") |>
    as.data.frame()
}


#' Bar chart of a binned weather variable, sample against its own history
#'
#' One group of bars per bin. Within a group each survey wave contributes two
#' bars - what the sample actually experienced and what the same locations and
#' calendar months looked like over the historical years - drawn in the wave's
#' colour, the historical bar in a lighter shade of it.
#'
#' Bars show the share of observations in each bin *within* a wave-source
#' series, not raw counts: the historical series spans decades and would
#' otherwise dwarf the single wave behind it.
#'
#' @param df        Merged survey-weather frame with `countryyear` and a binned
#'   `hv` column.
#' @param hv        Scalar character. Name of the weather variable column.
#' @param label     Scalar character. Human-readable label for the x-axis.
#' @param hist_df   Optional data frame from `join_hist_sample_cells()`. When
#'   `NULL` (or when `breaks` is missing) only the sample bars are drawn.
#' @param breaks    Numeric break vector for `hv`, from `stored_breaks`.
#' @param year_from,year_to Integer calendar years bounding the historical
#'   series (inclusive).
#'
#' @return A `ggplot` object, or `NULL` invisibly when there is nothing to plot.
#'
#' @export
plot_weather_bins_compare <- function(df, hv, label, hist_df = NULL,
                                      breaks = NULL, year_from = NULL,
                                      year_to = NULL) {
  if (is.null(df) || is.na(hv) || !(hv %in% names(df))) return(invisible(NULL))
  if (!("countryyear" %in% names(df))) return(invisible(NULL))

  keep <- !is.na(df[[hv]])
  if (!any(keep)) return(invisible(NULL))

  lvls <- if (is.factor(df[[hv]])) {
    levels(df[[hv]])
  } else {
    sort(unique(as.character(df[[hv]][keep])))
  }

  samp <- data.frame(
    countryyear = as.character(df$countryyear[keep]),
    bin         = as.character(df[[hv]][keep]),
    w           = 1,
    stringsAsFactors = FALSE
  ) |>
    dplyr::group_by(.data$countryyear, .data$bin) |>
    dplyr::summarise(w = sum(.data$w), .groups = "drop") |>
    as.data.frame()
  samp$source <- .wx_sample_lab

  hist_lab   <- .wx_hist_lab(year_from, year_to)
  hist_bins  <- .hist_bin_counts(hist_df, hv, breaks, year_from, year_to)
  has_hist   <- !is.null(hist_bins) && nrow(hist_bins) > 0
  if (has_hist) hist_bins$source <- hist_lab

  d <- if (has_hist) rbind(samp, hist_bins) else samp

  # Share within each wave x series, so a wave's sample bars and its
  # historical bars each sum to 100 and can be read against each other.
  d <- d |>
    dplyr::group_by(.data$countryyear, .data$source) |>
    dplyr::mutate(share = 100 * .data$w / sum(.data$w, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    as.data.frame()

  waves <- sort(unique(d$countryyear))
  pal   <- .wave_palette(waves)

  # Wave-major key order, so each wave's pair of bars sits side by side inside
  # a bin rather than all samples first and all histories after. The historical
  # bar is a lighter shade of the wave's own colour.
  sources <- if (has_hist) c(.wx_sample_lab, hist_lab) else .wx_sample_lab
  series  <- .wx_series_grid(waves, sources)
  key_cols <- stats::setNames(
    ifelse(series$source == .wx_sample_lab,
           pal[series$wave],
           .blend_colour(pal[series$wave], "white", 0.6)),
    series$key
  )

  d$key <- factor(.wx_series_key(d$countryyear, d$source),
                  levels = series$key)
  d$bin <- factor(d$bin, levels = lvls)

  ggplot2::ggplot(
    d, ggplot2::aes(x = .data$bin, y = .data$share, fill = .data$key)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(preserve = "single"),
      colour   = "grey45", linewidth = 0.25, alpha = 0.9
    ) +
    ggplot2::scale_fill_manual(values = key_cols, name = NULL,
                               drop = FALSE) +
    theme_wise() +
    ggplot2::labs(
      x = stringr::str_wrap(paste0(label, "\n(as configured)"), 40),
      y = "Share of observations (%)"
    ) +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 45, hjust = 1),
      legend.position = "top",
      legend.text     = ggplot2::element_text(size = 9)
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = length(sources)))
}


# Series key shared by the bar chart and the ridge plot: one entry per survey
# wave x series (sample / historical).
#' @noRd
.wx_series_key <- function(wave, source) paste0(wave, " - ", source)

# Wave-major grid of every wave x series combination, in plotting order, so
# colour lookups are built alongside the keys rather than parsed back out of
# them.
#' @noRd
.wx_series_grid <- function(waves, sources) {
  g <- expand.grid(source = sources, wave = waves,
                   KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  g$key <- .wx_series_key(g$wave, g$source)
  g[, c("wave", "source", "key")]
}


#' Ridge plot of a continuous weather variable, sample against its own history
#'
#' One row per survey wave: the wave's own weather as a filled density in the
#' wave's colour, with the same locations' historical distribution drawn over
#' it as a dashed outline. Both are on one shared height scale, so the two
#' curves in a row are directly comparable.
#'
#' The historical density is weighted by the number of sampled households
#' behind each location-month cell, so it is composed like the sample rather
#' than like the raw weather grid.
#'
#' @param df        Data frame with `countryyear` and a numeric `hv` column
#'   (the continuous series, even when the variable is binned for modelling).
#' @param hv        Scalar character. Name of the weather variable column.
#' @param label     Scalar character. Human-readable label for the x-axis.
#' @param hist_df   Optional data frame from `join_hist_sample_cells()`. When
#'   `NULL` only the sample ridges are drawn.
#' @param year_from,year_to Integer calendar years bounding the historical
#'   series (inclusive).
#'
#' @return A `ggplot` object, or `NULL` invisibly when there is nothing to plot.
#'
#' @export
plot_weather_ridges_compare <- function(df, hv, label, hist_df = NULL,
                                        year_from = NULL, year_to = NULL) {
  if (is.null(df) || is.na(hv) || !(hv %in% names(df))) return(invisible(NULL))
  if (!("countryyear" %in% names(df))) return(invisible(NULL))

  sv   <- suppressWarnings(as.numeric(df[[hv]]))
  keep <- is.finite(sv)
  if (!any(keep)) return(invisible(NULL))

  samp <- data.frame(
    countryyear = as.character(df$countryyear[keep]),
    x           = sv[keep],
    w           = 1,
    source      = .wx_sample_lab,
    stringsAsFactors = FALSE
  )

  hist_lab <- .wx_hist_lab(year_from, year_to)
  hist_use <- NULL
  if (!is.null(hist_df) && !is.na(hv) && hv %in% names(hist_df) &&
      all(c("n_hh", "countryyear") %in% names(hist_df))) {
    hv_vals <- suppressWarnings(as.numeric(hist_df[[hv]]))
    hkeep   <- is.finite(hv_vals)
    if (!is.null(year_from) && !is.null(year_to) &&
        "cal_year" %in% names(hist_df)) {
      hkeep <- hkeep &
        hist_df$cal_year >= as.integer(year_from) &
        hist_df$cal_year <= as.integer(year_to)
    }
    # A density needs something to smooth over; a couple of cells would draw a
    # spike that says more about the bandwidth than about the climate.
    if (sum(hkeep) >= 10) {
      hist_use <- data.frame(
        countryyear = as.character(hist_df$countryyear[hkeep]),
        x           = hv_vals[hkeep],
        w           = as.numeric(hist_df$n_hh[hkeep]),
        source      = hist_lab,
        stringsAsFactors = FALSE
      )
      # Only keep waves the sample also has, so no row appears with a
      # historical curve and no sample curve.
      hist_use <- hist_use[hist_use$countryyear %in% samp$countryyear, ,
                           drop = FALSE]
      if (nrow(hist_use) == 0) hist_use <- NULL
    }
  }

  d <- if (is.null(hist_use)) samp else rbind(samp, hist_use)

  waves   <- sort(unique(samp$countryyear))
  pal     <- .wave_palette(waves)
  sources <- if (is.null(hist_use)) .wx_sample_lab else
    c(.wx_sample_lab, hist_lab)

  # The sample is a filled ridge in the wave's colour; the history is drawn
  # over it as an unfilled dashed outline in a darker shade of the same colour,
  # so the pair reads as one wave rather than two unrelated series.
  series <- .wx_series_grid(waves, sources)
  fills  <- stats::setNames(
    ifelse(series$source == .wx_sample_lab, pal[series$wave], NA_character_),
    series$key
  )
  lines <- stats::setNames(
    ifelse(series$source == .wx_sample_lab, "grey30",
           .blend_colour(pal[series$wave], "black", 0.35)),
    series$key
  )

  d$countryyear <- factor(d$countryyear, levels = waves)
  d$source      <- factor(d$source, levels = sources)
  d$key         <- factor(.wx_series_key(as.character(d$countryyear),
                                         as.character(d$source)),
                          levels = series$key)

  # Pre-computing the bandwidth silences ggridges' "Picking joint bandwidth"
  # message without changing the visual.
  bw <- tryCatch(stats::bw.nrd0(d$x), error = function(e) NULL)
  if (is.null(bw) || !is.finite(bw) || bw <= 0) bw <- NULL

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x        = .data$x,
      y        = .data$countryyear,
      weight   = .data$w,
      group    = .data$key,
      fill     = .data$key,
      colour   = .data$key,
      linetype = .data$source
    )
  ) +
    ggridges::geom_density_ridges(
      alpha = 0.7, scale = 2, bandwidth = bw, linewidth = 0.5
    ) +
    ggplot2::scale_fill_manual(values = fills, na.value = NA, guide = "none") +
    ggplot2::scale_colour_manual(values = lines, guide = "none") +
    ggplot2::scale_linetype_manual(
      values = stats::setNames(
        c("solid", "22")[seq_along(sources)], sources
      ),
      name = NULL
    ) +
    theme_wise() +
    ggplot2::labs(
      x = stringr::str_wrap(paste0(label, "\n(as configured)"), 40),
      y = ""
    ) +
    ggplot2::theme(
      legend.position = if (length(sources) > 1) "top" else "none",
      legend.text     = ggplot2::element_text(size = 9)
    )

  p
}


#' Plot the distribution of a weather variable
#'
#' For binned variables renders a dodged bar chart of bin shares by
#' `countryyear`. For continuous variables renders a ridge density plot. Both
#' can carry the wave's own climate history alongside the sample - pass
#' `hist_df` (and, for the bar chart, the `breaks` the sample was binned on).
#'
#' @param df          A data frame with a `countryyear` column and a column
#'   named `hv`.
#' @param hv          Scalar character. Name of the weather variable column.
#' @param label       Scalar character. Human-readable label for the x-axis.
#' @param cont_binned One of `"Binned"` or `"Continuous"` (or `NA`).
#' @param hist_df     Optional data frame from `join_hist_sample_cells()`.
#' @param breaks      Numeric break vector for `hv`, from `stored_breaks`.
#'   Only used for binned variables.
#' @param year_from,year_to Integer calendar years bounding the historical
#'   series (inclusive).
#'
#' @return A `ggplot` object, or `NULL` invisibly when `hv` is absent or `NA`.
#'
#' @export
plot_weather_dist <- function(df, hv, label, cont_binned, hist_df = NULL,
                              breaks = NULL, year_from = NULL,
                              year_to = NULL) {
  if (is.null(df) || is.na(hv) || !(hv %in% names(df))) return(invisible(NULL))

  if (!is.na(cont_binned) && cont_binned == "Binned") {
    plot_weather_bins_compare(
      df = df, hv = hv, label = label, hist_df = hist_df, breaks = breaks,
      year_from = year_from, year_to = year_to
    )
  } else {
    plot_weather_ridges_compare(
      df = df, hv = hv, label = label, hist_df = hist_df,
      year_from = year_from, year_to = year_to
    )
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
#' are handled by `get_weather()` itself - the rolling window is applied
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
#' * keeps only the calendar months the wave was fielded in - per wave, so
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


# ---------------------------------------------------------------------------- #
# Weather-by-location map                                                       #
# ---------------------------------------------------------------------------- #

#' Prepare the shared grouping behind `summarise_weather_by_loc()`
#'
#' The location grouping (year coercion, economy default, interview months,
#' location grouping) is identical for every weather variable, so the Step 1
#' weather map computes it once per survey frame and passes it back in via
#' `summarise_weather_by_loc(prep = )` instead of rebuilding it once per
#' variable (PERF-25). Since PERF-05 the grouping itself is a single
#' `collapse::GRP()` over the frame, shared by every variable.
#'
#' Rows with a missing location key are dropped here, matching the rows the
#' previous `interaction()` + `split()` grouping silently discarded.
#'
#' @param survey_weather Merged survey-weather frame (household level).
#'
#' @return A list with `df` (the normalised frame, missing-key rows dropped),
#'   `months`, and `grp` (a `collapse::GRP()` grouping over `df`). `NULL` when
#'   the input is `NULL`.
#' @noRd
.summarise_loc_prep <- function(survey_weather) {
  if (is.null(survey_weather)) return(NULL)

  df <- survey_weather
  df$year <- as.character(df$year)
  if (!"economy" %in% names(df)) df$economy <- df$code

  keys <- c("code", "year", "survname", "loc_id")
  df <- df[complete.cases(df[keys]), , drop = FALSE]

  months <- if ("timestamp" %in% names(df)) {
    as.integer(format(as.Date(df$timestamp), "%m"))
  } else {
    rep(1L, nrow(df))
  }

  grp <- collapse::GRP(df, by = keys, group.sizes = TRUE)

  list(df = df, months = months, grp = grp)
}


#' Collapse a weather variable to one value per survey location
#'
#' The merged survey-weather frame holds one weather value per location *and
#' interview month*, so a location visited across several months carries
#' several values. Mapping needs a single value per location: continuous
#' variables are averaged and binned variables take their modal bin - the same
#' convention `.compute_hazard_values()` uses elsewhere in the app.
#'
#' @param survey_weather Merged survey-weather frame (household level).
#' @param hv Scalar character. Name of the weather variable column.
#' @param prep Optional result of `.summarise_loc_prep()` on the same frame.
#'   Pass it when collapsing several variables over the same frame so the
#'   grouping is built once (PERF-25). Since PERF-05 the collapse itself runs
#'   as grouped `collapse` passes over that shared grouping.
#'
#' @return A data frame with one row per wave x location: `code`, `year`,
#'   `survname`, `economy`, `loc_id`, `value`, `n_hh`, `n_months`, plus a
#'   `binned` attribute (logical) and, for binned variables, a `levels`
#'   attribute holding the bin order. `NULL` when `hv` is absent.
#'
#' @export
summarise_weather_by_loc <- function(survey_weather, hv, prep = NULL) {
  keys <- c("code", "year", "survname", "loc_id")
  if (is.null(survey_weather) || is.na(hv) ||
      !(hv %in% names(survey_weather)) ||
      !all(keys %in% names(survey_weather))) {
    return(NULL)
  }

  if (is.null(prep)) prep <- .summarise_loc_prep(survey_weather)
  df     <- prep$df
  months <- prep$months
  g      <- prep$grp
  vals   <- df[[hv]]

  binned <- is.factor(vals) || is.character(vals)
  lvls   <- if (!binned) NULL
            else if (is.factor(vals)) levels(vals)
            else sort(unique(as.character(vals)))

  n_g <- g$N.groups
  first_idx <- match(seq_len(n_g), g$group.id)

  out <- data.frame(
    code     = df$code[first_idx],
    year     = df$year[first_idx],
    survname = df$survname[first_idx],
    economy  = df$economy[first_idx],
    loc_id   = df$loc_id[first_idx],
    value    = if (binned) rep(NA_character_, n_g) else rep(NA_real_, n_g),
    n_hh     = as.integer(g$group.sizes),
    n_months = as.integer(collapse::fndistinct(months, g = g, na.rm = FALSE)),
    stringsAsFactors = FALSE
  )

  if (binned) {
    # Modal bin per location: unweighted counts of the non-NA values, ties
    # broken by the alphabetical order `table()` used before (PERF-05).
    vv <- as.character(vals)
    ok <- !is.na(vv)
    if (any(ok)) {
      gv <- collapse::GRP(
        list(gid = g$group.id[ok], value = vv[ok]),
        group.sizes = TRUE
      )
      cnt <- as.integer(gv$group.sizes)
      ord <- order(gv$groups$gid, -cnt, match(gv$groups$value, sort(unique(vv[ok]))))
      take <- ord[!duplicated(gv$groups$gid[ord])]
      out$value[gv$groups$gid[take]] <- gv$groups$value[take]
    }
  } else {
    m <- suppressWarnings(collapse::fmean(as.numeric(vals), g = g, na.rm = TRUE))
    m[is.nan(m)] <- NA_real_
    out$value <- unname(m)
  }

  # `interaction()` ordered its levels with `code` varying fastest; restore
  # that presentation order (GRP() sorts lexicographically instead).
  out <- out[order(out$loc_id, out$survname, out$year, out$code), ]
  rownames(out) <- NULL
  attr(out, "binned") <- binned
  attr(out, "levels") <- lvls
  out
}


#' Compare each location's wave weather with its own history
#'
#' Where `summarise_weather_by_loc()` gives the value the sample experienced
#' (a cross-sectional view - how locations compare with each other), this puts
#' each location against *itself*: how far the wave's weather sat from what
#' that same location normally gets in the same calendar months.
#'
#' * `measure = "anomaly"` - the wave value minus the location's mean over the
#'   historical years, in the variable's own units.
#' * `measure = "percentile"` - where the wave value falls within the
#'   location's own historical distribution, 0-100 (50 = a typical year).
#'
#' Both the wave value and the historical reference are household-weighted by
#' the `n_hh` of each location-month cell, so a location's month composition
#' is the same on both sides of the comparison. The historical window includes
#' the survey year itself, matching the histogram in the same tab.
#'
#' @param cells_df  Data frame from `join_hist_sample_cells()`.
#' @param hv        Scalar character. Name of the weather variable column.
#' @param year_from,year_to Integer calendar years bounding the historical
#'   reference (inclusive).
#' @param measure   `"anomaly"` or `"percentile"`.
#'
#' @return A data frame shaped like `summarise_weather_by_loc()` - `code`,
#'   `year`, `survname`, `economy`, `loc_id`, `value`, `n_hh`, `n_months` -
#'   with a `binned` attribute of `FALSE`. `NULL` when nothing can be
#'   computed.
#'
#' @export
summarise_weather_anomaly_by_loc <- function(cells_df, hv, year_from, year_to,
                                             measure = c("anomaly", "percentile")) {
  measure <- match.arg(measure)
  keys    <- c("code", "year", "survname", "loc_id", "cal_year", "is_sample")
  if (is.null(cells_df) || is.na(hv) || !(hv %in% names(cells_df)) ||
      !all(keys %in% names(cells_df))) {
    return(NULL)
  }

  v <- suppressWarnings(as.numeric(cells_df[[hv]]))
  d <- cells_df[is.finite(v), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  d$.v <- v[is.finite(v)]
  d$.w <- if ("n_hh" %in% names(d)) d$n_hh else 1
  if (!"economy" %in% names(d)) d$economy <- d$code
  if (!"int_month" %in% names(d)) {
    d$int_month <- as.integer(format(as.Date(d$timestamp), "%m"))
  }

  in_range <- d$cal_year >= as.integer(year_from) &
    d$cal_year <= as.integer(year_to)

  keys <- c("code", "year", "survname", "loc_id")
  keep <- complete.cases(d[keys])   # interaction()/split() dropped NA-key rows
  if (!any(keep)) return(NULL)
  d  <- d[keep, , drop = FALSE]
  # NA cal_year was never in the old in-window sums (na.rm skipped it), so
  # it counts as out-of-window here
  hi <- !is.na(in_range[keep]) & in_range[keep]
  si <- isTRUE_vec(d$is_sample)

  g   <- collapse::GRP(d, by = keys)
  gid <- as.integer(g$group.id)
  n_g <- g$N.groups
  first_idx <- match(seq_len(n_g), gid)
  vv <- d$.v
  w  <- as.numeric(d$.w)

  # --- sample-side stats: one grouped pass over the wave's own rows ---------
  # stats::weighted.mean(na.rm = TRUE) strips NA values but an NA *weight*
  # poisons the result, so samp is a plain weighted ratio over the sample
  # rows (groups with an NA sample weight come out NA and are dropped below,
  # exactly as the old per-group loop did).
  si_rows <- which(si)
  g_si    <- collapse::GRP(list(gid = gid[si_rows]))
  samp_full  <- rep(NA_real_, n_g)
  n_hh_full  <- rep(NA_real_, n_g)
  n_mn_full  <- rep(NA_integer_, n_g)
  if (length(si_rows)) {
    gid_si <- as.integer(g_si$groups$gid)
    vw     <- vv[si_rows] * w[si_rows]
    samp_full[gid_si] <- as.numeric(
      collapse::fsum(vw, g = g_si, na.rm = FALSE) /
        collapse::fsum(w[si_rows], g = g_si, na.rm = FALSE)
    )
    n_hh_full[gid_si] <- as.numeric(
      collapse::fsum(w[si_rows], g = g_si, na.rm = TRUE)
    )
    n_mn_full[gid_si] <- as.integer(
      collapse::fndistinct(d$int_month[si_rows], g = g_si, na.rm = FALSE)
    )
  }

  # --- historical-side stats: one grouped pass over the window rows ---------
  hi_rows <- which(hi)
  g_hi    <- collapse::GRP(list(gid = gid[hi_rows]))
  samp_row <- samp_full[gid]

  value <- if (measure == "percentile") {
    # sum(..., na.rm = TRUE) skips NA weights here (they never entered the
    # old numerator/denominator sums)
    denom <- rep(NA_real_, n_g)
    denom[as.integer(g_hi$groups$gid)] <- as.numeric(
      collapse::fsum(w[hi_rows], g = g_hi, na.rm = TRUE)
    )
    num <- rep(NA_real_, n_g)
    num[as.integer(g_hi$groups$gid)] <- as.numeric(collapse::fsum(
      w[hi_rows] * (vv[hi_rows] < samp_row[hi_rows]),
      g = g_hi, na.rm = TRUE
    ))
    num_eq <- rep(NA_real_, n_g)
    num_eq[as.integer(g_hi$groups$gid)] <- as.numeric(collapse::fsum(
      w[hi_rows] * (vv[hi_rows] == samp_row[hi_rows]),
      g = g_hi, na.rm = TRUE
    ))
    # Mid-rank so an exact tie sits at the middle of its own mass.
    100 * (num + 0.5 * num_eq) / denom
  } else {
    ref <- rep(NA_real_, n_g)
    ref[as.integer(g_hi$groups$gid)] <- as.numeric(
      collapse::fsum(vv[hi_rows] * w[hi_rows], g = g_hi, na.rm = FALSE) /
        collapse::fsum(w[hi_rows], g = g_hi, na.rm = FALSE)
    )
    samp_full - ref
  }

  # Groups the old loop dropped: no sample rows, no window rows, or a
  # degenerate window weight sum.
  ok_g <- is.finite(value)
  if (!any(ok_g)) return(NULL)

  out <- data.frame(
    code     = d$code[first_idx],
    year     = d$year[first_idx],
    survname = d$survname[first_idx],
    economy  = d$economy[first_idx],
    loc_id   = d$loc_id[first_idx],
    value    = value,
    n_hh     = n_hh_full,
    n_months = n_mn_full,
    stringsAsFactors = FALSE
  )
  out <- out[ok_g, ]

  # interaction() ordered its levels with `code` varying fastest; restore
  # that presentation order (GRP() sorts lexicographically instead).
  out <- out[order(out$loc_id, out$survname, out$year, out$code), ]
  rownames(out) <- NULL
  attr(out, "binned") <- FALSE
  attr(out, "levels") <- NULL
  out
}


# Vectorised isTRUE for a logical column that may carry NAs.
#' @noRd
isTRUE_vec <- function(x) !is.na(x) & x


#' Compact map legend
#'
#' `leaflet::addLegend()` renders a tall colour ramp with a full-width title,
#' which swamps the small per-wave maps. This builds a small fixed-width
#' block instead: a short title, a thin horizontal ramp (or a few swatches for
#' bins) and two or three tick labels, with the long explanation moved into an
#' info marker's hover text.
#'
#' @param pal_info  Palette list from `.weather_map_palette()`.
#' @param binned    Logical. Bin swatches rather than a continuous ramp.
#' @param levels    Character vector of bin levels, in order.
#' @param title     Short title, kept to a couple of words.
#' @param info      Full explanation, shown on hover.
#'
#' @return An HTML string.
#' @noRd
.compact_legend_html <- function(pal_info, binned, levels = NULL,
                                 title = "", info = "") {
  head <- paste0(
    .wx_tip_css(),
    '<div style="font-weight: 600; white-space: nowrap;">',
    .html_escape(title), .wx_info_marker(info), '</div>'
  )

  body <- if (binned) {
    lv   <- levels %||% character(0)
    rows <- vapply(lv, function(l) {
      paste0(
        '<div style="white-space: nowrap;"><span style="display: inline-block; ',
        'width: 10px; height: 10px; background: ', pal_info$pal(l),
        '; border: 1px solid #999; vertical-align: -1px;"></span> ',
        .html_escape(l), '</div>'
      )
    }, character(1))
    paste(rows, collapse = "")
  } else {
    dom <- pal_info$domain
    # Sampling the ramp evenly in value space is wrong whenever the palette is
    # non-linear: on a log scale two thirds of the colour range would land in
    # the first eighth of the bar. Callers with a transformed scale pass the
    # values that are evenly spaced in *colour* space instead.
    stops <- pal_info$stops %||% seq(dom[1], dom[2], length.out = 9)
    grad  <- paste(pal_info$pal(stops), collapse = ", ")
    # On a non-linear ramp the middle of the bar is not the middle of the
    # range, so the caller can say which value sits there.
    mid   <- pal_info$mid %||% mean(dom)
    lab   <- function(x) format(signif(x, 3), trim = TRUE)
    paste0(
      '<div style="width: 108px; height: 8px; border: 1px solid #bbb; ',
      'background: linear-gradient(to right, ', grad, ');"></div>',
      '<div style="width: 110px; display: flex; justify-content: space-between;">',
      '<span>', lab(dom[1]), '</span><span>', lab(mid), '</span>',
      '<span>', lab(dom[2]), '</span></div>'
    )
  }

  paste0(
    '<div style="background: rgba(255,255,255,0.88); padding: 3px 5px; ',
    'border-radius: 4px; font-size: 10px; line-height: 1.3; color: #333; ',
    'max-width: 160px;">', head, body, '</div>'
  )
}


# Info marker for the map legends.
#
# The browser's native `title` tooltip only appears after a ~1s delay and puts
# a question-mark cursor on the element, which reads as a broken control. This
# uses a CSS tooltip instead: it shows on hover (and on keyboard focus) with no
# delay, and the marker itself is always visible.
#' @param side Which map corner the marker sits in - the tooltip opens away
#'   from that edge so it is not clipped.
#' @noRd
.wx_info_marker <- function(info, side = c("right", "left")) {
  if (!nzchar(info %||% "")) return("")
  side <- match.arg(side)
  paste0(
    '<span class="wx-tip', if (side == "left") " wx-tip-l" else "",
    '" tabindex="0" data-tip="', .html_escape(info), '">i</span>'
  )
}

# Styles for the marker above. Emitted inside the legend control; duplicated
# blocks across maps are harmless and keep each map self-contained.
#' @noRd
.wx_tip_css <- function() {
  paste0(
    '<style>',
    '.wx-tip{position:relative;display:inline-block;width:12px;height:12px;',
    'line-height:12px;text-align:center;border:1px solid #888;',
    'border-radius:50%;font-size:9px;font-weight:700;font-style:normal;',
    'color:#555;margin-left:3px;cursor:pointer;background:#fff;}',
    '.wx-tip::after{content:attr(data-tip);position:absolute;bottom:150%;',
    'right:-4px;width:210px;background:rgba(33,33,33,0.96);color:#fff;',
    'padding:6px 8px;border-radius:4px;font-size:11px;font-weight:400;',
    'line-height:1.35;white-space:normal;text-align:left;opacity:0;',
    'visibility:hidden;pointer-events:none;z-index:1200;',
    'transition:opacity 0.06s linear;}',
    '.wx-tip.wx-tip-l::after{right:auto;left:-4px;}',
    '.wx-tip:hover::after,.wx-tip:focus::after{opacity:1;visibility:visible;}',
    '</style>'
  )
}


# Minimal HTML escaping for text placed into legend markup / title attributes.
#' @noRd
.html_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;",  x, fixed = TRUE)
  x <- gsub(">", "&gt;",  x, fixed = TRUE)
  gsub('"', "&quot;", x, fixed = TRUE)
}


#' Colour palette for a weather variable, matching its configuration
#'
#' Binned variables get a sequential ramp across the bin levels in order.
#' Continuous variables get a sequential ramp over their range, or a diverging
#' ramp centred on zero when the variable is configured as a deviation from
#' mean or a standardised anomaly (where the sign is what matters).
#'
#' @param values         Value vector (numeric, or character/factor bins).
#' @param binned         Logical. Treat `values` as bins.
#' @param levels         Character vector of bin levels, in order.
#' @param transformation The variable's configured transformation.
#' @param force          `"diverging"` or `"sequential"` to override what the
#'   transformation implies - used by the anomaly and percentile map views,
#'   whose scale type follows the view rather than the variable.
#' @param domain         Optional length-2 numeric to fix the colour domain
#'   (e.g. `c(0, 100)` for percentiles) instead of deriving it from `values`.
#'
#' @return A list with `pal` (a leaflet palette function), `domain` (values to
#'   pass to `addLegend`) and `diverging` (logical).
#'
#' @noRd
.weather_map_palette <- function(values, binned, levels = NULL,
                                 transformation = "None",
                                 force = NULL, domain = NULL) {
  if (binned) {
    lv <- levels %||% sort(unique(as.character(values)))
    # Trim the palest stops: a near-white bin is invisible against the light
    # basemap, which reads as a hole in the map rather than as a low value.
    n_lv <- max(length(lv), 2L)
    ramp <- rev(grDevices::hcl.colors(n_lv + 2L, "YlOrRd"))[seq(2L, n_lv + 1L)]
    return(list(
      pal = leaflet::colorFactor(
        palette = ramp, levels = lv, na.color = "#cccccc"
      ),
      # A factor, not a bare character vector: addLegend sorts its values, and
      # bin labels like "[-Inf,29.5]" sort after "(29.5,31]" as plain text.
      domain    = factor(lv, levels = lv),
      diverging = FALSE
    ))
  }

  v <- suppressWarnings(as.numeric(values))
  v <- v[is.finite(v)]
  if (length(v) == 0) v <- c(0, 1)

  diverging <- if (!is.null(force)) {
    identical(force, "diverging")
  } else {
    !is.na(transformation) &&
      transformation %in% c("Deviation from mean", "Standardized anomaly")
  }

  if (diverging) {
    dom <- domain
    if (is.null(dom)) {
      lim <- max(abs(v), na.rm = TRUE)
      if (!is.finite(lim) || lim == 0) lim <- 1
      dom <- c(-lim, lim)
    }
    pal <- leaflet::colorNumeric("RdBu", domain = dom, reverse = TRUE,
                                 na.color = "#cccccc")
  } else {
    dom <- domain
    if (is.null(dom)) {
      dom <- range(v, na.rm = TRUE)
      if (diff(dom) == 0) dom <- dom + c(-0.5, 0.5)
    }
    pal <- leaflet::colorNumeric(
      rev(grDevices::hcl.colors(11, "YlOrRd"))[2:10],
      domain = dom, na.color = "#cccccc"
    )
  }

  list(pal = pal, domain = dom, diverging = diverging)
}


#' Map survey locations shaded by a weather variable
#'
#' Draws one survey wave's locations, filled by the weather value the sample
#' experienced there - the bin for binned variables, the value on its
#' configured scale (raw, deviation from mean, standardised anomaly) for
#' continuous ones.
#'
#' The shaded value is the same quantity that enters the model as a regressor.
#' `fit_model()` puts the weather column on the right-hand side unchanged, so
#' a household's regressor is the value for its location in its own interview
#' month, with the configured temporal aggregation and transformation already
#' applied. For a location surveyed in a single interview month - the majority
#' - every household there shares that one value and the colour *is* the
#' regressor. A location surveyed across several interview months holds
#' several values, so its colour is the household-weighted mean (or modal
#' bin); those locations are drawn with a dotted outline to mark the colour as
#' a summary rather than a single household's input.
#'
#' @param geojson  GeoJSON FeatureCollection of survey locations, as built by
#'   `mod_1_02_surveystats_server()`. Features carry `code`, `year`,
#'   `survname` and `loc_id` properties plus a raw `geom_json` string.
#' @param loc_vals One wave's rows from `summarise_weather_by_loc()`.
#' @param label    Scalar character. Legend / popup label for the variable.
#' @param transformation The variable's configured transformation.
#' @param pal_info Optional palette from `.weather_map_palette()`. Pass a
#'   shared one to keep the colour scale comparable across waves.
#' @param legend_title Short legend heading - a couple of words, since the
#'   per-wave maps are small. Defaults to `label`.
#' @param legend_info Longer explanation shown when hovering the legend's info
#'   marker. Defaults to `label`.
#'
#' @return A `leaflet` widget, or `NULL` invisibly when there is nothing to
#'   draw.
#'
#' @export
plot_weather_loc_map <- function(geojson, loc_vals, label,
                                 transformation = "None", pal_info = NULL,
                                 legend_title = NULL, legend_info = NULL) {
  if (is.null(geojson) || length(geojson$features) == 0) return(invisible(NULL))
  if (is.null(loc_vals) || nrow(loc_vals) == 0) return(invisible(NULL))

  binned <- isTRUE(attr(loc_vals, "binned"))
  lvls   <- attr(loc_vals, "levels")

  # Keep this wave's features - all of them, not just the ones carrying a
  # value. Filtering on the individual area id used to drop any area without
  # a weather value, punching holes in a footprint that the outcome map draws
  # whole; areas with no value are shown in neutral grey instead.
  wave_keys  <- unique(paste(loc_vals$code, loc_vals$year, loc_vals$survname,
                             sep = "\r"))
  feat_waves <- vapply(geojson$features, function(f) {
    p <- f$properties
    paste(as.character(p$code), as.character(p$year),
          as.character(p$survname), sep = "\r")
  }, character(1))

  feats <- geojson$features[feat_waves %in% wave_keys]
  if (length(feats) == 0) return(invisible(NULL))

  val_by_loc <- stats::setNames(loc_vals$value, as.character(loc_vals$loc_id))
  n_by_loc   <- stats::setNames(loc_vals$n_hh, as.character(loc_vals$loc_id))
  mon_by_loc <- stats::setNames(loc_vals$n_months %||% rep(1L, nrow(loc_vals)),
                                as.character(loc_vals$loc_id))

  if (is.null(pal_info)) {
    pal_info <- .weather_map_palette(loc_vals$value, binned, lvls, transformation)
  }
  pal <- pal_info$pal

  # Areas without a value are drawn too (see above), so every lookup has to
  # tolerate a missing key - `[[` errors on one, unlike `[`.
  lookup <- function(v, key) {
    i <- match(key, names(v))
    if (is.na(i)) NULL else v[[i]]
  }

  fmt <- function(v) {
    if (binned || is.character(v)) as.character(v) else format(round(v, 2), nsmall = 2)
  }

  # Group features by fill colour, and by whether the value is a single
  # interview month's value or an average across several. Each combination
  # costs one addGeoJSON call rather than one call per location.
  cols <- vapply(feats, function(f) {
    v <- lookup(val_by_loc, as.character(f$properties$loc_id))
    if (is.null(v) || (is.numeric(v) && !is.finite(v)) || is.na(v)) {
      "#cccccc"
    } else {
      pal(if (binned) as.character(v) else as.numeric(v))
    }
  }, character(1))

  # A location surveyed across several interview months has several weather
  # values behind it; its colour is a summary of them rather than the value
  # any single household contributes to the regression. Draw those dotted.
  averaged <- vapply(feats, function(f) {
    isTRUE((lookup(mon_by_loc, as.character(f$properties$loc_id)) %||% 1L) > 1L)
  }, logical(1))

  bounds <- .geojson_bounds(list(features = feats))

  # Same rendering as the other cell maps: no outlines (at a country view the
  # borders of a thousand hexagons cover more pixels than the fills do), and
  # canvas rather than SVG so panning stays smooth with that many polygons.
  m <- leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

  for (cl in unique(cols)) {
    {
      sel <- cols == cl
      grp <- feats[sel]
      fj  <- vapply(grp, function(f) {
        lid   <- as.character(f$properties$loc_id)
        v     <- lookup(val_by_loc, lid)
        nm    <- lookup(mon_by_loc, lid) %||% 1L
        props <- f$properties
        props$popup <- paste0(
          "<b>", htmltools::htmlEscape(label), "</b><br/>",
          if (is.null(v) || all(is.na(v))) "no value" else htmltools::htmlEscape(fmt(v)),
          "<br/><small>loc ", htmltools::htmlEscape(lid), " &middot; ",
          lookup(n_by_loc, lid) %||% 0, " households",
          if (isTRUE(nm > 1L)) {
            paste0("<br/>surveyed over ", nm, " interview months - shown value",
                   " is the household-weighted ",
                   if (binned) "modal bin" else "mean",
                   " across them")
          } else {
            "<br/>single interview month"
          },
          "</small>"
        )
        sprintf('{"type":"Feature","geometry":%s,"properties":%s}',
                f$geom_json, jsonlite::toJSON(props, auto_unbox = TRUE))
      }, character(1L))

      m <- m |>
        leaflet::addGeoJSON(
          geojson     = sprintf('{"type":"FeatureCollection","features":[%s]}',
                                paste(fj, collapse = ",")),
          stroke      = FALSE,
          weight      = 0,
          fillColor   = cl,
          fillOpacity = 0.75
        )
    }
  }

  m <- m |>
    leaflet::fitBounds(
      lng1 = bounds$lng1, lat1 = bounds$lat1,
      lng2 = bounds$lng2, lat2 = bounds$lat2
    )

  # Areas with no weather value are drawn in neutral grey rather than left
  # out, so a gap in the weather is visible as a gap rather than as absent
  # geography. Count them so the size of the gap is legible.
  n_missing <- sum(cols == "#cccccc")

  # Areas whose value averages several interview months are no longer marked
  # on the map itself - per-area marks turned out to be more distracting than
  # informative. Instead, state how many there are.
  if (any(averaged) || n_missing > 0) {
    n_avg <- sum(averaged)
    missing_line <- if (n_missing > 0) {
      paste0(
        '<div style="white-space: nowrap;">',
        '<span style="display: inline-block; width: 10px; height: 10px; ',
        'background: #cccccc; border: 1px solid #aaa; ',
        'vertical-align: -1px;"></span> ',
        n_missing, " of ", length(feats), " areas without weather",
        .wx_info_marker(
          paste0(
            n_missing, " of the ", length(feats), " areas shown have no",
            " weather value for this variable - the survey reached them, but",
            " the weather series did not, so they drop out of the merge that",
            " feeds the model. They are shaded grey rather than left off the",
            " map so the gap is visible."
          ),
          side = "left"
        ),
        '</div>'
      )
    } else ""

    avg_line <- if (n_avg > 0) {
      paste0(
        '<div style="white-space: nowrap;">',
        n_avg, " of ", length(feats), " areas averaged",
        .wx_info_marker(
            paste0(
                n_avg, " of the ", length(feats), " area",
              if (length(feats) == 1) "" else "s",
              " shown draw on several interview months and so have several",
              " weather values behind them. Their colour is the area's",
              " household-weighted ",
              if (binned) "modal bin" else "mean",
              " across those household-month observations, rather than the",
              " single value every household there contributes to the model."
            ),
            side = "left"   # this control sits bottom-left
          ),
          '</div>'
      )
    } else ""

    m <- m |>
      leaflet::addControl(
        position = "bottomleft",
        html = paste0(
          '<div style="background: rgba(255,255,255,0.88); padding: 3px 5px; ',
          'border-radius: 4px; font-size: 10px; line-height: 1.35; ',
          'color: #333;">',
          missing_line, avg_line,
          '</div>'
        )
      )
  }

  m <- m |>
    leaflet::addControl(
      position = "bottomright",
      html = .compact_legend_html(
        pal_info = pal_info,
        binned   = binned,
        levels   = lvls,
        title    = legend_title %||% stringr::str_replace_all(label, "\n", " "),
        info     = legend_info %||% stringr::str_replace_all(label, "\n", " ")
      )
    )

  # Keeps the map correct when its card is expanded to full screen, scales the
  # averaged-area dots with zoom, and offers a way back to the opening view.
  m |>
    .add_reset_button(bounds) |>
    htmlwidgets::onRender(.map_autofit_js(bounds, dashes = TRUE))
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
    # in one grouped pass (PERF-09)
    if ("countryyear" %in% names(tab) && "variable" %in% names(tab)) {
      miss_df <- survey_missingness_long(df, vars)
      tab <- dplyr::left_join(tab, miss_df, by = c("countryyear", "variable"))
    }

    # Show only the readable variable label, falling back to the raw name
    if ("variable" %in% names(tab)) {
      lab_map <- sw |>
        dplyr::select(name, label) |>
        dplyr::distinct()
      tab <- tab |>
        dplyr::left_join(lab_map, by = c("variable" = "name")) |>
        dplyr::mutate(variable = dplyr::coalesce(.data$label, .data$variable)) |>
        dplyr::select(variable, dplyr::everything(), -dplyr::any_of("label"))
    }

    # Rename key columns
    if ("variable" %in% names(tab))       names(tab)[names(tab) == "variable"] <- "Variable"
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

    # One grouped pass for every binned variable's missingness (PERF-09)
    miss_all <- survey_missingness_long(df, binned_vars)

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

      miss_df <- miss_all[miss_all$variable == v,
                          c("countryyear", "% Missing"), drop = FALSE]

      counts |> dplyr::left_join(miss_df, by = "countryyear")
    })

    tab <- dplyr::bind_rows(rows_list)
    if (nrow(tab) == 0) {
      return(data.frame(Note = "No binned weather observations found."))
    }

    # Show only the readable variable label, falling back to the raw name
    if ("variable" %in% names(tab) &&
        all(c("name", "label") %in% names(sw))) {
      lab_map <- sw |>
        dplyr::select(.data$name, .data$label) |>
        dplyr::distinct()
      tab <- tab |>
        dplyr::left_join(lab_map, by = c("variable" = "name")) |>
        dplyr::mutate(
          variable = dplyr::coalesce(.data$label, .data$variable)
        ) |>
        dplyr::select(.data$variable, .data$countryyear, .data$level,
                      .data$N, .data$share, .data$`% Missing`)
    }

    # Sort: by variable, country-year, then by level (factor order if available)
    tab <- tab |>
      dplyr::arrange(.data$variable, .data$countryyear, .data$level)

    if ("variable" %in% names(tab))
      names(tab)[names(tab) == "variable"] <- "Variable"
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
#' @param ids      Character vector of length 2 - output IDs for plot 1
#'                 and plot 2. Only `ids[1]` is used when `n_vars < 2`.
#' @param height   CSS height passed to `shiny::plotOutput`.
#' @param alts     Optional character vector of alt texts (UI-36), one per
#'                 plot id; entries beyond `n_vars` are unused.
#'
#' @return A Shiny tag.
#' @noRd
weather_plot_layout <- function(ns, n_vars, ids, height = "500px",
                                alts = NULL) {
  plot_at <- function(i) {
    alt <- if (!is.null(alts) && length(alts) >= i &&
               !is.na(alts[i]) && nzchar(alts[i])) alts[i] else NULL
    if (is.null(alt)) {
      shiny::plotOutput(ns(ids[i]), height = height)
    } else {
      wise_plot_output(ns(ids[i]), alt, height = height)
    }
  }
  if (isTRUE(n_vars >= 2)) {
    bslib::layout_columns(
      col_widths = c(6, 6),
      bslib::card(plot_at(1)),
      bslib::card(plot_at(2))
    )
  } else {
    bslib::card(plot_at(1))
  }
}