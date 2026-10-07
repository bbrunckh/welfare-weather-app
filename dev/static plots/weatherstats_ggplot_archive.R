# Archived static ggplot renderers from R/fct_weatherstats.R.
#
# These functions are retained for reference only and are not part of the
# package runtime or public API. They rely on shared helpers that remain in
# R/fct_weatherstats.R: `.wx_*`, `.wave_palette()`, `.blend_colour()`,
# `.hist_bin_counts()`, and `build_ridge_distribution_data()`. The binscatter
# additionally uses the shared plot theme and palette constants from the app.

# Weather distribution plots ----
# Both the binned bar chart and the continuous ridge plot draw the survey wave
# and that wave's own climate history in the same panel, so the comparison the
# user cares about ("was this wave unusual?") is a within-panel one. Historical
# weather is loaded for the same locations and calendar months as each wave and
# weighted by the households behind them, so the two series are composed the
# same way - see `join_hist_sample_cells()`.

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
#' @param wave_labels Optional named character vector replacing wave labels.
#'
#' @return A `ggplot` object, or `NULL` invisibly when there is nothing to plot.
#'
#' @noRd
plot_weather_bins_compare <- function(df, hv, label, hist_df = NULL,
                                      breaks = NULL, year_from = NULL,
                                      year_to = NULL, wave_labels = NULL) {
  if (is.null(df) || is.na(hv) || !(hv %in% names(df))) {
    return(invisible(NULL))
  }
  if (!("countryyear" %in% names(df))) {
    return(invisible(NULL))
  }

  keep <- !is.na(df[[hv]])
  if (!any(keep)) {
    return(invisible(NULL))
  }

  lvls <- if (is.factor(df[[hv]])) {
    levels(df[[hv]])
  } else {
    sort(unique(as.character(df[[hv]][keep])))
  }

  samp <- data.frame(
    countryyear = as.character(df$countryyear[keep]),
    bin = as.character(df[[hv]][keep]),
    w = 1,
    stringsAsFactors = FALSE
  ) |>
    dplyr::group_by(.data$countryyear, .data$bin) |>
    dplyr::summarise(w = sum(.data$w, na.rm = TRUE), .groups = "drop") |>
    as.data.frame()
  samp$source <- .wx_sample_lab

  hist_lab <- .wx_hist_lab(year_from, year_to)
  hist_bins <- .hist_bin_counts(hist_df, hv, breaks, year_from, year_to)
  has_hist <- !is.null(hist_bins) && nrow(hist_bins) > 0
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
  pal <- .wave_palette(waves)

  # Wave-major key order, so each wave's pair of bars sits side by side inside
  # a bin rather than all samples first and all histories after. The historical
  # bar is a lighter shade of the wave's own colour.
  sources <- if (has_hist) c(.wx_sample_lab, hist_lab) else .wx_sample_lab
  series <- .wx_series_grid(waves, sources)
  key_cols <- stats::setNames(
    ifelse(series$source == .wx_sample_lab,
      pal[series$wave],
      .blend_colour(pal[series$wave], "white", 0.6)
    ),
    series$key
  )

  d$key <- factor(.wx_series_key(d$countryyear, d$source),
    levels = series$key
  )
  d$bin <- factor(d$bin, levels = lvls)

  ggplot2::ggplot(
    d, ggplot2::aes(x = .data$bin, y = .data$share, fill = .data$key)
  ) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge(preserve = "single"),
      colour = "grey45", linewidth = 0.25, alpha = 0.9
    ) +
    ggplot2::scale_fill_manual(
      values = key_cols, name = NULL, drop = FALSE,
      labels = .wx_display_series_labels(series$key, wave_labels)
    ) +
    theme_wise() +
    ggplot2::labs(
      x = stringr::str_wrap(paste0(label, "\n(as configured)"), 40),
      y = "Share of observations (%)"
    ) +
    ggplot2::theme(
      axis.text.x     = ggplot2::element_text(angle = 30, hjust = 1),
      legend.position = "top"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = length(sources)))
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
#' @param wave_labels Optional named character vector replacing wave labels.
#'
#' @return A `ggplot` object, or `NULL` invisibly when there is nothing to plot.
#'
#' @noRd
plot_weather_ridges_compare <- function(df, hv, label, hist_df = NULL,
                                        year_from = NULL, year_to = NULL,
                                        wave_labels = NULL) {
  if (is.null(df) || is.na(hv) || !(hv %in% names(df))) {
    return(invisible(NULL))
  }
  if (!("countryyear" %in% names(df))) {
    return(invisible(NULL))
  }

  sv <- suppressWarnings(as.numeric(df[[hv]]))
  keep <- is.finite(sv)
  if (!any(keep)) {
    return(invisible(NULL))
  }

  samp <- data.frame(
    countryyear = as.character(df$countryyear[keep]),
    x = sv[keep],
    w = 1,
    source = .wx_sample_lab,
    stringsAsFactors = FALSE
  )

  hist_lab <- .wx_hist_lab(year_from, year_to)
  hist_use <- NULL
  if (!is.null(hist_df) && !is.na(hv) && hv %in% names(hist_df) &&
    all(c("n_hh", "countryyear") %in% names(hist_df))) {
    hv_vals <- suppressWarnings(as.numeric(hist_df[[hv]]))
    hkeep <- is.finite(hv_vals)
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
        x = hv_vals[hkeep],
        w = as.numeric(hist_df$n_hh[hkeep]),
        source = hist_lab,
        stringsAsFactors = FALSE
      )
      # Only keep waves the sample also has, so no row appears with a
      # historical curve and no sample curve.
      hist_use <- hist_use[hist_use$countryyear %in% samp$countryyear, ,
        drop = FALSE
      ]
      if (nrow(hist_use) == 0) hist_use <- NULL
    }
  }

  waves <- sort(unique(samp$countryyear))
  pal <- .wave_palette(waves)
  sources <- if (is.null(hist_use)) {
    .wx_sample_lab
  } else {
    c(.wx_sample_lab, hist_lab)
  }

  # The sample is a filled ridge in the wave's colour; the history is drawn
  # over it as an unfilled dashed outline in a darker shade of the same colour,
  # so the pair reads as one wave rather than two unrelated series.
  series <- .wx_series_grid(waves, sources)
  fills <- stats::setNames(
    ifelse(series$source == .wx_sample_lab, pal[series$wave], NA_character_),
    series$key
  )
  lines <- stats::setNames(
    ifelse(series$source == .wx_sample_lab, "grey30",
      .blend_colour(pal[series$wave], "black", 0.35)
    ),
    series$key
  )

  d <- if (is.null(hist_use)) samp else rbind(samp, hist_use)
  d$key <- .wx_series_key(as.character(d$countryyear), as.character(d$source))
  d$source <- factor(d$source, levels = sources)
  d$key <- factor(d$key, levels = series$key)

  # Aggregate each wave/source to a fixed histogram before smoothing. This
  # keeps the weather ridge cost proportional to cells/waves, not households.
  rd <- build_ridge_distribution_data(
    d,
    x_var = "x",
    group_var = "key",
    fill_var = "key",
    weight_var = "w",
    ridge_var = "countryyear",
    n_bins = 256L,
    n_grid = 256L,
    bandwidth_scale = 0.85
  )
  if (is.null(rd)) {
    return(invisible(NULL))
  }
  ridge_data <- rd$data
  ridge_data$key <- factor(ridge_data$group, levels = series$key)
  ridge_data$source <- factor(
    vapply(
      strsplit(as.character(ridge_data$key), " - ", fixed = TRUE),
      `[`, character(1), 2L
    ),
    levels = sources
  )
  ridge_data$fill <- ridge_data$key
  ridge_data$colour <- ridge_data$key

  p <- ggplot2::ggplot(
    ridge_data,
    ggplot2::aes(
      x        = .data$x,
      y        = .data$y,
      group    = .data$group,
      fill     = .data$fill,
      colour   = .data$colour,
      linetype = .data$source
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = .data$y,
        ymax = .data$y + .data$height * 2
      ),
      alpha = 0.7, colour = NA
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$y + .data$height * 2),
      linewidth = 0.5
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_along(rd$ridges),
      labels = .wx_display_wave_labels(rd$ridges, wave_labels),
      expand = ggplot2::expansion(mult = c(0.02, 0.12))
    ) +
    ggplot2::scale_fill_manual(values = fills, na.value = NA, guide = "none") +
    ggplot2::scale_colour_manual(values = lines, guide = "none") +
    ggplot2::scale_linetype_manual(
      values = stats::setNames(
        c("solid", "22")[seq_along(sources)], sources
      ),
      labels = sources,
      name = NULL
    ) +
    theme_wise() +
    ggplot2::labs(
      x = stringr::str_wrap(paste0(label, "\n(as configured)"), 40),
      y = ""
    ) +
    ggplot2::theme(
      legend.position = if (length(sources) > 1) "top" else "none",
      legend.key      = ggplot2::element_rect(fill = "white", colour = "white")
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
#' @param wave_labels Optional named character vector replacing wave labels.
#'
#' @return A `ggplot` object, or `NULL` invisibly when `hv` is absent or `NA`.
#'
#' @noRd
plot_weather_dist <- function(df, hv, label, cont_binned, hist_df = NULL,
                              breaks = NULL, year_from = NULL,
                              year_to = NULL, wave_labels = NULL) {
  if (is.null(df) || is.na(hv) || !(hv %in% names(df))) {
    return(invisible(NULL))
  }

  if (!is.na(cont_binned) && cont_binned == "Binned") {
    plot_weather_bins_compare(
      df = df, hv = hv, label = label, hist_df = hist_df, breaks = breaks,
      year_from = year_from, year_to = year_to, wave_labels = wave_labels
    )
  } else {
    plot_weather_ridges_compare(
      df = df, hv = hv, label = label, hist_df = hist_df,
      year_from = year_from, year_to = year_to, wave_labels = wave_labels
    )
  }
}

# Binscatter plot ----

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
#' @noRd
plot_binscatter <- function(df, hv, hv_label = hv, y_var, y_label = y_var) {
  if (is.null(df) || !all(c(hv, y_var) %in% names(df))) {
    return(NULL)
  }

  raw <- df[, c(hv, y_var), drop = FALSE]
  x_raw <- raw[[1L]]
  y_raw <- raw[[2L]]

  # Weather factors/characters are model bins; numeric weather is continuous.
  is_binned_x <- is.factor(x_raw) || is.character(x_raw)
  if (is_binned_x) {
    x_levels <- if (is.factor(x_raw)) {
      levels(x_raw)
    } else {
      unique(as.character(x_raw[!is.na(x_raw)]))
    }
    x_num <- factor(as.character(x_raw), levels = x_levels)
  } else {
    x_levels <- NULL
    x_num <- suppressWarnings(as.numeric(as.character(x_raw)))
  }

  # Normalize all supported outcome representations before sampling. This also
  # ensures the capped point layer has the same rows and types as the summary.
  y_num <- suppressWarnings(as.numeric(as.character(y_raw)))
  is_binary_y <- FALSE
  finite_y <- y_num[is.finite(y_num)]
  if (length(finite_y) && all(unique(finite_y) %in% c(0, 1))) {
    is_binary_y <- TRUE
  } else if (is.logical(y_raw)) {
    y_num <- as.integer(y_raw)
    is_binary_y <- TRUE
  } else if (is.factor(y_raw) && nlevels(y_raw) == 2L) {
    y_levels <- levels(y_raw)
    y_num <- match(as.character(y_raw), y_levels) - 1L
    is_binary_y <- TRUE
  } else if (is.character(y_raw)) {
    y_levels <- sort(unique(as.character(y_raw[!is.na(y_raw)])))
    if (length(y_levels) == 2L) {
      y_num <- match(as.character(y_raw), y_levels) - 1L
      is_binary_y <- TRUE
    }
  }

  if (!is_binary_y) {
    y_num <- suppressWarnings(as.numeric(as.character(y_raw)))
  }

  keep <- is.finite(y_num)
  if (is_binned_x) {
    keep <- keep & !is.na(x_num)
  } else {
    keep <- keep & is.finite(x_num)
  }
  if (!any(keep)) {
    return(NULL)
  }

  d <- data.frame(
    x = x_num[keep], y = y_num[keep],
    stringsAsFactors = FALSE
  )

  # Keep the point layer bounded; bin summaries below still use every row.
  # Round to integer row positions because tibble row slicing rejects the
  # fractional doubles returned by seq(..., length.out = ...).
  point_max <- 2500L
  point_df <- if (nrow(d) > point_max) {
    idx <- unique(as.integer(round(
      seq(1, nrow(d), length.out = point_max)
    )))
    d[idx, , drop = FALSE]
  } else {
    d
  }

  summarise_bins <- function(bin, x_value = NULL) {
    g <- collapse::GRP(data.frame(bin = bin), by = "bin")
    out <- data.frame(
      bin = as.character(g$groups[[1]]),
      mean = as.numeric(collapse::fmean(d$y, g = g, na.rm = TRUE)),
      n = as.integer(collapse::fnobs(d$y, g = g)),
      stringsAsFactors = FALSE
    )
    if (!is.null(x_value)) {
      idx <- suppressWarnings(as.integer(out$bin))
      out$x <- x_value[idx]
    }
    out
  }

  if (is_binned_x) {
    summary_df <- summarise_bins(d$x)
    summary_df$bin <- factor(summary_df$bin, levels = x_levels)

    p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_jitter(
        data = transform(point_df, x = factor(as.character(x), levels = x_levels)),
        width = 0.12, height = if (is_binary_y) 0.025 else 0,
        alpha = 0.10, colour = .wise_charcoal, size = 0.8
      ) +
      ggplot2::geom_line(
        data = summary_df,
        ggplot2::aes(x = .data$bin, y = .data$mean, group = 1),
        colour = .wise_blue, linewidth = 0.7
      ) +
      ggplot2::geom_point(
        data = summary_df,
        ggplot2::aes(x = .data$bin, y = .data$mean, size = .data$n),
        colour = .wise_cyan
      ) +
      ggplot2::scale_size_continuous(range = c(2, 5), guide = "none") +
      theme_wise() +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1)
      ) +
      ggplot2::labs(
        x = stringr::str_wrap(hv_label, 40),
        y = stringr::str_wrap(y_label, 40)
      )

    if (is_binary_y) {
      p <- p + ggplot2::scale_y_continuous(limits = c(0, 1))
    }

    return(p)
  }

  # Continuous x
  x_range <- range(d$x, finite = TRUE)
  if (!all(is.finite(x_range))) {
    return(NULL)
  }
  breaks <- if (diff(x_range) == 0) {
    x_range[1] + c(-0.5, 0.5)
  } else {
    seq(x_range[1], x_range[2], length.out = 21L)
  }
  bin <- cut(d$x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  bin_mid <- (breaks[-length(breaks)] + breaks[-1L]) / 2
  summary_df <- summarise_bins(bin, bin_mid)
  summary_df <- summary_df[is.finite(summary_df$mean), , drop = FALSE]

  p <- ggplot2::ggplot() +
    ggplot2::geom_point(
      data = point_df,
      ggplot2::aes(x = .data$x, y = .data$y),
      alpha = 0.10, colour = .wise_charcoal, size = 0.8
    ) +
    ggplot2::geom_line(
      data = summary_df,
      ggplot2::aes(x = .data$x, y = .data$mean),
      colour = .wise_blue, linewidth = 0.9
    ) +
    ggplot2::geom_point(
      data = summary_df,
      ggplot2::aes(x = .data$x, y = .data$mean, size = .data$n),
      colour = .wise_cyan
    ) +
    ggplot2::scale_size_continuous(range = c(2, 5), guide = "none") +
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
