# ==============================================================================
# echarts4r advanced distribution plots
#
# Three reusable functions, each built on native echarts4r primitives where
# they exist:
#
#   1. e_raincloud()   - violin + boxplot + jittered points
#                         (fully native: e_scatter jitter, e_violin, e_boxplot)
#   2. e_ridgeline()   - stacked/overlapping density "ridges"
#                         (NOTE: echarts4r has no native ridgeline series.
#                          e_density() is used per group for the native KDE,
#                          but the vertical offset/stacking is computed in R.)
#   3. e_exceedance()  - exceedance probability curves with uncertainty ribbons
#                         (fully native: e_band() for the ribbons, e_line() for
#                          the central curve)
#
# Requires: echarts4r (>= 0.5.0), dplyr, tidyr, rlang
# ==============================================================================

library(echarts4r)
library(dplyr)
library(tidyr)
library(rlang)


# ------------------------------------------------------------------------------
# 1. RAINCLOUD PLOT  (violin + boxplot + jittered dots) -- fully native
# ------------------------------------------------------------------------------
#
# Native building blocks used:
#   - e_scatter(jitter_factor = , jitter_amount = )  -> built-in point jitter
#   - e_violin(binCount = , bandWidthScale = , areaOpacity = ) -> native violin
#   - e_boxplot(outliers = )                          -> native quartile/whisker
#   - e_flip_coords()                                 -> horizontal orientation
#
#' Horizontal raincloud-style plot using native echarts4r primitives
#'
#' @param data A data.frame in long format
#' @param value Unquoted numeric column
#' @param group Unquoted grouping column
#' @param jitter_factor Passed to the scatter layer (0-1 range typically)
#' @param jitter_amount Alternative to jitter_factor; absolute jitter spread
#' @param bin_count Passed to e_violin's binCount (density resolution)
#' @param band_width_scale Passed to e_violin's bandWidthScale (violin "fatness")
#' @param area_opacity Violin fill opacity
#' @param show_box Logical; overlay a native boxplot
#' @param outliers Logical; show boxplot outliers (only relevant if show_box = TRUE)
#' @param theme Optional echarts4r theme name
#' @param title,subtitle,x_name Optional labels
#'
#' @return an echarts4r htmlwidget
e_raincloud <- function(data,
                         value,
                         group,
                         jitter_factor = 0.3,
                         jitter_amount = NULL,
                         bin_count = 100,
                         band_width_scale = 1,
                         area_opacity = 0.5,
                         show_box = TRUE,
                         outliers = TRUE,
                         theme = NULL,
                         title = NULL,
                         subtitle = NULL,
                         x_name = NULL) {

  value_sym <- rlang::ensym(value)
  group_sym <- rlang::ensym(group)

  df <- data |>
    mutate(.value = !!value_sym, .group = !!group_sym) |>
    filter(!is.na(.value))

  p <- df |>
    group_by(.group) |>
    e_charts() |>
    e_scatter(
      .value,
      jitter_factor = jitter_factor,
      jitter_amount = jitter_amount,
      symbol_size = 6,
      legend = FALSE
    ) |>
    e_violin(
      binCount = bin_count,
      bandWidthScale = band_width_scale,
      areaOpacity = area_opacity,
      legend = FALSE
    )

  if (show_box) {
    p <- p |> e_boxplot(.value, outliers = outliers)
  }

  p <- p |>
    e_flip_coords() |>
    e_tooltip(trigger = "item") |>
    e_x_axis(name = x_name)

  if (!is.null(title)) p <- p |> e_title(text = title, subtext = subtitle, left = "center")
  if (!is.null(theme)) p <- p |> e_theme(theme)

  p
}


# ------------------------------------------------------------------------------
# 2. RIDGELINE PLOT  (overlapping density curves) -- partially native
# ------------------------------------------------------------------------------
#
# echarts4r has NO native ridgeline/joyplot series (confirmed against the full
# 0.5.0 function index: no e_ridge / e_ridgeline / e_joyplot). The closest
# native pieces are e_density() (KDE computed by echarts, not R) and e_river()
# (a theme-river/streamgraph, which is a different chart type: stacked flowing
# bands, not offset density silhouettes).
#
# This function computes each group's KDE in R with density() on a SHARED
# x-grid (required so curves align and tooltips are meaningful), then applies
# a manual vertical offset per group to create the overlapping "ridge" effect,
# and renders each ridge as a filled e_line() area.
#
#' Ridgeline plot (stacked/overlapping density curves)
#'
#' @param data A data.frame
#' @param value Unquoted numeric column to compute densities on
#' @param group Unquoted grouping column (one ridge per level)
#' @param group_order Optional character vector giving explicit group order
#'   (top to bottom). Defaults to the order levels appear in the data.
#' @param n_points Number of points to evaluate each density curve at
#' @param overlap Controls vertical overlap between ridges. 0 = no overlap,
#'   1 = heavy overlap. Default 0.6
#' @param scale Height scale factor for density curves relative to offset spacing
#' @param fill Logical; whether to fill the ridges with color (default TRUE)
#' @param opacity Fill opacity when fill = TRUE
#' @param palette Character vector of hex colors to cycle through
#' @param theme Optional echarts4r theme name
#' @param title,subtitle,x_name Optional labels
#'
#' @return an echarts4r htmlwidget
e_ridgeline <- function(data,
                         value,
                         group,
                         group_order = NULL,
                         n_points = 256,
                         overlap = 0.6,
                         scale = 1.4,
                         fill = TRUE,
                         opacity = 0.75,
                         palette = c("#5470C6","#91CC75","#FAC858","#EE6666",
                                     "#73C0DE","#3BA272","#FC8452","#9A60B4",
                                     "#ea7ccc"),
                         theme = NULL,
                         title = NULL,
                         subtitle = NULL,
                         x_name = NULL) {

  value_sym <- rlang::ensym(value)
  group_sym <- rlang::ensym(group)

  df <- data |>
    mutate(.value = !!value_sym, .group = as.character(!!group_sym)) |>
    filter(!is.na(.value))

  if (is.null(group_order)) group_order <- unique(df$.group)
  plot_order <- rev(group_order)  # first group ends up drawn on top

  # Shared x-grid across all groups so curves align and tooltips make sense
  x_min <- min(df$.value, na.rm = TRUE)
  x_max <- max(df$.value, na.rm = TRUE)
  x_grid <- seq(x_min, x_max, length.out = n_points)

  dens_df <- df |>
    group_by(.group) |>
    summarise(dens = list(stats::density(.value, n = n_points)), .groups = "drop") |>
    rowwise() |>
    mutate(y_interp = list(stats::approx(dens$x, dens$y, xout = x_grid, rule = 2)$y)) |>
    ungroup() |>
    select(.group, y_interp) |>
    mutate(x = list(x_grid)) |>
    unnest(c(x, y_interp)) |>
    rename(y = y_interp)

  offset_step <- overlap * (max(dens_df$y, na.rm = TRUE) * scale) / 2
  if (offset_step <= 0) offset_step <- 1

  offsets <- setNames(
    seq(0, by = offset_step, length.out = length(plot_order)),
    plot_order
  )

  dens_df <- dens_df |>
    mutate(
      baseline = offsets[.group],
      y_shifted = y * scale + baseline
    )

  first_group <- plot_order[1]
  p <- dens_df |> filter(.group == first_group) |> e_charts(x)

  for (i in seq_along(plot_order)) {
    g <- plot_order[i]
    sub <- dens_df |> filter(.group == g)
    col <- palette[((i - 1) %% length(palette)) + 1]

    p <- p |>
      e_data(sub, x) |>
      e_line(
        y_shifted,
        name = g,
        symbol = "none",
        lineStyle = list(width = 1.5, color = col),
        areaStyle = if (fill) list(color = col, opacity = opacity) else NULL,
        z = length(plot_order) - i,
        emphasis = list(focus = "series")
      )
  }

  p <- p |>
    e_x_axis(name = x_name, splitLine = list(show = FALSE)) |>
    e_y_axis(show = FALSE, min = 0) |>
    e_tooltip(trigger = "axis") |>
    e_legend(orient = "vertical", right = 10, top = "middle") |>
    e_grid(left = "8%", right = "18%", top = "12%", bottom = "8%")

  if (!is.null(title)) p <- p |> e_title(text = title, subtext = subtitle, left = "center")
  if (!is.null(theme)) p <- p |> e_theme(theme)

  p
}


# ------------------------------------------------------------------------------
# 3. EXCEEDANCE PROBABILITY CURVES  (with uncertainty ribbons) -- fully native
# ------------------------------------------------------------------------------
#
# Native building blocks used:
#   - e_band(min, max, stack = , areaStyle = ) -> native confidence/uncertainty
#     ribbon (stacks a lower + upper line series into one filled band)
#   - e_line()                                  -> central estimate curve
#
# IMPORTANT: `stack` must be unique per group, otherwise multiple ribbons will
# stack on top of one another instead of overlapping independently (this
# mirrors a historical echarts4r bug, #237, caused by stack-name collisions).
#
#' Exceedance probability curves with uncertainty ribbons, native echarts4r
#'
#' Plots P(X > x) for one or more series, each with an optional shaded
#' uncertainty band (e.g. from bootstrap or ensemble runs) via e_band().
#'
#' @param data Long-format data.frame with one row per (group, x) pair
#' @param x Unquoted column: the threshold / exceedance value
#' @param p Unquoted column: central exceedance probability estimate at x
#' @param lower,upper Optional unquoted columns: lower/upper uncertainty bounds
#'   at each x. If omitted, no ribbon is drawn for that group.
#' @param group Unquoted grouping column, one curve (+ band) per level
#' @param log_y Logical; use log scale on y (typical for exceedance probability)
#' @param band_opacity Fill opacity for uncertainty ribbons
#' @param palette Hex colors, one per group (cycled)
#' @param theme Optional echarts4r theme
#' @param title,subtitle,x_name,y_name Optional labels
#'
#' @return an echarts4r htmlwidget
e_exceedance <- function(data,
                          x,
                          p,
                          lower = NULL,
                          upper = NULL,
                          group,
                          log_y = TRUE,
                          band_opacity = 0.2,
                          palette = c("#5470C6","#91CC75","#FAC858","#EE6666",
                                      "#73C0DE","#3BA272","#FC8452","#9A60B4"),
                          theme = NULL,
                          title = NULL,
                          subtitle = NULL,
                          x_name = "Threshold",
                          y_name = "Exceedance probability") {

  x_sym     <- rlang::ensym(x)
  p_sym     <- rlang::ensym(p)
  group_sym <- rlang::ensym(group)
  lower_sym <- if (!missing(lower)) rlang::ensym(lower) else NULL
  upper_sym <- if (!missing(upper)) rlang::ensym(upper) else NULL

  df <- data |>
    mutate(
      .x = !!x_sym,
      .p = !!p_sym,
      .group = as.character(!!group_sym),
      .lower = if (!is.null(lower_sym)) !!lower_sym else NA_real_,
      .upper = if (!is.null(upper_sym)) !!upper_sym else NA_real_
    ) |>
    arrange(.group, .x)

  groups <- unique(df$.group)
  has_band <- !is.null(lower_sym) && !is.null(upper_sym)

  p_chart <- df |> filter(.group == groups[1]) |> e_charts(.x)

  for (i in seq_along(groups)) {
    g <- groups[i]
    col <- palette[((i - 1) %% length(palette)) + 1]
    sub <- df |> filter(.group == g)

    p_chart <- p_chart |> e_data(sub, .x)

    if (has_band && !all(is.na(sub$.lower))) {
      p_chart <- p_chart |>
        e_band(
          .lower, .upper,
          stack = paste0("band-", g),
          areaStyle = list(
            list(color = "rgba(0,0,0,0)"),
            list(color = col, opacity = band_opacity)
          ),
          name = c(paste0(g, " lower"), paste0(g, " upper"))
        )
    }

    p_chart <- p_chart |>
      e_line(
        .p,
        name = g,
        symbol = "none",
        lineStyle = list(width = 2, color = col),
        emphasis = list(focus = "series")
      )
  }

  p_chart <- p_chart |>
    e_x_axis(name = x_name) |>
    e_y_axis(
      name = y_name,
      type = if (log_y) "log" else "value",
      min = if (log_y) NULL else 0,
      max = if (log_y) NULL else 1
    ) |>
    e_tooltip(trigger = "axis") |>
    e_legend(top = "bottom")

  if (!is.null(title)) p_chart <- p_chart |> e_title(text = title, subtext = subtitle, left = "center")
  if (!is.null(theme)) p_chart <- p_chart |> e_theme(theme)

  p_chart
}


# ==============================================================================
# EXAMPLES
# ==============================================================================

## ---- 1. Raincloud plot ------------------------------------------------------
## Uses the same PlantGrowth dataset as the official e_violin() docs

data(PlantGrowth)

raincloud_plot <- e_raincloud(
  PlantGrowth, weight, group,
  title = "Plant Growth by Treatment",
  subtitle = "Native violin + boxplot + jitter",
  x_name = "Dry weight"
)
raincloud_plot


## ---- 2. Ridgeline plot -------------------------------------------------------
## Simulated monthly distributions

set.seed(42)
months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul")

ridgeline_df <- lapply(seq_along(months), function(i) {
  data.frame(month = months[i], value = rnorm(500, mean = i * 1.5, sd = 2 + i * 0.2))
}) |> bind_rows()

ridgeline_plot <- e_ridgeline(
  ridgeline_df, value, month,
  group_order = months,
  title = "Monthly Value Distributions",
  subtitle = "Ridgeline (density curves, manually offset)",
  x_name = "Value"
)
ridgeline_plot


## ---- 3. Exceedance probability curves ---------------------------------------
## Simulated exponential exceedance curves for three models, with bootstrap-like
## uncertainty bounds

set.seed(1)
x_vals <- seq(0, 10, length.out = 50)

make_curve <- function(model, rate) {
  p_central <- exp(-rate * x_vals)
  spread <- p_central * runif(length(x_vals), 0.15, 0.35)
  data.frame(
    model = model,
    x = x_vals,
    p = p_central,
    lower = pmax(p_central - spread, 1e-4),  # floor to avoid -Inf on log axis
    upper = p_central + spread
  )
}

exceedance_df <- bind_rows(
  make_curve("Model A", 0.5),
  make_curve("Model B", 0.35),
  make_curve("Model C", 0.65)
)

exceedance_plot <- e_exceedance(
  exceedance_df, x, p, lower, upper, model,
  title = "Exceedance Probability Curves",
  subtitle = "Shaded regions show uncertainty bounds",
  x_name = "Magnitude", y_name = "P(X > x)"
)
exceedance_plot
