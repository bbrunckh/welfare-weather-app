# Brand tokens (inst/app/_brand.yml) ----
# Single source for plot colours so figures match the UI.

.wise_navy <- "#002244" # headings, strongest text emphasis
.wise_blue <- "#0071BC" # brand primary: single-series charts
.wise_cyan <- "#009FDA" # bright cyan accent (wave/binscatter means)
.wise_charcoal <- "#1D2A35" # body text colour
.wise_slate <- "#5B6B79" # secondary text, reference/zero lines, baseline
.wise_grid <- "#E3E9EE" # major gridlines

# Semantic role colours ----
# Fixed meaning across every figure; do not repurpose ad hoc.

.wise_history <- "#808080" # historical sample / pre-policy reference
.wise_baseline <- "#5B6B79" # baseline scenario in policy comparisons
.wise_policy <- "#D55E00" # policy accent (bright, colourblind-safe)
.wise_policy_dark <- "#7F2704" # darker companion for policy outlines/labels
.wise_marker <- "#D55E00" # poverty lines, tau/quantile markers
.wise_marker_alt <- "#E69F00" # secondary markers (quantile labels)
.wise_support <- "#243746" # ensemble / support points and outlines
.wise_zero <- "#5B6B79" # zero / reference lines

# Categorical palettes (UI-04) ----
# Okabe-Ito is the canonical colorblind-safe qualitative palette, reordered
# blue-first so the lead colour echoes the brand blue. All categorical or
# discrete scales must draw from here via the wrappers below instead of
# ColorBrewer palettes or ad-hoc red/green choices.

.okabe_ito <- c(
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#009E73", # bluish green
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#CC79A7", # reddish purple
  "#F0E442", # yellow
  "#000000" # black
)
.wise_cat <- .okabe_ito

# SSP scenario colours ----
# Fixed semantic mapping (not positional): lower emissions = green, mid = blue,
# high = vermillion. Canonical keys must match .normalise_ssp().

.ssp_colours <- c(
  "SSP2-4.5" = "#009E73", # bluish green (lower emissions)
  "SSP3-7.0" = "#0072B2", # blue          (mid emissions)
  "SSP5-8.5" = "#D55E00" # vermillion    (high emissions)
)

# Sequential ramp (charts) ----
# Single-hue brand-blue ramp for magnitude fills in ggplot charts. Map ramps
# (YlOrRd / RdBu / Mako in fct_weatherstats.R, fct_surveystats.R,
# fct_outcome.R) are reserved for maps and must not be reused in charts.

wise_seq_ramp <- function(n) {
  grDevices::colorRampPalette(c("#D9EFF8", "#0071BC", "#002244"))(n)
}

# Shared echarts4r styling (guidelines §7) ----
# Shared ECharts styling and WISE color tokens.

#' Style fragments for echarts4r axes, legends and tooltips
#'
#' Uses the shared WISE color tokens. echarts4r replaces whole option
#' blocks on each `e_*()` call, so the theme ships as fragments to pass inside
#' the builders' own axis/legend calls:
#'
#' ```r
#' df |> e_charts(x) |>
#'   e_bar(y) |>
#'   e_x_axis(name = "Year", axisLabel = wise_eaxis_label(),
#'            splitLine = wise_esplit_line()) |>
#'   wise_echart_theme()
#' ```
#'
#' `wise_echart_theme()` applies the widget-level defaults (font family, the
#' Okabe-Ito series palette, tooltip text) and must be piped last - builders'
#' own `e_tooltip()`/`e_color()` calls still win where set.
#'
#' @noRd
wise_eaxis_label <- function(...) {
  modifyList(list(color = .wise_slate, fontSize = 13), list(...))
}

#' @rdname wise_eaxis_label
#' @noRd
wise_eaxis_name <- function(...) {
  modifyList(
    list(color = .wise_charcoal, fontSize = 13, align = "center", padding = c(0, 0, 8, 0)),
    list(...)
  )
}

# Horizontal y-axis titles sit at the top/end of the axis and start at its
# left edge, allowing wrapped text to extend into the plot instead of forcing
# a large vertical-label margin.
wise_eyaxis_name <- function(...) {
  modifyList(
    list(
      color = .wise_charcoal,
      fontSize = 13,
      align = "left",
      verticalAlign = "top",
      padding = c(0, 0, 0, 0)
    ),
    list(...)
  )
}

#' @rdname wise_eaxis_label
#' @noRd
wise_esplit_line <- function(...) {
  modifyList(
    list(lineStyle = list(color = .wise_grid, width = 0.5)),
    list(...)
  )
}

#' @rdname wise_eaxis_label
#' @noRd
wise_elegend_style <- function(...) {
  modifyList(
    list(textStyle = list(color = .wise_charcoal, fontSize = 13)),
    list(...)
  )
}

#' @rdname wise_eaxis_label
#' @param e         An `echarts4r` widget (as returned by the `e_*` verbs).
#' @param base_size Unused; retained for internal call-site compatibility.
#' @noRd
wise_echart_theme <- function(e, base_size = 14) {
  # Value axes should keep the regular interior ticks readable without
  # duplicating the data-range endpoints at the plot edges. Category axes are
  # left unchanged so their first and last category labels remain visible.
  hide_value_axis_endpoints <- function(axes) {
    if (is.null(axes)) return(axes)

    axis_fields <- c("type", "axisLabel", "axisTick", "data", "gridIndex", "show")
    single_axis <- !is.null(names(axes)) && any(names(axes) %in% axis_fields)
    axis_list <- if (single_axis) list(axes) else axes
    axis_list <- lapply(axis_list, function(axis) {
      axis_type <- axis$type
      is_value <- identical(axis_type, "value") || identical(axis_type, "log") ||
        (is.null(axis_type) && is.null(axis$data))
      if (is_value) {
        labels <- if (is.null(axis$axisLabel)) list() else axis$axisLabel
        if (is.null(labels$showMinLabel)) labels$showMinLabel <- FALSE
        if (is.null(labels$showMaxLabel)) labels$showMaxLabel <- FALSE
        axis$axisLabel <- labels
      }
      axis
    })
    if (single_axis) axis_list[[1L]] else axis_list
  }

  e$x$opts$xAxis <- hide_value_axis_endpoints(e$x$opts$xAxis)
  e$x$opts$yAxis <- hide_value_axis_endpoints(e$x$opts$yAxis)
  keep_x_axis_at_bottom <- function(axes) {
    if (is.null(axes)) return(axes)
    axis_fields <- c("type", "axisLabel", "axisTick", "data", "gridIndex", "show")
    single_axis <- !is.null(names(axes)) && any(names(axes) %in% axis_fields)
    axis_list <- if (single_axis) list(axes) else axes
    axis_list <- lapply(axis_list, function(axis) {
      axis_line <- axis$axisLine %||% list()
      axis_line$onZero <- FALSE
      axis$axisLine <- axis_line
      axis
    })
    if (single_axis) axis_list[[1L]] else axis_list
  }
  e$x$opts$xAxis <- keep_x_axis_at_bottom(e$x$opts$xAxis)
  e$x$opts$textStyle <- list(fontFamily = "Helvetica, Arial, sans-serif")
  if (is.null(e$x$opts$color)) {
    e$x$opts$color <- as.character(.wise_cat)
  }
  tip <- e$x$opts$tooltip
  e$x$opts$tooltip <- modifyList(
    list(textStyle = list(color = .wise_charcoal, fontSize = 13)),
    if (is.null(tip)) list() else tip
  )
  if (is.null(e$x$opts$toolbox)) {
    e$x$opts$toolbox <- list(
      show = TRUE,
      right = 8,
      top = 0,
      feature = list(
        saveAsImage = list(
          show = TRUE,
          title = "Download image",
          pixelRatio = 2
        )
      )
    )
  }
  # Dynamic results panes can first render while narrow or hidden; window resize
  # alone does not notify the widget when its own container subsequently grows.
  htmlwidgets::onRender(e, "function(el) {
    if (el._wiseChartResizeObserver) el._wiseChartResizeObserver.disconnect();
    var widget = this;
    var resize = function() {
      if (!el.isConnected) { el._wiseChartResizeObserver.disconnect(); return; }
      if (el.clientWidth > 0 && el.clientHeight > 0) {
        var chart = widget.getChart();
        if (chart && !chart.isDisposed()) chart.resize({width: el.clientWidth, height: el.clientHeight});
      }
    };
    el._wiseChartResizeObserver = new ResizeObserver(function() { requestAnimationFrame(resize); });
    el._wiseChartResizeObserver.observe(el);
    requestAnimationFrame(resize);
  }")
}

#' Uniform echarts placeholder for figures whose inputs are unavailable
#'
#' Empty chart with a centred, slate message. Use it when a chart has a message the
#' user can act on ("Insufficient data", "Select scenarios first"); charts
#' that were simply empty can return `NULL` and let the render `req()` clear
#' the output.
#'
#' @param message Text to display.
#' @param height  Widget height; pass the slot's plot height.
#'
#' @return An `echarts4r` widget.
#' @noRd
echart_blank <- function(message = "Not available", height = "300px") {
  # echarts4r 0.5.x rejects single-column and <2-row frames in e_charts(); a
  # two-column, two-row dummy is invisible (no series are ever drawn on it).
  # Axis hiding goes through direct opts injection: e_axis_*() re-enters
  # e_charts() and hits the same single-column defect.
  e <- echarts4r::e_charts(data.frame(x = 0:1, y = 0:1), x, height = height)
  e <- echarts4r::e_title(
    e,
    text = message,
    left = "center",
    top = "middle",
    textStyle = list(
      color = .wise_slate,
      fontSize = 14,
      fontWeight = "normal"
    )
  )
  e$x$opts$xAxis <- lapply(
    if (is.null(e$x$opts$xAxis)) list() else e$x$opts$xAxis,
    function(ax) modifyList(ax, list(show = FALSE))
  )
  e$x$opts$yAxis <- lapply(
    if (is.null(e$x$opts$yAxis)) list() else e$x$opts$yAxis,
    function(ax) modifyList(ax, list(show = FALSE))
  )
  wise_echart_theme(e)
}
