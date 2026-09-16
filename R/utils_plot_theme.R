#' Shared ggplot2 theme and colour system for all WISE-APP plots
#'
#' UI panel headers own plot titles; in-plot titles are removed wherever
#' redundant. Text sizes are floored so labels stay readable at Shiny's
#' default 72 dpi rendering (1 pt == 1 px on screen). Per-plot
#' `+ theme(...)` overrides layered after this still win.
#'
#' @param base_size Base font size, default 16 for full-card plots. Use 13-14
#'   only for multi-panel patchwork layouts; never below 13.
#' @param ... Passed to [ggplot2::theme_minimal()].
#' @noRd

theme_wise <- function(base_size = 16, ...) {
  ggplot2::theme_minimal(base_size = base_size, ...) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = ggplot2::rel(1.0),
                                            face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.85),
                                            colour = "grey40"),
      plot.caption  = ggplot2::element_text(size = ggplot2::rel(0.8),
                                            colour = "grey40", hjust = 0),
      axis.title    = ggplot2::element_text(size = ggplot2::rel(1.0),
                                            colour = .wise_charcoal),
      axis.text     = ggplot2::element_text(size = ggplot2::rel(0.85),
                                            colour = .wise_slate),
      legend.title  = ggplot2::element_text(size = ggplot2::rel(0.9)),
      legend.text   = ggplot2::element_text(size = ggplot2::rel(0.85)),
      strip.text    = ggplot2::element_text(size = ggplot2::rel(0.95), face = "bold"),
      panel.grid.major = ggplot2::element_line(colour = "#E3E9EE",
                                               linewidth = 0.4),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position  = "bottom",
      legend.justification = "left"
    )
}

# Brand tokens (inst/app/_brand.yml) ----
# Single source for plot colours so figures match the UI.

.wise_navy      <- "#002244"  # headings, strongest text emphasis
.wise_blue      <- "#0071BC"  # brand primary: single-series charts
.wise_cyan      <- "#009FDA"  # bright cyan accent (wave/binscatter means)
.wise_charcoal  <- "#1D2A35"  # body text colour
.wise_slate     <- "#5B6B79"  # secondary text, reference/zero lines, baseline
.wise_grid      <- "#E3E9EE"  # major gridlines

# Semantic role colours ----
# Fixed meaning across every figure; do not repurpose ad hoc.

.wise_history     <- "#808080"  # historical sample / pre-policy reference
.wise_baseline    <- "#5B6B79"  # baseline scenario in policy comparisons
.wise_policy      <- "#D55E00"  # policy accent (bright, colourblind-safe)
.wise_policy_dark <- "#7F2704"  # darker companion for policy outlines/labels
.wise_marker      <- "#D55E00"  # poverty lines, tau/quantile markers
.wise_marker_alt  <- "#E69F00"  # secondary markers (quantile labels)
.wise_support     <- "#243746"  # ensemble / support points and outlines
.wise_zero        <- "#5B6B79"  # zero / reference lines

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
  "#000000"  # black
)
.wise_cat <- .okabe_ito

#' Colorblind-safe discrete scales (Okabe-Ito, blue-first), UI-04.
#' `...` forwards to [ggplot2::scale_colour_manual()] (name, breaks, labels, ...).
#' @name wise_scale_cat
#' @noRd
wise_scale_colour_cat <- function(...) {
  ggplot2::scale_colour_manual(values = .wise_cat, ...)
}

#' @rdname wise_scale_cat
#' @noRd
wise_scale_fill_cat <- function(...) {
  ggplot2::scale_fill_manual(values = .wise_cat, ...)
}

# Backwards-compatible aliases for the original wrapper names.
wise_scale_colour_okabe_ito <- wise_scale_colour_cat
wise_scale_fill_okabe_ito <- wise_scale_fill_cat

# SSP scenario colours ----
# Fixed semantic mapping (not positional): lower emissions = green, mid = blue,
# high = vermillion. Canonical keys must match .normalise_ssp().

.ssp_colours <- c(
  "SSP2-4.5" = "#009E73",   # bluish green (lower emissions)
  "SSP3-7.0" = "#0072B2",   # blue          (mid emissions)
  "SSP5-8.5" = "#D55E00"    # vermillion    (high emissions)
)

#' Discrete scales bound to the fixed SSP scenario mapping.
#' @name wise_scale_ssp
#' @noRd
wise_scale_colour_ssp <- function(...) {
  ggplot2::scale_colour_manual(values = .ssp_colours, ...)
}

#' @rdname wise_scale_ssp
#' @noRd
wise_scale_fill_ssp <- function(...) {
  ggplot2::scale_fill_manual(values = .ssp_colours, ...)
}

# Sequential ramp (charts) ----
# Single-hue brand-blue ramp for magnitude fills in ggplot charts. Map ramps
# (YlOrRd / RdBu / Mako in fct_weatherstats.R, fct_surveystats.R,
# fct_outcome.R) are reserved for maps and must not be reused in charts.

wise_seq_ramp <- function(n) {
  grDevices::colorRampPalette(c("#D9EFF8", "#0071BC", "#002244"))(n)
}

# Shared placeholder ----

#' Uniform placeholder for figures whose inputs are unavailable.
#' Replaces the per-file `blank_plot()` copies and base-graphics fallbacks.
#' @noRd
blank_plot <- function(message = "Not available", size = 4.2) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0.5, y = 0.5, label = message,
                      size = size, colour = "grey40") +
    ggplot2::theme_void()
}
