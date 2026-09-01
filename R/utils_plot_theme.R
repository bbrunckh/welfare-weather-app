#' Shared ggplot2 theme for all WISE-APP plots
#'
#' Panel headers in the UI own plot titles; in-plot titles are removed
#' wherever redundant, so plot.title is styled small for any stragglers.
#' Captions (CI notes) stay readable. Per-plot `+ theme(...)` overrides
#' layered after this still win.
#'
#' @param base_size Base font size, default 14. Use 11-12 only for
#'   multi-panel patchwork layouts.
#' @param ... Passed to [ggplot2::theme_minimal()].
#' @noRd
theme_wise <- function(base_size = 14, ...) {
  ggplot2::theme_minimal(base_size = base_size, ...) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(size = ggplot2::rel(0.85),
                                            face = "bold", hjust = 0),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(0.75),
                                            colour = "grey40"),
      plot.caption  = ggplot2::element_text(size = ggplot2::rel(0.65),
                                            colour = "grey40", hjust = 0),
      strip.text    = ggplot2::element_text(face = "bold")
    )
}

# ---- Colorblind-safe categorical palettes (UI-04) --------------------------
# Okabe-Ito is the canonical colorblind-safe qualitative palette. All
# categorical/discrete plot scales should draw from here via the wrappers
# below instead of ColorBrewer "Set1" or ad-hoc red/green choices.

.okabe_ito <- c(
  "#E69F00", # orange
  "#56B4E9", # sky blue
  "#009E73", # bluish green
  "#F0E442", # yellow
  "#0072B2", # blue
  "#D55E00", # vermillion
  "#CC79A7", # reddish purple
  "#000000"  # black
)

#' Colorblind-safe discrete scales (Okabe-Ito), UI-04.
#' `...` forwards to [ggplot2::scale_*_manual()] (name, breaks, labels, ...).
#' @noRd
scale_colour_okabe_ito <- function(...) {
  ggplot2::scale_colour_manual(values = .okabe_ito, ...)
}

#' @rdname scale_colour_okabe_ito
#' @noRd
scale_fill_okabe_ito <- function(...) {
  ggplot2::scale_fill_manual(values = .okabe_ito, ...)
}
