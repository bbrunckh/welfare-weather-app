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
