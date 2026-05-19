library(tidyverse)
library(patchwork)

#' Plot a Specification Curve from Model Coefficient Summary Data
#'
#' @description
#' Builds a two-panel specification curve plot from a coefficient summary data
#' frame (e.g. `_summary_coefficients.csv`). The **top panel** shows point
#' estimates and confidence intervals for each estimand (or weather bin),
#' overlaid on a single x-axis ranked by a chosen estimand. The **bottom
#' panel** shows a dot-matrix grid of the specification choices that produced
#' each ranked result, with one row per unique value of each `spec_var`.
#'
#' @section Weather column parsing:
#' The `weather` column is expected in the form `<var>_<N>m_<cont|binn>` (e.g.
#' `t_1m_cont`, `t_12m_binn`). It is automatically parsed into three derived
#' columns that can be used in `spec_vars`:
#' \describe{
#'   \item{`wx_var`}{Weather variable name (e.g. `"t"`).}
#'   \item{`wx_ref_period`}{Reference period formatted for display (e.g.
#'     `"1 month"`, `"12 months"`).}
#'   \item{`wx_binned`}{Whether the weather variable is binned (`"Yes"` /
#'     `"No"`).}
#' }
#'
#' @section Binned specifications:
#' When `term_regex` matches bin terms (e.g. `t(17.6,22.2]`,
#' `t(22.2,25.3]:electricity`), bins are auto-detected, ordered low-to-high
#' by their lower bound, and labelled `"Bin 2 (low)"` … `"Bin N (high)"`. Bin
#' colours in the top panel run from blue (low) to red (high). Because break
#' points differ across reference periods, labels are ordinal; the caption
#' notes this and that all bin coefficients are relative to the omitted lowest
#' bin category.
#'
#' @section Engine / estimand collinearity:
#' When each engine maps to exactly one estimand class (e.g. `fixest` →
#' `Mean`, `rif` → `UQR *`), `engine` is automatically excluded from the spec
#' identity key and from `spec_vars` to avoid misalignment in the rank join. A
#' message is emitted when this happens.
#'
#' @param data A data frame with the structure of `_summary_coefficients.csv`.
#'   Required columns: `code`, `weather`, `engine`, `fe_profile`,
#'   `cov_profile`, `cov_method`, `interaction`, `model`, `term`, `estimand`,
#'   `estimate`, `std_error`.
#' @param term Character scalar. Exact match on the `term` column (e.g.
#'   `"t"`, `"t:urban"`). Mutually exclusive with `term_regex`; if both are
#'   supplied `term_regex` takes precedence.
#' @param term_regex Character scalar. A regex pattern applied via
#'   [stringr::str_detect()] to the `term` column. Use this to match binned
#'   coefficient terms, e.g. `"^t\\(.*\\]:electricity$"`. When matches
#'   contain interval notation (`(lower,upper]`), bins are auto-parsed.
#' @param estimands Character vector of `estimand` values to retain. Defaults
#'   to `c("Mean", "UQR p10", "UQR p50", "UQR p90")`.
#' @param model_filter Character vector of `model` values to retain (e.g.
#'   `"fit3"`). Set to `NULL` to skip model filtering.
#' @param code_filter Character vector of `code` values (sample identifiers)
#'   to retain. If `NULL` (default) and multiple codes exist, the first code
#'   is auto-selected with a warning. Include `"code"` in `spec_vars` to
#'   display sample as a grid row when multiple codes are plotted.
#' @param rank_by Character scalar. The `estimand` whose estimates determine
#'   the x-axis ordering of specifications. Defaults to `"Mean"`, falling back
#'   to `"UQR p50"` if `Mean` is absent.
#' @param rank_by_bin Integer. When binned specs are detected, which bin
#'   (1 = lowest) drives the ranking. Defaults to `1L`.
#' @param spec_vars Character vector of column names to display in the bottom
#'   specification grid. Parsed weather components (`wx_var`,
#'   `wx_ref_period`, `wx_binned`) and `code` can be included alongside the
#'   raw model columns. Defaults to `c("wx_var", "wx_ref_period", "wx_binned",
#'   "engine", "fe_profile", "cov_profile", "cov_method", "interaction",
#'   "model")`.
#' @param drop_constant Logical. If `TRUE` (default), columns in `spec_vars`
#'   with only one unique value after filtering are silently dropped from the
#'   grid.
#' @param ci_level Numeric. Z-multiplier for the confidence interval half-width
#'   (`estimate ± ci_level * std_error`). Defaults to `1.96`.
#' @param colour_by Character scalar: `"auto"` (default), `"estimand"`, or
#'   `"wx_bin"`. `"auto"` resolves to `"wx_bin"` when bins are detected and
#'   `"estimand"` otherwise.
#' @param heights Numeric vector of length 2. Relative heights of the top and
#'   bottom panels passed to [patchwork::plot_layout()]. Defaults to
#'   `c(3, 2)`.
#' @param title Character scalar. Plot title passed to
#'   [patchwork::plot_annotation()]. `NULL` (default) omits the title.
#' @param subtitle Character scalar. Plot subtitle. `NULL` (default) omits it.
#' @param y_label Character scalar. Y-axis label for the top panel. Defaults
#'   to `"Coefficient estimate"`.
#' @param point_size Numeric. Controls the size of the point in
#'   [ggplot2::geom_pointrange()]. Defaults to `1.5`.
#' @param line_size Numeric. Controls the `linewidth` of the range lines in
#'   [ggplot2::geom_pointrange()]. Defaults to `0.4`.
#'
#' @return A [patchwork] object combining the top and bottom panels, returned
#'   invisibly. Print or pass to [ggplot2::ggsave()] to render.
#'
#' @examples
#' # Build minimal sample data matching the _summary_coefficients.csv structure
#' set.seed(42)
#' specs <- expand.grid(
#'   weather     = c("t_1m_cont", "t_3m_cont", "t_6m_cont", "t_12m_cont"),
#'   interaction = c("urban", "electricity"),
#'   stringsAsFactors = FALSE
#' )
#' mean_rows <- dplyr::mutate(specs,
#'   code = "All countries", engine = "fixest", model = "fit3",
#'   fe_profile = "default", cov_profile = "hhsize", cov_method = "User-defined",
#'   term = "t", estimand = "Mean",
#'   estimate  = rnorm(nrow(specs), mean = -0.05, sd = 0.03),
#'   std_error = runif(nrow(specs), 0.01, 0.02)
#' )
#' uqr_rows <- dplyr::bind_rows(lapply(c("UQR p10", "UQR p50", "UQR p90"), function(e) {
#'   dplyr::mutate(specs,
#'     code = "All countries", engine = "rif", model = "fit3",
#'     fe_profile = "default", cov_profile = "hhsize", cov_method = "User-defined",
#'     term = "t", estimand = e,
#'     estimate  = rnorm(nrow(specs), mean = -0.05, sd = 0.04),
#'     std_error = runif(nrow(specs), 0.01, 0.025)
#'   )
#' }))
#' sample_data <- dplyr::bind_rows(mean_rows, uqr_rows)
#'
#' p <- plot_spec_curve(
#'   data      = sample_data,
#'   term      = "t",
#'   estimands = c("Mean", "UQR p10", "UQR p50", "UQR p90"),
#'   spec_vars = c("wx_ref_period", "interaction"),
#'   title     = "Specification curve: temperature coefficient",
#'   subtitle  = "Model: fit3"
#' )
#' print(p)
plot_spec_curve <- function(
    data,

    # --- filtering -----------------------------------------------------------
    term         = NULL,
    term_regex   = NULL,
    estimands    = c("Mean", "UQR p10", "UQR p50", "UQR p90"),
    model_filter = "fit3",
    code_filter  = NULL,

    # --- ranking -------------------------------------------------------------
    rank_by      = "Mean",
    rank_by_bin  = 1L,

    # --- spec grid -----------------------------------------------------------
    spec_vars = c(
      "wx_var", "wx_ref_period", "wx_binned",
      "engine", "fe_profile", "cov_profile",
      "cov_method", "interaction", "model"
    ),
    drop_constant = TRUE,

    # --- CI ------------------------------------------------------------------
    ci_level  = 1.96,

    # --- display -------------------------------------------------------------
    colour_by  = "auto",
    heights    = c(3, 2),
    title      = NULL,
    subtitle   = NULL,
    y_label    = "Coefficient estimate",
    point_size = 1.5,
    line_size  = 0.4
) {

  # ---------------------------------------------------------------------------
  # 0. Validate
  # ---------------------------------------------------------------------------

  if (is.null(term) && is.null(term_regex))
    stop("Provide either `term` (exact match) or `term_regex` (regex pattern).")
  if (!is.null(term) && !is.null(term_regex))
    message("Both `term` and `term_regex` provided; using `term_regex`.")

  # ---------------------------------------------------------------------------
  # 1. Palettes & theme
  # ---------------------------------------------------------------------------

  estimand_palette <- c(
    "Mean"    = "#3d405b",
    "UQR p10" = "#e07a5f",
    "UQR p50" = "#81b29a",
    "UQR p90" = "#f2cc8f"
  )

  # Low (blue) -> high (red) for ordered bins
  bin_base_palette <- c("#4575b4", "#91bfdb", "#fc8d59", "#d73027")

  theme_spec <- function(hide_x = TRUE) {
    t <- ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        panel.grid.major.x = ggplot2::element_blank(),
        panel.grid.minor   = ggplot2::element_blank(),
        axis.ticks         = ggplot2::element_blank(),
        legend.position    = "top",
        legend.title       = ggplot2::element_blank(),
        legend.key.size    = ggplot2::unit(0.4, "cm"),
        strip.text.y.left  = ggplot2::element_text(angle = 0, hjust = 1,
                                                    size = 9, colour = "#3d405b"),
        strip.placement    = "outside"
      )
    if (hide_x)
      t <- t + ggplot2::theme(
        axis.text.x  = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank()
      )
    t
  }

  # ---------------------------------------------------------------------------
  # 2. Code filter — warn + auto-select
  # ---------------------------------------------------------------------------

  all_codes <- unique(data$code)

  if (!is.null(code_filter)) {
    data <- dplyr::filter(data, code %in% code_filter)
    if (nrow(data) == 0) stop("code_filter matched no rows.")
  } else if (length(all_codes) > 1) {
    chosen <- all_codes[[1]]
    warning(
      "Multiple codes found: ", paste(all_codes, collapse = ", "),
      ". Auto-selecting '", chosen,
      "'. Use code_filter to override, or include 'code' in spec_vars."
    )
    data <- dplyr::filter(data, code == chosen)
  }

  # ---------------------------------------------------------------------------
  # 3. Parse `weather` -> wx_var, wx_ref_period, wx_binned
  # ---------------------------------------------------------------------------

  data <- data |>
    dplyr::mutate(
      .wx           = stringr::str_match(weather, "^(.+?)_(\\d+)m_(cont|binn)$"),
      wx_var        = .wx[, 2],
      wx_ref_period = dplyr::if_else(
        !is.na(.wx[, 3]),
        paste0(.wx[, 3], dplyr::if_else(.wx[, 3] == "1", " month", " months")),
        NA_character_
      ),
      wx_binned = dplyr::case_when(
        .wx[, 4] == "binn" ~ "Yes",
        .wx[, 4] == "cont" ~ "No",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::select(-.wx)

  # NA -> "None" for profile columns
  data <- data |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c("fe_profile", "cov_profile", "cov_method")),
      ~ dplyr::if_else(is.na(.) | . == "NA", "None", .)
    ))

  # ---------------------------------------------------------------------------
  # 4. Filter: model, estimands, term / term_regex
  # ---------------------------------------------------------------------------

  if (!is.null(model_filter))
    data <- dplyr::filter(data, model %in% model_filter)

  data <- dplyr::filter(data, estimand %in% estimands)

  if (!is.null(term_regex)) {
    data <- dplyr::filter(data, stringr::str_detect(term, term_regex))
  } else {
    data <- dplyr::filter(data, term %in% !!term)
  }

  if (nrow(data) == 0)
    stop(
      "No rows remain after filtering. ",
      "Check `term`/`term_regex`, `model_filter`, and `estimands`."
    )

  # ---------------------------------------------------------------------------
  # 5. Detect and parse weather bins
  #
  #   Bin terms look like: t(17.6,22.2]  or  t(27.7, Inf]:urban
  #   Pattern captures lower and upper bounds.
  # ---------------------------------------------------------------------------

  bin_rx       <- "^.+?\\((-?[\\d.]+),\\s*(-?[\\d.]+|Inf)\\]"
  bins_present <- any(stringr::str_detect(unique(data$term), bin_rx))

  if (bins_present) {

    data <- data |>
      dplyr::mutate(
        .bm       = stringr::str_match(term, bin_rx),
        bin_lower = as.numeric(.bm[, 2]),
        bin_upper = .bm[, 3]   # string; may be "Inf"
      ) |>
      dplyr::select(-.bm)

    # bin_order: rank lower bound within each weather spec (1 = lowest value)
    data <- data |>
      dplyr::group_by(weather) |>
      dplyr::mutate(bin_order = dplyr::dense_rank(bin_lower)) |>
      dplyr::ungroup()

    # Ordinal labels: "Bin 2 (low)" … "Bin N (high)"
    # Exact break points intentionally omitted — they differ by reference
    # period; the caption notes this.
    n_bins_total <- dplyr::n_distinct(data$bin_order)
    data <- data |>
      dplyr::mutate(
        wx_bin = dplyr::case_when(
          bin_order == 1              ~ "Bin 2 (low)",
          bin_order == n_bins_total   ~ paste0("Bin ", bin_order + 1, " (high)"),
          TRUE                        ~ paste0("Bin ", bin_order + 1)
        )
      )

    bin_levels <- data |>
      dplyr::distinct(bin_order, wx_bin) |>
      dplyr::arrange(bin_order) |>
      dplyr::pull(wx_bin)

    data <- dplyr::mutate(data, wx_bin = factor(wx_bin, levels = bin_levels))

    if (colour_by == "auto") colour_by <- "wx_bin"

    if (colour_by == "wx_bin" && dplyr::n_distinct(data$estimand) > 1) {
      message(
        "Colouring by bin. With multiple estimands this creates ",
        dplyr::n_distinct(data$estimand), " series per bin. ",
        "Consider filtering via `estimands` for a cleaner plot, ",
        "e.g. estimands = \"Mean\"."
      )
    }

  } else {
    data <- dplyr::mutate(data, bin_order = NA_integer_, wx_bin = NA_character_)
    if (colour_by == "auto") colour_by <- "estimand"
  }

  # ---------------------------------------------------------------------------
  # 6. Engine / estimand collinearity check + spec identity key
  # ---------------------------------------------------------------------------

  spec_key_cols <- c(
    "code", "wx_var", "wx_ref_period", "wx_binned",
    "fe_profile", "cov_profile", "cov_method",
    "interaction", "model"
  )
  if (bins_present) spec_key_cols <- c(spec_key_cols, "bin_order")
  spec_key_cols <- intersect(spec_key_cols, names(data))

  if ("engine" %in% names(data)) {
    eng_est <- dplyr::distinct(data, engine, estimand)
    if (dplyr::n_distinct(eng_est$engine) < dplyr::n_distinct(eng_est$estimand)) {
      message(
        "Note: `engine` is collinear with `estimand`. ",
        "Excluded from spec key and spec_vars."
      )
      spec_vars <- setdiff(spec_vars, "engine")
    } else if ("engine" %in% spec_vars) {
      spec_key_cols <- union(spec_key_cols, "engine")
    }
  }

  # ---------------------------------------------------------------------------
  # 7. Compute CIs
  # ---------------------------------------------------------------------------

  data <- data |>
    dplyr::mutate(
      conf_low  = estimate - ci_level * std_error,
      conf_high = estimate + ci_level * std_error
    )

  # ---------------------------------------------------------------------------
  # 8. Derive ranks
  #
  #   rank_key = spec_key_cols minus bin_order so all bins within a spec share
  #   the same rank position.
  # ---------------------------------------------------------------------------

  effective_rank_by <- if (rank_by %in% unique(data$estimand)) {
    rank_by
  } else {
    alt <- intersect(c("UQR p50", estimands), unique(data$estimand))[[1]]
    warning(
      "rank_by estimand '", rank_by, "' not found; falling back to '", alt, "'."
    )
    alt
  }

  rank_key    <- setdiff(spec_key_cols, "bin_order")
  rank_filter <- dplyr::filter(data, estimand == effective_rank_by)
  if (bins_present) rank_filter <- dplyr::filter(rank_filter, bin_order == rank_by_bin)

  if (nrow(rank_filter) == 0)
    stop(
      "No rows for rank_by = '", effective_rank_by, "'",
      if (bins_present) paste0(", bin ", rank_by_bin) else "",
      ". Adjust `rank_by` or `rank_by_bin`."
    )

  rank_df <- rank_filter |>
    dplyr::arrange(estimate) |>
    dplyr::mutate(rank = dplyr::row_number()) |>
    dplyr::select(dplyr::all_of(rank_key), rank)

  data <- dplyr::left_join(data, rank_df, by = rank_key)

  rank_caption <- paste(
    paste0(
      "Ranked by ", effective_rank_by,
      if (bins_present) paste0(" (Bin ", rank_by_bin, ")") else "",
      ". Error bars \u00b1", ci_level, " SE."
    ),
    if (bins_present) paste(
      "Bin coefficients are relative to the omitted lowest bin category.",
      "Exact break points vary by reference period; labels are ordinal."
    ) else NULL
  )

  # ---------------------------------------------------------------------------
  # 9. Resolve spec_vars for bottom grid
  # ---------------------------------------------------------------------------

  grid_spec_vars      <- setdiff(spec_vars, c("wx_bin", "bin_order"))
  available_spec_vars <- intersect(grid_spec_vars, names(data))

  if (drop_constant) {
    n_uniq <- sapply(
      available_spec_vars,
      function(v) dplyr::n_distinct(data[[v]], na.rm = TRUE)
    )
    available_spec_vars <- available_spec_vars[n_uniq > 1]
  }

  if (length(available_spec_vars) == 0)
    stop("No spec_vars remaining after drop_constant. Check spec_vars argument.")

  var_labels <- c(
    wx_var        = "Weather var",
    wx_ref_period = "Reference period",
    wx_binned     = "Binned",
    engine        = "Engine",
    fe_profile    = "Fixed effects",
    cov_profile   = "Covariate set",
    cov_method    = "Cov. selection",
    interaction   = "Interaction",
    model         = "Model",
    code          = "Sample"
  )

  # ---------------------------------------------------------------------------
  # 10. Build plot_top
  # ---------------------------------------------------------------------------

  used_estimands <- intersect(estimands, unique(data$estimand))

  if (colour_by == "wx_bin") {
    bin_lvls <- levels(data$wx_bin)
    n_b      <- length(bin_lvls)
    pal <- if (n_b <= 4) {
      stats::setNames(bin_base_palette[seq_len(n_b)], bin_lvls)
    } else {
      stats::setNames(grDevices::colorRampPalette(c("#4575b4", "#d73027"))(n_b), bin_lvls)
    }
    colour_scale <- ggplot2::scale_colour_manual(values = pal, name = NULL)
    colour_var   <- "wx_bin"
  } else {
    pal   <- estimand_palette[names(estimand_palette) %in% used_estimands]
    extra <- setdiff(used_estimands, names(pal))
    if (length(extra) > 0)
      pal <- c(pal, stats::setNames(scales::hue_pal()(length(extra)), extra))
    colour_scale <- ggplot2::scale_colour_manual(
      values = pal, breaks = used_estimands, name = NULL
    )
    colour_var <- "estimand"
  }

  plot_top <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x      = rank,
      y      = estimate,
      ymin   = conf_low,
      ymax   = conf_high,
      colour = .data[[colour_var]]
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0, linetype = "dashed", colour = "grey60", linewidth = 0.4
    ) +
    ggplot2::geom_pointrange(
      linewidth = line_size,
      size      = point_size * 0.2,
      alpha     = 0.85,
      position  = ggplot2::position_dodge(width = 0.6)
    ) +
    colour_scale +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::labs(y = y_label) +
    theme_spec(hide_x = TRUE)

  # ---------------------------------------------------------------------------
  # 11. Build unique spec table for bottom grid
  #
  #   One row per spec (rank_key, excluding bin_order): bins share a rank
  #   position and are shown via colour in the top panel only.
  # ---------------------------------------------------------------------------

  spec_df <- data |>
    dplyr::select(dplyr::all_of(c(rank_key, "rank"))) |>
    dplyr::distinct()

  avail_in_spec <- intersect(available_spec_vars, names(spec_df))

  grid_long <- spec_df |>
    dplyr::select(rank, dplyr::all_of(avail_in_spec)) |>
    tidyr::pivot_longer(
      cols      = -rank,
      names_to  = "spec_var",
      values_to = "spec_value"
    ) |>
    dplyr::mutate(
      spec_var = factor(
        spec_var,
        levels = rev(avail_in_spec),
        labels = rev(dplyr::recode(avail_in_spec, !!!var_labels))
      )
    )

  # ---------------------------------------------------------------------------
  # 12. Build plot_bottom
  # ---------------------------------------------------------------------------

  plot_bottom <- ggplot2::ggplot(
    grid_long,
    ggplot2::aes(x = rank, y = spec_value)
  ) +
    ggplot2::geom_point(shape = 16, size = 1.8, colour = "#3d405b") +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(add = 0.5)) +
    ggplot2::facet_grid(
      spec_var ~ .,
      scales = "free_y",
      space  = "free_y",
      switch = "y"
    ) +
    ggplot2::labs(
      x = paste0("Specification rank (", nrow(spec_df), " specs)")
    ) +
    theme_spec(hide_x = FALSE) +
    ggplot2::theme(
      panel.background   = ggplot2::element_rect(fill = "grey97", colour = NA),
      panel.spacing.y    = ggplot2::unit(0.15, "cm"),
      axis.title.y       = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 8, colour = "#3d405b"),
      panel.grid.major.y = ggplot2::element_blank()
    )

  # ---------------------------------------------------------------------------
  # 13. Combine and return
  # ---------------------------------------------------------------------------

  combined <- (plot_top / plot_bottom) +
    patchwork::plot_layout(heights = heights) +
    patchwork::plot_annotation(
      title    = title,
      subtitle = subtitle,
      caption  = rank_caption,
      theme    = ggplot2::theme(
        plot.title    = ggplot2::element_text(size = 13, face = "bold",
                                              colour = "#3d405b"),
        plot.subtitle = ggplot2::element_text(size = 10, colour = "grey40"),
        plot.caption  = ggplot2::element_text(size = 8, colour = "grey50",
                                              hjust = 0)
      )
    )

  invisible(combined)
}


# =============================================================================
# Usage examples
# =============================================================================

d <- readr::read_csv(
  "dev/outputs/pooled/_summary_coefficients.csv",
  show_col_types = FALSE
)

# --- Continuous: temperature main effect ------------------------------------
p_cont <- plot_spec_curve(
  data         = d,
  term         = "t",
  model_filter = "fit3",
  estimands    = c("Mean", "UQR p10", "UQR p50", "UQR p90"),
  spec_vars    = c(
    "wx_var", "wx_ref_period", "wx_binned",
    "fe_profile", "cov_profile", "cov_method", "interaction"
  ),
  rank_by  = "Mean",
  title    = "Specification curve: temperature coefficient (continuous)",
  subtitle = "Outcome: electricity | Model: fit3"
)
print(p_cont)

# --- Binned: temperature bins, electricity interaction ----------------------
# term_regex matches e.g. "t(17.6,22.2]:electricity", "t(22.2,25.3]:electricity"
p_bin <- plot_spec_curve(
  data         = d,
  term_regex   = "^t\\(.*\\]:electricity$",
  model_filter = "fit3",
  estimands    = "Mean",   # single estimand keeps the plot clean when colouring by bin
  spec_vars    = c(
    "wx_ref_period", "wx_binned",
    "fe_profile", "cov_profile", "cov_method"
  ),
  rank_by      = "Mean",
  rank_by_bin  = 1L,       # rank specs by coldest bin
  colour_by    = "wx_bin",
  title    = "Specification curve: temperature bins × electricity",
  subtitle = "Outcome: electricity | Model: fit3 | Estimand: Mean"
)
print(p_bin)