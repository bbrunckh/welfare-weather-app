# ============================================================================ #
# Pure functions translating a selected-weather specification row into the     #
# stage sequence rendered by the "Selected weather" pipeline card.             #
# Stateless and testable without Shiny.                                        #
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Stage labels                                                                  #
# ---------------------------------------------------------------------------- #

#' Human label for a weather spec's reference window
#'
#' @param ref_start Integer; first month of the window, in months before the
#'   interview.
#' @param ref_end   Integer; last month of the window.
#'
#' @return e.g. `"1 month before interview"` or `"1-6 months before interview"`.
#'
#' @export
weather_ref_label <- function(ref_start, ref_end) {
  ref_start <- as.integer(ref_start[1])
  ref_end   <- as.integer(ref_end[1])
  if (anyNA(c(ref_start, ref_end))) return(NA_character_)
  if (identical(ref_start, ref_end)) {
    paste0(ref_start, " month", if (ref_start == 1) "" else "s",
           " before interview")
  } else {
    paste0(ref_start, "-", ref_end, " months before interview")
  }
}

#' Compact symbol for a temporal aggregation method
#'
#' @param agg Scalar character: `"Mean"`, `"Median"`, `"Sum"`, `"Min"`,
#'   `"Max"`.
#'
#' @return A single character symbol (`x\u0304`, `x\u0303`, `\u03A3`, ...).
#'
#' @export
weather_agg_symbol <- function(agg) {
  switch(as.character(agg[1]) %||% "",
    Mean   = "x\u0304",
    Median = "x\u0303",
    Sum    = "\u03A3",
    Min    = "\u2193",
    Max    = "\u2191",
    ""
  )
}

#' Label for the model form (bins or continuous) of a weather spec
#'
#' @param cont_binned   `"Binned"` or `"Continuous"`.
#' @param num_bins      Integer number of bins (binned specs).
#' @param binning_method Binning method name (binned specs).
#' @param custom_breaks Numeric vector of custom breaks (custom binning).
#' @param polynomial    Character vector of polynomial degrees (continuous).
#'
#' @return e.g. `"5 equal-width bins"`, `"4 custom bins"`,
#'   `"continuous \u00B7 quadratic"`.
#'
#' @export
weather_form_label <- function(cont_binned, num_bins, binning_method,
                               custom_breaks = numeric(0), polynomial = character(0)) {
  if (identical(cont_binned, "Binned")) {
    is_custom <- identical(binning_method, "Custom")
    n <- if (is_custom) length(custom_breaks) + 1L else as.integer(num_bins[1])
    method <- switch(as.character(binning_method),
      "Equal frequency" = "equal-frequency",
      "Equal width"     = "equal-width",
      "K-means"         = "k-means",
      "custom"
    )
    paste0(n, " ", method, " bins")
  } else {
    poly <- sort(as.character(polynomial))
    suffix <- switch(paste(poly, collapse = ","),
      "2"     = " \u00B7 quadratic",
      "3"     = " \u00B7 cubic",
      "2,3"   = " \u00B7 quadratic + cubic",
      ""
    )
    paste0("continuous", suffix)
  }
}


# ---------------------------------------------------------------------------- #
# Stage glyphs (compact inline SVG)                                             #
# ---------------------------------------------------------------------------- #

#' Timeline strip glyph: 12 month cells, reference window highlighted,
#' interview block at the right edge.
#' @noRd
.wx_timeline_svg <- function(ref_start, ref_end, height = 12) {
  cell <- 10
  gap  <- 2
  w_interview <- 12
  width  <- 12 * cell + 11 * gap + 4 + w_interview
  rects <- vapply(seq_len(12), function(p) {
    months_before <- 13 - p
    fill <- if (months_before >= ref_start && months_before <= ref_end) {
      "#0071BC"
    } else "#e3e9ee"
    sprintf('<rect x="%.1f" y="1" width="%d" height="%d" rx="2" fill="%s"/>',
            (p - 1) * (cell + gap), cell, height - 2, fill)
  }, character(1))
  x_int <- 12 * cell + 11 * gap + 4
  htmltools::HTML(paste0(
    '<svg width="', width, '" height="', height, '" viewBox="0 0 ', width, ' ',
    height, '" aria-hidden="true">',
    paste(rects, collapse = ""),
    '<rect x="', x_int, '" y="1" width="', w_interview,
    '" height="', height - 2, '" rx="2" fill="#002244"/></svg>'
  ))
}

#' Staircase glyph for binned specs (steps capped at 5 for legibility).
#' @noRd
.wx_steps_svg <- function(n_steps, width = 40, height = 14) {
  n <- max(2, min(as.integer(n_steps), 5))
  step_w <- (width - (n - 1)) / n
  rects <- vapply(seq_len(n), function(i) {
    h  <- round(height * i / n)
    sprintf('<rect x="%.1f" y="%.1f" width="%.1f" height="%.1f" rx="1.5" fill="#0071BC"/>',
            (i - 1) * (step_w + 1), height - h, step_w, h)
  }, character(1))
  htmltools::HTML(paste0(
    '<svg width="', width, '" height="', height, '" viewBox="0 0 ', width, ' ',
    height, '" aria-hidden="true">', paste(rects, collapse = ""), '</svg>'
  ))
}

#' Curve glyph for continuous specs; bends encode polynomial degree.
#' @noRd
.wx_curve_svg <- function(polynomial, width = 40, height = 14) {
  poly <- as.character(polynomial)
  d <- switch(paste(sort(poly), collapse = ","),
    "2"   = "M2,3 Q20,14 38,3",
    "3"   = "M2,3 C12,14 26,1 38,8",
    "2,3" = "M2,3 C10,14 16,1 24,9 S34,6 38,4",
    "M2,7 C14,5 26,9 38,7"
  )
  htmltools::HTML(paste0(
    '<svg width="', width, '" height="', height, '" viewBox="0 0 ', width, ' ',
    height, '" aria-hidden="true"><path d="', d,
    '" fill="none" stroke="#0071BC" stroke-width="1.8"/></svg>'
  ))
}

#' Dashed mean line + solid value line: deviation from mean.
#' @noRd
.wx_devline_svg <- function(width = 24, height = 12) {
  y_mean <- height - 3
  y_top  <- 2
  htmltools::HTML(paste0(
    '<svg width="', width, '" height="', height, '" viewBox="0 0 ', width, ' ',
    height, '" aria-hidden="true">',
    '<line x1="0" y1="', y_mean, '" x2="', width, '" y2="', y_mean,
    '" stroke="#5B6B79" stroke-width="1" stroke-dasharray="2.5 2"/>',
    '<line x1="3" y1="', y_mean, '" x2="', width - 3, '" y2="', y_top,
    '" stroke="#0071BC" stroke-width="1.6"/>',
    '</svg>'
  ))
}

#' Shaded spread band + dashed centre line: standardized anomaly.
#' @noRd
.wx_band_svg <- function(width = 24, height = 12) {
  band_h <- height - 4
  y_mid  <- 2 + band_h / 2
  htmltools::HTML(paste0(
    '<svg width="', width, '" height="', height, '" viewBox="0 0 ', width, ' ',
    height, '" aria-hidden="true">',
    '<rect x="0" y="2" width="', width, '" height="', band_h,
    '" fill="rgba(0,113,188,.12)"/>',
    '<line x1="0" y1="', y_mid, '" x2="', width, '" y2="', y_mid,
    '" stroke="#5B6B79" stroke-width="1" stroke-dasharray="2.5 2"/>',
    '</svg>'
  ))
}

#' Year-range strip on a 1950-to-today track, configured window highlighted.
#' @noRd
.wx_years_svg <- function(from, to, min_year = 1950L, width = 110, height = 10) {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  span <- max(this_year - min_year, 1L)
  x <- (as.integer(from) - min_year) / span * width
  w <- (as.integer(to) - as.integer(from)) / span * width
  h <- height - 2
  htmltools::HTML(paste0(
    '<svg width="', width, '" height="', height, '" viewBox="0 0 ', width, ' ',
    height, '" aria-hidden="true">',
    '<rect x="0" y="1" width="', width, '" height="', h, '" rx="1.5" fill="#e3e9ee"/>',
    '<rect x="', round(x, 1), '" y="1" width="', round(w, 1), '" height="', h,
    '" rx="1.5" fill="#0071BC"/></svg>'
  ))
}


# ---------------------------------------------------------------------------- #
# Pipeline assembly                                                             #
# ---------------------------------------------------------------------------- #

#' Build the stage list for one weather spec row
#'
#' Translates `ref_start`/`ref_end` -> window stage, `temporalAgg` ->
#' aggregation stage, `transformation` -> transformation stage (skipped when
#' `"None"`), and `cont_binned`/`num_bins`/`binning_method`/`custom_breaks`/
#' `polynomial` -> form stage.
#'
#' @param spec_row One-row data frame/tibble as produced by
#'   `build_selected_weather()`.
#'
#' @return A list of stages, each a list with `glyph` (HTML tag or `NULL`),
#'   `symbol` (character or `NULL`) and `label`.
#'
#' @export
weather_pipeline_stages <- function(spec_row) {
  r <- spec_row
  stages <- list()

  stages[[1]] <- list(
    glyph  = .wx_timeline_svg(r$ref_start[1], r$ref_end[1]),
    symbol = NULL,
    label  = weather_ref_label(r$ref_start, r$ref_end)
  )

  # Aggregation only means something when the window spans several months;
  # for a single month the value is that month's own reading.
  if (!identical(as.integer(r$ref_start[1]), as.integer(r$ref_end[1]))) {
    stages[[length(stages) + 1L]] <- list(
      glyph  = NULL,
      symbol = weather_agg_symbol(r$temporalAgg),
      label  = as.character(r$temporalAgg[1])
    )
  }

  if (!identical(as.character(r$transformation[1]), "None")) {
    stages[[length(stages) + 1L]] <- list(
      glyph  = if (identical(as.character(r$transformation[1]),
                             "Standardized anomaly")) {
        .wx_band_svg()
      } else {
        .wx_devline_svg()
      },
      symbol = NULL,
      label  = as.character(r$transformation[1])
    )
  }

  poly <- unlist(r$polynomial)
  brks <- as.numeric(unlist(r$custom_breaks))
  is_custom <- identical(as.character(r$binning_method[1]), "Custom")
  stages[[length(stages) + 1L]] <- list(
    glyph = if (identical(as.character(r$cont_binned[1]), "Binned")) {
      .wx_steps_svg(if (is_custom) length(brks) + 1L else as.integer(r$num_bins[1]))
    } else {
      .wx_curve_svg(poly)
    },
    symbol = NULL,
    label  = weather_form_label(
      cont_binned    = as.character(r$cont_binned[1]),
      num_bins       = as.integer(r$num_bins[1]),
      binning_method = as.character(r$binning_method[1]),
      custom_breaks  = brks,
      polynomial     = as.character(poly)
    )
  )

  stages
}


#' One pipeline stage chip (glyph/symbol + label)
#' @noRd
weather_stage_chip <- function(stage) {
  shiny::tags$span(
    class = "selection-card-stage",
    if (!is.null(stage$glyph)) stage$glyph,
    if (!is.null(stage$symbol) && nzchar(stage$symbol)) {
      shiny::tags$span(class = "selection-card-symbol", stage$symbol)
    },
    stage$label
  )
}

#' Pipeline row content for one weather spec row: variable name + sub, then
#' the left-to-right stage chips joined by chevrons.
#'
#' @param spec_row One-row data frame from `build_selected_weather()`.
#' @return A `shiny.tag` row, ready to pass as a pre-built row of
#'   `selection_summary_card()`.
#'
#' @export
weather_pipeline_row <- function(spec_row, hist_years = NULL) {
  r <- spec_row
  stages <- weather_pipeline_stages(r)

  chips <- lapply(stages, weather_stage_chip)

  # UI-55: the comparison period is where the chain starts - every stage
  # downstream is defined against it - so it leads the row as a chip rather
  # than sitting apart as a card badge.
  hist_chip <- if (!is.null(hist_years) &&
                   all(c("from", "to") %in% names(hist_years))) {
    shiny::tags$span(
      class = "selection-card-stage selection-card-stage-hist",
      paste0("History ", hist_years[["from"]], "-", hist_years[["to"]])
    )
  }
  if (!is.null(hist_chip)) chips <- c(list(hist_chip), chips)
  # interleave: chip, chevron, chip, ... inside one wrapping pipe span
  pipe_kids <- list()
  for (i in seq_along(chips)) {
    pipe_kids[[length(pipe_kids) + 1L]] <- chips[[i]]
    if (i < length(chips)) {
      pipe_kids[[length(pipe_kids) + 1L]] <-
        shiny::tags$span(class = "selection-card-chevron", "\u203A")
    }
  }

  label <- sub("^Monthly\\s+", "", as.character(r$label[1]))
  label <- paste0(toupper(substr(label, 1, 1)), substr(label, 2, nchar(label)))
  units <- as.character(r$units[1])

  # UI-55: the chevrons used to start only between the pipeline chips, so the
  # row read as "<variable>  <stage> > <stage>" - a separator that appeared
  # partway along and looked like an inconsistency rather than a chain. The
  # variable is where the chain starts, so it gets one too and every adjacent
  # pair in the row is separated the same way.
  shiny::tags$div(
    class = "selection-card-row",
    shiny::tags$span(
      class = "selection-card-wname",
      shiny::tags$span(class = "selection-card-name", label),
      shiny::tags$span(
        class = "selection-card-sub",
        if (!is.na(units) && nzchar(units)) {
          paste0(r$name[1], " \u00B7 ", units)
        } else {
          as.character(r$name[1])
        }
      )
    ),
    if (length(chips)) {
      shiny::tags$span(class = "selection-card-chevron", "\u203A")
    },
    shiny::tags$span(
      class = "selection-card-pipe",
      do.call(htmltools::tagList, pipe_kids)
    )
  )
}

#' Assemble the weather pipeline rows for `selection_summary_card()`
#'
#' @param sw A data frame from `build_selected_weather()` (one or more rows).
#' @param hist_years Optional named vector `c(from = , to = )`; when supplied
#'   it leads each row's chain as a "History from-to" chip (UI-55).
#'
#' @return A list of pre-built row tags.
#'
#' @export
weather_pipeline_rows <- function(sw, hist_years = NULL) {
  lapply(seq_len(nrow(sw)),
         function(i) weather_pipeline_row(sw[i, ], hist_years = hist_years))
}
