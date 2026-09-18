# Survey statistics and data preparation ----
# Pure functions for survey statistics and data preparation logic.
# Used by mod_1_02_surveystats_server().
# All functions are stateless and testable without a Shiny session.


# Time columns ----

#' Add derived time columns to a survey data frame
#'
#' Constructs `month` (integer 1-12), and `countryyear` (character label) from the integer
#' columns `int_year` and `int_month` that are present in raw survey parquet
#' files.
#'
#' @param df A data frame containing integer columns `int_year`, `int_month`,
#'   `economy`, and `year`.
#'
#' @return `df` with two additional columns: `month` (integer), and `countryyear` (character).
#'
#' @export
add_time_columns <- function(df) {
  timestamp <- df$timestamp
  month <- if (inherits(timestamp, "Date") || inherits(timestamp, "POSIXt")) {
    # POSIXlt exposes the month directly and avoids formatting every date.
    as.integer(as.POSIXlt(timestamp)$mon) + 1L
  } else {
    lubridate::month(timestamp)
  }

  df$month <- month
  df$countryyear <- paste0(df$economy, ", ", df$year)
  df
}


# Data level ----

#' Assign a data level indicator for CPI/PPP joining
#'
#' Produces a `data_level` column used to join the CPI/PPP deflator table.
#' For China (`code == "CHN"`) the level is `"urban"` or `"rural"` based on
#' the `urban` column; for all other countries the level is `"national"`.
#'
#' @param df A data frame containing columns `code` (character ISO3) and
#'   `urban` (integer or logical; 1/TRUE = urban, 0/FALSE = rural).
#'
#' @return `df` with an additional `data_level` column (character).
#'
#' @export
assign_data_level <- function(df) {
  df |>
    dplyr::mutate(
      data_level = dplyr::case_when(
        code == "CHN" & urban == 1 ~ "urban",
        code == "CHN" & urban == 0 ~ "rural",
        .default = "national"
      )
    )
}


# LCU variable identification ----

#' Identify LCU monetary variables present in a data frame
#'
#' Filters a variable list to variables flagged as Local Currency Unit (LCU)
#' that are also present as columns in `df`. These variables should be
#' deflated to 2021 PPP before analysis.
#'
#' @param df A data frame (survey microdata).
#' @param variable_list A data frame with at minimum columns `name` (character)
#'   and `units` (character). Rows with `units == "LCU"` are candidates.
#'
#' @return A character vector of variable names that are both LCU and present
#'   in `df`. Returns `character(0)` when none qualify.
#'
#' @export
get_lcu_vars <- function(df, variable_list) {
  variable_list |>
    dplyr::filter(units == "LCU", name %in% colnames(df)) |>
    dplyr::pull(name)
}


# LCU to PPP conversion ----

#' Convert LCU monetary variables to 2021 PPP
#'
#' Left-joins the CPI/PPP deflator table to `df` (matching on `code`, `year`,
#' and `data_level`) then divides each LCU variable by `cpi * ppp2021`.
#' `assign_data_level()` must be called on `df` before this function.
#'
#' @param df A data frame with columns `code`, `year`, and `data_level`
#'   (as produced by `assign_data_level()`), plus any columns named in
#'   `lcu_vars`.
#' @param cpi_ppp_data A data frame of CPI/PPP deflators with columns `code`,
#'   `year`, `data_level`, `cpi`, and `ppp2021`.
#' @param lcu_vars A character vector of column names in `df` to convert.
#'   If `character(0)` `df` is returned unchanged.
#'
#' @return `df` with the `lcu_vars` columns rescaled to 2021 PPP and the
#'   deflator join columns (`cpi`, `ppp2021`) attached.
#'
#' @export
convert_lcu_to_ppp <- function(df, cpi_ppp_data, lcu_vars) {
  if (length(lcu_vars) == 0) {
    return(df)
  }
  df |>
    dplyr::left_join(cpi_ppp_data, by = c("code", "year", "data_level")) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(lcu_vars), ~ .x / cpi / ppp2021)
    )
}

# Bottom code welfare (2021 PPP) ----

#' Bottom code welfare (2021 PPP)
#'
#' Replaces any welfare values below `floor_value` $/person/day (2021 PPP) with `floor_value`.
#'
#' @param df A data frame with column `welfare`
#' @param floor_value A numeric value representing the minimum welfare value in $/person/day at 2021 PPP.
#'
#' @return `df` with the
#'
#' @export
bottom_code_welfare <- function(df, floor_value = 0.28) {
  if ("welfare" %in% colnames(df)) {
    df |>
      dplyr::mutate(
        welfare = ifelse(welfare < floor_value, floor_value, welfare)
      )
  } else {
    return(df)
  }
}

# Interview date summary ----

#' Summarise interview dates for the timing-of-interviews bar chart
#'
#' Groups survey microdata by `economy`, `countryyear`, and interview month,
#' counting the number of household records (`hh`) in each month. Rows with
#' missing timestamps are dropped before aggregating. When the `month` column
#' created by [add_time_columns()] is present, it is reused rather than
#' extracting the month from `timestamp` a second time.
#'
#' @param df A data frame with columns `economy` (character), `countryyear`
#'   (character), and `timestamp` (Date), as produced by `add_time_columns()`.
#'
#' @return A data frame with columns `economy`, `countryyear`, `month_num`
#'   (integer 1-12), and `hh` (integer count). Returns a zero-row
#'   data frame when `df` is empty or all timestamps are `NA`.
#'
#' @export
summarise_interview_dates <- function(df) {
  empty <- data.frame(
    economy = character(0), countryyear = character(0),
    month_num = integer(0), hh = integer(0),
    stringsAsFactors = FALSE
  )
  if (is.null(df) || nrow(df) == 0L ||
    !all(c("timestamp", "economy") %in% names(df))) {
    return(empty)
  }

  ok <- !is.na(df$timestamp)
  if (!any(ok)) {
    return(empty)
  }

  month_num <- if ("month" %in% names(df)) {
    as.integer(df$month)
  } else if (inherits(df$timestamp, "Date") || inherits(df$timestamp, "POSIXt")) {
    as.integer(as.POSIXlt(df$timestamp)$mon) + 1L
  } else {
    lubridate::month(df$timestamp)
  }
  ok <- ok & !is.na(month_num)
  if (!any(ok)) {
    return(empty)
  }

  countryyear <- if ("countryyear" %in% names(df)) {
    df$countryyear
  } else {
    paste0(df$economy, ", ", df$year)
  }
  grouped <- data.frame(
    economy = as.character(df$economy[ok]),
    countryyear = as.character(countryyear[ok]),
    month_num = month_num[ok],
    stringsAsFactors = FALSE
  )

  # GRP performs the only grouping pass needed by this chart and is already a
  # package dependency used by the survey statistics tables.
  groups <- collapse::GRP(
    grouped,
    by = c("economy", "countryyear", "month_num"),
    group.sizes = TRUE
  )
  out <- groups$groups
  out$hh <- as.integer(groups$group.sizes)
  out
}


# Welfare poverty lines ----

#' Standard welfare poverty line thresholds (2021 PPP)
#'
#' Returns the three World Bank international poverty lines expressed in
#' USD per day at 2021 PPP prices, along with display labels. Used to add
#' reference lines to the welfare distribution plot.
#'
#' @return A data frame with columns `value` (numeric, $/day 2021 PPP) and
#'   `label` (character, formatted label for annotation).
#'
#' @export
welfare_poverty_lines <- function() {
  data.frame(
    value = c(3.00, 4.20, 8.30),
    label = c("$3.00", "$4.20", "$8.30"),
    stringsAsFactors = FALSE
  )
}


# Interview date bar chart ----

# Ordered waves and display labels behind the interview-date chart. Shared by
# the ggplot renderer (plot_interview_dates) and the echarts4r renderer
# (echart_interview_dates), so the two can never drift apart.
#' @param plot_data The summarise_interview_dates() frame.
#' @param wave_labels Optional named character vector replacing wave labels.
#' @return A list with `waves` (ordered countryyear keys) and `display`
#'   (labels shown in legend/facets).
#' @noRd
.interview_wave_info <- function(plot_data, wave_labels = NULL) {
  waves <- unique(as.character(plot_data$countryyear))
  wave_info <- data.frame(
    countryyear = waves,
    year = suppressWarnings(as.integer(sub(
      ".*,[[:space:]]*", "", waves
    ))),
    economy = vapply(waves, function(wave) {
      as.character(plot_data$economy[
        match(wave, as.character(plot_data$countryyear))
      ])[1L]
    }, character(1)),
    stringsAsFactors = FALSE
  )
  wave_info$economy[is.na(wave_info$economy) |
    !nzchar(wave_info$economy)] <- "Unknown"
  wave_info <- wave_info[order(
    wave_info$economy,
    is.na(wave_info$year),
    wave_info$year,
    wave_info$countryyear
  ), , drop = FALSE]
  waves <- wave_info$countryyear
  display_waves <- waves
  if (!is.null(wave_labels)) {
    mapped <- unname(wave_labels[waves])
    keep <- !is.na(mapped) & nzchar(mapped)
    display_waves[keep] <- mapped[keep]
  }
  list(waves = waves, display = display_waves)
}

# Wave palette behind the interview-date chart, shared by both renderers.
#' @noRd
.interview_wave_colors <- function(palette, waves) {
  cols <- switch(palette,
    sequential = .wave_palette(waves),
    okabe_ito = stats::setNames(
      c("#0072B2", "#009E73", "#E69F00", "#56B4E9", "#CC79A7"), waves
    ),
    wise = stats::setNames(
      c("#0071BC", "#00AB51", "#FDB714", "#009FDA", "#5B6B79"), waves
    ),
    blue = stats::setNames(
      c("#003B5C", "#0071BC", "#2C9CCB", "#78C6D0", "#B6DDE2"), waves
    )
  )
  if (palette != "sequential") {
    cols <- stats::setNames(rep(cols, length.out = length(waves)), waves)
  }
  cols
}

# Thousands-separated integer formatter injected into the chart's axes and
# tooltip (scales::label_number(big.mark = ",") equivalent, client-side).
#' @noRd
.interview_count_fmt <- function() {
  htmlwidgets::JS(paste0(
    "function(v) { if (v == null || isNaN(v)) return ''; ",
    "var s = Number(v).toFixed(0); ",
    "return s.replace(/\\B(?=(\\d{3})+(?!\\d))/g, ','); }"
  ))
}

#' Timing-of-interviews bar chart (echarts4r)
#'
#' The on-screen renderer for the monthly interview calendar: grouped bars,
#' one series per survey wave, months fixed to Jan-Dec. Interaction (hover,
#' legend toggling) runs entirely in the browser, so chart re-renders never
#' touch the Shiny event loop. Shares wave ordering, display labels and the
#' wave palette with [plot_interview_dates()], which remains the static
#' renderer behind the ggplot-based export path.
#'
#' @param plot_data A data frame with columns `month_num` (integer 1-12),
#'   `hh` (numeric count), `economy` (character), and `countryyear`
#'   (character), as returned by `summarise_interview_dates()`.
#' @param unit_label Y-axis noun for the observation unit ("Households",
#'   "Individuals", "Firms").
#' @param wave_labels Optional named character vector replacing wave labels.
#' @param palette Fill palette (see [plot_interview_dates()]).
#' @param height Widget height; a CSS length or a number of pixels.
#'
#' @return An `echarts4r` widget, or `NULL` invisibly when `plot_data` is
#'   `NULL` or has zero rows.
#'
#' @export
echart_interview_dates <- function(plot_data,
                                   unit_label = "Households",
                                   wave_labels = NULL,
                                   palette = c("sequential", "okabe_ito", "wise", "blue"),
                                   height = "300px") {
  if (is.null(plot_data) || nrow(plot_data) == 0 ||
    !all(c("countryyear", "month_num", "hh") %in% names(plot_data))) {
    return(invisible(NULL))
  }
  palette <- match.arg(palette)
  unit_label <- as.character(unit_label)[1L]
  if (is.na(unit_label) || !nzchar(unit_label)) unit_label <- "Observations"

  info <- .interview_wave_info(plot_data, wave_labels)
  waves <- info$waves
  cols <- unname(.interview_wave_colors(palette, waves))

  # One row per (wave, month) with zero-filled months, so every series covers
  # the same 12 categories (the ggplot scale_x_discrete(drop = FALSE) parity).
  month_labels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  grid <- expand.grid(
    countryyear = waves,
    month_num = 1:12,
    stringsAsFactors = FALSE
  )
  grid <- merge(grid, plot_data[, c("countryyear", "month_num", "hh")],
    all.x = TRUE
  )
  grid$hh[is.na(grid$hh)] <- 0
  grid$month <- factor(month_labels[grid$month_num], levels = month_labels)
  # Series (and legend entries) appear in first-appearance order, so the
  # display-labelled wave factor fixes both.
  grid$wave <- factor(
    info$display[match(grid$countryyear, waves)],
    levels = info$display
  )
  grid <- grid[order(grid$wave, grid$month_num), ]

  fmt <- .interview_count_fmt()

  grid |>
    echarts4r::group_by(wave) |>
    echarts4r::e_charts(month, height = height) |>
    echarts4r::e_bar(hh) |>
    echarts4r::e_color(cols) |>
    echarts4r::e_legend(orient = "horizontal", right = 36, top = 0) |>
    echarts4r::e_x_axis(
      axisLabel = list(interval = 0L, fontSize = 11),
      axisTick = list(alignWithLabel = TRUE)
    ) |>
    echarts4r::e_y_axis(
      name = unit_label,
      nameLocation = "end",
      nameTextStyle = wise_eaxis_name(),
      axisLabel = list(formatter = fmt)
    ) |>
    echarts4r::e_tooltip(
      trigger = "axis",
      axisPointer = list(type = "shadow"),
      valueFormatter = fmt
    ) |>
    echarts4r::e_grid(containLabel = TRUE, left = 8, right = 14, top = 42, bottom = 8) |>
    wise_echart_theme()
}


#' Plot timing of survey interviews by month.
#'
#' @param plot_data A data frame with columns `month_num` (integer 1-12),
#'   `hh` (integer count), `economy` (character), and `countryyear`
#'   (character), as returned by `summarise_interview_dates()`.
#' @param variant Display style: `"grouped"` (default), `"faceted"`, or
#'   `"heatmap"`.
#' @param unit_label Y-axis noun for the observation unit. Defaults to
#'   `"Households"`; use `"Individuals"` or `"Firms"` when the selected
#'   survey files are at those levels.
#' @param palette Fill palette. The default `"sequential"` assigns each
#'   survey wave its colour from the shared wave palette (same series as the
#'   weather and outcome distribution charts). Other options are
#'   `"okabe_ito"`, `"wise"`, and `"blue"`.
#' @param wave_labels Optional named character vector replacing wave labels.
#'
#' @return A `ggplot` object, or `NULL` invisibly when `plot_data` is
#'   `NULL` or has zero rows.
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap geom_tile geom_text
#'   scale_x_discrete scale_fill_manual scale_fill_gradient scale_y_continuous
#'   labs theme element_text
#' @export
plot_interview_dates <- function(plot_data,
                                 variant = c("grouped", "faceted", "heatmap"),
                                 unit_label = "Households",
                                 palette = c("sequential", "okabe_ito", "wise", "blue"),
                                 wave_labels = NULL) {
  if (is.null(plot_data) || nrow(plot_data) == 0) {
    return(invisible(NULL))
  }
  variant <- match.arg(variant)
  palette <- match.arg(palette)
  unit_label <- as.character(unit_label)[1L]
  if (is.na(unit_label) || !nzchar(unit_label)) unit_label <- "Observations"

  month_labels <- c(
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
  )
  plot_data <- as.data.frame(plot_data)
  plot_data$month_fct <- factor(
    plot_data$month_num,
    levels = 1:12,
    labels = month_labels
  )
  info <- .interview_wave_info(plot_data, wave_labels)
  waves <- info$waves
  display_waves <- info$display
  plot_data$countryyear <- factor(plot_data$countryyear, levels = waves)
  wave_cols <- .interview_wave_colors(palette, waves)

  if (variant == "heatmap") {
    return(
      ggplot2::ggplot(
        plot_data,
        ggplot2::aes(
          x = .data$month_fct, y = .data$countryyear,
          fill = .data$hh
        )
      ) +
        ggplot2::geom_tile(colour = "white", linewidth = 0.7) +
        ggplot2::geom_text(
          ggplot2::aes(label = scales::comma(.data$hh)),
          size = 3, colour = "#1D2A35"
        ) +
        ggplot2::scale_x_discrete(drop = FALSE) +
        ggplot2::scale_fill_gradientn(
          colours = wise_seq_ramp(100),
          labels = scales::label_number(big.mark = ","),
          name = unit_label
        ) +
        ggplot2::labs(x = NULL, y = NULL) +
        theme_wise(base_size = 13) +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          legend.position = "top",
          legend.justification = "left",
          legend.key.width = grid::unit(1.5, "cm")
        )
    )
  }

  p <- if (variant == "faceted") {
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(x = .data$month_fct, y = .data$hh)
    ) +
      ggplot2::geom_col(fill = "#0071BC", width = 0.72) +
      ggplot2::facet_wrap(~countryyear, ncol = 1, scales = "free_y")
  } else {
    ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data$month_fct, y = .data$hh,
        fill = .data$countryyear
      )
    ) +
      ggplot2::geom_col(
        position = ggplot2::position_dodge2(width = 0.82, preserve = "single"),
        width = 0.72, colour = "white", linewidth = 0.2
      ) +
      ggplot2::scale_fill_manual(
        values = wave_cols, breaks = waves, labels = display_waves, drop = FALSE,
        name = NULL
      )
  }

  p +
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_continuous(
      labels = scales::label_number(big.mark = ","),
      expand = ggplot2::expansion(mult = c(0, 0.08))
    ) +
    ggplot2::labs(x = NULL, y = unit_label) +
    theme_wise(base_size = 13) +
    ggplot2::theme(
      axis.ticks.x = ggplot2::element_line(colour = "#5B6B79"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#E3E9EE"),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position = if (variant == "faceted") "none" else "top",
      legend.justification = "left",
      plot.margin = ggplot2::margin(4, 8, 4, 4)
    )
}


#' Survey waves present in a data frame, for a wave picker
#'
#' @param df Any frame carrying `code`, `year` and `survname` (and ideally
#'   `economy` for the label).
#'
#' @return A data frame with `code`, `year`, `survname`, `key`
#'   (`"code|year|survname"`) and `label`, ordered by label. `NULL` when the
#'   keys are absent.
#'
#' @export
survey_wave_list <- function(df) {
  keys <- c("code", "year", "survname")
  if (is.null(df) || !all(keys %in% names(df))) {
    return(NULL)
  }

  d <- df
  d$year <- as.character(d$year)
  if (!"economy" %in% names(d)) d$economy <- d$code

  w <- unique(d[, c(keys, "economy")])
  if (nrow(w) == 0) {
    return(NULL)
  }

  w$key <- paste(w$code, w$year, w$survname, sep = "|")
  w$label <- paste0(w$economy, ", ", w$year)
  w <- w[order(w$label), , drop = FALSE]
  rownames(w) <- NULL
  w
}


#' Build survey-wave metadata used by controls and plots
#'
#' @param df Any frame accepted by [survey_wave_list()].
#'
#' @return A list containing the ordered wave data frame and its plot labels.
#' @noRd
survey_wave_metadata <- function(df) {
  waves <- survey_wave_list(df)
  list(
    waves       = waves,
    plot_labels = wave_plot_labels(waves)
  )
}


#' Cache survey-wave metadata for one published survey generation
#'
#' @param session A Shiny session.
#' @param df Survey data used when the version is not cached.
#' @param generation Survey-data publication generation.
#'
#' @return The value from `survey_wave_metadata()`.
#' @noRd
cached_survey_wave_metadata <- function(session, df, generation) {
  cache <- session$userData$survey_wave_metadata
  if (!is.null(cache) && identical(cache$generation, generation)) {
    return(cache$value)
  }

  value <- survey_wave_metadata(df)
  session$userData$survey_wave_metadata <- list(
    generation = generation,
    value = value
  )
  value
}


#' Restrict a data frame to one survey wave
#'
#' @param df  A frame with `code`, `year`, `survname`.
#' @param key A `"code|year|survname"` string, or `"all"`.
#'
#' @return The filtered frame.
#'
#' @export
filter_by_wave <- function(df, key = "all") {
  keys <- c("code", "year", "survname")
  if (is.null(df) || identical(key, "all") || is.null(key)) {
    return(df)
  }
  if (!all(keys %in% names(df))) {
    return(df)
  }
  k <- paste(df$code, as.character(df$year), df$survname, sep = "|")
  df[k == key, , drop = FALSE]
}


# Sample density heatmap ----

#' Spread each location's sampled units across the H3 cells it covers
#'
#' Survey locations overlap - in a capital, dozens of `loc_id`s can cover the
#' same ground - so a map of outlined location polygons says little about how
#' many households sit where. H3 cells, by contrast, tile the plane without
#' gaps or overlaps, which makes them the natural unit for a heatmap.
#'
#' A location's units are spread over the cells it covers in proportion to
#' each cell's 2020 population (`pop_2020`), so a large settlement inside a
#' multi-cell location keeps most of its households; locations without usable
#' weights fall back to an even split. Cells are then summed across
#' locations, so a cell reached by several overlapping locations accumulates
#' a share from each. The result is smooth and the totals reconcile with the
#' sample exactly. Values are therefore fractional: a location of a dozen
#' households spread over two dozen cells contributes half a household to
#' each, and only the accumulation across overlapping locations builds that
#' back up.
#'
#' @param cell_map    Data frame with one row per location-cell pair: `code`,
#'   `year`, `survname`, `loc_id`, `h3`, optionally `pop_2020`.
#' @param survey_data Loaded survey observations, one row per sampled unit,
#'   with `code`, `year`, `survname` and `loc_id`.
#'
#' @return A data frame with one row per `h3` cell holding a positive share:
#'   `h3` and `n_units`. `NULL` when the inputs cannot be combined.
#'
#' @export
allocate_units_to_cells <- function(cell_map, survey_data) {
  out <- .density_cell_summary(cell_map, survey_data)
  if (is.null(out)) NULL else out$cells
}


#' @noRd
.density_cell_summary <- function(cell_map, survey_data) {
  keys <- c("code", "year", "survname", "loc_id")
  if (is.null(cell_map) || is.null(survey_data)) {
    return(NULL)
  }
  if (!all(c(keys, "h3") %in% names(cell_map))) {
    return(NULL)
  }
  if (!all(keys %in% names(survey_data))) {
    return(NULL)
  }

  cm <- cell_map
  cm$year <- as.character(cm$year)

  sd <- survey_data
  sd$year <- as.character(sd$year)

  # One location grouping supplies both the survey counts and stable IDs that
  # survive the duplicate-sensitive join onto the mapping rows.
  g_loc <- collapse::GRP(sd, by = keys, group.sizes = TRUE)
  n_loc <- g_loc$groups
  n_loc$n_units <- as.integer(g_loc$group.sizes)
  n_loc$.loc_group <- seq_len(g_loc$N.groups)

  cm <- cm |>
    dplyr::inner_join(n_loc, by = keys)
  if (nrow(cm) == 0) {
    return(NULL)
  }

  # Spread each location's units across its cells in proportion to the
  # cells' 2020 population, so a big settlement inside a multi-cell
  # location keeps most of its households. Locations without usable
  # weights (all NA / zero / negative) fall back to an even split.
  has_pop <- "pop_2020" %in% names(cm)
  loc_group <- cm$.loc_group
  n_cells <- collapse::fsum(rep.int(1L, nrow(cm)),
    g = loc_group,
    TRA = "replace"
  )
  alloc <- if (has_pop) {
    pop <- pmax(cm$pop_2020, 0, na.rm = TRUE)
    pop_sum <- collapse::fsum(pop,
      g = loc_group, na.rm = TRUE,
      TRA = "replace"
    )
    use_pop <- pop_sum > 0
    ifelse(use_pop, cm$n_units * pop / pop_sum, cm$n_units / n_cells)
  } else {
    cm$n_units / n_cells
  }

  g_h3 <- collapse::GRP(cm, by = "h3")
  n_units <- collapse::fsum(alloc, g = g_h3, na.rm = TRUE)
  # base::sum(..., na.rm = TRUE), used by the old summarise(), returns zero
  # for groups containing only NA/NaN allocations; collapse returns NA.
  n_units[is.na(n_units)] <- 0
  cells <- data.frame(
    h3 = g_h3$groups$h3,
    n_units = as.numeric(n_units),
    stringsAsFactors = FALSE
  )
  cells <- cells[cells$n_units > 0, , drop = FALSE]
  rownames(cells) <- NULL

  list(
    cells = cells,
    n_locations = length(unique(loc_group))
  )
}


# Per-wave density allocation in one pass (PERF-43) ----
# Same allocation rule as .density_cell_summary(), but the final h3 grouping
# carries the survey wave. A location's keys include its wave, so the
# allocation never crosses waves; the module therefore computes every wave's
# cells once per load, and a wave toggle re-slices (or row-sums) this table
# instead of re-running the grouped passes over the full mapping. The
# pooled-all result is the sum of the per-wave cells within rounding.
#' @param cell_map    Data frame with one row per location-cell pair.
#' @param survey_data Survey observations with the location keys.
#'
#' @return A list with `cells` (wave, h3, n_units; one row per positive
#'   wave-cell) and `n_locations` (named integer vector of mapped locations
#'   per wave). NULL when the inputs cannot be combined.
#' @noRd
.density_wave_summary <- function(cell_map, survey_data) {
  keys <- c("code", "year", "survname", "loc_id")
  if (is.null(cell_map) || is.null(survey_data)) {
    return(NULL)
  }
  if (!all(c(keys, "h3") %in% names(cell_map))) {
    return(NULL)
  }
  if (!all(keys %in% names(survey_data))) {
    return(NULL)
  }

  cm <- cell_map
  cm$year <- as.character(cm$year)

  sd <- survey_data
  sd$year <- as.character(sd$year)

  # One location grouping supplies both the survey counts and stable IDs that
  # survive the duplicate-sensitive join onto the mapping rows.
  g_loc <- collapse::GRP(sd, by = keys, group.sizes = TRUE)
  n_loc <- g_loc$groups
  n_loc$n_units <- as.integer(g_loc$group.sizes)
  n_loc$.loc_group <- seq_len(g_loc$N.groups)

  cm <- cm |>
    dplyr::inner_join(n_loc, by = keys)
  if (nrow(cm) == 0) {
    return(NULL)
  }

  # Same spread-and-sum allocation as .density_cell_summary(): units across
  # the cells a location covers, in proportion to pop_2020, falling back to
  # an even split without usable weights.
  has_pop <- "pop_2020" %in% names(cm)
  loc_group <- cm$.loc_group
  n_cells <- collapse::fsum(rep.int(1L, nrow(cm)),
    g = loc_group,
    TRA = "replace"
  )
  alloc <- if (has_pop) {
    pop <- pmax(cm$pop_2020, 0, na.rm = TRUE)
    pop_sum <- collapse::fsum(pop,
      g = loc_group, na.rm = TRUE,
      TRA = "replace"
    )
    use_pop <- pop_sum > 0
    ifelse(use_pop, cm$n_units * pop / pop_sum, cm$n_units / n_cells)
  } else {
    cm$n_units / n_cells
  }

  # The wave key matches filter_by_wave()'s `code|year|survname` strings.
  cm$wave <- paste(cm$code, cm$year, cm$survname, sep = "|")
  g_h3 <- collapse::GRP(cm, by = c("wave", "h3"))
  n_units <- collapse::fsum(alloc, g = g_h3, na.rm = TRUE)
  # base::sum(..., na.rm = TRUE) semantics: zero for all-NA allocations.
  n_units[is.na(n_units)] <- 0
  cells <- data.frame(
    wave = as.character(g_h3$groups$wave),
    h3 = as.character(g_h3$groups$h3),
    n_units = as.numeric(n_units),
    stringsAsFactors = FALSE
  )
  cells <- cells[cells$n_units > 0, , drop = FALSE]
  rownames(cells) <- NULL

  # Mapped locations per wave: one distinct (wave, location) row each.
  uq <- !duplicated(cm[, c("wave", ".loc_group")])
  wave_lvls <- unique(cm$wave)
  n_locations <- stats::setNames(
    as.integer(table(factor(cm$wave[uq], levels = wave_lvls))), wave_lvls
  )

  list(cells = cells, n_locations = n_locations)
}


#' Merge per-location values onto the H3 cells they cover
#'
#' Survey locations overlap: several `loc_id`s can cover the same ground, so
#' drawing one semi-transparent polygon per location stacks fills on top of
#' each other and invents colours that are in neither the data nor the legend.
#' Aggregating onto H3 cells - which tile without overlapping - gives one
#' unambiguous value per patch of ground.
#'
#' Each contributing location is weighted by how many of its sampled units sit
#' in that cell (its unit count times the cell's share of the location's
#' `pop_2020`). Continuous values are averaged with those weights; categorical
#' values (weather bins) take the highest-weight category.
#'
#' @param cell_map Data frame with one row per location-cell pair: `code`,
#'   `year`, `survname`, `loc_id`, `h3`, `pop_2020`.
#' @param loc_vals Data frame with one row per location: `code`, `year`,
#'   `survname`, `loc_id`, `value` and `n_hh`, optionally `n_months`. A
#'   `binned` attribute marks `value` as categorical.
#'
#' @param by_wave Keep waves separate (the default), giving one row per wave
#'   and cell. Set `FALSE` to pool every selected wave into a single value per
#'   cell - needed wherever all waves are drawn on one map, since a cell
#'   sampled by two waves would otherwise be painted twice.
#'
#' @return A data frame with one row per wave-cell: `code`, `year`,
#'   `survname`, `loc_id` (the H3 index, so the result drops straight into the
#'   location-map plotters), `value`, `n_hh`, `n_months` and `n_locs`. Carries
#'   the `binned` and `levels` attributes of `loc_vals`. `NULL` when the
#'   inputs cannot be combined.
#'
#' @export
merge_loc_values_to_cells <- function(cell_map, loc_vals, by_wave = TRUE) {
  keys <- c("code", "year", "survname", "loc_id")
  if (is.null(cell_map) || is.null(loc_vals) || nrow(loc_vals) == 0) {
    return(NULL)
  }
  if (!all(c(keys, "h3") %in% names(cell_map))) {
    return(NULL)
  }
  if (!all(c(keys, "value") %in% names(loc_vals))) {
    return(NULL)
  }

  binned <- isTRUE(attr(loc_vals, "binned"))
  lvls <- attr(loc_vals, "levels")

  cm <- cell_map
  cm$year <- as.character(cm$year)
  if (!"pop_2020" %in% names(cm)) cm$pop_2020 <- 1
  cm$pop_2020[!is.finite(cm$pop_2020) | cm$pop_2020 < 0] <- 0

  lv <- loc_vals
  lv$year <- as.character(lv$year)
  if (!"n_hh" %in% names(lv)) lv$n_hh <- 1
  if (!"n_months" %in% names(lv)) lv$n_months <- 1L

  carry <- intersect(c("economy"), names(lv))
  j <- cm |>
    dplyr::inner_join(
      lv[, c(keys, "value", "n_hh", "n_months", carry)],
      by = keys
    )
  if (nrow(j) == 0) {
    return(NULL)
  }

  # Units of this location that sit in this cell.
  j <- j |>
    dplyr::group_by(.data$code, .data$year, .data$survname, .data$loc_id) |>
    dplyr::mutate(
      .tot = sum(.data$pop_2020, na.rm = TRUE),
      .w = .data$n_hh * dplyr::if_else(.data$.tot > 0,
        .data$pop_2020 / .data$.tot,
        1 / dplyr::n()
      )
    ) |>
    dplyr::ungroup()

  grp <- if (by_wave) {
    c("code", "year", "survname", "h3")
  } else {
    "h3"
  }

  # `split()` dropped rows with missing grouping keys, so they are removed
  # before the grouping is built (PERF-05).
  j <- j[complete.cases(j[grp]), , drop = FALSE]
  if (nrow(j) == 0) {
    return(NULL)
  }

  g <- collapse::GRP(j, by = grp)
  n_g <- g$N.groups
  first_idx <- match(seq_len(n_g), g$group.id)
  w <- j$.w

  out <- data.frame(
    code = j$code[first_idx],
    year = j$year[first_idx],
    survname = j$survname[first_idx],
    loc_id = j$h3[first_idx],
    value = if (binned) rep(NA_character_, n_g) else rep(NA_real_, n_g),
    # The colour summarises more than one number when several locations
    # meet here, or when a contributing location spans several months.
    n_hh = {
      nh <- collapse::fsum(w, g = g, na.rm = TRUE)
      nh[is.na(nh)] <- 0
      nh
    },
    n_months = collapse::fmax(j$n_months, g = g, na.rm = TRUE),
    n_locs = as.integer(collapse::fndistinct(j$loc_id, g = g, na.rm = FALSE)),
    stringsAsFactors = FALSE
  )
  if (length(carry)) out$economy <- j$economy[first_idx]

  if (binned) {
    # Modal bin per cell by summed weight; ties broken by the alphabetical
    # order `tapply()` used before, and NA values carry no vote (PERF-05).
    vv <- as.character(j$value)
    ok <- !is.na(vv)
    if (any(ok)) {
      gv <- collapse::GRP(
        list(gid = g$group.id[ok], value = vv[ok]),
        group.sizes = TRUE
      )
      wsum <- collapse::fsum(w[ok], g = gv, na.rm = TRUE)
      wsum[is.na(wsum)] <- 0
      ord <- order(gv$groups$gid, -wsum, match(gv$groups$value, sort(unique(vv[ok]))))
      take <- ord[!duplicated(gv$groups$gid[ord])]
      out$value[gv$groups$gid[take]] <- gv$groups$value[take]
    }
  } else {
    vn <- suppressWarnings(as.numeric(j$value))
    ok <- is.finite(vn) & is.finite(w) & (w > 0)
    # Invalid rows are NA-ed out of both value and weight so the grouped mean
    # skips them, as the old per-cell mask did.
    vn2 <- vn
    w2 <- w
    vn2[!ok] <- NA_real_
    w2[!ok] <- NA_real_
    val <- suppressWarnings(collapse::fmean(vn2, g = g, w = w2, na.rm = TRUE))
    val[is.nan(val)] <- NA_real_
    out$value <- unname(val)
  }

  # `interaction()` ordered its levels with `code` varying fastest (by_wave)
  # and `factor(h3)` sorted alphabetically otherwise; restore that order.
  out <- if (by_wave) {
    out[order(out$loc_id, out$survname, out$year, out$code), ]
  } else {
    out[order(out$loc_id), ]
  }
  rownames(out) <- NULL
  attr(out, "binned") <- binned
  attr(out, "levels") <- lvls
  out
}


# Colour-ramp builders ----
# Stand-ins for leaflet's colour-scale helpers (colorNumeric / colorFactor):
# the map palettes only
# need a function mapping values to hex colours, not a leaflet dependency.
#' @noRd
.ramp_numeric <- function(colors, domain, na_color = "#cccccc") {
  lo <- domain[1]
  hi <- domain[length(domain)]
  ramp <- grDevices::colorRamp(colors)
  function(v) {
    v <- as.numeric(v)
    out <- rep(na_color, length(v))
    ok <- is.finite(v)
    if (any(ok)) {
      t <- if (hi > lo) (v[ok] - lo) / (hi - lo) else rep(0.5, sum(ok))
      t <- pmin(1, pmax(0, t))
      rgbv <- ramp(t)
      out[ok] <- grDevices::rgb(rgbv[, 1], rgbv[, 2], rgbv[, 3],
        maxColorValue = 255
      )
    }
    out
  }
}

#' @param levels Level order for binned palettes; the palette recycles
#'   across them, matching the leaflet colorFactor() mapping.
#' @noRd
.ramp_factor <- function(colors, levels, na_color = "#cccccc") {
  cols <- rep(colors, length.out = length(levels))
  names(cols) <- as.character(levels)
  function(v) {
    out <- rep(na_color, length(v))
    key <- as.character(v)
    hit <- key %in% names(cols)
    out[hit] <- unname(cols[key[hit]])
    out
  }
}

# Square-root domain + Mako ramp behind the density map's colour scale. The
# legend reads the same palette state as the payloads, so the two cannot
# drift apart.
#' @param n_units Numeric vector of per-cell unit counts.
#' @return A list with `breaks` (increasing colour-break values over the
#'   positive counts) and `ramp` (colour hex stops, light to dark, one per
#'   break).
#' @noRd
.density_ramp <- function(n_units) {
  # Binned colour scale. Cells averaging fewer than one unit — where a
  # location's units spread thin across many cells — share one pale "less
  # than 1" bin; the occupied range above 1 is split at its quantiles, so
  # rural variation stays visible and cities sit in the dark end. A single
  # global transform cannot do both: sqrt left most cells nearly
  # indistinguishable, log made thin cells look busier than they are.
  band <- rev(grDevices::hcl.colors(12, "Mako"))[3:11]
  pale <- band[1]
  pos <- n_units[is.finite(n_units) & n_units > 0]
  above <- pos[pos >= 1]
  has_thin <- length(pos) > length(above)
  fmt <- function(x) format(signif(x, 3), trim = TRUE, scientific = FALSE)

  if (length(above) == 0) {
    return(list(levels = "< 1", colors = pale, cuts = 1, thin = TRUE))
  }

  brks <- unique(stats::quantile(above,
    probs = seq(0, 1, length.out = 4),
    names = FALSE, type = 7
  ))
  cuts <- unique(brks[-c(1, length(brks))])
  cuts <- cuts[cuts > brks[1] & cuts < brks[length(brks)]]

  upper <- if (length(cuts) == 0) {
    paste0("\u2265 ", fmt(brks[1]))
  } else {
    edges <- c(brks[1], cuts, brks[length(brks)])
    vapply(seq_len(length(edges) - 1), function(i) {
      if (i == length(edges) - 1) {
        paste0("\u2265 ", fmt(edges[i + 1]))
      } else {
        paste0(fmt(edges[i]), " \u2013 ", fmt(edges[i + 1]))
      }
    }, character(1))
  }

  n_up <- length(upper)
  colors <- c(
    if (has_thin) pale,
    band[round(seq(3, length(band), length.out = n_up))]
  )
  levels <- c(if (has_thin) "< 1", upper)
  list(levels = levels, colors = colors, cuts = c(1, cuts), thin = has_thin)
}

# Long explanation under the density legend's info marker, shared by both
# renderers.
#' @noRd
.density_legend_info <- function(unit_label) {
  unit_s <- sub("s$", "", unit_label)
  paste0(
    "Number of sampled ", unit_label, " in each hexagon. A location's ",
    unit_label, " are spread across the hexagons it covers in proportion ",
    "to each cell's 2020 population, then summed across overlapping ",
    "locations, so the totals match the sample exactly. Values are ",
    "shares, not whole counts: a hexagon can hold less than one ",
    unit_s, " when a location covers many cells or its cells are ",
    "sparsely populated. Those thin cells share the palest colour; the ",
    "occupied range above one is split at its quantiles, so thin cells ",
    "stay pale and dense cities stay dark."
  )
}

#' Columnar hex-map payload for the sample density map
#'
#' The payload carries only cell ids and values; the browser decodes
#' geometry and applies colour. `v` stays on the count scale (the hover
#' tooltip shows raw counts), and the colour ramp interpolates on `v`
#' against the quantile breaks in `stops$domain`.
#'
#' @param cells      Data frame with `h3` and `n_units` (bbox columns ride
#'   along from `density_cells()` when present), as produced for one wave.
#' @param unit_label Plural noun for what a row of the survey is.
#'
#' @return A list with `payload` (for `hexmap_update()`) and `legend` (for
#'   `.compact_legend_html()`); `NULL` when there is nothing to draw.
#'
#' @noRd
.density_hex_payload <- function(cells, unit_label = "households") {
  if (is.null(cells) || nrow(cells) == 0) {
    return(NULL)
  }
  if (!all(c("h3", "n_units") %in% names(cells))) {
    return(NULL)
  }

  ok <- !is.na(cells$h3) & nzchar(cells$h3) &
    is.finite(cells$n_units) & cells$n_units > 0
  cells <- cells[ok, , drop = FALSE]
  if (nrow(cells) == 0) {
    return(NULL)
  }

  ramp_info <- .density_ramp(cells$n_units)
  lvls <- ramp_info$levels

  bounds <- NULL
  if (all(c("xmin", "ymin", "xmax", "ymax") %in% names(cells))) {
    bounds <- c(
      min(cells$xmin, na.rm = TRUE), min(cells$ymin, na.rm = TRUE),
      max(cells$xmax, na.rm = TRUE), max(cells$ymax, na.rm = TRUE)
    )
  }

  # Classify each cell into its bin: findInterval against the bin edges
  # (the first edge is the thin/occupied boundary at 1), shifted by one
  # level when the pale "< 1" bin exists.
  fi <- findInterval(cells$n_units, ramp_info$cuts)
  v <- lvls[pmin(length(lvls), fi + as.integer(ramp_info$thin))]

  payload <- hexmap_payload(
    h3     = cells$h3,
    v      = v,
    v_kind = "binned",
    stops  = list(levels = lvls, colors = ramp_info$colors),
    bounds = bounds,
    label  = paste0(.capitalise(unit_label), " per cell")
  )

  pal <- setNames(ramp_info$colors, lvls)
  list(
    payload = payload,
    legend = list(
      pal_info = list(pal = function(l) unname(pal[as.character(l)])),
      binned   = TRUE,
      levels   = lvls,
      title    = paste0(.capitalise(unit_label), " per area"),
      info     = .density_legend_info(unit_label)
    )
  )
}


# Upper-case the first letter only; `toupper()` on the whole word would shout.
#' @noRd
.capitalise <- function(x) {
  if (!nzchar(x %||% "")) {
    return(x)
  }
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}


# Summary stats tables ----

#' Build the summary-stats table frame by variable group flag
#'
#' Pure builder behind the survey summary-statistics tables: aggregates (or row-filters the
#' shared PERF-40 base), joins wave missingness and readable labels, and
#' applies the display transformations (N filter, sort, column renames,
#' HTML-escaped soft wrapping). Stateless and testable without a Shiny
#' session.
#'
#' @param df A survey data frame.
#' @param vl A data frame containing at least columns `name` and `label`
#'   (plus the flag column named by `flag_col` when `vars` is not given).
#' @param flag_col,vars,base See `make_stats_reactable()`.
#'
#' @return The display-ready summary data frame.
#' @noRd
stats_table_frame <- function(df, vl, flag_col = NULL, vars = NULL, base = NULL) {
  target <- if (!is.null(vars)) vars else vl$name[vl[[flag_col]] == 1]
  vars <- intersect(target, names(df))
  if (length(vars) == 0) {
    tag <- flag_col %||% "specified"
    return(data.frame(Note = paste("No", tag, "variables found")))
  }

  # PERF-42: when the module's one-pass display base is supplied it already
  # carries the full display pipeline (aggregation, missingness, labels, N
  # filter, sort, renames, wrapping), so a row filter reproduces the
  # standalone path exactly at a fraction of the cost. The hidden `.var` key
  # is dropped by the slice; an uncovered variable set returns the Note frame.
  # A base that predates a newly-requested variable (stale union) falls back
  # to the local aggregation below.
  base_list <- if (is.function(base)) base() else base
  if (!is.null(base_list) && !is.null(base_list$display) &&
      all(vars %in% base_list$vars)) {
    return(stats_display_slice(base_list, vl, flag_col = flag_col, vars = vars))
  }

  tab <- weighted_summary_long(df, vars = vars)

  # Add missingness by survey wave (countryyear) and variable
  if ("variable" %in% names(tab) && "countryyear" %in% names(tab)) {
    if (!"countryyear" %in% names(df)) {
      stop("countryyear column is required in survey_data() to compute wave-specific missingness.")
    }

    # Wave-specific missingness by countryyear and variable, in one
    # grouped pass (PERF-09)
    tab <- tab |>
      dplyr::left_join(survey_missingness_long(df, vars), by = c("countryyear", "variable"))
  }

  # Show only the readable variable label, falling back to the raw name
  if ("variable" %in% names(tab)) {
    lab_map <- vl[, c("name", "label"), drop = FALSE]
    tab <- tab |>
      dplyr::left_join(lab_map, by = c("variable" = "name")) |>
      dplyr::mutate(variable = dplyr::coalesce(.data$label, .data$variable)) |>
      dplyr::select(variable, dplyr::everything(), -dplyr::any_of("label"))
  }

  # Omit variables with no observed values and the redundant unweighted mean.
  # The degenerate empty frame (no group/weight column, or no numeric target
  # variables) carries no N column and renders as an empty table instead of
  # erroring on the filter.
  if ("N" %in% names(tab)) {
    tab <- tab |>
      dplyr::filter(is.na(.data$N) | .data$N > 0) |>
      dplyr::select(-dplyr::any_of("unweighted_mean"))
  }

  # Sort by variable label, then wave (countryyear) where available
  if (all(c("variable", "countryyear") %in% names(tab))) {
    tab <- tab |>
      dplyr::arrange(.data$variable, .data$countryyear)
  }

  tab <- .stats_display_polish(tab)
  tab
}

# Display transformations shared by stats_table_frame() and the one-pass
# base (stats_display_base()): countryyear rename, capitalized column names,
# HTML-escaped soft wrapping of text columns. One definition so the two
# paths cannot drift apart.
#' @param tab       The joined/filtered/sorted summary frame.
#' @param skip_cols Column names excluded from text wrapping (the base's
#'   hidden raw-name column).
#' @noRd
.stats_display_polish <- function(tab, skip_cols = character(0)) {
  # Column renaming ----
  if ("countryyear" %in% names(tab)) names(tab)[names(tab) == "countryyear"] <- "Country, Year"

  names(tab) <- vapply(names(tab), function(nm) {
    if (!nzchar(nm)) {
      return(nm)
    }
    paste0(toupper(substr(nm, 1, 1)), substr(nm, 2, nchar(nm)))
  }, character(1))

  wrap_width <- 28
  text_cols <- setdiff(
    names(tab)[vapply(tab, function(x) is.character(x) || is.factor(x), logical(1))],
    skip_cols
  )
  if (length(text_cols) > 0) {
    tab[text_cols] <- lapply(tab[text_cols], function(x) {
      x_chr <- as.character(x)
      vapply(x_chr, function(s) {
        if (is.na(s)) {
          return(NA_character_)
        }
        # HTML-escape each wrapped line before joining with the literal
        # <br> markup below (the table is rendered with escape = FALSE /
        # html = TRUE, so any unescaped data-derived text would render as
        # raw HTML; see SEC-05).
        lines <- strwrap(s, width = wrap_width)
        paste(htmltools::htmlEscape(lines), collapse = "<br>")
      }, character(1))
    })
  }

  tab
}

# One-pass display base (PERF-42) ----

#' Build the post-processed display frame for all summary-stats tables at once
#'
#' Extension of the module's shared union aggregation (PERF-40): the entire
#' display pipeline - wave missingness join, readable labels, N filter, sort,
#' column renames and text wrapping - runs once over the union of table
#' variables, and every table is then served by row-filtering this frame
#' (stats_display_slice()). The per-table cost drops to a row subset, instead
#' of repeating the joins, sort and per-cell wrapping for each of the module's
#' seven tables on every load.
#'
#' The frame carries the raw variable names in a hidden `.var` column; the
#' slice filters on it and drops it, so its output is identical to
#' `stats_table_frame()` for the same variable set.
#'
#' @param df   A survey data frame.
#' @param vl   A data frame with columns `name` and `label`.
#' @param vars Character vector: the union of variables behind every table.
#'
#' @return A list with `vars` (the union) and `display` (the polished frame),
#'   or NULL when no target variables survive.
#' @noRd
stats_display_base <- function(df, vl, vars) {
  vars <- intersect(vars, names(df))
  if (!length(vars)) {
    return(NULL)
  }

  tab <- weighted_summary_long(df, vars = vars)
  if (!nrow(tab)) {
    return(NULL)
  }

  # Wave-specific missingness by countryyear and variable, one grouped pass.
  if ("countryyear" %in% names(df)) {
    tab <- tab |>
      dplyr::left_join(survey_missingness_long(df, vars), by = c("countryyear", "variable"))
  }

  # Hidden raw-name column for slicing, then the readable label.
  tab$.var <- tab$variable
  lab_map <- vl[, c("name", "label"), drop = FALSE]
  tab <- tab |>
    dplyr::left_join(lab_map, by = c("variable" = "name")) |>
    dplyr::mutate(variable = dplyr::coalesce(.data$label, .data$variable)) |>
    dplyr::select(variable, .var, dplyr::everything(), -dplyr::any_of("label"))

  if ("N" %in% names(tab)) {
    tab <- tab |>
      dplyr::filter(is.na(.data$N) | .data$N > 0) |>
      dplyr::select(-dplyr::any_of("unweighted_mean"))
  }

  # Sorting once over the union: a row subset of a globally sorted frame is
  # itself sorted, so slices need no re-sort.
  if (all(c("variable", "countryyear") %in% names(tab))) {
    tab <- tab |>
      dplyr::arrange(.data$variable, .data$countryyear)
  }

  list(
    vars = vars,
    display = .stats_display_polish(tab, skip_cols = ".var")
  )
}

#' Row-filter the shared display frame for one table
#'
#' @param base         The list returned by `stats_display_base()`.
#' @param vl           Variable metadata (columns `name`, `label`).
#' @param flag_col,vars Variable selection: `vars` wins over `flag_col`.
#' @param round_digits Optional rounding applied to numeric display columns
#'   (everything but `N`); NULL keeps full precision for the export path.
#'
#' @return The display-ready frame for one table, identical to
#'   `stats_table_frame()` for the same variable set, or the `Note` frame
#'   when no target variables are covered.
#' @noRd
stats_display_slice <- function(base, vl, flag_col = NULL, vars = NULL,
                                round_digits = NULL) {
  display <- base$display
  target <- if (!is.null(vars)) vars else vl$name[vl[[flag_col]] == 1]
  tab <- display[display$.var %in% target, , drop = FALSE]
  tab$.var <- NULL
  if (!nrow(tab)) {
    tag <- flag_col %||% "specified"
    return(data.frame(Note = paste("No", tag, "variables found")))
  }
  if (is.numeric(round_digits)) {
    num <- setdiff(
      names(tab)[vapply(tab, is.numeric, logical(1))], "N"
    )
    for (nm in num) tab[[nm]] <- round(tab[[nm]], round_digits)
  }
  rownames(tab) <- NULL
  tab
}

#' Build a reactable summary table by variable group flag (guidelines §6)
#'
#' Same data pipeline as `build_stats_table()` (shared
#' one-pass display base via `build_stats_table()`), same 2-decimal display
#' for numeric columns except `N`, same soft-wrapped text cells - but the
#' pagination, search and sorting run client-side, so re-renders and
#' interactions never touch the Shiny event loop. The per-table CSV download
#' is a separate `wise_reactable_csv_button()` (client-side
#' `Reactable.downloadDataCSV()`), exported from the raw values.
#'
#' @param survey_data,variable_list,flag_col,vars,base See `build_stats_table()`.
#'
#' @return A `shiny.render.function` (from `reactable::renderReactable`).
#' @export
make_stats_reactable <- function(survey_data, variable_list, flag_col = NULL,
                                 vars = NULL, base = NULL) {
  reactable::renderReactable({
    shiny::req(survey_data())
    tab <- build_stats_table(survey_data, variable_list, flag_col, vars, base)
    if (is.null(tab)) {
      tag <- flag_col %||% "specified"
      tab <- data.frame(Note = paste("No", tag, "variables found"))
    }
    .stats_reactable(tab)
  })
}

# Shared reactable styling behind the summary-stats tables: compact rows,
# soft-wrapped text cells (class in custom.css), 2-decimal numerics except
# the raw integer N.
#' @noRd
.stats_reactable <- function(tab) {
  cols <- lapply(names(tab), function(nm) {
    x <- tab[[nm]]
    if (is.numeric(x) && !identical(nm, "N")) {
      reactable::colDef(
        format = reactable::colFormat(digits = 2),
        class = "wise-dt-wrap"
      )
    } else if (is.character(x) || is.factor(x)) {
      reactable::colDef(
        html = TRUE, class = "wise-dt-wrap", minWidth = 170
      )
    } else {
      reactable::colDef(class = "wise-dt-wrap", minWidth = 70)
    }
  })
  names(cols) <- names(tab)
  reactable::reactable(
    tab,
    columns = cols,
    compact = TRUE,
    searchable = FALSE,
    defaultPageSize = 10,
    showPageSizeOptions = TRUE,
    pageSizeOptions = c(10, 25, 50, 100),
    highlight = TRUE
  )
}

#' Build the summary-statistics data frame for export consumers
#'
#' Serves from the module's one-pass display base (PERF-42) when it covers
#' the requested variables - identical output to the standalone
#' `stats_table_frame()` path, at a row-filter's cost. Full precision is
#' kept: the bundle CSVs carry unrounded values, unlike the on-screen
#' tables' 2-decimal display.
#'
#' @noRd
build_stats_table <- function(survey_data, variable_list, flag_col = NULL,
                              vars = NULL, base = NULL) {
  df <- tryCatch(if (is.function(survey_data)) survey_data() else survey_data,
    error = function(e) NULL
  )
  if (is.null(df) || !nrow(as.data.frame(df))) {
    return(NULL)
  }
  vl <- tryCatch(if (is.function(variable_list)) variable_list() else variable_list,
    error = function(e) NULL
  )
  if (is.null(vl)) {
    return(NULL)
  }
  base_value <- tryCatch(if (is.function(base)) base() else base,
    error = function(e) NULL
  )
  target <- if (!is.null(vars)) vars else vl$name[vl[[flag_col]] == 1]
  if (!is.null(base_value) && !is.null(base_value$display) &&
    !is.null(base_value$vars) && all(target %in% base_value$vars)) {
    return(stats_display_slice(base_value, vl,
      flag_col = flag_col, vars = vars
    ))
  }
  tab <- stats_table_frame(df, vl,
    flag_col = flag_col, vars = vars,
    base = base_value
  )
  if (identical(names(tab), "Note")) {
    return(NULL)
  }
  tab
}


# Outcome summary statistics ----

#' Summary statistics for one outcome, pooled and per survey wave
#'
#' Computes the Outcome stats summary table in a few grouped `collapse`
#' passes (PERF-41): every wave's statistics and the pooled sample's come
#' out of one grouping, so switching the wave pill re-slices a precomputed
#' table instead of re-running the quantiles over the whole sample.
#'
#' Observations, Missing and Coverage (%) are raw row counts of outcome
#' availability. The remaining statistics are sample-weighted with the
#' `weight` column (unweighted when the column is absent): Mean, Std Dev
#' and the deciles P10-P90 (P50 = median) for numeric outcomes, and the
#' weighted share of 1s for binary outcomes (the 1/0 counts stay raw
#' counts). Weighted statistics use only rows with a finite, positive
#' weight - the app-wide validity rule - and the weighted SD uses
#' `collapse::fsd(w=)`, whose denominator is $\sum w - 1$ (NA when a
#' wave's valid weights sum to at most 1).
#'
#' `collapse::fquantile()` does not group in 2.x, so the per-wave deciles
#' run one weighted `fquantile()` (type 7) per contiguous group slice
#' after a radix sort on the group id.
#'
#' @param df Survey data (as produced by `outcome_data()`), including the
#'   outcome column, the `weight` column, and - for wave columns - `code`,
#'   `year`, `survname`.
#' @param outcome Character name of the outcome column in `df`.
#' @param type Character outcome type: `"numeric"` for the continuous
#'   statistics, anything else for the binary set.
#'
#' @return A list with `stat` (character stat ids, the matrix rownames),
#'   `label` (display labels), `waves` (character wave keys, `"all"` for
#'   the pooled sample first) and `mat` (numeric matrix: stats x waves,
#'   unformatted).
#'
#' @noRd
outcome_summary_wide <- function(df, outcome, type) {
  empty <- list(
    stat  = character(),
    label = character(),
    waves = "all",
    mat   = matrix(NA_real_, 0, 1, dimnames = list(NULL, "all"))
  )
  if (is.null(df) || !nrow(df) || !outcome %in% names(df)) {
    return(empty)
  }

  numeric <- identical(as.character(type)[1], "numeric")
  x <- df[[outcome]]

  # Sample weights: weighted statistics use only rows with a finite,
  # positive weight (the app-wide validity rule); counts stay raw.
  w_all <- if ("weight" %in% names(df)) {
    as.numeric(df$weight)
  } else {
    rep(1, nrow(df))
  }
  w_ok <- is.finite(w_all) & (w_all > 0)

  stat_ids <- if (numeric) {
    c(
      "n", "n_miss", "coverage", "mean", "sd", "min",
      paste0("p", seq(10, 90, 10)), "max"
    )
  } else {
    c("n", "n_miss", "coverage", "n1", "n0", "share1")
  }
  stat_labels <- if (numeric) {
    c(
      "Observations", "Missing", "Coverage (%)", "Mean", "Std Dev", "Min",
      "P10", "P20", "P30", "P40", "Median (P50)", "P60", "P70", "P80", "P90",
      "Max"
    )
  } else {
    c(
      "Observations", "Missing", "Coverage (%)",
      "Count = 1 (Yes)", "Count = 0 (No)", "Share = 1"
    )
  }

  # Wave columns: the same "code|year|survname" keys the wave pickers use,
  # with the pooled sample ("all") first.
  w <- survey_wave_list(df)
  has_waves <- !is.null(w)
  waves <- if (has_waves) c("all", w$key) else "all"
  mat <- matrix(NA_real_,
    nrow = length(stat_ids), ncol = length(waves),
    dimnames = list(stat_ids, waves)
  )

  key <- if (has_waves) {
    paste(df$code, as.character(df$year), df$survname, sep = "|")
  } else {
    rep("all", nrow(df))
  }
  g <- collapse::GRP(key, group.sizes = TRUE)
  gl <- as.character(g$groups[[1]])

  # Row counts and available (non-missing) values per wave. Group levels
  # are mapped by name onto the wave columns, so the pooled column and the
  # per-wave columns cannot drift apart.
  total <- setNames(as.numeric(g$group.sizes), gl)
  avail <- setNames(
    as.numeric(collapse::fsum(as.integer(!is.na(x)), g = g)), gl
  )
  miss <- total - avail

  n_all <- nrow(df)
  a_all <- sum(!is.na(x))
  mat["n", "all"] <- n_all
  mat["n_miss", "all"] <- n_all - a_all
  mat["coverage", "all"] <- 100 * a_all / max(n_all, 1)

  cols <- setdiff(intersect(gl, waves), "all")
  if (length(cols)) {
    mat["n", cols] <- total[cols]
    mat["n_miss", cols] <- miss[cols]
    mat["coverage", cols] <- 100 * avail[cols] / pmax(total[cols], 1)
  }

  if (numeric) {
    xv <- as.numeric(x)
    ok <- !is.na(xv) & w_ok
    xv <- xv[ok]
    xw <- w_all[ok]
    probs <- seq(0.1, 0.9, 0.1)
    deciles <- paste0("p", seq(10, 90, 10))

    mat["mean", "all"] <- if (length(xv)) collapse::fmean(xv, w = xw) else NA_real_
    # fsd's sum(w) - 1 denominator needs more than one unit of weight.
    mat["sd", "all"] <- if (length(xv) > 1 && sum(xw) > 1) {
      collapse::fsd(xv, w = xw)
    } else {
      NA_real_
    }
    mat["min", "all"] <- if (length(xv)) collapse::fmin(xv) else NA_real_
    mat["max", "all"] <- if (length(xv)) collapse::fmax(xv) else NA_real_
    if (length(xv)) {
      mat[deciles, "all"] <- collapse::fquantile(xv,
        probs = probs, w = xw,
        type = 7
      )
    }

    if (length(xv)) {
      gv <- collapse::GRP(key[ok], group.sizes = TRUE)
      gvl <- as.character(gv$groups[[1]])
      gcols <- intersect(gvl, waves)
      mat["mean", gcols] <-
        setNames(collapse::fmean(xv, g = gv, w = xw), gvl)[gcols]
      sds <- setNames(collapse::fsd(xv, g = gv, w = xw), gvl)
      swg <- setNames(collapse::fsum(xw, g = gv), gvl)
      sds[swg <= 1] <- NA_real_
      mat["sd", gcols] <- sds[gcols]
      mat["min", gcols] <- setNames(collapse::fmin(xv, g = gv), gvl)[gcols]
      mat["max", gcols] <- setNames(collapse::fmax(xv, g = gv), gvl)[gcols]

      # Grouped weighted deciles: sort valid rows by group id (radix), then
      # one weighted fquantile per contiguous slice.
      ord <- order(gv$group.id, method = "radix")
      xs <- xv[ord]
      ws <- xw[ord]
      sz <- gv$group.sizes
      starts <- c(1L, head(cumsum(sz), -1) + 1L)
      for (j in seq_along(gvl)) {
        wv <- gvl[[j]]
        if (!wv %in% waves) next
        idx <- starts[[j]] + seq_len(sz[[j]]) - 1L
        mat[deciles, wv] <- collapse::fquantile(xs[idx],
          probs = probs,
          w = ws[idx], type = 7
        )
      }
    }
  } else {
    xi <- suppressWarnings(as.integer(x))
    one <- as.integer(!is.na(xi) & xi == 1L)
    zero <- as.integer(!is.na(xi) & xi == 0L)

    n1_all <- sum(one)
    n0_all <- sum(zero)
    mat["n1", "all"] <- n1_all
    mat["n0", "all"] <- n0_all
    # Weighted share of 1s among rows with a valid weight.
    sw1_all <- sum(w_all * one * w_ok)
    sw0_all <- sum(w_all * zero * w_ok)
    mat["share1", "all"] <- if (sw1_all + sw0_all > 0) {
      sw1_all / (sw1_all + sw0_all)
    } else {
      NA_real_
    }

    n1w <- setNames(as.numeric(collapse::fsum(one, g = g)), gl)
    n0w <- setNames(as.numeric(collapse::fsum(zero, g = g)), gl)
    sw1w <- setNames(as.numeric(collapse::fsum(w_all * one * w_ok, g = g)), gl)
    sw0w <- setNames(as.numeric(collapse::fsum(w_all * zero * w_ok, g = g)), gl)
    mat["n1", cols] <- n1w[cols]
    mat["n0", cols] <- n0w[cols]
    den <- sw1w[cols] + sw0w[cols]
    share <- ifelse(den > 0, sw1w[cols] / den, NA_real_)
    mat["share1", cols] <- share
  }

  list(stat = stat_ids, label = stat_labels, waves = waves, mat = mat)
}

#' Format one wave column of the outcome summary for display
#'
#' Counts get thousands separators, coverage a percent suffix, other
#' statistics three decimals (the pooled table's formatting), and missing
#' values an en dash.
#'
#' @param s    The list returned by `outcome_summary_wide()`.
#' @param wave Wave key to format; falls back to the first wave when the
#'   key is unknown.
#'
#' @return A two-column data frame (`Statistic`, `Value`).
#'
#' @noRd
.format_outcome_summary <- function(s, wave = "all") {
  wave <- if (wave %in% colnames(s$mat)) wave else s$waves[1]
  v <- s$mat[, wave]
  counts <- c("n", "n_miss", "n1", "n0")
  fmt <- vapply(seq_along(v), function(i) {
    x <- v[[i]]
    if (is.na(x)) {
      return("\u2013")
    }
    id <- s$stat[[i]]
    if (id %in% counts) {
      format(x, big.mark = ",", scientific = FALSE)
    } else if (identical(id, "coverage")) {
      paste0(as.character(round(x, 1)), "%")
    } else {
      as.character(round(x, 3))
    }
  }, character(1), USE.NAMES = FALSE)
  data.frame(Statistic = s$label, Value = fmt, stringsAsFactors = FALSE)
}
