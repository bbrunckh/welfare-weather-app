# ============================================================================ #
# Pure functions for survey statistics and data preparation logic.             #
# Used by mod_1_02_surveystats_server(). 
# All functions are stateless and testable without a Shiny session.                                            #
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Time columns                                                                  #
# ---------------------------------------------------------------------------- #

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
  df |>
    dplyr::mutate(
      month       = lubridate::month(timestamp),
      countryyear = paste0(economy, ", ", year)
    )
}


# ---------------------------------------------------------------------------- #
# Data level                                                                    #
# ---------------------------------------------------------------------------- #

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


# ---------------------------------------------------------------------------- #
# LCU variable identification                                                   #
# ---------------------------------------------------------------------------- #

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


# ---------------------------------------------------------------------------- #
# LCU -> PPP conversion                                                         #
# ---------------------------------------------------------------------------- #

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
  if (length(lcu_vars) == 0) return(df)
  df |>
    dplyr::left_join(cpi_ppp_data, by = c("code", "year", "data_level")) |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(lcu_vars), ~ .x / cpi / ppp2021)
    )
}

# ---------------------------------------------------------------------------- #
# Bottom code welfare (2021 PPP)                                          #
# ---------------------------------------------------------------------------- #

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

# ---------------------------------------------------------------------------- #
# Interview date summary                                                        #
# ---------------------------------------------------------------------------- #

#' Summarise interview dates for the timing-of-interviews bar chart
#'
#' Groups survey microdata by `economy`, `countryyear`, and a year-month floor
#' date, counting the number of household records (`hh`) in each month. Rows
#' with missing timestamps are dropped before aggregating.
#'
#' @param df A data frame with columns `economy` (character), `countryyear`
#'   (character), and `timestamp` (Date), as produced by `add_time_columns()`.
#'
#' @return A data frame with columns `economy`, `countryyear`, `month`
#'   (Date, first of month), and `hh` (integer count). Returns a zero-row
#'   data frame when `df` is empty or all timestamps are `NA`.
#'
#' @export
summarise_interview_dates <- function(df) {
  df |>
    dplyr::filter(!is.na(timestamp)) |>
    dplyr::mutate(
      month_num = as.integer(format(timestamp, "%m"))
    ) |>
    dplyr::summarise(
      hh = dplyr::n(),
      .by = c(economy, countryyear, month_num)
    )
}


# ---------------------------------------------------------------------------- #
# Welfare poverty lines                                                         #
# ---------------------------------------------------------------------------- #

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


# ---------------------------------------------------------------------------- #
# Interview date bar chart                                                      #
# ---------------------------------------------------------------------------- #

#' Plot timing of survey interviews as a monthly bar chart, faceted by wave
#'
#' @param plot_data A data frame with columns `month` (Date, first of month),
#'   `hh` (integer count), `economy` (character), and `countryyear`
#'   (character), as returned by `summarise_interview_dates()`.
#'
#' @return A `ggplot` object, or `NULL` invisibly when `plot_data` is
#'   `NULL` or has zero rows.
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap scale_x_date labs
#'   theme_minimal theme element_text
#' @export
plot_interview_dates <- function(plot_data) {
  if (is.null(plot_data) || nrow(plot_data) == 0) return(invisible(NULL))

  month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  plot_data$month_fct <- factor(
    plot_data$month_num,
    levels = 1:12,
    labels = month_labels
  )

  ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x = .data$month_fct,
      y = .data$hh,
      fill = .data$countryyear
    )
  ) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::scale_x_discrete(drop = FALSE) +
    theme_wise() +
    ggplot2::labs(
      x = NULL, y = "Households", fill = "Survey wave"
    ) +
    ggplot2::theme(
      axis.text.x        = ggplot2::element_text(
        angle = 0, hjust = 0.5, size = 9
      ),
      axis.ticks.x       = ggplot2::element_line(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      legend.position    = "bottom"
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
  if (is.null(df) || !all(keys %in% names(df))) return(NULL)

  d <- df
  d$year <- as.character(d$year)
  if (!"economy" %in% names(d)) d$economy <- d$code

  w <- unique(d[, c(keys, "economy")])
  if (nrow(w) == 0) return(NULL)

  w$key   <- paste(w$code, w$year, w$survname, sep = "|")
  w$label <- paste0(w$economy, ", ", w$year)
  w <- w[order(w$label), , drop = FALSE]
  rownames(w) <- NULL
  w
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
  if (is.null(df) || identical(key, "all") || is.null(key)) return(df)
  if (!all(keys %in% names(df))) return(df)
  k <- paste(df$code, as.character(df$year), df$survname, sep = "|")
  df[k == key, , drop = FALSE]
}




# ---------------------------------------------------------------------------- #
# Sample density heatmap                                                        #
# ---------------------------------------------------------------------------- #

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
  keys <- c("code", "year", "survname", "loc_id")
  if (is.null(cell_map) || is.null(survey_data)) return(NULL)
  if (!all(c(keys, "h3") %in% names(cell_map))) return(NULL)
  if (!all(keys %in% names(survey_data))) return(NULL)

  cm <- cell_map
  cm$year <- as.character(cm$year)

  sd <- survey_data
  sd$year <- as.character(sd$year)

  # Sampled units per location.
  n_loc <- sd |>
    dplyr::count(.data$code, .data$year, .data$survname, .data$loc_id,
                 name = "n_units")

  cm <- cm |>
    dplyr::inner_join(n_loc, by = keys)
  if (nrow(cm) == 0) return(NULL)

  # Spread each location's units across its cells in proportion to the
  # cells' 2020 population, so a big settlement inside a multi-cell
  # location keeps most of its households. Locations without usable
  # weights (all NA / zero / negative) fall back to an even split.
  has_pop <- "pop_2020" %in% names(cm)
  cm |>
    dplyr::group_by(.data$code, .data$year, .data$survname, .data$loc_id) |>
    dplyr::mutate(
      .alloc = if (has_pop) {
        .pop <- pmax(.data$pop_2020, 0, na.rm = TRUE)
        .pop_sum <- sum(.pop)
        if (.pop_sum > 0) {
          .data$n_units * .pop / .pop_sum
        } else {
          .data$n_units / dplyr::n()
        }
      } else {
        .data$n_units / dplyr::n()
      }
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$h3) |>
    dplyr::summarise(n_units = sum(.data$.alloc, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(.data$n_units > 0) |>
    as.data.frame()
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
  if (is.null(cell_map) || is.null(loc_vals) || nrow(loc_vals) == 0) return(NULL)
  if (!all(c(keys, "h3") %in% names(cell_map))) return(NULL)
  if (!all(c(keys, "value") %in% names(loc_vals))) return(NULL)

  binned <- isTRUE(attr(loc_vals, "binned"))
  lvls   <- attr(loc_vals, "levels")

  cm <- cell_map
  cm$year <- as.character(cm$year)
  if (!"pop_2020" %in% names(cm)) cm$pop_2020 <- 1
  cm$pop_2020[!is.finite(cm$pop_2020) | cm$pop_2020 < 0] <- 0

  lv <- loc_vals
  lv$year <- as.character(lv$year)
  if (!"n_hh" %in% names(lv))     lv$n_hh <- 1
  if (!"n_months" %in% names(lv)) lv$n_months <- 1L

  carry <- intersect(c("economy"), names(lv))
  j <- cm |>
    dplyr::inner_join(
      lv[, c(keys, "value", "n_hh", "n_months", carry)], by = keys
    )
  if (nrow(j) == 0) return(NULL)

  # Units of this location that sit in this cell.
  j <- j |>
    dplyr::group_by(.data$code, .data$year, .data$survname, .data$loc_id) |>
    dplyr::mutate(
      .tot = sum(.data$pop_2020, na.rm = TRUE),
      .w   = .data$n_hh * dplyr::if_else(.data$.tot > 0,
                                         .data$pop_2020 / .data$.tot,
                                         1 / dplyr::n())
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
  if (nrow(j) == 0) return(NULL)

  g <- collapse::GRP(j, by = grp)
  n_g <- g$N.groups
  first_idx <- match(seq_len(n_g), g$group.id)
  w <- j$.w

  out <- data.frame(
    code     = j$code[first_idx],
    year     = j$year[first_idx],
    survname = j$survname[first_idx],
    loc_id   = j$h3[first_idx],
    value    = if (binned) rep(NA_character_, n_g) else rep(NA_real_, n_g),
    # The colour summarises more than one number when several locations
    # meet here, or when a contributing location spans several months.
    n_hh     = { nh <- collapse::fsum(w, g = g, na.rm = TRUE); nh[is.na(nh)] <- 0; nh },
    n_months = collapse::fmax(j$n_months, g = g, na.rm = TRUE),
    n_locs   = as.integer(collapse::fndistinct(j$loc_id, g = g, na.rm = FALSE)),
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
      ord  <- order(gv$groups$gid, -wsum, match(gv$groups$value, sort(unique(vv[ok]))))
      take <- ord[!duplicated(gv$groups$gid[ord])]
      out$value[gv$groups$gid[take]] <- gv$groups$value[take]
    }
  } else {
    vn <- suppressWarnings(as.numeric(j$value))
    ok <- is.finite(vn) & is.finite(w) & (w > 0)
    # Invalid rows are NA-ed out of both value and weight so the grouped mean
    # skips them, as the old per-cell mask did.
    vn2 <- vn; w2 <- w
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


# ---- Colour-ramp builders ------------------------------------------------ #
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
                                maxColorValue = 255)
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
  pos   <- n_units[is.finite(n_units) & n_units > 0]
  above <- pos[pos >= 1]
  has_thin <- length(pos) > length(above)
  fmt <- function(x) format(signif(x, 3), trim = TRUE, scientific = FALSE)

  if (length(above) == 0) {
    return(list(levels = "< 1", colors = pale, cuts = 1, thin = TRUE))
  }

  brks <- unique(stats::quantile(above, probs = seq(0, 1, length.out = 4),
                                 names = FALSE, type = 7))
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
  colors <- c(if (has_thin) pale,
              band[round(seq(3, length(band), length.out = n_up))])
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
  if (is.null(cells) || nrow(cells) == 0) return(NULL)
  if (!all(c("h3", "n_units") %in% names(cells))) return(NULL)

  ok <- !is.na(cells$h3) & nzchar(cells$h3) &
    is.finite(cells$n_units) & cells$n_units > 0
  cells <- cells[ok, , drop = FALSE]
  if (nrow(cells) == 0) return(NULL)

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
  if (!nzchar(x %||% "")) return(x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}


# ---------------------------------------------------------------------------- #
# Summary stats tables                                                         #
# ---------------------------------------------------------------------------- #

#' Build the summary-statistics data frame behind `make_stats_dt()`
#'
#' UI-45/UI-48: the table, its per-table CSV button and the export bundle
#' all read this one builder, so a downloaded file cannot differ from what
#' is on screen.
#'
#' @param survey_data A reactive (or data frame) of survey data.
#' @param variable_list A reactive (or data frame) of variable metadata.
#' @param flag_col Character scalar naming the grouping flag column.
#' @param vars Optional character vector of variable names to summarise;
#'   takes precedence over `flag_col`.
#'
#' @return A data frame, or NULL when there is nothing to summarise.
#' @noRd
build_stats_table <- function(survey_data, variable_list, flag_col = NULL,
                              vars = NULL) {
    df <- if (is.function(survey_data)) survey_data() else survey_data
    if (is.null(df) || !nrow(as.data.frame(df))) return(NULL)
    vl <- if (is.function(variable_list)) variable_list() else variable_list

    target <- if (!is.null(vars)) vars else vl$name[vl[[flag_col]] == 1]
    vars   <- intersect(target, names(df))
    if (length(vars) == 0) return(NULL)

    tab <- weighted_summary_long(df, vars = vars)

    # Add missingness by survey wave (countryyear) and variable
    if ("variable" %in% names(tab) && "countryyear" %in% names(tab)) {
      if (!"countryyear" %in% names(df)) {
        stop("countryyear column is required in survey_data() to compute wave-specific missingness.")
      }

      # Wave-specific missingness by countryyear and variable, in one
      # grouped pass (PERF-09)
      fill_df <- survey_missingness_long(df, vars)

      tab <- tab |>
        dplyr::left_join(fill_df, by = c("countryyear", "variable"))
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
    tab <- tab |>
      dplyr::filter(is.na(.data$N) | .data$N > 0) |>
      dplyr::select(-dplyr::any_of("unweighted_mean"))

    # Sort by variable label, then wave (countryyear) where available
    if (all(c("variable", "countryyear") %in% names(tab))) {
      tab <- tab |>
        dplyr::arrange(.data$variable, .data$countryyear)
    }

    # ---- Column renaming ----------------------------------------------------
    if ("countryyear" %in% names(tab))    names(tab)[names(tab) == "countryyear"]    <- "Country, Year"

    names(tab) <- vapply(names(tab), function(nm) {
      if (!nzchar(nm)) return(nm)
      paste0(toupper(substr(nm, 1, 1)), substr(nm, 2, nchar(nm)))
    }, character(1))

    wrap_width <- 28
    text_cols <- names(tab)[vapply(tab, function(x) is.character(x) || is.factor(x), logical(1))]
    if (length(text_cols) > 0) {
      tab[text_cols] <- lapply(tab[text_cols], function(x) {
        x_chr <- as.character(x)
        vapply(x_chr, function(s) {
          if (is.na(s)) return(NA_character_)
          # HTML-escape each wrapped line before joining with the literal
          # <br> markup below (the table is rendered with escape = FALSE,
          # so any unescaped data-derived text would render as raw HTML;
          # see SEC-05).
          lines <- strwrap(s, width = wrap_width)
          paste(htmltools::htmlEscape(lines), collapse = "<br>")
        }, character(1))
      })
    }

    tab
}

#' Build a formatted DT summary table by variable group flag
#'
#' Creates a `DT::renderDT()` expression for survey summary statistics of the
#' variables flagged in `variable_list[[flag_col]] == 1` and present in
#' `survey_data()`.
#'
#' The returned table includes:
#' \itemize{
#'   \item Weighted summary statistics from `weighted_summary_long()`
#'   \item Wave-specific missingness (`% Missing`) by `countryyear` and variable
#'   \item Readable variable labels (from `variable_list`) shown in a single
#'     `Variable` column, falling back to the raw name when no label exists
#'   \item Standardized column names (capitalized first letter)
#'   \item Basic display formatting (numeric columns to 2 decimals except `N`)
#'   \item Soft text wrapping for long character/factor fields
#' }
#'
#' @param survey_data A reactive expression returning a survey `data.frame`.
#'   The data should include `countryyear` when wave-specific missingness is
#'   required.
#' @param variable_list A reactive expression or `data.frame` containing at least
#'   columns `name`, `label`, and the grouping flag column given in `flag_col`.
#' @param flag_col Character scalar naming the grouping flag column in
#'   `variable_list` (e.g., `"outcome"`, `"ind"`, `"hh"`, `"firm"`, `"area"`).
#'   Ignored if `vars` is supplied.
#' @param vars Optional character vector of variable names to summarise. When
#'   supplied, takes precedence over `flag_col`.
#'
#' @return A `shiny.render.function` (from `DT::renderDT`) that renders the
#'   formatted summary statistics table.
#' @export
make_stats_dt <- function(survey_data, variable_list, flag_col = NULL,
                          vars = NULL) {
  DT::renderDT({
    shiny::req(survey_data())
    tab <- build_stats_table(survey_data, variable_list, flag_col, vars)
    if (is.null(tab)) {
      tag <- flag_col %||% "specified"
      return(data.frame(Note = paste("No", tag, "variables found")))
    }

    dt <- DT::datatable(
      tab,
      rownames = FALSE,
      escape = FALSE,
      extensions = "Buttons",
      options = list(
        autoWidth = TRUE,
        pageLength = 10,
        columnDefs = list(list(className = "dt-wrap", targets = "_all")),
        dom     = wise_csv_dom("lfrtip"),
        buttons = wise_csv_button(
          paste0("summary_stats_", flag_col %||% "selected")
        )
      )
    )

    # 2 decimals for numeric columns except N
    num_cols <- names(tab)[vapply(tab, is.numeric, logical(1))]
    num_cols <- setdiff(num_cols, "N")
    if (length(num_cols) > 0) {
      dt <- DT::formatRound(dt, columns = num_cols, digits = 2)
    }

    dt
  })
}
