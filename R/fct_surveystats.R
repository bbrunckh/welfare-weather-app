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
    ggplot2::theme_minimal(base_size = 12) +
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

# ---------------------------------------------------------------------------- #
# Leaflet survey location map                                                  #
# ---------------------------------------------------------------------------- #

#' Extract bounding box from a GeoJSON FeatureCollection
#'
#' Iterates over all Polygon and MultiPolygon coordinate arrays to find the
#' overall lng/lat extent. No spatial libraries required.
#'
#' @param geojson A GeoJSON FeatureCollection list, as produced by
#'   `jsonlite::fromJSON` or assembled manually.
#' @return A named list with `lng1`, `lat1`, `lng2`, `lat2`.
#' 
.geojson_bounds <- function(geojson) {
  all_lng <- numeric()
  all_lat <- numeric()

  # Recursively extract all coordinate pairs from any nesting depth
  extract_coords <- function(coords) {
    if (is.numeric(coords) && length(coords) == 2) {
      # A single [lng, lat] pair
      all_lng <<- c(all_lng, coords[1])
      all_lat <<- c(all_lat, coords[2])
    } else if (is.matrix(coords)) {
      # A ring already coerced to a matrix by jsonlite
      all_lng <<- c(all_lng, coords[, 1])
      all_lat <<- c(all_lat, coords[, 2])
    } else if (is.list(coords)) {
      lapply(coords, extract_coords)
    }
  }

  for (f in geojson$features) {
    extract_coords(f$geometry$coordinates)
  }

  list(
    lng1 = min(all_lng), lat1 = min(all_lat),
    lng2 = max(all_lng), lat2 = max(all_lat)
  )
}



#' Build a leaflet map of survey interview locations
#'
#' Renders a `leaflet` widget from a GeoJSON FeatureCollection of H3-aggregated
#' survey locations. Polygons are coloured by economy code and the viewport is
#' fitted to the bounding box. Accepts both Polygon and MultiPolygon geometries.
#'
#' @param loc A GeoJSON FeatureCollection list with a `code` property on each
#'   feature, as produced by the H3-to-polygon aggregation step in
#'   `mod_1_02_surveystats_server()`.
#'
#' @return A `leaflet` widget, or `NULL` invisibly when `loc` is `NULL` or
#'   has no features.
#'
#' @export
plot_survey_map <- function(loc) {
  if (is.null(loc) || length(loc$features) == 0) return(invisible(NULL))

  codes      <- sapply(loc$features, function(f) f$properties$code)
  u_codes    <- unique(codes)
  code_color <- setNames(scales::hue_pal()(length(u_codes)), u_codes)
  bounds     <- .geojson_bounds(loc)

  m <- leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)

  # One addGeoJSON call per code so each gets a static colour
  for (cd in u_codes) {
    features_for_code <- loc$features[codes == cd]
    sub_geojson <- list(type = "FeatureCollection", features = features_for_code)

    m <- m |>
      leaflet::addGeoJSON(
        geojson     = sub_geojson,
        color       = code_color[[cd]],
        opacity     = 0.5,
        fillOpacity = 0,
        weight      = 1
      )
  }

  # Hover highlight via vanilla Leaflet JS
  hover_js <- htmlwidgets::JS("
    function(el, x) {
      this.eachLayer(function(layer) {
        if (layer.eachLayer) {         // it's a GeoJSON layer group
          layer.eachLayer(function(sublayer) {
            var origColor  = sublayer.options.color;
            var origWeight = sublayer.options.weight;
            sublayer.on('mouseover', function(e) {
              e.target.setStyle({ weight: 2, color: '#FF0000' });
              e.target.bringToFront();
            });
            sublayer.on('mouseout', function(e) {
              e.target.setStyle({ weight: origWeight, color: origColor });
            });
          });
        }
      });
    }
  ")

  m |>
    leaflet::fitBounds(
      lng1 = bounds$lng1, lat1 = bounds$lat1,
      lng2 = bounds$lng2, lat2 = bounds$lat2
    ) |>
    htmlwidgets::onRender(hover_js)
}


# ---------------------------------------------------------------------------- #
# Summary stats tables                                                         #
# ---------------------------------------------------------------------------- #

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
#'   \item Variable labels joined from `variable_list` (`Variable Label`)
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
#'
#' @return A `shiny.render.function` (from `DT::renderDT`) that renders the
#'   formatted summary statistics table.
#' @export
make_stats_dt <- function(survey_data, variable_list, flag_col) {
  DT::renderDT({
    shiny::req(survey_data())
    df <- survey_data()
    vl <- if (is.function(variable_list)) variable_list() else variable_list

    vars <- intersect(vl$name[vl[[flag_col]] == 1], names(df))
    if (length(vars) == 0) {
      return(data.frame(Note = paste("No", flag_col, "variables found")))
    }

    tab <- weighted_summary_long(df, vars = vars)

    # Add missingness by survey wave (countryyear) and variable
    if ("variable" %in% names(tab) && "countryyear" %in% names(tab)) {
      if (!"countryyear" %in% names(df)) {
        stop("countryyear column is required in survey_data() to compute wave-specific missingness.")
      }

      # Build long missingness table by wave
      miss_list <- lapply(vars, function(v) {
        df |>
          dplyr::group_by(countryyear) |>
          dplyr::summarise(
            `% Missing` = 100 * mean(is.na(.data[[v]])),
            .groups = "drop"
          ) |>
          dplyr::mutate(variable = v)
      })

      fill_df <- dplyr::bind_rows(miss_list)

      tab <- tab |>
        dplyr::left_join(fill_df, by = c("countryyear", "variable"))
    }

    if ("variable" %in% names(tab)) {
      lab_map <- vl[, c("name", "label"), drop = FALSE]
      tab <- tab |>
        dplyr::left_join(lab_map, by = c("variable" = "name")) |>
        dplyr::mutate(variable_label = dplyr::coalesce(.data$label, .data$variable)) |>
        dplyr::select(variable, variable_label, dplyr::everything(), -dplyr::any_of("label"))
    }

    # Sort by variable name, then wave (countryyear) where available
    if (all(c("variable", "countryyear") %in% names(tab))) {
      tab <- tab |>
        dplyr::arrange(.data$variable, .data$countryyear)
    }

    # ---- Column renaming ----------------------------------------------------
    if ("variable_label" %in% names(tab)) names(tab)[names(tab) == "variable_label"] <- "Variable Label"
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
          paste(strwrap(s, width = wrap_width), collapse = "<br>")
        }, character(1))
      })
    }

    dt <- DT::datatable(
      tab,
      rownames = FALSE,
      escape = FALSE,
      options = list(
        autoWidth = TRUE,
        pageLength = 10,
        columnDefs = list(list(className = "dt-wrap", targets = "_all"))
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
