# fct_get_weather.R
# -----------------
# Weather loading and processing pipeline.
# Loads ERA5 historical weather and CMIP6 climate projections from parquet
# files via DuckDB. Applies spatial aggregation (H3 → survey location),
# temporal rolling windows, and climate perturbations.
#
# Called by:
#   - mod_1_04_weather.R  (weather preview in Module 1)
#   - fct_run_simulation.R / mod_2_01_weathersim.R (simulation in Module 2)
#
# Main exports:
#   get_weather()          — full weather loading pipeline
#   summarise_ensemble()   — reduce CMIP6 ensemble to representatives
#
# Internal helpers (not exported):
#   .harmonise_h3()        — H3 resolution matching
#   .apply_transformations() — anomaly/deviation computation in DuckDB
#   .compute_breaks()      — histogram/quantile breakpoints
#   .apply_binning()       — apply cut points to weather data frame

# ---------------------------------------------------------------------------- #
# Section 1 — H3 spatial helpers                                               #
# ---------------------------------------------------------------------------- #

#' Harmonise H3 resolution and type between microdata and weather tables.
#' 
#' This helper:
#' 1. Detects the H3 resolution of each table by sampling one row.
#' 2. Chooses the **coarser** (lower numeric) resolution as the join key.
#'    This handles all three cases:
#'    * weather coarser than microdata  → map microdata up to weather res
#'    * microdata coarser than weather  → map weather up to microdata res
#'    * same resolution                 → type-cast only, no parent lookup
#' 3. Adds an `h3_weather` column (bigint) to `h3_slim` so the caller can
#'    join on `h3_slim$h3_weather == weather$h3` without further casting.
#'
#' The H3 DuckDB extension must already be loaded before calling this
#' function (`.duck_load_ext("h3")`).
#'
#' @param h3_slim  Lazy `dplyr::tbl` with an `h3` column (string).
#' @param weather  Lazy `dplyr::tbl` with an `h3` column (bigint / int64).
#' @param con      DBI connection — used for the resolution-detection query.
#'
#' @return A list with:
#'   * `h3_slim`      — original `h3_slim` augmented with an `h3_weather`
#'                      bigint column at the chosen target resolution.
#'   * `weather`      — `weather` tbl, possibly with `h3` mapped to the
#'                      target resolution (when weather is *finer* than micro).
#'   * `target_res`   — integer, the target (coarser) H3 resolution.
#'   * `same_res`     — logical, TRUE when no parent lookup was needed.
#' @noRd
.harmonise_h3 <- function(h3_slim, weather, con) {

  micro_h3_sql   <- dbplyr::sql_render(
    h3_slim |> dplyr::filter(!is.na(h3)) |> dplyr::select(h3) |> head(1)
  )
  weather_h3_sql <- dbplyr::sql_render(
    weather |> dplyr::filter(!is.na(h3)) |> dplyr::select(h3) |> head(1)
  )

  res_micro <- DBI::dbGetQuery(
    con,
    sprintf("SELECT h3_get_resolution(h3) AS res FROM (%s) _t", micro_h3_sql)
  )$res[[1L]]

  res_weather <- DBI::dbGetQuery(
    con,
    sprintf("SELECT h3_get_resolution(h3) AS res FROM (%s) _t", weather_h3_sql)
  )$res[[1L]]

  target_res <- min(res_micro, res_weather)
  same_res   <- (res_micro == res_weather)

  if (same_res) {
    h3_slim <- h3_slim |>
      dplyr::mutate(h3_weather = dbplyr::sql("h3_string_to_h3(h3)"))
  } else if (res_micro > res_weather) {
    h3_slim <- h3_slim |>
      dplyr::mutate(
        h3_weather = dbplyr::sql(
          sprintf("h3_cell_to_parent(h3_string_to_h3(h3), %d)", target_res)
        )
      )
  } else {
    h3_slim <- h3_slim |>
      dplyr::mutate(h3_weather = dbplyr::sql("h3_string_to_h3(h3)"))
    weather <- weather |>
      dplyr::mutate(
        h3 = dbplyr::sql(
          sprintf("h3_cell_to_parent(h3, %d)", target_res)
        )
      )
  }

  list(
    h3_slim     = h3_slim,
    weather     = weather,
    target_res  = target_res,
    res_micro   = res_micro,
    res_weather = res_weather,
    same_res    = same_res
  )
}

# ---------------------------------------------------------------------------- #
# Section 2 — Weather transformation helpers                                   #
# .apply_transformations() — anomaly/deviation in DuckDB SQL                   #
# .compute_breaks()        — binning breakpoint computation                    #
# .apply_binning()         — apply breakpoints to data frame                   #
# ---------------------------------------------------------------------------- #

#' Apply deviation-from-mean or standardised-anomaly transformations lazily.
#'
#' Iterates over `selected_weather` rows and chains lazy join + mutate steps
#' onto `tbl` for every variable that requires a transformation.  Variables
#' in `skip_vars` (e.g. `"spi6"`, `"spei6"`) and variables with
#' `transformation == "None"` or `NA` are left untouched.
#'
#' Reference statistics (monthly mean and SD) are always derived from
#' `loc_weather_base` over the 1991–2020 climate normal period.
#'
#' @param tbl              Lazy `dplyr::tbl` containing rolled weather columns.
#' @param selected_weather Data frame with columns `name` and `transformation`.
#' @param loc_weather_base Materialised temp-table tbl (the unperturbed rolled
#'   series) used as the climate reference source.
#' @param skip_vars        Character vector of variable names to skip.
#'   Defaults to `c("spi6", "spei6")`.
#'
#' @return `tbl` with transformation steps chained lazily.
#' @noRd
.apply_transformations <- function(
  tbl,
  selected_weather,
  loc_weather_base,
  skip_vars = c("spi6", "spei6")
) {

  for (i in seq_len(nrow(selected_weather))) {
    v              <- selected_weather$name[i]
    transformation <- selected_weather$transformation[i]

    if (is.na(transformation) || transformation == "None" || v %in% skip_vars) next

    climate_ref <- loc_weather_base |>
      dplyr::filter(
        timestamp >= as.Date("1991-01-01"),
        timestamp <= as.Date("2020-12-31")
      ) |>
      dplyr::mutate(month = dbplyr::sql("MONTH(timestamp)")) |>
      dplyr::group_by(code, year, survname, loc_id, month) |>
      dplyr::summarise(
        ref_mean = dbplyr::sql(paste0("AVG(", v, ")")),
        ref_sd   = dbplyr::sql(paste0("STDDEV_SAMP(", v, ")")),
        .groups  = "drop"
      )

    tbl <- local({
      .v   <- v
      .tf  <- transformation
      .ref <- climate_ref
      function(t) {
        t |>
          dplyr::mutate(month = dbplyr::sql("MONTH(timestamp)")) |>
          dplyr::left_join(.ref, by = c("code", "year", "survname", "loc_id", "month")) |>
          dplyr::mutate(
            !!.v := if (.tf == "Deviation from mean") {
              dbplyr::sql(paste0(.v, " - ref_mean"))
            } else if (.tf == "Standardized anomaly") {
              dbplyr::sql(paste0("(", .v, " - ref_mean) / ref_sd"))
            }
          ) |>
          dplyr::select(-month, -ref_mean, -ref_sd)
      }
    })(tbl)
  }

  tbl
}

# ---------------------------------------------------------------------------- #

#' Compute bin breakpoints from a reference data frame.
#'
#' Examines each row of `selected_weather` whose `cont_binned` column is
#' `"Binned"` and derives the requested number of cut-points from `ref_df`
#' (typically the historical result filtered to actual survey timestamps).
#'
#' The bottom and top bins are always open-ended (`-Inf` / `Inf`) so that
#' values outside the reference range (e.g. from climate projections) still
#' map into the extreme bins.
#'
#' @param ref_df           Collected data frame containing the weather columns.
#' @param selected_weather Data frame with columns `name`, `cont_binned`,
#'   `num_bins`, `binning_method`.
#'
#' @return A named list of break vectors (one per binned variable).
#'   Variables that are not binned or fail to produce valid breaks are omitted.
#' @noRd
.compute_breaks <- function(ref_df, selected_weather) {
  stored_breaks <- list()

  for (i in seq_len(nrow(selected_weather))) {
    v              <- selected_weather$name[i]
    cont_binned    <- selected_weather$cont_binned[i]
    num_bins       <- selected_weather$num_bins[i]
    binning_method <- selected_weather$binning_method[i]

    if (is.na(cont_binned) || cont_binned != "Binned") next

    haz_vals <- ref_df[[v]][is.finite(ref_df[[v]])]
    
    # sort for consistent quantile breaks and k-means results
    haz_vals <- sort(haz_vals)

    cutoffs <- switch(binning_method,
      "Equal frequency" = {
        unique(quantile(haz_vals, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE))
      },
      "Equal width" = {
        unique(seq(min(haz_vals, na.rm = TRUE), max(haz_vals, na.rm = TRUE), length.out = num_bins + 1))
      },
      "K-means" = {
        tryCatch({
          if (length(unique(haz_vals)) >= num_bins) {
            set.seed(123)
            km      <- kmeans(haz_vals, centers = num_bins)
            centers <- sort(as.numeric(km$centers))
            unique(c(
              min(haz_vals, na.rm = TRUE),
              (centers[-length(centers)] + centers[-1]) / 2,
              max(haz_vals, na.rm = TRUE)
            ))
          } else {
            message("Not enough unique values for K-means in ", v, ". Keeping continuous.")
            NULL
          }
        }, error = function(e) {
          message("K-means failed for ", v, ": ", e$message)
          NULL
        })
      },
      NULL
    )

    if (!is.null(cutoffs) && length(cutoffs) > 1) {
      breaks_ext         <- c(-Inf, cutoffs[-c(1, length(cutoffs))], Inf)
      stored_breaks[[v]] <- breaks_ext
      message(binning_method, " cutoffs for ", v, ": ", paste(round(cutoffs, 3), collapse = ", "))
    } else {
      message("Insufficient variation in ", v, ". Keeping continuous.")
    }
  }

  stored_breaks
}


#' Apply pre-computed bin breaks to weather columns in a data frame.
#'
#' @param df     Collected data frame.
#' @param breaks Named list of break vectors as returned by `.compute_breaks()`.
#'
#' @return `df` with binned columns converted to factors via `cut()`.
#' @noRd
.apply_binning <- function(df, breaks) {
  for (v in names(breaks)) {
    if (v %in% names(df)) {
      df[[v]] <- cut(df[[v]], breaks = breaks[[v]], include.lowest = TRUE)
    }
  }
  df
}

# ---------------------------------------------------------------------------- #
# Section 3 — Main weather loading pipeline                                    #
# get_weather() — loads ERA5 + CMIP6, applies rolling windows + perturbations  #
# Note: DuckDB rolling window (0%/16% stall) occurs in loc_weather_base        #
# materialisation and batch query. See tradeoffs for improvements              #
# in known_issues.md #13, #15.                                                 #
# ---------------------------------------------------------------------------- #

#' Load, aggregate, and construct weather variables for survey locations.
#'
#' 1. Loads weather and H3-to-location parquet files lazily in DuckDB.
#' 2. Spatially aggregates weather to `loc_id` (population-weighted mean).
#' 3. Applies rolling temporal aggregation and materialises the unperturbed
#'    weather series as a temp table (`loc_weather_base`).
#' 4. If `ssp` is supplied: loads CMIP6 files for each scenario.
#'    For each SSP, computes population-weighted loc-level raw deltas,
#'    then processes all models in a single batched DuckDB query —
#'    perturb raw monthly values → roll → transform → collect.
#'    All models with a complete set of weather variable deltas are returned.
#' 5. Transformations (deviation-from-mean, standardised anomaly) are applied
#'    using the 1991–2020 climate reference, always derived from
#'    `loc_weather_base`.
#' 6. Returns a flat named list of collected data frames.
#'
#' @param survey_data       Data frame with columns: `code`, `year`, `survname`,
#'   `loc_id`, `timestamp`. Loaded microdata observations.
#' @param selected_surveys  Data frame from the survey list with columns: `code`,
#'   `year`, `survname`, `source`. Used to derive H3 file paths.
#' @param selected_weather  Data frame with one row per weather variable and
#'   columns: `name`, `ref_start`, `ref_end`, `temporalAgg`, `transformation`.
#' @param dates             Date vector of unique survey timestamps (monthly).
#'   Only these rows are retained after temporal aggregation.
#' @param connection_params List passed to `load_data()`.
#' @param ssp               Character vector of SSP scenario identifiers, e.g.
#'   `c("ssp2_4_5", "ssp5_8_5")`. `NULL` (default) skips climate perturbation.
#' @param future_period     A length-2 character vector of dates for a single
#'   projection period, e.g. `c("2045-01-01", "2055-12-31")`, **or** a list
#'   of such vectors for multiple periods.  Required when `ssp != NULL`.
#' @param perturbation_method Named character vector mapping each weather
#'   variable name to `"additive"` or `"multiplicative"`. Required when
#'   `ssp != NULL`.
#' @param epsilon           Guard constant for multiplicative delta denominators.
#'   Default `0.001`.
#' @param weather_source    Source identifier for observed weather files.
#'   Default `"era5land"`.
#' @param proj_source       Source identifier for climate projection files.
#'   Default `"cmip6"`.
#' @return A named list of collected data frames with columns
#'   `code, year, survname, loc_id, timestamp, <weather_vars>`:
#'   * `"historical"` — unperturbed result filtered to `dates`.
#'   * `"<ssp>_<start>_<end>_<model>"` — e.g.
#'     `"ssp2_4_5_2045_2055_MPI.ESM1.2.HR"` (model name sanitised via
#'     `make.names()`).  All models with a complete set of weather variable
#'     deltas are returned.
#'
#' @export
get_weather <- function(
  survey_data,
  selected_surveys,
  selected_weather,
  dates,
  connection_params,
  ssp                  = NULL,
  future_period        = NULL,
  perturbation_method  = NULL,
  epsilon              = 0.001,
  weather_source       = "era5land",
  proj_source          = "cmip6",
  stored_breaks        = NULL
) {

  # -- Validate ---------------------------------------------------------------
  climate_scenario <- !is.null(ssp)

  if (climate_scenario) {
    stopifnot(
      "future_period is required when ssp is supplied" =
        !is.null(future_period),
      "perturbation_method is required when ssp is supplied" =
        !is.null(perturbation_method),
      "All selected_weather variables must have an entry in perturbation_method" =
        all(selected_weather$name %in% names(perturbation_method)),
      "perturbation_method values must be 'additive' or 'multiplicative'" =
        all(perturbation_method[selected_weather$name] %in% c("additive", "multiplicative"))
    )

    # Normalise future_period: a bare length-2 vector → single-element list
    if (is.character(future_period) || inherits(future_period, "Date")) {
      future_period <- list(future_period)
    }
  }

  # -- File paths -------------------------------------------------------------
  survey_codes <- unique(survey_data$code)

  weather_fnames <- paste0(
    "hazard/weather/historical/", survey_codes, "/",
    survey_codes, "_", weather_source, ".parquet"
  )

  h3_fnames <- selected_surveys |>
    dplyr::distinct(code, year, survname, source) |>
    dplyr::mutate(fname = paste0(
      "microdata/h3/", code, "/",
      code, "_", year, "_", survname, "_", source, "_h3.parquet"
    )) |>
    dplyr::pull(fname)

  # -- Date range ------------------------------------------------------------
  weather_vars <- selected_weather$name
  max_lag      <- as.integer(max(selected_weather$ref_end, na.rm = TRUE))
  date_min     <- seq.Date(min(dates), by = paste0("-", max_lag, " months"), length.out = 2L)[[2L]]
  date_max     <- max(dates)

  needs_climate_ref <- any(
    !is.na(selected_weather$transformation) &
      selected_weather$transformation != "None"
  )
  if (needs_climate_ref) {
    date_min <- min(date_min, seq.Date(as.Date("1991-01-01"), by = paste0("-", max_lag, " months"), length.out = 2L)[[2L]])
    date_max <- max(date_max, as.Date("2020-12-31"))
  }

  # -- Load weather lazily -------------------------------------------------------
  .duck_load_ext("h3")
  con <- .duck_con()

  weather <- load_data(weather_fnames, connection_params, collect = FALSE) |>
    dplyr::select(h3, timestamp, dplyr::all_of(weather_vars)) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(weather_vars), ~ !is.na(.x))) |>
    dplyr::filter(timestamp >= date_min, timestamp <= date_max)

  h3_slim <- load_data(h3_fnames, connection_params, collect = FALSE)

  if (!"pop_2020" %in% colnames(h3_slim)) {
    h3_slim <- h3_slim |> dplyr::mutate(pop_2020 = 1L)
  }

  h3_slim <- h3_slim |>
    dplyr::filter(!is.na(h3), !is.na(pop_2020), pop_2020 > 0) |>
    dplyr::select(h3, code, year, survname, loc_id, pop_2020) |>
    dplyr::distinct()

  # -- H3 resolution + type harmonisation ------------------------------------
  h3_harmonised <- .harmonise_h3(h3_slim, weather, con)
  h3_slim       <- h3_harmonised$h3_slim
  weather       <- h3_harmonised$weather

  # -- Spatial aggregation: h3 -> loc_id (population-weighted mean) ----------
  .pop_weighted_mean <- function(tbl, vars) {
    tbl |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(vars),
          ~ dplyr::if_else(
              sum(dplyr::if_else(!is.na(.x), pop_2020, 0), na.rm = TRUE) > 0,
              sum(dplyr::if_else(!is.na(.x), .x * pop_2020, 0), na.rm = TRUE) /
              sum(dplyr::if_else(!is.na(.x), pop_2020, 0), na.rm = TRUE),
              NA_real_
            )
        ),
        .groups = "drop"
      )
  }

  loc_monthly <- weather |>
    dplyr::inner_join(h3_slim, by = c("h3" = "h3_weather")) |>
    dplyr::group_by(code, year, survname, loc_id, timestamp) |>
    .pop_weighted_mean(weather_vars)

  # -- Rolling window expressions --------------------------------------------
  agg_fn_map <- c(
    "Mean"   = "AVG",
    "Median" = "MEDIAN",
    "Min"    = "MIN",
    "Max"    = "MAX",
    "Sum"    = "SUM"
  )

  roll_exprs <- stats::setNames(
    lapply(seq_len(nrow(selected_weather)), function(i) {
      v      <- selected_weather$name[i]
      agg_fn <- agg_fn_map[[selected_weather$temporalAgg[i]]]
      dbplyr::sql(sprintf(
        "%s(%s) FILTER (WHERE %s IS NOT NULL) OVER (PARTITION BY code, year, survname, loc_id ORDER BY timestamp ROWS BETWEEN %d PRECEDING AND %d PRECEDING)",
        agg_fn, v, v,
        as.integer(selected_weather$ref_end[i]),
        as.integer(selected_weather$ref_start[i])
      ))
    }),
    weather_vars
  )

  # -- Materialise unperturbed rolled base (loc_weather_base) ----------------
  # All transformations and the historical result are derived from this table.
  tmp_base_name <- paste0("lw_base_", paste0(sample(letters, 8L, replace = TRUE), collapse = ""))

  loc_weather_base <- loc_monthly |>
    dplyr::mutate(!!!roll_exprs) |>
    dplyr::compute(name = tmp_base_name, temporary = TRUE)

  # -- Assemble result -------------------------------------------------------
  result <- list()

  # Historical: transform base → filter to dates → collect
  result[["historical"]] <- loc_weather_base |>
    .apply_transformations(selected_weather, loc_weather_base) |>
    dplyr::filter(timestamp %in% !!dates) |>
    dplyr::collect()

  # -- Binning setup ----------------------------------------------------------
  # Determine whether any variables require binning.  Guard against

  # selected_weather missing the binning columns entirely (backward compat).
  has_binning <- "cont_binned" %in% names(selected_weather) &&
    any(!is.na(selected_weather$cont_binned) & selected_weather$cont_binned == "Binned")

  if (has_binning) {
    if (is.null(stored_breaks) || length(stored_breaks) == 0) {
      # First call (regression): compute breaks from survey-period historical data
      survey_timestamps <- unique(survey_data$timestamp[!is.na(survey_data$timestamp)])
      hist_ref <- result[["historical"]][result[["historical"]]$timestamp %in% survey_timestamps, ]

      stored_breaks <- .compute_breaks(hist_ref, selected_weather)
    }

    # Apply to historical slice immediately
    result[["historical"]] <- .apply_binning(result[["historical"]], stored_breaks)
  }

  # -- Climate perturbation ---------------------------------------------------
  if (climate_scenario) {

    bp           <- range(dates)
    baseline_start <- as.Date(bp[1])
    baseline_end   <- as.Date(bp[2])
    delta_vars   <- paste0("delta_", weather_vars)

    # -- CMIP6 helpers --------------------------------------------------------

    # Build the delta expressions once (shared across SSPs)
    .make_delta_exprs <- function(perturbation_method, weather_vars, epsilon) {
      stats::setNames(
        lapply(weather_vars, function(v) {
          h <- paste0(v, "_hist")
          f <- paste0(v, "_fut")
          if (perturbation_method[[v]] == "additive") {
            dbplyr::sql(paste0(f, " - ", h))
          } else {
            dbplyr::sql(paste0("(", f, " + ", epsilon, ") / (", h, " + ", epsilon, ")"))
          }
        }),
        paste0("delta_", weather_vars)
      )
    }

    .make_perturb_exprs <- function(perturbation_method, weather_vars) {
      stats::setNames(
        lapply(weather_vars, function(v) {
          delta_col <- paste0("delta_", v)
          if (identical(perturbation_method[[v]], "multiplicative")) {
            dbplyr::sql(paste0(v, " * ", delta_col))
          } else {
            dbplyr::sql(paste0(v, " + ", delta_col))
          }
        }),
        weather_vars
      )
    }

    delta_exprs_h3 <- .make_delta_exprs(perturbation_method, weather_vars, epsilon)
    perturb_exprs  <- .make_perturb_exprs(perturbation_method, weather_vars)

    # Rolling window expressions with `model` added to PARTITION BY.
    # Used in the batch climate query so each model gets its own independent
    # rolling window over its own perturbed series.
    roll_exprs_climate <- stats::setNames(
      lapply(seq_len(nrow(selected_weather)), function(i) {
        v      <- selected_weather$name[i]
        agg_fn <- agg_fn_map[[selected_weather$temporalAgg[i]]]
        dbplyr::sql(sprintf(
          "%s(%s) FILTER (WHERE %s IS NOT NULL) OVER (PARTITION BY model, code, year, survname, loc_id ORDER BY timestamp ROWS BETWEEN %d PRECEDING AND %d PRECEDING)",
          agg_fn, v, v,
          as.integer(selected_weather$ref_end[i]),
          as.integer(selected_weather$ref_start[i])
        ))
      }),
      weather_vars
    )

    # Detect CMIP6 H3 resolution once using the first SSP's historical file
    hist_fnames_probe <- paste0(
      "hazard/weather/projections/", survey_codes, "/",
      survey_codes, "_", proj_source, "_historical.parquet"
    )

    cmip6_res <- tryCatch({
      probe_sql <- dbplyr::sql_render(
        load_data(hist_fnames_probe, connection_params, collect = FALSE) |>
          dplyr::filter(!is.na(h3)) |>
          dplyr::select(h3) |>
          head(1)
      )
      DBI::dbGetQuery(
        con,
        sprintf("SELECT h3_get_resolution(h3) AS res FROM (%s) _t", probe_sql)
      )$res[[1L]]
    }, error = function(e) h3_harmonised$target_res)

    # Determine the join resolution between CMIP6 and microdata.
    # Both must be brought to the coarser (lower) of the two.
    cmip6_join_res <- min(cmip6_res, h3_harmonised$target_res)

    # Add h3_cmip6 column to h3_slim for CMIP6 spatial joins.
    # When CMIP6 is coarser than the observed-weather target resolution,
    # micro H3 cells must be mapped further up to match CMIP6.
    if (cmip6_join_res == h3_harmonised$target_res) {
      # CMIP6 same or finer than target — reuse existing h3_weather column
      h3_slim <- h3_slim |>
        dplyr::mutate(h3_cmip6 = h3_weather)
    } else {
      # CMIP6 coarser than target — map micro cells up to CMIP6 resolution
      h3_slim <- h3_slim |>
        dplyr::mutate(
          h3_cmip6 = dbplyr::sql(
            sprintf("h3_cell_to_parent(h3_string_to_h3(h3), %d)", cmip6_join_res)
          )
        )
    }

    # Helper: load CMIP6 parquets → lazy (model, h3, month, vars)
    .cmip6_h3_monthly <- function(fnames, ts_start, ts_end) {
      tbl <- load_data(fnames, connection_params, collect = FALSE) |>
        dplyr::select(model, h3, timestamp, dplyr::all_of(weather_vars)) |>
        dplyr::filter(timestamp >= ts_start, timestamp <= ts_end) |>
        dplyr::mutate(month = dbplyr::sql("MONTH(timestamp)")) |>
        dplyr::group_by(model, h3, month) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(weather_vars), ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        )

      # Map CMIP6 h3 to the join resolution when CMIP6 is finer
      if (cmip6_res > cmip6_join_res) {
        tbl <- tbl |>
          dplyr::mutate(
            h3 = dbplyr::sql(
              sprintf("h3_cell_to_parent(h3, %d)", cmip6_join_res)
            )
          )
      }
      tbl
    }

    # CMIP6 historical baseline — shared across all SSPs (same files)
    h3_hist_raw <- .cmip6_h3_monthly(hist_fnames_probe, baseline_start, baseline_end)

    # -- Per-SSP worker -------------------------------------------------------
    # Processes all models × all future periods.  The CMIP6 historical
    # baseline and SSP baseline-period data are loaded once and shared
    # across periods; only the future-period projection varies.
    # Returns a named list keyed by "<ssp>_<start>_<end>_<model>".
    .process_ssp <- function(ssp_i) {
      
      t_ssp_start <- proc.time()[["elapsed"]]
      message(sprintf("[wiseapp] Processing SSP: %s at %s",
                ssp_i, format(Sys.time(), "%H:%M:%S")))

      ssp_fname     <- gsub("_", "", ssp_i)
      future_fnames <- paste0(
        "hazard/weather/projections/", survey_codes, "/",
        survey_codes, "_", proj_source, "_", ssp_fname, ".parquet"
      )

      # SSP baseline overlap — shared across all future periods
      h3_ssp_raw <- .cmip6_h3_monthly(future_fnames, baseline_start, baseline_end)

      # Combined CMIP6 historical baseline (hist + ssp overlap period)
      h3_hist <- dplyr::union_all(h3_hist_raw, h3_ssp_raw) |>
        dplyr::group_by(model, h3, month) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(weather_vars), ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        )

      # -- Loop over future periods ------------------------------------------
      out <- list()
      tmp_delta_tables <- character(0L) #DRK addition
      for (fp in future_period) {
        t_fp_start <- proc.time()[["elapsed"]]
        fp_start <- as.Date(fp[1])
        fp_end   <- as.Date(fp[2])
        fp_label <- paste0(
          format(fp_start, "%Y"), "_", format(fp_end, "%Y")
        )
        message(sprintf("[wiseapp]   Period %s: loading...", fp_label))

        h3_fut <- .cmip6_h3_monthly(future_fnames, fp_start, fp_end)

        # Lazy per-model H3-level delta table
        h3_deltas <- dplyr::inner_join(
          h3_hist, h3_fut,
          by     = c("model", "h3", "month"),
          suffix = c("_hist", "_fut")
        ) |>
          dplyr::mutate(!!!delta_exprs_h3) |>
          dplyr::select(model, h3, month, dplyr::all_of(delta_vars))

        # Population-weighted loc-level deltas
        loc_deltas_by_model <- h3_deltas |>
          dplyr::inner_join(h3_slim, by = c("h3" = "h3_cmip6")) |>
          dplyr::group_by(model, code, year, survname, loc_id, month) |>
          .pop_weighted_mean(delta_vars)
        
        
        # Materialise delta table — lets DuckDB plan a hash join in the
        # batch query instead of replanning the full lazy delta chain.
        tmp_delta_name <- paste0("lw_delta_",
                                  paste0(sample(letters, 8L, replace = TRUE),
                                         collapse = ""))
        loc_deltas_by_model <- dplyr::compute(
          loc_deltas_by_model,
          name      = tmp_delta_name,
          temporary = TRUE
        )
        tmp_delta_tables <- c(tmp_delta_tables, tmp_delta_name)


        # Filter incomplete models
        complete_model_tbl <- loc_deltas_by_model |>
          dplyr::group_by(model) |>
          dplyr::summarise(
            n_complete = sum(
              dplyr::if_all(dplyr::all_of(delta_vars), ~ !is.na(.x)),
              na.rm = TRUE
            ),
            .groups = "drop"
          ) |>
          dplyr::collect()

        incomplete_models <- complete_model_tbl$model[complete_model_tbl$n_complete == 0L]
        if (length(incomplete_models) > 0L) {
          warning(sprintf(
            "%s / %s: %d model(s) excluded due to missing variables (%s): %s",
            ssp_i, fp_label, length(incomplete_models),
            paste(delta_vars, collapse = ", "),
            paste(incomplete_models, collapse = ", ")
          ), call. = FALSE)
        }

        complete_models <- complete_model_tbl$model[complete_model_tbl$n_complete > 0L]
        message(sprintf("[wiseapp]   Period %s: %d complete models", fp_label, length(complete_models)))
        if (length(complete_models) == 0L) next

        loc_deltas_by_model <- loc_deltas_by_model |>
          dplyr::filter(model %in% complete_models)

        # -- Batch query: split into two steps to help DuckDB plan ----------
        # Step 1: join + perturb + select → materialise before rolling window
        tmp_perturb_name <- paste0("lw_perturb_",
                                    paste0(sample(letters, 8L, replace = TRUE),
                                           collapse = ""))
        tmp_delta_tables <- c(tmp_delta_tables, tmp_perturb_name)

        perturbed <- loc_monthly |>
          dplyr::mutate(month = dbplyr::sql("MONTH(timestamp)")) |>
          dplyr::inner_join(
            loc_deltas_by_model,
            by = c("code", "year", "survname", "loc_id", "month")
          ) |>
          dplyr::mutate(!!!perturb_exprs) |>
          dplyr::select(
            model, code, year, survname, loc_id, timestamp,
            dplyr::all_of(weather_vars)
          ) |>
          dplyr::compute(name = tmp_perturb_name, temporary = TRUE)

        # Step 2: rolling window + transformations + filter → collect
        batch <- perturbed |>
          dplyr::mutate(!!!roll_exprs_climate) |>
          .apply_transformations(selected_weather, loc_weather_base) |>
          dplyr::filter(timestamp %in% !!dates) |>
          dplyr::collect()

        message(sprintf("[wiseapp]   Period %s: collected %d rows in %s",   # ← ADD HERE
                fp_label,
                nrow(batch),
                format_elapsed(proc.time()[["elapsed"]] - t_fp_start)))

        if (nrow(batch) == 0L) next

        # Split into per-model data frames
        model_list <- split(batch, batch$model)
        period_out <- stats::setNames(
          lapply(model_list, function(df) {
            df$model <- NULL
            if (has_binning) df <- .apply_binning(df, stored_breaks)
            df
          }),
          paste0(ssp_i, "_", fp_label, "_", make.names(names(model_list)))
        )
        out <- c(out, period_out)
      }

      # Cleanup all materialised delta temp tables
      for (tdn in tmp_delta_tables) {
        try(DBI::dbRemoveTable(dbplyr::remote_con(loc_deltas_by_model), tdn), silent = TRUE)
      }
      message(sprintf("[wiseapp] SSP %s complete in %s — %d keys",
                  ssp_i,
                  format_elapsed(proc.time()[["elapsed"]] - t_ssp_start),
                  length(out)))  
      
      out
      }

    # -- Run SSPs sequentially — DuckDB handles per-query parallelism --------
    result <- c(result, do.call(c, lapply(ssp, .process_ssp)))
  }

  # -- Cleanup base temp table -----------------------------------------------
  con_cleanup <- dbplyr::remote_con(loc_weather_base)
  DBI::dbRemoveTable(con_cleanup, tmp_base_name)

  # Attach computed breaks so the caller can reuse them in subsequent calls
  if (has_binning && !is.null(stored_breaks)) {
    attr(result, "stored_breaks") <- stored_breaks
  }

  result
}

# ---------------------------------------------------------------------------- #
# Section 4 — Ensemble summarisation                                           #
# summarise_ensemble() — reduce CMIP6 ensemble to lo/mean/hi representatives   #
# ---------------------------------------------------------------------------- #

#' Summarise CMIP6 Ensemble to Representative Weather Series
#'
#' Replaces the full set of per-model weather keys with three representative
#' series per SSP \eqn{\times} period group: the ensemble mean and the
#' \code{lo_q}/\code{hi_q} quantile of monthly weather values across models.
#' Reduces future weather keys from ~20--30 per group to 3, giving a ~6--13x
#' speedup in the simulation loop with no loss of coefficient uncertainty.
#'
#' @param weather_result Named list returned by \code{get_weather()}. Must
#'   contain an entry named \code{"historical"} plus one entry per CMIP6 model
#'   key (\code{ssp\{x\}_\{start\}_\{end\}_\{ModelName\}}).
#' @param lo_q Numeric in (0, 0.5). Lower quantile for ensemble spread band.
#'   Default \code{0.10} (10th percentile).
#' @param hi_q Numeric in (0.5, 1). Upper quantile for ensemble spread band.
#'   Default \code{0.90} (90th percentile).
#' @param weather_vars Optional character vector of weather variable column
#'   names to summarise across models. If \code{NULL} (default), detected
#'   automatically as numeric non-identifier columns.
#'
#' @return Named list with the same structure as \code{weather_result}:
#'   \code{"historical"} entry unchanged; each SSP \eqn{\times} period group
#'   replaced by three entries (\code{_ensemble_mean}, \code{_ensemble_lo},
#'   \code{_ensemble_hi}). Any \code{stored_breaks} attribute is preserved.
#'
#' @section Methodology note:
#' Ensemble uncertainty bounds are constructed from the \code{lo_q}/\code{hi_q}
#' quantile of monthly weather variable values across CMIP6 models. This
#' synthetic series does not preserve cross-month physical coherence and should
#' be interpreted as a spread indicator rather than a specific climate
#' trajectory. Full per-model simulation is required for physically coherent
#' ensemble bounds.
#'
#' @importFrom dplyr bind_rows group_by summarise across all_of mutate
#' @export
summarise_ensemble <- function(weather_result,
                               lo_q         = 0.10,
                               hi_q         = 0.90,
                               weather_vars = NULL) {

  # ---- Input validation ----------------------------------------------------- #
  stopifnot(
    "weather_result must be a named list"      = is.list(weather_result),
    "weather_result must contain 'historical'" = "historical" %in% names(weather_result),
    "lo_q must be in (0, 0.5)"                 = is.numeric(lo_q) && lo_q > 0 && lo_q < 0.5,
    "hi_q must be in (0.5, 1)"                 = is.numeric(hi_q) && hi_q > 0.5 && hi_q < 1
  )

  future_keys <- setdiff(names(weather_result), "historical")
  if (length(future_keys) == 0L) return(weather_result)

  # ---- Key prefix parsing --------------------------------------------------- #
  # Key format: ssp{x}_{lo}_{hi}_{start}_{end}_{ModelName}
  # Regex extracts the SSP/period prefix: first 5 underscore-separated
  # segments. {4} matches exactly 4 separators = 5 segments.
  # Update this regex if the key naming convention changes.
  .key_prefix <- function(key) {
    m <- regmatches(key, regexpr("^(?:[^_]+_){4}[^_]+", key, perl = TRUE))
    if (length(m) == 0L || nchar(m) == 0L) key else m
  }

  prefixes        <- vapply(future_keys, .key_prefix, character(1L))
  unique_prefixes <- unique(prefixes)

  # ---- Identifier columns --------------------------------------------------- #
  # These columns identify the observation and are NEVER averaged across models.
  # IMPORTANT: year is numeric (integer from DuckDB) but is a survey identifier
  # — it must not be averaged. Explicit listing here prevents the is.numeric()
  # filter below from including it in w_vars.
  meta_cols <- c("code", "year", "survname", "loc_id", "timestamp")

  # Detect weather variable columns from the first future data frame.
  # If weather_vars supplied explicitly, use those — more robust.
  ref_df <- weather_result[[future_keys[[1L]]]]

  if (is.null(weather_vars)) {
    weather_vars <- names(ref_df)[
      vapply(ref_df, is.numeric, logical(1L)) &
      !names(ref_df) %in% meta_cols
    ]
  }

  if (length(weather_vars) == 0L) {
    warning("[summarise_ensemble] No weather variable columns detected — returning unchanged.")
    return(weather_result)
  }

  # ---- Build representative series per SSP/period group -------------------- #
  rep_entries <- lapply(unique_prefixes, function(pfx) {
    member_keys <- future_keys[prefixes == pfx]
    if (length(member_keys) == 0L) return(list())

    # Stack all model data frames for this SSP/period group
    stacked <- dplyr::bind_rows(
      lapply(seq_along(member_keys), function(i) {
        df              <- weather_result[[member_keys[[i]]]]
        df[[".member"]] <- i
        df
      })
    )

    # Grouping columns: identifier columns present in the stacked data
    group_cols <- intersect(meta_cols, names(stacked))

    # Summarise weather variables across models per (loc_id × timestamp × ...)
    # Using dplyr::summarise to preserve column types exactly — avoids
    # stats::aggregate() which coerces factor/integer types unpredictably.
    mean_df <- stacked |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        dplyr::across(dplyr::all_of(weather_vars), \(x) mean(x, na.rm = TRUE)),
        .groups = "drop"
      )

    lo_df <- stacked |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(weather_vars),
          \(x) stats::quantile(x, lo_q, na.rm = TRUE)
        ),
        .groups = "drop"
      )

    hi_df <- stacked |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(weather_vars),
          \(x) stats::quantile(x, hi_q, na.rm = TRUE)
        ),
        .groups = "drop"
      )

    stats::setNames(
      list(mean_df, lo_df, hi_df),
      paste0(pfx, c("_ensemble_mean", "_ensemble_lo", "_ensemble_hi"))
    )
  })

  # ---- Assemble output ----------------------------------------------------- #
  # Preserve historical entry unchanged; replace future keys with the three
  # representative series per SSP/period group.
  out <- list(historical = weather_result[["historical"]])
  for (entry in rep_entries) out <- c(out, entry)

  # Preserve stored_breaks attribute from get_weather() if present
  if (!is.null(attr(weather_result, "stored_breaks"))) {
    attr(out, "stored_breaks") <- attr(weather_result, "stored_breaks")
  }

  out
}
