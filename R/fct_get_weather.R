#' Load, aggregate, and construct weather variables for survey locations
#'
#' Executes as much as possible lazily in DuckDB before collecting:
#' 1. Loads weather and H3-to-location parquet files
#' 2. Aggregates weather to survey locations (population-weighted mean)
#' 3. Materialises the unperturbed rolled series (loc_weather_base) by
#'    applying the rolling window to loc_monthly. This is the stable
#'    reference used for climate_ref and bin cutoffs in all scenarios,
#'    ensuring consistency with and without climate change arguments.
#' 4. If a climate scenario is supplied: loads CMIP6 baseline (period
#'    derived from dates) and future files, aggregates each H3 -> loc_id,
#'    computes the delta at loc_id level, applies it to loc_monthly
#'    (monthly observations, before rolling), then rolls and materialises
#'    the perturbed series as loc_weather. The delta is applied before the
#'    rolling window so that month-specific deltas are correctly propagated
#'    across the window. Skipped entirely when ssp is NULL.
#' 5. Applies transformation (deviation from mean / standardised anomaly)
#'    using climate_ref always derived from loc_weather_base (unperturbed).
#'    When binning is needed under a climate scenario, the same
#'    transformation is applied to loc_base_t in the same loop pass.
#' 6. Filters to interview dates only and collects
#' 7. Computes binning cutoffs from the unperturbed (transformed) distribution
#'    (loc_base_t, filtered to survey timestamps) then applies the same
#'    breaks to the collected result — consistent across scenarios.
#'
#' @param survey_data       Data frame with columns: code, year, survname,
#'   loc_id, timestamp. Used to derive file paths.
#' @param selected_weather  Data frame with one row per weather variable and
#'   columns: name, ref_start, ref_end, temporalAgg, transformation,
#'   cont_binned, num_bins, binning_method.
#' @param dates             Date vector of unique survey timestamps (monthly).
#'   Only these rows are retained after temporal aggregation. When a climate
#'   scenario is active, the range of dates also defines the CMIP6 baseline
#'   period (min(dates) to max(dates)).
#' @param connection_params List passed to load_data().
#' @param ssp               SSP scenario string: `"ssp2_4_5"`, `"ssp3_7_0"`,
#'   or `"ssp5_8_5"`. NULL (default) skips climate perturbation entirely.
#' @param future_period     Length-2 vector of dates for the future projection
#'   period, e.g. c("2045-01-01", "2055-12-31"). Required when ssp != NULL.
#' @param perturbation_method Named character vector mapping each variable to
#'   `"additive"` or `"multiplicative"`. Required when ssp != NULL.
#' @param epsilon           Guard constant for multiplicative delta denominators.
#'   Default 0.001.
#'
#' @return A collected data frame with columns loc_id, timestamp, and one
#'   column per weather variable (numeric, or factor if binned).
get_weather <- function(
  survey_data,
  selected_weather,
  dates,
  connection_params,
  ssp                 = NULL,
  future_period       = NULL,
  perturbation_method = NULL,
  epsilon             = 0.001
) {

  # -- Validate climate change arguments -------------------------------------
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
  }

  # -- File paths -------------------------------------------------------------
  weather_fnames <- paste0(unique(survey_data$code), "_weather.parquet")

  h3_fnames <- survey_data |>
    dplyr::distinct(code, year, survname) |>
    dplyr::mutate(fname = paste0(code, "_", year, "_", survname, "_h3.parquet")) |>
    dplyr::pull(fname)

    # -- Date range ------------------------------------------------------------
    max_lag  <- as.integer(max(selected_weather$ref_end, na.rm = TRUE))
    date_min <- min(dates) - months(max_lag)
    date_max <- max(dates)

  # Extend back to cover the 1991-2020 climate reference period (plus max lag)
  # if any variable requires a deviation-from-mean or standardised anomaly
  # transformation — the reference stats must be computed over this period.
  needs_climate_ref <- any(
    !is.na(selected_weather$transformation) &
      selected_weather$transformation != "None"
  )
  if (needs_climate_ref) {
    date_min <- min(date_min, as.Date("1991-01-01") - months(max_lag))
  }

  # -- Load lazily -----------------------------------------------------------
  weather_vars <- selected_weather$name

  weather <- load_data(weather_fnames, connection_params, collect = FALSE) |>
    dplyr::select(h3, timestamp, dplyr::all_of(weather_vars)) |>
    dplyr::filter(if_all(dplyr::all_of(weather_vars), ~ !is.na(.x))) |>
    dplyr::filter(timestamp >= date_min, timestamp <= date_max)

  h3_slim <- load_data(h3_fnames, connection_params, collect = FALSE)

  if (!"pop_2020" %in% colnames(h3_slim)) {
    h3_slim <- h3_slim |> dplyr::mutate(pop_2020 = 1L)
  }

  h3_slim <- h3_slim |>
    dplyr::filter(!is.na(h3), !is.na(pop_2020), pop_2020 > 0) |>
    dplyr::select(h3, code, year, survname, loc_id, pop_2020) |>
    dplyr::distinct()

  # -- Spatial aggregation: h3 -> loc_id (population-weighted mean) ----------
  # Kept as a lazy query so it can be reused for both the unperturbed base
  # and the perturbed series without re-reading the parquet files twice.
  loc_monthly <- weather |>
    dplyr::inner_join(h3_slim, by = "h3") |>
    dplyr::group_by(code, year, survname, loc_id, timestamp) |>
    dplyr::summarise(
      dplyr::across(
        dplyr::all_of(weather_vars),
        ~ dplyr::if_else(
            sum(dplyr::if_else(!is.na(.x), pop_2020, 0), na.rm = TRUE) > 0,
            sum(dplyr::if_else(!is.na(.x), .x * pop_2020, 0), na.rm = TRUE) /
            sum(dplyr::if_else(!is.na(.x), pop_2020, 0), na.rm = TRUE),
            NA_real_
          )
      ),
      .groups = "drop"
    )

  # -- Rolling window expressions (shared by base and perturbed paths) -------
  agg_fn_map <- c(
    "Mean"   = "AVG",
    "Median" = "MEDIAN",
    "Min"    = "MIN",
    "Max"    = "MAX",
    "Sum"    = "SUM"
  )

  roll_exprs <- setNames(
    lapply(seq_len(nrow(selected_weather)), function(i) {
      v         <- selected_weather$name[i]
      ref_start <- selected_weather$ref_start[i]
      ref_end   <- selected_weather$ref_end[i]
      agg_fn    <- agg_fn_map[[selected_weather$temporalAgg[i]]]
      dbplyr::sql(sprintf(
        "%s(%s) FILTER (WHERE %s IS NOT NULL) OVER (PARTITION BY code, year, survname, loc_id ORDER BY timestamp ROWS BETWEEN %d PRECEDING AND %d PRECEDING)",
        agg_fn, v, v, ref_end, ref_start
      ))
    }),
    selected_weather$name
  )

  # -- Materialise unperturbed rolled series (loc_weather_base) --------------
  # climate_ref and bin cutoffs are always derived from this table, ensuring
  # they are identical with and without climate change arguments.
  tmp_base_name <- paste0("lw_base_", paste0(sample(letters, 8, replace = TRUE), collapse = ""))

  loc_weather_base <- loc_monthly |>
    dplyr::mutate(!!!roll_exprs) |>
    dplyr::compute(name = tmp_base_name, temporary = TRUE)

  # -- Climate change perturbation (delta method) ----------------------------
  # The delta is applied to loc_monthly (monthly loc_id observations) BEFORE
  # the rolling window. This ensures month-specific deltas are correctly
  # propagated: e.g. a 3-month rolling mean for March uses Δ_jan, Δ_feb,
  # Δ_mar rather than applying Δ_mar to the already-aggregated value.
  #
  # The CMIP6 baseline period is derived from dates (min to max), matching
  # the observational period being simulated. CMIP6 baseline and future are
  # each aggregated H3 -> loc_id before the delta is computed (smaller join).
  # The perturbed monthly series is then rolled and materialised as a second
  # temporary table. Two compute() calls when active; one in the baseline path.
  tmp_name <- NULL

  if (climate_scenario) {

    hist_fnames   <- paste0(unique(survey_data$code), "_cmip6_historical.parquet")
    future_fnames <- paste0(unique(survey_data$code), "_cmip6_", ssp, ".parquet")

    future_start <- as.Date(future_period[1])
    future_end   <- as.Date(future_period[2])

    # Aggregate CMIP6 H3 monthly means to loc_id level (population-weighted mean).
    # Aggregating before computing the delta keeps the delta join small
    # (loc_id x month rather than H3 x month).
    cmip6_to_loc <- function(fnames, ts_start, ts_end) {
      load_data(fnames, connection_params, collect = FALSE) |>
        dplyr::select(h3, timestamp, dplyr::all_of(weather_vars)) |>
        dplyr::filter(timestamp >= ts_start, timestamp <= ts_end) |>
        dplyr::mutate(month = dbplyr::sql("MONTH(timestamp)")) |>
        dplyr::group_by(h3, month) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(weather_vars), ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        ) |>
        dplyr::inner_join(h3_slim, by = "h3") |>
        dplyr::group_by(code, year, survname, loc_id, month) |>
        dplyr::summarise(
          dplyr::across(
            dplyr::all_of(weather_vars),
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

    # Baseline period derived from dates (the observational period)
    loc_hist <- cmip6_to_loc(hist_fnames,   min(dates), max(dates))
    loc_fut  <- cmip6_to_loc(future_fnames, future_start, future_end)

    # Compute delta at loc_id level lazily.
    # After the join, historical columns carry suffix _hist, future _fut.
    delta_vars  <- paste0("delta_", weather_vars)

    delta_exprs <- setNames(
      lapply(weather_vars, function(v) {
        h <- paste0(v, "_hist")
        f <- paste0(v, "_fut")
        if (perturbation_method[[v]] == "additive") {
          dbplyr::sql(paste0(f, " - ", h))
        } else {
          dbplyr::sql(paste0("(", f, " + ", epsilon, ") / (", h, " + ", epsilon, ")"))
        }
      }),
      delta_vars
    )

    loc_deltas <- dplyr::inner_join(
      loc_hist, loc_fut,
      by     = c("code", "year", "survname", "loc_id", "month"),
      suffix = c("_hist", "_fut")
    ) |>
      dplyr::mutate(!!!delta_exprs) |>
      dplyr::select(code, year, survname, loc_id, month, dplyr::all_of(delta_vars))

    # Perturbation expressions applied to monthly observations
    perturb_exprs <- setNames(
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

    # Apply delta to monthly observations, then roll and materialise.
    # The full chain (spatial agg -> perturb -> roll) is folded into one
    # DuckDB execution at compute().
    tmp_name <- paste0("lw_", paste0(sample(letters, 8, replace = TRUE), collapse = ""))

    loc_weather <- loc_monthly |>
      dplyr::mutate(month = dbplyr::sql("MONTH(timestamp)")) |>
      dplyr::left_join(loc_deltas, by = c("code", "year", "survname", "loc_id", "month")) |>
      dplyr::mutate(!!!perturb_exprs) |>
      dplyr::select(code, year, survname, loc_id, timestamp, dplyr::all_of(weather_vars)) |>
      dplyr::mutate(!!!roll_exprs) |>
      dplyr::compute(name = tmp_name, temporary = TRUE)

  } else {
    loc_weather <- loc_weather_base
  }

  # -- Transformation: deviation from mean / standardised anomaly ------------
  # climate_ref is always computed from loc_weather_base (unperturbed rolled
  # series), ensuring reference statistics are identical across scenarios.
  # The transformation is applied to loc_weather (perturbed or not).
  # When binning cutoffs need to be derived from the unperturbed distribution,
  # the same transformation is applied to loc_base_t in the same loop pass,
  # avoiding a second separate loop.
  skip_transform <- c("spi6", "spei6")
  needs_binning  <- any(!is.na(selected_weather$cont_binned) & selected_weather$cont_binned == "Binned")

  if (climate_scenario && needs_binning) loc_base_t <- loc_weather_base

  for (i in seq_len(nrow(selected_weather))) {
    v              <- selected_weather$name[i]
    transformation <- selected_weather$transformation[i]

    if (is.na(transformation) || transformation == "None" || v %in% skip_transform) next

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

    # Closure captures v, transformation, and climate_ref for this iteration
    add_transform <- function(tbl) {
      tbl |>
        dplyr::mutate(month = dbplyr::sql("MONTH(timestamp)")) |>
        dplyr::left_join(climate_ref, by = c("code", "year", "survname", "loc_id", "month")) |>
        dplyr::mutate(
          !!v := if (transformation == "Deviation from mean") {
            dbplyr::sql(paste0(v, " - ref_mean"))
          } else if (transformation == "Standardized anomaly") {
            dbplyr::sql(paste0("(", v, " - ref_mean) / ref_sd"))
          }
        ) |>
        dplyr::select(-month, -ref_mean, -ref_sd)
    }

    loc_weather <- add_transform(loc_weather)
    if (climate_scenario && needs_binning) loc_base_t <- add_transform(loc_base_t)
  }

  # -- Filter to interview dates and collect ---------------------------------
  # Filtering to dates serves double duty:
  # (a) drops the climate reference period rows (1991-2020) loaded solely
  #     for transformation statistics
  # (b) drops any rows with NA window values where the rolling window could
  #     not be filled (loc_id x timestamps before enough history existed)
  result <- loc_weather |>
    dplyr::filter(timestamp %in% dates) |>
    dplyr::collect()

  # Collect unperturbed transformed values for bin cutoff computation.
  # Filtered to actual survey timestamps so cutoffs reflect the survey-sample
  # distribution, not the full simulation range.
  survey_timestamps <- unique(survey_data$timestamp)

  if (climate_scenario && needs_binning) {
    base_for_bins <- loc_base_t |>
      dplyr::filter(timestamp %in% survey_timestamps) |>
      dplyr::collect()
  }

  # -- Cleanup temporary tables ----------------------------------------------
  # Done after all DuckDB queries (including base_for_bins collect) are complete.
  con_cleanup <- dbplyr::remote_con(loc_weather_base)
  DBI::dbRemoveTable(con_cleanup, tmp_base_name)
  if (!is.null(tmp_name)) {
    tryCatch(
      DBI::dbRemoveTable(con_cleanup, tmp_name),
      error = function(e) invisible(NULL)
    )
  }

  # -- Apply bin labels (R only) ---------------------------------------------
  for (i in seq_len(nrow(selected_weather))) {
    v              <- selected_weather$name[i]
    cont_binned    <- selected_weather$cont_binned[i]
    num_bins       <- selected_weather$num_bins[i]
    binning_method <- selected_weather$binning_method[i]

    if (is.na(cont_binned) || cont_binned != "Binned") next

    # Source for cutoff computation: unperturbed transformed values when a
    # climate scenario is active; the result itself otherwise.
    bin_source <- if (climate_scenario) base_for_bins else result
    haz_vals   <- bin_source[[v]][
      is.finite(bin_source[[v]]) & bin_source$timestamp %in% survey_timestamps
    ]

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
      breaks_ext  <- c(-Inf, cutoffs[-c(1, length(cutoffs))], Inf)
      result[[v]] <- cut(result[[v]], breaks = breaks_ext, include.lowest = TRUE)
      message(binning_method, " cutoffs for ", v, ": ", paste(round(cutoffs, 3), collapse = ", "))
    } else {
      message("Insufficient variation in ", v, ". Keeping continuous.")
    }
  }

  result
}


#___________________________________________
# # TEST

# # inputs
# survey_data <- load_data("/Users/bbrunckhorst/Github/wise-app-dev/data/GNB_2021_EHCVM_hh.parquet", collect = TRUE) |>
#     dplyr::mutate(
#       timestamp = as.Date(paste(int_year, int_month, "01", sep = "-")),
#       month = int_month
#     ) 

# dates <- survey_data |>
#     dplyr::filter(!is.na(timestamp)) |>
#     dplyr::pull(timestamp)

# connection_params <- list(type = "local", path = "/Users/bbrunckhorst/Github/wise-app-dev/data")

# # 1. Baseline: Mean, no transformation, continuous
# sw1 <- data.frame(
#   name = "tx",
#   ref_start = 1,
#   ref_end = 7,
#   temporalAgg = "Mean",
#   cont_binned = "Continuous",
#   binning_method = NA_character_,
#   num_bins = NA_integer_,
#   transformation = "None"
# )
# res1 <- get_weather(survey_data, sw1, dates, connection_params)

# # 2. Median, no transformation, binned (equal frequency)
# sw2 <- data.frame(
#   name = "tx",
#   ref_start = 1,
#   ref_end = 7,
#   temporalAgg = "Median",
#   cont_binned = "Binned",
#   binning_method = "Equal frequency",
#   num_bins = 4,
#   transformation = "None"
# )
# res2 <- get_weather(survey_data, sw2, dates, connection_params)

# # 3. Min, standardised anomaly, binned (equal width)
# sw3 <- data.frame(
#   name = "tx",
#   ref_start = 3,
#   ref_end = 6,
#   temporalAgg = "Min",
#   cont_binned = "Binned",
#   binning_method = "Equal width",
#   num_bins = 5,
#   transformation = "Standardized anomaly"
# )
# res3 <- get_weather(survey_data, sw3, dates, connection_params)

# # 4. Multiple variables, mixed settings
# sw4 <- data.frame(
#   name = c("tx", "t"),
#   ref_start = c(1, 0),
#   ref_end = c(7, 11),
#   temporalAgg = c("Mean", "Max"),
#   cont_binned = c("Continuous", "Binned"),
#   binning_method = c(NA_character_, "Equal frequency"),
#   num_bins = c(NA, 4),
#   transformation = c("None", "Deviation from mean")
# )
# res4 <- get_weather(survey_data, sw4, dates, connection_params)

# # FOR SIMULATIONS - get weather data for survey locations across many weather years
# start_year <- 1990
# end_year <- 2024

#   # get dates for every year in range with month in survey data
#   dates_sim <- with(
#   expand.grid(
#     int_month = unique(survey_data$int_month),
#     int_year  = start_year:end_year
#   ),
#   as.Date(paste(int_year, int_month, "01", sep = "-"))
# )

#   # get weather from every year in a date range for simulations
#   # using same settings as sw4

#   sim_weather <- get_weather(survey_data, sw4, dates_sim, connection_params) |>
#     dplyr::mutate(
#       # month for merging with survey data
#       int_month=as.integer(format(timestamp, "%m")),
#       # simulation weather year for reference
#       sim_year = as.integer(format(timestamp, "%Y"))) |> 
#     # drop timestamp to avoid confusion with survey interview timestamps
#     dplyr::select(-timestamp) 