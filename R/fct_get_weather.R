#' Load, aggregate, and construct weather variables for survey locations
#'
#' Executes as much as possible lazily in DuckDB before a single collect:
#' 1. Loads weather and H3-to-location parquet files
#' 2. Aggregates weather to survey locations (population-weighted mean)
#' 3. Applies rolling window temporal aggregation over the reference period
#' 4. Applies transformation (deviation from mean / standardised anomaly)
#'    using the 1991-2020 climate reference period
#' 5. Filters to interview dates only (dropping NA-producing window rows)
#'    and collects
#' 6. Computes binning cutoffs in R (quantiles / k-means require collected
#'    data) then applies bin labels to the collected frame
#'
#' @param survey_data       Data frame with columns: code, year, survname,
#'   loc_id, timestamp. Used to derive file paths.
#' @param selected_weather  Data frame with one row per weather variable and
#'   columns: name, ref_start, ref_end, temporalAgg, transformation,
#'   cont_binned, num_bins, binning_method.
#' @param dates   Date vector of unique survey timestamps (monthly).
#'   Only these rows are retained after temporal aggregation; rows outside
#'   this set have NA window values and are dropped.
#' @param connection_params List passed to load_data() describing the data
#'   source (e.g. list(type = "local", path = "...")).
#'
#' @return A collected data frame with columns loc_id, timestamp, and one
#'   column per weather variable (numeric, or factor if binned).
get_weather <- function(survey_data, selected_weather, dates, connection_params) {

  # -- File paths -------------------------------------------------------------
  weather_fnames <- paste0(unique(survey_data$code), "_weather.parquet")

  h3_fnames <- survey_data |>
    dplyr::distinct(code, year, survname) |>
    dplyr::mutate(fname = paste0(code, "_", year, "_", survname, "_h3.parquet")) |>
    dplyr::pull(fname)

  # -- Date range ------------------------------------------------------------
  max_lag  <- max(selected_weather$ref_end, na.rm = TRUE)
  date_min <- min(dates) - months(max_lag)
  date_max <- max(dates)

  # Extend back to cover the 1991-2020 climate reference period (plus max lag)
  # if any variable requires a deviation-from-mean or standardised anomaly
  # transformation — the reference stats must be computed over this period
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

    # Ensure population column exists for weighting; if missing, assume equal weights (pop = 1)
    if (!"pop_2020" %in% colnames(h3_slim)) {
      h3_slim <- h3_slim |> dplyr::mutate(pop_2020 = 1L)
    }

  h3_slim <- h3_slim |>
    dplyr::filter(!is.na(h3), !is.na(pop_2020), pop_2020 > 0) |>
    dplyr::select(h3, code, year, survname, loc_id, pop_2020) |>
    dplyr::distinct()

  # -- Spatial aggregation: h3 -> loc_id (population-weighted mean) ----------
  loc_weather <- weather |>
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
  # -- Temporal aggregation: rolling window per variable in DuckDB -----------
  # ROWS BETWEEN ref_end PRECEDING AND ref_start PRECEDING covers the full
  # reference period lag range without materialising one column per lag step.
  # Rows where the window cannot be fully filled (early timestamps per loc_id)
  # will have NA — these are dropped by the dates filter below.
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

  # create a temporary named table for loc_weather to allow referencing the rolling
  tmp_name <- paste0("lw_", paste0(sample(letters, 8, replace = TRUE), collapse = ""))
  
  loc_weather <- loc_weather |>
    dplyr::mutate(!!!roll_exprs) |>
    dplyr::compute(name = tmp_name, temporary = TRUE)

  # -- Transformation: deviation from mean / standardised anomaly ------------
  # Skipped for spi6/spei6 which are already standardised indices.
  # climate_ref is computed from the post-aggregation values over 1991-2020,
  # grouped by loc_id x calendar month, and joined back for the arithmetic.
  # Each iteration wraps loc_weather in a new lazy subquery; DuckDB folds
  # these into a single execution plan.
  skip_transform <- c("spi6", "spei6")

  for (i in seq_len(nrow(selected_weather))) {
    v              <- selected_weather$name[i]
    transformation <- selected_weather$transformation[i]

    if (is.na(transformation) ||
        transformation == "None" ||
        v %in% skip_transform) next

    climate_ref <- loc_weather |>
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

    loc_weather <- loc_weather |>
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

  # -- Filter to interview dates and collect ---------------------------------
  # Filtering to dates serves double duty:
  # (a) drops the climate reference period rows (1991-2020) loaded solely
  #     for transformation statistics
  # (b) drops any rows with NA window values where the rolling window could
  #     not be filled (loc_id x timestamps before enough history existed)
  result <- loc_weather |>
    dplyr::filter(timestamp %in% dates) |>
    dplyr::collect()

  # Drop the temporary table to avoid accumulation across repeated calls
  DBI::dbRemoveTable(dbplyr::remote_con(loc_weather), tmp_name)

  # -- Binning (R only: cutoff computation requires collected data) -----------
  # Cutoffs are derived from the survey-sample distribution of the (possibly
  # transformed) variable. cut() is then applied to the collected frame.
  for (i in seq_len(nrow(selected_weather))) {
    v              <- selected_weather$name[i]
    cont_binned    <- selected_weather$cont_binned[i]
    num_bins       <- selected_weather$num_bins[i]
    binning_method <- selected_weather$binning_method[i]

    if (is.na(cont_binned) || cont_binned != "Binned") next

    haz_vals <- result[[v]][is.finite(result[[v]])]

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
      # Strip the outer bounds before adding -Inf/Inf to avoid duplicate
      # boundary values that would cause cut() to produce empty intervals
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

# # 3. Max, deviation from mean, continuous
# sw3 <- data.frame(
#   name = "tx",
#   ref_start = 0,
#   ref_end = 11,
#   temporalAgg = "Max",
#   cont_binned = "Continuous",
#   binning_method = NA_character_,
#   num_bins = NA_integer_,
#   transformation = "Deviation from mean"
# )
# res3 <- get_weather(survey_data, sw3, dates, connection_params)

# # 4. Min, standardised anomaly, binned (equal width)
# sw4 <- data.frame(
#   name = "tx",
#   ref_start = 3,
#   ref_end = 6,
#   temporalAgg = "Min",
#   cont_binned = "Binned",
#   binning_method = "Equal width",
#   num_bins = 5,
#   transformation = "Standardized anomaly"
# )
# res4 <- get_weather(survey_data, sw4, dates, connection_params)

# # 5. Sum, no transformation, binned (K-means)
# sw5 <- data.frame(
#   name = "tx",
#   ref_start = 1,
#   ref_end = 3,
#   temporalAgg = "Sum",
#   cont_binned = "Binned",
#   binning_method = "K-means",
#   num_bins = 3,
#   transformation = "None"
# )
# res5 <- get_weather(survey_data, sw5, dates, connection_params)

# # 6. Multiple variables, mixed settings
# sw6 <- data.frame(
#   name = c("tx", "t"),
#   ref_start = c(1, 0),
#   ref_end = c(7, 11),
#   temporalAgg = c("Mean", "Max"),
#   cont_binned = c("Continuous", "Binned"),
#   binning_method = c(NA_character_, "Equal frequency"),
#   num_bins = c(NA, 4),
#   transformation = c("None", "Deviation from mean")
# )
# res6 <- get_weather(survey_data, sw6, dates, connection_params)
