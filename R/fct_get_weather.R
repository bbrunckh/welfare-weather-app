# Weather loading and processing ----
# Weather loading and processing pipeline.
# Loads ERA5 historical weather and CMIP6 climate projections from parquet
# files via DuckDB. Applies spatial aggregation (H3 -> survey location),
# temporal rolling windows, and climate perturbations.
#
# Called by:
#   - mod_1_04_weather.R  (weather preview in Module 1)
#   - fct_run_simulation.R / mod_2_01_weathersim.R (simulation in Module 2)
#
# Main exports:
#   get_weather()          - full weather loading pipeline
#
# Internal helpers (not exported):
#   .harmonise_h3()        - H3 resolution matching
#   .apply_transformations() - anomaly/deviation computation in DuckDB
#   .compute_breaks()      - histogram/quantile breakpoints
#   .apply_binning()       - apply cut points to weather data frame

# Remote weather disk cache (PERF-13) ----
#                                                                              #
# get_weather() re-reads identical ERA5/CMIP6 parquet files from the remote    #
# store on every run. For remote backends (s3/gcs/azure/databricks) each read  #
# is a network fetch; a bounded local parquet cache removes the repeat cost    #
# across runs while leaving the data flow byte-identical: the cache stores     #
# exactly the rows (and, where applied, the column/date slice) the lazy remote #
# scan would have produced, in the same scan order (DuckDB is pinned to one    #
# thread for the duration of get_weather, so order is deterministic).          #
#                                                                              #
# Keying: digest of (cache version, resolved file paths, variable columns,     #
# date bounds). The version constant must be bumped whenever the upstream      #
# file layout changes. Kill switch: WISEAPP_WEATHER_CACHE_DISABLE=1. Size cap: #
# WISEAPP_WEATHER_CACHE_MAX_MB (default 2048), LRU-evicted by mtime.           #

WISEAPP_WX_CACHE_VERSION <- "v1"
WISEAPP_WX_ROUND_DIGITS <- 5L

.wx_env_flag <- function(name, default = FALSE) {
  value <- Sys.getenv(name, unset = if (default) "1" else "0")
  value %in% c("1", "true", "TRUE", "yes", "YES")
}

.wx_env_number <- function(name, default) {
  value <- suppressWarnings(as.numeric(Sys.getenv(name, unset = "")))
  if (!is.finite(value)) default else value
}

.wx_available_cpu_count <- function() {
  configured <- .wx_env_number("WISEAPP_WEATHER_CPU_COUNT", NA_real_)
  if (is.finite(configured)) {
    return(max(1L, floor(configured)))
  }
  detected <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_integer_)
  if (length(detected) != 1L || !is.finite(detected)) 1L else max(1L, as.integer(detected))
}

.wx_round_weather_values <- function(df, vars, digits = WISEAPP_WX_ROUND_DIGITS) {
  if (!is.data.frame(df) || !length(vars)) {
    return(df)
  }
  for (v in intersect(vars, names(df))) {
    x <- df[[v]]
    if (!is.numeric(x)) next
    finite <- is.finite(x)
    x[finite] <- round(x[finite], digits = digits)
    df[[v]] <- x
  }
  df
}

.wx_thread_policy <- function(requested = c("auto", "1", "2"),
                              connection_type = "local",
                              estimated_bytes = NA_real_,
                              rss_before = NA_real_,
                              budget_bytes = NA_real_,
                              available_cpus = .wx_available_cpu_count(),
                              auto_enabled = .wx_env_flag(
                                "WISEAPP_WEATHER_THREADS_AUTO_ENABLE"
                              ),
                              min_workload_bytes = .wx_env_number(
                                "WISEAPP_WEATHER_THREADS_MIN_BYTES", 64 * 1024^2
                              )) {
  requested <- match.arg(requested)
  connection_type <- connection_type %||% "local"
  available_cpus <- max(1L, as.integer(available_cpus[[1L]] %||% 1L))
  finite_rss <- is.finite(rss_before) && is.finite(budget_bytes)
  projected_rss <- if (is.finite(rss_before) && is.finite(estimated_bytes)) {
    rss_before + estimated_bytes * 1.25
  } else {
    NA_real_
  }
  fits_budget <- finite_rss && is.finite(projected_rss) &&
    projected_rss <= budget_bytes
  local_source <- identical(connection_type, "local")
  workload_large <- is.finite(estimated_bytes) &&
    estimated_bytes >= min_workload_bytes

  selected <- 1L
  reason <- switch(requested,
    "1" = "explicit_one",
    "2" = "explicit_two_requires_preflight",
    "auto" = if (!isTRUE(auto_enabled)) {
      "auto_rollout_disabled"
    } else {
      "auto_requires_preflight"
    }
  )

  if (requested %in% c("auto", "2")) {
    can_use_two <- available_cpus >= 2L && fits_budget
    if (requested == "auto") {
      can_use_two <- can_use_two && local_source && workload_large
      if (!isTRUE(auto_enabled)) can_use_two <- FALSE
    }
    if (can_use_two) {
      selected <- 2L
      reason <- if (requested == "auto") "auto_preflight_passed" else "explicit_two"
    } else if (available_cpus < 2L) {
      reason <- "insufficient_cpu"
    } else if (!finite_rss) {
      reason <- "rss_unavailable"
    } else if (!fits_budget) {
      reason <- "rss_budget_exceeded"
    } else if (requested == "auto" && !local_source) {
      reason <- "remote_backend"
    } else if (requested == "auto" && !workload_large) {
      reason <- "workload_below_minimum"
    }
  }

  list(
    requested = requested,
    selected = selected,
    selected_threads = selected,
    reason = reason,
    connection_type = connection_type,
    available_cpus = available_cpus,
    estimated_bytes = estimated_bytes,
    min_workload_bytes = min_workload_bytes,
    rss_before = rss_before,
    projected_rss = projected_rss,
    budget_bytes = budget_bytes,
    auto_enabled = isTRUE(auto_enabled),
    rounding_digits = WISEAPP_WX_ROUND_DIGITS
  )
}

# `rss` is deliberately measured through an external `ps` process. R's
# `gc()`/object.size() do not include DuckDB children or allocator-retained
# pages, so they are not safe gates for a deployment memory budget.
.wx_process_tree_rss_bytes <- function(pid = Sys.getpid()) {
  rows <- tryCatch(system2("ps", c("-axo", "pid=,ppid=,rss="), stdout = TRUE),
    error = function(e) character()
  )
  if (!length(rows)) {
    return(NA_real_)
  }
  fields <- strsplit(trimws(rows), "[[:space:]]+")
  tab <- do.call(rbind, lapply(fields, function(x) {
    if (length(x) < 3L) {
      return(c(NA, NA, NA))
    }
    as.numeric(x[1:3])
  }))
  tab <- tab[stats::complete.cases(tab), , drop = FALSE]
  if (!nrow(tab)) {
    return(NA_real_)
  }
  pids <- as.numeric(pid)
  repeat {
    children <- tab[tab[, 2L] %in% pids, 1L]
    new <- setdiff(children, pids)
    if (!length(new)) break
    pids <- c(pids, new)
  }
  sum(tab[tab[, 1L] %in% pids, 3L], na.rm = TRUE) * 1024
}

.wx_estimate_weather_bytes <- function(survey_data, selected_weather, dates,
                                       ssp = NULL, future_period = NULL) {
  n_loc <- if ("loc_id" %in% names(survey_data)) {
    length(unique(survey_data$loc_id[!is.na(survey_data$loc_id)]))
  } else {
    nrow(survey_data)
  }
  n_dates <- max(1L, length(unique(as.character(dates))))
  n_periods <- if (is.null(future_period)) 0L else length(future_period)
  n_members <- if (is.null(ssp)) 0L else max(1L, length(ssp)) * 16L
  n_rows <- n_loc * n_dates * (1 + n_members * max(1L, n_periods))
  n_cols <- length(unique(c(STEP2_WEATHER_KEY_COLUMNS, selected_weather$name)))
  as.numeric(n_rows) * n_cols * 8 * 1.5
}

.wx_collection_policy <- function(estimated_bytes, requested = c("fast", "bounded")) {
  requested <- match.arg(requested)
  budget_mb <- suppressWarnings(as.numeric(Sys.getenv(
    "WISEAPP_STEP2_WEATHER_RSS_BUDGET_MB", "4096"
  )))
  if (!is.finite(budget_mb) || budget_mb <= 0) budget_mb <- 4096
  budget <- budget_mb * 1024^2
  fallback <- identical(requested, "fast") && is.finite(estimated_bytes) &&
    estimated_bytes > budget
  measure_rss <- isTRUE(Sys.getenv("WISEAPP_STEP2_WEATHER_RSS_MEASURE") %in%
    c("1", "true", "TRUE"))
  list(
    requested = requested, effective = if (fallback) "bounded" else requested,
    estimated_bytes = estimated_bytes, budget_bytes = budget,
    fallback = fallback,
    external_rss_before = if (measure_rss) .wx_process_tree_rss_bytes() else NULL
  )
}

.wx_collection_rss_guard <- function(policy) {
  rss <- .wx_process_tree_rss_bytes()
  list(
    rss = rss,
    exceeded = is.finite(rss) && is.finite(policy$budget_bytes) &&
      rss > policy$budget_bytes
  )
}

.weather_cache_dir <- function() {
  base <- Sys.getenv("WISEAPP_WEATHER_CACHE_DIR")
  if (!nzchar(base)) {
    base <- tools::R_user_dir("wiseapp", "cache")
  }
  file.path(base, "weather", WISEAPP_WX_CACHE_VERSION)
}

.weather_cache_evict <- function(dir, max_mb = NULL) {
  if (is.null(max_mb)) {
    max_mb <- suppressWarnings(as.numeric(Sys.getenv("WISEAPP_WEATHER_CACHE_MAX_MB")))
    if (is.na(max_mb) || max_mb < 0) max_mb <- 2048
  }
  files <- list.files(dir,
    pattern = "\\.parquet$", full.names = TRUE,
    recursive = TRUE
  )
  if (length(files) == 0) {
    return(invisible(NULL))
  }
  info <- file.info(files)
  total_mb <- sum(info$size, na.rm = TRUE) / 1024^2
  if (total_mb <= max_mb) {
    return(invisible(NULL))
  }
  # LRU: delete oldest-accessed files first until under budget
  ord <- order(info$mtime)
  for (f in files[ord]) {
    if (total_mb <= max_mb) break
    sz <- info[f, "size"]
    if (unlink(f) == 0 && is.finite(sz)) total_mb <- total_mb - sz / 1024^2
  }
  invisible(NULL)
}

#' Load a remote weather/h3 parquet set through the bounded disk cache.
#'
#' Returns a lazy DuckDB relation whose contents are identical to applying
#' `cols` / `tmin` / `tmax` directly to `load_data(fnames, ...)`. On local
#' connections the cache is bypassed (local parquet reads are already fast)
#' and the plain lazy relation is returned.
#'
#' @param fnames           Character vector of store-relative parquet paths.
#' @param connection_params Passed to load_data().
#' @param cols             Character column subset to keep, or NULL for all
#'   columns. The slice copied to the cache holds exactly these columns.
#' @param tmin,tmax        Optional date bounds on `tcol` (applied in the
#'   cached COPY and re-applied downstream - idempotent).
#' @param cache_version    Bump to invalidate every cached slice.
#' @noRd
.wx_cache_load <- function(fnames, connection_params,
                           cols = NULL, tcol = "timestamp",
                           tmin = NULL, tmax = NULL,
                           cache_version = WISEAPP_WX_CACHE_VERSION) {
  apply_slice <- function(lazy) {
    if (!is.null(cols)) lazy <- dplyr::select(lazy, dplyr::all_of(cols))
    if (!is.null(tcol) && !is.null(tmin)) {
      lazy <- dplyr::filter(
        lazy,
        !!rlang::sym(tcol) >= !!tmin, !!rlang::sym(tcol) <= !!tmax
      )
    }
    lazy
  }

  force_cache <- isTRUE(Sys.getenv("WISEAPP_WEATHER_CACHE_FORCE") %in%
    c("1", "true", "TRUE"))
  type <- connection_params$type %||% "local"
  use_cache <- (!identical(type, "local") || force_cache) &&
    !isTRUE(Sys.getenv("WISEAPP_WEATHER_CACHE_DISABLE") %in% c("1", "true", "TRUE"))

  key <- digest::digest(list(cache_version, sort(fnames), cols, tcol, tmin, tmax))
  dir <- .weather_cache_dir()
  path <- file.path(dir, paste0(key, ".parquet"))

  # Cache hit: load the local slice WITHOUT opening the remote store, so a
  # cached fetch still works when the source is temporarily unreachable.
  if (use_cache && file.exists(path)) {
    local <- tryCatch(
      load_data(path, list(type = "local", path = dirname(path)), collect = FALSE),
      error = function(e) NULL
    )
    if (!is.null(local)) {
      return(apply_slice(local))
    }
  }

  lazy <- load_data(fnames, connection_params, collect = FALSE)

  if (!use_cache) {
    return(apply_slice(lazy))
  }

  if (!file.exists(path)) {
    ok_create <- dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    filtered <- apply_slice(lazy)
    con <- .duck_con()
    tmp_path <- paste0(path, ".tmp")
    ok <- tryCatch(
      {
        DBI::dbExecute(con, sprintf(
          "COPY (%s) TO '%s' (FORMAT PARQUET, COMPRESSION ZSTD);",
          dbplyr::sql_render(filtered), tmp_path
        ))
        TRUE
      },
      error = function(e) {
        warning("[wiseapp] weather disk cache write failed; continuing remote: ",
          conditionMessage(e),
          call. = FALSE
        )
        FALSE
      }
    )
    if (!ok) {
      return(filtered)
    }
    if (!file.rename(tmp_path, path)) {
      # Concurrent write race: another session won; use its file
      try(unlink(tmp_path), silent = TRUE)
      if (!file.exists(path)) {
        return(filtered)
      }
    }
    .weather_cache_evict(dir)
  }

  # Load the cached slice locally. Contents are identical to the remote scan
  # (same rows, same order - single-threaded COPY preserves scan order), and
  # the slice filter is re-applied downstream as a no-op.
  local <- tryCatch(
    load_data(path, list(type = "local", path = dirname(path)), collect = FALSE),
    error = function(e) NULL
  )
  if (is.null(local)) {
    return(apply_slice(lazy))
  }
  apply_slice(local)
}

# H3 spatial helpers ----

#' Harmonise H3 resolution and type between microdata and weather tables.
#'
#' This helper:
#' 1. Detects the H3 resolution of each table by sampling one row.
#' 2. Chooses the **coarser** (lower numeric) resolution as the join key.
#'    This handles all three cases:
#'    * weather coarser than microdata  -> map microdata up to weather res
#'    * microdata coarser than weather  -> map weather up to microdata res
#'    * same resolution                 -> type-cast only, no parent lookup
#' 3. Adds an `h3_weather` column (bigint) to `h3_slim` so the caller can
#'    join on `h3_slim$h3_weather == weather$h3` without further casting.
#'
#' The H3 DuckDB extension must already be loaded before calling this
#' function (`.duck_load_ext("h3")`).
#'
#' @param h3_slim  Lazy `dplyr::tbl` with an `h3` column (string).
#' @param weather  Lazy `dplyr::tbl` with an `h3` column (bigint / int64).
#' @param con      DBI connection - used for the resolution-detection query.
#'
#' @return A list with:
#'   * `h3_slim`      - original `h3_slim` augmented with an `h3_weather`
#'                      bigint column at the chosen target resolution.
#'   * `weather`      - `weather` tbl, possibly with `h3` mapped to the
#'                      target resolution (when weather is *finer* than micro).
#'   * `target_res`   - integer, the target (coarser) H3 resolution.
#'   * `same_res`     - logical, TRUE when no parent lookup was needed.
#' @noRd
.harmonise_h3 <- function(h3_slim, weather, con) {
  micro_h3_sql <- dbplyr::sql_render(
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
  same_res <- (res_micro == res_weather)

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

# Weather transformation helpers ----
# .apply_transformations() - anomaly/deviation in DuckDB SQL                   #
# .compute_breaks()        - binning breakpoint computation                    #
# .apply_binning()         - apply breakpoints to data frame                   #

#' Build transformation specifications for selected weather variables.
#'
#' @param selected_weather Data frame with `name` and `transformation`.
#' @param skip_vars Character vector of variables to leave untransformed.
#' @noRd
.transformation_specs <- function(
  selected_weather,
  skip_vars = c("spi6", "spei6")
) {
  required <- c("name", "transformation")
  missing <- setdiff(required, names(selected_weather))
  if (length(missing) > 0L) {
    stop("selected_weather is missing column(s): ", paste(missing, collapse = ", "))
  }
  if (anyDuplicated(selected_weather$name)) {
    stop("selected_weather contains duplicate weather variable names")
  }

  keep <- !is.na(selected_weather$transformation) &
    selected_weather$transformation != "None" &
    !selected_weather$name %in% skip_vars
  idx <- which(keep)
  if (length(idx) == 0L) {
    return(NULL)
  }

  data.frame(
    row_id = idx,
    name = as.character(selected_weather$name[idx]),
    transformation = as.character(selected_weather$transformation[idx]),
    mean_col = paste0("__wise_ref_mean_", idx),
    sd_col = paste0("__wise_ref_sd_", idx),
    stringsAsFactors = FALSE
  )
}

#' Build one wide monthly climate-reference relation.
#'
#' @param loc_weather_base Lazy or materialised rolled weather relation.
#' @param selected_weather Data frame with `name` and `transformation`.
#' @param skip_vars Character vector of variables to leave untransformed.
#' @return A list containing the reference relation and transformation specs.
#' @noRd
.build_climate_reference <- function(
  loc_weather_base,
  selected_weather,
  skip_vars = c("spi6", "spei6")
) {
  specs <- .transformation_specs(selected_weather, skip_vars)
  if (is.null(specs)) {
    return(NULL)
  }

  existing <- colnames(loc_weather_base)
  ref_cols <- c(specs$mean_col, specs$sd_col)
  if (any(ref_cols %in% existing)) {
    stop(
      "Generated climate-reference column collides with weather data: ",
      paste(intersect(ref_cols, existing), collapse = ", ")
    )
  }

  stats_exprs <- c(
    stats::setNames(
      lapply(specs$name, function(v) {
        dbplyr::sql(paste0(
          "AVG(", v, ") FILTER (WHERE ", v, " IS NOT NULL)"
        ))
      }),
      specs$mean_col
    ),
    stats::setNames(
      lapply(specs$name, function(v) {
        dbplyr::sql(paste0(
          "STDDEV_SAMP(", v, ") FILTER (WHERE ", v, " IS NOT NULL)"
        ))
      }),
      specs$sd_col
    )
  )

  reference <- loc_weather_base |>
    dplyr::filter(
      timestamp >= as.Date("1991-01-01"),
      timestamp <= as.Date("2020-12-31")
    ) |>
    dplyr::mutate(month = dbplyr::sql("MONTH(timestamp)")) |>
    dplyr::group_by(code, year, survname, loc_id, month) |>
    dplyr::summarise(!!!stats_exprs, .groups = "drop")

  list(tbl = reference, specs = specs)
}

#' Apply climate-reference transformations with one shared left join.
#'
#' @param tbl Lazy rolled weather relation to transform.
#' @param selected_weather Data frame with `name` and `transformation`.
#' @param loc_weather_base Rolled weather relation used when building a reference.
#' @param skip_vars Character vector of variables to leave untransformed.
#' @param climate_ref Optional result from `.build_climate_reference()`.
#' @return A lazy transformed weather relation.
#' @noRd
.apply_transformations <- function(
  tbl,
  selected_weather,
  loc_weather_base,
  skip_vars = c("spi6", "spei6"),
  climate_ref = NULL
) {
  if (is.null(climate_ref)) {
    climate_ref <- .build_climate_reference(
      loc_weather_base, selected_weather, skip_vars
    )
  }
  if (is.null(climate_ref)) {
    return(tbl)
  }

  specs <- climate_ref$specs
  ref_cols <- c(specs$mean_col, specs$sd_col)
  tbl <- tbl |>
    dplyr::mutate(month = dbplyr::sql("MONTH(timestamp)")) |>
    dplyr::left_join(
      climate_ref$tbl,
      by = c("code", "year", "survname", "loc_id", "month")
    )

  for (i in seq_len(nrow(specs))) {
    v <- specs$name[i]
    tf <- specs$transformation[i]
    expr <- if (tf == "Deviation from mean") {
      dbplyr::sql(paste0(v, " - ", specs$mean_col[i]))
    } else if (tf == "Standardized anomaly") {
      dbplyr::sql(paste0(
        "(", v, " - ", specs$mean_col[i], ") / ", specs$sd_col[i]
      ))
    } else {
      NULL
    }
    tbl <- dplyr::mutate(tbl, !!v := expr)
  }

  tbl |>
    dplyr::select(-month, -dplyr::all_of(ref_cols))
}

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
#'   `num_bins`, `binning_method`, and (optionally) `custom_breaks` (list
#'   column of numeric vectors used when `binning_method == "Custom"`).
#'
#' @return A named list of break vectors (one per binned variable).
#'   Variables that are not binned or fail to produce valid breaks are omitted.
#' @noRd
.compute_breaks <- function(ref_df, selected_weather) {
  stored_breaks <- list()
  has_custom_col <- "custom_breaks" %in% names(selected_weather)

  for (i in seq_len(nrow(selected_weather))) {
    v <- selected_weather$name[i]
    cont_binned <- selected_weather$cont_binned[i]
    num_bins <- selected_weather$num_bins[i]
    binning_method <- selected_weather$binning_method[i]

    if (is.na(cont_binned) || cont_binned != "Binned") next

    haz_vals <- ref_df[[v]][is.finite(ref_df[[v]])]

    # sort for consistent quantile breaks and k-means results
    haz_vals <- sort(haz_vals)

    cutoffs <- switch(binning_method,
      "Equal frequency" = {
        unique(stats::quantile(haz_vals, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE))
      },
      "Equal width" = {
        unique(seq(min(haz_vals, na.rm = TRUE), max(haz_vals, na.rm = TRUE), length.out = num_bins + 1))
      },
      "K-means" = {
        tryCatch(
          {
            if (length(unique(haz_vals)) >= num_bins) {
              km <- withr::with_seed(
                123,
                stats::kmeans(haz_vals, centers = num_bins)
              )
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
          },
          error = function(e) {
            message("K-means failed for ", v, ": ", e$message)
            NULL
          }
        )
      },
      "Custom" = {
        user_cuts <- if (has_custom_col) selected_weather$custom_breaks[[i]] else NULL
        if (is.null(user_cuts) || length(user_cuts) == 0) {
          message("Custom binning for ", v, " requires cut values but none were provided. Keeping continuous.")
          NULL
        } else {
          user_cuts <- sort(unique(as.numeric(user_cuts[is.finite(user_cuts)])))
          expected <- as.integer(num_bins) - 1L
          if (length(user_cuts) != expected) {
            message(
              "Custom binning for ", v, " expected ", expected,
              " cut values (num_bins - 1) but got ", length(user_cuts),
              ". Using the supplied values as-is."
            )
          }
          # Mirror the structure used by the other branches: a vector whose
          # first/last entries are dropped by the breaks_ext step below.
          c(min(haz_vals, na.rm = TRUE), user_cuts, max(haz_vals, na.rm = TRUE))
        }
      },
      NULL
    )

    if (!is.null(cutoffs) && length(cutoffs) > 1) {
      breaks_ext <- c(-Inf, cutoffs[-c(1, length(cutoffs))], Inf)
      stored_breaks[[v]] <- breaks_ext
      # The extended breaks are what cut() needs, but the outer sentinel
      # edges hide the observed weather range from every downstream label.
      # Carry the observed cutoffs alongside (attr ignored by consumers that
      # use the vector numerically).
      attr(stored_breaks[[v]], "observed") <- cutoffs
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
#' @return `df` with binned columns converted to factors via `cut()` and using
#'   the same display-safe levels as the fitted model data.
#' @noRd
.apply_binning <- function(df, breaks) {
  for (v in names(breaks)) {
    if (v %in% names(df)) {
      df[[v]] <- cut(df[[v]], breaks = breaks[[v]], include.lowest = TRUE)
    }
  }
  # Model fitting relabels sentinel outer edges for display. Apply that
  # canonical relabelling here as well so simulation newdata has exactly the
  # factor levels used by the fitted model and its weather coefficients are
  # not silently omitted from the prediction design matrix.
  relabel_bin_levels(df, breaks)
}

# Main weather loading pipeline ----
# get_weather() - loads ERA5 + CMIP6, applies rolling windows + perturbations  #
# Note: DuckDB rolling window (0%/16% stall) occurs in loc_weather_base        #
# materialisation and batch query. See tradeoffs for improvements              #
# in known_issues.md #13, #15.                                                 #

#' Load, aggregate, and construct weather variables for survey locations.
#'
#' 1. Loads weather and H3-to-location parquet files lazily in DuckDB.
#' 2. Spatially aggregates weather to `loc_id` (population-weighted mean).
#' 3. Applies rolling temporal aggregation and materialises the unperturbed
#'    weather series as a temp table (`loc_weather_base`).
#' 4. If `ssp` is supplied: loads CMIP6 files for each scenario.
#'    For each SSP, computes population-weighted loc-level raw deltas,
#'    then processes all models in a single batched DuckDB query -
#'    perturb raw monthly values -> roll -> transform -> collect.
#'    All models with a complete set of weather variable deltas are returned.
#' 5. Transformations (deviation-from-mean, standardised anomaly) are applied
#'    using the 1991-2020 climate reference, always derived from
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
#' @param stored_breaks     Optional named list of pre-computed bin breaks
#'   keyed by weather variable name. When non-empty, these breaks are used
#'   for the matching binned variables instead of re-deriving them.
#' @param weather_threads   DuckDB weather-query thread mode: `"auto"` (the
#'   default), `"1"`, or `"2"`. Automatic selection is conservative and remains
#'   pinned to one thread until `WISEAPP_WEATHER_THREADS_AUTO_ENABLE=1` is set.
#'   All returned finite weather values use the fixed 5-decimal output policy.
#' @return A named list of collected data frames with columns
#'   `code, year, survname, loc_id, timestamp, <weather_vars>`:
#'   * `"historical"` - unperturbed result filtered to `dates`.
#'   * `"<ssp>_<start>_<end>_<model>"` - e.g.
#'     `"ssp2_4_5_2045_2055_MPI.ESM1.2.HR"` (model name sanitised via
#'     `make.names()`).  All models with a complete set of weather variable
#'     deltas are returned.
#'
#'   When any variable is binned, the list also carries a
#'   `"continuous_weather"` attribute: the key columns plus the binned
#'   variables' pre-`cut()` (but post-transformation) values for the
#'   historical slice. Descriptive use only - the modelling pipeline uses the
#'   binned columns in `"historical"`.
#'
#' @export
get_weather <- function(
  survey_data,
  selected_surveys,
  selected_weather,
  dates,
  connection_params,
  ssp = NULL,
  future_period = NULL,
  perturbation_method = NULL,
  epsilon = 0.001,
  weather_source = "era5land",
  proj_source = "cmip6",
  stored_breaks = NULL,
  weather_collect = c("fast", "bounded"),
  weather_threads = c("auto", "1", "2"),
  weather_consumer = NULL
) {
  # -- Select and pin DuckDB weather-query threads ----------------------------
  # Multi-threaded aggregation sums floats in non-deterministic order. The
  # output boundary rounds weather values to a fixed five-decimal precision, while one
  # thread remains the conservative default and automatic fallback.
  con_det <- .duck_con()
  prev_threads <- DBI::dbGetQuery(con_det, "SELECT current_setting('threads') AS t")$t
  weather_threads <- match.arg(weather_threads)

  climate_scenario <- !is.null(ssp)
  weather_collect <- match.arg(weather_collect)
  estimated_weather_bytes <- .wx_estimate_weather_bytes(
    survey_data, selected_weather, dates, ssp, future_period
  )
  rss_before_threads <- .wx_process_tree_rss_bytes()
  thread_policy <- .wx_thread_policy(
    requested = weather_threads,
    connection_type = connection_params$type %||% "local",
    estimated_bytes = estimated_weather_bytes,
    rss_before = rss_before_threads,
    budget_bytes = .wx_env_number(
      "WISEAPP_STEP2_WEATHER_RSS_BUDGET_MB", 4096
    ) * 1024^2
  )
  DBI::dbExecute(con_det, paste("SET threads TO", thread_policy$selected_threads))
  on.exit(DBI::dbExecute(con_det, paste("SET threads TO", prev_threads)), add = TRUE)
  # Process RSS is a preflight input, not part of the deterministic weather
  # contract. Keep the stable selection decision in the returned policy while
  # leaving volatile measurements to the benchmark instrumentation.
  thread_policy$rss_before <- NULL
  thread_policy$projected_rss <- NULL

  # -- Validate ---------------------------------------------------------------
  collection_policy <- .wx_collection_policy(
    estimated_weather_bytes, weather_collect
  )
  collection_policy$weather_threads <- thread_policy
  weather_collect <- collection_policy$effective
  if (is.function(weather_consumer)) {
    # Callback consumers must never wait for a whole period to materialise.
    # The legacy fast return path remains available when no consumer is used.
    weather_collect <- "bounded"
    collection_policy$effective <- "bounded"
    collection_policy$consumer_bounded <- TRUE
  } else {
    collection_policy$consumer_bounded <- FALSE
  }
  collection_policy$rss_guard_activated <- FALSE
  if (!is.null(collection_policy$external_rss_before) &&
    is.finite(collection_policy$external_rss_before) &&
    collection_policy$external_rss_before + collection_policy$estimated_bytes >
      collection_policy$budget_bytes) {
    weather_collect <- "bounded"
    collection_policy$effective <- "bounded"
    collection_policy$rss_guard_activated <- TRUE
    collection_policy$fallback_reason <-
      "observed_process_tree_rss_plus_estimate_exceeded_budget"
  }
  collection_policy$buffered_member_peak <- 0L

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

    # Normalise future_period: a bare length-2 vector -> single-element list
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
  max_lag <- as.integer(max(selected_weather$ref_end, na.rm = TRUE))
  date_min <- seq.Date(min(dates), by = paste0("-", max_lag, " months"), length.out = 2L)[[2L]]
  date_max <- max(dates)

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

  # -- Temp-table cleanup ledger (SEC-02) --------------------------------------
  # DuckDB connections are process-wide, so materialised temp tables survive
  # errors until the worker exits. Every table created below is registered here
  # the moment it is created and dropped best-effort on function exit (happy
  # path or error). Tables released early are removed from the ledger first;
  # on.exit only runs after every relation has been collected, so no live lazy
  # query can reference a dropped table when results are returned.
  tmp_tables <- character(0)
  on.exit(
    {
      for (tn in tmp_tables) {
        try(DBI::dbRemoveTable(con, tn), silent = TRUE)
      }
    },
    add = TRUE
  )

  # PERF-13: remote parquet loads go through the bounded disk cache. The
  # cached slice holds exactly the columns/rows the lazy scan would produce,
  # so the pipelines below are unchanged and results stay bit-identical.
  weather <- .wx_cache_load(
    weather_fnames, connection_params,
    cols = c("h3", "timestamp", weather_vars),
    tmin = date_min, tmax = date_max
  ) |>
    dplyr::select(h3, timestamp, dplyr::all_of(weather_vars)) |>
    dplyr::filter(dplyr::if_all(dplyr::all_of(weather_vars), ~ !is.na(.x))) |>
    dplyr::filter(timestamp >= date_min, timestamp <= date_max)

  # Projection-prune the mapping scan. These are the only fields used by the
  # H3 harmonisation and population-weighted aggregation below; requesting the
  # full parquet schema made DuckDB read unused microdata columns.
  h3_cols <- c("h3", "code", "year", "survname", "loc_id", "pop_2020")
  h3_slim <- tryCatch(
    .wx_cache_load(h3_fnames, connection_params, cols = h3_cols, tcol = NULL),
    error = function(e) {
      # Older mapping files may not carry population weights; preserve their
      # unit-weight fallback without making the common weighted path read the
      # full parquet schema.
      .wx_cache_load(
        h3_fnames, connection_params,
        cols = setdiff(h3_cols, "pop_2020"), tcol = NULL
      )
    }
  )

  if (!"pop_2020" %in% colnames(h3_slim)) {
    h3_slim <- h3_slim |> dplyr::mutate(pop_2020 = 1L)
  }

  h3_slim <- h3_slim |>
    dplyr::filter(!is.na(h3), !is.na(pop_2020), pop_2020 > 0) |>
    dplyr::select(h3, code, year, survname, loc_id, pop_2020)

  # -- H3 resolution + type harmonisation ------------------------------------
  h3_harmonised <- .harmonise_h3(h3_slim, weather, con)
  h3_slim <- h3_harmonised$h3_slim
  weather <- h3_harmonised$weather

  # -- One population weight per location x weather cell ---------------------
  # The mapping file is finer-grained than the weather grid: it carries one row
  # per populated sub-cell of the cell the weather is measured on, so a cell's
  # weight in a location is the *sum* of the sub-cells of it that fall there.
  #
  # These rows must not be de-duplicated on `pop_2020`. Sub-cell populations
  # are small integers (they run from 1 upwards) and two sub-cells of the same
  # cell frequently carry the same count, so a `distinct()` here silently
  # deletes real population: about 1% of rows in the EHCVM files, which shifts
  # the relative weight of cells inside a location by up to a fifth in the
  # worst case. Aggregating explicitly also makes the join below one-to-one.
  h3_slim <- h3_slim |>
    dplyr::group_by(code, year, survname, loc_id, h3_weather) |>
    dplyr::summarise(pop_2020 = sum(pop_2020, na.rm = TRUE), .groups = "drop")

  # Materialise the normalized location-to-weather-cell weights once. The same
  # relation is joined by historical weather and every future model/period.
  h3_weights_name <- basename(tempfile(pattern = "lw_h3_weights_"))
  tmp_tables <- c(tmp_tables, h3_weights_name)
  h3_slim <- dplyr::compute(h3_slim, name = h3_weights_name, temporary = TRUE)

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
  # Materialise location-month weather once. The same relation is consumed by
  # the historical result and every future SSP/period batch.
  tmp_loc_monthly_name <- basename(tempfile(pattern = "lw_loc_monthly_"))
  tmp_tables <- c(tmp_tables, tmp_loc_monthly_name)
  loc_monthly <- dplyr::compute(
    loc_monthly,
    name = tmp_loc_monthly_name, temporary = TRUE
  )

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
      v <- selected_weather$name[i]
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
  # Name generated via tempfile() rather than sample() so this does not
  # consume/advance the caller's RNG stream (see DET-04).
  tmp_base_name <- basename(tempfile(pattern = "lw_base_"))
  tmp_tables <- c(tmp_tables, tmp_base_name)

  loc_weather_base <- loc_monthly |>
    dplyr::mutate(!!!roll_exprs) |>
    dplyr::compute(name = tmp_base_name, temporary = TRUE)

  # PERF-02: aggregate every transformed weather variable once and reuse the
  # materialised reference across the historical and future-period queries.
  climate_ref <- .build_climate_reference(loc_weather_base, selected_weather)
  if (!is.null(climate_ref)) {
    tmp_ref_name <- basename(tempfile(pattern = "lw_ref_"))
    tmp_tables <- c(tmp_tables, tmp_ref_name)
    climate_ref$tbl <- dplyr::compute(
      climate_ref$tbl,
      name = tmp_ref_name,
      temporary = TRUE
    )
  }

  # -- Assemble result -------------------------------------------------------
  result <- list()

  result[["historical"]] <- loc_weather_base |>
    .apply_transformations(
      selected_weather, loc_weather_base,
      climate_ref = climate_ref
    ) |>
    dplyr::filter(timestamp %in% !!dates) |>
    dplyr::arrange(code, year, survname, loc_id, timestamp) |>
    dplyr::collect()
  result[["historical"]] <- .wx_round_weather_values(
    result[["historical"]], weather_vars
  )
  # -- Binning setup ----------------------------------------------------------
  # Determine whether any variables require binning.  Guard against

  # selected_weather missing the binning columns entirely (backward compat).
  has_binning <- "cont_binned" %in% names(selected_weather) &&
    any(!is.na(selected_weather$cont_binned) & selected_weather$cont_binned == "Binned")

  # Pre-binning copy of the binned columns, kept for descriptive plots only
  # (attached as an attribute below so the returned list keeps exactly one
  # element per scenario).
  continuous_hist <- NULL

  if (has_binning) {
    if (is.null(stored_breaks) || length(stored_breaks) == 0) {
      # Compute breaks from the full survey-period loc_id x timestamp distribution.
      # Sort by full location identity + timestamp for a deterministic
      # order regardless of DuckDB's non-guaranteed collect() row order.
      survey_timestamps <- unique(survey_data$timestamp[!is.na(survey_data$timestamp)])
      wx_cols <- selected_weather$name[selected_weather$cont_binned == "Binned" & !is.na(selected_weather$cont_binned)]
      sort_cols <- intersect(c("code", "year", "survname", "loc_id", "timestamp"), names(result[["historical"]]))
      keep <- unique(c(sort_cols, wx_cols))
      hist_ref <- result[["historical"]][result[["historical"]]$timestamp %in% survey_timestamps, keep, drop = FALSE]
      stored_breaks <- .compute_breaks(hist_ref, selected_weather)
    }

    # Keep the untouched (already transformed) values of the binned columns
    # before `cut()` overwrites them, so the UI can show the underlying
    # continuous distribution alongside the bins.
    binned_vars <- intersect(names(stored_breaks), names(result[["historical"]]))
    if (length(binned_vars) > 0) {
      keep_cols <- unique(c(
        intersect(
          c("code", "year", "survname", "loc_id", "timestamp"),
          names(result[["historical"]])
        ),
        binned_vars
      ))
      continuous_hist <- result[["historical"]][, keep_cols, drop = FALSE]
    }

    # Apply to historical slice immediately
    result[["historical"]] <- .apply_binning(result[["historical"]], stored_breaks)
  }

  emitted_order <- 0L
  if (is.function(weather_consumer)) {
    emitted_order <- emitted_order + 1L
    weather_consumer(
      "historical", result[["historical"]],
      list(order = emitted_order, is_historical = TRUE)
    )
  }

  # -- Climate perturbation ---------------------------------------------------
  if (climate_scenario) {
    bp <- range(dates)
    baseline_start <- as.Date(bp[1])
    baseline_end <- as.Date(bp[2])
    delta_vars <- paste0("delta_", weather_vars)

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
    perturb_exprs <- .make_perturb_exprs(perturbation_method, weather_vars)

    # Rolling window expressions with `model` added to PARTITION BY.
    # Used in the batch climate query so each model gets its own independent
    # rolling window over its own perturbed series.
    roll_exprs_climate <- stats::setNames(
      lapply(seq_len(nrow(selected_weather)), function(i) {
        v <- selected_weather$name[i]
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

    # Detect CMIP6 H3 resolution once using the first SSP's historical file.
    # PERF-13: the raw historical slice is loaded once through the disk cache
    # and reused both for the resolution probe (no second remote open) and
    # for the monthly historical baseline below.
    hist_fnames_probe <- paste0(
      "hazard/weather/projections/", survey_codes, "/",
      survey_codes, "_", proj_source, "_historical.parquet"
    )

    cmip6_cols <- c("model", "h3", "timestamp", weather_vars)
    cmip6_hist_raw_lazy <- .wx_cache_load(
      hist_fnames_probe, connection_params,
      cols = cmip6_cols, tcol = NULL
    )

    cmip6_res <- tryCatch(
      {
        probe_sql <- dbplyr::sql_render(
          cmip6_hist_raw_lazy |>
            dplyr::filter(!is.na(h3)) |>
            dplyr::select(h3) |>
            head(1)
        )
        DBI::dbGetQuery(
          con,
          sprintf("SELECT h3_get_resolution(h3) AS res FROM (%s) _t", probe_sql)
        )$res[[1L]]
      },
      error = function(e) h3_harmonised$target_res
    )

    # Determine the join resolution between CMIP6 and microdata.
    # Both must be brought to the coarser (lower) of the two.
    cmip6_join_res <- min(cmip6_res, h3_harmonised$target_res)

    # Add h3_cmip6 column to h3_slim for CMIP6 spatial joins.
    # When CMIP6 is coarser than the observed-weather target resolution,
    # micro H3 cells must be mapped further up to match CMIP6.
    if (cmip6_join_res == h3_harmonised$target_res) {
      # CMIP6 same or finer than target - reuse existing h3_weather column
      h3_slim <- h3_slim |>
        dplyr::mutate(h3_cmip6 = h3_weather)
    } else {
      # CMIP6 coarser than target - map micro cells up to CMIP6 resolution.
      # Coarsen the `h3_weather` bigint column: the original `h3` string
      # column was dropped by the population summarise above, and referencing
      # it here made DuckDB silently bind `h3` to the sibling CMIP6 `h3`
      # join column (a bigint), failing with `h3_string_to_h3(BIGINT)`.
      h3_slim <- h3_slim |>
        dplyr::mutate(
          h3_cmip6 = dbplyr::sql(
            sprintf("h3_cell_to_parent(h3_weather, %d)", cmip6_join_res)
          )
        )
    }

    # Helper: aggregate a pre-loaded raw CMIP6 lazy tbl -> (model, h3, month, vars).
    # PERF-13: callers pass a disk-cache-backed lazy relation instead of
    # re-opening the remote parquet for every (SSP, period) combination.
    .cmip6_h3_monthly <- function(raw_tbl, ts_start, ts_end) {
      tbl <- raw_tbl |>
        dplyr::select(dplyr::all_of(cmip6_cols)) |>
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

    # CMIP6 historical baseline - shared across all SSPs (same files)
    h3_hist_raw <- .cmip6_h3_monthly(cmip6_hist_raw_lazy, baseline_start, baseline_end)

    period_specs <- lapply(seq_along(future_period), function(i) {
      fp <- future_period[[i]]
      list(
        id = i,
        start = as.Date(fp[1]),
        end = as.Date(fp[2]),
        label = paste0(
          format(as.Date(fp[1]), "%Y"), "_",
          format(as.Date(fp[2]), "%Y")
        )
      )
    })

    # -- Per-SSP worker -------------------------------------------------------
    # Processes all models * all future periods.  The CMIP6 historical
    # baseline and SSP baseline-period data are loaded once and shared
    # across periods; only the future-period projection varies.
    # Returns a named list keyed by "<ssp>_<start>_<end>_<model>".
    .process_ssp <- function(ssp_i) {
      ssp_fname <- gsub("_", "", ssp_i)
      future_fnames <- paste0(
        "hazard/weather/projections/", survey_codes, "/",
        survey_codes, "_", proj_source, "_", ssp_fname, ".parquet"
      )

      # The SSP relation is reused for the baseline overlap and every requested
      # future period. Slice it once to their union so the cache/remote parquet
      # scan does not retain unrelated years from the full projection file.
      ssp_starts <- as.Date(vapply(future_period, function(x) as.character(x[[1L]]), character(1L)))
      ssp_ends <- as.Date(vapply(future_period, function(x) as.character(x[[2L]]), character(1L)))
      ssp_tmin <- min(baseline_start, ssp_starts, na.rm = TRUE)
      ssp_tmax <- max(baseline_end, ssp_ends, na.rm = TRUE)

      # PERF-13: the future file is fetched through the disk cache once per
      # SSP and reused for the baseline overlap *and* every future period
      # (previously one remote read per period).
      ssp_raw_lazy <- .wx_cache_load(
        future_fnames,
        connection_params,
        cols = cmip6_cols,
        tmin = ssp_tmin,
        tmax = ssp_tmax
      )

      # SSP baseline overlap - shared across all future periods
      h3_ssp_raw <- .cmip6_h3_monthly(ssp_raw_lazy, baseline_start, baseline_end)

      # Combined CMIP6 historical baseline (hist + ssp overlap period)
      h3_hist <- dplyr::union_all(h3_hist_raw, h3_ssp_raw) |>
        dplyr::group_by(model, h3, month) |>
        dplyr::summarise(
          dplyr::across(dplyr::all_of(weather_vars), ~ mean(.x, na.rm = TRUE)),
          .groups = "drop"
        )

      # Aggregate every requested period once, then join the combined relation
      # to h3_slim once. This removes the repeated location-level spatial join
      # from the period loop while retaining a period key for exact semantics.
      h3_fut_by_period <- lapply(period_specs, function(spec) {
        .cmip6_h3_monthly(ssp_raw_lazy, spec$start, spec$end) |>
          dplyr::mutate(period_id = spec$id)
      })
      h3_fut_all <- Reduce(dplyr::union_all, h3_fut_by_period)
      h3_deltas_all <- dplyr::inner_join(
        h3_hist, h3_fut_all,
        by = c("model", "h3", "month"),
        suffix = c("_hist", "_fut")
      ) |>
        dplyr::mutate(!!!delta_exprs_h3) |>
        dplyr::select(period_id, model, h3, month, dplyr::all_of(delta_vars))

      loc_deltas_all <- h3_deltas_all |>
        dplyr::inner_join(h3_slim, by = c("h3" = "h3_cmip6")) |>
        dplyr::group_by(period_id, model, code, year, survname, loc_id, month) |>
        .pop_weighted_mean(delta_vars)

      # Materialise the complete location-level delta relation before the
      # completeness scan. Both the scan and the per-period filtered queries
      # consume this relation; leaving it lazy would repeat the expensive H3
      # join and population-weighted aggregation.
      tmp_delta_all_name <- basename(tempfile(pattern = "lw_delta_all_"))
      tmp_tables <<- c(tmp_tables, tmp_delta_all_name)
      loc_deltas_all <- dplyr::compute(
        loc_deltas_all,
        name = tmp_delta_all_name,
        temporary = TRUE
      )

      complete_model_tbl <- loc_deltas_all |>
        dplyr::group_by(period_id, model) |>
        dplyr::summarise(
          n_complete = sum(
            dplyr::if_all(dplyr::all_of(delta_vars), ~ !is.na(.x)),
            na.rm = TRUE
          ),
          .groups = "drop"
        ) |>
        dplyr::collect()

      complete_keys <- complete_model_tbl |>
        dplyr::filter(n_complete > 0L) |>
        dplyr::select(period_id, model)
      if (!nrow(complete_keys)) {
        return(list())
      }

      complete_predicates <- vapply(seq_len(nrow(complete_keys)), function(i) {
        sprintf(
          "(period_id = %d AND model = %s)",
          complete_keys$period_id[[i]],
          DBI::dbQuoteString(con, complete_keys$model[[i]])
        )
      }, character(1L))
      loc_deltas_complete <- loc_deltas_all |>
        dplyr::filter(!!dbplyr::sql(paste(complete_predicates, collapse = " OR ")))

      # -- Loop over future periods ------------------------------------------
      out <- list()
      tmp_delta_tables <- character(0L)

      # Once a period has been collected, neither temporary relation is needed
      # by the returned weather frames. Drop it before constructing the next
      # period so DuckDB's materialised intermediates do not accumulate across
      # a future workload.
      .drop_period_tables <- function(...) {
        table_names <- unique(unlist(list(...), use.names = FALSE))
        table_names <- table_names[nzchar(table_names)]
        for (table_name in table_names) {
          try(DBI::dbRemoveTable(con, table_name), silent = TRUE)
        }
        tmp_tables <<- setdiff(tmp_tables, table_names)
        tmp_delta_tables <<- setdiff(tmp_delta_tables, table_names)
        invisible(NULL)
      }

      for (spec in period_specs) {
        fp_start <- spec$start
        fp_end <- spec$end
        fp_label <- spec$label
        current_period_id <- spec$id

        complete_for_period <- complete_model_tbl |>
          dplyr::filter(period_id == !!current_period_id)
        incomplete_models <- complete_for_period$model[complete_for_period$n_complete == 0L]
        if (length(incomplete_models) > 0L) {
          warning(sprintf(
            "%s / %s: %d model(s) excluded due to missing variables (%s): %s",
            ssp_i, fp_label, length(incomplete_models),
            paste(delta_vars, collapse = ", "),
            paste(incomplete_models, collapse = ", ")
          ), call. = FALSE)
        }

        complete_models <- complete_for_period$model[complete_for_period$n_complete > 0L]
        if (length(complete_models) == 0L) next

        loc_deltas_by_model <- loc_deltas_complete |>
          dplyr::filter(
            period_id == !!current_period_id,
            model %in% complete_models
          )

        # Materialise the filtered delta table - lets DuckDB plan a hash join in
        # the batch query without retaining rows for incomplete models.
        tmp_delta_name <- basename(tempfile(pattern = "lw_delta_"))
        tmp_tables <<- c(tmp_tables, tmp_delta_name)
        loc_deltas_by_model <- dplyr::compute(
          loc_deltas_by_model |>
            dplyr::select(-period_id),
          name = tmp_delta_name,
          temporary = TRUE
        )
        tmp_delta_tables <- c(tmp_delta_tables, tmp_delta_name)

        # -- Batch query: split into two steps to help DuckDB plan ----------
        # Step 1: join + perturb + select -> materialise before rolling window
        # Name generated via tempfile() rather than sample() so this does not
        # consume/advance the caller's RNG stream (see DET-04).
        tmp_perturb_name <- basename(tempfile(pattern = "lw_perturb_"))
        tmp_tables <<- c(tmp_tables, tmp_perturb_name)
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

        # Step 2: rolling window + transformations. The fast path keeps this
        # relation lazy and performs one direct collect; the bounded path
        # materialises it so model-specific slices can be collected safely.
        rolled_lazy <- perturbed |>
          dplyr::mutate(!!!roll_exprs_climate) |>
          .apply_transformations(
            selected_weather, loc_weather_base,
            climate_ref = climate_ref
          ) |>
          dplyr::filter(timestamp %in% !!dates)
        tmp_roll_name <- NULL
        rolled <- rolled_lazy
        if (identical(weather_collect, "bounded")) {
          tmp_roll_name <- basename(tempfile(pattern = "lw_roll_"))
          tmp_tables <<- c(tmp_tables, tmp_roll_name)
          rolled <- dplyr::compute(rolled_lazy, name = tmp_roll_name, temporary = TRUE)
        }

        period_out <- if (identical(weather_collect, "fast")) {
          # Production path: one collect after the transformed relation has
          # been materialised. This avoids a DuckDB query/collect round trip per
          # model and is materially faster when latency is the primary concern.
          batch <- rolled_lazy |>
            dplyr::arrange(model, code, year, survname, loc_id, timestamp) |>
            dplyr::collect()
          if (!nrow(batch)) {
            list()
          } else {
            model_list <- split(batch, batch$model)
            stats::setNames(
              lapply(model_list, function(model_df) {
                model_df$model <- NULL
                model_df <- .wx_round_weather_values(model_df, weather_vars)
                if (has_binning) model_df <- .apply_binning(model_df, stored_breaks)
                model_df
              }),
              paste0(ssp_i, "_", fp_label, "_", make.names(names(model_list)))
            )
          }
        } else {
          # Bounded-memory path: collect one model at a time. Keep this option
          # for deployments with a hard RSS ceiling; it is intentionally not
          # the production default because each model repeats the collect work.
          model_names <- DBI::dbGetQuery(
            con,
            paste0(
              "SELECT DISTINCT model FROM (",
              dbplyr::sql_render(rolled |> dplyr::select(model)),
              ") models ORDER BY model"
            )
          )$model
          model_out <- if (is.function(weather_consumer)) NULL else list()
          for (model_name in model_names) {
            model_df <- rolled |>
              dplyr::filter(model == !!model_name) |>
              dplyr::arrange(code, year, survname, loc_id, timestamp) |>
              dplyr::select(-model) |>
              dplyr::collect()
            if (!nrow(model_df)) {
              rm(model_df)
              next
            }
            model_df <- .wx_round_weather_values(model_df, weather_vars)
            if (has_binning) model_df <- .apply_binning(model_df, stored_breaks)
            member_key <- paste0(ssp_i, "_", fp_label, "_", make.names(model_name))
            collection_policy$buffered_member_peak <- max(
              collection_policy$buffered_member_peak, 1L
            )
            if (is.function(weather_consumer)) {
              guard <- .wx_collection_rss_guard(collection_policy)
              if (isTRUE(guard$exceeded)) {
                collection_policy$rss_guard_activated <<- TRUE
                collection_policy$effective <<- "bounded"
                collection_policy$fallback_reason <<-
                  "observed_process_tree_rss_exceeded_budget"
              }
              emitted_order <<- emitted_order + 1L
              weather_consumer(
                member_key, model_df,
                list(
                  order = emitted_order, is_historical = FALSE,
                  ssp = ssp_i, period = fp_label,
                  collection = "bounded",
                  rss_bytes = guard$rss,
                  budget_exceeded = guard$exceeded,
                  buffered_members = 1L
                )
              )
              rm(model_df)
              gc(verbose = FALSE)
            } else {
              model_out[[member_key]] <- model_df
            }
          }
          model_out
        }
        if (!is.function(weather_consumer)) out <- c(out, period_out)
        if (is.function(weather_consumer) && length(period_out)) {
          invisible(lapply(names(period_out), function(key) {
            emitted_order <<- emitted_order + 1L
            weather_consumer(
              key, period_out[[key]],
              list(
                order = emitted_order, is_historical = FALSE,
                ssp = ssp_i, period = fp_label
              )
            )
          }))
        }

        # All returned frames are now detached from the query intermediates.
        .drop_period_tables(tmp_delta_name, tmp_perturb_name, tmp_roll_name)
        rm(
          perturbed, rolled_lazy,
          rolled, period_out
        )
        if (exists("batch", inherits = FALSE)) rm(batch)
        if (exists("model_list", inherits = FALSE)) rm(model_list)
        if (exists("model_names", inherits = FALSE)) rm(model_names)
        if (exists("model_out", inherits = FALSE)) rm(model_out)
        gc(verbose = FALSE)
      }

      try(DBI::dbRemoveTable(con, tmp_delta_all_name), silent = TRUE)
      tmp_tables <<- setdiff(tmp_tables, tmp_delta_all_name)

      # Cleanup all materialised delta temp tables (best-effort, early release;
      # the on.exit ledger still covers any that fail to drop here)
      for (tdn in tmp_delta_tables) {
        try(DBI::dbRemoveTable(dbplyr::remote_con(loc_deltas_by_model), tdn), silent = TRUE)
      }
      tmp_tables <<- setdiff(tmp_tables, tmp_delta_tables)
      out
    }

    # -- Run SSPs sequentially - DuckDB handles per-query parallelism --------
    result <- c(result, do.call(c, lapply(ssp, .process_ssp)))
  }

  # -- Cleanup base temp table (best-effort; on.exit ledger is the backstop) --
  try(
    DBI::dbRemoveTable(dbplyr::remote_con(loc_weather_base), tmp_base_name),
    silent = TRUE
  )
  tmp_tables <- setdiff(tmp_tables, tmp_base_name)

  # Attach computed breaks so the caller can reuse them in subsequent calls
  if (has_binning && !is.null(stored_breaks)) {
    attr(result, "stored_breaks") <- stored_breaks
  }

  # Attach the pre-binning values of the binned columns (descriptive use only)
  if (!is.null(continuous_hist)) {
    attr(result, "continuous_weather") <- continuous_hist
  }

  if (!is.null(collection_policy$external_rss_before)) {
    collection_policy$external_rss_after <- .wx_process_tree_rss_bytes()
  }
  attr(result, "weather_collection_policy") <- collection_policy

  result
}
