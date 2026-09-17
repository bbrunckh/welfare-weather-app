# Simulation orchestration ----
# Orchestration function for the full simulation pipeline.
# Pure function - no reactives. Called from mod_2_01_weathersim.R.
#
# Called by:
#   - mod_2_01_weathersim.R (observeEvent(input$run_sim))
#
# Depends on:
#   - fct_simulations.R  (run_sim_pipeline, compute_chol_vcov, format_elapsed)
#   - fct_get_weather.R  (get_weather)
#   - fct_aggregation.R  (compute_hist_agg, compute_scenario_agg)


# Run full simulation pipeline ----

.run_simulation_parallel_chunk <- function(jobs, pipeline_args, package_path = NULL) {
  if (!is.null(package_path) && !isNamespaceLoaded("wiseapp")) {
    pkgload::load_all(package_path, quiet = TRUE)
  }
  worker_pipeline <- get("run_sim_pipeline", envir = asNamespace("wiseapp"))
  lapply(jobs, function(job) {
    started <- proc.time()[["elapsed"]]
    tryCatch(
      c(list(index = job$index, key = job$key),
        list(out = do.call(worker_pipeline, c(list(weather_raw = job$weather), pipeline_args)),
          error = NULL, elapsed = proc.time()[["elapsed"]] - started)),
      error = function(e) list(index = job$index, key = job$key, out = NULL,
        error = conditionMessage(e), elapsed = proc.time()[["elapsed"]] - started)
    )
  })
}
environment(.run_simulation_parallel_chunk) <- baseenv()

# REACT-12: parse a future simulation key into its (SSP x period) group.
# Key format: "ssp2_4_5_2030_2040_ensemble_mean" ->
#   ssp_code "ssp2_4_5", yr_parts c(2030, 2040), gk "ssp2_4_5_2030_2040".
.key_group <- function(key) {
  ssp_code <- sub("^(ssp[^_]+_[^_]+_[^_]+)_.*", "\\1", key)
  yr_parts <- regmatches(key, gregexpr("[0-9]{4}", key))[[1L]]
  period <- if (length(yr_parts) >= 2L) {
    paste0(yr_parts[[1L]], "_", yr_parts[[2L]])
  } else {
    "unknown"
  }
  list(
    ssp_code = ssp_code,
    yr_parts = yr_parts,
    gk = paste0(ssp_code, "_", period)
  )
}

.step2_formula_vars <- function(x) {
  if (is.null(x) || !length(x)) {
    return(character(0))
  }
  fml <- tryCatch(
    {
      if (inherits(x, "formula")) {
        x
      } else {
        text <- paste(as.character(x), collapse = " ")
        if (!grepl("~", text, fixed = TRUE)) text <- paste("~", text)
        stats::as.formula(text)
      }
    },
    error = function(e) NULL
  )
  if (is.null(fml)) character(0) else all.vars(fml)
}

.step2_survey_projection <- function(svy, mf, sw, so, id_col = NULL,
                                     weight_cols = NULL) {
  join_keys <- c("code", "year", "survname", "loc_id", "int_month")
  full_frame <- function() {
    svy[, setdiff(names(svy), c(sw$name, so$name)), drop = FALSE] |>
      dplyr::mutate(year = as.character(year))
  }
  formula3 <- mf$formulas$formula3
  formula_vars <- .step2_formula_vars(formula3)
  fit_formula <- tryCatch(stats::formula(mf$fit3), error = function(e) NULL)
  fit_vars <- .step2_formula_vars(fit_formula)
  metadata_names <- c("weather_terms", "interaction_terms", "fe_terms")
  metadata_complete <- !is.null(formula3) && length(formula_vars) > 0L &&
    !is.null(fit_formula) && length(fit_vars) > 0L &&
    setequal(formula_vars, fit_vars) && all(metadata_names %in% names(mf)) &&
    !is.null(mf$weather_terms) && !is.null(mf$interaction_terms) &&
    !is.null(mf$fe_terms)
  if (!metadata_complete) {
    return(full_frame())
  }
  weather_vars <- unique(c(sw$name, mf$weather_terms))
  declared_vars <- unique(c(
    formula_vars,
    .step2_formula_vars(mf$interaction_terms), mf$fe_terms, mf$weather_terms
  ))
  required_svy <- setdiff(
    unique(c(join_keys, declared_vars, id_col, weight_cols)),
    c(weather_vars, so$name)
  )
  weather_complete <- length(mf$weather_terms) > 0L &&
    all(mf$weather_terms %in% sw$name) &&
    all(intersect(formula_vars, sw$name) %in% mf$weather_terms)
  if (!weather_complete || !all(required_svy %in% names(svy))) {
    return(full_frame())
  }
  required <- setdiff(
    unique(c(join_keys, declared_vars, id_col, weight_cols)),
    c(weather_vars, so$name)
  )
  keep <- names(svy)[names(svy) %in% required]
  svy[, keep, drop = FALSE] |>
    dplyr::mutate(year = as.character(year))
}

#' Prepare the reusable weather manifest for Step 2.
#'
#' This is deliberately independent of model fitting and prediction. The
#' returned frames are the canonical products emitted by `get_weather()` and
#' can be consumed by multiple calls to `fct_run_simulation()`.
prepare_weather_manifest <- function(
    survey_data, selected_surveys, selected_weather, dates, connection_params,
    ssp = NULL, future_period = NULL, perturbation_method = NULL,
    stored_breaks = NULL, epsilon = 0.001, weather_source = "era5land",
    proj_source = "cmip6", weather_collect = c("fast", "bounded"),
    weather_threads = c("auto", "1", "2"),
    prepared_weather_cache = c("auto", "off", "read_write"),
    prepared_weather_cache_root = NULL, weather_fn = get_weather) {
  weather_collect <- match.arg(weather_collect)
  weather_threads <- match.arg(weather_threads)
  prepared_weather_cache <- match.arg(prepared_weather_cache)
  fp_list <- future_period %||% list()
  ssps <- ssp %||% character(0)
  signature <- .step2_prepared_weather_cache_signature(
    selected_weather, selected_surveys, survey_data, connection_params, dates,
    fp_list, ssps, perturbation_method, stored_breaks, epsilon,
    weather_source, proj_source
  )
  cache <- if (.step2_prepared_weather_cache_enabled(
    prepared_weather_cache, default_loader = identical(weather_fn, get_weather)
  )) {
    .step2_prepared_weather_cache_create(signature, prepared_weather_cache_root)
  } else NULL
  if (!is.null(cache) && is.character(cache$stage) &&
    length(cache$stage) == 1L && nzchar(cache$stage)) {
    on.exit(
      if (is.character(cache$stage) && dir.exists(cache$stage)) {
        unlink(cache$stage, recursive = TRUE)
      },
      add = TRUE
    )
  }
  frames <- .step2_prepared_weather_cache_read(cache, signature)
  cache_hit <- !is.null(frames)
  if (!cache_hit && !is.null(cache) && is.null(cache$stage)) {
    .step2_prepared_weather_cache_open_stage(cache)
  }
  if (is.null(frames)) {
    frames <- list()
    weather_result <- tryCatch(
      weather_fn(
        survey_data = survey_data, selected_surveys = selected_surveys,
        selected_weather = selected_weather, dates = dates,
        connection_params = connection_params, ssp = if (length(ssps)) ssps else NULL,
        future_period = if (length(ssps)) fp_list else NULL,
        perturbation_method = perturbation_method, stored_breaks = stored_breaks,
        epsilon = epsilon, weather_source = weather_source, proj_source = proj_source,
        weather_collect = weather_collect, weather_threads = weather_threads,
        weather_consumer = function(key, weather, metadata = NULL) {
          frames[[key]] <<- weather
        }
      ),
      error = function(e) {
        if (length(frames)) stop(e)
        weather_fn(
          survey_data = survey_data, selected_surveys = selected_surveys,
          selected_weather = selected_weather, dates = dates,
          connection_params = connection_params, ssp = if (length(ssps)) ssps else NULL,
          future_period = if (length(ssps)) fp_list else NULL,
          perturbation_method = perturbation_method, stored_breaks = stored_breaks,
          epsilon = epsilon, weather_source = weather_source, proj_source = proj_source,
          weather_collect = weather_collect, weather_threads = weather_threads
        )
      }
    )
    if (is.list(weather_result) && length(weather_result)) {
      for (key in setdiff(names(weather_result), names(frames))) {
        frames[[key]] <- weather_result[[key]]
      }
    }
    if (length(frames)) {
      for (key in names(frames)) {
        .step2_prepared_weather_cache_put(cache, key, frames[[key]])
      }
      .step2_prepared_weather_cache_publish(cache, ssps, fp_list)
    }
  }
  structure(
    list(schema = 1L, signature = signature, frames = frames,
         cache_hit = cache_hit, cache_root = cache$root %||% NULL),
    class = "wiseapp_weather_manifest"
  )
}

#' Run the full welfare-weather simulation pipeline
#'
#' Pure function - no reactives. Extracts all business logic from
#' observeEvent(input\$run_sim) in mod_2_01_weathersim.R.
#'
#' @param sw               Data frame. Selected weather variables.
#' @param so               Data frame. Selected outcome (one row).
#' @param svy              Data frame. Baseline survey data.
#' @param ss               Data frame. Selected surveys.
#' @param mf               List. Model fit (fit3, engine, train_data).
#' @param cp               List. Connection parameters.
#' @param fp_list          List of character(2) vectors. Future period date ranges.
#' @param ssps             Character vector. Climate SSP codes.
#' @param residuals        Character. Residual method.
#' @param skip_coef_draws  Logical. If TRUE bypass Cholesky draws.
#' @param propagate_all_covariate_uncertainty Logical. When FALSE (default)
#'   and `residuals == "original"`, the additive-decomposition SE is applied:
#'   only coefficients on variables that change between baseline and
#'   counterfactual (weather, plus policy-modified variables in Module 3)
#'   contribute to `var_coef`. Coefficients on unchanged covariates cancel
#'   through the held-fixed residual term, so masking them is exact under
#'   additive separability. Set TRUE to recover the legacy full-coefficient
#'   propagation (more conservative but inconsistent with the model's own
#'   additive-separability assumption). Ignored when residuals are not
#'   `"original"` - the cancellation argument requires fixed-per-household
#'   residuals.
#' @param sim_dates        Character vector. Historical simulation dates.
#' @param perturbation_method List or NULL. Built by build_perturbation_method().
#' @param stored_breaks    Named list or NULL. Pre-computed histogram breaks.
#' @param payload_mode     "compact" (default) or "legacy" shared-context
#'   result payload.
#' @param weather_storage  "memory" (default) or "reference". Reference mode
#'   stores future member weather in a run-scoped signed RDS store and resolves
#'   it only at consumer boundaries.
#' @param weather_collect  "fast" (default) or "bounded" future-weather
#'   collection strategy passed to `get_weather()`.
#' @param weather_threads  DuckDB weather-query thread mode passed to
#'   `get_weather()`: `"auto"` (default), `"1"`, or `"2"`.
#' @param prepared_weather_cache  Cross-run prepared-weather cache mode:
#'   `"auto"` (default), `"off"`, or `"read_write"`.
#' @param prepared_weather_cache_root Optional cache root for prepared weather.
#' @param weather_manifest Prepared weather manifest returned by
#'   `prepare_weather_manifest()`. When supplied, weather loading is skipped.
#' @param epsilon          CMIP6 perturbation epsilon passed to `get_weather()`.
#' @param weather_source   Historical weather source passed to `get_weather()`.
#' @param proj_source      Projection source passed to `get_weather()`.
#' @param join_cache       Logical. Use the experimental survey-side join
#'   cache. Defaults to FALSE until full-scale benchmarks establish a win.
#' @param key_workers      Integer. Opt-in number of multisession workers for
#'   future keys. Historical execution remains serial and parent-side result
#'   assembly preserves canonical key order. Values are limited to 1 or 2.
#' @param direct_rif_predictions Logical. Use direct RIF prediction with
#'   automatic fallback for unsupported model structures. Defaults to TRUE.
#' @param notify_fn   Function(msg). Called for user-facing notifications.
#'   Default is message() to console only.
#' @param progress_fn      Function(value, detail). Called to update progress.
#'   Default is a no-op - Shiny passes shiny::setProgress here.
#' @param weather_fn       Function. Weather loader, injectable for tests.
#'   Default [get_weather].
#' @param pipeline_fn      Function. Per-key simulation pipeline, injectable
#'   for tests. Default [run_sim_pipeline].
#'
#' @return Named list with elements:
#'   \describe{
#'     \item{hist_sim_result}{List. Historical simulation output.}
#'     \item{new_scenarios}{Named list. Future scenario outputs; each entry
#'       carries `n_models` (succeeded) and `n_models_requested` (REACT-12
#'       provenance).}
#'     \item{chol_obj}{List or NULL. Cholesky VCV object.}
#'     \item{n_keys}{Integer. Total number of simulation keys.}
#'     \item{total_runs}{Integer. Total prediction runs.}
#'     \item{t_elapsed}{Numeric. Wall-clock seconds elapsed.}
#'     \item{failures}{List. REACT-12 failure ledger - one entry per failed
#'       key with `key`, `gk`, `is_hist`, `error`. Empty when all keys ran.}
#'   }
#'
#' @details
#' REACT-12: per-key pipeline failures are collected in a ledger instead of
#' vanishing. The run fails (throws) when the historical key fails or when
#' *all* ensemble members of a requested (SSP x period) group fail - in that
#' case no partial results are published and the caller keeps its previous
#' state. Partial member failures publish results with the failure ledger
#' attached for the caller to surface.
#' @noRd
fct_run_simulation <- function(sw,
                               so,
                               svy,
                               ss,
                               mf,
                               cp,
                               fp_list,
                               ssps,
                               residuals,
                               skip_coef_draws,
                               sim_dates,
                               perturbation_method,
                               stored_breaks,
                               propagate_all_covariate_uncertainty = FALSE,
                               fit_multi = NULL,
                               taus = NULL,
                               weather_cols = NULL,
                               payload_mode = c("compact", "legacy"),
                               weather_storage = c("memory", "reference"),
                               weather_store_root = NULL,
                                weather_collect = c("fast", "bounded"),
                                weather_threads = c("auto", "1", "2"),
                                prepared_weather_cache = c("auto", "off", "read_write"),
                                prepared_weather_cache_root = NULL,
                                epsilon = 0.001,
                                weather_source = "era5land",
                                 proj_source = "cmip6",
                                 weather_manifest = NULL,
                                 join_cache = FALSE,
                                key_workers = 1L,
                                direct_rif_predictions = TRUE,
                               seed = WISEAPP_DEFAULT_SEED,
                               notify_fn = function(msg) message(msg),
                               progress_fn = function(value, detail) invisible(NULL),
                                weather_fn = get_weather,
                                pipeline_fn = run_sim_pipeline) {
  memory_profile <- if (identical(tolower(Sys.getenv("WISEAPP_MEMORY_PROFILE", "")), "1")) {
    new.env(parent = emptyenv())
  } else {
    NULL
  }
  if (!is.null(memory_profile)) {
    memory_profile$records <- list()
    memory_profile$started <- proc.time()[["elapsed"]]
  }
  profile_memory <- function(stage, value = NULL, detail = NULL,
                             serialize_value = TRUE) {
    if (is.null(memory_profile)) return(invisible(NULL))
    rss <- if (exists(".wx_process_tree_rss_bytes", mode = "function")) {
      .wx_process_tree_rss_bytes()
    } else {
      NA_real_
    }
    memory_profile$records[[length(memory_profile$records) + 1L]] <- data.frame(
      stage = stage,
      elapsed_seconds = proc.time()[["elapsed"]] - memory_profile$started,
      object_bytes = if (is.null(value)) NA_real_ else as.numeric(utils::object.size(value)),
      serialized_bytes = if (is.null(value) || !isTRUE(serialize_value)) {
        NA_real_
      } else {
        length(serialize(value, NULL, version = 3L))
      },
      rss_bytes = rss,
      detail = detail %||% "",
      stringsAsFactors = FALSE
    )
    invisible(NULL)
  }
  model <- mf$fit3
  engine <- mf$engine
  train_data <- mf$train_data
  weather_terms <- mf$weather_terms
  payload_mode <- match.arg(payload_mode)
  weather_storage <- match.arg(weather_storage)
  weather_collect <- match.arg(weather_collect)
  weather_threads <- match.arg(weather_threads)
  prepared_weather_cache <- match.arg(prepared_weather_cache)
  key_workers <- as.integer(key_workers)[1L]
  if (is.na(key_workers) || key_workers < 1L || key_workers > 2L) {
    stop("key_workers must be an integer between 1 and 2.", call. = FALSE)
  }
  seed <- as.integer(seed)[1L]
  if (is.na(seed)) seed <- WISEAPP_DEFAULT_SEED
  withr::local_seed(seed)
  has_future <- length(fp_list) > 0 && length(ssps) > 0
  weather_store <- NULL
  weather_store_published <- FALSE
  if (identical(weather_storage, "reference") && isTRUE(has_future)) {
    run_id <- paste0(
      format(Sys.time(), "%Y%m%dT%H%M%OS3"), "-",
      substr(digest::digest(list(Sys.getpid(), Sys.time())), 1L, 12L)
    )
    weather_store <- step2_weather_store_create(
      run_id = run_id,
      signature = digest::digest(list(
        ss, fp_list, ssps, sim_dates,
        perturbation_method, weather_terms
      )),
      root = weather_store_root
    )
    on.exit(
      if (!weather_store_published) step2_weather_store_cleanup(weather_store),
      add = TRUE
    )
  }

  ssp_labels <- c(
    "ssp2_4_5" = "SSP2-4.5",
    "ssp3_7_0" = "SSP3-7.0",
    "ssp5_8_5" = "SSP5-8.5"
  )

  # Total elapsed timer - starts here, covers everything ----
  t_start_total <- proc.time()[["elapsed"]]

  # Weather loading ----
  progress_fn(0.05, "Loading climate data...")
  t_weather_start <- proc.time()[["elapsed"]]

  weather_result <- NULL

  # Cholesky VCV ----
  chol_obj <- if (isTRUE(skip_coef_draws)) {
    message("[wiseapp] Coefficient draws skipped (point estimates only)")
    NULL
  } else {
    tryCatch(
      compute_chol_vcov(fit = model, vcov_spec = COEF_VCOV_SPEC),
      error = function(e) {
        warning(
          "[fct_run_simulation] compute_chol_vcov() failed - ",
          "falling back to point estimates: ", conditionMessage(e)
        )
        NULL
      }
    )
  }

  # Active-coefficient mask (additive-decomposition SE) ----
  # Under residuals = "original" the residual is held fixed per household, so
  # uncertainty on coefficients for variables that do not change between
  # baseline and counterfactual cancels through the residual. In Module 2
  # only weather variables change, so active = weather_terms. (Module 3
  # re-builds the mask in resimulate_with_svy() with policy-modified vars
  # added.) Skipped when the user has requested full propagation or when
  # residuals are not "original".
  #
  # svy_reference = svy here: in Module 2 the baseline survey is the
  # reference because weather substitution happens *inside* the pipeline
  # (prepare_hist_weather), not on `svy` itself, so diffing svy against
  # itself yields empty modifications and active_terms = weather_terms.
  chol_obj <- attach_active_mask(
    chol_obj                            = chol_obj,
    svy_modified                        = svy,
    svy_reference                       = svy,
    train_data                          = train_data,
    weather_terms                       = weather_terms,
    outcome_col                         = so$name,
    residuals                           = residuals,
    propagate_all_covariate_uncertainty = propagate_all_covariate_uncertainty
  )

  # Cluster counts ----
  cluster_counts <- tryCatch(
    compute_cluster_counts(train_data),
    error = function(e) NULL
  )
  # Key loop setup ----

  weight_col_sim <- grep("^weight$|^hhweight$|^wgt$|^pw$",
    names(svy),
    value = TRUE, ignore.case = TRUE
  )[1L]
  if (is.na(weight_col_sim %||% NA)) weight_col_sim <- NULL
  wt_detected <- grep("^weight$|^hhweight$|^wgt$|^pw$",
    names(svy),
    value = TRUE, ignore.case = TRUE
  )
  if (length(wt_detected) > 1L) {
    warning(sprintf(
      "[wiseapp] Multiple weight columns detected: %s. Using '%s'.",
      paste(wt_detected, collapse = ", "), weight_col_sim
    ))
  }

  # Serial execution only - parallelisation removed
  n_workers_safe <- 1L

  # Precompute objects shared across all keys ----

  is_rif <- identical(engine, "rif")

  # train_aug: identical for every key (same model, same train_data). Compute
  # once here instead of repeating predict(model, train_data) per key.
  precomputed_train_aug <- if (is_rif) {
    NULL
  } else {
    tryCatch(
      {
        fitted_train <- as.numeric(stats::predict(model, newdata = train_data))
        train_data |>
          dplyr::mutate(
            .fitted = fitted_train,
            .resid  = !!rlang::sym(so$name) - fitted_train
          )
      },
      error = function(e) {
        warning(
          "[fct_run_simulation] train_aug precomputation failed: ",
          conditionMessage(e)
        )
        NULL
      }
    )
  }
  shared_id_col <- if (identical(residuals, "original")) {
    resolve_id_col(train_data, svy)
  } else {
    NULL
  }

  # ecdf_train: RIF-only analogue of the above - train_data[[outcome]] is
  # identical for every key, so the ecdf used to assign each household's
  # quantile position is built once here rather than per key inside
  # predict_rif() (see PERF-27).
  precomputed_ecdf_train <- if (is_rif) {
    tryCatch(
      {
        stats::ecdf(train_data[[so$name]])
      },
      error = function(e) {
        warning(
          "[fct_run_simulation] ecdf_train precomputation failed: ",
          conditionMessage(e)
        )
        NULL
      }
    )
  } else {
    NULL
  }
  direct_rif_metadata <- if (is_rif && isTRUE(direct_rif_predictions)) {
    tryCatch(build_direct_rif_metadata(fit_multi), error = function(e) NULL)
  } else {
    NULL
  }
  direct_rif_baseline_cache <- if (is_rif && isTRUE(direct_rif_predictions)) {
    new.env(parent = emptyenv())
  } else {
    NULL
  }

  # Project the survey before the weather expansion. The full baseline remains
  # retained separately in hist_sim_result$svy for Step 3 policy consumers.
  svy_prepared <- .step2_survey_projection(
    svy, mf, sw, so,
    id_col = shared_id_col, weight_cols = wt_detected
  )
  profile_memory("survey_prepared", svy_prepared)
  weather_join_cache <- if (isTRUE(join_cache) &&
    all(c(
      "code", "year", "survname", "loc_id",
      "int_month"
    ) %in% names(svy_prepared))) {
    build_weather_join_cache(
      dplyr::mutate(svy_prepared, .svy_row_id = seq_len(nrow(svy_prepared)))
    )
  } else {
    NULL
  }

  hist_sim_result <- NULL
  new_scenarios <- list()
  group_agg <- list()
  group_weather_rep <- list()
  group_weather_shared <- list()
  group_meta <- list()
  group_n <- list()
  group_requested <- list()
  failures <- list()

  # Run pipelines (one key at a time) ----
  t_start <- t_start_total # key loop elapsed = total elapsed from function entry


  t_start_pipeline <- proc.time()[["elapsed"]]
  message("[wiseapp] Running simulation pipelines...")
  progress_fn(0.15, "Preparing climate scenarios...")

  weather_refs <- list()
  emitted_keys <- character(0)
  n_keys <- 0L
  n_hist_yrs <- 30L
  consume_key <- function(key, weather_input, metadata = NULL,
                          out_override = NULL, key_err_override = NULL,
                          out_supplied = FALSE) {
    emitted_keys <<- c(emitted_keys, key)
    n_keys <<- n_keys + 1L
    is_hist <- identical(key, "historical")
    key_group <- if (is_hist) NULL else .key_group(key)
    if (!is.null(key_group)) {
      gk0 <- key_group$gk
      group_requested[[gk0]] <<- (group_requested[[gk0]] %||% 0L) + 1L
      if (is.null(group_meta[[gk0]])) {
        group_meta[[gk0]] <<- list(
          ssp_code = key_group$ssp_code,
          year_range = key_group$yr_parts
        )
      }
    }
    key_err <- key_err_override
    out <- if (isTRUE(out_supplied) || !is.null(key_err_override)) {
      if (!is.null(key_err)) {
        warning(sprintf("[fct_run_simulation] Key %s failed: %s", key, key_err))
      }
      out_override
    } else tryCatch(
      pipeline_fn(
        weather_raw = weather_input, svy = svy, sw = sw, so = so,
        model = model, residuals = residuals, train_data = train_data,
        engine = engine, chol_obj = chol_obj, fit_multi = fit_multi,
        taus = taus, weather_cols = weather_cols,
        precomputed_train_aug = precomputed_train_aug,
        svy_prepared = svy_prepared, weather_join_cache = weather_join_cache,
        precomputed_ecdf_train = precomputed_ecdf_train,
        direct_rif_predictions = direct_rif_predictions,
        direct_rif_metadata = direct_rif_metadata,
        direct_rif_baseline_cache = direct_rif_baseline_cache
      ),
      error = function(e) {
        key_err <<- conditionMessage(e)
        warning(sprintf("[fct_run_simulation] Key %s failed: %s", key, key_err))
        NULL
      }
    )
    if (is.null(out)) {
      failures[[length(failures) + 1L]] <<- list(
        key = key, gk = if (is.null(key_group)) NA_character_ else key_group$gk,
        is_hist = is_hist, error = key_err
      )
      return(invisible(NULL))
    }
    if (identical(weather_storage, "reference") && !is_hist) {
      weather_refs[[key]] <<- step2_weather_store_put(weather_store, key, weather_input)
    }
    if (is_hist) {
      n_hist_yrs <<- length(unique(format(weather_input$timestamp, "%Y")))
      hist_sim_result <<- list(
        pipeline = out, chol_obj = chol_obj, so = so,
        has_weights = !is.null(out$weight), weather_raw = weather_input,
        train_data = train_data, cluster_counts = cluster_counts, svy = svy,
        residuals = residuals
      )
      profile_memory("historical_pipeline", hist_sim_result$pipeline, detail = key)
      out$weather_raw <- NULL
    } else {
      gk <- key_group$gk
      if (is.null(group_agg[[gk]])) group_agg[[gk]] <<- list()
      if (is.null(group_weather_rep[[gk]])) {
        group_weather_rep[[gk]] <<- if (identical(weather_storage, "reference")) {
          weather_refs[[key]]
        } else {
          out$weather_raw
        }
      }
      if (is.null(group_n[[gk]])) group_n[[gk]] <<- 0L
      member_type <- sub(".*_(ensemble_mean|ensemble_lo|ensemble_hi)$", "\\1", key)
      if (!nchar(member_type) || member_type == key) {
        member_type <- paste0("model_", group_n[[gk]] + 1L)
      }
      if (identical(weather_storage, "reference")) out$weather_raw <- weather_refs[[key]]
      group_agg[[gk]][[member_type]] <<- out
      profile_memory("future_pipeline", out, detail = key)
      group_n[[gk]] <<- group_n[[gk]] + 1L
    }
    invisible(NULL)
  }

  manifest <- weather_manifest %||% prepare_weather_manifest(
    survey_data = svy, selected_surveys = ss, selected_weather = sw,
    dates = sim_dates, connection_params = cp,
    ssp = if (has_future) ssps else NULL,
    future_period = if (has_future) fp_list else NULL,
    perturbation_method = perturbation_method, stored_breaks = stored_breaks,
    epsilon = epsilon, weather_source = weather_source, proj_source = proj_source,
    weather_collect = weather_collect, weather_threads = weather_threads,
    prepared_weather_cache = prepared_weather_cache,
    prepared_weather_cache_root = prepared_weather_cache_root,
    weather_fn = weather_fn
  )
  profile_memory(
    "weather_manifest", manifest,
    detail = if (isTRUE(manifest$cache_hit)) "cache_hit" else "built"
  )
  if (!inherits(manifest, "wiseapp_weather_manifest")) {
    stop("weather_manifest must be created by prepare_weather_manifest().", call. = FALSE)
  }
  cached_weather <- manifest$frames
  profile_memory("cached_weather", cached_weather)
  if (isTRUE(manifest$cache_hit)) message("[wiseapp] Reusing prepared weather cache")
  weather_result <- list()
  cached_keys <- names(cached_weather)
  hist_keys <- intersect("historical", cached_keys)
  future_keys <- setdiff(cached_keys, hist_keys)
  for (key in hist_keys) consume_key(key, cached_weather[[key]])
  if (length(future_keys) && key_workers > 1L) {
    future_jobs <- lapply(seq_along(future_keys), function(i) {
      list(index = i, key = future_keys[[i]], weather = cached_weather[[future_keys[[i]]]])
    })
    future_chunks <- lapply(seq_len(min(key_workers, length(future_jobs))), function(worker) {
      future_jobs[seq.int(worker, length(future_jobs), by = key_workers)]
    })
    cluster <- parallel::makeCluster(length(future_chunks), type = "PSOCK")
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    if (!identical(pipeline_fn, run_sim_pipeline)) {
      stop("key_workers > 1 requires the default run_sim_pipeline().", call. = FALSE)
    }
    pipeline_args <- list(
      svy = svy, sw = sw, so = so, model = model, residuals = residuals,
      train_data = train_data, engine = engine, chol_obj = chol_obj,
      fit_multi = fit_multi, taus = taus, weather_cols = weather_cols,
      precomputed_train_aug = precomputed_train_aug, svy_prepared = svy_prepared,
      weather_join_cache = weather_join_cache,
      precomputed_ecdf_train = precomputed_ecdf_train,
      direct_rif_predictions = direct_rif_predictions,
      direct_rif_metadata = direct_rif_metadata,
      direct_rif_baseline_cache = NULL
    )
    package_path <- getNamespaceInfo(asNamespace("wiseapp"), "path")
    future_results <- parallel::clusterApply(
      cluster, future_chunks, .run_simulation_parallel_chunk,
      pipeline_args = pipeline_args, package_path = package_path
    )
    future_results <- do.call(c, future_results)
    future_results <- future_results[order(vapply(future_results, `[[`, integer(1), "index"))]
    for (item in future_results) {
      key <- item$key
      consume_key(key, cached_weather[[key]], out_override = item$out,
        key_err_override = item$error %||% NULL, out_supplied = TRUE)
    }
  } else {
    for (key in future_keys) consume_key(key, cached_weather[[key]])
  }
  t_weather <- proc.time()[["elapsed"]] - t_weather_start
  progress_fn(0.35, "Climate data loaded. Running scenarios...")
  if (is.list(weather_result) && length(weather_result)) {
    for (key in setdiff(names(weather_result), emitted_keys)) {
      consume_key(key, weather_result[[key]])
    }
  }
  all_keys <- emitted_keys
  if (is.list(weather_result)) {
    all_keys <- unique(c(all_keys, setdiff(names(weather_result), emitted_keys)))
  }
  n_future_keys <- sum(all_keys != "historical")
  total_runs <- n_hist_yrs * (1L + n_future_keys)

  compact_train_aug <- .compact_residual_context(
    precomputed_train_aug, shared_id_col, residuals,
    compact = identical(payload_mode, "compact")
  )
  has_weather_references <- identical(weather_storage, "reference") &&
    length(weather_refs) > 0L
  rm(
    weather_result, weather_refs, precomputed_train_aug,
    svy_prepared, weather_join_cache, direct_rif_metadata,
    direct_rif_baseline_cache
  )
  gc(verbose = FALSE)

  t_pipeline_done <- proc.time()[["elapsed"]] - t_start_pipeline
  progress_fn(0.80, "Finalizing scenario results...")

  # REACT-12: classify failures - fail fast or publish with ledger ----
  # The run is unusable when the historical key failed (no baseline to show)
  # or when every requested member of a group failed (that scenario would
  # silently vanish from the results charts). In those cases throw so the
  # caller keeps its previous results and shows an error. Partial member
  # failures continue: the ledger travels with the result for the caller to
  # surface as a prominent warning.
  if (length(failures) > 0L) {
    fail_lines <- vapply(failures, function(f) {
      sprintf("  - %s: %s", f$key, f$error)
    }, character(1))

    if (any(vapply(failures, `[[`, logical(1), "is_hist"))) {
      stop("Historical simulation failed - no results published.\n",
        paste(fail_lines, collapse = "\n"),
        call. = FALSE
      )
    }

    dead_gks <- setdiff(names(group_requested), names(group_agg))
    if (length(dead_gks) > 0L) {
      dead_lbl <- vapply(dead_gks, function(gk) {
        meta <- group_meta[[gk]]
        if (is.null(meta)) {
          return(gk)
        }
        pretty <- ssp_labels[meta$ssp_code] %||% meta$ssp_code
        yr <- meta$year_range
        paste0(
          pretty, " / ",
          if (length(yr) >= 2L) paste0(yr[1], "-", yr[2]) else "unknown"
        )
      }, character(1))
      stop("All ensemble members failed for: ",
        paste(dead_lbl, collapse = ", "),
        " - no results published.\n",
        paste(fail_lines, collapse = "\n"),
        call. = FALSE
      )
    }
  }


  # Assemble new_scenarios ----
  for (gk in names(group_agg)) {
    if (identical(weather_storage, "memory")) {
      shared_members <- step2_weather_share_members(
        lapply(group_agg[[gk]], `[[`, "weather_raw")
      )
      if (!is.null(shared_members$shared)) {
        group_weather_shared[[gk]] <- shared_members$shared
        member_names <- names(group_agg[[gk]]) %||%
          paste0("model_", seq_along(group_agg[[gk]]))
        for (mi in seq_along(member_names)) {
          group_agg[[gk]][[member_names[[mi]]]]$weather_raw <-
            shared_members$members[[mi]]
        }
        group_weather_rep[[gk]] <- shared_members$members[[1L]]
      }
    }
    meta <- group_meta[[gk]]
    ssp_pretty <- ssp_labels[meta$ssp_code] %||% meta$ssp_code
    period_lbl <- paste0(meta$year_range[1], "-", meta$year_range[2])
    display_key <- paste0(ssp_pretty, " / ", period_lbl)
    new_scenarios[[display_key]] <- list(
      pipelines = group_agg[[gk]],
      weather_raw = group_weather_rep[[gk]],
      chol_obj = chol_obj,
      so = so,
      year_range = meta$year_range,
      n_models = group_n[[gk]],
      # REACT-12 provenance: how many members were requested vs succeeded.
      n_models_requested = group_requested[[gk]] %||% group_n[[gk]],
      residuals = residuals
    )
    if (!is.null(group_weather_shared[[gk]])) {
      new_scenarios[[display_key]]$weather_shared <- group_weather_shared[[gk]]
    }
    if (identical(weather_storage, "reference")) {
      new_scenarios[[display_key]]$weather_store <- weather_store
      new_scenarios[[display_key]]$weather_signature <- weather_store$signature
    }
    profile_memory(
      "scenario_group_staging",
      list(
        pipelines = group_agg[[gk]],
        weather_raw = group_weather_rep[[gk]],
        weather_shared = group_weather_shared[[gk]]
      ),
      detail = display_key,
      serialize_value = FALSE
    )
    # The scenario now owns these members; release the staging containers
    # before assembling the next SSP/period group.
    group_agg[[gk]] <- NULL
    group_weather_rep[[gk]] <- NULL
    group_weather_shared[[gk]] <- NULL
  }
  profile_memory("pre_publish_context", list(
    hist_sim_result = hist_sim_result,
    new_scenarios = new_scenarios,
    group_agg = group_agg,
    group_weather_rep = group_weather_rep,
    group_weather_shared = group_weather_shared,
    compact_train_aug = compact_train_aug
  ), serialize_value = FALSE)
  rm(group_agg, group_weather_rep, group_weather_shared, group_meta, group_n)
  gc(verbose = FALSE)

  t_elapsed_total <- proc.time()[["elapsed"]] - t_start_total
  t_pipeline_elapsed <- proc.time()[["elapsed"]] - t_start_pipeline

  n_failed <- length(failures)
  message(sprintf(
    "[wiseapp] Simulation complete in %s total | weather: %s | pipelines: %s | %d key(s) | ~%d runs%s",
    format_elapsed(t_elapsed_total),
    format_elapsed(t_weather),
    format_elapsed(t_pipeline_elapsed),
    n_keys,
    total_runs,
    if (n_failed > 0L) sprintf(" | %d key(s) FAILED", n_failed) else ""
  ))

  result <- list(
    hist_sim_result = hist_sim_result,
    new_scenarios   = new_scenarios,
    chol_obj        = chol_obj,
    n_keys          = n_keys,
    total_runs      = total_runs,
    t_elapsed       = t_elapsed_total,
    t_weather       = t_weather, # <- expose for UI notification
    failures        = failures, # <- REACT-12 failure ledger
    n_keys_ok       = n_keys - n_failed
  )
  if (identical(weather_storage, "reference")) {
    result$weather_storage <- weather_storage
    result$weather_store <- weather_store
  }

  if (identical(payload_mode, "compact")) {
    result <- compact_step2_result(
      result = result,
      train_aug = compact_train_aug,
      id_col = shared_id_col,
      residuals = residuals,
      chol_obj = chol_obj,
      so = so,
      train_data = train_data,
      model_metadata = list(
        engine = engine,
        weather_terms = weather_terms,
        fit_multi = !is.null(fit_multi),
        taus = taus
      )
    )
  }
  if (!is.null(memory_profile)) {
    profile_memory("final_result", result)
    attr(result, "memory_profile") <- do.call(rbind, memory_profile$records)
  }
  if (has_weather_references) {
    result$weather_store_lease <- step2_weather_store_acquire(weather_store)
  } else if (identical(weather_storage, "reference") && !is.null(weather_store)) {
    # A total future-group failure has no published consumer. Do not retain an
    # empty run store merely because the requested configuration had a future
    # period.
    step2_weather_store_cleanup(weather_store)
  }
  weather_store_published <- has_weather_references
  result
}
