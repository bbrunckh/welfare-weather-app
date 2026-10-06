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

.STEP2_SSP_LABELS <- c(
  "ssp2_4_5" = "SSP2-4.5",
  "ssp3_7_0" = "SSP3-7.0",
  "ssp5_8_5" = "SSP5-8.5"
)

# Display key of a future scenario group, e.g. "SSP2-4.5 / 2030-2040". Shared
# by the new_scenarios assembly and the group_completed progress event.
.step2_scenario_display_key <- function(ssp_code, year_range) {
  ssp_pretty <- .STEP2_SSP_LABELS[ssp_code] %||% ssp_code
  paste0(ssp_pretty, " / ", year_range[1], "-", year_range[2])
}

# Results-tab display settings the streamed partial tables are computed for.
# Falls back like the module: unsupported method -> "mean"; poverty line from
# the outcome metadata (as .results_content_ui()), else 3.00; bandwidth 0.05.
.step2_resolve_display <- function(display, so, residuals, skip_coef_draws) {
  methods <- unname(hist_aggregate_choices(so$type, so$name))
  method <- as.character(display$method %||% "mean")[1L]
  if (!length(methods) || !method %in% methods) method <- "mean"
  finite_pos <- function(x) {
    is.numeric(x) && length(x) >= 1L && is.finite(x[[1L]]) && x[[1L]] > 0
  }
  pov <- if (finite_pos(display$pov_line)) {
    display$pov_line[[1L]]
  } else if (finite_pos(so[["povline"]])) {
    so[["povline"]][[1L]]
  } else {
    3
  }
  bw <- if (finite_pos(display$bandwidth_p0)) display$bandwidth_p0[[1L]] else 0.05
  list(
    method = method, pov_line = as.numeric(pov), bandwidth_p0 = as.numeric(bw),
    residuals = residuals, skip_coef = isTRUE(skip_coef_draws),
    is_log = isTRUE(so$transform == "log")
  )
}

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
    prepared_weather_cache = c("off", "auto", "read_write"),
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
#' @param progress_fn      Function(value, detail). Called to update progress.
#'   Default is a no-op - Shiny passes shiny::setProgress here.
#' @param weather_fn       Function. Weather loader, injectable for tests.
#'   Default [get_weather].
#' @param pipeline_fn      Function. Per-key simulation pipeline, injectable
#'   for tests. Default [run_sim_pipeline].
#' @param preview_fn       Optional function receiving a bounded historical
#'   mean summary after historical prediction and before future weather work.
#' @param partial_fn       Optional function receiving one display-ready
#'   aggregated table (schema 1 list) after the historical key and after each
#'   completed (SSP x period) group. Errors are swallowed except conditions
#'   inheriting `wiseapp_step2_cancelled`.
#' @param display          Optional list(method, pov_line, bandwidth_p0) of
#'   the Results tab settings the partial tables are computed for. Defaults:
#'   "mean", `so$povline` (else 3), 0.05.
#' @param checkpoint_fn    Optional cooperative checkpoint function receiving
#'   a fixed-stage/member context. Throw a condition inheriting
#'   `wiseapp_step2_cancelled` to cancel the run.
#'
#' @return Named list with elements:
#'   \describe{
#'     \item{hist_sim_result}{List. Historical simulation output.}
#'     \item{new_scenarios}{Named list. Future scenario outputs; each entry
#'       carries `n_models` (succeeded) and `n_models_requested` (REACT-12
#'       provenance).}
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
                                prepared_weather_cache = c("off", "auto", "read_write"),
                                prepared_weather_cache_root = NULL,
                                epsilon = 0.001,
                                weather_source = "era5land",
                                 proj_source = "cmip6",
                                 weather_manifest = NULL,
                                 join_cache = FALSE,
                                key_workers = 1L,
                                direct_rif_predictions = TRUE,
                                seed = WISEAPP_DEFAULT_SEED,
                                progress_fn = function(value, detail) invisible(NULL),
                                weather_fn = get_weather,
                                pipeline_fn = run_sim_pipeline,
                                preview_fn = NULL,
                                partial_fn = NULL,
                                display = NULL,
                                checkpoint_fn = NULL) {
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
    rss <- .wx_process_tree_rss_bytes()
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

  ssp_labels <- .STEP2_SSP_LABELS
  groups_total <- if (has_future) length(ssps) * length(fp_list) else 0L

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

  # Precompute objects shared across all keys ----

  is_rif <- identical(engine, "rif")

  # train_aug: identical for every key (same model, same train_data). Compute
  # once here instead of repeating predict(model, train_data) per key.
  precomputed_train_aug <- if (is_rif) {
    NULL
  } else {
    tryCatch(
      {
        fitted_values <- if (inherits(model, "fixest")) {
          model$fitted.values %||% numeric(0)
        } else {
          numeric(0)
        }
        fitted_train <- if (length(fitted_values) == nrow(train_data) &&
          all(is.finite(as.numeric(fitted_values)))) {
          # train_data is the complete-case frame used for fitting. Reuse the
          # model's stored fitted values instead of rebuilding a fixest model
          # matrix, which is slower and can fail for slimmed/serialized fits.
          as.numeric(model$fitted.values)
        } else {
          tryCatch(
            stats::predict(model, newdata = as.data.frame(train_data)),
            error = function(e) NULL
          )
        }
        if (is.null(fitted_train) || length(fitted_train) != nrow(train_data)) {
          NULL
        } else {
          fitted_train <- as.numeric(fitted_train)
          train_data |>
            dplyr::mutate(
              .fitted = fitted_train,
              .resid  = !!rlang::sym(so$name) - fitted_train
            )
        }
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
  n_pipeline_completed <- 0L
  n_pipeline_failed <- 0L

  # Group progress state: every checkpoint event carries the cumulative
  # groups_done/groups_total so the latest record is always sufficient.
  groups_done <- 0L
  groups_completed <- character(0)
  open_gk <- NULL
  current_member <- list()

  checkpoint <- function(stage, member_ordinal = 0L, extra = NULL) {
    if (!is.function(checkpoint_fn)) return(invisible(NULL))
    event <- list(
      stage = stage,
      member_ordinal = as.integer(member_ordinal),
      completed = n_pipeline_completed,
      failed = n_pipeline_failed,
      requested = NA_integer_,
      groups_done = as.integer(groups_done),
      groups_total = as.integer(groups_total)
    )
    if (is.null(extra)) extra <- current_member
    if (length(extra)) event[names(extra)] <- extra
    tryCatch(
      checkpoint_fn(event),
      error = function(e) {
        if (inherits(e, "wiseapp_step2_cancelled")) stop(e)
        invisible(NULL)
      }
    )
    invisible(NULL)
  }

  # Run pipelines (one key at a time) ----
  t_start <- t_start_total # key loop elapsed = total elapsed from function entry


  t_start_pipeline <- proc.time()[["elapsed"]]
  message("[wiseapp] Running simulation pipelines...")
  progress_fn(0.15, "Preparing climate scenarios...")

  # Shared display context, built once after the historical pipeline. The same
  # object the preview and compact_step2_result() use, so partial tables see
  # exactly the inputs the Results module sees later.
  shared_ctx <- NULL
  partial_display <- .step2_resolve_display(display, so, residuals, skip_coef_draws)

  # Phase 3: compute the displayed table with the module's helper and hand it
  # to partial_fn. Never fails the run, except for cancellation.
  emit_partial <- function(kind, label, ordinal, pipes, n_models,
                           n_models_requested, historical = FALSE) {
    if (!is.function(partial_fn) || is.null(shared_ctx)) return(invisible(NULL))
    tryCatch({
      agg <- step2_display_aggregation_suite(
        pipelines = pipes,
        method = partial_display$method,
        so = so,
        residuals = residuals,
        skip_coef = isTRUE(skip_coef_draws),
        pov_line = partial_display$pov_line,
        bandwidth_p0 = partial_display$bandwidth_p0,
        shared_context = shared_ctx,
        historical = historical
      )
      shown <- agg$tables[[partial_display$method]]
      if (is.null(shown)) return(invisible(NULL))
      partial_fn(list(
        schema = 1L,
        kind = kind,
        label = label,
        ordinal = as.integer(ordinal),
        groups_total = as.integer(groups_total),
        n_models = as.integer(n_models),
        n_models_requested = as.integer(n_models_requested),
        display = c(partial_display, list(
          weight_key = agg$weight_key,
          methods = names(agg$tables)
        )),
        so = so,
        has_weights = agg$weighted,
        has_draws = !is.null(chol_obj),
        table = shown,
        tables = agg$tables
      ))
    }, error = function(e) {
      if (inherits(e, "wiseapp_step2_cancelled")) stop(e)
      invisible(NULL)
    })
    invisible(NULL)
  }

  # A group is complete when its last member arrives (metadata), when a key of
  # another group arrives, or when the weather loop ends. Exactly once each.
  on_group_complete <- function(gk) {
    if (is.null(gk) || gk %in% groups_completed) return(invisible(NULL))
    groups_completed <<- c(groups_completed, gk)
    groups_done <<- groups_done + 1L
    if (identical(open_gk, gk)) open_gk <<- NULL
    meta <- group_meta[[gk]]
    label <- if (is.null(meta)) gk else {
      .step2_scenario_display_key(meta$ssp_code, meta$year_range)
    }
    n_ok <- as.integer(group_n[[gk]] %||% 0L)
    n_req <- as.integer(group_requested[[gk]] %||% 0L)
    checkpoint("group_completed", n_keys, extra = list(
      label = label, n_models = n_ok, n_models_requested = n_req
    ))
    if (n_ok >= 1L && !is.null(group_agg[[gk]])) {
      emit_partial(
        "scenario", label, groups_done,
        lapply(group_agg[[gk]], .compact_pipeline), n_ok, n_req
      )
    }
    invisible(NULL)
  }

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
    # Trigger (b): a key from another group (or historical) closes the open one.
    if (!is.null(open_gk) && !identical(open_gk, key_group$gk)) {
      on_group_complete(open_gk)
    }
    current_member <<- list()
    last_of_group <- FALSE
    if (!is.null(key_group)) {
      open_gk <<- key_group$gk
      mi <- metadata$member_index
      pm <- metadata$period_members
      if (is.numeric(mi) && length(mi) == 1L && is.finite(mi) &&
          is.numeric(pm) && length(pm) == 1L && is.finite(pm)) {
        current_member <<- list(
          member_index = as.integer(mi), period_members = as.integer(pm),
          current_label = .step2_scenario_display_key(
            key_group$ssp_code, key_group$yr_parts
          )
        )
        # Trigger (a): last member by metadata. May be unreachable if empty
        # models were skipped upstream; (b) and (c) cover that case.
        last_of_group <- mi >= pm
      }
    }
    checkpoint("pipeline_started", n_keys)
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
        if (inherits(e, "wiseapp_step2_cancelled")) stop(e)
        key_err <<- conditionMessage(e)
        warning(sprintf("[fct_run_simulation] Key %s failed: %s", key, key_err))
        NULL
      }
    )
    if (is.null(out)) {
      n_pipeline_failed <<- n_pipeline_failed + 1L
      failures[[length(failures) + 1L]] <<- list(
        key = key, gk = if (is.null(key_group)) NA_character_ else key_group$gk,
        is_hist = is_hist, error = key_err
      )
      checkpoint("pipeline_failed", n_keys)
      if (last_of_group) on_group_complete(key_group$gk)
      return(invisible(NULL))
    }
    if (identical(weather_storage, "reference") && !is_hist) {
      weather_refs[[key]] <<- step2_weather_store_put(weather_store, key, weather_input)
    }
    n_pipeline_completed <<- n_pipeline_completed + 1L
    if (is_hist) {
      n_hist_yrs <<- length(unique(format(weather_input$timestamp, "%Y")))
      hist_sim_result <<- list(
        pipeline = out, chol_obj = chol_obj, so = so,
        has_weights = !is.null(out$weight), weather_raw = weather_input,
        train_data = train_data, svy = svy,
        residuals = residuals
      )
      profile_memory("historical_pipeline", hist_sim_result$pipeline, detail = key)
      out$weather_raw <- NULL
      checkpoint("historical_ready", n_keys)
      if (is.function(preview_fn) || is.function(partial_fn)) {
        shared_ctx <<- tryCatch(
          step2_shared_context(
            train_aug = .compact_residual_context(
              precomputed_train_aug, shared_id_col, residuals,
              compact = identical(payload_mode, "compact")
            ),
            id_col = shared_id_col,
            residuals = residuals,
            model_metadata = list(
              engine = engine,
              weather_terms = weather_terms,
              fit_multi = !is.null(fit_multi),
              taus = taus
            )
          ),
          error = function(e) NULL
        )
      }
      if (is.function(preview_fn) && !is.null(shared_ctx)) {
        preview_context <- shared_ctx
        tryCatch({
          weighted <- !is.null(out$weight)
          is_log <- isTRUE(so$transform == "log")
          withr::with_seed(WISEAPP_DEFAULT_SEED, {
            preview_pipeline <- .compact_pipeline(out)
            summary <- aggregate_pipeline_tables_multi(
              pipelines = preview_pipeline,
              methods = "mean",
              weighted = weighted,
              residuals = residuals,
              is_log = is_log,
              band_q = c(lo = 0.10, hi = 0.90),
              skip_coef = isTRUE(skip_coef_draws),
              bandwidth_p0 = 0.05,
              seed = WISEAPP_DEFAULT_SEED,
              model_ids = "Historical",
              scenario = "Historical",
              shared_context = preview_context
            )[["mean"]]
            preview_data <- if (is.null(summary) || !nrow(summary)) {
              data.frame(sim_year = integer(0), value = numeric(0),
                         uncertainty = numeric(0))
            } else {
              data.frame(
                sim_year = summary$sim_year,
                value = summary$value,
                uncertainty = sqrt(pmax(summary$var_within, 0)),
                stringsAsFactors = FALSE
              )
            }
            preview_fn(list(
              data = preview_data,
              metadata = list(
                method = "mean",
                weighted = weighted,
                transform = so$transform %||% NA_character_,
                is_log = is_log,
                residuals = residuals,
                seed = WISEAPP_DEFAULT_SEED,
                bands = c(lo = 0.10, hi = 0.90),
                bandwidth_p0 = 0.05
              )
            ))
          })
        }, error = function(e) invisible(NULL))
      }
      emit_partial(
        "historical", "Historical", 0L, .compact_pipeline(out), 1L, 1L,
        historical = TRUE
      )
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
        # Keep the GCM name from the member key; fall back to model_<n>.
        gcm <- sub("^ssp[^_]+_[^_]+_[^_]+_[0-9]{4}_[0-9]{4}_", "", key)
        member_type <- if (nzchar(gcm) && gcm != key &&
                           !gcm %in% names(group_agg[[gk]])) {
          gcm
        } else {
          paste0("model_", group_n[[gk]] + 1L)
        }
      }
      if (identical(weather_storage, "reference")) out$weather_raw <- weather_refs[[key]]
      group_agg[[gk]][[member_type]] <<- out
      profile_memory("future_pipeline", out, detail = key)
      group_n[[gk]] <<- group_n[[gk]] + 1L
    }
    checkpoint("pipeline_completed", n_keys)
    if (last_of_group) on_group_complete(key_group$gk)
    invisible(NULL)
  }

  checkpoint("pipeline_started", 0L)
  weather_result <- if (!is.null(weather_manifest) ||
      !identical(prepared_weather_cache, "off")) {
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
    if (!inherits(manifest, "wiseapp_weather_manifest")) {
      stop("weather_manifest must be created by prepare_weather_manifest().", call. = FALSE)
    }
    manifest$frames
  } else tryCatch(
    weather_fn(
      survey_data = svy, selected_surveys = ss, selected_weather = sw,
      dates = sim_dates, connection_params = cp,
      ssp = if (has_future) ssps else NULL,
      future_period = if (has_future) fp_list else NULL,
      perturbation_method = perturbation_method, stored_breaks = stored_breaks,
      weather_collect = weather_collect, weather_threads = weather_threads,
      weather_consumer = consume_key
    ),
    error = function(e) {
      if (inherits(e, "wiseapp_step2_cancelled")) stop(e)
      if (length(emitted_keys)) stop(e)
      weather_fn(
        survey_data = svy, selected_surveys = ss, selected_weather = sw,
        dates = sim_dates, connection_params = cp,
        ssp = if (has_future) ssps else NULL,
        future_period = if (has_future) fp_list else NULL,
        perturbation_method = perturbation_method,
        stored_breaks = stored_breaks, weather_collect = weather_collect,
        weather_threads = weather_threads
      )
    }
  )
  t_weather <- proc.time()[["elapsed"]] - t_weather_start
  progress_fn(0.35, "Climate data loaded. Running scenarios...")
  if (is.list(weather_result) && length(weather_result)) {
    for (key in setdiff(names(weather_result), emitted_keys)) {
      consume_key(key, weather_result[[key]])
    }
  }
  # Trigger (c): the weather loop is finished, so the open group is complete.
  on_group_complete(open_gk)
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
    display_key <- .step2_scenario_display_key(meta$ssp_code, meta$year_range)
    new_scenarios[[display_key]] <- list(
      pipelines = group_agg[[gk]],
      weather_raw = group_weather_rep[[gk]],
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
