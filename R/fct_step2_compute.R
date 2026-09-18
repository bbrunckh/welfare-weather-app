# Phase 3: pure, serial Step 2 compute boundary.
#
# The numerical reference remains fct_run_simulation(). This adapter owns the
# boundary contract needed by synchronous, asynchronous, and future worker
# callers: ordinary-object inputs, explicit RNG, process-local setup, stage
# events, validation, and immutable run metadata.

.step2_compute_required <- c(
  "sw", "so", "svy", "ss", "mf", "cp", "fp_list", "ssps",
  "residuals", "skip_coef_draws", "sim_dates", "perturbation_method",
  "stored_breaks"
)

.step2_compute_copy <- function(x) {
  if (is.null(x) || is.atomic(x)) {
    return(x)
  }
  if (is.data.frame(x)) {
    out <- lapply(x, .step2_compute_copy)
    names(out) <- names(x)
    class(out) <- class(x)
    row.names(out) <- row.names(x)
    return(out)
  }
  if (is.list(x)) {
    out <- lapply(x, .step2_compute_copy)
    names(out) <- names(x)
    attributes(out) <- attributes(x)
    return(out)
  }
  x
}

.step2_compute_validate <- function(input) {
  if (!is.list(input) || is.null(names(input))) {
    stop("step2_compute() requires a named ordinary-object input snapshot.",
      call. = FALSE
    )
  }
  missing <- setdiff(.step2_compute_required, names(input))
  if (length(missing)) {
    stop("step2_compute() input snapshot is missing: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.list(input$mf)) stop("step2_compute()$mf must be a list.", call. = FALSE)
  if (!is.list(input$cp) || !validate_connection_params(input$cp)) {
    stop("step2_compute()$cp is not a valid connection parameter list.",
      call. = FALSE
    )
  }
  if (!is.data.frame(input$sw) || !"name" %in% names(input$sw)) {
    stop("step2_compute()$sw must be a data frame with a name column.",
      call. = FALSE
    )
  }
  if (!is.data.frame(input$so) || !"name" %in% names(input$so) ||
    length(input$so$name) != 1L) {
    stop("step2_compute()$so must contain exactly one outcome name.",
      call. = FALSE
    )
  }
  if (!is.data.frame(input$svy)) {
    stop("step2_compute()$svy must be a data frame.", call. = FALSE)
  }
  if (!is.list(input$fp_list) || !is.character(input$ssps) ||
    !is.character(input$sim_dates)) {
    stop("step2_compute() date and scenario fields have invalid types.",
      call. = FALSE
    )
  }
  if (length(input$fp_list) && any(lengths(input$fp_list) != 2L)) {
    stop("Each step2_compute() future period must contain two dates.",
      call. = FALSE
    )
  }
  if (length(input$residuals) != 1L ||
    !input$residuals %in% c("none", "original", "normal", "resample")) {
    stop("step2_compute()$residuals is invalid.", call. = FALSE)
  }
  if (length(input$skip_coef_draws) != 1L ||
    !is.logical(input$skip_coef_draws) || is.na(input$skip_coef_draws)) {
    stop("step2_compute()$skip_coef_draws must be one non-missing logical value.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

.step2_compute_signature <- function(input, seed, run_id) {
  list(
    step = "sim",
    schema = 1L,
    seed = as.integer(seed),
    sw = .sig_plain(input$sw),
    so = .sig_plain(input$so),
    survey_shape = c(nrow(input$svy), ncol(input$svy)),
    survey_columns = names(input$svy),
    model = .sig_plain(input$mf$.snap$model %||% input$mf[c("engine", "weather_terms")]),
    connection = .provenance_source(input$cp),
    fp_list = .sig_plain(input$fp_list),
    ssps = .sig_plain(input$ssps),
    residuals = input$residuals,
    skip_coef_draws = isTRUE(input$skip_coef_draws),
    sim_dates = .sig_plain(input$sim_dates),
    perturbation_method = .sig_plain(input$perturbation_method),
    stored_breaks = .sig_plain(input$stored_breaks)
  )
}

.step2_compute_event <- function(events, stage, status, started,
                                 detail = NULL, error = NULL) {
  event <- list(
    stage = stage,
    status = status,
    elapsed = unname(proc.time()[["elapsed"]] - started)
  )
  if (!is.null(detail)) event$detail <- as.character(detail)
  if (!is.null(error)) event$error <- conditionMessage(error)
  events[[length(events) + 1L]] <- event
  events
}

.step2_compute_init_process <- function(input, cache_dir = NULL) {
  .duck_con()
  if (!is.null(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    Sys.setenv(
      WISEAPP_WEATHER_CACHE_DIR = normalizePath(cache_dir, mustWork = FALSE),
      WISEAPP_WEATHER_CACHE_DISABLE = "0"
    )
  }
  # Creating the process-local connection is deliberately lazy. Local runs do
  # not need DuckDB until get_weather()/load_data() is called, while remote runs
  # acquire credentials in this process rather than serializing them.
  invisible(TRUE)
}

#' Execute the pure serial Step 2 computation boundary
#'
#' @param input Named ordinary-object snapshot accepted by
#'   \\code{fct_run_simulation()}. It must contain the fields validated by
#'   \\code{step2_compute()}.
#' @param seed Integer base seed. RNG state is set inside the call and restored
#'   before return.
#' @param run_id Optional stable caller/run identifier.
#' @param event_fn Function receiving one structured stage-event list.
#' @param cache_dir Optional process/run-scoped weather-cache directory.
#' @param weather_fn,pipeline_fn Injectable serial reference functions.
#' @return A list with `result`, `signature`, `run`, and `events`.
#' @export
step2_compute <- function(input,
                          seed = WISEAPP_DEFAULT_SEED,
                          run_id = NULL,
                          event_fn = function(event) invisible(NULL),
                          cache_dir = NULL,
                          weather_storage = c("memory", "reference"),
                          weather_store_root = NULL,
                          weather_collect = c("fast", "bounded"),
                          weather_threads = c("auto", "1", "2"),
                          weather_fn = get_weather,
                          pipeline_fn = run_sim_pipeline) {
  .step2_compute_validate(input)
  weather_storage <- match.arg(weather_storage)
  weather_collect <- match.arg(weather_collect)
  weather_threads <- match.arg(weather_threads)
  propagate_all_covariate_uncertainty <-
    isTRUE(input$propagate_all_covariate_uncertainty)
  fit_multi <- input$fit_multi %||% NULL
  taus <- input$taus %||% NULL
  weather_cols <- input$weather_cols %||% NULL
  seed <- as.integer(seed)[1L]
  if (is.na(seed)) stop("step2_compute()$seed must be an integer.", call. = FALSE)
  run_id <- as.character(run_id %||% paste0("step2-", .provenance_digest(
    list(seed = seed, now = format(Sys.time(), "%Y%m%dT%H%M%OS3Z"))
  )))[1L]
  snapshot <- .step2_compute_copy(input)
  signature <- .step2_compute_signature(snapshot, seed, run_id)
  started <- proc.time()[["elapsed"]]
  events <- list()
  emit <- function(stage, status, detail = NULL, error = NULL) {
    events <<- .step2_compute_event(events, stage, status, started, detail, error)
    try(event_fn(events[[length(events)]]), silent = TRUE)
    invisible(NULL)
  }

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
    get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  } else {
    NULL
  }
  had_seed <- !is.null(old_seed)
  old_rng_kind <- RNGkind()
  cache_env_names <- c(
    "WISEAPP_WEATHER_CACHE_DIR",
    "WISEAPP_WEATHER_CACHE_DISABLE"
  )
  old_cache_env <- Sys.getenv(cache_env_names, unset = NA_character_)
  restore_rng <- function() {
    do.call(RNGkind, as.list(old_rng_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
    for (i in seq_along(cache_env_names)) {
      if (is.na(old_cache_env[[i]])) {
        Sys.unsetenv(cache_env_names[[i]])
      } else {
        Sys.setenv(setNames(old_cache_env[[i]], cache_env_names[[i]]))
      }
    }
  }
  on.exit(restore_rng(), add = TRUE)

  emit("initialize", "started", "initializing process-local Step 2 state")
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(seed)
  .step2_compute_init_process(snapshot, cache_dir)
  emit("initialize", "completed")

  emit("weather", "started")
  weather_wrapper <- function(...) {
    value <- tryCatch(
      weather_fn(...),
      error = function(e) {
        emit("weather", "failed", error = e)
        stop(e)
      }
    )
    emit("weather", "completed")
    value
  }
  pipeline_index <- 0L
  pipeline_wrapper <- function(...) {
    pipeline_index <<- pipeline_index + 1L
    value <- tryCatch(
      pipeline_fn(...),
      error = function(e) {
        emit("pipeline", "failed", sprintf("key %d", pipeline_index), e)
        stop(e)
      }
    )
    emit("pipeline", "completed", sprintf("key %d", pipeline_index))
    value
  }
  simulation_args <- snapshot
  simulation_args$progress_fn <- function(value, detail) {
    emit("simulation", "progress", detail)
  }
  simulation_args$weather_fn <- weather_wrapper
  simulation_args$pipeline_fn <- pipeline_wrapper
  simulation_args$weather_storage <- weather_storage
  simulation_args$weather_store_root <- weather_store_root
  simulation_args$weather_collect <- weather_collect
  simulation_args$weather_threads <- weather_threads
  simulation_args$propagate_all_covariate_uncertainty <-
    propagate_all_covariate_uncertainty
  simulation_args$fit_multi <- fit_multi
  simulation_args$taus <- taus
  simulation_args$weather_cols <- weather_cols
  simulation_args$payload_mode <- "compact"
  simulation_args$direct_rif_predictions <- TRUE
  simulation_args$seed <- seed
  result <- tryCatch(
    do.call(fct_run_simulation, simulation_args),
    error = function(e) {
      emit("simulation", "failed", error = e)
      stop(e)
    }
  )
  emit("simulation", "completed")

  result$.sig <- signature
  result$.run <- list(
    id = run_id,
    seed = seed,
    schema = 1L,
    elapsed = result$t_elapsed,
    n_keys = result$n_keys,
    n_keys_ok = result$n_keys_ok,
    generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE)
  )
  emit("publish", "completed")
  list(result = result, signature = signature, run = result$.run, events = events)
}
