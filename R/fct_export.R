# Export bundle ----
# Configuration, tables, figures and a metadata document.
#                                                                              #
# UI-48. Two problems this solves:                                             #
#                                                                              #
#   1. An analysis could not be saved, shared or reproduced. Bookmarking is    #
#      inactive, so the only record of a run was the browser tab it lived in.  #
#   2. Individual CSV buttons export one table at a time, with no record of    #
#      what produced it - no data source, model specification, seed or run     #
#      signature travelled with the numbers.                                   #
#                                                                              #
# The bundle answers both: every artefact the session can produce, named to a  #
# single scheme, alongside a machine-readable manifest and a human-readable    #
# README that explains each file and states the provenance of the run.         #
#                                                                              #
# Registration is deliberately side-band. `session$userData` is shared between #
# a root session and every module session under it, so a module registers an   #
# artefact without any change to its server signature or return API - see      #
# `wise_export_table()` / `wise_export_figure()`.                              #


# Registry ----

#' Access (creating on first use) the session's export registry
#'
#' @param session A Shiny session; defaults to the current reactive domain.
#' @return An environment with an `items` list, or NULL outside a session.
#' @noRd
.export_store <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) {
    return(NULL)
  }
  ud <- session$userData
  if (is.null(ud$wise_exports)) {
    store <- new.env(parent = emptyenv())
    store$items <- list()
    ud$wise_exports <- store
  }
  ud$wise_exports
}

#' Register an exportable artefact
#'
#' Called by modules at server-construction time. `fun` is stored, not called:
#' artefacts are materialised only when a bundle is actually requested, so
#' registering costs nothing and a surface the user never opened contributes
#' nothing to the session's memory.
#'
#' Re-registering the same `key` replaces the earlier entry, so a module that
#' re-registers on re-render cannot accumulate duplicates.
#'
#' @param key   Stable identifier, snake_case. Becomes part of the file name.
#' @param label Human title, used in the README.
#' @param step  Integer step number (0-3) the artefact belongs to.
#' @param kind  `"table"` or `"figure"`.
#' @param fun   Zero-argument function returning a data frame (tables) or a
#'   ggplot (figures). Return NULL when unavailable.
#' @param description One sentence on what the artefact contains.
#' @param width,height Figure size in inches. Ignored for tables.
#' @param session Shiny session; defaults to the current reactive domain.
#'
#' @return Invisibly, the key.
#' @noRd
wise_export_register <- function(key, label, step, kind, fun,
                                 description = NULL,
                                 width = 10, height = 6,
                                 session = shiny::getDefaultReactiveDomain(),
                                 stale = NULL) {
  store <- .export_store(session)
  if (is.null(store)) {
    return(invisible(key))
  }
  stopifnot(is.function(fun))
  stale_ref <- stale
  if (is.null(stale_ref) && !is.null(session) && as.integer(step) > 0L) {
    stale_ref <- local({
      s <- session
      step_id <- as.integer(step)
      function() {
        ref <- s$userData[[paste0("wise_step", step_id, "_stale")]]
        if (is.function(ref)) isTRUE(ref()) else isTRUE(ref)
      }
    })
  }
  store$items[[key]] <- list(
    key         = key,
    label       = label,
    step        = as.integer(step),
    kind        = match.arg(kind, c("table", "figure")),
    fun         = fun,
    description = description %||% label,
    width       = width,
    height      = height,
    stale       = stale_ref
  )
  invisible(key)
}

wise_export_remove <- function(keys, session = shiny::getDefaultReactiveDomain()) {
  store <- .export_store(session)
  if (is.null(store) || !length(keys)) {
    return(invisible(FALSE))
  }
  store$items[keys] <- NULL
  invisible(TRUE)
}

wise_export_retain <- function(prefix, keys,
                               session = shiny::getDefaultReactiveDomain()) {
  store <- .export_store(session)
  if (is.null(store)) {
    return(invisible(FALSE))
  }
  current <- names(store$items)
  drop <- current[startsWith(current, prefix) & !current %in% keys]
  wise_export_remove(drop, session)
}

#' @rdname wise_export_register
#' @noRd
wise_export_table <- function(key, label, step, fun, description = NULL,
                              session = shiny::getDefaultReactiveDomain(),
                              stale = NULL) {
  wise_export_register(key, label, step, "table", fun, description,
    session = session, stale = stale
  )
}

#' @rdname wise_export_register
#' @noRd
wise_export_figure <- function(key, label, step, fun, description = NULL,
                               width = 10, height = 6,
                               session = shiny::getDefaultReactiveDomain(),
                               stale = NULL) {
  wise_export_register(key, label, step, "figure", fun, description,
    width = width, height = height, session = session,
    stale = stale
  )
}

#' List registered artefacts, ordered for the bundle
#'
#' Ordered by step, then kind (tables before figures), then key, so file
#' numbering is stable between exports of the same analysis.
#'
#' @param session Shiny session.
#' @return A list of registry items.
#' @noRd
wise_export_items <- function(session = shiny::getDefaultReactiveDomain()) {
  store <- .export_store(session)
  if (is.null(store) || !length(store$items)) {
    return(list())
  }
  items <- store$items
  ord <- order(
    vapply(items, `[[`, integer(1), "step"),
    match(vapply(items, `[[`, character(1), "kind"), c("table", "figure")),
    vapply(items, `[[`, character(1), "key")
  )
  items[ord]
}


# File naming ----

#' Slugify a string for use in a file name
#'
#' Lowercase, non-alphanumerics collapsed to single hyphens, trimmed. Keeps
#' names portable across Windows, macOS and Linux and safe inside a zip.
#'
#' @param x Character vector.
#' @return A character vector of slugs.
#' @noRd
.export_slug <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("^-+|-+$", "", x)
  x[!nzchar(x)] <- "item"
  x
}

#' Build the file name for one artefact
#'
#' Scheme: `NN_stepN_key.ext`
#'   NN    two-digit sequence, so a directory listing sorts into bundle order
#'   stepN the pipeline step the artefact comes from (step0-step3)
#'   key   the artefact's registry key, slugified
#'   ext   csv for tables, png for figures
#'
#' Documented verbatim in the README so a reader can decode any file name
#' without the manifest.
#'
#' @param index 1-based position in the bundle.
#' @param step  Step number.
#' @param key   Registry key.
#' @param kind  `"table"` or `"figure"`.
#' @return A single file name.
#' @noRd
.export_filename <- function(index, step, key, kind) {
  ext <- if (identical(kind, "figure")) "png" else "csv"
  sprintf("%02d_step%d_%s.%s", index, as.integer(step), .export_slug(key), ext)
}


# Configuration state ----

# Inputs that describe transient UI state rather than analysis configuration.
# Restoring them would replay clicks (re-running models on import) or fight the
# user's current layout, so they are dropped from the exported config. Buttons
# that trigger work (`apply_connection` connects) and the import control's own
# state are replayable clicks too. The Lasso panel toggles (`show_lasso_*`)
# open and close flyouts - layout state, not analysis state - while the
# result-shaping toggles (`show_model_spread`, `show_coef_uncertainty`,
# `show_return_period`, `show_regression_input`) travel: the exported figures
# read them. Credential-shaped ids are dropped outright: the bundle is meant
# to be shared, and `.provenance_source()` redacts the same shapes on the
# provenance side - the snapshot must agree with it.
.EXPORT_INPUT_DROP <- c(
  "^run_model$", "^run_sim$", "^run_policy_sim$", "^load_", "^refresh",
  # These controls may appear under names that are not known to the app. The
  # class-based snapshot filter below handles current sessions; these patterns
  # keep older exported configurations from restoring them.
  "^survey_stats$", "^weather_stats$", "_btn$",
  "^apply_", "^import_config", "^show_lasso",
  "_toggle$", "_open$", "^hide_",
  "_rows_current$", "_rows_all$", "_rows_selected$", "_columns_selected$",
  "_cells_selected$", "_search$", "_state$", "_cell_clicked$",
  "^DataTables_Table_", "_cell_edit$", "_state_change$",
  "^plotly_", "_click$", "_hover$", "_brush$", "_dblclick$",
  "^\\.clientdata", "^sidebar", "^accordion$", "_bounds$", "_center$",
  "_zoom$", "_shape_", "_marker_", "_groups$",
  "secret", "key", "token", "password", "credential", "client_id", "tenant"
)

# How long the import retry keeps waiting for renderUI() controls to appear
# before it gives up, audibly (see export_menu_server()).
.EXPORT_RETRY_SECONDS <- 120

wise_current_seed <- function(default = WISEAPP_DEFAULT_SEED,
                              session = shiny::getDefaultReactiveDomain()) {
  value <- if (!is.null(session)) session$userData$wise_analysis_seed else NULL
  value <- suppressWarnings(as.integer(value)[1L])
  if (is.na(value)) as.integer(default) else value
}

#' Should an input be carried in the exported configuration?
#'
#' @param ids Character vector of (namespaced) input ids.
#' @return A logical vector.
#' @noRd
.export_keep_input <- function(ids) {
  # Match on the final, un-namespaced segment as well as the full id, so a
  # module-scoped "step1-model-run_model" is dropped by the "^run_model$" rule.
  leaf <- sub("^.*-", "", ids)
  keep <- rep(TRUE, length(ids))
  for (pat in .EXPORT_INPUT_DROP) {
    keep <- keep & !grepl(pat, leaf) & !grepl(pat, ids)
  }
  keep
}

#' Capture the full analysis configuration as a plain list
#'
#' Shiny namespaces module inputs as a prefix on one flat input map, so a
#' single `reactiveValuesToList()` at the root session captures every control
#' in the app - no per-module wiring, and nothing silently missing when a new
#' control is added.
#'
#' @param input   The root session's `input` object.
#' @param seed    Base random seed in force for the session.
#' @param provenance Optional list of per-step run provenance records.
#'
#' @return A named list ready for `jsonlite::toJSON()`.
#' @noRd
wise_config_snapshot <- function(input, seed = WISEAPP_DEFAULT_SEED,
                                 provenance = list()) {
  vals <- tryCatch(shiny::reactiveValuesToList(input), error = function(e) list())
  if (length(vals)) {
    vals <- vals[.export_keep_input(names(vals))]
    # Shiny action-button values are click counters, not restorable settings;
    # filtering by class also catches buttons added without a known id.
    vals <- vals[!vapply(vals, function(v) {
      inherits(v, "shinyActionButtonValue")
    }, logical(1))]
    # Drop values that carry no meaning outside the live session.
    vals <- vals[vapply(vals, function(v) {
      is.null(v) || is.atomic(v) || is.list(v)
    }, logical(1))]
  }
  list(
    wiseapp_config_version = 1L,
    exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    app_version = tryCatch(as.character(golem::get_golem_version()),
      error = function(e) NA_character_
    ),
    r_version = paste0(R.version$major, ".", R.version$minor),
    random_seed = as.integer(seed),
    provenance = provenance,
    inputs = vals
  )
}

#' Restore a configuration snapshot into the live session
#'
#' Values are pushed with `session$sendInputMessage()`, which every stock Shiny
#' input binding understands. Controls that live inside `renderUI()` do not
#' exist until their upstream data has loaded, so a single pass would silently
#' drop them; `wise_config_apply()` therefore returns the ids it could not
#' place, and the caller re-applies them as the UI fills in.
#'
#' @param config  Parsed configuration list (from `wise_config_snapshot()`).
#' @param session Root Shiny session.
#' @param existing Character vector of input ids that currently exist.
#'
#' @return Invisibly, a list with `applied` and `pending` id vectors.
#' @noRd
wise_config_apply <- function(config, session, existing = character(0)) {
  vals <- config$inputs %||% list()
  if (!length(vals)) {
    return(invisible(list(
      applied = character(0),
      pending = character(0)
    )))
  }
  # Filter on import as well as export so configurations written by older
  # versions cannot queue controls that can never be restored.
  vals <- vals[.export_keep_input(names(vals))]
  if (!length(vals)) {
    return(invisible(list(
      applied = character(0),
      pending = character(0)
    )))
  }
  ids <- names(vals)
  can <- if (length(existing)) ids %in% existing else rep(TRUE, length(ids))

  is_button <- function(id) {
    value <- tryCatch(session$input[[id]], error = function(e) NULL)
    inherits(value, "shinyActionButtonValue")
  }

  applied <- character(0)
  failed <- character(0)
  for (id in ids[can]) {
    if (is_button(id)) next
    ok <- tryCatch(
      {
        session$sendInputMessage(id, list(value = vals[[id]]))
        TRUE
      },
      error = function(e) FALSE
    )
    if (ok) applied <- c(applied, id) else failed <- c(failed, id)
  }
  invisible(list(
    applied = applied, pending = unique(c(ids[!can], failed)),
    failed = failed
  ))
}

#' Validate an imported configuration before anything is applied
#'
#' Rejects files that are not WISE-APP exports or that a newer configuration
#' format wrote, and collects warnings (e.g. a different `app_version`) to
#' show alongside the success message rather than silently continuing.
#'
#' @param cfg Parsed configuration list.
#' @param app_version This session's app version (NA when unknown).
#' @return A list: `ok` (logical), `text` (rejection reason), `notes`
#'   (warnings to show with the success status).
#' @noRd
.import_validate <- function(cfg, app_version = NA_character_) {
  reject <- function(text) list(ok = FALSE, text = text, notes = character(0))
  if (!is.list(cfg) || is.null(names(cfg))) {
    return(reject("That file must contain a JSON object exported by WISE-APP."))
  }
  if (is.null(cfg$inputs)) {
    return(reject(paste(
      "That file has no `inputs` section - it does not look like a",
      "WISE-APP configuration export."
    )))
  }
  if (!is.list(cfg$inputs) || is.null(names(cfg$inputs))) {
    return(reject("The `inputs` section must be a named JSON object."))
  }
  v <- cfg$wiseapp_config_version
  if (is.null(v)) {
    return(reject(paste(
      "That file has no `wiseapp_config_version` field - it does not look",
      "like a WISE-APP configuration export."
    )))
  }
  if (isTRUE(v > 1)) {
    return(reject(paste0(
      "That file was written in configuration version ", v, "; this app ",
      "reads version 1. Update WISE-APP, then re-import."
    )))
  }
  notes <- character(0)
  if (!is.null(cfg$app_version) && !is.na(app_version) &&
    !identical(as.character(cfg$app_version), app_version)) {
    notes <- c(notes, paste0(
      "Exported from WISE-APP ", cfg$app_version, "; this session runs ",
      app_version, " - settings may differ."
    ))
  }
  list(ok = TRUE, text = character(0), notes = notes)
}


# Configuration pipeline runner ----

.PIPELINE_STAGE_TIMEOUT <- 900
.PIPELINE_SETTLE_SECONDS <- 3

.pipeline_stages <- function() {
  list(
    list(key = "load_survey", label = "Load the survey sample"),
    list(key = "load_weather", label = "Load the weather variables"),
    list(key = "step1", label = "Step 1 - Fit the welfare model"),
    list(key = "step2", label = "Step 2 - Run the climate simulation"),
    list(key = "step3", label = "Step 3 - Run the policy simulation")
  )
}

.pipeline_settle_seconds <- function() {
  getOption("wiseapp.pipeline_settle", .PIPELINE_SETTLE_SECONDS)
}

.pipeline_progress_ui <- function(state) {
  icons <- list(
    pending = shiny::icon("circle", class = "pipeline-pending"),
    running = shiny::icon("spinner", class = "fa-spin pipeline-running"),
    done    = shiny::icon("circle-check", class = "pipeline-done"),
    failed  = shiny::icon("circle-xmark", class = "pipeline-failed"),
    skipped = shiny::icon("minus", class = "pipeline-skipped")
  )
  shiny::tags$ul(
    class = "pipeline-stages",
    lapply(.pipeline_stages(), function(stage) {
      status <- state[[stage$key]] %||% "pending"
      shiny::tags$li(
        class = paste0("pipeline-stage pipeline-stage-", status),
        icons[[status]],
        shiny::tags$span(stage$label),
        if (identical(status, "running")) {
          shiny::tags$span(class = "pipeline-note", "running...")
        }
      )
    })
  )
}

#' Drive the ordered stages after a configuration import
#'
#' A stage is complete only when its generation advances and its explicit
#' status is `success`. This avoids mistaking an unchanged result object for a
#' successful rerun and still advances cached/no-op loads.
#' @noRd
pipeline_runner <- function(triggers, results, on_state = NULL,
                            on_settle = NULL,
                            ready_to_run = function() TRUE,
                            stage_timeout = .PIPELINE_STAGE_TIMEOUT) {
  stages <- .pipeline_stages()
  keys <- vapply(stages, `[[`, character(1), "key")
  keys <- keys[vapply(keys, function(key) {
    is.function(triggers[[key]]) &&
      is.list(results[[key]]) &&
      is.function(results[[key]]$generation) &&
      is.function(results[[key]]$status)
  }, logical(1))]

  state <- shiny::reactiveVal(stats::setNames(
    rep("pending", length(keys)), keys
  ))
  phase <- shiny::reactiveVal("idle")
  message <- shiny::reactiveVal(NULL)
  active <- shiny::reactiveVal(NULL)
  baseline <- shiny::reactiveVal(NULL)
  fired <- shiny::reactiveVal(FALSE)
  deadline <- shiny::reactiveVal(NULL)
  settling <- shiny::reactiveVal(NULL)
  settle_until <- shiny::reactiveVal(NULL)
  request_id <- 0L

  emit <- function() {
    if (is.function(on_state)) on_state(state(), phase(), message())
  }

  set_stage <- function(key, status) {
    current <- state()
    current[[key]] <- status
    state(current)
  }

  clear_triggers <- function() {
    for (key in keys) {
      trigger <- triggers[[key]]
      if (is.function(trigger)) {
        try(trigger(NULL), silent = TRUE)
      }
    }
  }

  clear_trigger <- function(key) {
    trigger <- triggers[[key]]
    if (is.function(trigger)) try(trigger(NULL), silent = TRUE)
  }

  fail <- function(key, text) {
    set_stage(key, "failed")
    idx <- match(key, keys)
    if (!is.na(idx) && idx < length(keys)) {
      for (later in keys[seq.int(idx + 1L, length(keys))]) {
        set_stage(later, "skipped")
      }
    }
    active(NULL)
    baseline(NULL)
    fired(FALSE)
    deadline(NULL)
    settling(NULL)
    settle_until(NULL)
    clear_triggers()
    phase("failed")
    message(text)
    emit()
  }

  advance <- function(key) {
    idx <- match(key, keys)
    if (is.na(idx) || idx >= length(keys)) {
      active(NULL)
      baseline(NULL)
      fired(FALSE)
      deadline(NULL)
      phase("done")
      message("Data loaded and all three steps re-run successfully.")
      emit()
      return(invisible(NULL))
    }
    next_key <- keys[[idx + 1L]]
    set_stage(next_key, "running")
    active(next_key)
    baseline(results[[next_key]]$generation())
    fired(FALSE)
    deadline(Sys.time() + stage_timeout)
    settling(next_key)
    settle_until(Sys.time() + .pipeline_settle_seconds())
    if (is.function(on_settle)) on_settle()
    emit()
  }

  shiny::observe({
    key <- settling()
    until <- settle_until()
    if (is.null(key) || is.null(until)) {
      return(invisible(NULL))
    }
    if (Sys.time() >= until) {
      if (!isTRUE(ready_to_run())) {
        if (is.function(on_settle)) on_settle()
        shiny::invalidateLater(250)
        return(invisible(NULL))
      }
      settling(NULL)
      settle_until(NULL)
      request_id <<- request_id + 1L
      triggers[[key]](list(n = request_id, at = Sys.time()))
      fired(TRUE)
      emit()
    } else {
      if (is.function(on_settle)) on_settle()
      shiny::invalidateLater(250)
    }
  })

  for (key in keys) {
    local({
      stage_key <- key
      shiny::observe({
        current <- active()
        if (!identical(current, stage_key)) {
          return(invisible(NULL))
        }
        if (!isTRUE(fired())) {
          return(invisible(NULL))
        }
        result <- results[[stage_key]]
        generation <- result$generation()
        status <- result$status()
        if (identical(status, "failure") &&
          isTRUE(generation > (baseline() %||% generation))) {
          fail(stage_key, paste0(.pipeline_label(stage_key), " failed."))
        } else if (identical(status, "success") &&
          isTRUE(generation > (baseline() %||% generation))) {
          set_stage(stage_key, "done")
          clear_trigger(stage_key)
          active(NULL)
          baseline(NULL)
          fired(FALSE)
          deadline(NULL)
          emit()
          advance(stage_key)
        }
      })
    })
  }

  shiny::observe({
    key <- active()
    dl <- deadline()
    if (is.null(key) || is.null(dl)) {
      return(invisible(NULL))
    }
    shiny::invalidateLater(1000)
    if (Sys.time() > dl) {
      fail(key, paste0(
        .pipeline_label(key), " did not finish within ",
        round(stage_timeout / 60), " minutes."
      ))
    }
  })

  cancel <- function() {
    if (!identical(phase(), "running")) {
      return(invisible(NULL))
    }
    active(NULL)
    baseline(NULL)
    fired(FALSE)
    deadline(NULL)
    settling(NULL)
    settle_until(NULL)
    clear_triggers()
    current <- state()
    current[current %in% c("pending", "running")] <- "skipped"
    state(current)
    phase("cancelled")
    message(paste(
      "Stopped. The current synchronous operation, if already started, ",
      "will finish; later stages will not run."
    ))
    emit()
    invisible(NULL)
  }

  start <- function() {
    if (!length(keys)) {
      phase("failed")
      message("No pipeline stages are wired.")
      emit()
      return(invisible(NULL))
    }
    state(stats::setNames(rep("pending", length(keys)), keys))
    phase("running")
    message(NULL)
    request_id <<- request_id + 1L
    first <- keys[[1L]]
    set_stage(first, "running")
    active(first)
    baseline(results[[first]]$generation())
    fired(FALSE)
    deadline(Sys.time() + stage_timeout)
    settling(first)
    settle_until(Sys.time() + .pipeline_settle_seconds())
    if (is.function(on_settle)) on_settle()
    emit()
    invisible(NULL)
  }

  reset <- function() {
    state(stats::setNames(rep("pending", length(keys)), keys))
    phase("idle")
    message(NULL)
    active(NULL)
    baseline(NULL)
    fired(FALSE)
    deadline(NULL)
    settling(NULL)
    settle_until(NULL)
    clear_triggers()
    emit()
    invisible(NULL)
  }

  list(
    start = start, cancel = cancel, reset = reset,
    state = state, phase = phase, message = message
  )
}

.pipeline_label <- function(key) {
  stage <- Filter(function(x) identical(x$key, key), .pipeline_stages())[[1L]]
  stage$label
}


# Bundle assembly ----

#' Materialise one registry item to a file
#'
#' Make a data frame writable as CSV
#'
#' `utils::write.csv()` aborts with "unimplemented type 'list' in
#' 'EncodeElement'" on a list column, and several app tables carry one (the
#' weather specification's custom breaks, for instance). Rather than excluding
#' those tables, collapse anything non-atomic to a readable string so the
#' column survives the round trip.
#'
#' @param df A data frame.
#' @return A data frame whose columns are all atomic.
#' @noRd
.export_flatten_df <- function(df) {
  if (!is.data.frame(df) || !ncol(df)) {
    return(df)
  }
  as.data.frame(
    lapply(df, function(col) {
      # A matrix column: one string per row. digits = 15 keeps the double
      # precision write.csv() itself would give - format()'s 7-digit default
      # silently rounded specification values (bin breaks, polynomials).
      if (is.matrix(col)) {
        return(apply(col, 1L, function(r) {
          paste(format(r, trim = TRUE, digits = 15), collapse = "; ")
        }))
      }
      if (is.list(col)) {
        return(vapply(col, function(x) {
          if (is.null(x) || !length(x)) {
            return(NA_character_)
          }
          x <- unlist(x, use.names = FALSE)
          paste(format(x, trim = TRUE, digits = 15), collapse = "; ")
        }, character(1)))
      }
      # Factors and dates write fine; everything else atomic passes through.
      if (is.atomic(col) || inherits(col, c("Date", "POSIXct", "factor"))) {
        return(col)
      }
      as.character(col)
    }),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

.export_write_item <- function(item, dir, file) {
  fail <- function(msg) list(status = "error", note = msg)
  path <- file.path(dir, file)

  # Do not materialise a previous result while the published run is stale.
  if (is.function(item$stale) &&
    isTRUE(shiny::isolate(item$stale()))) {
    if (file.exists(path)) unlink(path)
    return(list(status = "skipped", note = paste0(
      "Step ", item$step,
      " results are stale."
    )))
  }

  value <- tryCatch(item$fun(), error = function(e) e)
  if (inherits(value, "error")) {
    # A req() throw (shiny.silent.error) means "this surface is not ready" -
    # the step has not produced anything to export. That is not a failure:
    # it is skipped exactly like a NULL return, with no note. Any other
    # error is a genuine failure: the artefact is named in the README's
    # "Not exported" section rather than vanishing beside the not-run ones.
    if (inherits(value, "shiny.silent.error")) {
      return(NULL)
    }
    # An item can ask to be skipped with a note (e.g. a Step 2 run is still
    # streaming); the note reaches the README and the export notification.
    if (inherits(value, "wise_export_skip")) {
      return(list(status = "skipped", note = conditionMessage(value)))
    }
    msg <- conditionMessage(value)
    if (!nzchar(msg)) msg <- "artefact could not be produced"
    return(fail(msg))
  }
  if (is.null(value)) {
    return(NULL)
  }

  if (identical(item$kind, "table")) {
    # The write itself is guarded, not just the builder. An unwritable table
    # used to propagate out of the download handler, so Shiny answered the
    # request with its HTML error page - which the browser then saved under
    # the .zip/.csv name. One bad table must cost that table, not the bundle.
    return(tryCatch(
      {
        if (!is.data.frame(value)) value <- as.data.frame(value)
        if (nrow(value) == 0L) {
          return(NULL)
        }
        flat <- .export_flatten_df(value)
        utils::write.csv(flat, path, row.names = FALSE, na = "")
        list(status = "ok", rows = nrow(flat), cols = ncol(flat))
      },
      error = function(e) {
        if (file.exists(path)) unlink(path)
        fail(conditionMessage(e))
      }
    ))
  }

  # Figures: ggplot objects render through ggsave with the ragg AGG device -
  # the same renderer the on-screen plots use (shiny.useragg), so fonts and
  # antialiasing in the PNG match what the user saw, and ragg is faster than
  # the grDevices cairo path. echarts4r widgets render through a headless
  # browser (htmlwidgets -> standalone HTML -> webshot2 screenshot), which is
  # exactly the canvas ECharts paints on screen. Anything else is skipped
  # rather than guessed at.
  tryCatch(
    {
      if (inherits(value, "echarts4r")) {
        res <- .export_write_echarts(value, path, item)
        if (res$status %in% c("error", "skipped")) {
          return(res)
        }
      } else if (inherits(value, "ggplot")) {
        ggplot2::ggsave(path,
          plot = value, width = item$width,
          height = item$height, dpi = 150, bg = "white",
          device = ragg::agg_png
        )
      } else {
        return(NULL)
      }
      list(status = "ok", rows = NA_integer_, cols = NA_integer_)
    },
    error = function(e) {
      if (file.exists(path)) unlink(path)
      fail(conditionMessage(e))
    }
  )
}

# Render one echarts4r widget to PNG through a headless browser
#
# The widget is written to a standalone HTML file (animation disabled so the
# first paint is the final state), loaded in Chromium via webshot2, and
# captured at the registry item's figure size (inches at the same 150 dpi the
# ggplot path uses). Requires webshot2 and a Chrome/Chromium binary; when
# either is unavailable the figure is skipped with a note - one missing
# capability costs that figure, not the bundle.
#
#' @param value An echarts4r widget.
#' @param path  Destination PNG path.
#' @param item  Registry item (width/height in inches).
#' @noRd
.export_write_echarts <- function(value, path, item) {
  fail <- function(msg) list(status = "error", note = msg)
  skip <- function(msg) list(status = "skipped", note = msg)
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    return(skip("htmlwidgets is not installed"))
  }
  if (!requireNamespace("webshot2", quietly = TRUE)) {
    return(skip("PNG export of echarts figures needs the webshot2 package"))
  }

  # R2-SEC-05: remove the intermediate page and any non-self-contained
  # "<name>_files" dependency folder on every exit path.
  html <- tempfile(fileext = ".html")
  on.exit(unlink(c(
    html, paste0(tools::file_path_sans_ext(html), "_files")
  ), recursive = TRUE, force = TRUE), add = TRUE)

  tryCatch(
    {
      value$x$opts$animation <- FALSE
      htmlwidgets::saveWidget(value, html)
      webshot2::webshot(html, path,
        vwidth = max(round(item$width * 150), 1L),
        vheight = max(round(item$height * 150), 1L),
        delay = 1.5, quiet = TRUE
      )
      if (!file.exists(path)) {
        return(fail("the headless browser did not produce a PNG"))
      }
      list(status = "ok")
    },
    error = function(e) {
      if (file.exists(path)) unlink(path)
      fail(conditionMessage(e))
    }
  )
}

#' Write the machine-readable manifest
#' @noRd
.export_manifest_df <- function(entries) {
  if (!length(entries)) {
    return(data.frame(
      file = character(0), kind = character(0),
      step = character(0), title = character(0),
      description = character(0), rows = integer(0),
      columns = integer(0), stringsAsFactors = FALSE
    ))
  }
  data.frame(
    file = vapply(entries, `[[`, character(1), "file"),
    kind = vapply(entries, `[[`, character(1), "kind"),
    step = vapply(entries, `[[`, character(1), "step_label"),
    title = vapply(entries, `[[`, character(1), "label"),
    description = vapply(entries, `[[`, character(1), "description"),
    rows = vapply(entries, function(e) e$rows %||% NA_integer_, integer(1)),
    columns = vapply(entries, function(e) e$cols %||% NA_integer_, integer(1)),
    stringsAsFactors = FALSE
  )
}

#' Human-readable label for a pipeline step
#' @noRd
.export_step_label <- function(step) {
  switch(as.character(step),
    "0" = "Step 0 - Overview and data source",
    "1" = "Step 1 - Model welfare",
    "2" = "Step 2 - Climate scenarios",
    "3" = "Step 3 - Policy scenarios",
    paste("Step", step)
  )
}


# Metadata document ----

#' Build the README that explains the bundle
#'
#' The naming scheme is stated in full, so a reader can decode any file name
#' without consulting the manifest, and every file is listed with what it
#' contains. The provenance block is what makes the numbers traceable: which
#' source, which specification, which seed, which run.
#'
#' @param entries    List of written-file records.
#' @param provenance Named list of per-step `wise_provenance()` records.
#' @param config     The configuration snapshot (for version/seed reporting).
#' @param included   Character vector: which parts the bundle contains.
#'
#' @return A character vector of Markdown lines.
#' @noRd
wise_export_readme <- function(entries, provenance = list(), config = list(),
                               included = c("config", "tables", "figures"),
                               skipped = list()) {
  L <- function(...) c(...)
  hdr <- c(
    "# WISE-APP export bundle",
    "",
    paste0("Exported: ", config$exported_at %||%
      format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
    paste0("App version: ", config$app_version %||% "unknown"),
    paste0("R version: ", config$r_version %||% "unknown"),
    paste0("Random seed: ", config$random_seed %||% WISEAPP_DEFAULT_SEED),
    "",
    paste(
      "This bundle is a self-contained record of one WISE-APP analysis:",
      "the configuration that produced it, the tables and figures it",
      "generated, and this document explaining both."
    ),
    ""
  )

  contents <- c(
    "## What is in this bundle",
    "",
    "| File | Contents |",
    "| --- | --- |",
    if ("config" %in% included) {
      "| `configuration.json` | Every input in the app at export time, plus the random seed and per-step run provenance. Re-import it through Export -> Import configuration to restore this analysis. |"
    },
    "| `manifest.csv` | Machine-readable index of every file below: name, kind, step, title, description, row/column counts, and registered metadata. |",
    "| `README.md` | This document. |",
    if ("tables" %in% included) {
      "| `*.csv` | One file per table the session produced. |"
    },
    if ("figures" %in% included) {
      "| `*.png` | One file per figure the session produced. |"
    },
    ""
  )

  naming <- c(
    "## File naming",
    "",
    "Data and figure files follow one scheme:",
    "",
    "```",
    "NN_stepN_artefact-name.ext",
    "```",
    "",
    "| Part | Meaning |",
    "| --- | --- |",
    "| `NN` | Two-digit sequence number. Sorting a directory by name reproduces the order of this document. |",
    "| `stepN` | The pipeline step that produced the artefact: `step0` overview and data source, `step1` model welfare, `step2` climate scenarios, `step3` policy scenarios. |",
    "| `artefact-name` | Lowercase, hyphen-separated identifier for the artefact. |",
    "| `.ext` | `.csv` for tables, `.png` for figures. |",
    "",
    paste(
      "Names are stable across exports of the same analysis, so two bundles",
      "can be diffed file by file. Numbers follow the registry's fixed",
      "order, so a step that produced nothing leaves its number unused",
      "rather than shifting every later file."
    ),
    ""
  )

  files <- c("## Files", "")
  if (!length(entries)) {
    files <- c(
      files,
      paste(
        "No tables or figures were exported. Artefacts are only included",
        "once the step that produces them has been run."
      ), ""
    )
  } else {
    by_step <- split(entries, vapply(entries, `[[`, character(1), "step_label"))
    for (sl in names(by_step)) {
      files <- c(files, paste0("### ", sl), "")
      for (e in by_step[[sl]]) {
        size <- if (!is.na(e$rows %||% NA)) {
          sprintf(
            " (%s %s x %s %s)",
            fmt_count(e$rows), if (identical(e$rows, 1L)) "row" else "rows",
            e$cols, if (identical(e$cols, 1L)) "column" else "columns"
          )
        } else {
          ""
        }
        files <- c(
          files,
          sprintf("- **`%s`** - %s%s", e$file, e$description, size)
        )
      }
      files <- c(files, "")
    }
  }

  prov <- c("## Provenance", "")
  prov_recs <- Filter(Negate(is.null), provenance)
  if (!length(prov_recs)) {
    prov <- c(
      prov,
      "No step has produced results yet, so there is no run to describe.", ""
    )
  } else {
    prov <- c(prov, paste(
      "Each block below describes one completed run: the data source it read,",
      "the specification it fitted, the seed it used, and a short signature",
      "identifying the exact combination of inputs. Tables and figures above",
      "come from these runs."
    ), "")
    for (p in prov_recs) {
      prov <- c(prov, paste0("### ", p$step_label %||% "Run"), "")
      src <- p$source %||% list()
      kv <- list(
        "Run signature" = p$run_signature,
        "Data source" = if (length(src)) {
          paste(
            paste0(names(src), "=", vapply(src, function(v) {
              paste(as.character(v), collapse = "/")
            }, character(1))),
            collapse = "; "
          )
        } else {
          NULL
        },
        "Survey version" = p$survey_version,
        "Outcome" = p$outcome,
        "Weather" = p$weather,
        "Specification" = p$model_spec,
        "Engine" = p$engine,
        "Observations" = if (is.na(p$n_observations %||% NA)) {
          NULL
        } else {
          fmt_count(p$n_observations)
        },
        "Random seed" = p$random_seed,
        # A stale run's numbers still describe its own inputs, but the
        # session's current inputs have moved on: say so, rather than letting
        # a diff-hunting reader wonder why a re-run disagrees.
        "Results stale" = if (isTRUE(p$stale)) {
          "yes - current inputs have changed since this run"
        } else {
          NULL
        },
        "App version" = p$app_version
      )
      extra_keys <- setdiff(names(p), c(
        "step", "step_label", "run_signature", "source", "survey_version",
        "outcome", "weather", "model_spec", "engine", "n_observations",
        "random_seed", "app_version", "fallbacks", "stale"
      ))
      for (k in extra_keys) kv[[k]] <- p[[k]]
      for (k in names(kv)) {
        v <- kv[[k]]
        if (is.null(v) || !length(v)) next
        v <- paste(as.character(v), collapse = ", ")
        if (!nzchar(v) || identical(v, "NA")) next
        prov <- c(prov, sprintf("- **%s:** %s", k, v))
      }
      if (length(p$fallbacks)) {
        prov <- c(prov, sprintf(
          "- **Specification fallbacks:** %s",
          paste(p$fallbacks, collapse = "; ")
        ))
      }
      prov <- c(prov, "")
    }
  }

  omitted <- character(0)
  if (length(skipped)) {
    omitted <- c(
      "## Not exported", "",
      paste(
        "These artefacts could not be written. Everything else in this bundle",
        "is unaffected."
      ),
      "",
      vapply(
        skipped, function(sk) {
          sprintf(
            "- **%s** (%s) - %s",
            sk$label, sk$step_label, sk$note
          )
        },
        character(1)
      ),
      ""
    )
  }

  repro <- c(
    "## Reproducing this analysis",
    "",
    "1. Open WISE-APP and connect to the data source named under Provenance.",
    "2. Choose **Export -> Import configuration** and select `configuration.json`.",
    "3. Re-run each step in order (Step 1 model, Step 2 simulation, Step 3 policy).",
    "",
    paste(
      "The random seed travels in the configuration, so a re-run on the same",
      "data reproduces the same draws. The run signature in each provenance",
      "block should match after re-running; if it does not, an input differs."
    ),
    "",
    "### Limits",
    "",
    paste(
      "- Import restores control values, then offers to replay the stages in",
      "order. Fitting and simulation can take several minutes."
    ),
    paste(
      "- Controls that only appear once data has loaded are restored as the",
      "interface fills in. Connect to the data source before importing."
    ),
    paste(
      "- The configuration records the data source's identity, never its",
      "credentials. Supply those as usual."
    ),
    ""
  )

  notes <- c(
    "## Notes", "",
    paste(
      "- Figures are PNG renders at fixed per-figure dimensions, not the",
      "on-screen plot size: ggplot figures render through the AGG device,",
      "echarts figures through a headless browser, so text rendering matches",
      "what the app shows."
    ),
    paste(
      "- Data CSVs carry the raw values behind the display at full double",
      "precision; the Download CSV buttons on the on-screen tables carry",
      "the displayed formatting instead."
    ),
    ""
  )

  L(hdr, contents, naming, files, notes, omitted, prov, repro)
}


# Bundle writer ----

#' Assemble an export bundle as a zip archive
#'
#' @param zipfile    Destination path.
#' @param items      Registry items (from `wise_export_items()`).
#' @param config     Configuration snapshot, or NULL to omit it.
#' @param provenance Named list of `wise_provenance()` records.
#' @param include    Which parts to write: any of "config", "tables", "figures".
#'
#' @return Invisibly, the manifest data frame.
#' @noRd
wise_export_bundle <- function(zipfile, items, config = NULL,
                               provenance = list(),
                               include = c("config", "tables", "figures"),
                               progress = NULL) {
  stage <- tempfile("wise-export-")
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  wanted <- Filter(function(it) {
    (identical(it$kind, "table") && "tables" %in% include) ||
      (identical(it$kind, "figure") && "figures" %in% include)
  }, items)

  all_items <- Filter(function(it) {
    identical(it$kind, "table") || identical(it$kind, "figure")
  }, items)
  stable_index <- setNames(
    seq_along(all_items),
    vapply(all_items, `[[`, character(1), "key")
  )

  entries <- list()
  skipped <- list()
  # Numbering follows the registry's stable order (step, then tables before
  # figures): a surface that contributes nothing - not run, or a named
  # failure - leaves a gap instead of renumbering every later file, so two
  # bundles of the same analysis stay file-by-file diffable even when steps
  # differ in what they produced.
  for (i in seq_along(wanted)) {
    it <- wanted[[i]]
    file <- .export_filename(stable_index[[it$key]], it$step, it$key, it$kind)
    if (is.function(progress)) progress(i, length(wanted), it$label)
    res <- .export_write_item(it, stage, file)
    if (is.null(res)) next
    if (res$status %in% c("error", "skipped")) {
      # Recorded and reported in the README rather than dropped in silence -
      # a missing file with no explanation is worse than a named failure.
      skipped[[length(skipped) + 1L]] <- list(
        label = it$label, step_label = .export_step_label(it$step),
        note = res$note
      )
      if (identical(res$status, "error")) {
        warning("[wise_export_bundle] skipping '", it$key, "': ", res$note)
      }
      next
    }
    entries[[length(entries) + 1L]] <- list(
      file = file, kind = it$kind, step_label = .export_step_label(it$step),
      label = it$label, description = it$description,
      rows = res$rows, cols = res$cols
    )
  }

  manifest <- .export_manifest_df(entries)
  utils::write.csv(manifest, file.path(stage, "manifest.csv"),
    row.names = FALSE, na = ""
  )

  if ("config" %in% include && !is.null(config)) {
    jsonlite::write_json(config, file.path(stage, "configuration.json"),
      auto_unbox = TRUE, pretty = TRUE, null = "null",
      digits = NA
    )
  }

  writeLines(
    wise_export_readme(entries, provenance, config %||% list(), include,
      skipped = skipped
    ),
    file.path(stage, "README.md")
  )

  files <- list.files(stage)
  .export_zip(zipfile, stage, files)

  # The skipped list rides the return value as an attribute, so a caller can
  # report counts without a second pass over the stage.
  invisible(structure(manifest, skipped = skipped))
}

#' Write a flat zip archive of `files` inside `dir`
#'
#' The `zip` package is a declared dependency, so it is the primary writer: no
#' system-binary hunt, no process-global `setwd()`, C-level speed. The system
#' `zip` binary is only a fallback for environments where the package is
#' somehow absent, and failure is loud and actionable rather than silently
#' producing an unreadable file.
#'
#' @param zipfile Destination archive path.
#' @param dir     Staging directory holding the files.
#' @param files   File names within `dir`, stored flat in the archive.
#' @return Invisibly TRUE.
#' @noRd
.export_zip <- function(zipfile, dir, files) {
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(
      zipfile = zipfile, files = files, root = dir,
      mode = "cherry-pick"
    )
    return(invisible(TRUE))
  }
  if (nzchar(Sys.which("zip"))) {
    old <- setwd(dir)
    on.exit(setwd(old), add = TRUE, after = FALSE)
    status <- utils::zip(zipfile = zipfile, files = files, flags = "-rXq")
    if (identical(as.integer(status), 0L)) {
      return(invisible(TRUE))
    }
  }
  stop(
    "Cannot create the export archive: the `zip` R package is not installed ",
    "and no `zip` system utility was found. Install one of them, or use ",
    "\"Configuration only (.json)\", which needs neither.",
    call. = FALSE
  )
}


# Navbar export menu ----

#' Export dropdown for the navbar
#'
#' Deliberately not a Shiny module: the configuration snapshot is taken with a
#' single `reactiveValuesToList()` on the *root* input object, which a module's
#' namespaced input cannot see.
#'
#' @return A `bslib::nav_menu()`.
#' @noRd
export_menu_ui <- function() {
  item <- function(...) bslib::nav_item(...)
  note <- function(txt) shiny::tags$div(class = "export-menu-note", txt)

  bslib::nav_menu(
    title = shiny::tagList(shiny::icon("file-export"), "Export"),
    align = "right",
    item(shiny::tags$div(
      class = "export-menu-head",
      "Save this analysis, or hand it to someone else."
    )),
    item(shiny::downloadLink(
      "export_all",
      class = "export-menu-link",
      shiny::tagList(
        shiny::tags$span(
          class = "export-menu-title",
          shiny::icon("box-archive"), "Export all (.zip)"
        ),
        note(paste(
          "The configuration, every table as a CSV, every figure as a PNG,",
          "and a README documenting them"
        ))
      )
    )),
    item(shiny::tags$hr(class = "export-menu-sep")),
    item(shiny::downloadLink(
      "export_config",
      class = "export-menu-link",
      shiny::tagList(
        shiny::tags$span(
          class = "export-menu-title",
          shiny::icon("gear"), "Configuration only (.json)"
        ),
        note("Every setting, the random seed and each run's provenance")
      )
    )),
    item(shiny::downloadLink(
      "export_tables",
      class = "export-menu-link",
      shiny::tagList(
        shiny::tags$span(
          class = "export-menu-title",
          shiny::icon("table"), "Tables only (.zip)"
        ),
        note("Every table as a CSV")
      )
    )),
    item(shiny::downloadLink(
      "export_figures",
      class = "export-menu-link",
      shiny::tagList(
        shiny::tags$span(
          class = "export-menu-title",
          shiny::icon("chart-line"), "Figures only (.zip)"
        ),
        note("Every figure as a PNG")
      )
    )),
    item(shiny::tags$hr(class = "export-menu-sep")),
    item(shiny::actionLink(
      "import_config_open",
      class = "export-menu-link",
      shiny::tagList(
        shiny::tags$span(
          class = "export-menu-title",
          shiny::icon("file-import"), "Import configuration..."
        ),
        note("Restore analysis from a previously exported configuration.json.")
      )
    ))
  )
}

#' Wire the navbar export menu
#'
#' @param input,output,session Root session objects.
#' @param provenance Reactive returning a named list of `wise_provenance()`
#'   records, one per completed step.
#' @param seed Base random seed for the session.
#'
#' @return Invisibly NULL.
#' @noRd
export_menu_server <- function(input, output, session,
                               provenance = shiny::reactive(list()),
                               seed = WISEAPP_DEFAULT_SEED,
                               run_triggers = NULL,
                               step_results = NULL,
                               on_import = NULL) {
  pipeline_enabled <- is.list(run_triggers) && length(run_triggers) > 0L &&
    is.list(step_results) && length(step_results) > 0L

  stamp <- function() format(Sys.time(), "%Y%m%d-%H%M%S")

  snapshot <- function() {
    wise_config_snapshot(
      input,
      seed = wise_current_seed(seed, session),
      provenance = tryCatch(provenance(), error = function(e) list())
    )
  }

  bundle_handler <- function(include, tag) {
    shiny::downloadHandler(
      filename = function() paste0("wiseapp-", tag, "-", stamp(), ".zip"),
      content = function(file) {
        shiny::withProgress(message = "Preparing export...", value = 0.2, {
          items <- wise_export_items(session)
          shiny::setProgress(0.3, detail = "Writing files")
          mf <- wise_export_bundle(
            zipfile = file,
            items = items,
            config = if ("config" %in% include) snapshot() else NULL,
            provenance = tryCatch(provenance(), error = function(e) list()),
            include = include,
            # Per-item detail instead of two jumps: a 20-figure bundle takes
            # seconds, and a frozen bar reads as a hang.
            progress = function(i, total, label) {
              shiny::setProgress(0.3 + 0.6 * i / max(total, 1L),
                detail = label %||% ""
              )
            }
          )
          shiny::setProgress(1, detail = "Done")
          n_tbl <- sum(mf$kind == "table")
          n_fig <- sum(mf$kind == "figure")
          skipped <- attr(mf, "skipped") %||% list()
          n_skip <- length(skipped)
          skip_detail <- if (n_skip > 0L) {
            paste0(
              " (",
              paste(vapply(skipped, function(x) {
                paste0(
                  x$label %||% "Unnamed artefact", ": ", x$note %||% "unknown error"
                )
              }, character(1L)), collapse = "; "), ")"
            )
          } else {
            ""
          }
          shiny::showNotification(
            shiny::span(
              sprintf("Exported %d table(s) and %d figure(s)", n_tbl, n_fig),
              if (n_skip > 0L) {
                paste0(
                  "; ", n_skip, " artefact(s) could not be written - ",
                  "see the bundle README for why", skip_detail
                )
              } else {
                NULL
              },
              "."
            ),
            type = if (n_skip > 0L) "warning" else "message",
            duration = 10
          )
        })
      },
      contentType = "application/zip"
    )
  }

  output$export_all <- bundle_handler(
    c("config", "tables", "figures"), "export"
  )
  output$export_tables <- bundle_handler(c("tables"), "tables")
  output$export_figures <- bundle_handler(c("figures"), "figures")

  output$export_config <- shiny::downloadHandler(
    filename = function() paste0("wiseapp-configuration-", stamp(), ".json"),
    content = function(file) {
      jsonlite::write_json(snapshot(), file,
        auto_unbox = TRUE, pretty = TRUE,
        null = "null", digits = NA
      )
    },
    contentType = "application/json"
  )

  # Import ----
  # A file input inside a dropdown is awkward to operate (any click inside the
  # menu closes it), so the import flow opens a modal instead.

  shiny::observeEvent(input$import_config_open, {
    shiny::showModal(shiny::modalDialog(
      title = "Restore a saved analysis",
      shiny::p(
        class = "import-lede",
        "Load an exported configuration to restore previous analysis."
      ),
      shiny::tags$div(
        class = "import-prereq",
        shiny::tags$div(class = "import-prereq-title", "Before you start"),
        shiny::tags$p(
          class = "import-prereq-step",
          "Ensure you have connected to a data source from", shiny::tags$b("Overview")
        )
      ),
      shiny::fileInput("import_config_file",
        "Configuration file (.json)",
        accept = c(".json"),
        width = "100%"
      ),
      shiny::uiOutput("import_config_status"),
      shiny::uiOutput("import_pipeline_ui"),
      footer = shiny::tagList(
        shiny::uiOutput("import_action_ui", inline = TRUE),
        shiny::actionButton("import_close", "Close",
          class = "btn-outline-secondary btn-sm"
        )
      ),
      easyClose = TRUE
    ))
  })

  # Deferred-control retry state lives in a plain environment, not a
  # reactiveVal: the retry observer must not be invalidated by its own
  # bookkeeping. (A reactiveVal write from its own reader burns the whole
  # retry budget in one synchronous burst, long before renderUI() can create
  # a single control - and once exhausted it stopped reading the input set,
  # so deferred settings were never restored at all.) The state is mirrored
  # in `session$userData` so tests can seed and inspect it.
  import_state <- session$userData$wise_import_state
  if (is.null(import_state)) {
    import_state <- new.env(parent = emptyenv())
    session$userData$wise_import_state <- import_state
  }
  import_status <- shiny::reactiveVal(NULL)
  set_import_status <- function(st) {
    import_status(st)
    session$userData$wise_import_status <- st
  }
  # Non-reactive wake-up, bumped only by the import handler: the retry
  # observer runs once per import, once per heartbeat while work is
  # outstanding, and once per input change in that window - never in a
  # self-scheduled loop.
  import_wake <- shiny::reactiveVal(0L)

  output$import_config_status <- shiny::renderUI({
    st <- import_status()
    if (is.null(st)) {
      return(NULL)
    }
    shiny::div(
      class = paste("alert", st$class), role = "alert",
      style = "margin-top: 8px; font-size: 13px;", st$text
    )
  })

  # File selection only stages a validated configuration. Applying it and
  # starting the expensive pipeline require an explicit Start action.
  staged_cfg <- shiny::reactiveVal(NULL)
  previous_cfg <- shiny::reactiveVal(NULL)
  pipeline_view <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$import_config_file, {
    staged_cfg(NULL)
    f <- input$import_config_file
    if (is.null(f) || !nzchar(f$datapath %||% "")) {
      return(invisible(NULL))
    }

    cfg <- tryCatch(
      jsonlite::read_json(f$datapath, simplifyVector = TRUE),
      error = function(e) e
    )
    if (inherits(cfg, "error")) {
      set_import_status(list(
        class = "alert-danger",
        text = paste(
          "Could not read that file:",
          conditionMessage(cfg)
        )
      ))
      return(invisible(NULL))
    }
    verdict <- .import_validate(cfg, app_version = as.character(
      tryCatch(golem::get_golem_version(), error = function(e) NA_character_)
    ))
    if (!verdict$ok) {
      set_import_status(list(class = "alert-danger", text = verdict$text))
      return(invisible(NULL))
    }
    staged_cfg(cfg)
    import_state$cancelled <- FALSE
    if (!pipeline_enabled) {
      imported_seed <- suppressWarnings(as.integer(cfg$random_seed)[1L])
      session$userData$wise_analysis_seed <- if (is.na(imported_seed)) as.integer(seed) else imported_seed
      live <- names(shiny::reactiveValuesToList(input))
      res <- wise_config_apply(cfg, session, existing = live)
      import_state$pending <- if (length(res$pending)) {
        list(
          config = cfg, ids = res$pending,
          deadline = Sys.time() + .EXPORT_RETRY_SECONDS
        )
      } else {
        NULL
      }
      import_wake(import_wake() + 1L)
      set_import_status(list(
        class = "alert-success",
        text = if (length(res$pending)) {
          paste0(
            "Applied ", length(res$applied), " setting(s); ",
            length(res$pending), " more will be applied as controls appear."
          )
        } else {
          "Configuration settings restored. Data-source settings are not imported; re-run each step to refresh results."
        }
      ))
      return(invisible(NULL))
    }
    parts <- c(
      verdict$notes,
      if (!is.null(cfg$exported_at)) {
        paste0("Saved ", cfg$exported_at, ".")
      },
      "Press Start to apply the settings and re-run Steps 1 to 3."
    )
    set_import_status(list(
      class = if (length(verdict$notes)) "alert-warning" else "alert-info",
      text = paste(parts, collapse = " ")
    ))
  })

  # Make deferred controls available to the runner during every settle window.
  apply_config <- function(cfg) {
    live <- names(shiny::reactiveValuesToList(input))
    res <- wise_config_apply(cfg, session, existing = live)
    import_state$pending <- if (length(res$pending)) {
      list(
        config = cfg, ids = res$pending,
        deadline = Sys.time() + .EXPORT_RETRY_SECONDS
      )
    } else {
      NULL
    }
    import_wake(import_wake() + 1L)
    if (length(res$failed)) {
      set_import_status(list(
        class = "alert-warning",
        text = paste0(
          "Some settings could not be restored yet: ",
          paste(res$failed, collapse = ", ")
        )
      ))
    }
    res
  }

  apply_pending <- function() {
    if (isTRUE(import_state$cancelled)) {
      return(invisible(NULL))
    }
    pending <- import_state$pending
    if (is.null(pending) || !length(pending$ids)) {
      return(invisible(NULL))
    }
    live <- names(shiny::reactiveValuesToList(input))
    ready <- intersect(pending$ids, live)
    if (length(ready)) {
      sub <- pending$config
      sub$inputs <- sub$inputs[ready]
      retry <- wise_config_apply(sub, session, existing = ready)
      failed <- retry$failed %||% character(0)
    } else {
      failed <- character(0)
    }
    still <- unique(c(setdiff(pending$ids, ready), failed))
    if (length(still)) {
      import_state$pending <- list(
        config = pending$config, ids = still, deadline = pending$deadline
      )
    } else {
      import_state$pending <- NULL
    }
    invisible(NULL)
  }

  # Controls inside renderUI() may not exist until an upstream stage finishes.
  # Keep retrying in both manual and pipeline imports: the pipeline itself is
  # what causes some downstream controls to appear.
  shiny::observe({
    import_wake()
    if (isTRUE(import_state$cancelled)) {
      return(invisible(NULL))
    }
    pending <- import_state$pending
    if (is.null(pending) || !length(pending$ids)) {
      return(invisible(NULL))
    }
    apply_pending()
    pending <- import_state$pending
    if (is.null(pending) || !length(pending$ids)) {
      set_import_status(list(
        class = "alert-success",
        text = "All deferred settings restored as controls appeared."
      ))
      return(invisible(NULL))
    }
    if (Sys.time() < pending$deadline) {
      shiny::invalidateLater(1000)
      return(invisible(NULL))
    }
    set_import_status(list(
      class = "alert-warning",
      text = paste0(
        "Gave up on ", length(pending$ids),
        " setting(s) whose controls did not appear: ",
        paste(pending$ids, collapse = ", ")
      )
    ))
    import_state$pending <- NULL
  })

  runner <- pipeline_runner(
    triggers = run_triggers,
    results = step_results,
    on_settle = function() {
      apply_pending()
      import_wake(shiny::isolate(import_wake()) + 1L)
    },
    ready_to_run = function() {
      pending <- import_state$pending
      is.null(pending) || !length(pending$ids)
    },
    on_state = function(state, phase, text) {
      pipeline_view(list(state = state, phase = phase, message = text))
      pending <- import_state$pending
      if (!is.null(pending) && length(pending$ids)) {
        apply_pending()
        pending <- import_state$pending
      }
      if (!is.null(pending) && length(pending$ids)) {
        import_state$pending <- list(
          config = pending$config, ids = pending$ids,
          deadline = Sys.time() + .EXPORT_RETRY_SECONDS
        )
        import_wake(shiny::isolate(import_wake()) + 1L)
      }
    }
  )

  shiny::observeEvent(input$import_close, {
    runner$cancel()
    import_state$cancelled <- TRUE
    import_state$pending <- NULL
    staged_cfg(NULL)
    shiny::removeModal()
  })

  shiny::observeEvent(input$import_dismissed, {
    runner$cancel()
    import_state$cancelled <- TRUE
    import_state$pending <- NULL
  })

  shiny::observeEvent(input$import_run_config, {
    cfg <- staged_cfg()
    if (is.null(cfg)) {
      return(invisible(NULL))
    }
    import_state$cancelled <- FALSE
    previous_cfg(wise_config_snapshot(input, seed = wise_current_seed(seed, session)))
    imported_seed <- suppressWarnings(as.integer(cfg$random_seed)[1L])
    session$userData$wise_analysis_seed <- if (is.na(imported_seed)) {
      as.integer(seed)
    } else {
      imported_seed
    }
    apply_config(cfg)
    if (is.function(on_import)) on_import(if (is.na(imported_seed)) NULL else imported_seed)
    set_import_status(NULL)
    runner$start()
  })

  shiny::observeEvent(input$import_restore_previous, {
    previous <- previous_cfg()
    if (is.null(previous)) {
      return(invisible(NULL))
    }
    runner$cancel()
    apply_config(previous)
    if (is.function(on_import)) on_import(wise_current_seed(seed, session))
    set_import_status(list(
      class = "alert-secondary",
      text = "Previous settings restored. Re-run any stale steps as needed."
    ))
  })

  output$import_pipeline_ui <- shiny::renderUI({
    view <- pipeline_view()
    if (is.null(view) || identical(view$phase, "idle")) {
      return(NULL)
    }
    shiny::tagList(
      .pipeline_progress_ui(view$state),
      if (!is.null(view$message)) {
        shiny::div(
          class = paste("alert", switch(view$phase,
            done = "alert-success",
            failed = "alert-danger",
            cancelled = "alert-warning",
            "alert-info"
          )),
          role = "alert", view$message
        )
      }
    )
  })

  output$import_action_ui <- shiny::renderUI({
    view <- pipeline_view()
    phase <- view$phase %||% "idle"
    if (identical(phase, "running")) {
      return(shiny::tags$span(
        class = "text-muted small me-2",
        "The current operation will finish, but later steps will not run."
      ))
    }
    shiny::tagList(
      if (!is.null(previous_cfg())) {
        shiny::actionButton(
          "import_restore_previous", "Restore previous settings",
          class = "btn-outline-secondary btn-sm me-2"
        )
      },
      if (!is.null(staged_cfg())) {
        shiny::actionButton(
          "import_run_config",
          if (identical(phase, "idle")) "Start" else "Run again",
          class = "btn-primary btn-sm", icon = shiny::icon("play")
        )
      }
    )
  })

  invisible(NULL)
}
