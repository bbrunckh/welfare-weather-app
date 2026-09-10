# ============================================================================ #
# Export bundle: configuration, tables, figures and a metadata document.       #
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
# ============================================================================ #


# ---------------------------------------------------------------------------- #
# Registry                                                                      #
# ---------------------------------------------------------------------------- #

#' Access (creating on first use) the session's export registry
#'
#' @param session A Shiny session; defaults to the current reactive domain.
#' @return An environment with an `items` list, or NULL outside a session.
#' @noRd
.export_store <- function(session = shiny::getDefaultReactiveDomain()) {
  if (is.null(session)) return(NULL)
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
                                 session = shiny::getDefaultReactiveDomain()) {
  store <- .export_store(session)
  if (is.null(store)) return(invisible(key))
  stopifnot(is.function(fun))
  store$items[[key]] <- list(
    key         = key,
    label       = label,
    step        = as.integer(step),
    kind        = match.arg(kind, c("table", "figure")),
    fun         = fun,
    description = description %||% label,
    width       = width,
    height      = height
  )
  invisible(key)
}

#' @rdname wise_export_register
#' @noRd
wise_export_table <- function(key, label, step, fun, description = NULL,
                              session = shiny::getDefaultReactiveDomain()) {
  wise_export_register(key, label, step, "table", fun, description,
                       session = session)
}

#' @rdname wise_export_register
#' @noRd
wise_export_figure <- function(key, label, step, fun, description = NULL,
                               width = 10, height = 6,
                               session = shiny::getDefaultReactiveDomain()) {
  wise_export_register(key, label, step, "figure", fun, description,
                       width = width, height = height, session = session)
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
  if (is.null(store) || !length(store$items)) return(list())
  items <- store$items
  ord <- order(
    vapply(items, `[[`, integer(1), "step"),
    match(vapply(items, `[[`, character(1), "kind"), c("table", "figure")),
    vapply(items, `[[`, character(1), "key")
  )
  items[ord]
}


# ---------------------------------------------------------------------------- #
# File naming                                                                   #
# ---------------------------------------------------------------------------- #

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


# ---------------------------------------------------------------------------- #
# Configuration state                                                           #
# ---------------------------------------------------------------------------- #

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
  # Buttons in configurations exported before they were excluded by type.
  # The class-based rule in wise_config_snapshot() covers anything written
  # since; these keep older files from queueing controls that can never be
  # restored.
  "^survey_stats$", "^weather_stats$", "_btn$",
  "^apply_", "^import_config", "^show_lasso",
  "_toggle$", "_open$", "^hide_",
  "_rows_current$", "_rows_all$", "_rows_selected$", "_columns_selected$",
  "_cells_selected$", "_search$", "_state$", "_cell_clicked$",
  # DT registers its own controls as Shiny inputs under auto-numbered ids
  # (DataTables_Table_0_length, ..._3_length). They are per-render UI state,
  # not analysis configuration, and the number is assigned by render order -
  # so the same table is Table_0 in one session and Table_3 in the next, and
  # the value could never be restored to the control it came from anyway.
  "^DataTables_Table_", "_cell_edit$", "_state_change$",
  "^plotly_", "_click$", "_hover$", "_brush$", "_dblclick$",
  "^\\.clientdata", "^sidebar", "^accordion$", "_bounds$", "_center$",
  "_zoom$", "_shape_", "_marker_", "_groups$",
  "secret", "key", "token", "password", "credential", "client_id", "tenant"
)

# How long the import retry keeps waiting for renderUI() controls to appear
# before it gives up, audibly (see export_menu_server()).
#
# UI-70: this is measured from the last sign of progress, not from the import.
# Controls that only exist once a step has run (the outcome picker, the
# currency selector) cannot appear until that step finishes, and a model fit
# alone outlasts any fixed window - so every one of them was abandoned while
# the pipeline was still working. `export_menu_server()` pushes the deadline
# out on each pipeline transition; the window then applies from the moment the
# run stops making progress.
.EXPORT_RETRY_SECONDS <- 120

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
    # UI-70: action buttons cannot be restored at all - Shiny's button binding
    # ignores `value` in receiveMessage - so carrying them achieves nothing and
    # they sit in the deferred queue until it gives up, reported to the user as
    # settings that could not be applied. Excluded by type rather than by name,
    # which catches every button however it is called (survey_stats,
    # outcome_stats_btn, run_model, ...).
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
    exported_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    app_version   = tryCatch(as.character(golem::get_golem_version()),
                             error = function(e) NA_character_),
    r_version     = paste0(R.version$major, ".", R.version$minor),
    random_seed   = as.integer(seed),
    provenance    = provenance,
    inputs        = vals
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
  if (!length(vals)) return(invisible(list(applied = character(0),
                                           pending = character(0))))

  # UI-72: filter on the way in as well as on the way out. A file exported
  # before a given control was excluded still lists it, and an id that can
  # never be restored - a button, a DataTables page-length - would otherwise
  # sit in the deferred queue until the retry window closed and then be
  # reported to the user as a setting that could not be applied.
  vals <- vals[.export_keep_input(names(vals))]
  if (!length(vals)) return(invisible(list(applied = character(0),
                                           pending = character(0))))

  ids <- names(vals)
  can <- if (length(existing)) ids %in% existing else rep(TRUE, length(ids))

  # A control that exists and turns out to be an action button is skipped for
  # the same reason: Shiny's binding ignores `value`, so the send is a no-op
  # that would be counted as a restored setting.
  is_button <- function(id) {
    v <- tryCatch(session$input[[id]], error = function(e) NULL)
    inherits(v, "shinyActionButtonValue")
  }

  applied <- character(0)
  for (id in ids[can]) {
    if (is_button(id)) next
    tryCatch(
      session$sendInputMessage(id, list(value = vals[[id]])),
      error = function(e) NULL
    )
    applied <- c(applied, id)
  }
  invisible(list(applied = applied, pending = ids[!can]))
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
  if (is.null(cfg$inputs)) {
    return(reject(paste(
      "That file has no `inputs` section - it does not look like a",
      "WISE-APP configuration export.")))
  }
  v <- cfg$wiseapp_config_version
  if (is.null(v)) {
    return(reject(paste(
      "That file has no `wiseapp_config_version` field - it does not look",
      "like a WISE-APP configuration export.")))
  }
  if (isTRUE(v > 1)) {
    return(reject(paste0(
      "That file was written in configuration version ", v, "; this app ",
      "reads version 1. Update WISE-APP, then re-import.")))
  }
  notes <- character(0)
  if (!is.null(cfg$app_version) && !is.na(app_version) &&
      !identical(as.character(cfg$app_version), app_version)) {
    notes <- c(notes, paste0(
      "Exported from WISE-APP ", cfg$app_version, "; this session runs ",
      app_version, " - settings may differ."))
  }
  list(ok = TRUE, text = character(0), notes = notes)
}

#' Compare an imported value with the control's current value
#'
#' `all.equal()` rather than `identical()`: the JSON round-trip turns integers
#' into doubles, and a restored setting equal to what the control already
#' shows is not a restoration - it must not be reported as one.
#'
#' @noRd
.import_value_same <- function(a, b) {
  if (is.null(a) || is.null(b)) return(is.null(a) && is.null(b))
  isTRUE(all.equal(a, b))
}


# ---------------------------------------------------------------------------- #
# Bundle assembly                                                               #
# ---------------------------------------------------------------------------- #

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
  if (!is.data.frame(df) || !ncol(df)) return(df)
  as.data.frame(
    lapply(df, function(col) {
      # A matrix column: one string per row. digits = 15 keeps the double
      # precision write.csv() itself would give - format()'s 7-digit default
      # silently rounded specification values (bin breaks, polynomials).
      if (is.matrix(col)) {
        return(apply(col, 1L, function(r)
          paste(format(r, trim = TRUE, digits = 15), collapse = "; ")))
      }
      if (is.list(col)) {
        return(vapply(col, function(x) {
          if (is.null(x) || !length(x)) return(NA_character_)
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

  value <- tryCatch(item$fun(), error = function(e) e)
  if (inherits(value, "error")) {
    # A req() throw (shiny.silent.error) means "this surface is not ready" -
    # the step has not produced anything to export. That is not a failure:
    # it is skipped exactly like a NULL return, with no note. Any other
    # error is a genuine failure: the artefact is named in the README's
    # "Not exported" section rather than vanishing beside the not-run ones.
    if (inherits(value, "shiny.silent.error")) return(NULL)
    msg <- conditionMessage(value)
    if (!nzchar(msg)) msg <- "artefact could not be produced"
    return(fail(msg))
  }
  if (is.null(value)) return(NULL)

  path <- file.path(dir, file)

  if (identical(item$kind, "table")) {
    # The write itself is guarded, not just the builder. An unwritable table
    # used to propagate out of the download handler, so Shiny answered the
    # request with its HTML error page - which the browser then saved under
    # the .zip/.csv name. One bad table must cost that table, not the bundle.
    return(tryCatch({
      if (!is.data.frame(value)) value <- as.data.frame(value)
      if (nrow(value) == 0L) return(NULL)
      flat <- .export_flatten_df(value)
      utils::write.csv(flat, path, row.names = FALSE, na = "")
      list(status = "ok", rows = nrow(flat), cols = ncol(flat))
    }, error = function(e) fail(conditionMessage(e))))
  }

  # Figures: ggplot objects render through ggsave with the ragg AGG device -
  # the same renderer the on-screen plots use (shiny.useragg), so fonts and
  # antialiasing in the PNG match what the user saw, and ragg is faster than
  # the grDevices cairo path. Anything else is skipped rather than guessed at.
  tryCatch({
    if (inherits(value, "ggplot")) {
      ggplot2::ggsave(path, plot = value, width = item$width,
                      height = item$height, dpi = 150, bg = "white",
                      device = ragg::agg_png)
    } else {
      return(NULL)
    }
    list(status = "ok", rows = NA_integer_, cols = NA_integer_)
  }, error = function(e) fail(conditionMessage(e)))
}

#' Write the machine-readable manifest
#' @noRd
.export_manifest_df <- function(entries) {
  if (!length(entries)) {
    return(data.frame(file = character(0), kind = character(0),
                      step = character(0), title = character(0),
                      description = character(0), rows = integer(0),
                      columns = integer(0), stringsAsFactors = FALSE))
  }
  data.frame(
    file        = vapply(entries, `[[`, character(1), "file"),
    kind        = vapply(entries, `[[`, character(1), "kind"),
    step        = vapply(entries, `[[`, character(1), "step_label"),
    title       = vapply(entries, `[[`, character(1), "label"),
    description = vapply(entries, `[[`, character(1), "description"),
    rows        = vapply(entries, function(e) e$rows %||% NA_integer_, integer(1)),
    columns     = vapply(entries, function(e) e$cols %||% NA_integer_, integer(1)),
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


# ---------------------------------------------------------------------------- #
# Metadata document                                                             #
# ---------------------------------------------------------------------------- #

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
    if ("config" %in% included)
      "| `configuration.json` | Every input in the app at export time, plus the random seed and per-step run provenance. Re-import it through Export -> Import configuration to restore this analysis. |",
    "| `manifest.csv` | Machine-readable index of every file below: name, kind, step, title, description, and row/column counts. |",
    "| `README.md` | This document. |",
    if ("tables" %in% included)
      "| `*.csv` | One file per table the session produced. |",
    if ("figures" %in% included)
      "| `*.png` | One file per figure the session produced. |",
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
    files <- c(files,
      paste(
        "No tables or figures were exported. Artefacts are only included",
        "once the step that produces them has been run."
      ), "")
  } else {
    by_step <- split(entries, vapply(entries, `[[`, character(1), "step_label"))
    for (sl in names(by_step)) {
      files <- c(files, paste0("### ", sl), "")
      for (e in by_step[[sl]]) {
        size <- if (!is.na(e$rows %||% NA)) {
          sprintf(" (%s %s x %s %s)",
                  fmt_count(e$rows), if (identical(e$rows, 1L)) "row" else "rows",
                  e$cols, if (identical(e$cols, 1L)) "column" else "columns")
        } else ""
        files <- c(files,
          sprintf("- **`%s`** - %s%s", e$file, e$description, size))
      }
      files <- c(files, "")
    }
  }

  prov <- c("## Provenance", "")
  prov_recs <- Filter(Negate(is.null), provenance)
  if (!length(prov_recs)) {
    prov <- c(prov,
      "No step has produced results yet, so there is no run to describe.", "")
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
        "Run signature"  = p$run_signature,
        "Data source"    = if (length(src)) paste(
          paste0(names(src), "=", vapply(src, function(v)
            paste(as.character(v), collapse = "/"), character(1))),
          collapse = "; ") else NULL,
        "Survey version" = p$survey_version,
        "Outcome"        = p$outcome,
        "Weather"        = p$weather,
        "Specification"  = p$model_spec,
        "Engine"         = p$engine,
        "Observations"   = if (is.na(p$n_observations %||% NA)) NULL
                           else fmt_count(p$n_observations),
        "Random seed"    = p$random_seed,
        # A stale run's numbers still describe its own inputs, but the
        # session's current inputs have moved on: say so, rather than letting
        # a diff-hunting reader wonder why a re-run disagrees.
        "Results stale"  = if (isTRUE(p$stale))
          "yes - current inputs have changed since this run" else NULL,
        "App version"    = p$app_version
      )
      extra_keys <- setdiff(names(p), c(
        "step", "step_label", "run_signature", "source", "survey_version",
        "outcome", "weather", "model_spec", "engine", "n_observations",
        "random_seed", "app_version", "fallbacks", "stale"))
      for (k in extra_keys) kv[[k]] <- p[[k]]
      for (k in names(kv)) {
        v <- kv[[k]]
        if (is.null(v) || !length(v)) next
        v <- paste(as.character(v), collapse = ", ")
        if (!nzchar(v) || identical(v, "NA")) next
        prov <- c(prov, sprintf("- **%s:** %s", k, v))
      }
      if (length(p$fallbacks)) {
        prov <- c(prov, sprintf("- **Specification fallbacks:** %s",
                                paste(p$fallbacks, collapse = "; ")))
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
      vapply(skipped, function(sk) sprintf("- **%s** (%s) - %s",
                                           sk$label, sk$step_label, sk$note),
             character(1)),
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
      "- Import restores control values, not results. Runs are never replayed",
      "automatically, because re-fitting can take minutes."
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
        "on-screen plot size; text rendering matches what the app shows."
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


# ---------------------------------------------------------------------------- #
# Bundle writer                                                                 #
# ---------------------------------------------------------------------------- #

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
    (identical(it$kind, "table")  && "tables"  %in% include) ||
    (identical(it$kind, "figure") && "figures" %in% include)
  }, items)

  entries <- list()
  skipped <- list()
  # Numbering follows the registry's stable order (step, then tables before
  # figures): a surface that contributes nothing - not run, or a named
  # failure - leaves a gap instead of renumbering every later file, so two
  # bundles of the same analysis stay file-by-file diffable even when steps
  # differ in what they produced.
  for (i in seq_along(wanted)) {
    it   <- wanted[[i]]
    file <- .export_filename(i, it$step, it$key, it$kind)
    if (is.function(progress)) progress(i, length(wanted), it$label)
    res  <- .export_write_item(it, stage, file)
    if (is.null(res)) next
    if (identical(res$status, "error")) {
      # Recorded and reported in the README rather than dropped in silence -
      # a missing file with no explanation is worse than a named failure.
      skipped[[length(skipped) + 1L]] <- list(
        label = it$label, step_label = .export_step_label(it$step),
        note = res$note
      )
      warning("[wise_export_bundle] skipping '", it$key, "': ", res$note)
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
                   row.names = FALSE, na = "")

  if ("config" %in% include && !is.null(config)) {
    jsonlite::write_json(config, file.path(stage, "configuration.json"),
                         auto_unbox = TRUE, pretty = TRUE, null = "null",
                         digits = NA)
  }

  writeLines(
    wise_export_readme(entries, provenance, config %||% list(), include,
                       skipped = skipped),
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
    zip::zip(zipfile = zipfile, files = files, root = dir,
             mode = "cherry-pick")
    return(invisible(TRUE))
  }
  if (nzchar(Sys.which("zip"))) {
    old <- setwd(dir)
    on.exit(setwd(old), add = TRUE, after = FALSE)
    status <- utils::zip(zipfile = zipfile, files = files, flags = "-rXq")
    if (identical(as.integer(status), 0L)) return(invisible(TRUE))
  }
  stop(
    "Cannot create the export archive: the `zip` R package is not installed ",
    "and no `zip` system utility was found. Install one of them, or use ",
    "\"Configuration only (.json)\", which needs neither.",
    call. = FALSE
  )
}


# ---------------------------------------------------------------------------- #
# Navbar export menu                                                            #
# ---------------------------------------------------------------------------- #

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
    title = shiny::tagList(shiny::icon("file-export"), "Export/Import"),
    align = "right",

    item(shiny::tags$div(
      class = "export-menu-head",
      "Save this analysis, or hand it to someone else."
    )),

    item(shiny::downloadLink(
      "export_all",
      class = "export-menu-link",
      shiny::tagList(
        shiny::tags$span(class = "export-menu-title",
                         shiny::icon("box-archive"), "Export all (.zip)"),
        note(paste(
          "The configuration, every table as a CSV, every figure as a PNG,",
          "and a metadata document explaining each file name and its",
          "contents."
        ))
      )
    )),

    item(shiny::tags$hr(class = "export-menu-sep")),

    item(shiny::downloadLink(
      "export_config",
      class = "export-menu-link",
      shiny::tagList(
        shiny::tags$span(class = "export-menu-title",
                         shiny::icon("gear"), "Configuration only (.json)"),
        note("Every setting, the random seed and each run's provenance.")
      )
    )),

    item(shiny::downloadLink(
      "export_tables",
      class = "export-menu-link",
      shiny::tagList(
        shiny::tags$span(class = "export-menu-title",
                         shiny::icon("table"), "Tables only (.zip)"),
        note("Every table as a CSV, with the metadata document.")
      )
    )),

    item(shiny::downloadLink(
      "export_figures",
      class = "export-menu-link",
      shiny::tagList(
        shiny::tags$span(class = "export-menu-title",
                         shiny::icon("chart-line"), "Figures only (.zip)"),
        note("Every figure as a PNG, with the metadata document.")
      )
    )),

    item(shiny::tags$hr(class = "export-menu-sep")),

    item(shiny::actionLink(
      "import_config_open",
      class = "export-menu-link",
      shiny::tagList(
        shiny::tags$span(class = "export-menu-title",
                         shiny::icon("file-import"), "Import configuration..."),
        note("Restore settings from a previously exported configuration.json.")
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
                               on_import = NULL,
                               run_triggers = list(),
                               step_results = list()) {

  stamp <- function() format(Sys.time(), "%Y%m%d-%H%M%S")

  snapshot <- function() {
    wise_config_snapshot(
      input, seed = seed,
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
            zipfile    = file,
            items      = items,
            config     = if ("config" %in% include) snapshot() else NULL,
            provenance = tryCatch(provenance(), error = function(e) list()),
            include    = include,
            # Per-item detail instead of two jumps: a 20-figure bundle takes
            # seconds, and a frozen bar reads as a hang.
            progress   = function(i, total, label) {
              shiny::setProgress(0.3 + 0.6 * i / max(total, 1L),
                                 detail = label %||% "")
            }
          )
          shiny::setProgress(1, detail = "Done")
          n_tbl  <- sum(mf$kind == "table")
          n_fig  <- sum(mf$kind == "figure")
          n_skip <- length(attr(mf, "skipped"))
          shiny::showNotification(
            shiny::span(
              sprintf("Exported %d table(s) and %d figure(s)", n_tbl, n_fig),
              if (n_skip > 0L)
                paste0("; ", n_skip, " artefact(s) could not be written - ",
                       "see the bundle README for why")
              else NULL,
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
    c("config", "tables", "figures"), "export")
  output$export_tables <- bundle_handler(c("tables"), "tables")
  output$export_figures <- bundle_handler(c("figures"), "figures")

  output$export_config <- shiny::downloadHandler(
    filename = function() paste0("wiseapp-configuration-", stamp(), ".json"),
    content = function(file) {
      jsonlite::write_json(snapshot(), file, auto_unbox = TRUE, pretty = TRUE,
                           null = "null", digits = NA)
    },
    contentType = "application/json"
  )

  # ---- Import ---------------------------------------------------------------
  # A file input inside a dropdown is awkward to operate (any click inside the
  # menu closes it), so the import flow opens a modal instead.

  shiny::observeEvent(input$import_config_open, {
    shiny::showModal(shiny::modalDialog(
      title = "Restore a saved analysis",
      shiny::p(
        class = "import-lede",
        "Load a configuration you exported earlier and re-run the analysis ",
        "from it. Your current work is untouched until you choose to start."
      ),

      shiny::tags$div(
        class = "import-prereq",
        shiny::tags$div(class = "import-prereq-title", "Before you start"),
        shiny::tags$p(
          class = "import-prereq-step",
          "Open ", shiny::tags$b("Overview"), " in the navigation bar at the ",
          "top and connect to your data source."
        ),
        shiny::tags$p(
          class = "import-prereq-note",
          "Everything else is done for you: the sample and weather variables ",
          "named in the file are loaded, then the model is fitted and both ",
          "simulations run. The one thing this window cannot do is connect ",
          "to data or enter credentials on your behalf."
        )
      ),

      shiny::fileInput("import_config_file",
                       "Configuration file (.json)", accept = c(".json"),
                       width = "100%"),
      shiny::uiOutput("import_config_status"),
      shiny::uiOutput("import_pipeline_ui"),
      footer = shiny::tagList(
        shiny::uiOutput("import_action_ui", inline = TRUE),
        # UI-71: a real button, not modalButton(), so closing can stop the
        # run. `custom.js` mirrors this for Escape and backdrop clicks.
        shiny::actionButton("import_close", "Close",
                            class = "btn-outline-secondary btn-sm")
      ),
      easyClose = TRUE
    ))
  })

  shiny::observeEvent(input$import_close, {
    runner$cancel()
    shiny::removeModal()
  })

  # Escape and backdrop dismissals never reach the Close button; custom.js
  # sends this instead so every way out of the dialog stops the pipeline.
  shiny::observeEvent(input$import_dismissed, {
    runner$cancel()
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
    if (is.null(st)) return(NULL)
    shiny::div(class = paste("alert", st$class), role = "alert",
               style = "margin-top: 8px; font-size: 13px;", st$text)
  })

  # UI-69: selecting a file stages it - parsed, validated, described - but
  # applies nothing. The app is only touched when the user presses Run, which
  # makes closing the preview a genuine no-op rather than an undo.
  staged_cfg <- shiny::reactiveVal(NULL)
  # Configuration in force before a run, so it can be put back.
  previous_cfg <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$import_config_file, {
    staged_cfg(NULL)
    f <- input$import_config_file
    if (is.null(f) || !nzchar(f$datapath %||% "")) return(invisible(NULL))

    cfg <- tryCatch(
      jsonlite::read_json(f$datapath, simplifyVector = TRUE),
      error = function(e) e
    )
    if (inherits(cfg, "error")) {
      set_import_status(list(class = "alert-danger",
                             text = paste("Could not read that file:",
                                          conditionMessage(cfg))))
      return(invisible(NULL))
    }
    verdict <- .import_validate(cfg, app_version = as.character(
      tryCatch(golem::get_golem_version(), error = function(e) NA_character_)))
    if (!verdict$ok) {
      set_import_status(list(class = "alert-danger", text = verdict$text))
      return(invisible(NULL))
    }

    staged_cfg(cfg)
    exported <- cfg$exported_at %||% NULL
    set_import_status(list(
      class = if (length(verdict$notes)) "alert-warning" else "alert-info",
      # UI-72: settings counts told the user nothing they could act on. What
      # matters is that the file is usable and what pressing Start will do.
      text = paste(c(
        verdict$notes,
        if (!is.null(exported)) paste0("Saved ", exported, ".") ,
        "Press Start to load the data it names and re-run Steps 1 to 3."
      ), collapse = " ")
    ))
  })

  # ---- Applying, and running the pipeline ---------------------------------

  apply_config <- function(cfg) {
    live <- names(shiny::reactiveValuesToList(input))
    res  <- wise_config_apply(cfg, session, existing = live)
    # Honesty: the server cannot see client-side send failures, and a value
    # equal to what the control already shows is not a restoration - so the
    # count reported is settings actually changed, never sends attempted.
    n_changed <- if (length(res$applied)) sum(vapply(res$applied, function(id) {
      !.import_value_same(input[[id]], cfg$inputs[[id]])
    }, logical(1))) else 0L

    import_state$pending <- if (length(res$pending)) list(
      config = cfg, ids = res$pending,
      deadline = Sys.time() + .EXPORT_RETRY_SECONDS
    ) else NULL
    import_wake(import_wake() + 1L)

    list(applied = res$applied, pending = res$pending, n_changed = n_changed)
  }

  pipeline_view <- shiny::reactiveVal(NULL)
  runner <- pipeline_runner(
    triggers = run_triggers,
    results  = step_results,
    # Re-arm the deferred apply during every settle window.
    on_settle = function() import_wake(shiny::isolate(import_wake()) + 1L),
    on_state = function(state, phase, message) {
      pipeline_view(list(state = state, phase = phase, message = message))
      # UI-70: a stage finishing is what makes the next step's controls exist,
      # so every transition both re-arms the retry pass and pushes its
      # deadline out. Without this the window expired mid-fit and every
      # control not yet rendered was given up on.
      p <- import_state$pending
      if (!is.null(p) && length(p$ids)) {
        import_state$pending <- list(
          config = p$config, ids = p$ids,
          deadline = Sys.time() + .EXPORT_RETRY_SECONDS
        )
        import_wake(shiny::isolate(import_wake()) + 1L)
      }
    }
  )

  shiny::observeEvent(input$import_run_config, {
    cfg <- staged_cfg()
    if (is.null(cfg)) return(invisible(NULL))

    # Snapshot what is in force so the user can put it back. Results are not
    # captured: each step owns its result privately, so a cancelled run
    # leaves whatever completed marked stale against the restored settings
    # rather than reverting it.
    previous_cfg(wise_config_snapshot(input, seed = seed))

    out <- apply_config(cfg)
    if (is.function(on_import) && (out$n_changed > 0L || length(out$pending))) {
      on_import()
    }
    set_import_status(NULL)   # the stage list below is the feedback now
    runner$start()
  })

  shiny::observeEvent(input$import_restore_previous, {
    prev <- previous_cfg()
    if (is.null(prev)) return(invisible(NULL))
    out <- apply_config(prev)
    runner$reset()
    if (is.function(on_import)) on_import()
    set_import_status(list(
      class = "alert-secondary",
      text = paste(
        "Your previous settings are back. Any step that had already re-run",
        "keeps its new results and is marked out of date in the navigation",
        "bar - re-run it when you are ready."
      )
    ))
  })

  # ---- Modal panels --------------------------------------------------------

  output$import_pipeline_ui <- shiny::renderUI({
    v <- pipeline_view()
    if (is.null(v) || identical(v$phase, "idle")) return(NULL)
    shiny::tagList(
      .pipeline_progress_ui(v$state),
      if (!is.null(v$message)) shiny::div(
        class = paste("alert", switch(v$phase,
                                      done = "alert-success",
                                      failed = "alert-danger",
                                      "alert-info")),
        role = "alert", style = "margin-top: 8px; font-size: 13px;",
        v$message
      )
    )
  })

  output$import_action_ui <- shiny::renderUI({
    v <- pipeline_view()
    phase <- v$phase %||% "idle"
    if (identical(phase, "running")) {
      # Say plainly what Close does here, because it is not obvious and it is
      # not instant: the step in flight cannot be interrupted.
      return(shiny::tags$span(
        class = "import-running-note me-2",
        shiny::icon("circle-info"),
        " Closing stops the run after the current step. That step has to ",
        "finish before the app responds again."
      ))
    }
    shiny::tagList(
      if (!is.null(previous_cfg())) shiny::actionButton(
        "import_restore_previous", "Undo - restore my previous settings",
        class = "btn-outline-secondary btn-sm me-2"
      ),
      if (!is.null(staged_cfg())) shiny::actionButton(
        "import_run_config",
        if (identical(phase, "idle")) "Start" else "Run again",
        class = "btn-primary btn-sm me-2", icon = shiny::icon("play")
      )
    )
  })

  # Controls inside renderUI() do not exist until their upstream data has
  # loaded, so a single pass silently drops them. While anything is
  # outstanding, re-apply on (a) any input change - the observer reads the
  # whole live set, so a control that has appeared re-arms it - and (b) a
  # slow heartbeat for controls that appear without an input change. It gives
  # up, naming the ids, when the window closes rather than retrying forever.
  shiny::observe({
    import_wake()
    p <- import_state$pending
    if (is.null(p) || !length(p$ids)) return(invisible(NULL))
    live <- names(shiny::reactiveValuesToList(input))
    ready <- intersect(p$ids, live)
    still <- setdiff(p$ids, live)
    if (length(ready)) {
      sub <- p$config
      sub$inputs <- sub$inputs[ready]
      wise_config_apply(sub, session, existing = ready)
    }
    if (length(still) && Sys.time() < p$deadline) {
      import_state$pending <- list(config = p$config, ids = still,
                                   deadline = p$deadline)
      if (length(ready)) {
        set_import_status(list(class = "alert-success", text = paste0(
          "Applied ", length(ready), " more setting(s) as controls ",
          "appeared; ", length(still), " still pending.")))
      }
      shiny::invalidateLater(1000)
      return(invisible(NULL))
    }
    if (length(still)) {
      set_import_status(list(class = "alert-warning", text = paste0(
        "Some settings could not be restored because their controls were ",
        "never shown in this session - usually the sample and outcome ",
        "pickers, which need a data source connected in Overview and Step 1 ",
        "loaded first. Affected: ",
        paste(utils::head(vapply(still, function(id) sub("^.*-", "", id),
                          character(1)), 6L), collapse = ", "),
        if (length(still) > 6L) paste0(" and ", length(still) - 6L, " more"),
        ". Set those by hand, or connect the data source and import again.")))
    } else {
      set_import_status(list(class = "alert-success", text = paste0(
        "All settings restored.")))
    }
    import_state$pending <- NULL
  })

  invisible(NULL)
}


# ---------------------------------------------------------------------------- #
# Configuration pipeline runner (UI-69)                                         #
#                                                                              #
# Importing settings without running anything leaves the app in a state no one  #
# asked for: the sidebar says one thing, the results say another, and the       #
# restored values are lost the moment a renderUI-built control rebuilds with    #
# its own defaults. The runner closes that gap - read the file, apply it, and   #
# drive Step 1 -> 2 -> 3 to completion, reporting progress in the modal.        #
#                                                                              #
# What is and is not transactional:                                            #
#                                                                              #
#   Settings   fully. The current configuration is snapshotted before anything  #
#              is applied and can be put back exactly.                         #
#   Results    not. Each step owns its result in a private reactiveVal and      #
#              appends its own output tabs; once Step 1 re-fits, the previous   #
#              fit is gone. Cancelling therefore restores the settings and      #
#              leaves whatever ran marked stale against them - which the navbar #
#              badges already show. Nothing is applied at all until the user    #
#              presses Run, so closing the preview is a true no-op.             #
# ---------------------------------------------------------------------------- #

# Seconds a stage may take before the runner stops waiting. Fits and
# simulations on large surveys are minutes, not seconds.
.PIPELINE_STAGE_TIMEOUT <- 900

#' The stages the runner drives, in order
#' @noRd
.pipeline_stages <- function() list(
  list(key = "load_survey",  label = "Load the survey sample"),
  list(key = "load_weather", label = "Load the weather variables"),
  list(key = "step1",        label = "Step 1 - Fit the welfare model"),
  list(key = "step2",        label = "Step 2 - Run the climate simulation"),
  list(key = "step3",        label = "Step 3 - Run the policy simulation")
)

# UI-74: how long to wait between "a stage may fire" and firing it. Restored
# settings reach a control through session$sendInputMessage(), which is a
# client round-trip: the server does not hold the new value until the browser
# echoes it back. Firing a stage in the same flush as the apply made it read
# the *old* values - the sample loaded was whatever was selected before the
# import, not what the file said. Each stage also creates the controls the
# next one's settings need (the outcome picker exists only after the survey
# loads), so the pending re-apply has to get a turn here too.
.PIPELINE_SETTLE_SECONDS <- 3

#' Settle window in force, overridable for tests via
#' `options(wiseapp.pipeline_settle = 0)`.
#' @noRd
.pipeline_settle_seconds <- function() {
  getOption("wiseapp.pipeline_settle", .PIPELINE_SETTLE_SECONDS)
}

#' Render the runner's stage list
#'
#' @param state Named list: `status` per stage key, one of "pending",
#'   "running", "done", "failed", "skipped".
#' @noRd
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
    lapply(.pipeline_stages(), function(st) {
      status <- state[[st$key]] %||% "pending"
      shiny::tags$li(
        class = paste0("pipeline-stage pipeline-stage-", status),
        icons[[status]],
        shiny::tags$span(st$label),
        if (identical(status, "running")) {
          shiny::tags$span(class = "pipeline-note", "running\u2026")
        }
      )
    })
  )
}

#' Drive Step 1 -> 2 -> 3 after a configuration import
#'
#' A small state machine. Each stage fires its module's run trigger and then
#' waits for that step's result reactive to change; a stage that does not
#' complete within `.PIPELINE_STAGE_TIMEOUT` is reported rather than left
#' hanging, because a failed run publishes no result and would otherwise wait
#' for ever.
#'
#' @param triggers Named list of `reactiveVal`s, one per stage key, that the
#'   step modules observe.
#' @param results  Named list of reactives returning each step's stored result.
#' @param on_state Callback invoked with (state, phase, message) on every
#'   transition; drives the modal.
#'
#' @return A list with `start()` and `state()`.
#' @noRd
pipeline_runner <- function(triggers, results, on_state = NULL,
                            on_settle = NULL) {
  stages <- .pipeline_stages()
  keys   <- vapply(stages, `[[`, character(1), "key")
  # Only stages that were actually wired can be driven. A caller that supplies
  # none (tests of the import half, say) gets an inert runner rather than an
  # error from calling a NULL result reactive.
  keys <- keys[vapply(keys, function(k) {
    is.function(triggers[[k]]) && is.function(results[[k]])
  }, logical(1))]

  state    <- shiny::reactiveVal(stats::setNames(
    rep(list("pending"), length(keys)), keys))
  phase    <- shiny::reactiveVal("idle")   # idle | running | done | failed
  message  <- shiny::reactiveVal(NULL)
  active   <- shiny::reactiveVal(NULL)     # key of the stage being awaited
  deadline <- shiny::reactiveVal(NULL)
  # Result value seen when the active stage started; the stage is complete
  # when its result differs from this.
  baseline <- shiny::reactiveVal(NULL)

  emit <- function() {
    if (is.function(on_state)) on_state(state(), phase(), message())
  }
  set_stage <- function(key, status) {
    st <- state(); st[[key]] <- status; state(st)
  }

  # A stage waiting for its settle window; fired by the observer below once
  # the window closes, so restored values have made the round trip first.
  settling     <- shiny::reactiveVal(NULL)
  settle_until <- shiny::reactiveVal(NULL)

  n_fired <- 0L

  # UI-76: a trigger is disarmed once its stage has been consumed. Left set,
  # it kept the merged run signal non-NULL - so when a lazily rendered Run
  # button later registered (opening Step 3 for the first time makes its
  # button report 0), the signal re-evaluated to a new list and the step ran
  # again with nobody clicking. NULL is what ignoreNULL blocks.
  clear_trigger <- function(key) {
    trig <- triggers[[key]]
    if (is.function(trig) && !is.null(shiny::isolate(trig()))) trig(NULL)
  }
  clear_all_triggers <- function() for (k in keys) clear_trigger(k)

  fire_now <- function(key) {
    settling(NULL); settle_until(NULL)
    active(key)
    baseline(tryCatch(results[[key]](), error = function(e) NULL))
    deadline(Sys.time() + .PIPELINE_STAGE_TIMEOUT)
    n_fired <<- n_fired + 1L
    triggers[[key]](list(n = n_fired, at = Sys.time()))
    emit()
  }

  fire <- function(key) {
    set_stage(key, "running")
    settling(key)
    settle_until(Sys.time() + .pipeline_settle_seconds())
    if (is.function(on_settle)) on_settle()
    emit()
  }

  shiny::observe({
    key <- settling()
    until <- settle_until()
    if (is.null(key) || is.null(until)) return(invisible(NULL))
    if (Sys.time() >= until) {
      fire_now(key)
    } else {
      # Give the pending re-apply another turn while we wait: a control that
      # appeared during the window gets its value before the stage reads it.
      if (is.function(on_settle)) on_settle()
      shiny::invalidateLater(750)
    }
  })

  advance <- function(from) {
    idx <- match(from, keys)
    if (is.na(idx) || idx >= length(keys)) {
      active(NULL); deadline(NULL)
      phase("done")
      message(paste("Data loaded and all three steps re-run. Close this",
                    "window to see the results."))
      emit()
      return(invisible(NULL))
    }
    fire(keys[[idx + 1L]])
  }

  fail <- function(key, why) {
    set_stage(key, "failed")
    later <- keys[seq_along(keys) > match(key, keys)]
    for (k in later) set_stage(k, "skipped")
    active(NULL); deadline(NULL)
    clear_all_triggers()
    phase("failed")
    message(why)
    emit()
  }

  # Completion watcher: a stage is done when its step publishes a result that
  # differs from the one standing when the stage started.
  for (k in keys) local({
    key <- k
    shiny::observeEvent(results[[key]](), {
      # `active` is cleared by cancel(), so a stage that publishes after the
      # user stopped the run does not start the next one.
      if (!identical(active(), key)) return(invisible(NULL))
      if (identical(results[[key]](), baseline())) return(invisible(NULL))
      set_stage(key, "done")
      clear_trigger(key)
      emit()
      advance(key)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)
  })

  # Timeout watcher. A failed run publishes no result, so without this the
  # runner would wait for ever on a stage that already gave up.
  shiny::observe({
    dl <- deadline()
    if (is.null(dl) || is.null(active())) return(invisible(NULL))
    shiny::invalidateLater(2000)
    if (Sys.time() > dl) {
      fail(active(), paste0(
        "This step has not finished after ",
        round(.PIPELINE_STAGE_TIMEOUT / 60), " minutes, so the run has ",
        "stopped waiting. Check the notifications for an error, then run the ",
        "remaining steps from their own tabs."
      ))
    }
  })

  # UI-71: closing the dialog stops the pipeline. A stage already executing
  # cannot be interrupted - the fit and the simulations run synchronously
  # inside their modules' observers - so cancelling means "do not start the
  # next one". Whatever finished stays, and the navbar badges mark anything
  # the settings have since moved past.
  cancel <- function() {
    if (!identical(phase(), "running")) return(invisible(NULL))
    running_now <- active()
    active(NULL); deadline(NULL)
    settling(NULL); settle_until(NULL)
    clear_all_triggers()
    st <- state()
    for (k in keys) {
      if (identical(st[[k]], "running") || identical(st[[k]], "pending")) {
        st[[k]] <- "skipped"
      }
    }
    state(st)
    phase("cancelled")
    message(paste0(
      "Stopped.",
      if (!is.null(running_now)) paste0(
        " The step that was already running has to finish before the app",
        " responds again - nothing after it was started."
      ),
      " Whatever completed is kept; anything now out of date is marked with",
      " a reload symbol in the navigation bar."
    ))
    emit()
  }

  list(
    cancel = cancel,
    start = function() {
      if (!length(keys)) {
        phase("failed")
        message("No steps are wired to the runner in this session.")
        emit()
        return(invisible(NULL))
      }
      state(stats::setNames(rep(list("pending"), length(keys)), keys))
      phase("running")
      message(NULL)
      emit()
      fire(keys[[1]])
    },
    reset = function() {
      state(stats::setNames(rep(list("pending"), length(keys)), keys))
      phase("idle"); message(NULL); active(NULL); deadline(NULL)
      clear_all_triggers()
      emit()
    },
    state   = state,
    phase   = phase,
    message = message
  )
}
