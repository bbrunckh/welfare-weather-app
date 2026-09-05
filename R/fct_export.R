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
#'   ggplot / recordedplot (figures). Return NULL when unavailable.
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
# user's current layout, so they are dropped from the exported config.
.EXPORT_INPUT_DROP <- c(
  "^run_model$", "^run_sim$", "^run_policy_sim$", "^load_", "^refresh",
  "_toggle$", "_open$", "^show_", "^hide_",
  "_rows_current$", "_rows_all$", "_rows_selected$", "_columns_selected$",
  "_cells_selected$", "_search$", "_state$", "_cell_clicked$",
  "^plotly_", "_click$", "_hover$", "_brush$", "_dblclick$",
  "^\\.clientdata", "^sidebar", "^accordion$", "_bounds$", "_center$",
  "_zoom$", "_shape_", "_marker_", "_groups$"
)

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
  ids <- names(vals)
  can <- if (length(existing)) ids %in% existing else rep(TRUE, length(ids))
  for (id in ids[can]) {
    tryCatch(
      session$sendInputMessage(id, list(value = vals[[id]])),
      error = function(e) NULL
    )
  }
  invisible(list(applied = ids[can], pending = ids[!can]))
}


# ---------------------------------------------------------------------------- #
# Bundle assembly                                                               #
# ---------------------------------------------------------------------------- #

#' Materialise one registry item to a file
#'
#' @return A list describing what was written, or NULL when the artefact had
#'   no data (a surface the user never ran).
#' @noRd
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
      # A matrix column: one string per row.
      if (is.matrix(col)) {
        return(apply(col, 1L, function(r)
          paste(format(r, trim = TRUE), collapse = "; ")))
      }
      if (is.list(col)) {
        return(vapply(col, function(x) {
          if (is.null(x) || !length(x)) return(NA_character_)
          x <- unlist(x, use.names = FALSE)
          paste(format(x, trim = TRUE), collapse = "; ")
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

  value <- tryCatch(item$fun(), error = function(e) {
    structure(list(), class = "wise_export_error", message = conditionMessage(e))
  })
  if (inherits(value, "wise_export_error")) {
    return(fail(attr(value, "message") %||% "artefact could not be produced"))
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

  # Figures: ggplot objects render through ggsave; base-R plots recorded with
  # recordPlot() replay onto a device. Anything else is skipped rather than
  # guessed at.
  tryCatch({
    if (inherits(value, "ggplot")) {
      ggplot2::ggsave(path, plot = value, width = item$width,
                      height = item$height, dpi = 150, bg = "white")
    } else if (inherits(value, "recordedplot")) {
      grDevices::png(path, width = item$width, height = item$height,
                     units = "in", res = 150, bg = "white")
      on.exit(grDevices::dev.off(), add = TRUE)
      grDevices::replayPlot(value)
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
      "| `configuration.json` | Every input in the app at export time, plus the random seed and per-step run provenance. Re-import it through Export → Import configuration to restore this analysis. |",
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
      "can be diffed file by file."
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
          sprintf("- **`%s`** — %s%s", e$file, e$description, size))
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
        "App version"    = p$app_version
      )
      extra_keys <- setdiff(names(p), c(
        "step", "step_label", "run_signature", "source", "survey_version",
        "outcome", "weather", "model_spec", "engine", "n_observations",
        "random_seed", "app_version", "fallbacks"))
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
      vapply(skipped, function(sk) sprintf("- **%s** (%s) — %s",
                                           sk$label, sk$step_label, sk$note),
             character(1)),
      ""
    )
  }

  repro <- c(
    "## Reproducing this analysis",
    "",
    "1. Open WISE-APP and connect to the data source named under Provenance.",
    "2. Choose **Export → Import configuration** and select `configuration.json`.",
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

  L(hdr, contents, naming, files, omitted, prov, repro)
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
                               include = c("config", "tables", "figures")) {
  stage <- file.path(tempdir(), paste0("wise-export-",
                                       format(Sys.time(), "%Y%m%d%H%M%S"),
                                       "-", sample.int(1e6, 1)))
  dir.create(stage, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(stage, recursive = TRUE), add = TRUE)

  wanted <- Filter(function(it) {
    (identical(it$kind, "table")  && "tables"  %in% include) ||
    (identical(it$kind, "figure") && "figures" %in% include)
  }, items)

  entries <- list()
  skipped <- list()
  idx <- 0L
  for (it in wanted) {
    idx <- idx + 1L
    file <- .export_filename(idx, it$step, it$key, it$kind)
    res  <- .export_write_item(it, stage, file)
    if (is.null(res)) {
      # Nothing to export from this surface (the step has not been run);
      # keep numbering contiguous.
      idx <- idx - 1L
      next
    }
    if (identical(res$status, "error")) {
      idx <- idx - 1L
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

  invisible(manifest)
}

#' Write a flat zip archive of `files` inside `dir`
#'
#' `utils::zip()` shells out to a system `zip` binary, which is not guaranteed
#' on a deployment host. Falls back to the `zip` package when it is installed
#' (it usually is, as an rsconnect dependency) and fails with an actionable
#' message when neither is available - rather than silently producing an
#' unreadable file.
#'
#' @param zipfile Destination archive path.
#' @param dir     Staging directory holding the files.
#' @param files   File names within `dir`, stored flat in the archive.
#' @return Invisibly TRUE.
#' @noRd
.export_zip <- function(zipfile, dir, files) {
  if (nzchar(Sys.which("zip"))) {
    old <- setwd(dir)
    on.exit(setwd(old), add = TRUE, after = FALSE)
    status <- utils::zip(zipfile = zipfile, files = files, flags = "-r9Xq")
    if (identical(as.integer(status), 0L)) return(invisible(TRUE))
  }
  if (requireNamespace("zip", quietly = TRUE)) {
    zip::zip(zipfile = zipfile, files = files, root = dir,
             mode = "cherry-pick")
    return(invisible(TRUE))
  }
  stop(
    "Cannot create the export archive: no `zip` system utility was found and ",
    "the `zip` R package is not installed. Install one of them, or use ",
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
                         shiny::icon("file-import"), "Import configuration…"),
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
                               seed = WISEAPP_DEFAULT_SEED) {

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
        shiny::withProgress(message = "Preparing export…", value = 0.2, {
          items <- wise_export_items(session)
          shiny::setProgress(0.4, detail = "Writing files")
          wise_export_bundle(
            zipfile    = file,
            items      = items,
            config     = if ("config" %in% include) snapshot() else NULL,
            provenance = tryCatch(provenance(), error = function(e) list()),
            include    = include
          )
          shiny::setProgress(1, detail = "Done")
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
      title = "Import configuration",
      shiny::p(
        "Select a ", shiny::tags$code("configuration.json"),
        " exported from WISE-APP. Settings are restored; results are not ",
        "re-computed, so re-run each step afterwards."
      ),
      shiny::p(
        class = "text-muted small",
        "Connect to the data source first. Controls that only appear once ",
        "data has loaded are restored as the interface fills in."
      ),
      shiny::fileInput("import_config_file", NULL, accept = c(".json"),
                       width = "100%"),
      shiny::uiOutput("import_config_status"),
      footer = shiny::modalButton("Close"),
      easyClose = TRUE
    ))
  })

  # Ids that could not be applied yet because their control does not exist.
  pending <- shiny::reactiveVal(NULL)

  import_status <- shiny::reactiveVal(NULL)
  output$import_config_status <- shiny::renderUI({
    st <- import_status()
    if (is.null(st)) return(NULL)
    shiny::div(class = paste("alert", st$class), role = "alert",
               style = "margin-top: 8px; font-size: 13px;", st$text)
  })

  shiny::observeEvent(input$import_config_file, {
    f <- input$import_config_file
    if (is.null(f) || !nzchar(f$datapath %||% "")) return(invisible(NULL))

    cfg <- tryCatch(
      jsonlite::read_json(f$datapath, simplifyVector = TRUE),
      error = function(e) e
    )
    if (inherits(cfg, "error")) {
      import_status(list(class = "alert-danger",
                         text = paste("Could not read that file:",
                                      conditionMessage(cfg))))
      return(invisible(NULL))
    }
    if (is.null(cfg$inputs)) {
      import_status(list(
        class = "alert-danger",
        text = paste("That file has no `inputs` section - it does not look",
                     "like a WISE-APP configuration export.")))
      return(invisible(NULL))
    }

    live <- names(shiny::reactiveValuesToList(input))
    res  <- wise_config_apply(cfg, session, existing = live)
    pending(list(config = cfg, ids = res$pending, tries = 0L))

    import_status(list(
      class = "alert-success",
      text = paste0(
        "Restored ", length(res$applied), " setting(s).",
        if (length(res$pending))
          paste0(" ", length(res$pending), " more will be applied as the ",
                 "matching controls appear.") else "",
        " Re-run each step to refresh results."
      )
    ))
  })

  # Controls inside renderUI() do not exist until their upstream data has
  # loaded, so a single pass would silently drop them. Re-apply whatever is
  # still outstanding whenever the set of live inputs changes, and give up
  # after a bounded number of attempts rather than retrying forever.
  shiny::observe({
    p <- pending()
    if (is.null(p) || !length(p$ids) || p$tries > 40L) return(invisible(NULL))
    live <- names(shiny::reactiveValuesToList(input))
    still <- setdiff(p$ids, live)
    ready <- intersect(p$ids, live)
    if (length(ready)) {
      sub <- p$config
      sub$inputs <- sub$inputs[ready]
      wise_config_apply(sub, session, existing = ready)
    }
    pending(list(config = p$config, ids = still, tries = p$tries + 1L))
  })

  invisible(NULL)
}
