#' Info icon that opens a click-triggered popover
#'
#' Place inside a heading: `h4("My header", info_popover(p("Explanation...")))`.
#' Works in static UI, renderUI output, and insertUI/appendTab content -
#' bslib popovers render as `<bslib-popover>` web components that
#' self-initialize when inserted into the DOM.
#'
#' @param ... Popover body content (character strings and/or tags; use
#'   `shiny::p()` per paragraph).
#' @param title Optional popover header text.
#' @param docs If TRUE, append a muted "See documentation for details." line.
#' @param placement "auto", "top", "right", "bottom", "left". Default "right".
#' @noRd
info_popover <- function(..., title = NULL, docs = FALSE, placement = "right") {
  body <- htmltools::tagList(...)
  if (isTRUE(docs)) {
    body <- htmltools::tagList(
      body,
      shiny::tags$p(class = "text-muted small mb-0",
                    "See documentation for details.")
    )
  }
  bslib::popover(
    trigger = shiny::tags$span(
      class = "wise-info-icon", tabindex = "0", role = "button",
      `aria-label` = "More information",
      shiny::icon("circle-info")
    ),
    body, title = title, placement = placement
  )
}

# ---- Config flyout blocks (UI-02) ---------------------------------------------

#' Accessible plot output (UI-36)
#'
#' `shiny::plotOutput()` output has no intrinsic alt text. This wrapper adds
#' `role="img"` and a descriptive `aria-label` directly on the plot's own
#' container (no extra DOM, so bslib fill/layout behaviour is unchanged), so
#' screen readers announce what the plot shows. Where the surrounding UI
#' re-renders with the current selections (per-variable panels), the label is
#' built from the live variable names; fixed-id plots describe the plot type
#' and content.
#'
#' @param plot_id Namespaced output id for the plot.
#' @param alt     Descriptive text: what the plot shows.
#' @param ...     Forwarded to `shiny::plotOutput()` (height, width, brush,
#'   click, ...).
#'
#' @noRd
wise_plot_output <- function(plot_id, alt, ...) {
  shiny::tagAppendAttributes(
    shiny::plotOutput(plot_id, ...),
    role = "img",
    `aria-label` = alt
  )
}

#' Toggle button + anchored config flyout panel
#'
#' Shared builder for the Step 1/2 config sidebars' disclosure panels
#' (`.config-flyout` in custom.css). The button and panel are wired for the
#' shared `custom.js` behavior: the button carries `aria-expanded` /
#' `aria-controls`, the panel is marked `data-flyout-for` = toggle id and gets
#' a stable `_panel` id, and `custom.js` enforces one-open-at-a-time, moves
#' focus on open/close, closes on Escape, and positions the flyout beside its
#' own toggle instead of at a shared viewport position.
#'
#' The content stays in the DOM at all times (conditionalPanel odd/even parity,
#' as before). Note that this is not on its own enough for input defaults to
#' register: a `uiOutput()` placed in here is still *hidden*, and hidden
#' outputs are suspended until first shown. Callers that need the flyout's
#' inputs to exist before it is opened must also set
#' `outputOptions(output, "<id>", suspendWhenHidden = FALSE)` (see
#' `mod_1_06_model.R` and `mod_1_04_weather.R`).
#'
#' @param toggle_id    Namespaced input id of the toggle button.
#' @param title        Flyout header title.
#' @param ...          Flyout content, rendered below the header.
#' @param toggle_label Button label. Default "Configure".
#'
#' @noRd
config_flyout_block <- function(toggle_id, title, ..., toggle_label = "Configure") {
  panel_id <- paste0(toggle_id, "_panel")
  shiny::tags$div(
    class = "config-flyout-anchor",
    shiny::actionButton(
      toggle_id, toggle_label,
      icon  = shiny::icon("sliders"),
      class = "btn-outline-primary btn-sm config-flyout-toggle",
      style = "margin-bottom: 10px;",
      `aria-expanded` = "false",
      `aria-controls` = panel_id
    ),
    shiny::conditionalPanel(
      condition = paste0("input['", toggle_id, "'] % 2 == 1"),
      class     = "config-flyout",
      id        = panel_id,
      `data-flyout-for` = toggle_id,
      shiny::tags$div(
        class = "config-flyout-header",
        shiny::tags$h6(title),
        shiny::tags$button(
          type    = "button",
          class   = "btn-close",
          `aria-label` = "Close",
          onclick = sprintf("document.getElementById('%s').click();", toggle_id)
        )
      ),
      ...
    )
  )
}

# ---- Number formatting for displayed figures (UI-32) -------------------------

#' Format a number for display at one decimal place
#'
#' One rule for every dynamically computed figure the app shows - the Step 3
#' sidebar's social-protection preview and the diagnostics tab's transfer
#' summary included - so the same quantity never appears at two precisions.
#' Values are rounded, not truncated, and thousands are separated.
#'
#' @param x       Numeric vector.
#' @param digits  Decimal places. Default 1.
#' @param prefix,suffix Optional strings placed either side of the number
#'   (e.g. `prefix = "$"`, `suffix = "%"`).
#' @param na Text used for non-finite values.
#'
#' @return A character vector the same length as `x`.
#' @noRd
fmt_num <- function(x, digits = 1, prefix = "", suffix = "", na = "\u2014") {
  x <- suppressWarnings(as.numeric(x))
  out <- vapply(x, function(v) {
    if (!is.finite(v)) return(na)
    paste0(prefix,
           formatC(round(v, digits), format = "f", digits = digits,
                   big.mark = ","),
           suffix)
  }, character(1))
  out
}

#' Format a count for display
#'
#' Whole units (households, individuals, firms) are counts, so they get
#' thousands separators and no decimals - even when survey weights make the
#' underlying value fractional.
#'
#' @param x Numeric vector.
#' @param na Text used for non-finite values.
#' @return A character vector.
#' @noRd
fmt_count <- function(x, na = "\u2014") {
  x <- suppressWarnings(as.numeric(x))
  vapply(x, function(v) {
    if (!is.finite(v)) return(na)
    formatC(round(v), format = "d", big.mark = ",")
  }, character(1))
}


# ---- Nav-header step status (UI-47) ------------------------------------------
#
# Steps can be visited in any order, and results survive a move to another tab,
# so the navbar is the only place where the state of every step is visible at
# once. Each step tab carries a small badge:
#
#   none  - the step has not produced results yet; no badge, navbar stays quiet
#   done  - results exist and match the current inputs (check mark)
#   stale - results exist but an input has changed since (reload arrow)
#
# "stale" reuses the per-step run signatures already maintained for the
# in-page stale banners (INT-08), so the badge and the banner can never
# disagree. A stale step is still fully usable - the badge asks for a re-run,
# it does not lock anything.

#' Placeholder for a step's status badge in the navbar
#'
#' @param output_id Output id, matched by `render_step_badge()` in the server.
#' @return A `span` output container, safe to nest inside a nav link.
#' @noRd
step_badge_ui <- function(output_id) {
  shiny::uiOutput(output_id, container = shiny::tags$span, inline = TRUE,
                  class = "nav-step-status-slot")
}

#' Build the badge for one step state
#'
#' @param state One of `"none"`, `"done"`, `"stale"`.
#' @param step_label Human name of the step, used in the accessible text.
#' @return A `span` tag, or NULL for `"none"`.
#' @noRd
step_status_badge <- function(state, step_label = "This step") {
  # The glyphs are decorative: aria-hidden takes them out of the
  # accessibility tree (which also suppresses the aria-label shiny::icon()
  # always attaches), and the visually-hidden text carries the meaning.
  deco <- function(name) shiny::icon(name, `aria-hidden` = "true")

  if (identical(state, "done")) {
    tip <- paste0(step_label, ": complete \u2014 results are up to date.")
    return(shiny::tags$span(
      class = "nav-step-status nav-step-status-done",
      title = tip,
      deco("check"),
      shiny::tags$span(class = "visually-hidden", tip)
    ))
  }
  if (identical(state, "stale")) {
    tip <- paste0(step_label, ": inputs changed \u2014 re-run to refresh ",
                  "the results.")
    return(shiny::tags$span(
      class = "nav-step-status nav-step-status-stale",
      title = tip,
      deco("rotate"),
      shiny::tags$span(class = "visually-hidden", tip)
    ))
  }
  NULL
}

#' Classify a step as none / done / stale
#'
#' @param has_result Reactive returning the step's result object (NULL until it
#'   has run), or a logical.
#' @param is_stale   Reactive returning TRUE when the stored result no longer
#'   matches the live inputs.
#'
#' @return A reactive returning `"none"`, `"done"` or `"stale"`.
#' @noRd
step_status <- function(has_result, is_stale = NULL) {
  # Fail fast on a mis-wired badge. If an upstream module renames the key this
  # reads, the argument arrives as NULL - and without this the badge would
  # simply sit on "none" (or, for a missing staleness flag, permanently on
  # "done"), which is worse than an error: it looks like working UI.
  if (!is.function(has_result)) {
    stop("step_status(): `has_result` must be a reactive, got ",
         class(has_result)[1], ".", call. = FALSE)
  }
  if (!is.null(is_stale) && !is.function(is_stale)) {
    stop("step_status(): `is_stale` must be a reactive or NULL, got ",
         class(is_stale)[1], ".", call. = FALSE)
  }

  shiny::reactive({
    res <- tryCatch(has_result(), error = function(e) NULL)
    done <- if (is.logical(res) && length(res) == 1L) isTRUE(res) else !is.null(res)
    if (!done) return("none")
    stale <- if (is.null(is_stale)) FALSE else
      isTRUE(tryCatch(is_stale(), error = function(e) FALSE))
    if (stale) "stale" else "done"
  })
}

#' Render a step's navbar status badge
#'
#' @param has_result,is_stale Reactives, as for `step_status()`.
#' @param step_label Human name of the step, used in the accessible text.
#'
#' @return A `renderUI` expression to assign to the matching output id.
#' @noRd
render_step_badge <- function(has_result, is_stale = NULL,
                              step_label = "This step") {
  status <- step_status(has_result, is_stale)
  shiny::renderUI(step_status_badge(status(), step_label))
}


# ---- Table CSV export (UI-45) ------------------------------------------------
#
# Every table in the app offers the same export affordance: one small, quiet
# "Download CSV" control. For DT tables that is the Buttons extension, driven
# by the two helpers below; for the handful of hand-built HTML tables it is
# `csv_download_link()` over a `downloadHandler`. Both render as
# `.wise-csv-btn` so they look identical wherever they appear (custom.css).

#' DT `buttons` spec for a single, discreet CSV export
#'
#' @param filename Base name of the downloaded file, without extension.
#' @param enabled  When FALSE, returns NULL so the button is omitted (used to
#'   withhold exports while results are stale - INT-08).
#'
#' @return A list suitable for `DT::datatable(options = list(buttons = ...))`,
#'   or NULL.
#' @noRd
wise_csv_button <- function(filename, enabled = TRUE) {
  if (!isTRUE(enabled)) return(NULL)
  list(list(
    extend        = "csv",
    text          = "Download CSV",
    filename      = filename,
    className     = "wise-csv-btn",
    # Export every row, not just the visible page; keep any active search.
    exportOptions = list(modifier = list(page = "all"))
  ))
}

#' Add the Buttons placeholder to a DT `dom` string
#'
#' @param dom A DataTables `dom` string (e.g. "t", "lfrtip").
#' @return The same string with a leading "B" if it lacked one.
#' @noRd
wise_csv_dom <- function(dom = "lfrtip") {
  if (grepl("B", dom, fixed = TRUE)) dom else paste0("B", dom)
}

#' Small "Download CSV" link for a non-DT table
#'
#' Pairs with a `downloadHandler()` registered under the same output id. Use
#' for hand-built HTML tables (`renderTable()` / `renderUI()`), which have no
#' DataTables toolbar to hang a button off.
#'
#' @param output_id Namespaced id of the matching `downloadHandler` output.
#' @param label     Link text. Default "Download CSV".
#'
#' @return A `downloadLink` tag.
#' @noRd
csv_download_link <- function(output_id, label = "Download CSV") {
  shiny::downloadLink(
    output_id,
    label = shiny::tagList(shiny::icon("download"), label),
    class = "wise-csv-btn wise-csv-link"
  )
}

#' `downloadHandler` writing a data frame to CSV
#'
#' @param filename_base Base name of the file, without extension.
#' @param data_fun      Function of no arguments returning a data frame, or
#'   NULL when there is nothing to export.
#'
#' @return A shiny download handler.
#' @noRd
csv_download_handler <- function(filename_base, data_fun) {
  shiny::downloadHandler(
    filename = function() {
      paste0(filename_base, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      df <- tryCatch(data_fun(), error = function(e) NULL)
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        df <- data.frame(Note = "No data available")
      }
      utils::write.csv(df, file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )
}

# ---- Run signatures & stale-state marking (INT-08) ---------------------------

#' Canonicalise a value for identity comparison in a run signature.
#'
#' Data frames become lists of columns and nested lists recurse, so two
#' independently-built signature components holding equal data compare
#' `identical()` even when captured at different times.
#' @noRd
.sig_plain <- function(x) {
  if (is.null(x) || is.atomic(x)) return(x)
  if (is.data.frame(x)) return(lapply(x, .sig_plain))
  if (is.list(x)) return(lapply(x, .sig_plain))
  as.character(x)
}

#' Stale-results banner (INT-08).
#'
#' Rendered above a result surface while its run signature no longer matches
#' the current upstream inputs. Results stay visible (they are expensive) but
#' are explicitly labelled as describing an earlier configuration.
#'
#' @param step        Optional label for the affected surface.
#' @param note        Optional extra sentence; the export-gating surfaces pass
#'   "Interpretation and exports are disabled until then."
#' @noRd
.stale_banner <- function(step = NULL, note = NULL) {
  step_txt <- if (!is.null(step)) paste0(" (", step, ")") else NULL
  note_txt <- if (!is.null(note)) paste0(" ", note) else NULL
  shiny::div(
    class = "alert alert-warning",
    role  = "alert",
    style = "margin-bottom: 10px;",
    shiny::tags$b("\u26a0 Results are out of date", step_txt, "."),
    "Upstream inputs changed after this run, so the results below were",
    "produced by an earlier configuration and no longer describe the",
    "current selections. Re-run to refresh them.", note_txt
  )
}

# ---- Dynamic-input selection restore (INT-01) -------------------------------

#' Build a label-lookup function bound to a fixed variable-metadata frame
#' (INT-05). Result renderers pass this instead of looking labels up in the
#' live `variable_list`, so re-labelling a variable cannot rewrite the
#' description of an already-fitted result.
#'
#' @param vl Data frame (or NULL) with `name` and `label` columns.
#' @return `function(var_name) -> label`.
#' @noRd
.label_lookup <- function(vl) {
  force(vl)
  function(var_name) {
    if (is.null(vl)) return(var_name)
    idx <- match(var_name, vl$name)
    if (is.na(idx)) var_name else as.character(vl$label[idx])
  }
}

#' Compute `selected` for a re-rendered dynamic input.
#'
#' renderUI-hosted inputs are rebuilt from scratch whenever their choice set
#' changes, which resets them to hardcoded defaults and silently wipes user
#' selections (INT-01). Call this inside the renderUI, passing the previous
#' selection read with `shiny::isolate(input$...)`; values that still exist
#' in the new choice set are restored, invalid values are dropped, and an
#' empty result falls back to the historical default.
#'
#' @param prev     Previous selection (character or NULL on first render).
#' @param choices  Valid choice values (coerced to character).
#' @param fallback Default used when no previous value survives. May be NULL.
#' @return Character vector (unnamed) or `fallback`.
#' @noRd
.restore_selection <- function(prev, choices, fallback) {
  choices <- as.character(choices)
  if (is.null(prev) || length(prev) == 0) return(fallback)
  keep <- unname(as.character(prev)[as.character(prev) %in% choices])
  if (length(keep) == 0) fallback else keep
}

#' Restore a numeric input's previous value across re-renders (INT-01).
#'
#' Numeric companion to `.restore_selection()`: returns `prev` when it is a
#' single finite value inside `[min - tol, max + tol]`, else `fallback`.
#' @noRd
.restore_numeric <- function(prev, min, max, fallback, tol = 1e-8) {
  if (is.null(prev) || length(prev) != 1L || !is.finite(prev)) return(fallback)
  if (prev < (min - tol) || prev > (max + tol)) return(fallback)
  prev
}

# ---- Busy guards (REACT-02) -------------------------------------------------
# Long actions (data loads, model fits, simulations) use a module-local
# `running` reactiveVal so double-clicks cannot re-enter the observer while
# the previous run is still executing. Triggering controls are disabled for
# the duration via shinyjs-style input attribute updates (no shinyjs dep).

#' Create a module-local busy guard.
#'
#' Usage inside moduleServer:
#'   guard <- .busy_guard(session)
#'   observeEvent(input$run, {
#'     guard$begin(); on.exit(guard$end(), add = TRUE)
#'     ...
#'   })
#' `busy()` is the reactiveVal backing the guard (FALSE = idle). The trigger
#' buttons are disabled/enabled via shiny::updateActionButton whenever the
#' state flips.
#'
#' @param session  Module session.
#' @param ...      Named input ids of action buttons to disable while running
#'   (unquoted, evaluated via updateActionButton in the module namespace).
#' @noRd
.busy_guard <- function(session, ...) {
  running <- shiny::reactiveVal(FALSE)
  btn_ids <- vapply(substitute(list(...))[-1L], as.character, character(1))

  enable_disable <- function(state) {
    for (btn in btn_ids) {
      tryCatch(
        shinyjs_disable_button(session$ns(btn), enabled = !state),
        error = function(e) NULL
      )
    }
  }
  # Buttons may not exist yet when the guard flips (renderUI-hosted) - the
  # tryCatch above swallows that; re-apply on each transition.
  shiny::observeEvent(running(), enable_disable(running()), ignoreInit = TRUE)

  list(
    is_running = running,
    begin      = function() {
      if (isTRUE(running())) return(FALSE)
      running(TRUE)
      TRUE
    },
    end        = function() running(FALSE)
  )
}

#' Toggle an action button's disabled state without shinyjs.
#' Sets the disabled attribute server-side; Shiny propagates it to the DOM.
#' @noRd
shinyjs_disable_button <- function(input_id, enabled = TRUE) {
  shiny::updateActionButton(
    session = shiny::getDefaultReactiveDomain(),
    inputId = input_id,
    disabled = !enabled
  )
}

# ---- Wave / Survey Year Toggle Slider ----------------------------------------

#' Survey year / wave toggle-style slider input
#'
#' Renders an inline segmented toggle-style slider for selecting survey years or waves.
#' Handles few or many survey years gracefully with compact styling and horizontal
#' overflow scrolling.
#'
#' @param inputId Input ID.
#' @param choices Named character vector of choices (values = wave keys,
#'   names = display labels).
#' @param selected Currently selected value.
#' @param label Optional control label.
#' @param width Optional width.
#'
#' @noRd
wave_toggle_slider <- function(inputId, choices, selected = NULL, label = NULL, width = NULL) {
  if (is.null(selected) && length(choices) > 0) {
    selected <- unname(choices)[1]
  }
  rb <- shiny::radioButtons(
    inputId  = inputId,
    label    = label,
    choices  = choices,
    selected = selected,
    inline   = TRUE,
    width    = width
  )
  htmltools::tagAppendAttributes(
    rb,
    class = "toggle-slider wave-toggle-slider"
  )
}

#' Build choices vector for survey wave toggle slider
#'
#' Formats wave choices: "All" (if include_all = TRUE) followed by
#' survey years. When multiple economies are selected, prefixes with the country
#' code (e.g. "MWI 2010", "TZA 2012"); when only one economy is selected,
#' uses the year directly (e.g. "2010", "2013", "2016").
#'
#' @param wave_df Data frame from `survey_wave_list()` with columns `key`, `year`,
#'   `code`, `economy`.
#' @param include_all Logical; whether to include an "All" choice at the start.
#'
#' @return Named character vector for use in `wave_toggle_slider()`.
#' @noRd
wave_slider_choices <- function(wave_df, include_all = TRUE) {
  if (is.null(wave_df) || nrow(wave_df) == 0) return(character(0))
  multi_country <- length(unique(wave_df$code)) > 1
  labels <- if (multi_country) {
    paste(wave_df$code, wave_df$year)
  } else {
    as.character(wave_df$year)
  }
  wave_choices <- stats::setNames(wave_df$key, labels)
  if (isTRUE(include_all)) {
    c(stats::setNames("all", "All"), wave_choices)
  } else {
    wave_choices
  }
}

