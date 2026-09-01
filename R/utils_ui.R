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

# ---- Dynamic-input selection restore (INT-01) -------------------------------

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
