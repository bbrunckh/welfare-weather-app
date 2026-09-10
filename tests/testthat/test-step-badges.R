# ============================================================================ #
# tests/testthat/test-step-badges.R                                            #
# UI-47: navbar step badges - a check once a step's results match its inputs,  #
#        a reload arrow once an input has changed, nothing before it has run.  #
# ============================================================================ #

library(testthat)
library(shiny)

badge_html <- function(state) {
  b <- step_status_badge(state, "Step 1")
  if (is.null(b)) NA_character_ else as.character(b)
}

test_that("step_status_badge renders nothing before a step has run", {
  expect_null(step_status_badge("none"))
  # Anything unrecognised is treated as "no badge" rather than erroring.
  expect_null(step_status_badge("something-else"))
})

test_that("a completed step gets a check, a stale step gets a reload arrow", {
  done <- badge_html("done")
  expect_match(done, "nav-step-status-done", fixed = TRUE)
  expect_match(done, "fa-check", fixed = TRUE)
  expect_false(grepl("fa-rotate", done, fixed = TRUE))

  stale <- badge_html("stale")
  expect_match(stale, "nav-step-status-stale", fixed = TRUE)
  expect_match(stale, "fa-rotate", fixed = TRUE)
  expect_false(grepl("fa-check", stale, fixed = TRUE))
})

test_that("badges carry an accessible name and a hover tooltip", {
  for (state in c("done", "stale")) {
    h <- badge_html(state)
    expect_match(h, "visually-hidden", fixed = TRUE)
    expect_match(h, "Step 1", fixed = TRUE)
    expect_match(h, "title=", fixed = TRUE)
    # The glyph itself is decorative - the hidden text carries the meaning.
    expect_match(h, 'aria-hidden="true"', fixed = TRUE)
  }
  expect_match(badge_html("done"), "up to date", fixed = TRUE)
  expect_match(badge_html("stale"), "re-run", fixed = TRUE)
})

test_that("the step label appears in the badge text", {
  h <- as.character(step_status_badge("stale", "Step 2 (climate scenarios)"))
  expect_match(h, "Step 2 (climate scenarios)", fixed = TRUE)
})


test_that("step_status classifies none / done / stale from result + staleness", {
  result <- reactiveVal(NULL)
  stale  <- reactiveVal(FALSE)
  status <- step_status(result, stale)

  isolate(expect_equal(status(), "none"))

  # A fit lands: complete.
  result(list(fit = "x"))
  isolate(expect_equal(status(), "done"))

  # An upstream input changes: still complete, but out of date.
  stale(TRUE)
  isolate(expect_equal(status(), "stale"))

  # Re-running clears the flag.
  stale(FALSE)
  isolate(expect_equal(status(), "done"))
})

test_that("step_status treats a missing result as none regardless of staleness", {
  status <- step_status(reactiveVal(NULL), reactiveVal(TRUE))
  isolate(expect_equal(status(), "none"))
})

test_that("step_status accepts a logical has_result", {
  flag <- reactiveVal(FALSE)
  status <- step_status(flag, reactiveVal(FALSE))
  isolate(expect_equal(status(), "none"))
  flag(TRUE)
  isolate(expect_equal(status(), "done"))
})

test_that("step_status survives upstream reactives that req() out", {
  # Steps read reactives that req() internally before anything is loaded; the
  # navbar must stay quiet rather than erroring the whole session.
  blocked <- reactive(shiny::req(FALSE))
  status  <- step_status(blocked, reactive(shiny::req(FALSE)))
  isolate(expect_equal(status(), "none"))

  # A present result with a throwing staleness flag reads as done, not broken.
  status2 <- step_status(reactiveVal(list(1)), reactive(stop("boom")))
  isolate(expect_equal(status2(), "done"))
})

test_that("step_status defaults to done when no staleness flag is supplied", {
  status <- step_status(reactiveVal(list(1)))
  isolate(expect_equal(status(), "done"))
})


test_that("a mis-wired badge errors rather than sitting on a stale state", {
  # A renamed upstream API key arrives here as NULL. Silently rendering
  # "none" forever - or worse, a permanent check - would look like working UI.
  expect_error(step_status(NULL), "must be a reactive")
  expect_error(step_status("model_fit"), "must be a reactive")
  expect_error(step_status(reactiveVal(1), is_stale = "fit_stale"),
               "must be a reactive or NULL")
})

test_that("the badge placeholder is a span, so it nests inside a nav link", {
  h <- as.character(step_badge_ui("step1_badge"))
  expect_match(h, "^<span", fixed = FALSE)
  expect_match(h, 'id="step1_badge"', fixed = TRUE)
  expect_match(h, "shiny-html-output", fixed = TRUE)
  expect_false(grepl("<div", h, fixed = TRUE))
})

test_that("every step tab carries a badge slot and a stable tab value", {
  # app_ui() stamps the navbar with the packaged golem version, which is read
  # relative to the working directory - not resolvable from tests/testthat.
  local_mocked_bindings(get_golem_version = function(...) "0.0.0",
                        .package = "golem")

  h <- as.character(htmltools::renderTags(app_ui(list()))$html)
  for (id in c("step1_badge", "step2_badge", "step3_badge")) {
    expect_match(h, paste0('id="', id, '"'), fixed = TRUE)
  }
  # nav_panel value defaults to title; without an explicit value the badge
  # placeholder would be serialised into data-value.
  for (v in c("overview", "step1", "step2", "step3")) {
    expect_match(h, paste0('data-value="', v, '"'), fixed = TRUE)
  }
  expect_false(grepl("data-value=\"Step 1", h, fixed = TRUE))
})


# ---- UI-68: a configuration import invalidates every stored result ----------
#
# Importing a configuration.json replaces the settings but never replays the
# runs, so results produced under the previous configuration are out of date
# the moment the file lands. The per-step stale flags cannot see this: they
# compare a stored run signature against live inputs, and an imported value
# that happens to match what was already there moves neither.

test_that("an import marks a completed step stale until it is re-run", {
  srv <- function(input, output, session) {
    result   <- reactiveVal(NULL)
    imported <- reactiveVal(0L)
    session$userData$h <- list(
      result = result, imported = imported,
      status = step_status(result,
                           stale_after_import(result, reactiveVal(FALSE),
                                              imported))
    )
  }
  testServer(srv, {
    h <- session$userData$h
    session$flushReact()
    expect_equal(isolate(h$status()), "none")

    h$result(list(fit = 1)); session$flushReact()
    expect_equal(isolate(h$status()), "done")

    h$imported(1L); session$flushReact()
    expect_equal(isolate(h$status()), "stale")

    # Re-running the step is what clears it.
    h$result(list(fit = 2)); session$flushReact()
    expect_equal(isolate(h$status()), "done")
  })
})

test_that("each step clears independently as it is re-run", {
  srv <- function(input, output, session) {
    imported <- reactiveVal(0L)
    r1 <- reactiveVal(list(a = 1)); r2 <- reactiveVal(list(b = 1))
    session$userData$h <- list(
      imported = imported, r1 = r1, r2 = r2,
      s1 = step_status(r1, stale_after_import(r1, NULL, imported)),
      s2 = step_status(r2, stale_after_import(r2, NULL, imported))
    )
  }
  testServer(srv, {
    h <- session$userData$h
    session$flushReact()
    h$imported(1L); session$flushReact()
    expect_equal(isolate(h$s1()), "stale")
    expect_equal(isolate(h$s2()), "stale")

    # Re-running step 1 must not clear step 2's badge.
    h$r1(list(a = 2)); session$flushReact()
    expect_equal(isolate(h$s1()), "done")
    expect_equal(isolate(h$s2()), "stale")
  })
})

test_that("a step that never ran shows no badge, imported or not", {
  srv <- function(input, output, session) {
    result <- reactiveVal(NULL); imported <- reactiveVal(0L)
    session$userData$h <- list(
      result = result, imported = imported,
      status = step_status(result, stale_after_import(result, NULL, imported)))
  }
  testServer(srv, {
    h <- session$userData$h
    h$imported(1L); session$flushReact()
    expect_equal(isolate(h$status()), "none")
  })
})

test_that("a later import re-stales a step that was re-run after an earlier one", {
  srv <- function(input, output, session) {
    result <- reactiveVal(list(v = 1)); imported <- reactiveVal(0L)
    session$userData$h <- list(
      result = result, imported = imported,
      status = step_status(result, stale_after_import(result, NULL, imported)))
  }
  testServer(srv, {
    h <- session$userData$h
    session$flushReact()
    h$imported(1L); session$flushReact()
    h$result(list(v = 2)); session$flushReact()
    expect_equal(isolate(h$status()), "done")
    # Generations are compared, not a one-shot flag.
    h$imported(2L); session$flushReact()
    expect_equal(isolate(h$status()), "stale")
  })
})

test_that("the step's own staleness still applies without an import", {
  srv <- function(input, output, session) {
    result <- reactiveVal(list(v = 1)); own <- reactiveVal(FALSE)
    session$userData$h <- list(
      result = result, own = own,
      status = step_status(result,
                           stale_after_import(result, own, reactiveVal(0L))))
  }
  testServer(srv, {
    h <- session$userData$h
    session$flushReact()
    expect_equal(isolate(h$status()), "done")
    h$own(TRUE); session$flushReact()
    expect_equal(isolate(h$status()), "stale")
  })
})

test_that("without an import counter the step's own flag is used unchanged", {
  own <- reactiveVal(TRUE)
  st <- stale_after_import(reactiveVal(list(1)), own, imported = NULL)
  isolate(expect_true(st()))
})

test_that("the navbar menu is labelled for both directions (UI-68)", {
  # nav_menu() returns a list, not a rendered tag - flatten before matching.
  h <- paste(as.character(export_menu_ui()), collapse = " ")
  # It imports as well as exports; the tile said only "Export".
  expect_match(h, "Export/Import", fixed = TRUE)
  expect_false(grepl('">Export"', h, fixed = TRUE))
})
