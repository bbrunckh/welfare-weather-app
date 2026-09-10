# ============================================================================ #
# tests/testthat/test-pipeline-runner.R                                        #
# UI-69: after a configuration import, Step 1 -> 2 -> 3 are driven to          #
#        completion in order, with progress reported in the modal. Importing   #
#        settings without running anything left the app in a state nobody      #
#        asked for - the sidebar saying one thing, the results another.        #
# ============================================================================ #

library(testthat)
library(shiny)

# testServer() quotes its expression, so the harness supplies the server
# function and each test calls testServer() itself.
KEYS <- c("load_survey", "load_weather", "step1", "step2", "step3")
rv_list <- function() stats::setNames(lapply(KEYS, function(k) reactiveVal(NULL)), KEYS)

mk_srv <- function() function(input, output, session) {
  # The settle window is real time; zero it here so a flush fires the stage.
  options(wiseapp.pipeline_settle = 0)
  trg <- rv_list()
  res <- rv_list()
  emitted <- reactiveVal(0L)
  session$userData$h <- list(
    trg = trg, res = res, emitted = emitted,
    runner = pipeline_runner(trg, res, on_state = function(...) {
      emitted(isolate(emitted()) + 1L)
    }))
}

fired <- function(h, key) !is.null(isolate(h$trg[[key]]()))
statuses <- function(h) unlist(isolate(h$runner$state()))
# Publishing a result flushes once for the completion watcher and once more
# for the (zero-length) settle window of the stage it advances to.
publish <- function(session, h, key, value) {
  h$res[[key]](value); session$flushReact(); session$flushReact()
}
start <- function(session, h) {
  h$runner$start(); session$flushReact(); session$flushReact()
}


test_that("stages run in order, each waiting for the one before", {
  testServer(mk_srv(), {
    h <- session$userData$h
    session$flushReact()
    expect_equal(isolate(h$runner$phase()), "idle")

    start(session, h)
    # Only the first stage is fired; the rest wait.
    expect_true(fired(h, "load_survey"))
    expect_false(fired(h, "load_weather"))
    expect_false(fired(h, "step1"))
    expect_equal(unname(statuses(h)),
                 c("running", "pending", "pending", "pending", "pending"))

    publish(session, h, "load_survey", list(svy = 1))
    expect_true(fired(h, "load_weather"))
    expect_false(fired(h, "step1"))

    publish(session, h, "load_weather", list(wx = 1))
    expect_true(fired(h, "step1"))
    expect_false(fired(h, "step2"))
    expect_equal(unname(statuses(h)),
                 c("done", "done", "running", "pending", "pending"))

    publish(session, h, "step1", list(fit = 1))
    expect_true(fired(h, "step2"))
    publish(session, h, "step2", list(sim = 1))
    expect_true(fired(h, "step3"))
    publish(session, h, "step3", list(pol = 1))
    expect_equal(unname(statuses(h)), rep("done", 5))
    expect_equal(isolate(h$runner$phase()), "done")
    expect_match(isolate(h$runner$message()), "Close this window")
  })
})

test_that("a result that has not changed does not advance the pipeline", {
  testServer(mk_srv(), {
    h <- session$userData$h
    # The first stage already has a result before the run starts.
    h$res$load_survey(list(svy = "old")); session$flushReact()
    start(session, h)

    # Re-publishing the identical value is not a completion: a step that
    # failed leaves its previous result standing, and treating that as
    # success would march the pipeline on over a failure.
    publish(session, h, "load_survey", list(svy = "old"))
    expect_false(fired(h, "load_weather"))
    expect_equal(unname(statuses(h))[1], "running")

    publish(session, h, "load_survey", list(svy = "new"))
    expect_true(fired(h, "load_weather"))
  })
})

test_that("a trigger is disarmed once its stage completes (UI-76)", {
  testServer(mk_srv(), {
    h <- session$userData$h
    start(session, h)
    expect_true(fired(h, "load_survey"))
    publish(session, h, "load_survey", list(a = 1))
    # Consumed: cleared, not left set. Left set, a lazily rendered Run button
    # registering later re-evaluated the merged signal and ran the step again.
    expect_null(isolate(h$trg$load_survey()))
    # The next stage is armed, and only it.
    expect_false(is.null(isolate(h$trg$load_weather())))
    expect_null(isolate(h$trg$step1()))
  })
})

test_that("cancel, failure and reset disarm every trigger", {
  testServer(mk_srv(), {
    h <- session$userData$h
    start(session, h)
    publish(session, h, "load_survey", list(a = 1))
    expect_false(is.null(isolate(h$trg$load_weather())))
    h$runner$cancel(); session$flushReact()
    for (k in KEYS) expect_null(isolate(h$trg[[k]]()), info = k)

    start(session, h)
    h$runner$reset(); session$flushReact()
    for (k in KEYS) expect_null(isolate(h$trg[[k]]()), info = k)
  })
})

test_that("a lazily rendered button registering after the run does not re-fire", {
  # Faithful to mod_3_scenario: the merged signal a module observes, fed by a
  # button reactive that only registers (as 0) when its tab is first opened.
  runs <- 0L
  srv <- function(input, output, session) {
    options(wiseapp.pipeline_settle = 0)
    trg <- rv_list(); res <- rv_list()
    button <- reactiveVal(NULL)                    # not rendered yet
    merged <- reactive({
      btn <- button(); ext <- trg$step3()
      if (!shiny::isTruthy(btn) && is.null(ext)) return(NULL)
      list(btn = btn, ext = ext)
    })
    observeEvent(merged(), runs <<- runs + 1L)     # the module's run()
    session$userData$h <- list(trg = trg, res = res, button = button,
                               runner = pipeline_runner(trg, res))
  }
  testServer(srv, {
    h <- session$userData$h
    start(session, h)
    for (k in c("load_survey", "load_weather", "step1", "step2")) {
      publish(session, h, k, list(v = 1))
    }
    expect_equal(runs, 1L)                         # pipeline ran step 3 once
    publish(session, h, "step3", list(v = 1))      # ...and it completed

    # Now the user opens Step 3: its Run button renders and reports 0.
    h$button(structure(0L, class = c("shinyActionButtonValue", "integer")))
    session$flushReact()
    expect_equal(runs, 1L)                         # no second run

    # A real click still works.
    h$button(structure(1L, class = c("shinyActionButtonValue", "integer")))
    session$flushReact()
    expect_equal(runs, 2L)
  })
})

test_that("a second run increments the trigger so modules see a change", {
  testServer(mk_srv(), {
    h <- session$userData$h
    start(session, h)
    first <- isolate(h$trg$load_survey())$n
    for (k in KEYS) publish(session, h, k, list(v = 1))

    start(session, h)
    # A repeated run with an identical payload would not invalidate the
    # module's observer, so the counter has to move.
    expect_gt(isolate(h$trg$load_survey())$n, first)
  })
})

test_that("reset returns the runner to its idle state", {
  testServer(mk_srv(), {
    h <- session$userData$h
    start(session, h)
    h$runner$reset(); session$flushReact()
    expect_equal(isolate(h$runner$phase()), "idle")
    expect_equal(unname(statuses(h)), rep("pending", 5))
    expect_null(isolate(h$runner$message()))
  })
})

test_that("every transition notifies the modal", {
  testServer(mk_srv(), {
    h <- session$userData$h
    start(session, h)
    n <- isolate(h$emitted())
    publish(session, h, "load_survey", list(a = 1))
    # Progress that never reached the UI would leave the user watching a
    # frozen dialog.
    expect_gt(isolate(h$emitted()), n)
  })
})


# ---- Progress rendering ------------------------------------------------------

test_that("the progress list names every stage", {
  html <- as.character(.pipeline_progress_ui(
    list(load_survey = "done", load_weather = "done",
         step1 = "done", step2 = "running", step3 = "pending")))
  expect_match(html, "survey sample", fixed = TRUE)
  expect_match(html, "weather variables", fixed = TRUE)
  expect_match(html, "Step 1", fixed = TRUE)
  expect_match(html, "Step 2", fixed = TRUE)
  expect_match(html, "Step 3", fixed = TRUE)
  expect_match(html, "pipeline-stage-done", fixed = TRUE)
  expect_match(html, "pipeline-stage-running", fixed = TRUE)
  expect_match(html, "pipeline-stage-pending", fixed = TRUE)
})

test_that("an unknown or absent status renders as pending", {
  html <- as.character(.pipeline_progress_ui(list()))
  expect_equal(
    lengths(regmatches(html, gregexpr("pipeline-stage-pending", html))), 5L)
})

test_that("a failed stage is distinguishable from a skipped one", {
  html <- as.character(.pipeline_progress_ui(
    list(step1 = "done", step2 = "failed", step3 = "skipped")))
  expect_match(html, "pipeline-stage-failed", fixed = TRUE)
  expect_match(html, "pipeline-stage-skipped", fixed = TRUE)
})

test_that("the stage list is the documented order", {
  # Loads come first: the file names the sample and weather variables, so
  # asking the user to click Survey stats / Weather stats by hand first was
  # asking them to repeat what it already says (UI-74).
  expect_equal(vapply(.pipeline_stages(), `[[`, character(1), "key"),
               c("load_survey", "load_weather", "step1", "step2", "step3"))
})


# ---- UI-71: closing the dialog stops the run -------------------------------

test_that("cancel stops the pipeline advancing", {
  testServer(mk_srv(), {
    h <- session$userData$h
    start(session, h)
    publish(session, h, "load_survey", list(a = 1))
    expect_true(fired(h, "load_weather"))

    h$runner$cancel(); session$flushReact()
    expect_equal(isolate(h$runner$phase()), "cancelled")
    # What finished is kept; what had not started is marked skipped.
    expect_equal(unname(statuses(h)),
                 c("done", "skipped", "skipped", "skipped", "skipped"))
  })
})

test_that("a stage already under way cannot restart the pipeline", {
  testServer(mk_srv(), {
    h <- session$userData$h
    start(session, h)
    publish(session, h, "load_survey", list(a = 1))
    h$runner$cancel(); session$flushReact()

    # The weather load was executing when the user closed the dialog; it
    # finishes on its own, but must not kick off Step 1 behind a closed window.
    publish(session, h, "load_weather", list(b = 1))
    expect_false(fired(h, "step1"))
    expect_equal(isolate(h$runner$phase()), "cancelled")
  })
})

test_that("cancelling an idle runner does nothing", {
  testServer(mk_srv(), {
    h <- session$userData$h
    session$flushReact()
    h$runner$cancel(); session$flushReact()
    expect_equal(isolate(h$runner$phase()), "idle")
    expect_null(isolate(h$runner$message()))
  })
})

test_that("a cancelled run can be started again", {
  testServer(mk_srv(), {
    h <- session$userData$h
    start(session, h)
    h$runner$cancel(); session$flushReact()

    start(session, h)
    expect_equal(isolate(h$runner$phase()), "running")
    expect_equal(unname(statuses(h)),
                 c("running", "pending", "pending", "pending", "pending"))
  })
})

test_that("the cancel message says the in-flight step still finishes", {
  testServer(mk_srv(), {
    h <- session$userData$h
    start(session, h)
    h$runner$cancel(); session$flushReact()
    msg <- isolate(h$runner$message())
    expect_match(msg, "Stopped", fixed = TRUE)
    # The in-flight step cannot be interrupted, and the copy has to say so -
    # the app is unresponsive until it finishes.
    expect_match(msg, "has to finish before the app responds", fixed = TRUE)
    expect_match(msg, "nothing after it was started", fixed = TRUE)
    expect_match(msg, "navigation bar", fixed = TRUE)
  })
})


# ---- UI-74: a stage settles before it fires ---------------------------------

test_that("a stage waits out its settle window before its trigger fires", {
  srv <- function(input, output, session) {
    # A real window here: the stage must not fire on the first flush.
    options(wiseapp.pipeline_settle = 60)
    trg <- rv_list(); res <- rv_list(); settles <- reactiveVal(0L)
    session$userData$h <- list(
      trg = trg, res = res, settles = settles,
      runner = pipeline_runner(trg, res, on_settle = function() {
        settles(isolate(settles()) + 1L)
      }))
  }
  testServer(srv, {
    h <- session$userData$h
    h$runner$start(); session$flushReact(); session$flushReact()
    # Marked running for the user, but the module has not been told yet -
    # restored values are still on their way back from the client.
    expect_equal(unname(statuses(h))[1], "running")
    expect_false(fired(h, "load_survey"))
    # The deferred apply was given a turn during the window.
    expect_gt(isolate(h$settles()), 0L)
  })
})

test_that("cancelling during the settle window abandons the stage", {
  srv <- function(input, output, session) {
    options(wiseapp.pipeline_settle = 60)
    trg <- rv_list(); res <- rv_list()
    session$userData$h <- list(trg = trg, res = res,
                               runner = pipeline_runner(trg, res))
  }
  testServer(srv, {
    h <- session$userData$h
    h$runner$start(); session$flushReact()
    h$runner$cancel(); session$flushReact(); session$flushReact()
    expect_false(fired(h, "load_survey"))
    expect_equal(isolate(h$runner$phase()), "cancelled")
  })
})

test_that("the settle window is configurable for tests", {
  withr::local_options(wiseapp.pipeline_settle = 0)
  expect_equal(.pipeline_settle_seconds(), 0)
  withr::local_options(wiseapp.pipeline_settle = NULL)
  expect_equal(.pipeline_settle_seconds(), .PIPELINE_SETTLE_SECONDS)
})
