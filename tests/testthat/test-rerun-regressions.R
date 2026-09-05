# ============================================================================ #
# tests/testthat/test-rerun-regressions.R                                      #
#                                                                              #
# Two faults that only show up on the *second* run of a step:                  #
#                                                                              #
# UI-50    Step 2 appended a fresh output tab on every run instead of reusing  #
#          the one it already had, so the tab bar grew a duplicate per run.    #
# REACT-18 Step 3's staleness observers were handed a reactive's *value*       #
#          rather than the reactive. `observeEvent()` quotes the parameter     #
#          symbol, so the promise was forced once - registering the dependency #
#          once - and every later evaluation returned the cached value without #
#          re-registering it. The observers fired once and then went deaf, so  #
#          changing a Step 3 lever never marked the results stale.             #
# ============================================================================ #

library(testthat)
library(shiny)


# ---- REACT-18: the lazy-evaluation trap, in isolation ----------------------

test_that("passing a reactive's value to observeEvent deafens it after one fire", {
  # This is the shape the bug had. Kept as an executable statement of *why*
  # the helper now takes the reactive itself.
  fires <- 0L
  broken <- function(input, output, session) {
    rv <- reactiveVal(0L)
    mark <- function(observe_what) {
      observeEvent(observe_what, fires <<- fires + 1L, ignoreInit = TRUE)
    }
    mark(rv())                       # value: forced once, then cached
    observeEvent(input$bump, rv(isolate(rv()) + 1L))
  }
  testServer(broken, {
    for (i in 1:5) { session$setInputs(bump = i); session$flushReact() }
  })
  expect_equal(fires, 1L)
})

test_that("passing the reactive itself keeps the dependency alive", {
  fires <- 0L
  fixed <- function(input, output, session) {
    rv <- reactiveVal(0L)
    mark <- function(react) {
      observeEvent(react(), fires <<- fires + 1L, ignoreInit = TRUE)
    }
    mark(rv)                         # reactive: re-read on every invalidation
    observeEvent(input$bump, rv(isolate(rv()) + 1L))
  }
  testServer(fixed, {
    for (i in 1:5) { session$setInputs(bump = i); session$flushReact() }
  })
  expect_equal(fires, 5L)
})


# ---- REACT-18: the real Step 3 module --------------------------------------

policy_args <- function(sp, infra, survey_version) {
  list(
    id                 = "ps",
    survey_weather     = reactiveVal(data.frame(welfare = runif(20))),
    sp_scenario        = sp,
    infra_scenario     = infra,
    digital_scenario   = reactive(NULL),
    labor_scenario     = reactive(NULL),
    education_scenario = reactive(NULL),
    hist_sim           = reactiveVal(NULL),
    survey_version     = survey_version
  )
}

test_that("every change to a Step 3 lever marks the policy results stale", {
  sp    <- reactiveVal(list(transfer_amount_usd = 0))
  infra <- reactiveVal(list(elec_access_change_pct = 0))
  ver   <- reactiveVal(1L)

  testServer(mod_3_06_policy_sim_server,
             args = policy_args(sp, infra, ver), {
    # Stand in for a completed policy run: results exist, and their stored
    # signature will not match anything the live inputs produce.
    baseline_hist_sim_rv(list(.sig = "signature-of-a-previous-run"))
    session$flushReact()
    policy_stale(FALSE)

    # Change the same lever repeatedly. Before the fix only the first of
    # these registered.
    for (amount in c(10, 20, 30, 40, 50)) {
      policy_stale(FALSE)
      sp(list(transfer_amount_usd = amount))
      session$flushReact()
      expect_true(policy_stale(),
                  info = paste("transfer amount", amount))
    }
  })
})

test_that("each Step 3 lever is independently watched", {
  sp    <- reactiveVal(list(transfer_amount_usd = 0))
  infra <- reactiveVal(list(elec_access_change_pct = 0))
  ver   <- reactiveVal(1L)

  testServer(mod_3_06_policy_sim_server,
             args = policy_args(sp, infra, ver), {
    baseline_hist_sim_rv(list(.sig = "signature-of-a-previous-run"))
    session$flushReact()

    # Burn one change on each watcher, then check they all still respond -
    # the deafness only appeared from the second change onwards.
    sp(list(transfer_amount_usd = 1));      session$flushReact()
    infra(list(elec_access_change_pct = 1)); session$flushReact()
    ver(2L);                                 session$flushReact()

    policy_stale(FALSE)
    sp(list(transfer_amount_usd = 2)); session$flushReact()
    expect_true(policy_stale(), info = "social protection")

    policy_stale(FALSE)
    infra(list(elec_access_change_pct = 2)); session$flushReact()
    expect_true(policy_stale(), info = "infrastructure")

    policy_stale(FALSE)
    ver(3L); session$flushReact()
    expect_true(policy_stale(), info = "survey version")
  })
})

test_that("staleness clears when a new policy run publishes results", {
  sp  <- reactiveVal(list(transfer_amount_usd = 0))
  ver <- reactiveVal(1L)
  testServer(mod_3_06_policy_sim_server,
             args = policy_args(sp, reactive(NULL), ver), {
    baseline_hist_sim_rv(list(.sig = "old"))
    session$flushReact()
    sp(list(transfer_amount_usd = 99)); session$flushReact()
    expect_true(policy_stale())

    # Publishing a fresh baseline is what a completed run does.
    baseline_hist_sim_rv(list(.sig = "new"))
    session$flushReact()
    expect_false(policy_stale())
  })
})


# ---- UI-50: Step 2 must not append a tab per run ---------------------------

test_that("re-running Step 2 reuses its Results tab instead of adding another", {
  appended <- character(0)
  removed  <- character(0)
  local_mocked_bindings(
    appendTab = function(inputId, tab, ...) {
      appended <<- c(appended, tab$attribs$`data-value` %||% "tab")
      invisible(NULL)
    },
    removeTab = function(inputId, target, ...) {
      removed <<- c(removed, target); invisible(NULL)
    },
    insertUI = function(...) invisible(NULL),
    removeUI = function(...) invisible(NULL),
    updateTabsetPanel = function(...) invisible(NULL),
    .package = "shiny"
  )

  hs <- reactiveVal(NULL)
  testServer(mod_2_02_results_server,
             args = list(id = "res", hist_sim = hs,
                         saved_scenarios = reactive(list()),
                         selected_hist = reactive(NULL),
                         tabset_id = "tabs"), {
    # Consume the observer's ignoreInit run with hist_sim still NULL, the way
    # a real session's startup flush does.
    session$flushReact()
    for (i in 1:4) {
      hs(list(so = data.frame(name = "welfare", label = "Welfare"), run = i))
      session$flushReact()
    }
    # Four runs, one tab.
    expect_length(appended, 1L)
    expect_true(results_tab_added())
  })
})

test_that("clearing Step 2 removes the tab so a later run re-adds it once", {
  appended <- 0L
  removed  <- 0L
  local_mocked_bindings(
    appendTab = function(...) { appended <<- appended + 1L; invisible(NULL) },
    removeTab = function(...) { removed  <<- removed  + 1L; invisible(NULL) },
    insertUI = function(...) invisible(NULL),
    removeUI = function(...) invisible(NULL),
    updateTabsetPanel = function(...) invisible(NULL),
    .package = "shiny"
  )

  hs <- reactiveVal(NULL)
  testServer(mod_2_02_results_server,
             args = list(id = "res", hist_sim = hs,
                         saved_scenarios = reactive(list()),
                         selected_hist = reactive(NULL),
                         tabset_id = "tabs"), {
    session$flushReact()
    so <- data.frame(name = "welfare", label = "Welfare")
    hs(list(so = so, run = 1)); session$flushReact()
    hs(list(so = so, run = 2)); session$flushReact()
    expect_equal(appended, 1L)

    # INT-07: clearing the simulation takes the tab away again...
    hs(NULL); session$flushReact()
    expect_equal(removed, 1L)
    expect_false(results_tab_added())

    # ...and the next run gets a fresh one, still only one.
    hs(list(so = so, run = 3)); session$flushReact()
    expect_equal(appended, 2L)
    expect_true(results_tab_added())
  })
})

test_that("re-running Step 2 refreshes the Results pane rather than stacking it", {
  inserts <- 0L
  clears  <- 0L
  local_mocked_bindings(
    appendTab = function(...) invisible(NULL),
    removeTab = function(...) invisible(NULL),
    insertUI  = function(...) { inserts <<- inserts + 1L; invisible(NULL) },
    removeUI  = function(...) { clears  <<- clears  + 1L; invisible(NULL) },
    updateTabsetPanel = function(...) invisible(NULL),
    .package = "shiny"
  )

  hs <- reactiveVal(NULL)
  testServer(mod_2_02_results_server,
             args = list(id = "res", hist_sim = hs,
                         saved_scenarios = reactive(list()),
                         selected_hist = reactive(NULL),
                         tabset_id = "tabs"), {
    session$flushReact()
    so <- data.frame(name = "welfare", label = "Welfare")
    for (i in 1:3) { hs(list(so = so, run = i)); session$flushReact() }

    # Content is written once per run...
    expect_equal(inserts, 3L)
    # ...but the previous run's content is cleared first on runs 2 and 3, so
    # the pane shows one set of results rather than three stacked copies.
    expect_equal(clears, 2L)
  })
})

test_that("re-running Step 2 reuses its Diagnostics tab", {
  appended <- 0L
  local_mocked_bindings(
    appendTab = function(...) { appended <<- appended + 1L; invisible(NULL) },
    removeTab = function(...) invisible(NULL),
    updateSelectInput = function(...) invisible(NULL),
    .package = "shiny"
  )

  hs <- reactiveVal(NULL)
  testServer(mod_2_03_diagnostics_server,
             args = list(id = "diag", hist_sim = hs,
                         selected_weather = reactive(
                           data.frame(name = "tx", label = "Max temp")),
                         tabset_id = "tabs"), {
    session$flushReact()
    for (i in 1:4) { hs(list(run = i)); session$flushReact() }
    expect_equal(appended, 1L)
    expect_true(diag_tab_added())
  })
})


# ---- REACT-19: the first "Run simulation" click must run ---------------------
#
# Step 3 ignored its first click and worked on every one after. The trigger
# reactive `req()`s the dynamically rendered button, so it throws a silent
# error while the button is NULL and again at 0 (an action button's 0 is not
# truthy). `ignoreInit = TRUE` skips the handler on the observer's first
# *successful* evaluation - which, with everything earlier aborting, was the
# user's first click.

btn_val <- function(n) {
  structure(as.integer(n), class = c("shinyActionButtonValue", "integer"))
}

test_that("ignoreInit swallows the first click when the event expr can throw", {
  # Documents why `ignoreInit` was removed rather than kept "for safety".
  count_clicks <- function(trigger_builder, ignore_init) {
    runs <- 0L
    srv <- function(input, output, session) {
      trig <- trigger_builder(input)
      if (ignore_init) {
        observeEvent(trig(), runs <<- runs + 1L, ignoreInit = TRUE)
      } else {
        observeEvent(trig(), runs <<- runs + 1L)
      }
    }
    testServer(srv, {
      session$flushReact()
      session$setInputs(b = btn_val(0)); session$flushReact()  # button renders
      for (i in 1:3) { session$setInputs(b = btn_val(i)); session$flushReact() }
    })
    runs
  }

  plain <- function(input) reactive(input$b)
  guarded <- function(input) reactive({ req(input$b); input$b })

  # No req() in the event expression: the render at 0 spends the init budget,
  # so all three clicks run. This is the Step 1 / Step 2 shape.
  expect_equal(count_clicks(plain, ignore_init = TRUE), 3L)

  # req() in the event expression: every pre-click evaluation aborts, so the
  # first click is spent on init and only two of three clicks run.
  expect_equal(count_clicks(guarded, ignore_init = TRUE), 2L)

  # Dropping ignoreInit restores all three; req() alone already blocks the
  # NULL and 0 states, so nothing fires before a real click.
  expect_equal(count_clicks(guarded, ignore_init = FALSE), 3L)
})

test_that("the Step 3 run trigger fires on the very first click", {
  clicks <- reactiveVal(NULL)
  trigger <- reactive({ req(clicks()); clicks() })

  testServer(mod_3_06_policy_sim_server,
             args = list(id = "ps",
                         survey_weather = reactiveVal(data.frame(welfare = 1:5)),
                         run_trigger    = trigger), {
    session$flushReact()
    clicks(btn_val(0)); session$flushReact()
    expect_null(sim_error())            # rendering the button runs nothing

    # First click. No model fit is available, so run() takes its earliest
    # guard - which is exactly the observable proving the handler executed.
    clicks(btn_val(1)); session$flushReact()
    expect_false(is.null(sim_error()))
    expect_match(conditionMessage(sim_error()), "No fitted model")
  })
})

test_that("every subsequent Step 3 click still runs", {
  clicks <- reactiveVal(NULL)
  trigger <- reactive({ req(clicks()); clicks() })

  testServer(mod_3_06_policy_sim_server,
             args = list(id = "ps",
                         survey_weather = reactiveVal(data.frame(welfare = 1:5)),
                         run_trigger    = trigger), {
    session$flushReact()
    for (i in 1:4) {
      sim_error(NULL)
      clicks(btn_val(i)); session$flushReact()
      expect_false(is.null(sim_error()), info = paste("click", i))
    }
  })
})
