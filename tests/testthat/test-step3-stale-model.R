# R2-BUG-14: Step 3 must refuse to run when the Step 2 result was built
# from a different Step 1 model than the live one.

test_that(".step2_model_mismatch compares the live fit with Step 2's fit_sig", {
  fit_a <- list(.sig = list(step = "fit", model = "a"))
  fit_b <- list(.sig = list(step = "fit", model = "b"))
  hs_a <- list(.sig = list(step = "sim", fit_sig = fit_a$.sig))

  expect_false(.step2_model_mismatch(fit_a, hs_a))
  expect_true(.step2_model_mismatch(fit_b, hs_a))
  # Nothing to compare: missing fit, missing Step 2 or no recorded signature.
  expect_false(.step2_model_mismatch(NULL, hs_a))
  expect_false(.step2_model_mismatch(fit_b, NULL))
  expect_false(.step2_model_mismatch(fit_b, list(svy = data.frame())))
})

test_that("a policy run on a Step 2 result from an older model is blocked", {
  notes <- character(0)
  local_mocked_bindings(
    showNotification = function(ui, ...) {
      notes <<- c(notes, paste(ui, collapse = " "))
      invisible(NULL)
    },
    .package = "shiny"
  )
  clicks <- reactiveVal(NULL)
  trigger <- reactive({ req(clicks()); clicks() })
  old_sig <- list(step = "fit", model = "old")
  new_sig <- list(step = "fit", model = "new")
  testServer(mod_3_06_policy_sim_server,
             args = list(
               id = "ps",
               survey_weather = reactiveVal(data.frame(welfare = 1:5)),
               model_fit = reactive(list(.sig = new_sig)),
               selected_weather = reactive(data.frame(name = "tx")),
               hist_sim = reactive(list(
                 svy = data.frame(welfare = 1:5),
                 .sig = list(step = "sim", fit_sig = old_sig)
               )),
               run_trigger = trigger
             ), {
    session$flushReact()
    clicks(1L); session$flushReact()
    expect_identical(run_status(), "failure")
    expect_true(any(grepl("Step 1 model changed", notes)))
    expect_null(baseline_hist_sim_rv())
  })
})
