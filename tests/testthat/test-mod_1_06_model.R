# ============================================================================ #
# tests/testthat/test-mod_1_06_model.R                                         #
# REACT-16: the Lasso runs on "Run model", never on selecting it.              #
# UI-42: a model with weather + the default interaction/fixed effects and no   #
#        covariates is a complete specification and must be runnable.          #
# ============================================================================ #

library(testthat)
library(shiny)

make_vl <- function() {
  data.frame(
    name     = c("welfare", "tx", "urban", "year", "gaul1_code",
                 "hhsize", "educ"),
    label    = c("Welfare", "Max temp", "Urban", "Year", "Region",
                 "Household size", "Education"),
    type     = c("numeric", "numeric", "binary", "numeric", "character",
                 "numeric", "numeric"),
    ind      = c(0L, 0L, 0L, 0L, 0L, 0L, 1L),
    hh       = c(0L, 0L, 0L, 0L, 0L, 1L, 0L),
    firm     = c(0L, 0L, 0L, 0L, 0L, 0L, 0L),
    area     = c(0L, 0L, 1L, 0L, 0L, 0L, 0L),
    fe       = c(0L, 0L, 0L, 1L, 1L, 0L, 0L),
    interact = c(0L, 0L, 1L, 0L, 0L, 0L, 0L),
    outcome  = c(1L, 0L, 0L, 0L, 0L, 0L, 0L),
    stringsAsFactors = FALSE
  )
}

make_sw <- function(n = 60) {
  set.seed(3)
  data.frame(
    code       = "ABC",
    year       = rep(c(2010L, 2015L), each = n / 2),
    survname   = "S",
    welfare    = runif(n, 1, 5),
    tx         = rnorm(n, 25, 3),
    urban      = rep(c(0, 1), length.out = n),
    gaul1_code = rep(c("r1", "r2"), length.out = n),
    hhsize     = sample(1:8, n, replace = TRUE),
    educ       = rnorm(n),
    stringsAsFactors = FALSE
  )
}

make_outcome  <- function() {
  data.frame(name = "welfare", label = "Welfare", type = "numeric",
             transform = "none", stringsAsFactors = FALSE)
}
make_weather <- function() {
  data.frame(name = "tx", label = "Max temp", stringsAsFactors = FALSE)
}

model_args <- function() {
  list(
    id               = "model",
    variable_list    = reactiveVal(make_vl()),
    selected_surveys = reactiveVal(data.frame(code = "ABC", year = 2010L)),
    analysis_unit    = reactiveVal("hh"),
    selected_outcome = reactiveVal(make_outcome()),
    selected_weather = reactiveVal(make_weather()),
    survey_weather   = reactiveVal(make_sw())
  )
}


test_that("selecting Lasso does not run it; only Run model does", {
  calls <- 0L
  local_mocked_bindings(
    run_lasso_selection = function(...) {
      calls <<- calls + 1L
      list(selected_covariates = c("hhsize", "educ"),
           selection_frequency = c(hhsize = 1, educ = 1))
    },
    prepare_outcome_df = function(df, so) df
  )

  testServer(mod_1_06_model_server, args = model_args(), {
    session$setInputs(model_type = "Linear regression")

    # Switching the covariate method used to invalidate selected_model(),
    # whose first reader forced the (expensive) Lasso right there.
    session$setInputs(covariates = "Lasso")
    session$flushReact()
    spec <- selected_model()
    expect_equal(calls, 0L)
    expect_equal(spec$covariate_selection, "Lasso")
    # Nothing has been selected yet, so no covariates are carried.
    expect_length(spec$hh_covariates, 0L)

    # Reading the spec repeatedly must stay free.
    invisible(selected_model())
    invisible(selected_model())
    expect_equal(calls, 0L)

    # The click is what runs it.
    session$setInputs(run_model = 1L)
    expect_equal(calls, 1L)
    expect_setequal(
      c(selected_model()$hh_covariates, selected_model()$ind_covariates),
      c("hhsize", "educ")
    )
  })
})


test_that("switching away from Lasso drops the stored selection", {
  local_mocked_bindings(
    run_lasso_selection = function(...) {
      list(selected_covariates = c("hhsize"), selection_frequency = c(hhsize = 1))
    },
    prepare_outcome_df = function(df, so) df
  )

  testServer(mod_1_06_model_server, args = model_args(), {
    session$setInputs(model_type = "Linear regression", covariates = "Lasso")
    session$setInputs(run_model = 1L)
    expect_equal(selected_model()$hh_covariates, "hhsize")

    # A Lasso set chosen under one method must not leak into the next.
    session$setInputs(covariates = "User-defined")
    session$flushReact()
    spec <- selected_model()
    expect_equal(spec$covariate_selection, "User-defined")
    expect_length(spec$hh_covariates, 0L)
  })
})


test_that("a covariate-free spec is complete: weather, interaction, and FE", {
  testServer(mod_1_06_model_server, args = model_args(), {
    # Only the model type is set - the "Model settings" flyout has never been
    # opened, so covariates/interactions/fixedeffects have not reported.
    session$setInputs(model_type = "Linear regression")
    session$flushReact()

    spec <- selected_model()
    expect_type(spec, "list")
    expect_equal(spec$type, "Linear regression")
    # Defaults the sidebar shows are the defaults the spec carries.
    expect_equal(spec$covariate_selection, "User-defined")
    expect_equal(spec$interactions, "urban")
    expect_setequal(spec$fixedeffects, c("year", "gaul1_code"))
    # No covariates is a valid model, not a missing one.
    expect_length(spec$hh_covariates, 0L)
    expect_length(spec$ind_covariates, 0L)
  })
})


test_that("run prerequisites do not include covariates", {
  testServer(mod_1_06_model_server, args = model_args(), {
    session$setInputs(model_type = "Linear regression")
    session$flushReact()
    # Outcome, weather, data and model type are all present; nothing else is
    # required to enable "Run model".
    expect_length(run_prereqs_missing(), 0L)
  })
})


# ---------------------------------------------------------------------------- #
# Audit follow-up: does collapsing the Lasso forced-covariates panel change the  #
# model contract?                                                               #
#                                                                               #
# It does not. Shiny's `unbindInputs()` (shiny.js) deregisters a removed input's #
# binding and unsubscribes it, but never calls `setInput(id, null)` - only       #
# *outputs* notify the server when they are hidden (`sendOutputHiddenState`,     #
# which is what suspendWhenHidden keys off). So tearing down the panel's         #
# `renderUI` leaves `input$force_in_*` / `input$force_out_*` intact server-side, #
# and re-opening restores them through `.restore_selection()` (INT-01).          #
# ---------------------------------------------------------------------------- #

test_that("collapsing the forced-covariates panel leaves the spec unchanged", {
  local_mocked_bindings(
    run_lasso_selection = function(...) {
      list(selected_covariates = c("educ"), selection_frequency = c(educ = 1))
    },
    prepare_outcome_df = function(df, so) df
  )

  testServer(mod_1_06_model_server, args = model_args(), {
    session$setInputs(model_type = "Linear regression", covariates = "Lasso")

    # Open the panel and force one covariate in and one out.
    session$setInputs(show_lasso_force = 1L)
    session$setInputs(force_in_hh = "hhsize", force_out_ind = "educ")
    session$setInputs(run_model = 1L)

    before <- selected_model()
    expect_true("hhsize" %in% before$hh_covariates)   # forced in
    expect_false("educ" %in% before$ind_covariates)   # forced out, despite
                                                      # being Lasso-selected

    # Collapse the panel. The inputs leave the DOM but keep their values.
    session$setInputs(show_lasso_force = 2L)
    session$flushReact()

    after <- selected_model()
    expect_identical(after, before)
    expect_true("hhsize" %in% after$hh_covariates)
    expect_false("educ" %in% after$ind_covariates)

    # Re-opening is likewise a no-op on the contract.
    session$setInputs(show_lasso_force = 3L)
    session$flushReact()
    expect_identical(selected_model(), before)
  })
})

test_that("forced include/exclude still bind when the panel is closed", {
  local_mocked_bindings(
    run_lasso_selection = function(...) {
      list(selected_covariates = c("hhsize", "educ"),
           selection_frequency = c(hhsize = 1, educ = 1))
    },
    prepare_outcome_df = function(df, so) df
  )

  testServer(mod_1_06_model_server, args = model_args(), {
    session$setInputs(model_type = "Linear regression", covariates = "Lasso")
    session$setInputs(show_lasso_force = 1L)
    session$setInputs(force_out_hh = "hhsize")
    session$setInputs(show_lasso_force = 2L)   # collapsed before the run
    session$setInputs(run_model = 1L)

    spec <- selected_model()
    # The exclusion is honoured even though its control is not on screen.
    expect_false("hhsize" %in% spec$hh_covariates)
    expect_true("educ" %in% spec$ind_covariates)
  })
})
