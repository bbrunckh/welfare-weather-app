# ============================================================================ #
# tests/testthat/test-derived-outcome-column.R                                 #
# UI-65: `poor` is derived (welfare < poverty line), not stored. Step 2's      #
#        baseline survey never passes through prepare_outcome_df(), so the     #
#        Step 3 decomposition received a frame with no outcome column and      #
#        stats::ecdf(NULL) aborted the whole policy run with                   #
#        "'x' must have 1 or more non-missing values".                        #
# ============================================================================ #

library(testthat)

svy_frame <- function(n = 50) {
  set.seed(4)
  data.frame(welfare = runif(n, 1, 5), ppp2021 = 1,
             hhsize = sample(1:6, n, replace = TRUE))
}
poor_so <- function(...) {
  utils::modifyList(
    list(name = "poor", units = "PPP", transform = "none", povline = 3),
    list(...))
}


test_that("a derived outcome column is created when it is absent", {
  svy <- svy_frame()
  expect_false("poor" %in% names(svy))

  out <- ensure_outcome_column(svy, poor_so())
  expect_true("poor" %in% names(out))
  expect_setequal(unique(out$poor), c(0, 1))
  # It is the poverty line that decides, not something else.
  expect_equal(out$poor, as.numeric(svy$welfare < 3))
})

test_that("an existing outcome column is left exactly as it is", {
  svy <- svy_frame()
  svy$poor <- 0                       # deliberately not what the line implies
  out <- ensure_outcome_column(svy, poor_so())
  expect_identical(out$poor, svy$poor)
})

test_that("the log transform is deliberately NOT applied", {
  # decompose_policy_effect() takes the outcome on the level scale and logs it
  # itself; running the full prepare_outcome_df() here would double-transform.
  svy <- svy_frame()
  so  <- list(name = "welfare", units = "PPP", transform = "log",
              povline = NA_real_)
  out <- ensure_outcome_column(svy, so)
  expect_identical(out$welfare, svy$welfare)
})

test_that("a non-derivable outcome is passed through untouched", {
  svy <- svy_frame()
  # No poverty line to derive from.
  expect_false("poor" %in% names(
    ensure_outcome_column(svy, poor_so(povline = NA_real_))))
  # No welfare column to derive from.
  bare <- data.frame(x = 1:5)
  expect_identical(ensure_outcome_column(bare, poor_so()), bare)
})

test_that("ensure_outcome_column degrades instead of erroring", {
  expect_null(ensure_outcome_column(NULL, poor_so()))
  svy <- svy_frame()
  expect_identical(ensure_outcome_column(svy, NULL), svy)
  expect_identical(ensure_outcome_column(svy, list(name = NA_character_)), svy)
})

test_that("it agrees with prepare_outcome_df on the derived values", {
  svy <- svy_frame()
  so  <- poor_so()
  expect_equal(ensure_outcome_column(svy, so)$poor,
               prepare_outcome_df(svy, so)$poor)
})


# ---- The guard that replaced the opaque ecdf abort --------------------------

test_that("a missing outcome yields NULL and a named warning, not an abort", {
  svy <- svy_frame()
  so  <- poor_so()
  mf  <- list(engine = "fixest", weather_terms = "tx", fit3 = NULL)

  expect_warning(
    res <- decompose_policy_effect(svy_baseline = svy, svy_policy = svy,
                                   model_fit = mf, so = so),
    "outcome 'poor' is missing"
  )
  # Callers already treat NULL as "no decomposition available".
  expect_null(res)
})

test_that("an all-missing outcome is caught the same way", {
  svy <- svy_frame()
  svy$poor <- NA_real_
  mf <- list(engine = "fixest", weather_terms = "tx", fit3 = NULL)
  expect_warning(
    decompose_policy_effect(svy_baseline = svy, svy_policy = svy,
                            model_fit = mf, so = poor_so()),
    "no usable values"
  )
})

test_that("the warning names the outcome so the cause is identifiable", {
  # The old failure - "'x' must have 1 or more non-missing values" from
  # stats::ecdf - said nothing about which column or why.
  svy <- svy_frame()
  mf  <- list(engine = "fixest", weather_terms = "tx", fit3 = NULL)
  w <- tryCatch(
    decompose_policy_effect(svy_baseline = svy, svy_policy = svy,
                            model_fit = mf, so = poor_so()),
    warning = conditionMessage)
  expect_match(w, "poor", fixed = TRUE)
  expect_match(w, "ensure_outcome_column", fixed = TRUE)
})

test_that("a survey carrying the derived outcome gets past the guard", {
  svy <- ensure_outcome_column(svy_frame(), poor_so())
  mf  <- list(engine = "fixest", weather_terms = "tx", fit3 = NULL)
  # Past the outcome guard it stops for an unrelated reason (no fitted model),
  # which is the point: the outcome is no longer what blocks it.
  expect_silent(
    res <- decompose_policy_effect(svy_baseline = svy, svy_policy = svy,
                                   model_fit = mf, so = poor_so())
  )
  expect_null(res)
})
