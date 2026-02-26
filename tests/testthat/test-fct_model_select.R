library(testthat)

# ---- helpers ----------------------------------------------------------------

make_vl <- function() {
  data.frame(
    name     = c("age", "urban", "year", "gaul1_code", "welfare", "tx"),
    label    = c("Age", "Urban", "Year", "District", "Welfare", "Max temp"),
    type     = c("numeric", "logical", "integer", "integer", "numeric", "numeric"),
    fe       = c(0L, 0L, 1L, 1L, 0L, 0L),
    hh       = c(0L, 1L, 0L, 0L, 0L, 0L),
    ind      = c(1L, 0L, 0L, 0L, 0L, 0L),
    area     = c(0L, 0L, 0L, 1L, 0L, 0L),
    firm     = c(0L, 0L, 0L, 0L, 0L, 0L),
    interact = c(0L, 1L, 0L, 0L, 0L, 1L),
    weather  = c(0L, 0L, 0L, 0L, 0L, 1L),
    stringsAsFactors = FALSE
  )
}

# ============================================================================ #
# filter_valid_vars                                                            #
# ============================================================================ #

test_that("filter_valid_vars keeps columns with >= 50% non-missing", {
  df <- data.frame(age = c(1, 2, NA, NA), urban = c(1, 1, 1, 1))
  vl <- make_vl()
  out <- filter_valid_vars(df, vl)
  expect_true("urban" %in% out$name)
  expect_false("age" %in% out$name)
})

test_that("filter_valid_vars returns variable_list unchanged for NULL df", {
  vl <- make_vl()
  expect_equal(filter_valid_vars(NULL, vl), vl)
})

test_that("filter_valid_vars respects custom min_complete threshold", {
  df <- data.frame(age = c(1, NA, NA, NA))
  vl <- make_vl()[make_vl()$name == "age", ]
  expect_equal(nrow(filter_valid_vars(df, vl, min_complete = 0.2)), 1L)
  expect_equal(nrow(filter_valid_vars(df, vl, min_complete = 0.5)), 0L)
})

# ============================================================================ #
# filter_vars_by_role                                                          #
# ============================================================================ #

test_that("filter_vars_by_role returns fe vars only", {
  out <- filter_vars_by_role(make_vl(), "fe")
  expect_setequal(out$name, c("year", "gaul1_code"))
})

test_that("filter_vars_by_role returns interact vars excl. numeric", {
  out <- filter_vars_by_role(make_vl(), "interact", extra_filter = list(type = "numeric"))
  expect_true("urban" %in% out$name)
  expect_false("tx" %in% out$name)
})

test_that("filter_vars_by_role returns zero-row df for unknown role", {
  out <- filter_vars_by_role(make_vl(), "nonexistent")
  expect_equal(nrow(out), 0L)
})

test_that("filter_vars_by_role returns input unchanged for NULL/empty vl", {
  expect_null(filter_vars_by_role(NULL, "fe"))
  expect_equal(nrow(filter_vars_by_role(make_vl()[0L, ], "fe")), 0L)
})

# ============================================================================ #
# model_type_choices                                                           #
# ============================================================================ #

test_that("model_type_choices: logical outcome has logistic + linear", {
  mc <- model_type_choices("logical")
  expect_true("Logistic regression" %in% mc$choices)
  expect_true("Linear regression"   %in% mc$choices)
  expect_equal(mc$label, "Classification model:")
})

test_that("model_type_choices: numeric outcome has linear only", {
  mc <- model_type_choices("numeric")
  expect_equal(mc$choices, "Linear regression")
  expect_equal(mc$label, "Regression model:")
})

test_that("model_type_choices: non-logical types default to regression", {
  expect_equal(model_type_choices("double")$choices,  "Linear regression")
  expect_equal(model_type_choices("integer")$choices, "Linear regression")
})

# ============================================================================ #
# exclude_selected_vars                                                        #
# ============================================================================ #

test_that("exclude_selected_vars removes outcome, weather, interactions, fe", {
  vl  <- make_vl()
  out <- exclude_selected_vars(
    vl,
    outcome_name  = "welfare",
    weather_names = "tx",
    interactions  = "urban",
    fixedeffects  = c("year", "gaul1_code")
  )
  expect_false(any(c("welfare", "tx", "urban", "year", "gaul1_code") %in% out$name))
  expect_true("age" %in% out$name)
})

test_that("exclude_selected_vars returns input unchanged when all args empty", {
  vl  <- make_vl()
  out <- exclude_selected_vars(vl)
  expect_equal(nrow(out), nrow(vl))
})

test_that("exclude_selected_vars handles NULL args gracefully", {
  vl  <- make_vl()
  out <- exclude_selected_vars(vl, outcome_name = NULL, weather_names = NULL)
  expect_equal(nrow(out), nrow(vl))
})

test_that("exclude_selected_vars returns input unchanged for NULL/empty vl", {
  expect_null(exclude_selected_vars(NULL))
  expect_equal(nrow(exclude_selected_vars(make_vl()[0L, ])), 0L)
})

# ============================================================================ #
# build_selected_model                                                         #
# ============================================================================ #

test_that("build_selected_model returns correct structure", {
  out <- build_selected_model(
    model_type   = "Linear regression",
    interactions = "urban",
    fixedeffects = c("year", "gaul1_code"),
    covariate_selection = "User-defined",
    hh_covariates = "urban",
    area_covariates = "gaul1_code"
  )
  expect_named(out, c("type", "engine", "interactions", "interaction_mode",
                       "fixedeffects", "covariate_selection", "ind_covariates",
                       "hh_covariates", "firm_covariates", "area_covariates",
                       "cluster"))
  expect_equal(out$type,         "Linear regression")
  expect_equal(out$interactions, "urban")
  expect_equal(out$fixedeffects, c("year", "gaul1_code"))
})

test_that("build_selected_model coerces NULL args to character(0)", {
  out <- build_selected_model()
  expect_equal(out$type,            character(0))
  expect_equal(out$interactions,    character(0))
  expect_equal(out$ind_covariates,  character(0))
  expect_equal(out$hh_covariates,   character(0))
  expect_equal(out$firm_covariates, character(0))
  expect_equal(out$area_covariates, character(0))
  expect_equal(out$cluster,         character(0))
})

test_that("build_selected_model defaults covariate_selection to User-defined", {
  out <- build_selected_model()
  expect_equal(out$covariate_selection, "User-defined")
})

test_that("build_selected_model defaults engine to lm", {
  out <- build_selected_model()
  expect_equal(out$engine, "lm")
})

test_that("build_selected_model defaults interaction_mode to pairwise", {
  out <- build_selected_model()
  expect_equal(out$interaction_mode, "pairwise")
})
