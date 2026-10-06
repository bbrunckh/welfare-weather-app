# ============================================================================ #
# tests/testthat/test-fct_model_card.R                                         #
# Model card rows and model badge.                                              #
# ============================================================================ #

library(testthat)

make_model_spec <- function(...) {
  args <- list(
    type = "Linear regression", engine = "fixest",
    interactions = "urban", interaction_mode = "pairwise",
    fixedeffects = c("year", "gaul1_code"), covariate_selection = "Lasso",
    ind_covariates = c("age", "sex_female"), hh_covariates = c("a", "b", "c"),
    firm_covariates = character(0), area_covariates = c("x", "y"),
    cluster = "loc_id_panel",
    lasso_alpha = 1, lasso_lambda = "lambda.1se", lasso_nfolds = 10L,
    lasso_standardize = TRUE, mi_m = 5L, mi_maxit = 5L,
    stability_threshold = 0.5
  )
  args[names(list(...))] <- list(...)
  args
}

test_that("model_badge pairs model type with a readable engine", {
  expect_identical(model_badge(make_model_spec()),
                   "Linear regression \u00B7 OLS")
  expect_identical(model_badge(make_model_spec(type = "Logistic regression")),
                   "Logistic regression \u00B7 logit")
  expect_identical(
    model_badge(make_model_spec(type = "Random forest", engine = "ranger")),
    "Random forest \u00B7 ranger"
  )
})

test_that("model_cluster_phrase maps location panels to plain language", {
  expect_identical(model_cluster_phrase("loc_id_panel"),
                   "clustered by location panel")
  expect_identical(model_cluster_phrase("loc_id"), "clustered by location")
  expect_identical(model_cluster_phrase("village"), "clustered by village")
  expect_identical(model_cluster_phrase(NULL), "unclustered")
  expect_identical(model_cluster_phrase(NA_character_), "unclustered")
})

test_that("covariate counts break down by role, dropping zeros", {
  sm <- make_model_spec()
  expect_identical(unname(model_covariate_counts(sm)), c(3L, 2L, 2L))
  expect_identical(model_covariate_total(sm), 7L)
})

test_that("R2-BUG-20: covariate counts keep role names when roles are empty", {
  sm <- make_model_spec(
    ind_covariates = character(0), hh_covariates = character(0)
  )
  expect_identical(model_covariate_counts(sm), c(area = 2L))
  sm <- make_model_spec()
  expect_identical(
    model_covariate_counts(sm),
    c(household = 3L, area = 2L, individual = 2L)
  )
})

test_that("R2-BUG-20: role filters ignore NA flags instead of adding NA rows", {
  vl <- data.frame(
    name = c("a", "b", "c", "d"),
    hh   = c(1L, NA, 0L, 1L),
    type = c("numeric", "numeric", "binary", NA),
    stringsAsFactors = FALSE
  )
  out <- filter_vars_by_role(vl, "hh")
  expect_identical(out$name, c("a", "d"))
  out <- filter_vars_by_role(vl, "hh", extra_filter = list(type = "binary"))
  expect_identical(out$name, c("a", "d"))
  expect_false(anyNA(out$name))
})

test_that("model_card_rows renders one concise equation line", {
  rows <- model_card_rows(
    make_model_spec(),
    label_fun      = function(x) x,
    outcome_label  = "Welfare per day",
    weather_labels = "Monthly max temperature"
  )
  expect_length(rows, 1)
  html <- paste(as.character(rows), collapse = " ")
  expect_match(html, "Linear regression", fixed = TRUE)
  expect_match(html, "selection-card-op", fixed = TRUE)
  expect_match(html, "Welfare per day", fixed = TRUE)
  expect_match(html, "Max temperature", fixed = TRUE)
  expect_match(html, "urban", fixed = TRUE)
  expect_match(html, "\u00D7", fixed = TRUE)
  expect_match(html, "7 covariates", fixed = TRUE)
  expect_match(html, "2 FEs", fixed = TRUE)
  expect_false(grepl("year|gaul1_code", html))
  expect_false(grepl("selection-card-op\">:</span>", html, fixed = TRUE))
  expect_false(grepl("clustered by location panel", html, fixed = TRUE))
  # no tuning knobs, no "div" leak
  expect_false(grepl("alpha|lambda|1se", html))
  expect_false(grepl(">div<", html))
})

test_that("saturated mode crosses each weather with the full moderator set", {
  rows <- model_card_rows(
    make_model_spec(
      interactions = c("urban", "grid"),
      interaction_mode = "saturated"
    ),
    label_fun     = function(x) x,
    outcome_label = "Poor",
    weather_labels = c("Monthly max temperature", "Monthly precipitation")
  )
  html <- paste(as.character(rows), collapse = " ")
  expect_match(html, "Max temperature", fixed = TRUE)
  expect_match(html, "Precipitation", fixed = TRUE)
})

test_that("no covariates and no FE render a clean minimal line", {
  sm <- make_model_spec(
    covariate_selection = "User-defined",
    interactions = character(0),
    fixedeffects = character(0),
    cluster = NULL,
    ind_covariates = character(0), hh_covariates = character(0),
    firm_covariates = character(0), area_covariates = character(0)
  )
  rows <- model_card_rows(sm, outcome_label = "Welfare per day",
                          weather_labels = "Max temperature")
  expect_length(rows, 1)
  html <- paste(as.character(rows), collapse = " ")
  expect_match(html, "Welfare per day", fixed = TRUE)
  expect_match(html, "Max temperature", fixed = TRUE)
  expect_false(grepl("0 covariates", html, fixed = TRUE))
  expect_false(grepl(" FEs", html, fixed = TRUE))

  # Empty interaction and fixed-effect terms are omitted.
  rows2 <- model_card_rows(sm)
  html2 <- paste(as.character(rows2), collapse = " ")
  expect_false(grepl("No interactions|No fixed effects|unclustered", html2))
})
