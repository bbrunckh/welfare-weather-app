# ============================================================================ #
# tests/testthat/test-fct_model_card.R                                         #
# Model card (sparse formula): badge, cluster phrase, and the formula row      #
# outcome ~ weather (+ interactions) + covariates | FE | clustering.           #
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

test_that("model_card_rows names every part of the specification (UI-56)", {
  rows <- model_card_rows(
    make_model_spec(),
    label_fun      = function(x) x,
    outcome_label  = "Welfare per day",
    weather_labels = "Monthly max temperature"
  )
  # One labelled row per part, matching the Sample/Outcome/Weather cards.
  expect_length(rows, 5)
  html <- paste(as.character(
    selection_summary_card("Selected model", rows)), collapse = " ")

  expect_match(html, "Welfare per day", fixed = TRUE)
  # "Monthly" prefix stripped; pairwise mode: one term per weather x moderator
  expect_match(html, "max temperature \u00D7 urban", fixed = TRUE)
  # 7 chosen covariates + the "urban" interaction moderator, which
  # build_formulas() also estimates as a main effect (UI-67).
  expect_match(html, "8 covariates", fixed = TRUE)
  # The fixed-effect row is labelled, not a bare "| year . gaul1_code FE"
  # segment in a formula string.
  expect_match(html, "Fixed effects", fixed = TRUE)
  expect_match(html, "year", fixed = TRUE)
  expect_match(html, "gaul1_code", fixed = TRUE)
  expect_match(html, "Standard errors", fixed = TRUE)
  expect_match(html, "clustered by location panel", fixed = TRUE)
  # no tuning knobs, no "div" leak
  expect_false(grepl("alpha|lambda|1se", html))
  expect_false(grepl(">div<", html))
})

test_that("covariates are broken down by role rather than just counted", {
  html <- paste(as.character(selection_summary_card(
    "Selected model", model_card_rows(make_model_spec()))), collapse = " ")
  # A bare total said nothing about what the covariates were.
  expect_match(html, "3 household", fixed = TRUE)
  expect_match(html, "2 area", fixed = TRUE)
  expect_match(html, "2 individual", fixed = TRUE)
})

test_that("the outcome row is labelled first, like every other row (UI-66)", {
  html <- paste(as.character(selection_summary_card(
    "Selected model",
    model_card_rows(make_model_spec(), outcome_label = "Poor (welfare < line)")
  )), collapse = " ")
  # Not "Poor (welfare < line) outcome" - the label leads, the value follows.
  expect_lt(regexpr("Outcome", html), regexpr("Poor \\(welfare", html))
  expect_false(grepl(">outcome<", html, fixed = TRUE))
})

test_that("an interaction moderator counts as a covariate (UI-67)", {
  # build_formulas() puts terms$interactions_main on the RHS of models 2 and
  # 3, so a policy-locked moderator is estimated as a main effect. The card
  # used to report "No covariates" while the model was fitting one.
  sm <- make_model_spec(
    interactions = "urban",
    ind_covariates = character(0), hh_covariates = character(0),
    firm_covariates = character(0), area_covariates = character(0))
  html <- paste(as.character(selection_summary_card(
    "Selected model", model_card_rows(sm, label_fun = function(x) x))),
    collapse = " ")

  expect_match(html, "1 covariate", fixed = TRUE)
  expect_false(grepl("No covariates", html, fixed = TRUE))
  # Named, so it is clear it came from the policy lock and not a selection.
  expect_match(html, "urban (moderator)", fixed = TRUE)
})

test_that("moderators add to the chosen covariates rather than replacing them", {
  sm <- make_model_spec(interactions = c("urban", "grid"))
  html <- paste(as.character(selection_summary_card(
    "Selected model", model_card_rows(sm, label_fun = function(x) x))),
    collapse = " ")
  # 7 chosen + 2 moderators.
  expect_match(html, "9 covariates", fixed = TRUE)
  expect_match(html, "urban (moderator)", fixed = TRUE)
  expect_match(html, "grid (moderator)", fixed = TRUE)
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
  html <- paste(as.character(
    selection_summary_card("Selected model", rows)), collapse = " ")
  expect_match(html, "max temperature \u00D7 urban \u00D7 grid", fixed = TRUE)
  expect_match(html, "precipitation \u00D7 urban \u00D7 grid", fixed = TRUE)
})

test_that("an absent part says so instead of going silent", {
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
  expect_length(rows, 5)
  html <- paste(as.character(
    selection_summary_card("Selected model", rows)), collapse = " ")

  expect_match(html, "Welfare per day", fixed = TRUE)
  expect_match(html, "Max temperature", fixed = TRUE)
  # A model with no covariates and no fixed effects is a real specification;
  # the card states both rather than dropping the rows.
  # No chosen covariates *and* no moderator: genuinely nothing.
  expect_match(html, "No covariates", fixed = TRUE)
  expect_match(html, "No fixed effects", fixed = TRUE)
  expect_match(html, "unclustered", fixed = TRUE)
})

# The head is a <div> containing a single <span>; take everything up to the
# badge row (or the first detail row) as "the head".
head_of <- function(html) {
  i <- regexpr("selection-card-head", html)
  if (i < 0) return("")
  rest <- substring(html, i)
  j <- regexpr("selection-card-row", rest)
  if (j < 0) rest else substring(rest, 1, j)
}

test_that("the badge never sits in the card head (UI-52)", {
  spec <- make_model_spec()
  for (lbl in list(NULL, "Regression model")) {
    html <- as.character(selection_summary_card(
      "Selected model", model_card_rows(spec),
      badge = model_badge(spec), badge_label = lbl))
    expect_false(grepl("selection-card-badge", head_of(html), fixed = TRUE),
                 info = paste("badge_label:", lbl %||% "NULL"))
    # The badge text is still on the card, wherever it landed.
    expect_match(html, "Linear regression", fixed = TRUE)
  }
})

test_that("a badge with a preamble gets its own labelled row (UI-63)", {
  spec <- make_model_spec()
  html <- as.character(selection_summary_card(
    "Selected model", model_card_rows(spec),
    badge = model_badge(spec), badge_label = "Regression model"))
  # "Linear regression . OLS" alone does not say what it describes.
  expect_match(html, "Regression model", fixed = TRUE)
  # Preamble and badge sit in one row, preamble first.
  expect_lt(regexpr("Regression model", html),
            regexpr("Linear regression", html))
})

test_that("a self-explanatory badge rides on the first row instead (UI-63)", {
  html <- as.character(selection_summary_card(
    "Selected sample",
    list(list(name = "Kenya", sub = "LSMS", pills = "2015")),
    badge = "Household level"))
  # No row spent on a bare pill: it joins the pills already on row one.
  expect_false(grepl("selection-card-badge-row", html, fixed = TRUE))
  expect_match(html, "Household level", fixed = TRUE)
  expect_match(html, "Kenya", fixed = TRUE)
  # One row in, one row out.
  expect_equal(
    lengths(regmatches(html, gregexpr("selection-card-row", html))), 1L)
})

test_that("an inline badge also attaches to a pre-built row tag", {
  html <- as.character(selection_summary_card(
    NULL,
    list(selection_card_row(name = "Max temperature", sub = "tx")),
    badge = "History 1991-2020"))
  expect_match(html, "History 1991-2020", fixed = TRUE)
  expect_equal(
    lengths(regmatches(html, gregexpr("selection-card-row", html))), 1L)
})
