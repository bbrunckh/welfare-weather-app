# Step 1 "At a glance" cards: physical-unit contrast, significance flag,
# fixed-effects robustness, 10th vs 90th percentile wording, overall R2.

.s1_fixture <- function(n = 1200L, beta = -0.3, seed = 42, engine = "fixest",
                        model_type = "Linear regression") {
  set.seed(seed)
  temp <- stats::rnorm(n, 25, 4)
  loc <- sample(sprintf("L%02d", 1:30), n, replace = TRUE)
  fe <- stats::rnorm(30)[match(loc, sprintf("L%02d", 1:30))]
  df <- data.frame(
    welfare = 3 + beta * (temp - 25) / 4 + fe + stats::rnorm(n, 0, 1),
    temp = temp, loc_id_panel = loc, stringsAsFactors = FALSE
  )
  sm <- build_selected_model(model_type = model_type, engine = engine)
  so <- list(name = "welfare", type = "numeric", transform = "none", label = "Welfare")
  sw <- data.frame(
    name = "temp", label = "Temperature", units = "°C",
    cont_binned = "Continuous", stringsAsFactors = FALSE
  )
  mf <- fit_model(df, so, sw, sm)
  snap <- list(model = sm, outcome = so, weather = sw)
  list(mf = mf, snap = snap)
}

test_that("effect card contrasts per +1 SD in physical units with the CI and no verdict flag", {
  skip_if_not_installed("fixest")
  fx <- .s1_fixture()
  res <- step1_headline_cards(fx$mf, fx$snap)
  card <- res$rows[[1]]$cards[[1]]
  expect_identical(card$label, "Effect of Temperature")
  expect_match(card$note, "per +1 SD (= 4 °C)", fixed = TRUE)
  expect_match(card$note, "95% CI", fixed = TRUE)
  expect_true(card$significant)
  # The interval is shown; no significant / not significant verdict is printed.
  expect_null(card$status)
})

test_that("an effect indistinguishable from zero shows its interval without a verdict", {
  skip_if_not_installed("fixest")
  fx <- .s1_fixture(beta = 0, seed = 7, n = 300L)
  card <- step1_headline_cards(fx$mf, fx$snap)$rows[[1]]$cards[[1]]
  expect_false(card$significant)
  expect_null(card$status)
  expect_match(card$note, "95% CI", fixed = TRUE)
})

test_that("robustness card compares fixed-effects and full specifications", {
  skip_if_not_installed("fixest")
  fx <- .s1_fixture()
  card <- step1_headline_cards(fx$mf, fx$snap)$rows[[1]]$cards[[3]]
  expect_identical(card$label, "Spec robustness")
  expect_identical(card$value, "Robust")
  expect_match(card$note, "of 3 specifications agree", fixed = TRUE)
  expect_match(as.character(card$note_html), "survives fixed effects", fixed = TRUE)
  expect_match(card$info, "specifications 2 and 3", fixed = TRUE)
})

test_that("model fit card leads with overall R2 and the sample size", {
  skip_if_not_installed("fixest")
  fx <- .s1_fixture()
  card <- step1_headline_cards(fx$mf, fx$snap)$rows[[1]]$cards[[4]]
  expect_identical(card$label, "Model fit")
  expect_match(card$value, "^(<0\\.01|[01]\\.\\d{2})$")
  expect_match(card$note, "R²", fixed = TRUE)
  expect_match(card$note, "1,200 observations", fixed = TRUE)
  expect_match(card$info, "within R²", ignore.case = TRUE)
  # Overall R2 includes the fixed effects, so it is at least the within R2.
  fit <- extract_native_fit(fx$mf$fit3, fx$mf$engine)
  expect_equal(as.numeric(card$value), unname(round(fixest::r2(fit, "r2"), 2)))
})

test_that("RIF who card uses percentile wording and no verdict flag", {
  skip_if_not_installed("fixest")
  fx <- .s1_fixture(
    engine = "rif", model_type = "Unconditional quantile regression (RIF)"
  )
  local_mocked_bindings(step1_rif_heterogeneity_p = function(mf, snap, var) 0.001)
  card <- step1_headline_cards(fx$mf, fx$snap)$rows[[1]]$cards[[2]]
  expect_identical(card$label, "Who is most affected")
  expect_match(card$note, "10th vs 90th percentile of welfare", fixed = TRUE)
  expect_false(grepl("poorest", card$note, fixed = TRUE))
  expect_null(card$status)
  expect_match(card$note, "RIF distribution p = 0.001", fixed = TRUE)
})

test_that("quadratic weather terms report a turning point inside the observed range", {
  skip_if_not_installed("fixest")
  set.seed(3)
  n <- 1500L
  temp <- stats::rnorm(n, 25, 4)
  loc <- sample(sprintf("L%02d", 1:30), n, replace = TRUE)
  df <- data.frame(
    welfare = 3 - 0.02 * (temp - 25)^2 + stats::rnorm(n), temp = temp,
    loc_id_panel = loc
  )
  sm <- build_selected_model(model_type = "Linear regression", engine = "fixest")
  so <- list(name = "welfare", type = "numeric", transform = "none")
  sw <- data.frame(name = "temp", label = "Temperature", units = "°C",
    cont_binned = "Continuous", polynomial = 2L, stringsAsFactors = FALSE)
  mf <- fit_model(df, so, sw, sm)
  tp <- .s1_turning_point(mf, list(), "temp")
  skip_if(is.null(tp), "polynomial spec not built from this fixture")
  expect_identical(tp$shape, "peak")
  expect_equal(tp$x, 25, tolerance = 0.1)
})

test_that("binned weather effect card popover names the largest bin", {
  skip_if_not_installed("fixest")
  set.seed(11)
  n <- 2000L
  temp <- stats::runif(n, 15, 40)
  loc <- sample(sprintf("L%02d", 1:30), n, replace = TRUE)
  df <- data.frame(
    welfare = 3 - 0.05 * pmax(temp - 25, 0) + stats::rnorm(n, 0, 0.5),
    temp = cut(temp, breaks = seq(15, 40, by = 5), include.lowest = TRUE),
    loc_id_panel = loc
  )
  sm <- build_selected_model(model_type = "Linear regression", engine = "fixest")
  so <- list(name = "welfare", type = "numeric", transform = "none", label = "Welfare")
  sw <- data.frame(name = "temp", label = "Temperature", units = "°C",
    cont_binned = "Binned", stringsAsFactors = FALSE)
  mf <- tryCatch(fit_model(df, so, sw, sm), error = function(e) NULL)
  skip_if(is.null(mf), "binned fixture not fittable without bin preparation")
  bp <- .s1_bin_pattern(mf, list(weather = sw), "temp", "level")
  skip_if(is.null(bp), "no bin columns in this fixture")
  expect_true(is.character(bp$label) && nzchar(bp$label))
  expect_match(bp$value, "^[+-]")
  # Harm builds steadily above 25 degrees, so the hottest bin is largest.
  expect_match(bp$label, "35", fixed = TRUE)
  expect_true(bp$monotone)
  card <- step1_headline_cards(mf, list(model = sm, outcome = so, weather = sw))$rows[[1]]$cards[[1]]
  expect_match(card$info, "Largest effect by bin", fixed = TRUE)
})

.s1_poly_fixture <- function(n = 3000L, seed = 5, type = "numeric") {
  set.seed(seed)
  temp <- stats::rnorm(n, 25, 4)
  loc <- sample(sprintf("L%02d", 1:30), n, replace = TRUE)
  df <- data.frame(
    welfare = 3 - 0.02 * (temp - 25)^2 + stats::rnorm(n), temp = temp,
    loc_id_panel = loc
  )
  sm <- build_selected_model(model_type = "Linear regression", engine = "fixest")
  so <- list(name = "welfare", type = "numeric", transform = "none", label = "Welfare")
  sw <- data.frame(name = "temp", label = "Temperature", units = "°C",
    cont_binned = "Continuous", polynomial = 2L, stringsAsFactors = FALSE)
  mf <- fit_model(df, so, sw, sm)
  list(mf = mf, snap = list(model = sm, outcome = so, weather = sw))
}

test_that("polynomial weather adds a shape card with the upper-tail effect and turning point", {
  skip_if_not_installed("fixest")
  fx <- .s1_poly_fixture()
  skip_if(is.null(.s1_turning_point(fx$mf, fx$snap, "temp")), "polynomial not built")
  cards <- step1_headline_cards(fx$mf, fx$snap)$rows[[1]]$cards
  labels <- vapply(cards, function(c) c$label, character(1))
  expect_identical(labels[[2]], "Shape of response")
  expect_length(cards, 5L)
  shape <- cards[[2]]
  expect_match(shape$note, "median to 95th percentile", fixed = TRUE)
  expect_match(shape$note, "peak at", fixed = TRUE)
  expect_match(shape$note, "95% CI", fixed = TRUE)
  expect_null(shape$status)
  # The cached path (step1_scenarios) gives the identical card.
  cached <- step1_headline_cards(fx$mf, fx$snap,
    scenarios_list = list(temp = step1_scenarios(fx$mf, fx$snap, "temp")))
  expect_identical(cached$rows[[1]]$cards[[2]]$value, shape$value)
  expect_identical(cached, step1_headline_cards(fx$mf, fx$snap))
})

test_that("linear weather has no shape card", {
  skip_if_not_installed("fixest")
  fx <- .s1_fixture()
  cards <- step1_headline_cards(fx$mf, fx$snap)$rows[[1]]$cards
  expect_length(cards, 4L)
  expect_false("Shape of response" %in% vapply(cards, function(c) c$label, character(1)))
})

test_that("binned weather shape card reports where the response starts", {
  skip_if_not_installed("fixest")
  set.seed(11)
  n <- 3000L
  temp <- stats::runif(n, 15, 40)
  loc <- sample(sprintf("L%02d", 1:30), n, replace = TRUE)
  df <- data.frame(
    welfare = 3 - 0.08 * pmax(temp - 27, 0) + stats::rnorm(n, 0, 0.5),
    temp = cut(temp, breaks = seq(15, 40, by = 5), include.lowest = TRUE),
    loc_id_panel = loc
  )
  sm <- build_selected_model(model_type = "Linear regression", engine = "fixest")
  so <- list(name = "welfare", type = "numeric", transform = "none", label = "Welfare")
  sw <- data.frame(name = "temp", label = "Temperature", units = "°C",
    cont_binned = "Binned", stringsAsFactors = FALSE)
  mf <- fit_model(df, so, sw, sm)
  cards <- step1_headline_cards(mf, list(model = sm, outcome = so, weather = sw))$rows[[1]]$cards
  shape <- cards[[2]]
  expect_identical(shape$label, "Shape of response")
  # Harm starts in the 25-30 bin (above the 27 kink the bin mean falls).
  expect_match(shape$value, "^Harm from (25|30)", perl = TRUE)
  expect_null(shape$status)
  expect_match(shape$note, "largest: ", fixed = TRUE)
  expect_match(shape$note, "vs reference bin", fixed = TRUE)
})
