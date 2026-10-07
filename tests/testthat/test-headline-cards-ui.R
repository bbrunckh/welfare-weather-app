test_that("headline_status is direction-aware and significance-aware", {
  expect_identical(
    headline_status(-1, "lower_is_better")$kind, "favourable"
  )
  expect_identical(
    headline_status(1, "lower_is_better")$kind, "adverse"
  )
  expect_identical(
    headline_status(1, "higher_is_better")$text, "Favourable"
  )
  sig <- headline_status(-1, "lower_is_better", significant = TRUE)
  expect_identical(sig$text, "Significant")
  expect_identical(sig$kind, "favourable")
  ns <- headline_status(-1, "lower_is_better", significant = FALSE)
  expect_identical(ns$kind, "uncertain")
  expect_identical(ns$text, "Not significant")
  # Unknown direction or no change: nothing to colour or word.
  expect_null(headline_status(1, "unknown"))
  expect_null(headline_status(NA_real_, "lower_is_better"))
  expect_identical(
    headline_status(1, "unknown", significant = TRUE)$kind, "neutral"
  )
})

test_that("headline_cards_ui moves basis_only cards into a basis strip", {
  cards <- list(
    list(label = "A", value = "1", status = list(kind = "adverse", text = "Adverse")),
    list(
      label = "Simulation years", value = "6", basis_only = TRUE,
      basis_text = "6 simulation years", info = "Provenance detail."
    ),
    list(label = "B", value = "2", basis_text = "extra basis")
  )
  html <- as.character(headline_cards_ui(cards))
  expect_equal(lengths(regmatches(html, gregexpr("headline-card ", html))), 2L)
  expect_match(html, "status-adverse", fixed = TRUE)
  expect_match(html, "headline-card-flag", fixed = TRUE)
  expect_match(html, "headline-basis", fixed = TRUE)
  expect_match(html, "6 simulation years", fixed = TRUE)
  expect_match(html, "extra basis", fixed = TRUE)
  expect_false(grepl(">Simulation years<", html, fixed = TRUE))
  expect_null(headline_cards_ui(NULL))
})

test_that("headline_status gives no flag for a change that renders as zero", {
  expect_null(headline_status(-1e-6, "higher_is_better", display = "-0.00 pp"))
  expect_null(headline_status(1e-6, "lower_is_better", display = "+0.0"))
  expect_identical(
    headline_status(-0.5, "higher_is_better", display = "-0.50 pp")$kind, "adverse"
  )
  expect_identical(
    headline_status(-1200, "higher_is_better", display = "-1,200 outcome units")$kind,
    "adverse"
  )
})
