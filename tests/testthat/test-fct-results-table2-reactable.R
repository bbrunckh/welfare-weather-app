# ============================================================================ #
# tests/testthat/test-fct-results-table2-reactable.R                           #
# Reactable Step 1 regression tables (guidelines §6): focused table (fixest    #
# + RIF pivot), specification comparison, and the full make_regtable_df view.  #
# ============================================================================ #

library(testthat)

# Reactable serialises the row data as column-oriented JSON on the widget tag.
.reactable_rows <- function(w) {
  cols <- jsonlite::fromJSON(w$x$tag$attribs$data, simplifyVector = TRUE)
  do.call(data.frame, c(cols, list(check.names = FALSE, stringsAsFactors = FALSE)))
}

.t2_rows <- function() {
  data.frame(
    Variable = c("Temperature", "Temperature \u00d7 Urban"),
    Group = c("Weather effects", "Interactions"),
    Term = c("tmax_1to3m", "tmax_1to3m:urban"),
    Effect = c(0.1234567, -0.05),
    CI_low = c(0.05, -0.09),
    CI_high = c(0.2, -0.01),
    SE = c(0.03, 0.02),
    p = c(0.0004, 0.07),
    Translation = c("+2.3%", "-1.1 pp"),
    stringsAsFactors = FALSE
  )
}

test_that("focused reactable injects group separators and formats cells", {
  w <- make_regtable_focused_reactable(.t2_rows())
  expect_s3_class(w, "reactable")

  dat <- .reactable_rows(w)
  # Two data rows + one separator row before each group (old HTML behaviour).
  expect_equal(nrow(dat), 4L)
  expect_true(all(c(".t2grp", "Variable", "Effect", "95% CI", "SE", "p") %in% names(dat)))
  # Separator rows carry the group label in the Variable column.
  expect_equal(dat$Variable[dat$.t2grp == "sep"], c("Weather effects", "Interactions"))
  # Weather rows marked for the tint class, interaction rows not.
  expect_setequal(dat$.t2grp, c("sep", "wx", ""))
  # Stars, CI dash and the p threshold formatting match the old .wise-table.
  wx <- dat[dat$.t2grp == "wx", ]
  expect_match(wx$Effect, "0.123***", fixed = TRUE)
  expect_match(wx[["95% CI"]], "0.050 \u2013 0.200", fixed = TRUE)
  inter <- dat[dat$.t2grp == "", ]
  expect_match(inter$Effect, "-0.050\u2020", fixed = TRUE)
  expect_match(inter$p, "0.070", fixed = TRUE)
})

test_that("focused reactable falls back to a note when there are no rows", {
  w <- make_regtable_focused_reactable(NULL)
  expect_s3_class(w, "reactable")
  expect_match(.reactable_rows(w)$Note[1], "unavailable", fixed = TRUE)
})

test_that("focused reactable pivots the RIF grid over taus", {
  rows <- do.call(rbind, lapply(c(0.1, 0.5, 0.9), function(t) data.frame(
    Variable = "Temperature", Group = "Weather effects", Term = "t",
    Tau = t, Effect = 0.1 * t, CI_low = 0, CI_high = 0.3, SE = 0.01,
    p = 0.001,
    Translation = if (t == 0.5) "+1.2%" else NA_character_,
    stringsAsFactors = FALSE
  )))
  w <- make_regtable_focused_reactable(rows, engine = "rif")
  expect_s3_class(w, "reactable")
  dat <- .reactable_rows(w)
  expect_true(all(c("\u03c4 = 0.1", "\u03c4 = 0.5", "\u03c4 = 0.9") %in% names(dat)))
  # Translation column and only the tau = 0.5 entry is translated. Row 1 is
  # the group separator, the data row follows.
  expect_match(dat[["Per +1 SD (\u03c4 = 0.5)"]][2], "+1.2%", fixed = TRUE)
  expect_match(dat[["\u03c4 = 0.5"]][2], "0.050** (0.010)", fixed = TRUE)
})

test_that("specs reactable compares three specifications with (SE) cells", {
  set.seed(7)
  d <- data.frame(
    y = rnorm(200), x1 = rnorm(200), x2 = rnorm(200),
    g = factor(sample(1:4, 200, TRUE))
  )
  f1 <- fixest::feols(y ~ x1, data = d)
  f2 <- fixest::feols(y ~ x1 | g, data = d)
  f3 <- fixest::feols(y ~ x1 + x2 | g, data = d)

  w <- make_regtable_specs_reactable(f1, f2, f3, "x1", character(0))
  expect_s3_class(w, "reactable")
  dat <- .reactable_rows(w)
  expect_true(all(c("(1) No FE", "(2) FE", "(3) FE + Controls") %in% names(dat)))
  expect_match(dat[["(1) No FE"]][2], "(", fixed = TRUE)
  expect_match(dat[["(1) No FE"]][2], ")", fixed = TRUE)
  # Group separator precedes the single weather row.
  expect_equal(dat$.t2grp[1], "sep")
  expect_equal(dat$Variable[1], "Weather effects")
  expect_equal(dat$.t2grp[2], "wx")
})

test_that("specs reactable returns NULL for the RIF engine", {
  f <- fixest::feols(y ~ x1, data = data.frame(y = 1:10, x1 = 1:10))
  expect_null(make_regtable_specs_reactable(f, f, f, "x1", character(0), engine = "rif"))
})

test_that("full-coefficient reactable renders the tidy df and notes on empty", {
  df <- data.frame(
    Model = "(1) No FE", Variable = "Rainfall (mm)", Term = "x1",
    Estimate = 1.234, `Std. error` = 0.12, `p value` = 0.001,
    Observations = 200L, check.names = FALSE, stringsAsFactors = FALSE
  )
  w <- make_regtable_df_reactable(df)
  expect_s3_class(w, "reactable")
  dat <- .reactable_rows(w)
  expect_equal(nrow(dat), 1L)
  expect_true(all(c("Model", "Variable", "Estimate", "Std. error", "p value")
                  %in% names(dat)))

  wn <- make_regtable_df_reactable(NULL)
  expect_s3_class(wn, "reactable")
  expect_match(.reactable_rows(wn)$Note[1], "No coefficients", fixed = TRUE)
})
