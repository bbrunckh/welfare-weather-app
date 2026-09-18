# UI-45: every table exports to CSV through one shared affordance.

test_that("make_regtable_df returns one row per specification and term", {
  skip_if_not_installed("fixest")
  set.seed(42)
  d <- data.frame(y = rnorm(200), x1 = rnorm(200), x2 = rnorm(200),
                  g = factor(sample(1:5, 200, TRUE)))
  f1 <- fixest::feols(y ~ x1, data = d)
  f2 <- fixest::feols(y ~ x1 | g, data = d)
  f3 <- fixest::feols(y ~ x1 + x2 | g, data = d)

  df <- make_regtable_df(f1, f2, f3, engine = "fixest")
  expect_s3_class(df, "data.frame")
  expect_true(all(c("Model", "Variable", "Estimate", "Std. error", "p value")
                  %in% names(df)))
  expect_setequal(unique(df$Model),
                  c("(1) No FE", "(2) FE", "(3) FE + Controls"))
  # x2 only enters the third specification.
  expect_equal(unique(df$Model[df$Term == "x2"]), "(3) FE + Controls")
  # Estimates are numeric, not the starred display strings.
  expect_type(df$Estimate, "double")
  expect_equal(
    df$Estimate[df$Model == "(1) No FE" & df$Term == "x1"],
    unname(stats::coef(f1)["x1"])
  )
})

test_that("make_regtable_df labels terms via label_fun", {
  skip_if_not_installed("fixest")
  set.seed(1)
  d <- data.frame(y = rnorm(100), x1 = rnorm(100))
  f <- fixest::feols(y ~ x1, data = d)
  df <- make_regtable_df(f, f, f, engine = "fixest",
                         label_fun = function(x) ifelse(x == "x1",
                                                        "Rainfall (mm)", x))
  expect_true("Rainfall (mm)" %in% df$Variable)
  # The raw term is kept alongside the label.
  expect_true("x1" %in% df$Term)
})

test_that("make_regtable_df reads the RIF grid with matching column names", {
  grid <- data.frame(
    model     = c(1L, 3L, 3L, 3L, 3L),
    term      = c("x1", "x1", "x1", "x2", "x2"),
    tau       = c(0.5, 0.25, 0.75, 0.25, 0.75),
    estimate  = c(99, 1, 2, 3, 4),
    std.error = c(9, 0.1, 0.2, 0.3, 0.4),
    p.value   = c(0.9, 0.01, 0.02, 0.03, 0.04)
  )
  df <- make_regtable_df(NULL, NULL, NULL, engine = "rif", rif_grid = grid)

  # Only the full specification (model 3) is reported.
  expect_equal(nrow(df), 4L)
  expect_false(99 %in% df$Estimate)
  expect_equal(names(df)[1:3], c("Model", "Variable", "Quantile"))
  expect_true(all(c("Estimate", "Std. error", "p value") %in% names(df)))
})

test_that("make_regtable_df returns NULL when there is nothing to export", {
  expect_null(make_regtable_df(NULL, NULL, NULL))
  expect_null(make_regtable_df(NULL, NULL, NULL, engine = "rif",
                               rif_grid = data.frame(model = 1L, term = "x",
                                                     tau = 0.5, estimate = 1)))
})

test_that("wise_reactable_csv_button wires the client-side export call", {
  btn <- wise_reactable_csv_button("ss-hh_stats", "survey_summary_hh")
  html <- as.character(shiny::HTML(as.character(btn)))
  # The browser decodes &quot; in the onclick attribute back to quotes, so
  # the executed call is Reactable.downloadDataCSV("ss-hh_stats", "...csv").
  expect_match(
    html,
    paste0(
      'Reactable.downloadDataCSV(&quot;ss-hh_stats&quot;, ',
      '&quot;survey_summary_hh.csv&quot;)'
    ),
    fixed = TRUE
  )
  expect_match(html, "wise-csv-btn", fixed = TRUE)
})
