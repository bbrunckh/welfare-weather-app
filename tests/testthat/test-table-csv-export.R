# UI-45: every table exports to CSV through one shared affordance.

test_that("wise_csv_dom adds the Buttons placeholder exactly once", {
  expect_equal(wise_csv_dom("t"), "Bt")
  expect_equal(wise_csv_dom("lfrtip"), "Blfrtip")
  expect_equal(wise_csv_dom("tip"), "Btip")
  # Already carries a B - left alone rather than gaining a second toolbar.
  expect_equal(wise_csv_dom("Btip"), "Btip")
  expect_equal(wise_csv_dom("Bt"), "Bt")
})

test_that("wise_csv_button emits one discreet, fully-exporting CSV button", {
  b <- wise_csv_button("my_table")
  expect_length(b, 1L)
  spec <- b[[1]]
  expect_equal(spec$extend, "csv")
  expect_equal(spec$text, "Download CSV")
  expect_equal(spec$filename, "my_table")
  expect_equal(spec$className, "wise-csv-btn")
  # Paginated tables must still export every row, not the visible page.
  expect_equal(spec$exportOptions$modifier$page, "all")
})

test_that("wise_csv_button is withheld when disabled (stale results, INT-08)", {
  expect_null(wise_csv_button("my_table", enabled = FALSE))
  expect_null(wise_csv_button("my_table", enabled = NA))
  expect_null(wise_csv_button("my_table", enabled = NULL))
})

test_that("a DT built with the helpers carries the Buttons extension", {
  dt <- DT::datatable(
    head(iris),
    extensions = "Buttons",
    options = list(dom = wise_csv_dom("t"),
                   buttons = wise_csv_button("iris"))
  )
  expect_s3_class(dt, "datatables")
  expect_true("Buttons" %in% dt$x$extensions)
  expect_equal(dt$x$options$dom, "Bt")
  expect_equal(dt$x$options$buttons[[1]]$text, "Download CSV")
})

# shiny::downloadHandler() closes over `filename` / `content` one level below
# the returned render function; reach them to exercise the handler directly.
.handler_parts <- function(h) environment(environment(h)$renderFunc)

test_that("csv_download_handler writes the data frame it is given", {
  h <- csv_download_handler("stats", function() {
    data.frame(Statistic = c("Mean", "N"), Value = c("1.5", "10"))
  })
  expect_type(h, "closure")

  f <- withr::local_tempfile(fileext = ".csv")
  .handler_parts(h)$content(f)
  out <- utils::read.csv(f, stringsAsFactors = FALSE)
  expect_equal(names(out), c("Statistic", "Value"))
  expect_equal(out$Statistic, c("Mean", "N"))
})

test_that("csv_download_handler degrades to a note instead of erroring", {
  f <- withr::local_tempfile(fileext = ".csv")

  # No data at all.
  h_null <- csv_download_handler("stats", function() NULL)
  .handler_parts(h_null)$content(f)
  expect_equal(utils::read.csv(f)$Note, "No data available")

  # The producing reactive threw - the download must still resolve.
  h_err <- csv_download_handler("stats", function() stop("boom"))
  .handler_parts(h_err)$content(f)
  expect_equal(utils::read.csv(f)$Note, "No data available")
})

test_that("csv_download_handler stamps the filename with the date", {
  h <- csv_download_handler("model_coefficients", function() head(iris))
  expect_equal(
    .handler_parts(h)$filename(),
    paste0("model_coefficients_", format(Sys.Date(), "%Y%m%d"), ".csv")
  )
})


# UI-45: the coefficient table is presentation HTML, so its CSV comes from a
# tidy frame of the same estimates.

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
