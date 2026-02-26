# ============================================================================ #
# tests/testthat/test-fct_weatherstats.R                                       #
# ============================================================================ #

library(testthat)

# ---- shared test data -------------------------------------------------------

make_survey <- function(with_na = FALSE) {
  ts <- as.Date(c("2018-01-01", "2018-02-01", "2018-03-01"))
  if (with_na) ts <- c(ts, NA)
  data.frame(
    code     = "TST",
    year     = 2018L,
    survname = "SRV",
    loc_id   = "L1",
    timestamp = ts,
    weight   = rep(1, length(ts)),
    welfare  = c(3.5, 2.1, 4.8, if (with_na) 1.0),
    stringsAsFactors = FALSE
  )
}

make_weather <- function() {
  data.frame(
    code      = "TST",
    year      = 2018L,
    survname  = "SRV",
    loc_id    = "L1",
    timestamp = as.Date(c("2018-01-01", "2018-02-01", "2018-03-01")),
    tx        = c(28.1, 29.3, 27.5),
    stringsAsFactors = FALSE
  )
}

# ============================================================================ #
# extract_survey_dates                                                         #
# ============================================================================ #

test_that("extract_survey_dates returns sorted unique dates", {
  df  <- make_survey()
  out <- extract_survey_dates(df)
  expect_equal(out, sort(unique(df$timestamp)))
})

test_that("extract_survey_dates drops NA timestamps", {
  df  <- make_survey(with_na = TRUE)
  out <- extract_survey_dates(df)
  expect_false(anyNA(out))
  expect_equal(length(out), 3L)
})

test_that("extract_survey_dates returns empty Date vector for NULL input", {
  out <- extract_survey_dates(NULL)
  expect_s3_class(out, "Date")
  expect_equal(length(out), 0L)
})

test_that("extract_survey_dates returns empty Date vector when timestamp absent", {
  df  <- data.frame(x = 1:3)
  out <- extract_survey_dates(df)
  expect_equal(length(out), 0L)
})

# ============================================================================ #
# merge_survey_weather                                                         #
# ============================================================================ #

test_that("merge_survey_weather returns NULL for NULL inputs", {
  expect_null(merge_survey_weather(NULL, make_weather()))
  expect_null(merge_survey_weather(make_survey(), NULL))
})

test_that("merge_survey_weather joins correctly and returns correct nrow", {
  out <- merge_survey_weather(make_survey(), make_weather())
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 3L)
  expect_true("tx" %in% names(out))
})

test_that("merge_survey_weather converts year to factor", {
  out <- merge_survey_weather(make_survey(), make_weather())
  expect_s3_class(out$year, "factor")
})

test_that("merge_survey_weather normalises weights to sum to 1 per group", {
  out <- merge_survey_weather(make_survey(), make_weather())
  total_weight <- sum(out$weight, na.rm = TRUE)
  expect_equal(total_weight, 1, tolerance = 1e-6)
})

test_that("merge_survey_weather returns NULL when join produces zero rows", {
  wd <- make_weather()
  wd$code <- "XXX"   # no matching code
  expect_null(merge_survey_weather(make_survey(), wd))
})

# ============================================================================ #
# plot_weather_dist                                                            #
# ============================================================================ #

test_that("plot_weather_dist returns NULL for NULL df", {
  expect_null(plot_weather_dist(NULL, "tx", "Temp", "Continuous"))
})

test_that("plot_weather_dist returns NULL when hv not in df", {
  df <- data.frame(countryyear = "A, 2018", other = 1:5)
  expect_null(plot_weather_dist(df, "tx", "Temp", "Continuous"))
})

test_that("plot_weather_dist returns NULL when hv is NA", {
  df <- data.frame(countryyear = "A, 2018", tx = 1:5)
  expect_null(plot_weather_dist(df, NA_character_, "Temp", "Continuous"))
})

test_that("plot_weather_dist returns ggplot for continuous variable", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggridges")
  df <- merge_survey_weather(make_survey(), make_weather()) |>
    dplyr::mutate(countryyear = paste0("TST, ", year))
  p <- plot_weather_dist(df, "tx", "Max temp", "Continuous")
  expect_s3_class(p, "ggplot")
})

test_that("plot_weather_dist returns ggplot for binned variable", {
  skip_if_not_installed("ggplot2")
  df <- merge_survey_weather(make_survey(), make_weather()) |>
    dplyr::mutate(
      countryyear = paste0("TST, ", year),
      tx = cut(tx, breaks = c(-Inf, 28, Inf), include.lowest = TRUE)
    )
  p <- plot_weather_dist(df, "tx", "Max temp", "Binned")
  expect_s3_class(p, "ggplot")
})

# ============================================================================ #
# plot_binscatter                                                              #
# ============================================================================ #

test_that("plot_binscatter returns NULL for NULL df", {
  expect_null(plot_binscatter(NULL, "tx", "Temp", "welfare", "Welfare"))
})

test_that("plot_binscatter returns NULL when hv is NA", {
  df <- merge_survey_weather(make_survey(), make_weather())
  expect_null(plot_binscatter(df, NA_character_, "Temp", "welfare", "Welfare"))
})

test_that("plot_binscatter returns NULL when hv not in df", {
  df <- merge_survey_weather(make_survey(), make_weather())
  expect_null(plot_binscatter(df, "missing_var", "Temp", "welfare", "Welfare"))
})

test_that("plot_binscatter returns NULL when y_var not in df", {
  df <- merge_survey_weather(make_survey(), make_weather())
  expect_null(plot_binscatter(df, "tx", "Temp", "missing_outcome", "Outcome"))
})

test_that("plot_binscatter returns NULL when no finite data remain", {
  df <- merge_survey_weather(make_survey(), make_weather())
  df$welfare <- NA_real_
  expect_null(plot_binscatter(df, "tx", "Temp", "welfare", "Welfare"))
})

test_that("plot_binscatter returns ggplot for continuous outcome", {
  skip_if_not_installed("ggplot2")
  df <- merge_survey_weather(make_survey(), make_weather())
  p  <- plot_binscatter(df, "tx", "Max temp", "welfare", "Welfare (PPP)")
  expect_s3_class(p, "ggplot")
})

test_that("plot_binscatter returns ggplot for binary outcome", {
  skip_if_not_installed("ggplot2")
  df <- merge_survey_weather(make_survey(), make_weather()) |>
    dplyr::mutate(poor = as.integer(welfare < 3))
  p  <- plot_binscatter(df, "tx", "Max temp", "poor", "Poor (0/1)")
  expect_s3_class(p, "ggplot")
})
