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

test_that("merge_survey_weather preserves raw survey weights", {
  sv  <- make_survey()
  out <- merge_survey_weather(sv, make_weather())
  expect_equal(sum(out$weight, na.rm = TRUE), sum(sv$weight, na.rm = TRUE))
})

test_that("merge_survey_weather returns NULL when join produces zero rows", {
  wd <- make_weather()
  wd$code <- "XXX"   # no matching code
  expect_null(merge_survey_weather(make_survey(), wd))
})

test_that("CR-BUG-16: merge_survey_weather counts dropped records", {
  sv <- make_survey(with_na = TRUE)
  expect_message(
    out <- merge_survey_weather(sv, make_weather()),
    "1 of 4 survey records have no matching weather"
  )
  expect_equal(nrow(out), 3L)
  expect_identical(attr(out, "n_dropped"), 1L)
  out <- merge_survey_weather(make_survey(), make_weather())
  expect_identical(attr(out, "n_dropped"), 0L)
})

test_that("CR-BUG-16: duplicated weather keys error instead of multiplying records", {
  wd <- rbind(make_weather(), make_weather()[1, ])
  expect_error(merge_survey_weather(make_survey(), wd), "many-to-one|multiple")
})

test_that("CR-BUG-16: LCU -> PPP join guards duplicates and counts unmatched", {
  df <- data.frame(
    code = c("A", "A", "B"), year = 2020L, data_level = "national",
    welfare_lcu = c(100, 200, 300), stringsAsFactors = FALSE
  )
  defl <- data.frame(
    code = "A", year = 2020L, data_level = "national",
    cpi = 1, ppp2021 = 10, stringsAsFactors = FALSE
  )
  expect_message(
    out <- convert_lcu_to_ppp(df, defl, "welfare_lcu"),
    "1 of 3 survey records have no CPI/PPP deflator"
  )
  expect_equal(nrow(out), 3L)
  expect_equal(out$welfare_lcu, c(10, 20, NA))
  expect_error(
    convert_lcu_to_ppp(df, rbind(defl, defl), "welfare_lcu"),
    "many-to-one|multiple"
  )
})

test_that("historical cell joins preserve duplicate-sensitive multiplicity", {
  hist <- data.frame(
    code = c("A", "A", "A", "A"),
    year = 2020L,
    survname = "S",
    loc_id = "L1",
    timestamp = as.Date(c("2018-01-01", "2018-01-01", "2018-02-01", "2018-01-01")),
    tx = 1:4,
    stringsAsFactors = FALSE
  )
  survey <- data.frame(
    code = c("A", "A", "A", "A"),
    year = 2020L,
    survname = "S",
    loc_id = "L1",
    timestamp = as.Date(c("2020-01-01", "2020-01-01", "2020-02-01", NA)),
    economy = "Alpha",
    stringsAsFactors = FALSE
  )

  out <- join_hist_sample_cells(hist, survey)
  expect_equal(nrow(out), 4L)
  expect_equal(out$tx, 1:4)
  expect_equal(out$n_hh, c(2L, 2L, 1L, 2L))
  expect_equal(out$is_sample, rep(FALSE, 4L))
  expect_equal(out$countryyear, rep("Alpha, 2020", 4L))
})

test_that("historical cell joins retain NA-key rows and exact sample dates", {
  hist <- data.frame(
    code = c(NA, "A", "A"), year = 2020L, survname = "S",
    loc_id = "L1", timestamp = as.Date(c("2018-01-01", "2020-01-01", NA)),
    tx = 1:3, stringsAsFactors = FALSE
  )
  survey <- data.frame(
    code = c(NA, "A"), year = 2020L, survname = "S", loc_id = "L1",
    timestamp = as.Date(c("2020-01-01", "2020-01-01")),
    economy = c("Missing code", "Alpha"), stringsAsFactors = FALSE
  )

  out <- join_hist_sample_cells(hist, survey)
  expect_equal(nrow(out), 2L)
  expect_true(is.na(out$code[[1L]]))
  expect_equal(out$economy, c("Missing code", "Alpha"))
  expect_equal(out$is_sample, c(FALSE, TRUE))
  expect_equal(out$n_hh, c(1L, 1L))
  expect_type(out$timestamp, "double")
  expect_s3_class(out$timestamp, "Date")
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

test_that("interactive binned weather distribution keeps labels and tooltip visible", {
  skip_if_not_installed("echarts4r")
  df <- data.frame(
    countryyear = rep(c("TST, 2018", "TST, 2021"), each = 4),
    tx = factor(
      rep(c("[0, 10]", "(10, 20]"), 4),
      levels = c("[0, 10]", "(10, 20]")
    ),
    stringsAsFactors = FALSE
  )

  chart <- echart_weather_bins_compare(df, "tx", "Max temp", units = "C")

  expect_s3_class(chart, "echarts4r")
  expect_null(names(chart$x$opts$xAxis$data))
  expect_equal(chart$x$opts$xAxis$axisLabel$rotate, 0)
  expect_null(chart$x$opts$yAxis$name)
  expect_null(chart$x$opts$title)
  expect_equal(chart$x$opts$grid$bottom, 84)
  expect_true(!is.null(chart$x$opts$tooltip$formatter))
})

test_that("interactive binscatter serializes readable bin labels as strings", {
  skip_if_not_installed("echarts4r")
  binned <- data.frame(
    tx = factor(
      rep(c("[0, 10]", "(10, 20]"), 20),
      levels = c("[0, 10]", "(10, 20]")
    ),
    welfare = seq(1, 3, length.out = 40)
  )
  continuous <- data.frame(
    tx = seq(0, 20, length.out = 40),
    welfare = seq(1, 3, length.out = 40)
  )

  binned_chart <- echart_binscatter(
    binned, "tx", "Temperature bins", "welfare", "Welfare"
  )
  continuous_chart <- echart_binscatter(
    continuous, "tx", "Temperature", "welfare", "Welfare"
  )

  expect_null(names(binned_chart$x$opts$xAxis$data))
  expect_equal(
    binned_chart$x$opts$xAxis$data,
    paste0(c("0 ", "10 "), intToUtf8(8211), c(" 10", " 20")),
    ignore_attr = TRUE
  )
  expect_null(names(continuous_chart$x$opts$series[[3]]$data[[1]]$binLabel))
  expect_equal(
    continuous_chart$x$opts$series[[3]]$data[[1]]$binLabel,
    paste0("0 ", intToUtf8(8211), " 1")
  )
})

test_that("historical binned counts use the survey's finite outer labels", {
  hist <- data.frame(
    countryyear = "TST, historical",
    n_hh = c(1, 1, 1),
    tx = c(10, 25, 40),
    cal_year = c(2018, 2018, 2018)
  )
  breaks <- c(-Inf, 20, 30, Inf)
  attr(breaks, "observed") <- c(10, 20, 30, 40)

  out <- wiseapp:::.hist_bin_counts(hist, "tx", breaks)

  expect_false(anyNA(out$bin))
  expect_setequal(out$bin, c("[10.0, 20.0]", "(20.0, 30.0]", "(30.0, 40.0]"))
})

test_that("weather wave palette follows the app blue and teal series", {
  pal <- wiseapp:::.wave_palette(c("A, 2020", "A, 2021", "B, 2020"))
  expect_equal(unname(pal), c("#0071BC", "#00A6C7", "#8667B3"))
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

test_that("plot_binscatter handles binned weather with continuous outcome", {
  skip_if_not_installed("ggplot2")
  df <- merge_survey_weather(make_survey(), make_weather()) |>
    dplyr::mutate(
      tx = cut(tx, breaks = c(-Inf, 28, Inf), include.lowest = TRUE)
    )
  p <- plot_binscatter(df, "tx", "Max temp", "welfare", "Welfare")
  expect_s3_class(p, "ggplot")
})

test_that("plot_binscatter handles binned weather with binary outcome", {
  skip_if_not_installed("ggplot2")
  df <- merge_survey_weather(make_survey(), make_weather()) |>
    dplyr::mutate(
      tx = cut(tx, breaks = c(-Inf, 28, Inf), include.lowest = TRUE),
      poor = as.integer(welfare < 3)
    )
  p <- plot_binscatter(df, "tx", "Max temp", "poor", "Poor")
  expect_s3_class(p, "ggplot")
})

test_that("plot_binscatter samples large tibbles with integer row indices", {
  skip_if_not_installed("ggplot2")
  df <- tibble::tibble(
    tx = rep(seq(20, 40, length.out = 100), 40),
    welfare = rep(c(1, 2, 3, 4), 1000)
  )
  p <- plot_binscatter(df, "tx", "Max temp", "welfare", "Welfare")
  expect_s3_class(p, "ggplot")
})

# ============================================================================ #
# binned weather summary table                                                   #
# ============================================================================ #

test_that("binned weather summary aggregates all variables in shared groups", {
  df <- data.frame(
    code = c("A", "A", "A", "A", "B", "B"),
    year = c(2018L, 2018L, 2018L, 2018L, 2019L, 2019L),
    survname = "SRV",
    loc_id = seq_len(6),
    timestamp = as.Date(c("2018-01-01", "2018-02-01", "2018-03-01",
                          "2018-04-01", "2019-01-01", "2019-02-01")),
    economy = c(rep("Alpha", 4), rep("Beta", 2)),
    countryyear = c(rep("Alpha, 2018", 4), rep("Beta, 2019", 2)),
    temp_bin = factor(c("Low", "High", "Low", NA, "High", "Low"),
                      levels = c("Low", "High", "Unused")),
    rain_bin = c("Dry", "Dry", "Wet", "Wet", "Dry", NA),
    stringsAsFactors = FALSE
  )
  selected <- data.frame(
    name = c("temp_bin", "rain_bin"),
    label = c("Temperature", "Rainfall"),
    stringsAsFactors = FALSE
  )

  out <- build_weather_binned_table(
    survey_weather = function() df,
    selected_weather = function() selected
  )

  expect_named(out, c("Variable", "Country, Year", "Level", "N",
                      "Share (%)", "% Missing"))
  expect_equal(
    out[, c("Variable", "Country, Year", "Level", "N")],
    data.frame(
      Variable = c("Rainfall", "Rainfall", "Rainfall", "Temperature",
                   "Temperature", "Temperature", "Temperature"),
      `Country, Year` = c("Alpha, 2018", "Alpha, 2018", "Beta, 2019",
                          "Alpha, 2018", "Alpha, 2018", "Beta, 2019",
                          "Beta, 2019"),
      Level = c("Dry", "Wet", "Dry", "Low", "High", "Low", "High"),
      N = c(2L, 2L, 1L, 2L, 1L, 1L, 1L),
      stringsAsFactors = FALSE
    ),
    ignore_attr = TRUE
  )
  expect_equal(
    out$`Share (%)`,
    c(50, 50, 100, 66.6666667, 33.3333333, 50, 50),
    tolerance = 1e-7
  )
  expect_equal(
    out$`% Missing`,
    c(0, 0, 50, 25, 25, 0, 0),
    tolerance = 1e-10
  )
})

test_that("A11Y-ALT: weather_plot_layout names every chart slot", {
  ns <- shiny::NS("m")
  for (ec in c(FALSE, TRUE)) {
    one <- as.character(weather_plot_layout(ns, 1L, c("p1", "p2"),
                                            echarts = ec))
    expect_match(one, 'role="img"', fixed = TRUE)
    expect_match(one, 'aria-label="Chart for the selected weather variable"',
                 fixed = TRUE)
    two <- as.character(weather_plot_layout(ns, 2L, c("p1", "p2"),
                                            alts = c("Custom alt", NA),
                                            echarts = ec))
    expect_match(two, 'aria-label="Custom alt"', fixed = TRUE)
    expect_match(two, 'aria-label="Chart for weather variable 2 of 2"',
                 fixed = TRUE)
  }
})
