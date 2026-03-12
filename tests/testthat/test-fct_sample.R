library(testthat)

# ---- list_available_files ---------------------------------------------------

test_that("list_available_files returns NULL for remote types", {
  expect_null(list_available_files(list(type = "s3",    bucket = "b")))
  expect_null(list_available_files(list(type = "gcs",   bucket = "b")))
  expect_null(list_available_files(list(type = "azure", account = "a", container = "c")))
  expect_null(list_available_files(list(type = "hf",    repo = "r")))
  expect_null(list_available_files(NULL))
})

test_that("list_available_files returns character(0) when local path missing", {
  expect_equal(list_available_files(list(type = "local", path = "/does/not/exist")), character(0))
  expect_equal(list_available_files(list(type = "local", path = "")),               character(0))
})

# ---- build_survey_fnames ----------------------------------------------------

sl <- data.frame(
  code     = c("ZAF", "ZAF", "NGA"),
  year     = c(2018,   2022,   2019),
  survname = c("NIDS", "NIDS", "GHS"),
  level    = c("hh",   "hh",   "hh"),
  economy  = c("South Africa", "South Africa", "Nigeria"),
  stringsAsFactors = FALSE
)

test_that("build_survey_fnames filters to correct unit", {
  out <- build_survey_fnames(sl, "hh", list(type = "local", path = "data/"))
  expect_equal(nrow(out), 3L)
})

test_that("build_survey_fnames constructs correct fname", {
  out <- build_survey_fnames(sl, "hh", list(type = "local", path = "data/"))
  expect_equal(out$fname[1], "ZAF_2018_NIDS_hh.parquet")
})

test_that("build_survey_fnames uses bare fname for remote", {
  out <- build_survey_fnames(sl, "hh", list(type = "s3", bucket = "b"))
  expect_equal(out$fpath[1], out$fname[1])
})

test_that("build_survey_fnames returns empty when survey_list is empty", {
  expect_equal(nrow(build_survey_fnames(sl[0L, ], "hh", list(type = "local", path = "data/"))), 0L)
})

# ---- filter_surveys_to_available --------------------------------------------

surveys <- data.frame(fname = c("A.parquet", "B.parquet"), stringsAsFactors = FALSE)

test_that("filter_surveys_to_available returns all rows when available_files is NULL", {
  expect_equal(nrow(filter_surveys_to_available(surveys, NULL)), 2L)
})

test_that("filter_surveys_to_available filters correctly", {
  out <- filter_surveys_to_available(surveys, "A.parquet")
  expect_equal(nrow(out), 1L)
  expect_equal(out$fname, "A.parquet")
})

# ---- get_available_years ----------------------------------------------------

test_that("get_available_years returns sorted years per code", {
  out <- get_available_years(sl, c("ZAF", "NGA"))
  expect_equal(out$ZAF, c(2018, 2022))
  expect_equal(out$NGA, 2019)
})

test_that("get_available_years returns empty list for empty surveys", {
  out <- get_available_years(sl[0L, ], "ZAF")
  expect_length(out$ZAF, 0L)
})

# ---- build_selected_surveys -------------------------------------------------

test_that("build_selected_surveys inner-joins correctly", {
  surveys_full <- build_survey_fnames(sl, "hh", list(type = "local", path = "data/"))
  out <- build_selected_surveys(surveys_full, list(ZAF = c("2018"), NGA = c("2019")))
  expect_equal(nrow(out), 2L)
  expect_setequal(out$code, c("ZAF", "NGA"))
})

test_that("build_selected_surveys drops NULL entries from years_by_code", {
  surveys_full <- build_survey_fnames(sl, "hh", list(type = "local", path = "data/"))
  out <- build_selected_surveys(surveys_full, list(ZAF = NULL, NGA = c("2019")))
  expect_equal(nrow(out), 1L)
  expect_equal(out$code, "NGA")
})

test_that("build_selected_surveys returns zero rows when years_by_code is empty", {
  surveys_full <- build_survey_fnames(sl, "hh", list(type = "local", path = "data/"))
  out <- build_selected_surveys(surveys_full, list())
  expect_equal(nrow(out), 0L)
})