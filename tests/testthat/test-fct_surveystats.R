library(testthat)

# ---- add_time_columns -------------------------------------------------------

test_that("add_time_columns adds timestamp, month, countryyear", {
  df  <- data.frame(int_year = 2018L, int_month = 3L, economy = "A", year = 2018L)
  out <- add_time_columns(df)
  expect_equal(out$timestamp,   as.Date("2018-03-01"))
  expect_equal(out$month,       3L)
  expect_equal(out$countryyear, "A, 2018")
})

test_that("add_time_columns handles multiple rows", {
  df  <- data.frame(int_year = c(2018L,2019L), int_month = c(1L,12L),
                    economy = c("A","B"), year = c(2018L,2019L))
  out <- add_time_columns(df)
  expect_equal(out$timestamp, as.Date(c("2018-01-01","2019-12-01")))
})

# ---- assign_data_level ------------------------------------------------------

test_that("assign_data_level national for non-CHN", {
  expect_equal(assign_data_level(data.frame(code="ZAF", urban=1L))$data_level, "national")
})

test_that("assign_data_level urban/rural for CHN", {
  df  <- data.frame(code=c("CHN","CHN"), urban=c(1L,0L))
  out <- assign_data_level(df)
  expect_equal(out$data_level, c("urban","rural"))
})

# ---- get_lcu_vars -----------------------------------------------------------

test_that("get_lcu_vars returns LCU vars in df", {
  df <- data.frame(welfare=1, age=2)
  vl <- data.frame(name=c("welfare","age"), units=c("LCU","years"), stringsAsFactors=FALSE)
  expect_equal(get_lcu_vars(df, vl), "welfare")
})

test_that("get_lcu_vars returns character(0) when no LCU vars", {
  df <- data.frame(age=1)
  vl <- data.frame(name="age", units="years", stringsAsFactors=FALSE)
  expect_equal(get_lcu_vars(df, vl), character(0))
})

# ---- convert_lcu_to_ppp -----------------------------------------------------

test_that("convert_lcu_to_ppp divides by cpi * ppp2021", {
  df      <- data.frame(code="ZAF", year=2018L, data_level="national", welfare=1000)
  cpi_ppp <- data.frame(code="ZAF", year=2018L, data_level="national", cpi=2, ppp2021=5)
  out     <- convert_lcu_to_ppp(df, cpi_ppp, lcu_vars="welfare")
  expect_equal(out$welfare, 100)
})

test_that("convert_lcu_to_ppp returns df unchanged for empty lcu_vars", {
  df  <- data.frame(code="ZAF", year=2018L, welfare=1000)
  cpi <- data.frame(code="ZAF", year=2018L, cpi=2, ppp2021=5)
  out <- convert_lcu_to_ppp(df, cpi, lcu_vars=character(0))
  expect_equal(out$welfare, 1000)
})

# ---- derive_h3_fnames -------------------------------------------------------

test_that("derive_h3_fnames replaces unit suffix with _h3", {
  expect_equal(derive_h3_fnames("ZAF_2018_NIDS_hh.parquet"), "ZAF_2018_NIDS_h3.parquet")
})

test_that("derive_h3_fnames deduplicates when hh and ind both present", {
  fnames <- c("ZAF_2018_NIDS_hh.parquet","ZAF_2018_NIDS_ind.parquet")
  expect_equal(derive_h3_fnames(fnames), "ZAF_2018_NIDS_h3.parquet")
})

# ---- summarise_interview_dates ----------------------------------------------

test_that("summarise_interview_dates counts hh and drops NA timestamps", {
  df <- data.frame(
    economy     = rep("A", 4),
    countryyear = rep("A, 2018", 4),
    timestamp   = as.Date(c("2018-01-01","2018-01-01","2018-02-01",NA))
  )
  out <- summarise_interview_dates(df)
  expect_equal(nrow(out), 2L)
  expect_equal(sum(out$hh), 3L)
})

test_that("summarise_interview_dates returns zero rows when all timestamps NA", {
  df  <- data.frame(economy="A", countryyear="A, 2018", timestamp=as.Date(NA))
  expect_equal(nrow(summarise_interview_dates(df)), 0L)
})

# ---- welfare_poverty_lines --------------------------------------------------

test_that("welfare_poverty_lines returns 3 rows with correct values", {
  pl <- welfare_poverty_lines()
  expect_equal(pl$value, c(3.00, 4.20, 8.30))
  expect_equal(pl$label, c("$3.00", "$4.20", "$8.30"))
})

# ---- plot_interview_dates ---------------------------------------------------

test_that("plot_interview_dates returns NULL for empty/NULL input", {
  expect_null(plot_interview_dates(NULL))
  expect_null(plot_interview_dates(data.frame()))
})

test_that("plot_interview_dates returns ggplot for valid data", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(timestamp=as.Date("2018-01-01"), hh=50L, economy="A", countryyear="A, 2018")
  expect_s3_class(plot_interview_dates(d), "ggplot")
})

test_that("plot_interview_dates maps x=timestamp, y=hh", {
  skip_if_not_installed("ggplot2")
  d <- data.frame(timestamp=as.Date("2018-01-01"), hh=50L, economy="A", countryyear="A, 2018")
  p <- plot_interview_dates(d)
  expect_equal(rlang::as_label(p$mapping$x), "timestamp")
  expect_equal(rlang::as_label(p$mapping$y), "hh")
})

# ---- plot_welfare_dist ------------------------------------------------------

test_that("plot_welfare_dist returns NULL when welfare absent or NULL", {
  expect_null(plot_welfare_dist(NULL))
  expect_null(plot_welfare_dist(data.frame(x=1:5)))
})

test_that("plot_welfare_dist returns ggplot for valid data", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggridges")
  set.seed(1)
  df <- data.frame(welfare=exp(rnorm(200, log(3.5), 0.8)),
                   countryyear=rep(c("A, 2018","B, 2019"), each=100))
  expect_s3_class(plot_welfare_dist(df), "ggplot")
})

test_that("plot_welfare_dist accepts custom poverty_lines", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggridges")
  set.seed(1)
  df <- data.frame(welfare=exp(rnorm(100, log(3.5), 0.8)), countryyear=rep("A, 2018", 100))
  pl <- data.frame(value=2.15, label="$2.15", stringsAsFactors=FALSE)
  expect_s3_class(plot_welfare_dist(df, poverty_lines=pl), "ggplot")
})

# ---- plot_survey_map --------------------------------------------------------

