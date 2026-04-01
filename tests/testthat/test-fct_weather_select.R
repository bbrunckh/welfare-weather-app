library(testthat)

# ---- test data --------------------------------------------------------------

make_var_info <- function() {
  data.frame(
    name    = c("tx", "pr", "spi6"),
    label   = c("Max temp", "Precipitation", "SPI-6"),
    units   = c("°C", "mm", ""),
    hazard  = c(1L, 1L, 1L),
    stringsAsFactors = FALSE
  )
}

# ============================================================================ #
# get_weather_vars                                                             #
# ============================================================================ #

test_that("get_weather_vars filters to hazard == 1", {
  vl <- rbind(make_var_info(),
              data.frame(name = "age", label = "Age", units = "years",
                         hazard = 0L, stringsAsFactors = FALSE))
  out <- get_weather_vars(vl)
  expect_equal(nrow(out), 3L)
  expect_false("age" %in% out$name)
})

test_that("get_weather_vars returns zero-row df for NULL input", {
  out <- get_weather_vars(NULL)
  expect_equal(nrow(out), 0L)
  expect_setequal(names(out), c("name", "label", "units"))
})

test_that("get_weather_vars returns correct columns", {
  out <- get_weather_vars(make_var_info())
  expect_setequal(names(out), c("name", "label", "units"))
})

# ============================================================================ #
# temporal_agg_choices / temporal_agg_default                                 #
# ============================================================================ #

test_that("temporal_agg_choices includes Sum for mm", {
  expect_true("Sum" %in% temporal_agg_choices("mm"))
})

test_that("temporal_agg_choices includes Sum for days", {
  expect_true("Sum" %in% temporal_agg_choices("days"))
})

test_that("temporal_agg_choices excludes Sum for °C", {
  expect_false("Sum" %in% temporal_agg_choices("°C"))
})

test_that("temporal_agg_choices excludes Sum for NA units", {
  expect_false("Sum" %in% temporal_agg_choices(NA_character_))
})

test_that("temporal_agg_default returns Sum for mm", {
  expect_equal(temporal_agg_default("mm"), "Sum")
})

test_that("temporal_agg_default returns Mean for °C", {
  expect_equal(temporal_agg_default("°C"), "Mean")
})

# ============================================================================ #
# transformation_choices / transformation_default                             #
# ============================================================================ #

test_that("transformation_choices restricted for dimensionless units", {
  expect_equal(transformation_choices(""), "Standardized anomaly")
})

test_that("transformation_choices offers three options for °C", {
  expect_equal(
    transformation_choices("°C"),
    c("None", "Deviation from mean", "Standardized anomaly")
  )
})

test_that("transformation_default returns Standardized anomaly for empty units", {
  expect_equal(transformation_default(""), "Standardized anomaly")
})

test_that("transformation_default returns None for normal units", {
  expect_equal(transformation_default("°C"), "None")
  expect_equal(transformation_default(NA_character_), "None")
})

# ============================================================================ #
# weather_spec_defaults                                                        #
# ============================================================================ #

test_that("weather_spec_defaults returns correct structure", {
  d <- weather_spec_defaults("°C")
  expect_equal(d$ref_start,      1L)
  expect_equal(d$ref_end,        1L)
  expect_equal(d$temporal_agg,   "Mean")
  expect_equal(d$transformation, "None")
  expect_equal(d$cont_binned,    "Continuous")
  expect_true(is.na(d$num_bins))
  expect_true(is.na(d$binning_method))
  expect_equal(d$polynomial, character(0))
})

test_that("weather_spec_defaults uses Sum default for mm", {
  d <- weather_spec_defaults("mm")
  expect_equal(d$temporal_agg, "Sum")
})

# ============================================================================ #
# build_weather_spec                                                           #
# ============================================================================ #

test_that("build_weather_spec returns one-row tibble with correct columns", {
  s <- build_weather_spec("tx", "°C")
  expect_s3_class(s, "tbl_df")
  expect_equal(nrow(s), 1L)
  expect_true(all(c("name", "ref_start", "ref_end", "temporalAgg",
                     "transformation", "cont_binned", "num_bins",
                     "binning_method", "polynomial") %in% names(s)))
})

test_that("build_weather_spec applies defaults when inputs are NULL", {
  s <- build_weather_spec("tx", "°C")
  expect_equal(s$ref_start,      1L)
  expect_equal(s$ref_end,        1L)
  expect_equal(s$temporalAgg,    "Mean")
  expect_equal(s$transformation, "None")
  expect_equal(s$cont_binned,    "Continuous")
  expect_true(is.na(s$num_bins))
  expect_true(is.na(s$binning_method))
})

test_that("build_weather_spec respects supplied ref_period", {
  s <- build_weather_spec("tx", "°C", ref_period = c(2L, 6L))
  expect_equal(s$ref_start, 2L)
  expect_equal(s$ref_end,   6L)
})

test_that("build_weather_spec sets num_bins and binning_method when Binned", {
  s <- build_weather_spec("tx", "°C", cont_binned = "Binned",
                           num_bins = 4L, binning_method = "K-means")
  expect_equal(s$cont_binned,    "Binned")
  expect_equal(s$num_bins,       4L)
  expect_equal(s$binning_method, "K-means")
})

test_that("build_weather_spec NAs num_bins and binning_method when Continuous", {
  s <- build_weather_spec("tx", "°C", cont_binned = "Continuous")
  expect_true(is.na(s$num_bins))
  expect_true(is.na(s$binning_method))
})

test_that("build_weather_spec defaults num_bins to 5 when Binned and not supplied", {
  s <- build_weather_spec("tx", "°C", cont_binned = "Binned")
  expect_equal(s$num_bins, 5L)
  expect_equal(s$binning_method, "Equal frequency")
})

test_that("build_weather_spec stores polynomial as list column", {
  s <- build_weather_spec("tx", "°C", polynomial = c("2", "3"))
  expect_type(s$polynomial, "list")
  expect_equal(s$polynomial[[1]], c("2", "3"))
})

# ============================================================================ #
# build_selected_weather                                                       #
# ============================================================================ #

test_that("build_selected_weather returns empty tibble for empty selected_vars", {
  out <- build_selected_weather(character(0), make_var_info())
  expect_equal(nrow(out), 0L)
})

test_that("build_selected_weather returns one row per selected variable", {
  out <- build_selected_weather(c("tx", "pr"), make_var_info())
  expect_equal(nrow(out), 2L)
  expect_setequal(out$name, c("tx", "pr"))
})

test_that("build_selected_weather attaches label and units from var_info", {
  out <- build_selected_weather("tx", make_var_info())
  expect_equal(out$label, "Max temp")
  expect_equal(out$units, "°C")
})

test_that("build_selected_weather applies spec_inputs over defaults", {
  inputs <- list(
    tx_relativePeriod = c(2L, 5L),
    tx_temporalAgg    = "Max",
    tx_contOrBinned   = "Binned",
    tx_numBins        = 3L,
    tx_binningMethod  = "Equal width"
  )
  out <- build_selected_weather("tx", make_var_info(), spec_inputs = inputs)
  expect_equal(out$ref_start,      2L)
  expect_equal(out$ref_end,        5L)
  expect_equal(out$temporalAgg,    "Max")
  expect_equal(out$cont_binned,    "Binned")
  expect_equal(out$num_bins,       3L)
  expect_equal(out$binning_method, "Equal width")
})

test_that("build_selected_weather uses Sum default for mm variable", {
  out <- build_selected_weather("pr", make_var_info())
  expect_equal(out$temporalAgg, "Sum")
})

test_that("build_selected_weather uses Standardized anomaly for dimensionless", {
  out <- build_selected_weather("spi6", make_var_info())
  expect_equal(out$transformation, "Standardized anomaly")
})