library(testthat)

# ---- Fixtures ---------------------------------------------------------------

make_varlist <- function() {
  data.frame(
    name    = c("welfare", "educ",  "poor_hh", "age"),
    label   = c("Welfare", "Education", "Poor HH", "Age"),
    units   = c("LCU",    "years",  "",        "years"),
    type    = c("numeric", "numeric", "logical", "numeric"),
    outcome = c(1L,        0L,        1L,        0L),
    stringsAsFactors = FALSE
  )
}

survey_cols <- c("welfare", "poor_hh", "age", "weight", "ppp2021")


# ============================================================================ #
# filter_outcome_vars                                                           #
# ============================================================================ #

test_that("filter_outcome_vars keeps only outcome == 1 vars in survey", {
  out <- filter_outcome_vars(make_varlist(), survey_cols)
  expect_true(all(out$name %in% survey_cols | out$name == "poor"))
  expect_false("educ" %in% out$name)
  expect_false("age"  %in% out$name)
})

test_that("filter_outcome_vars appends 'poor' when welfare present", {
  vl  <- make_varlist()
  out <- filter_outcome_vars(vl, c("welfare"))
  expect_true("poor" %in% out$name)
})

test_that("filter_outcome_vars does NOT append 'poor' when welfare absent", {
  vl  <- make_varlist()
  out <- filter_outcome_vars(vl, c("poor_hh"))
  expect_false("poor" %in% out$name)
})

test_that("filter_outcome_vars orders welfare -> poor -> rest", {
  vl  <- make_varlist()
  out <- filter_outcome_vars(vl, c("welfare", "poor_hh"))
  expect_equal(out$name[1], "welfare")
  expect_equal(out$name[2], "poor")
})

test_that("filter_outcome_vars returns zero rows for empty survey_colnames", {
  expect_equal(nrow(filter_outcome_vars(make_varlist(), character(0))), 0L)
})

test_that("filter_outcome_vars coerces columns to character", {
  vl       <- make_varlist()
  vl$name  <- as.factor(vl$name)
  vl$units <- as.factor(vl$units)
  out <- filter_outcome_vars(vl, survey_cols)
  expect_type(out$name,  "character")
  expect_type(out$units, "character")
})


# ============================================================================ #
# is_monetary_outcome                                                           #
# ============================================================================ #

test_that("is_monetary_outcome TRUE for welfare / poor", {
  expect_true(is_monetary_outcome("welfare", ""))
  expect_true(is_monetary_outcome("poor",    ""))
})

test_that("is_monetary_outcome TRUE for LCU units", {
  expect_true(is_monetary_outcome("other_var", "LCU"))
})

test_that("is_monetary_outcome FALSE for non-monetary outcome", {
  expect_false(is_monetary_outcome("educ",    "years"))
  expect_false(is_monetary_outcome("poor_hh", ""))
  expect_false(is_monetary_outcome("educ",    NA))
})


# ============================================================================ #
# default_lcu_poverty_line                                                      #
# ============================================================================ #

test_that("default_lcu_poverty_line returns numeric", {
  df <- data.frame(welfare = c(2, 3, 5, 8, 10), ppp2021 = rep(50, 5))
  expect_type(default_lcu_poverty_line(df), "double")
})

test_that("default_lcu_poverty_line uses weight when present", {
  df <- data.frame(welfare = 1:10, ppp2021 = rep(1, 10),
                   weight  = c(rep(1, 9), 100))
  result <- default_lcu_poverty_line(df)
  expect_true(is.finite(result))
})

test_that("default_lcu_poverty_line falls back to 1.00 when columns missing", {
  expect_equal(default_lcu_poverty_line(data.frame(x = 1:5)), 1.00)
})

test_that("default_lcu_poverty_line falls back to 1.00 on error", {
  df <- data.frame(welfare = rep(NA_real_, 5), ppp2021 = rep(1, 5))
  expect_equal(default_lcu_poverty_line(df), 1.00)
})


# ============================================================================ #
# poverty_line_label                                                            #
# ============================================================================ #

test_that("poverty_line_label returns PPP label for NULL / 'PPP'", {
  expect_match(poverty_line_label(NULL),  "PPP")
  expect_match(poverty_line_label("PPP"), "PPP")
})

test_that("poverty_line_label returns LCU label for 'LCU'", {
  expect_match(poverty_line_label("LCU"), "LCU")
})


# ============================================================================ #
# outcome_transform                                                             #
# ============================================================================ #

test_that("outcome_transform returns 'log' for numeric", {
  expect_equal(outcome_transform("numeric"), "log")
  expect_equal(outcome_transform("Numeric"), "log")  # case-insensitive
})

test_that("outcome_transform returns NA_character_ for logical/binary/other", {
  expect_true(is.na(outcome_transform("logical")))
  expect_true(is.na(outcome_transform("binary")))
  expect_true(is.na(outcome_transform("character")))
})


# ============================================================================ #
# build_selected_outcome                                                        #
# ============================================================================ #

make_info <- function(name = "welfare", units = "LCU", type = "numeric") {
  data.frame(name = name, label = name, units = units, type = type,
             stringsAsFactors = FALSE)
}

test_that("build_selected_outcome adds transform column", {
  out <- build_selected_outcome(make_info("welfare", "LCU", "numeric"), "PPP", 3)
  expect_equal(out$transform, "log")
})

test_that("build_selected_outcome sets transform NA for logical outcome", {
  out <- build_selected_outcome(make_info("poor_hh", "", "logical"), NULL, NULL)
  expect_true(is.na(out$transform))
})

test_that("build_selected_outcome overrides units with currency for monetary", {
  out <- build_selected_outcome(make_info("welfare", "LCU", "numeric"), "PPP", 3)
  expect_equal(out$units, "PPP")
})

test_that("build_selected_outcome keeps original units for non-monetary", {
  out <- build_selected_outcome(make_info("educ", "years", "numeric"), "PPP", NULL)
  expect_equal(out$units, "years")
})

test_that("build_selected_outcome attaches valid poverty line", {
  out <- build_selected_outcome(make_info("poor", "", "logical"), "PPP", 2.15)
  expect_equal(out$povline, 2.15)
})

test_that("build_selected_outcome defaults to 3.00 when poverty_line NULL and PPP", {
  out <- build_selected_outcome(make_info("poor", "", "logical"), "PPP", NULL)
  expect_equal(out$povline, 3.00)
})

test_that("build_selected_outcome sets povline NA for non-monetary outcome", {
  out <- build_selected_outcome(make_info("educ", "years", "numeric"), "PPP", 3)
  expect_true(is.na(out$povline))
})

test_that("build_selected_outcome returns NULL/empty safely", {
  expect_null(build_selected_outcome(NULL))
  expect_equal(nrow(build_selected_outcome(make_info()[0L, ])), 0L)
})


# ============================================================================ #
# outcome_info_message                                                          #
# ============================================================================ #

test_that("outcome_info_message returns tagList for numeric", {
  skip_if_not_installed("shiny")
  msg <- outcome_info_message("numeric")
  expect_s3_class(msg, "shiny.tag.list")
  html <- as.character(msg)
  expect_match(html, "log-transformed")
})

test_that("outcome_info_message returns tagList for logical", {
  skip_if_not_installed("shiny")
  msg <- outcome_info_message("logical")
  html <- as.character(msg)
  expect_match(html, "Binary")
})

test_that("outcome_info_message returns empty tagList for unknown type", {
  skip_if_not_installed("shiny")
  msg <- outcome_info_message("character")
  expect_equal(length(msg), 0L)
})
