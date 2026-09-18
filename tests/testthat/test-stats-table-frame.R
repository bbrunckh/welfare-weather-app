# ============================================================================ #
# tests/testthat/test-stats-table-frame.R                                      #
# PERF-40: the shared-base path (stats_table_frame) must produce exactly the   #
# table the standalone per-table aggregation produced.                         #
# ============================================================================ #

library(testthat)

# Adversarial frame: NA / NaN / Inf values, zero / negative / NA / Inf
# weights, an all-NA variable, a character variable, two waves.
make_stats_df <- function() {
  add_time_columns(data.frame(
    code      = "TST",
    economy   = "Testland",
    year      = c("2020", "2021")[c(1, 1, 1, 2, 2, 2, 1, 2)],
    survname  = "SRV",
    timestamp = as.Date(c(
      "2020-06-01", "2020-07-01", "2020-06-15",
      "2021-01-05", "2021-02-05", "2021-02-25",
      "2020-08-01", "2021-03-05"
    )),
    weight = c(1, 2, 0, -1, NA, Inf, 3, 1.5),
    x1     = c(1, 2, 3, 4, NA, 6, Inf, 8),
    x2     = c(10, NA, 30, 40, 50, NaN, 70, 80),
    x3     = NA_real_,
    chr    = c("a", "b", "a", "b", "a", "b", "a", "b"),
    stringsAsFactors = FALSE
  ))
}

make_stats_vl <- function() {
  data.frame(
    name    = c("x1", "x2", "x3", "chr", "unused"),
    label   = c("X one", "X two", "X three", "Letters", "Not present"),
    units   = "x",
    outcome = c(1, 1, 0, 0, 1),
    hh      = c(1, 1, 1, 1, 0),
    firm    = c(0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
}

# The module's real one-pass display base (PERF-42): built exactly as
# mod_1_02_surveystats builds stats_base().
build_stats_base <- function(df, vl) {
  policy_vars <- unique(unlist(lapply(POLICY_DEFINITIONS, `[[`, "vars")))
  targets <- unlist(lapply(c("outcome", "ind", "hh", "firm", "area"), function(fc) {
    if (fc %in% names(vl)) vl$name[vl[[fc]] == 1] else character(0)
  }), use.names = FALSE)
  union_vars <- intersect(unique(c(targets, policy_vars)), names(df))
  if (!length(union_vars)) return(NULL)
  stats_display_base(df, vl, union_vars)
}

# REGRESSION (runtime BFA 2018 walkthrough): stats_table_frame used to read a
# `base_list$summary` field the display base never carried, so every
# base-supplied table crashed with "attempt to set an attribute on NULL".
test_that("stats_table_frame: display base reproduces the standalone table (flag path)", {
  df <- make_stats_df(); vl <- make_stats_vl()
  base <- build_stats_base(df, vl)

  for (fc in c("hh", "outcome")) {
    standalone <- stats_table_frame(df, vl, flag_col = fc)
    shared     <- stats_table_frame(df, vl, flag_col = fc, base = base)
    expect_identical(standalone, shared, label = paste("flag_col =", fc))
    # And the slice is the same answer at row-filter cost.
    expect_identical(shared, stats_display_slice(base, vl, flag_col = fc))
  }
})

test_that("stats_table_frame: display base reproduces the standalone table (vars path)", {
  df <- make_stats_df(); vl <- make_stats_vl()
  base <- build_stats_base(df, vl)

  standalone <- stats_table_frame(df, vl, vars = c("x1", "x2"))
  shared     <- stats_table_frame(df, vl, vars = c("x1", "x2"), base = base)
  expect_identical(standalone, shared)
})

test_that("stats_table_frame: base supplied as a function is resolved", {
  df <- make_stats_df(); vl <- make_stats_vl()
  base <- build_stats_base(df, vl)

  from_fn <- stats_table_frame(df, vl, flag_col = "hh", base = function() base)
  expect_identical(from_fn, stats_table_frame(df, vl, flag_col = "hh", base = base))
})

test_that("stats_table_frame: base missing a variable falls back to local aggregation", {
  df <- make_stats_df(); vl <- make_stats_vl()
  base <- build_stats_base(df, vl)
  # A stale base from before "x2" was added to the union.
  base$vars    <- setdiff(base$vars, "x2")
  base$display <- base$display[base$display$.var != "x2", ]

  standalone <- stats_table_frame(df, vl, vars = c("x1", "x2"))
  fallback   <- stats_table_frame(df, vl, vars = c("x1", "x2"), base = base)
  expect_identical(standalone, fallback)
})

test_that("stats_table_frame: empty flag set returns the note frame", {
  df <- make_stats_df(); vl <- make_stats_vl()
  out <- stats_table_frame(df, vl, flag_col = "firm")
  expect_identical(names(out), "Note")
  expect_match(out$Note, "No firm variables found")

  # Even a covering base must not bypass the note frame.
  base <- build_stats_base(df, vl)
  expect_identical(
    out, stats_table_frame(df, vl, flag_col = "firm", base = base)
  )
})

test_that("stats_table_frame: adversarial cells summarise per the masked rules", {
  df <- make_stats_df(); vl <- make_stats_vl()
  tab <- stats_table_frame(df, vl, flag_col = "hh")

  # Labels replace raw names.
  expect_true("X one" %in% tab$Variable)

  # The all-NA variable is dropped (N = 0 rows are filtered).
  expect_false("X three" %in% tab$Variable)

  # The character variable produces no summary rows (weighted_summary_long
  # keeps numeric variables only), so its missingness-only rows never make
  # it through the left join - it is absent even though the union pass
  # computed its missingness.
  expect_false("Letters" %in% tab$Variable)

  # x1 keeps only finite-value rows with finite positive weights per wave:
  # 2020: values 1, 2 (3 has weight 0, Inf value dropped) -> N = 2
  # 2021: value 8 only (4 has weight -1, NA value, Inf weight dropped) -> N = 1
  x1_rows <- tab[tab$Variable == "X one", ]
  x1_rows <- x1_rows[order(x1_rows[["Country, Year"]]), ]
  expect_identical(x1_rows$N, c(2L, 1L))
})

test_that("stats_table_frame: data without countryyear skips wave missingness", {
  df <- make_stats_df()
  df$countryyear <- NULL
  vl <- make_stats_vl()
  base <- build_stats_base(df, vl)

  standalone <- stats_table_frame(df, vl, flag_col = "hh")
  shared     <- stats_table_frame(df, vl, flag_col = "hh", base = base)
  expect_identical(standalone, shared)
  expect_false("% Missing" %in% names(standalone))
})


test_that("stats_display_base slice reproduces stats_table_frame exactly", {
  df <- make_stats_df(); vl <- make_stats_vl()
  union_vars <- intersect(c(
    vl$name[vl$outcome == 1], vl$name[vl$hh == 1], "chr"
  ), names(df))
  base <- stats_display_base(df, vl, union_vars)

  for (fc in c("hh", "outcome")) {
    expect_identical(
      stats_table_frame(df, vl, flag_col = fc),
      stats_display_slice(base, vl, flag_col = fc),
      label = paste("flag_col =", fc)
    )
  }
  expect_identical(
    stats_table_frame(df, vl, vars = c("x1", "x2")),
    stats_display_slice(base, vl, vars = c("x1", "x2"))
  )
  # A table whose variables the base does not cover produces the note frame.
  expect_identical(
    stats_table_frame(df, vl, flag_col = "firm"),
    stats_display_slice(base, vl, flag_col = "firm")
  )
  expect_null(stats_display_base(df, vl, character(0)))
})

test_that("stats_display_slice rounds display columns without touching N", {
  df <- make_stats_df(); vl <- make_stats_vl()
  union_vars <- intersect(c(vl$name[vl$outcome == 1], vl$name[vl$hh == 1]), names(df))
  base <- stats_display_base(df, vl, union_vars)
  raw <- stats_display_slice(base, vl, flag_col = "hh")
  rounded <- stats_display_slice(base, vl, flag_col = "hh", round_digits = 2)

  expect_identical(raw[["N"]], rounded[["N"]])
  for (col in setdiff(names(raw), c("Variable", "Country, Year", "N"))) {
    expect_identical(round(raw[[col]], 2), rounded[[col]], info = col)
  }
})

test_that("make_stats_reactable renders through a session with the shared base", {
  df <- make_stats_df(); vl <- make_stats_vl()
  union_vars <- intersect(c(vl$name[vl$outcome == 1], vl$name[vl$hh == 1]), names(df))
  base <- stats_display_base(df, vl, union_vars)

  shiny::testServer(
    function(input, output, session) {
      output$tbl <- make_stats_reactable(
        shiny::reactive(df), shiny::reactive(vl), "hh",
        base = shiny::reactive(base)
      )
    },
    {
      # reactable sends the table data to the client; the payload carries the
      # column names as data keys.
      payload <- paste(jsonlite::toJSON(output$tbl, auto_unbox = TRUE),
                       collapse = "")
      expect_match(payload, "Variable", fixed = TRUE)
      expect_match(payload, "Country, Year", fixed = TRUE)
      expect_match(payload, "% Missing", fixed = TRUE)
    }
  )
})
