# ============================================================================ #
# tests/testthat/test-ui-tables-fake-data.R                                    #
# Isolated §6 table renders with fake survey data. Regression guard for the
# runtime failures surfaced by the interactive BFA 2018 walkthrough:
#   - stats_table_frame read a base field the display base never carried, so
#     every base-supplied summary table crashed ("attempt to set an attribute
#     on NULL");
#   - .weather_reactable() rejected the pct_cols argument, so the binned
#     weather table never rendered.
# ============================================================================ #

library(testthat)

# testServer hands back the serialized render payload for htmlwidget outputs;
# the reactable data arrives as column-oriented lists inside it.
.ui_rows <- function(out) {
  cols <- if (inherits(out, "reactable")) {
    jsonlite::fromJSON(out$x$tag$attribs$data, simplifyVector = TRUE)
  } else {
    jsonlite::fromJSON(out)$x$tag$attribs$data
  }
  as.data.frame(cols, stringsAsFactors = FALSE, optional = TRUE)
}

# Fake BFA-2018-like household frame: two waves, numeric + factor variables,
# adversarial weights, and the policy-defined columns.
.ui_df <- function() {
  data.frame(
    economy     = "Burkina Faso",
    year        = rep(c(2018L, 2021L), each = 8),
    countryyear = rep(c("Burkina Faso, 2018", "Burkina Faso, 2021"), each = 8),
    weight      = c(1, 2, 0, Inf, NA, 1, 1, 1, 1, 1, -1, 1, 1, 1, 1, 1),
    welfare     = c(rnorm(7, 10), NA, rnorm(7, 10), NA),
    x1          = c(1:7, NA, 8, NA, 9:14),
    urban       = rep(c(0, 1), each = 8),
    electricity = rep(c(1L, 0L), each = 8),
    imp_wat_rec = rep(c(1L, 0L), times = 8),
    imp_san_rec = rep(c(1L, 0L), each = 4, length.out = 16),
    wx_bin      = factor(rep(c("Low", "High"), times = 8)),
    # Numeric-range bin labels in deliberately scrambled creation order:
    # the binned table must display them ordered by bin bounds, not
    # lexicographically / by first appearance.
    t_bin       = rep(c("(10, 12]", "(6, 8]", "\u2264 6"), length.out = 16),
    stringsAsFactors = FALSE
  )
}

.ui_vl <- function() {
  data.frame(
    name    = c("welfare", "x1", "urban", "electricity", "imp_wat_rec",
                "imp_san_rec", "unused"),
    label   = c("Welfare", "X one", "Urban", "Electricity", "Improved water",
                "Improved sanitation", "Not present"),
    units   = "x",
    outcome = c(1, 1, 0, 0, 0, 0, 1),
    ind     = c(0, 1, 1, 0, 0, 0, 0),
    hh      = c(1, 1, 1, 1, 1, 1, 0),
    firm    = c(0, 0, 0, 0, 0, 0, 0),
    area    = c(0, 1, 0, 0, 0, 0, 0),
    stringsAsFactors = FALSE
  )
}

# The module's stats_base, exactly as mod_1_02 builds it.
.ui_base <- function(df, vl) {
  policy_vars <- unique(unlist(lapply(POLICY_DEFINITIONS, `[[`, "vars")))
  targets <- unlist(lapply(c("outcome", "ind", "hh", "firm", "area"), function(fc) {
    if (fc %in% names(vl)) vl$name[vl[[fc]] == 1] else character(0)
  }), use.names = FALSE)
  union_vars <- intersect(unique(c(targets, policy_vars)), names(df))
  stats_display_base(df, vl, union_vars)
}

test_that("all six survey summary tables render from the display base", {
  df <- .ui_df(); vl <- .ui_vl()
  base <- .ui_base(df, vl)
  expect_false(is.null(base))

  for (fc in c("outcome", "ind", "hh", "firm", "area")) {
    out <- NULL
    shiny::testServer(
      function(input, output, session) {
        output$tbl <- make_stats_reactable(
          shiny::reactive(df), shiny::reactive(vl), fc, base = shiny::reactive(base)
        )
      },
      {
        out <<- output$tbl
      }
    )
    expect_true(inherits(out, "reactable") || is.character(out))
    dat <- .ui_rows(out)
    if (fc == "firm") {
      # firm carries no flagged variables -> the Note frame.
      expect_true(all(names(dat) == "Note"))
    } else {
      # Regression: these used to crash instead of rendering.
      expect_true(all(c("Variable", "Country, Year") %in% names(dat)))
      expect_true(nrow(dat) >= 1)
    }
  }
})

test_that("policy summary table renders from vars + display base", {
  df <- .ui_df(); vl <- .ui_vl()
  base <- .ui_base(df, vl)
  policy_vars <- unique(unlist(lapply(POLICY_DEFINITIONS, `[[`, "vars")))
  policy_vars <- intersect(policy_vars, names(df))

  out <- NULL
  shiny::testServer(
    function(input, output, session) {
      output$tbl <- make_stats_reactable(
        shiny::reactive(df), shiny::reactive(vl), vars = policy_vars,
        base = shiny::reactive(base)
      )
    },
    {
      out <<- output$tbl
    }
  )
  dat <- .ui_rows(out)
  expect_true(nrow(dat) >= 1)
  expect_true("Electricity" %in% dat$Variable)
})

test_that("survey summary exports return the same frame the tables show", {
  df <- .ui_df(); vl <- .ui_vl()
  base <- .ui_base(df, vl)

  for (fc in c("outcome", "hh")) {
    tbl <- stats_table_frame(df, vl, fc, base = base)
    exp <- stats_display_slice(base, vl, flag_col = fc) # export path, unrounded
    expect_identical(tbl, exp, label = fc)
  }
})

test_that("weather stats tables render continuous and binned frames", {
  df <- .ui_df()
  sw_sel <- data.frame(
    name  = c("welfare", "x1", "electricity", "wx_bin", "t_bin"),
    label = c("Welfare", "X one", "Electricity", "Temperature bins",
              "Temperature bins (num)"),
    stringsAsFactors = FALSE
  )

  cont <- NULL
  shiny::testServer(
    function(input, output, session) {
      output$tbl <- make_weather_stats_reactable(
        shiny::reactive(df), shiny::reactive(sw_sel)
      )
    },
    {
      cont <<- output$tbl
    }
  )
  cdat <- .ui_rows(cont)
  expect_true(all(c("Variable", "Country, Year", "Mean") %in% names(cdat)))

  binned <- NULL
  shiny::testServer(
    function(input, output, session) {
      output$tbl <- make_weather_binned_stats_reactable(
        shiny::reactive(df), shiny::reactive(sw_sel)
      )
    },
    {
      binned <<- output$tbl
    }
  )
  # Regression: .weather_reactable() used to reject the pct_cols argument.
  bdat <- .ui_rows(binned)
  expect_true(all(c("Variable", "Country, Year", "Level", "N", "Share (%)") %in%
                    names(bdat)))
  expect_true("Temperature bins" %in% bdat$Variable)
  expect_true(nrow(bdat) >= 1)

  # Bin levels display ordered by numeric bounds: creation order starts
  # "(10, 12]" first, but the table must show "≤ 6" before "(6, 8]" before
  # "(10, 12]" within each country-year block.
  bnum <- bdat[bdat$Variable == "Temperature bins (num)", ]
  lv_seen <- unique(bnum$Level)
  expect_identical(lv_seen, c("\u2264 6", "(6, 8]", "(10, 12]"))
})
