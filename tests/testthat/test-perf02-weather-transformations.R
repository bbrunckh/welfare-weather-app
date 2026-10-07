test_that("wide climate references preserve legacy transformations", {

  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE))

  base <- data.frame(
    code = c(rep("A", 5L), "B"),
    year = c(rep(2000L, 5L), 2025L),
    survname = "S",
    loc_id = c(rep("L1", 5L), "L2"),
    timestamp = as.Date(c(
      "2000-01-01", "2000-01-15", "2000-02-01", "2000-02-15",
      "2000-03-01", "2025-01-01"
    )),
    tx = c(10, 14, 20, 24, 30, 99),
    rain = c(1, NA, 3, 5, 7, 11),
    constant = 4,
    spi6 = c(100, 101, 102, 103, 104, 105),
    unchanged = c(9, 8, 7, 6, 5, 4),
    stringsAsFactors = FALSE
  )
  base_tbl <- dplyr::copy_to(con, base, "perf02_base", temporary = TRUE)

  selected <- data.frame(
    name = c("tx", "rain", "constant", "spi6", "unchanged"),
    transformation = c(
      "Deviation from mean", "Standardized anomaly", "Deviation from mean",
      "Deviation from mean", "None"
    ),
    stringsAsFactors = FALSE
  )

  legacy <- function(tbl, selected_weather, loc_weather_base) {
    for (i in seq_len(nrow(selected_weather))) {
      v <- selected_weather$name[i]
      tf <- selected_weather$transformation[i]
      if (is.na(tf) || tf == "None" || v %in% c("spi6", "spei6")) next

      ref <- loc_weather_base |>
        dplyr::filter(
          timestamp >= as.Date("1991-01-01"),
          timestamp <= as.Date("2020-12-31")
        ) |>
        dplyr::mutate(month = dbplyr::sql("MONTH(timestamp)")) |>
        dplyr::group_by(code, year, survname, loc_id, month) |>
        dplyr::summarise(
          ref_mean = dbplyr::sql(paste0("AVG(", v, ")")),
          ref_sd = dbplyr::sql(paste0("STDDEV_SAMP(", v, ")")),
          .groups = "drop"
        )

      tbl <- tbl |>
        dplyr::mutate(month = dbplyr::sql("MONTH(timestamp)")) |>
        dplyr::left_join(
          ref,
          by = c("code", "year", "survname", "loc_id", "month")
        ) |>
        dplyr::mutate(
          !!v := if (tf == "Deviation from mean") {
            dbplyr::sql(paste0(v, " - ref_mean"))
          } else {
            dbplyr::sql(paste0("(", v, " - ref_mean) / ref_sd"))
          }
        ) |>
        dplyr::select(-month, -ref_mean, -ref_sd)
    }
    tbl
  }

  old <- legacy(base_tbl, selected, base_tbl) |>
    dplyr::arrange(code, year, survname, loc_id, timestamp) |>
    dplyr::collect()
  ref <- .build_climate_reference(base_tbl, selected)
  new <- .apply_transformations(base_tbl, selected, base_tbl, climate_ref = ref) |>
    dplyr::arrange(code, year, survname, loc_id, timestamp) |>
    dplyr::collect()

  expect_identical(names(new), names(old))
  expect_equal(new, old, tolerance = 0)
  expect_identical(new$spi6, old$spi6)
  expect_identical(new$unchanged, old$unchanged)

  # The 2025 row has no climate-normal reference and must remain in the left
  # join with an NA transformed value.
  absent <- new[new$code == "B", ]
  expect_equal(absent$tx, NA_real_)
  expect_equal(absent$constant, NA_real_)

  sql <- dbplyr::sql_render(.apply_transformations(
    base_tbl, selected, base_tbl, climate_ref = ref
  ))
  expect_equal(length(regmatches(sql, gregexpr("LEFT JOIN", sql, fixed = TRUE))[[1L]]), 1L)

  ref_sql <- dbplyr::sql_render(ref$tbl)
  expect_match(ref_sql, "AVG(tx) FILTER (WHERE tx IS NOT NULL)", fixed = TRUE)
  expect_match(ref_sql, "STDDEV_SAMP(rain) FILTER (WHERE rain IS NOT NULL)",
               fixed = TRUE)
})


test_that("PERF-02 rejects duplicate selected weather names", {
  selected <- data.frame(
    name = c("tx", "tx"),
    transformation = c("Deviation from mean", "Standardized anomaly"),
    stringsAsFactors = FALSE
  )
  expect_error(
    .transformation_specs(selected),
    "duplicate weather variable names"
  )
})


test_that("PERF-02 builds no reference relation when nothing is transformed", {
  selected <- data.frame(
    name = c("tx", "spi6", "rain"),
    transformation = c("None", "Deviation from mean", NA_character_),
    stringsAsFactors = FALSE
  )
  expect_null(.transformation_specs(selected))
})
