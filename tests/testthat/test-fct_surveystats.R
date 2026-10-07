# allocate_units_to_cells: population-weighted spread of sampled units onto
# the H3 cells each location covers, with an even-split fallback.

test_that("allocate_units_to_cells weights by pop_2020 and conserves totals", {
  cm <- data.frame(
    code = "BFA", year = 2018L, survname = "EHCVM",
    loc_id = c("L1", "L1", "L1", "L2"),
    h3 = c("a", "b", "c", "a"),
    pop_2020 = c(300, 100, NA, 50)
  )
  sd <- data.frame(
    code = "BFA", year = 2018L, survname = "EHCVM",
    loc_id = c("L1", "L1", "L2"),
    x = 1:3
  )

  out <- allocate_units_to_cells(cm, sd)

  # L1: 2 units over pops 300/100/0 (NA -> 0): a=1.5, b=0.5, c=0 (dropped)
  # L2: 1 unit, single cell: a=1
  expect_setequal(out$h3, c("a", "b"))
  expect_equal(out$n_units[out$h3 == "a"], 2.5)
  expect_equal(out$n_units[out$h3 == "b"], 0.5)
  expect_equal(sum(out$n_units), 3) # reconciles with the sample
})

test_that("allocate_units_to_cells falls back to an even split without weights", {
  cm <- data.frame(
    code = "BFA", year = 2018L, survname = "EHCVM",
    loc_id = c("L1", "L1", "L2", "L2"),
    h3 = c("a", "b", "c", "d"),
    pop_2020 = c(0, 0, NA, NA) # zero weights and no column value both fall back
  )
  sd <- data.frame(
    code = "BFA", year = 2018L, survname = "EHCVM",
    loc_id = c("L1", "L2", "L2"),
    x = 1:3
  )

  out <- allocate_units_to_cells(cm, sd)

  # L1: 1 unit over 2 cells, all-zero pops -> 0.5 each
  # L2: 2 units over 2 cells, NA pops -> 1 each
  expect_equal(out$n_units[out$h3 == "a"], 0.5)
  expect_equal(out$n_units[out$h3 == "b"], 0.5)
  expect_equal(out$n_units[out$h3 == "c"], 1)
  expect_equal(out$n_units[out$h3 == "d"], 1)
})

test_that("allocate_units_to_cells splits evenly when pop_2020 is absent", {
  cm <- data.frame(
    code = "BFA", year = 2018L, survname = "EHCVM",
    loc_id = c("L1", "L1"),
    h3 = c("a", "b")
  )
  sd <- data.frame(
    code = "BFA", year = 2018L, survname = "EHCVM",
    loc_id = c("L1", "L1", "L1"),
    x = 1:3
  )

  out <- allocate_units_to_cells(cm, sd)

  expect_equal(out$n_units[out$h3 == "a"], 1.5)
  expect_equal(out$n_units[out$h3 == "b"], 1.5)
})

test_that("allocate_units_to_cells returns NULL on unusable inputs", {
  cm <- data.frame(code = "BFA", year = 2018L, survname = "EHCVM",
                   loc_id = "L1", h3 = "a")
  sd <- data.frame(code = "BFA", year = 2018L, survname = "EHCVM",
                   loc_id = "L1")

  expect_null(allocate_units_to_cells(NULL, sd))
  expect_null(allocate_units_to_cells(cm, NULL))
  expect_null(allocate_units_to_cells(cm[, -5], sd)) # no h3 column
})

test_that("density allocation preserves duplicate, missing, factor, and nonfinite behavior", {
  cm <- data.frame(
    code = factor(c("B", "A", "A", "A", "A", NA), levels = c("B", "A")),
    year = c(2019L, 2018L, 2018L, 2018L, 2018L, 2018L),
    survname = c("S2", rep("S1", 5)),
    loc_id = c("L2", "L1", "L1", "L1", "L3", NA),
    h3 = factor(c("z", "b", "a", "a", "inf", NA),
                levels = c("z", "b", "a", "inf", "unused")),
    pop_2020 = c(-1, 3, NA, 1, Inf, 0)
  )
  sd <- data.frame(
    code = factor(c("A", "A", "A", "B", NA), levels = c("B", "A")),
    year = c("2018", "2018", "2018", "2019", "2018"),
    survname = c("S1", "S1", "S1", "S2", "S1"),
    loc_id = c("L1", "L1", "L1", "L2", NA)
  )

  reference <- function(cell_map, survey_data) {
    keys <- c("code", "year", "survname", "loc_id")
    cell_map$year <- as.character(cell_map$year)
    survey_data$year <- as.character(survey_data$year)
    n_loc <- dplyr::count(survey_data, .data$code, .data$year,
                          .data$survname, .data$loc_id, name = "n_units")
    joined <- dplyr::inner_join(cell_map, n_loc, by = keys)
    has_pop <- "pop_2020" %in% names(joined)
    joined |>
      dplyr::group_by(.data$code, .data$year, .data$survname, .data$loc_id) |>
      dplyr::mutate(.alloc = if (has_pop) {
        .pop <- pmax(.data$pop_2020, 0, na.rm = TRUE)
        .pop_sum <- sum(.pop)
        if (.pop_sum > 0) .data$n_units * .pop / .pop_sum else
          .data$n_units / dplyr::n()
      } else .data$n_units / dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::group_by(.data$h3) |>
      dplyr::summarise(n_units = sum(.data$.alloc, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::filter(.data$n_units > 0) |>
      as.data.frame()
  }

  expected <- reference(cm, sd)
  summary <- wiseapp:::.density_cell_summary(cm, sd)

  expect_identical(summary$cells, expected)
  expect_identical(summary$n_locations, 3L)
  expect_identical(allocate_units_to_cells(cm, sd), expected)
  expect_identical(levels(summary$cells$h3), levels(cm$h3))
})

test_that("density allocation matches the reference on representative data", {
  set.seed(20260911)
  locations <- data.frame(
    code = rep(c("A", "B"), each = 60L),
    year = rep(c(2018L, 2021L), each = 60L),
    survname = rep(c("S1", "S2"), each = 60L),
    loc_id = sprintf("L%03d", seq_len(120L))
  )
  sd <- locations[rep(seq_len(nrow(locations)), sample(1:20, 120L, TRUE)), ]
  cm <- locations[rep(seq_len(nrow(locations)), sample(1:8, 120L, TRUE)), ]
  cm$h3 <- sprintf("h%04d", sample.int(600L, nrow(cm), TRUE))
  cm$pop_2020 <- sample(c(NA, -1, 0, Inf, runif(20, 1, 1000)),
                        nrow(cm), TRUE)
  cm <- rbind(cm, cm[sample.int(nrow(cm), 50L), ])

  old <- function(cell_map, survey_data) {
    keys <- c("code", "year", "survname", "loc_id")
    cell_map$year <- as.character(cell_map$year)
    survey_data$year <- as.character(survey_data$year)
    n_loc <- dplyr::count(survey_data, .data$code, .data$year,
                          .data$survname, .data$loc_id, name = "n_units")
    cm <- dplyr::inner_join(cell_map, n_loc, by = keys)
    cm |>
      dplyr::group_by(.data$code, .data$year, .data$survname, .data$loc_id) |>
      dplyr::mutate(.pop = pmax(.data$pop_2020, 0, na.rm = TRUE),
                    .pop_sum = sum(.data$.pop),
                    .alloc = if (.data$.pop_sum[1] > 0) {
                      .data$n_units * .data$.pop / .data$.pop_sum
                    } else .data$n_units / dplyr::n()) |>
      dplyr::ungroup() |>
      dplyr::group_by(.data$h3) |>
      dplyr::summarise(n_units = sum(.data$.alloc, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::filter(.data$n_units > 0) |>
      as.data.frame()
  }

  expected <- old(cm, sd)
  actual <- allocate_units_to_cells(cm, sd)
  expect_identical(names(actual), names(expected))
  expect_identical(vapply(actual, class, character(1)),
                   vapply(expected, class, character(1)))
  expect_identical(actual$h3, expected$h3)
  expect_equal(actual$n_units, expected$n_units, tolerance = 1e-12)
})

test_that("interview date summary reuses month and counts by wave", {
  df <- data.frame(
    economy = c("A", "A", "A", "A", "B"),
    year = c(2018L, 2018L, 2018L, 2021L, 2021L),
    countryyear = c("A, 2018", "A, 2018", "A, 2018", "A, 2021", "B, 2021"),
    timestamp = as.Date(c("2018-01-01", "2018-01-15", NA,
                          "2021-02-01", "2021-02-15")),
    month = c(1L, 1L, NA_integer_, 2L, 2L),
    stringsAsFactors = FALSE
  )

  out <- summarise_interview_dates(df)
  expect_equal(nrow(out), 3L)
  expect_equal(out$hh, c(2L, 1L, 1L))
  expect_equal(out$month_num, c(1L, 2L, 2L))
  expect_equal(as.character(out$countryyear),
               c("A, 2018", "A, 2021", "B, 2021"))
})

test_that("P8: survey-wave metadata preserves wave ordering and labels", {
  df <- data.frame(
    code = c("TST", "TST", "ABC"),
    year = c("2021", "2018", "2020"),
    survname = c("S", "S", "A"),
    economy = c("Testland", "Testland", "Abcland"),
    stringsAsFactors = FALSE
  )

  meta <- survey_wave_metadata(df)
  expect_identical(meta$waves, survey_wave_list(df))
  expect_identical(meta$plot_labels, wave_plot_labels(meta$waves))
  expect_identical(names(meta$plot_labels), meta$waves$label)
})

# ---- Interview-dates echarts renderer (guidelines §7) ------------------------

test_that("echart_interview_dates builds one series per wave over 12 months", {
  skip_if_not_installed("echarts4r")
  d <- data.frame(
    economy = rep("A", 4),
    countryyear = rep(c("A, 2018", "A, 2021"), each = 2),
    month_num = c(1L, 2L, 1L, 2L),
    hh = c(10L, 15L, 12L, 9L)
  )
  ch <- echart_interview_dates(
    d, unit_label = "Households",
    wave_labels = c("A, 2018" = "Wave 18")
  )
  expect_s3_class(ch, "echarts4r")
  expect_equal(length(ch$x$opts$series), 2L)
  expect_equal(
    vapply(ch$x$opts$series, function(s) s$name, character(1)),
    c("Wave 18", "A, 2021")
  )
  expect_equal(length(ch$x$opts$xAxis[[1]]$data), 12L)
  # Month categories align across series: both series cover all 12 months.
  expect_equal(sapply(ch$x$opts$series, function(s) length(s$data)), c(12L, 12L))
  expect_null(echart_interview_dates(NULL))
})

# ---- Wave-keyed density allocation (PERF-43) ---------------------------------

make_density_fixture <- function() {
  waves <- expand.grid(code = c("AAA", "BBB"), year = c("2020", "2021"), survname = "S")
  cm <- do.call(rbind, lapply(seq_len(nrow(waves)), function(i) {
    data.frame(
      code = waves$code[i], year = waves$year[i], survname = waves$survname[i],
      loc_id = paste0(waves$code[i], waves$year[i], "-L", rep(1:3, each = 2)),
      h3 = paste0("h", 1:6), pop_2020 = c(NA, 5, 0, 2, 1, 4)
    )
  }))
  sd <- do.call(rbind, lapply(seq_len(nrow(waves)), function(i) {
    data.frame(
      code = waves$code[i], year = waves$year[i], survname = waves$survname[i],
      loc_id = rep(paste0(waves$code[i], waves$year[i], "-L", 1:3), c(2, 1, 3))
    )
  }))
  list(cm = cm, sd = sd)
}

test_that("wave-keyed density allocation reproduces the per-wave reference", {
  f <- make_density_fixture()
  wav <- .density_wave_summary(f$cm, f$sd)
  wl <- paste(f$cm$code, as.character(f$cm$year), f$cm$survname, sep = "|")
  for (w in unique(wl)) {
    ref <- .density_cell_summary(
      filter_by_wave(f$cm, w), filter_by_wave(f$sd, w)
    )
    d <- wav$cells[wav$cells$wave == w, c("h3", "n_units"), drop = FALSE]
    rownames(d) <- NULL
    expect_identical(d$h3, ref$cells$h3, info = w)
    expect_identical(d$n_units, ref$cells$n_units, info = w)
    expect_identical(unname(wav$n_locations[w]), ref$n_locations, info = w)
  }
})

test_that("pooled wave cells equal the full-mapping allocation within rounding", {
  f <- make_density_fixture()
  ref <- .density_cell_summary(f$cm, f$sd)
  wav <- .density_wave_summary(f$cm, f$sd)
  g <- collapse::GRP(wav$cells, by = "h3")
  pooled <- data.frame(
    h3 = as.character(g$groups$h3),
    n_units = collapse::fsum(wav$cells$n_units, g = g, na.rm = TRUE)
  )
  expect_identical(pooled$h3, ref$cells$h3)
  expect_equal(pooled$n_units, ref$cells$n_units, tolerance = 1e-9)
  expect_identical(as.integer(sum(wav$n_locations)), ref$n_locations)
  expect_null(.density_wave_summary(NULL, f$sd))
})
