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
