# ============================================================================ #
# tests/testthat/test-csv-export-wiring-contract.R                             #
# Guidelines §6 contract: every client-side table CSV button must be backed
# by an export-bundle registration of the same key, so the download a user
# gets from a table is always the same data the bundle carries.
# ============================================================================ #

library(testthat)

scan_keys <- function(pattern) {
  files <- list.files(system.file("R", package = "wiseapp"), pattern = "[.]R$",
                      full.names = TRUE)
  if (!length(files)) {
    # load_all() layout: fall back to the source tree relative to the test.
    files <- list.files("../../R", pattern = "[.]R$", full.names = TRUE)
  }
  keys <- character(0)
  for (f in files) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "\n")
    m <- gregexpr(pattern, txt, perl = TRUE)[[1]]
    if (m[1] == -1) next
    for (i in seq_along(m)) {
      s <- substr(txt, m[i], m[i] + attr(m, "match.length")[i] - 1L)
      keys <- c(keys, sub(pattern, "\\1", s, perl = TRUE))
    }
  }
  sort(unique(keys))
}

test_that("every reactable CSV button has an export-bundle registration", {
  button_keys <- scan_keys(
    "wise_reactable_csv_button\\(\\s*ns\\(\"[^\"]+\"\\),\\s*\"([^\"]+)\""
  )
  expect_gt(length(button_keys), 10)

  bundle_keys <- scan_keys("wise_export_table\\(\\s*key\\s*=\\s*\"([^\"]+)\"")
  expect_gt(length(bundle_keys), 10)

  # mod_1_02 registers its five level keys + the policy key through a
  # paste0() loop; the family is covered when that dynamic registration
  # exists. Every other button key must appear literally in the bundle.
  dynamic_family <- any(grepl(
    'key\\s*=\\s*paste0\\("survey_summary_"',
    scan_dynamic <- paste(vapply(
      list.files(system.file("R", package = "wiseapp"), pattern = "[.]R$",
                  full.names = TRUE),
      function(f) paste(readLines(f, warn = FALSE), collapse = "\n"), ""
    ), collapse = "\n")
  , perl = TRUE))
  if (!dynamic_family) {
    src_dir <- "../../R"
    src <- paste(vapply(list.files(src_dir, pattern = "[.]R$", full.names = TRUE),
                        function(f) paste(readLines(f, warn = FALSE), collapse = "\n"), ""),
                 collapse = "\n")
    dynamic_family <- grepl('key\\s*=\\s*paste0\\("survey_summary_"', src, perl = TRUE)
  }

  missing <- setdiff(
    button_keys,
    c(bundle_keys, if (dynamic_family) grep("^survey_summary_", button_keys, value = TRUE))
  )
  expect_equal(missing, character(0),
               info = paste("CSV buttons without bundle registration:", paste(missing, collapse = ", ")))
})
