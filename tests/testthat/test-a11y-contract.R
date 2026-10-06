# Static accessibility contracts (WCAG 2.2 AA review findings in
# review/REVIEW-2026-10-06.md section 9). These read custom.css and render UI
# fragments with htmltools, so they run without a browser.

a11y_css_rules <- function() {
  path <- file.path("..", "..", "inst", "app", "www", "custom.css")
  skip_if_not(file.exists(path), "custom.css not found")
  css <- paste(readLines(path, warn = FALSE), collapse = "\n")
  css <- gsub("(?s)/\\*.*?\\*/", "", css, perl = TRUE)
  m <- gregexpr("[^{}]+\\{[^{}]*\\}", css)[[1L]]
  rules <- regmatches(css, list(m))[[1L]]
  data.frame(
    selector = trimws(sub("\\{.*$", "", rules)),
    body = sub("^[^{]*\\{", "", sub("\\}\\s*$", "", rules)),
    stringsAsFactors = FALSE
  )
}

# Bodies of every rule whose selector list contains `selector` exactly.
a11y_css_bodies <- function(rules, selector) {
  hit <- vapply(strsplit(rules$selector, ","), function(s) {
    selector %in% trimws(gsub("\\s+", " ", s))
  }, logical(1))
  rules$body[hit]
}

test_that("CR-A11Y-01: info icons keep a visible keyboard focus outline", {
  rules <- a11y_css_rules()
  focus <- paste(a11y_css_bodies(rules, ".wise-info-icon:focus-visible"),
                 collapse = ";")
  expect_match(focus, "outline:\\s*2px solid")
  # No rule may strip the outline from the icon on focus.
  for (sel in c(".wise-info-icon:focus", ".wise-info-icon:focus-visible")) {
    expect_false(any(grepl("outline:\\s*(none|0)",
                           a11y_css_bodies(rules, sel))), info = sel)
  }
})

test_that("R2-A11Y-01: sidebar accordion headers keep a focus indicator", {
  rules <- a11y_css_rules()
  focus <- paste(
    a11y_css_bodies(rules, ".sidebar .accordion-button:focus-visible"),
    collapse = ";"
  )
  expect_match(focus, "outline:\\s*2px solid")
})
