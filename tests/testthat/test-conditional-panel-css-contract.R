# shiny 1.14 changed conditionalPanel visibility from inline styles to the
# shiny-conditional--shown class (shiny.css:
#   [data-display-if]:not(.shiny-conditional--shown) { display: none }).
# Any custom class on a conditionalPanel whose CSS sets `display` (or any
# stylesheet rule setting `display` on .shiny-panel-conditional without the
# :not(...) guard) pins the panel open or shut regardless of its condition.
# The config-flyout panels hit exactly this when shiny moved 1.13 -> 1.14.
# These tests pin the two invariants that keep that class of bug out.

test_that("conditionalPanel calls only carry the audited flyout attributes", {
  srcs <- c(Sys.glob(file.path("..", "..", "R", "*.R")), "app.R")
  srcs <- srcs[file.exists(srcs)]
  skip_if(length(srcs) == 0, "R sources not found")

  calls <- list()
  call_name <- function(e) {
    fn <- e[[1L]]
    if (is.symbol(fn)) return(as.character(fn))
    if (is.call(fn) && identical(deparse(fn[[1L]]), "::")) {
      return(paste(deparse(fn), collapse = ""))
    }
    ""
  }
  collect <- function(e, file) {
    if (!is.call(e)) return(invisible(NULL))
    fname <- call_name(e)
    if (fname %in% c("conditionalPanel", "shiny::conditionalPanel") &&
        length(e) > 1L) {
      for (i in seq_along(e)[-1L]) {
        nm <- names(e)[i]
        if (is.null(nm) || !nzchar(nm) || nm %in% c("condition", "ns")) {
          next
        }
        val <- tryCatch(paste(deparse(e[[i]]), collapse = " "),
                        error = function(e) "?")
        calls[[length(calls) + 1L]] <<- list(
          file = file, name = nm, value = val
        )
      }
    }
    for (j in seq_along(e)[-1L]) {
      if (is.call(e[[j]])) collect(e[[j]], file)
    }
    invisible(NULL)
  }
  for (f in srcs) {
    exprs <- tryCatch(parse(f), error = function(e) NULL)
    if (is.null(exprs)) next
    for (i in seq_along(exprs)) collect(exprs[[i]], basename(f))
  }

  # The audited case: the config flyout in utils_ui.R (custom.css carries
  # the paired shown/hidden rules). The panel id / data-flyout-for values
  # are runtime expressions (paste0 of the toggle id), so only names are
  # asserted for them. Any other attribute — in particular class or style —
  # reintroduces the pinned-open bug under shiny >= 1.14.
  allowed_names <- c("class", "id", "data-flyout-for")
  for (x in calls) {
    desc <- sprintf("%s: %s = %s", x$file, x$name, substr(x$value, 1, 60))
    expect_true(x$name %in% allowed_names, info = desc)
    if (x$name == "class") {
      # x$value is deparsed source, so the string literal keeps its quotes.
      expect_identical(x$value, '"config-flyout"', info = desc)
    }
  }
  expect_true(length(calls) >= 1L,
              info = "expected the config-flyout conditionalPanel to exist")
})

test_that("custom.css pairs the flyout block rule with the 1.14 hidden-state guard", {
  path <- file.path("..", "..", "inst", "app", "www", "custom.css")
  skip_if_not(file.exists(path), "custom.css not found")
  css <- paste(readLines(path, warn = FALSE), collapse = "\n")

  # The base restore rule and the class-guarded hidden rule must exist
  # together: the base rule alone pins the panel open under shiny >= 1.14.
  expect_match(
    css,
    "(?s)\\.shiny-panel-conditional\\.config-flyout\\s*\\{[^}]*display:\\s*block",
    perl = TRUE
  )
  expect_match(
    css,
    paste0("(?s)\\.shiny-panel-conditional\\.config-flyout",
           ":not\\(\\.shiny-conditional--shown\\)\\s*\\{[^}]*display:\\s*none"),
    perl = TRUE
  )

  # No other rule may set display on the panel base class without the guard:
  # an unguarded display:block reintroduces the pinned-open bug, and an
  # unconditional display:none would hide panels on every shiny version.
  rule_matches <- gregexpr("[^{}]+\\{[^}]*\\}", css)[[1L]]
  starts <- as.integer(rule_matches)
  lens <- attr(rule_matches, "match.length")
  selectors <- vapply(seq_along(starts), function(i) {
    r <- substr(css, starts[i], starts[i] + lens[i] - 1L)
    trimws(substr(r, 1L, regexpr("\\{", r) - 1L))
  }, character(1))
  bodies <- vapply(seq_along(starts), function(i) {
    r <- substr(css, starts[i], starts[i] + lens[i] - 1L)
    sub("^[^{]*\\{", "", sub("\\}\\s*$", "", r))
  }, character(1))
  panel_rules <- grepl("shiny-panel-conditional", selectors) &
    grepl("display\\s*:", bodies)
  guarded <- grepl("shiny-conditional--shown", selectors)

  # Allowed display rules touching .shiny-panel-conditional:
  #   1. the shown-state box restore (base rule, display: block only) —
  #      overridden by rule 2 when hidden;
  #   2. the class-guarded hidden state (:not(.shiny-conditional--shown)).
  # Anything else is a pinned-open/pinned-shut bug on some shiny version.
  bad <- character(0)
  for (i in which(panel_rules)) {
    decls <- regmatches(
      bodies[i],
      gregexpr("display\\s*:\\s*[a-zA-Z-]+", bodies[i])
    )[[1L]]
    values <- unique(trimws(sub("^display\\s*:\\s*", "", decls)))
    if (guarded[i]) {
      if (!"none" %in% values) {
        bad <- c(bad, sprintf("%s (guarded but sets %s)", selectors[i],
                              paste(values, collapse = "/")))
      }
    } else if (length(values) != 1L || !identical(values[[1L]], "block")) {
      bad <- c(bad, sprintf("%s (unguarded, sets %s)", selectors[i],
                            paste(values, collapse = "/")))
    }
  }
  expect_true(length(bad) == 0L, info = paste(
    "unguarded display rules on .shiny-panel-conditional:",
    paste(bad, collapse = " | ")
  ))
})
