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

# WCAG 2.x contrast ratio between two hex colours.
a11y_contrast <- function(fg, bg) {
  lum <- function(h) {
    v <- grDevices::col2rgb(h)[, 1] / 255
    v <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
    sum(c(0.2126, 0.7152, 0.0722) * v)
  }
  l <- sort(c(lum(fg), lum(bg)), decreasing = TRUE)
  (l[1] + 0.05) / (l[2] + 0.05)
}

# `fg` composited at `alpha` over `bg`.
a11y_blend <- function(fg, bg, alpha) {
  f <- grDevices::col2rgb(fg)[, 1]
  b <- grDevices::col2rgb(bg)[, 1]
  grDevices::rgb(t(round(alpha * f + (1 - alpha) * b)), maxColorValue = 255)
}

a11y_decl <- function(bodies, prop) {
  m <- regmatches(bodies, regexpr(paste0("(^|[;\\s])", prop,
                                         ":\\s*[^;]+"), bodies, perl = TRUE))
  trimws(sub(paste0("^.*", prop, ":\\s*"), "", m))
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

test_that("R2-A11Y-02: theme focus ring is a solid brand-blue ring", {
  skip_if_not_installed("brand.yml")
  local_mocked_bindings(get_golem_version = function(...) "0.0.0",
                        .package = "golem")
  deps <- htmltools::resolveDependencies(
    htmltools::findDependencies(app_ui(NULL))
  )
  bs <- Filter(function(d) identical(d$name, "bootstrap"), deps)
  skip_if(length(bs) == 0L, "bootstrap dependency not found")
  css_files <- file.path(bs[[1L]]$src$file, bs[[1L]]$stylesheet)
  css <- paste(unlist(lapply(css_files[file.exists(css_files)], readLines,
                             warn = FALSE)), collapse = "\n")
  # Inputs, selects and nav links: opaque 2px #0071bc, not a 25% alpha glow.
  expect_match(css, "\\.form-control:focus\\{[^}]*box-shadow:0 0 0 2px #0071bc",
               ignore.case = TRUE)
  expect_match(css, "\\.nav-link:focus-visible[^{]*\\{[^}]*box-shadow:0 0 0 2px #0071bc",
               ignore.case = TRUE)
  expect_match(css, "--bs-accordion-btn-focus-box-shadow: 0 0 0 2px #0071bc",
               ignore.case = TRUE)
})

test_that("R2-A11Y-02: buttons and selected pills meet 3:1 non-text contrast", {
  rules <- a11y_css_rules()
  btn <- paste(a11y_css_bodies(rules, "body .btn"), collapse = ";")
  expect_match(btn, "--bs-btn-focus-box-shadow:[^;]*4px var\\(--bs-primary")
  pill <- paste(
    a11y_css_bodies(rules, '.toggle-slider input[type="radio"]:checked + span'),
    collapse = ";"
  )
  expect_match(pill, "inset 0 0 0 1px var\\(--bs-primary")
})

test_that("CR-A11Y-02: small muted and status text reaches 4.5:1", {
  rules <- a11y_css_rules()
  # Text rules flagged in the review; all sit on white or near-white cards.
  sels <- c(".step1-headline-note", ".headline-card-note", ".diagnostic-note",
            ".wise-table .se", ".wise-table .t2-note", ".selection-card-op",
            ".selection-card-chevron", ".step2-summary-separator",
            ".pipeline-done", ".pipeline-failed")
  for (sel in sels) {
    col <- a11y_decl(a11y_css_bodies(rules, sel), "color")
    expect_length(col, 1L)
    expect_true(grepl("^#[0-9a-fA-F]{6}$", col), info = sel)
    expect_gte(a11y_contrast(col, "#f8fafc"), 4.5)
  }
  # Hero text is translucent white on a navy-to-blue gradient; check every
  # text rule against the lightest (last) gradient stop.
  bg <- a11y_css_bodies(rules, ".hero-panel")
  stops <- regmatches(bg, gregexpr("#[0-9a-fA-F]{6}", bg))[[1L]]
  lightest <- stops[length(stops)]
  for (sel in c(".hero-panel h1", ".hero-panel .hero-subtitle",
                ".hero-panel p", ".hero-panel .hero-team")) {
    op <- a11y_decl(a11y_css_bodies(rules, sel), "opacity")
    alpha <- if (length(op)) as.numeric(op) else 1
    expect_gte(a11y_contrast(a11y_blend("#ffffff", lightest, alpha),
                             lightest), 4.5)
  }
  link <- a11y_decl(a11y_css_bodies(rules, ".hero-panel a"), "color")
  expect_gte(a11y_contrast(link, lightest), 4.5)
})

test_that("CR-A11Y-04: unlabeled pill toggles are named by aria_label", {
  tag <- pill_toggle("x", choices = c(A = "a", B = "b"), aria_label = "Pick")
  expect_identical(tag$attribs[["aria-label"]], "Pick")
  expect_null(tag$attribs[["aria-labelledby"]])
  # A visible label keeps the native aria-labelledby association.
  tag <- pill_toggle("x", choices = c(A = "a"), label = "Shown",
                     aria_label = "Pick")
  expect_identical(tag$attribs[["aria-labelledby"]], "x-label")
  expect_null(tag$attribs[["aria-label"]])
  # Disabled pills go through the tree walk; the name must survive it.
  tag <- pill_toggle("x", choices = c(A = "a", B = "b"), disabled = TRUE,
                     aria_label = "Pick")
  expect_identical(tag$attribs[["aria-label"]], "Pick")
  tag <- wave_toggle_slider("w", choices = c(`2010` = "k1", `2015` = "k2"))
  expect_identical(tag$attribs[["aria-label"]], "Survey wave")
})

test_that("CR-A11Y-04: every label-less pill toggle in R/ passes aria_label", {
  srcs <- Sys.glob(file.path("..", "..", "R", "*.R"))
  skip_if(length(srcs) == 0, "R sources not found")
  bad <- character(0)
  walk <- function(e, file) {
    if (!is.call(e)) return(invisible(NULL))
    fn <- e[[1L]]
    nm <- if (is.symbol(fn)) as.character(fn) else ""
    if (nm %in% c("pill_toggle", "wave_toggle_slider")) {
      an <- names(as.list(e))
      if (is.null(an)) an <- rep("", length(e))
      # label is the fourth formal; count positional args to detect it.
      named <- an[-1L]
      pos <- sum(!nzchar(named))
      formals_left <- setdiff(c("inputId", "choices", "selected", "label"),
                              named)
      has_label <- "label" %in% named && !is.null(e[["label"]])
      if (!"label" %in% named && pos >= match("label", formals_left, 99L)) {
        has_label <- TRUE
      }
      if (!has_label && nm == "pill_toggle" && !"aria_label" %in% named) {
        bad <<- c(bad, paste(file, deparse(e)[1L]))
      }
    }
    for (i in seq_along(e)[-1L]) {
      a <- e[[i]]
      if (!missing(a) && is.call(a)) walk(a, file)
    }
    invisible(NULL)
  }
  for (f in srcs) {
    for (ex in parse(f, keep.source = FALSE)) walk(ex, basename(f))
  }
  expect_identical(bad, character(0), info = paste(bad, collapse = " | "))
})

test_that("CR-A11Y-04: slider focus targets get role and name in custom.js", {
  path <- file.path("..", "..", "inst", "app", "www", "custom.js")
  skip_if_not(file.exists(path), "custom.js not found")
  js <- paste(readLines(path, warn = FALSE), collapse = "\n")
  expect_match(js, "setAttribute('role', 'slider')", fixed = TRUE)
  expect_match(js, "aria-labelledby", fixed = TRUE)
  expect_match(js, "aria-valuenow", fixed = TRUE)
})

test_that("CR-A11Y-04: social protection select and slider have hidden labels", {
  src <- file.path("..", "..", "R", "mod_3_01_sp.R")
  skip_if_not(file.exists(src), "R sources not found")
  code <- paste(readLines(src, warn = FALSE), collapse = "\n")
  expect_match(code, 'visually-hidden", "Targeting"', fixed = TRUE)
  expect_match(code, 'visually-hidden", "Transfers per year"', fixed = TRUE)
})
