# Deployment contract (R2-OPS-01/02/03/04/06): the app must build from its
# declared dependencies, without third-party network access, and the Connect
# manifest must list what the package needs.

pkg_root <- function() {
  root <- testthat::test_path("..", "..")
  if (file.exists(file.path(root, "DESCRIPTION"))) root else NULL
}

declared_packages <- function(root) {
  d <- read.dcf(file.path(root, "DESCRIPTION"),
    fields = c("Depends", "Imports", "Suggests", "LinkingTo"))
  x <- trimws(sub("\\(.*", "", unlist(strsplit(paste(d, collapse = ","), ",|\n"))))
  setdiff(x[nzchar(x) & !is.na(x)], "R")
}

test_that("every pkg:: reference in R/ is a declared dependency", {
  root <- pkg_root()
  skip_if(is.null(root) || !dir.exists(file.path(root, "R")), "source tree not available")

  refs <- character()
  for (f in list.files(file.path(root, "R"), pattern = "\\.R$", full.names = TRUE)) {
    lines <- readLines(f, warn = FALSE)
    lines <- lines[!grepl("^\\s*#", lines)]
    hits <- regmatches(lines, gregexpr("\\b([A-Za-z][A-Za-z0-9.]*)::[A-Za-z._]", lines))
    refs <- c(refs, sub("::.*", "", unlist(hits)))
  }
  # JS/CSS snippets and hint strings that merely contain "::", not packages.
  not_packages <- c("b", "focus", "hover", "l", "tip", "usethis")
  undeclared <- setdiff(
    unique(refs),
    c(declared_packages(root), rownames(utils::installed.packages(priority = "base")),
      "wiseapp", not_packages)
  )
  expect_identical(sort(undeclared), character(0))
})

test_that("Suggests-only engine packages are not imported unconditionally", {
  root <- pkg_root()
  skip_if(is.null(root), "source tree not available")
  imports <- read.dcf(file.path(root, "DESCRIPTION"), fields = "Imports")[1, 1]
  imports <- trimws(sub("\\(.*", "", strsplit(imports, ",|\n")[[1]]))
  expect_false(any(c("parsnip", "ranger", "xgboost", "future", "future.apply", "DT") %in% imports))
})

test_that("the page renders without third-party font or script hosts", {
  local_mocked_bindings(get_golem_version = function(...) "0.0.0",
                        .package = "golem")
  html <- htmltools::renderTags(app_ui(NULL))
  page <- paste(html$head, html$html)
  expect_false(grepl("fonts.googleapis.com|fonts.gstatic.com", page))
})

test_that("Step 3 decomposition UI builds without DT", {
  ui <- mod_3_09_decomposition_ui("x")
  html <- as.character(htmltools::renderTags(ui)$html)
  expect_match(html, "x-headline_decomp_table", fixed = TRUE)
  expect_false(grepl("datatables|DTOutput", html, ignore.case = TRUE))
})

test_that("manifest.json lists every Import and every R/ source file", {
  root <- pkg_root()
  manifest_path <- if (is.null(root)) "" else file.path(root, "manifest.json")
  skip_if_not(file.exists(manifest_path), "manifest.json not available")

  manifest <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  # Runtime closure only: Suggests (test and optional engine packages) are not
  # part of the manifest.
  d <- read.dcf(file.path(root, "DESCRIPTION"), fields = c("Depends", "Imports", "LinkingTo"))
  imports <- trimws(sub("\\(.*", "", unlist(strsplit(paste(d, collapse = ","), ",|\n"))))
  imports <- setdiff(imports[nzchar(imports) & !is.na(imports)], "R")
  imports <- setdiff(imports, rownames(utils::installed.packages(priority = "base")))
  expect_identical(sort(setdiff(imports, names(manifest$packages))), character(0))

  r_files <- paste0("R/", list.files(file.path(root, "R"), pattern = "\\.R$"))
  expect_identical(sort(setdiff(r_files, names(manifest$files))), character(0))
})

test_that("external links opened in a new tab carry rel=noopener", {
  local_mocked_bindings(get_golem_version = function(...) "0.0.0",
                        .package = "golem")
  pages <- list(app_ui(NULL), mod_0_overview_ui("overview"))
  for (ui in pages) {
    html <- as.character(htmltools::renderTags(ui)$html)
    anchors <- regmatches(html, gregexpr("<a [^>]*target=\"_blank\"[^>]*>", html))[[1]]
    # Same-origin shiny download links are not external.
    anchors <- anchors[grepl("href=\"https?://", anchors)]
    expect_gt(length(anchors), 0L)
    expect_true(all(grepl("noopener", anchors, fixed = TRUE)), info = paste(anchors, collapse = "\n"))
  }
})
