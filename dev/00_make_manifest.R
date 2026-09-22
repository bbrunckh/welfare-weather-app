# Generate the Posit Connect deployment manifest (manifest.json).
#
# Why a script: rsconnect::writeManifest() ships the full recursive
# dependency closure *plus* Suggests trees, so the app's ~53 direct
# dependencies became ~172 manifest packages — including covr/testthat/
# spelling chains that never run on Connect. This script generates the
# manifest normally, then filters the package set to the runtime
# closure: the app's Depends + Imports + LinkingTo resolved recursively
# over the manifest's own embedded package metadata (no reliance on the
# local library state), so Suggests-only subtrees are dropped while
# everything Connect needs to restore the runtime env stays in.
#
# The manifest is generated from committed content only (git archive
# HEAD), so file checksums match what is committed rather than any
# in-flight working-tree edits. Re-run after dependency changes and
# commit the result.
#
# Usage: Rscript dev/make_manifest.R [--include-suggests]
#   --include-suggests  skip the closure filter and keep every package
#                       writeManifest resolves (deployment-debugging aid)

args <- commandArgs(trailingOnly = TRUE)
filter_enabled <- !"--include-suggests" %in% args

repo_root <- normalizePath(file.path(dirname(
  sub("^--file=", "", grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE))
), ".."), mustWork = TRUE)

stopifnot(requireNamespace("rsconnect", quietly = TRUE),
          requireNamespace("jsonlite", quietly = TRUE))

# -- 1. Stage committed content ---------------------------------------------
stage <- file.path(tempdir(), "wiseapp-manifest-stage")
if (dir.exists(stage)) unlink(stage, recursive = TRUE)
dir.create(stage, recursive = TRUE)
tarball <- file.path(tempdir(), "wiseapp-manifest-stage.tar")
system2("git", c("-C", shQuote(repo_root), "archive", "--output", shQuote(tarball), "HEAD"))
utils::untar(tarball, exdir = stage)
unlink(tarball)
cat("Staged committed content (git archive HEAD) at:", stage, "\n")

# -- 2. Generate the manifest ------------------------------------------------
rsconnect::writeManifest(
  appDir = stage,
  appFiles = c("app.R", "R", "src", "inst", "DESCRIPTION", "NAMESPACE"),
  appPrimaryDoc = "app.R"
)
manifest <- jsonlite::fromJSON(file.path(stage, "manifest.json"),
                               simplifyVector = FALSE)
pkg_names_all <- names(manifest$packages)
cat("writeManifest packages:", length(pkg_names_all),
    "| files:", length(manifest$files), "\n")

# -- 3. Filter to the runtime closure ----------------------------------------
app_desc <- readLines(file.path(stage, "DESCRIPTION"))
read_dep_field <- function(field) {
  line <- grep(paste0("^", field, ":"), app_desc, value = TRUE)
  if (!length(line)) return(character(0))
  out <- line
  i <- grep(paste0("^", field, ":"), app_desc) + 1L
  while (i <= length(app_desc) && grepl("^[[:space:]]", app_desc[i])) {
    out <- c(out, app_desc[i])
    i <- i + 1L
  }
  paste(out, collapse = " ")
}
dep_text <- paste(
  read_dep_field("Depends"), read_dep_field("Imports"),
  read_dep_field("LinkingTo"), sep = ",")
direct <- trimws(strsplit(dep_text, ",")[[1]])
direct <- sub("\\(.*$", "", direct)
direct <- trimws(direct)
direct <- sub("^(Depends|Imports|LinkingTo):[[:space:]]*", "", direct)
direct <- setdiff(unique(direct[direct != "" & direct != "R"]), "wiseapp")
# Base packages ship with R itself; Connect does not install them from
# the manifest and writeManifest never lists them.
base_pkgs <- rownames(installed.packages(priority = "base"))
missing_direct <- setdiff(direct, c(pkg_names_all, base_pkgs))
direct <- setdiff(direct, base_pkgs)

# Embedded DESCRIPTION metadata of every manifest package.
embedded_deps <- function(pkg) {
  d <- manifest$packages[[pkg]]$description
  if (is.null(d)) return(character(0))
  paste(unlist(d[c("Depends", "Imports", "LinkingTo")], use.names = FALSE),
        collapse = ",")
}
split_dep_names <- function(txt) {
  if (is.null(txt) || !nzchar(txt)) return(character(0))
  fields <- trimws(strsplit(txt, ",")[[1]])
  fields <- fields[nzchar(fields)]
  out <- trimws(sub("\\(.*$", "", fields))
  out <- out[out != "R" & nzchar(out)]
  out
}
graph <- lapply(pkg_names_all, function(p) split_dep_names(embedded_deps(p)))
names(graph) <- pkg_names_all

# BFS over the manifest's own package set.
keep <- character(0)
queue <- intersect(direct, pkg_names_all)
if (length(missing_direct)) {
  stop("Direct dependencies missing from the manifest: ",
       paste(missing_direct, collapse = ", "),
       "\n(If a listed dependency is a file or a non-package, fix DESCRIPTION.)",
       call. = FALSE)
}
while (length(queue)) {
  p <- queue[1L]
  queue <- queue[-1L]
  if (p %in% keep) next
  keep <- c(keep, p)
  # Only dependencies that exist in the manifest can be kept; embedded
  # DESCRIPTIONs may name Suggests or soft deps renv skipped.
  queue <- union(queue,
                 setdiff(intersect(graph[[p]] %||% character(0), pkg_names_all),
                         keep))
}
keep <- unique(keep)
stopifnot(all(keep %in% pkg_names_all))

dropped <- setdiff(pkg_names_all, keep)
if (filter_enabled) {
  manifest$packages <- manifest$packages[keep]
  cat("Runtime closure:", length(keep), "packages;",
      length(dropped), "dropped (Suggests-only trees)\n")
  cat("Dropped:", paste(sort(dropped), collapse = ", "), "\n")
} else {
  cat("Closure filter disabled (--include-suggests); keeping all",
      length(pkg_names_all), "packages\n")
}

# -- 4. Write back ------------------------------------------------------------
jsonlite::write_json(manifest, file.path(repo_root, "manifest.json"),
                     auto_unbox = TRUE, pretty = TRUE, null = "null")
cat("Wrote", file.path(repo_root, "manifest.json"), "\n")
