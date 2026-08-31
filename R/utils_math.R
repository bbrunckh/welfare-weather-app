# Server-side LaTeX rendering via {katex}. All math in the app is static, so
# rendered HTML is cached per R process. KaTeX CSS + fonts are bundled at
# inst/app/www/katex/ and auto-linked by golem::bundle_resources() — no
# htmlDependency wiring needed. This replaces client-side MathJax, which
# failed to render on some Windows setups.

.katex_cache <- new.env(parent = emptyenv())

# One internal reproducibility contract for stochastic analysis paths. The seed
# is intentionally not user-facing yet; configuration/provenance support is a
# separate release-readiness concern.
WISEAPP_DEFAULT_SEED <- 123L

#' Derive a stable integer seed for an analysis substream
#'
#' @param seed Base integer seed.
#' @param ... Values identifying the stochastic operation.
#'
#' @return A positive integer suitable for \code{set.seed()}.
#' @noRd
wise_seed <- function(seed = WISEAPP_DEFAULT_SEED, ...) {
  seed <- as.integer(seed %||% WISEAPP_DEFAULT_SEED)[1L]
  if (is.na(seed)) seed <- WISEAPP_DEFAULT_SEED
  context <- paste(vapply(list(...), function(x) {
    paste(as.character(x), collapse = "\r")
  }, character(1)), collapse = "\n")
  hash <- digest::digest(
    paste(seed, context, sep = "\n"),
    algo = "xxhash32",
    serialize = FALSE
  )
  derived <- strtoi(substr(hash, 1L, 7L), base = 16L)
  as.integer((as.double(derived) + abs(as.double(seed))) %%
               (.Machine$integer.max - 1L) + 1L)
}

#' Select deterministic fallback values from a reference vector
#'
#' @param values Reference values to select from.
#' @param keys Stable identifiers for the requested values.
#' @param seed Base seed used to namespace the lookup.
#'
#' @return A vector of length \code{keys} drawn deterministically from
#'   \code{values}.
#' @noRd
deterministic_values_by_key <- function(values, keys,
                                        seed = WISEAPP_DEFAULT_SEED) {
  if (length(values) == 0L) return(values)
  keys <- as.character(keys)
  keys[is.na(keys)] <- paste0("<NA>", which(is.na(keys)))
  unique_keys <- unique(keys)
  index_by_key <- vapply(unique_keys, function(key) {
    wise_seed(seed, "keyed-value", key) %% length(values) + 1L
  }, integer(1))
  values[index_by_key[match(keys, unique_keys)]]
}

#' Render LaTeX to static HTML (no client-side JS)
#' @param tex LaTeX string (no $ delimiters).
#' @param display Use display (block) mode. Default FALSE = inline.
#' @noRd
wise_math <- function(tex, display = FALSE) {
  key <- paste0(as.integer(display), "\r", tex)
  out <- .katex_cache[[key]]
  if (is.null(out)) {
    out <- as.character(
      katex::katex_html(tex, displayMode = display, include_css = FALSE))
    .katex_cache[[key]] <- out
  }
  htmltools::HTML(out)
}

#' Welfare-function equation block (replaces equation.md / equation2.md)
#' @param predicted TRUE for the Step 2/3 predicted-welfare variant
#'   (formerly equation2.md); FALSE for Step 1 (formerly equation.md).
#' @noRd
welfare_equation_ui <- function(predicted = FALSE) {
  y_tex <- if (predicted) "\\widehat{Y_{ijt}}" else "Y_{ijt}"
  eq    <- paste0(y_tex, " = f(W_{jf(t)}, X_{ijt}, E_{jt}) + \\epsilon_{ijt}")
  y_txt <- if (predicted) ": predicted outcome of individual/household "
           else ": outcome of individual/household "

  htmltools::tagList(
    shiny::h5(if (predicted) "Predicted welfare" else "Welfare function"),
    shiny::tags$div(class = "wise-equation", wise_math(eq, display = TRUE)),
    shiny::tags$ul(
      shiny::tags$li(
        wise_math(y_tex), y_txt, wise_math("i"),
        " in location ", wise_math("j"), " at time ", wise_math("t")
      ),
      shiny::tags$li(
        wise_math("W_{jf(t)}"), ": weather conditions in location ",
        wise_math("j"), " at time ", wise_math("f(t)")
      ),
      shiny::tags$li(
        wise_math("X_{ijt}"), ": characteristics of individual/household ",
        wise_math("i"), " in location ", wise_math("j"),
        " at time ", wise_math("t")
      ),
      shiny::tags$li(
        wise_math("E_{jt}"), ": characteristics of location ",
        wise_math("j"), " at time ", wise_math("t")
      ),
      shiny::tags$li(wise_math("\\epsilon_{ijt}"), ": error term")
    )
  )
}
