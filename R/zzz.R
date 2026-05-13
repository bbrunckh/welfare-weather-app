# Suppress the chatty jsonlite future-deprecation warning emitted by
# leaflet (and any other htmlwidget) when properties contain named
# vectors: "Input to asJSON(keep_vec_names=TRUE) is a named vector...".
#
# The warning fires during htmlwidget JSON serialization, which Shiny
# performs *outside* the renderLeaflet expression's call stack — so a
# local withCallingHandlers around the render block can't catch it.
# Installing a custom `htmlwidgets.TOJSON_FUNC` is the documented seam:
# htmlwidgets reads this option each time it serialises a widget to
# JSON, so we can wrap its internal `toJSON2` with a calling handler
# that mutes only this specific warning.
.onLoad <- function(libname, pkgname) {
  toJSON2 <- tryCatch(
    utils::getFromNamespace("toJSON2", "htmlwidgets"),
    error = function(e) NULL
  )
  if (is.null(toJSON2)) return(invisible(NULL))

  silent_tojson <- function(...) {
    withCallingHandlers(
      toJSON2(...),
      warning = function(w) {
        if (grepl("keep_vec_names", conditionMessage(w), fixed = TRUE))
          invokeRestart("muffleWarning")
      }
    )
  }

  # Only set if no other package has already installed a custom encoder
  # (don't clobber user customisations).
  if (is.null(getOption("htmlwidgets.TOJSON_FUNC"))) {
    options(htmlwidgets.TOJSON_FUNC = silent_tojson)
  }

  invisible(NULL)
}
