# ============================================================================ #
# tests/testthat/test-map-view-memory.R                                        #
# map_view_memory must only restore the recorded view while the data key is    #
# unchanged. When the key changes (e.g. another sample country), the stale     #
# view is dropped so the rebuilt map fits the new geography.                   #
# ============================================================================ #

library(testthat)

make_map <- function() {
  m <- mapgl::maplibre(style = "https://example.invalid/style.json")
  m$x$fitBounds <- list(bounds = c(10, -18, 40, 15), options = list(animate = FALSE))
  m
}

test_that("map_view_memory keeps the view for the same key and re-fits when it changes", {
  key <- shiny::reactiveVal("A")

  mod <- function(id) {
    moduleServer(id, function(input, output, session) {
      mem <- map_view_memory(input, session, "m", key = key)
      mem$remember()
      list(restore = mem$restore, stored = mem$get)
    })
  }

  shiny::testServer(mod, args = list(id = "t"), {
    mem <- session$returned

    # ignoreInit = TRUE swallows the observer's first (merged) flush, so prime
    # the inputs with a throwaway event before the recorded one.
    session$setInputs(m_center = list(lng = 0, lat = 0), m_zoom = 0L)
    session$flushReact()

    # MapLibre reports a view; it is recorded under the current key.
    session$setInputs(m_center = list(lng = 10, lat = 0), m_zoom = 5L)
    session$flushReact()
    st <- mem$stored()
    expect_equal(st$lng, 10)
    expect_equal(st$zoom, 5L)
    expect_identical(st$key, "A")

    # Same key: the view is reapplied to a rebuilt widget - center/zoom are
    # overridden and the widget's own fitBounds is dropped.
    r <- mem$restore(make_map())
    expect_equal(r$x$center, c(10, 0))
    expect_equal(r$x$zoom, 5L)
    expect_null(r$x$fitBounds)

    # Key change (new sample country): the stale view is dropped, so the
    # rebuilt widget fits its own data bounds.
    key("B")
    m2 <- make_map()
    r2 <- mem$restore(m2)
    expect_identical(r2, m2)
    expect_null(mem$stored())

    # Panning under the new key records against the new key.
    session$setInputs(m_center = list(lng = 35, lat = 0), m_zoom = 4L)
    session$flushReact()
    expect_identical(mem$stored()$key, "B")
    r3 <- mem$restore(make_map())
    expect_equal(r3$x$center, c(35, 0))
    expect_equal(r3$x$zoom, 4L)
  })
})

test_that("map_view_memory without a key behaves as before (view always restored)", {
  mod <- function(id) {
    moduleServer(id, function(input, output, session) {
      mem <- map_view_memory(input, session, "m")
      mem$remember()
      list(restore = mem$restore, stored = mem$get)
    })
  }

  shiny::testServer(mod, args = list(id = "t"), {
    mem <- session$returned
    session$setInputs(m_center = list(lng = 0, lat = 0), m_zoom = 0L)
    session$flushReact()
    session$setInputs(m_center = list(lng = 1, lat = 2), m_zoom = 3L)
    session$flushReact()
    expect_identical(mem$stored()$key, NULL)
    r <- mem$restore(make_map())
    expect_equal(r$x$center, c(1, 2))
    expect_equal(r$x$zoom, 3L)
    expect_null(r$x$fitBounds)
  })
})
