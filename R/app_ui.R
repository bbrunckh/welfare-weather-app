#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  page <- bslib::page_navbar(
    title = tagList(
      "WISE-APP",
      tags$span(class = "app-version", golem::get_golem_version())
    ),
    window_title = "WISE-APP",
    lang = "en",
    theme = bslib::bs_theme(
      version = 5,
      brand   = app_sys("app/_brand.yml"),
      # R2-A11Y-02 (WCAG 1.4.11): Bootstrap's default focus glow is the
      # primary colour at 25% alpha (about 1.4:1 on white). Use a solid 2px
      # ring in the brand blue instead (5.1:1 on white, 3.1:1 on the navbar).
      "input-btn-focus-width"         = "2px",
      "input-btn-focus-color-opacity" = 1,
      "focus-ring-width"              = "2px",
      "focus-ring-opacity"            = 1
    ),
    navbar_options = bslib::navbar_options(theme = "dark", bg = "#002244"),
    header = tagList(
      golem_add_external_resources(),
      shiny::useBusyIndicators(),
      shiny::busyIndicatorOptions(
        spinner_type  = "bars3",
        spinner_color = "#0071BC",
        spinner_size  = "36px"
      )
    ),

    # Page modules
    #
    # UI-47: each step tab carries a status badge (check / reload) fed by
    # `render_step_badge()` in app_server. `value` is set explicitly because
    # it otherwise defaults to `title`, and the badge placeholder in the title
    # would end up serialised into the tab's data-value.
    bslib::nav_panel(
      "Overview",
      value = "overview",
      icon = icon("house"),
      mod_0_overview_ui("overview")
    ),
    bslib::nav_panel(
      tagList("Step 1 - Model welfare", step_badge_ui("step1_badge")),
      value = "step1",
      icon = icon("chart-line"),
      mod_1_modelling_ui("step1")
    ),
    bslib::nav_panel(
      tagList("Step 2 - Climate scenarios", step_badge_ui("step2_badge")),
      value = "step2",
      icon = icon("cloud-sun-rain"),
      mod_2_simulation_ui("step2")
    ),
    bslib::nav_panel(
      tagList("Step 3 - Policy scenarios", step_badge_ui("step3_badge")),
      value = "step3",
      icon = icon("scale-balanced"),
      mod_3_scenario_ui("step3")
    ),
    bslib::nav_spacer(),

    # UI-48: save / share / restore an analysis. Sits with the other
    # top-right utilities rather than inside a step, because it spans all of
    # them.
    export_menu_ui(),
    bslib::nav_item(
      tags$a(
        icon("book-open"), "Docs",
        href = "https://datanalytics.worldbank.org/wise-app-docs/",
        target = "_blank",
        rel = "noopener noreferrer",
        title = "User guide and documentation",
        `aria-label` = "Docs: user guide and documentation (opens in a new tab)"
      )
    ),
    bslib::nav_item(
      tags$a(
        icon("database"), "Data",
        href = "https://github.com/worldbank/wise-app-data",
        target = "_blank",
        rel = "noopener noreferrer",
        title = "View data prep repository on GitHub",
        `aria-label` = "Data: view data prep repository on GitHub (opens in a new tab)"
      )
    ),
    bslib::nav_item(
      tags$a(
        icon("github"),
        href = "https://github.com/worldbank/wise-app",
        target = "_blank",
        rel = "noopener noreferrer",
        title = "View source on GitHub",
        `aria-label` = "View source on GitHub (opens in a new tab)"
      )
    )
  )
  add_main_landmark(page)
}

#' Skip link and main landmark for the navbar page (R2-A11Y-05)
#'
#' page_navbar() has no slot before the navbar and renders its panels in a
#' plain div, so mark the top-level tab content as the `main` landmark and
#' put a "Skip to main content" link first in the tab order.
#' @noRd
add_main_landmark <- function(page) {
  tq <- htmltools::tagQuery(page)
  tq$find(".tab-content")$
    filter(function(x, i) i == 1L)$
    addAttrs(id = "main-content", role = "main", tabindex = "-1")
  tq$resetSelected()$find("nav.navbar")$before(
    tags$a(class = "skip-link", href = "#main-content", "Skip to main content")
  )
  out <- tq$allTags()
  class(out) <- class(page)
  out
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "wiseapp"
    ),
    # Vendored MapLibre/H3 map engine. The engine lives in inst/app/vendor/,
    # outside the bundle_resources scan tree, so this explicit dependency is
    # its only serving path and keeps the strict script order
    # (maplibre -> h3-js -> hexmap.js) with a single execution per page load.
    hexmap_dependency()
  )
}
