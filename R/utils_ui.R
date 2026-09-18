#' Info icon that opens a click-triggered popover
#'
#' Place inside a heading: `h4("My header", info_popover(p("Explanation...")))`.
#' Works in static UI, renderUI output, and insertUI/appendTab content -
#' bslib popovers render as `<bslib-popover>` web components that
#' self-initialize when inserted into the DOM.
#'
#' @param ... Popover body content (character strings and/or tags; use
#'   `shiny::p()` per paragraph).
#' @param title Optional popover header text.
#' @param docs If TRUE, append a muted "See documentation for details." line.
#' @param placement "auto", "top", "right", "bottom", "left". Default "right".
#' @noRd
info_popover <- function(..., title = NULL, docs = FALSE, placement = "right") {
  body <- htmltools::tagList(...)
  if (isTRUE(docs)) {
    body <- htmltools::tagList(
      body,
      shiny::tags$p(
        class = "text-muted small mb-0",
        "See documentation for details."
      )
    )
  }
  bslib::popover(
    trigger = shiny::tags$span(
      class = "wise-info-icon", tabindex = "0", role = "button",
      `aria-label` = "More information",
      shiny::icon("circle-info")
    ),
    body, title = title, placement = placement
  )
}

# Inline "no data" warning ----

#' Amber inline warning with an exclamation icon, shown when a selection
#' has no data (e.g. no variables found for the current level of analysis).
#'
#' @param ... Message text; multiple strings are wrapped in a single span.
#' @noRd
no_data_warning <- function(...) {
  shiny::tags$div(
    class = "no-data-warning warning-message",
    shiny::icon("triangle-exclamation"),
    shiny::tags$span(...)
  )
}

# Selection summary card ----
# Thin "what is loaded" card at the top of the stats tabs (Survey, Outcome,
# Weather, Model). Head: uppercase title + right-aligned badge; body: one row
# per item with a bold name, muted sub-label and small pills.
# Styled by .selection-card rules in inst/app/www/custom.css.

#' Build one row of a `selection_summary_card()`
#'
#' @param name  Bold primary text (e.g. economy name, variable label).
#' @param sub   Optional muted secondary text (e.g. survey program, variable
#'   name).
#' @param pills Optional character vector of small pill labels (e.g. years,
#'   units, transform).
#' @param note  Optional trailing note in grey text at the same size as
#'   `name` (e.g. a plain-language interpretation of the selection).
#'
#' @noRd
selection_card_row <- function(name, sub = NULL, pills = NULL, note = NULL) {
  shiny::tags$div(
    class = "selection-card-row",
    shiny::tags$span(class = "selection-card-name", name),
    if (!is.null(sub) && nzchar(sub)) {
      shiny::tags$span(class = "selection-card-sub", sub)
    },
    if (length(pills)) {
      shiny::tags$span(
        class = "selection-card-pills",
        lapply(pills, function(p) {
          shiny::tags$span(class = "selection-card-pill", p)
        })
      )
    },
    if (!is.null(note) && nzchar(note)) {
      shiny::tags$span(class = "selection-card-note", note)
    }
  )
}

#' Thin summary card describing the current selection
#'
#' Shared component for the "Selected sample" card on the Survey stats tab and
#' the outcome/weather/model equivalents on other tabs.
#'
#' @param title Card title, rendered small and uppercase; `NULL` for a
#'   headerless card.
#' @param rows  List of row specs; each a list with `name`, optional `sub`
#'   and `pills` (see `selection_card_row()`), or pre-built row tags.
#' @param badge Optional right-aligned badge in the card head (e.g. level of
#'   analysis).
#' @param info  Optional text; when supplied an (i) popover explaining the
#'   card is attached to the title.
#' @param compact Logical; tighter paddings/font sizes for sidebar use.
#'
#' @noRd
selection_summary_card <- function(title, rows, badge = NULL, info = NULL,
                                   compact = FALSE) {
  head <- if (is.null(title) && is.null(badge)) {
    NULL
  } else {
    shiny::tags$div(
      class = "selection-card-head",
      if (!is.null(title)) {
        shiny::tags$span(
          class = "selection-card-title",
          title,
          if (!is.null(info) && nzchar(info)) info_popover(shiny::p(info))
        )
      },
      if (!is.null(badge) && nzchar(badge)) {
        shiny::tags$span(class = "selection-card-badge", badge)
      }
    )
  }
  # Headerless card with an info popover: anchor the (i) on the first row.
  if (is.null(title) && !is.null(info) && nzchar(info)) {
    rows[[1]] <- shiny::tagAppendChildren(rows[[1]], info_popover(shiny::p(info)))
  }
  shiny::tags$div(
    class = paste("selection-card", if (isTRUE(compact)) "compact"),
    head,
    lapply(rows, function(r) {
      # Accept both pre-built row tags and raw name/sub/pills spec lists
      if (inherits(r, c("shiny.tag", "shiny.tag.list"))) {
        r
      } else {
        selection_card_row(
          name  = r$name,
          sub   = r$sub,
          pills = r$pills,
          note  = r$note
        )
      }
    })
  )
}

#' Compact summary of the Step 2 simulation run
#'
#' Shared by the Results and Diagnostics tabs so both surfaces describe the
#' same historical baseline, weather inputs, and saved future scenarios.
#'
#' @noRd
simulation_summary_card <- function(hist_sim, saved_scenarios = list(),
                                    selected_hist = NULL,
                                    selected_weather = NULL) {
  if (is.null(hist_sim) || is.null(hist_sim$so)) {
    return(NULL)
  }

  run <- hist_sim$sim_summary %||% list()

  hist_years <- run$historical_years %||% tryCatch(
    {
      if (!is.null(selected_hist) && "year_range" %in% names(selected_hist)) {
        unlist(selected_hist$year_range[[1]], use.names = FALSE)
      } else {
        numeric(0)
      }
    },
    error = function(e) numeric(0)
  )
  hist_period <- if (length(hist_years) >= 2L) {
    paste0(hist_years[1], "-", hist_years[2])
  } else {
    as.character(hist_sim$hist_label %||% "Historical baseline")
  }

  scenarios <- if (is.list(saved_scenarios)) names(saved_scenarios) else character(0)
  scenario_count <- length(scenarios)

  baseline <- run$baseline_survey %||% "Selected baseline survey"
  total_runs <- run$total_runs %||% NA_integer_
  badge <- if (is.finite(total_runs)) {
    paste(format(total_runs, big.mark = ","), "simulation years")
  } else if (scenario_count == 0L) {
    "Historical only"
  } else {
    paste(scenario_count, "future", if (scenario_count == 1L) "scenario" else "scenarios")
  }

  scenario_items <- lapply(scenarios, function(key) {
    parts <- strsplit(key, " / ", fixed = TRUE)[[1]]
    ssp <- parts[1] %||% key
    period <- if (length(parts) >= 2L) parts[2] else ""
    n <- suppressWarnings(as.numeric(saved_scenarios[[key]]$n_models %||% NA_real_))[1]
    n_label <- if (is.finite(n)) paste(n, "models") else "model count unavailable"
    shiny::tags$span(
      class = "step2-summary-item",
      shiny::tags$span(
        class = "step2-summary-label step2-summary-scenario-label",
        ssp
      ),
      if (nzchar(period)) {
        shiny::tags$span(class = "step2-summary-value", period)
      },
      shiny::tags$span(class = "selection-card-pill", n_label)
    )
  })
  if (!length(scenario_items)) {
    scenario_items <- list(
      shiny::tags$span(
        class = "step2-summary-item",
        shiny::tags$span(class = "step2-summary-value", "Historical only")
      )
    )
  }
  separator <- shiny::tags$span(class = "step2-summary-separator", "·")
  summary_row <- c(
    scenario_items,
    list(separator),
    list(
      shiny::tags$span(
        class = "step2-summary-item",
        shiny::tags$span(class = "step2-summary-label", "Reference climate"),
        shiny::tags$span(class = "selection-card-pill", hist_period)
      )
    ),
    list(separator),
    list(
      shiny::tags$span(
        class = "step2-summary-item",
        shiny::tags$span(class = "step2-summary-label", "Baseline survey"),
        shiny::tags$span(class = "selection-card-pill", baseline)
      )
    )
  )

  selection_summary_card(
    title = "Selected climate scenarios",
    badge = badge,
    rows = list(shiny::tags$div(
      class = "selection-card-row step2-summary-row",
      summary_row
    ))
  )
}

headline_cards_ui <- function(cards) {
  if (is.null(cards) || !length(cards)) {
    return(NULL)
  }
  shiny::tags$div(
    class = "headline-cards",
    lapply(cards, function(card) {
      shiny::tags$div(
        class = paste("headline-card", card$class %||% ""),
        shiny::tags$div(
          class = "headline-card-label",
          card$label %||% "Result",
          if (!is.null(card$info) && nzchar(card$info)) {
            info_popover(shiny::p(card$info))
          }
        ),
        shiny::tags$div(class = "headline-card-value", card$value %||% "Unavailable"),
        if (!is.null(card$note_html)) {
          shiny::tags$div(class = "headline-card-note", card$note_html)
        } else if (!is.null(card$note) && nzchar(card$note)) {
          shiny::tags$div(class = "headline-card-note", card$note)
        }
      )
    })
  )
}

#' Compact summary of the selected Step 3 policy scenarios
#'
#' @noRd
policy_summary_card <- function(selected_policies = NULL,
                                baseline_hist_sim = NULL,
                                policy_saved_scenarios = list(),
                                selected_weather = NULL,
                                sp_scenario = NULL,
                                policy_scenarios = list(),
                                infra_scenario = NULL,
                                digital_scenario = NULL,
                                labor_scenario = NULL,
                                education_scenario = NULL) {
  policies <- selected_policies %||% character(0)
  policies <- policies[!is.na(policies) & nzchar(policies)]
  labels <- vapply(policies, function(key) {
    def <- POLICY_DEFINITIONS[[key]]
    if (!is.null(def) && !is.null(def$label) && nzchar(def$label)) {
      as.character(def$label)
    } else {
      key
    }
  }, character(1))

  deref <- function(x) {
    if (is.function(x)) x <- tryCatch(x(), error = function(e) NULL)
    if (is.list(x)) x else NULL
  }
  sp <- deref(sp_scenario)
  infra <- deref(infra_scenario)
  digital <- deref(digital_scenario)
  labor <- deref(labor_scenario)
  education <- deref(education_scenario)

  sp_label <- if (has_sp_change(sp)) {
    amount <- if (isTRUE(sp$transfer_amount_usd > 0)) {
      paste0("$", format(sp$transfer_amount_usd, trim = TRUE, big.mark = ","), "/payment")
    } else {
      paste0("$", format(sp$budget_fixed, trim = TRUE, big.mark = ","), " budget")
    }
    payments <- if (is.finite(sp$transfer_n_payments %||% NA_integer_)) {
      paste0(" x ", sp$transfer_n_payments, "/year")
    } else {
      ""
    }
    targeting <- sp$targeting %||% "universal"
    paste("Social protection", amount, payments, "-", targeting)
  } else {
    NULL
  }
  active <- c(
    sp_label,
    if (has_infra_change(infra)) "Infrastructure",
    if (has_digital_change(digital)) "Digital inclusion",
    if (has_labor_change(labor)) "Labor market",
    if (has_education_change(education)) "Education"
  )
  configured_count <- length(active)
  policy_pills <- if (configured_count > 0L) active else "None"
  rows <- list(list(
    name = "Policies",
    sub = if (!configured_count) "no lever has been changed yet",
    pills = policy_pills
  ))
  if (length(labels)) {
    rows <- c(rows, list(list(
      name = "Policy interaction",
      pills = labels
    )))
  }
  selection_summary_card(
    title = "Selected Policy Scenarios",
    badge = paste(
      configured_count,
      if (configured_count == 1L) "policy" else "policies"
    ),
    rows = rows
  )
}

# Variable label shortening ----

#' Drop the "Monthly " prefix from a variable label and capitalise the first
#' letter, so "Monthly daily maximum temperature" reads
#' "Daily maximum temperature" in cards, plots and equations.
#'
#' @param lab Character label (or vector of labels).
#' @return Character, same length.
#' @noRd
wise_label_short <- function(lab) {
  lab <- sub("^Monthly\\s+", "", as.character(lab))
  first <- toupper(substring(lab, 1, 1))
  ifelse(nzchar(lab), paste0(first, substring(lab, 2)), lab)
}

#' Human label for an analysis unit code
#'
#' @param unit One of `"ind"`, `"hh"`, `"firm"` (or `NULL`).
#' @return e.g. `"Household level"`; `NULL` for unknown codes.
#' @noRd
analysis_unit_label <- function(unit) {
  switch(unit %||% "",
    ind  = "Individual level",
    hh   = "Household level",
    firm = "Firm level",
    NULL
  )
}

#' Format weather variable(s) into a concise phrase for card section headings
#'
#' @param weather_var String, character vector, data.frame (with label/name cols),
#'   or NULL.
#' @return Scalar character phrase suitable for "How is outcome predicted to vary with <phrase>..."
#'   Returns empty string `""` if NULL, empty, or more than 2 variables.
#' @noRd
format_weather_heading_phrase <- function(weather_var) {
  if (is.null(weather_var)) {
    return("")
  }
  raw_labels <- if (is.data.frame(weather_var)) {
    cols <- intersect(c("label", "name"), names(weather_var))
    if (length(cols)) as.character(weather_var[[cols[1]]]) else character(0)
  } else if (is.character(weather_var)) {
    weather_var
  } else if (is.list(weather_var)) {
    cols <- intersect(c("label", "name"), names(weather_var))
    if (length(cols)) as.character(weather_var[[cols[1]]]) else character(0)
  } else {
    character(0)
  }
  raw_labels <- raw_labels[!is.na(raw_labels) & nzchar(trimws(raw_labels))]
  if (length(raw_labels) == 1 && grepl(",", raw_labels, fixed = TRUE)) {
    raw_labels <- unlist(strsplit(raw_labels, ",\\s*"), use.names = FALSE)
    raw_labels <- raw_labels[!is.na(raw_labels) & nzchar(trimws(raw_labels))]
  }
  if (length(raw_labels) == 1) {
    tolower(trimws(raw_labels[1]))
  } else if (length(raw_labels) == 2) {
    paste(tolower(trimws(raw_labels[1])), "and", tolower(trimws(raw_labels[2])))
  } else {
    ""
  }
}

# Config flyout blocks (UI-02) ----

#' Accessible plot output (UI-36)
#'
#' `shiny::plotOutput()` output has no intrinsic alt text. This wrapper adds
#' `role="img"` and a descriptive `aria-label` directly on the plot's own
#' container (no extra DOM, so bslib fill/layout behaviour is unchanged), so
#' screen readers announce what the plot shows. Where the surrounding UI
#' re-renders with the current selections (per-variable panels), the label is
#' built from the live variable names; fixed-id plots describe the plot type
#' and content.
#'
#' @param plot_id Namespaced output id for the plot.
#' @param alt     Descriptive text: what the plot shows.
#' @param ...     Forwarded to `shiny::plotOutput()` (height, width, brush,
#'   click, ...).
#'
#' @noRd
wise_plot_output <- function(plot_id, alt, ...) {
  shiny::tagAppendAttributes(
    shiny::plotOutput(plot_id, ...),
    role = "img",
    `aria-label` = alt
  )
}

#' Accessible echarts output (UI-36 analogue)
#'
#' `echarts4rOutput()` has no intrinsic alt text. This wrapper adds
#' `role="img"` and a descriptive `aria-label` on the chart's own container,
#' so screen readers announce what the chart shows.
#'
#' @param chart_id Namespaced output id for the chart.
#' @param alt      Descriptive text: what the chart shows.
#' @param ...      Forwarded to `echarts4r::echarts4rOutput()` (height, ...).
#'
#' @noRd
wise_chart_output <- function(chart_id, alt, ...) {
  shiny::tagAppendAttributes(
    echarts4r::echarts4rOutput(chart_id, ...),
    role = "img",
    `aria-label` = alt
  )
}

#' Toggle button + anchored config flyout panel
#'
#' Shared builder for the Step 1/2 config sidebars' disclosure panels
#' (`.config-flyout` in custom.css). The button and panel are wired for the
#' shared `custom.js` behavior: the button carries `aria-expanded` /
#' `aria-controls`, the panel is marked `data-flyout-for` = toggle id and gets
#' a stable `_panel` id, and `custom.js` enforces one-open-at-a-time, moves
#' focus on open/close, closes on Escape, and positions the flyout beside its
#' own toggle instead of at a shared viewport position.
#'
#' The content stays in the DOM at all times (conditionalPanel odd/even parity,
#' as before). Note that this is not on its own enough for input defaults to
#' register: a `uiOutput()` placed in here is still *hidden*, and hidden
#' outputs are suspended until first shown. Callers that need the flyout's
#' inputs to exist before it is opened must also set
#' `outputOptions(output, "<id>", suspendWhenHidden = FALSE)` (see
#' `mod_1_06_model.R` and `mod_1_04_weather.R`).
#'
#' @param toggle_id    Namespaced input id of the toggle button.
#' @param title        Flyout header title.
#' @param ...          Flyout content, rendered below the header.
#' @param toggle_label  Button label. Default "Configure".
#' @param display_label Optional label rendered beside the button.
#'
#' @noRd
config_flyout_block <- function(toggle_id, title, ..., toggle_label = "Configure",
                                display_label = NULL) {
  panel_id <- paste0(toggle_id, "_panel")
  shiny::tags$div(
    class = paste(
      "config-flyout-anchor",
      if (!is.null(display_label)) "config-flyout-inline"
    ),
    shiny::actionButton(
      toggle_id, toggle_label,
      icon = shiny::icon("sliders"),
      class = "btn-outline-primary btn-sm config-flyout-toggle",
      style = "margin-bottom: 10px;",
      `aria-expanded` = "false",
      `aria-controls` = panel_id
    ),
    if (!is.null(display_label)) {
      shiny::tags$span(display_label, class = "config-flyout-label")
    },
    shiny::conditionalPanel(
      condition = paste0("input['", toggle_id, "'] % 2 == 1"),
      class = "config-flyout",
      id = panel_id,
      `data-flyout-for` = toggle_id,
      shiny::tags$div(
        class = "config-flyout-header",
        shiny::tags$h6(title),
        shiny::tags$button(
          type = "button",
          class = "btn-close",
          `aria-label` = "Close",
          onclick = sprintf("document.getElementById('%s').click();", toggle_id)
        )
      ),
      ...
    )
  )
}

# Number formatting for displayed figures (UI-32) ----

#' Format a number for display at one decimal place
#'
#' One rule for every dynamically computed figure the app shows - the Step 3
#' sidebar's social-protection preview and the diagnostics tab's transfer
#' summary included - so the same quantity never appears at two precisions.
#' Values are rounded, not truncated, and thousands are separated.
#'
#' @param x       Numeric vector.
#' @param digits  Decimal places. Default 1.
#' @param prefix,suffix Optional strings placed either side of the number
#'   (e.g. `prefix = "$"`, `suffix = "%"`).
#' @param na Text used for non-finite values.
#'
#' @return A character vector the same length as `x`.
#' @noRd
fmt_num <- function(x, digits = 1, prefix = "", suffix = "", na = "\u2014") {
  x <- suppressWarnings(as.numeric(x))
  out <- vapply(x, function(v) {
    if (!is.finite(v)) {
      return(na)
    }
    paste0(
      prefix,
      formatC(round(v, digits),
        format = "f", digits = digits,
        big.mark = ","
      ),
      suffix
    )
  }, character(1))
  out
}

#' Format a count for display
#'
#' Whole units (households, individuals, firms) are counts, so they get
#' thousands separators and no decimals - even when survey weights make the
#' underlying value fractional.
#'
#' @param x Numeric vector.
#' @param na Text used for non-finite values.
#' @return A character vector.
#' @noRd
fmt_count <- function(x, na = "\u2014") {
  x <- suppressWarnings(as.numeric(x))
  vapply(x, function(v) {
    if (!is.finite(v)) {
      return(na)
    }
    formatC(round(v), format = "d", big.mark = ",")
  }, character(1))
}


# Nav-header step status (UI-47) ----
#
# Steps can be visited in any order, and results survive a move to another tab,
# so the navbar is the only place where the state of every step is visible at
# once. Each step tab carries a small badge:
#
#   none  - the step has not produced results yet; no badge, navbar stays quiet
#   done  - results exist and match the current inputs (check mark)
#   stale - results exist but an input has changed since (reload arrow)
#
# "stale" reuses the per-step run signatures already maintained for the
# in-page stale banners (INT-08), so the badge and the banner can never
# disagree. A stale step is still fully usable - the badge asks for a re-run,
# it does not lock anything.

#' Placeholder for a step's status badge in the navbar
#'
#' @param output_id Output id, matched by `render_step_badge()` in the server.
#' @return A `span` output container, safe to nest inside a nav link.
#' @noRd
step_badge_ui <- function(output_id) {
  shiny::uiOutput(output_id,
    container = shiny::tags$span, inline = TRUE,
    class = "nav-step-status-slot"
  )
}

#' Build the badge for one step state
#'
#' @param state One of `"none"`, `"done"`, `"stale"`.
#' @param step_label Human name of the step, used in the accessible text.
#' @return A `span` tag, or NULL for `"none"`.
#' @noRd
step_status_badge <- function(state, step_label = "This step") {
  # The glyphs are decorative: aria-hidden takes them out of the
  # accessibility tree (which also suppresses the aria-label shiny::icon()
  # always attaches), and the visually-hidden text carries the meaning.
  deco <- function(name) shiny::icon(name, `aria-hidden` = "true")

  if (identical(state, "done")) {
    tip <- paste0(step_label, ": complete \u2014 results are up to date.")
    return(shiny::tags$span(
      class = "nav-step-status nav-step-status-done",
      title = tip,
      deco("check"),
      shiny::tags$span(class = "visually-hidden", tip)
    ))
  }
  if (identical(state, "stale")) {
    tip <- paste0(
      step_label, ": inputs changed \u2014 re-run to refresh ",
      "the results."
    )
    return(shiny::tags$span(
      class = "nav-step-status nav-step-status-stale",
      title = tip,
      deco("rotate"),
      shiny::tags$span(class = "visually-hidden", tip)
    ))
  }
  NULL
}

#' Classify a step as none / done / stale
#'
#' @param has_result Reactive returning the step's result object (NULL until it
#'   has run), or a logical.
#' @param is_stale   Reactive returning TRUE when the stored result no longer
#'   matches the live inputs.
#'
#' @return A reactive returning `"none"`, `"done"` or `"stale"`.
#' @noRd
step_status <- function(has_result, is_stale = NULL) {
  # Fail fast on a mis-wired badge. If an upstream module renames the key this
  # reads, the argument arrives as NULL - and without this the badge would
  # simply sit on "none" (or, for a missing staleness flag, permanently on
  # "done"), which is worse than an error: it looks like working UI.
  if (!is.function(has_result)) {
    stop("step_status(): `has_result` must be a reactive, got ",
      class(has_result)[1], ".",
      call. = FALSE
    )
  }
  if (!is.null(is_stale) && !is.function(is_stale)) {
    stop("step_status(): `is_stale` must be a reactive or NULL, got ",
      class(is_stale)[1], ".",
      call. = FALSE
    )
  }

  shiny::reactive({
    res <- tryCatch(has_result(), error = function(e) NULL)
    done <- if (is.logical(res) && length(res) == 1L) isTRUE(res) else !is.null(res)
    if (!done) {
      return("none")
    }
    stale <- if (is.null(is_stale)) {
      FALSE
    } else {
      isTRUE(tryCatch(is_stale(), error = function(e) FALSE))
    }
    if (stale) "stale" else "done"
  })
}

#' Render a step's navbar status badge
#'
#' @param has_result,is_stale Reactives, as for `step_status()`.
#' @param step_label Human name of the step, used in the accessible text.
#'
#' @return A `renderUI` expression to assign to the matching output id.
#' @noRd
render_step_badge <- function(has_result, is_stale = NULL,
                              step_label = "This step") {
  status <- step_status(has_result, is_stale)
  shiny::renderUI(step_status_badge(status(), step_label))
}

# Treat any successful import as making existing results stale, even if an
# imported value happens to equal the current control value.
stale_after_import <- function(has_result, is_stale = NULL, imported = NULL) {
  own <- is_stale %||% shiny::reactive(FALSE)
  if (is.null(imported)) {
    return(own)
  }
  seen <- shiny::reactiveVal(0L)
  shiny::observeEvent(has_result(),
    {
      seen(shiny::isolate(imported()))
    },
    ignoreInit = FALSE
  )
  shiny::reactive(
    isTRUE(own()) || isTRUE(imported() > seen())
  )
}


# Table CSV export (UI-45) ----
#
# Every table in the app offers the same export affordance: one small, quiet
# "Download CSV" control. For DT tables that is the Buttons extension, driven
# by the two helpers below; for the handful of hand-built HTML tables it is
# `csv_download_link()` over a `downloadHandler`. Both render as
# `.wise-csv-btn` so they look identical wherever they appear (custom.css).

#' DT `buttons` spec for a single, discreet CSV export
#'
#' @param filename Base name of the downloaded file, without extension.
#' @param enabled  When FALSE, returns NULL so the button is omitted (used to
#'   withhold exports while results are stale - INT-08).
#'
#' @return A list suitable for `DT::datatable(options = list(buttons = ...))`,
#'   or NULL.
#' @noRd
wise_csv_button <- function(filename, enabled = TRUE) {
  if (!isTRUE(enabled)) {
    return(NULL)
  }
  list(list(
    extend        = "csv",
    text          = "Download CSV",
    filename      = filename,
    className     = "wise-csv-btn",
    # Export every row, not just the visible page; keep any active search.
    exportOptions = list(modifier = list(page = "all"))
  ))
}

#' Add the Buttons placeholder to a DT `dom` string
#'
#' When both the page-length picker (`l`) and search box (`f`) are present,
#' wrap Buttons and those controls in one flex row. Tables without both retain
#' the simple leading `B` form.
#'
#' @param dom A DataTables `dom` string (e.g. "t", "lfrtip").
#' @return A `dom` string including the Buttons placeholder.
#' @noRd
wise_csv_dom <- function(dom = "lfrtip") {
  if (grepl("B", dom, fixed = TRUE)) {
    return(dom)
  }
  has_len <- grepl("l", dom, fixed = TRUE)
  has_search <- grepl("f", dom, fixed = TRUE)
  if (!has_len || !has_search) {
    return(paste0("B", dom))
  }

  # Pull l and f out of their original positions into the shared toolbar.
  rest <- gsub("[lf]", "", dom)
  paste0("<'wise-dt-controls'Blf>", rest)
}

#' Small "Download CSV" link for a non-DT table
#'
#' Pairs with a `downloadHandler()` registered under the same output id. Use
#' for hand-built HTML tables (`renderTable()` / `renderUI()`), which have no
#' DataTables toolbar to hang a button off.
#'
#' @param output_id Namespaced id of the matching `downloadHandler` output.
#' @param label     Link text. Default "Download CSV".
#'
#' @return A `downloadLink` tag.
#' @noRd
csv_download_link <- function(output_id, label = "Download CSV") {
  shiny::downloadLink(
    output_id,
    label = shiny::tagList(shiny::icon("download"), label),
    class = "wise-csv-btn wise-csv-link"
  )
}

#' Client-side "Download CSV" button for a reactable table (guidelines §6)
#'
#' reactable tables export through the browser: the button calls
#' `Reactable.downloadDataCSV()` on the table's element id, so the download
#' never round-trips the server. Pairs with a `renderReactable()` output
#' whose widget id equals `table_id` (the id of the matching
#' `reactableOutput()`).
#'
#' @param table_id Namespaced id of the `reactableOutput()` this button
#'   downloads from.
#' @param filename Base name of the downloaded file, without extension.
#'
#' @return A `button` tag styled as `.wise-csv-btn` (custom.css).
#' @noRd
wise_reactable_csv_button <- function(table_id, filename) {
  shiny::tags$button(
    type = "button",
    class = "wise-csv-btn",
    onclick = sprintf(
      "Reactable.downloadDataCSV(%s, %s)",
      jsonlite::toJSON(table_id, auto_unbox = TRUE),
      jsonlite::toJSON(paste0(filename, ".csv"), auto_unbox = TRUE)
    ),
    shiny::tagList(shiny::icon("download"), "Download CSV")
  )
}

#' `downloadHandler` writing a data frame to CSV
#'
#' @param filename_base Base name of the file, without extension.
#' @param data_fun      Function of no arguments returning a data frame, or
#'   NULL when there is nothing to export.
#' @param stale         Optional reactive stale flag. Stale downloads fail
#'   before writing a file.
#'
#' @return A shiny download handler.
#' @noRd
csv_download_handler <- function(filename_base, data_fun, stale = NULL) {
  shiny::downloadHandler(
    filename = function() {
      paste0(filename_base, "_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      if (is.function(stale) && isTRUE(shiny::isolate(stale()))) {
        if (file.exists(file)) unlink(file)
        stop("Results are stale; rerun before downloading.", call. = FALSE)
      }
      df <- tryCatch(data_fun(), error = function(e) NULL)
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        df <- data.frame(Note = "No data available")
      }
      utils::write.csv(.export_flatten_df(df), file, row.names = FALSE, na = "")
    },
    contentType = "text/csv"
  )
}

# Run signatures & stale-state marking (INT-08) ----

#' Canonicalise a value for identity comparison in a run signature.
#'
#' Data frames become lists of columns and nested lists recurse, so two
#' independently-built signature components holding equal data compare
#' `identical()` even when captured at different times.
#' @noRd
.sig_plain <- function(x) {
  if (is.null(x) || is.atomic(x)) {
    return(x)
  }
  if (is.data.frame(x)) {
    return(lapply(x, .sig_plain))
  }
  if (is.list(x)) {
    return(lapply(x, .sig_plain))
  }
  as.character(x)
}

#' Stale-results banner (INT-08).
#'
#' Rendered above a result surface while its run signature no longer matches
#' the current upstream inputs. Results stay visible (they are expensive) but
#' are explicitly labelled as describing an earlier configuration.
#'
#' @param step        Optional label for the affected surface.
#' @param note        Optional extra sentence.
#' @noRd
.stale_banner <- function(step = NULL, note = NULL) {
  step_txt <- if (!is.null(step)) paste0(" (", step, ")") else NULL
  note_txt <- if (!is.null(note)) paste0(" ", note) else NULL
  shiny::div(
    class = "alert alert-warning",
    role = "alert",
    style = "margin-bottom: 10px;",
    shiny::tags$b("\u26a0 Results are out of date", step_txt, "."),
    "Inputs changed after this run. Re-run this step before interpreting or",
    "exporting these results.", note_txt
  )
}

# Dynamic-input selection restore (INT-01) ----

#' Build a label-lookup function bound to a fixed variable-metadata frame
#' (INT-05). Result renderers pass this instead of looking labels up in the
#' live `variable_list`, so re-labelling a variable cannot rewrite the
#' description of an already-fitted result.
#'
#' @param vl Data frame (or NULL) with `name` and `label` columns.
#' @return `function(var_name) -> label`.
#' @noRd
.label_lookup <- function(vl) {
  force(vl)
  lookup <- function(var_name) {
    if (is.null(vl)) {
      return(var_name)
    }
    idx <- match(var_name, vl$name)
    if (is.na(idx)) var_name else as.character(vl$label[idx])
  }
  function(var_name) {
    # Polynomial terms arrive as fixest's double-wrapped "I(I(x^2))" (or the
    # plain "I(x^2)") - render as "<label of x>²/³" instead of raw syntax.
    m <- regmatches(
      var_name,
      regexec(
        "^I\\((?:I\\()?([^\\^]+)\\^([23])\\)\\)?$",
        var_name
      )
    )[[1]]
    if (length(m) == 3) {
      base <- lookup(m[2])
      return(paste0(base, if (m[3] == "2") "\u00b2" else "\u00b3"))
    }
    lookup(var_name)
  }
}

#' Compute `selected` for a re-rendered dynamic input.
#'
#' renderUI-hosted inputs are rebuilt from scratch whenever their choice set
#' changes, which resets them to hardcoded defaults and silently wipes user
#' selections (INT-01). Call this inside the renderUI, passing the previous
#' selection read with `shiny::isolate(input$...)`; values that still exist
#' in the new choice set are restored, invalid values are dropped, and an
#' empty result falls back to the historical default.
#'
#' @param prev     Previous selection (character or NULL on first render).
#' @param choices  Valid choice values (coerced to character).
#' @param fallback Default used when no previous value survives. May be NULL.
#' @return Character vector (unnamed) or `fallback`.
#' @noRd
.restore_selection <- function(prev, choices, fallback) {
  choices <- as.character(choices)
  if (is.null(prev) || length(prev) == 0) {
    return(fallback)
  }
  keep <- unname(as.character(prev)[as.character(prev) %in% choices])
  if (length(keep) == 0) fallback else keep
}

#' Restore a numeric input's previous value across re-renders (INT-01).
#'
#' Numeric companion to `.restore_selection()`: returns `prev` when it is a
#' single finite value inside `[min - tol, max + tol]`, else `fallback`.
#' @noRd
.restore_numeric <- function(prev, min, max, fallback, tol = 1e-8) {
  if (is.null(prev) || length(prev) != 1L || !is.finite(prev)) {
    return(fallback)
  }
  if (prev < (min - tol) || prev > (max + tol)) {
    return(fallback)
  }
  prev
}

# Busy guards (REACT-02) ----
# Long actions (data loads, model fits, simulations) use a module-local
# `running` reactiveVal so double-clicks cannot re-enter the observer while
# the previous run is still executing. Triggering controls are disabled for
# the duration via shinyjs-style input attribute updates (no shinyjs dep).

#' Create a module-local busy guard.
#'
#' Usage inside moduleServer:
#'   guard <- .busy_guard(session)
#'   observeEvent(input$run, {
#'     guard$begin(); on.exit(guard$end(), add = TRUE)
#'     ...
#'   })
#' `busy()` is the reactiveVal backing the guard (FALSE = idle). The trigger
#' buttons are disabled/enabled via shiny::updateActionButton whenever the
#' state flips.
#'
#' @param session  Module session.
#' @param ...      Named input ids of action buttons to disable while running
#'   (unquoted, evaluated via updateActionButton in the module namespace).
#' @noRd
.busy_guard <- function(session, ...) {
  running <- shiny::reactiveVal(FALSE)
  btn_ids <- vapply(substitute(list(...))[-1L], as.character, character(1))

  enable_disable <- function(state) {
    for (btn in btn_ids) {
      tryCatch(
        shinyjs_disable_button(session$ns(btn), enabled = !state),
        error = function(e) NULL
      )
    }
  }
  # Buttons may not exist yet when the guard flips (renderUI-hosted) - the
  # tryCatch above swallows that; re-apply on each transition.
  shiny::observeEvent(running(), enable_disable(running()), ignoreInit = TRUE)

  list(
    is_running = running,
    begin = function() {
      if (isTRUE(running())) {
        return(FALSE)
      }
      running(TRUE)
      TRUE
    },
    end = function() running(FALSE)
  )
}

#' Toggle an action button's disabled state without shinyjs.
#' Sets the disabled attribute server-side; Shiny propagates it to the DOM.
#' @noRd
shinyjs_disable_button <- function(input_id, enabled = TRUE) {
  shiny::updateActionButton(
    session = shiny::getDefaultReactiveDomain(),
    inputId = input_id,
    disabled = !enabled
  )
}

# Segmented radio controls ----

pill_toggle <- function(
  inputId,
  choices = NULL,
  selected = NULL,
  label = NULL,
  width = NULL,
  choiceNames = NULL,
  choiceValues = NULL,
  extra_class = NULL,
  layout = c("horizontal", "vertical")
) {
  layout <- match.arg(layout)
  if (is.null(choiceNames) && is.null(selected) && length(choices) > 0) {
    selected <- unname(choices)[1]
  }

  args <- list(
    inputId = inputId,
    label = label,
    selected = selected,
    inline = TRUE,
    width = width
  )
  if (!is.null(choiceNames)) {
    args$choiceNames <- choiceNames
    args$choiceValues <- choiceValues
  } else {
    args$choices <- choices
  }

  rb <- do.call(shiny::radioButtons, args)
  classes <- c(
    "toggle-slider",
    "pill-toggle",
    if (identical(layout, "vertical")) "pill-toggle-vertical",
    extra_class
  )
  htmltools::tagAppendAttributes(
    rb,
    class = paste(classes[!is.na(classes) & nzchar(classes)], collapse = " ")
  )
}

# Wave / Survey Year Toggle Slider ----

#' Survey year / wave toggle-style slider input
#'
#' Renders an inline segmented toggle-style slider for selecting survey years or waves.
#' Handles few or many survey years gracefully with compact styling and horizontal
#' overflow scrolling.
#'
#' @param inputId Input ID.
#' @param choices Named character vector of choices (values = wave keys,
#'   names = display labels).
#' @param selected Currently selected value.
#' @param label Optional control label.
#' @param width Optional width.
#'
#' @noRd
wave_toggle_slider <- function(inputId, choices, selected = NULL, label = NULL, width = NULL) {
  pill_toggle(
    inputId = inputId,
    choices = choices,
    selected = selected,
    label = label,
    width = width,
    extra_class = "wave-toggle-slider"
  )
}

#' Build choices vector for survey wave toggle slider
#'
#' Formats wave choices: "All" (if include_all = TRUE) followed by
#' survey years. When multiple economies are selected, prefixes with the country
#' code (e.g. "MWI 2010", "TZA 2012"); when only one economy is selected,
#' uses the year directly (e.g. "2010", "2013", "2016").
#'
#' @param wave_df Data frame from `survey_wave_list()` with columns `key`, `year`,
#'   `code`, `economy`.
#' @param include_all Logical; whether to include an "All" choice at the start.
#'
#' @return Named character vector for use in `wave_toggle_slider()`.
#' @noRd
wave_slider_choices <- function(wave_df, include_all = TRUE) {
  if (is.null(wave_df) || nrow(wave_df) == 0) {
    return(character(0))
  }
  multi_country <- length(unique(wave_df$code)) > 1
  labels <- if (multi_country) {
    paste(wave_df$code, wave_df$year)
  } else {
    as.character(wave_df$year)
  }
  wave_choices <- stats::setNames(wave_df$key, labels)
  if (isTRUE(include_all)) {
    c(stats::setNames("all", "All"), wave_choices)
  } else {
    wave_choices
  }
}

#' Return wave labels shared by map controls and charts
#'
#' @param wave_df Data frame from `survey_wave_list()`.
#' @return A named character vector keyed by `wave_df$label`.
#' @noRd
wave_plot_labels <- function(wave_df) {
  if (is.null(wave_df) || nrow(wave_df) == 0) {
    return(character(0))
  }
  choices <- wave_slider_choices(wave_df, include_all = FALSE)
  chart_keys <- paste0(wave_df$economy, ", ", wave_df$year)
  stats::setNames(names(choices), chart_keys)
}
