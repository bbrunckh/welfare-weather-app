#' 2_02_results UI Function
#'
#' @description A shiny Module. Renders the Results tab content: point-range
#'   chart, threshold table/bar, and exceedance curve. Consolidates logic from
#'   the former mod_2_02_historical_sim (tab insertion) and
#'   mod_2_06_sim_compare (all visualisations).
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_2_02_results_ui <- function(id) {
  # Placeholder - the real content is injected via insertUI in the server.
  tagList()
}


#' Results tab content UI (inserted into the Results tabPanel once).
#' @noRd
.results_content_ui <- function(ns, so, weather_var = NULL) {
  so_name <- if (!is.null(so) && "name" %in% names(so) && !is.null(so[["name"]])) as.character(so[["name"]][1]) else "welfare"
  so_type <- if (!is.null(so) && "type" %in% names(so) && !is.null(so[["type"]])) as.character(so[["type"]][1]) else "numeric"
  so_label <- if (!is.null(so) && "label" %in% names(so) && !is.null(so[["label"]])) as.character(so[["label"]][1]) else so_name
  so_level <- if (!is.null(so) && "level" %in% names(so) && !is.null(so[["level"]])) as.character(so[["level"]][1]) else ""

  outcome_lbl <- tolower(so_label)
  unit_lbl <- switch(tolower(so_level),
    ind  = "individuals",
    firm = "firms",
    "households"
  )
  panel_title <- paste0("How to summarise ", outcome_lbl, " across ", unit_lbl, "?")

  wx_phrase <- format_weather_heading_phrase(weather_var)
  sec1_heading <- if (nzchar(wx_phrase)) {
    paste0("How is ", outcome_lbl, " predicted to vary with ", wx_phrase, " across climate scenarios?")
  } else {
    paste0("How is ", outcome_lbl, " predicted to vary across climate scenarios and weather years?")
  }

  agg_choices <- hist_aggregate_choices(so_type, so_name)

  pov_units <- if (!is.null(so) && "units" %in% names(so) && !is.null(so[["units"]]) && nzchar(as.character(so[["units"]][1]))) {
    as.character(so[["units"]][1])
  } else {
    "$/day, 2021 PPP"
  }
  pov_val <- if (!is.null(so) && "povline" %in% names(so) && !is.null(so[["povline"]]) && is.finite(so[["povline"]][1]) && so[["povline"]][1] > 0) {
    so[["povline"]][1]
  } else {
    3.00
  }

  tagList(
    # 0. Stale banner (INT-08) ----
    shiny::uiOutput(ns("stale_banner")),
    shiny::uiOutput(ns("simulation_summary_ui")),

    # 1. Analysis controls: Aggregation method & poverty line ----
    shiny::div(
      class = "results-aggregation-panel",
      shiny::div(
        class = "results-aggregation-head",
        style = "margin-bottom: 8px;",
        shiny::h5(
          panel_title,
          info_popover(
            title = "Aggregation method",
            shiny::p(
              "Choose how household-level welfare is aggregated into an annual",
              "population outcome for each simulated weather year and climate model.",
              "Poverty and prosperity metrics evaluate outcomes relative to the",
              "specified poverty line."
            )
          ),
          style = "font-size: 0.92rem; font-weight: 700; color: #173042; margin: 0;"
        )
      ),
      shiny::div(
        style = "display: flex; align-items: center; gap: 14px; flex-wrap: wrap;",
        pill_toggle(
          inputId  = ns("cmp_agg_method"),
          label    = NULL,
          choices  = agg_choices,
          selected = "mean",
          layout   = "horizontal"
        ),
        pill_toggle(
          inputId = ns("cmp_deviation"),
          label = NULL,
          choices = c(
            "Outcome level"                 = "none",
            "Change from historical mean"   = "mean",
            "Change from historical median" = "median"
          ),
          selected = "none",
          layout = "horizontal"
        ),
        shiny::conditionalPanel(
          condition = paste0(
            "['headcount_ratio','gap','fgt2','prosperity_gap']",
            ".indexOf(input['", ns("cmp_agg_method"), "']) > -1"
          ),
          shiny::div(
            style = "display: flex; align-items: center; gap: 6px;",
            shiny::tags$label(
              `for` = ns("pov_line"),
              style = "font-size: 0.8rem; font-weight: 600; color: #526575; margin: 0; white-space: nowrap;",
              paste0("Poverty line (", pov_units, "):")
            ),
            shiny::numericInput(
              ns("pov_line"),
              label = NULL,
              value = pov_val,
              min   = 0,
              step  = 0.5,
              width = "105px"
            )
          )
        )
      )
    ),

    # 2. Headline cards ----
    shiny::uiOutput(ns("headline_cards_ui")),
    # Section 1: Central outcomes & weather-year variation ----
    shiny::h4(
      sec1_heading,
      info_popover(
        title = "Annual weather variation",
        shiny::p(
          "Each dot represents the population aggregate outcome under one simulated",
          "weather year. The box and violin illustrate the full range of annual",
          "weather-year variation for the fixed baseline population under each",
          "climate regime."
        ),
        shiny::p(
          "The dashed vertical line marks the historical baseline mean. The white circle",
          "shows the central expected outcome."
        ),
        docs = TRUE
      ),
      style = "font-size: 1.05rem; font-weight: 700; color: #173042; margin-top: 24px; margin-bottom: 8px;"
    ),
    shiny::div(
      class = "results-section-card",
      shiny::div(
        style = "display: flex; justify-content: flex-end; align-items: center; margin-bottom: 8px;",
        pill_toggle(
          ns("annual_distribution_type"),
          label    = NULL,
          choices  = c("Violin" = "violin", "Boxplot" = "boxplot"),
          selected = "violin",
          layout   = "horizontal"
        )
      ),
      wise_chart_output(
        ns("annual_distribution_plot"),
        "Distribution of annual aggregates across simulated weather years by climate scenario",
        height = "470px"
      ),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 18px; margin-bottom: 0;",
        "Each dot is one simulated weather-year annual aggregate for the fixed baseline population. The selected violin or boxplot summarizes the distribution; open circles mark scenario means. This captures weather-year variability, not household inequality."
      )
    ),

    # Section 2: Adverse weather years (tail risk) ----
    shiny::h4(
      "What outcomes are predicted in adverse weather years?",
      info_popover(
        title = "Adverse weather-year risk",
        shiny::p(
          "Adverse return-period outcomes represent severe annual weather conditions.",
          "An adverse 1-in-10-year outcome is reached or exceeded in the unfavorable",
          "direction in approximately one out of ten simulated weather years."
        ),
        shiny::p(
          "Points show expected outcomes and adverse-year thresholds; intervals",
          "show disagreement across CMIP6 climate models (ensemble spread)."
        ),
        docs = TRUE
      ),
      style = "font-size: 1.05rem; font-weight: 700; color: #173042; margin-top: 24px; margin-bottom: 8px;"
    ),
    shiny::div(
      class = "results-section-card",
      shiny::div(
        style = "display: flex; justify-content: flex-end; align-items: center; margin-bottom: 8px;",
        pill_toggle(
          ns("ensemble_band"),
          label = "Climate model spread",
          choices = c(
            "None" = "none",
            "Full ensemble spread" = "minmax",
            "95%" = "p025_p975",
            "90%" = "p05_p95",
            "80%" = "p10_p90"
          ),
          selected = "none",
          layout = "horizontal"
        )
      ),
      wise_chart_output(
        ns("adverse_dot_plot"),
        "Expected and adverse-year outcomes with climate-model ensemble spread",
        height = "380px"
      ),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 8px; margin-bottom: 0;",
        "Adverse tail direction is mapped automatically according to the selected outcome metric. Horizontal bars show inter-model ensemble spread across climate projections."
      )
    ),

    # Section 3: Exceedance probability curves ----
    shiny::h4(
      "What is the probability of severe outcomes occurring?",
      info_popover(
        title = "Exceedance probability",
        shiny::p(
          "Shows the annual probability of reaching or exceeding severe outcome",
          "thresholds across simulated weather years under each climate regime.",
          "The probability axis labels standard return periods (e.g. 1 in 10 or 1 in 20 year events)."
        ),
        shiny::p(
          "The navy curve is the historical baseline. Coloured curves show the",
          "ensemble median across climate models, with shaded ribbons indicating inter-model spread."
        ),
        docs = TRUE
      ),
      style = "font-size: 1.05rem; font-weight: 700; color: #173042; margin-top: 24px; margin-bottom: 8px;"
    ),
    shiny::div(
      class = "results-section-card",
      shiny::div(
        style = "display: flex; justify-content: flex-end; align-items: center; margin-bottom: 8px;",
        pill_toggle(
          inputId = ns("exceedance_model_spread"),
          label = "Climate model spread",
          choices = c(
            "None"                 = "none",
            "Full ensemble spread" = "minmax",
            "95%"                  = "p025_p975",
            "90%"                  = "p05_p95",
            "80%"                  = "p10_p90"
          ),
          selected = "none",
          layout = "horizontal"
        )
      ),
      wise_chart_output(
        ns("exceedance_plot"),
        "Exceedance probability curves across simulated climate scenarios",
        height = "400px"
      ),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 8px; margin-bottom: 0;",
        "Read each curve as the annual probability of reaching an outcome level in the adverse direction: lower outcomes for higher-is-better measures and higher outcomes for lower-is-better measures. Coloured lines show the across-model median; shaded ribbons show climate-model disagreement for each future baseline and policy series. Return-period guides and ticks are limited to the available simulated years per climate model; unsupported periods are not extrapolated."
      )
    ),

    # Section 4: Decision & return-period table ----
    shiny::h4(
      "Detailed return-period outcomes and uncertainty",
      info_popover(
        title = "Return-period decision table",
        shiny::p(
          "Summary of central expected outcomes and severe weather-year thresholds",
          "for each scenario across return periods, with climate model and econometric uncertainty bounds."
        ),
        docs = TRUE
      ),
      style = "font-size: 1.05rem; font-weight: 700; color: #173042; margin-top: 24px; margin-bottom: 8px;"
    ),
    shiny::div(
      class = "results-section-card",
      shiny::div(
        style = "display: flex; justify-content: flex-end; align-items: center; margin-bottom: 8px;",
        wise_reactable_csv_button(
          ns("summary_threshold_table"),
          "climate_outcome_thresholds"
        )
      ),
      reactable::reactableOutput(ns("summary_threshold_table")),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 8px; margin-bottom: 0;",
        "Central estimates show median outcomes across weather years and climate models. Bounds capture CMIP6 climate model disagreement (Ensemble), econometric sampling precision (Coef), and combined uncertainty (Pooled)."
      )
    ),

    # Section 5: Uncertainty decomposition ----
    shiny::h4(
      "What drives the uncertainty in these predictions?",
      info_popover(
        title = "Uncertainty sources",
        shiny::p(
          "Compares the absolute standard deviation contributed by each distinct source:",
          "year-to-year weather variability, climate-model disagreement, and model-estimation uncertainty."
        ),
        shiny::p(
          "Coefficient uncertainty is the analytic delta-method standard error from the fitted model's coefficient covariance matrix",
          "(plus residual-draw variance when stochastic residuals are enabled). It is shown as one standard deviation, not a 95% confidence interval;",
          "an approximate normal 95% interval would be estimate +/- 1.96 times this value."
        ),
        shiny::p(
          "Because standard deviations are not additive, bars are displayed side-by-side on a common scale."
        ),
        docs = TRUE
      ),
      style = "font-size: 1.05rem; font-weight: 700; color: #173042; margin-top: 24px; margin-bottom: 8px;"
    ),
    shiny::div(
      class = "results-section-card",
      wise_chart_output(
        ns("uncertainty_sources_plot"),
        "Standard deviation of outcome by uncertainty source",
        height = "300px"
      ),
      shiny::tags$p(
        class = "text-muted small",
        style = "margin-top: 8px; margin-bottom: 0;",
        "Bars are standard deviations in outcome units, not confidence intervals. Inter-annual variability is the within-model standard deviation across simulated weather years; inter-model spread is the standard deviation of model means across climate models; coefficient uncertainty is the analytic delta-method SE from the fitted model covariance (plus any enabled stochastic residual variance)."
      )
    )
  )
}


#' 2_02_results Server Functions
#'
#' Appends a Results tab to the main tabset once the historical simulation
#' has run. All comparison outputs update reactively as saved_scenarios change.
#'
#' @param id              Module id.
#' @param hist_sim        ReactiveVal. Named list with slots:
#'   \code{$preds} (full prediction data frame), \code{$agg} (pre-aggregated
#'   summary by method x weighted x deviation x sim_year), \code{$so}
#'   (selected outcome metadata), \code{$pov_line} (simulation-time poverty
#'   line), \code{$has_weights} (logical weight flag), \code{$weather_raw},
#'   \code{$train_data}, \code{$n_pre_join}.
#' @param saved_scenarios ReactiveVal holding named scenario entries.
#' @param selected_hist   Reactive one-row data frame from weathersim.
#' @param selected_weather Reactive data frame of selected weather variables.
#' @param tabset_id       Character id of the parent tabset panel.
#' @param tabset_session  Shiny session for the tabset.
#'
#' @noRd
mod_2_02_results_server <- function(id,
                                    hist_sim,
                                    saved_scenarios,
                                    selected_hist,
                                    selected_weather = NULL,
                                    tabset_id,
                                    tabset_session = NULL,
                                     residuals = reactive("original"),
                                     skip_coef_draws = reactive(FALSE),
                                     shared_aggregation_cache = NULL,
                                     stale = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # INT-08: stale banner above the results pane. This surface gates its
    # CSV export while stale.
    output$stale_banner <- renderUI({
      if (isTRUE(stale())) {
        .stale_banner(
          "Step 2 simulation results",
          note = NULL
        )
      } else {
        NULL
      }
    })

    output$simulation_summary_ui <- renderUI({
      simulation_summary_card(
        hist_sim = hist_sim(),
        saved_scenarios = saved_scenarios(),
        selected_hist = if (!is.null(selected_hist)) selected_hist() else NULL,
        selected_weather = if (is.function(selected_weather)) selected_weather() else selected_weather
      )
    })

    headline_cards_data_rv <- reactive({
      req(pointrange_bands_rv())
      bands <- pointrange_bands_rv()
      if (!nrow(bands) ||
        !all(c("is_historical", "scenario") %in% names(bands))) {
        return(NULL)
      }
      step2_headline_cards(
        bands = bands,
        threshold_tbl = tryCatch(threshold_table_rv(), error = function(e) NULL),
        hist_sim = tryCatch(hist_sim(), error = function(e) NULL),
        saved_scenarios = tryCatch(if (!is.null(saved_scenarios)) saved_scenarios() else list(),
          error = function(e) list()
        ),
        method = input$cmp_agg_method %||% "mean",
        timeseries_curves = tryCatch(timeseries_curves_rv(), error = function(e) NULL)
      )
    })

    output$headline_cards_ui <- renderUI({
      cards <- headline_cards_data_rv()
      if (is.null(cards)) {
        return(NULL)
      }
      headline_cards_ui(cards)
    })

    # Lazy delta-method aggregation ----
    # Replaces the eager compute_hist_agg / compute_scenario_agg path. Returns
    # the same nested list shape (weighted/unweighted -> method -> tibble) so
    # downstream consumers in fct_sim_compare.R see a compatible schema.
    agg_methods <- reactive({
      req(hist_sim())
      so <- hist_sim()$so
      unname(hist_aggregate_choices(so$type, so$name))
    })

    # pov_line is always supplied (the aggregation pre-computes every method
    # per year, not just the currently selected one). Non-poverty methods
    # ignore it; poverty methods need it. Default 3.00 USD/day if the input
    # hasn't been initialised yet.
    pov_line_val <- debounce(reactive({
      as.numeric(input$pov_line %||% 3.00)
    }), 400)

    bandwidth_p0 <- reactive({
      as.numeric(input$bandwidth_p0 %||% 0.05)
    })

    # Value-affecting aggregation inputs ----
    # The aggregation cache is keyed by the inputs each method actually
    # consumes (PERF-30), so moving the coefficient-band or poverty-line
    # control only invalidates the methods that read it. band_q is display-
    # only: aggregate_with_uncertainty_delta() applies it to value_lo/hi,
    # which no builder below consumes - the band is re-derived from the
    # cached SDs at render time. Display uses a fixed neutral pair.
    AGG_BAND_Q <- c(lo = 0.10, hi = 0.90)
    .POV_LINE_METHODS <- c("headcount_ratio", "gap", "fgt2")
    .BANDWIDTH_METHODS <- "headcount_ratio"

    # Aggregation workspace + per-method cache ----
    # Captures the heavy dependencies that invalidate every cached method
    # (hist_sim, saved scenarios, residuals, coef-draw skipping) into a
    # workspace that's recreated whenever any of them changes. The workspace
    # carries a mutable cache so we only compute each aggregation method once
    # per workspace version. The Results tab reads only the currently selected
    # method, and an eager observer pre-computes the default ("mean") as soon
    # as hist_sim() arrives - so the first render is fast even before the
    # user clicks anything.
    #
    # Display-only controls (coefficient band, poverty line, headcount
    # bandwidth) are deliberately NOT workspace dependencies: changing them
    # used to destroy the whole cache. Instead they are read at cache-lookup
    # time and folded into the per-method cache key for exactly the methods
    # that consume them (see .pl_bw_key).
    cache_workspace_ref <- new.env(parent = emptyenv())
    cache_workspace_ref$current <- NULL

    .clear_aggregation_cache <- function(ws = cache_workspace_ref$current) {
      if (is.null(ws)) {
        return(invisible(NULL))
      }
      if (is.environment(ws$cache)) {
        cached <- ls(ws$cache, all.names = TRUE)
        if (length(cached)) rm(list = cached, envir = ws$cache)
      }
      if (is.environment(ws$cache_order)) {
        ws$cache_order$keys <- character(0)
        ws$cache_order$hits <- 0L
        ws$cache_order$misses <- 0L
        ws$cache_order$evictions <- character(0)
      }
      if (is.environment(ws$prep_cache)) {
        ws$prep_cache$entries <- list()
        ws$prep_cache$keys <- character(0)
      }
      if (is.environment(ws$weighted_suite_cache)) {
        cached_suites <- ls(ws$weighted_suite_cache, all.names = TRUE)
        if (length(cached_suites)) {
          rm(list = cached_suites, envir = ws$weighted_suite_cache)
        }
      }
      if (identical(ws, cache_workspace_ref$current)) {
        cache_workspace_ref$current <- NULL
      }
      invisible(NULL)
    }

    .cache_touch <- function(ws, key) {
      order <- ws$cache_order
      order$keys <- c(setdiff(order$keys, key), key)
      invisible(NULL)
    }

    .cache_get <- function(ws, key) {
      if (!exists(key, envir = ws$cache, inherits = FALSE)) {
        ws$cache_order$misses <- ws$cache_order$misses + 1L
        return(list(found = FALSE, value = NULL))
      }
      ws$cache_order$hits <- ws$cache_order$hits + 1L
      .cache_touch(ws, key)
      list(found = TRUE, value = get(key, envir = ws$cache, inherits = FALSE))
    }

    .cache_put <- function(ws, key, value) {
      assign(key, value, envir = ws$cache)
      .cache_touch(ws, key)
      while (length(ws$cache_order$keys) > ws$cache_order$max_entries) {
        old <- ws$cache_order$keys[[1L]]
        ws$cache_order$keys <- ws$cache_order$keys[-1L]
        if (exists(old, envir = ws$cache, inherits = FALSE)) {
          rm(list = old, envir = ws$cache)
          ws$cache_order$evictions <- c(ws$cache_order$evictions, old)
        }
      }
      invisible(value)
    }

    .cache_snapshot <- function(ws = cache_workspace_ref$current) {
      if (is.null(ws)) {
        return(list(
          keys = character(0), n_entries = 0L, max_entries = 0L,
          hits = 0L, misses = 0L, evictions = character(0),
          object_bytes = 0, serialized_bytes = 0,
          entry_object_bytes = numeric(0)
        ))
      }
      keys <- ws$cache_order$keys
      values <- if (length(keys)) {
        lapply(keys, get, envir = ws$cache, inherits = FALSE)
      } else {
        list()
      }
      object_bytes <- if (length(values)) {
        sum(vapply(values, function(x) as.numeric(utils::object.size(x)), numeric(1)))
      } else {
        0
      }
      serialized_bytes <- tryCatch(
        length(serialize(values, NULL, version = 3L)),
        error = function(e) NA_real_
      )
      list(
        keys = keys,
        n_entries = length(keys),
        max_entries = ws$cache_order$max_entries,
        hits = ws$cache_order$hits,
        misses = ws$cache_order$misses,
        evictions = ws$cache_order$evictions,
        object_bytes = object_bytes,
        serialized_bytes = serialized_bytes,
        entry_object_bytes = if (length(values)) {
          stats::setNames(
            vapply(values, function(x) as.numeric(utils::object.size(x)), numeric(1)),
            keys
          )
        } else {
          numeric(0)
        }
      )
    }
    aggregation_cache <- function() {
      published <- tryCatch(shiny::isolate(hist_sim()), error = function(e) NULL)
      if (is.null(published)) {
        return(.cache_snapshot(NULL))
      }
      .cache_snapshot()
    }

    agg_workspace <- reactive({
      req(hist_sim())
      .clear_aggregation_cache()
      cache_order <- new.env(parent = emptyenv())
      cache_order$keys <- character(0)
      cache_order$max_entries <- 8L
      cache_order$hits <- 0L
      cache_order$misses <- 0L
      cache_order$evictions <- character(0)
       ws <- list(
        hs = hist_sim(),
        sc = saved_scenarios(),
        res = hist_sim()$residuals %||% residuals() %||% "original",
        skip = isTRUE(skip_coef_draws()),
        cache = new.env(parent = emptyenv()),
         cache_order = cache_order,
         prep_cache = .new_aggregation_preparation_cache(max_entries = 32L),
         weighted_suite_cache = new.env(parent = emptyenv())
       )
      cache_workspace_ref$current <- ws
      ws
    })

    session$onSessionEnded(function() .clear_aggregation_cache())
    observeEvent(hist_sim(),
      {
        if (is.null(hist_sim())) .clear_aggregation_cache()
      },
      ignoreInit = FALSE
    )

    # Cache-key suffix for the poverty line / bandwidth values a method reads.
    # Methods that ignore them get a constant key so moving the poverty-line
    # slider does not force their recomputation.
    .pl_bw_key <- function(method, pl_v, bw) {
      parts <- character(0)
      if (method %in% .POV_LINE_METHODS) parts <- c(parts, format(pl_v))
      if (method %in% .BANDWIDTH_METHODS) parts <- c(parts, format(bw))
      if (length(parts) == 0L) "" else paste0("_", paste(parts, collapse = "_"))
    }

    .new_lazy_aggregation_method_list <- function(builder, method) {
      state <- new.env(parent = emptyenv())
      state$builder <- builder
      state$value <- NULL
      state$built <- FALSE
      structure(
        setNames(list(NULL), method),
        class = c("wise_lazy_aggregation_method_list", "list"),
        state = state
      )
    }

    .build_lazy_aggregation_method <- function(builder, method) {
      .new_lazy_aggregation_method_list(builder, method)
    }

    .force_lazy_aggregation_method <- function(x) {
      if (!inherits(x, "wise_lazy_aggregation_method_list")) {
        return(x)
      }
      state <- attr(x, "state", exact = TRUE)
      if (!isTRUE(state$built)) {
        state$value <- state$builder()
        state$built <- TRUE
      }
      x
    }

    .lazy_aggregation_value <- function(x) {
      if (inherits(x, "wise_lazy_aggregation_method_list")) {
        state <- attr(x, "state", exact = TRUE)
        if (!isTRUE(state$built)) .force_lazy_aggregation_method(x)
        return(state$value)
      }
      x
    }

    .lazy_aggregation_table <- function(x, method) {
      value <- .lazy_aggregation_value(x)
      if (is.list(value) && !is.null(value[[method]])) value[[method]] else value
    }

    `[[.wise_lazy_aggregation_method_list` <- function(x, i, ...) {
      x <- .force_lazy_aggregation_method(x)
      attr(x, "state", exact = TRUE)$value[[i]]
    }

    `$.wise_lazy_aggregation_method_list` <- function(x, name) {
      x <- .force_lazy_aggregation_method(x)
      attr(x, "state", exact = TRUE)$value[[name]]
    }

    names.wise_lazy_aggregation_method_list <- function(x) {
      names(unclass(x))
    }

    length.wise_lazy_aggregation_method_list <- function(x) 1L

     .build_hist_for_method <- function(ws, method, pl_v) {
      pl <- ws$hs$pipeline
      bq <- AGG_BAND_Q
      is_log <- isTRUE(ws$hs$so$transform == "log")
       build_for <- function(weighted) {
          if (isTRUE(weighted) || !isTRUE(has_w)) {
            suite_pov <- pl_v %||% 3
            suite_key <- paste0("hist_suite_", weighted, "_", format(suite_pov), "_", format(bandwidth_p0()))
            shared_key <- shared_aggregation_cache_key(
              ws$hs$.sig %||% list(pipeline = "step2"), suite_pov,
              bandwidth_p0(), weighted, ws$res, ws$skip, is_log, agg_methods()
            )
            suite <- shared_aggregation_cache_get(shared_aggregation_cache, shared_key)
            if (!is.null(suite)) return(setNames(list(suite[[method]]), method))
            suite <- get0(suite_key, envir = ws$weighted_suite_cache)
           if (is.null(suite)) {
             suite <- aggregate_pipeline_tables_multi(
               pipelines = pl,
               methods = agg_methods(),
               weighted = weighted,
                pov_lines = setNames(lapply(agg_methods(), function(x) suite_pov), agg_methods()),
               residuals = ws$res,
               is_log = is_log,
               band_q = bq,
               skip_coef = ws$skip,
               bandwidth_p0 = bandwidth_p0(),
               model_ids = "Historical",
               scenario = "Historical",
               shared_context = ws$hs$shared_context,
               preparation_cache = ws$prep_cache
              )
              assign(suite_key, suite, envir = ws$weighted_suite_cache)
              shared_aggregation_cache_put(shared_aggregation_cache, shared_key, suite)
           }
           out <- suite[[method]]
           return(setNames(list(out), method))
         }
         out <- aggregate_pipeline_table(
          pipelines = pl,
          method = method,
          weighted = weighted,
          pov_line = pl_v,
          residuals = ws$res,
          is_log = is_log,
          band_q = bq,
          skip_coef = ws$skip,
          bandwidth_p0 = bandwidth_p0(),
          model_ids = "Historical",
          scenario = "Historical",
          shared_context = ws$hs$shared_context,
          preparation_cache = ws$prep_cache
         )
        setNames(list(out), method)
       }
       has_w <- !is.null(pl$weight)
      list(
        unweighted = .build_lazy_aggregation_method(
          function() build_for(FALSE), method
        ),
        weighted = .build_lazy_aggregation_method(
          if (has_w) function() build_for(TRUE) else function() build_for(FALSE),
          method
        )
      )
    }

     .build_scn_for_method <- function(ws, method, pl_v) {
      sc <- ws$sc
      if (length(sc) == 0L) {
        return(NULL)
      }
      bq <- AGG_BAND_Q
      setNames(lapply(seq_along(sc), function(s_idx) {
        s <- sc[[s_idx]]
        pipes <- s$pipelines
        is_log <- isTRUE(s$so$transform == "log")
        has_w <- !is.null(pipes[[1L]]$weight)
         build_for <- function(weighted) {
           if (isTRUE(weighted) || !isTRUE(has_w)) {
             suite_key <- paste0("scenario_suite_", names(sc)[[s_idx]], "_", format(pl_v), "_", format(bandwidth_p0()))
             suite <- get0(suite_key, envir = ws$weighted_suite_cache)
             if (is.null(suite)) {
               suite <- aggregate_pipeline_tables_multi(
                 pipelines = pipes,
                 methods = agg_methods(),
                 weighted = weighted,
                 pov_lines = setNames(lapply(agg_methods(), function(x) pl_v %||% 3), agg_methods()),
                 residuals = ws$res,
                 is_log = is_log,
                 band_q = bq,
                 skip_coef = ws$skip,
                 bandwidth_p0 = bandwidth_p0(),
                 model_ids = names(pipes) %||% paste0("m", seq_along(pipes)),
                 shared_context = s$shared_context,
                 preparation_cache = ws$prep_cache
               )
               assign(suite_key, suite, envir = ws$weighted_suite_cache)
             }
             out <- suite[[method]]
             return(setNames(list(out), method))
           }
           out <- aggregate_pipeline_table(
            pipelines = pipes,
            method = method,
            weighted = weighted,
            pov_line = pl_v,
            residuals = ws$res,
            is_log = is_log,
            band_q = bq,
            skip_coef = ws$skip,
            bandwidth_p0 = bandwidth_p0(),
            model_ids = names(pipes) %||% paste0("m", seq_along(pipes)),
            shared_context = s$shared_context,
            preparation_cache = ws$prep_cache
          )
          setNames(list(out), method)
        }
        list(
          unweighted = .build_lazy_aggregation_method(
            function() build_for(FALSE), method
          ),
          weighted = .build_lazy_aggregation_method(
            if (has_w) function() build_for(TRUE) else function() build_for(FALSE),
            method
          )
        )
      }), names(sc))
    }

    .get_hist_agg <- function(method) {
      ws <- agg_workspace()
      pl_v <- pov_line_val()
      bw <- bandwidth_p0()
      key <- paste0("h_", method, .pl_bw_key(method, pl_v, bw))
      cached <- .cache_get(ws, key)
      if (!isTRUE(cached$found)) {
        value <- .build_hist_for_method(ws, method, pl_v)
        .cache_put(ws, key, value)
      } else {
        value <- cached$value
      }
      value
    }

    .get_scn_agg <- function(method) {
      ws <- agg_workspace()
      pl_v <- pov_line_val()
      bw <- bandwidth_p0()
      key <- paste0("s_", method, .pl_bw_key(method, pl_v, bw))
      cached <- .cache_get(ws, key)
      if (!isTRUE(cached$found)) {
        value <- .build_scn_for_method(ws, method, pl_v)
        .cache_put(ws, key, value)
      } else {
        value <- cached$value
      }
      value
    }

    hist_agg_rv <- reactive({
      method <- input$cmp_agg_method %||% "mean"
      .get_hist_agg(method)
    })

    scenario_agg_rv <- reactive({
      req(saved_scenarios())
      if (length(saved_scenarios()) == 0L) {
        return(NULL)
      }
      method <- input$cmp_agg_method %||% "mean"
      .get_scn_agg(method)
    })

    # Reactive computations (carried over from mod_2_06) ----

    hist_label <- reactive({
      nm <- if (!is.null(selected_hist)) selected_hist()$scenario_name else NULL
      if (!is.null(nm) && nzchar(nm)) nm else "Historical"
    })


    # Always use survey weights when available (UI toggle removed - weighting
    # is the correct default for survey-based welfare estimates).
    weight_key <- reactive({
      if (!is.null(hist_sim()) && isTRUE(hist_sim()$has_weights)) {
        "weighted"
      } else {
        "unweighted"
      }
    })

    # Shared deviation reference - used by all_series_tbl and exceedance_ribbon
    hist_ref_val <- reactive({
      req(hist_agg_rv())
      method <- input$cmp_agg_method %||% "mean"
      wk <- weight_key()
      deviation <- input$cmp_deviation %||% "none"
      if (identical(deviation, "none")) {
        return(0)
      }
      raw_vals <- hist_agg_rv()[[wk]][[method]]$value
      if (identical(deviation, "mean")) {
        mean(raw_vals, na.rm = TRUE)
      } else {
        stats::median(raw_vals, na.rm = TRUE)
      }
    })

    # Per-coefficient gradient of the historical reference being subtracted.
    # When deviation = mean: average of per-year F_agg across historical years.
    # When deviation = median: F_agg at the historical year closest to the median.
    # Used by .apply_contrast_sd() below to switch coefficient SE from
    # level-CI (||F_s||) to contrast-CI (||F_s - F_ref||), the correct SE for
    # paired counterfactual analysis on the same population.
    hist_F_agg_ref <- reactive({
      req(hist_agg_rv())
      method <- input$cmp_agg_method %||% "mean"
      wk <- weight_key()
      deviation <- input$cmp_deviation %||% "none"
      if (identical(deviation, "none")) {
        return(NULL)
      }
      ht <- hist_agg_rv()[[wk]][[method]]
      if (is.null(ht) || nrow(ht) == 0L || !"F_agg_all" %in% names(ht)) {
        return(NULL)
      }
      # Historical has one "model" so each F_agg_all row is a 1 x K matrix.
      F_list <- lapply(ht$F_agg_all, function(m) {
        if (is.null(m) || !is.matrix(m) || nrow(m) == 0L) {
          NULL
        } else {
          as.numeric(m[1L, ])
        }
      })
      F_list <- Filter(Negate(is.null), F_list)
      if (length(F_list) == 0L) {
        return(NULL)
      }
      if (identical(deviation, "mean")) {
        Reduce(`+`, F_list) / length(F_list)
      } else {
        vals <- ht$value
        med_v <- stats::median(vals, na.rm = TRUE)
        med_idx <- which.min(abs(vals - med_v))
        if (length(med_idx) == 0L) {
          Reduce(`+`, F_list) / length(F_list)
        } else {
          F_list[[med_idx]]
        }
      }
    })

    # Replace per-(model, year) coefficient SDs with paired-contrast SDs
    # when a deviation reference is active. By overwriting `value_all_sd`
    # here, every downstream consumer (pointrange_bands_rv,
    # threshold_table_rv, exceedance_curves_rv) automatically uses the
    # tightened contrast variance.
    .apply_contrast_sd <- function(tbl, F_ref) {
      if (is.null(F_ref) || is.null(tbl) || nrow(tbl) == 0L) {
        return(tbl)
      }
      if (!"F_agg_all" %in% names(tbl) || !"value_all_sd" %in% names(tbl)) {
        return(tbl)
      }
      for (k in seq_len(nrow(tbl))) {
        F_mat <- tbl$F_agg_all[[k]]
        if (is.null(F_mat) || !is.matrix(F_mat) || ncol(F_mat) != length(F_ref)) {
          next
        }
        F_diff <- sweep(F_mat, 2L, F_ref, "-")
        tbl$value_all_sd[[k]] <- sqrt(rowSums(F_diff * F_diff))
      }
      tbl
    }

    # One immutable derived frame per published run/control state. Every
    # Results consumer below resolves its model/year matrix from this frame,
    # avoiding repeated reshaping and preserving identical canonical ordering.
    derived_results_frame_rv <- reactive({
      req(hist_agg_rv())
      wk <- weight_key()
      method <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation %||% "none"
      F_ref <- hist_F_agg_ref()
      entries <- list()
      add_entry <- function(tbl, label, historical) {
        tbl <- .lazy_aggregation_table(tbl, method)
        if (is.null(tbl) || !nrow(tbl)) {
          return(invisible(NULL))
        }
        tbl <- .apply_contrast_sd(tbl, F_ref)
        key <- digest::digest(tbl, serialize = TRUE)
        entry <- new.env(parent = emptyenv())
        entry$table <- tbl
        entry$matrix <- by_model_matrix(tbl)
        entry$scenario <- label
        entry$is_historical <- historical
        entry$key <- key
        lockEnvironment(entry, bindings = TRUE)
        entries[[label]] <<- entry
        invisible(NULL)
      }
      add_entry(hist_agg_rv()[[wk]][[method]], "Historical", TRUE)
      sa <- scenario_agg_rv()
      if (!is.null(sa)) {
        for (label in names(sa)) {
          add_entry(sa[[label]][[wk]][[method]], label, FALSE)
        }
      }
      frame <- new.env(parent = emptyenv())
      frame$method <- method
      frame$deviation <- deviation
      frame$weight <- wk
      frame$.entries <- entries
      frame$.matrices <- stats::setNames(
        lapply(entries, `[[`, "matrix"),
        vapply(entries, `[[`, character(1L), "key")
      )
      lockEnvironment(frame, bindings = TRUE)
      frame
    })


    # Coefficient draws availability ----
    has_draws <- reactive({
      req(hist_sim())
      !is.null(hist_sim()$chol_obj)
    })


    selected_scenario_names <- reactive({
      sc <- saved_scenarios()
      if (length(sc) == 0L) {
        return(character(0))
      }
      names(sc)
    })

    agg_hist <- reactive({
      req(hist_agg_rv())
      method <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation %||% "none"
      out <- hist_agg_rv()[[weight_key()]][[method]]
      req(!is.null(out))
      hist_ref <- hist_ref_val()
      if (!identical(deviation, "none") && nrow(out) > 0) {
        out <- dplyr::mutate(out, value = value - hist_ref)
      }
      x_label <- if (identical(deviation, "none")) {
        label_agg_method(method)
      } else {
        paste0(label_agg_method(method), " \u2014 ", label_deviation(deviation))
      }
      list(out = out, x_label = x_label)
    })

    # `exceedance_ribbon` removed - the ribbon is now built inside
    # enhance_exceedance() directly from each series' (value_all, value_all_sd)
    # using analytic delta-method bands, so there is nothing to precompute here.

    # Three-source uncertainty decomposition ----
    # All three downstream displays (hero, exceedance, table) source their
    # bands from the helpers below. Each helper produces a per-scenario view
    # that decomposes uncertainty into:
    #   - coefficient (per-outcome SE from value_all_sd)
    #   - inter-annual (within-model spread of value_all across years)
    #   - inter-model  (across-model spread of model means; future only)

    # Helpers are now defined in R/fct_uncertainty_helpers.R as package-internal
    # functions so Module 3 can call the same code path. Aliases keep the
    # existing inline call sites below readable.
    .by_model_matrix <- function(tbl) {
      frame <- derived_results_frame_rv()
      key <- digest::digest(tbl, serialize = TRUE)
      hit <- .results_frame_matrix(frame, key)
      if (!is.null(hit)) {
        return(hit)
      }
      by_model_matrix(tbl)
    }
    .pct_label <- pct_label
    .rank_interp <- rank_interp

    # pointrange_bands_rv: one row per scenario, three nested bands ----
    pointrange_bands_rv <- reactive({
      req(hist_agg_rv())
      bq_ens <- if (identical(input$ensemble_band %||% "none", "none")) {
        c(lo = 0.5, hi = 0.5)
      } else {
        resolve_band_q(input$ensemble_band %||% "none")
      }
      hist_ref <- hist_ref_val()
      wk <- weight_key()
      method <- input$cmp_agg_method %||% "mean"

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) {
          return(NULL)
        }
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) {
          return(NULL)
        }
        vals <- mm$vals

        # Inter-model spread: per-model mean across years, then quantile across models.
        model_means <- rowMeans(vals, na.rm = TRUE)
        intermod <- if (is_hist || length(model_means) <= 1L) {
          mean_v <- mean(model_means, na.rm = TRUE)
          c(lo = mean_v, hi = mean_v)
        } else {
          c(
            lo = unname(stats::quantile(model_means, bq_ens[["lo"]], na.rm = TRUE)),
            hi = unname(stats::quantile(model_means, bq_ens[["hi"]], na.rm = TRUE))
          )
        }

        # Inter-annual variability: for each model take the band_q quantile
        # across years, then average across models.
        if (is_hist) {
          v_flat <- as.numeric(vals)
          interann <- c(
            lo = unname(stats::quantile(v_flat, bq_ens[["lo"]], na.rm = TRUE)),
            hi = unname(stats::quantile(v_flat, bq_ens[["hi"]], na.rm = TRUE))
          )
        } else {
          per_mod_lo <- apply(vals, 1L, stats::quantile,
            probs = bq_ens[["lo"]], na.rm = TRUE
          )
          per_mod_hi <- apply(vals, 1L, stats::quantile,
            probs = bq_ens[["hi"]], na.rm = TRUE
          )
          interann <- c(
            lo = mean(per_mod_lo, na.rm = TRUE),
            hi = mean(per_mod_hi, na.rm = TRUE)
          )
        }

        # Coefficient uncertainty: per-outcome SE, centred on ensemble mean.
        # Owner-approved convention: summarise each model across its weather
        # years, then take the median across equally weighted models.
        ens_mean <- if (is_hist) {
          mean(as.numeric(vals), na.rm = TRUE)
        } else {
          stats::median(model_means, na.rm = TRUE)
        }

        tibble::tibble(
          scenario = scenario_label,
          value = ens_mean - hist_ref,
          interann_lo = unname(interann[["lo"]]) - hist_ref,
          interann_hi = unname(interann[["hi"]]) - hist_ref,
          intermod_lo = unname(intermod[["lo"]]) - hist_ref,
          intermod_hi = unname(intermod[["hi"]]) - hist_ref,
          is_historical = is_hist,
          n_models = length(mm$model_ids)
        )
      }

      rows <- list(one_scenario(
        .apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
        "Historical", TRUE
      ))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(
            .apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
            dk, FALSE
          )
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # timeseries_curves_rv: per (scenario, model, sim_year) values ----
    build_timeseries_curves <- function(selected_only = TRUE) {
      req(hist_agg_rv())
      hist_ref <- hist_ref_val()
      wk <- weight_key()
      method <- input$cmp_agg_method %||% "mean"

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) {
          return(NULL)
        }
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) {
          return(NULL)
        }
        vals <- mm$vals
        rows <- lapply(seq_len(nrow(vals)), function(i) {
          tibble::tibble(
            scenario      = scenario_label,
            model_id      = mm$model_ids[[i]],
            sim_year      = as.integer(mm$sim_years),
            value         = vals[i, ] - hist_ref,
            is_historical = is_hist
          )
        })
        dplyr::bind_rows(rows)
      }

      rows <- list(one_scenario(
        .apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
        "Historical", TRUE
      ))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (isTRUE(selected_only) && !dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(
            .apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
            dk, FALSE
          )
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    }

    timeseries_curves_rv <- reactive(build_timeseries_curves(TRUE))
    annual_distribution_curves_rv <- reactive(build_timeseries_curves(FALSE))

    # variance_breakdown_rv: one row per scenario, three components ----
    # Aggregates the per-(sim_year) var_within / var_across columns to scalars
    # and re-computes var_coef from the per-(model, year) SD list-column.
    variance_breakdown_rv <- reactive({
      req(derived_results_frame_rv())
      frame <- derived_results_frame_rv()

      one_scenario <- function(entry) {
        if (is.null(entry)) {
          return(NULL)
        }
        tbl <- entry$table
        scenario_label <- entry$scenario
        is_hist <- entry$is_historical
        sds_flat <- as.numeric(unlist(tbl$value_all_sd))
        var_coef <- if (length(sds_flat)) {
          mean(sds_flat^2, na.rm = TRUE)
        } else {
          0
        }
        # Use value-matrix-derived var_within / var_across so the metric
        # matches what the inter-annual / inter-model bands visualise and
        # avoids double-counting var_coef. (Unlike the pointrange total
        # band, this decomposition panel intentionally includes
        # var_within - its purpose is to show the share of every source,
        # including year-to-year spread.)
        mm <- .results_frame_matrix(frame, entry$key)
        vals <- if (is.null(mm)) NULL else mm$vals
        var_within <- if (!is.null(vals) && ncol(vals) > 1L) {
          v <- mean(apply(vals, 1L, stats::var, na.rm = TRUE), na.rm = TRUE)
          if (is.finite(v)) v else 0
        } else {
          0
        }
        var_across <- if (!is_hist && !is.null(vals) && nrow(vals) > 1L) {
          v <- stats::var(rowMeans(vals, na.rm = TRUE), na.rm = TRUE)
          if (is.finite(v)) v else 0
        } else {
          0
        }
        tibble::tibble(
          scenario      = scenario_label,
          var_coef      = var_coef,
          var_within    = var_within,
          var_across    = var_across,
          is_historical = is_hist
        )
      }

      labels <- names(frame$.entries)
      rows <- list(one_scenario(.results_frame_entry(frame, "Historical")))
      for (dk in setdiff(labels, "Historical")) {
        if (!dk %in% selected_scenario_names()) next
        rows[[length(rows) + 1L]] <- one_scenario(
          .results_frame_entry(frame, dk)
        )
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # exceedance_curves_rv: per (scenario, model) ECDF rows ----
    # One row per (scenario, model, rank). welfare_val is sorted in the adverse
    # tail direction; exceed_prob is (rank - 0.5)/n_pts limited to <= 0.50 AEP.
    exceedance_curves_rv <- reactive({
      req(hist_agg_rv())
      hist_ref <- hist_ref_val()
      wk <- weight_key()
      method <- input$cmp_agg_method %||% "mean"
      so_obj <- tryCatch(if (!is.null(hist_sim())) hist_sim()$so else NULL, error = function(e) NULL)
      spec <- metric_metadata(method, so_obj)
      adverse_tail <- spec$adverse_tail

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) {
          return(NULL)
        }
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) {
          return(NULL)
        }
        vals <- mm$vals
        sds <- mm$sds
        n_yrs <- ncol(vals)
        if (n_yrs == 0L) {
          return(NULL)
        }

        do.call(dplyr::bind_rows, lapply(seq_len(nrow(vals)), function(i) {
          v <- vals[i, ]
          s <- sds[i, ]
          ok <- is.finite(v)
          if (!any(ok)) {
            return(NULL)
          }
          v <- v[ok]
          s <- s[ok]
          n_pts <- length(v)

          # Adverse tail direction:
          # If adverse_tail == "low" (welfare/consumption): smaller is worse -> ascending.
          # If adverse_tail == "high" (poverty): larger is worse -> descending.
          ord <- if (identical(adverse_tail, "high")) {
            order(v, decreasing = TRUE)
          } else {
            order(v, decreasing = FALSE)
          }
          v_ord <- v[ord]
          s_ord <- if (length(s) == length(ord)) s[ord] else rep(0, length(ord))
          # Use empirical plotting positions so the rarest point is exactly
          # 1-in-n, rather than implying support beyond the simulated years.
          probs <- seq_along(ord) / n_pts

          # Limit strictly to adverse tail: 0.50 AEP or less
          keep <- probs <= 0.50
          if (!any(keep)) {
            return(NULL)
          }

          tibble::tibble(
            scenario      = scenario_label,
            model_id      = mm$model_ids[[i]],
            rank          = seq_along(ord)[keep],
            welfare_val   = v_ord[keep] - hist_ref,
            coef_sd       = s_ord[keep],
            exceed_prob   = probs[keep],
            is_historical = is_hist
          )
        }))
      }

      rows <- list(one_scenario(
        .apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
        "Historical", TRUE
      ))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(
            .apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
            dk, FALSE
          )
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # threshold_table_rv: long-format rows ready to pivot wide ----
    # One row per (scenario, Estimate, RP). Estimate names are derived from
    # the user's band quantile selection: e.g., with coef=p10_p90 and
    # ensemble=minmax the rows are "Central (P50)", "Coef P10", "Coef P90",
    # "Ensemble min", "Ensemble max", "Pooled P10", "Pooled P90". Pooled
    # rows combine the coefficient and inter-model components assuming
    # independence: SE_pooled = sqrt(coef_sd^2 + var_across_at_rp).
    # Historical (no inter-model component) does not emit Pooled rows -
    # they would duplicate the Coef rows.
    threshold_table_rv <- reactive({
      req(hist_agg_rv())
      bq_coef <- resolve_band_q(input$uncertainty_band %||% "p10_p90")
      bq_ens <- if (identical(input$ensemble_band %||% "none", "none")) {
        c(lo = 0.5, hi = 0.5)
      } else {
        resolve_band_q(input$ensemble_band %||% "none")
      }
      z_coef_lo <- stats::qnorm(bq_coef[["lo"]])
      z_coef_hi <- stats::qnorm(bq_coef[["hi"]])
      hist_ref <- hist_ref_val()
      wk <- weight_key()
      method <- input$cmp_agg_method %||% "mean"
      so_obj <- tryCatch(if (!is.null(hist_sim())) hist_sim()$so else NULL,
        error = function(e) NULL
      )
      adverse_tail <- metric_metadata(method, so_obj)$adverse_tail

      RPs <- c(RP_LOW, c("1:1" = 0.5), RP_HIGH)

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) {
          return(NULL)
        }
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) {
          return(NULL)
        }
        vals <- mm$vals
        sds <- mm$sds
        n_yrs <- ncol(vals)
        n_pts <- if (is_hist) sum(is.finite(as.numeric(vals))) else n_yrs

        # Drop RPs that aren't comfortably supported by n_yrs of data. A 1-in-N
        # return period needs at least N observations (p in [1/n, 1-1/n]); we
        # don't report tighter probabilities - they'd rest on the single most
        # extreme observed year and are not meaningful as a "1-in-N" estimate.
        rp_ok <- RPs >= (1 / n_yrs) & RPs <= (1 - 1 / n_yrs)
        RPs_keep <- RPs[rp_ok]
        if (length(RPs_keep) == 0L) {
          return(NULL)
        }

        # Per-model rank-interp at each kept RP (matrix: model * RP) - shape
        # guaranteed by the helper (see by_model_rp_matrix()).
        mm <- by_model_rp_matrix(vals, sds, RPs_keep, adverse_tail)
        per_model_rp <- mm$rp
        per_model_sd_at_rp <- mm$sd

        # Aggregate across models for each RP
        central_vec <- if (is_hist) {
          per_model_rp[1L, ]
        } else {
          apply(per_model_rp, 2L, stats::median, na.rm = TRUE)
        }
        coef_sd_vec <- if (is_hist) {
          per_model_sd_at_rp[1L, ]
        } else {
          apply(per_model_sd_at_rp, 2L, stats::median, na.rm = TRUE)
        }
        coef_lo_vec <- central_vec + z_coef_lo * coef_sd_vec
        coef_hi_vec <- central_vec + z_coef_hi * coef_sd_vec

        intermod_lo_vec <- if (is_hist) {
          rep(NA_real_, length(RPs_keep))
        } else {
          apply(per_model_rp, 2L, stats::quantile,
            probs = bq_ens[["lo"]], na.rm = TRUE
          )
        }
        intermod_hi_vec <- if (is_hist) {
          rep(NA_real_, length(RPs_keep))
        } else {
          apply(per_model_rp, 2L, stats::quantile,
            probs = bq_ens[["hi"]], na.rm = TRUE
          )
        }

        # Total band combines coefficient and inter-model variance at each RP,
        # assuming independence. Inter-annual variability is already baked
        # into the per-rank value so it isn't added a second time here.
        var_across_at_rp <- if (is_hist) {
          rep(0, length(RPs_keep))
        } else {
          apply(per_model_rp, 2L, stats::var, na.rm = TRUE)
        }
        var_across_at_rp[is.na(var_across_at_rp)] <- 0
        sd_total_vec <- sqrt(pmax(coef_sd_vec^2 + var_across_at_rp, 0,
          na.rm = FALSE
        ))
        total_lo_vec <- central_vec + z_coef_lo * sd_total_vec
        total_hi_vec <- central_vec + z_coef_hi * sd_total_vec

        make_row <- function(estimate, vec) {
          tibble::tibble(
            scenario = scenario_label,
            Estimate = estimate,
            rp_name = names(RPs_keep),
            rp_label = names(RPs_keep),
            value = vec - hist_ref,
            n_obs = n_pts,
            is_historical = is_hist
          )
        }
        coef_lo_lbl <- paste0("Coef ", .pct_label(bq_coef[["lo"]]))
        coef_hi_lbl <- paste0("Coef ", .pct_label(bq_coef[["hi"]]))
        ens_lo_lbl <- paste0("Ensemble ", .pct_label(bq_ens[["lo"]],
          use_minmax = TRUE
        ))
        ens_hi_lbl <- paste0("Ensemble ", .pct_label(bq_ens[["hi"]],
          use_minmax = TRUE
        ))
        pooled_lo_lbl <- paste0("Pooled ", .pct_label(bq_coef[["lo"]]))
        pooled_hi_lbl <- paste0("Pooled ", .pct_label(bq_coef[["hi"]]))

        rows <- list(
          make_row("Central (P50)", central_vec),
          make_row(coef_lo_lbl, coef_lo_vec),
          make_row(coef_hi_lbl, coef_hi_vec)
        )
        if (!is_hist) {
          ensemble_rows <- if (identical(ens_lo_lbl, ens_hi_lbl)) {
            list(make_row(ens_lo_lbl, central_vec))
          } else {
            list(
              make_row(ens_lo_lbl, intermod_lo_vec),
              make_row(ens_hi_lbl, intermod_hi_vec)
            )
          }
          rows <- c(rows, ensemble_rows, list(
            make_row(pooled_lo_lbl, total_lo_vec),
            make_row(pooled_hi_lbl, total_hi_vec)
          ))
        }
        dplyr::bind_rows(rows)
      }

      rows <- list(one_scenario(
        .apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
        "Historical", TRUE
      ))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(
            .apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
            dk, FALSE
          )
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # UI-48: register Step 2's result figures for the export bundle.
    wise_export_table(
      key = "climate_headline_summary",
      label = "Climate headline summary cards",
      step = 2L,
      fun = function() step2_headline_df(headline_cards_data_rv()),
      description = "At-a-glance summary cards for the focus climate scenario."
    )

    # Zero-arg echarts closures shared by the renders and the export bundle
    # (guidelines §7): one builder call per chart, used for both the live
    # output and wise_export_figure(). Heights match the UI slots.
    pointrange_chart <- function() {
      bands <- pointrange_bands_rv()
      req(bands)
      if (identical(input$ensemble_band %||% "none", "none")) {
        bands$intermod_lo <- NA_real_
        bands$intermod_hi <- NA_real_
      }
      echart_pointrange_climate(
        bands_tbl    = bands,
        x_label      = agg_hist()$x_label,
        group_order  = input$cmp_group_order %||% "scenario_x_year",
        show_coef    = isTRUE(input$show_coef_uncertainty) && has_draws(),
        height       = "600px"
      )
    }
    wise_export_figure(
      key = "climate_outcome_distribution",
      label = "Simulated welfare by scenario and period",
      step = 2L,
      fun = pointrange_chart,
      description = "Simulated welfare by climate scenario and projection period.",
      width = 10, height = 6.5
    )

    annual_distribution_chart <- function() {
      req(annual_distribution_curves_rv())
      echart_annual_distribution(
        annual_distribution_curves_rv(),
        x_label = metric_axis_label(
          input$cmp_agg_method %||% "mean",
          hist_sim()$so,
          input$cmp_deviation %||% "none"
        ),
        plot_type = input$annual_distribution_type %||% "violin",
        height = "470px"
      )
    }
    output$annual_distribution_plot <- echarts4r::renderEcharts4r({
      ch <- annual_distribution_chart()
      req(!is.null(ch))
      ch
    })

    incidence_data_rv <- reactive({
      req(hist_sim(), saved_scenarios(), shiny::isolate(input$cmp_agg_method))
      sc <- selected_scenario_names()
      if (!length(sc)) {
        return(tibble::tibble())
      }
      is_log <- identical(hist_sim()$so$transform, "log")
      svy <- hist_sim()$svy %||% hist_sim()$survey
      if (is.null(svy)) {
        return(tibble::tibble())
      }
      dplyr::bind_rows(lapply(sc, function(nm) {
        entry <- saved_scenarios()[[nm]]
        pipes <- entry$pipelines %||% list(entry$pipeline %||% entry)
        step2_incidence_by_decile(
          svy, hist_sim()$so$name, hist_sim()$pipeline, pipes, is_log, nm
        )
      }))
    })

    incidence_chart <- function() {
      req(incidence_data_rv())
      echart_incidence_by_decile(
        incidence_data_rv(),
        height = "420px"
      )
    }
    # Distributional incidence table: raw values; the CSV button for it
    # lives in the export bundle's reactable flow (there is no mounted UI
    # slot - the data table is exported as the bundle artefact below).
    wise_export_figure(
      key = "climate_distributional_incidence",
      label = "Distributional incidence by baseline decile",
      step = 2L,
      fun = incidence_chart,
      description = "Weighted household-level simulated effects by fixed observed baseline welfare decile.",
      width = 10, height = 6
    )
    wise_export_table(
      key = "climate_distributional_incidence_data",
      label = "Distributional incidence data",
      step = 2L,
      fun = function() {
        annotate_visualization_export(
          incidence_data_rv(), input$cmp_agg_method %||% "mean", hist_sim()$so,
          observation_unit = "household-level simulated welfare effect",
          aggregation_order = "fixed weighted observed baseline decile; weighted mean over households and model summaries",
          uncertainty = "scenario/model variation summarized by selected model set"
        )
      },
      description = "Tidy weighted incidence data by fixed baseline welfare decile."
    )

    # Export the same tidy annual aggregates used by the distribution plot.
    annual_distribution_export <- function() {
      curves <- annual_distribution_curves_rv()
      req(curves)
      annotate_visualization_export(
        curves,
        input$cmp_agg_method %||% "mean",
        hist_sim()$so,
        observation_unit = "annual aggregate for fixed survey population under one weather-year draw",
        aggregation_order = "weighted household aggregate by model and weather-year; model means retained",
        uncertainty = "inter-annual weather variation"
      )
    }
    wise_export_figure(
      key = "climate_annual_distribution",
      label = "Annual outcome distribution across weather years",
      step = 2L,
      fun = annual_distribution_chart,
      description = "Annual aggregate distribution for the fixed population; one observation is one model-weather-year draw.",
      width = 10, height = 6.5
    )
    wise_export_table(
      key = "climate_annual_distribution_data",
      label = "Annual outcome distribution data",
      step = 2L,
      fun = annual_distribution_export,
      description = "Tidy data behind the annual aggregate distribution, including metric and aggregation metadata."
    )
    # UI-48: one builder behind the on-screen table, its CSV button and the
    # export bundle.
    threshold_table_df <- function() {
      tbl <- threshold_table_rv()
      if (is.null(tbl) || !nrow(tbl) || !"Estimate" %in% names(tbl)) {
        return(NULL)
      }
      so_obj <- tryCatch(if (!is.null(hist_sim())) hist_sim()$so else NULL, error = function(e) NULL)
      n_h_yrs <- tryCatch(
        {
          run_info <- if (!is.null(hist_sim())) hist_sim()$sim_summary %||% list() else list()
          hy <- run_info$historical_years %||% integer(0)
          if (length(hy) >= 2L) as.integer(hy[2] - hy[1] + 1L) else max(tbl$n_obs, na.rm = TRUE)
        },
        error = function(e) max(tbl$n_obs, na.rm = TRUE)
      )

      build_threshold_table_df(
        threshold_tbl = tbl,
        group_order   = input$cmp_group_order %||% "scenario_x_year",
        show_coef     = TRUE,
        adverse_only  = TRUE,
        method        = input$cmp_agg_method %||% "mean",
        so            = so_obj,
        n_hist_years  = n_h_yrs
      )
    }

    uncertainty_chart <- function() {
      req(variance_breakdown_rv())
      echart_variance_contribution(
        variance_breakdown_rv(), height = "300px",
        percent = identical(metric_metadata(
          input$cmp_agg_method %||% "mean", hist_sim()$so
        )$format, "percent")
      )
    }
    output$uncertainty_sources_plot <- echarts4r::renderEcharts4r({
      ch <- uncertainty_chart()
      req(!is.null(ch))
      ch
    })
    outputOptions(output, "uncertainty_sources_plot", suspendWhenHidden = TRUE)

    wise_export_figure(
      key = "climate_uncertainty_sources",
      label = "Climate simulation uncertainty sources",
      step = 2L,
      fun = uncertainty_chart,
      description = "Standard deviation contribution from weather-year, coefficient, residual, and climate-model uncertainty sources.",
      width = 9, height = 5
    )

    adverse_dot_data_rv <- reactive({
      req(threshold_table_rv())
      dot <- step2_adverse_dot_data(
        threshold_table_rv(),
        method = input$cmp_agg_method %||% "mean",
        so = hist_sim()$so
      )
      if (identical(input$ensemble_band %||% "none", "none") && nrow(dot)) {
        dot$intermod_lo <- NA_real_
        dot$intermod_hi <- NA_real_
      }
      dot
    })
    adverse_dot_chart <- function() {
      req(adverse_dot_data_rv())
      echart_step2_adverse_dot(
        adverse_dot_data_rv(),
        x_label = metric_axis_label(
          input$cmp_agg_method %||% "mean",
          hist_sim()$so,
          input$cmp_deviation %||% "none"
        ),
        height = "380px"
      )
    }
    output$adverse_dot_plot <- echarts4r::renderEcharts4r({
      ch <- adverse_dot_chart()
      req(!is.null(ch))
      ch
    })
    outputOptions(output, "adverse_dot_plot", suspendWhenHidden = TRUE)

    wise_export_figure(
      key = "climate_adverse_return_periods",
      label = "Outcome in adverse weather years",
      step = 2L,
      fun = adverse_dot_chart,
      description = "Expected and adverse return-period outcomes with inter-model ensemble spread.",
      width = 9, height = 5
    )

    wise_export_table(
      key = "climate_outcome_thresholds",
      label = "Outcome threshold exceedance",
      step = 2L,
      fun = threshold_table_df,
      description = paste(
        "Simulated welfare outcomes against each threshold, by climate",
        "scenario and projection period, with uncertainty bounds."
      )
    )

    # Return-period decision table as a reactable (guidelines §6): raw values
    # in the data (already 2-dp rounded by build_threshold_table_df), rendered
    # client-side; the CSV download moved to the shared client-side
    # wise_reactable_csv_button() next to the widget. INT-08: while the
    # results are stale the table stays visible.
    threshold_reactable <- function() {
      df <- threshold_table_df()
      if (is.null(df) || nrow(df) == 0L) {
        return(.step2_reactable_note("Insufficient data"))
      }
      .step2_reactable(df)
    }
    output$summary_threshold_table <- reactable::renderReactable({
      threshold_reactable()
    })

    # UI-48: the exceedance curve, for the export bundle.
    exceedance_chart <- function() {
      curves <- exceedance_curves_rv()
      ah <- agg_hist()
      if (is.null(curves) || is.null(ah)) {
        return(NULL)
      }
      sel_spread <- input$exceedance_model_spread %||% "none"
      ens_q <- if (identical(sel_spread, "none")) {
        c(lo = 0.5, hi = 0.5)
      } else {
        resolve_band_q(sel_spread)
      }
      echart_exceedance(
        curves_tbl = curves,
        x_label = metric_axis_label(
          input$cmp_agg_method %||% "mean",
          hist_sim()$so,
          input$cmp_deviation %||% "none"
        ),
        n_sim_years = nrow(ah$out),
        logit_x = TRUE,
        band_q = NULL,
        ensemble_band_q = ens_q,
        height = "400px"
      )
    }
    wise_export_figure(
      key = "climate_exceedance_curve",
      label = "Welfare exceedance probability",
      step = 2L,
      fun = exceedance_chart,
      description = paste(
        "Probability of welfare falling below each level in adverse weather years, by climate scenario",
        "and projection period."
      ),
      width = 10, height = 6.5
    )

    output$exceedance_plot <- echarts4r::renderEcharts4r({
      ch <- exceedance_chart()
      req(!is.null(ch))
      ch
    })


    # observeEvent handlers ----

    # Insert Results tab + content on first hist_sim; remove it again when
    # hist_sim is cleared (INT-07) so the empty state returns and a later
    # run re-inserts a fresh tab instead of writing into a stale one.
    results_tab_added <- reactiveVal(FALSE)

    observeEvent(hist_sim(),
      {
        if (is.null(hist_sim())) {
          if (results_tab_added()) {
            shiny::removeTab(
              inputId = tabset_id,
              target  = "sim_tab",
              session = tabset_session
            )
            results_tab_added(FALSE)
          }
          return()
        }

        # UI-50: only ever one Results tab. This used to append unconditionally,
        # so every re-run of Step 2 added another copy - and because the new tab
        # carried a second `#results_section`, the `insertUI()` below targeted
        # the *first* match, filling the original tab and leaving the new one
        # empty. Steps 1 and 3 already guarded their appends; this brings Step 2
        # into line.
        if (!results_tab_added()) {
          shiny::appendTab(
            inputId = tabset_id,
            shiny::tabPanel(
              title = "Results",
              value = "sim_tab",
              shiny::div(id = "results_section")
            ),
            select = TRUE,
            session = tabset_session
          )
          results_tab_added(TRUE)
        } else {
          # The tab is already there. Clear its contents so the re-run's results
          # replace the previous run's rather than stacking beneath them - the
          # pane is built from `hist_sim()$so`, which a new run may have changed.
          # Both this and the insert below are deferred to the end of the flush
          # and run in call order, so the clear always precedes the rewrite.
          # Deferring also keeps the first-run path byte-for-byte as it was:
          # appendTab's DOM insertion lands before anything targets
          # #results_section.
          shiny::removeUI(selector = "#results_section > *", multiple = TRUE)
          try(shiny::updateTabsetPanel(tabset_session,
            inputId = tabset_id,
            selected = "sim_tab"
          ), silent = TRUE)
        }

        sw_obj <- tryCatch(if (is.function(selected_weather)) selected_weather() else selected_weather, error = function(e) NULL)
        wx_lbl <- if (!is.null(sw_obj) && "label" %in% names(sw_obj) && length(sw_obj$label)) {
          paste(tolower(sw_obj$label), collapse = ", ")
        } else if (!is.null(sw_obj) && "name" %in% names(sw_obj) && length(sw_obj$name)) {
          paste(tolower(sw_obj$name), collapse = ", ")
        } else {
          ""
        }

        shiny::insertUI(
          selector = "#results_section",
          where    = "afterBegin",
          ui       = .results_content_ui(ns, hist_sim()$so, weather_var = wx_lbl)
        )
      },
      ignoreInit = TRUE,
      ignoreNULL = FALSE
    )

    # On subsequent runs, just re-select the tab.
    observeEvent(hist_sim(),
      {
        if (!is.null(hist_sim())) {
          shiny::updateTabsetPanel(
            session  = tabset_session,
            inputId  = tabset_id,
            selected = "sim_tab"
          )
        }
      },
      ignoreInit = TRUE
    )

    # Keep agg method choices in sync with outcome.
    observeEvent(hist_sim(),
      {
        req(hist_sim()$so)
        so <- hist_sim()$so
        choices <- hist_aggregate_choices(so$type, so$name)
        current <- isolate(input$cmp_agg_method)
        new_sel <- if (!is.null(current) && current %in% choices) current else "mean"
        shiny::updateRadioButtons(session, "cmp_agg_method",
          choices  = choices,
          selected = new_sel,
          inline   = TRUE
        )
      },
      ignoreInit = TRUE
    )

    # Suspend outputs when Results tab is hidden ----
    outputOptions(output, "annual_distribution_plot", suspendWhenHidden = TRUE)
    outputOptions(output, "summary_threshold_table", suspendWhenHidden = TRUE)
    outputOptions(output, "exceedance_plot", suspendWhenHidden = TRUE)

    # Return API ----
    # timeseries_curves bundles everything the Diagnostics tab needs to render
    # the per-model trajectories plot (the plot lives there now): the
    # per-(scenario, model, sim_year) table, the x-axis label, and the
    # inter-model band quantiles resolved from the Results-tab controls.
    list(
      variance_breakdown = variance_breakdown_rv,
      results_tab_added = results_tab_added,
      aggregation_cache = aggregation_cache,
      derived_results_frame = derived_results_frame_rv,
      timeseries_curves = reactive({
        req(timeseries_curves_rv())
        ens_q <- if (!identical(input$ensemble_band %||% "none", "none")) {
          resolve_band_q(input$ensemble_band %||% "none")
        } else {
          c(lo = 0.5, hi = 0.5)
        }
        list(
          tbl     = timeseries_curves_rv(),
          x_label = metric_axis_label(input$cmp_agg_method %||% "mean", hist_sim()$so),
          ens_q   = ens_q
        )
      })
    )
  })
}
