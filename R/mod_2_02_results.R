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
.results_content_ui <- function(ns, so) {
  tagList(
    # ---- 0. Stale banner (INT-08) -------------------------------------------
    shiny::uiOutput(ns("stale_banner")),

    # ---- 1. Results header -------------------------------------------------
    shiny::uiOutput(ns("results_header_ui")),

    # ---- 2. Analysis controls ----------------------------------------------
    shiny::wellPanel(
      class = "results-controls",
      style = "padding: 8px 12px 4px 12px;",
      # Single compact row: outcome + uncertainty controls wrap as needed
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap;",
        shiny::tags$div(style = "flex:0 1 180px;",
          shiny::selectInput(
            ns("cmp_agg_method"),
            label    = "Aggregation method",
            choices  = hist_aggregate_choices(so$type, so$name),
            selected = "mean"
          )
        ),
        shiny::conditionalPanel(
          condition = paste0("['headcount_ratio','gap','fgt2',",
                             "'prosperity_gap','avg_poverty']",
                             ".indexOf(input['", ns("cmp_agg_method"), "']) > -1"),
          style = "flex:0 1 170px;",
          shiny::numericInput(
            ns("pov_line"),
            label = "Poverty line ($/day, 2021 PPP)",
            value = 3.00, min = 0, step = 0.5
          )
        ),
        shiny::tags$div(style = "flex:0 1 200px;",
          shiny::selectInput(
            ns("cmp_deviation"),
            label    = "Deviation from historical baseline",
            choices  = c(
              "None (raw value)" = "none",
              "Historical mean"   = "mean",
              "Historical median" = "median"
            ),
            selected = "none"
          )
        ),
        shiny::tags$div(style = "flex:0 1 170px;",
          shiny::selectInput(
            ns("uncertainty_band"),
            label   = "Coefficient band",
            choices = c(
              "50% (p25-p75)"   = "p25_p75",
              "60% (p20-p80)"   = "p20_p80",
              "80% (p10-p90)"   = "p10_p90",
              "90% (p05-p95)"   = "p05_p95",
              "95% (p025-p975)" = "p025_p975",
              "99% (p005-p995)" = "p005_p995",
              "Max (min-max)"   = "minmax"
            ),
            selected = "p10_p90"
          )
        ),
        shiny::tags$div(style = "flex:0 1 180px;",
          shiny::selectInput(
            ns("ensemble_band"),
            label    = "Inter-model band",
            choices  = c(
              "50% (p25-p75)"   = "p25_p75",
              "60% (p20-p80)"   = "p20_p80",
              "80% (p10-p90)"   = "p10_p90",
              "90% (p05-p95)"   = "p05_p95",
              "95% (p025-p975)" = "p025_p975",
              "99% (p005-p995)" = "p005_p995",
              "Full range (min-max)" = "minmax"
            ),
            selected = "minmax"
          )
        ),
        shiny::tags$div(
          style = "flex:0 0 auto; padding-bottom:2px;",
          shiny::checkboxInput(
            ns("show_coef_uncertainty"),
            label = "Show coefficient uncertainty",
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("show_model_spread"),
            label = "Show inter-model spread",
            value = TRUE
          ),
          shiny::uiOutput(ns("coef_uncertainty_status_ui"))
        )
      ),

      # Advanced
      shiny::tags$details(
        shiny::tags$summary(
          style = "cursor:pointer; font-size:11px; color:#555; font-weight:600;",
          "Advanced \u25BC"
        ),
        shiny::tags$div(
          style = "display:flex; gap:10px; flex-wrap:wrap; margin-top:4px;",
          shiny::tags$div(style = "flex:1; min-width:160px;",
            shiny::numericInput(
              ns("bandwidth_p0"),
              label = "Headcount smoothing bandwidth (log scale)",
              value = 0.05, min = 0.005, max = 0.5, step = 0.01
            )
          ),
          shiny::tags$div(style = "flex:1; min-width:160px;",
            shiny::radioButtons(
              ns("cmp_group_order"),
              label    = "Group by",
              choices  = c(
                "Scenario \u00D7 Year" = "scenario_x_year",
                "Year \u00D7 Scenario" = "year_x_scenario"
              ),
              selected = "scenario_x_year",
              inline   = TRUE
            )
          )
        ),
        shiny::helpText(
          "Smoothing bandwidth for the headcount-ratio kernel approximation. ",
          "Smaller = sharper indicator, wider bands at the threshold. 0.05 (default) is a good starting point.",
          style = "font-size:11px; color:#555;"
        )
      ),

      # Scenario filters
      shiny::tags$hr(style = "margin: 6px 0;"),
      shiny::tags$p("Scenario filters",
                    style = "font-weight:600; margin: 0 0 4px 0; font-size:12px;"),
      shiny::uiOutput(ns("scenario_filter_ui"))
    ),

    # ---- 3. Hero point-range chart -----------------------------------------
    shiny::wellPanel(
      shiny::h4(
        "Distribution of outcome across weather conditions, by climate scenario",
        info_popover(
          title = "Reading this chart",
          shiny::p(
            "All bands are drawn relative to the central dot and answer",
            "different questions about uncertainty. They are not meant to be",
            "added together - see the variance-contribution bar on the",
            "Diagnostics tab for how the sources combine."
          ),
          shiny::p(shiny::tags$b("Central dot"),
            " = mean of the annual aggregate across all simulated (model, year) outcomes."),
          shiny::p(shiny::tags$b("Thick coloured band"),
            " (future scenarios only) - how much do climate models disagree?",
            " Inter-model spread: quantile across CMIP6 ensemble members of",
            " each model's time-mean. Can be asymmetric around the dot when",
            " models lean one way."),
          shiny::p(shiny::tags$b("Middle band"),
            " - how much does weather vary year-to-year within a typical model?",
            " Inter-annual variability: per-model quantile across simulation",
            " years, then averaged across models. Reflects the natural range",
            " of outcomes a single climate trajectory produces."),
          shiny::p(shiny::tags$b("Innermost line"),
            " (shown when coefficient uncertainty is enabled) - how precisely",
            " is each (model, year) aggregate estimated? Analytic per-outcome",
            " SE from the regression fit. By default, under 'original'",
            " residuals, restricted to coefficients on weather variables and",
            " their interactions (additive-decomposition SE - see Step 2",
            " settings to widen to all coefficients). This is precision of a",
            " point estimate, not a spread of outcomes - conceptually",
            " distinct from the two coloured bands."),
          shiny::p(
            "Historical = single 'model', so no inter-model band is shown.",
            "A pooled summary SE combining coefficient and inter-model",
            "uncertainty is available in the return-period table on the",
            "Diagnostics tab."
          ),
          docs = TRUE
        )
      ),
      shiny::plotOutput(ns("summary_box_plot"), height = "600px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "Dot = mean outcome; bands = uncertainty ranges (not additive) - click ",
        shiny::icon("circle-info"), " above for details."
      )
    ),

    # ---- 4. Exceedance curve -----------------------------------------------
    shiny::wellPanel(
      shiny::h4(
        "Exceedance probability by climate scenario",
        info_popover(
          title = "Exceedance probability",
          shiny::p(
            "Shows the probability that the outcome exceeds a given",
            "threshold, by scenario. The logit axis emphasises both tails;",
            "return period lines mark standard thresholds (e.g. 1-in-20-year",
            "events)."
          ),
          shiny::p(shiny::tags$b("Solid curve"),
            " (historical) = empirical exceedance curve of the historical baseline."),
          shiny::p(shiny::tags$b("Filled ribbon"),
            " (future scenarios only) = inter-model spread; quantile across ensemble members at each return period. No central curve is drawn for future scenarios so the full spread stays visible."),
          shiny::p(shiny::tags$b("Dashed outlines"),
            " (when coefficient uncertainty enabled) = analytic per-outcome SE band around the median. May fall inside or outside the inter-model ribbon depending on which source dominates."),
          shiny::p(
            "Low odds = value exceeded in only 1-in-N years; high odds =",
            "value reached in all but 1-in-N years."
          ),
          docs = TRUE
        )
      ),
      shiny::tags$div(
        style = "display:flex; gap:20px; flex-wrap:wrap; margin-bottom:6px;",
        shiny::checkboxInput(
          ns("exceedance_logit_x"),
          "Logit probability axis (emphasise both tails)",
          value = FALSE
        ),
        shiny::checkboxInput(
          ns("show_return_period"),
          "Show return period lines",
          value = TRUE
        ),
      ),
      shiny::plotOutput(ns("exceedance_plot"), height = "400px"),
      shiny::uiOutput(ns("exceedance_caption"))
    ),

    # ---- 5. Threshold table ------------------------------------------------
    shiny::wellPanel(
      shiny::uiOutput(ns("threshold_table_header")),
      DT::DTOutput(ns("summary_threshold_table")),
      shiny::uiOutput(ns("threshold_table_footer"))
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
#' @param tabset_id       Character id of the parent tabset panel.
#' @param tabset_session  Shiny session for the tabset.
#'
#' @noRd
mod_2_02_results_server <- function(id,
                                     hist_sim,
                                     saved_scenarios,
                                     selected_hist,
                                     tabset_id,
                                     tabset_session = NULL,
                                     residuals = reactive("original"),
                                     skip_coef_draws = reactive(FALSE),
                                     stale = reactive(FALSE)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    if (is.null(tabset_session)) tabset_session <- session$parent %||% session

    # INT-08: stale banner above the results pane. This surface gates its
    # CSV export while stale.
    output$stale_banner <- renderUI({
      if (isTRUE(stale())) .stale_banner(
        "Step 2 simulation results",
        note = "Interpretation and exports are disabled until then."
      ) else NULL
    })

    # ---- Lazy delta-method aggregation -------------------------------------
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

    # ---- Value-affecting aggregation inputs ---------------------------------
    # The aggregation cache is keyed by the inputs each method actually
    # consumes (PERF-30), so moving the coefficient-band or poverty-line
    # control only invalidates the methods that read it. band_q is display-
    # only: aggregate_with_uncertainty_delta() applies it to value_lo/hi,
    # which no builder below consumes - the band is re-derived from the
    # cached SDs at render time. Display uses a fixed neutral pair.
    AGG_BAND_Q <- c(lo = 0.10, hi = 0.90)
    .POV_LINE_METHODS   <- c("headcount_ratio", "gap", "fgt2")
    .BANDWIDTH_METHODS  <- "headcount_ratio"

    .one_member_delta <- function(pipe, idx, method, weighted, pov_line,
                                  band_q, is_log, seed, res_mode,
                                  resid_lookup = NULL, resid_sigma2 = NULL) {
      # Some upstream paths can hand us F_loading as a length-K numeric
      # vector instead of a 1*K matrix (single-row residual / dropped dim).
      # Promote to matrix before any row-subset so the indexing below never
      # triggers "incorrect number of dimensions".
      F_full <- pipe$F_loading
      if (!is.null(F_full) && is.null(dim(F_full))) {
        F_full <- matrix(F_full, nrow = 1L)
      }

      # Filter to non-NA y_point rows only (NA rows cause non-finite h,
      # which silently zeros out F_agg and var_coef)
      valid  <- idx & !is.na(pipe$y_point)

      F_idx <- if (!is.null(F_full) && !isTRUE(skip_coef_draws()))
                 F_full[valid, , drop = FALSE] else NULL
      w_idx <- if (weighted && !is.null(pipe$weight)) pipe$weight[valid] else NULL
      id_idx <- if (!is.null(pipe$id_vec)) pipe$id_vec[valid] else NULL
      # RIF pipelines set train_aug = NULL by construction (fct_simulations.R).
      # If the residuals selector still says "original" or "resample", honour
      # the pipeline by falling back to "none" so draw_residuals_vec doesn't
      # blow up on a missing .resid column.
      if (is.null(pipe$train_aug) && !identical(res_mode, "none"))
        res_mode <- "none"
      aggregate_with_uncertainty_delta(
        y_point      = pipe$y_point[valid],
        F_loading    = F_idx,
        method       = method,
        weights      = w_idx,
        pov_line     = pov_line,
        residuals    = res_mode,
        train_aug    = pipe$train_aug,
        id_vec       = id_idx,
        id_col       = pipe$id_col,
        is_log       = is_log,
        band_q       = band_q,
        bandwidth_p0 = bandwidth_p0(),
        seed          = seed,
        resid_lookup  = resid_lookup,
        resid_sigma2  = resid_sigma2
      )
    }

    # ---- Aggregation workspace + per-method cache --------------------------
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
    agg_workspace <- reactive({
      req(hist_sim())
      list(
        hs       = hist_sim(),
        sc       = saved_scenarios(),
        res      = hist_sim()$residuals %||% residuals() %||% "original",
        skip     = isTRUE(skip_coef_draws()),
        cache    = new.env(parent = emptyenv())
      )
    })

    # Cache-key suffix for the poverty line / bandwidth values a method reads.
    # Methods that ignore them get a constant key so moving the poverty-line
    # slider does not force their recomputation.
    .pl_bw_key <- function(method, pl_v, bw) {
      parts <- character(0)
      if (method %in% .POV_LINE_METHODS)  parts <- c(parts, format(pl_v))
      if (method %in% .BANDWIDTH_METHODS) parts <- c(parts, format(bw))
      if (length(parts) == 0L) "" else paste0("_", paste(parts, collapse = "_"))
    }

    .build_hist_for_method <- function(ws, method, pl_v) {
      pl   <- ws$hs$pipeline
      yrs  <- sort(unique(pl$sim_year))
      bq   <- AGG_BAND_Q
      is_log <- isTRUE(ws$hs$so$transform == "log")
      # PERF-34: residual lookup/variance are per-pipeline constants.
      lk   <- .residual_lookup(pl$train_aug, pl$id_col)
      sg2  <- .residual_sigma2(pl$train_aug)
      build_for <- function(weighted) {
        rows <- lapply(yrs, function(yr) {
          idx <- pl$sim_year == yr
          m   <- .one_member_delta(
            pl, idx, method, weighted, pl_v, bq, is_log,
            seed = wise_seed(WISEAPP_DEFAULT_SEED, "residual", yr),
            res_mode = ws$res, resid_lookup = lk, resid_sigma2 = sg2
          )
          sd_yr <- sqrt((m$var_coef %||% 0) + (m$var_resid %||% 0))
          F_yr  <- m$F_agg
          tibble::tibble(
            sim_year     = yr,
            value        = m$value,
            model_id     = list("Historical"),
            value_all    = list(m$value),
            value_all_sd = list(sd_yr),
            F_agg_all    = list(if (is.null(F_yr)) NULL else matrix(F_yr, nrow = 1L)),
            var_within   = sd_yr^2,
            var_across   = 0,
            agg_method   = method,
            weighted     = weighted,
            scenario     = "Historical"
          )
        })
        out <- dplyr::bind_rows(rows)
        setNames(list(out), method)
      }
      has_w <- !is.null(pl$weight)
      list(
        unweighted = build_for(FALSE),
        weighted   = if (has_w) build_for(TRUE) else build_for(FALSE)
      )
    }

    .build_scn_for_method <- function(ws, method, pl_v) {
      sc <- ws$sc
      if (length(sc) == 0L) return(NULL)
      bq   <- AGG_BAND_Q
      setNames(lapply(sc, function(s) {
        pipes  <- s$pipelines
        is_log <- isTRUE(s$so$transform == "log")
        yrs    <- sort(unique(pipes[[1L]]$sim_year))
        has_w  <- !is.null(pipes[[1L]]$weight)
        # PERF-34: per-member lookup/variance, built once per member.
        lk_s   <- lapply(pipes, function(pp) .residual_lookup(pp$train_aug, pp$id_col))
        sg2_s  <- lapply(pipes, function(pp) .residual_sigma2(pp$train_aug))
        build_for <- function(weighted) {
          rows <- lapply(yrs, function(yr) {
            mod_ids <- names(pipes) %||% paste0("m", seq_along(pipes))
            per_member_named <- lapply(seq_along(pipes), function(i) {
              idx <- pipes[[i]]$sim_year == yr
              m   <- .one_member_delta(
                pipes[[i]], idx, method, weighted, pl_v, bq, is_log,
                seed = wise_seed(WISEAPP_DEFAULT_SEED, "residual", yr),
                res_mode = ws$res, resid_lookup = lk_s[[i]],
                resid_sigma2 = sg2_s[[i]]
              )
              if (is.null(m)) return(NULL)
              list(id = mod_ids[[i]], m = m)
            })
            per_member_named <- Filter(Negate(is.null), per_member_named)
            if (length(per_member_named) == 0L) return(NULL)
            comb <- combine_ensemble_results(
              lapply(per_member_named, `[[`, "m"), band_q = bq)
            if (is.null(comb)) return(NULL)
            vals_m <- vapply(per_member_named,
                             function(x) x$m$value, numeric(1L))
            sd_m   <- sqrt(pmax(vapply(per_member_named,
                                       function(x) (x$m$var_coef  %||% 0)
                                                 + (x$m$var_resid %||% 0),
                                       numeric(1L)), 0))
            ids_m  <- vapply(per_member_named,
                             function(x) x$id, character(1L))
            F_list <- lapply(per_member_named, function(x) x$m$F_agg)
            F_mat  <- if (all(vapply(F_list, is.null, logical(1L)))) NULL
                      else do.call(rbind, lapply(F_list, function(v) {
                        if (is.null(v)) rep(NA_real_, length(F_list[[which(!vapply(F_list, is.null, logical(1L)))[1]]]))
                        else as.numeric(v)
                      }))
            tibble::tibble(
              sim_year     = yr,
              value        = mean(vals_m, na.rm = TRUE),
              model_id     = list(ids_m),
              value_all    = list(vals_m),
              value_all_sd = list(sd_m),
              F_agg_all    = list(F_mat),
              var_within   = comb$var_within %||% mean(sd_m^2, na.rm = TRUE),
              var_across   = comb$var_across %||%
                               (if (length(vals_m) > 1L)
                                  stats::var(vals_m, na.rm = TRUE) else 0),
              agg_method   = method,
              weighted     = weighted
            )
          })
          out <- dplyr::bind_rows(Filter(Negate(is.null), rows))
          setNames(list(out), method)
        }
        list(
          unweighted = build_for(FALSE),
          weighted   = if (has_w) build_for(TRUE) else build_for(FALSE)
        )
      }), names(sc))
    }

    .get_hist_agg <- function(method) {
      ws   <- agg_workspace()
      pl_v <- pov_line_val()
      bw   <- bandwidth_p0()
      key <- paste0("h_", method, .pl_bw_key(method, pl_v, bw))
      if (!exists(key, envir = ws$cache, inherits = FALSE)) {
        assign(key, .build_hist_for_method(ws, method, pl_v), envir = ws$cache)
      }
      get(key, envir = ws$cache, inherits = FALSE)
    }

    .get_scn_agg <- function(method) {
      ws   <- agg_workspace()
      pl_v <- pov_line_val()
      bw   <- bandwidth_p0()
      key <- paste0("s_", method, .pl_bw_key(method, pl_v, bw))
      if (!exists(key, envir = ws$cache, inherits = FALSE)) {
        assign(key, .build_scn_for_method(ws, method, pl_v), envir = ws$cache)
      }
      get(key, envir = ws$cache, inherits = FALSE)
    }

    # Eagerly pre-compute the default ("mean") aggregation as soon as the
    # simulation finishes, so the Results tab renders immediately when the
    # user opens it. Subsequent method changes are computed on-demand and
    # cached within the current workspace.
    observeEvent(agg_workspace(), {
      req(agg_workspace())
      isolate({
        .get_hist_agg("mean")
        if (length(agg_workspace()$sc) > 0L) .get_scn_agg("mean")
      })
    }, priority = 100, ignoreInit = FALSE)

    hist_agg_rv <- reactive({
      method <- input$cmp_agg_method %||% "mean"
      .get_hist_agg(method)
    })

    scenario_agg_rv <- reactive({
      req(saved_scenarios())
      if (length(saved_scenarios()) == 0L) return(NULL)
      method <- input$cmp_agg_method %||% "mean"
      .get_scn_agg(method)
    })

    # ---- Reactive computations (carried over from mod_2_06) ----------------

    hist_label <- reactive({
      nm <- if (!is.null(selected_hist)) selected_hist()$scenario_name else NULL
      if (!is.null(nm) && nzchar(nm)) nm else "Historical"
    })

    all_ssps <- reactive({
      sc <- saved_scenarios()
      if (length(sc) == 0) return(character(0))
      ssps <- unique(.normalise_ssp(names(sc)))
      sort(ssps[!is.na(ssps) & grepl("^SSP", ssps)])
    })

    all_anchor_years <- reactive({
      sc <- saved_scenarios()
      if (length(sc) == 0) return(character(0))
      ranges <- sort(na.omit(unique(.parse_year(names(sc)))))
      setNames(sub("-", "_", ranges), ranges)
    })

    all_models_info <- reactive({
      sc <- saved_scenarios()
      if (length(sc) == 0) return(character(0))
      # Return model counts per scenario for display
      vapply(sc, function(s) s$n_models %||% 1L, integer(1))
    })



  output$coef_uncertainty_status_ui <- shiny::renderUI({
      req(hist_sim())
      if (!has_draws()) {
        shiny::tags$p(
          style = "font-size:11px; color:#c62828; margin:2px 0 6px 0;",
          "\U0001f534 Coefficient draws skipped at simulation time"
        )
      } else if (!isTRUE(input$show_coef_uncertainty)) {
        shiny::tags$p(
          style = "font-size:11px; color:#e65100; margin:2px 0 6px 0;",
          "\u26a0 Coefficient uncertainty available but not shown"
        )
      } else {
        NULL
      }
    })
    outputOptions(output, "coef_uncertainty_status_ui",
                  suspendWhenHidden = TRUE)

    # Always use survey weights when available (UI toggle removed - weighting
    # is the correct default for survey-based welfare estimates).
    weight_key <- reactive({
      if (!is.null(hist_sim()) && isTRUE(hist_sim()$has_weights))
        "weighted" else "unweighted"
    })

    # Shared deviation reference - used by all_series_tbl and exceedance_ribbon
        hist_ref_val <- reactive({
          req(hist_agg_rv())
          method    <- input$cmp_agg_method %||% "mean"
          wk        <- weight_key()
          deviation <- input$cmp_deviation %||% "none"
          if (identical(deviation, "none")) return(0)
          raw_vals <- hist_agg_rv()[[wk]][[method]]$value
          if (identical(deviation, "mean"))
            mean(raw_vals, na.rm = TRUE)
          else
            stats::median(raw_vals, na.rm = TRUE)
        })

    # Per-coefficient gradient of the historical reference being subtracted.
    # When deviation = mean: average of per-year F_agg across historical years.
    # When deviation = median: F_agg at the historical year closest to the median.
    # Used by .apply_contrast_sd() below to switch coefficient SE from
    # level-CI (||F_s||) to contrast-CI (||F_s - F_ref||), the correct SE for
    # paired counterfactual analysis on the same population.
    hist_F_agg_ref <- reactive({
      req(hist_agg_rv())
      method    <- input$cmp_agg_method %||% "mean"
      wk        <- weight_key()
      deviation <- input$cmp_deviation %||% "none"
      if (identical(deviation, "none")) return(NULL)
      ht <- hist_agg_rv()[[wk]][[method]]
      if (is.null(ht) || nrow(ht) == 0L || !"F_agg_all" %in% names(ht))
        return(NULL)
      # Historical has one "model" so each F_agg_all row is a 1 x K matrix.
      F_list <- lapply(ht$F_agg_all, function(m) {
        if (is.null(m) || !is.matrix(m) || nrow(m) == 0L) NULL
        else as.numeric(m[1L, ])
      })
      F_list <- Filter(Negate(is.null), F_list)
      if (length(F_list) == 0L) return(NULL)
      if (identical(deviation, "mean")) {
        Reduce(`+`, F_list) / length(F_list)
      } else {
        vals    <- ht$value
        med_v   <- stats::median(vals, na.rm = TRUE)
        med_idx <- which.min(abs(vals - med_v))
        if (length(med_idx) == 0L) Reduce(`+`, F_list) / length(F_list)
        else F_list[[med_idx]]
      }
    })

    # Replace per-(model, year) coefficient SDs with paired-contrast SDs
    # when a deviation reference is active. By overwriting `value_all_sd`
    # here, every downstream consumer (pointrange_bands_rv,
    # threshold_table_rv, exceedance_curves_rv) automatically uses the
    # tightened contrast variance.
    .apply_contrast_sd <- function(tbl, F_ref) {
      if (is.null(F_ref) || is.null(tbl) || nrow(tbl) == 0L) return(tbl)
      if (!"F_agg_all" %in% names(tbl) || !"value_all_sd" %in% names(tbl))
        return(tbl)
      for (k in seq_len(nrow(tbl))) {
        F_mat <- tbl$F_agg_all[[k]]
        if (is.null(F_mat) || !is.matrix(F_mat) || ncol(F_mat) != length(F_ref))
          next
        F_diff <- sweep(F_mat, 2L, F_ref, "-")
        tbl$value_all_sd[[k]] <- sqrt(rowSums(F_diff * F_diff))
      }
      tbl
    }


        # ---- Coefficient draws availability -----------------------------------
    has_draws <- reactive({
      req(hist_sim())
      !is.null(hist_sim()$chol_obj)
    })

    # Sync the "Show coefficient uncertainty" toggle to the current sim:
    #   - When the new sim has no chol_obj (skip_coef_draws was TRUE), force
    #     the box off so the user sees the toggle reflect reality.
    #   - When the new sim does have chol_obj (uncertainty was included),
    #     re-enable the box. Without this re-set, a prior simulation that
    #     ran without draws would leave the box stuck OFF even after the
    #     user enables coefficient uncertainty and re-runs.
    observeEvent(hist_sim(), {
      req(hist_sim())
      shiny::updateCheckboxInput(
        session, "show_coef_uncertainty",
        value = isTRUE(has_draws())
      )
    }, ignoreInit = TRUE)





    # UI-38: hold the most recent non-empty grid selection so unchecking the
    # final scenario never silently re-displays the first one.
    last_selected_scenarios <- reactiveVal(NULL)

    observe({
      sc   <- saved_scenarios()
      if (length(sc) == 0L) return(invisible(NULL))
      keys <- names(sc)

      selected <- Filter(Negate(is.null), lapply(keys, function(key) {
        cb_id <- paste0("sc_", gsub("[^a-zA-Z0-9]", "_", key))
        if (isTRUE(input[[cb_id]])) key else NULL
      }))

      if (length(selected) > 0L) {
        last_selected_scenarios(unlist(selected))
      } else {
        # Re-check the held boxes so the grid never sits fully unchecked.
        held <- last_selected_scenarios()
        held <- held[held %in% keys]
        if (length(held) == 0L) held <- keys[1L]
        for (key in held) {
          shiny::updateCheckboxInput(
            session,
            inputId = paste0("sc_", gsub("[^a-zA-Z0-9]", "_", key)),
            value   = TRUE
          )
        }
      }
    })

    selected_scenario_names <- reactive({
      sc   <- saved_scenarios()
      if (length(sc) == 0L) return(character(0))
      keys <- names(sc)

      # Read each grid checkbox
      selected <- Filter(Negate(is.null), lapply(keys, function(key) {
        cb_id <- paste0("sc_", gsub("[^a-zA-Z0-9]", "_", key))
        val   <- input[[cb_id]]
        if (isTRUE(val)) key else NULL
      }))

      # Enforce minimum 1 selected: hold the last real selection (UI-38)
      # rather than silently re-adding the first scenario.
      if (length(selected) == 0L) {
        held <- last_selected_scenarios()
        held <- held[held %in% keys]
        if (length(held) == 0L) held <- keys[1L]
        held
      } else {
        unlist(selected)
      }
    })

    agg_hist <- reactive({
      req(hist_agg_rv())
      method    <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation  %||% "none"
      out       <- hist_agg_rv()[[weight_key()]][[method]]
      req(!is.null(out))
      hist_ref  <- hist_ref_val()
      if (!identical(deviation, "none") && nrow(out) > 0)
        out <- dplyr::mutate(out, value = value - hist_ref)
      x_label <- if (identical(deviation, "none")) label_agg_method(method) else
        paste0(label_agg_method(method), " \u2014 ", label_deviation(deviation))
      list(out = out, x_label = x_label)
    })

    agg_scenarios <- reactive({
      req(scenario_agg_rv())
      sc <- saved_scenarios()
      if (length(sc) == 0) return(list())
      method    <- input$cmp_agg_method %||% "mean"
      deviation <- input$cmp_deviation  %||% "none"
      hist_ref <- hist_ref_val() 
      x_label <- if (identical(deviation, "none")) label_agg_method(method) else
        paste0(label_agg_method(method), " \u2014 ", label_deviation(deviation))
        selected <- selected_scenario_names()
        result <- setNames(lapply(names(sc), function(display_key) {
          if (!display_key %in% selected) return(NULL)
          out <- scenario_agg_rv()[[display_key]][[weight_key()]][[method]]
          if (is.null(out) || nrow(out) == 0L) return(NULL)
          if (!identical(deviation, "none"))
            out <- dplyr::mutate(out, value = value - hist_ref)
          list(out = out, x_label = x_label)
        }), names(sc))
      Filter(function(x) !is.null(x) && !is.null(x$out) && nrow(x$out) > 0, result)
    })

    # `exceedance_ribbon` removed - the ribbon is now built inside
    # enhance_exceedance() directly from each series' (value_all, value_all_sd)
    # using analytic delta-method bands, so there is nothing to precompute here.
    

    # `all_series` is now a thin passthrough: it gathers the deviation-shifted
    # tibbles from agg_hist()/agg_scenarios() and tags each with its scenario
    # name. No analytic band augmentation - each plot/table reactive below
    # constructs its own bands from value_all + value_all_sd directly.
    all_series <- reactive({
      req(agg_hist())
      hist_list <- list(Historical = list(
        out      = dplyr::mutate(agg_hist()$out, scenario = "Historical"),
        x_label  = agg_hist()$x_label
      ))
      sc <- agg_scenarios()
      if (length(sc) == 0L) return(hist_list)
      sc_list <- setNames(lapply(names(sc), function(dk) {
        out <- sc[[dk]]$out
        if (is.null(out) || nrow(out) == 0L) return(NULL)
        list(out = dplyr::mutate(out, scenario = dk),
             x_label = sc[[dk]]$x_label)
      }), names(sc))
      c(hist_list, Filter(Negate(is.null), sc_list))
    })




    table_subtitle <- reactive({
      req(agg_hist(), input$cmp_agg_method, input$cmp_deviation)
      paste0(
        agg_hist()$x_label, " \u2014 ",
        label_agg_method(input$cmp_agg_method), " | ",
        label_deviation(input$cmp_deviation)
      )
    })

    # ---- renderUI / render* outputs ----------------------------------------

    output$results_header_ui <- renderUI({
      req(hist_sim(), input$cmp_agg_method, input$cmp_deviation)
      so <- hist_sim()$so

      agg_label    <- label_agg_method(input$cmp_agg_method)
      dev_label    <- label_deviation(input$cmp_deviation)
      pov_txt      <- if (!is.null(pov_line_val()))
        paste0(" | Poverty line: $", pov_line_val(), "/day") else ""

      notes_txt <- paste0(
        "Showing ", agg_label, " of ", so$label %||% so$name,
        " expressed as ", dev_label, pov_txt, "."
      )

      shiny::div(
        style = paste0(
          "border-left: 4px solid #2166ac; background: #f4f8fd; ",
          "padding: 10px 14px; margin-bottom: 12px; border-radius: 3px;"
        ),
        shiny::tags$strong(style = "font-size:15px;",
                           paste0("Results: ", so$label %||% so$name)),
        shiny::tags$br(),
        shiny::tags$span(style = "color:#555; font-size:12px;", notes_txt)
      )
    })

    output$scenario_filter_ui <- renderUI({
      sc <- saved_scenarios()
      if (length(sc) == 0L)
        return(shiny::helpText("Run a simulation."))

      # Parse scenario keys into SSP * period grid
      keys  <- names(sc)
      ssps  <- sort(unique(sub(" / .*$", "", keys)))
      yrs   <- sort(unique(sub("^.* / ", "", keys)))

      # Build header row
      header <- shiny::tags$tr(
        shiny::tags$th(""),
        lapply(ssps, function(s)
          shiny::tags$th(s,
            style = "text-align:center; font-size:11px;
                    font-weight:600; padding:2px 8px;"))
      )

      # Build one row per period
      period_rows <- lapply(yrs, function(yr) {
        shiny::tags$tr(
          shiny::tags$td(yr,
            style = "font-size:11px; font-weight:600;
                    padding:2px 8px; white-space:nowrap;"),
          lapply(ssps, function(s) {
            key     <- paste0(s, " / ", yr)
            exists  <- key %in% keys
            cb_id   <- ns(paste0("sc_", gsub("[^a-zA-Z0-9]", "_", key)))
            shiny::tags$td(
              style = "text-align:center; padding:2px 4px;",
              if (exists)
                shiny::checkboxInput(
                  cb_id,
                  label = NULL,
                  value = TRUE
                )
              else
                shiny::tags$span(
                  style = "color:#ccc; font-size:11px;",
                  "-"
                )
            )
          })
        )
      })

      shiny::tags$table(
        id    = "scenario-filter-grid",
        style = "border-collapse:collapse; margin-top:4px;",
        shiny::tags$style(shiny::HTML("
          #scenario-filter-grid .checkbox { margin: 0; padding: 0; }
          #scenario-filter-grid .checkbox label { 
            padding-left: 0; 
            min-height: 0;
          }
          #scenario-filter-grid .checkbox label span { display: none; }
          #scenario-filter-grid input[type='checkbox'] { 
            width: 16px; height: 16px; 
            margin: 0 auto; 
            display: block;
            position: static;
          }
          #scenario-filter-grid td { padding: 4px 12px; }
          #scenario-filter-grid th { padding: 4px 12px; font-size: 11px; }
        ")),
        shiny::tags$thead(header),
        shiny::tags$tbody(period_rows)
      )
    })

    # ---- Three-source uncertainty decomposition ----------------------------
    # All three downstream displays (hero, exceedance, table) source their
    # bands from the helpers below. Each helper produces a per-scenario view
    # that decomposes uncertainty into:
    #   - coefficient (per-outcome SE from value_all_sd)
    #   - inter-annual (within-model spread of value_all across years)
    #   - inter-model  (across-model spread of model means; future only)

    # Helpers are now defined in R/fct_uncertainty_helpers.R as package-internal
    # functions so Module 3 can call the same code path. Aliases keep the
    # existing inline call sites below readable.
    .by_model_matrix <- by_model_matrix
    .pct_label       <- pct_label
    .rank_interp     <- rank_interp

    # ---- pointrange_bands_rv: one row per scenario, three nested bands -----
    pointrange_bands_rv <- reactive({
      req(hist_agg_rv())
      bq_coef <- resolve_band_q(input$uncertainty_band %||% "p10_p90")
      bq_ens  <- resolve_band_q(input$ensemble_band    %||% "minmax")
      z_coef_lo <- stats::qnorm(bq_coef[["lo"]])
      z_coef_hi <- stats::qnorm(bq_coef[["hi"]])
      hist_ref  <- hist_ref_val()
      wk        <- weight_key()
      method    <- input$cmp_agg_method %||% "mean"

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) return(NULL)
        vals <- mm$vals; sds <- mm$sds

        # Inter-model spread: per-model mean across years, then quantile across models.
        model_means <- rowMeans(vals, na.rm = TRUE)
        intermod <- if (is_hist || length(model_means) <= 1L) {
          mean_v <- mean(model_means, na.rm = TRUE)
          c(lo = mean_v, hi = mean_v)
        } else {
          c(lo = unname(stats::quantile(model_means, bq_ens[["lo"]], na.rm = TRUE)),
            hi = unname(stats::quantile(model_means, bq_ens[["hi"]], na.rm = TRUE)))
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
                              probs = bq_ens[["lo"]], na.rm = TRUE)
          per_mod_hi <- apply(vals, 1L, stats::quantile,
                              probs = bq_ens[["hi"]], na.rm = TRUE)
          interann <- c(lo = mean(per_mod_lo, na.rm = TRUE),
                        hi = mean(per_mod_hi, na.rm = TRUE))
        }

        # Coefficient uncertainty: per-outcome SE, centred on ensemble mean.
        ens_mean <- mean(as.numeric(vals), na.rm = TRUE)
        sd_mean  <- mean(as.numeric(sds),  na.rm = TRUE)
        coef     <- c(lo = ens_mean + z_coef_lo * sd_mean,
                      hi = ens_mean + z_coef_hi * sd_mean)

        # "Pooled" band: pooled SE on the central (year- and model-
        # averaged) estimate. Mirrors the return-period table's "Pooled"
        # convention (see fct_sim_compare.R::build_threshold_table_df) -
        # inter-annual variability is a property of the simulated
        # distribution, not uncertainty about the central tendency, and
        # is shown separately as the middle band.
        # var_coef     = mean per-outcome regression-fit variance (uses
        #                paired-contrast SEs when deviation is selected,
        #                via .apply_contrast_sd above).
        # var_across   = variance across model means; matches the inter-
        #                model band's underlying statistic.
        # When var_across is zero (historical or single-member future),
        # the pooled SE degenerates to the coef SE; we suppress the
        # outer whisker (NA) to avoid drawing a duplicate of the coef
        # band.
        var_coef_total <- mean(as.numeric(sds)^2, na.rm = TRUE)
        var_across <- if (!is_hist && nrow(vals) > 1L) {
          v <- stats::var(rowMeans(vals, na.rm = TRUE), na.rm = TRUE)
          if (is.finite(v)) v else 0
        } else 0
        if (var_across > 0) {
          sd_total <- sqrt(max(var_coef_total + var_across, 0,
                               na.rm = TRUE))
          total <- c(lo = ens_mean + z_coef_lo * sd_total,
                     hi = ens_mean + z_coef_hi * sd_total)
        } else {
          total <- c(lo = NA_real_, hi = NA_real_)
        }

        tibble::tibble(
          scenario     = scenario_label,
          value        = ens_mean - hist_ref,
          coef_lo      = unname(coef[["lo"]])       - hist_ref,
          coef_hi      = unname(coef[["hi"]])       - hist_ref,
          interann_lo  = unname(interann[["lo"]])   - hist_ref,
          interann_hi  = unname(interann[["hi"]])   - hist_ref,
          intermod_lo  = unname(intermod[["lo"]])   - hist_ref,
          intermod_hi  = unname(intermod[["hi"]])   - hist_ref,
          total_lo     = unname(total[["lo"]])      - hist_ref,
          total_hi     = unname(total[["hi"]])      - hist_ref,
          is_historical = is_hist,
          n_models     = length(mm$model_ids)
        )
      }

      rows <- list(one_scenario(.apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
                                "Historical", TRUE))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(.apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
                                                    dk, FALSE)
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # ---- timeseries_curves_rv: per (scenario, model, sim_year) values ------
    timeseries_curves_rv <- reactive({
      req(hist_agg_rv())
      hist_ref <- hist_ref_val()
      wk       <- weight_key()
      method   <- input$cmp_agg_method %||% "mean"

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) return(NULL)
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

      rows <- list(one_scenario(.apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
                                "Historical", TRUE))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(.apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
                                                    dk, FALSE)
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # ---- variance_breakdown_rv: one row per scenario, three components -----
    # Aggregates the per-(sim_year) var_within / var_across columns to scalars
    # and re-computes var_coef from the per-(model, year) SD list-column.
    variance_breakdown_rv <- reactive({
      req(hist_agg_rv())
      wk     <- weight_key()
      method <- input$cmp_agg_method %||% "mean"

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
        sds_flat <- as.numeric(unlist(tbl$value_all_sd))
        var_coef <- if (length(sds_flat))
          mean(sds_flat^2, na.rm = TRUE) else 0
        # Use value-matrix-derived var_within / var_across so the metric
        # matches what the inter-annual / inter-model bands visualise and
        # avoids double-counting var_coef. (Unlike the pointrange total
        # band, this decomposition panel intentionally includes
        # var_within - its purpose is to show the share of every source,
        # including year-to-year spread.)
        mm <- by_model_matrix(tbl)
        vals <- if (is.null(mm)) NULL else mm$vals
        var_within <- if (!is.null(vals) && ncol(vals) > 1L) {
          v <- mean(apply(vals, 1L, stats::var, na.rm = TRUE), na.rm = TRUE)
          if (is.finite(v)) v else 0
        } else 0
        var_across <- if (!is_hist && !is.null(vals) && nrow(vals) > 1L) {
          v <- stats::var(rowMeans(vals, na.rm = TRUE), na.rm = TRUE)
          if (is.finite(v)) v else 0
        } else 0
        tibble::tibble(
          scenario      = scenario_label,
          var_coef      = var_coef,
          var_within    = var_within,
          var_across    = var_across,
          is_historical = is_hist
        )
      }

      rows <- list(one_scenario(.apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
                                "Historical", TRUE))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(.apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
                                                    dk, FALSE)
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # ---- exceedance_curves_rv: per (scenario, model) ECDF rows -------------
    # One row per (scenario, model, rank). welfare_val is sorted ascending per
    # model; exceed_prob is rev((seq - 0.5)/n_years). coef_sd is the per-
    # (model, year) SD reordered to match the welfare sort.
    exceedance_curves_rv <- reactive({
      req(hist_agg_rv())
      hist_ref <- hist_ref_val()
      wk       <- weight_key()
      method   <- input$cmp_agg_method %||% "mean"

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) return(NULL)
        vals <- mm$vals; sds <- mm$sds
        n_yrs <- ncol(vals)
        if (n_yrs == 0L) return(NULL)
        probs <- rev((seq_len(n_yrs) - 0.5) / n_yrs)

        do.call(dplyr::bind_rows, lapply(seq_len(nrow(vals)), function(i) {
          v <- vals[i, ]; s <- sds[i, ]
          ok <- is.finite(v)
          if (!any(ok)) return(NULL)
          v <- v[ok]; s <- s[ok]
          ord <- order(v)
          tibble::tibble(
            scenario    = scenario_label,
            model_id    = mm$model_ids[[i]],
            rank        = seq_along(ord),
            welfare_val = v[ord] - hist_ref,
            coef_sd     = if (length(s) == length(ord)) s[ord] else rep(0, length(ord)),
            exceed_prob = rev((seq_len(length(ord)) - 0.5) / length(ord)),
            is_historical = is_hist
          )
        }))
      }

      rows <- list(one_scenario(.apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
                                "Historical", TRUE))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(.apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
                                                    dk, FALSE)
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    # ---- threshold_table_rv: long-format rows ready to pivot wide ---------
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
      bq_ens  <- resolve_band_q(input$ensemble_band    %||% "minmax")
      z_coef_lo <- stats::qnorm(bq_coef[["lo"]])
      z_coef_hi <- stats::qnorm(bq_coef[["hi"]])
      hist_ref  <- hist_ref_val()
      wk        <- weight_key()
      method    <- input$cmp_agg_method %||% "mean"

      RPs <- c(RP_LOW, c("1:1" = 0.5), RP_HIGH)

      one_scenario <- function(tbl, scenario_label, is_hist) {
        if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
        mm <- .by_model_matrix(tbl)
        if (is.null(mm)) return(NULL)
        vals <- mm$vals; sds <- mm$sds
        n_yrs <- ncol(vals)
        n_pts <- if (is_hist) sum(is.finite(as.numeric(vals))) else n_yrs

        # Drop RPs that aren't comfortably supported by n_yrs of data. A 1-in-N
        # return period needs at least N observations (p in [1/n, 1-1/n]); we
        # don't report tighter probabilities - they'd rest on the single most
        # extreme observed year and are not meaningful as a "1-in-N" estimate.
        rp_ok    <- RPs >= (1 / n_yrs) & RPs <= (1 - 1 / n_yrs)
        RPs_keep <- RPs[rp_ok]
        if (length(RPs_keep) == 0L) return(NULL)

        # Per-model rank-interp at each kept RP (matrix: model * RP) - shape
        # guaranteed by the helper (see by_model_rp_matrix()).
        mm        <- by_model_rp_matrix(vals, sds, RPs_keep)
        per_model_rp       <- mm$rp
        per_model_sd_at_rp <- mm$sd

        # Aggregate across models for each RP
        central_vec <- if (is_hist) per_model_rp[1L, ] else
          apply(per_model_rp, 2L, stats::median, na.rm = TRUE)
        coef_sd_vec <- if (is_hist) per_model_sd_at_rp[1L, ] else
          apply(per_model_sd_at_rp, 2L, stats::median, na.rm = TRUE)
        coef_lo_vec <- central_vec + z_coef_lo * coef_sd_vec
        coef_hi_vec <- central_vec + z_coef_hi * coef_sd_vec

        intermod_lo_vec <- if (is_hist) rep(NA_real_, length(RPs_keep)) else
          apply(per_model_rp, 2L, stats::quantile,
                probs = bq_ens[["lo"]], na.rm = TRUE)
        intermod_hi_vec <- if (is_hist) rep(NA_real_, length(RPs_keep)) else
          apply(per_model_rp, 2L, stats::quantile,
                probs = bq_ens[["hi"]], na.rm = TRUE)

        # Total band combines coefficient and inter-model variance at each RP,
        # assuming independence. Inter-annual variability is already baked
        # into the per-rank value so it isn't added a second time here.
        var_across_at_rp <- if (is_hist) rep(0, length(RPs_keep)) else
          apply(per_model_rp, 2L, stats::var, na.rm = TRUE)
        var_across_at_rp[is.na(var_across_at_rp)] <- 0
        sd_total_vec <- sqrt(pmax(coef_sd_vec^2 + var_across_at_rp, 0,
                                  na.rm = FALSE))
        total_lo_vec <- central_vec + z_coef_lo * sd_total_vec
        total_hi_vec <- central_vec + z_coef_hi * sd_total_vec

        make_row <- function(estimate, vec) {
          tibble::tibble(
            scenario   = scenario_label,
            Estimate   = estimate,
            rp_name    = names(RPs_keep),
            rp_label   = names(RPs_keep),
            value      = vec - hist_ref,
            n_obs      = n_pts,
            is_historical = is_hist
          )
        }
        coef_lo_lbl <- paste0("Coef ",  .pct_label(bq_coef[["lo"]]))
        coef_hi_lbl <- paste0("Coef ",  .pct_label(bq_coef[["hi"]]))
        ens_lo_lbl  <- paste0("Ensemble ", .pct_label(bq_ens[["lo"]],
                                                       use_minmax = TRUE))
        ens_hi_lbl  <- paste0("Ensemble ", .pct_label(bq_ens[["hi"]],
                                                       use_minmax = TRUE))
        pooled_lo_lbl <- paste0("Pooled ", .pct_label(bq_coef[["lo"]]))
        pooled_hi_lbl <- paste0("Pooled ", .pct_label(bq_coef[["hi"]]))

        rows <- list(
          make_row("Central (P50)", central_vec),
          make_row(coef_lo_lbl,     coef_lo_vec),
          make_row(coef_hi_lbl,     coef_hi_vec)
        )
        if (!is_hist) {
          rows <- c(rows, list(
            make_row(ens_lo_lbl,    intermod_lo_vec),
            make_row(ens_hi_lbl,    intermod_hi_vec),
            make_row(pooled_lo_lbl, total_lo_vec),
            make_row(pooled_hi_lbl, total_hi_vec)
          ))
        }
        dplyr::bind_rows(rows)
      }

      rows <- list(one_scenario(.apply_contrast_sd(hist_agg_rv()[[wk]][[method]], hist_F_agg_ref()),
                                "Historical", TRUE))
      sa <- scenario_agg_rv()
      if (!is.null(sa) && length(sa) > 0L) {
        for (dk in names(sa)) {
          if (!dk %in% selected_scenario_names()) next
          rows[[length(rows) + 1L]] <- one_scenario(.apply_contrast_sd(sa[[dk]][[wk]][[method]], hist_F_agg_ref()),
                                                    dk, FALSE)
        }
      }
      dplyr::bind_rows(Filter(Negate(is.null), rows))
    })

    output$summary_box_plot <- renderPlot({
      req(pointrange_bands_rv())
      bands <- pointrange_bands_rv()
      if (!isTRUE(input$show_model_spread)) {
        bands$intermod_lo <- NA_real_
        bands$intermod_hi <- NA_real_
      }
      plot_pointrange_climate(
        bands_tbl    = bands,
        x_label      = agg_hist()$x_label,
        group_order  = input$cmp_group_order %||% "scenario_x_year",
        show_coef    = isTRUE(input$show_coef_uncertainty) && has_draws()
      )
    }, height = 600)

    output$summary_threshold_table <- DT::renderDT({
      req(threshold_table_rv())
      tbl <- threshold_table_rv()
      if (!isTRUE(input$show_model_spread)) {
        tbl <- tbl[!grepl("^Ensemble |^Pooled ", tbl$Estimate), , drop = FALSE]
      }
      df <- build_threshold_table_df(
        threshold_tbl = tbl,
        group_order   = input$cmp_group_order %||% "scenario_x_year",
        show_coef     = isTRUE(input$show_coef_uncertainty) && has_draws()
      )
      if (is.null(df) || nrow(df) == 0L)
        return(DT::datatable(data.frame(Message = "Insufficient data"),
                             rownames = FALSE, class = "compact stripe",
                             options  = list(dom = "t")))
      # INT-08: export is disabled while the results are stale - the table
      # stays visible, the CSV button does not.
      dt_buttons <- if (isTRUE(stale())) NULL else
        list(list(extend = "csv", filename = "outcome_thresholds"))
      DT::datatable(
        df, rownames = FALSE, class = "compact stripe",
        options = list(
          pageLength = 15, dom = "Btip", ordering = list(list(2, "desc")),
          columnDefs = list(list(className = "dt-center", targets = "_all")),
          buttons = dt_buttons
        ),
        extensions = "Buttons"
      )
    })

    output$threshold_table_header <- renderUI({
      req(agg_hist())
      tagList(
        shiny::h4(
          "Outcome value at return-period thresholds (both tails)",
          info_popover(
            title = "Return-period thresholds",
            shiny::p(shiny::tags$b("Central (P50)"),
              " = across-model median of each model's return-period value (or the single historical curve)."),
            shiny::p(shiny::tags$b("Coef Pxx"),
              " = analytic per-outcome SE band around the central value (coefficient + residual uncertainty). Percentiles follow the 'Coefficient uncertainty band' selector."),
            shiny::p(shiny::tags$b("Ensemble Pxx / min / max"),
              " (future only) = quantile of per-model return-period values across CMIP6 ensemble members. Percentiles follow the 'Weather + model spread band' selector."),
            shiny::p(shiny::tags$b("Pooled Pxx"),
              " = combined band assuming independence: SE_pooled = sqrt(coef_SE\u00B2 + var_across_models). Future scenarios only - for historical (no inter-model component) Pooled would equal Coef and is not reported."),
            shiny::p(
              "Low odds show the value exceeded in only 1-in-N years; high odds",
              "= value reached in all but 1-in-N years; 1:1 is the median year."
            ),
            shiny::p(
              "Obs = number of simulated years feeding each per-model exceedance",
              "curve. Return periods that fall outside the empirical range",
              "supported by Obs (probability < 0.5/Obs or > 1 - 0.5/Obs) are not",
              "reported rather than extrapolated."
            ),
            docs = TRUE
          )
        ),
        shiny::tags$small(class = "text-muted", table_subtitle())
      )
    })

    output$threshold_table_footer <- renderUI({
      req(agg_hist())
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "Central = median estimate; Coef/Ensemble/Pooled = uncertainty bands - click ",
        shiny::icon("circle-info"), " above for definitions."
      )
    })

    output$exceedance_plot <- renderPlot({
      req(exceedance_curves_rv())
      ens_q <- if (isTRUE(input$show_model_spread))
        resolve_band_q(input$ensemble_band %||% "minmax")
      else c(lo = 0.5, hi = 0.5)
      enhance_exceedance(
        curves_tbl      = exceedance_curves_rv(),
        x_label         = agg_hist()$x_label,
        return_period   = isTRUE(input$show_return_period),
        n_sim_years     = nrow(agg_hist()$out),
        logit_x         = isTRUE(input$exceedance_logit_x),
        band_q          = if (isTRUE(input$show_coef_uncertainty) && has_draws())
                            resolve_band_q(input$uncertainty_band %||% "p10_p90")
                          else NULL,
        ensemble_band_q = ens_q
      )
    })

    output$exceedance_caption <- renderUI({
      req(agg_hist())
      axis_txt <- if (isTRUE(input$exceedance_logit_x))
        "Probability axis is logit-scaled, giving equal visual weight to both tails."
      else
        "Annual exceedance probability - each curve is computed over the simulation years."
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        axis_txt
      )
    })


    # ---- observeEvent handlers ---------------------------------------------

    # Insert Results tab + content on first hist_sim; remove it again when
    # hist_sim is cleared (INT-07) so the empty state returns and a later
    # run re-inserts a fresh tab instead of writing into a stale one.
    results_tab_added <- reactiveVal(FALSE)

    observeEvent(hist_sim(), {
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

      shiny::appendTab(
        inputId = tabset_id,
        shiny::tabPanel(
          title = "Results",
          value = "sim_tab",
          shiny::div(id = "results_section")
        ),
        select  = TRUE,
        session = tabset_session
      )

      shiny::insertUI(
        selector = "#results_section",
        where    = "afterBegin",
        ui       = .results_content_ui(ns, hist_sim()$so)
      )
      results_tab_added(TRUE)
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # On subsequent runs, just re-select the tab.
    observeEvent(hist_sim(), {
      if (!is.null(hist_sim())) {
        shiny::updateTabsetPanel(
          session  = tabset_session,
          inputId  = tabset_id,
          selected = "sim_tab"
        )
      }
    }, ignoreInit = TRUE)

    # Keep agg method choices in sync with outcome.
    observeEvent(hist_sim(), {
      req(hist_sim()$so)
      so      <- hist_sim()$so
      choices <- hist_aggregate_choices(so$type, so$name)
      current <- isolate(input$cmp_agg_method)
      new_sel <- if (!is.null(current) && current %in% choices) current else "mean"
      shiny::updateSelectInput(session, "cmp_agg_method",
                               choices  = choices,
                               selected = new_sel)
    }, ignoreInit = TRUE)

    # ---- Suspend outputs when Results tab is hidden ----------------------
    outputOptions(output, "summary_box_plot",        suspendWhenHidden = TRUE)
    outputOptions(output, "summary_threshold_table", suspendWhenHidden = TRUE)
    outputOptions(output, "exceedance_plot",         suspendWhenHidden = TRUE)
    outputOptions(output, "results_header_ui",       suspendWhenHidden = TRUE)
    outputOptions(output, "scenario_filter_ui",      suspendWhenHidden = TRUE)
    outputOptions(output, "threshold_table_header",  suspendWhenHidden = TRUE)
    outputOptions(output, "threshold_table_footer",  suspendWhenHidden = TRUE)
    outputOptions(output, "exceedance_caption",      suspendWhenHidden = TRUE)
    
    # ---- Return API --------------------------------------------------------
    # timeseries_curves bundles everything the Diagnostics tab needs to render
    # the per-model trajectories plot (the plot lives there now): the
    # per-(scenario, model, sim_year) table, the x-axis label, and the
    # inter-model band quantiles resolved from the Results-tab controls.
    list(
      variance_breakdown = variance_breakdown_rv,
      results_tab_added  = results_tab_added,
      timeseries_curves  = reactive({
        req(timeseries_curves_rv())
        ens_q <- if (isTRUE(input$show_model_spread))
          resolve_band_q(input$ensemble_band %||% "minmax")
        else c(lo = 0.5, hi = 0.5)
        list(
          tbl     = timeseries_curves_rv(),
          x_label = agg_hist()$x_label,
          ens_q   = ens_q
        )
      })
    )
  })
}
