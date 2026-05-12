
#' Make a before/after histogram for a single variable
#' @noRd
.make_before_after_hist <- function(baseline_vals, policy_vals,
                                    var_name) {
  baseline_clean <- baseline_vals[!is.na(baseline_vals)]
  policy_clean   <- policy_vals[!is.na(policy_vals)]
  all_vals       <- c(baseline_clean, policy_clean)

  blank_plot <- function(msg) {
    ggplot2::ggplot() +
      ggplot2::annotate("text", x = 0.5, y = 0.5, label = msg,
                        size = 4, colour = "grey40") +
      ggplot2::theme_void()
  }

  if (length(all_vals) == 0) return(blank_plot("No data available"))

  fill_vals <- c(Baseline = "#bdbdbd", `Policy-adjusted` = "#d32f2f")
  uniq_vals <- unique(all_vals)
  is_binary <- length(uniq_vals) <= 2 && all(uniq_vals %in% c(0, 1))

  # ---- Binary: grouped bar plot of proportions -----------------------------
  if (is_binary) {
    df <- data.frame(
      Group = factor(rep(c("Baseline", "Policy-adjusted"), each = 2),
                     levels = c("Baseline", "Policy-adjusted")),
      Value = factor(rep(c("0", "1"), 2), levels = c("0", "1")),
      Proportion = c(
        if (length(baseline_clean)) mean(baseline_clean == 0) else NA_real_,
        if (length(baseline_clean)) mean(baseline_clean == 1) else NA_real_,
        if (length(policy_clean))   mean(policy_clean   == 0) else NA_real_,
        if (length(policy_clean))   mean(policy_clean   == 1) else NA_real_
      )
    )

    return(
      ggplot2::ggplot(df, ggplot2::aes(x = Value, y = Proportion,
                                       fill = Group)) +
        ggplot2::geom_col(
          position = ggplot2::position_dodge(width = 0.75),
          width    = 0.65,
          colour   = NA
        ) +
        ggplot2::scale_fill_manual(values = fill_vals) +
        ggplot2::scale_y_continuous(limits = c(0, 1),
                                    expand = ggplot2::expansion(c(0, 0.05))) +
        ggplot2::labs(
          title = paste0("Distribution: ", var_name),
          x     = var_name,
          y     = "Proportion",
          fill  = NULL
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          legend.position    = "top",
          plot.title         = ggplot2::element_text(size = 11),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.minor.x = ggplot2::element_blank()
        )
    )
  }

  # ---- Continuous: ridge density (Policy on top, Baseline on bottom) -------
  use_log <- all(all_vals > 0)

  df <- data.frame(
    Group = factor(
      c(rep("Baseline", length(baseline_clean)),
        rep("Policy-adjusted", length(policy_clean))),
      # ggridges renders the FIRST level at the bottom, last at the top —
      # so "Baseline" first puts policy-adjusted on top.
      levels = c("Baseline", "Policy-adjusted")
    ),
    Value = c(baseline_clean, policy_clean)
  )

  if (use_log) df <- df[df$Value > 0, , drop = FALSE]
  if (!nrow(df)) return(blank_plot("No data available"))

  p <- ggplot2::ggplot(df, ggplot2::aes(x = Value, y = Group, fill = Group)) +
    ggridges::geom_density_ridges(alpha = 0.7, scale = 1.5) +
    ggplot2::scale_fill_manual(values = fill_vals) +
    ggplot2::labs(
      title = paste0("Distribution: ", var_name),
      x     = if (use_log) paste0(var_name, " (log scale)") else var_name,
      y     = "",
      fill  = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      legend.position = "none",
      plot.title      = ggplot2::element_text(size = 11)
    )

  if (use_log) {
    p <- p + ggplot2::scale_x_log10(labels = scales::comma_format())
  }
  p
}


#' Detect columns that differ between the baseline and policy-adjusted frames
#'
#' Returns the names of columns whose values differ between
#' \code{baseline_svy} and \code{policy_svy}. Used by the Step 3 diagnostics
#' table to surface any variable a user manipulation has touched —
#' covariates, interaction variables, or outcomes alike.
#'
#' Comparison rules:
#' \itemize{
#'   \item Numeric columns are compared with tolerance via
#'     \code{isTRUE(all.equal(..., check.attributes = FALSE))}.
#'   \item Other columns are compared with \code{identical()}.
#' }
#'
#' Rows must match across the two frames; if \code{nrow()} differs the
#' function returns the union of column names instead (since values can no
#' longer be compared element-wise).
#'
#' @param baseline_svy Data frame before \code{apply_policy_to_svy()}.
#' @param policy_svy   Data frame after \code{apply_policy_to_svy()}.
#'
#' @return Character vector of column names that changed.
#' @export
detect_manipulated_vars <- function(baseline_svy, policy_svy) {
  if (is.null(baseline_svy) || is.null(policy_svy)) return(character(0))
  shared <- intersect(names(baseline_svy), names(policy_svy))
  if (length(shared) == 0) return(character(0))
  if (nrow(baseline_svy) != nrow(policy_svy)) {
    return(setdiff(union(names(baseline_svy), names(policy_svy)), character(0)))
  }
  changed <- vapply(shared, function(v) {
    xb <- baseline_svy[[v]]
    xp <- policy_svy[[v]]
    if (is.numeric(xb) && is.numeric(xp)) {
      !isTRUE(all.equal(xb, xp, check.attributes = FALSE))
    } else {
      !identical(xb, xp)
    }
  }, logical(1))
  shared[changed]
}


#' Build a Diagnostics Summary for Policy-Adjusted Inputs
#'
#' Computes mean / sd / n_nonNA for each covariate in both the baseline and
#' policy-adjusted survey frames, so the Step 3 Results tab can display
#' what changed.
#'
#' @param baseline_svy Data frame before \code{apply_policy_to_svy()}.
#' @param policy_svy   Data frame after \code{apply_policy_to_svy()}.
#' @param vars         Character vector of variable names to summarise. If
#'   \code{NULL}, uses the intersection of the two frames' numeric cols.
#'
#' @return A tibble with columns \code{variable}, \code{mean_baseline},
#'   \code{mean_policy}, \code{delta_mean}, \code{sd_baseline},
#'   \code{sd_policy}, \code{n_nonNA}.
#' @export
policy_input_diagnostics <- function(baseline_svy, policy_svy, vars = NULL) {
  if (is.null(baseline_svy) || is.null(policy_svy)) return(NULL)

  if (is.null(vars)) {
    num_b <- names(baseline_svy)[vapply(baseline_svy, is.numeric, logical(1))]
    num_p <- names(policy_svy)[vapply(policy_svy, is.numeric, logical(1))]
    vars  <- intersect(num_b, num_p)
    # Drop obvious non-covariate keys
    vars  <- setdiff(vars, c("loc_id", "int_year", "int_month", "sim_year"))
  }

  vars <- vars[vars %in% names(baseline_svy) & vars %in% names(policy_svy)]

  if (length(vars) == 0) return(NULL)

  rows <- lapply(vars, function(v) {
    xb <- suppressWarnings(as.numeric(baseline_svy[[v]]))
    xp <- suppressWarnings(as.numeric(policy_svy[[v]]))
    data.frame(
      variable       = v,
      mean_baseline  = mean(xb, na.rm = TRUE),
      mean_policy    = mean(xp, na.rm = TRUE),
      delta_mean     = mean(xp, na.rm = TRUE) - mean(xb, na.rm = TRUE),
      sd_baseline    = stats::sd(xb, na.rm = TRUE),
      sd_policy      = stats::sd(xp, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}


#' Render the UI block for the combined Baseline + Policy results pane.
#'
#' Single-pane layout. The visualisations beneath display baseline and
#' policy series side-by-side. Inputs and outputs are namespaced via
#' \code{ns()}.
#' @noRd
.results_pane_ui <- function(ns, so) {
  tagList(
    shiny::uiOutput(ns("results_header_ui")),
    shiny::wellPanel(
      style = "padding: 10px 14px 6px 14px;",
      shiny::tags$p("Outcome of interest:",
                    style = "font-weight:600; margin-bottom:4px;"),
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:12px; flex-wrap:wrap;",
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::selectInput(
            ns("cmp_agg_method"),
            label    = "Aggregation method",
            choices  = hist_aggregate_choices(so$type, so$name),
            selected = "mean"
          )
        ),
        shiny::tags$div(style = "flex:1; min-width:160px;",
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
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::uiOutput(ns("cmp_pov_line_ui"))
        )
      ),
      # Row 2: Uncertainty controls (mirrors Step 2 results layout)
      shiny::tags$p("Uncertainty",
                    style = "font-weight:600; margin: 6px 0 4px 0; font-size:12px;"),
      shiny::tags$div(
        style = "display:flex; align-items:flex-end; gap:10px; flex-wrap:wrap; margin-bottom:4px;",
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::selectInput(
            ns("uncertainty_band"),
            label   = "Coefficient band",
            choices = c(
              "50% (p25–p75)"   = "p25_p75",
              "60% (p20–p80)"   = "p20_p80",
              "80% (p10–p90)"   = "p10_p90",
              "90% (p05–p95)"   = "p05_p95",
              "95% (p025–p975)" = "p025_p975",
              "99% (p005–p995)" = "p005_p995",
              "Max (min–max)"   = "minmax"
            ),
            selected = "p10_p90"
          )
        ),
        shiny::tags$div(style = "flex:1; min-width:160px;",
          shiny::selectInput(
            ns("ensemble_band"),
            label    = "Inter-model band",
            choices  = c(
              "50% (p25–p75)"   = "p25_p75",
              "60% (p20–p80)"   = "p20_p80",
              "80% (p10–p90)"   = "p10_p90",
              "90% (p05–p95)"   = "p05_p95",
              "95% (p025–p975)" = "p025_p975",
              "99% (p005–p995)" = "p005_p995",
              "Full range (min–max)" = "minmax"
            ),
            selected = "minmax"
          )
        ),
        shiny::tags$div(
          style = "flex:1; min-width:160px; padding-bottom:6px;",
          shiny::checkboxInput(
            ns("show_coef_uncertainty"),
            label = "Show coefficient uncertainty",
            value = TRUE
          ),
          shiny::checkboxInput(
            ns("show_model_spread"),
            label = "Show inter-model spread",
            value = TRUE
          )
        )
      ),
      shiny::tags$details(
        shiny::tags$summary(
          style = "cursor:pointer; font-size:11px; color:#555; font-weight:600;",
          "Advanced ▼"
        ),
        shiny::radioButtons(
          ns("cmp_group_order"),
          label    = "Group charts and tables by",
          choices  = c(
            "Scenario × Year" = "scenario_x_year",
            "Year × Scenario" = "year_x_scenario"
          ),
          selected = "scenario_x_year",
          inline   = TRUE
        )
      ),
      shiny::tags$hr(style = "margin: 6px 0;"),
      shiny::tags$p("Scenario filters",
                    style = "font-weight:600; margin: 0 0 4px 0; font-size:12px;"),
      shiny::uiOutput(ns("scenario_filter_ui"))
    ),
    shiny::wellPanel(
      shiny::h4("Distribution of outcomes across climate scenarios and timeframes"),
      shiny::plotOutput(ns("summary_box_plot"), height = "600px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "Each scenario shows two dots: baseline (grey) and policy-adjusted (red), all bands centred on the dot (the ensemble-mean annual aggregate).",
        shiny::tags$br(),
        shiny::tags$b("Outer grey whisker with caps"),
        " = combined total uncertainty assuming independence: SE = sqrt(var_coef + var_within + var_across).",
        shiny::tags$br(),
        shiny::tags$b("Thick band"),
        " (future scenarios) = inter-model spread across CMIP6 ensemble members of each model's time-mean.",
        shiny::tags$br(),
        shiny::tags$b("Middle band"),
        " = inter-annual weather variability (quantile across simulation years).",
        shiny::tags$br(),
        shiny::tags$b("Innermost line"),
        " (when coefficient uncertainty is enabled) = analytic per-outcome SE from the regression fit.",
        shiny::tags$br(),
        "Historical = single 'model' so no inter-model band is shown. Dashed line = historical mean."
      )
    ),
    shiny::wellPanel(
      shiny::h4("Exceedance probability by climate scenario"),
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
        )
      ),
      shiny::plotOutput(ns("exceedance_plot"), height = "400px"),
      shiny::uiOutput(ns("exceedance_caption"))
    ),
    shiny::wellPanel(
      shiny::uiOutput(ns("threshold_table_header")),
      DT::DTOutput(ns("summary_threshold_table")),
      shiny::uiOutput(ns("threshold_table_footer"))
    ),
    shiny::wellPanel(
      shiny::h4("Per-model trajectories over simulation years"),
      shiny::plotOutput(ns("timeseries_plot"), height = "420px"),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px;",
        "Thin lines = one CMIP6 member's annual trajectory; bold line = ",
        "across-model median per simulation year. Baseline is rendered ",
        "faded; policy-adjusted is fully opaque."
      )
    )
  )
}

#' Wire reactives and output bindings for the combined results pane.
#'
#' Takes both baseline and policy reactives and renders one pane that
#' compares them side-by-side. Controls (aggregation method, deviation,
#' weights, scenario filter) drive both sources jointly.
#' @noRd
.wire_results_pane <- function(input, output, session,
                               baseline_hist_sim,
                               baseline_saved_scenarios,
                               policy_hist_sim,
                               policy_saved_scenarios,
                               selected_hist) {
  ns <- session$ns

  hist_label <- reactive({
    nm <- if (!is.null(selected_hist)) selected_hist()$scenario_name else NULL
    if (!is.null(nm) && nzchar(nm)) nm else "Historical"
  })

  # SSP / year / model metadata is identical across baseline and policy
  # (same set of saved scenarios), so derive from baseline.
  all_ssps <- reactive({
    sc <- baseline_saved_scenarios()
    if (length(sc) == 0) return(character(0))
    ssps <- unique(.normalise_ssp(names(sc)))
    sort(ssps[!is.na(ssps) & grepl("^SSP", ssps)])
  })

  all_anchor_years <- reactive({
    sc <- baseline_saved_scenarios()
    if (length(sc) == 0) return(character(0))
    ranges <- sort(na.omit(unique(.parse_year(names(sc)))))
    setNames(sub("-", "_", ranges), ranges)
  })

  all_models_info <- reactive({
    sc <- baseline_saved_scenarios()
    if (length(sc) == 0) return(character(0))
    vapply(sc, function(s) s$n_models %||% 1L, integer(1))
  })

  # Survey weights are always applied when available (toggle removed).
  output$weight_status_ui <- shiny::renderUI(NULL)

  pov_line_val <- reactive({
    if (isTRUE(input$cmp_agg_method %in%
               c("headcount_ratio", "gap", "fgt2"))) {
      as.numeric(input$cmp_pov_line) %||% 3.00
    } else NULL
  })

  selected_scenario_names <- reactive({
    sc <- baseline_saved_scenarios()
    if (length(sc) == 0) return(character(0))
    nms  <- names(sc)
    ssps <- if (length(input$filter_ssp) == 0) all_ssps()
            else input$filter_ssp
    yr_vals <- if (length(input$filter_year) == 0) names(all_anchor_years())
               else sub("_", "-", input$filter_year)
    keep <- vapply(nms, function(nm) {
      is_ssp <- grepl("^SSP", nm)
      if (!is_ssp) return(TRUE)
      ssp_match <- any(vapply(ssps, function(s) startsWith(nm, s), logical(1)))
      yr_match  <- length(yr_vals) == 0 ||
        any(vapply(yr_vals, function(y) grepl(y, nm, fixed = TRUE), logical(1)))
      ssp_match && yr_match
    }, logical(1))
    nms[keep]
  })

  # Helper: aggregate hist_sim into Mod 2's rich list-col schema
  # (one row per sim_year, list-cols value_all / value_all_sd / model_id,
  # plus scalar var_within / var_across). This lets us reuse Mod 2's
  # by_model_matrix() + downstream plot helpers verbatim.
  make_agg_hist <- function(hs) {
    if (is.null(hs) || is.null(hs$y_point)) return(NULL)
    method    <- input$cmp_agg_method %||% "mean"
    deviation <- input$cmp_deviation  %||% "none"
    use_w     <- TRUE

    yrs    <- sort(unique(hs$sim_year))
    is_log <- isTRUE(hs$so$transform == "log")
    bq     <- c(lo = 0.10, hi = 0.90)
    train_aug_df <- if (!is.null(hs$train_data) && ".resid" %in% names(hs$train_data))
                      hs$train_data else NULL

    rows <- lapply(yrs, function(yr) {
      idx <- hs$sim_year == yr
      F_full <- hs$F_loading
      if (!is.null(F_full) && is.null(dim(F_full)))
        F_full <- matrix(F_full, nrow = 1L)
      F_idx <- if (!is.null(F_full)) F_full[idx, , drop = FALSE] else NULL
      m <- aggregate_with_uncertainty_delta(
        y_point   = hs$y_point[idx],
        F_loading = F_idx,
        method    = method,
        weights   = if (use_w && !is.null(hs$weight)) hs$weight[idx] else NULL,
        pov_line  = pov_line_val(),
        residuals = hs$residuals %||% "none",
        train_aug = train_aug_df,
        id_vec    = if (!is.null(hs$id_vec)) hs$id_vec[idx] else NULL,
        id_col    = hs$id_col,
        is_log    = is_log,
        band_q    = bq
      )
      sd_yr <- sqrt((m$var_coef %||% 0) + (m$var_resid %||% 0))
      tibble::tibble(
        sim_year     = yr,
        value        = m$value,
        model_id     = list("Historical"),
        value_all    = list(m$value),
        value_all_sd = list(sd_yr),
        var_within   = sd_yr^2,
        var_across   = 0,
        scenario     = "Historical"
      )
    })
    agg <- dplyr::bind_rows(rows)
    x_label <- if (identical(deviation, "none")) label_agg_method(method)
               else paste0(label_agg_method(method), " \u2014 ",
                           label_deviation(deviation))
    list(out = agg, x_label = x_label)
  }

  # Helper: build agg per saved scenario in Mod 2 schema. Each `s$models`
  # entry is one CMIP6 ensemble member with its own y_point / F_loading.
  make_agg_scenarios <- function(sc, hs_for_dev) {
    if (length(sc) == 0) return(list())
    method    <- input$cmp_agg_method %||% "mean"
    deviation <- input$cmp_deviation  %||% "none"
    use_w     <- TRUE
    x_label   <- if (identical(deviation, "none")) label_agg_method(method)
                 else paste0(label_agg_method(method), " \u2014 ",
                             label_deviation(deviation))

    lapply(sc, function(s) {
      tryCatch({
        if (is.null(s$models)) return(NULL)
        is_log <- isTRUE(s$so$transform == "log")
        bq     <- c(lo = 0.10, hi = 0.90)
        train_aug_df <- if (!is.null(hs_for_dev$train_data) &&
                            ".resid" %in% names(hs_for_dev$train_data))
                          hs_for_dev$train_data else NULL
        yrs    <- sort(unique(s$models[[1L]]$sim_year))
        model_ids_all <- vapply(seq_along(s$models), function(i) {
          s$models[[i]]$name %||% paste0("model_", i)
        }, character(1L))

        per_year_rows <- lapply(yrs, function(yr) {
          per_member <- lapply(s$models, function(mod) {
            idx <- mod$sim_year == yr
            F_full <- mod$F_loading
            if (!is.null(F_full) && is.null(dim(F_full)))
              F_full <- matrix(F_full, nrow = 1L)
            F_idx <- if (!is.null(F_full)) F_full[idx, , drop = FALSE] else NULL
            aggregate_with_uncertainty_delta(
              y_point   = mod$y_point[idx],
              F_loading = F_idx,
              method    = method,
              weights   = if (use_w && !is.null(mod$weight)) mod$weight[idx] else NULL,
              pov_line  = pov_line_val(),
              residuals = hs_for_dev$residuals %||% "none",
              train_aug = train_aug_df,
              id_vec    = if (!is.null(mod$id_vec)) mod$id_vec[idx] else NULL,
              id_col    = mod$id_col,
              is_log    = is_log,
              band_q    = bq
            )
          })
          keep <- !vapply(per_member, is.null, logical(1L))
          per_member <- per_member[keep]
          ids_yr     <- model_ids_all[keep]
          if (length(per_member) == 0L) return(NULL)
          comb <- combine_ensemble_results(per_member, band_q = bq)
          vals_m <- vapply(per_member, function(x) x$value, numeric(1L))
          sd_m   <- sqrt(pmax(
            vapply(per_member,
                   function(x) (x$var_coef %||% 0) + (x$var_resid %||% 0),
                   numeric(1L)), 0))
          tibble::tibble(
            sim_year     = yr,
            value        = mean(vals_m, na.rm = TRUE),
            model_id     = list(ids_yr),
            value_all    = list(vals_m),
            value_all_sd = list(sd_m),
            var_within   = comb$var_within %||% mean(sd_m^2, na.rm = TRUE),
            var_across   = comb$var_across %||%
                             (if (length(vals_m) > 1L)
                                stats::var(vals_m, na.rm = TRUE) else 0)
          )
        })
        combined <- dplyr::bind_rows(Filter(Negate(is.null), per_year_rows))
        if (nrow(combined) == 0L) return(NULL)
        list(out = combined, x_label = x_label)
      }, error = function(e) NULL)
    })
  }

  baseline_agg_hist <- reactive({
    req(baseline_hist_sim())
    make_agg_hist(baseline_hist_sim())
  })
  policy_agg_hist <- reactive({
    req(policy_hist_sim())
    make_agg_hist(policy_hist_sim())
  })

  baseline_agg_scenarios <- reactive({
    req(baseline_hist_sim())
    make_agg_scenarios(baseline_saved_scenarios(), baseline_hist_sim())
  })
  policy_agg_scenarios <- reactive({
    req(policy_hist_sim())
    make_agg_scenarios(policy_saved_scenarios(), policy_hist_sim())
  })

  baseline_all_series <- reactive({
    sc  <- baseline_agg_scenarios()
    sel <- selected_scenario_names()
    c(setNames(list(baseline_agg_hist()), hist_label()),
      sc[intersect(sel, names(sc))])
  })
  policy_all_series <- reactive({
    sc  <- policy_agg_scenarios()
    sel <- selected_scenario_names()
    c(setNames(list(policy_agg_hist()), hist_label()),
      sc[intersect(sel, names(sc))])
  })

  # ---- Shared deviation reference (baseline historical) -------------------
  hist_ref_val <- reactive({
    req(baseline_agg_hist())
    deviation <- input$cmp_deviation %||% "none"
    raw_vals  <- baseline_agg_hist()$out$value
    if (identical(deviation, "mean"))   mean(raw_vals,   na.rm = TRUE)
    else if (identical(deviation, "median")) median(raw_vals, na.rm = TRUE)
    else 0
  })

  has_draws <- reactive({
    bh <- baseline_hist_sim()
    ph <- policy_hist_sim()
    isTRUE(!is.null(bh$F_loading) || !is.null(ph$F_loading))
  })

  # ---- Per-source helpers that mirror Mod 2's reactive trio --------------
  # Each takes the per-source aggregate (Mod 2 list-col tibble) and emits
  # the same long-format pointrange / timeseries / exceedance / threshold
  # rows Mod 2's plotters consume, tagged with a `source` column.
  .build_pointrange_rows <- function(agg_hist, agg_scn, hist_ref,
                                     source_label, bq_coef, bq_ens) {
    z_lo <- stats::qnorm(bq_coef[["lo"]])
    z_hi <- stats::qnorm(bq_coef[["hi"]])
    one <- function(tbl, scenario_label, is_hist) {
      if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
      mm <- by_model_matrix(tbl)
      if (is.null(mm)) return(NULL)
      vals <- mm$vals; sds <- mm$sds
      model_means <- rowMeans(vals, na.rm = TRUE)
      intermod <- if (is_hist || length(model_means) <= 1L) {
        mv <- mean(model_means, na.rm = TRUE); c(lo = mv, hi = mv)
      } else c(
        lo = unname(stats::quantile(model_means, bq_ens[["lo"]], na.rm = TRUE)),
        hi = unname(stats::quantile(model_means, bq_ens[["hi"]], na.rm = TRUE))
      )
      if (is_hist) {
        v_flat <- as.numeric(vals)
        interann <- c(
          lo = unname(stats::quantile(v_flat, bq_ens[["lo"]], na.rm = TRUE)),
          hi = unname(stats::quantile(v_flat, bq_ens[["hi"]], na.rm = TRUE))
        )
      } else {
        per_lo <- apply(vals, 1L, stats::quantile, probs = bq_ens[["lo"]], na.rm = TRUE)
        per_hi <- apply(vals, 1L, stats::quantile, probs = bq_ens[["hi"]], na.rm = TRUE)
        interann <- c(lo = mean(per_lo, na.rm = TRUE), hi = mean(per_hi, na.rm = TRUE))
      }
      ens_mean <- mean(as.numeric(vals), na.rm = TRUE)
      sd_mean  <- mean(as.numeric(sds),  na.rm = TRUE)
      coef <- c(lo = ens_mean + z_lo * sd_mean, hi = ens_mean + z_hi * sd_mean)
      # See mod_2_02_results.R for the rationale on recomputing var_within /
      # var_across from the value matrix rather than the stored parametric
      # tbl$var_within (which would double-count var_coef and not respond
      # to deviation mode).
      var_coef_total <- mean(as.numeric(sds)^2, na.rm = TRUE)
      var_within <- if (ncol(vals) > 1L) {
        per_mod_var <- apply(vals, 1L, stats::var, na.rm = TRUE)
        mean(per_mod_var, na.rm = TRUE)
      } else 0
      if (!is.finite(var_within)) var_within <- 0
      var_across <- if (!is_hist && nrow(vals) > 1L) {
        v <- stats::var(rowMeans(vals, na.rm = TRUE), na.rm = TRUE)
        if (is.finite(v)) v else 0
      } else 0
      sd_total <- sqrt(max(var_coef_total + var_within + var_across, 0, na.rm = TRUE))
      total <- c(lo = ens_mean + z_lo * sd_total, hi = ens_mean + z_hi * sd_total)
      tibble::tibble(
        scenario      = scenario_label,
        source        = source_label,
        value         = ens_mean - hist_ref,
        coef_lo       = unname(coef[["lo"]])     - hist_ref,
        coef_hi       = unname(coef[["hi"]])     - hist_ref,
        interann_lo   = unname(interann[["lo"]]) - hist_ref,
        interann_hi   = unname(interann[["hi"]]) - hist_ref,
        intermod_lo   = unname(intermod[["lo"]]) - hist_ref,
        intermod_hi   = unname(intermod[["hi"]]) - hist_ref,
        total_lo      = unname(total[["lo"]])    - hist_ref,
        total_hi      = unname(total[["hi"]])    - hist_ref,
        is_historical = is_hist,
        n_models      = length(mm$model_ids)
      )
    }
    rows <- list(one(agg_hist$out, "Historical", TRUE))
    if (!is.null(agg_scn)) {
      for (dk in names(agg_scn)) {
        if (!dk %in% selected_scenario_names()) next
        rows[[length(rows) + 1L]] <- one(agg_scn[[dk]]$out, dk, FALSE)
      }
    }
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  .build_timeseries_rows <- function(agg_hist, agg_scn, hist_ref, source_label) {
    one <- function(tbl, scenario_label, is_hist) {
      if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
      mm <- by_model_matrix(tbl)
      if (is.null(mm)) return(NULL)
      vals <- mm$vals
      dplyr::bind_rows(lapply(seq_len(nrow(vals)), function(i) {
        tibble::tibble(
          scenario      = scenario_label,
          source        = source_label,
          model_id      = mm$model_ids[[i]],
          sim_year      = as.integer(mm$sim_years),
          value         = vals[i, ] - hist_ref,
          is_historical = is_hist
        )
      }))
    }
    rows <- list(one(agg_hist$out, "Historical", TRUE))
    if (!is.null(agg_scn)) {
      for (dk in names(agg_scn)) {
        if (!dk %in% selected_scenario_names()) next
        rows[[length(rows) + 1L]] <- one(agg_scn[[dk]]$out, dk, FALSE)
      }
    }
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  .build_exceedance_rows <- function(agg_hist, agg_scn, hist_ref, source_label) {
    one <- function(tbl, scenario_label, is_hist) {
      if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
      mm <- by_model_matrix(tbl)
      if (is.null(mm)) return(NULL)
      vals <- mm$vals; sds <- mm$sds
      dplyr::bind_rows(lapply(seq_len(nrow(vals)), function(i) {
        v <- vals[i, ]; s <- sds[i, ]
        ok <- is.finite(v)
        if (!any(ok)) return(NULL)
        v <- v[ok]; s <- s[ok]
        ord <- order(v)
        tibble::tibble(
          scenario      = scenario_label,
          source        = source_label,
          model_id      = mm$model_ids[[i]],
          rank          = seq_along(ord),
          welfare_val   = v[ord] - hist_ref,
          coef_sd       = if (length(s) == length(ord)) s[ord] else rep(0, length(ord)),
          exceed_prob   = rev((seq_len(length(ord)) - 0.5) / length(ord)),
          is_historical = is_hist
        )
      }))
    }
    rows <- list(one(agg_hist$out, "Historical", TRUE))
    if (!is.null(agg_scn)) {
      for (dk in names(agg_scn)) {
        if (!dk %in% selected_scenario_names()) next
        rows[[length(rows) + 1L]] <- one(agg_scn[[dk]]$out, dk, FALSE)
      }
    }
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  .build_threshold_rows <- function(agg_hist, agg_scn, hist_ref, source_label,
                                    bq_coef, bq_ens) {
    z_lo <- stats::qnorm(bq_coef[["lo"]])
    z_hi <- stats::qnorm(bq_coef[["hi"]])
    RPs <- c(RP_LOW, c("1:1" = 0.5), RP_HIGH)
    one <- function(tbl, scenario_label, is_hist) {
      if (is.null(tbl) || nrow(tbl) == 0L) return(NULL)
      mm <- by_model_matrix(tbl)
      if (is.null(mm)) return(NULL)
      vals <- mm$vals; sds <- mm$sds
      n_yrs <- ncol(vals)
      n_pts <- if (is_hist) sum(is.finite(as.numeric(vals))) else n_yrs
      rp_ok    <- RPs >= (1 / n_yrs) & RPs <= (1 - 1 / n_yrs)
      RPs_keep <- RPs[rp_ok]
      if (length(RPs_keep) == 0L) return(NULL)
      per_model_rp <- t(apply(vals, 1L, function(v) {
        v <- v[is.finite(v)]
        if (length(v) < 2L) return(rep(NA_real_, length(RPs_keep)))
        sv <- sort(v)
        vapply(RPs_keep, function(p) rank_interp(sv, p), numeric(1L))
      }))
      per_model_sd_at_rp <- t(vapply(seq_len(nrow(vals)), function(i) {
        v <- vals[i, ]; s <- sds[i, ]
        ok <- is.finite(v)
        if (sum(ok) < 2L) return(rep(NA_real_, length(RPs_keep)))
        ord <- order(v[ok]); s_sorted <- s[ok][ord]
        vapply(RPs_keep, function(p) rank_interp(s_sorted, p), numeric(1L))
      }, numeric(length(RPs_keep))))
      central_vec <- if (is_hist) per_model_rp[1L, ] else
        apply(per_model_rp, 2L, stats::median, na.rm = TRUE)
      coef_sd_vec <- if (is_hist) per_model_sd_at_rp[1L, ] else
        apply(per_model_sd_at_rp, 2L, stats::median, na.rm = TRUE)
      coef_lo_vec <- central_vec + z_lo * coef_sd_vec
      coef_hi_vec <- central_vec + z_hi * coef_sd_vec
      intermod_lo_vec <- if (is_hist) rep(NA_real_, length(RPs_keep)) else
        apply(per_model_rp, 2L, stats::quantile, probs = bq_ens[["lo"]], na.rm = TRUE)
      intermod_hi_vec <- if (is_hist) rep(NA_real_, length(RPs_keep)) else
        apply(per_model_rp, 2L, stats::quantile, probs = bq_ens[["hi"]], na.rm = TRUE)
      var_across_at_rp <- if (is_hist) rep(0, length(RPs_keep)) else
        apply(per_model_rp, 2L, stats::var, na.rm = TRUE)
      var_across_at_rp[is.na(var_across_at_rp)] <- 0
      sd_total_vec <- sqrt(pmax(coef_sd_vec^2 + var_across_at_rp, 0, na.rm = FALSE))
      total_lo_vec <- central_vec + z_lo * sd_total_vec
      total_hi_vec <- central_vec + z_hi * sd_total_vec
      make_row <- function(estimate, vec) {
        tibble::tibble(
          scenario      = scenario_label,
          source        = source_label,
          Estimate      = estimate,
          rp_name       = names(RPs_keep),
          rp_label      = names(RPs_keep),
          value         = vec - hist_ref,
          n_obs         = n_pts,
          is_historical = is_hist
        )
      }
      coef_lo_lbl  <- paste0("Coef ",     pct_label(bq_coef[["lo"]]))
      coef_hi_lbl  <- paste0("Coef ",     pct_label(bq_coef[["hi"]]))
      ens_lo_lbl   <- paste0("Ensemble ", pct_label(bq_ens[["lo"]], use_minmax = TRUE))
      ens_hi_lbl   <- paste0("Ensemble ", pct_label(bq_ens[["hi"]], use_minmax = TRUE))
      total_lo_lbl <- paste0("Total ",    pct_label(bq_coef[["lo"]]))
      total_hi_lbl <- paste0("Total ",    pct_label(bq_coef[["hi"]]))
      rows <- list(
        make_row("Central (P50)", central_vec),
        make_row(coef_lo_lbl,     coef_lo_vec),
        make_row(coef_hi_lbl,     coef_hi_vec)
      )
      if (!is_hist) {
        rows <- c(rows, list(
          make_row(ens_lo_lbl,   intermod_lo_vec),
          make_row(ens_hi_lbl,   intermod_hi_vec),
          make_row(total_lo_lbl, total_lo_vec),
          make_row(total_hi_lbl, total_hi_vec)
        ))
      } else {
        rows <- c(rows, list(
          make_row(total_lo_lbl, coef_lo_vec),
          make_row(total_hi_lbl, coef_hi_vec)
        ))
      }
      dplyr::bind_rows(rows)
    }
    rows <- list(one(agg_hist$out, "Historical", TRUE))
    if (!is.null(agg_scn)) {
      for (dk in names(agg_scn)) {
        if (!dk %in% selected_scenario_names()) next
        rows[[length(rows) + 1L]] <- one(agg_scn[[dk]]$out, dk, FALSE)
      }
    }
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  pointrange_bands_rv <- reactive({
    req(baseline_agg_hist())
    bq_coef <- resolve_band_q(input$uncertainty_band %||% "p10_p90")
    bq_ens  <- resolve_band_q(input$ensemble_band    %||% "minmax")
    hr      <- hist_ref_val()
    dplyr::bind_rows(
      .build_pointrange_rows(baseline_agg_hist(), baseline_agg_scenarios(),
                             hr, "Baseline", bq_coef, bq_ens),
      .build_pointrange_rows(policy_agg_hist(),   policy_agg_scenarios(),
                             hr, "Policy",   bq_coef, bq_ens)
    )
  })

  timeseries_curves_rv <- reactive({
    req(baseline_agg_hist())
    hr <- hist_ref_val()
    dplyr::bind_rows(
      .build_timeseries_rows(baseline_agg_hist(), baseline_agg_scenarios(),
                             hr, "Baseline"),
      .build_timeseries_rows(policy_agg_hist(),   policy_agg_scenarios(),
                             hr, "Policy")
    )
  })

  exceedance_curves_rv <- reactive({
    req(baseline_agg_hist())
    hr <- hist_ref_val()
    dplyr::bind_rows(
      .build_exceedance_rows(baseline_agg_hist(), baseline_agg_scenarios(),
                             hr, "Baseline"),
      .build_exceedance_rows(policy_agg_hist(),   policy_agg_scenarios(),
                             hr, "Policy")
    )
  })

  threshold_table_rv <- reactive({
    req(baseline_agg_hist())
    bq_coef <- resolve_band_q(input$uncertainty_band %||% "p10_p90")
    bq_ens  <- resolve_band_q(input$ensemble_band    %||% "minmax")
    hr      <- hist_ref_val()
    dplyr::bind_rows(
      .build_threshold_rows(baseline_agg_hist(), baseline_agg_scenarios(),
                            hr, "Baseline", bq_coef, bq_ens),
      .build_threshold_rows(policy_agg_hist(),   policy_agg_scenarios(),
                            hr, "Policy",   bq_coef, bq_ens)
    )
  })

  table_subtitle <- reactive({
    req(baseline_agg_hist(), input$cmp_agg_method, input$cmp_deviation)
    paste0(
      baseline_agg_hist()$x_label, " — ",
      label_agg_method(input$cmp_agg_method), " | ",
      label_deviation(input$cmp_deviation)
    )
  })

  output$results_header_ui <- renderUI({
    req(baseline_hist_sim(), input$cmp_agg_method, input$cmp_deviation)
    so <- baseline_hist_sim()$so
    agg_label <- label_agg_method(input$cmp_agg_method)
    dev_label <- label_deviation(input$cmp_deviation)
    pov_txt   <- if (!is.null(pov_line_val()))
      paste0(" | Poverty line: $", pov_line_val(), "/day") else ""
    notes_txt <- paste0(
      "Showing ", agg_label, " of ", so$label %||% so$name,
      " expressed as ", dev_label, pov_txt,
      ". Baseline (grey) and policy (red) shown side-by-side."
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

  output$cmp_pov_line_ui <- renderUI({
    req(input$cmp_agg_method)
    if (input$cmp_agg_method %in% c("headcount_ratio", "gap", "fgt2")) {
      default_val <- baseline_hist_sim()$pov_line %||% 3.00
      shiny::numericInput(
        inputId = ns("cmp_pov_line"),
        label   = "Poverty line ($/day)",
        value   = default_val,
        min     = 0.01,
        step    = 0.5
      )
    }
  })

  output$scenario_filter_ui <- renderUI({
    sc <- baseline_saved_scenarios()
    if (length(sc) == 0)
      return(shiny::helpText("Run a simulation."))
    ssps <- all_ssps()
    yrs  <- all_anchor_years()
    mi   <- all_models_info()
    tagList(
      shiny::fluidRow(
        shiny::column(4,
          if (length(yrs) > 0)
            shiny::checkboxGroupInput(
              ns("filter_year"), label = "Projection periods",
              choices = yrs, selected = yrs, inline = TRUE
            )
        ),
        shiny::column(4,
          if (length(ssps) > 0)
            shiny::checkboxGroupInput(
              ns("filter_ssp"), label = "SSPs",
              choices = ssps, selected = ssps, inline = TRUE
            )
        ),
        shiny::column(4,
          if (any(mi > 1L))
            shiny::helpText(
              style = "font-size:11px; color:#555; margin-top:24px;",
              paste0("Each SSP aggregates results from ",
                     max(mi), " ensemble model(s).")
            )
        )
      )
    )
  })

  output$summary_box_plot <- renderPlot({
    req(pointrange_bands_rv())
    bands <- pointrange_bands_rv()
    if (!isTRUE(input$show_model_spread)) {
      bands$intermod_lo <- NA_real_
      bands$intermod_hi <- NA_real_
    }
    plot_pointrange_climate(
      bands_tbl   = bands,
      x_label     = baseline_agg_hist()$x_label,
      group_order = input$cmp_group_order %||% "scenario_x_year",
      show_coef   = isTRUE(input$show_coef_uncertainty) && has_draws()
    )
  }, height = 600)
  outputOptions(output, "summary_box_plot", suspendWhenHidden = FALSE)

  output$summary_threshold_table <- DT::renderDT({
    req(threshold_table_rv())
    tbl <- threshold_table_rv()
    if (!isTRUE(input$show_model_spread))
      tbl <- tbl[!grepl("^Ensemble |^Total ", tbl$Estimate), , drop = FALSE]
    df <- build_threshold_table_df(
      threshold_tbl = tbl,
      group_order   = input$cmp_group_order %||% "scenario_x_year",
      show_coef     = isTRUE(input$show_coef_uncertainty) && has_draws()
    )
    if (is.null(df) || nrow(df) == 0L)
      return(DT::datatable(data.frame(Message = "Insufficient data"),
                           rownames = FALSE, class = "compact stripe",
                           options  = list(dom = "t")))
    DT::datatable(
      df, rownames = FALSE, class = "compact stripe",
      options = list(
        pageLength = 30, dom = "t", ordering = FALSE,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    )
  })
  outputOptions(output, "summary_threshold_table", suspendWhenHidden = FALSE)

  output$threshold_table_header <- renderUI({
    req(baseline_agg_hist())
    tagList(
      shiny::h4("Outcome value at return-period thresholds (both tails)"),
      shiny::tags$small(class = "text-muted", table_subtitle())
    )
  })

  output$threshold_table_footer <- renderUI({
    req(baseline_agg_hist())
    tagList(
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;",
        "Low odds show the value exceeded in only 1-in-N years."),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
        "High odds show the value reached in all but 1-in-N years."),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:0;",
        "1:1 shows the median (50th percentile) simulated value.")
    )
  })

  output$exceedance_plot <- renderPlot({
    req(exceedance_curves_rv())
    ens_q <- if (isTRUE(input$show_model_spread))
      resolve_band_q(input$ensemble_band %||% "minmax")
    else c(lo = 0.5, hi = 0.5)
    enhance_exceedance(
      curves_tbl      = exceedance_curves_rv(),
      x_label         = baseline_agg_hist()$x_label,
      return_period   = isTRUE(input$show_return_period),
      n_sim_years     = nrow(baseline_agg_hist()$out),
      logit_x         = isTRUE(input$exceedance_logit_x),
      band_q          = if (isTRUE(input$show_coef_uncertainty) && has_draws())
                          resolve_band_q(input$uncertainty_band %||% "p10_p90")
                        else NULL,
      ensemble_band_q = ens_q
    )
  })
  outputOptions(output, "exceedance_plot", suspendWhenHidden = FALSE)

  output$timeseries_plot <- renderPlot({
    req(timeseries_curves_rv())
    ens_q <- if (isTRUE(input$show_model_spread))
      resolve_band_q(input$ensemble_band %||% "minmax")
    else c(lo = 0.5, hi = 0.5)
    plot_timeseries_spaghetti(
      ts_tbl          = timeseries_curves_rv(),
      x_label         = baseline_agg_hist()$x_label,
      ensemble_band_q = ens_q
    )
  })
  outputOptions(output, "timeseries_plot", suspendWhenHidden = FALSE)

  output$exceedance_caption <- renderUI({
    req(baseline_agg_hist())
    axis_txt <- if (isTRUE(input$exceedance_logit_x))
      "Probability axis is logit-scaled, giving equal visual weight to both tails."
    else
      "The curve shows the estimated annual exceedance probability for each outcome value."
    tagList(
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:6px; margin-bottom:2px;",
        axis_txt),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:0; margin-bottom:2px;",
        "Low odds show the value exceeded in only 1-in-N years."),
      shiny::tags$p(
        style = "font-size:11px; color:#666; margin-top:0;",
        "High odds show the value reached in all but 1-in-N years.")
    )
  })

  invisible(NULL)
}


# ---------------------------------------------------------------------------- #
# Grouped point-range chart: baseline (grey) vs policy (red), side-by-side     #
# ---------------------------------------------------------------------------- #

#' Grouped Point-Range Chart Comparing Baseline and Policy Scenarios
#'
#' For each future scenario, draws two dots side-by-side:
#'   * Baseline (grey)
#'   * Policy   (red)
#' The Historical entry uses only the baseline dot. The legend at the top
#' of the chart distinguishes baseline vs policy.
#'
#' @param baseline_scenarios Named list; entries from aggregate_sim_preds()
#'   on the baseline survey frame. Should include a Historical entry.
#' @param policy_scenarios   Named list; entries from aggregate_sim_preds()
#'   on the policy-adjusted frame. Historical entry is ignored.
#' @param hist_agg           Historical aggregate (baseline) — used for the
#'   reference line and the y-axis label.
#' @param group_order        Ordering on the x-axis. Default
#'   \code{"scenario_x_year"}.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_linerange geom_point geom_hline
#'   scale_colour_manual scale_x_discrete labs theme_minimal theme
#'   element_blank element_text position_dodge
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @export
pol_plot_pointrange_climate <- function(baseline_scenarios,
                                        policy_scenarios,
                                        hist_agg,
                                        group_order = "scenario_x_year",
                                        band_q_coef = c(lo = 0.10, hi = 0.90),
                                        band_q_ens  = c(lo = 0.00, hi = 1.00),
                                        show_coef     = TRUE,
                                        show_intermod = TRUE) {

  stopifnot(is.list(hist_agg), all(c("out", "x_label") %in% names(hist_agg)))

  ssp_short_map <- c("SSP2-4.5" = "SSP2",
                     "SSP3-7.0" = "SSP3",
                     "SSP5-8.5" = "SSP5")
  z_coef_lo <- stats::qnorm(band_q_coef[["lo"]])
  z_coef_hi <- stats::qnorm(band_q_coef[["hi"]])

  # ---- helper: bands for one scenario's $out tibble -----------------------
  # `out` has at least `sim_year, value`. Future scenarios additionally carry
  # `value_p05` / `value_p95` (per-year coefficient band) and `model_values`
  # (per-year list of ensemble-member values). Historical lacks these.
  bands_one <- function(out, is_hist) {
    if (is.null(out) || nrow(out) == 0L) return(NULL)
    vals <- out$value
    ens_mean <- mean(vals, na.rm = TRUE)

    # Inter-annual: quantile across simulation years.
    interann <- c(
      lo = unname(stats::quantile(vals, band_q_ens[["lo"]], na.rm = TRUE)),
      hi = unname(stats::quantile(vals, band_q_ens[["hi"]], na.rm = TRUE))
    )

    # Inter-model: time-mean per model, then quantile across models.
    # Historical has no per-model decomposition → collapse to the dot.
    intermod <- c(lo = ens_mean, hi = ens_mean)
    if (!is_hist && "model_values" %in% names(out)) {
      mv <- out$model_values
      n_models <- max(vapply(mv, length, integer(1L)), 0L)
      if (n_models >= 2L) {
        mv_mat <- vapply(mv, function(v) {
          length(v) <- n_models  # pad with NA when membership varies
          as.numeric(v)
        }, numeric(n_models))
        # mv_mat is (n_models × n_years)
        model_means <- rowMeans(mv_mat, na.rm = TRUE)
        intermod <- c(
          lo = unname(stats::quantile(model_means, band_q_ens[["lo"]], na.rm = TRUE)),
          hi = unname(stats::quantile(model_means, band_q_ens[["hi"]], na.rm = TRUE))
        )
      }
    }

    # Coefficient: average the per-year coef band across years (typical
    # per-outcome SE) and re-centre on the ensemble mean.
    if (all(c("value_p05", "value_p95") %in% names(out))) {
      typical_lo <- mean(out$value_p05, na.rm = TRUE)
      typical_hi <- mean(out$value_p95, na.rm = TRUE)
      typical_se <- (typical_hi - typical_lo) /
                      (stats::qnorm(0.9) - stats::qnorm(0.1))
      coef <- c(lo = ens_mean + z_coef_lo * typical_se,
                hi = ens_mean + z_coef_hi * typical_se)
    } else {
      coef <- c(lo = ens_mean, hi = ens_mean)
    }

    # Total band assuming the three sources are independent:
    #   var_total = var_coef + var_within + var_across
    var_coef   <- ((coef[["hi"]] - coef[["lo"]]) /
                     (z_coef_hi - z_coef_lo))^2
    var_within <- stats::var(vals, na.rm = TRUE)
    if (is.na(var_within)) var_within <- 0
    var_across <- 0
    if (!is_hist && "model_values" %in% names(out) && exists("mv_mat")) {
      mm <- tryCatch(rowMeans(mv_mat, na.rm = TRUE), error = function(e) NULL)
      if (!is.null(mm) && length(mm) >= 2L) {
        v <- stats::var(mm, na.rm = TRUE)
        if (!is.na(v)) var_across <- v
      }
    }
    sd_total <- sqrt(max(var_coef + var_within + var_across, 0))
    total <- c(lo = ens_mean + z_coef_lo * sd_total,
               hi = ens_mean + z_coef_hi * sd_total)

    list(value = ens_mean,
         coef_lo = unname(coef[["lo"]]),  coef_hi = unname(coef[["hi"]]),
         interann_lo = unname(interann[["lo"]]),
         interann_hi = unname(interann[["hi"]]),
         intermod_lo = unname(intermod[["lo"]]),
         intermod_hi = unname(intermod[["hi"]]),
         total_lo    = unname(total[["lo"]]),
         total_hi    = unname(total[["hi"]]))
  }

  # ---- historical row (baseline only) -------------------------------------
  hist_b <- bands_one(hist_agg$out, is_hist = TRUE)
  if (is.null(hist_b)) {
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a simulation to see results."))
  }
  hist_mean <- hist_b$value
  hist_row  <- tibble::as_tibble(c(hist_b,
                                   list(pt_key = "Historical",
                                        source = "Baseline",
                                        ssp_short = NA_character_,
                                        yr = NA_character_)))

  # ---- future rows: one per (scenario, source) ----------------------------
  build_fut <- function(scenarios, source_label) {
    if (length(scenarios) == 0) return(NULL)
    future_nms <- names(scenarios)[vapply(names(scenarios),
      function(nm) !is.na(.normalise_ssp(nm)), logical(1))]
    if (length(future_nms) == 0) return(NULL)
    rows <- lapply(future_nms, function(nm) {
      b <- bands_one(scenarios[[nm]]$out, is_hist = FALSE)
      if (is.null(b)) return(NULL)
      ssp_key   <- .normalise_ssp(nm)
      ssp_short <- ssp_short_map[ssp_key] %||% ssp_key
      yr        <- .parse_year(nm)
      tibble::as_tibble(c(b,
                          list(pt_key = paste0(ssp_short, "\n", yr),
                               source = source_label,
                               ssp_short = ssp_short,
                               yr = yr)))
    })
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  base_fut <- build_fut(baseline_scenarios, "Baseline")
  pol_fut  <- build_fut(policy_scenarios,   "Policy")
  fut_df   <- dplyr::bind_rows(base_fut, pol_fut)

  # ---- x-axis factor levels with spacers ----------------------------------
  ordered_levels <- "Historical"
  spacer_ids     <- character(0)

  if (!is.null(fut_df) && nrow(fut_df) > 0) {
    if (isTRUE(group_order == "year_x_scenario")) {
      yrs_present  <- sort(unique(fut_df$yr))
      ssps_present <- intersect(c("SSP2", "SSP3", "SSP5"),
                                unique(fut_df$ssp_short))
      spacer_n <- 0L
      for (yr_i in yrs_present) {
        spacer_n       <- spacer_n + 1L
        sid            <- strrep(" ", spacer_n)
        spacer_ids     <- c(spacer_ids, sid)
        ordered_levels <- c(ordered_levels, sid)
        for (ssp in ssps_present) {
          ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
        }
      }
    } else {
      ssps_present <- intersect(c("SSP2", "SSP3", "SSP5"),
                                unique(fut_df$ssp_short))
      spacer_n <- 0L
      for (ssp in ssps_present) {
        spacer_n       <- spacer_n + 1L
        sid            <- strrep(" ", spacer_n)
        spacer_ids     <- c(spacer_ids, sid)
        ordered_levels <- c(ordered_levels, sid)
        ssp_yrs <- sort(unique(fut_df$yr[fut_df$ssp_short == ssp]))
        for (yr_i in ssp_yrs) {
          ordered_levels <- c(ordered_levels, paste0(ssp, "\n", yr_i))
        }
      }
    }
  }

  x_label_map <- setNames(
    vapply(ordered_levels, function(lv) {
      if (lv %in% spacer_ids) return("")
      lv
    }, character(1)),
    ordered_levels
  )
  data_levels <- setdiff(ordered_levels, spacer_ids)

  # ---- combine into one data frame ----------------------------------------
  plot_df <- dplyr::bind_rows(hist_row, fut_df)
  plot_df$pt_key <- factor(plot_df$pt_key, levels = ordered_levels)
  plot_df$source <- factor(plot_df$source, levels = c("Baseline", "Policy"))
  plot_df <- plot_df[plot_df$pt_key %in% data_levels, ]

  if (nrow(plot_df) == 0)
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a future simulation to see comparisons."))

  pos <- ggplot2::position_dodge(width = 0.55)

  p <- ggplot2::ggplot(plot_df,
         ggplot2::aes(x = .data$pt_key, y = .data$value,
                      colour = .data$source, group = .data$source))

  # Outermost: combined "total" whisker (all sources, independence).
  p <- p +
    ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$total_lo, ymax = .data$total_hi),
      linewidth = 0.6, alpha = 0.8, na.rm = TRUE, position = pos
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = .data$total_lo, ymax = .data$total_hi),
      width = 0.22, linewidth = 0.4, na.rm = TRUE, position = pos
    )

  # Inter-model spread (future only — historical collapses to the dot).
  if (isTRUE(show_intermod)) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$intermod_lo, ymax = .data$intermod_hi),
      linewidth = 6.0, alpha = 0.45, na.rm = TRUE, position = pos
    )
  }

  # Inter-annual variability across simulation years.
  p <- p + ggplot2::geom_linerange(
    ggplot2::aes(ymin = .data$interann_lo, ymax = .data$interann_hi),
    linewidth = 3.0, alpha = 0.9, na.rm = TRUE, position = pos
  )

  # Innermost: coefficient uncertainty (regression-fit SE).
  if (isTRUE(show_coef)) {
    p <- p + ggplot2::geom_linerange(
      ggplot2::aes(ymin = .data$coef_lo, ymax = .data$coef_hi),
      linewidth = 1.1, colour = "black", na.rm = TRUE, position = pos
    )
  }

  p +
    ggplot2::geom_point(
      shape = 21, fill = "white", size = 3, stroke = 1.4,
      position = pos
    ) +
    ggplot2::geom_hline(
      yintercept = hist_mean, linetype = "dashed",
      colour = "#808080", linewidth = 0.55
    ) +
    ggplot2::scale_colour_manual(
      values = c("Baseline" = "#808080", "Policy" = "#d32f2f"),
      name   = NULL, drop = FALSE
    ) +
    ggplot2::scale_x_discrete(
      limits = ordered_levels,
      labels = x_label_map,
      drop   = FALSE
    ) +
    ggplot2::labs(title = NULL, x = NULL, y = hist_agg$x_label) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = 10),
      legend.position    = "top"
    )
}

# ---------------------------------------------------------------------------- #
# Threshold table data frame: baseline + policy with Source column             #
# ---------------------------------------------------------------------------- #

#' Build Return-Period Threshold Table Data Frame (Baseline + Policy)
#'
#' Builds a single sorted data frame with one row per (scenario, source)
#' combination. The Historical entry is included only from the baseline
#' series. Future scenarios contribute two rows each: one Baseline, one
#' Policy.
#'
#' @param baseline_all_series Named list of aggregated baseline series
#'   (Historical + SSP scenarios). Each element must have \code{$out$value}.
#' @param policy_all_series   Named list of aggregated policy series. The
#'   Historical entry, if present, is skipped.
#' @param group_order Character. \code{"scenario_x_year"} (default) or
#'   \code{"year_x_scenario"}.
#'
#' @return A data frame with columns: Scenario, Source, Obs, one column
#'   per return-period threshold label. Returns NULL when every series has
#'   insufficient data.
#'
#' @importFrom stats quantile median
#' @export
pol_build_threshold_table_df <- function(baseline_all_series,
                                         policy_all_series,
                                         group_order = "scenario_x_year") {

  build_rows <- function(all_series, source_label, drop_historical = FALSE) {
    if (length(all_series) == 0) return(list())
    lapply(names(all_series), function(nm) {
      is_hist <- !grepl("^SSP", nm)
      if (drop_historical && is_hist) return(NULL)

      vals <- all_series[[nm]]$out$value
      vals <- vals[is.finite(vals)]
      n    <- length(vals)

      keep_low  <- names(RP_LOW)[c(n >= 50, n >= 20, n >= 10, n >= 5)]
      keep_high <- names(RP_HIGH)[c(n >= 5, n >= 10, n >= 20, n >= 50)]
      if (length(keep_low) == 0 && length(keep_high) == 0) return(NULL)

      low_df <- if (length(keep_low) > 0)
        as.data.frame(t(sapply(keep_low,
          function(th) round(stats::quantile(vals, RP_LOW[th]), 3))))
      else data.frame()
      high_df <- if (length(keep_high) > 0)
        as.data.frame(t(sapply(keep_high,
          function(th) round(stats::quantile(vals, RP_HIGH[th]), 3))))
      else data.frame()

      names(low_df)  <- keep_low
      names(high_df) <- keep_high
      median_df <- data.frame("1:1" = round(stats::median(vals), 3),
                              check.names = FALSE)

      cbind(
        data.frame(Scenario = nm, Source = source_label, Obs = n,
                   check.names = FALSE),
        low_df, median_df, high_df
      )
    })
  }

  # Baseline rows include Historical; policy rows skip it.
  rows_b <- Filter(Negate(is.null),
                   build_rows(baseline_all_series, "Baseline",
                              drop_historical = FALSE))
  rows_p <- Filter(Negate(is.null),
                   build_rows(policy_all_series, "Policy",
                              drop_historical = TRUE))
  rows <- c(rows_b, rows_p)
  if (length(rows) == 0) return(NULL)

  all_cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(r) {
    for (m in setdiff(all_cols, names(r))) r[[m]] <- NA_real_
    r[all_cols]
  })
  df <- do.call(rbind, rows)

  # Sort: Historical first, then SSP rows by group_order, then by Source.
  hist_rows <- df[!grepl("^SSP", df$Scenario), , drop = FALSE]
  ssp_rows  <- df[grepl("^SSP", df$Scenario), , drop = FALSE]

  if (nrow(ssp_rows) > 0) {
    ssp_rows$ssp_sort <- sub(" /.*", "", ssp_rows$Scenario)
    yr_m <- regexpr("[0-9]{4}-[0-9]{4}", ssp_rows$Scenario)
    ssp_rows$yr_sort  <- ifelse(
      yr_m > 0,
      regmatches(ssp_rows$Scenario, yr_m),
      regmatches(ssp_rows$Scenario, regexpr("[0-9]{4}", ssp_rows$Scenario))
    )
    pct_m <- regexpr("P([0-9]+)$", ssp_rows$Scenario)
    ssp_rows$pct_sort <- ifelse(
      pct_m > 0,
      as.integer(regmatches(ssp_rows$Scenario,
                            regexpr("[0-9]+$", ssp_rows$Scenario))),
      50L
    )
    ssp_rows <- if (isTRUE(group_order == "year_x_scenario"))
      ssp_rows[order(ssp_rows$yr_sort, ssp_rows$ssp_sort,
                     ssp_rows$pct_sort, ssp_rows$Source), ]
    else
      ssp_rows[order(ssp_rows$ssp_sort, ssp_rows$yr_sort,
                     ssp_rows$pct_sort, ssp_rows$Source), ]
    ssp_rows$ssp_sort <- NULL
    ssp_rows$yr_sort  <- NULL
    ssp_rows$pct_sort <- NULL
  }

  rbind(hist_rows, ssp_rows)
}

# ---------------------------------------------------------------------------- #
# Exceedance curve: baseline (transparent) + policy (opaque) overlays          #
# ---------------------------------------------------------------------------- #

#' Enhanced Exceedance Probability Curve \u2014 Baseline vs Policy
#'
#' Same colour-by-SSP / linetype-by-period scheme as the Step 2 exceedance
#' curve, with an additional alpha mapping for source: baseline is rendered
#' more transparent, policy fully opaque. The Historical entry is drawn
#' once from the baseline data only (policy adjustments do not apply to
#' historical predictions).
#'
#' @param baseline_scenarios Named list of aggregated baseline series
#'   (Historical + SSP scenarios).
#' @param policy_scenarios   Named list of aggregated policy series. The
#'   Historical entry, if present, is skipped.
#' @param hist_agg           Historical aggregate (baseline) \u2014 used only
#'   for the historical-mean reference line.
#' @param x_label            Axis label for the welfare outcome.
#' @param return_period      Logical. Show return period lines. Default TRUE.
#' @param n_sim_years        Integer. Triggers reliability annotation.
#' @param logit_x            Logical. Use logit scale on the probability
#'   axis. Default FALSE.
#' @return A ggplot object.
#' @importFrom ggplot2 ggplot aes geom_line geom_vline annotate
#'   labs theme_minimal theme scale_color_manual scale_linetype_manual
#'   scale_alpha_manual coord_flip guide_legend element_text
#' @importFrom scales logit_trans
#' @importFrom dplyr bind_rows
#' @importFrom rlang .data
#' @export
pol_enhance_exceedance <- function(baseline_scenarios,
                                   policy_scenarios,
                                   hist_agg,
                                   x_label,
                                   return_period = TRUE,
                                   n_sim_years   = NULL,
                                   logit_x       = FALSE) {

  stopifnot(is.list(baseline_scenarios), length(baseline_scenarios) > 0)

  build_long <- function(scenarios, source_label, drop_historical = FALSE) {
    if (length(scenarios) == 0) return(NULL)
    labels <- names(scenarios)
    rows <- lapply(seq_along(scenarios), function(i) {
      nm      <- labels[i]
      vals    <- scenarios[[nm]]$out$value
      ssp_key <- .normalise_ssp(nm)
      yr      <- .parse_year(nm)
      is_hist <- is.na(ssp_key)
      if (drop_historical && is_hist) return(NULL)
      data.frame(
        value   = vals,
        group   = nm,
        source  = source_label,
        ssp_key = if (is_hist) "Historical" else ssp_key,
        yr      = if (is.na(yr)) "Historical" else yr,
        stringsAsFactors = FALSE
      )
    })
    dplyr::bind_rows(Filter(Negate(is.null), rows))
  }

  base_long <- build_long(baseline_scenarios, "Baseline",
                          drop_historical = FALSE)
  pol_long  <- build_long(policy_scenarios,   "Policy",
                          drop_historical = TRUE)
  long_df   <- dplyr::bind_rows(base_long, pol_long)
  if (is.null(long_df) || nrow(long_df) == 0)
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a simulation to see exceedance probabilities."))

  long_df$source <- factor(long_df$source, levels = c("Baseline", "Policy"))

  # ---- Aesthetic mappings -------------------------------------------------
  all_yr_labels <- sort(unique(long_df$yr))
  yr_styles     <- .resolve_year_styles(all_yr_labels)
  ltype_map_yr  <- yr_styles$linetype_map
  present_yrs   <- names(ltype_map_yr)

  present_ssps <- sort(unique(long_df$ssp_key[long_df$ssp_key != "Historical"]))
  colour_map_ssp <- c(
    "Historical" = "black",
    .ssp_colours[intersect(names(.ssp_colours), present_ssps)]
  )

  hist_mean <- mean(hist_agg$out$value, na.rm = TRUE)
  eps       <- if (isTRUE(logit_x)) 0.005 else 0

  # ---- Build ECDF per (group, source) -------------------------------------
  grp_src_keys <- unique(paste(long_df$group, long_df$source, sep = "::"))
  ecdf_df <- dplyr::bind_rows(lapply(grp_src_keys, function(gs) {
    parts <- strsplit(gs, "::", fixed = TRUE)[[1]]
    grp   <- parts[1]
    src   <- parts[2]
    sub   <- long_df[long_df$group == grp & long_df$source == src &
                     is.finite(long_df$value), ]
    if (nrow(sub) == 0) return(NULL)
    fn     <- stats::ecdf(sub$value)
    xs     <- sort(unique(sub$value))
    exceed <- 1 - fn(xs)
    keep   <- exceed > eps & exceed < (1 - eps)
    if (!any(keep)) return(NULL)
    data.frame(
      value   = xs[keep],
      exceed  = exceed[keep],
      group   = grp,
      source  = src,
      ssp_key = sub$ssp_key[1],
      yr      = sub$yr[1],
      stringsAsFactors = FALSE
    )
  }))

  if (is.null(ecdf_df) || nrow(ecdf_df) == 0)
    return(ggplot2::ggplot() +
      ggplot2::labs(title = "Run a simulation to see exceedance probabilities."))

  ecdf_df$source  <- factor(ecdf_df$source, levels = c("Baseline", "Policy"))
  ecdf_df$line_id <- paste(ecdf_df$group, ecdf_df$source, sep = " \u2014 ")

  ann_y <- if (isTRUE(logit_x)) 0.97 else 0.95

  p <- ggplot2::ggplot(
    ecdf_df,
    ggplot2::aes(
      x        = .data$value,
      y        = .data$exceed,
      colour   = .data$ssp_key,
      linetype = .data$yr,
      alpha    = .data$source,
      group    = .data$line_id
    )
  ) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::geom_vline(
      xintercept = hist_mean, linetype = "dotted",
      colour = "black", linewidth = 0.5
    ) +
    ggplot2::annotate(
      "text", x = hist_mean, y = ann_y,
      label = "Hist. mean", hjust = 1.05, vjust = -0.4,
      size = 2.8, colour = "grey30"
    ) +
    ggplot2::scale_color_manual(
      values = colour_map_ssp,
      breaks = c("Historical", present_ssps),
      labels = c("Historical", present_ssps),
      name   = "Climate scenario",
      guide  = ggplot2::guide_legend(order = 1,
                                     override.aes = list(linewidth = 0.9))
    ) +
    ggplot2::scale_linetype_manual(
      values = ltype_map_yr,
      breaks = present_yrs,
      labels = present_yrs,
      name   = "Period",
      guide  = ggplot2::guide_legend(order = 2,
                                     override.aes = list(linewidth = 0.9))
    ) +
    ggplot2::scale_alpha_manual(
      values = c("Baseline" = 0.4, "Policy" = 1.0),
      breaks = c("Baseline", "Policy"),
      labels = c("Baseline", "Policy"),
      name   = "Source",
      guide  = ggplot2::guide_legend(order = 3,
                                     override.aes = list(linewidth = 0.9))
    ) +
    ggplot2::labs(
      title = "Exceedance probability by climate scenario",
      x     = x_label,
      y     = "Annual exceedance probability"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title      = ggplot2::element_text(face = "bold", size = 12,
                                              hjust = 0.5),
      legend.position = "bottom"
    ) +
    ggplot2::coord_flip()

  # ---- Return period lines (both tails, symmetric) -----------------------
  if (isTRUE(return_period)) {
    rp_all <- c(RP_LOW, RP_HIGH)
    for (nm in names(rp_all)) {
      prob      <- rp_all[nm]
      reliable  <- is.null(n_sim_years) ||
        (!(nm == "1:20" && n_sim_years < 40) &&
         !(nm == "1:50" && n_sim_years < 100))
      rp_label  <- if (reliable) nm else paste0(nm, "*")
      label_col <- if (reliable) "grey40" else "grey65"
      p <- p +
        ggplot2::geom_hline(
          yintercept = prob, linetype = "dashed",
          colour = "grey60", linewidth = 0.35
        ) +
        ggplot2::annotate(
          "text", x = -Inf, y = prob, label = rp_label,
          hjust = -0.1, vjust = -0.3, size = 2.8, colour = label_col
        )
    }
    if (!is.null(n_sim_years) && n_sim_years < 100) {
      p <- p + ggplot2::annotate(
        "text", x = Inf, y = 0.02,
        label  = paste0("\u26a0 unreliable (n = ", n_sim_years, " yrs)"),
        hjust  = 1.05, vjust = 1.5, size = 2.8, colour = "grey50"
      )
    }
  }

  if (isTRUE(logit_x)) {
    logit_breaks <- c(unname(RP_LOW), 0.50, rev(1 - unname(RP_LOW)))
    logit_labels <- c(names(RP_LOW), "Median", rev(names(RP_HIGH)))
    p <- p + ggplot2::scale_y_continuous(
      trans  = scales::logit_trans(),
      breaks = logit_breaks,
      labels = logit_labels,
      limits = c(0.005, 0.995)
    )
  }
  p
}