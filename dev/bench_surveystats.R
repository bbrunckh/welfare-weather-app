# Development-only Step 1 survey-stats benchmark.
#
# Benchmarks the summary-stats table build path behind mod_1_02_surveystats
# (weighted_summary_long, survey_missingness_long, and the table post-
# processing) on real local microdata. Default payload: Iran HEIS hh - the
# largest single-country N in the local store (8 waves, ~306k rows). Set
# WISEAPP_BENCH_CC=BFA for the smaller BFA payload (guidelines §9).
#
# Usage:
#   WISEAPP_DATA_PATH="~/Library/CloudStorage/OneDrive-WBG/wiseapp - Documents" \
#     Rscript dev/bench_surveystats.R
#   WISEAPP_BENCH_CC=BFA WISEAPP_DATA_PATH=... Rscript dev/bench_surveystats.R
#
# Alternatives benchmarked against the shipped collapse implementation:
#   - .wsl_nocopy : collapse grouped passes directly on the data.frame,
#                   masking columns in place (no as.matrix copy, no N x V
#                   mask matrix)
#   - .wsl_dt     : data.table grouped passes
#   - .wsl_base   : pre-PERF-33 style split() + per-(group, variable) passes
#   - .sml_dt     : data.table missingness (vs shipped collapse pass)
#   - .sml_base   : pre-PERF-09 style per-variable dplyr loop
#
# Parity of every candidate is checked against the shipped implementation
# before timing. A stress scale (4x Iran N) and a shared single-pass variant
# for the module's six tables are included.
#
# PERF-42/43 sections (one-pass display base, reactable build, wave-keyed
# density prep, echarts interview chart) compare the pre-optimization render
# paths with the implemented ones, with parity gates.
#
# With `WISEAPP_BENCH_DENSITY=1`, also benchmarks the W1-B density allocation
# on the audited two-wave Colombia workload. That mode checks schema, order,
# types, and values within 1e-12 before reporting timings and R allocations.

options(golem.app.prod = FALSE)
devtools::load_all(quiet = TRUE)

data_path <- Sys.getenv("WISEAPP_DATA_PATH", unset = "")
if (!nzchar(data_path)) stop(
  "bench_surveystats: Set WISEAPP_DATA_PATH to the local data folder ",
  "(e.g. ~/Library/CloudStorage/OneDrive-WBG/wiseapp - Documents)"
)
data_path <- normalizePath(data_path, mustWork = TRUE)
bench_cc <- toupper(Sys.getenv("WISEAPP_BENCH_CC", unset = "IRN"))

.density_reference <- function(cell_map, survey_data) {
  keys <- c("code", "year", "survname", "loc_id")
  cm <- cell_map
  cm$year <- as.character(cm$year)
  sd <- survey_data
  sd$year <- as.character(sd$year)
  n_loc <- sd |>
    dplyr::count(.data$code, .data$year, .data$survname, .data$loc_id,
                 name = "n_units")
  cm <- dplyr::inner_join(cm, n_loc, by = keys)
  if (!nrow(cm)) return(NULL)
  has_pop <- "pop_2020" %in% names(cm)
  cells <- cm |>
    dplyr::group_by(.data$code, .data$year, .data$survname, .data$loc_id) |>
    dplyr::mutate(.alloc = if (has_pop) {
      .pop <- pmax(.data$pop_2020, 0, na.rm = TRUE)
      .pop_sum <- sum(.pop)
      if (.pop_sum > 0) .data$n_units * .pop / .pop_sum else
        .data$n_units / dplyr::n()
    } else .data$n_units / dplyr::n()) |>
    dplyr::ungroup() |>
    dplyr::group_by(.data$h3) |>
    dplyr::summarise(n_units = sum(.data$.alloc, na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(.data$n_units > 0) |>
    as.data.frame()
  locs <- dplyr::inner_join(
    dplyr::distinct(sd, .data$code, .data$year, .data$survname, .data$loc_id),
    dplyr::distinct(cm, .data$code, .data$year, .data$survname, .data$loc_id),
    by = keys
  )
  list(cells = cells, n_locations = nrow(locs))
}

if (identical(Sys.getenv("WISEAPP_BENCH_DENSITY"), "1")) {
  years <- c(2008L, 2018L)
  survey <- dplyr::bind_rows(lapply(years, function(year) {
    arrow::read_parquet(
      file.path(data_path, "microdata/hh/COL", sprintf(
        "COL_%d_GEIH_GMD_hh.parquet", year
      )),
      col_select = c("code", "year", "survname", "loc_id")
    )
  }))
  cell_map <- dplyr::bind_rows(lapply(years, function(year) {
    arrow::read_parquet(file.path(data_path, "microdata/h3/COL", sprintf(
      "COL_%d_GEIH_GMD_h3.parquet", year
    )))
  }))

  implementation <- Sys.getenv("WISEAPP_BENCH_DENSITY_IMPL")
  if (implementation %in% c("reference", "collapse")) {
    gc()
    if (identical(implementation, "reference")) {
      invisible(.density_reference(cell_map, survey))
    } else {
      invisible(.density_cell_summary(cell_map, survey))
    }
    quit(save = "no", status = 0L)
  }

  reference <- .density_reference(cell_map, survey)
  candidate <- .density_cell_summary(cell_map, survey)
  stopifnot(
    identical(names(candidate$cells), names(reference$cells)),
    identical(vapply(candidate$cells, class, character(1)),
              vapply(reference$cells, class, character(1))),
    identical(candidate$cells$h3, reference$cells$h3),
    isTRUE(all.equal(candidate$cells$n_units, reference$cells$n_units,
                     tolerance = 1e-12)),
    identical(candidate$n_locations, reference$n_locations)
  )
  cat(sprintf(
    paste0("Density: %d survey rows, %d mapping rows, %d cells, ",
           "%d mapped locations, max abs diff %.3g\n"),
    nrow(survey), nrow(cell_map), nrow(candidate$cells), candidate$n_locations,
    max(abs(candidate$cells$n_units - reference$cells$n_units))
  ))
  print(bench::mark(
    reference = .density_reference(cell_map, survey),
    collapse  = .density_cell_summary(cell_map, survey),
    iterations = 10L, check = FALSE, memory = TRUE, filter_gc = FALSE
  )[, c("expression", "median", "mem_alloc", "n_gc")])
  quit(save = "no", status = 0L)
}

# ---------------------------------------------------------------------------
# 1. Load survey data through the module's own pipeline
# ---------------------------------------------------------------------------

params <- list(type = "local", path = data_path)
meta   <- load_overview_metadata(params)

files <- sort(list.files(
  file.path(data_path, "microdata/hh", bench_cc),
  full.names = TRUE, pattern = "parquet$"
))
stopifnot(length(files) > 0)

df <- load_data(files, params, collect = TRUE, unify_schemas = TRUE)
df <- add_time_columns(df)
lcu_vars <- get_lcu_vars(df, meta$variable_list)
df <- df |>
  assign_data_level() |>
  convert_lcu_to_ppp(meta$cpi_ppp, lcu_vars) |>
  bottom_code_welfare(0.28) |>
  apply_policy_derivations()

vl <- meta$variable_list

# Var sets exactly as mod_1_02_surveystats builds them.
var_sets <- list(
  outcome = vl$name[vl$outcome == 1],
  ind     = vl$name[vl$ind == 1],
  hh      = vl$name[vl$hh == 1],
  firm    = vl$name[vl$firm == 1],
  area    = vl$name[vl$area == 1],
  policy  = unique(unlist(lapply(POLICY_DEFINITIONS, `[[`, "vars")))
)
var_sets <- lapply(var_sets, function(vs) intersect(vs, names(df)))
var_sets <- var_sets[lengths(var_sets) > 0]

cat(sprintf(
  "\n%s hh: %d rows x %d cols, %d waves, var sets: %s\n",
  bench_cc, nrow(df), ncol(df), length(unique(df$countryyear)),
  paste(names(var_sets), vapply(var_sets, length, integer(1)),
        sep = "=", collapse = "  ")
))

# ---------------------------------------------------------------------------
# 2. Alternative implementations
# ---------------------------------------------------------------------------

na_nan <- function(x) { x[is.nan(x)] <- NA_real_; x }

# Guard + filter preamble shared by all weighted candidates.
.wsl_prep <- function(df, vars, group, weight) {
  if (!length(vars)) return(NULL)
  if (!all(c(group, weight) %in% names(df))) return(NULL)
  df <- df[, unique(c(group, weight, vars)), drop = FALSE]
  vars <- intersect(vars, names(df))
  vars <- vars[vapply(df[vars], is.numeric, logical(1))]
  if (!length(vars)) return(NULL)
  df <- df[!is.na(df[[group]]), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  list(df = df, vars = vars)
}

# Variant A: collapse on the data.frame, per-column masking - no as.matrix
# copy, no N x V mask matrix.
.wsl_nocopy <- function(df, vars, group = "countryyear", weight = "weight") {
  p <- .wsl_prep(df, vars, group, weight)
  if (is.null(p)) return(data.frame())
  df <- p$df; vars <- p$vars

  g   <- collapse::GRP(df, by = group)
  n_g <- g$N.groups
  w   <- as.numeric(df[[weight]])
  w_ok <- is.finite(w) & (w > 0)
  w[!w_ok] <- NA_real_

  X <- df[vars]
  for (v in vars) {
    x <- X[[v]]
    bad <- !(is.finite(x) & w_ok)
    if (any(bad)) x[bad] <- NA_real_
    X[[v]] <- x
  }

  # collapse returns a data.frame for data.frame input; the G x V result is
  # tiny, so as.matrix() before as.vector() is free.
  av <- function(m) as.vector(as.matrix(m))
  res <- data.frame(
    countryyear     = as.character(rep(g$groups[[1]], times = length(vars))),
    variable        = rep(vars, each = n_g),
    unweighted_mean = na_nan(av(collapse::fmean(X, g = g, na.rm = TRUE))),
    Mean            = na_nan(av(collapse::fmean(X, g = g, w = w, na.rm = TRUE))),
    `Std. Dev.`     = na_nan(av(collapse::fsd(X, g = g, w = w, na.rm = TRUE))),
    Min             = na_nan(av(collapse::fmin(X, g = g, na.rm = TRUE))),
    Max             = na_nan(av(collapse::fmax(X, g = g, na.rm = TRUE))),
    N               = as.integer(av(collapse::fnobs(X, g = g))),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  res <- res[order(res$countryyear), ]
  rownames(res) <- NULL
  res
}

# Variant B: data.table grouped passes.
.wsl_dt <- function(df, vars, group = "countryyear", weight = "weight") {
  p <- .wsl_prep(df, vars, group, weight)
  if (is.null(p)) return(data.frame())
  dt <- data.table::as.data.table(p$df)
  vars <- p$vars

  w_ok <- is.finite(dt[[weight]]) & (dt[[weight]] > 0)
  data.table::set(dt, j = weight,
                  value = replace(dt[[weight]], !w_ok, NA_real_))
  for (v in vars) {
    x <- dt[[v]]
    bad <- !(is.finite(x) & w_ok)
    if (any(bad)) data.table::set(dt, j = v, value = replace(x, bad, NA_real_))
  }

  wname <- weight
  # FUN(x, w); w is fetched per group directly in the j env (get() inside a
  # closure would resolve the function argument, not the column).
  grp_pass <- function(FUN) {
    out <- dt[, {
      w <- get(wname)
      lapply(.SD, function(x) FUN(x, w))
    }, by = group, .SDcols = vars]
    gv <- as.character(out[[group]])
    m  <- as.matrix(out[, vars, with = FALSE])
    dimnames(m) <- list(gv, vars)
    m
  }

  gmean_u <- grp_pass(function(x, w) {
    ok <- !is.na(x); if (!any(ok)) return(NaN); mean(x[ok])
  })
  gmean_w <- grp_pass(function(x, w) {
    ok <- !is.na(x); xo <- x[ok]; wo <- w[ok]
    sw <- sum(wo)
    if (!length(xo) || !is.finite(sw) || sw <= 0) return(NaN)
    sum(xo * wo) / sw
  })
  gsd_w <- grp_pass(function(x, w) {
    ok <- !is.na(x); xo <- x[ok]; wo <- w[ok]
    sw <- sum(wo)
    if (length(xo) < 2 || !is.finite(sw) || sw - 1 <= 0) return(NA_real_)
    m <- sum(xo * wo) / sw
    sqrt(sum(wo * (xo - m)^2) / (sw - 1))
  })
  gmin <- grp_pass(function(x, w) {
    ok <- !is.na(x); if (!any(ok)) return(NaN); min(x[ok])
  })
  gmax <- grp_pass(function(x, w) {
    ok <- !is.na(x); if (!any(ok)) return(NaN); max(x[ok])
  })
  gn <- grp_pass(function(x, w) sum(!is.na(x)))

  gv <- rownames(gmean_u)
  res <- data.frame(
    countryyear     = as.character(rep(gv, times = length(vars))),
    variable        = rep(vars, each = nrow(gmean_u)),
    unweighted_mean = na_nan(as.vector(gmean_u)),
    Mean            = na_nan(as.vector(gmean_w)),
    `Std. Dev.`     = na_nan(as.vector(gsd_w)),
    Min             = na_nan(as.vector(gmin)),
    Max             = na_nan(as.vector(gmax)),
    N               = as.integer(as.vector(gn)),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  res <- res[order(res$countryyear), ]
  rownames(res) <- NULL
  res
}

# Variant C: pre-PERF-33 baseline - split() + per-(group, variable) passes.
.wsl_base <- function(df, vars, group = "countryyear", weight = "weight") {
  p <- .wsl_prep(df, vars, group, weight)
  if (is.null(p)) return(data.frame())
  df <- p$df; vars <- p$vars

  idx_by_g <- split(seq_len(nrow(df)), df[[group]])
  rows <- lapply(names(idx_by_g), function(gv) {
    idx <- idx_by_g[[gv]]
    w <- as.numeric(df[[weight]][idx])
    w_ok <- is.finite(w) & (w > 0)
    do.call(rbind, lapply(vars, function(v) {
      x <- df[[v]][idx]
      ok <- is.finite(x) & w_ok
      xo <- x[ok]; wo <- w[ok]
      n  <- length(xo)
      fx <- is.finite(x)
      m_u <- if (any(fx)) mean(x[fx]) else NaN
      if (n) {
        m   <- sum(xo * wo) / sum(wo)
        sw  <- sum(wo)
        sdv <- if (n > 1 && sw > 1) sqrt(sum(wo * (xo - m)^2) / (sw - 1)) else NA_real_
        mnv <- min(xo); mxv <- max(xo)
      } else {
        m <- NaN; sdv <- NA_real_; mnv <- NaN; mxv <- NaN
      }
      data.frame(countryyear = gv, variable = v, unweighted_mean = m_u,
                 Mean = m, `Std. Dev.` = sdv, Min = mnv, Max = mxv, N = n,
                 check.names = FALSE, stringsAsFactors = FALSE)
    }))
  })
  out <- do.call(rbind, rows)
  out <- out[order(out$countryyear), ]
  rownames(out) <- NULL
  out
}

# Missingness variant: data.table.
.sml_dt <- function(df, vars, group = "countryyear") {
  vars <- intersect(vars, names(df))
  vars <- vars[vapply(df[vars], function(x) !is.list(x), logical(1))]
  if (!length(vars) || !group %in% names(df)) {
    return(data.frame(countryyear = character(), variable = character(),
                      `% Missing` = numeric(), check.names = FALSE,
                      stringsAsFactors = FALSE))
  }
  dt <- data.table::as.data.table(df[, c(group, vars), drop = FALSE])
  out <- dt[, lapply(.SD, function(x) 100 * mean(is.na(x))),
            by = group, .SDcols = vars]
  gv   <- as.character(out[[group]])
  wide <- as.matrix(out[, vars, with = FALSE])
  res <- data.frame(
    countryyear = rep(gv, times = length(vars)),
    variable    = rep(vars, each = length(gv)),
    `% Missing` = as.vector(wide),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  res <- res[order(res$countryyear), ]
  rownames(res) <- NULL
  res
}

# Missingness baseline: pre-PERF-09 per-variable dplyr loop.
.sml_base <- function(df, vars, group = "countryyear") {
  vars <- intersect(vars, names(df))
  vars <- vars[vapply(df[vars], function(x) !is.list(x), logical(1))]
  if (!length(vars) || !group %in% names(df)) {
    return(data.frame(countryyear = character(), variable = character(),
                      `% Missing` = numeric(), check.names = FALSE,
                      stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, lapply(vars, function(v) {
    df |>
      dplyr::group_by(.data[[group]]) |>
      dplyr::summarise(
        variable   = v,
        `% Missing` = 100 * mean(is.na(.data[[v]])),
        .groups = "drop"
      )
  }))
  names(out)[names(out) == group] <- "countryyear"
  out <- out[order(out$countryyear, out$variable), ]
  rownames(out) <- NULL
  out
}

# ---------------------------------------------------------------------------
# 3. Parity checks (shipped implementation is the reference)
# ---------------------------------------------------------------------------

.canon <- function(tab) {
  tab <- tab[order(tab$variable, tab$countryyear), , drop = FALSE]
  rownames(tab) <- NULL
  tab
}

check_parity <- function(name, ref, cand, tol = 1e-8) {
  ref <- .canon(ref); cand <- .canon(cand)
  ok <- TRUE
  if (!identical(dim(ref), dim(cand))) {
    cat(sprintf("  [FAIL] %s: dims %s vs %s\n", name,
                paste(dim(ref), collapse = "x"),
                paste(dim(cand), collapse = "x")))
    return(FALSE)
  }
  if (!identical(ref$variable, cand$variable) ||
      !identical(ref$countryyear, cand$countryyear)) {
    cat(sprintf("  [FAIL] %s: keys differ\n", name)); ok <- FALSE
  }
  for (col in c("unweighted_mean", "Mean", "Min", "Max")) {
    eq <- isTRUE(all.equal(ref[[col]], cand[[col]], tolerance = tol,
                           check.attributes = FALSE))
    if (!eq) { cat(sprintf("  [FAIL] %s: %s\n", name, col)); ok <- FALSE }
  }
  eq_sd <- isTRUE(all.equal(ref[["Std. Dev."]], cand[["Std. Dev."]],
                            tolerance = tol, check.attributes = FALSE))
  if (!eq_sd) { cat(sprintf("  [FAIL] %s: Std. Dev.\n", name)); ok <- FALSE }
  eq_n <- identical(as.numeric(ref$N), as.numeric(cand$N))
  if (!eq_n) { cat(sprintf("  [FAIL] %s: N\n", name)); ok <- FALSE }
  if (ok) cat(sprintf("  [ok]   %s parity\n", name))
  ok
}

check_parity_miss <- function(name, ref, cand, tol = 1e-8) {
  ref <- .canon(ref); cand <- .canon(cand)
  ok <- identical(dim(ref), dim(cand)) &&
    identical(ref$variable, cand$variable) &&
    identical(ref$countryyear, cand$countryyear) &&
    isTRUE(all.equal(ref[["% Missing"]], cand[["% Missing"]],
                     tolerance = tol, check.attributes = FALSE))
  cat(sprintf("  [%s] %s missingness parity\n", if (ok) "ok" else "FAIL", name))
  ok
}

vars_bench <- var_sets$hh
cat("\nParity vs shipped implementation (hh var set):\n")
ref  <- weighted_summary_long(df, vars = vars_bench)
ok_nocopy <- check_parity("wsl_nocopy", ref, .wsl_nocopy(df, vars = vars_bench))
ok_dt     <- check_parity("wsl_dt",     ref, .wsl_dt(df, vars = vars_bench))
ok_base   <- check_parity("wsl_base",   ref, .wsl_base(df, vars = vars_bench))
ref_m <- survey_missingness_long(df, vars = vars_bench)
ok_sml_dt   <- check_parity_miss("sml_dt",   ref_m, .sml_dt(df, vars = vars_bench))
ok_sml_base <- check_parity_miss("sml_base", ref_m, .sml_base(df, vars = vars_bench))

# ---------------------------------------------------------------------------
# 4. Aggregate benchmarks (bench::mark, medians + memory)
# ---------------------------------------------------------------------------

fmt_bench <- function(bm) {
  bm$expression <- vapply(bm$expression, function(e) deparse(e)[1], character(1))
  bm[, c("expression", "min", "median", "itr/sec", "mem_alloc", "n_itr")]
}

cat("\n== weighted_summary_long, Iran hh vars (", length(vars_bench),
    "vars, ", nrow(df), "rows) ==\n", sep = "")
bm_hh <- bench::mark(
  shipped = weighted_summary_long(df, vars = vars_bench),
  nocopy  = .wsl_nocopy(df, vars = vars_bench),
  dt      = .wsl_dt(df, vars = vars_bench),
  base    = .wsl_base(df, vars = vars_bench),
  min_time = 1, check = FALSE
)
print(fmt_bench(bm_hh))

for (nm in setdiff(names(var_sets), "hh")) {
  vs <- var_sets[[nm]]
  cat("\n== weighted_summary_long,", nm, "vars (", length(vs), "vars) ==\n")
  bm <- bench::mark(
    shipped = weighted_summary_long(df, vars = vs),
    nocopy  = .wsl_nocopy(df, vars = vs),
    dt      = .wsl_dt(df, vars = vs),
    min_time = 1, check = FALSE
  )
  print(fmt_bench(bm))
}

cat("\n== survey_missingness_long, hh vars ==\n")
bm_miss <- bench::mark(
  shipped = survey_missingness_long(df, vars = vars_bench),
  dt      = .sml_dt(df, vars = vars_bench),
  base    = .sml_base(df, vars = vars_bench),
  min_time = 1, check = FALSE
)
print(fmt_bench(bm_miss))

# ---------------------------------------------------------------------------
# 5. Render-side segment timing (make_stats_dt post-aggregation)
# ---------------------------------------------------------------------------

segment_pipeline <- function(df, vl, vars) {
  tt <- numeric(0)
  tt["aggregate"] <- system.time(tab0 <- weighted_summary_long(df, vars = vars))[["elapsed"]]
  tt["missingness"] <- system.time(fill_df <- survey_missingness_long(df, vars))[["elapsed"]]
  tt["join_missing"] <- system.time(
    tab <- dplyr::left_join(tab0, fill_df, by = c("countryyear", "variable"))
  )[["elapsed"]]
  lab_map <- vl[, c("name", "label"), drop = FALSE]
  tt["join_labels"] <- system.time(
    tab <- tab |>
      dplyr::left_join(lab_map, by = c("variable" = "name")) |>
      dplyr::mutate(variable = dplyr::coalesce(.data$label, .data$variable)) |>
      dplyr::select(variable, dplyr::everything(), -dplyr::any_of("label"))
  )[["elapsed"]]
  tt["filter_arrange"] <- system.time(
    tab <- tab |>
      dplyr::filter(is.na(.data$N) | .data$N > 0) |>
      dplyr::select(-dplyr::any_of("unweighted_mean")) |>
      dplyr::arrange(.data$variable, .data$countryyear)
  )[["elapsed"]]
  tt["rename_wrap"] <- system.time({
    if ("countryyear" %in% names(tab))
      names(tab)[names(tab) == "countryyear"] <- "Country, Year"
    names(tab) <- vapply(names(tab), function(nm) {
      if (!nzchar(nm)) return(nm)
      paste0(toupper(substr(nm, 1, 1)), substr(nm, 2, nchar(nm)))
    }, character(1))
    wrap_width <- 28
    text_cols <- names(tab)[vapply(tab, function(x)
      is.character(x) || is.factor(x), logical(1))]
    if (length(text_cols) > 0) {
      tab[text_cols] <- lapply(tab[text_cols], function(x) {
        x_chr <- as.character(x)
        vapply(x_chr, function(s) {
          if (is.na(s)) return(NA_character_)
          lines <- strwrap(s, width = wrap_width)
          paste(htmltools::htmlEscape(lines), collapse = "<br>")
        }, character(1))
      })
    }
  })[["elapsed"]]
  tt["datatable"] <- system.time(
    dt <- DT::datatable(
      tab, rownames = FALSE, escape = FALSE,
      options = list(
        autoWidth = TRUE, pageLength = 10,
        columnDefs = list(list(className = "dt-wrap", targets = "_all"))
      )
    )
  )[["elapsed"]]
  num_cols <- names(tab)[vapply(tab, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, "N")
  tt["formatround"] <- if (length(num_cols) > 0) system.time(
    dt <- DT::formatRound(dt, columns = num_cols, digits = 2)
  )[["elapsed"]] else 0
  list(segments = tt, rows = nrow(tab))
}

cat("\n== make_stats_dt segment timing, hh vars (median of 10) ==\n")
reps <- 10
seg_mat <- NULL; rows_out <- NA
for (i in seq_len(reps)) {
  res_i <- segment_pipeline(df, vl, vars_bench)
  seg_mat <- rbind(seg_mat, res_i$segments)
  rows_out <- res_i$rows
}
seg_med <- apply(seg_mat, 2, median)
seg_tot <- sum(seg_med)
cat(sprintf("table rows: %d\n", rows_out))
for (s in names(seg_med)) {
  cat(sprintf("  %-15s %8.1f ms  (%4.1f%%)\n",
              s, seg_med[s] * 1000, 100 * seg_med[s] / seg_tot))
}
cat(sprintf("  %-15s %8.1f ms\n", "TOTAL", seg_tot * 1000))

# ---------------------------------------------------------------------------
# 6. Whole-click cost: per-table pipelines vs the implemented shared base
# ---------------------------------------------------------------------------

cat("\n== Whole-click cost: per-table pipelines vs shared union pass ==\n")
tbl_pipes <- function() {
  for (nm in names(var_sets)) segment_pipeline(df, vl, var_sets[[nm]])
  invisible(NULL)
}
t_per_table <- bench::mark(
  per_table = tbl_pipes(), min_time = 1, iterations = 5
)

# The implemented module path (PERF-40): one union pass into the shared
# base, then row-filtered frames per table.
module_click <- function() {
  policy_vars <- unique(unlist(lapply(POLICY_DEFINITIONS, `[[`, "vars")))
  targets <- unlist(lapply(c("outcome", "ind", "hh", "firm", "area"), function(fc) {
    if (fc %in% names(vl)) vl$name[vl[[fc]] == 1] else character(0)
  }), use.names = FALSE)
  union_vars <- intersect(unique(c(targets, policy_vars)), names(df))
  base <- list(
    vars    = union_vars,
    summary = weighted_summary_long(df, vars = union_vars),
    missing = survey_missingness_long(df, vars = union_vars)
  )
  for (nm in names(var_sets)) {
    stats_table_frame(df, vl, vars = var_sets[[nm]], base = base)
  }
  invisible(NULL)
}

# Parity on real data: every shared-base frame must equal its standalone
# per-table aggregation.
base_real <- NULL
module_click_parity <- function() {
  policy_vars <- unique(unlist(lapply(POLICY_DEFINITIONS, `[[`, "vars")))
  targets <- unlist(lapply(c("outcome", "ind", "hh", "firm", "area"), function(fc) {
    if (fc %in% names(vl)) vl$name[vl[[fc]] == 1] else character(0)
  }), use.names = FALSE)
  union_vars <- intersect(unique(c(targets, policy_vars)), names(df))
  base_real <<- list(
    vars    = union_vars,
    summary = weighted_summary_long(df, vars = union_vars),
    missing = survey_missingness_long(df, vars = union_vars)
  )
  for (nm in names(var_sets)) {
    stopifnot(identical(
      stats_table_frame(df, vl, vars = var_sets[[nm]]),
      stats_table_frame(df, vl, vars = var_sets[[nm]], base = base_real)
    ))
  }
  invisible(NULL)
}
module_click_parity()
cat(sprintf("  shared-base vs standalone parity on %s data: [ok]\n", bench_cc))

t_module <- bench::mark(
  module_click = module_click(), min_time = 1, iterations = 5
)
cat(sprintf("  per-table (independent pipelines): %8.1f ms median\n",
            median(t_per_table$median) * 1000))
cat(sprintf("  shared union pass (implemented):    %8.1f ms median\n",
            median(t_module$median) * 1000))

# ---------------------------------------------------------------------------
# 7. Stress: 4x Iran N
# ---------------------------------------------------------------------------

cat("\n== Stress: 4x N (", nrow(df), "-> ",
    4 * nrow(df), "rows, hh vars) ==\n", sep = "")
big_sub <- df[, unique(c("countryyear", "weight", vars_bench)), drop = FALSE]
big_sub <- big_sub[rep(seq_len(nrow(big_sub)), times = 4), , drop = FALSE]
big_df  <- df[rep(seq_len(nrow(df)), times = 4), , drop = FALSE]
bm_stress <- bench::mark(
  shipped = weighted_summary_long(big_df, vars = vars_bench),
  nocopy  = .wsl_nocopy(big_df, vars = vars_bench),
  dt      = .wsl_dt(big_df, vars = vars_bench),
  min_time = 1, iterations = 5, check = FALSE
)
print(fmt_bench(bm_stress))
rm(big_df, big_sub); invisible(gc())

# ---------------------------------------------------------------------------
# 8. PERF-42: one-pass display base vs per-table stats_table_frame
# ---------------------------------------------------------------------------

cat("\n== PERF-42 display path: per-table post-processing vs base + slices ==\n")
module_union <- function() {
  policy_vars <- unique(unlist(lapply(POLICY_DEFINITIONS, `[[`, "vars")))
  targets <- unlist(lapply(c("outcome", "ind", "hh", "firm", "area"), function(fc) {
    if (fc %in% names(vl)) vl$name[vl[[fc]] == 1] else character(0)
  }), use.names = FALSE)
  intersect(unique(c(targets, policy_vars)), names(df))
}
union_vars_bench <- module_union()

old_display_path <- function() {
  for (nm in names(var_sets)) {
    stats_table_frame(df, vl, vars = var_sets[[nm]],
      base = list(
        vars = union_vars_bench,
        summary = weighted_summary_long(df, vars = union_vars_bench),
        missing = survey_missingness_long(df, vars = union_vars_bench)
      ))
  }
  invisible(NULL)
}
new_display_path <- function() {
  base <- stats_display_base(df, vl, union_vars_bench)
  for (nm in names(var_sets)) stats_display_slice(base, vl, vars = var_sets[[nm]])
  invisible(NULL)
}

# Parity: every new slice must equal its old frame exactly.
base_par <- stats_display_base(df, vl, union_vars_bench)
for (nm in names(var_sets)) {
  stopifnot(identical(
    stats_table_frame(df, vl, vars = var_sets[[nm]]),
    stats_display_slice(base_par, vl, vars = var_sets[[nm]])
  ))
}
cat("  display parity (all var sets, standalone vs base slice): [ok]\n")

bm_disp <- bench::mark(
  old_per_table  = old_display_path(),
  new_base_slice = new_display_path(),
  min_time = 1, iterations = 10, check = FALSE
)
print(fmt_bench(bm_disp))

# ---------------------------------------------------------------------------
# 9. Widget build: DT::datatable + formatRound vs reactable
# ---------------------------------------------------------------------------

cat("\n== Widget build cost, hh table (", nrow(stats_table_frame(df, vl, vars = var_sets$hh)), "rows) ==\n", sep = "")
tab_hh <- stats_table_frame(df, vl, vars = var_sets$hh)
build_dt <- function(tab) {
  dt <- DT::datatable(tab, rownames = FALSE, escape = FALSE,
    options = list(autoWidth = TRUE, pageLength = 10))
  num_cols <- setdiff(names(tab)[vapply(tab, is.numeric, logical(1))], "N")
  if (length(num_cols)) dt <- DT::formatRound(dt, columns = num_cols, digits = 2)
  dt
}
bm_widget <- bench::mark(
  dt        = build_dt(tab_hh),
  reactable = .stats_reactable(tab_hh),
  min_time = 1, iterations = 10, check = FALSE
)
print(fmt_bench(bm_widget))

# ---------------------------------------------------------------------------
# 10. PERF-43: density wave prep vs per-toggle recompute
# ---------------------------------------------------------------------------

cat("\n== PERF-43 density: per-toggle .density_cell_summary vs wave prep + slice ==\n")
h3_files <- sort(list.files(
  file.path(data_path, "microdata/h3", bench_cc),
  full.names = TRUE, pattern = "parquet$"
))
h3_cols <- c("code", "year", "survname", "loc_id", "h3", "pop_2020")
cell_map_bench <- tryCatch(
  load_data(h3_files, params, collect = TRUE),
  error = function(e) NULL
)
if (!is.null(cell_map_bench) && nrow(cell_map_bench) &&
  all(h3_cols %in% names(cell_map_bench))) {
  cell_map_bench <- cell_map_bench[, h3_cols]
  wave_keys <- paste(cell_map_bench$code, as.character(cell_map_bench$year),
    cell_map_bench$survname, sep = "|"
  )
  df_wave_keys <- paste(df$code, as.character(df$year), df$survname, sep = "|")
  uniq_waves <- unique(wave_keys)

  # Old per-toggle cost: filter both frames, full allocation. (The old path
  # also re-joined geometry strings per toggle; that join is pure overhead the
  # new path drops, so it is excluded here - the comparison is conservative.)
  old_toggle <- function(wave) {
    cm_w <- cell_map_bench[wave_keys == wave | wave == "all", , drop = FALSE]
    sd_w <- df[df_wave_keys == wave | wave == "all", , drop = FALSE]
    .density_cell_summary(cm_w, sd_w)
  }

  # New toggle cost: slice the precomputed wave-keyed cells (+ pooled sum).
  wave_prep <- .density_wave_summary(cell_map_bench, df)
  new_toggle <- function(wave) {
    if (identical(wave, "all")) {
      g <- collapse::GRP(wave_prep$cells, by = "h3")
      cells <- data.frame(
        h3 = as.character(g$groups$h3),
        n_units = collapse::fsum(wave_prep$cells$n_units, g = g, na.rm = TRUE)
      )
    } else {
      cells <- wave_prep$cells[wave_prep$cells$wave %in% wave, c("h3", "n_units"), drop = FALSE]
    }
    cells
  }

  # Parity: every wave's cells and the pooled-all cells.
  max_diff <- 0
  for (w in uniq_waves) {
    o <- old_toggle(w); n <- new_toggle(w)
    o <- o$cells[order(o$cells$h3), ]
    n <- n[order(n$h3), ]
    stopifnot(identical(o$h3, n$h3))
    max_diff <- max(max_diff, max(abs(o$n_units - n$n_units)))
  }
  o_all <- old_toggle("all")$cells
  o_all <- o_all[order(o_all$h3), ]
  n_all <- new_toggle("all")
  n_all <- n_all[order(n_all$h3), ]
  stopifnot(identical(o_all$h3, n_all$h3))
  pooled_diff <- max(abs(o_all$n_units - n_all$n_units))
  cat(sprintf(
    "  parity: %d waves exact (max abs diff %.3g), pooled-all diff %.3g\n",
    length(uniq_waves), max_diff, pooled_diff
  ))

  prep_once <- bench::mark(
    wave_prep = .density_wave_summary(cell_map_bench, df),
    old_toggle = old_toggle(uniq_waves[[1]]),
    new_toggle = new_toggle(uniq_waves[[1]]),
    min_time = 1, iterations = 10, check = FALSE
  )
  print(fmt_bench(prep_once))
  cat(sprintf("  waves in payload: %d (toggle cost repeats per wave pick)\n",
              length(uniq_waves)))
} else {
  cat("  h3 parquet unavailable or schema mismatch; density bench skipped\n")
}

# ---------------------------------------------------------------------------
# 11. Interview-dates chart build: ggplot vs echarts4r
# ---------------------------------------------------------------------------

cat("\n== Interview-dates chart build (R-side; browser interaction offloaded) ==\n")
interview_plot_data <- summarise_interview_dates(df)
stopifnot(nrow(interview_plot_data) > 0)
# memory = FALSE: htmlwidget assembly trips bench's memory profiler
# (foreground thread finalizers) on this platform.
bm_chart <- bench::mark(
  ggplot   = plot_interview_dates(interview_plot_data, palette = "sequential"),
  echarts  = echart_interview_dates(interview_plot_data, unit_label = "Households"),
  min_time = 1, iterations = 10, check = FALSE, memory = FALSE
)
print(fmt_bench(bm_chart))

cat("\nDone. Parity: nocopy=", ok_nocopy, " dt=", ok_dt, " base=", ok_base,
    " sml_dt=", ok_sml_dt, " sml_base=", ok_sml_base, "\n", sep = "")
