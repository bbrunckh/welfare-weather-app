# Benchmark: R-side builder cost, ggplot vs echarts (guidelines §7 migration).
#
# Measures the R-side cost of building the Step 1 coefficient plot and the
# continuous (poly + moderated) weather effect plot with both the canonical
# ggplot builders and their echarts4r counterparts, on synthetic data shaped
# like a real fit snapshot (~5k households, 1 weather var with quadratic
# terms, an interaction, controls and a fixed effect).
#
# Run: Rscript dev/bench_ui_step1.R

pkgload::load_all(".", quiet = TRUE)

set.seed(11)
n <- 5000
bench_dat <- data.frame(
  welfare = rnorm(n, 10, 2),
  tx      = runif(n, 20, 40),
  urban   = rbinom(n, 1, 0.5),
  hhsize  = sample(1:8, n, replace = TRUE)
)
bench_dat$welfare <- 5 + 0.3 * bench_dat$tx + 0.02 * bench_dat$tx^2 -
  0.05 * bench_dat$urban * bench_dat$tx + 0.1 * bench_dat$hhsize +
  rnorm(n, sd = 1.5)

fit1 <- fixest::feols(welfare ~ tx + I(tx^2) + tx:urban, data = bench_dat)
fit2 <- fixest::feols(welfare ~ tx + I(tx^2) + tx:urban | urban, data = bench_dat)
fit3 <- fixest::feols(
  welfare ~ tx + I(tx^2) + tx:urban + hhsize | urban, data = bench_dat
)

lab_fun <- function(x) switch(x,
  tx = "Max temp (deg C)", urban = "Urban", hhsize = "Household size",
  welfare = "Welfare ($/day)", x
)

med_of <- function(build, iters = 7L) {
  times <- numeric(iters)
  for (i in seq_len(iters)) {
    t0 <- proc.time()[["elapsed"]]
    build()
    times[i] <- proc.time()[["elapsed"]] - t0
  }
  stats::median(times)
}

coef_args <- list(
  fit1 = fit1, fit2 = fit2, fit3 = fit3,
  weather_terms = "tx", interaction_terms = "tx:urban",
  outcome_label = "Welfare ($/day)", label_fun = lab_fun,
  engine = "fixest", pred_var = "tx",
  x_label = "Coefficient (log points)", has_controls = TRUE
)

effect_args <- list(
  fit = fit3, pred_var = "tx", interaction_terms = "tx:urban",
  is_binned = FALSE, label_fun = lab_fun, engine = "fixest",
  x_label = "Max temp (deg C)",
  y_label = "Change in welfare per +1 unit", show_rug = TRUE
)

# Warm-up both sides once (first-call costs: ggplot builds, widget scaffolds).
invisible(do.call(make_coefplot, coef_args))
invisible(do.call(echart_make_coefplot, coef_args))
invisible(do.call(make_weather_effect_plot, effect_args))
invisible(do.call(echart_weather_effect_plot, effect_args))

rows <- list(
  c("coefplot", "ggplot make_coefplot",
    med_of(function() do.call(make_coefplot, coef_args))),
  c("coefplot", "echarts echart_make_coefplot",
    med_of(function() do.call(echart_make_coefplot, coef_args))),
  c("effect (continuous, poly + moderated)", "ggplot make_weather_effect_plot",
    med_of(function() do.call(make_weather_effect_plot, effect_args))),
  c("effect (continuous, poly + moderated)", "echarts echart_weather_effect_plot",
    med_of(function() do.call(echart_weather_effect_plot, effect_args)))
)

out <- do.call(rbind, lapply(rows, function(r) data.frame(
  figure = r[1], builder = r[2], median_ms = round(as.numeric(r[3]) * 1000, 2),
  stringsAsFactors = FALSE
)))

cat("\nR-side builder cost (median of 7 iters, n = 5k synthetic snapshot)\n\n")
print(out, row.names = FALSE)
gg <- out$median_ms[c(1, 3)]
ec <- out$median_ms[c(2, 4)]
cat(sprintf(
  "\necharts/ggplot ratio: coefplot %.2fx, effect %.2fx\n",
  ec[1] / gg[1], ec[2] / gg[2]
))
