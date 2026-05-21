suppressMessages(devtools::load_all(quiet = TRUE))
suppressMessages(library(dplyr))

make_data <- function(n, p_total = 50, seed = 1L) {
  set.seed(seed)
  village <- factor(sample(LETTERS[1:8], n, TRUE))
  village_fx <- setNames(rnorm(8, sd = 0.5), levels(village))

  dat <- tibble(
    village  = village,
    temp     = rnorm(n),
    prec     = rnorm(n),
    int1     = sample(c(0L, 1L), n, TRUE),
    signal_a = rnorm(n),
    signal_b = rnorm(n),
    signal_c = rnorm(n)
  )
  n_noise <- p_total - 3
  noise <- matrix(rnorm(n * n_noise), nrow = n,
                  dimnames = list(NULL, sprintf("noise_%02d", seq_len(n_noise))))
  dat <- bind_cols(dat, as.data.frame(noise))

  dat$y <- with(dat,
    0.6*temp + 0.4*prec - 0.3*temp*int1 +
    1.0*signal_a + 0.7*signal_b - 0.5*signal_c +
    village_fx[village] + rnorm(n, sd = 0.8)
  )
  # ~5% NA in two candidates to exercise the mice path
  dat$noise_05[sample(n, n * 0.05)]  <- NA
  dat$signal_b[sample(n, n * 0.05)]  <- NA

  vl <- data.frame(
    name = c("signal_a","signal_b","signal_c",
             sprintf("noise_%02d", seq_len(n_noise)),
             "village","temp","prec","int1","y"),
    ind  = c(rep(1L, 3 + n_noise), 0L,0L,0L,0L,0L),
    hh = 0L, area = 0L, firm = 0L,
    outcome = c(rep(0L, 3 + n_noise + 4L), 1L)
  )
  list(dat = as.data.frame(dat), vl = vl)
}

run_one <- function(dat, vl, use_parallel, workers) {
  t0 <- Sys.time()
  res <- run_lasso_selection(
    df = dat,
    selected_outcome = list(name = "y", type = "numeric"),
    weather_vars = c("temp","prec"),
    fe_vars      = "village",
    int_vars     = "int1",
    valid_vl     = vl,
    mi_m = 5, mi_maxit = 2, mi_method = "norm",
    stability_threshold = 0.5,
    use_parallel = use_parallel,
    n_workers    = workers,
    parallel_seed = 17L,
    cv_selection = "random",
    glmnet_tol = 1e-4
  )
  elapsed <- as.numeric(Sys.time() - t0, units = "secs")
  list(elapsed = elapsed, n_sel = length(res$selected_covariates))
}

# Warm-up so package JIT / future plan setup doesn't pollute the first cell
invisible({
  d <- make_data(2000, p_total = 20, seed = 99)
  run_one(d$dat, d$vl, FALSE, NULL)
  run_one(d$dat, d$vl, TRUE,  2L)
})

ns      <- c(10000L, 50000L, 100000L)
configs <- list(
  list(label = "sequential",    use_par = FALSE, workers = NULL),
  list(label = "parallel x2",   use_par = TRUE,  workers = 2L),
  list(label = "parallel x15",  use_par = TRUE,  workers = 15L)
)

results <- data.frame(n = integer(), config = character(),
                      elapsed_s = numeric(), n_selected = integer())

for (n in ns) {
  cat(sprintf("\n--- n = %s ---\n", format(n, big.mark=",")))
  d <- make_data(n, p_total = 50, seed = 17L)
  for (cfg in configs) {
    r <- run_one(d$dat, d$vl, cfg$use_par, cfg$workers)
    cat(sprintf("  %-14s  %6.1f s   (selected %d)\n",
                cfg$label, r$elapsed, r$n_sel))
    results <- rbind(results,
      data.frame(n = n, config = cfg$label,
                 elapsed_s = round(r$elapsed, 2),
                 n_selected = r$n_sel))
  }
}

cat("\n\n=== SUMMARY ===\n")
wide <- reshape(results[, c("n","config","elapsed_s")],
                idvar = "n", timevar = "config", direction = "wide")
names(wide) <- sub("^elapsed_s\\.", "", names(wide))
print(wide, row.names = FALSE)

cat("\nSpeed-ups vs sequential:\n")
wide$x2_speedup  <- round(wide$sequential / wide$`parallel x2`,  2)
wide$x15_speedup <- round(wide$sequential / wide$`parallel x15`, 2)
print(wide[, c("n","x2_speedup","x15_speedup")], row.names = FALSE)
