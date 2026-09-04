# Development-only Overview metadata benchmark.
# Set WISEAPP_DATA_PATH in .Renviron to include a local source.

options(golem.app.prod = FALSE)
devtools::load_all(quiet = TRUE)

local_path <- Sys.getenv("WISEAPP_DATA_PATH", unset = "")
repetitions <- 5L
reset_benchmark_state <- function() {
  rm(
    list = ls(.overview_metadata_cache, all.names = TRUE),
    envir = .overview_metadata_cache
  )
  .duck$db_tokens <- list()
}

run_loader <- function(params, parallel, cache = FALSE) {
  withr::local_envvar(c(
    WISEAPP_METADATA_LOAD_PARALLEL = if (parallel) "1" else "0",
    WISEAPP_METADATA_CACHE_DISABLE = if (cache) "0" else "1"
  ))
  .duck$db_tokens <- list()
  result <- NULL
  elapsed <- system.time(
    result <- load_overview_metadata(params)
  )[["elapsed"]]
  list(result = result, elapsed = elapsed)
}

benchmark <- function(params, label, compare_parallel = FALSE) {
  uncached <- numeric(repetitions)
  parallel <- numeric(repetitions)
  cold_cache <- numeric(repetitions)
  warm_cache <- numeric(repetitions)

  for (i in seq_len(repetitions)) {
    reset_benchmark_state()
    uncached_run <- run_loader(params, parallel = FALSE)
    uncached[[i]] <- uncached_run$elapsed

    comparison_run <- uncached_run
    if (isTRUE(compare_parallel)) {
      reset_benchmark_state()
      comparison_run <- run_loader(params, parallel = TRUE)
      parallel[[i]] <- comparison_run$elapsed

      stopifnot(all(vapply(
        names(uncached_run$result),
        function(name) {
          isTRUE(all.equal(
            uncached_run$result[[name]], comparison_run$result[[name]]
          ))
        },
        logical(1)
      )))
    }

    reset_benchmark_state()
    cold_run <- run_loader(
      params,
      parallel = isTRUE(compare_parallel), cache = TRUE
    )
    cold_cache[[i]] <- cold_run$elapsed
    warm_cache[[i]] <- run_loader(
      params,
      parallel = isTRUE(compare_parallel), cache = TRUE
    )$elapsed

    stopifnot(all(vapply(
      names(comparison_run$result),
      function(name) {
        isTRUE(all.equal(
          comparison_run$result[[name]], cold_run$result[[name]]
        ))
      },
      logical(1)
    )))
  }

  cat(label, "\n", sep = "")
  cat("  uncached median:    ", median(uncached), "s\n", sep = "")
  if (isTRUE(compare_parallel)) {
    cat("  parallel median:    ", median(parallel), "s\n", sep = "")
    cat(
      "  speedup:            ",
      median(uncached) / median(parallel),
      "x\n",
      sep = ""
    )
  }
  cat("  cold-cache median:  ", median(cold_cache), "s\n", sep = "")
  cat("  warm-cache median:  ", median(warm_cache), "s\n", sep = "")
  cat("  uncached runs:      ", paste(uncached, collapse = ", "), "\n", sep = "")
  if (isTRUE(compare_parallel)) {
    cat("  parallel runs:      ", paste(parallel, collapse = ", "), "\n", sep = "")
  }
}

if (nzchar(local_path)) {
  local_path <- path.expand(local_path)
  if (!dir.exists(local_path)) {
    stop("Local benchmark path does not exist: ", local_path)
  }

  benchmark(
    list(type = "local", path = local_path),
    "Local metadata",
    compare_parallel = FALSE
  )
} else {
  cat("Local metadata: skipped; set WISEAPP_DATA_PATH to enable.\n")
}

db_params <- build_connection_params("databricks")
if (validate_connection_params(db_params)) {
  benchmark(db_params, "Databricks metadata", compare_parallel = TRUE)
} else {
  cat("Databricks metadata: skipped; required .Renviron values are not set.\n")
}
