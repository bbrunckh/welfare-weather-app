# Run-stage log ----
#
# One line per long-running stage (Step 2 run, Step 3 run), written to stderr so
# it lands in the Posit Connect process log. Fields are an allowlist of run
# metadata (ids, counts, timings, memory, outcome); callers never pass
# credentials, connection parameters or survey values, and every value is
# reduced to a safe character set so a message cannot add lines or fields.
# Disable with WISEAPP_STAGE_LOG=0.

#' Resident memory of this process in MB, `NA` when it cannot be read.
#'
#' Peak resident size (`VmHWM`) on Linux; current resident size elsewhere.
#' @noRd
.wise_rss_mb <- function() {
  status_file <- "/proc/self/status"
  if (file.exists(status_file)) {
    line <- grep("^VmHWM:", readLines(status_file, warn = FALSE), value = TRUE)
    kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", line)))
    if (length(kb) == 1L && is.finite(kb)) return(kb / 1024)
  }
  ps <- tryCatch(
    suppressWarnings(system2("ps", c("-o", "rss=", "-p", Sys.getpid()), stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  kb <- suppressWarnings(as.numeric(trimws(ps[1L])))
  if (length(kb) == 1L && is.finite(kb)) kb / 1024 else NA_real_
}

#' Write one structured stage line.
#'
#' @param stage Short stage name, for example "step2_worker".
#' @param outcome "succeeded", "failed" or "cancelled".
#' @param run_id Run or job identifier.
#' @param elapsed Seconds the stage took.
#' @param keys Number of weather/pipeline keys processed.
#' @param cache Weather cache state, for example "warm".
#' @param rss_mb Resident memory in MB (default: this process now).
#' @return The line, invisibly.
#' @noRd
.wise_log_stage <- function(stage, outcome, run_id = NULL, elapsed = NULL,
                            keys = NULL, cache = NULL, rss_mb = .wise_rss_mb()) {
  if (tolower(trimws(Sys.getenv("WISEAPP_STAGE_LOG", "1"))) %in% c("0", "false", "no", "off")) {
    return(invisible(NULL))
  }
  clean <- function(x) gsub("[^A-Za-z0-9._:-]", "_", as.character(x)[1L])
  num <- function(x, digits) {
    x <- suppressWarnings(as.numeric(x)[1L])
    if (is.finite(x)) format(round(x, digits), scientific = FALSE, trim = TRUE) else NULL
  }
  fields <- list(
    stage = clean(stage),
    outcome = clean(outcome),
    run = if (!is.null(run_id) && !is.na(run_id[1L])) clean(run_id) else NULL,
    keys = if (!is.null(keys)) num(keys, 0) else NULL,
    cache = if (!is.null(cache) && !is.na(cache[1L])) clean(cache) else NULL,
    elapsed_s = if (!is.null(elapsed)) num(elapsed, 1) else NULL,
    rss_mb = if (!is.null(rss_mb)) num(rss_mb, 0) else NULL
  )
  fields <- Filter(Negate(is.null), fields)
  line <- paste(names(fields), unlist(fields), sep = "=", collapse = " ")
  message("[wiseapp] ", line)
  invisible(line)
}
