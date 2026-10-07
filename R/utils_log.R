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

# User-facing errors (CR-SEC-08) ----

#' Log an error in full and return a short message for the user.
#'
#' Raw condition messages can contain hosts, volume paths, file paths or SQL.
#' The full condition goes to the server log under a short id; the user sees a
#' classified message with the same id, so support can find the log line.
#' Messages without a URL or path are short enough to show as they are.
#'
#' @param e A condition (or a character message).
#' @param context Optional prefix such as "Simulation".
#' @return A single string for a notification or status panel.
#' @noRd
wise_user_error <- function(e, context = NULL) {
  msg <- if (inherits(e, "condition")) conditionMessage(e) else paste(as.character(e), collapse = " ")
  id <- substr(digest::digest(list(Sys.time(), Sys.getpid(), basename(tempfile()))), 1L, 8L)
  message("[wiseapp] error id=", id,
    if (!is.null(context)) paste0(" context=", context),
    " class=", class(e)[1L], ": ", msg)
  first <- trimws(strsplit(msg, "\n", fixed = TRUE)[[1]][1L] %||% "")
  text <- if (inherits(e, "wise_async_error")) {
    first
  } else if (grepl("Timed out|timeout", msg, ignore.case = TRUE)) {
    "The operation took too long and was stopped."
  } else if (grepl("HTTP 40[13]|Unauthori[sz]ed|Forbidden|OAuth|credential", msg, ignore.case = TRUE)) {
    "The data source rejected the request. Check the connection credentials."
  } else if (grepl("HTTP 404|not found|No files found|does not exist", msg, ignore.case = TRUE)) {
    "A required data file was not found at the data source."
  } else if (grepl("HTTP [0-9]{3}|Could not resolve|Connection (refused|reset)|curl", msg, ignore.case = TRUE)) {
    "The data source could not be reached."
  } else if (grepl("cannot allocate|out of memory", msg, ignore.case = TRUE)) {
    "The server ran out of memory. Try a smaller selection."
  } else if (nzchar(first) && nchar(first) <= 200L &&
             !grepl("://|(^|[ (='\"])[~.]?/[^ ]|[A-Za-z]:\\\\|SELECT |FROM ", first)) {
    first
  } else {
    "An unexpected error occurred."
  }
  paste0(if (!is.null(context)) paste0(context, " failed: "), text,
    " (error id ", id, ")")
}
