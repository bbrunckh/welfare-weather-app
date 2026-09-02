# Test Databricks M2M OAuth connection
# Run interactively to verify credentials and data access
# -----------------------------------------------------------------------

host      <- Sys.getenv("DATABRICKS_HOST")
client_id <- Sys.getenv("DATABRICKS_CLIENT_ID")
secret    <- Sys.getenv("DATABRICKS_CLIENT_SECRET")
vol_path  <- Sys.getenv("DATABRICKS_VOLUME_PATH")

stopifnot(
  "Set DATABRICKS_HOST in .Renviron"          = nzchar(host),
  "Set DATABRICKS_CLIENT_ID in .Renviron"     = nzchar(client_id),
  "Set DATABRICKS_CLIENT_SECRET in .Renviron" = nzchar(secret),
  "Set DATABRICKS_VOLUME_PATH in .Renviron" = nzchar(vol_path)
)

# ---- 1. Get OAuth token ------------------------------------------------
cat("Step 1: Requesting OAuth token...\n")
resp <- httr::POST(
  paste0(host, "/oidc/v1/token"),
  httr::authenticate(client_id, secret),
  body   = list(grant_type = "client_credentials", scope = "all-apis"),
  encode = "form"
)
httr::stop_for_status(resp, "obtain OAuth token")
token <- httr::content(resp, as = "parsed")$access_token
cat("✓ Token obtained:", substr(token, 1, 40), "...\n\n")

auth <- httr::add_headers(Authorization = paste("Bearer", token))

# ---- 2. List files in Volume -------------------------------------------
cat("Step 2: Listing files in Volume...\n")
resp2 <- httr::GET(
  paste0(host, "/api/2.0/fs/directories", vol_path),
  auth
)
dir_content <- httr::content(resp2, as = "parsed")

if (!is.null(dir_content$error_code)) {
  cat("✗ Directory listing failed:", dir_content$message, "\n")
} else {
  files <- dir_content$contents
  cat("✓ Files found:", length(files), "\n")
  for (f in head(files, 10)) {
    cat(" ", f$path,
        if (isTRUE(f$is_directory)) "[dir]"
        else paste0("[", round(f$file_size / 1024, 1), " KB]"), "\n")
  }
}

# ---- 3. List SQL warehouses --------------------------------------------
cat("\nStep 3: Listing SQL warehouses...\n")
resp3 <- httr::GET(paste0(host, "/api/2.0/sql/warehouses"), auth)
warehouses <- httr::content(resp3, as = "parsed")$warehouses

if (length(warehouses) == 0) {
  cat("✗ No SQL warehouses found\n")
} else {
  for (w in warehouses) cat(" ", w$name, "(", w$id, ") state:", w$state, "\n")
  wh_id <- warehouses[[1]]$id

}

cat("\nDone.\n")

# Test loading a parquet file via DuckDB

host      <- Sys.getenv("DATABRICKS_HOST")
vol_path  <- "/Volumes/prd_decdg/swisea170/vwisea170/Documents"
test_file <- "GNB_2018_EHCVM_hh.parquet"
file_url  <- paste0(host, "/api/2.0/fs/files", vol_path, "/", test_file)

con <- duckdb::dbConnect(duckdb::duckdb())
DBI::dbExecute(con, "INSTALL httpfs; LOAD httpfs;")

# DuckDB v1.1+ supports HTTP secret type with bearer token
DBI::dbExecute(con, sprintf("
  CREATE OR REPLACE SECRET db_http (
    TYPE  http,
    BEARER_TOKEN '%s'
  );", token))

cat("✓ HTTP secret created\n")
cat("Testing lazy read from:", file_url, "\n\n")

result <- tryCatch({
  DBI::dbGetQuery(con, sprintf(
    "SELECT code, economy, welfare, urban, electricity, internet
     FROM read_parquet('%s')
     LIMIT 5", file_url
  ))
}, error = function(e) {
  cat("Failed:", conditionMessage(e), "\n")
  NULL
})

if (!is.null(result)) {
  cat("✓ DuckDB lazy remote read succeeded!\n")
  print(result)
}

duckdb::dbDisconnect(con)

# ---- Test CSV loading via load_data() ----------------------------------
devtools::load_all()

cat("\n---- Testing load_data() for CSV ----\n")

params <- list(
  type          = "databricks",
  workspace     = Sys.getenv("DATABRICKS_HOST"),
  client_id     = Sys.getenv("DATABRICKS_CLIENT_ID"),
  client_secret = Sys.getenv("DATABRICKS_CLIENT_SECRET"),
  storage_path  = Sys.getenv("DATABRICKS_STORAGE_PATH")
)

cat("DATABRICKS_STORAGE_PATH:", params$storage_path, "\n")

# List CSV files from earlier directory listing (files var from Step 2)
csv_files <- Filter(function(f) grepl("\\.csv$", f$path, ignore.case = TRUE), files)

if (length(csv_files) == 0) {
  cat("No CSV files found in volume. All files:\n")
  for (f in files) cat(" ", f$path, "\n")
} else {
  cat("CSV files found:\n")
  for (f in csv_files) cat(" ", f$path, "\n")
  test_csv <- basename(csv_files[[1]]$path)

  # Manually build expected URL to verify path resolution
  expected_url <- paste0(
    host, "/api/2.0/fs/files", vol_path, "/", test_csv
  )
  cat("\nExpected URL:", expected_url, "\n")

  # Direct DuckDB read to confirm URL + auth works for CSV
  con2 <- duckdb::dbConnect(duckdb::duckdb())
  DBI::dbExecute(con2, "INSTALL httpfs; LOAD httpfs;")
  DBI::dbExecute(con2, sprintf("
    CREATE OR REPLACE SECRET db_http (
      TYPE         http,
      BEARER_TOKEN '%s'
    );", token))

  csv_result <- tryCatch(
    DBI::dbGetQuery(con2,
      sprintf("SELECT * FROM read_csv('%s', AUTO_DETECT=TRUE) LIMIT 5",
              expected_url)),
    error = function(e) {
      cat("✗ Direct CSV read failed:", conditionMessage(e), "\n")
      NULL
    }
  )
  if (!is.null(csv_result)) {
    cat("✓ Direct CSV read succeeded! Columns:",
        paste(names(csv_result), collapse = ", "), "\n")
    print(csv_result)
  }
  duckdb::dbDisconnect(con2)

  # Now test via load_data() — this is what the app calls
  cat("\nTesting load_data() with format = 'csv'...\n")
  tbl <- tryCatch(
    load_data(test_csv, params, format = "csv", collect = FALSE),
    error = function(e) {
      cat("✗ load_data() failed:", conditionMessage(e), "\n")
      NULL
    }
  )
  if (!is.null(tbl)) {
    cat("✓ load_data() succeeded!\n")
    print(dplyr::collect(head(tbl, 3)))
  }
}
