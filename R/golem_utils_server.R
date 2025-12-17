#' Read local Parquet files using DuckDB (single or multiple files)
#'
#' Uses DuckDB's read_parquet() SQL table function. For multiple files the
#' function calls the VARCHAR[] overload. union_by_name = TRUE ensures columns
#' are combined by name.
#'
#' @param paths Character vector of local Parquet file paths.
#' @return A data.frame with the combined contents of the files.
#' @noRd
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbQuoteLiteral
#' @importFrom duckdb duckdb
read_parquet_duckdb <- function(paths) {
  if (length(paths) == 0) return(tibble::tibble())
  
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  
  files <- normalizePath(as.character(paths))
  
  if (length(files) == 1) {
    # single-file call
    sql <- paste0("SELECT * FROM read_parquet(", DBI::dbQuoteLiteral(con, files), ", union_by_name => TRUE)")
  } else {
    # array form: read_parquet(ARRAY['f1','f2'], union_by_name => TRUE)
    files_quoted <- paste(DBI::dbQuoteLiteral(con, files), collapse = ", ")
    sql <- paste0("SELECT * FROM read_parquet(ARRAY[", files_quoted, "], union_by_name => TRUE)")
  }
  
  DBI::dbGetQuery(con, sql)
}