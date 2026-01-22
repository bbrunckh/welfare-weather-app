#' @importFrom magrittr %>%
NULL

#' Access files in the current app (packaged path)
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "wiseapp")
}

#' Read App Config (always use packaged config)
#' @noRd
get_golem_config <- function(
    value,
    config = Sys.getenv(
      "GOLEM_CONFIG_ACTIVE",
      Sys.getenv("R_CONFIG_ACTIVE", "default")
    ),
    use_parent = TRUE
) {
  # Always load the packaged config file from inst/
  config_file <- app_sys("golem-config.yml")
  if (config_file == "") {
    stop("[app_config] packaged golem-config.yml not found. Did you set package name correctly?")
  }
  config::get(value = value, config = config, file = config_file, use_parent = use_parent)
}

# -------------------------
#  NOTE: DO NOT run heavy I/O at package load.
#  The helpers below are safe to call at runtime from server/startup.
# -------------------------

# Helper: choose pin board (no side-effects beyond returning a board object)
get_pin_board <- function() {
  if (Sys.getenv("R_CONFIG_ACTIVE") == "") {
    message("[app_config] Using LOCAL pin board (data/pins)")
    pins::board_folder("data/pins")
  } else {
    message("[app_config] Using CONNECT pin board")
    pins::board_connect()
  }
}

# Safe pin_list wrapper: returns character vector on error
safe_pin_list <- function(board) {
  tryCatch(pins::pin_list(board), error = function(e) {
    message("[app_config] pin_list() failed: ", conditionMessage(e))
    character(0)
  })
}

# Safe pin_read: returns tibble() on missing pin or error
safe_pin_read <- function(board, name, prefix = "") {
  fullname <- paste0(prefix, name)
  pins_available <- safe_pin_list(board)
  if (!(fullname %in% pins_available)) {
    warning("[app_config] Pin not found: ", fullname)
    return(tibble::tibble())  # lightweight empty tibble
  }
  tryCatch(
    pins::pin_read(board, fullname),
    error = function(e) {
      warning("[app_config] pin_read(", fullname, ") failed: ", conditionMessage(e))
      tibble::tibble()
    }
  )
}

# Main runtime loader (call this at app/server start)
# returns a list with board, prefix, and metadata tibbles (may be empty)
load_runtime_data <- function() {
  board <- get_pin_board()
  prefix <- if (Sys.getenv("R_CONFIG_ACTIVE") == "") "" else "bbrunckhorst/"
  
  survey_list_master <- safe_pin_read(board, "surveys", prefix) %>%
    dplyr::mutate(
      external = TRUE,
      weather = paste0(code, "_weather")
    ) %>%
    dplyr::filter(paste0(prefix, weather) %in% safe_pin_list(board))
  
  # New varlist (load via xlsx)
  varlist_path <- system.file(
    "data/pins/varlist/variable_list.csv",
    package = "wiseapp"
  )
  # When running in dev mode (pkgload::load_all()), the file isn't in inst/.
  # Fall back to the repo path so this works without installing.
  if (identical(varlist_path, "")) {
    varlist_path <- file.path(
      getwd(),
      "data",
      "pins",
      "varlist",
      "variable_list.csv"
    )
  }
  varlist <- readr::read_csv(varlist_path, show_col_types = FALSE)

  weather_list <- safe_pin_read(board, "weather_varlist", prefix)

  # Survey metadata schema lives in the repo under data/pins/ as a CSV.
  survey_metadata_path <- system.file(
    "data/pins/survey_metadata/survey_list.csv",
    package = "wiseapp"
  )
  # When running in dev mode (pkgload::load_all()), the file isn't in inst/.
  # Fall back to the repo path so this works without installing.
  if (identical(survey_metadata_path, "")) {
    survey_metadata_path <- file.path(
      getwd(),
      "data",
      "pins",
      "survey_metadata",
      "survey_list.csv"
    )
  }
  survey_metadata <- readr::read_csv(survey_metadata_path, show_col_types = FALSE)
  
  pov_lines <- data.frame(
    ppp_year = c(rep(2021,3), rep(2017,3), rep(2011,3)),
    ln = c(3.00,4.20,8.30, 2.15,3.65,6.85, 1.90,3.20,5.50),
    stringsAsFactors = FALSE
  )
  
  list(
    board = board,
    pin_prefix = prefix,
    pin_list = safe_pin_list(board),
    survey_list_master = survey_list_master,
    varlist = varlist,
    weather_list = weather_list,
    pov_lines = pov_lines,
    survey_metadata = survey_metadata
  )
}
