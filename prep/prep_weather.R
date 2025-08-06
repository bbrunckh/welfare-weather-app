# pins H3 indexed weather data for wise-app

rm(list = ls())

# Load libraries
library(pins)

#------------------------------------------------------------------------------#
# Board for pins

# Local folder
board_local <- board_folder("../app/data/pins")

# Posit - internal
board_posit <- board_connect(auth = "envvar", version = FALSE)

# Posit - external
# board_posit <- board_connect(server = "external-server")

# overwrite existing data 
overwrite <- TRUE
#------------------------------------------------------------------------------#

# countries with processed surveys
survey_codes <- pin_read(board_local, "surveys")$code |> unique()

# weather data files
era5_land_path <- "~/Library/CloudStorage/OneDrive-WBG/Household survey locations to H3/02_data/h3/era5land"
weather_data <- list.files(era5_land_path,".parquet$", full.names = TRUE)

# filter to files < 500 MB
file_info <- file.info(weather_data)
max_size_bytes <- 500 * 1024^2 
small_files <- rownames(file_info)[file_info$size < max_size_bytes]

# pin files
for (f in small_files){
  code <- substr(f, 108, 110)
  print(code)
  
  # skip if no processed survey data
  if (!code %in% survey_codes) next
  
  
  # pin to connect board
  pin_name <- paste0(code, "_weather")
  
  if (!overwrite) {if (pin_exists(board, pin_name)) next} # skip if exists
  
  weather <- read_parquet(f)
  
  pin_write(board_local, weather, pin_name, type = "parquet")
  pin_write(board_posit, weather, pin_name, type = "parquet")
}

# pin weather variable list
weather_list <- read.csv("~/Library/CloudStorage/OneDrive-WBG/Household survey locations to H3/02_data/era5land_varlist.csv")

pin_write(board_local, weather_list, "weather_varlist", type = "parquet")
pin_write(board_posit, weather_list, "weather_varlist", type = "parquet")
