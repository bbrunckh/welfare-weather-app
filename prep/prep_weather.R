# pins H3 indexed weather data for wise-app

# Load libraries
library(pins)
library(nanoparquet)

#------------------------------------------------------------------------------#
# Posit connect board for pins

board <- board_connect()

#------------------------------------------------------------------------------#

# countries with processed surveys
survey_codes <- pin_read(board, "bbrunckhorst/surveys")$code

# weather data files
era5_land_path <- "~/Library/CloudStorage/OneDrive-WBG/Household survey locations to H3/02_data/h3/era5land"
weather_data <- list.files(era5_land_path,".parquet$", full.names = TRUE)

# filter to files < 250 MB
file_info <- file.info(weather_data)
max_size_bytes <- 250 * 1024^2 
small_files <- rownames(file_info)[file_info$size < max_size_bytes]

# pin files
plist <- c()
for (f in small_files){
  code <- substr(f, 108, 110)
  print(code)
  
  # skip if no processed survey data
  if (!code %in% survey_codes) next
  
  # pin to connect board
  pin_name <- paste0(code, "_weather")
  weather <- read_parquet(f)
  pin_write(board, weather, pin_name, type = "parquet")
  plist <- c(plist, pin_name)
}

# save list of pinned weather data
write.csv(plist, "../app/data/weather_data.csv", row.names = FALSE)
pin_write(board, as.data.frame(plist), "weather_data", type = "parquet")

# pin weather variable list
weather_list <- read.csv("~/Library/CloudStorage/OneDrive-WBG/Household survey locations to H3/02_data/era5land_varlist.csv")
pin_write(board, weather_list, "weather_varlist", type = "parquet")
