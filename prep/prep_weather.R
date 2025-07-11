# H3 indexed weather data for wise-app and GMD_SPAT

# Load libraries
library(duckdbfs)
load_h3()
load_spatial()
library(dplyr)
library(terra)
library(ecmwfr)
library(data.table)
library(sf)

setwd("../app/")
#------------------------------------------------------------------------------#

# Get list of countries with H3 survey data
loc_path <- paste0("~/Library/CloudStorage/OneDrive-WBG/",
                   "Household survey locations to H3/LOC/")
codes <- list.files(loc_path, recursive = TRUE, "H3.dta$") |>
  substr(1, 3) |> unique()

# weather variables
weather_list <- read.csv("data/weather.csv")
weather_names <- gsub(" |-", "_", weather_list$name) |> tolower()
weather_vars <- weather_list$varname

# world bank admin-0 h3_6 cell centroids
path <- "~/Library/CloudStorage/OneDrive-WBG/Household survey locations to H3/02_data/"
adm0_h3_6 <- open_dataset(paste0(path, "boundaries_h3_7.parquet")) |>
  mutate(h3 = h3_cell_to_parent(h3_7, 6L)) |>
  distinct(code, h3) |>
  mutate(lon = h3_cell_to_lng(h3), lat = h3_cell_to_lat(h3)) |>
  write_dataset("data/temp/adm0_h3_6.parquet")

# loop over countries
for (c in codes){
  
  print(c)
  
  # skip if no survey data prepared...
  if(length(list.files("data/surveys/",paste0(c,"_.+.parquet")))==0){next}
  
  # skip if file exists
  if(file.exists(paste0("data/weather/", c,"_weather.parquet"))){next}
  
  # get H3 cell centroids covering country 
  h3_points <- open_dataset("data/temp/adm0_h3_6.parquet") |>
  filter(code == c) |>
  collect() 
  
  #aoi
  area <- c(ceiling(max(h3_points$lat)*10)/10,
            floor(min(h3_points$lon)*10)/10,
            floor(min(h3_points$lat)*10)/10,
            ceiling(max(h3_points$lon)*10)/10)
  
  # build batch request - all weather variables in list
  dynamic_request <- wf_archetype(    
    request <- list(
      dataset_short_name = "multi-origin-c3s-atlas",
      origin = "era5_land",
      domain = "global",
      period = "1950-2024",
      variable = "monthly_temperature",
      bias_adjustment = "no_bias_adjustment",
      area = area,
      target = "t.zip"
    ),
    dynamic_fields = c("variable", "target"))
  
  # get data - all weather variables in list
  batch_request <- lapply(1:length(weather_vars), function(n) {
    dynamic_request(variable = weather_names[n], target = paste0(weather_vars[n], ".zip"))
  })
  
  wf_request_batch(batch_request, workers = 10, path = "data/temp",
                   time_out = 10000, retry = 10)
  
  # unzip and open files
  lapply(list.files("data/temp",".zip", full.names = T), function(zip_file) {
    unzip(zip_file, exdir = "data/temp")
  })
  
  raster <- rast(list.files("data/temp",".nc$", full.names = T))
  names(raster) <- paste0(sub("_.*$", "", names(raster)),"_",time(raster))
  crs(raster) <- "EPSG:4326"
  
  # extract values at H3 centroids
  h3_weather <- cbind(as.data.frame(h3_points[,1:2]),
                      extract(raster,h3_points[,3:4], method = "simple",ID = FALSE)) |>
    setDT() |>
    melt(id.vars = c("code", "h3"),
         measure.vars = measure(value.name, timestamp, sep = "_")) 

  # clean and save H3 weather (full country coverage)
  h3_weather_pqt <- as_dataset(h3_weather) |>
    mutate(timestamp = as.Date(timestamp),
           across(c("tx35","tx40","tr","r01","r10","r20"), ~ round(.x, 0)),
           across(c("t","tn","tx","dtr","tnn","txx","r","sdii",
                    "rx1day","rx5day"), ~ round(.x, 1)),
           across(c("mrsos","spi6","spei6"), ~ round(.x, 2))) |>
    rename(h3_6 = h3) |>
    select(code, h3_6, timestamp, all_of(weather_vars)) |>
    write_dataset(paste0("data/weather/",c, "_weather.parquet"))
  
  # delete temporary files
  rm(raster, h3_points, h3_weather)
  unlink("data/temp/*")
}

#------------------------------------------------------------------------------#

