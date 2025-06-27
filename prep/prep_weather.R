# Load libraries
library(duckdbfs)
load_h3()
load_spatial()
library(dplyr)
library(terra)
library(ecmwfr)
library(data.table)
library(sf)
#------------------------------------------------------------------------------#

# Get list of countries with processed survey data
codes <- list.files("data/surveys/",".parquet$") |>
  substr(1, 3) |> unique()

# weather variables
weather_list <- read.csv("data/weather.csv")
weather_names <- gsub(" |-", "_", weather_list$name) |> tolower()
weather_vars <- weather_list$varname

# world bank admin-0 boundary data
adm0_path <- paste0("~/Library/CloudStorage/OneDrive-WBG/",
                    "spid-boundaries/data/raw/WB_2025/",
                    "World Bank Official Boundaries - Admin 0.gpkg")

# loop over countries
for (c in codes){
  
  # get H3 cell centroids covering aoi ##FIX st_dump geometries first...
  
  st_read(adm0_path) |>
    rename(code = WB_A3) |>
    filter(code == c) |>
    st_cast("POLYGON") |>
    st_write("data/temp/admin0.gpkg", append = FALSE)
  
  h3_points <- open_dataset("data/temp/admin0.gpkg", format = "sf") |> 
    mutate( # ADD ST_Dump...
      h3 = unlist(h3_polygon_wkt_to_cells_experimental_string(
        geom, 6L, "overlap")),
      lat = h3_cell_to_lat(h3),
      lng = h3_cell_to_lng(h3)) |>
    distinct(code, h3, lat, lng) |>
    collect() |>
    vect(geom=c("lng", "lat"), crs = "EPSG:4326")
  
  #aoi
  bbox <- ext(h3_points)
  area <- c(bbox[4],bbox[1],bbox[2],bbox[3])
  
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
  
  wf_request_batch(batch_request, workers = 20, path = "data/temp",
                   time_out = 10000, retry = 10)
  
  # unzip and open files
  lapply(list.files("data/temp",".zip", full.names = T), function(zip_file) {
    unzip(zip_file, exdir = "data/temp")
  })
  
  raster <- rast(list.files("data/temp",".nc$", full.names = T))
  names(raster) <- paste0(sub("_.*$", "", names(raster)),"_",time(raster))
  
  # extract values at H3 centroids
  h3_weather <- cbind(as.data.frame(h3_points),
                      extract(raster,h3_points, method = "simple",ID = FALSE)) |>
    as.data.table() |>
    melt(id.vars = c("code", "h3"),
         measure.vars = measure(value.name, timestamp, sep = "_")) 

  # clean and save H3 weather (full country coverage)
  h3_weather_pqt <- as_dataset(h3_weather) |>
    mutate(timestamp = as.Date(timestamp),
           across(c("tx35","tx40","tr","r01","r10","r20"), ~ round(.x, 0)),
           across(c("t","tn","tx","dtr","tnn","txx","r","sdii",
                    "rx1day","rx5day"), ~ round(.x, 1)),
           across(c("mrsos","spi6","spei6"), ~ round(.x, 2))) |>
    rename("h3_6" = "h3") |>
    select(code, year, h3_6, timestamp, all_of(weather_vars)) |>
    write_dataset(paste0("data/weather/",c, "_weather.parquet"))
  
  # delete temporary files
  unlink("data/temp/*")
  rm(h3_points, h3_weather, raster)
}

# convert partitioned directory of parquet files into single file
# folder_list <- list.dirs("data/weather", recursive = FALSE)
# for (f in folder_list){
#   open_dataset(paste0(f,"/\\*/\\*.parquet"), recursive = FALSE) |>
#     write_dataset(paste0(f,".parquet"))
# }

