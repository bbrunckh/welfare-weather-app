# Prep survey location (H3) data for wise-app

# Load libraries
library(duckdbfs)
load_h3()
load_spatial()
library(dplyr)
library(haven)
library(pins)
library(nanoparquet)

# library(dlw)

setwd("../app/")

#------------------------------------------------------------------------------#
# Posit connect board for pins

board <- board_connect()

#----------------------------------------------------------------------------#
# Location data for WISE-APP

# Get list of countries with H3 survey data
loc_path <- paste0("~/Library/CloudStorage/OneDrive-WBG/",
                   "Household survey locations to H3/LOC/")

h3_list <- list.files(loc_path, recursive = TRUE, "H3.dta$") 

weather_codes <- gsub("_weather", "", 
                      pin_read(board, "bbrunckhorst/weather_data")$plist)

#------------------------------------------------------------------------------#
# Loop over surveys 

# loop over H3 files
for (n in 1:length(h3_list)){
  
  code <- sub("/.*", "", h3_list[n])
  year <- sub("^[^_]*_([^_]*)_.*$", "\\1", h3_list[n])
  print(paste0(code, " ", year))
  
  # skip if no survey data prepared...
  if(!file.exists(paste0("data/surveys/",code,"_", year, ".parquet"))){next}
  
  # skip if no weather data
  if(!code %in% weather_codes){next}
  
  # skip if file exists
  # if(file.exists(paste0("data/surveys/",code,"_", year, "_H3.parquet")) & 
  #    file.exists(paste0("data/surveys/",code,"_", year, "_LOC.parquet"))){next}
  
  h3 <- read_dta(paste0(loc_path,h3_list[n]), encoding = "latin1") 
  
    # h3 <- dlw_get_gmd(code, year, "H3") # from datalibweb
  
  # save H3 level parquet data to disk
  write_parquet(h3, 
                paste0("data/surveys/",code, "_", year, "_H3.parquet"),
                options = parquet_options(write_minmax_values = FALSE))
  
  # Pin H3 level parquet data to Posit Connect board for app
  pin_write(board, h3, paste0(code, "_", year,"_H3"), type = "parquet")

  # get interview dates and number of households from processed survey data
  loc_dates <- open_dataset(paste0("data/surveys/",code,"_", year, ".parquet")) |>
    distinct(loc_id, urban, int_date) |>
    summarise(int_dates = str_flatten(int_date, ", "), .by = c(loc_id, urban)) 

  # Aggregate H3 hexagon boundaries to spatial units (loc_id)
  loc_geo <- as_dataset(h3) |>
    mutate(geom = st_geomfromtext(h3_cell_to_boundary_wkt(h3_7))) |> 
    summarise(geom = ST_AsText(st_union_agg(geom)), 
              .by = c(code, year, loc_id)) |> 
    left_join(loc_dates) |>
    collect()

  # save loc_id level parquet data to disk
  write_parquet(loc_geo, 
                paste0("data/surveys/",code, "_", year, "_LOC.parquet"),
                options = parquet_options(write_minmax_values = FALSE))
  
  # Pin loc_id level parquet data to Posit Connect board for app
  pin_write(board, loc_geo, paste0(code, "_", year,"_LOC"), type = "parquet")
  
}
#------------------------------------------------------------------------------#
# this is how to read the parquet with wkt column using sf

# board |> pin_read("AGO_2018_LOC") |> st_as_sf(wkt = "geom", crs = 4326)