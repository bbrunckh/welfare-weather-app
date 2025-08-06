# Prep survey location (H3) data for wise-app
rm(list = ls())

# Load libraries
library(pins)
library(dlw)
options(dlw.local_dir = "~/dlw/")
library(duckdbfs)
load_h3()
load_spatial()
library(dplyr)

setwd("../app/")

#------------------------------------------------------------------------------#
# Board for pins

# Local folder
board_local <- board_folder("../app/data/pins")

# Posit - internal
board_posit <- board_connect(auth = "envvar")

# Posit - external
# board_posit <- board_connect(server = "external-server")

# overwrite existing data 
overwrite <- TRUE

#----------------------------------------------------------------------------#
# Location data for WISE-APP

# Get list of countries with processed survey data
surveys <- pin_read(board_local, "surveys")

#------------------------------------------------------------------------------#
# Loop over surveys 

# loop over H3 files
for (n in 1:nrow(surveys)){
  
  pin_name <- surveys$wiseapp_pin[n]
  print(pin_name)
  
  code <- substr(pin_name, 1, 3)
  year <- substr(pin_name, 5, 8)
  
  # fname <- substr(surveys$gmd_spat[n], 1, nchar(surveys$gmd_spat[n]) - 8)
  
  # skip if pins exist
  if (!overwrite) {
    if (pin_exists(board_local, paste0(pin_name,"_H3")) & 
        pin_exists(board_local, paste0(pin_name,"_LOC"))) {next}
  }
  
  # h3 <- dlw_get_gmd(code, year, "H3")
  
  h3 <- haven::read_dta(
    list.files('/Users/bbrunckhorst/Library/CloudStorage/OneDrive-WBG/Household survey locations to H3/LOC',
               paste0(pin_name,".+_H3.dta$"), recursive = TRUE, full.names = TRUE))
  
  # Pin H3 level parquet data to Posit Connect board for app
  pin_write(board_local, h3, paste0(pin_name,"_H3"), type = "parquet")
  pin_write(board_posit, h3, paste0(pin_name,"_H3"), type = "parquet")

  # get interview dates and number of households from processed survey data
  loc_dates <- pin_read(board_local, pin_name) |> 
    as_dataset() |>
    distinct(loc_id, urban, int_date) |>
    summarise(int_dates = str_flatten(int_date, ", "), .by = c(loc_id, urban))
    
  # Aggregate H3 hexagon boundaries to spatial units (loc_id)
  
  if (code != "FJI"){
  loc_geo <- as_dataset(h3) |>
    mutate(geom = st_geomfromtext(h3_cell_to_boundary_wkt(h3_7))) |> 
    summarise(geom = ST_AsText(st_union_agg(geom)), 
              .by = c(code, year, loc_id)) |> 
    left_join(loc_dates) |>
    collect()
  
  # Pin loc_id level parquet data to Posit Connect board for app
  pin_write(board_local, loc_geo, paste0(pin_name,"_LOC"), type = "parquet")
  pin_write(board_posit, loc_geo, paste0(pin_name,"_LOC"), type = "parquet")
  }
  
  rm(h3, loc_dates, loc_geo)
  close_connection()
  
}
#------------------------------------------------------------------------------#
# this is how to read the parquet with wkt column using sf

# board |> pin_read("AGO_2018_LOC") |> st_as_sf(wkt = "geom", crs = 4326)