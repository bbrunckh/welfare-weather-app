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
# Board for pins

# Local folder
board_local <- board_folder("../app/data/pins")

# Posit - external
board_posit <- board_connect(server = "external-server")

# # Posit - internal
# board_posit <- board_connect(server = "internal-server")

#----------------------------------------------------------------------------#
# Location data for WISE-APP

# Get list of countries with H3 survey data
loc_path <- paste0("~/Library/CloudStorage/OneDrive-WBG/",
                   "Household survey locations to H3/LOC/")

h3_list <- list.files(loc_path, recursive = TRUE, "H3.dta$") 

surveys <- pin_read(board_local, "surveys")$wiseapp_pin |> unique()

#------------------------------------------------------------------------------#
# Loop over surveys 

# loop over H3 files
for (n in 1:length(h3_list)){
  
  pin_name <- substring(h3_list[n], 5, 12)
  print(pin_name)
  
  # skip if no survey data
  if(!pin_name %in% surveys){next}
  
  # skip if file exists
  # if(file.exists(paste0("data/surveys/",code,"_", year, "_H3.parquet")) & 
  #    file.exists(paste0("data/surveys/",code,"_", year, "_LOC.parquet"))){next}
  
  h3 <- read_dta(paste0(loc_path,h3_list[n]), encoding = "latin1") 
  
    # h3 <- dlw_get_gmd(code, year, "H3") # from datalibweb
  
  # save H3 level parquet data to disk
  write_parquet(h3, 
                paste0("data/surveys/",pin_name, "_H3.parquet"),
                options = parquet_options(write_minmax_values = FALSE))
  
  # Pin H3 level parquet data to Posit Connect board for app
  pin_write(board_local, h3, paste0(pin_name,"_H3"), type = "parquet")
  pin_write(board_posit, h3, paste0(pin_name,"_H3"), type = "parquet")

  # get interview dates and number of households from processed survey data
  loc_dates <- pin_read(board_local, pin_name) |> 
    as_dataset() |>
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
                paste0("data/surveys/",pin_name, "_LOC.parquet"),
                options = parquet_options(write_minmax_values = FALSE))
  
  # Pin loc_id level parquet data to Posit Connect board for app
  pin_write(board_local, loc_geo, paste0(pin_name,"_LOC"), type = "parquet")
  pin_write(board_posit, loc_geo, paste0(pin_name,"_LOC"), type = "parquet")
  
}
#------------------------------------------------------------------------------#
# this is how to read the parquet with wkt column using sf

# board |> pin_read("AGO_2018_LOC") |> st_as_sf(wkt = "geom", crs = 4326)