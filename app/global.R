# Load libraries
library(shiny)
library(bslib)
library(bsplus)

library(readr)
library(nanoparquet)

# library(igraph)
library(duckplyr)
library(dplyr)
library(sf)

library(tidyr)
library(tibble)
library(lubridate)
library(labelled)

library(ggplot2)
library(ggridges)
library(scales)
library(stringr)
library(leaflet)
library(vtable)
library(DT)

library(openxlsx2)
library(jtools)
library(margins)
library(interactions)
library(broom)

library(pins)
library(httr2)

# App version
version <- "v0.0.2"

# connect to pin board

  # use local pin board if no R config active
  if (Sys.getenv("R_CONFIG_ACTIVE") == "") {
    board <- board_folder("data/pins")
    pin_prefix <- ""
    # If on Posit Connect server, use Connect pin board
  } else {
    board <- board_connect()
    pin_prefix <- "bbrunckhorst/"
  }

pin_list <- pin_list(board)

# Survey data list
survey_list_master <- pin_read(board, paste0(pin_prefix, "surveys")) |>
  mutate(external = TRUE, 
         weather = paste0(code, "_weather")) |>
  filter(paste0(pin_prefix,weather) %in% pin_list)

# Survey variable list
varlist <- pin_read(board, paste0(pin_prefix, "varlist"))

# Weather variable list
weather_list <- pin_read(board, paste0(pin_prefix, "weather_varlist"))

# Welfare outcomes
outcomes <- c(
  "Log welfare ($/day, PPP)",
  "Poor (PPP)",
  "Log welfare (LCU/day)",
  "Poor (LCU)"
)

# Intl poverty lines
pov_lines <- data.frame(
  ppp_year = c(rep(2021,3),rep(2017,3),rep(2011,3)),
  ln = c(3.00, 4.20, 8.30, 2.15, 3.65, 6.85, 1.90, 3.20, 5.50)
  )
          