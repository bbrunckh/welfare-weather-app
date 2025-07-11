# Load libraries
library(shiny)
library(bslib)
library(bsplus)

library(readr)
library(nanoparquet)

library(igraph)
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

# App version
version <- "v0.0.1"

# Survey data list
survey_list_master <- read_parquet("data/surveys.parquet") 

# For testing only!!
survey_list_master <- survey_list_master |>
  mutate(access = if_else(code %in% c("BFA", "SEN", "TGO"), 
                          "public","need dlw subscription"))

# Welfare outcomes
outcomes <- c(
  "Log welfare ($/day, PPP)",
  "Poor (PPP)",
  "Log welfare (LCU/day)",
  "Poor (LCU)"
  )

# Survey variable list
varlist <- read.csv("data/wiseapp_variables.csv")

# Weather variable list
weather_list <- read.csv("data/weather.csv")

# Intl poverty lines
pov_lines <- data.frame(
  ppp_year = c(rep(2021,3),rep(2017,3),rep(2011,3)),
  ln = c(3.00, 4.20, 8.30, 2.15, 3.65, 6.85, 1.90, 3.20, 5.50)
  )
                                 