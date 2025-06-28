# Load libraries
library(shiny)
library(bslib)
library(bsplus)

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

app_version <- "v0.0.1"

# Survey list
survey_list <- read.csv("data/surveys.csv")

# Welfare outcomes
outcomes <- c(
  "Log welfare ($/day, PPP)",
  "Poor (PPP)",
  "Log welfare (LCU/day)",
  "Poor (LCU)"
  )

# Weather variable list
weather_list <- read.csv("data/weather.csv")

# Survey variable list
varlist <- read_xlsx("data/wiseapp_variables.xlsx")

# Intl poverty lines
pov_lines <- data.frame(ppp_year = c(rep(2021,3),
                                     rep(2017,3),
                                     rep(2011,3)),
                        ln = c(3.00, 4.20, 8.30, 
                               2.15, 3.65, 6.85,
                               1.90, 3.20, 5.50))
                                 