# Prep surveys v2

# load libraries
library(dlw)
library(duckdbfs)
library(dplyr)
library(haven)
library(openxlsx2)
library(sf)

#------------------------------------------------------------------------------#
# Get WISE-APP variable lists

varlist <- read_xlsx("data/wiseapp_variables.xlsx")

gmd_vars <- c(
  filter(varlist, !is.na(ALL)) |> pull("gmd varname"),
  filter(varlist, !is.na(`gmd altname`)) |> pull("gmd altname"))

gmd_hh_vars <- filter(
  varlist, !is.na(`wiseapp`) & !is.na(ALL) & is.na(`hh aggregation`)) |> 
  pull("varname")

#------------------------------------------------------------------------------#
# Get latest CPI/ICP conversion factors from datalibweb, keep select vars

cpiicp <- dlw_get_gmd_support("CPIICP") |>
  select(code, countryname, year, survname, datalevel, 
         cpi2011:cpi2021, icp2011:icp2021)

#------------------------------------------------------------------------------#
# Get list of geocoded GMD surveys with H3 module

loc_path <- paste0("~/Library/CloudStorage/OneDrive-WBG/",
                   "Household survey locations to H3/LOC/")

survey_list <- list.files(loc_path, recursive = TRUE, "SPAT.dta$")

# filter survey list for loop (optional)
survey_list <- survey_list[grepl("^BFA|^CIV|^MLI|^MWI|^SEN|^TGO|^VNM", survey_list)]

#------------------------------------------------------------------------------#
# Loop over surveys 
#------------------------------------------------------------------------------#

for (n in 1:length(survey_list)){
  
  code <- sub("/.*", "", survey_list[n])
  year <- sub("^[^_]*_([^_]*)_.*$", "\\1", survey_list[n])
  print(paste0(code, " ", year))
  
  # skip if file exists
  # if(file.exists(paste0("data/surveys/",code,"_",year,".parquet"))){next}
  
  #----------------------------------------------------------------------------#
  # Standardised HH level data for WISE-APP
  #----------------------------------------------------------------------------#
  # try to get latest version GMD ALL module from datalibweb
  error_occurred <- FALSE
  tryCatch({
    
    survey <- dlw_get_gmd(code, year, "ALL") 
    
  }, error = function(e) {
    cat(paste0("Failed to get GMD ALL module for ", code, " ", year))
    error_occurred <<- TRUE
  })
  if (error_occurred) {next} #skip if fail
  
  # convert to duckdb dataset
  survey <- as_dataset(survey) 
  
  # add GMD variables not in dataset
  gmd_add <- setdiff(gmd_vars, colnames(survey))
  for (col in gmd_add) {
    survey <- mutate(survey, !!col := NA_real_)
  }

  # harmonize different names for variables
  vars <- colnames(survey)
   survey <- survey |> 
     mutate(code = countrycode,
            hhsize = if_else(is.na(hhsize),hsize,hhsize),
            weight = if_else(is.na(weight),weight_p,weight),
            subnatid1 = if_else(is.na(subnatid1),subnatid,subnatid1))
   
  # summarise variables at household level
   survey <- survey |>
     summarise(
       across(c("t_wage_total", "laborincome", "weight"),
              ~ sum(.x, na.rm = TRUE)),
       across(c("literacy", "educat7", "educat5", "educat4","primarycomp"),
              ~ max(.x[age>=15], na.rm = TRUE)),
       educy = mean(educy[age>=15], na.rm = TRUE),
       depend = if_else(sum(age>=15 & age <65)>0,
                        (sum(age<15) + sum(age>=65))/sum(age>=15 & age <65),
                        NA),
       across(c("male", "lstatus", "empstat", "ocusec", "industrycat10",
                "industrycat4", "occup", "lstatus_year", "empstat_year", 
                "ocusec_year", "industrycat10_year", "industrycat4_year", 
                "occup_year", "njobs"),
              ~ first(.x[relationharm==1])),
     .by = all_of(gmd_hh_vars)) |> 
     ungroup() 
   
  # check no duplicate hhid
  if (any(duplicated(pull(survey, hhid)))){
    cat(paste0("hhid not unique for ", code, " ", year))
    next
    }
   
  # construct household welfare outcomes
   cpiicp_duckdb <- as_dataset(cpiicp)
   survey <- survey |>
     mutate(datalevel = if_else(code %in% c("CHN", "IND"), 
                                as.numeric(urban), 2)) |>
     left_join(cpiicp_duckdb) |>
     mutate(
       welf_ppp_2021 = welfare/cpi2021/icp2021/365,
       welf_ppp_2017 = welfare/cpi2017/icp2017/365,
       welf_ppp_2011 = welfare/cpi2011/icp2011/365,
       poor_300ln = welf_ppp_2021 < 3.00,
       poor_420ln = welf_ppp_2021 < 4.20,
       poor_830ln = welf_ppp_2021 < 8.30,
       poor_215ln = welf_ppp_2017 < 2.15,
       poor_365ln = welf_ppp_2017 < 3.65,
       poor_685ln = welf_ppp_2017 < 6.85,
       poor_190ln = welf_ppp_2011 < 1.90,
       poor_320ln = welf_ppp_2011 < 3.20,
       poor_550ln = welf_ppp_2011 < 5.50,
       welf_lcu_2021 = welfare/cpi2021/365,
       welf_lcu_2017 = welfare/cpi2017/365,
       welf_lcu_2011 = welfare/cpi2011/365,
       wages_ppp_2021 = t_wage_total/hhsize/cpi2021/icp2021/365,
       wages_ppp_2017 = t_wage_total/hhsize/cpi2017/icp2017/365,
       wages_ppp_2011 = t_wage_total/hhsize/cpi2011/icp2011/365,
       wages_lcu_2021 = t_wage_total/hhsize/cpi2021/365,
       wages_lcu_2017 = t_wage_total/hhsize/cpi2017/365,
       wages_lcu_2011 = t_wage_total/hhsize/cpi2011/365,
       laborincome_ppp_2021 = laborincome/hhsize/cpi2021/icp2021/365,
       laborincome_ppp_2017 = laborincome/hhsize/cpi2017/icp2017/365,
       laborincome_ppp_2011 = laborincome/hhsize/cpi2011/icp2011/365,
       laborincome_lcu_2021 = laborincome/hhsize/cpi2021/365,
       laborincome_lcu_2017 = laborincome/hhsize/cpi2017/365,
       laborincome_lcu_2011 = laborincome/hhsize/cpi2011/365
     ) |>
     collect()
   
  # check $3.00 poverty rate
   weighted.mean(survey$poor_300ln, survey$weight)
   
   # merge SPAT data
  spat <- read_dta(paste0(loc_path, survey_list[n]), encoding = "latin1") 
  
  built_area_var <- paste0("built_area_",5*round(as.numeric(year)/5))
  ntl_var <- paste0("viirs_ntl_",max(2012,as.numeric(year)))
  
  spat <- as_dataset(spat) |>
      mutate(built_area = .data[[built_area_var]]/1e6/area,
             viirs_ntl = .data[[ntl_var]]*built_area) |>
      select(-starts_with(c("urban", "survname","built_area_", "viirs_ntl_"))) 

  add_dates <- setdiff(c("int_year","int_month","int_day"), colnames(spat))
  for (col in add_dates) {
    spat <- mutate(spat, !!col := NA_real_)
  }
  # use dates available from GMD or SPAT
    survey <- as_dataset(survey) |>
      rename(int_year_gmd = int_year, int_month_gmd = int_month) |>
      left_join(spat) |>
      mutate(int_year = if_else(is.na(int_year), int_year_gmd, int_year),
             int_month = if_else(is.na(int_month), int_month_gmd, int_month)) |>
      select(-int_year_gmd, int_month_gmd)
  
  # construct interview date variable
  survey <- survey |>
    mutate(
      across(c(int_year, int_month, int_day), ~as.integer(.x)),
      int_date = if_else(is.na(int_year) | is.na(int_month), NA,
                         as.Date(if_else(is.na(int_day),
                                 paste0(int_year,"-",int_month,"-01"),
                                 paste0(int_year,"-",int_month,"-",
                                        int_day)))))
  
  # final clean and order variables
  wise_vars <- filter(varlist, !is.na(wiseapp)) |> pull("varname")
  
  integer_cols <- filter(varlist, !is.na(wiseapp) & datatype == "Integer") |>
    pull("varname")
  numeric_cols <- filter(varlist, !is.na(wiseapp) & datatype == "Numeric") |>
    pull("varname")
  binary_cols <- filter(varlist, !is.na(wiseapp) & datatype == "Binary") |>
    pull("varname")
  categorical_cols <- filter(varlist, !is.na(wiseapp) & datatype == "Categorical") |>
    pull("varname")
  string_cols <- filter(varlist, !is.na(wiseapp) & datatype == "String" & !varname %in% c("hhid", "loc_id")) |>
    pull("varname")

  survey_clean <- survey |>
    select(any_of(wise_vars)) |>
    mutate(across(any_of(integer_cols), ~ as.integer(.x)),
           across(any_of(numeric_cols), ~ as.numeric(.x)),
           across(any_of(binary_cols), ~ as.logical(.x)),
           across(any_of(categorical_cols), ~ as.integer(.x)),
           across(any_of(string_cols), ~ as.character(.x))) |>
    collect() |>
    mutate(across(any_of(c("hhid", "loc_id")), ~ as.character(.x))) |>
    select(where(~ !( (all(is.na(.))) || is.character(.) && all(is.na(.) | . == ""))))
   
  # save HH level data to parquet
  write_dataset(survey_clean, paste0("data/surveys/",code, "_", year, ".parquet"))
  
  #----------------------------------------------------------------------------#
  # Location data for WISE-APP
  #----------------------------------------------------------------------------#
  # Load H3 module
  fname <- paste0(loc_path,
                  substr(survey_list[n], 1, nchar(survey_list[n]) - 8), 
                  "H3.dta")
  
  h3 <- read_dta(fname, encoding = "latin1") 
  
  # Save H3 data to parquet
  write_dataset(h3, paste0("data/surveys/",code, "_", year, "_H3.parquet"))
  
  # Aggregate H3 hexagon boundaries to spatial unit (loc_id) level
  load_h3()
  load_spatial()
  
  loc_dates <- survey |> 
    distinct(loc_id, urban, int_date) |>
    summarise(int_dates = str_flatten(int_date, ", "), .by = c(loc_id, urban)) 
  
  loc_geo <- as_dataset(h3) |>
    mutate(geom = st_geomfromtext(h3_cell_to_boundary_wkt(h3_7))) |> 
    summarise(geom = st_union_agg(geom), .by = c(code, year, loc_id)) |>
    left_join(loc_dates) |>
    to_sf(crs = 4326) 
  
  # Save interview location data as geopackage
  st_write(loc_geo,
           paste0("data/surveys/",code, "_", year, "_LOC.gpkg"),
           append = FALSE)

  rm(survey, spat, h3, loc_geo)
  close_connection()
}

#------------------------------------------------------------------------------#
# Get country listing for app UI
#------------------------------------------------------------------------------#

files <- list.files("data/surveys", "\\d{4}\\.parquet", full.names = TRUE)
all_surveys <- open_dataset(files)

survey_list <- all_surveys |> 
  summarise(n = n(), .by = c(countryname, code, year)) |> 
  mutate(filename = paste0(code, "_", year, ".parquet"),
         countryname = if_else(code=="CIV","Cote dIvoire", countryname)) |>
  arrange(code, year) |>
  collect() 

write.csv(survey_list, "data/surveys.csv", 
          row.names = FALSE)
