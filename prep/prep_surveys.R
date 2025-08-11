# Prep survey data for wise-app
rm(list = ls())

# load libraries
library(pins)
library(haven)
library(lubridate)
library(dlw)
options(dlw.local_dir = "~/dlw/")
library(pipr)
library(duckdbfs)
library(dplyr)
library(nanoparquet)

setwd("../app/")
#------------------------------------------------------------------------------#
# Board for pins

# Local folder
board_local <- board_folder("../app/data/pins")

# Posit - internal
board_posit <- board_connect(auth = "envvar", versioned = FALSE)

# Posit - external
# board_posit <- board_connect(server = "external-server")

# # remove all existing pins from board
# pin_delete(board, pin_list(board))

# overwrite existing data 
overwrite <- TRUE

#------------------------------------------------------------------------------#
# WISE-APP variable list

varlist <- open_dataset("data/wiseapp_variables.csv") |> collect()

gmd_vars <- c(
  filter(varlist, !is.na(ALL)) |> pull(`gmd varname`),
  filter(varlist, !is.na(`gmd altname`)) |> pull(`gmd altname`))

gmd_hh_vars <- filter(
  varlist, !is.na(wiseapp) & !is.na(ALL) & is.na(`hh aggregation`)) |> 
  pull(varname)

wise_vars <- filter(varlist, !is.na(wiseapp)) |> 
  select(wiseapp, varname, label, datatype)

integer_cols <- filter(wise_vars, datatype == "Integer") |> pull(varname)
numeric_cols <- filter(wise_vars, datatype == "Numeric") |> pull(varname)
logical_cols <- filter(wise_vars, datatype == "Binary") |> pull(varname)
cat_cols <- filter(wise_vars, datatype == "Categorical") |> pull(varname)
string_cols <- filter(wise_vars, datatype == "String") |> pull(varname)

# pin wiseapp variable list to Posit Connect board for app
pin_write(board_posit, wise_vars, "varlist", type = "parquet")
pin_write(board_local, wise_vars, "varlist", type = "parquet")

#------------------------------------------------------------------------------#
# Get latest CPI/ICP conversion factors from datalibweb, keep key vars

cpiicp <- dlw_get_gmd_support("CPIICP") |>
  select(code, countryname, year, survname, datalevel, 
         cpi2011:cpi2021, icp2011:icp2021) 

#------------------------------------------------------------------------------#
# Get GMD catalog of geocoded GMD surveys (SPAT module)

spat_cat <- dlw_server_catalog("GMD")[
  Collection == "GMD" & Module == "SPAT"][
    , .SD[toupper(Vermast) == max(toupper(Vermast))],
    by = .(Year, Country)][
      , .SD[toupper(Veralt) == max(toupper(Veralt), na.rm = TRUE)],
      by = .(Year, Country)][
        , fname := substr(FileName, 1, nchar(FileName) - 8)][
          order(Country)]

# Initialize survey list for app
survey_list <- tibble()

# continue existing survey list if not overwriting data
if (!overwrite) survey_list <- pin_read(board_local, "surveys")

# error log
errors <- c()

#------------------------------------------------------------------------------#
# Loop over surveys 
#------------------------------------------------------------------------------#

for (n in 1:nrow(spat_cat)){
  
  code <- spat_cat$Country_code[n]
  year <- spat_cat$Survey_year[n]
  print(paste0(code, " ", year))
  
  fname <- substr(spat_cat$FileName[n], 1, nchar(spat_cat$FileName[n]) - 8)
  print(fname)
  
  # skip if file exists
  if (!overwrite) if (pin_exists(board_local, paste0(code,"_",year))) next
  
  #----------------------------------------------------------------------------#
  # Check if SPAT module has interview date
  #----------------------------------------------------------------------------#
  error_occurred <- FALSE
  tryCatch({
  
      spat <- dlw_get_data(code, paste0(fname,"SPAT.dta"))
    
  }, error = function(e) {
    cat(paste0("Failed to get GMD SPAT module for ", code, " ", year))
    errors <<- c(errors, paste0("Failed to get SPAT ALL module for ", code, " ", year))
    error_occurred <<- TRUE
  })
  if (error_occurred) {next} #skip if fail
  
  # skip if interview month missing for > 50% households
  if (sum(is.na(spat$int_month))/nrow(spat)>0.5) {
    cat(paste0("No inverview month in SPAT for ", code, " ", year))
    errors <<- c(errors, paste0("No inverview month in SPAT for ", code, " ", year))
    next
  }
  
  #----------------------------------------------------------------------------#
  # Standardised HH level data for WISE-APP
  #----------------------------------------------------------------------------#
  # try to get latest version GMD ALL module from datalibweb
  error_occurred <- FALSE
  tryCatch({
    
    if (code == "IND" & year %in% c("2019", "2020", "2021")){
      survey <- dlw_get_data(code, paste0(fname,"GPWG.dta"))
    } else if (code == "GHA" & year %in% c("2016")){
      survey <- dlw_get_data(code, paste0(fname,"ALL.dta"))
    } else {
      
      survey <- dlw_get_gmd(code, year, "ALL", latest_version = TRUE)
    }
    
  }, error = function(e) {
    cat(paste0("Failed to get GMD ALL module for ", code, " ", year))
    errors <<- c(errors, paste0("Failed to get GMD ALL module for ", code, " ", year))
    error_occurred <<- TRUE
  })
  if (error_occurred || is.character(survey)) {next} #skip if fail
  
  
  # to duckdb
  survey_db <- as_dataset(survey)
  
  # add standard GMD variables not in dataset
  gmd_add <- setdiff(gmd_vars, colnames(survey_db))
  if (length(gmd_add)>0){
  survey_db <- survey_db |>
    mutate(!!!setNames(rep(list(NA), length(gmd_add)), gmd_add))
  }
  
  # harmonize different names for the same variable, drop missing welfare
   survey_db <- survey_db |> 
     mutate(code = if_else(is.na(code), countrycode, code),
            hhid = as.character(hhid),
            hhsize = if_else(is.na(hhsize),hsize,hhsize),
            weight = if_else(is.na(weight),weight_p,weight),
            subnatid1 = if_else(is.na(subnatid1),subnatid,subnatid1)) |>
     filter(!is.na(welfare), !is.na(weight), !is.na(urban))
   
  # summarise variables at household level
   survey_db <- survey_db |>
     summarise(
       across(c("welfare"), # use mean welfare (simulated GMD data has >1 per hhid)
              ~ mean(.x, na.rm = TRUE)), 
       across(c("t_wage_total", "laborincome", "weight"),
              ~ sum(.x, na.rm = TRUE)),
       across(c("cellphone"),
              ~ max(.x, na.rm = TRUE)),
       across(c("literacy", "educat7", "educat5", "educat4"),
              ~ max(.x[age>=15], na.rm = TRUE)),
       educy = mean(educy[age>=15], na.rm = TRUE),
       depend = if_else(sum(age>=15 & age <65, na.rm = TRUE)>0,
                        (sum(age<15, na.rm = TRUE) + sum(age>=65, na.rm = TRUE))/sum(age>=15 & age <65, na.rm = TRUE),
                        NA),
       across(c("male", "lstatus", "empstat", "ocusec", "industrycat10",
                "industrycat4", "occup", "lstatus_year", "empstat_year", 
                "ocusec_year", "industrycat10_year", "industrycat4_year", 
                "occup_year", "njobs"),
              ~ first(.x[relationharm==1])),
     .by = all_of(setdiff(gmd_hh_vars, "welfare"))) |> ungroup()
   
   # CHECK no duplicate hhid
   if (any(duplicated(pull(survey_db, hhid)))){
     cat(paste0("hhid not unique in ", code, " ", year))
     errors <- c(errors, paste0("hhid not unique in ", code, " ", year))
     next
   }
   
   # construct binary education variables (missing = NA)
   survey_db <- survey_db |>
     mutate(
       educ_com1 = case_when(
         educat7>=3 ~ 1, educat5>=3 ~ 1, educat4>=2 ~ 1, 
         !is.na(educat7) | !is.na(educat5) | !is.na(educat4) ~ 0),
       educ_com2 = case_when(
         educat7>=3 ~ 1, educat5>=4 ~ 1, educat4>=5 ~ 1, 
         !is.na(educat7) | !is.na(educat5) | !is.na(educat4) ~ 0),
       educ_com3 = case_when(
         educat7>=4 ~ 1, educat5>=5 ~ 1,  educat4>=6 ~ 1, 
         !is.na(educat7) | !is.na(educat5) | !is.na(educat4) ~ 0))
   
   # construct binary labor force variables (missing = NA)
   survey_db <- survey_db |>
     mutate(
       employed = case_when(lstatus == 1 ~ 1, !is.na(lstatus) ~ 0),
       unemployed = case_when(lstatus == 2 ~ 1, !is.na(lstatus) ~ 0),
       notinlf = case_when(lstatus == 3 ~ 1, !is.na(lstatus) ~ 0),
       employed_year = case_when(lstatus_year == 1 ~ 1, !is.na(lstatus_year) ~ 0),
       unemployed_year = case_when(lstatus_year == 2 ~ 1, !is.na(lstatus_year) ~ 0),
       notinlf_year = case_when(lstatus_year == 3 ~ 1, !is.na(lstatus_year) ~ 0),
       selfemployed = case_when(empstat == 4 ~ 1, !is.na(empstat) ~ 0),
       selfemployed_year = case_when(empstat_year == 4 ~ 1, !is.na(empstat_year) ~ 0),
       agriculture = case_when(industrycat4 == 1 ~ 1, !is.na(industrycat4) ~ 0),
       industry = case_when(industrycat4 == 2 ~ 1, !is.na(industrycat4) ~ 0),
       services = case_when(industrycat4 == 3 ~ 1, !is.na(industrycat4) ~ 0),
       agriculture_year = case_when(industrycat4_year == 1 ~ 1, !is.na(industrycat4_year) ~ 0),
       industry_year = case_when(industrycat4_year == 2 ~ 1, !is.na(industrycat4_year) ~ 0),
       services_year = case_when(industrycat4_year == 3 ~ 1, !is.na(industrycat4_year) ~ 0))
   
   # construct other binary variables (missing = NA)
    # !! seems to be a problem with encoding of roof, floor, wall not being harmonized???
   survey_db <- survey_db |>
     mutate(
       solidcookfuel = case_when(cooksource == 1 || cooksource == 3 ~ 1, !is.na(cooksource) ~ 0),
       internet_access = case_when(internet <= 3 ~ 1, internet ==4 ~ 0),
       # roof_finished = case_when(roof > 30 & roof < 40 ~ 1, !is.na(roof) ~ 0),
       # wall_finished = case_when(wall > 30 & wall < 40 ~ 1, !is.na(wall) ~ 0),
       # floor_finished = case_when(floor > 30 & floor < 40 ~ 1, !is.na(floor) ~ 0),
       ownhouse_secure = case_when(ownhouse == 1 ~ 1, !is.na(ownhouse) ~ 0),
       renthouse = case_when(ownhouse == 2 ~ 1, !is.na(ownhouse) ~ 0))
   
  # construct household level welfare outcomes
   cpiicp_db <- as_dataset(cpiicp)
   survey_db <- survey_db |>
     mutate(
       datalevel = if_else(code %in% c("CHN", "IND"), as.numeric(urban), 2)) |> 
     left_join(cpiicp_db) |> 
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
     ) 
   
  # try to merge SPAT data
   error_occurred <- FALSE
   tryCatch({

  spat_db <- as_dataset(spat) |>
      mutate(hhid = as.character(hhid)) |>
    select(-survname, -urban, -ends_with(c("_m1", "_sy", "_ref")))
  
 # use interview dates from either GMD_SPAT (1st) or GMD_ALL (2nd)
  survey_db <- survey_db |>
    rename(int_year_gmd = int_year, int_month_gmd = int_month) |>
    left_join(spat_db) |>
    mutate(int_year = if_else(is.na(int_year), int_year_gmd, int_year),
             int_month = if_else(is.na(int_month), int_month_gmd, int_month))
  
   }, error = function(e) {
     cat(paste0("Failed to merge GMD SPAT module for ", code, " ", year))
     errors <<- c(errors, paste0("Failed to merge GMD SPAT module for ", code, " ", year))
     error_occurred <<- TRUE
   })
   if (error_occurred) {next} #skip if fail
   
  # construct interview date and timestamp variables
     
  survey_db <- survey_db |>
    mutate(
      across(c(int_year, int_month, int_day), ~as.integer(.x)),
      int_date = if_else(is.na(int_year) | is.na(int_month), NA,
                         as.Date(if_else(is.na(int_day),
                                 paste0(int_year,"-",int_month,"-01"),
                                 paste0(int_year,"-",int_month,"-",
                                        int_day)))),
      timestamp = case_when(
        !is.na(int_date) ~ as.Date(paste0(int_year,"-",int_month,"-01")))) |>
    collect()
   
  error_occurred <- FALSE
  tryCatch({
  # clean and order variables
  survey_clean <- survey_db |>
    select(any_of(pull(wise_vars,varname))) |>
    mutate(across(any_of(c(integer_cols, cat_cols, logical_cols)), as.integer),
           across(any_of(numeric_cols), as.numeric)) |>
    collect() |> 
    mutate(across(any_of(string_cols), as.character)) 
  
  }, error = function(e) {
    cat(paste0("Error collecting data frame for ", code, " ", year))
    errors <<- c(errors, paste0("Error collecting data frame for ", code, " ", year))
    error_occurred <<- TRUE
  })
  if (error_occurred) {next} #skip if fail
  
  # drop empty columns
  survey_clean <- survey_clean |>
    select(where(~ !((all(is.na(.))) || is.character(.) && all(is.na(.) | . == ""))))
  
  # CHECK $3.00 poverty rate vs PIP
  pip_poor_300ln <- get_stats(code, year) |> pull(headcount)
  svy_poor_300ln <- weighted.mean(survey_clean$poor_300ln, survey_clean$weight)
  
  if (length(pip_poor_300ln)>0) { #IND 2019, 2020, 2021 no longer in PIP
    if (round(pip_poor_300ln, 2) != round(svy_poor_300ln, 2)){
      errors <- c(errors, paste0("$3.00 poverty rates does not match PIP for ", code, " ", year))
      next
    }
  }
  
  # Pin HH level parquet data to Posit Connect board for app
  pin_write(board_local, survey_clean, paste0(code, "_", year), type = "parquet")
  pin_write(board_posit, survey_clean, paste0(code, "_", year), type = "parquet")
  
  #----------------------------------------------------------------------------#
  # Add survey to list for app
  #----------------------------------------------------------------------------#
  
  survey_list = bind_rows(survey_list,
    data.frame(
      countryname = pull(survey_clean[1,],countryname), 
      code = code, 
      year = year, 
      hh = nrow(survey_clean), 
      wiseapp_pin = paste0(code, "_", year),
      gmd_all = paste0(fname, "ALL.dta"), 
      gmd_spat = paste0(fname, "SPAT.dta")
    )
  )

  rm(survey, survey_db, survey_clean, spat)
  close_connection()
}

#------------------------------------------------------------------------------#
# save error log
write.csv(errors, "data/survey_prep_errors.csv", row.names = FALSE)

# fix country names in survey list
survey_list <- survey_list |>
  mutate(countryname = if_else(code =="CIV", "Côte d`Ivoire", countryname))

# pin country listing to Posit Connect board for app
pin_write(board_local, survey_list, "surveys", type = "parquet")
pin_write(board_posit, survey_list, "surveys", type = "parquet")
