################################################################################
## Code name - dlnm_model_main_script.R
## Author: Al Morgan, Bella Tortora Brayda & Sarah Reed
## Created: June 2025
##
## Written/run on - R Studio SERVER
## R version - 4.4.2
## Memory required to run: ~2 GB
##
## This script configures the necessary all parameters, arguments, file paths etc 
## before wrangling the base data (either deaths-based or hospitalisations-based)
## needed for the climate DLNM model to run.
## After choosing required configuration options (geographical scope, age brackets),
## the DLNM code is run and the results are rendered in a markdown report.
##
## Please ensure you update all relevant sections below
################################################################################

library(odbc) #load first to avoid masking of certain functions within dplyr
library(tidyverse)
library(janitor) # for cleaning data
# library(tidylog) # useful in console dplyr messages
library(phsmethods) # useful PHS specific functions
library(phsstyles)
library(lubridate)
library(dlnm)
library(mixmeta)
library(splines)
library(tsModel)
library(zeallot)
library(FluMoDL)
library(R2admb)
library(tseries)
library(rmarkdown)
library(here)
library(openxlsx)

# Today's date
today_date <- Sys.Date()

## 1) Select main variables ---- MAIN SECTION TO CUSTOMISE 
# This is the main section to change which model is run, choose accordingly

# CHOOSE your exposure: "high temperature", "low temperature"
exposure <- "high temperature"

# CHOOSE your event (comment out others): "death", "hospital admissions", "NHS24 calls"
# event <- "death"
event <- "hospital admissions"

# geog_area needs to be written as "healthboard" or "council area"
# geog_area <- "council area"
geog_area <- "healthboard"

# Covid flag: This has been trialled for use instead of including a covid variable. (It works well)
# Keep as FALSE if including a covid variable (about line 255) in "Extra independent variables"
covid = FALSE

# Age-split flag: this parameter determines if the model is run on under/over 65 age brackets
# age_65_split = TRUE
age_65_split = FALSE

##THE CODE BELOW DOESN'T NEED ADJUSTING EXCEPT FOR WHEN TRIALLING DIFFERENT MODEL PARAMETERS  ##


## 2) Indicator context ---- 
# At the top of the markdown produced in dlnm_model_checking.Rmd is a section for
# general indicator context, that is specific to the indicator in question and 
# will vary depending on the exposure and event chosen. Any detailed text for
# the indicator context can be input below:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (event == "death") {
  indicator_context <- paste("Scotland has a small population and relatively few days",
                             "with high temperatures, which has provided challenges",
                             "for producing a robust model. ")
  
} else if (event == "hospital admissions") {
  indicator_context <- "" 
}

## 3) Choose time period ----
# Different indicators will require different time periods (i.e. heat related
# indicators may look specifically at the summer months). 
# If looking at summer months only, set summer to TRUE. Otherwise, set to FALSE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summer = TRUE

# If summer is set to true, month choice is filtered to the requested months
# UKHSA and PHW have used June (6) to September (9) as the summer months,
# so we have mirrored that choice here

if(summer == TRUE){
  month_choice = c(6:9)
} else{
  month_choice = c(1:12)
}

# VARPER chooses which percentile to have internal knot positions at 
# - Fewer knot positions and no need for a low knot for summer months
# varper is assigned when using a b-spline for the exposure function. Otherwise
# vardegree is used for the ns exposure function

if(summer == TRUE){
  varper <- c(50,90)
}else{
  varper <- c(10,75,90)
}

## 4) Set up indicator specific variables ----
# Where this script has so far been used for heat related mortality and morbidity
# it is split into two conditional sections for if event == death, and for if
# event == hospital admissions. The arguments that may differ for specific indicators,
# from the standard base arguments are listed here.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Geography names for output folder are selected automatically:
if(geog_area == "healthboard"){folder_geog <- "nhsboard"}
if(geog_area == "council area"){folder_geog <- "councilarea"}

# 4a) Heat related mortality: arguments
# ~~~~~~~~~~~~~~~~~~~~~~~
if (event == "death"){
  
  # CHECK DATA PATHS are current and correct!!
  
  # DATA PATHS:
  # ~~~~
  
  # INPUT:
  # Input folder paths:
  input_data_path <- "/conf/quality_indicators/Climate/data/base_data/all_deaths_data_ephss20_near_ALLcovid/"
  
  
  # Suffix of input data files:
  if(geog_area == "healthboard"){
    input_data_pattern <- "all_deaths_data_nhsboard_age65split.csv$"
  }
  if(geog_area == "council area"){
    input_data_pattern <- "all_deaths_data_councilarea_age65split.csv$"
  }
  
  
  # OUTPUT:
  # Output folder paths:
  climate_data_folder <- "/conf/quality_indicators/Climate/data/dlnm_results/"
  
  
  # Suffix of output data files:
  indicator_output_folder <- paste0("heat_deaths_ephss_", folder_geog)
  
  
  #If the output folder doesn't yet exist, create it:
  if (!dir.exists(paste0(climate_data_folder, indicator_output_folder))) {
    dir.create(paste0(climate_data_folder, indicator_output_folder), recursive = TRUE)
  }
  
  # DEFINE VARIABLES AND PARAMETERS:
  # ~~~~~
  # Define dependent column inline with dataset:
  dependent_col = "death"
  
  
  # VARFUN: Specification of the exposure function 
  # (natural cubic spline (ns) or b-spline (bs))
  # Natural cubic splines are a type of B-spline with additional constraints at the boundaries,
  # ensuring that the spline is linear beyond the boundary knots 
  # NOTE: Can use "bs" if looking at a year, "ns" better for shorter time period
  varfun = "ns" 
  
  # VARDEGREE sets the degrees of freedom in the exposure function when using ns
  vardegree =  length(varper) + 1
  
  
  # LAG FUNCTIONS
  # Specification of the lag function
  # The model uses either lagnk or lagdf - NOT BOTH
  # lagnk for bs and lagdf for ns
  
  # LAG - lag length in days 
  lag = 2 
  
  # LAGNK - Number of knots in lag function 
  lagnk = 1 
  
  # LAGDF - degrees of freedom for lag function
  lagdf = 1 
  
  
  # Set the Degrees of freedom for seasonality based on timeframe choice
  if(summer == TRUE){
    dfseas = 2
  }else{
    dfseas = 8
  }
  
  
  # 4b) Heat related hospitalisations: arguments
  # ~~~~~~~~~~~~~~~~~~~~~~~
  
} else if (event == "hospital admissions"){
  
  # CHECK DATA PATHS are current and correct!!
  
  # DATA PATHS:
  # ~~~~
  
  # INPUT:
  # Input folder paths:
  input_data_path <- "/conf/quality_indicators/Climate/data/base_data/heat_hosps_data_ephss20_near_ALLcovid/"
  
  # Suffix of input data files
  if(geog_area == "healthboard"){
    input_data_pattern <- "heat_hum_hosps_data_nhsboard_vuln_split.csv$"
  }
  if(geog_area == "council area"){
    input_data_pattern <- "heat_hum_hosps_data_councilarea_vuln_split.csv$"
  }
  
  
  # OUTPUT:
  # Output folder paths:
  climate_data_folder <- "/conf/quality_indicators/Climate/data/dlnm_results/"
  
  # Suffix of output data files:
  indicator_output_folder <- paste0("heat_hosps_ephss_", folder_geog)
  
  #If the output folder doesn't yet exist, create it:
  if (!dir.exists(paste0(climate_data_folder, indicator_output_folder))) {
    dir.create(paste0(climate_data_folder, indicator_output_folder), recursive = TRUE)
  }
  
  # Define dependent variable
  dependent_col = "admissions"
  
  # VARFUN: Specification of the exposure function 
  # (natural cubic spline (ns) or b-spline (bs))
  # NOTE: Can use "bs" if looking at a year, "ns" better for shorter time period
  
  varfun = "ns" 
  
  # VARDEGREE sets the degrees of freedom in the exposure function
  vardegree =  length(varper) + 1
  
  # LAG FUNCTIONS
  # Specification of the lag function
  # The model uses either lagnk or lagdf - NOT BOTH
  # Uses lagnk when using bs, or lagdf if using ns.
  
  # LAG - lag length in days 
  lag = 2
  
  # LAGNK - Number of knots in lag function - use this if using b-spline
  lagnk = 1 
  
  # LAGDF - degrees of freedom for lag function - use this if using natural cubic spline
  lagdf = 1 
  
  # With different summer month_choice, for hospitalisation comes a different
  # degrees of freedom for seasonality
  if(summer == TRUE){
    dfseas = 2
  }else{
    dfseas = 8
  }
}

## 5) Define the non-indicator specific parameters ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define columns
# ~~~~~~~~~~
time_col = "date"
region_col = "regnames" # currently using council areas
weather_col = "tmean"  # whilst the model reads in 'tmean' we're actually using max temp here
population_col = "pop"
humidity_col = "humidity"

# Extra independent variables
# ~~~~~~~~~~
# Cross-basis and seasonal spline included by 
# default. Use ('NONE' if none).
# If controlling for COVID data, set one of the independent_cols arguments
# to 'covid', otherwise set to 'NONE'
independent_col1 = 'dow'  # day of week
independent_col2 = 'NONE' # scottish deprivation index
independent_col3 = 'covid'
#independent_col3 = 'NONE'
independent_col4 = 'NONE'

# Comment out these lines if using covid flag (line 42/43 ish) instead of covid variable
if (event == "death" & independent_col3 == "covid"){
  independent_col3 = 'covid_death' # deaths due to covid (codes used: U07.1 and U07.2)
} else if (event == "hospital admissions" & independent_col3 == "covid"){
  independent_col3 = 'covid_adm' # hospital admissions due to covid (codes used: U07.1 and U07.2)
} else {
  independent_col3 = 'NONE'
}

# Centring predictions 
# ~~~~~~~~~~~~~~~~~~~~
# Define the percentile of temperatures used for centring predictions at regional level
percentile = 0.5


### Meta-predictors are NOT CURRENTLY IN USE ###

# Define meta-predictors:
# ~~~~~~~~~~~~~~~~~~~~~~~
# Define meta-predictors to assess which variables(if any) should be included 
# in the regional meta model
#   OPTIONS: 1) avgweather (average temperature)
#            2) maxweather (maximum temperature)
#            3) rangeweather (temperature range)

#Define empty predictors first which will be populated in dlnm_code
avgweather <- NA
rangeweather <- NA
maxweather <- NA
metapreds <- character(0)  # if no meta predictors



############################## END OF CUSTOMISATION SECTION ############################


# Set up function for producing indicator estimates spreadsheets 
source("functions/create_estimates_xl.R")

# This is the default age component for the markdown title and is changed automatically as needed
age_split_title = "all ages"

if(age_65_split == TRUE){
  for (age_split in c("under_65yrs", "65yrs_over")){
    # Adjust dependent column according to event and age bracket
    if(event == "death"){dependent_col = paste0("death_", age_split)}
    if(event == "hospital admissions"){dependent_col = paste0("adm_", age_split)}
    
    source(here("dlnm/dlnm_code.R"), echo = FALSE)
    
    # Markdown file title
    if(age_split == "under_65yrs"){age_split_title = "ages <65"}
    if(age_split == "65yrs_over"){age_split_title = "ages 65+"}
    
    # Render markdown file
    render("dlnm/dlnm_model_checking.Rmd",
           output_dir = paste0("/conf/quality_indicators/Climate/data/dlnm_results/", indicator_output_folder, "/"),
           output_file = paste0("dlnm_model_checking_", age_split)
           
    )
    # Create excel workbook for estimates
    wb <- create_estimates_xl(temps_rr_df, scot_weather, hot_days_long, antot, artot, dat)
    saveWorkbook(wb, paste0("/conf/quality_indicators/Climate/data/dlnm_results/", indicator_output_folder, "/estimates_", age_split, ".xlsx"), overwrite = TRUE)
    
  }} else{
    
    source(here("dlnm/dlnm_code.R"), echo = FALSE)
    
    render("dlnm/dlnm_model_checking.Rmd",
           output_dir = paste0("/conf/quality_indicators/Climate/data/dlnm_results/", indicator_output_folder, "/"),
           output_file = paste0("dlnm_model_checking_all_ages")
    )
    
    # Create excel workbook for estimates
    wb <- create_estimates_xl(temps_rr_df, scot_weather, hot_days_long, antot, artot, dat)
    saveWorkbook(wb, paste0("/conf/quality_indicators/Climate/data/dlnm_results/", indicator_output_folder, "/estimates_all_ages", ".xlsx"), overwrite = TRUE)
    
  }
