#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Comparison_function_development.R
# June 2023
# Bella Tortora Brayda 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Aim of function ---------------------------------------------------------

# Write a function to compare hospital admissions between two time periods
# at a given geography level (can be specified down to IZ, HSCP, HB, Scotland)
# for given conditions (default argument is 'all' for all heat related conditions 
# - see condition codes script)

# Ideally running this code the first time will extract the necessary data queries,
# which could potentially be stored as objects for future retrieval. 

# Libraries ---------------------------------------------------------------

library(odbc) #load first to avoid masking of certain functions within dplyr
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(tidylog)
library(lubridate)
library(stringr)
library(here)


# Sourcing other scripts --------------------------------------------------

source("condition_codes.R")
source("most_recent_postcode_lookup.R")

# Function ----------------------------------------------------------------

# ARGUMENTS:

# start_date = the start date of the time period that you are interested in
# end_date = the end date of the time period that you are interested in
# geography_level = the geography level at which you want the data extracted
# conditions = a list of conditions that you would like to compare admissions for
#              with heatwave related lists already stored in condition_codes.R
# comp_window = the time window which you are comparing hospital admissions with.
#
#               Prebuilt options: ten_y (10 years prior), five_y (5 years prior),
#               one_y (1 year prior), one_m (one month prior) one_w (one week prior)
#               
#               These will be used to inform a comparison start and end date within 
#               the function.
# 

#Example arguments provided below for testing:
start_date <- lubridate::dmy(01072021)
end_date <- lubridate::dmy(01082021)
geography_level <- "Scotland"
conditions <- all
comp_window <- "five_y"


compare_admissions <- function(start_date, end_date, geography_level, conditions,
                               comp_window = c("ten_y", "five_y", "one_y", "one_m",
                                               "one_w")){
  
  ## evaluate choices - should throw out an error message if not met.
  comp_window <- match.arg(comp_window)
  
  if(comp_window == "ten_y"){
  # Calculate comparison start_date and end_date
  comp_start <- start_date - years(10)
  comp_end <- end_date - years(10)
  }
  
  if(comp_window == "five_y"){
    # Calculate comparison start_date and end_date
    comp_start <- start_date - years(5)
    comp_end <- end_date - years(5)
  }
  
  if(comp_window == "one_y"){
    # Calculate comparison start_date and end_date
    comp_start <- start_date - years(1)
    comp_end <- end_date - years(1)
  }
  
  if(comp_window == "one_m"){
    # Calculate comparison start_date and end_date
    comp_start <- start_date - months(1)
    comp_end <- end_date - months(1)
  }
  
  if(comp_window == "one_w"){
    # Calculate comparison start_date and end_date
    comp_start <- start_date - weeks(1)
    comp_end <- end_date - weeks(1)
  }
  
  
  # SMRA login information
  channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                        uid=.rs.askForPassword("SMRA Username:"),
                                        pwd=.rs.askForPassword("SMRA Password:")))
  
  # Extract data for period of interest----
  # This query takes a while to run. It is faster using UNION to bind the two queries before
  # than when nesting, but in the long run it would be good to streamline the query to 
  # run faster. 

  smr01_test <- as_tibble(dbGetQuery(channel, statement = paste0(
      "SELECT link_no, cis_marker, admission_date, admission_type, discharge_date, hbtreat_currentdate, length_of_stay,
          main_condition, other_condition_1, other_condition_2, other_condition_3, other_condition_4, other_condition_5, 
          main_operation, other_operation_1, other_operation_2, other_operation_3,
          admission, discharge, uri, postcode
  FROM ANALYSIS.SMR01_PI  z
  WHERE exists(
  SELECT *
  FROM ANALYSIS.SMR01_PI
  WHERE link_no=z.link_no and cis_marker=z.cis_marker
  AND regexp_like(main_operation || other_condition_1 || other_condition_2
                  || other_condition_3 || other_condition_4 || other_condition_5, '", conditions ,"')  
  AND (ADMISSION_DATE BETWEEN
  TO_DATE(", shQuote(start_date, type = "sh"),",'YYYY-MM-DD') AND
  TO_DATE(", shQuote(end_date, type = "sh"),",'yyyy-mm-dd')))
  
  UNION
  
  SELECT link_no, cis_marker, admission_date, admission_type, discharge_date, hbtreat_currentdate, length_of_stay,
          main_condition, other_condition_1, other_condition_2, other_condition_3, other_condition_4, other_condition_5, 
          main_operation, other_operation_1, other_operation_2, other_operation_3,
          admission, discharge, uri, postcode
  FROM ANALYSIS.SMR01_PI  z
  WHERE exists(
  SELECT *
  FROM ANALYSIS.SMR01_PI
  WHERE link_no=z.link_no and cis_marker=z.cis_marker
  AND regexp_like(main_operation || other_condition_1 || other_condition_2
                  || other_condition_3 || other_condition_4 || other_condition_5, '", conditions ,"')  
  AND (ADMISSION_DATE BETWEEN
  TO_DATE(", shQuote(comp_start, type = "sh"),",'YYYY-MM-DD') AND
  TO_DATE(", shQuote(comp_end, type = "sh"),",'yyyy-mm-dd')))
  "))) %>%
    clean_names() %>%
    arrange(link_no, admission_date, discharge_date, admission, discharge, uri)
  
  # As this is pulling in data for a continuous inpatient stay, the date range 
  # that is being pulled in includes some dates preceeding the selected time windows.
  # We are only interested in first admissions occurring during the given timeframe, 
  # so when metoffice data comes in, we can match records to temperatures and exclude
  # admissions that don't correlate to the temperature thresholds of interest.
  
  ## Geography lookup
  #~~~~~~~~~~~~~~~~~~
  
  geo_lookup <-  readRDS(most_recent_postcode_lookup(postcode_folder)) |>  
    select(pc8, hb2019name,datazone2011) |> 
    mutate(pc8 = str_remove_all(pc8, " ")) |>  
    rename(postcode = pc8)
  
  ## CIS  ----
  #~~~~~~~~~~~~~~~~~~~~~
  
  smr01_cis <- smr01_test %>% 
    arrange(link_no,cis_marker,admission_date) |> 
    distinct(link_no,cis_marker, .keep_all = TRUE) |> 
    mutate(postcode = str_remove_all(postcode, " ")) |> 
    left_join(geo_lookup, by = c("postcode")) |> 
    filter(hb2019name =="NHS Greater Glasgow and Clyde" ) |> 
    filter((admission_date >= start_date & admission_date < end_date)|
             (admission_date >= comp_start & admission_date < comp_end))
  
  mutate(link_no = as.numeric(link_no),
         cis_marker = as.numeric(cis_marker)) |> 
  
  count_data <- smr01_cis %>% 
    mutate(year = format(admission_date, "%Y"),
           month_day = format(admission_date, "%m-%d ")) |> 
    group_by(year, month_day) |>  
    summarise(count = n()) |> 
    ungroup() |> 
    pivot_wider(names_from = year, values_from = count) |> 
    arrange(month_day)
  
  
  
}

# aggregate by link and cis 
# sort by admission date
# take the first admission date then filter out any not within the time window.
# Need original admissions from within that date.
