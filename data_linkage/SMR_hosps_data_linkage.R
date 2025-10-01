#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SMR_data_linkage.R 
# Updated Feb 25 for new EPHSS data and humidity data
# 
# Sarah Reed
# 
# Script to link SMR data with geog and metoffice data 

# SCRIPT CONTENTS:
# This script first calls the setup_environment and condition codes
# before creating 2 functions used later in the script 
# 
#  i) check if any rows have NA
#  ii) reformat data for output
#  
# It then creates a final function which collates SMR and weather data
# and produces csv files for each quarter in the timeseries. Details are
# provided throughout the script, but the high level process is as follows:
#  
# a) extracts data from SMR for hospital admissions
# b) links the hosp admissions extract to geospatial MetOffice data 
# c) reformats the data to be fed into the dlnm model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Set up ---------------------------------------------------------------

# testing parallel
library(parallelly)
available_cores <- as.numeric(parallelly::availableCores())
options(Ncpus = available_cores)
Sys.setenv(MAKEFLAGS = paste("-j", as.character(available_cores), sep = ""))


# Source relevant scripts
# ~~~~~~~~~~~~~~~~~~~~~~~

#CHECK these scripts are inputting correct set of data

source(here::here("setup/setup_environment.R"))
source(here::here("lookups/condition_codes.R")) # needed for SQL query

### Create list of quarter start dates for full time period
### This is for use when running the met_smr_analysis 
### function over the full list of dates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## NEED TO UPDATE EACH TIME: ##


# 1) Input dates for required quarters
#    For a full year, need to give dates like 2019-01-01 to 2020-01-01 -------> NOT to 2019-12-31 

# 2) Need to update the number in the q_start_dates[5] line. It should be the size 
#    of the list length of q_start_dates (shown in the environment)


q_start_dates <- as.list(seq.Date(from = as.Date("2023-04-01"),
                                  to = as.Date("2025-01-01"), 
                                  by = "quarter"))

q_end_dates <- map(q_start_dates,~
                     as.Date(as.character(.x),format = "%Y-%m-%d")-1)

# remove the first date in q_end_dates and last in q_start_date list as they
# are outside time frame
q_end_dates[1] <- NULL
q_start_dates[length(q_start_dates)] <- NULL
# q_start_dates[81]<-NULL     # Check this number


# SMRA login information
# ~~~~~~~~~~~~~~~~~~~~~~
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"),
                                      pwd=.rs.askForPassword("SMRA Password:")))

# Just use these if testing code within the met_smr_analysis function
#quarter_start = "2020-01-01"
#quarter_end = "2020-12-31"

met_smr_analysis <- function(quarter_start,quarter_end){
  
  # Create list of quarter end dates
  start_date_short <- as.Date(quarter_start, format = "%Y-%m-%d")
  end_date_short <- as.Date(quarter_end, format = "%Y-%m-%d")
  start_date_long <- format(start_date_short, "%d %B %Y")
  end_date_long <- format(end_date_short, "%d %B %Y")
  start_mmmYY <- paste0(format(start_date_short, "%b"),year(start_date_short))
  end_mmmYY <- paste0(format(end_date_short, "%b"),year(end_date_short))
  
  print(paste0("Extracting data from ", start_mmmYY, " to ", end_mmmYY, "..."))
  
  
  # 3. SMR extract ----------------------------------------------------
  
  smr01_extract <- as_tibble(dbGetQuery(channel, statement = paste0(
    "SELECT link_no, sex, age_in_years, cis_marker, admission_date,
           admission_type, main_condition, other_condition_1, other_condition_2, 
           other_condition_3, other_condition_4, other_condition_5, 
           postcode, location
   FROM ANALYSIS.SMR01_PI  z
   WHERE exists(
   SELECT *
   FROM ANALYSIS.SMR01_PI
   WHERE link_no=z.link_no and cis_marker=z.cis_marker
   --AND regexp_like(main_condition || other_condition_1 || other_condition_2
    --             || other_condition_3 || other_condition_4 || other_condition_5, '", all ,"')
   AND (admission_date between '",start_date_long,"' and '",end_date_long,"')

   )"))) %>%
    clean_names() %>%
    arrange(link_no, admission_date) %>% 
  # Filter out elective/non-emergecy admissions
    filter(!str_detect(admission_type, "^[12]"))
  
  
  # Add covid column for those who were admitted with mention of covid    ------------------------------
  
  smr01_extract <- smr01_extract %>% 
    mutate(covid_adm = ifelse(main_condition == "U071" | main_condition == "U072"|
                                other_condition_1 == "U071" | other_condition_1 == "U072"|
                                other_condition_2 == "U071" | other_condition_2 == "U072"|
                                other_condition_3 == "U071" | other_condition_3 == "U072"|
                                other_condition_4 == "U071" | other_condition_4 == "U072"|
                                other_condition_5 == "U071" | other_condition_5 == "U072",
                              1, 0))
  
  
  #format postcode to pc8 in SMR data to match with same type in postcode data
  smr01_extract <- smr01_extract %>% 
    mutate(postcode = format_postcode(postcode, format="pc8"))
  
  
  # select just the postcode and IZ info (postcode_dir loaded in setup_environment.R)
  # postcode_dir no longer contains intzone2011name (only 2022 name), join from pop_iz
  pc <- postcode_dir %>% 
    dplyr::select(pc8, intzone2011, hb2019, hb2019name, ca2019, ca2019name) %>% 
    left_join(pop_iz %>% select(intzone2011, intzone2011name), by = "intzone2011") %>% 
    relocate(intzone2011name, .after = intzone2011)
  
  
  # Add intermediate zone column to SMR data by joining with selected postcode data
  SMR <- left_join(smr01_extract, pc, by = c("postcode" = "pc8"))
  
  # Should also check that postcodes are all Scottish!
  unmatched2 <- anti_join(smr01_extract, pc, by = c("postcode" = "pc8")) # There are 911 unrecognised postcodes / non - Scottish postcodes in this quarter
  
  # Print statement in console about unmatched2
  print(paste("There are", length(unmatched2), "unrecognised postcodes")) 
  
  
  # Extract the first episode of a CIS
  SMR %<>%
    distinct(link_no, cis_marker, .keep_all = TRUE)
  
  
  # 5. SIMD lookups and combined file ---------------------------------------
  
  simd_2020 <- readRDS(paste0("/conf/linkage/output/",
                              "lookups/Unicode/Deprivation",
                              "/postcode_2025_1_simd2020v2.rds")) %>%
    dplyr::select(pc7, simd2020v2_sc_quintile) %>%
    rename(postcode = pc7,
           simd = simd2020v2_sc_quintile) %>%
    mutate(year = "simd_2020")
  
  simd_2016 <- readRDS(paste0("/conf/linkage/output/",
                              "lookups/Unicode/Deprivation",
                              "/postcode_2019_2_simd2016.rds")) %>%
    dplyr::select(pc7, simd2016_sc_quintile) %>%
    rename(postcode = pc7,
           simd = simd2016_sc_quintile) %>%
    mutate(year = "simd_2016")
  
  simd_2012 <- readRDS(paste0("/conf/linkage/output/",
                              "lookups/Unicode/Deprivation/",
                              "postcode_2016_1_simd2012.rds")) %>%
    dplyr::select(pc7, simd2012_sc_quintile) %>%
    rename(postcode = pc7,
           simd = simd2012_sc_quintile) %>%
    mutate(year = "simd_2012")
  
  simd_2009 <- readRDS(paste0("/conf/linkage/output/",
                              "lookups/Unicode/Deprivation/",
                              "postcode_2012_2_simd2009v2.rds")) %>%
    dplyr::select(pc7, simd2009v2_sc_quintile) %>%
    rename(postcode = pc7,
           simd = simd2009v2_sc_quintile) %>%
    mutate(year = "simd_2009")
  
  # SIMD deprivation order changed from these years so need to switch order of deprivation round 
  # Add mapping vector
  mapping <- c(`5`=1, `4`=2, `3`=3, `2`=4, `1`=5)
  
  simd_2006 <- readRDS(paste0("/conf/linkage/output/",
                              "lookups/Unicode/Deprivation/",
                              "postcode_2009_2_simd2006.rds")) %>%
    dplyr::select(pc7, simd2006_sc_quintile) %>%
    rename(postcode = pc7,
           simd = simd2006_sc_quintile) %>%
    mutate(simd = recode(simd, !!!mapping)) %>% 
    mutate(year = "simd_2006")
  
  # Combine postcode lookups into a single dataset
  simd_all <- bind_rows(simd_2020, simd_2016, simd_2012, simd_2009, simd_2006) %>%
    pivot_wider(names_from = year, values_from = simd) %>%
    mutate(postcode = format_postcode(postcode, format="pc8")) 
  
  #join SMR data to SIMD, with the correct SIMD lookup for given years
  SMR <- SMR %>%
    #add a year column to SMR first 
    mutate(year = year(admission_date)) %>% 
    
    # filter out rows from years outside current range (not sure why these would be here but are causing issues - Al)
    filter(year %in% c(year(start_date_short), year(end_date_short))) %>% 
    
    # join simd data and fill in simd column according to year
    left_join(simd_all, by = c("postcode" = "postcode"))%>%
    dplyr::mutate(simd = dplyr::case_when(
      year >= 2017 ~ simd_2020,
      year >= 2014 & year < 2017 ~ simd_2016,
      year >= 2010 & year < 2014 ~ simd_2012,
      year >= 2007 & year < 2010 ~ simd_2009,
      year >= 2004 & year < 2007 ~ simd_2006
    )) %>%
    
    # Remove the not needed year-specific SIMD variables
    dplyr::select(-c(simd_2006, simd_2009, simd_2012, simd_2016, simd_2020))
  
  
  
  ###################
  # Adjusting population estimates --------------------------------------------
  
  print("Adjusting pop estimates for 2023/2024")
  
  if(unique(SMR$year) %in% c(2023,2024)){
    
    # Due to missing population estimates for 2023 and 2024, using 2022/23 estimates
    # for these years
    
    pop_iz <- pop_iz %>% filter(year == 2022) 
    
    if(unique(SMR$year) == 2024){
      pop_board <- pop_board %>% filter(year == 2023)
      
      pop_la <- pop_la %>% filter(year == 2023)
    }
  }
  
  # This code takes the IZ population lookup and calculates population, adding
  # both sexes together (separate in pop_iz)
  pop_iz_short <- pop_iz |> 
    dplyr::select(c(year, intzone2011,intzone2011name, total_pop)) |> 
    group_by(year, intzone2011, intzone2011name) |> 
    summarise(total_pop_iz = sum(total_pop)) |> 
    ungroup() 
  
  # This code takes the HB population lookup and calculates population, adding
  # both sexes together (separate in pop_board)
  pop_board_short <- pop_board |> 
    dplyr::select(c(year, hb2019, hb2019name, pop)) |> 
    group_by(year, hb2019, hb2019name) |> 
    summarise(total_pop_board = sum(pop)) |> 
    ungroup()
  
  # This code takes the LA population lookup and calculates population, adding
  # both sexes together (separate in pop_la)
  pop_la_short <- pop_la |> 
    dplyr::select(c(year, ca2019, ca2019name, pop)) |> 
    group_by(year, ca2019, ca2019name) |> 
    summarise(total_pop_la = sum(pop)) |> 
    ungroup()
  
  #Account for missing population estimates
  if(unique(SMR$year) == 2024){
    pop_iz_short %<>% mutate(year = 2024)
    pop_board_short %<>% mutate(year = 2024)
    pop_la_short %<>% mutate(year = 2024)
  }
  
  if(unique(SMR$year) == 2023){
    pop_iz_short %<>% mutate(year = 2023)
  }
  
  
  # 8. Join Weather data together then join with SMR ----------------------------------------------------------
  
  print("Linking met and SMR data")
  
  # Get unique IZ level areas from postcode data
  geos <- pc %>% dplyr::select(-pc8) %>%
    unique()
  
  # select only the quarter for linkage of metgeo data 
  met_geo1 <- met_geo |> 
    filter(time >= start_date_short & time <= end_date_short)
  
  # select only the quarter for linkage of humid_geo data 
  humid_geo1 <- humid_geo |> 
    filter(time >= start_date_short & time <= end_date_short) 
  
  # Join weather data, add date for SMR linkage and join to geog data
  temp_hum <- left_join(met_geo1, humid_geo1, by = c("InterZone", "time")) %>% 
    mutate(date = as.Date(time, format = "%Y-%m-%d")) %>%
    left_join(geos, by = c("InterZone" = "intzone2011"))
  
  # Then join to smr data
  met_smr <- left_join(temp_hum, SMR, by = c("InterZone" ="intzone2011", "date" = "admission_date",
                                             "hb2019","hb2019name", "ca2019", "ca2019name")) %>%
    mutate(year = year(date))%>%
    rename(intzone2011 = InterZone)
  
  print("Linking SMR data to population data")
  
  # Join all datasets together
  met_smr_pop <- met_smr |> 
    left_join(pop_iz_short, by = c("year", "intzone2011")) |> 
    left_join(pop_board_short, by = c("year", "hb2019", "hb2019name")) |> 
    left_join(pop_la_short, by = c("year", "ca2019", "ca2019name")) 
  
  # Instances where no postcode provided. (it's actually okay if there's no
  # postcode in a row as it just means that CA/HB/etc had no admissions that day)
  NAs_postcode <- sum(is.na(met_smr_pop$postcode))
  
  # Also only select columns needed for final data.
  
  ### BEFORE CONTINUING, CHECK THAT health board population has NO MISSING DATA
  
  check <- filter(met_smr_pop, is.na(total_pop_board))
  
  print(paste(quarter_start,"to",quarter_end))
  print(paste("There are", nrow(check), "dates with missing data."))
  
  if(nrow(check) != 0){
    
    met_smr_pop <- met_smr_pop |>
      anti_join(check)
    
  }
  
  # 9. Reformatting ready to run model --------------------------------------
  # Data needs to be aggregated into the same format used by ONS ready to be input
  # into DLNM model code
  
  # ONS COLUMNS:
  # date
  # year	
  # month: month of the year (numeric 1:12)
  # day: day of the month (numeric 1:length of month)
  # time: day of full time series (numeric 1:length of data)
  # yday: day of the year (numeric 1:365/6)
  # dow: day of the week (categorical Mon:Sun)
  # region: Sub region of interest (in our case we will do 3 regions: 1. Intzone, 
  #                                 2. NHSBoard, 3. Local Authority)
  # regnames: names of sub regions
  # tmean: Mean daily temperature (tmin+tmax/2) - for now this will be NA
  # tmin: Minimum daily temperature
  # tmax: Maximum daily temperature
  # dewp: ??
  # rh: Relative humidity - for now excluded
  # admissions: number of hospital admissions
  # pop: population size
  
  data = met_smr_pop
  iz_pop = pop_iz_short
  pop_data = pop_board_short
  # geography_code = "ca2019"
  geography_code = "hb2019"
  # geography_name = "ca2019name"
  geography_name = "hb2019name"
  # population = "total_pop_la"
  population = "total_pop_board"
  
  # rename columns inline with DLNM (also helps standardise when running function 
  # with different geographies)
  hosp_admissions <- data %>% 
    filter(!is.na(population),  # filtering missing region pop estimates
           !(stringr::str_detect(intzone2011name.x, "^IZ")) # filtering out IZ zones with IZXX instead of name (these are duplicates)
    ) %>% # filtering missing region pop estimates
    rename(pop = population,
           tmean = max_temperature,
           humidity = humidity,
           covid_adm = covid_adm,
           region = geography_code,
           regnames = geography_name) %>%
    # the following three lines remove any duplicate link no's on each day,
    # but keep the NAs (i.e. 0 admissions) as these rows are needed for temp/humidity data
    group_by(date, link_no) %>%
    filter(is.na(link_no) | row_number() == 1) %>%
    ungroup() %>%
    group_by(year, date, region, regnames, pop) %>%
    summarise(tmean = sum(tmean*pop/sum(pop)),
              humidity = sum(humidity*pop/sum(pop)),
              simd_mean = mean(simd, na.rm = TRUE),
              admissions = n_distinct(link_no, na.rm = TRUE),
              covid_adm = sum(covid_adm, na.rm = TRUE),
              adm_under_65yrs = sum(age_in_years < 65, na.rm = TRUE),
              adm_65yrs_over = sum(age_in_years >= 65, na.rm = TRUE),
              adm_simd1 = sum(simd == 1, na.rm = TRUE),
              adm_simd2 = sum(simd == 2, na.rm = TRUE),
              adm_simd3 = sum(simd == 3, na.rm = TRUE),
              adm_simd4 = sum(simd == 4, na.rm = TRUE),
              adm_simd5 = sum(simd == 5, na.rm = TRUE),
              # adm_simdna = sum(is.na(simd) & !is.na(link_no)),
              adm_males = sum(sex == 1, na.rm = TRUE),
              adm_females = sum(sex == 2, na.rm = TRUE)
    ) %>%
    ungroup()# %>% 
    # this mutate just helps check that the total number of hosp admissions match for each region 
    # mutate(adm_sum_check = (adm_under_65yrs + adm_65yrs_over) == admissions) %>%
    # filter(adm_sum_check == FALSE)
    # mutate(sex_sum_check = (adm_males + adm_females) == admissions) %>%
    # filter(sex_sum_check == FALSE)
  # mutate(simd_sum_check = (adm_simd1 + adm_simd2 + adm_simd3 +
  #                            adm_simd4 + adm_simd5 + adm_simdna) == admissions) %>%
  #   filter(simd_sum_check == FALSE)
  
  
  pop_dat <- pop_data %>%
    rename(region = hb2019)
  
  # create other variables required for DLNM
  
  hosp_admissions <- hosp_admissions %>%
    mutate(month = month(date),
           day = day(date),
           yday = yday(date),
           dow = wday(date)) 
  
  
  # Create time variable:
  unique_dates <- hosp_admissions %>%
    distinct(date) %>%
    mutate(time = seq(1, n())) %>%
    dplyr::select(date, time)
  
  # Join time variable back to hosp_admissions data & add an admissions column
  hosp_admissions <- hosp_admissions %>%
    left_join(unique_dates, by = c("date" = "date")) %>%
    group_by(date, year, month, day,	time,	yday,	dow,
             region, regnames, simd_mean,	tmean, pop, admissions, covid_adm, humidity) %>% 
    ungroup
  
  # 9. Save out data ---------------------------------------------
  
  if(geography_name == "hb2019name"){
    write.csv(hosp_admissions, paste0("/conf/quality_indicators/Climate/data/base_data/heat_hosps_data_ephss20_near_ALLcovid/",
                                      start_mmmYY,"-",end_mmmYY,"heat_hum_hosps_data_nhsboard_vuln_split.csv"))
    # write.csv(hosp_admissions, paste0("/conf/quality_indicators/Climate/data/base_data/all_hosps_data_ephss20_near_ALLcovid/",
    #                                   start_mmmYY,"-",end_mmmYY,"heat_hum_hosps_data_nhsboard_vuln_split.csv"))
  }
  if(geography_name == "ca2019name"){
    write.csv(hosp_admissions, paste0("/conf/quality_indicators/Climate/data/base_data/heat_hosps_data_ephss20_near_ALLcovid/",
                                      start_mmmYY,"-",end_mmmYY,"heat_hum_hosps_data_councilarea_vuln_split.csv"))
    # write.csv(hosp_admissions, paste0("/conf/quality_indicators/Climate/data/base_data/all_hosps_data_ephss20_near_ALLcovid/",
    #                                   start_mmmYY,"-",end_mmmYY,"heat_hum_hosps_data_councilarea_vuln_split.csv"))
  }
}

###    ONLY RUN ONE OF THESE:    ###

## Run quarterly data extraction: Use either 1) or 2)
# given date range:

# 1) 
# For TESTING change these and run code within function - use the exact days that you want here though eg. 2023-01-01 to 2023-03-31
# debug(met_smr_analysis)
# met_smr_analysis(quarter_start = "2004-01-01",
#                  quarter_end = "2004-01-05")

# 2)
# For full date range
# Remember to update for line 57: q_start_dates[?]
# add time to run the script also
start <- Sys.time()
# debug(met_smr_analysis)
map2(q_start_dates,q_end_dates, ~
       met_smr_analysis(.x,.y))
end <- Sys.time()

end-start

## End session (useful for running as a night session)
# Get the Process ID of the R session
ppid <- system(paste("ps -o ppid= -p", Sys.getpid()), intern = TRUE)
# Gracefully terminate the R session
system(paste("kill -15", ppid))