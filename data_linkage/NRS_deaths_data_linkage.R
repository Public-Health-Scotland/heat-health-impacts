#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NRS_data_linkage_hum_covid.R
# Jan 2025
# Bella Tortora Brayda (updated by Sarah Reed March 2025)
#
# Script to link NRS data with metoffice data 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# SCRIPT CONTENTS:
# This script first calls the setup_environment and condition codes
# before creating 2 functions used later in the script 
# 
#  i) check if any rows have NA
#  ii) reformat data for output
#  
# It then creates a final function which collates deaths, SMR and weather data
# and produces csv files for each quarter in the timeseries. Details are
# provided throughout the script, but the high level process is as follows:
#  
# a) extracts data from NRS deaths for ALL DEATHS
# b) links the deaths extract to geospatial MetOffice data 
# c) links the deaths/weather data to hospital admissions for the reasons 
#    detailed in initial thoughts - 2 Jul commented out this section
# d) reformats the data to be fed into the dlnm model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# testing parallel
library(parallelly)
available_cores <- as.numeric(parallelly::availableCores())
options(Ncpus = available_cores)
Sys.setenv(MAKEFLAGS = paste("-j", as.character(available_cores), sep = ""))

# 1. Set up ---------------------------------------------------------------

# Source relevant scripts
# ~~~~~~~~~~~~~~~~~~~~~~~
source(here::here("setup/setup_environment.R"))

### Create list of quarter start dates for full time period
### This is for use when running the met_deaths_analysis 
### function over the full list of dates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## NEED TO UPDATE EACH TIME: ##

#  Input dates for required quarters
# For a full year, need to give dates like 2019-01-01 to 2020-01-01 -------> NOT to 2019-12-31 

q_start_dates <- as.list(seq.Date(from = as.Date("2005-01-01"),
                                  to = as.Date("2025-01-01"), 
                                  by = "quarter"))

q_end_dates <- map(q_start_dates,~
                     as.Date(as.character(.x),format = "%Y-%m-%d")-1
)

# remove the first date in q_end_dates and last in q_start_date list as they
# are outside time frame
q_end_dates[1] <- NULL
q_start_dates[length(q_start_dates)] <- NULL

causes <- "all" # "heat-related" or "all"
geog <- "hb" # `hb` for health boards, `ca` for council areas


# Functions 
# ~~~~~~~~~

# i) Function to check if any rows have NA and return those rows. 
check_na_rows <- function(data) {
  # Check for rows with any NA values
  rows_with_na <- data %>%
    filter(if_any(everything(), is.na))
  
  # Return the rows with NAs
  return(rows_with_na)
}

# SMRA login information
# ~~~~~~~~~~~~~~~~~~~~~~
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"),
                                      pwd=.rs.askForPassword("SMRA Password:")))

# 2. Create function (met_deaths_analysis) for quarterly outputs ---------------------------

# Just use these if testing code within the function
# quarter_start = "2020-01-01"
# quarter_end = "2020-12-31"

met_deaths_analysis <- function(quarter_start,quarter_end){
  
  # Checks for correctly-named variables
  stopifnot(causes %in% c("heat-related", "all"))
  stopifnot(geog %in% c("hb", "ca"))
  
  # Create list of quarter end dates
  start_date_short <- as.Date(quarter_start, format = "%Y-%m-%d")
  end_date_short <- as.Date(quarter_end, format = "%Y-%m-%d")
  start_date_long <- format(start_date_short, "%d %B %Y")
  end_date_long <- format(end_date_short, "%d %B %Y")
  start_mmmYY <- paste0(format(start_date_short, "%b"),year(start_date_short))
  end_mmmYY <- paste0(format(end_date_short, "%b"),year(end_date_short))
  
  print(paste0("Extracting data from ", start_mmmYY, " to ", end_mmmYY, "..."))
  
  # Lookups and data used in script, loaded in setup_environment:
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # humid_geo
  # met_geo (n.b. met_geo created in data_linkage/metoffice_IZ_linkage.R)
  # hospitals
  # postcode_dir
  
  # 3. NRS deaths extract ----------------------------------------------------
  
  # Query depends on if "heat-related" or "all" causes is selected
  if(causes == "all"){
    deaths <- as_tibble(dbGetQuery(channel, statement = paste0(
      "SELECT LINK_NO, DATE_OF_DEATH, UNDERLYING_CAUSE_OF_DEATH,
              CAUSE_OF_DEATH_CODE_0, CAUSE_OF_DEATH_CODE_1, CAUSE_OF_DEATH_CODE_2, CAUSE_OF_DEATH_CODE_3,
              CAUSE_OF_DEATH_CODE_4, CAUSE_OF_DEATH_CODE_5, CAUSE_OF_DEATH_CODE_6, CAUSE_OF_DEATH_CODE_7,
              CAUSE_OF_DEATH_CODE_8, CAUSE_OF_DEATH_CODE_9,
              DATE_OF_BIRTH, SEX, AGE,
              AGE_LESS_THAN_2_YRS, PLACE_OF_DEATH_POSTCODE, POSTCODE -- need both postcodes as PoD only available from 2009
    FROM ANALYSIS.GRO_DEATHS_C
    WHERE DATE_OF_DEATH between '",start_date_long,"' and '",end_date_long,"'
    "))) %>%
      clean_names() %>%
      arrange(link_no)
  }else if(causes == "heat-related"){
    deaths <- as_tibble(dbGetQuery(channel, statement = paste0(
      "SELECT LINK_NO, DATE_OF_DEATH, UNDERLYING_CAUSE_OF_DEATH,
              CAUSE_OF_DEATH_CODE_0, CAUSE_OF_DEATH_CODE_1, CAUSE_OF_DEATH_CODE_2, CAUSE_OF_DEATH_CODE_3,
              CAUSE_OF_DEATH_CODE_4, CAUSE_OF_DEATH_CODE_5, CAUSE_OF_DEATH_CODE_6, CAUSE_OF_DEATH_CODE_7,
              CAUSE_OF_DEATH_CODE_8, CAUSE_OF_DEATH_CODE_9,
              DATE_OF_BIRTH, SEX, AGE,
              AGE_LESS_THAN_2_YRS, PLACE_OF_DEATH_POSTCODE, POSTCODE -- need both postcodes as PoD only available from 2009
    FROM ANALYSIS.GRO_DEATHS_C
    WHERE DATE_OF_DEATH between '",start_date_long,"' and '",end_date_long,"'
    AND regexp_like(UNDERLYING_CAUSE_OF_DEATH || CAUSE_OF_DEATH_CODE_0 || CAUSE_OF_DEATH_CODE_1
          || CAUSE_OF_DEATH_CODE_3 || CAUSE_OF_DEATH_CODE_4 || CAUSE_OF_DEATH_CODE_5 ||
           CAUSE_OF_DEATH_CODE_6 || CAUSE_OF_DEATH_CODE_7 || CAUSE_OF_DEATH_CODE_8 ||
           CAUSE_OF_DEATH_CODE_9, '", all ,"')
    "))) %>%
      clean_names() %>%
      arrange(link_no)
  }
  
  # Add covid column for those who died due to covid ------------------------------
  
  deaths <- deaths %>% 
    mutate(covid_death = ifelse(underlying_cause_of_death == "U071" | underlying_cause_of_death == "U072"|
                                  cause_of_death_code_0 == "U071" | cause_of_death_code_0 == "U072"|
                                  cause_of_death_code_1 == "U071" | cause_of_death_code_1 == "U072"|
                                  cause_of_death_code_2 == "U071" | cause_of_death_code_2 == "U072"|
                                  cause_of_death_code_3 == "U071" | cause_of_death_code_3 == "U072"|
                                  cause_of_death_code_4 == "U071" | cause_of_death_code_4 == "U072"|
                                  cause_of_death_code_5 == "U071" | cause_of_death_code_5 == "U072"|
                                  cause_of_death_code_6 == "U071" | cause_of_death_code_6 == "U072"|
                                  cause_of_death_code_7 == "U071" | cause_of_death_code_7 == "U072"|
                                  cause_of_death_code_8 == "U071" | cause_of_death_code_8 == "U072",
                                1, 0))
    # more efficient method, but possibly harder to understand
    # mutate(covid_death = if_else(
    #   if_any(c("underlying_cause_of_death", paste0("cause_of_death_code_", 0:8)), ~ .x %in% c("U071", "U072")),
    #   1, 0
    # ))
  
  
  # 4. link deaths to geography ----------------------------------------------------
  
  # - use place of death postcode for linkage
  
  print("linking postcode data")
  
  # format postcode to pc8 in deaths data to match with same type in postcode data
  # - `place_of_death_postcode` only exists from 2009 onwards so use `postcode` until then
  #   (which is based on the person's place of residence)
  if(start_date_short < "2009-01-01"){
    deaths_pc <- deaths %>% 
      mutate(place_of_death_postcode = format_postcode(postcode, format="pc8")) 
  }else{ # From 2009 onwards, use place_of_death_postcode when available, postcode if not
    deaths_pc <- deaths %>% 
      mutate(place_of_death_postcode = case_when(
        !is.na(place_of_death_postcode) ~ format_postcode(place_of_death_postcode, format="pc8"),
        !is.na(postcode) ~ format_postcode(postcode, format="pc8"),
        TRUE ~ NA
      ))
  }
  
  # Count number of rows without a postcode, run a check, and then exclude these
  postcode_na_n <- deaths_pc %>% 
    filter(is.na(place_of_death_postcode)) %>% 
    nrow()
  
  if(postcode_na_n/nrow(deaths_pc) > 0.01){
    warning("More than 1% of rows of deaths data have been excluded due to
            lack of postcode - consider checking data quality.")
    Sys.sleep(5)
  }
  
  deaths_pc <- deaths_pc %>% 
    filter(!is.na(place_of_death_postcode))
  
  # select just the postcode and IZ info (postcode_dir loaded in setup_environment.R)
  # postcode_dir no longer contains intzone2011name (only 2022 name), join from pop_iz
  pc <- postcode_dir %>% 
    dplyr::select(pc8, intzone2011, hb2019, hb2019name, ca2019, ca2019name) %>% 
    left_join(pop_iz %>% select(intzone2011, intzone2011name), by = "intzone2011") %>% 
    relocate(intzone2011name, .after = intzone2011) %>% 
    distinct() # remove duplicates
  
  
  print("Joining pc data to deaths")
  
  ## Check for any incorrect postcodes and remove if present
  deaths_pc_check1 <- deaths_pc %>% 
    select(place_of_death_postcode) %>% 
    anti_join(pc, by = c("place_of_death_postcode" = "pc8")) %>% 
    distinct(place_of_death_postcode) %>% 
    pull()
  
  # If any exist, confirm this using postcode directory and then filter them out of deaths_pc
  if(length(deaths_pc_check1) > 0){
    test_that("Confirm that incorrect postcodes don't exist in the postcode directory",
              expect_false(any(pc$pc8 %in% deaths_pc_check1))
    )
    
    deaths_pc <- deaths_pc %>% 
      filter(!(place_of_death_postcode %in% deaths_pc_check1))
  }
  
  ## Check that postcode is working correctly for the data join
  deaths_pc_check2 <- deaths_pc %>%
    sample_n(100) %>% 
    select(place_of_death_postcode) %>% 
    left_join(pc %>% filter(!(stringr::str_detect(intzone2011name, "^IZ"))), # filtering out IZs with IZXX instead of name (these are duplicates and are properly dealt with later)
              by = c("place_of_death_postcode" = "pc8")) 
 
  test_that("100 randomly-selected rows in deaths data join to postcode data correctly",
      expect_equal(nrow(deaths_pc_check2), 100)
  )
  
  # Add intermediate zone column to SMR data by joining with selected postcode data
  deaths_pc <- left_join(deaths_pc, pc, by = c("place_of_death_postcode" = "pc8"))
  
  deaths_pc %<>%
    tidylog::distinct(link_no, .keep_all = TRUE) %>%
    dplyr::mutate(year = as.integer(year(date_of_death))) 
  
  sum(is.na(deaths_pc$intzone2011)) 
  
  unmatched <- anti_join(deaths_pc, pc, by=c("place_of_death_postcode" = "pc8")) 
  
  test_that("Number of death postcodes that can't be located in postcode directory should be 0",
            expect_equal(nrow(unmatched), 0)
  )

  if(nrow(unmatched)/nrow(deaths_pc) > 0.01){
    perc_missing <- round((nrow(unmatched)/nrow(deaths_pc))*100, 3)
    warning(
      paste(
        paste0("When extracting data from ", start_mmmYY, " to ", end_mmmYY),
        paste0(perc_missing, "% of rows of deaths data were excluded due to lack of postcode - consider checking data quality.")
      ))
    Sys.sleep(5)
  }
  
  # 5. SIMD lookups and combined file ---------------------------------------
  simd_2020 <- readRDS(paste0("/conf/linkage/output/",
                              "lookups/Unicode/Deprivation",
                              "/postcode_2026_1_simd2020v2.rds")) %>%
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
  
  simd_2006 <- readRDS(paste0("/conf/linkage/output/",
                              "lookups/Unicode/Deprivation/",
                              "postcode_2009_2_simd2006.rds")) %>%
    dplyr::select(pc7, simd2006_sc_quintile) %>%
    rename(postcode = pc7,
           simd = simd2006_sc_quintile) %>%
    mutate(simd = dplyr::case_match(simd,
                                    5 ~ 1,
                                    4 ~ 2,
                                    3 ~ 3,
                                    2 ~ 4,
                                    1 ~ 5)) %>%
    mutate(year = "simd_2006")
  
  # Combine postcode lookups into a single dataset
  simd_all <- bind_rows(simd_2020, simd_2016, simd_2012, simd_2009, simd_2006) %>%
    pivot_wider(names_from = year, values_from = simd) %>%
    mutate(postcode = format_postcode(postcode, format="pc8")) 
  
  deaths_pc <- deaths_pc %>%
    left_join(simd_all, by = c("place_of_death_postcode" = "postcode"))%>%
    dplyr::mutate(simd = dplyr::case_when(
      year >= 2017 ~ simd_2020,
      year >= 2014 & year < 2017 ~ simd_2016,
      year >= 2010 & year < 2014 ~ simd_2012,
      year >= 2007 & year < 2010 ~ simd_2009,
      year >= 2004 & year < 2007 ~ simd_2006
    )) %>%
    
    # Remove the not needed year-specific SIMD variables
    dplyr::select(-c(simd_2006, simd_2009, simd_2012, simd_2016, simd_2020))
  
  
  
  # 6. Population extract -----------------------------------------------------------
  
  # Add population lookup for each intermediate zone, hb and LA
  # first, need to 
  # 1) select only relevant columns from pop file (and aggregate across sex)
  #     for each of the three population files (IZ, board and LA)
  # 2) create a 'year' column in smr_nrs to link accurate pop estimates
  # 
  # Note: pop_iz, pop_board and pop_la are read in via setup_environment.R,
  # sourced at the start of this script

  print("Adjusting pop estimates for 2025")
  
  # Due to missing population estimates for 2025, using 2024 estimate
  if(unique(deaths_pc$year) == 2025){
    
    pop_iz <- pop_iz %>%
      filter(year == 2024) 
    
    pop_board <- pop_board %>%
      filter(year == 2024)
    
    pop_la <- pop_la %>%
      filter(year == 2024)
  }
  
  pop_iz_short <- pop_iz |> 
    dplyr::select(c(year, intzone2011,intzone2011name, total_pop)) |> 
    group_by(year, intzone2011, intzone2011name) |> 
    summarise(total_pop_iz = sum(total_pop)) |> 
    ungroup() 
  
  pop_board_short <- pop_board |> 
    dplyr::select(c(year, hb2019, hb2019name, pop)) |> 
    group_by(year, hb2019, hb2019name) |> 
    summarise(total_pop_board = sum(pop)) |> 
    ungroup()
  
  pop_la_short <- pop_la |> 
    dplyr::select(c(year, ca2019, ca2019name, pop)) |> 
    group_by(year, ca2019, ca2019name) |> 
    summarise(total_pop_la = sum(pop)) |> 
    ungroup()
  
  test_that("Population totals are equal for 2024",
            expect_equal(
              pop_iz_short %>% filter(year == 2024) %>% summarise(sum(total_pop_iz)) %>% pull(),
              pop_board_short %>% filter(year == 2024) %>%  summarise(sum(total_pop_board)) %>% pull(),
              pop_la_short %>% filter(year == 2024) %>%  summarise(sum(total_pop_la)) %>% pull()            )
  )
  
  # Adjust summarised population dataframes for missing 2025 data
  if(unique(deaths_pc$year) == 2025){
    pop_iz_short %<>% mutate(year = 2025)
    pop_board_short %<>% mutate(year = 2025)
    pop_la_short %<>% mutate(year = 2025)
  }
  
  # 7. Met_geo data match with nrs ----------------------------------------------------------
  
  print("Linking met and NRS data")
  
  geos <- pc %>% dplyr::select(-pc8) %>%
    unique()
  
  # select only the quarter for linkage of metgeo data 
  met_geo1 <- met_geo |> 
    filter(time >= start_date_short & time <= end_date_short)
  #%>%
  #  mutate(date = as.Date(time, format = "%Y-%m-%d")) %>%
  #  left_join(geos, by = c("InterZone" = "intzone2011"))
  
  # select only the quarter for linkage of humid_geo data 
  humid_geo1 <- humid_geo |> 
    filter(time >= start_date_short & time <= end_date_short)
  
  # Join weather data, add date for SMR linkage and join to geog data
  temp_hum <- left_join(met_geo1, humid_geo1, by = c("InterZone", "time")) %>% 
    mutate(date = as.Date(time, format = "%Y-%m-%d")) %>%
    left_join(geos, by = c("InterZone" = "intzone2011"))
  
  met_nrs <- left_join(temp_hum, deaths_pc, by = c("InterZone" ="intzone2011", "date" = "date_of_death",
                                                   "hb2019","hb2019name", "ca2019", "ca2019name")) %>%
    mutate(year = year(date))%>%
    rename(intzone2011 = InterZone)
  
  print("Linking deaths data to population data")
  
  # Join all datasets together
  met_nrs <- met_nrs |> 
    left_join(pop_iz_short, by = c("year", "intzone2011")) |> 
    left_join(pop_board_short, by = c("year", "hb2019", "hb2019name")) |> 
    left_join(pop_la_short, by = c("year", "ca2019", "ca2019name")) 
  
  # Some instances where no place_of_death_postcode provided or associated hospital 
  # admission. Exclude for now. Also only select columns needed for final
  # data.
  
  ### A check for populations with missing data
  
  #check <- check_na_rows(met_nrs)
  
  check <- filter(met_nrs, is.na(total_pop_iz))
  print(paste(quarter_start,"to",quarter_end))
  print(paste("There are", nrow(check), "dates with missing data."))
  
  # # Exclude NA's
  
  if(nrow(check) != 0){
    
    met_nrs <- met_nrs |>
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
  # death: number of deaths
  # pop: population size
  
  
  data = met_nrs
  iz_pop = pop_iz_short
  pop_data = pop_board_short
  # based on geography choice: code, name, and population values are assigned
  if(geog == "hb"){
    geography_code <- "hb2019"
    geography_name <- "hb2019name"
    population <-  "total_pop_board"
  } else if(geog == "ca"){
    geography_code <- "ca2019"
    geography_name <- "ca2019name"
    population <-  "total_pop_la"
  }
  
  # rename columns inline with DLNM (also helps standardise when running function 
  # with different geographies)
  final_deaths <- data %>%
    filter(!is.na(total_pop_board), # filtering missing hb pop estimates
           !(stringr::str_detect(intzone2011name.x, "^IZ"))) %>% # filtering out IZ zones with IZXX instead of name (these are duplicates)
    rename(pop = all_of(population), # need to use all_of() to call in named object
           tmean = max_temperature,
           humidity = humidity,
           covid_death = covid_death,
           region = all_of(geography_code),
           regnames = all_of(geography_name)
    ) %>%
    group_by(year, date, region, regnames, pop) %>%
    summarise(tmean = sum(tmean*pop/sum(pop)),
              humidity = sum(humidity*pop/sum(pop)),
              simd_mean = mean(simd, na.rm = TRUE),
              death = sum(!is.na(link_no)), # all link nos
              covid_death = sum(covid_death, na.rm = TRUE), 
              death_under_65yrs = sum(age < 65, na.rm = TRUE),
              death_65yrs_over = sum(age >= 65, na.rm = TRUE),
              death_simd1 = sum(simd == 1, na.rm = TRUE),
              death_simd2 = sum(simd == 2, na.rm = TRUE),
              death_simd3 = sum(simd == 3, na.rm = TRUE),
              death_simd4 = sum(simd == 4, na.rm = TRUE),
              death_simd5 = sum(simd == 5, na.rm = TRUE),
              death_simdna = sum(is.na(simd) & !is.na(link_no)), # including for the test below
              death_males = sum(sex == 1, na.rm = TRUE),
              death_females = sum(sex == 2, na.rm = TRUE),
              death_sex_other = sum(sex %in% c(0, 9), na.rm = TRUE), # unknown/intersex
    ) %>% 
    ungroup() 
  
  test_that("Deaths by all population subgroups equal total deaths", {
    expect_equal(final_deaths$death_under_65yrs + final_deaths$death_65yrs_over, final_deaths$death)
    expect_equal(final_deaths$death_males + final_deaths$death_females +
                   final_deaths$death_sex_other, final_deaths$death)
    expect_equal(final_deaths$death_simd1 + final_deaths$death_simd2 +
                   final_deaths$death_simd3 + final_deaths$death_simd4 +
                   final_deaths$death_simd5 + final_deaths$death_simdna, final_deaths$death)
  })
  
  
  
  pop_dat <- pop_data %>%
    rename(region = hb2019)
  
  # create other variables required for DLNM
  
  final_deaths <- final_deaths %>%
    mutate(month = month(date),
           day = day(date),
           yday = yday(date),
           dow = wday(date)) 
  
  # Create time variable:
  unique_dates <- final_deaths %>%
    distinct(date) %>%
    mutate(time = seq(1, n())) %>%
    dplyr::select(date, time)
  
  # Join time variable back to deaths data & add an deaths column
  final_deaths <- final_deaths %>%
    left_join(unique_dates, by = c("date" = "date")) %>%
    # health board population being used here:
    group_by(date, year, month, day,	time,	yday,	dow,
             region,	regnames, simd_mean,	tmean, pop, death, covid_death, humidity) %>% 
    ungroup()
  
  # 9. Save out data ---------------------------------------------
  
  if(geography_name == "hb2019name"){
    if(causes == "all"){
      write.csv(final_deaths, paste0("/conf/quality_indicators/Climate/data/base_data/all_deaths_data_ephss20_near_ALLcovid/",
                                     start_mmmYY,"-",end_mmmYY,"all_deaths_data_nhsboard_vuln_split.csv"))
    }else   if(causes == "heat-related"){
      write.csv(final_deaths, paste0("/conf/quality_indicators/Climate/data/base_data/heat_deaths_data_ephss20_near_ALLcovid/",
                                     start_mmmYY,"-",end_mmmYY,"heat_deaths_data_nhsboard_vuln_split.csv"))
    }
  }
  if(geography_name == "ca2019name"){
    if(causes == "all"){
      write.csv(final_deaths, paste0("/conf/quality_indicators/Climate/data/base_data/all_deaths_data_ephss20_near_ALLcovid/",
                                     start_mmmYY,"-",end_mmmYY,"all_deaths_data_councilarea_vuln_split.csv"))
    }else if(causes == "heat-related"){
      write.csv(final_deaths, paste0("/conf/quality_indicators/Climate/data/base_data/heat_deaths_data_ephss20_near_ALLcovid/",
                                     start_mmmYY,"-",end_mmmYY,"heat_deaths_data_councilarea_vuln_split.csv"))
    }
  }
}


# full date range
start <- Sys.time()
map2(q_start_dates,q_end_dates, ~
       met_deaths_analysis(.x,.y))
end <- Sys.time()

end-start # gives time duration


