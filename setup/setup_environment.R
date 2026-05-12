#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# setup_environment.R
# Jan 2025 (updated Feb 2025 for EPHSS data)
# Bella Tortora Brayda 
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1 - Libraries -----------------------------------------------------------

library(odbc) #load first to avoid masking of certain functions within dplyr
library(dplyr) # best package for data manipulation and more
library(magrittr) # for pipes
library(readr) # for reading in data
library(janitor) # for cleaning data
library(tidylog) # for useful in console dplyr messages
library(stringr) # for working with strings
library(purrr) # for mapping 
library(phsmethods) # useful PHS specific functions
library(ggplot2) # For visualisations
library(lubridate) # For working with dates
library(tidyr) # for tidying data
library(testthat) # for data-checking tests within scripts

#Note: to install geospatial packages see the script geospatial_install.R


# 2 - arguments -----------------------------------------------------------

# keep only the data you would like read in uncommented and check comma placement

data_type <- paste(
  "temperature",
  "humidity"
)

# 3 - data folders --------------------------------------------------------

climate_folder <- "/conf/quality_indicators/Climate/"

# 4 - Lookups -------------------------------------------------------------

# Scottish Postcode Directory
#
#source(here::here("functions/most_recent_postcode_lookup.R"))
# Call function to read the most recent postcode lookup
#postcode_dir <- readRDS(most_recent_postcode_lookup(postcode_folder)) |>
#  mutate(pc8 = format_postcode(pc8, format="pc8"))

# Read in 2025_1 postcode directory
postcode_dir <- readRDS("/conf/linkage/output/lookups/Unicode/Geography/Scottish Postcode Directory/Scottish_Postcode_Directory_2026_1.rds") %>% 
  mutate(pc8 = format_postcode(pc8, format="pc8"))


hospitals <- read.csv(paste0("https://www.opendata.nhs.scot/dataset/",
                             "cbd1802e-0e04-4282-88eb-d7bdcfb120f0/",
                             "resource/c698f450-eeed-41a0-88f7-c1e40a568acc/",
                             "download/hospitals.csv"))

# Population estimates:
# using previous census estimates:

pop_iz1 <- readRDS(paste0("/conf/linkage/output/lookups/Unicode/Populations/Estimates",
                      "/IntZone2011_pop_est_5year_agegroups_2011_2024.rds"))

pop_iz2 <- readRDS(paste0("/conf/linkage/output/lookups/Unicode/Populations/Estimates",
                         "/IntZone2011_pop_est_5year_agegroups_2001_2010.rds"))

pop_iz <- bind_rows(pop_iz2, pop_iz1) 

rm(pop_iz1, pop_iz2)


pop_board <- readRDS(paste0("/conf/linkage/output/lookups/Unicode/Populations/Estimates",
                            "/HB2019_pop_est_5year_agegroups_1981_2024.rds"))

pop_la <- readRDS(paste0("/conf/linkage/output/lookups/Unicode/Populations/Estimates",
                         "/CA2019_pop_est_5year_agegroups_1981_2024.rds"))

# 5 - Load data -----------------------------------------------------------

# temperature

### CHECK file path ###

# Read in data from files
if(str_detect(data_type,"temperature")){
  #  list_of_files <- list.files(path = paste0(climate_folder,"/data/ephss_data/Final Weather data"),
  list_of_files <- list.files(path = paste0(climate_folder,"/data/ephss_data/Final Weather data/nearest_station"),
                              recursive = TRUE,
                              #                           pattern = "\\MaxTemperature.csv$",
                              pattern = "\\MaxTemperature_nearest_station.csv$",
                              full.names = TRUE)
  met_geo <- readr::read_csv(list_of_files, skip = 2)
}

# Change names of columns to match what we want in linkage code
met_geo <- met_geo %>% 
  dplyr::rename(InterZone = "Site identifier") %>% 
  dplyr::rename(time = Time) %>% 
  dplyr::rename(max_temperature = Value) %>% 
  select(-c(Latitude, Longitude))

met_geo$time <- as.POSIXct(met_geo$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# humidity
# Read in data from files
if(str_detect(data_type,"humidity")){

list_of_files2 <- list.files(path = paste0(climate_folder,"/data/ephss_data/Final Weather data/nearest_station"),
                            recursive = TRUE,
                            pattern = "\\RelHumidity_nearest_station.csv$",
                            full.names = TRUE)

humid_geo <- readr::read_csv(list_of_files2, skip = 2)
}

# check for NAs
sum(is.na(humid_geo))

# check for spurious values of over 100% that we were warned about
sum(humid_geo$Value > 100)

# Update values over 100 to be 100
humid_geo$Value <- ifelse(humid_geo$Value > 100, 100, humid_geo$Value)

# Change names of columns to match what we want in linkage code
humid_geo <- humid_geo %>% 
  dplyr::rename(InterZone = "Site identifier") %>% 
  dplyr::rename(time = Time) %>% 
  dplyr::rename(humidity = Value) %>% 
  select(-c(Latitude, Longitude))

humid_geo$time <- as.POSIXct(humid_geo$time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

### Finding missing data ###
#temp_hum <- left_join(met_geo, humid_geo, by = c("InterZone", "time"))
#sum(is.na(temp_hum))

#temp_hum2 <- anti_join(met_geo, humid_geo, by = c("InterZone", "time")) # 5274 missing humidity

#IZs_missing_humidity <- temp_hum2 %>%
#  group_by(InterZone) %>%
#  summarise(Count = n())

### Finding missing data ###
#temp_hum <- left_join(met_geo, humid_geo, by = c("InterZone", "time"))
#sum(is.na(temp_hum))

#temp_hum2 <- anti_join(met_geo, humid_geo, by = c("InterZone", "time")) # 5274 missing humidity

#IZs_missing_humidity <- temp_hum2 %>%
#  group_by(InterZone) %>%
#  summarise(Count = n())
