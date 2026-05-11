#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# simd_pop_data_linkage.R
# Sept 2025
# Al Morgan
# 
# This script combines population data from cl-out with deprivation data to
# produce a dataframe with population by healthboard by SIMD to be used
# to calculate relevant attributable rates in the DLNM model.

# A CSV file is saved out to the 'lookups' folder of the repository, in which the
# yearly populations by SIMD level and health board are summarised.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(tidyverse)
library(phsmethods)

# populations 2005 - 2013
datazone_pops2005_2013 <- read_rds("/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2001_pop_est_2001_2014.rds") %>% 
  rename_with(tolower) %>% 
  select(year, datazone2001, sex, contains("age")) %>%
  filter(year >= 2005 & year <= 2013) %>% 
  mutate(pop = rowSums(select(., contains("age")), na.rm = TRUE)) %>%
  select(-contains("age")) %>% 
  group_by(year, datazone2001) %>% 
  summarise(pop = sum(pop)) %>% 
  mutate(datazone2011 = NA, .after = datazone2001)

# populations 2014 - 2024
datazone_pops2014_2024 <- read_csv("/conf/linkage/output/lookups/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2024.csv") %>% 
  select(year, datazone2011, sex, contains("age")) %>%
  filter(year > 2013) %>% 
  mutate(pop = rowSums(select(., contains("age")), na.rm = TRUE)) %>%
  select(-contains("age")) %>% 
  group_by(year, datazone2011) %>% 
  summarise(pop = sum(pop)) %>% 
  mutate(datazone2001 = NA, .before = datazone2011)

datazone_pops2014_2022 %>% 
  distinct(year)

# combine them
datazone_pops <- datazone_pops2005_2013 %>% 
  bind_rows(datazone_pops2014_2022) %>% 
  ungroup()

datazone_pops %>% 
  distinct(year)

# SIMD code from dlnm data linkage script
# Note: up to SIMD 2012, datazone2001 is used, so need to use relevant population data
# based on year
simd_2020 <- readRDS(paste0("/conf/linkage/output/",
                            "lookups/Unicode/Deprivation",
                            "/postcode_2026_1_simd2020v2.rds")) %>%
  dplyr::select(pc7, datazone2011, hb2019, hb2019name, simd2020v2_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2020v2_sc_quintile,
         datazone = datazone2011) %>%
  mutate(year = "simd_2020") 

simd_2016 <- readRDS(paste0("/conf/linkage/output/",
                            "lookups/Unicode/Deprivation",
                            "/postcode_2019_2_simd2016.rds")) %>%
  rename_with(tolower) %>% 
  dplyr::select(pc7,datazone2011, hb2019, hb2019name, simd2016_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2016_sc_quintile,
         datazone = datazone2011) %>%
  mutate(year = "simd_2016")

simd_2012 <- readRDS(paste0("/conf/linkage/output/",
                            "lookups/Unicode/Deprivation/",
                            "postcode_2016_1_simd2012.rds")) %>%
  rename_with(tolower) %>% 
  dplyr::select(pc7, datazone2001, hb2019, hb2019name, simd2012_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2012_sc_quintile,
         datazone = datazone2001) %>%
  mutate(year = "simd_2012")

simd_2009 <- readRDS(paste0("/conf/linkage/output/",
                            "lookups/Unicode/Deprivation/",
                            "postcode_2012_2_simd2009v2.rds")) %>%
  rename_with(tolower) %>% 
  dplyr::select(pc7, datazone2001, hb2019, hb2019name, simd2009v2_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2009v2_sc_quintile,
         datazone = datazone2001,
         ) %>%
  mutate(year = "simd_2009")

# SIMD deprivation order changed from these years so need to switch order of deprivation round 
# Add mapping vector
mapping <- c(`5`=1, `4`=2, `3`=3, `2`=4, `1`=5)

simd_2006 <- readRDS(paste0("/conf/linkage/output/",
                            "lookups/Unicode/Deprivation/",
                            "postcode_2009_2_simd2006.rds")) %>%
  rename_with(tolower) %>% 
  dplyr::select(pc7, datazone2001, hb2019, hb2019name, simd2006_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2006_sc_quintile,
         datazone = datazone2001) %>%
  mutate(simd = recode(simd, !!!mapping)) %>% 
  mutate(year = "simd_2006")

# Combine postcode lookups into a single dataset - note that datazone now encompasses
# the 2001 and 2011 values in one column (depending on the simd year)
simd_all <- bind_rows(simd_2020, simd_2016, simd_2012, simd_2009, simd_2006) %>%
  pivot_wider(names_from = year, values_from = simd) %>%
  distinct(datazone, hb2019, hb2019name, simd_2020, simd_2016, simd_2012, simd_2009, simd_2006)

# Join pop data to simd data
joined_data <- datazone_pops %>% 
  # Create a unified column for joining based on year
  mutate(join_dz = if_else(year < 2014, datazone2001, datazone2011)) %>%
  left_join(simd_all, by = c("join_dz" = "datazone")) %>% 
  # filter(year == 2013)

  mutate(simd = case_when(
    year >= 2017 ~ simd_2020,
    year >= 2014 & year < 2017 ~ simd_2016,
    year >= 2010 & year < 2014 ~ simd_2012,
    year >= 2007 & year < 2010 ~ simd_2009,
    year >= 2004 & year < 2007 ~ simd_2006
  )) %>%
  # Remove the not needed year-specific SIMD variables
  dplyr::select(-c(simd_2006, simd_2009, simd_2012, simd_2016, simd_2020)) %>% 
  filter(!is.na(simd))

# count number of distinct datazones
joined_data %>% 
  distinct(join_dz) # 13,481

# now see if this is the same amount when all SIMD NA rows are removed
joined_data %>% 
  filter(!is.na(simd)) %>% 
  distinct(join_dz) # 13,481 still

# calculate populations for each SIMD in each HB in each year
hb_pop_simd_data <- joined_data %>% 
  group_by(year, hb2019, hb2019name, simd) %>% 
  summarise(pop = sum(pop)) 

# this check just shows that across Scotland the quintiles are even, as
# they are not across healthboards using these values
hb_pop_simd_data %>% 
  ungroup() %>% 
  group_by(year, simd) %>% 
  summarise(pop = sum(pop)) 

# note that data does not go beyond 2024

write_csv(hb_pop_simd_data, "/conf/quality_indicators/Climate/lookups/simd_pops_by_healthboard.csv")
