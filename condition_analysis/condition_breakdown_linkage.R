# This is the script containing the SQL query to count the different condition
# types for heat-related hospitalisations


library(tidyverse)

# parallel programming
library(parallelly)
available_cores <- as.numeric(parallelly::availableCores())
options(Ncpus = available_cores)
Sys.setenv(MAKEFLAGS = paste("-j", as.character(available_cores), sep = ""))

source(here::here("setup/setup_environment.R"))
source(here::here("lookups/condition_codes.R")) # needed for SQL query

q_start_dates <- as.list(seq.Date(from = as.Date("2007-10-01"),
                                  to = as.Date("2025-01-01"),
                                  by = "quarter"))

q_end_dates <- map(q_start_dates,~
                     as.Date(as.character(.x),format = "%Y-%m-%d")-1)

# remove the first date in q_end_dates and last in q_start_date list as they
# are outside time frame
q_end_dates[1] <- NULL
q_start_dates <- q_start_dates[-length(q_start_dates)]
# q_start_dates[81]<-NULL


# SMRA login information
# ~~~~~~~~~~~~~~~~~~~~~~
channel <- suppressWarnings(dbConnect(odbc(),  dsn="SMRA",
                                      uid=.rs.askForPassword("SMRA Username:"),
                                      pwd=.rs.askForPassword("SMRA Password:")))

summer_months <- c(6,7,8,9)

summarise_conditions <- function(quarter_start,quarter_end){
  
  # Create list of quarter end dates
  start_date_short <- as.Date(quarter_start, format = "%Y-%m-%d")
  end_date_short <- as.Date(quarter_end, format = "%Y-%m-%d")
  start_date_long <- format(start_date_short, "%d %B %Y")
  end_date_long <- format(end_date_short, "%d %B %Y")
  start_mmmYY <- paste0(format(start_date_short, "%b"),year(start_date_short))
  end_mmmYY <- paste0(format(end_date_short, "%b"),year(end_date_short))
  
  # run the code only when the quarter contains at least 1 month between June and Sept
  if(month(start_date_short) %in% summer_months | month(end_date_short) %in% summer_months){
    
    print(paste0("Extracting data from ", start_mmmYY, " to ", end_mmmYY, "..."))
    
    
    smr01_extract <- as_tibble(dbGetQuery(channel, statement = paste0(
      "SELECT link_no, cis_marker, sex, age_in_years, admission_date,
           admission_type, main_condition, other_condition_1, other_condition_2, 
           other_condition_3, other_condition_4, other_condition_5, 
           postcode, location
      FROM ANALYSIS.SMR01_PI  z
      WHERE exists(
        SELECT *
        FROM ANALYSIS.SMR01_PI
        WHERE link_no=z.link_no and cis_marker=z.cis_marker
          AND(
            REGEXP_LIKE(main_condition, '", all, "') OR
            REGEXP_LIKE(other_condition_1, '", all, "') OR
            REGEXP_LIKE(other_condition_2, '", all, "') OR
            REGEXP_LIKE(other_condition_3, '", all, "') OR
            REGEXP_LIKE(other_condition_4, '", all, "') OR
            REGEXP_LIKE(other_condition_5, '", all, "')
          )
          -- AND regexp_like(main_condition || other_condition_1 || other_condition_2
             --      || other_condition_3 || other_condition_4 || other_condition_5, '", all ,"')
          --AND (admission_date between '",start_date_long,"' and '",end_date_long,"')
      )
      AND (admission_date between '",start_date_long,"' and '",end_date_long,"')"))) %>%
      clean_names() %>%
      filter(
        !str_detect(admission_type, "^[12]"), # filter out elective admissions
        month(admission_date) %in% summer_months # filter down to summer months
      ) %>% 
      arrange(link_no, admission_date)
    #### Postcode matching and SIMD retrieval ####
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
    
    # Extract the first episode of a CIS
    SMR %<>%
      distinct(link_no, cis_marker, .keep_all = TRUE)
    
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
    
    #### Link to weather data ####
    
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
      rename(intzone2011 = InterZone) %>% 
      filter(!is.na(link_no)) # remove rows/days with 0 hospitalisations
    
    
    #### Condition breakdown ####
    
    # Define all possible categories
    all_conditions <- c("alzheimers", "dehydration", "cardiovascular", "dementia",
                        "drowning", "falls", "hot_weather", "injuries", "mental_health",
                        "natural_forces", "parkinson", "renal", "respiratory",
                        "road_incidents", "suicide_selfharm", "violence")
    
    behavioural_conditions <- c("dehydration", "drowning", "falls", "injuries", "mental_health",
                                "natural_forces", "road_incidents", "suicide_selfharm", "violence")
    
    physiological_conditions <- c("alzheimers", "cardiovascular", "dementia",
                                  "hot_weather", "parkinson", "renal", "respiratory")
    
    
    # Remove unnecessary columns
    met_smr_trimmed <- met_smr %>% 
      select(time, year, max_temperature, humidity, link_no, age_in_years, sex, 
             simd, contains("condition")) %>% 
      rename(date = time)
    
    smr01_long <- met_smr_trimmed %>%
      mutate(sex = if_else(sex == 1, "male", "female")) %>% 
      mutate(age_group = if_else(age_in_years < 65, "under 65 yrs", "65 yrs plus"),
             .after = "age_in_years") %>% 
      pivot_longer(cols = contains("condition"),
                   names_to = "condition_type",
                   values_to = "condition_name") %>%
      mutate(condition_category2 = if_else(condition_type == "main_condition", "main", "other")) %>% 
      filter(!is.na(condition_name)) 
    
    condition_summary <- smr01_long %>% 
      mutate(condition_name = case_when(
        str_detect(condition_name, "^G30") ~ "alzheimers",
        str_detect(condition_name, "^I0[0-42]|^I5[0-1]|^I6[0-99]") ~ "cardiovascular",
        str_detect(condition_name, "^U07[1-2]") ~ "covid",
        str_detect(condition_name, "^E86|^X54") ~ "dehydration",
        str_detect(condition_name, "^F01|^F03") ~ "dementia",
        str_detect(condition_name, "^W6[7-9]|^W7[0-4]") ~ "drowning",
        str_detect(condition_name, "^W0[0-4]|^W09|^W1[0-9]") ~ "falls",
        str_detect(condition_name, "^T67[0-9]|^X30|X32") ~ "hot_weather",
        str_detect(condition_name, "^S[00-99]|^T[00-14]") ~ "injuries",
        str_detect(condition_name, "^F[10-63]|^F[67-89]|^F99") ~ "mental_health",
        str_detect(condition_name, "^T750|^X3[3-4]|^X3[6-9]") ~ "natural_forces",
        str_detect(condition_name, "^G70") ~ "parkinson",
        str_detect(condition_name, "^N[0-3][0-9]") ~ "renal",
        str_detect(condition_name, "^J[00-22]|^J30|^J39|^J[40-84]|^J[96-99]") ~ "respiratory",
        str_detect(condition_name, "^V[0-8][0-9]") ~ "road_incidents",
        str_detect(condition_name, "^X6[0-9]|^X7[0-9]|^X8[0-4]|^Y1[0-9]|^Y2[0-9]|^Y3[0-4]") ~ "suicide_selfharm",
        str_detect(condition_name, "^X8[5-9]|^X9[0-9]|^Y0[0-9]|^U50\\.9") ~ "violence",
        TRUE ~ "other"
      )) %>% 
      mutate(condition_label = case_when(
        condition_name %in% behavioural_conditions ~ "behavioural",
        condition_name %in% physiological_conditions ~ "physiological",
        condition_name == "covid" ~ "covid",
        TRUE ~ "other"
      )) %>% 
      group_by(link_no, date, year, max_temperature, humidity, age_in_years,
               age_group, sex, simd) %>%
      summarise(
        all_behavioural = as.integer(any(condition_label == "behavioural") & !any(condition_label == "physiological")),
        all_physiological = as.integer(any(condition_label == "physiological") & !any(condition_label == "behavioural")),
        mixed_conditions = as.integer(any(condition_label == "behavioural") & any(condition_label == "physiological")),
        .groups = "drop"
        
      )
    
    
    # # New row to append to main tibble
    # new_row <- tibble(
    #   quarter_starting = as.Date(start_date_short), 
    #   alzheimers_count = condition_summary %>% filter(main_condition == "alzheimers") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   dehydration_count = condition_summary %>% filter(main_condition == "dehydration") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   cardiovascular_count = condition_summary %>% filter(main_condition == "cardiovascular") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   covid_count = condition_summary %>% filter(main_condition == "covid") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   dementia_count = condition_summary %>% filter(main_condition == "dementia") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   drowning_count = condition_summary %>% filter(main_condition == "drowning") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   falls_count = condition_summary %>% filter(main_condition == "falls") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   hot_weather_count = condition_summary %>% filter(main_condition == "hot_weather") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   injuries_count = condition_summary %>% filter(main_condition == "injuries") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   mental_health_count = condition_summary %>% filter(main_condition == "mental_health") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   natural_forces_count = condition_summary %>% filter(main_condition == "natural_forces") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   parkinson_count = condition_summary %>% filter(main_condition == "parkinson") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   renal_count = condition_summary %>% filter(main_condition == "renal") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   respiratory_count = condition_summary %>% filter(main_condition == "respiratory") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   road_incidents_count = condition_summary %>% filter(main_condition == "road_incidents") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   suicide_selfharm_count = condition_summary %>% filter(main_condition == "suicide_selfharm") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)},
    #   violence_count = condition_summary %>% filter(main_condition == "violence") %>% pull(n) %>% {ifelse(length(.) == 0, 0, .)}
    # )
    
    ### Save CSV file 
    write.csv(condition_summary, paste0("/conf/quality_indicators/Climate/data/base_data/condition_counts/hospital_admissions/",
                                        start_mmmYY,"-",end_mmmYY,"summarised_event_causes.csv"))
  }
  
}


## Run function on all quarters specified above
#undebug(summarise_conditions)
map2(q_start_dates,q_end_dates, ~
       summarise_conditions(.x,.y))


## End session (useful for running as a night session)
# Get the Process ID of the R session
ppid <- system(paste("ps -o ppid= -p", Sys.getpid()), intern = TRUE)
# Gracefully terminate the R session
system(paste("kill -15", ppid))