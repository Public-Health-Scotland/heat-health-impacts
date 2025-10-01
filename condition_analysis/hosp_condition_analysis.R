# this is the script that is run by the associated Markdown file in preparation
# for knitting to PDF

library(tidyverse)

input_data_path <- "/conf/quality_indicators/Climate/data/base_data/condition_counts/hospital_admissions/"

input_data_pattern <- "summarised_event_causes.csv$"

  
data_files <- list.files(path = input_data_path,
                         recursive = TRUE,
                         pattern = input_data_pattern,
                         full.names = TRUE)

# Read in data and format variables
df <- read_csv(data_files) %>%
  dplyr::select(-1) %>% 
  mutate(age_group = factor(age_group, levels = c("under 65 yrs", "65 yrs plus"))) %>% 
  mutate(temperature_group = cut(max_temperature, 
                          breaks = c(0, 10, 15, 20, 25, 40), 
                          labels = c("<10°C", "10–15°C", "15–20°C", "20–25°C", ">25°C")
  ), .after = max_temperature
  )
  

# Data grouped by year
conditions_by_year <- df %>% 
  group_by(year) %>% 
  summarise()

# test <- read_csv(paste0(input_data_path, "Jul2005-Sep2005summarised_event_causes.csv")) %>% 
#   select(-1) %>% 
#   filter(condition_name != "other")
# 
# test



