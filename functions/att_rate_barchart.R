#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Attribution over 18 degrees
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Libraries
library(tidyverse)
library(readxl)
library(fs)

# Create a new folder with all of the attributable rate files in one place:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create function for extracting files for deaths and hospitalisations:

find_files <- function(event){

  # Set your main directory containing the folders
  if(event == "deaths"){
  main_dir <- "/conf/quality_indicators/Climate/data/dlnm_results/sliding_window_models/all_causes"
  }else if(event == "hospital"){
  main_dir <- "/conf/quality_indicators/Climate/data/dlnm_results/sliding_window_models/heat_related_causes"
  }
  
  # Set the destination directory
  dest_dir <- "/conf/quality_indicators/Climate/data/dlnm_results/sliding_window_models/collated_att_rate"
  
  # Set the target filename to look for
  target_filename <- "attrib_rates_scotland.csv"
  
  folder_prefix <- paste0(event,"_") 
  
  if (!dir_exists(dest_dir)) {
    dir_create(dest_dir)
  }
  
  # Recursively list all folders
  all_folders <- dir_ls(main_dir, type = "directory", recurse = TRUE)
  
  
  # Filter folders by prefix
  filtered_folders <- all_folders[grepl(paste0("^.*/", folder_prefix), all_folders)]
  
  
  for (folder in filtered_folders) {
    file_path <- path(folder, target_filename)
    
    if (file_exists(file_path)) {
      # Create a unique name using the full folder path
      folder_name_clean <- path_file(folder)
      new_filename <- paste0(event,"_", folder_name_clean,"_",target_filename)
      new_file_path <- path(dest_dir, new_filename)
      
      file_copy(file_path, new_file_path, overwrite = TRUE)
      cat("Copied:", file_path, "→", new_file_path, "\n")
    }
  }
}

find_files("deaths")
find_files("hospital")


# Extract data from a folder containing all attributable rate files:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the parent directory containing subfolders
folder_path <- "/conf/quality_indicators/Climate/data/dlnm_results/sliding_window_models/collated_att_rate"

files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)


# Define the columns you want to extract
columns_to_extract <- c("ar_scot_risk_inc", "scot_risk_inc_lci", "scot_risk_inc_uci",
                        "ar_heatwave_day","ar_heatwave_day_lci", "ar_heatwave_day_uci")


# Define the shared string in filenames (e.g., "commonstring_")
shared_string <- "attrib_rates_scotland"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract year from the first matching file
first_file <- files %>%
  keep(~ basename(.x) %>% str_starts("hospital_")) %>%
  first()

year_value <- read_csv(first_file, show_col_types = FALSE) %>%
  pull(year) %>%
  unique()

# Now use year_value inside the function
process_files <- function(file_list, prefix, shared_string) {
  file_list %>%
    keep(~ basename(.x) %>% str_starts(prefix)) %>%
    map(function(file) {
      suffix <- basename(file) %>%
        str_remove(paste0("^", prefix)) %>%
        str_remove(shared_string) %>%
        str_remove("\\.csv$")
      
      df <- read_csv(file, show_col_types = FALSE) %>%
        select(any_of(columns_to_extract)) %>%
        #filter(if_any(all_of(columns_to_extract), ~ !is.na(.x) & .x != "")) %>%
        mutate(row_id = row_number())
      
      colnames(df)[colnames(df) != "row_id"] <- paste0(
        prefix, suffix,
        str_remove(colnames(df)[colnames(df) != "row_id"], "^ar_")
      )
      
      return(df)
    }
    
    ) %>%
    reduce(full_join, by = "row_id") %>%
    mutate(year = year_value) %>%
    select(-row_id)
}


# Process each subgroup
df_d <- process_files(files, "deaths_", shared_string)%>%
  relocate(year)
df_h <- process_files(files, "hospital_", shared_string)%>%
  relocate(year)

# Function to reshape a wide dataframe to long format
reshape_to_long <- function(df, prefix) {
  df %>%
    pivot_longer(
      cols = -year,
      names_to = "breakdown",
      values_to = "value"
    ) %>%
    # Extract prefix, demographic, component_type, and CI suffix
    tidyr::extract(col = breakdown,
            into = c("prefix", "demographic", "component_type", "ci"),
            paste0("^(", prefix, ")(.+)_(scot_risk_inc|heatwave_day)(?:_((?:lci|uci)))?$"),
            remove = TRUE
    ) %>%
    mutate(
      ci = case_when(
        ci == "lci" ~ "lower_ci",
        ci == "uci" ~ "upper_ci",
        TRUE ~ "rate"
      )
    ) %>%
    pivot_wider(
      names_from = ci,
      values_from = value
    ) 
}

# Apply to both dataframes
df_d_long <- reshape_to_long(df_d, "deaths_")
df_h_long <- reshape_to_long(df_h, "hospital_")

df_long <- bind_rows(df_d_long, df_h_long)

av_rate <- df_long %>%
  mutate(breakdown = case_when(demographic == "under_65_yrs" ~ "Under 65",
                               demographic == "65_yrs_plus" ~ "65 plus",
                               demographic == "male" ~ "Males",
                               demographic == "female" ~ "Females",
                               demographic == "simd1_2" ~ "Most deprived",
                               demographic == "simd3" ~ "Central",
                               demographic == "simd4_5" ~ "Least deprived"))
av_rate <- av_rate %>%
  mutate(breakdown = factor(breakdown,levels = c("Under 65","65 plus","Females", "Males",
                                                 "Most deprived","Central","Least deprived")),
          type = case_when(prefix == "hospital_" ~ "Hospitalisations",
                           prefix == "deaths_" ~ "Deaths"))

av_rate <- av_rate %>%
  group_by(type,breakdown, component_type)%>%
  summarise(mean_rate = mean(rate),
            mean_lci = mean(lower_ci),
            mean_uci = mean(upper_ci))%>%
  ungroup()


(plot_av_rate_scot_risk_inc <- ggplot(av_rate%>%filter(component_type == "scot_risk_inc"),
                                      aes(x = breakdown, y = mean_rate, fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(
      aes(ymin = mean_lci, ymax = mean_uci),
      position = position_dodge(width = 0.9),
      width = 0.2
    ) +
  facet_wrap(~ type, scales = "free_y") +
  scale_fill_manual(values = c("Hospitalisations" = phs_colors("phs-blue"), "Deaths" = phs_colors("phs-magenta"))) +
  theme_minimal()+
   theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Average attributable rates over 18°C by demographic group",
       x = "Demographic",
       y = "Average attributable rate per 100,000",
       fill = "Type")
    )

(plot_av_rate_heatwave_day <- ggplot(av_rate%>%filter(component_type == "heatwave_day"),
                                      aes(x = breakdown, y = mean_rate, fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_errorbar(
      aes(ymin = mean_lci, ymax = mean_uci),
      position = position_dodge(width = 0.9),
      width = 0.2
    ) +
    facet_wrap(~ type, scales = "free_y") +
    scale_fill_manual(values = c("Hospitalisations" = phs_colors("phs-blue"), "Deaths" = phs_colors("phs-magenta"))) +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))+
    labs(title = "Average attributable rates over 25°C by demographic group",
         x = "Demographic",
         y = "Average attributable rate per 100,000",
         fill = "Type")
)

