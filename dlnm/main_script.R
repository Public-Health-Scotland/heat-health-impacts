#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# main_script.R
# July 2025
# Al Morgan
# 
# This script acts as a main hub from which to run the DLNM model and save out
# the resulting data and plots. Both normal and sliding-window versions of the
# model can be run from here.
#
# To use, adjust the variables at the start of the script (event, cause etc) as
# needed, then run the entire script. If running models on new/refreshed data,
# make sure to delete the relevant DLNM result folder(s) first to allow the DLNM
# code to re-run.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source("setup/dlnm_setup.R")
source("dlnm/dlnm_model_function.R")
source("functions/summarise_events_function.R")

event <- "Deaths" 
cause <-  "All causes" # "All causes", "Heat-related causes"
geog <- "NHS Health board" # "NHS Health board", "Local authority"
region <-  "Scotland"
vuln_breakdown <- "Age" # "Age", "Sex", "Deprivation index"
# vuln_group <-  "65 yrs plus" #, "All", "Under 65 yrs", "65 yrs plus"
# vuln_group <- "Male" # "All" "Male" "Female"
vuln_group <- "All" # "All", "1", "2", "3", "4", "5", "1 & 2", "4 & 5"

# Set up results folder objects (combined later)
cause_folder <- to_snake_case(cause)

results_folder <- paste(
  "test",
  to_snake_case(event),
  to_snake_case(geog),
  "by", to_snake_case(vuln_breakdown),
  sep = "_"
)


if(vuln_breakdown == "Deprivation index"){
  results_folder <- paste0(results_folder, "/simd", to_snake_case(vuln_group))
} else{
  results_folder <- paste0(results_folder, "/", to_snake_case(vuln_group))
}

# Define full path
results_filepath <- file.path(
  "/conf/quality_indicators/Climate/data/dlnm_results/sliding_window_models",
  cause_folder, results_folder
)

# Check if folder exists and has files, if so: read in cached data
if (dir.exists(results_filepath) && length(list.files(results_filepath)) > 0) {
  
  # Check if folder exists and has files, if so: read in cached data
  temp_thresholds <- read_csv(paste0(results_filepath, "/temp_thresholds.csv"))
  hist_list <- readRDS(paste0(results_filepath, "/temp_histogram_data.rds"))
  rr_list <- readRDS(paste0(results_filepath, "/rr_data.rds"))
  att_nums_scotland <- read_csv(paste0(results_filepath, "/attrib_nums_scotland.csv"))
  att_rates_scotland <- read_csv(paste0(results_filepath, "/attrib_rates_scotland.csv"))
  att_nums_region <- read_csv(paste0(results_filepath, "/attrib_nums_regions.csv"))
  att_rates_region <- read_csv(paste0(results_filepath, "/attrib_rates_regions.csv"))
  event_count_summary <-  read_csv(paste0(results_filepath, "/../event_count_summary.csv"))
  
  
} else { # Otherwise, create folder and run DLNM code
  dir.create(results_filepath, recursive = TRUE)
  # debug(dlnm)
  dlnm_results <- dlnm(event, cause, geog, vuln_breakdown, vuln_group, results_filepath)
  
  # extract results from DLNM function
  temp_thresholds <- dlnm_results[[1]]
  hist_list <-  dlnm_results[2][[1]] 
  rr_list <- dlnm_results[3][[1]]
  att_nums_scotland <- dlnm_results[4][[1]] # attributable numbers across Scotland for each year
  att_rates_scotland <- dlnm_results[5][[1]] #  attributable rates across Scotland for each year
  att_nums_region <- dlnm_results[6][[1]] # attributable numbers for each region and each year
  att_rates_region <- dlnm_results[7][[1]]  #  attributable rates for each region and each year
  aic <- dlnm_results[8][[1]] # Akaike's An Information Criterion
  event_count_summary <- dlnm_results[9][[1]] # dataframe of total counts by e.g. age, sex, simd
  model_residuals <- dlnm_results[10][[1]] # model residuals which can be put into a boxplot
  
  # Save results as CSV files
  write_csv(temp_thresholds, paste0(results_filepath, "/temp_thresholds.csv"))
  saveRDS(hist_list, paste0(results_filepath, "/temp_histogram_data.rds"))
  saveRDS(rr_list, paste0(results_filepath, "/rr_data.rds"))
  write_csv(att_nums_scotland, paste0(results_filepath, "/attrib_nums_scotland.csv"))
  write_csv(att_rates_scotland, paste0(results_filepath, "/attrib_rates_scotland.csv"))
  write_csv(att_nums_region, paste0(results_filepath, "/attrib_nums_regions.csv"))
  write_csv(att_rates_region, paste0(results_filepath, "/attrib_rates_regions.csv"))
  write_csv(event_count_summary, paste0(results_filepath, "/../event_count_summary.csv"))
}


region_filter <- region # need to do this for data filtering in plot creation

# Plot histogram with RR curve over the top
hist_data <- hist_list[[region]] %>% rename(temp_c = x)
rr_data <- rr_list[[region]] %>% rename(temp_c = x)

## Maximum values for histogram plot
max_hist <- max(hist_data$counts)
max_count <- ceiling(max(hist_data$counts) / 50) * 50

heatwave_day <-  25

rr_plot <- ggplot() +
  ## Insert green panel for optimal weather range
  annotate("rect", fill = phs_colors("phs-green-50"), alpha = 0.5,
           xmin = temp_thresholds %>% filter(region == region_filter) %>% pull(opt_temp_range_low), xmax = temp_thresholds %>% filter(region == region_filter) %>% pull(opt_temp_range_high),
           ymin = 0, ymax = Inf) +
  ## Histogram Layer (Plotted First)
  geom_col(data = hist_data, aes(x = temp_c, y = density *3.6),
           # fill = "grey80", color = "grey60"
           fill = phs_colors("phs-purple-80"), color = "black"
  ) +
  ## Relative Risk Curve
  geom_ribbon(data = rr_data, aes(x = temp_c, ymin = lower, ymax = upper),
              fill = phs_colors("phs-rust-30")) +  # Confidence interval
  geom_line(data = rr_data, aes(x = temp_c, y = rr), color = "black", linewidth = 1) +
  geom_vline(xintercept = temp_thresholds %>% filter(region == region_filter) %>% pull(risk_increase_temp), linetype = "solid", color = phs_colors("phs-blue")) +
  geom_vline(xintercept = temp_thresholds %>% filter(region == region_filter) %>% pull(high_risk_temp), linetype = "solid", color = phs_colors("phs-rust")) +
  geom_vline(xintercept = heatwave_day, linetype = "dashed", color = phs_colors("phs-purple")) +
  ## Horizontal RR = 1 Reference Lines
  geom_hline(yintercept = 1, linetype = "solid", color = "black")+
  ## Formatting y-axis with secondary axis for counts
  scale_y_continuous(
    limits = c(0, 1.5),  # Match ylim from base R
    sec.axis = sec_axis(~ .* max_hist , name = "No. of Days", breaks = seq(0, max_count, by = 350))
  ) +
  ## Theme and labels
  theme_minimal() +
  labs(
    subtitle = paste0(region, " ", str_to_lower(event), ": All years"),
    x = "Maximum temperature (°C)",
    y = "Relative Risk"
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5),
    text = element_text(size = 14),
    axis.line = element_line(color = "black"),
    axis.title.y.right = element_text(color = "black")  # Make secondary y-axis readable
  )

rr_plot

ggsave(plot = rr_plot, filename = paste0(results_filepath, "/rr_plot.png"),
       width = 6, height = 4)

# Plot att rates plots for risk increase, high risk and heatwave temps

if (event == "Deaths") {
  y_label <- "Attributable deaths\n(per 100,000)"
} else if (event == "Hospital admissions") {
  y_label <- "Attributable hospitalisations\n(per 100,000)"
}

if (region == "Scotland"){
  
  ar_data <- att_rates_scotland
  an_data <- att_nums_scotland
}else{
  ar_data <- att_rates_region
  an_data <- att_nums_region
}

## Get maximum rate so all 3 plots have same axis limits
y_limit_upper <- ar_data %>% 
  summarise(max = ceiling(max(c(ar_risk_increase_uci, ar_high_risk_uci, ar_heatwave_day_uci)))) %>% 
  pull(max)
y_limit_lower <- ar_data %>% 
  summarise(min = floor(min(c(ar_risk_increase_lci, ar_high_risk_lci, ar_heatwave_day_lci)))) %>% 
  pull(min)

## Get maximum and minimum number so all 3 plots have same axis limits
y_limit_upper_an <- an_data %>% 
  summarise(max = ceiling(max(c(risk_increase_uci, high_risk_uci, heatwave_day_uci)))) %>% 
  pull(max)
y_limit_lower_an <- an_data %>% 
  summarise(min = floor(min(c(risk_increase_lci, high_risk_lci, heatwave_day_lci)))) %>% 
  pull(min)

## Plot attributable rate
# ~~~~~~~~~~~~~~~~~~~~~~
plot_AR_risk_inc <- ar_data %>%
  # filter(regnames == region) %>%
  ggplot(aes(x = year, y = ar_risk_increase)) +
  geom_ribbon(aes(ymin = ar_risk_increase_lci, ymax = ar_risk_increase_uci), fill = phs_colors("phs-blue-30")) +
  geom_line(color = phs_colors("phs-blue")) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(y_limit_lower, y_limit_upper)) +
  labs(title = region,
       subtitle = paste0("Temperatures above risk increase threshold (",
                         temp_thresholds %>% filter(region == region_filter) %>% pull(risk_increase_temp),
                         "°C)"),
       x = "\nYear",
       y = y_label) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(color = "black"))

ggsave(plot_AR_risk_inc, filename = paste0(results_filepath, "/ar_plot_risk_increase.png"),
       width = 8, height = 4)

plot_AR_high_risk <- ar_data %>%
  # filter(regnames == region) %>%
  ggplot(aes(x = year, y = ar_high_risk)) +
  geom_ribbon(aes(ymin = ar_high_risk_lci, ymax = ar_high_risk_uci), fill = phs_colors("phs-rust-30")) +
  geom_line(color = phs_colors("phs-rust")) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(y_limit_lower, y_limit_upper)) +
  labs(
    subtitle = paste0("Temperatures above high risk threshold (",
                      temp_thresholds %>% filter(region == region_filter) %>% pull(high_risk_temp),
                      "°C)"),
    x = "\nYear",
    y = y_label) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(color = "black"))

ggsave(plot_AR_high_risk, filename = paste0(results_filepath, "/ar_plot_high_risk.png"),
       width = 8, height = 4)


plot_AR_ri_hr <-  ar_data %>%
  # filter(regnames == region) %>%
  ggplot() +
  
  geom_ribbon(aes(x = year,y = ar_risk_increase, ymin = ar_risk_increase_lci, ymax = ar_risk_increase_uci),
              fill = phs_colors("phs-purple-30"), alpha = 0.8) +
  geom_ribbon(aes(x = year,y = ar_risk_increase, ymin = ar_high_risk_lci, ymax = ar_high_risk_uci), 
              fill = phs_colors("phs-rust-30"), alpha = 0.4) +
  geom_line(aes(x = year, y = ar_risk_increase), color = phs_colors("phs-purple")) +   
  geom_line(aes(x = year, y = ar_high_risk), color = phs_colors("phs-rust")) +
  geom_point(aes(x = year, y = ar_risk_increase),color = "black") +
  geom_point(aes(x = year, y = ar_high_risk),color = "black") +
  scale_y_continuous(limits = c(y_limit_lower, y_limit_upper)) +
  labs(
    subtitle = paste0("Temperatures above risk thresholds"),
    x = "\nYear",
    y = y_label) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(color = "black"))

plot_AR_ri_hr

plot_AR_heatwave <- ar_data %>%
  # filter(regnames == region) %>%
  ggplot(aes(x = year, y = ar_heatwave_day)) +
  geom_ribbon(aes(ymin = ar_heatwave_day_lci, ymax = ar_heatwave_day_uci), fill = phs_colors("phs-purple-30")) +
  geom_line(color = phs_colors("phs-purple")) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(y_limit_lower, y_limit_upper)) +
  labs(
    subtitle = "Temperatures above heatwave threshold (25°C)",
    x = "\nYear",
    y = y_label) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(color = "black"))

combined_ar <- grid.arrange(plot_AR_risk_inc, plot_AR_high_risk, plot_AR_heatwave, ncol = 1)

ggsave(combined_ar, filename = paste0(results_filepath, "/ar_plot.png"),
       width = 8, height = 6)


## Plot attributable number
# ~~~~~~~~~~~~~~~~~~~~~~
plot_AN_risk_inc <- an_data %>%
  # filter(regnames == region) %>%
  ggplot(aes(x = year, y = risk_increase)) +
  geom_ribbon(aes(ymin = risk_increase_lci, ymax = risk_increase_uci), fill = phs_colors("phs-blue-30")) +
  geom_line(color = phs_colors("phs-blue")) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(y_limit_lower_an, y_limit_upper_an)) +
  labs(title = region,
       subtitle = paste0("Temperatures above risk increase threshold (",
                         temp_thresholds %>% filter(region == region_filter) %>% pull(risk_increase_temp),
                         "°C)"),
       x = "\nYear",
       y = y_label) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(color = "black"))

ggsave(plot_AN_risk_inc, filename = paste0(results_filepath, "/an_plot_risk_increase.png"),
       width = 8, height = 4)

plot_AN_high_risk <- an_data %>%
  # filter(regnames == region) %>%
  ggplot(aes(x = year, y = high_risk)) +
  geom_ribbon(aes(ymin = high_risk_lci, ymax = high_risk_uci), fill = phs_colors("phs-rust-30")) +
  geom_line(color = phs_colors("phs-rust")) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(y_limit_lower_an, y_limit_upper_an)) +
  labs(
    subtitle = paste0("Temperatures above high risk threshold (",
                      temp_thresholds %>% filter(region == region_filter) %>% pull(high_risk_temp),
                      "°C)"),
    x = "\nYear",
    y = y_label) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(color = "black"))

ggsave(plot_AN_high_risk, filename = paste0(results_filepath, "/an_plot_high_risk.png"),
       width = 8, height = 4)

plot_AN_heatwave <- an_data %>%
  # filter(regnames == region) %>%
  ggplot(aes(x = year, y = heatwave_day)) +
  geom_ribbon(aes(ymin = heatwave_day_lci, ymax = heatwave_day_uci), fill = phs_colors("phs-purple-30")) +
  geom_line(color = phs_colors("phs-purple")) +
  geom_point(color = "black") +
  scale_y_continuous(limits = c(y_limit_lower_an, y_limit_upper_an)) +
  labs(
    subtitle = "Temperatures above heatwave threshold (25°C)",
    x = "\nYear",
    y = y_label) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 8),
        axis.line = element_line(color = "black"))

combined_an <- grid.arrange(plot_AN_risk_inc, plot_AN_high_risk, plot_AN_heatwave, ncol = 1)

ggsave(combined_an, filename = paste0(results_filepath, "/an_plot.png"),
       width = 8, height = 6)

## Residuals boxplot
boxplot(model_residuals)
