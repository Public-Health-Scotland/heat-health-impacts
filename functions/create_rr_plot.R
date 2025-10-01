create_rr_plot <- function(hist_list, rr_list, temp_thresholds, region_filter, window_size, window_year = NULL){

  #  Extract region data from results lists
  if(window_size != "All years"){ # Sliding window DLNM
    hist_data <- hist_list[[as.character(window_year)]][[region_filter]] %>%
      rename(temp_c = x)
    rr_data <- rr_list[[as.character(window_year)]][[region_filter]] %>%
      rename(temp_c = x)
    thresholds <- temp_thresholds[[as.character(window_year)]] %>% 
      filter(region == region_filter)
    # Set break for n days y axis
    n_days_break <- 50
  } else{ # Normal DLNM 
    hist_data <- hist_list[[region_filter]] %>% rename(temp_c = x)
    rr_data <- rr_list[[region_filter]] %>% rename(temp_c = x)
    thresholds <- temp_thresholds %>% filter(region == region_filter)
    # Set break for n days y axis
    n_days_break <- 350
  }
  
  # Maximum values for histogram plot
  max_hist <- max(hist_data$counts)
  max_count <- ceiling(max(hist_data$counts) / 50) * 50
  
  heatwave_day <-  25
  
  # create a small dataframe of these values for legend later
  vline_data <- tibble(
    x = c(thresholds$risk_increase_temp, thresholds$high_risk_temp, heatwave_day),
    label = c("Risk increase (RR>1)", "High risk (RR=1.1)", "Heatwave temp"),
    style = c("solid", "solid", "dashed")
  )
  
  rr_plot <- ggplot() +
    # Insert green panel for optimal weather range
    annotate("rect", fill = "#b0e8a9", alpha = 0.5,
             xmin = thresholds %>% pull(opt_temp_range_low), xmax = thresholds %>% pull(opt_temp_range_high),
             ymin = 0, ymax = Inf) +
    # Histogram Layer (Plotted First)
    geom_col(data = hist_data, aes(x = temp_c, y = density *3.6),
             # fill = "grey80", color = "grey60"
             fill = "indianred", color = "black"
    ) +
    # Relative Risk Curve
    geom_ribbon(data = rr_data, aes(x = temp_c, ymin = lower, ymax = upper),
                fill = "#f9e38c") +  # Confidence interval
    geom_line(data = rr_data, aes(x = temp_c, y = rr), color = "black", linewidth = 1) +
    geom_vline(data = vline_data, aes(xintercept = x, colour = label, linetype = style), show.legend = c(linetype = FALSE)) +
    # Horizontal RR = 1 Reference Lines
    geom_hline(yintercept = 1, linetype = "solid", color = "black")+
    # Formatting y-axis with secondary axis for counts
    scale_y_continuous(
      limits = c(0, 1.5),  # Match ylim from base R
      sec.axis = sec_axis(~ .* max_hist , name = "No. of Days", breaks = seq(0, max_count, by = n_days_break))
    ) +
    # Theme and labels
    theme_minimal() +
    labs(
      x = "Maximum temperature (°C)",
      y = "Relative Risk"
    ) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      text = element_text(size = 14),
      axis.line = element_line(color = "black"),
      axis.title.y.right = element_text(color = "black"),  # Make secondary y-axis readable
      legend.position = "bottom"
    ) +
    scale_colour_manual(name = "Thresholds:",
                        breaks = c("Risk increase (RR>1)",
                                   "High risk (RR=1.1)",
                                   "Heatwave temp"),
                        values = c("Risk increase (RR>1)" = "#E9B62D",
                                   "High risk (RR=1.1)" = "#E57E41",
                                   "Heatwave temp" = "#D33F6A"
                        )) +
    scale_linetype_manual(
      values = c("solid" = "solid",
                 "solid" = "solid",
                 "dashed" = "dashed")
    )
  
  return(rr_plot)
}

