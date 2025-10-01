summarise_events <- function(df, event, vuln_breakdown) {
  # Standardise inputs
  event <- tolower(event)
  vuln_breakdown <- tolower(vuln_breakdown)
  
  # Define prefix and total column
  prefix <- ifelse(event == "deaths", "death_", "adm_")
  total_col <- ifelse(event == "deaths", "death", "admissions")
  
  # Define column suffixes based on breakdown
  col_map <- list(
    age = c("65yrs_over", "under_65yrs"),
    sex = c("males", "females"),
    `deprivation index` = paste0("simd", c(1:2, "1_2", 3:5, "4_5"))
  )
  
  suffixes <- col_map[[vuln_breakdown]]
  selected_cols <- paste0(prefix, suffixes)
  
  # Total events (sum of total_col)
  total_events <- sum(df[[total_col]], na.rm = TRUE)
  total_col_name <- paste0("total_", event)
  
  # Summarise breakdown columns
  events_summary <- df %>%
    summarise(across(all_of(selected_cols), ~sum(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "category", values_to = "count") %>%
    mutate(
      percentage = round(100 * count / total_events, 1),
      !!total_col_name := total_events
    )
  
  return(events_summary)
}
