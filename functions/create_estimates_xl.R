create_estimates_xl <- function(temps_rr_df, scot_weather, hot_days_long, antot,
                             artot, dat){
  # Create XLSX file with estimates
  wb <- createWorkbook() # Create a new workbook
  # Add each data frame to a new sheet
  addWorksheet(wb, "RR temps")
  writeData(wb, sheet = "RR temps", temps_rr_df)
  addWorksheet(wb, "Scotland temps")
  writeData(wb, sheet = "Scotland temps", scot_weather)
  addWorksheet(wb, "Scotland hot days")
  writeData(wb, sheet = "Scotland hot days", hot_days_long)
  addWorksheet(wb, "Scotland attributable numbers")
  # join attributable numbers dataframes with upper and lower CI dataframes
  antot_wb <- antot %>%
    full_join(antotlow %>%
                rename_with(~ paste0(., "_lower"), .cols = contains("ci"))) %>%
    full_join(antothigh %>%
                rename_with(~ paste0(., "_upper"), .cols = contains("ci")))
  writeData(wb, sheet = "Scotland attributable numbers", antot_wb)
  addWorksheet(wb, "Scotland attributable rates")
  # join attributable rates dataframes with upper and lower CI dataframes
  artot_wb <- artot %>%
    full_join(artotlow %>%
                rename_with(~ paste0(., "_lower"), .cols = contains("ci"))) %>%
    full_join(artothigh %>%
                rename_with(~ paste0(., "_upper"), .cols = contains("ci")))
  writeData(wb, sheet = "Scotland attributable rates", artot_wb)
  # regional data
  addWorksheet(wb, "Regional estimates")
  regional_wb <- dat %>%
    relocate(year) %>% # move year column to front
    select(-ci.x, -ci.y) %>% 
    rename_with(~ str_replace(., "lci", "ci_lower"), .cols = contains("lci")) %>% 
    rename_with(~ str_replace(., "uci", "ci_upper"), .cols = contains("uci"))
  writeData(wb, sheet = "Regional estimates", regional_wb)
  
  return(wb)
}