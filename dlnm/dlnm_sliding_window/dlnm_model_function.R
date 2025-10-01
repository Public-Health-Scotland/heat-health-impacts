dlnm <- function(event, cause, geog, vuln_breakdown, vuln_group,
                 results_filepath
                 # .progress = NULL
){
  
  ############# DLNM SETUP ############
  print(paste("DLNM function has taken in", event, cause, geog, vuln_breakdown, vuln_group,
              results_filepath))
  ## Set up indicator specific variables ----
  # Where this script has so far been used for heat related mortality and morbidity
  # it is split into two conditional sections for if event == death, and for if
  # event == hospital admissions. The arguments that may differ for specific indicators,
  # from the standard base arguments are listed here.
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Geography names for output folder are selected automatically:
  if(geog == "NHS Health board"){folder_geog <- "nhsboard"}
  else if(geog == "Local authority"){folder_geog <- "councilarea"}
  
  # Heat related mortality: arguments
  # ~~~~~~~~~~~~~~~~~~~~~~~
  if (event == "Deaths"){
    
    # DATA PATHS:
    # ~~~~
    
    # INPUT:
    # Input folder paths:
    if(cause == "All causes"){
      input_data_path <- "/conf/quality_indicators/Climate/data/base_data/all_deaths_data_ephss20_near_ALLcovid/"
    }else if(cause == "Heat-related causes"){
      input_data_path <- "/conf/quality_indicators/Climate/data/base_data/heat_deaths_data_ephss20_near_ALLcovid/"
    }
    input_data_pattern <- paste0("all_deaths_data_", folder_geog, "_vuln_split.csv$")
    
    # DEFINE VARIABLES AND PARAMETERS:
    # ~~~~~
    # Define dependent column inline with dataset, dependent on vulnerability breakdown:
    if(vuln_breakdown == "Age"){
      if(vuln_group == "All"){dependent_col <-  "death"}
      else if(vuln_group == "Under 65 yrs"){dependent_col <-  "death_under_65yrs"}
      else if(vuln_group == "65 yrs plus"){dependent_col <-  "death_65yrs_over"}
    }else if(vuln_breakdown == "Sex"){
      if(vuln_group == "All"){dependent_col <-  "death"}
      else if(vuln_group == "Male"){dependent_col <-  "death_males"}
      else if(vuln_group == "Female"){dependent_col <-  "death_females"}
    }else if(vuln_breakdown == "Deprivation index"){
      if(vuln_group == "All"){dependent_col <-  "death"}
      else if(vuln_group == "1"){dependent_col <-  "death_simd1"}
      else if(vuln_group == "2"){dependent_col <-  "death_simd2"}
      else if(vuln_group == "3"){dependent_col <-  "death_simd3"}
      else if(vuln_group == "4"){dependent_col <-  "death_simd4"}
      else if(vuln_group == "5"){dependent_col <-  "death_simd5"}
      else if(vuln_group == "1 & 2"){dependent_col <-  "death_simd1_2"}
      else if(vuln_group == "4 & 5"){dependent_col <-  "death_simd4_5"}
      
    }
    
    # VARFUN: Specification of the exposure function 
    # (natural cubic spline (ns) or b-spline (bs))
    # Natural cubic splines are a type of B-spline with additional constraints at the boundaries,
    # ensuring that the spline is linear beyond the boundary knots 
    # NOTE: Can use "bs" if looking at a year, "ns" better for shorter time period
    varfun = "ns" 
    
    # VARDEGREE sets the degrees of freedom in the exposure function when using ns
    vardegree =  length(varper) + 1
    
    
    # LAG FUNCTIONS
    # Specification of the lag function
    # The model uses either lagnk or lagdf - NOT BOTH
    # lagnk for bs and lagdf for ns
    
    # LAG - lag length in days 
    lag = 2 
    
    # LAGNK - Number of knots in lag function 
    lagnk = 1 
    
    # LAGDF - degrees of freedom for lag function
    lagdf = 1 
    
    
    # Set the Degrees of freedom for seasonality based on timeframe choice
    if(summer == TRUE){
      dfseas = 2
    }else{
      dfseas = 8
    }
    
    
    # 4b) Heat related hospitalisations: arguments
    # ~~~~~~~~~~~~~~~~~~~~~~~
    
  } else if (event == "Hospital admissions"){
    
    # CHECK DATA PATHS are current and correct!!
    
    # DATA PATHS:
    # ~~~~
    
    # INPUT:
    # Input folder paths:
    if(cause == "All causes"){
      input_data_path <- "/conf/quality_indicators/Climate/data/base_data/all_hosps_data_ephss20_near_ALLcovid/"
    }else if(cause == "Heat-related causes"){  
      input_data_path <- "/conf/quality_indicators/Climate/data/base_data/heat_hosps_data_ephss20_near_ALLcovid/"
    }
    # Suffix of input data files
    input_data_pattern <- paste0("heat_hum_hosps_data_", folder_geog, "_vuln_split.csv$")
    
    
    # Define dependent variable (based on age group input)
    if(vuln_breakdown == "Age"){
      if(vuln_group == "All"){dependent_col <-  "admissions"}
      else if(vuln_group == "Under 65 yrs"){dependent_col <-  "adm_under_65yrs"}
      else if(vuln_group == "65 yrs plus"){dependent_col <-  "adm_65yrs_over"}
    }else if(vuln_breakdown == "Sex"){
      if(vuln_group == "All"){dependent_col <-  "admissions"}
      else if(vuln_group == "Male"){dependent_col <-  "adm_males"}
      else if(vuln_group == "Female"){dependent_col <-  "adm_females"}
    }else if(vuln_breakdown == "Deprivation index"){
      if(vuln_group == "All"){dependent_col <-  "admissions"}
      else if(vuln_group == "1"){dependent_col <-  "adm_simd1"}
      else if(vuln_group == "2"){dependent_col <-  "adm_simd2"}
      else if(vuln_group == "3"){dependent_col <-  "adm_simd3"}
      else if(vuln_group == "4"){dependent_col <-  "adm_simd4"}
      else if(vuln_group == "5"){dependent_col <-  "adm_simd5"}
      else if(vuln_group == "1 & 2"){dependent_col <-  "adm_simd1_2"}
      else if(vuln_group == "4 & 5"){dependent_col <-  "adm_simd4_5"}
      
    }
    
    
    # VARFUN: Specification of the exposure function 
    # (natural cubic spline (ns) or b-spline (bs))
    # NOTE: Can use "bs" if looking at a year, "ns" better for shorter time period
    
    varfun = "ns" 
    
    # VARDEGREE sets the degrees of freedom in the exposure function
    vardegree =  length(varper) + 1
    
    # LAG FUNCTIONS
    # Specification of the lag function
    # The model uses either lagnk or lagdf - NOT BOTH
    # Uses lagnk when using bs, or lagdf if using ns.
    
    # LAG - lag length in days 
    lag = 2
    
    # LAGNK - Number of knots in lag function - use this if using b-spline
    lagnk = 1 
    
    # LAGDF - degrees of freedom for lag function - use this if using natural cubic spline
    lagdf = 1 
    
    # With different summer month_choice, for hospitalisation comes a different
    # degrees of freedom for seasonality
    if(summer == TRUE){
      dfseas = 2
    }else{
      dfseas = 8
    }
  }
  
  # if (!is.null(.progress)) .progress$incProgress(0.2)
  
  ## Define the non-indicator specific parameters ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Define columns
  # ~~~~~~~~~~
  time_col = "date"
  region_col = "regnames" 
  weather_col = "tmean"  # whilst the model reads in 'tmean' we're actually using max temp here
  population_col = "pop"
  humidity_col = "humidity"
  
  # Extra independent variables
  # ~~~~~~~~~~
  # Cross-basis and seasonal spline included by 
  # default. Use ('NONE' if none).
  # If controlling for COVID data, set one of the independent_cols arguments
  # to 'covid', otherwise set to 'NONE'
  independent_col1 = 'dow'  # day of week
  independent_col2 = 'NONE' # scottish deprivation index
  independent_col3 = 'covid'
  #independent_col3 = 'NONE'
  independent_col4 = 'NONE'
  
  # Comment out these lines if using covid flag (line 42/43 ish) instead of covid variable
  if (event == "death" & independent_col3 == "covid"){
    independent_col3 = 'covid_death' # deaths due to covid (codes used: U07.1 and U07.2)
  } else if (event == "hospital admissions" & independent_col3 == "covid"){
    independent_col3 = 'covid_adm' # hospital admissions due to covid (codes used: U07.1 and U07.2)
  } else {
    independent_col3 = 'NONE'
  }
  
  # Centring predictions 
  # ~~~~~~~~~~~~~~~~~~~~
  # Define the percentile of temperatures used for centring predictions at regional level
  percentile = 0.5
  
  
  ### Meta-predictors are NOT CURRENTLY IN USE ###
  
  # Define meta-predictors:
  # ~~~~~~~~~~~~~~~~~~~~~~~
  # Define meta-predictors to assess which variables(if any) should be included 
  # in the regional meta model
  #   OPTIONS: 1) avgweather (average temperature)
  #            2) maxweather (maximum temperature)
  #            3) rangeweather (temperature range)
  
  #Define empty predictors first which will be populated in dlnm_code
  avgweather <- NA
  rangeweather <- NA
  maxweather <- NA
  metapreds <- character(0)  # if no meta predictors
  
  
  ####### DLNM CODE #######
  
  
  
  # Load, read and manipulate data  ------------------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Select data files from input_data_path, detecting input_data_pattern
  
  data_files <- list.files(path = input_data_path,
                           recursive = TRUE,
                           pattern = input_data_pattern,
                           full.names = TRUE)
  
  
  # Read in data and rename variables
  df <- read_csv(data_files) %>%
    dplyr::select(-1) %>%
    dplyr::rename(
      # dependent = dependent_col,
      date = all_of(time_col),
      regnames = all_of(region_col),
      weather = all_of(weather_col),
      humidity = all_of(humidity_col),
      pop_col = all_of(population_col)) %>%
    dplyr::mutate(date = as.Date(date,format = "%d/%m/%y"),
                  year = as.numeric(year),
                  month = month(date)) %>%
    dplyr::filter(year > 2004 & year < 2025) 
  
  # Add columns combining SIMD groups
  if(event == "Deaths"){
    df <- df %>% 
      mutate(death_simd1_2 = death_simd1 + death_simd2, .after = death_simd2) %>% 
      mutate(death_simd4_5 = death_simd4 + death_simd5, .after = death_simd5)
  }else if(event == "Hospital admissions"){
    df <- df %>% 
      mutate(adm_simd1_2 = adm_simd1 + adm_simd2, .after = adm_simd2) %>% 
      mutate(adm_simd4_5 = adm_simd4 + adm_simd5, .after = adm_simd5)
  }
  
  # Use function to return summarised data (i.e. counts and proportions)
  summarised_events <- summarise_events(df, event, vuln_breakdown)
  
  # Rename the chosen dependent column (e.g. "adm_males") as "dependent"
  df <- df %>% 
    rename(dependent = dependent_col)
  
  
  # In some cases, there is no data in a region breakdown e.g. SIMD 1 for island health boards.
  # This code creates a list so that they can be filtered out of the data and no model buidling
  # is attempted using this data
  no_event_regions <- df %>% 
    group_by(regnames) %>% 
    summarise(total = sum(dependent, na.rm = TRUE)) %>% 
    filter(total == 0) %>% 
    pull(regnames)
  # Filter out any 'empty' regions
  df <- df %>% 
    filter(!(regnames %in% no_event_regions))
  
  
  # Save out median and mean weather conditions for reference
  
  scotmed <- median(df$weather)
  scotmean<- mean(df$weather)
  
  # If summer selected in dlnm_setup, then filter data to summer months
  if(summer == TRUE){
    df <- dplyr::filter(df, month %in% month_choice)
  }
  
  # Account for NAs
  df <- df %>% 
    mutate(dependent = ifelse(is.na(dependent), 0, dependent))
  
  # Sort regions alphabetically
  regions <- sort(as.character(unique(df$regnames)))
  
  # Break full datasets into a list of data by region
  df_list <- lapply(regions,
                    function(x)
                      df %>%
                      dplyr::filter(regnames == x))
  
  # Names each element in the list with the corresponding region name 
  # for use in loop in model function (section 4)
  names(df_list) <- regions
  
  # Save out dataset for troubleshooting (this can be removed once script is finalised)
  dataset = df
  
  # Tidy up
  rm(data_files)
  
  gc()
  
  # 3 - Define model function  ----
  # Used to pull in all independent columns, crossbases and parameters into the 
  # correct formula and model. 
  # Produces model/cb (cross-basis matrix) object
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  define_model <- function(dataset,
                           independent_col1,
                           independent_col2,
                           independent_col3,
                           independent_col4,
                           varfun,
                           varper,
                           vardegree,
                           lag,
                           #lagnk, # Commented out when using degrees of freedom
                           dfseas) {
    
    #Create numeric date
    dataset <- dataset %>% mutate(date_numeric = as.numeric(date))
    
    # Call list of independent columns from dlnm_setup, and name other independent
    # columns (i.e. cross-bases) which are created in the function
    independent_cols <- c(independent_col1, independent_col2, independent_col3, 
                          independent_col4, 
                          'cb_temp', 'cb_hum', # cross bases for temp and humidity
                          'ns(date, df = dfseas * length(unique(year)))') # to account for seasonality
    
    # Remove any independent columns with 'NONE'
    independent_cols <- independent_cols[independent_cols != "NONE"]
    
    # Model formula
    formula <- as.formula(paste(paste('dependent'), 
                                " ~ ",
                                paste(independent_cols, 
                                      collapse = " + ")))
    
    # Variables for the cross-bases: Use EITHER knots OR df
    
    # defines the functional form of the exposure-response relationship
    argvar <- list(fun = varfun,    
                   df = vardegree)  
    # knots = quantile(dataset$weather, varper / 100, na.rm = TRUE))
    
    # defines the functional form of the lag-response relationship
    arglag <- list(fun = varfun, 
                   df = lagdf)#, 
    #knots = logknots(lag, lagnk))
    
    # Ensure these are numeric
    lag <- as.numeric(lag)
    # lagnk <- as.numeric(lagnk)
    lagdf <- as.numeric(lagdf)
    dfseas <- as.numeric(dfseas)
    
    # Define the temp and humidity cross-bases (accounts for the lag)
    cb_temp <- crossbasis(dataset$weather,
                          lag = lag,
                          argvar = argvar,
                          arglag = arglag)
    
    cb_hum <- crossbasis(dataset$humidity,
                         lag = lag,
                         argvar = argvar,
                         arglag = arglag)
    
    # Call the model
    model <- glm(formula,
                 dataset,
                 family = quasipoisson,
                 na.action = "na.exclude")
    
    # Return, as a list, the model, and cross-bases terms
    return (list(model, cb_temp, cb_hum))
  }
  
  # if (!is.null(.progress)) .progress$incProgress(0.4)
  
  # 4 - Run_model function - extracted --------------------------------------
  
  # Define and run Quasi-poisson regression model for each regional dataframe
  # Adapted from: run_model
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Create two logical objects (minperregions and minweatherregions) each repeating NA for 
  # the length of the HB list (df_list)
  minperregions <- minweatherregions <- rep(NA,
                                            length(df_list))
  
  # Coefficients and vcov for overall cumulative summary:
  
  # Create a matrix for coef_ that is the length of the number of regions, 
  # and the width is equal to degrees of freedom in exposure function (vardegree)
  coef_ <- matrix(NA,
                  length(names(df_list)),
                  vardegree,
                  dimnames = list(names(df_list)))
  
  # Vector the length of the number of regions
  vcov_ <- vector("list", length(names(df_list)))
  
  # Name the levels of the vector
  names(vcov_) <- names(df_list)
  
  # For each region, extract the data, run the model, centre the predictions 
  # and populate the coef_ matrix and vcov vector created above
  
  for(i in seq(length(df_list))) {
    
    # cat(i,"")
    
    # Extract data
    data <- df_list[[i]]
    
    c(model, cb_temp, cb_hum) %<-% define_model(dataset = data,
                                                independent_col1 = independent_col1,
                                                independent_col2 = independent_col2,
                                                independent_col3 = independent_col3,
                                                independent_col4 = independent_col4,
                                                varfun = varfun,
                                                varper = varper,
                                                vardegree = vardegree,
                                                lag = lag,
                                                # lagnk = lagnk,
                                                dfseas = dfseas)
    summary(model)
    
    # Centre preds:
    cen <- quantile(data$weather, na.rm = TRUE, percentile)
    
    # crossreduce function summarizes the results of a DLNM incl. RR (relative risks) 
    pred <- crossreduce(cb_temp, model, cen = cen)
    
    # Centre preds:
    cen <- quantile(data$weather, na.rm = TRUE, percentile)
    
    # crossreduce function summarizes the results of a DLNM incl. RR (relative risks) 
    pred <- crossreduce(cb_temp, model, cen = cen)
    
    # find the temperature with the minimum RR for each region
    distances <- abs(pred$RRfit - 1.00)
    
    # Find the minimum distance
    min_distance <- min(distances)
    
    # Get all values with that minimum distance
    closest_values <- pred$RRfit[distances == min_distance]
    
    # Return the minimum among those
    closest_value <- names(which.min(closest_values))
    
    minweatherregions[i] <- closest_value
    
    #Save out coef and vcov 
    coef_[i,] <- coef(pred)   # coefficients from the model
    vcov_[[i]] <- vcov(pred)  # variance-covariance matrix associated with the model coefficients
  }
  
  # 5 - Run_meta_model - Extracted (only used once) ---------------------------------------
  
  # Run meta model next to compress the initial model into one coef and vcov for
  # each region. 
  # First need to assign coef_ output to coef and vcov_ output
  # to vcov otherwise wrong object read in code
  
  coef<-coef_  
  vcov<-vcov_  
  
  if(!is.list(df_list) | !is.data.frame(df_list[[1]])) {
    stop("Argument 'df_list' must be a list of data frames")
  }
  
  if(!is.matrix(coef) | !is.numeric(coef)) {
    stop("Argument 'coef' must be a numeric matrix")
  }
  
  if(!is.list(vcov) | !is.matrix(vcov[[1]])) {
    stop("Argument 'vcov' must be a list of matrices")
  }
  
  # Create meta-predictors
  # ~~~~~~~~~~~~~~~~~~~~~~
  # average weather:
  avgweather <- sapply(df_list,
                       function(x)
                         mean(x$weather, na.rm = TRUE))
  
  # weather range:
  rangeweather <- sapply(df_list,
                         function(x)
                           diff(range(x$weather, na.rm = TRUE)))
  
  # Maximum weather
  maxweather <- sapply(df_list,
                       function(x)
                         max(x$weather, na.rm = TRUE))
  
  # Minimum weather
  minweather <- sapply(df_list,
                       function(x)
                         min(x$weather, na.rm = TRUE))
  
  # Create meta formula so that the metapredictors selected in dlnm_setup are called
  if (length(metapreds) == 0) {
    metaformula_string <- "coef ~ 1"
  } else {
    metaformula_string <- paste("coef ~", paste(metapreds, collapse = "+"))
  }
  metaformula <- formula(metaformula_string)
  
  print(metaformula)
  
  # Meta-analysis
  # NB: country effects is not included in this example
  
  mv <- mixmeta(metaformula,
                # Insert other meta-predictors here for testing
                vcov,
                data = as.data.frame(unique(names(df_list))), # was data = regions_df
                control = list(showiter = TRUE))
  
  aic <- summary(mv)[["AIC"]]
  
  
  # SUMMARY FROM META ANALYSIS
  mvsummary <- summary(mv)
  
  # Obtain best linear unbiased prediction 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  blup <- blup(mv,vcov=T)
  
  
  # 7 - Calculate Scotland level predictions --------------------------------
  
  # Exclude extreme weathers
  #predvar <- quantile(data$weather, 1:99/100, na.rm=T)
  
  # All weathers
  predvar_scot <- data$weather
  
  argvar_scot <- list(x = predvar_scot,
                      fun = varfun,
                      df = vardegree)
  #knots = quantile(data$weather,
  #                varper / 100, na.rm = TRUE),
  #Bound = range(data$weather, na.rm = TRUE))
  
  bvar_scot <- do.call(onebasis, argvar_scot)
  
  #model <- NULL
  cen_scot <- median(as.numeric(minweatherregions)) # 
  # TRY BOTH
  #censcot <- median(minweatherregions_)
  
  pred_scot <- crosspred(bvar_scot,
                         coef = coef(mv),
                         vcov = vcov(mv),
                         model.link = "log",
                         by = 0.1,
                         cen = cen_scot)
  
  # calculate 25 degree prediction
  pred_scot25 <- crosspred(bvar_scot,
                           coef = coef(mv),
                           vcov = vcov(mv),
                           model.link = "log",
                           at=25,
                           cen = cen_scot)
  
  # 8 - Create Scotland RR plot ---------------------------------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Create new informative lines for plot:
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # line for the middle point/temp where the RR is 1
  lowestrisk <- as.numeric(names(
    which(pred_scot$allRRfit == 1)))
  
  # line for the lowest point/temp where the RR is 1
  lowestrisk_min <- as.numeric(names(
    which.min(pred_scot$allRRfit < 1.00001 & pred_scot$allRRfit > 0.99999)))
  
  # line for the maximum point/temp where the RR is 1
  lowestrisk_max <- as.numeric(names(
    which.max(pred_scot$allRRfit < 1.00001 & pred_scot$allRRfit > 0.99999)))
  
  # Line for the 97.5th percentile (the threshold for AN in the high heat chart)
  high_heat_line <- quantile(data$weather, 97.5/100, na.rm = TRUE)
  
  # line for heatwave day (i.e. 25 degrees celsius)
  heatwaveday <- 25
  
  # The point at which the RR estimate crosses 1.1 - i.e high risk
  high_risk <- as.numeric(names(
    which.max(which(pred_scot$allRRfit >= 1 & pred_scot$allRRfit <= 1.1) )))
  
  # The point at which the lower confidence interval for RR is above 1.005: i.e. the
  # point at which we can say the risk of mortality starts to increase
  risk_increase <- as.numeric(names(
    which.max(which(pred_scot$allRRlow < 1.006))))
  
  owr_low <- as.numeric(names(which.min(which(pred_scot$allRRlow <= 1.006))))
  owr_high <- as.numeric(names(which.max(which(pred_scot$allRRlow <= 1.006))))
  
  # Create segments for the chart so they can be colour coded according to the 
  # above points.
  
  segment_b <- pred_scot$predvar >= owr_high & pred_scot$predvar <= risk_increase
  segment_c <- pred_scot$predvar >= risk_increase & pred_scot$predvar <= high_risk
  segment_d <- pred_scot$predvar >= high_risk & pred_scot$predvar <= heatwaveday
  segment_e <- pred_scot$predvar >= heatwaveday
  
  
  # Create plot 
  # ~~~~~~~~~~~~~
  
  # Create a data frame for plotting
  plot_data_scot <- data.frame(
    x = pred_scot$predvar,          # Predictor variable (e.g., temperature, exposure)
    rr = pred_scot$allRRfit,        # Estimated relative risk
    lower = pred_scot$allRRlow,     # Lower confidence interval
    upper = pred_scot$allRRhigh     # Upper confidence interval
  )
  
  data25 <- data_frame(
    x = pred_scot25$predvar,
    rr = pred_scot25$allRRfit,
    lower = pred_scot25$allRRlow,     # Lower confidence interval
    upper = pred_scot25$allRRhigh 
  )
  
  #plot_data <- bind_rows(plot_data,data25)
  
  # Initialise list to store RR plot data for all regions
  rr_list <- list()
  rr_list[["Scotland"]] <- plot_data_scot
  
  breaks_scot <- c(min(data$weather, na.rm = TRUE) - 1,
                   seq(pred_scot$predvar[1],
                       pred_scot$predvar[length(pred_scot$predvar)],
                       length = 30),
                   max(data$weather, na.rm = TRUE) + 1)
  
  hist_scot <- hist(data$weather, breaks = breaks_scot, plot = FALSE)
  
  hist_data_scot <- data.frame(
    region = "Scotland",
    x = hist_scot$mids,     # Midpoints of histogram bins
    density = hist_scot$density,  # Scaled density values
    counts = hist_scot$counts
  )
  
  # Initialise list to store histogram data for all regions
  hist_list <- list()
  hist_list[["Scotland"]] <- hist_data_scot
  
  # Create dataframe of RR temps for indicators spreadsheet later
  temps_rr_df <- tibble(
    region = "Scotland",
    opt_temp_range_low = owr_low,
    opt_temp_range_high = owr_high,
    risk_increase_temp = risk_increase,
    high_risk_temp = high_risk
  )
  
  # if (!is.null(.progress)) .progress$incProgress(0.5)
  
  # 9 - Calculate Optimal weather range -------------------------------------
  
  # Generate matrix for storing results
  minpercregions_ <- minweatherregions_ <- rep(NA,
                                               length(df_list))
  names(minweatherregions_) <- names(minpercregions_) <- names(df_list)
  
  optimal_weather_range <- matrix(NA,
                                  length(df_list),
                                  2,
                                  dimnames = list(names(df_list),
                                                  c("lower","upper")))
  
  ranges <- t(sapply(df_list, function(x)
    range(x$weather,na.rm=T)))
  
  
  # Calculate optimal weather temperatures and minimum mortality 
  # looping across all regions
  
  for(i in seq(length(df_list))) {
    
    data <- df_list[[i]]
    predvar <- quantile(data$weather, 1:99 / 100, na.rm = TRUE)
    
    # Redefine the function using all arguments (boundary knots included)
    argvar_ <- list(x = predvar, fun = varfun,
                    df = vardegree)
    #knots = quantile(data$weather,
    #                varper / 100,
    #               na.rm = TRUE),
    #Bound = range(data$weather, na.rm = TRUE))
    
    bvar_ <- do.call(onebasis, argvar_)
    
    # Extract the minimum value from the product matrix of bvar_ *blup
    minpercregions_[i] <- (1:99)[which.min(bvar_ %*%
                                             blup[[i]]$blup)]
    minweatherregions_[i] <- quantile(data$weather,
                                      minpercregions_[i] / 100,
                                      na.rm = TRUE)
    
    # Use crosspred, using the coef and vcov from the blup to calculate
    # optimal weather range.
    
    cp <- crosspred(bvar_,
                    coef = blup[[i]]$blup,
                    vcov = blup[[i]]$vcov,
                    cen = minweatherregions_[i],
                    model.link = "log",
                    by = 0.1,
                    from = ranges[i,1],
                    to = ranges[i,2])
    
    # Extract the lower and upper limits where the RR is between 1 and 1.1
    optimal_weather_range[i,"lower"] <- as.numeric(names(
      which.min(which(cp$allRRfit >= 1 & cp$allRRfit <= 1.1))))
    optimal_weather_range[i, "upper"] <- as.numeric(names(
      which.max(which(cp$allRRfit >= 1 & cp$allRRfit <= 1.1))))
    
    below_one <- which(cp$allRRfit < 1)
    above_OWR <- which(as.numeric(names(cp$allRRfit)) > optimal_weather_range[i, "upper"])
    below_OWR <- which(as.numeric(names(cp$allRRfit))< optimal_weather_range[i, "lower"])
    
  }
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 10 - Create Regions RR plot ----------------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  for(i in seq(length(df_list))) {
    
    data <- df_list[[i]]
    
    # NB: Centering point different than original choice of 75th
    argvar <- list(x = data$weather,
                   fun = varfun,
                   df = vardegree)
    #knots = quantile(data$weather,
    #                varper / 100, na.rm = TRUE))
    
    
    bvar <- do.call(onebasis, argvar)
    
    coefs <- blup[[i]]$blup
    vcovs <- blup[[i]]$vcov
    model <- NULL
    cen <- cen_scot # centre based on Scotland MMT
    #cen <- minweatherregions[i]
    #cen <-  quantile(data$weather, na.rm = TRUE, 0.75)
    
    pred <- crosspred(bvar,
                      coef = blup[[i]]$blup,
                      vcov = blup[[i]]$vcov,
                      model.link = "log",
                      by = 0.1,
                      cen = cen)
    
    # line for the middle point where the RR is 1
    reg_lowestrisk_min <- as.numeric(names(
      which.min(pred$allRRfit < 1.00001 & pred$allRRfit > 0.9999)))
    
    reg_lowestrisk_max <- as.numeric(names(
      which.max(pred$allRRfit < 1.00001 & pred$allRRfit > 0.9999)))
    
    # Line for the 97.5th percentile (the threshold for AN in the high heat chart)
    high_heat_line <- quantile(data$weather, 97.5/100, na.rm = TRUE)
    
    # line for heatwave day (i.e. 25 degrees celsius)
    heatwaveday <- 25
    
    # The point at which the RR estimate crosses 1.1 -i.e high risk
    reg_high_risk <- as.numeric(names(
      which.max(which(pred$allRRfit >= 1 & pred$allRRfit <= 1.1) )))
    
    # the point at which the lower confidence interval for RR is above 1.005: i.e. the
    # point at which we can say the risk of mortality starts to increase
    reg_risk_increase <- as.numeric(names(
      which.max(which(pred$allRRlow < 1.006))))
    
    reg_owr_low <- as.numeric(names(which.min(which(pred$allRRlow <= 1.006))))
    reg_owr_high <- as.numeric(names(which.max(which(pred$allRRlow <= 1.006))))
    
    reg_optimal_segment <- (pred$predvar >= owr_low) & (pred$predvar <= owr_high)
    reg_segment_a <- pred$predvar <= reg_owr_low
    reg_segment_b <- pred$predvar >= reg_owr_high & pred$predvar <= reg_risk_increase
    reg_segment_c <- pred$predvar >= reg_risk_increase & pred$predvar <= reg_high_risk
    reg_segment_d <- pred$predvar >= reg_high_risk & pred$predvar <= heatwaveday
    reg_segment_e <- pred$predvar >= heatwaveday
    
    relative_risk_vals<- pred$allRRfit
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      region = unique(df_list[[i]]$regnames),
      x = pred$predvar,          # Predictor variable (e.g., temperature, exposure)
      rr = pred$allRRfit,        # Estimated relative risk
      lower = pred$allRRlow,     # Lower confidence interval
      upper = pred$allRRhigh     # Upper confidence interval
    )
    
    # Add RR data to list for each region
    name <- unique(df_list[[i]]$regnames)
    rr_list[[name]] <- plot_data
    
    breaks <- c(min(data$weather, na.rm = TRUE) - 1,
                seq(pred$predvar[1],
                    pred$predvar[length(pred$predvar)],
                    length = 30),
                max(data$weather, na.rm = TRUE) + 1)
    
    hist <- hist(data$weather, breaks = breaks, plot = FALSE)
    
    # Histogram data
    hist_data <- data.frame(
      region = unique(df_list[[i]]$regnames),
      x = hist$mids,     # Midpoints of histogram bins
      density = hist$density,  # Scaled density values
      counts = hist$counts
    )
    
    # Add histogram data to list for each region
    hist_list[[name]] <- hist_data
    
    # Scale factor:   
    max_hist <- max(hist_data$counts)
    max_density <- max(hist_data$density)
    scaled <- max_hist/max_density
    
    # Maximum value for histogram axis
    max_count <- ceiling(max(hist$counts) / 50) * 50
    
    # Add temperatures to dataframe for excel output later
    temps_rr_df <- temps_rr_df %>% 
      add_row(region = unique(data$regnames),
              opt_temp_range_low = reg_owr_low,
              opt_temp_range_high = reg_owr_high,
              risk_increase_temp = reg_risk_increase,
              high_risk_temp = reg_high_risk
      )
  }
  
  # 11 - Thresholds for attributable number/fraction ------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Extract the 90th and 97.5th percentile for all regions
  per <- t(sapply(df_list, function(x)
    quantile(x$weather, c(90, 97.5)/100, na.rm = T)))
  
  # Create a data frame with final thresholds to use 
  # (Some legacy thresholds retained)
  an_thresholds <- as.data.frame(cbind(per,optimal_weather_range)) %>%
    dplyr::mutate(
      max_high_heat = 100,
      moderate_cold_OWR = lower,
      low_risk = lowestrisk_max, #using Scotland level mmt/risk increase for consistency
      risk_increase = risk_increase,
      high_risk = high_risk,
      scot_risk_inc = 18,
      moderate_heat_OWR = upper,
      moderate_heat_90 = `90%`,
      high_moderate_heatOWR = ifelse(moderate_heat_OWR > `97.5%`,
                                     moderate_heat_OWR,
                                     `97.5%`),
      heatwave_day = 25,
      high_moderate_heat97.5 = `97.5%`
    )
  
  # Create subset of optimal temperature thresholds at regional level
  OWR_thresholds <- an_thresholds %>%
    dplyr::select(moderate_heat_OWR, high_moderate_heatOWR)
  
  # Create subset of percentage thresholds for comparison
  perc_thresholds <- an_thresholds %>%
    dplyr::select(moderate_heat_90, high_moderate_heat97.5)
  
  # Extract Scotland level points of minimum mortality 
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Median of all regional lowest risk (maximum point where RR line is 1)
  (minperccountry <- median(reg_lowestrisk_max))
  
  # if (!is.null(.progress)) .progress$incProgress(0.6)
  
  # 12 - Loop for attributable number of deaths ---------------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # USES BASE MODEL - NOT META
  
  # Create the vectors to store the total mortality (accounting for missing)
  totdeath <- rep(NA, length(names(df_list)))
  names(totdeath) <- names(df_list)
  
  # Number of simulation runs for computing empirical CI
  nsim_ <- 1000
  
  ## List and matrix/array for all years in the dataset
  
  allyears <- as.vector(unique(data$year))
  
  all_matsim <- tibble()
  all_arraysim <- tibble()
  
  # Run the loop
  for(i in seq(df_list)){
    
    # Print
    # cat("Processing region:", names(df_list)[i])
    
    # Extract the data
    data <- df_list[[i]]
    
    # Extract best linear unbiased prediction coefs and vcovs
    coefs <- blup[[i]]$blup
    vcovs <- blup[[i]]$vcov
    
    # Run model
    c(model, cb_temp, cb_hum)  %<-% define_model(dataset = data,
                                                 independent_col1 = independent_col1,
                                                 independent_col2 = independent_col2,
                                                 independent_col3 = independent_col3,
                                                 independent_col4 = independent_col4,
                                                 varfun = varfun,
                                                 varper = varper,
                                                 vardegree = vardegree,
                                                 lag = lag,
                                                 # lagnk = lagnk,
                                                 dfseas = dfseas)
    
    # Using coefs and vcovs from blup, crossbasis has now been extracted,
    # so model argument should be null.
    model <- NULL
    
    # Return heat attributable deaths/hosps for the output year
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    for(timeperiod in allyears){
      #  Create empty matrix 
      matsim <- matrix(NA, length(names(df_list)), 8,
                       dimnames = list(names(df_list),
                                       c("all_heat","risk_increase",
                                         "high_risk","scot_risk_inc",
                                         "moderate_heat", "high_heat",
                                         "heatwave_day", "heatwave"
                                       )))
      
      # Number of simulation runs for computing empirical CI
      nsim_ <- 1000
      
      # Create the array to store the CI of attributable deaths
      arraysim <- array(NA, dim = c(length(names(df_list)), 8, nsim_),
                        dimnames = list(names(df_list),
                                        c("all_heat_ci",
                                          "risk_increase_ci",
                                          "high_risk_ci",
                                          "scot_risk_inc_ci",
                                          "moderate_heat_ci",
                                          "high_heat_ci",
                                          "heatwave_day_ci",
                                          "heatwave_ci"
                                        )))
      
      # Filter data to the year of interest
      data_output_year <- data %>% dplyr::filter(year == timeperiod) %>%
        dplyr::mutate(high_heat_flag = ifelse(weather > 25,1, 0))
      
      # Prepare weather column for attribution to heatwaves
      
      # Force the temperature to be the centering value for non-heatwave days
      data_output_year$heatwave_flag <- NA
      for (j in seq(nrow(data_output_year))){
        # print(paste0("day in quarter: ", j))
        if(j==1){
          
          data_output_year$heatwave_flag[j] <-
            ifelse(data_output_year$high_heat_flag[j] == 1 &
                     data_output_year$high_heat_flag[j+1] == 1, 1, 0)
          
        } else if (j==nrow(data_output_year)){
          
          data_output_year$heatwave_flag[j] <-
            ifelse(data_output_year$high_heat_flag[j] == 1 &
                     data_output_year$high_heat_flag[j-1] == 1, 1, 0)
          
        } else {
          
          data_output_year$heatwave_flag[j] <-
            ifelse((data_output_year$high_heat_flag[j] == 1 &
                      data_output_year$high_heat_flag[j-1] == 1) |
                     (data_output_year$high_heat_flag[j] == 1 &
                        data_output_year$high_heat_flag[j+1] == 1), 1, 0)
        }
      }
      
      data_output_year <- data_output_year %>%
        dplyr::mutate(heatwave_temp = ifelse(heatwave_flag == 1, weather, minweatherregions_[i])) %>%
        dplyr::select(-high_heat_flag,-heatwave_flag)
      
      # Attribution over 90th percentile
      matsim[i, "all_heat"] <- attrdl(x = data_output_year$weather,
                                      basis = cb_temp,
                                      cases = data_output_year$dependent,
                                      coef = coefs,
                                      vcov = vcovs,
                                      type = "an",
                                      dir = "forw",
                                      cen = minweatherregions_[i],
                                      model = model,
                                      range = c(an_thresholds[i,"moderate_heat_90"],
                                                an_thresholds[i,"max_high_heat"]))
      
      
      # Attribution over the risk increase threshold
      matsim[i, "risk_increase"] <- attrdl(x = data_output_year$weather,
                                           basis = cb_temp,
                                           cases = data_output_year$dependent,
                                           coef = coefs,
                                           vcov = vcovs,
                                           type = "an",
                                           dir = "forw",
                                           cen = minweatherregions_[i],
                                           model = model,
                                           range = c(an_thresholds[i,"risk_increase"],
                                                     an_thresholds[i,"max_high_heat"]))
      
      #Attribution above RR = 1.1
      matsim[i, "high_risk"] <- attrdl(x = data_output_year$weather,
                                       basis = cb_temp,
                                       cases = data_output_year$dependent,
                                       coef = coefs,
                                       vcov = vcovs,
                                       type = "an",
                                       dir = "forw",
                                       cen = minweatherregions_[i],
                                       model = model,
                                       range = c(an_thresholds[i,"high_risk"],
                                                 an_thresholds[i,"max_high_heat"]))
      
      #Attribution above RR = 1.1
      matsim[i, "scot_risk_inc"] <- attrdl(x = data_output_year$weather,
                                           basis = cb_temp,
                                           cases = data_output_year$dependent,
                                           coef = coefs,
                                           vcov = vcovs,
                                           type = "an",
                                           dir = "forw",
                                           cen = minweatherregions_[i],
                                           model = model,
                                           range = c(18,
                                                     an_thresholds[i,"max_high_heat"]))
      
      # Attribution between 90 and 97.5th percentile
      matsim[i, "moderate_heat" ] <- attrdl(x = data_output_year$weather,
                                            basis = cb_temp,
                                            cases = data_output_year$dependent,
                                            coef = coefs,
                                            vcov = vcovs,
                                            type="an",
                                            dir = "forw",
                                            cen = minweatherregions_[i],
                                            model = model,
                                            range = c(an_thresholds[i,"moderate_heat_90"],
                                                      an_thresholds[i,"high_moderate_heat97.5"]))
      
      #Over 97.5th percentile
      matsim[i,"high_heat"] <- attrdl(x = data_output_year$weather,
                                      basis = cb_temp,
                                      cases = data_output_year$dependent,
                                      coef = coefs,
                                      vcov = vcovs,
                                      model = model,
                                      type = "an",
                                      dir = "forw",
                                      cen = minweatherregions_[i],
                                      range = c(an_thresholds[i,"high_moderate_heat97.5"],
                                                an_thresholds[i,"max_high_heat"]))
      
      # 1 day at 25 or higher
      matsim[i,"heatwave_day"] <- attrdl(x = data_output_year$heatwave_temp,
                                         basis = cb_temp,
                                         cases = data_output_year$dependent,
                                         coef = coefs,
                                         vcov = vcovs,
                                         model = model,
                                         type = "an",
                                         dir = "forw",
                                         cen = minweatherregions_[i],
                                         range = c(an_thresholds[i,"heatwave_day"],
                                                   an_thresholds[i,"max_high_heat"]))
      
      # 2 days at 25 or higher
      matsim[i,"heatwave"] <- attrdl(x = data_output_year$heatwave_temp,
                                     basis = cb_temp,
                                     cases = data_output_year$dependent,
                                     coef = coefs,
                                     vcov = vcovs,
                                     model = model,
                                     type = "an",
                                     dir = "forw",
                                     cen = minweatherregions_[i])
      
      # CONFIDENCE INTERVALS FOR ESTIMATES 
      # Compute empirical occurrences of the attributable deaths
      # Used to derive confidence intervals
      
      # Attribution over 90th percentile
      arraysim[i, "all_heat_ci", ] <- attrdl(x = data_output_year$weather,
                                             basis = cb_temp,
                                             cases = data_output_year$dependent,
                                             coef = coefs,
                                             vcov = vcovs,
                                             type = "an",
                                             dir = "forw",
                                             cen = minweatherregions_[i],
                                             model = model,
                                             range = c(an_thresholds[i,"moderate_heat_90"],
                                                       an_thresholds[i,"max_high_heat"]),
                                             sim = T, nsim = nsim_)
      
      # Attribution over risk increase threshold
      arraysim[i, "risk_increase_ci", ] <- attrdl(x = data_output_year$weather,
                                                  basis = cb_temp,
                                                  cases = data_output_year$dependent,
                                                  coef = coefs,
                                                  vcov = vcovs,
                                                  type = "an",
                                                  dir = "forw",
                                                  cen = minweatherregions_[i],
                                                  model = model,
                                                  range = c(an_thresholds[i,"risk_increase"],
                                                            an_thresholds[i,"max_high_heat"]),
                                                  sim = T, nsim = nsim_)
      
      #Attribution above RR = 1.1
      arraysim[i, "high_risk_ci", ] <- attrdl(x = data_output_year$weather,
                                              basis = cb_temp,
                                              cases = data_output_year$dependent,
                                              coef = coefs,
                                              vcov = vcovs,
                                              type = "an",
                                              dir = "forw",
                                              cen = minweatherregions_[i],
                                              model = model,
                                              range = c(an_thresholds[i,"high_risk"],
                                                        an_thresholds[i,"max_high_heat"]),
                                              sim = T, nsim = nsim_)
      
      #Attribution above scotland RI (18 degrees)
      arraysim[i, "scot_risk_inc_ci", ] <- attrdl(x = data_output_year$weather,
                                                  basis = cb_temp,
                                                  cases = data_output_year$dependent,
                                                  coef = coefs,
                                                  vcov = vcovs,
                                                  type = "an",
                                                  dir = "forw",
                                                  cen = minweatherregions_[i],
                                                  model = model,
                                                  range = c(18,
                                                            an_thresholds[i,"max_high_heat"]),
                                                  sim = T, nsim = nsim_)
      
      # Attribution between 90 and 97.5th percentile
      arraysim[i, "moderate_heat_ci", ] <- attrdl(x = data_output_year$weather,
                                                  basis = cb_temp,
                                                  cases = data_output_year$dependent,
                                                  coef = coefs,
                                                  vcov = vcovs,
                                                  type = "an",
                                                  dir = "forw",
                                                  cen = minweatherregions_[i],
                                                  model = model,
                                                  range = c(an_thresholds[i,"moderate_heat_90"],
                                                            an_thresholds[i,"high_moderate_heat97.5"]),
                                                  sim = T, nsim = nsim_)
      
      # Attribution over the 97.5th percentile
      arraysim[i, "high_heat_ci", ] <- attrdl(x = data_output_year$weather,
                                              basis = cb_temp,
                                              cases = data_output_year$dependent,
                                              coef = coefs,
                                              vcov = vcovs,
                                              type = "an",
                                              dir= "forw",
                                              cen = minweatherregions_[i],
                                              model = model,
                                              range = c(an_thresholds[i,"high_moderate_heat97.5"],
                                                        an_thresholds[i,"max_high_heat"]),
                                              sim = T, nsim = nsim_)
      
      # Attribution over 25 degrees for a day
      arraysim[i, "heatwave_day_ci", ] <- attrdl(x = data_output_year$heatwave_temp,
                                                 basis = cb_temp,
                                                 cases = data_output_year$dependent,
                                                 coef = coefs,
                                                 vcov = vcovs,
                                                 type = "an",
                                                 dir= "forw",
                                                 cen = minweatherregions_[i],
                                                 model = model,
                                                 sim = T, nsim = nsim_)
      
      # Attribution over 25 degrees for two days
      arraysim[i, "heatwave_ci", ] <- attrdl(x = data_output_year$heatwave_temp,
                                             basis = cb_temp,
                                             cases = data_output_year$dependent,
                                             coef = coefs,
                                             vcov = vcovs,
                                             type = "an",
                                             dir= "forw",
                                             cen = minweatherregions_[i],
                                             model = model,
                                             sim = T, nsim = nsim_)
      
      matsim = data.frame(matsim) |> 
        rownames_to_column(var = "regnames") |> 
        filter(regnames == unique(df_list[[i]]$regnames)) |> 
        mutate(year=timeperiod)
      
      arraysim_low = apply(arraysim, c(1,2), quantile, 0.025, na.rm = TRUE) |> 
        data.frame() |> 
        rownames_to_column(var = "regnames") |> 
        filter(regnames == unique(df_list[[i]]$regnames)) |> 
        mutate(year=timeperiod,
               ci = "low")
      
      arraysim_high = apply(arraysim, c(1,2), quantile, 0.975, na.rm = TRUE) |> 
        data.frame() |> 
        rownames_to_column(var = "regnames") |> 
        filter(regnames == unique(df_list[[i]]$regnames)) |> 
        mutate(year=timeperiod,
               ci = "high")
      
      all_matsim <- rbind(all_matsim,matsim) 
      all_arraysim <- rbind(all_arraysim,arraysim_low,arraysim_high)
      
    }
    
  }
  
  # if (!is.null(.progress)) .progress$incProgress(0.8)
  
  # 13 - Aggregate attributable numbers ------------------------------------------
  
  ## Attributable number estimate by region:
  
  #all_matsim
  
  ## Attributable number CI estimate by region:
  #all_arraysim
  
  ## Attributable number estimate at Scotland level
  
  scot_att <- all_matsim %>%
    group_by(year) %>%
    summarise_if(is.numeric, sum, na.rm=TRUE)%>%
    ungroup()
  
  ## Attributable number CI estimate at Scotland level
  
  # Low CI
  scot_att_low <- all_arraysim %>%
    filter(ci == "low") %>%
    group_by(year) %>%
    summarise_if(is.numeric, sum, na.rm=TRUE)%>%
    ungroup()
  
  # High CI
  scot_att_high <- all_arraysim%>%
    filter(ci == "high") %>%
    group_by(year) %>%
    summarise_if(is.numeric, sum, na.rm=TRUE)%>%
    ungroup()
  
  scot_att_low_all <- scot_att_low%>%
    rename(all_heat_lci = all_heat_ci,
           risk_increase_lci = risk_increase_ci,
           high_risk_lci = high_risk_ci,
           scot_risk_inc_lci = scot_risk_inc_ci,
           moderate_heat_lci = moderate_heat_ci,
           high_heat_lci = high_heat_ci,
           heatwave_day_lci = heatwave_day_ci,
           heatwave_lci= heatwave_ci)
  
  scot_att_high_all <- scot_att_high %>%
    rename(all_heat_uci = all_heat_ci,
           risk_increase_uci = risk_increase_ci,
           high_risk_uci = high_risk_ci,
           scot_risk_inc_uci = scot_risk_inc_ci,
           moderate_heat_uci = moderate_heat_ci,
           high_heat_uci = high_heat_ci,
           heatwave_day_uci = heatwave_day_ci,
           heatwave_uci= heatwave_ci)
  
  # Join all heat attributions (lci, uci, estimate) into one dataframe
  scot_dat <- scot_att %>%
    left_join(scot_att_low_all, by = "year")%>%
    left_join(scot_att_high_all, by = "year")
  
  
  # Extract annual weather data for charts from initial df
  scot_weather <- df %>%
    group_by(year)%>%
    summarise(max = max(weather),
              min = min(weather),
              mean = mean(weather))%>%
    ungroup()
  
  # Create quick plots for Scotland's weather
  scot_weather <- df %>%
    group_by(year)%>%
    summarise(max = max(weather),
              min = min(weather),
              mean = mean(weather))%>%
    ungroup()
  
  # Calculate number of days over 23.5 per year
  
  hot_days <- df %>%
    mutate(risk_inc = ifelse(weather >= 19.4, 1, 0),
           risk_inc_hosp = ifelse(weather >= 10.5,1,0),
           high_risk = ifelse(weather >= 23.1,1,0),
           high_risk_hosp = ifelse(weather >= 18.5,1,0),
           heatwave = ifelse(weather >=25,1,0)
    )%>%
    group_by(year) %>%
    summarise(risk_inc = sum(risk_inc),
              risk_inc_hosp = sum(risk_inc_hosp),
              high_risk = sum(high_risk),
              high_risk_hosp = sum(high_risk_hosp),
              heatwave = sum(heatwave))%>%
    ungroup()
  
  hot_days_long <- hot_days %>%
    pivot_longer(cols = c(risk_inc,
                          high_risk,
                          high_risk_hosp,
                          heatwave),
                 names_to = "threshold",
                 values_to = "number_of_days")
  
  # Extract region estimates
  # Regions-specific
  anregions <- all_matsim
  anregionslow <- all_arraysim %>%
    filter(ci == "low")
  anregionshigh <- all_arraysim %>%
    filter(ci == "high")
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 15 - Attributable rates per 100,000 (Scotland) --------------------------------
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  #match populations to anregions data
  pops <- dataset %>%
    dplyr::select(regnames,pop_col,year)%>%
    distinct()
  
  # Total attributable numbers summary:
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  antot <- all_matsim %>%
    left_join(pops, by = c("year","regnames"))%>%
    group_by(year)%>%
    summarise(across(c(2:8),~sum(., na.rm = TRUE)),
              pop_col = sum(pop_col))%>%
    ungroup()
  
  antotlow <- anregionslow %>%
    left_join(pops, by = c("year","regnames"))%>%
    group_by(year)%>%
    summarise(across(c(2:8),~sum(., na.rm = TRUE)),
              pop_col = sum(pop_col))%>%
    ungroup()
  
  antothigh <- anregionshigh %>%
    left_join(pops, by = c("year","regnames"))%>%
    group_by(year)%>%
    summarise(across(c(2:8),~sum(., na.rm = TRUE)),
              pop_col = sum(pop_col))%>%
    ungroup()
  
  # combine attrib nums and CIs for Scotland
  antot_scotland <- antot %>% 
    left_join(antotlow %>% 
                select(-pop_col) %>% 
                rename(all_heat_lci = all_heat_ci,
                       risk_increase_lci = risk_increase_ci,
                       high_risk_lci = high_risk_ci,
                       scot_risk_inc_lci = scot_risk_inc_ci,
                       moderate_heat_lci = moderate_heat_ci,
                       high_heat_lci = high_heat_ci,
                       heatwave_day_lci = heatwave_day_ci
                ), by = "year") %>% 
    left_join(antothigh %>% 
                select(-pop_col) %>% 
                rename(all_heat_uci = all_heat_ci,
                       risk_increase_uci = risk_increase_ci,
                       high_risk_uci = high_risk_ci,
                       scot_risk_inc_uci = scot_risk_inc_ci,
                       moderate_heat_uci = moderate_heat_ci,
                       high_heat_uci = high_heat_ci,
                       heatwave_day_uci = heatwave_day_ci
                ), by = "year")
  
  # Calculate attributable rate from attributable numbers:
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Use whole relevant popn values when examining popn groups i.e. age, sex, SIMD
  if(vuln_group != "All"){
    
    pop_data <- read_csv("/conf/linkage/output/lookups/Unicode/Populations/Estimates/HB2019_pop_est_1981_2024.csv")
    pop_data_simd <- read_csv("/conf/quality_indicators/Climate/lookups/simd_pops_by_healthboard.csv")
    
    year_list <- antot %>% distinct(year) %>% pull()
    
    # Filter population data to relevant years and wrangle columns
    pop_data <- pop_data %>% 
      filter(year %in% year_list) %>% 
      mutate(
        sex_name = case_when(
          sex_name == "M" ~ "Male",
          sex_name == "F" ~ "Female"
        ),
        age_band = case_when(
          age <= 65 ~ "Under 65 yrs",
          TRUE ~ "65 yrs plus"
        ))
    
    # Scotland-wide population data:
    scot_pop_data <- pop_data %>% 
      group_by(year, sex_name, age_band) %>% 
      summarise(pop = sum(pop))
    
    scot_pop_simd_data <- pop_data_simd %>% 
      group_by(year, simd) %>% 
      summarise(pop = sum(pop))
    
    # Popn data only up to 2022, need to copy and paste for remaining years
    # Extract 2022 rows only
    simd_2022_pop <- scot_pop_simd_data %>% 
      filter(year == 2022)
    
    # Create copies for 2023 and 2024
    simd_2023_pop <- simd_2022_pop %>% mutate(year = 2023)
    simd_2024_pop <- simd_2022_pop %>% mutate(year = 2024)
    
    # Combine all together
    scot_pop_simd_data <- bind_rows(scot_pop_simd_data, simd_2023_pop, simd_2024_pop)
    
    # Depending on vulnerability breakdown, calculate pop col
    if(vuln_breakdown == "Age"){
      pop_col_new <- scot_pop_data %>% 
        ungroup(sex_name) %>%
        filter(age_band == vuln_group) %>% 
        group_by(year, age_band) %>% 
        summarise(pop_col_new = sum(pop)) %>% 
        ungroup() %>% 
        pull(pop_col_new) 
    }else if(vuln_breakdown == "Sex"){
      pop_col_new <- scot_pop_data %>% 
        ungroup(age_band) %>%
        filter(sex_name == vuln_group) %>% 
        group_by(year, sex_name) %>% 
        summarise(pop_col_new = sum(pop)) %>% 
        ungroup() %>% 
        pull(pop_col_new) 
    }else if(vuln_breakdown == "Deprivation index"){
      if(vuln_group == "1 & 2"){
        simd_filter <- c(1, 2)
      }else if(vuln_group == "4 & 5"){
        simd_filter <- c(4, 5)
      }else{
        simd_filter <- as.numeric(vuln_group)
      }
      pop_col_new <- scot_pop_simd_data %>% 
        filter(simd %in% simd_filter) %>% 
        group_by(year) %>% 
        summarise(pop_col_new = sum(pop)) %>% 
        ungroup() %>% 
        pull(pop_col_new)
    }
    
    # Replace old population values with new ones in antot dataframes
    antot <- antot %>% 
      mutate(pop_col = pop_col_new)
    antotlow <- antotlow %>% 
      mutate(pop_col = pop_col_new)
    antothigh <- antothigh %>% 
      mutate(pop_col = pop_col_new)
    
  }
  # Calculate attributable rates using filtered populations based on chosen demographics
  artot <- antot %>%
    mutate(across(c(2:8), ~ ./pop_col * 100000, .names = "ar_{.col}"))
  
  artotlow <- antotlow %>%
    mutate(across(c(2:8), ~ ./pop_col * 100000, .names = "ar_{.col}"))
  
  artothigh <- antothigh %>%
    mutate(across(c(2:8), ~ ./pop_col * 100000, .names = "ar_{.col}"))
  
  
  
  # Now create attributable rates datasets and plots for each thresholds:
  # Scotland level
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # High heat
  att_rate_highheat <- bind_cols(artot,artotlow$ar_high_heat_ci,
                                 artothigh$ar_high_heat_ci) %>%
    rename(low_ci = "...17",
           high_ci = "...18")
  
  # Attributable to high risk
  att_rate_high_risk <- bind_cols(artot,artotlow$ar_high_risk_ci,
                                  artothigh$ar_high_risk_ci) %>%
    rename(low_ci = "...17",
           high_ci = "...18")
  
  # Attributable to increasing risk
  att_rate_risk_increase <- bind_cols(artot,artotlow$ar_risk_increase_ci,
                                      artothigh$ar_risk_increase_ci) %>%
    rename(low_ci = "...17",
           high_ci = "...18")
  
  # Attributable to Scotland increasing risk
  att_rate_scot_risk_inc <- bind_cols(artot,artotlow$ar_scot_risk_inc_ci,
                                      artothigh$scot_risk_inc_ci) %>%
    rename(low_ci = "...17",
           high_ci = "...18")
  
  # Attributable over 25 degrees (heatwave day)
  att_rate_heatwave_day <- bind_cols(artot,artotlow$ar_heatwave_day_ci,
                                     artothigh$ar_heatwave_day_ci) %>%
    rename(low_ci = "...17",
           high_ci = "...18")
  
  artot_scotland <- bind_cols(artot,artotlow$ar_risk_increase_ci,
                              artothigh$ar_risk_increase_ci) %>%
    rename(ar_risk_increase_lci = "...17",
           ar_risk_increase_uci = "...18") %>%
    bind_cols(artotlow$ar_high_risk_ci,
              artothigh$ar_high_risk_ci) %>%
    rename(ar_high_risk_lci = "...19",
           ar_high_risk_uci = "...20") %>% 
    bind_cols(artotlow$ar_scot_risk_inc_ci,
              artothigh$ar_scot_risk_inc_ci) %>%
    rename(scot_risk_inc_lci = "...21",
           scot_risk_inc_uci = "...22") %>% 
    bind_cols(artotlow$ar_heatwave_day_ci,
              artothigh$ar_heatwave_day_ci) %>%
    rename(ar_heatwave_day_lci = "...23",
           ar_heatwave_day_uci = "...24")
  
  
  # 16 - Regional Attributable number ---------------------------------------
  
  # Regional lower CI
  att_low <- all_arraysim %>%
    filter(ci == "low")
  
  att_low_all <- att_low %>%
    rename(all_heat_lci = all_heat_ci,
           risk_increase_lci = risk_increase_ci,
           high_risk_lci = high_risk_ci,
           moderate_heat_lci = moderate_heat_ci,
           high_heat_lci = high_heat_ci,
           heatwave_day_lci = heatwave_day_ci,
           heatwave_lci= heatwave_ci)
  
  # Regional upper CI
  att_high <- all_arraysim %>%
    filter(ci == "high")
  
  att_high_all <- att_high %>%
    rename(all_heat_uci = all_heat_ci,
           risk_increase_uci = risk_increase_ci,
           high_risk_uci = high_risk_ci,
           moderate_heat_uci = moderate_heat_ci,
           high_heat_uci = high_heat_ci,
           heatwave_day_uci = heatwave_day_ci,
           heatwave_uci= heatwave_ci)
  
  # Join all heat attributions (lci, uci, estimate) into one dataframe
  
  dat <- all_matsim %>%
    left_join(att_low_all, by = c("year", "regnames"))%>%
    left_join(att_high_all, by = c("year", "regnames"))
  
  # Get unique areas
  region_names <- names(df_list)
  
  # if (!is.null(.progress)) .progress$incProgress(0.9)
  # 
  # 17 - Regional Attributable Rate  ----------------------------------------
  
  # First, if building a model for a demographic group, need to calculate correct
  # population values for each region
  if(vuln_group != "All"){
    # Healthboard population data:
    hb_pop_data <- pop_data %>% 
      group_by(year, hb2019, hb2019name, sex_name, age_band) %>%
      summarise(pop = sum(pop))
    
    hb_pop_simd_data <- pop_data_simd %>% 
      group_by(year, hb2019, hb2019name, simd) %>%
      summarise(pop = sum(pop))
    
    # Popn SIMD data only up to 2022, need to copy and paste for remaining years
    # Extract 2022 rows only
    simd_2022_pop_hb <- hb_pop_simd_data %>% 
      filter(year == 2022)
    
    # Create copies for 2023 and 2024
    simd_2023_pop_hb <- simd_2022_pop_hb %>% mutate(year = 2023)
    simd_2024_pop_hb <- simd_2022_pop_hb %>% mutate(year = 2024)
    
    # Combine all together
    hb_pop_simd_data <- bind_rows(hb_pop_simd_data, simd_2023_pop_hb, simd_2024_pop_hb)
    
    # Calculate pop col based on vulnerability group
    if(vuln_breakdown == "Age"){
      pops <- hb_pop_data %>% 
        ungroup(sex_name) %>%
        filter(age_band == vuln_group) %>% 
        group_by(year, hb2019name, age_band) %>% 
        summarise(pop_col = sum(pop)) %>% 
        ungroup() %>% 
        select(-age_band) %>% 
        rename(regnames = hb2019name)
    }else if(vuln_breakdown == "Sex"){
      pops <- hb_pop_data %>% 
        ungroup(age_band) %>%
        filter(sex_name == vuln_group) %>% 
        group_by(year, hb2019name, sex_name) %>% 
        summarise(pop_col = sum(pop)) %>% 
        ungroup() %>%
        select(-sex_name) %>% 
        rename(regnames = hb2019name)
    }else if(vuln_breakdown == "Deprivation index"){
      pops <- hb_pop_simd_data %>% 
        filter(simd %in% simd_filter) %>% 
        group_by(year, hb2019name, simd) %>% 
        summarise(pop_col = sum(pop)) %>% 
        ungroup() %>%
        select(-simd) %>% 
        rename(regnames = hb2019name)
    }
  }
  
  # Regional AR
  arregions <- anregions %>%
    left_join(pops, by = c("year","regnames")) %>%
    mutate(across(c(2:8),~./pop_col *100000, .names = "ar_{.col}")) %>%
    select(-c(all_heat,risk_increase,high_risk,moderate_heat,
              high_heat,heatwave_day,heatwave))
  
  arregionslow <- anregionslow %>%
    left_join(pops, by = c("year","regnames")) %>%
    mutate(across(c(2:8),~./pop_col *100000, .names = "ar_{.col}")) %>%
    select(-c(all_heat_ci,risk_increase_ci,high_risk_ci,moderate_heat_ci,
              high_heat_ci,heatwave_day_ci,heatwave_ci, ci))
  
  arregionslow <- arregionslow%>%
    rename(ar_all_heat_lci = ar_all_heat_ci,
           ar_risk_increase_lci = ar_risk_increase_ci,
           ar_high_risk_lci = ar_high_risk_ci,
           ar_moderate_heat_lci = ar_moderate_heat_ci,
           ar_high_heat_lci = ar_high_heat_ci,
           ar_heatwave_day_lci = ar_heatwave_day_ci)
  
  arregionshigh <- anregionshigh %>%
    left_join(pops, by = c("year","regnames")) %>%
    mutate(across(c(2:8),~./pop_col *100000, .names = "ar_{.col}")) %>%
    select(-c(all_heat_ci,risk_increase_ci,high_risk_ci,moderate_heat_ci,
              high_heat_ci,heatwave_day_ci,heatwave_ci, ci))
  
  arregionshigh <- arregionshigh%>%
    rename(ar_all_heat_uci = ar_all_heat_ci,
           ar_risk_increase_uci = ar_risk_increase_ci,
           ar_high_risk_uci = ar_high_risk_ci,
           ar_moderate_heat_uci = ar_moderate_heat_ci,
           ar_high_heat_uci = ar_high_heat_ci,
           ar_heatwave_day_uci = ar_heatwave_day_ci)
  
  ARdat <- arregions %>%
    left_join(arregionslow, by = c("year", "regnames", "pop_col"))%>%
    left_join(arregionshigh, by = c("year", "regnames", "pop_col"))
  
  # Get unique areas
  region_names<- names(df_list)
  
  # if (!is.null(.progress)) .progress$incProgress(1)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # END
  
  return(list(
    temps_rr_df, # this is the OTR and RR increase temp thresholds for all regions
    hist_list, # data for temp histograms
    rr_list, # data for RR curves
    antot_scotland,
    artot_scotland,
    dat,
    ARdat,
    aic,
    summarised_events
  ))
  
}

