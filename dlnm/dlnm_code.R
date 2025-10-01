#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dlnm_code.R
# Jan 25
# Bella Tortora Brayda & Sarah Reed
# 
# Adapted from Scotland - heat related mortality.R - converted code into a versatile
# Distributed Lag Non-linear Model script for use on other indicators. 
# 
# Paired with the dlnm_setup.R script where arguments and data are updated for this
# script, and followed by dlnm_model_checking.R 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1 - Source files --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# source config script:
# source(here::here("dlnm/dlnm_setup.R"))

# 2 - Load, read and manipulate data  ------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Select data files from input_data_path, detecting input_data_pattern (both 
# provided in dlnm_setup.R)


data_files <- list.files(path = input_data_path,
                         recursive = TRUE,
                         pattern = input_data_pattern,
                         full.names = TRUE)

# Read in data and rename variables
df <- read_csv(data_files)%>%
  dplyr::select(-1) %>%
  dplyr::rename(dependent = all_of(dependent_col),
                date = all_of(time_col),
                regnames = all_of(region_col),
                weather = all_of(weather_col),
                humidity = all_of(humidity_col),
                pop_col = all_of(population_col)) %>%
  dplyr::mutate(date = as.Date(date,format = "%d/%m/%y"),
                year = as.numeric(year),
                month = month(date))%>%
  dplyr::filter(year > 2004 & year < 2025) 
#  dplyr::filter(year!=2020 & year!=2021) # Remove covid years from data

# use this to calculate summary stats
# deaths_summary <- df %>%
#   summarise(
#     total_deaths = sum(death),
#     total_deaths_under65 = sum(dependent, na.rm = TRUE),
#     total_deaths_65over = sum(death_65yrs_over, na.rm = TRUE),
#     perc_deaths_under65 = total_deaths_under65/sum(death, na.rm = TRUE) * 100,
#     perc_deaths_65over = total_deaths_65over/sum(death, na.rm = TRUE) * 100
#   )
# hosps_summary <- df %>%
#   summarise(
#     total_hosps = sum(admissions),
#     total_hosps_under65 = sum(dependent, na.rm = TRUE),
#     total_hosps_65over = sum(adm_65yrs_over, na.rm = TRUE),
#     perc_hosps_under65 = total_hosps_under65/sum(admissions, na.rm = TRUE) * 100,
#     perc_hosps_65over = total_hosps_65over/sum(admissions, na.rm = TRUE) * 100
#   )

# Save out median and mean weather conditions for reference

scotmed <- median(df$weather)
scotmean<- mean(df$weather)

# If summer selected in dlnm_setup, then filter data to summer months
if(summer == TRUE){
  df<- dplyr::filter(df, month %in% month_choice)
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
  
  cat(i,"")
  
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
print(summary(mv)["AIC"])


# SUMMARY FROM META ANALYSIS
mvsummary <- summary(mv)

# 6 - Model Validation ----------------------------------------------------

# Plot residuals for checking:
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mv_boxplot <- boxplot(mv$residuals)

acf(resid(mv))

# Use the Augmented Dickey-Fuller (ADF) test to check if the residuals are stationary 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

class(mv$residuals)  # Check the class - MATRIX but need vector
residuals_vector <- as.vector(mv$residuals[, 1]) # Extract the first column 
adf.test(residuals_vector)
# If the p-value is high (>0.05), the residuals are non-stationary. 


# Testing if the autocorrelation residuals are ok using Ljung-Box test 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# use a lag = sqrt(n)  where n is the number of observations in your residuals.
# uses residuals_vector from above. If the p-value > 0.05, the residuals are 
# likely white noise.

boxlag <- floor(sqrt(length(residuals_vector))) # 3
Box.test(residuals_vector, lag = boxlag, type = "Ljung-Box")

# Can check all columns with the following code:
for (i in 1:ncol(mv$residuals)) {
  residuals_vector <- as.vector(mv$residuals[, i])
  boxlag <- floor(sqrt(length(residuals_vector)))
  cat("Ljung-Box test for column", i, "\n")
  print(Box.test(residuals_vector, lag = boxlag, type = "Ljung-Box"))
}

# Function for computing the P-value of a wald test
fwald <- function(model,var) {
  ind <- grep(var,names(coef(model)))
  coef <- coef(model)[ind]
  vcov <- vcov(model)[ind,ind]
  waldstat <- coef%*%solve(vcov)%*%coef
  df <- length(coef)
  return(1-pchisq(waldstat,df))
}

# Use the fwald to test the effects of the metapredictors (if used)
if("avgweather" %in% metapreds){
  avgweather_fwald <- fwald(mv, "avgweather")
}

if("rangeweather" %in% metapreds){
  rangeweather_fwald <- fwald(mv,"rangeweather")
}

if("maxweather" %in% metapreds){
  maxweather_fwald <- fwald(mv,"maxweather")
}

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
plot_data <- data.frame(
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

breaks_scot <- c(min(data$weather, na.rm = TRUE) - 1,
                 seq(pred_scot$predvar[1],
                     pred_scot$predvar[length(pred_scot$predvar)],
                     length = 30),
                 max(data$weather, na.rm = TRUE) + 1)

hist_scot <- hist(data$weather, breaks = breaks_scot, plot = FALSE)

hist_data <- data.frame(
  x = hist_scot$mids,     # Midpoints of histogram bins
  density = hist_scot$density,  # Scaled density values
  counts = hist_scot$counts
)

max_hist <- max(hist_scot$counts)
max_density <- max(hist_scot$density)

# Scale factor:   
scaled <- max_hist/max_density
# Maximum value for histogram axis
max_count <- ceiling(max(hist_scot$counts) / 50) * 50

Scotland_RR_Plot <- ggplot() +
  
  # Insert green panel for optimal weather range
  annotate("rect", fill = "#b0e8a9", alpha = 0.5, 
           xmin = owr_low, xmax = owr_high,
           ymin = 0, ymax = Inf) +
  
  # Histogram Layer (Plotted First)
  geom_col(data = hist_data, aes(x = x, y = density *3.6), 
           fill = "grey80", color = "grey60"
  ) +
  
  # Relative Risk Curve
  geom_ribbon(data = plot_data, aes(x = x, ymin = lower, ymax = upper), 
              fill = "#f9e38c") +  # Confidence interval
  geom_line(data = plot_data, aes(x = x, y = rr), color = "black", linewidth = 1) +
  
  
  # Segment Lines (using proper filtering)
  geom_line(data = plot_data[segment_b, ], aes(x = x, y = rr), color = "black", linewidth = 1) +
  geom_line(data = plot_data[segment_c, ], aes(x = x, y = rr), color = "#FF0000", linewidth = 1) +
  geom_line(data = plot_data[segment_d, ], aes(x = x, y = rr), color = "#8B0000", linewidth = 1) +
  geom_line(data = plot_data[segment_e, ], aes(x = x, y = rr), color = "#D33F6A", linewidth = 1) +
  
  # Vertical Reference Lines
  #geom_vline(xintercept = lowestrisk_max, linetype = "solid", color = "#32CD32") +
  geom_vline(xintercept = risk_increase, linetype = "solid", color = "#E9B62D") +
  geom_vline(xintercept = high_risk, linetype = "solid", color = "#E57E41") +
  geom_vline(xintercept = heatwaveday, linetype = "dashed", color = "#D33F6A") +
  
  # Horizontal RR = 1 Reference Lines
  geom_hline(yintercept = 1, linetype = "solid", color = "black")+
  
  # Formatting y-axis with secondary axis for counts
  scale_y_continuous(
    limits = c(0, 1.5),  # Match ylim from base R
    sec.axis = sec_axis(~ .* max_hist , name = "No. of Days", breaks = seq(0, max_count, by = 350))
  ) +
  
  # Theme and labels
  theme_minimal() +
  labs(
    x = "Maximum temperature",
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
Scotland_RR_Plot

# Create dataframe of RR temps for indicators spreadsheet later
temps_rr_df <- tibble(
  region = "Scotland",
  opt_temp_range_low = owr_low,
  opt_temp_range_high = owr_high,
  risk_increase_temp = risk_increase,
  high_risk_temp = high_risk
)

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

plot_list <- list()

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
    x = pred$predvar,          # Predictor variable (e.g., temperature, exposure)
    rr = pred$allRRfit,        # Estimated relative risk
    lower = pred$allRRlow,     # Lower confidence interval
    upper = pred$allRRhigh     # Upper confidence interval
  )
  
  breaks <- c(min(data$weather, na.rm = TRUE) - 1,
              seq(pred$predvar[1],
                  pred$predvar[length(pred$predvar)],
                  length = 30),
              max(data$weather, na.rm = TRUE) + 1)
  
  hist <- hist(data$weather, breaks = breaks, plot = FALSE)
  
  # Histogram data
  hist_data <- data.frame(
    x = hist$mids,     # Midpoints of histogram bins
    density = hist$density,  # Scaled density values
    counts = hist$counts
  )
  
  # Scale factor:   
  max_hist <- max(hist_data$counts)
  max_density <- max(hist_data$density)
  scaled <- max_hist/max_density
  
  # Maximum value for histogram axis
  max_count <- ceiling(max(hist$counts) / 50) * 50
  
  p <- ggplot() +
    
    annotate("rect", fill = "#b0e8a9", alpha = 0.5, 
             xmin = reg_owr_low, xmax = reg_owr_high,
             ymin = 0, ymax = Inf) +
    
    # Histogram Layer (Plotted First)
    geom_col(data = hist_data, aes(x = x, y = density * 3.6), 
             fill = "grey80", color = "grey60"
    ) +
    
    # Relative Risk Curve
    geom_line(data = plot_data, aes(x = x, y = rr), color = "black", linewidth = 0.5) +
    
    geom_ribbon(data = plot_data, aes(x = x, ymin = lower, ymax = upper), 
                fill = "#f9e38c") +  # Confidence interval
    
    # Segment Lines (using proper filtering)
    geom_line(data = plot_data[reg_segment_a, ], aes(x = x, y = rr), color = "black", linewidth = 0.5) +
    geom_line(data = plot_data[reg_segment_b, ], aes(x = x, y = rr), color = "black", linewidth = 0.5) +
    geom_line(data = plot_data[reg_segment_c, ], aes(x = x, y = rr), color = "#FF0000", linewidth = 0.5) +
    geom_line(data = plot_data[reg_segment_d, ], aes(x = x, y = rr), color = "#8B0000", linewidth = 0.5) +
    geom_line(data = plot_data[reg_segment_e, ], aes(x = x, y = rr), color = "#D33F6A", linewidth = 0.5) +
    geom_line(data = plot_data[reg_optimal_segment, ], aes(x = x, y = rr), color = "51ba45", linewidth = 0.5) +
    
    # Vertical Reference Lines
    #geom_vline(xintercept = reg_lowestrisk_max, linetype = "solid", color = "#32CD32") +
    geom_vline(xintercept = reg_risk_increase, linetype = "solid", color = "#E9B62D") +
    geom_vline(xintercept = reg_high_risk, linetype = "solid", color = "#E57E41") +
    geom_vline(xintercept = heatwaveday, linetype = "dashed", color = "#D33F6A") +
    
    # Horizontal RR = 1 Reference Lines
    geom_hline(yintercept = 1, linetype = "dashed", color = "black")+
    
    # Formatting y-axis with secondary axis for counts
    scale_y_continuous(
      limits = c(0,1.5),  # Match ylim from base R
      sec.axis = sec_axis(~ . * max_hist, name = "No. of Days", breaks = seq(0, max_hist, by = 100))
    ) +
    
    # Theme and labels
    theme_minimal() +
    labs(
      title = unique(df_list[[i]]$regnames),
      x = "Maximum temperature",
      y = "Relative Risk"
    ) +
    theme(
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5),
      text = element_text(size = 8),
      axis.line = element_line(color = "black"), 
      axis.title.y.right = element_text(color = "black")  # Make secondary y-axis readable
    )
  
  plot_list[[i]] <- p 
  
  # Add temperatures to dataframe for excel output later
  temps_rr_df <- temps_rr_df %>% 
    add_row(region = unique(data$regnames),
            opt_temp_range_low = reg_owr_low,
            opt_temp_range_high = reg_owr_high,
            risk_increase_temp = reg_risk_increase,
            high_risk_temp = reg_high_risk
    )
}

do.call(gridExtra::grid.arrange, plot_list)


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
  cat("Processing region:", names(df_list)[i])
  
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
  
  # Return heat attributable deaths for the output year
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for(timeperiod in allyears){
    
    #  Create empty matrix 
    matsim <- matrix(NA, length(names(df_list)), 7,
                     dimnames = list(names(df_list),
                                     c("all_heat","risk_increase",
                                       "high_risk",
                                       "moderate_heat", "high_heat",
                                       "heatwave_day", "heatwave"
                                     )))
    
    # Number of simulation runs for computing empirical CI
    nsim_ <- 1000
    
    # Create the array to store the CI of attributable deaths
    arraysim <- array(NA, dim = c(length(names(df_list)), 7, nsim_),
                      dimnames = list(names(df_list),
                                      c("all_heat_ci",
                                        "risk_increase_ci",
                                        "high_risk_ci",
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
      print(paste0("day in quarter: ", j))
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

# 13 - Aggregate attributable numbers ------------------------------------------

## Attributable number estimate by region:

#all_matsim

## Attributable number CI estimate by region:
#all_arraysim

## Attributable number estimate at Scotland level

scot_att <- all_matsim %>%
  group_by(year) %>%
  summarise_if(is.numeric, sum)%>%
  ungroup()

## Attributable number CI estimate at Scotland level

# Low CI
scot_att_low <- all_arraysim %>%
  filter(ci == "low") %>%
  group_by(year) %>%
  summarise_if(is.numeric, sum)%>%
  ungroup()

# High CI
scot_att_high <- all_arraysim%>%
  filter(ci == "high") %>%
  group_by(year) %>%
  summarise_if(is.numeric, sum)%>%
  ungroup()

scot_att_low_all <- scot_att_low%>%
  rename(all_heat_lci = all_heat_ci,
         risk_increase_lci = risk_increase_ci,
         high_risk_lci = high_risk_ci,
         moderate_heat_lci = moderate_heat_ci,
         high_heat_lci = high_heat_ci,
         heatwave_day_lci = heatwave_day_ci,
         heatwave_lci= heatwave_ci)

scot_att_high_all <- scot_att_high %>%
  rename(all_heat_uci = all_heat_ci,
         risk_increase_uci = risk_increase_ci,
         high_risk_uci = high_risk_ci,
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

(scot_weather_plot_mean <- ggplot(scot_weather, aes(x = year)) +
    # Mean temperature
    geom_point(aes(y = mean, colour = "Mean Temperature")) +
    geom_line(aes(y = mean, colour = "Mean Temperature")) +
    geom_smooth(aes(y = mean, colour = "Mean Temperature"), method = "lm") +
    
    # Max temperature
    geom_point(aes(y = max, colour = "Max Temperature")) +  
    geom_line(aes(y = max, colour = "Max Temperature")) +
    geom_smooth(aes(y = max, colour = "Max Temperature"), method = "lm") +  
    
    scale_y_continuous(expand = c(0, 0), limits = c(0, 35))+
    scale_x_continuous(breaks = seq(2005, 2024, by = 5))+
    ylab("Temperature (°C)")+
    xlab("Year")+
    theme_minimal()+
    theme(
      axis.line = element_line(color = "black")
    ) +
  
    # Manually setting colors for the legend
    viridis::scale_color_viridis(discrete=TRUE, option="viridis")+
    #  scale_color_manual(values = c("Mean Temperature" = "#3F3685", "Max Temperature" = "#E03616")) +
    
    # Legend title
    labs(colour = "Temperature Type") 
)

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

(hotdays <- ggplot(hot_days_long, aes(x = year, y = number_of_days, colour = threshold)) +
    # Mean temperature
    geom_point() +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_minimal()+
    viridis::scale_color_viridis(discrete=TRUE, option="viridis"))

# 14 - Attributable number (Scotland level) -----------------------------------

# Attributable to all heat 
(scot_att_plot_all_heat <- ggplot(scot_dat,aes(x = year, y = all_heat)) +
   geom_ribbon(aes(ymin = all_heat_lci,
                   ymax = all_heat_uci)) + 
   geom_point()+
   geom_line() +
   scale_x_continuous(breaks=2003:2023)+
   theme_minimal())

# Attributable to moderate heat
(scot_att_plot_mod_heat <- ggplot(scot_dat,aes(x = year, y = moderate_heat)) +
    geom_ribbon(aes(ymin = moderate_heat_lci,
                    ymax = moderate_heat_uci)) +
    geom_point()+
    geom_line() +
    scale_x_continuous(breaks=2003:2023)+
    theme_minimal())

# Attributable to high heat
(scot_att_plot_high_heat <- ggplot(scot_dat,aes(x = year, y = high_heat)) +
    geom_ribbon(aes(ymin = high_heat_lci,
                    ymax = high_heat_uci),
                fill = "#f9e38c") +
    geom_point()+
    geom_line(colour = "#3F3685") +
    scale_x_continuous(breaks=2003:2023)+
    ylab("Attributable deaths to high heat (over 97.5th percentile)")+
    theme_minimal())

# Attributable to high risk
(scot_att_plot_high_risk <- ggplot(scot_dat,aes(x = year, y = high_risk)) +
    geom_ribbon(aes(ymin = high_risk_lci,
                    ymax = high_risk_uci),
                fill = "#f9e38c") +
    geom_point()+
    geom_line(colour = "#D33F6A") +
    scale_x_continuous(breaks = seq(2003, 2023, by = 5))+
    ylab("High risk\n (over RR = 1.1)")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Attributable to increasing risk
(scot_att_plot_risk_increase <- ggplot(scot_dat,aes(x = year, y = risk_increase)) +
    geom_ribbon(aes(ymin = risk_increase_lci,
                    ymax = risk_increase_uci),
                fill = "#f9e38c") +
    geom_point()+
    geom_line(colour = "#E9B62D") +
    scale_x_continuous(breaks = seq(2003, 2023, by = 5))+
    ylab("Risk increase\n threshold")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Attributable over 25 degrees (heatwave day)
(scot_att_plot_heatwave_day<- ggplot(scot_dat,aes(x = year, y = heatwave_day)) +
    geom_ribbon(aes(ymin = heatwave_day_lci,
                    ymax = heatwave_day_uci),
                fill = "#f9e38c") +
    geom_point()+
    geom_line(colour = "#E57E41") +
    scale_x_continuous(breaks = seq(2003, 2023, by = 5))+
    ylab("Heatwave day\n threshold")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

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
  summarise(across(c(2:8),~sum(.)),
            pop_col = sum(pop_col))%>%
  ungroup()

antotlow <- anregionslow %>%
  left_join(pops, by = c("year","regnames"))%>%
  group_by(year)%>%
  summarise(across(c(2:8),~sum(.)),
            pop_col = sum(pop_col))%>%
  ungroup()

antothigh <- anregionshigh %>%
  left_join(pops, by = c("year","regnames"))%>%
  group_by(year)%>%
  summarise(across(c(2:8),~sum(.)),
            pop_col = sum(pop_col))%>%
  ungroup()

# Calculate attributable rate from attributable numbers:
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

(scot_att_rate_plot_high_heat <- ggplot(att_rate_highheat,aes(x = year, y = ar_high_heat)) +
    geom_ribbon(aes(ymin = low_ci,
                    ymax = high_ci),
                fill = "#f9e38c") +
    geom_point()+
    geom_line(colour = "#3F3685") +
    scale_x_continuous(breaks=2010:2023)+
    ylab("Attributable rate per 100,000 to high heat (over 97.5th percentile)")+
    theme_minimal())

# Attributable to high risk
att_rate_high_risk <- bind_cols(artot,artotlow$ar_high_risk_ci,
                                artothigh$ar_high_risk_ci) %>%
  rename(low_ci = "...17",
         high_ci = "...18")

(scot_att_rate_high_risk <- ggplot(att_rate_high_risk,aes(x = year, y = ar_high_risk)) +
    geom_ribbon(aes(ymin = low_ci,
                    ymax = high_ci),
                fill = "#f9e38c") +
    geom_point()+
    geom_line(colour = "#D33F6A") +
    scale_x_continuous(breaks = seq(2003, 2023, by = 5))+
    ylab("High risk\n (over RR = 1.1)")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Attributable to increasing risk
att_rate_risk_increase <- bind_cols(artot,artotlow$ar_risk_increase_ci,
                                    artothigh$ar_risk_increase_ci) %>%
  rename(low_ci = "...17",
         high_ci = "...18")

(scot_att_rate_risk_increase <- ggplot(att_rate_risk_increase,aes(x = year, y = ar_risk_increase)) +
    geom_ribbon(aes(ymin = low_ci,
                    ymax = high_ci),
                fill = "#f9e38c") +
    geom_point()+
    geom_line(colour = "#E9B62D") +
    scale_x_continuous(breaks = seq(2003, 2023, by = 5))+
    ylab("Risk increase\n threshold")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Attributable over 25 degrees (heatwave day)
att_rate_heatwave_day <- bind_cols(artot,artotlow$ar_heatwave_day_ci,
                                   artothigh$ar_heatwave_day_ci) %>%
  rename(low_ci = "...17",
         high_ci = "...18")

(scot_att_rate_heatwave_day<- ggplot(att_rate_heatwave_day,aes(x = year, y = ar_heatwave_day)) +
    geom_ribbon(aes(ymin = low_ci,
                    ymax = high_ci),
                fill = "#f9e38c") +
    geom_point()+
    geom_line(colour = "#E57E41") +
    scale_x_continuous(breaks = seq(2003, 2023, by = 5))+
    ylab("Rate per 100,000")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# 16 - Regional Attributable number ---------------------------------------

# Regional lower CI
att_low <- all_arraysim %>%
  filter(ci == "low")

att_low_all <- att_low%>%
  rename(all_heat_lci = all_heat_ci,
         risk_increase_lci = risk_increase_ci,
         high_risk_lci = high_risk_ci,
         moderate_heat_lci = moderate_heat_ci,
         high_heat_lci = high_heat_ci,
         heatwave_day_lci = heatwave_day_ci,
         heatwave_lci= heatwave_ci)

# Regional upper CI
att_high <- all_arraysim%>%
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
region_names<- names(df_list)

# Function to create a plot for each area
plot_AN_risk_inc <- function(region_names, y_limit, y_label) {
  dat %>%
    filter(regnames == region_names) %>%
    ggplot(aes(x = year, y = risk_increase)) +
    geom_ribbon(aes(ymin = risk_increase_lci, ymax = risk_increase_uci), fill = "#f9e79f") +
    geom_line(color = "#E9B62D") +
    geom_point(color = "#E9B62D") +
    scale_y_continuous(limits = c(0, y_limit)) +
    labs(title = region_names,
         x = "Year",
         y = y_label) +
    theme_minimal() +
    theme(plot.title = element_text(size = 8),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6))
}

if (event == "death") {
  risk_increase_AN_plots <- map(region_names, 
                                ~plot_AN_risk_inc(.x, 60, "Attributable deaths"))
} else if (event == "hospital admissions") {
  risk_increase_AN_plots <- map(region_names,
                                ~plot_AN_risk_inc(.x, 2000, "Attributable hospital admissions"))
}

# Generate plots for each area and store them in a list
gridExtra::grid.arrange(grobs = risk_increase_AN_plots, ncol = 3)


# Function to create a plot for each area
plot_AN_heatwave_day <- function(region_names, y_limit, y_label) {
  dat %>%
    filter(regnames == region_names) %>%
    ggplot(aes(x = year, y = heatwave_day)) +
    geom_ribbon(aes(ymin = heatwave_day_lci, ymax = heatwave_day_uci), fill = "#f0b27a") +
    geom_line(color =  "#E57E41") +
    geom_point(color =  "#E57E41") +
    labs(title = region_names,
         x = "Year",
         y = y_label) +
    theme_minimal()+
    scale_y_continuous(limits = c(0, y_limit)) +
    theme(plot.title = element_text(size = 8),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6))
}

if (event == "death") {
  heatwaveday_AN_plots <- map(region_names, 
                              ~plot_AN_heatwave_day(.x, 50, "Attributable deaths"))
} else if (event == "hospital admissions") {
  heatwaveday_AN_plots <- map(region_names,
                              ~plot_AN_heatwave_day(.x, 125, "Attributable hospital admissions"))
}

# Generate plots for each area and store them in a list
gridExtra::grid.arrange(grobs = heatwaveday_AN_plots, ncol = 3)

# Function to create a plot for each area
plot_AN_high_risk <- function(region_names, y_limit, y_label) {
  dat %>%
    filter(regnames == region_names) %>%
    ggplot(aes(x = year, y = high_risk)) +
    geom_ribbon(aes(ymin = high_risk_lci, ymax = high_risk_uci), fill = "#f0b27a") +
    geom_line(color =  "#E57E41") +
    geom_point(color =  "#E57E41") +
    labs(title = region_names,
         x = "Year",
         y = y_label) +
    theme_minimal()+
    scale_y_continuous(limits = c(0,y_limit))+
    theme(plot.title = element_text(size = 8),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6))
}

if (event == "death") {
  high_risk_AN_plots <- map(region_names, 
                            ~plot_AN_high_risk(.x, 50, "Attributable deaths"))
} else if (event == "hospital admissions") {
  high_risk_AN_plots <- map(region_names,
                            ~plot_AN_high_risk(.x, 220, "Attributable hospital admissions"))
}
# Generate plots for each area and store them in a list
gridExtra::grid.arrange(grobs = high_risk_AN_plots, ncol = 3)


# 17 - Regional Attributable Rate  ----------------------------------------

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
         ar_heatwave_day_lci = ar_heatwave_day_ci,
         ar_heatwave_lci= ar_heatwave_ci)

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
         ar_heatwave_day_uci = ar_heatwave_day_ci,
         ar_heatwave_uci= ar_heatwave_ci)

ARdat <- arregions %>%
  left_join(arregionslow, by = c("year", "regnames", "pop_col"))%>%
  left_join(arregionshigh, by = c("year", "regnames", "pop_col"))

# Get unique areas
region_names<- names(df_list)

# Function to create a plot for each area
plot_AR_risk_inc <- function(region_names, y_limit, y_label) {
  ARdat %>%
    filter(regnames == region_names) %>%
    ggplot(aes(x = year, y = ar_risk_increase)) +
    geom_ribbon(aes(ymin = ar_risk_increase_lci, ymax = ar_risk_increase_uci), fill = "#f9e79f") +
    geom_line(color = "#E9B62D") +
    geom_point(color = "#E9B62D") +
    scale_y_continuous(limits = c(0, y_limit)) +
    labs(title = region_names,
         x = "Year",
         y = y_label) +
    theme_minimal() +
    theme(plot.title = element_text(size = 8),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6))
}

if (event == "death") {
  risk_increase_AR_plots <- map(region_names, 
                                ~plot_AR_risk_inc(.x, 10, "Attributable deaths"))
} else if (event == "hospital admissions") {
  risk_increase_AR_plots <- map(region_names,
                                ~plot_AR_risk_inc(.x, 200, "Attributable hospital admissions"))
}

# Generate plots for each area and store them in a list
gridExtra::grid.arrange(grobs = risk_increase_AR_plots, ncol = 3)


# Function to create a plot for each area
plot_AR_heatwave_day <- function(region_names, y_limit, y_label) {
  ARdat %>%
    filter(regnames == region_names) %>%
    ggplot(aes(x = year, y = ar_heatwave_day)) +
    geom_ribbon(aes(ymin = ar_heatwave_day_lci, ymax = ar_heatwave_day_uci), fill = "#f0b27a") +
    geom_line(color =  "#E57E41") +
    geom_point(color =  "#E57E41") +
    labs(title = region_names,
         x = "Year",
         y = y_label) +
    theme_minimal()+
    scale_y_continuous(limits = c(0, y_limit)) +
    theme(plot.title = element_text(size = 8),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6))
}

if (event == "death") {
  heatwaveday_AR_plots <- map(region_names, 
                              ~plot_AR_heatwave_day(.x, 5, "Attributable deaths"))
} else if (event == "hospital admissions") {
  heatwaveday_AR_plots <- map(region_names,
                              ~plot_AR_heatwave_day(.x, 200, "Attributable hospital admissions"))
}

# Generate plots for each area and store them in a list
gridExtra::grid.arrange(grobs = heatwaveday_AR_plots, ncol = 3)

# Function to create a plot for each area
plot_AR_high_risk <- function(region_names, y_limit, y_label) {
  ARdat %>%
    filter(regnames == region_names) %>%
    ggplot(aes(x = year, y = ar_high_risk)) +
    geom_ribbon(aes(ymin = ar_high_risk_lci, ymax = ar_high_risk_uci), fill = "#f0b27a") +
    geom_line(color =  "#E57E41") +
    geom_point(color =  "#E57E41") +
    labs(title = region_names,
         x = "Year",
         y = y_label) +
    theme_minimal()+
    scale_y_continuous(limits = c(0,y_limit))+
    theme(plot.title = element_text(size = 8),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6))
}

if (event == "death") {
  high_risk_AR_plots <- map(region_names, 
                            ~plot_AR_high_risk(.x, 10, "Attributable deaths"))
} else if (event == "hospital admissions") {
  high_risk_AR_plots <- map(region_names,
                            ~plot_AR_high_risk(.x, 20, "Attributable hospital admissions"))
}
# Generate plots for each area and store them in a list
gridExtra::grid.arrange(grobs = high_risk_AR_plots, ncol = 3)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# END
