#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dlnm_code_knots.R
# Feb 25
# Bella Tortora Brayda & Sarah Reed
# 
# Adapted from Scotland - heat related mortality.R - converted code into a versatile
# Distributed Lag Non-linear Model script for use on other indicators. 
# Paired with the dlnm_setup.R script where arguments and data are updated for this
# script. 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1 - Source files --------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# source config script:
source(here::here("dlnm/dlnm_setup.R"))

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
  dplyr::rename(dependent = dependent_col,
                date = time_col,
                regnames = region_col,
                weather = weather_col,
                humidity = humidity_col,
                pop_col = population_col) %>%
  dplyr::mutate(date = as.Date(date,format = "%d/%m/%y"),
                year = as.numeric(year),
                month = month(date),
                covid = ifelse(year %in% c(2020,2021),1,2))%>%
  dplyr::filter(year < 2024)

scotmed <- median(df$weather)
scotmean<- mean(df$weather)

if(summer == TRUE){
  df<- dplyr::filter(df, month %in% month_choice)
}

df <- df %>% 
  mutate(dependent = ifelse(is.na(dependent), 0, dependent))

# Sort into regions

regions <- sort(as.character(unique(df$regnames)))

# Breaks full datasets into a list of data by region
df_list <- lapply(regions,
                  function(x)
                    df %>%
                    dplyr::filter(regnames == x))

# Names each element in the list with the corresponding region name for use in 
# loop in model function (section 4)
names(df_list) <- regions
dataset = df

# 3 - Define model function (used a few times in other functions) ---------
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
                         lagnk,
                         dfseas) {
  
  # These are the crossbasis and seasonality components
  dataset <- dataset %>% mutate(date_numeric = as.numeric(date))
  
  independent_cols <- c(independent_col1, independent_col2, independent_col3, independent_col4, 
                        'cb_temp', 'cb_hum', # cross bases for temp and humidity
                        'ns(date, df = dfseas * length(unique(year)))')
  
  independent_cols <- independent_cols[independent_cols != "NONE"]
  
  if(covid == TRUE){
    # Model formula
    formula <- as.formula(paste(paste('dependent'), 
                                " ~ ",
                                paste(independent_cols, 
                                      collapse = " + "), 
                                "+ (1|covid)"
                                  ))
                              
  }else{
    # Model formula
    formula <- as.formula(paste(paste('dependent'), 
                                " ~ ",
                                paste(independent_cols, 
                                      collapse = " + ")))
  }
  
  # Variables for the crossbases: Use EITHER knots OR df
  argvar <- list(fun = varfun,
               # df = vardegree)
                 knots = quantile(dataset$weather, varper / 100, na.rm = TRUE))

  arglag <- list(fun = varfun, 
               # df = lagdf)#,
                 knots = logknots(lag, lagnk))
  
  
  # ensure these are numeric
  lag <- as.numeric(lag)
  lagnk <- as.numeric(lagnk)
# lagdf <- as.numeric(lagdf)
  dfseas <- as.numeric(dfseas)
  
  # Define the temp and humidity crossbases

  cb_temp <- crossbasis(dataset$weather,
                   lag = lag,
                   argvar = argvar,
                   arglag = arglag)
  
  cb_hum <- crossbasis(dataset$humidity,
                        lag = lag,
                        argvar = argvar,
                        arglag = arglag)
  
  model <- glm(formula,
               dataset,
               family = quasipoisson,
               na.action = "na.exclude")
  
  return (list(model, cb_temp, cb_hum))

}

# 5 - Run_model function - extracted --------------------------------------


# Define and run Quasi-poisson regression model for each dataframe
# Adapted from: run_model
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create two logical objects (minperregions and minweatherregions) each repeating NA for 
# the length of the HB list (df_list)
minperregions <- minweatherregions <- rep(NA,
                                       length(df_list))

# Coefficients and vcov for overall cumulative summary
# matrix for coef_ that is the length of the number of regions, and the width
# is equal to degrees of freedom (vardegree)
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
                                 lagnk = lagnk,
                                 dfseas = dfseas)
  
  cen <- quantile(data$weather, na.rm = TRUE, percentile)
  
  # Reduction to overall cumulative
  pred <- crossreduce(cb_temp, model, cen = cen)
  minweatherregions[i] <- as.numeric(names(which.min(pred$RRfit)))
  
  coef_[i,] <- coef(pred)
  vcov_[[i]] <- vcov(pred)

}

# 6 - Run_meta_model - Extracted (only used once) ---------------------------------------

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


# Locations (east or west)
# Define the regions corresponding to East
east_regions <- c("NHS Grampian", "NHS Tayside", "NHS Fife", "NHS Lothian", "NHS Borders")


# Create a vector to store the location assignment for each dataframe
locs <- sapply(names(df_list), function(df) {
  if (df %in% east_regions) {
    return("East")
  } else {
    return("West")
  }
})

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

########################################################
# SUMMARY FROM META ANALYSIS
mvsummary <- summary(mv)

### MODEL VALIDATION ###
#plot residuals for checking:


mv_boxplot <- boxplot(mv$residuals)

acf(resid(mv))

# Use the Augmented Dickey-Fuller (ADF) test to check if the residuals are stationary 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

class(mv$residuals)  # Check the class - MATRIX but need vector
residuals_vector <- as.vector(mv$residuals[, 1]) # Extract the first column 
adf.test(residuals_vector)
# If the p-value is high (>0.05), the residuals are non-stationary. 

# Testing if the autocorrelation residuals are ok using Ljung-Box test 
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


########################################################

# FUNCTION FOR COMPUTING THE P-VALUE OF A WALD TEST
fwald <- function(model,var) {
  ind <- grep(var,names(coef(model)))
  coef <- coef(model)[ind]
  vcov <- vcov(model)[ind,ind]
  waldstat <- coef%*%solve(vcov)%*%coef
  df <- length(coef)
  return(1-pchisq(waldstat,df))
}

# TEST THE EFFECTS
if("avgweather" %in% metapreds){
  avgweather_fwald <- fwald(mv, "avgweather")
}

if("rangeweather" %in% metapreds){
rangeweather_fwald <- fwald(mv,"rangeweather")
}

if("maxweather" %in% metapreds){
maxweather_fwald <- fwald(mv,"maxweather")
}

################################################################################
# Obtain best linear unbiased prediction 

blup <- blup(mv,vcov=T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9) Meta model for Scotland level RR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# META MODEL FOR ALL OF SCOTLAND:

# Exclude extreme weathers
# predvar <- quantile(data$weather, 1:99/100, na.rm=T)

# All weathers
predvar_scot <- data$weather

argvar_scot <- list(x = predvar_scot,
                    fun = varfun,
                   #df = vardegree)
                   
                    knots = quantile(data$weather,
                                   varper / 100, na.rm = TRUE),
                    Bound = range(data$weather, na.rm = TRUE))

bvar_scot <- do.call(onebasis, argvar_scot)

#model <- NULL
cen_scot <- median(minweatherregions) # 
# TRY BOTH
#censcot <- median(minweatherregions_)

pred_scot <- crosspred(bvar_scot,
                       coef = coef(mv),
                       vcov = vcov(mv),
                       model.link = "log",
                       by = 0.1,
                       cen = cen_scot)


# 10) Create Scotland RR plot ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pdf(paste(paste0(climate_data_folder, indicator_output_path),
          scotRRplot,
          ".pdf",
          sep = ''),
    width = 8, height = 9)

# Set up for plot using base R
par(mar = c(4, 3.8, 3, 2.4), mgp = c(2.5, 1, 0), las = 1)

layout(matrix(1:1, ncol = 1))

region_vector_scot <- c()
weather_vector_scot <- c()
relative_risk_vector_scot <- c()
upper_vector_scot <- c()
lower_vector_scot <- c()
cen_vector_scot <- c()

weather_vector_scot <- c()
weather_region_vector_scot <- c()

# Plot code
plot(pred_scot,
     # type = "n",
     ylab = "RR",
     ylim = c(.0, 2),
     xlim = c(5, 30),
     xlab=expression(paste("Temperature (", degree, "C)")),
     main = dependent_col)

abline(h = 1)

# Create new informative lines for plot:

# line for the middle point where the RR is 1
lowestrisk <- as.numeric(names(
  which(pred_scot$allRRfit == 1)))

lowestrisk_min <- as.numeric(names(
  which.min(pred_scot$allRRfit < 1.0001 & pred_scot$allRRfit > 0.99999)))

lowestrisk_max <- as.numeric(names(
  which.max(pred_scot$allRRfit < 1.0001 & pred_scot$allRRfit > 0.99999)))

# Line for the 97.5th percentile (the threshold for AN in the high heat chart)
high_heat_line <- quantile(data$weather, 97.5/100, na.rm = TRUE)

# line for heatwave day (i.e. 25 degrees celsius)
heatwaveday <- 25

# The point at which the RR estimate crosses 1.1 -i.e high risk
high_risk <- as.numeric(names(
  which.max(which(pred_scot$allRRfit >= 1 & pred_scot$allRRfit <= 1.1) )))

# the point at which the lower confidence interval for RR is above 1.005: i.e. the
# point at which we can say the risk of mortality starts to increase
risk_increase <- as.numeric(names(
  which.max(which(pred_scot$allRRlow < 1.006))))

segment_a <- pred_scot$predvar <= risk_increase
segment_b <- pred_scot$predvar >= risk_increase & pred_scot$predvar <= high_heat_line
segment_c <- pred_scot$predvar >= high_heat_line & pred_scot$predvar <= high_risk
segment_d <- pred_scot$predvar >= high_risk

relative_risk_vals_scot <- pred_scot$allRRfit

lines(pred_scot$predvar,
      pred_scot$allRRfit,
      col = 'black',
      lwd = 1)

lines(pred_scot$predvar[segment_a],
      pred_scot$allRRfit[segment_a],
      col = c("black"),
      lwd = 1.5)

lines(pred_scot$predvar[segment_b],
      pred_scot$allRRfit[segment_b],
      col = c("#FFA7A7"),
      lwd = 1.5)

lines(pred_scot$predvar[segment_c],
      pred_scot$allRRfit[segment_c],
      col = c("#FF0000"),
      lwd = 1.5)
lines(pred_scot$predvar[segment_d],
      pred_scot$allRRfit[segment_d],
      col = c("#8B0000"),
      lwd = 1.5)

axis(2, at = 1:5 * 0.5)

breaks_scot <- c(min(data$weather, na.rm = TRUE) - 1,
                 seq(pred_scot$predvar[1],
                     pred_scot$predvar[length(pred_scot$predvar)],
                     length = 30),
                 max(data$weather, na.rm = TRUE) + 1)

hist_scot <- hist(data$weather, breaks = breaks_scot, plot = FALSE)
hist_scot$density <- hist_scot$density / max(hist_scot$density) * 0.7
prop_scot <- max(hist_scot$density) / max(hist_scot$counts)
counts_scot <- pretty(hist_scot$count, 3)

plot(hist_scot,
     ylim = c(0, max(hist_scot$density) * 3.5),
     axes = FALSE, ann = FALSE, col = grey(0.95),
     breaks = breaks_scot, freq = FALSE, add = TRUE)

axis(4, at = counts_scot * prop_scot, labels = counts_scot, cex.axis = 0.7)
mtext("N", 4, line = -0.5, at = mean(counts_scot * prop_scot), cex = 0.5)

# Insert vertical lines for reference

abline(v = lowestrisk_max, lty = 1, col = "#E2E6BD")
abline(v = risk_increase, lty = 1, col = "#E9B62D")
#abline(v = high_heat_line, lty = 2, col = "#E99A2C")
abline(v = heatwaveday, lty = 2, col = "#E57E41")
abline(v = high_risk, lty = 1, col = "#D33F6A" )

hcl.colors(5, palette = "Heat 2")

#op <- par(pt.cex = 0.5)
legend(5, 1.75, legend=c("Lowest risk", "Risk increases","Heatwave day (25 degrees)", "High risk"),
       col=c(3, "orange", "red","dark red"), lty=c(1,1,2,1), cex = 0.4)

relative_risk_vector_scot <- append(relative_risk_vector_scot,
                                    pred_scot$allRRfit)

upper_vector_scot <- append(upper_vector_scot,
                            pred_scot$allRRhigh)

lower_vector_scot <- append(lower_vector_scot,
                            pred_scot$allRRlow)

region_vector_scot <-
  append(region_vector_scot,
         rep('Scotland',
             length(pred_scot$predvar)))

weather_vector_scot <- append(weather_vector_scot,
                              pred_scot$predvar)

cen_vector_scot <- append(cen_vector_scot,
                          rep(cen_scot,
                              length(pred_scot$predvar)))

weather_vector_scot <- append(weather_vector_scot,
                              data$weather)

weather_region_vector_scot <-
  append(weather_region_vector_scot,
         rep('Scotland',
             length(data$weather)))

dev.off()

#~~~~~~~~~~~~~~~~~~~~~
#GGPLOT VERSION TRIAL 
# Create a data frame for plotting
plot_data <- data.frame(
  x = pred_scot$predvar,          # Predictor variable (e.g., temperature, exposure)
  rr = pred_scot$allRRfit,        # Estimated relative risk
  lower = pred_scot$allRRlow,     # Lower confidence interval
  upper = pred_scot$allRRhigh     # Upper confidence interval
)

hist_data <- data.frame(x = pred_scot$predvar)

# Generate the relative risk curve using ggplot2
ggplot(plot_data, aes(x = x, y = rr)) +
  geom_line(color = "blue")+
              geom_ribbon(aes(ymin = lower, ymax = upper), fill = "blue", alpha = 0.3) +  # Confidence interval
              theme_minimal() +
              labs(
                title = "Relative Risk Curve",
                x = "Exposure Variable",
                y = "Relative Risk"
              ) +
              theme(
                plot.title = element_text(hjust = 0.5),
                text = element_text(size = 14)
              ) +
              geom_hline(yintercept = 1, linetype = "dashed", color = "red")  # Reference line at RR=1


################################################################################
# RE-CENTERING - to optimal weather range and calculating OPTIMAL weather
# RANGE (unfortunately with unreliable estimates, the OWR can't be accurately 
# calculated, but leaving this code here for reference)

# GENERATE THE MATRIX FOR STORING THE RESULTS
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

# DEFINE MINIMUM MORTALITY VALUES: 

for(i in seq(length(df_list))) {
  
  data <- df_list[[i]]
  predvar <- quantile(data$weather, 1:99 / 100, na.rm = TRUE)
  
  # Redefine the function using all arguments (boundary knots included)
  argvar_ <- list(x = predvar, fun = varfun,
                # df = vardegree)
                  knots = quantile(data$weather, varper / 100, na.rm = TRUE),

                  Bound = range(data$weather, na.rm = TRUE))
  
  bvar_ <- do.call(onebasis, argvar_)
  
  # minpercregions_[i] <- percentile *100
  
  minpercregions_[i] <- (1:99)[which.min(bvar_ %*%
                                           blup[[i]]$blup)]
  minweatherregions_[i] <- quantile(data$weather,
                                 minpercregions_[i] / 100,
                                 na.rm = TRUE)
  
  ### Next section used for regional crosspred to calculate optimal weather range.
  ### 
  ### Try excluding 0 death days for these??
  # OVERALL CUMULATIVE SUMMARY ASSOCIATION FOR MAIN MODEL
  cp <- crosspred(bvar_,
                  coef = blup[[i]]$blup,
                  vcov = blup[[i]]$vcov,
                  cen = minweatherregions_[i],
                  model.link = "log",
                  by = 0.1,
                  from = ranges[i,1],
                  to = ranges[i,2])
  
  optimal_weather_range[i,"lower"] <- as.numeric(names(
    which.min(which(cp$allRRfit >= 1 & cp$allRRfit <= 1.1))))
  optimal_weather_range[i, "upper"] <- as.numeric(names(
    which.max(which(cp$allRRfit >= 1 & cp$allRRfit <= 1.1))))
  
  below_one <- which(cp$allRRfit < 1)
  above_OWR <- which(as.numeric(names(cp$allRRfit)) > optimal_weather_range[i, "upper"])
  below_OWR <- which(as.numeric(names(cp$allRRfit))< optimal_weather_range[i, "lower"])

}


# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Create Regions RR plot ----
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  pdf(paste(climate_data_folder, indicator_output_path,
            regionsRRplot,
            ".pdf",
            sep = ''),
      width = 8, height = 9)

layout(matrix(c(0, 1, 1, 2, 2, 0,
                rep(3:8, each = 2), 0, 9, 9, 10, 10, 0),
              ncol = 6,
              byrow = T))

par(mar=c(4, 3.8, 3, 2.4), mgp = c(2.5, 1, 0), las = 1)

xlab <- expression(paste("Temperature (",degree,"C)"))

region_vector <- c()
weather_vector <- c()
relative_risk_vector <- c()
cen_vector <- c()
upper_vector <- c()
lower_vector <- c()

weather_vector <- c()
weather_region_vector <- c()

no_of_regions <- seq(length(df_list))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plots by region ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ### TRIALLING CENTRING TO THE MINweatherREGION FOR SCOTLAND (i.e 3.5) - this seems
# ### to produce reasonable graphs

for(i in no_of_regions) {

  data <- df_list[[i]]

  # NB: Centering point different than original choice of 75th
  argvar <- list(x = data$weather,
                 fun = varfun,
                # df = vardegree)
                 knots = quantile(data$weather,
                                varper / 100, na.rm = TRUE))


  bvar <- do.call(onebasis, argvar)

  coefs <- blup[[i]]$blup
  vcovs <- blup[[i]]$vcov
  model <- NULL
  cen <- cen_scot
  #cen <- minweatherregions[i]
  #cen <-  quantile(data$weather, na.rm = TRUE, 0.75)

  pred <- crosspred(bvar,
                    coef = blup[[i]]$blup,
                    vcov = blup[[i]]$vcov,
                    model.link = "log",
                    by = 0.1,
                    cen = cen)

  plot(pred, type = "n",
       ylim = c(0, 3),
       yaxt = "n",
       lab = c(6, 5, 7),
       xlab = xlab,
       ylab = "RR",
       main = names(df_list)[i])
  
  
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
    which.max(which(pred$allRRlow < 1.0001))))
  
  reg_segment_a <- pred$predvar <= reg_risk_increase
  reg_segment_b <- pred$predvar >= reg_risk_increase & pred$predvar <= high_heat_line
  reg_segment_c <- pred$predvar >= high_heat_line & pred$predvar <= reg_high_risk
  reg_segment_d <- pred$predvar >= reg_high_risk
  
  relative_risk_vals<- pred$allRRfit
  
  lines(pred$predvar,
        pred$allRRfit,
        col = 'black',
        lwd = 1)
  
  lines(pred$predvar[reg_segment_a],
        pred$allRRfit[reg_segment_a],
        col = c("black"),
        lwd = 1.5)
  
  lines(pred$predvar[reg_segment_b],
        pred$allRRfit[reg_segment_b],
        col = c("#FFA7A7"),
        lwd = 1.5)
  
  lines(pred$predvar[reg_segment_c],
        pred$allRRfit[reg_segment_c],
        col = c("#FF0000"),
        lwd = 1.5)
  lines(pred$predvar[reg_segment_d],
        pred$allRRfit[reg_segment_d],
        col = c("#8B0000"),
        lwd = 1.5)
  
  relative_risk_vals <- pred$allRRfit
  
  axis(2, at = 1:5 * 0.5)

  breaks <- c(min(data$weather, na.rm = TRUE) - 1,
              seq(pred$predvar[1],
                  pred$predvar[length(pred$predvar)],
                  length = 30),
              max(data$weather, na.rm = TRUE) + 1)

  hist <- hist(data$weather, breaks = breaks, plot = FALSE)
  hist$density <- hist$density / max(hist$density) * 0.7
  prop <- max(hist$density) / max(hist$counts)
  counts <- pretty(hist$count, 3)

  plot(hist,
       ylim = c(0, max(hist$density) * 3.5),
       axes = FALSE, ann = FALSE, col = grey(0.95),
       breaks = breaks, freq = FALSE, add = TRUE)

  axis(4, at = counts * prop, labels = counts, cex.axis = 0.7)
  mtext("N", 4, line = -0.5, at = mean(counts * prop), cex = 0.5)

  abline(v = reg_lowestrisk_max, lty = 1, col = "#E2E6BD")
  abline(v = reg_risk_increase, lty = 1, col = "#E9B62D")
  #abline(v = high_heat_line, lty = 2, col = "#E99A2C")
  abline(v = heatwaveday, lty = 2, col = "#E57E41")
  abline(v = reg_high_risk, lty = 1, col = "#D33F6A" )

  relative_risk_vector <- append(relative_risk_vector,
                                 pred$allRRfit)

  region_vector <-
    append(region_vector,
           rep(names(df_list)[i],
               length(pred$predvar)))

  weather_vector <- append(weather_vector,
                        pred$predvar)

  cen_vector <- append(cen_vector,
                       rep(cen,
                           length(pred$predvar)))

  weather_vector <- append(weather_vector,
                               data$weather)

  weather_region_vector <-
    append(weather_region_vector,
           rep(names(df_list)[i],
               length(data$weather)))


  upper_vector <- append(upper_vector,
                         pred$allRRhigh)


  lower_vector <- append(lower_vector,
                         pred$allRRlow)
}

dev.off()
## THRESHOLDS FOR ATTRIBUTION ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract the 2.5th and 97.5th percentile for all regions
per <- t(sapply(df_list, function(x)
  quantile(x$weather, c(2.5,10,90, 97.5)/100, na.rm = T)))

quantile(df$weather,.975)

# data frame with final thresholds to use for hot and cold days for deaths
# NB: adding in moderate cold and heat thresholds as .25 and .75 respectively
#     as OWR's don't make sense
an_thresholds <- as.data.frame(cbind(per,optimal_weather_range)) %>%
  dplyr::mutate(
    max_high_heat = 100,
    moderate_cold_OWR = lower,
    low_risk = lowestrisk_max, #using scotland level mmt/risk increase for consistency
    risk_increase = risk_increase,
    high_risk = high_risk,
    moderate_heat_OWR = upper,
    moderate_heat_90 = `90%`,
    high_moderate_heatOWR = ifelse(moderate_heat_OWR > `97.5%`,
                                   moderate_heat_OWR,
                                   `97.5%`),
    heatwave_day = 25,
    #high_moderate_cold2.5 = `2.5%`,
    high_moderate_heat97.5 = `97.5%`
  )

OWR_thresholds <- an_thresholds %>%
  dplyr::select(moderate_heat_OWR, high_moderate_heatOWR)

perc_thresholds <- an_thresholds %>%
  dplyr::select(moderate_heat_90, high_moderate_heat97.5)

# SCOTLAND LEVEL POINTS OF MINIMUM MORTALITY

(minperccountry <- median(reg_lowestrisk_max))
#(minperccountry <- median(minweatherregions_))
#(minperccountry <- MMT)

# 6) Loop for attributable deaths -----------------------------------------
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
  cat(i, "")
  
  # Extract the data
  data <- df_list[[i]]
  
  coefs <- blup[[i]]$blup
  vcovs <- blup[[i]]$vcov
  
  c(model, cb_temp, cb_hum)  %<-% define_model(dataset = data,
                                               independent_col1 = independent_col1,
                                               independent_col2 = independent_col2,
                                               independent_col3 = independent_col3,
                                               independent_col4 = independent_col4,
                                               varfun = varfun,
                                               varper = varper,
                                               vardegree = vardegree,
                                               lag = lag,
                                               lagnk = lagnk,
                                               dfseas = dfseas)
  
  
  #Why does this need to be NULL before the attrdl is calculated?
  model <- NULL
  
  #############################################
  # Return heat attributable deaths for the output year
  # ### THIS IS WHERE TO ADJUST FOR MONTHLY AND ANNUAL ATTRIBUTION? ----
  
  for(timeperiod in allyears){
    
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
    
    
    data_output_year <- data %>% dplyr::filter(year == timeperiod) %>%
      dplyr::mutate(high_heat_flag = ifelse(weather > 25,1, 0))
    
    # Prepare weather column for attribution to heatwaves
    # 
    # I think that this next code accounts a heatwave as 2 days at 25 degrees or higher
    # Force the temperature to be the centering value for non-heatwave days
    data_output_year$heatwave_flag <- NA
    for (j in seq(nrow(data_output_year))){
      
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
    
    ## Add another loop for each year of the data?
    
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
    
    # CONFIDENCE INTERVALS FOR ESTIMATES ----
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
  
  # matsim <- matsim %>%
  #   mutate(year = year)
  # 
  # arraysim <- arraysim %>%
  #   mutate(year = year)
  
  # all_matsim <- rbind(matsim,all_matsim) # All_matsim only generated one set, not annual. Will need to revisit next week. 
  # all_arraysim <- rbind(arraysim,all_arraysim) ## Arraysim didn't quite work - 1 obs of 98000 variables!!
  
}

# 7) compute attributable rates ------------------------------------------

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

scot_att_low <- all_arraysim %>%
  filter(ci == "low") %>%
  group_by(year) %>%
  summarise_if(is.numeric, sum)%>%
  ungroup()

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

## Scotland plot - all heat (75% plus)

(scot_weather_plot <- ggplot(scot_weather,aes(x = year, y = mean))+
    geom_ribbon(aes(ymin = min,
                    ymax = max, alpha = 0.2), fill = "#80BCEA") +
    geom_point(colour = "#3F3685")+
    geom_line(colour = "#3F3685") +
    #scale_x_continuous(breaks=2010:2023)+
    theme_minimal())

(scot_att_plot_all_heat <- ggplot(scot_dat,aes(x = year, y = all_heat)) +
    geom_point()+
    geom_line() +
    geom_ribbon(aes(ymin = all_heat_lci,
                    ymax = all_heat_uci, alpha = 0.2)) +
    scale_x_continuous(breaks=2003:2023)+
    theme_minimal())

(scot_att_plot_mod_heat <- ggplot(scot_dat,aes(x = year, y = moderate_heat)) +
    geom_point()+
    geom_line() +
    geom_ribbon(aes(ymin = moderate_heat_lci,
                    ymax = moderate_heat_uci, alpha = 0.2)) +
    scale_x_continuous(breaks=2003:2023)+
    theme_minimal())

(scot_att_plot_high_heat <- ggplot(scot_dat,aes(x = year, y = high_heat)) +
    geom_point()+
    geom_line(colour = "#3F3685") +
    geom_ribbon(aes(ymin = high_heat_lci,
                    ymax = high_heat_uci), alpha = 0.2,
                fill = "#80BCEA") +
    scale_x_continuous(breaks=2003:2023)+
    ylab("Attributable deaths to high heat (over 97.5th percentile)")+
    theme_minimal())

(scot_att_plot_high_risk <- ggplot(scot_dat,aes(x = year, y = high_risk)) +
    geom_point()+
    geom_line(colour = "#D33F6A") +
    geom_ribbon(aes(ymin = high_risk_lci,
                    ymax = high_risk_uci), alpha = 0.2,
                fill = "#E2E6BD") +
    scale_x_continuous(breaks = seq(2003, 2023, by = 5))+
    ylab("High risk\n (over RR = 1.1)")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(scot_att_plot_risk_increase <- ggplot(scot_dat,aes(x = year, y = risk_increase)) +
    geom_point()+
    geom_line(colour = "#E9B62D") +
    geom_ribbon(aes(ymin = risk_increase_lci,
                    ymax = risk_increase_uci), alpha = 0.2,
                fill = "#E2E6BD") +
    scale_x_continuous(breaks = seq(2003, 2023, by = 5))+
    ylab("Risk increase\n threshold")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

(scot_att_plot_heatwave_day<- ggplot(scot_dat,aes(x = year, y = heatwave_day)) +
    geom_point()+
    geom_line(colour = "#E57E41") +
    geom_ribbon(aes(ymin = heatwave_day_lci,
                    ymax = heatwave_day_uci), alpha = 0.2,
                fill = "#E2E6BD") +
    scale_x_continuous(breaks = seq(2003, 2023, by = 5))+
    ylab("Heatwave day\n threshold")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)))

# Regions-specific
anregions <- all_matsim
anregionslow <- all_arraysim %>%
  filter(ci == "low")
anregionshigh <- all_arraysim %>%
  filter(ci == "high")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 8) Summary for attributable numbers and rates --------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#match populations to anregions data
pops <- dataset %>%
  dplyr::select(regnames,pop_col,year)%>%
  distinct()

arregions <- anregions %>%
  left_join(pops, by = c("year","regnames")) %>%
  mutate(ar = across(c(2:8),~./pop_col *100000))

arregionslow <- anregionslow %>%
  left_join(pops, by = c("year","regnames")) %>%
  mutate(ar = across(c(2:8),~./pop_col *100000))

arregionshigh <- anregionshigh %>%
  left_join(pops, by = c("year","regnames")) %>%
  mutate(ar = across(c(2:8),~./pop_col *100000))

# Total AR

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

artot <- antot %>%
  mutate(ar = across(c(2:8),~./pop_col *100000))

artotlow <- antotlow %>%
  mutate(ar = across(c(2:8),~./pop_col *100000))

artothigh <- antothigh %>%
  mutate(ar = across(c(2:8),~./pop_col *100000))

att_rate <- bind_cols(artot,artotlow$ar$high_heat_ci,artothigh$ar$high_heat_ci)%>%
  rename(low_ci = "...11",
         high_ci = "...12")

(scot_att_rate_plot_high_heat <- ggplot(att_rate,aes(x = year, y = ar$high_heat)) +
    geom_point()+
    geom_line(colour = "#3F3685") +
    geom_ribbon(aes(ymin = low_ci,
                    ymax = high_ci), alpha = 0.2,
                fill = "#80BCEA") +
    scale_x_continuous(breaks=2010:2023)+
    ylab("Attributable rate per 100,000 to high heat (over 97.5th percentile)")+
    theme_minimal())

###################################################
# NEED TO TIDY THESE DATAFRAMES FOR CLEARER TABLES
# ###################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9) Meta model for Scotland level RR
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# META MODEL FOR ALL OF SCOTLAND:

data <- do.call(rbind, df_list)

# Estimation method
method <- "reml"

# Overall cumulative summary for main model
mvall <- mixmeta(coef ~ 1, vcov, method = method)

mvallsummary <- summary(mvall)



# Exclude extreme weathers
# predvar <- quantile(data$weather, 1:99/100, na.rm=T)

# All weathers
predvar_scot <- data$weather

argvar_scot <- list(x = predvar_scot,
                    fun = varfun,
                #  df = vardegree)
                    knots = quantile(data$weather, varper / 100, na.rm = TRUE),
                    Bound = range(data$weather, na.rm = TRUE))

bvar_scot <- do.call(onebasis, argvar_scot)

#model <- NULL
cen_scot <- median(minweatherregions) # currently median min weather is 3.75 where centred data (minweatherregions_) is at 75th percentile. 
# TRY BOTH
#censcot <- median(minweatherregions_)

pred_scot <- crosspred(bvar_scot,
                       coef = coef(mvall),
                       vcov = vcov(mvall),
                       model.link = "log",
                       by = 0.1,
                       cen = cen_scot)


# 10) Create Scotland RR plot ----
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pdf(paste(paste0(climate_data_folder, indicator_output_path),
          scotRRplot,
          ".pdf",
          sep = ''),
    width = 8, height = 9)

# Set up for plot using base R
par(mar = c(4, 3.8, 3, 2.4), mgp = c(2.5, 1, 0), las = 1)

layout(matrix(1:1, ncol = 1))

region_vector_scot <- c()
weather_vector_scot <- c()
relative_risk_vector_scot <- c()
upper_vector_scot <- c()
lower_vector_scot <- c()
cen_vector_scot <- c()

weather_vector_scot <- c()
weather_region_vector_scot <- c()

# Plot code
plot(pred_scot,
     type = "n",
     ylab = "RR",
     ylim = c(.0, 3),
     xlim = c(-8, 30),
     xlab=expression(paste("Temperature (", degree, "C)")),
     main = dependent_col)

abline(h = 1)

optimal_meta_lower_scot <- as.numeric(names(
  which.min(which(pred_scot$allRRfit >= 1 & pred_scot$allRRfit <= 1.1))))
optimal_meta_upper_scot <- as.numeric(names(
  which.max(which(pred_scot$allRRfit >= 1 & pred_scot$allRRfit <= 1.1))))

extreme_cold_scot <- ifelse(optimal_meta_lower_scot < quantile(data$weather, 2.5/100, na.rm = TRUE),
                            optimal_meta_lower_scot,
                            quantile(data$weather, 2.5/100, na.rm = TRUE))
extreme_heat_scot <- ifelse(optimal_meta_upper_scot > quantile(data$weather, 97.5/100, na.rm = TRUE),
                            optimal_meta_upper_scot,
                            quantile(data$weather, 97.5/100, na.rm = TRUE))

ind_a_scot <- pred_scot$predvar <= extreme_cold_scot
ind_b_scot <- pred_scot$predvar >= extreme_cold_scot & pred_scot$predvar <= optimal_meta_lower_scot
ind_c_scot <- pred_scot$predvar >= optimal_meta_lower_scot & pred_scot$predvar <= optimal_meta_upper_scot
ind_d_scot <- pred_scot$predvar >= optimal_meta_upper_scot & pred_scot$predvar <= extreme_heat_scot
ind_e_scot <- pred_scot$predvar >= extreme_heat_scot

relative_risk_vals_scot <- pred_scot$allRRfit

lines(pred_scot$predvar,
      pred_scot$allRRfit,
      col = 'black',
      lwd = 1)

lines(pred_scot$predvar[ind_a_scot],
      pred_scot$allRRfit[ind_a_scot],
      col = c("#000FFF"),
      lwd = 1.5)
lines(pred_scot$predvar[ind_b_scot],
      pred_scot$allRRfit[ind_b_scot],
      col = c("#ABAFFF"),
      lwd = 1.5)
lines(pred_scot$predvar[ind_c_scot],
      pred_scot$allRRfit[ind_c_scot],
      col = c("black"),
      lwd = 1.5)
lines(pred_scot$predvar[ind_d_scot],
      pred_scot$allRRfit[ind_d_scot],
      col = c("#FFA7A7"),
      lwd = 1.5)
lines(pred_scot$predvar[ind_e_scot],
      pred_scot$allRRfit[ind_e_scot],
      col = c("#FF0000"),
      lwd = 1.5)


axis(2, at = 1:5 * 0.5)

breaks_scot <- c(min(data$weather, na.rm = TRUE) - 1,
                 seq(pred_scot$predvar[1],
                     pred_scot$predvar[length(pred_scot$predvar)],
                     length = 30),
                 max(data$weather, na.rm = TRUE) + 1)


hist_scot <- hist(data$weather, breaks = breaks_scot, plot = FALSE)
hist_scot$density <- hist_scot$density / max(hist_scot$density) * 0.7
prop_scot <- max(hist_scot$density) / max(hist_scot$counts)
counts_scot <- pretty(hist_scot$count, 3)

plot(hist_scot,
     ylim = c(0, max(hist_scot$density) * 3.5),
     axes = FALSE, ann = FALSE, col = grey(0.95),
     breaks = breaks_scot, freq = FALSE, add = TRUE)

axis(4, at = counts_scot * prop_scot, labels = counts_scot, cex.axis = 0.7)
mtext("N", 4, line = -0.5, at = mean(counts_scot * prop_scot), cex = 0.5)

# axis(1,at = c(-5,0,5,10,15,20,25,30))
# axis(2,at = 1:6*0.5)

abline(v = cen_scot, lty = 1, col = 3)
abline(v = c(optimal_meta_lower_scot, optimal_meta_upper_scot), lty = 2)
abline(v = c(extreme_cold_scot, extreme_heat_scot), lty = 3)

relative_risk_vector_scot <- append(relative_risk_vector_scot,
                                    pred_scot$allRRfit)

upper_vector_scot <- append(upper_vector_scot,
                            pred_scot$allRRhigh)

lower_vector_scot <- append(lower_vector_scot,
                            pred_scot$allRRlow)

region_vector_scot <-
  append(region_vector_scot,
         rep('Scotland',
             length(pred_scot$predvar)))

weather_vector_scot <- append(weather_vector_scot,
                           pred_scot$predvar)

cen_vector_scot <- append(cen_vector_scot,
                          rep(cen_scot,
                              length(pred_scot$predvar)))

weather_vector_scot <- append(weather_vector_scot,
                                  data$weather)

weather_region_vector_scot <-
  append(weather_region_vector_scot,
         rep('Scotland',
             length(data$weather)))

# output_df_scot <- data.frame(regions = region_vector_scot,
#                              weather = weather_vector_scot,
#                              rel_risk = relative_risk_vector_scot,
#                              centre_weather = cen_vector_scot,
#                              upper = upper_vector_scot,
#                              lower = lower_vector_scot)
# 
# weather_df_scot <- data.frame(weather = weather_vector_scot,
#                            regions = weather_region_vector_scot)


dev.off()

# write.csv(output_df,
#           paste(output_folder_path,
#                 'output_met_data.csv', sep = ''),
#           row.names = FALSE)

# Regional Attribution plots
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Attributable annual number - all matsim, all_arraysim 

## Attributable number CI estimate at Scotland level

att_low <- all_arraysim %>%
  filter(ci == "low")

att_high <- all_arraysim%>%
  filter(ci == "high")

att_low_all <- att_low%>%
  rename(all_heat_lci = all_heat_ci,
         risk_increase_lci = risk_increase_ci,
         high_risk_lci = high_risk_ci,
         moderate_heat_lci = moderate_heat_ci,
         high_heat_lci = high_heat_ci,
         heatwave_day_lci = heatwave_day_ci,
         heatwave_lci= heatwave_ci)

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

#~~~~~~~~~~~~~~~~~~~~ CONTINUE FROM HERE

# Function to create a plot for each area
plot_attribution_risk_inc <- function(region_names) {
  dat %>%
    filter(regnames == region_names) %>%
    ggplot(aes(x = year, y = risk_increase)) +
    geom_ribbon(aes(ymin = risk_increase_lci, ymax = risk_increase_uci), fill = "#f9e79f" , alpha = 0.2) +
    geom_line(color = "#E9B62D") +
    geom_point(color = "#E9B62D") +
    scale_y_continuous(limits = c(0,50))+
    labs(title = region_names,
         x = "Year",
         y = "Attributable deaths") +
    theme_minimal()+
    theme(plot.title = element_text(size = 8),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6))
}

# Generate plots for each area and store them in a list

risk_increase_plots <- map(region_names, plot_attribution_risk_inc) %>%
  gridExtra::grid.arrange(grobs = ., ncol = 3)


# Function to create a plot for each area
plot_attribution_heatwave_day <- function(region_names) {
  dat %>%
    filter(regnames == region_names) %>%
    ggplot(aes(x = year, y = heatwave_day)) +
    geom_ribbon(aes(ymin = heatwave_day_lci, ymax = heatwave_day_uci), fill = "#f0b27a", alpha = 0.2) +
    geom_line(color =  "#E57E41") +
    geom_point(color =  "#E57E41") +
    labs(title = region_names,
         x = "Year",
         y = "Attributable deaths") +
    theme_minimal()+
    scale_y_continuous(limits = c(0,50))+
    theme(plot.title = element_text(size = 8),
          axis.title.x = element_text(size = 6),
          axis.title.y = element_text(size = 6),
          axis.text.x = element_text(size = 6),
          axis.text.y = element_text(size = 6))
}

# Generate plots for each area and store them in a list

heatwaveday_plots <- map(region_names, plot_attribution_heatwave_day) %>%
  gridExtra::grid.arrange(grobs = ., ncol = 3)
