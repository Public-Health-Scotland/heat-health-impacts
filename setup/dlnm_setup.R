library(snakecase)
library(tidyverse)
library(janitor) # for cleaning data
# library(tidylog) # useful in console dplyr messages
library(phsmethods) # useful PHS specific functions
library(phsstyles)
library(lubridate)
library(dlnm)
library(mixmeta)
library(splines)
library(tsModel)
library(zeallot)
library(FluMoDL)
library(R2admb)
library(tseries)
library(rmarkdown)
library(magrittr)
library(gridExtra)
library(shinyWidgets)
library(shinycssloaders)
library(car) # to check collinearity
library(assertr)
library(testthat)

## Choose time period ----
# Different indicators will require different time periods (i.e. heat related
# indicators may look specifically at the summer months). 
# If looking at summer months only, set summer to TRUE. Otherwise, set to FALSE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summer = TRUE

# If summer is set to true, month choice is filtered to the requested months
# UKHSA and PHW have used June (6) to September (9) as the summer months,
# so we have mirrored that choice here

if(summer == TRUE){
  month_choice = c(6:9)
} else{
  month_choice = c(1:12)
}

# VARPER chooses which percentile to have internal knot positions at 
# - Fewer knot positions and no need for a low knot for summer months
# varper is assigned when using a b-spline for the exposure function. Otherwise
# vardegree is used for the ns exposure function

if(summer == TRUE){
  varper <- c(50,90)
}else{
  varper <- c(10,75,90)
}