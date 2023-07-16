#-----------------------------------------------------------------------------------
# ValidationRadarDetector_min.R            
# Main script to validate methods used in Radar's paper
#-----------------------------------------------------------------------------------

#---------------------------------------------------------------
# Set working directory and Sys.Time
#---------------------------------------------------------------

#place <- "laptop"
place<- "miniPC"
if (place == "miniPC") WD <- "D:/Dropbox/"
if (place == "laptop") WD <- "C:/Users/lnh88/Dropbox/" 

Sys.setenv(TZ="GMT") ### !!! IMPORTANT: IF NOT SET LIKE THIS, MAKE PROBLEMS TO CREATE DATE_TIME FROM PASTING DATE plus TIME

#---------------------------------------------------------------
# Libraries and functions
#---------------------------------------------------------------
library(dplyr)
library(plyr)
library(purrr)
library(stringr)
library(ggplot2)
library(intervals)
library(data.table)
library(lubridate)
library(raster)
library(trip)
library(tidyr)

# to parallelize
pacman::p_load("parallel", "doParallel", "foreach")

fills_risk <- scale_fill_manual(name="Risk",
                                values = c("high"= "#ad1e02",
                                           "medium"= "#F0E442",
                                           "low"= "#009E73"))

cols_risk <- scale_colour_manual(name="Risk",
                                 values = c("high"= "#ad1e02",
                                            "medium"= "#F0E442",
                                            "low"= "#009E73"))
#---------------------------------------------------------------
# Processing workflow
# 1. Compare with raw AIS data
# 2. Compare with Welch, et al. 2022
#---------------------------------------------------------------

# rawAIS_validation.R

# ------------------
# welch_validation.R
#-------------------

# welch_validation_concurrent.R
# welch_validation_all.R