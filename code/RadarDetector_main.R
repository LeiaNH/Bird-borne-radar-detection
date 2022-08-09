#-----------------------------------------------------------------------------------
# Radar detectors.R            Main script for processing radar detector sensor data
#-----------------------------------------------------------------------------------

# Cleaning environment #
gdata::keep(place, WD, sure = TRUE)

#---------------------------------------------------------------
# Set working directory and Sys.Time
#---------------------------------------------------------------

place <- "miniPC"
#place<- "laptop"
if (place == "miniPC") WD <- "D:/Dropbox/LEIA/PROJECTS_LEIA/Radars_2022/"
if (place == "laptop") WD <- "C:/Users/lnh88/Dropbox/LEIA/PROJECTS_LEIA/Radars_2022/" 

Sys.setenv(TZ="GMT") ### !!! IMPORTANT: IF NOT SET LIKE THIS, MAKE PROBLEMS TO CREATE DATE_TIME FROM PASTING DATE plus TIME

#---------------------------------------------------------------
# Libraries and functions
#---------------------------------------------------------------

# Run code #
source(paste0(WD,"code/RadarDetector_sources.R"))

#---------------------------------------------------------------
# Palettes
#---------------------------------------------------------------

marinetraffic <- c("#f7f7f7","#32A2E7","#B5FA8D","#FFFFBF","#FDAE61", "#FF7900")

cols_colony <- scale_colour_manual(name="colonyName",
                                 values = c("CalaMorell"= "#0072B2",
                                            "MClara"= "#E69F00",
                                            "Veneguera"= "#D55E00",
                                            "CVelho"= "#009E73"), 
                                 labels = c("CalaMorell"= "Cala Morell",
                                            "MClara"= "Montaña Clara",
                                            "Veneguera"= "Veneguera",
                                            "CVelho"= "Curral Velho"))

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
#---------------------------------------------------------------

##########################################
#### GPS AND RADA DATA PRE-PROCESSING ####
##########################################

#### GLIMPSE DOWNLOADED DATA ####-----------------------------------------------

# Description #
# Here we read all files coming from radar tags (one file for GPS data and one file for RADAR data)
# We filter those recovery files were GPS and RADAR data was registered successfully. I.e., recoveries with GPS data but no RADAR data will not be selected.
# We finally filter for the study years (From 2018 to 2020), because GFW data from further years is not available. 

# Output data #
# Unique file with the file names that we will use to split trajectories into trips
# extension _L0.csv

# Run code #
# source(paste0(WD,"code/RadarTagFiles_comparison.R"))

# Cleaning environment #
# gdata::keep(place, WD, sure = TRUE)

#### SPLIT TRAJECTORIES INTO TRIPS ####-----------------------------------------------

# Description #
# Here we clean and split GPS trajectories from files named in filenames_L0.csv selection. Pseudoduplicate and overspeeded locations are removed. Trajectories are also interpolated to regular intervals. Large segments of interpolated locations will not be considered. Distance to colony per each location is also calculated.

# Output data #
# A file per colony with GPS trajectories labeled by trip.
# extension trips_L1.csv

# Set parameters#

# filter_dup()
# time difference to consider duplicated positions, in hours
step.time_v = 2/60
# spatial distance to consider duplicated positions, in km
step.dist_v = 0

# filter_speed()
# maximum of velocity using, in km/h
vmax_v = 90
# method see ?ddfilter.speed
method_v = 1

# interpolateTime()
# time interval which the interpolation should be done, in minutes
IntThr_v = 5

# tripSplit() 
# innerBuff: here you specify the radius around the colony that if the bird enters we consider it has "returned to the nest"
innerBuff_v = 5
# returnBuff: trips getting closer than this distance (in km) to the colony will not considered incomplete. Further than this distance, they will
returnBuff_v = 2
# duration: trips lasting less than this time (in hours) will not be considered foraging trips
duration_v = 5
# nests: set this to TRUE if you have a specific location for each nest
nests_v = F

# timedif.segment()
# time threshold criteria to split into segments, in hours
SplitThr_v = 1

# To consider as raw location
# We created an interval time before and after each real GPS location. All interpolated locations that fall within this interval we will be considered as raw locations.
# time threshold criteria before and after each GPS location to be considered as raw location, in seconds
TimeThr_v = 900

# Run code #
#source(paste0(WD,"code/trip_split.R"))

#### TRIP FILTER ####-----------------------------------------------

# Description #
# Final check and filtering of all trips already splitted to further analysis. 

# Output data #
# A file per colony with GPS trajectories labeled by trip.
# extension trips_L2.csv

# Set parameters#

# Minimum duration to be considered as a trip
duration_v = 5

# Run code #
#source(paste0(WD,"code/trip_filter.R"))


#### RADAR INTERPOLATION ####-----------------------------------------------

# Description #
# Here we interpolate locations of radar timestamps along GPS trips. Additionally we remove radar locations that were interpolated within a huge gap of time. 

# Output data #
# A file per colony with GPS trajectories labeled by trip.
# extension radar_L2.csv

# Set parameters#

# per each radar detection time threshold criteria before and after the previous and following GPS location, to be removed, in minutes
TimeThr_v = 60

# Run code #
source(paste0(WD,"code/radar_interpolation.R"))


#### TRIP VIEW EXPLORATION ####-----------------------------------------------

# Description #
# Here we just explore that everything is okey. That we have radar data for all gps trips and we plot how they look.

# Output data #
# view of radar and gps data trips
# extension L2.pdf

# Run code #
#source(paste0(WD,"code/trip_check.R"))

###############################
#### RADAR DATA PROCESSING ####
###############################

#### AGGREGATE RADAR INTO HOURLY BINS ####-----------------------------------------------

# Description #
# Here we aggregate radar data into 'hourly bins'. We will summarize per hour the presence or absence of radar detections. More than 1 radar detection would be considered as 'presence (1)' and 1 or less as 'absence(2)'. 

# Output data #
# A file per colony with radar info labeled by trip.
# extension HourlyBins_radar_L3.csv

# Set parameters#

# minimum number of radar detection registers per hourly bin to be considered as vessel presence
radar_v = 1

# Run code #
#source(paste0(WD,"code/radar_hourlybins.R"))


#### AGGREGATE RADAR INTO RADAR EVENTS ####-----------------------------------------------

# Description #
# Here we aggregate radar data into 'Radar events'. We will agregate consecutive radar detections as seabird-vessel attendance events. 

# Output data #
# A file per colony with radar info labeled by trip.
# extension events_radar_L3.csv

# Set parameters#

# time threshold criteria to split radar events into segments, in hours
radarID_v = 1

# Run code #
#source(paste0(WD,"code/radar_events.R"))

###############################
#### CORE AREAS PARAMETERS ####
###############################

# Description #
# Here we summarize trips, estimate h ref for core area calculation and test individual effect per colony

# Output data #
# indEffectTest.rds

# Set parameters#

# KernelUD percentage
perc_KUD = 50

# Run code #
#source(paste0(WD,"code/indEffectTest.R"))

################################
#### CORE AREAS CALCULATION ####
################################

# Description #
# Here we calculate specific KUDs of each population using GPS data

# Output data #
# CoreAreas.rds

# Set parameters#

# KernelUD percentage
perc_KUD = 50

# Run code #
#source(paste0(WD,"code/CoreAreas.R"))

##########################################################
#### PROPORTION OF RADAR DETECTIONS WITHIN CORE AREAS ####
##########################################################

# Description #
# Here we calculate the percemtage of radar events within core areas

# Output data #
# corearea_radarevents_sz

# Run code #
#source(paste0(WD,"code/CoreAreas_radar_events.R"))

################################
#### VESSEL DATA PROCESSING ####
################################

#### DAILY MMSI PRESENCE WITHIN RADAR EVENTS  ####-----------------------------------------------

# Description #
# Here we substract the daily presence of fishing vessels at finest scale from radar events

# Output data #
# A file per colony with radar info labeled by trip.
# extension events_radar_L4.csv

# Set parameters#

# Buffer distance of each radar detections to substract mmsi presence
distance <- 5000  # in meters


# Run code #
#source(paste0(WD,"code/dailyGFW_radar_events.R"))

#### PREPARATION OF MONTHLY SUMMARIZED NON-FISHING VESSEL FOOTPRINT  ####------------------------------

# Description #
# Here we substract monthly data from non-fishing s-AIS datasets
# Output data #
# A tif per month 
# extension _sAISnonfishing.tif including passenger-cargo-tanker-other sAIS types
# extension_sAISdomestif.tif including passenger-cargo-tanger sAIS types

# Run code #
source(paste0(WD,"code/monthlyAIS_sztiff.R"))


#### PREPARATION OF MONTHLY SUMMARIZED FISHING VESSEL FOOTPRINT  ####------------------------------

# Description #
# Here we substract monthly data from fishing GFW datasets
# Output data #
# A tif per month 
# extension _GFW_FishingHours.tif and _GFW_NavigationHours.tif

# Set parameters#

# resolution 
res <- 0.25

# Run code #
source(paste0(WD,"code/monthlyGFW_sztiff.R"))

#### EXTRACTION OF MARINE VESSEL TRAFFIC VALUES PER RADAR HOURLYBINS  ####------------------------------

# Description #
# Here we substract monthly data from fishing GFW datasets and non-fishing sAIS datasets per hourlybin

# Output data #
# A file per colony
# extention HourlyBins_radar_evaluation.csv

# Set parameters#

# Buffer distance of each radar detections to substract vessel data 
 
distance <- 5000  # in meters

# Run code #
source(paste0(WD,"code/radar_hourlybins_vesseltraffic.R"))


#### EXTRACTION OF MARINE VESSEL TRAFFIC VALUES PER RADAR EVENT  ####----------------------------

# Description #
# Here we substract monthly data from fishing GFW datasets and non-fishing sAIS datasets per radar event

# Output data #
# A file per colony
# extention events_radar_unreportedGFW.csv

# Set parameters#

# Buffer distance of each radar detections to substract mmsi presence

distance <- 5000  # in meters

# Run code #
source(paste0(WD,"code/radar_events_unreportedGFW.R"))


#########################################
#### DATA PROCESSING FOR GAMM MODELS ####
#########################################

#### TEMPORAL COVARS  ####------------------------------

# Description #
# Here we define per each radar hourly bin its light level (day or night) and period of the week (weekend or weekdays)

# Output data #
# A file per colony
# extention HourlyBins_radar_evaluation.csv (we overwrite same files)

# Run code #
source(paste0(WD,"code/radar_hourlybins_temporalcovars.R"))


#### GAMM models  ####------------------------------

# Description #
# Here we perform the GAMM models per population

# Output data #
# A file per colony
# extention gamm_models.csv 

# Run code #
source(paste0(WD,"code/radar_hourlybins_GAMM.R"))

#### GAMM Population comparative  ####------------------------------

# Description #
# Here we perform the best GAMM models per population and calculate its AUC.
# IMPORTANT! do not run next script before checking the last best model from previous script
# needs some manual modifications

# Output data #
# A file per colony
# extention AUC_models.csv 

# Run code #
source(paste0(WD,"code/radar_hourlybins_GAMM_sz.R"))


#---------------------------------------------------------------
# 6. Main Figures
#---------------------------------------------------------------

#### General map of trips within the study area ####


# Description #
# Map of the study area with  200 isobath, colony sites, trips and other general elements, 

# Output data #
# extension _GPStrips.png

# Run code #
source(paste0(WD,"code/Fig_GPStrips.R"))

# Cleaning environment #
gdata::keep(place, WD, sure = TRUE)

#### Core areas plot ####

# Description #
# Map of the study core areas and radar events. 

# Output data #
# extension _CoreArea.png

# Run code #
source(paste0(WD,"code/corearea_plot.R"))

# Cleaning environment #
gdata::keep(place, WD, sure = TRUE)

#### Marine traffic values on radar events not matching with GFW ####


# Description #
# plots trying to represent if radar events not matching daily with GFW, fall within
# areas with high fishing footprint or non-fishing routes

# Output data #
# ... nothing yet, exploratory status

# Run code #
source(paste0(WD,"code/radar_events_unreportedGFW_plot.R"))


#### GAMM: PARTIAL EFFECTS####


# Description #
# Here we plot the significative smoothed terms from GAMM models

# Output data #
# ggeffects_FishingvsNonFishing.png

# Run code #
source(paste0(WD,"code/radar_hourlybins_GAMM_partialeffects_plot.R"))

#### GAMM: RESIDUAL'S PLOT ####

# Description #
# Here we plot the higuest residuals from GAMM models

# Output data #
# ggeffects_FishingvsNonFishing.png

# Run code #
source(paste0(WD,"code/radar_hourlybins_GAMM_residuals_plot.R"))



#---------------------------------------------------------------
# 7. Main tables
#---------------------------------------------------------------

#### Radar detection and radar event sample size, related to GFW daily info ####


# Description #
# Summary table of the number of radar detections and number of radar events that match (presence) or don't match (absence) with GFW daily fleet dataset. The percentage of not matching sample size is calculated- 

# Output data #
# Two files, one for radar detections and one for radar events
# extension _dailyGFW_sz.csv

# Run code #
source(paste0(WD,"code/dailyGFW_radar_events_sz.R"))

# Cleaning environment #
gdata::keep(place, WD, sure = TRUE)

head(sz1)

#### Trip sample size and max.distance ####


# Description #
# Summary table of the number of trips and the mean and sd of the maximum distance 

# Output data #
# tripsamplesize_sz. csv

# Run code #
source(paste0(WD,"code/trip_distance_sz.R"))

# Cleaning environment #
gdata::keep(place, WD, sure = TRUE)

head(sz1)

#### All GAMM model stats ####


# Description #
# Summary table of the results of all possible GAMM models per population

# Output data #
# allmodels.csv

# Run code #
source(paste0(WD,"code/modelset_table.R"))

# Cleaning environment #
gdata::keep(place, WD, sure = TRUE)

