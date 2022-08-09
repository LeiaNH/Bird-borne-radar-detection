#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
#1. Read file name and n rows from GPS raw data 
#2. Read file name and n rows from RADAR raw data 
#3. Link by file name GPS and Radar files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# Make a list of available csv files 
setwd(paste0(WD,"input/GPS"))
files <- list.files(pattern = '.csv') ## here we are taking all files ending in .csv

# apply the function to all files
GPS <- map_df(files, glimpse_file)

# add GPS in all column names
colnames(GPS) <- paste(colnames(GPS), "GPS", sep = "_")

########
#Step 2#
########

# Make a list of available csv files 
setwd(paste0(WD,"input/RADAR"))
files <- list.files(pattern = '.csv') ## here we are taking all files ending in .csv

# apply the function to all files
RAD <- map_df(files, glimpse_file)

# add RAD in all column names
colnames(RAD) <- paste(colnames(RAD), "RAD", sep = "_")

########
#Step 3#
########

# we will merge GPS and RAD data by deployment ID, so let's unify that var in common

RAD <- RAD %>%
  rename(deploymentID = deploymentID_RAD)

GPS <- GPS %>%
  rename(deploymentID = deploymentID_GPS)

# merge both

comparison <- merge(GPS, RAD, by = "deploymentID", all = TRUE) %>%
  # this study comprises between 2018 and 2020
  dplyr::filter(
    yearFile_GPS < 2021) %>%
  # interval GPS deployments
  dplyr::mutate(
    duration = as.numeric(difftime(time1_GPS, time0_GPS, units = "days"))) %>%
  # filter deployments with more than a day of data
  dplyr::filter(duration > 1)
  

########
#Step 4#
########

# write csv

fwrite(comparison, file=paste0(WD,"output/filenames_L0.csv"),row.names=FALSE)


