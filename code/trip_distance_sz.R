#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. Read _L2 GPS files
# 2. Summarize sample size trip and maximum distance
# 3. Read radar_L2 radar files
# 4. Summarize sample size trip and relative percentage
# 4. Merge info and save it
# 5. Deployment sample size 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*trips_L2.csv", recursive = TRUE)

# Read all files
GPS <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 

# Add population label
GPS <- GPS %>%
  dplyr::mutate(population = recode(colonyName, 
                                    "CalaMorell" = "BalearicIs",
                                    "CVelho" = "CaboVerde",
                                    "MClara" = "CanaryIs",
                                    "Veneguera" = "CanaryIs")) 

########
#Step 2#
########

sz1 <- GPS %>%
  group_by(tripID) %>%
  slice(which.max(ColDist)) %>%
  dplyr::mutate(
    ColDist = ColDist / 1000) %>%
  group_by(population) %>%
  summarize(
    ndeployments = length(unique(deploymentID)),
    ntrips = length(unique(tripID)),
    meanMaxColDist = round(mean(ColDist),1),
    sdMaxColDist = round(sd(ColDist),1))

########
#Step 3#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*radar_L2.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 

# Add population label
RAD <- RAD %>%
  dplyr::mutate(population = recode(colonyName, 
                                    "CalaMorell" = "BalearicIs",
                                    "CVelho" = "CaboVerde",
                                    "MClara" = "CanaryIs",
                                    "Veneguera" = "CanaryIs")) 

########
#Step 4#
########

sz2 <- RAD %>%
  dplyr::filter(Radar_level > 0) %>%
  dplyr::select(tripID, population) %>%
  distinct() %>%
  group_by(population) %>%
  summarize(
    ntrips_rad = length(unique(tripID)))
  
########
#Step 5#
########

sz <- merge(sz1, sz2, by = "population")

sz <- sz %>%
  dplyr::mutate(
    percentage_rad_trips = round((ntrips_rad/ntrips)*100,1),
    Trips = paste0(ntrips, " (", ntrips_rad, ", ", percentage_rad_trips, "%)"),
    MaxColDist = paste0(meanMaxColDist, "±", sdMaxColDist)) %>%
  dplyr::select(population, Trips, MaxColDist, ndeployments) 

fwrite(sz, file=paste0(WD,"output/tables/tripsamplesize_sz.csv"),row.names=FALSE)

