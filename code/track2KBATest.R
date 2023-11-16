#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read trips and colony sites
# 2. prepare dataset for track2KBA functions
# 3. summarize trips
# 4. calculate h ref
# 5. test individual effect
# 6. write output
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*trips_L2.csv", recursive = TRUE)

# Read colonysites
c <- read.csv2(paste0(WD,"GitData/Bird-borne-radar-detection/input/colonysites.csv"))

# levels
colonySites <- unique(c$colonyName)

########
#Step 2#
########

#perc_KUD = 100

for (i in seq_along(colonySites)){

  # i = 2

# Read all files
trips <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) %>%
  dplyr::filter(colonyName == colonySites[i]) 

# Loading colonies
colony <- c %>%
  dplyr::filter(colonyName == colonySites[i]) %>%
  dplyr::rename(
    Longitude = longitude,
    Latitude = latitude) %>%
  dplyr::select(Longitude, Latitude)
  

## format data
tracks_formatted <- formatFields(
  dataGroup = trips,
  formatBL = F,
  fieldID   = "organismID",
  fieldLat  ="latitude",
  fieldLon  ="longitude",
  fieldDateTime ="time",
  fieldDate = NULL,
  fieldTime = NULL
)

########
#Step 3#
########

# summarize trip characteristics
sumTrips <- tripSummary(tracks_formatted, colony)
unique(sumTrips$ID)

# write summary
fwrite(sumTrips, file=paste0(WD,"GitData/Bird-borne-radar-detection/output/", colonySites[i] ,"_sumTrips.csv"),row.names=FALSE)

# should be TRUE
# length(unique(tracks_formatted$tripID)) == nrow(sumTrips)

########
#Step 4#
########

# project dataset
tracks_prj <- projectTracks(
  tracks_formatted,
  projType = "azim",
  custom = "TRUE")

# calculate candidate smoothing parameter values
h_vals <- findScale(tracks_prj, sumTrips = sumTrips, scaleARS = TRUE)

########
#Step 5#
########

## estimate fidelity of individuals across trips
result <- indEffectTest(
  tracks = tracks_prj, 
  tripID = "tripID", 
  groupVar = "ID",
  plot = TRUE,
  levelUD = perc_KUD,
  scale = h_vals$href)

# save this info
h_vals$indEffectTest <- result[["Kolmogorov-Smirnov"]][["ks"]][["p.value"]]

## estimate how representative this sample of animals is of the population
KDE <- estSpaceUse(
  tracks = tracks_prj, 
  scale = h_vals$href, 
  levelUD = perc_KUD, 
  polyOut = TRUE
)

mapKDE(KDE = KDE$UDPolygons, colony = colony)

repr <- repAssess(
  tracks    = tracks_prj, 
  KDE       = KDE$KDE.Surface,
  levelUD   = 50,
  iteration = 1, 
  bootTable = FALSE)

########
#Step 6#
########

# write summary
fwrite(h_vals, file=paste0(WD,"GitData/Bird-borne-radar-detection/output/", colonySites[i] ,"_h_vals.csv"),row.names=FALSE)


}
