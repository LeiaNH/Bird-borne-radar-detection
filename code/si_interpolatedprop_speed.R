# List L1.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*trips_L2.csv", recursive = TRUE)

# Read all files
GPS <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

# percent of locations interpolated per colony

prop <- GPS %>%
  mutate(sp = recode(colonyName, 
                     "CalaMorell" = "CALDIO",
                     "CVelho" = "CALEDW",
                     "MClara" = "CALBOR",
                     "Veneguera" = "CALBOR")) %>%
  dplyr::group_by(sp, type) %>%
  summarize(
    n=n()) %>%
  pivot_wider(names_from=type, values_from=n) %>%
  dplyr::mutate(propInt = (interpolated/(raw+interpolated))*100)


# maximum speed

trips <- unique(GPS$tripID)

cores <- 5
cl <- makeCluster(cores)
registerDoParallel(cl)

l <- foreach(i=1:length(trips), .packages=c("tidyverse", "lubridate", "dplyr", "data.table", "sp", "rgdal", "trajr")) %dopar% {
  # i=1
  
  # filter trip
  gps_j <- GPS %>% dplyr::filter(tripID == trips[[i]])
  
  # select longitude and latitude vars
  coords <- gps_j %>% 
    dplyr::select(longitude,latitude)
  
  # parse to coordinate object
  sp::coordinates(coords) <- c("longitude", "latitude")
  
  # set CRS projection
  sp::proj4string(coords) <- sp::CRS("+proj=longlat +datum=WGS84")
  
  # parse to UTM 
  coords <- sp::spTransform(coords, sp::CRS("+proj=utm"))# When I add ellps=WGS84 a warning pops. So I deleted it. But the result it's the same
  
  # parse to data frame
  coords <- as.data.frame(coords)
  
  # include time var
  coords$date <- gps_j$time
  
  # parse to trj object
  trj <- trajr::TrajFromCoords (track=coords,
                                xCol = 2,
                                yCol = 3,
                                timeCol=1,
                                spatialUnits="m",
                                timeUnits="s")
  # derivs
  derivs <- trajr::TrajDerivatives(trj)
  
  # speed
  speed <- derivs[["speed"]]
  speed <- as.data.frame(speed)
  
  # filter max velocity
  vmax_v = 90 * 1000 / 3600

  speed <- speed %>% filter(speed < vmax_v &
                              speed > 0)
  
  # sz
  sz <- speed %>% 
    summarize(maxSpeed = max(speed),
              meanSpeed = mean(speed))

  sz
  
}

#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

output <- rbindlist(l)

sz <- output %>% 
  summarize(maxSpeed = max(maxSpeed),
            meanSpeed = mean(meanSpeed))

sz

#---------------------------------------------------------------
# href
#---------------------------------------------------------------

# List L1.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*_h_vals.csv", recursive = TRUE)

# read h values estimated by track2kba. All values are in kilometers.
h_val <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) %>%
  pull(scaleARS)
