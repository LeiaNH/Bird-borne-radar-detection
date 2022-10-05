#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read trips_L2 gps files 
# 2. Calculate core area per population
# 3. Remove core area overlapping with landmask
# 4. Merge core areas from same population
# 5. Save it
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##########################
# general objects needed #
##########################

# load worldmap as sf

load(paste0(WD, "GitData/Bird-borne-radar-detection/input/valid_world_map.Rdata"))
msk_valid %>% 
  dplyr::select(Name) -> msk_sf  


########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*trips_L2.csv", recursive = TRUE)

# Read all files
GPS <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

########
#Step 2#
########

# select only the coordinates and the level desired
allGPS <- GPS %>%
  dplyr::select(longitude, latitude, colonyName)%>%
  dplyr::rename(id = colonyName)%>%
  dplyr::mutate(id = as.factor(id))



#-----------------#
# Prepare cluster #
#-----------------#

# let's %dopar% per colony site 
colonysites <- unique(allGPS$id)

cores <- length(colonysites) #Define number of cores
cl <- makeCluster(cores)
registerDoParallel(cl)

#-----------------------#
# Parallel foreach loop #
#-----------------------#

l <- foreach(i=1:length(colonysites), .packages=c("dplyr" ,"tidyverse","data.table", "lubridate", "purrr", "sp", "readr", "adehabitatHR", "rgeos", "sf" )) %dopar% {
  
#for (i in 1:length(colonysites)){
  #i=3
  
  GPS <- allGPS %>%
    dplyr::filter(id == colonysites[i])
  
  # create an object only with the coordinates
  xy <- GPS %>%
    dplyr::select(longitude,latitude)
  
  # parse coordinates to spatialpointsdataframe
  spdf <- SpatialPointsDataFrame (xy, proj4string=CRS("+proj=longlat +datum=WGS84"), data = GPS)
  plot(spdf)

  #spdf@proj4string
  #spdf@data$id

  # read h values estimated by track2kba. All values are in kilometers.
  h_val <- read_csv(paste0(WD,"GitData/Bird-borne-radar-detection/output/",colonysites[i], "_h_vals.csv")) %>%
    #pull(scaleARS)
    pull(href)
  
  # grid
  x <- seq(min(GPS$longitude)-0.5,max(GPS$longitude)+0.5,by=0.01) # where resolution is the pixel size you desire 
  y <- seq(min(GPS$latitude)-0.5,max(GPS$latitude)+0.5,by=0.01)
  xy <- expand.grid(x=x,y=y)
  coordinates(xy) <- ~x+y
  gridded(xy) <- TRUE
  class(xy)
  
  # Calculate kernelUD areas
 # KUDs <- adehabitatHR::kernelUD (spdf, same4all = T, h = "href")
  KUDs <- adehabitatHR::kernelUD (spdf, grid = xy, h = "href")
  
  #KUDs@h
  #h_val/100
  
  #KUDs <- adehabitatHR::kernelUD (spdf, h = h_val/100, grid = xy)
  
  plot(KUDs)
  
  # Substract home ranges
  perc_KUD = 50
  ver <- adehabitatHR::getverticeshr(KUDs, perc_KUD)

  # check it
  plot(ver, add=TRUE, col = "white")
  
  ########
  #Step 3#
  ########

  #overlaping
  class(ver)
  class(msk_sf)

  # parse mask land to spatial
  land <- as(msk_sf, Class = "Spatial") 
  class(land)

  # check projection
  proj4string(land)

  # crop both
  crop <- rgeos::gDifference(ver, land, byid=TRUE)

  # check it
  plot(ver, col = "red")
  plot(crop, col="blue", add = T)
  
  crop
  }


########
#Step 4#
########
colonysites

# split
BalearicIs <- l[[1]]
plot(BalearicIs)

CaboVerde <- l[[2]]
plot(CaboVerde)

# merge both kernels from Canary Is population

canaryPop_colony1 <- l[[3]]
plot(canaryPop_colony1)

canaryPop_colony2 <- l[[4]]
plot(canaryPop_colony2, add =T)

CanaryIs <-union(canaryPop_colony1, canaryPop_colony2)
CanaryIs <- aggregate(CanaryIs, dissolve = TRUE)
plot(CanaryIs)

########
#Step 5#
########

# save as list

KUDs <- list(BalearicIs, CaboVerde, CanaryIs)

saveRDS(KUDs, paste0(WD,"GitData/Bird-borne-radar-detection/output/CoreAreas.rds"))
