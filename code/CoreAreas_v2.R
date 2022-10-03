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

levels <- unique(allGPS$id)

for (i in 1:length(levels)){
  
  i=2
  
  GPS <- allGPS %>%
    dplyr::filter(id == levels[i])
  
  # create an object only with the coordinates
  xy <- GPS %>%
    dplyr::select(longitude,latitude)
  
  # parse coordinates to spatialpointsdataframe
  spdf <- SpatialPointsDataFrame (xy, proj4string=CRS("+proj=longlat +datum=WGS84"), data = GPS)
  plot(spdf)

  #spdf@proj4string
  #spdf@data$id

  # read h values estimated by track2kba
  h_val <- read_csv(paste0(WD,"GitData/Bird-borne-radar-detection/output/",levels[i], "_h_vals.csv")) %>%
    pull(scaleARS)

  # Calculate kernelUD areas
  #KUDs <- adehabitatHR::kernelUD (spdf[,3], same4all = T, h = "href", grid = 500, extent = 0.5)
  #KUDs <- adehabitatHR::kernelUD (spdf, h = "href", same4all = TRUE)
  KUDs <- adehabitatHR::kernelUD (spdf, h = h_val/1000, same4all = TRUE, grid = 600)
  
  plot(KUDs)
  
  # Substract home ranges
  ver <- adehabitatHR::getverticeshr(KUDs, perc_KUD)

  # check it
  plot(ver)

  
  ## Estimation of the UD with grid=20 and extent=0.2
  image(kernelUD(spdf, h = h_val, same4all = TRUE, grid=500, extent=0.5))

  
}



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

# split
BalearicIs <- crop[1]
plot(BalearicIs)

CaboVerde <- crop[2]
plot(CaboVerde)

########
#Step 4#
########

# merge both kernels from Canary Is population

canaryPop_colony1 <- crop[3]
plot(canaryPop_colony1)

canaryPop_colony2 <- crop[4]
plot(canaryPop_colony2)

CanaryIs <-  aggregate(rbind(canaryPop_colony1, canaryPop_colony2)) 
plot(CanaryIs)

########
#Step 5#
########

# save as list

KUDs <- list(BalearicIs, CaboVerde, CanaryIs)

saveRDS(KUDs, paste0(WD,"output/CoreAreas.rds"))
