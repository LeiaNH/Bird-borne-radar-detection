#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read trips_L2 gps files 
# 2. Calculate core area per population
# 3. Remove core area overlapping with landmask
# 4. Merge core areas from same population
# 5. Save it
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#messing

##########################
# general objects needed #
##########################

# load worldmap as sf

load(paste0(WD, "input/valid_world_map.Rdata"))
msk_valid %>% 
  dplyr::select(Name) -> msk_sf  


########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*trips_L2.csv", recursive = TRUE)

# Read all files
GPS <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 

########
#Step 2#
########

# select only the coordinates and the level desired
GPS <- GPS %>%
  dplyr::select(longitude, latitude, colonyName)%>%
  dplyr::rename(id = colonyName)%>%
  dplyr::mutate(id = as.factor(id))

# create an object only with the coordinates
xy <- GPS %>%
  dplyr::select(longitude,latitude)

# parse coordinates to spatialpointsdataframe
spdf <- SpatialPointsDataFrame (xy, proj4string=CRS("+proj=longlat +datum=WGS84"), data = GPS)

#spdf@proj4string
#spdf@data$id

# Calculate kernelUD areas
KUDs <- adehabitatHR::kernelUD (spdf[,3], same4all = T, "href", grid = 500, extent = 0.5)

# Substract home ranges
ver <- adehabitatHR::getverticeshr(KUDs, perc_KUD)

# check it
plot(ver)

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
