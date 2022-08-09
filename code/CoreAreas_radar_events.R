#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read radar events files 
# 2. read core areas
# 3. Overlap radar events per core area per population
# 4. sz
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*_events_radar_L3.csv", recursive = TRUE)

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
#Step 2#
########

CA <- readRDS(paste0(WD, "output/CoreAreas.rds"))


########
#Step 3#
########

populations <- unique(RAD$population)

sz <- list()

for (i in seq_along(populations)){
  
  #i = 3
  
  if(populations[[i]] == "BalearicIs"){
    poly <- CA[[1]]}
  
  if(populations[[i]] == "CaboVerde"){
    poly <- CA[[2]]}
  
  if(populations[[i]] == "CanaryIs"){
    poly <- CA[[3]]}
  
  # subset population 
  radar_group <- RAD %>% 
    dplyr::filter(population == populations[[i]]) 
  
  # subset coordinates
  coord <- radar_group%>%
    dplyr::select(longitude,latitude)
  
  # parse to spatialpointsdataframe
  point <- SpatialPointsDataFrame (coord,proj4string=CRS("+proj=longlat +datum=WGS84"), data=radar_group)
  
  # plot it
  plot(point, col="red")
  plot(poly,add=T)
  
  # over function
  over <- over(point, poly)
  
  # merge info
  radar_group <- cbind (radar_group, over)
  
  # check overlap
  point_ovr <- radar_group %>% drop_na(over) %>% dplyr::select(longitude, latitude)
  point_ovr <- SpatialPointsDataFrame (point_ovr,proj4string=CRS("+proj=longlat +datum=WGS84"), data=point_ovr)
  plot(point_ovr, col="blue", add=T)
  
  
  ########
  #Step 4#
  ########
  
  # number of radar event
  
  total <- radar_group %>%
    dplyr::mutate(
      # create an unique radar ID per trip
      radarID2 = paste0(tripID,"_", radarID)) 
  
  total$radarID2 = as.factor(total$radarID2)
  
  total <- length(unique(total$radarID2))
  
  # number of radar event totally or partially overlapping with core area
  
  ovr <- radar_group %>%
    dplyr::mutate(
      # create an unique radar ID per trip
      radarID2 = paste0(tripID,"_", radarID)) %>%
    # remove points outside core areas
    drop_na(over)
  
  ovr$radarID2 = as.factor(ovr$radarID2)
  
  ovr <- length(unique(ovr$radarID2))
  
  # sz
  
  ovr <- tibble(population = unique(radar_group$population), 
               n_radarevents = total, 
               n_radarevents_ca = ovr,
               percent_radarevents_ca = round(((ovr/total)*100),1))
  
  sz[i] <- list(ovr)
  
}

sz <- do.call(bind_rows, sz)

fwrite(sz, file=paste0(WD,"output/tables/corearea_radarevents_sz.csv"),row.names=FALSE)
