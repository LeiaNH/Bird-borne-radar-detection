#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read _L2 radar files
# 2. aggregate data per radar events
# 3. save data
# 4. plot it
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*radar_L2.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

########
#Step 2#
########

output <- list()

organisms <- unique(RAD$organismID)

for (i in seq_along(organisms)){
  
  #i=1
  
  print(length(organisms)-i)
  
  # filter organism data
  RADAR_ss <- RAD %>% 
    # filter the organism
    dplyr::filter(
      organismID ==  organisms[i]) %>%
    # arrange by time
    arrange(time)
  
  # filter timestamps when radar emission level was higher than 0
  timestamps <- RADAR_ss %>%
    dplyr::filter(
      # first filter radar detections
      Radar_level > 0) %>%
    # select time
    dplyr::select(time) 
  
  if(sum(nrow(timestamps))>0){
    
  # create identification number of each radar event
  timestamps$radarID <- timedif.segment(timestamps$time, radarID_v)
  
  # merge to original data 

  RADAR_ss <- merge(RADAR_ss, timestamps, by="time", all.x = TRUE)}
  
  output[i] <- list(RADAR_ss)
}

radar_events <- do.call(bind_rows, output)


########
#Step 3#
########


radar_events <- radar_events %>%
  # filter radarIDs
  drop_na(radarID)

# write dataset

colonysites <- unique(radar_events$colonyName)

for (i in seq_along(colonysites)){
  
  #i=1
  colony <-  colonysites[[i]]
  
  # read radar data
  radar_group <- radar_events %>%
    #filter colony group
    dplyr::filter(colonyName == colony)
  
  # write dataset
  fwrite(radar_group, file=paste0(WD,"GitData/Bird-borne-radar-detection//output/",colonysites[i],"_events_radar_L3.csv"),row.names=FALSE)}


########
#Step 4#
########

#-----------------#
# Prepare cluster #
#-----------------#

# let's %dopar% per colony site 
colonysites <- unique(c$colonyName)

cores <- length(colonysites) #Define number of cores
cl <- makeCluster(cores)
registerDoParallel(cl)


# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*trips_L2.csv", recursive = TRUE)

# Read all files
GPS <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

foreach(i=1:length(colonysites), .packages=c("dplyr" ,"sf","gridExtra", "tidyverse","cowplot", "data.table", "lubridate", "purrr")) %dopar% {
  
  #for (i in seq_along(colonysites)){
  #i=2
  #---------------------------------------#
  # Set the colony coordinates and extent #
  #---------------------------------------#
  
  c_i <- c %>%
    dplyr::filter(colonyName == colonysites[i]) %>%
    dplyr::rename(Longitude = longitude,
                  Latitude = latitude)
  
  colony <- c_i %>%
    dplyr::select(Longitude, Latitude)
  
  
  # filter the colony
  dataGroup <- radar_events %>% dplyr::filter(colonyName == colonysites[i])      
  
  # parse to factor and create a scale color palette 
  dataGroup$radarID = as.factor(dataGroup$radarID)
  
  # plot per trip
  tripvector <- unique(dataGroup$tripID)
  
  plotlist <- list()
  for (p in 1:length(tripvector)){
    
    # p = 4
    
    trip <-  GPS %>%
      dplyr::filter(tripID == tripvector[p])
    
    radar_trip <-  dataGroup %>%
      dplyr::filter(tripID == tripvector[p]) 
    
    # plot
    tripZoom <- ggplot() +
      # land mask
      geom_sf(data = msk_sf) +
      # add tracks
      geom_path(data = trip, aes(x = longitude, y = latitude), colour = "black") +      
      geom_point(data = trip, aes(x = longitude, y = latitude), colour = "black", size=1, alpha= 0.5) +
      geom_point(data = radar_trip, aes(x=longitude, y=latitude, colour =  radarID),alpha= 0.5, size=2) +
      # set spatial bounds
      coord_sf(xlim = range(radar_trip$longitude), ylim = range(radar_trip$latitude), expand=T) +
      # colony location
      geom_point(data = colony, aes(x=Longitude, y=Latitude), size=3, shape=24, fill = "yellow") + 
      # theme
      theme_classic() +
      ggtitle(paste0("Radar events"))
    
    # list plots
    
    plotlist[[p]] <- list(tripZoom)
    
  }
  
  plotlist_f <- flatten(plotlist)
  
  setwd(paste0(WD,"GitData/Bird-borne-radar-detection//output/plots/"))
  ggsave(
    filename = paste0("radarevents_",colonysites[i],"_L3.pdf"), 
    plot = gridExtra::marrangeGrob(grobs=plotlist_f, nrow=4, ncol=2), 
    width = 9, height = 15)
  
}
