#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read _L2 radar and GPS files
# 2. check were we have gps data we have radar data
# 3. plot it
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##########################
# general objects needed #
##########################

# load worldmap as sf

load(paste0(WD, "GitData/Bird-borne-radar-detection/input/valid_world_map.Rdata"))
msk_valid %>% 
  dplyr::select(Name) -> msk_sf  

# load colony locations

c <- read.csv2(paste0(WD,"GitData/Bird-borne-radar-detection/input/colonysites.csv"))


########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*trips_L2.csv", recursive = TRUE)

# Read all files
GPS <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*radar_L2.csv", recursive = TRUE)

# Read all files
RADAR <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

########
#Step 2#
########

GPS_sz <- GPS %>%
  dplyr::group_by(tripID) %>%
  summarize(
    duration_days = as.numeric(difftime(max(time), min(time), units = "days")),
    locations = n()
  )

RADAR_sz <- RADAR %>%
  dplyr::group_by(tripID) %>%
  summarize(
    duration_days = as.numeric(difftime(max(time), min(time), units = "days")),
    locations = n()
  )

sz <- merge(GPS_sz, RADAR_sz, by ="tripID", all = T)

########
#Step 3#
########

#-----------------#
# Prepare cluster #
#-----------------#

# let's %dopar% per colony site 
colonysites <- unique(c$colonyName)

cores <- length(colonysites) #Define number of cores
cl <- makeCluster(cores)
registerDoParallel(cl)


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
  dataGroup <- GPS %>% dplyr::filter(colonyName == colonysites[i])      
  
  tripvector <- unique(dataGroup$tripID)
  
  plotlist <- list()
  for (p in 1:length(tripvector)){
    
    # p = 1
    
    trip <-  dataGroup %>%
      dplyr::filter(tripID == tripvector[p])
    
    radar_trip <-  RADAR %>%
      dplyr::filter(tripID == tripvector[p])
    
    # plot
    tripZoom_GPS <- ggplot() +
      # land mask
      geom_sf(data = msk_sf) +
      # add tracks
      geom_path(data = trip, aes(x = longitude, y = latitude), colour = "black") +      
      geom_point(data = trip, aes(x=longitude, y=latitude), colour = "blue", size=2, alpha= 0.5) +
      # set spatial bounds
      coord_sf(xlim = range(trip$longitude), ylim = range(trip$latitude), expand=T) +
      # colony location
      geom_point(data = colony, aes(x=Longitude, y=Latitude), size=3, shape=24, fill = "yellow") + 
      # theme
      theme_classic() +
      ggtitle(paste0("GPS"))
    
    # plot
    tripZoom_RADAR <- ggplot() +
      # land mask
      geom_sf(data = msk_sf) +
      # add tracks
      geom_path(data = trip, aes(x = longitude, y = latitude), colour = "black") +      
      geom_point(data = radar_trip, aes(x=longitude, y=latitude),colour = "red", size=1, alpha= 0.5) +
      # set spatial bounds
      coord_sf(xlim = range(trip$longitude), ylim = range(trip$latitude), expand=T) +
      # colony location
      geom_point(data = colony, aes(x=Longitude, y=Latitude), size=3, shape=24, fill = "yellow") + 
      # theme
      theme_classic() +
      ggtitle(paste0("RADAR"))
    
    # arrange 
    
    plot <- cowplot::plot_grid(tripZoom_GPS, tripZoom_RADAR, 
                               ncol = 2, nrow = 1)
    
    # list plots
    
    plotlist[[p]] <- list(plot)
    
  }
  
  plotlist_f <- flatten(plotlist)
  
  setwd(paste0(WD,"GitData/Bird-borne-radar-detection//output/plots/"))
  ggsave(
    filename = paste0("splitTrips_II_",colonysites[i],"_L2.pdf"), 
    plot = gridExtra::marrangeGrob(grobs=plotlist_f, nrow=4, ncol=1), 
    width = 9, height = 15)
  
}
