#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read _L2 radar files
# 2. aggregate data per hourly bins
# 3. save data
# 4. plot it
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##########################
# general objects needed #
##########################

# load worldmap as sf

load(paste0(WD, "input/valid_world_map.Rdata"))
msk_valid %>% 
  dplyr::select(Name) -> msk_sf  

# load colony locations

c <- read.csv2(paste0(WD,"input/colonysites.csv"))


########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*radar_L2.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 


########
#Step 2#
########

hourlybins <- RAD %>%
  dplyr::mutate(
    # Label as radar detection all locations with radar emission level higher than 0
    radar_detection = if_else(Radar_level > 0, TRUE, FALSE),
    # substract hour and date
    Hour = lubridate::hour(time),
    Date = lubridate::date(time)) %>%
  # arrange by time
  dplyr::arrange(organismID, time) %>%
  # group hourly per organism
  dplyr::group_by(colonyName, year, organismID, tripID, Date, Hour) %>%
  summarize(
    # first coordinate
    longitude1 = dplyr::first(longitude),
    latitude1 = dplyr::first(latitude),
    # last coordinate
    longitude2 = dplyr::last(longitude),
    latitude2 = dplyr::last(latitude),
    # sum of registers with radar detections
    radar_detection = sum(radar_detection)
  ) %>%
  dplyr::mutate(
    radar_presence = if_else(radar_detection >= radar_v, 1, 0)
  )


########
#Step 3#
########

# write dataset

colonysites <- unique(hourlybins$colonyName)

for (i in seq_along(colonysites)){
  
  colony <-  colonysites[[i]]
  
  # read radar data
  radar_group <- hourlybins %>%
    #filter colony group
    dplyr::filter(colonyName == colony)
  
  # write dataset
  fwrite(radar_group, file=paste0(WD,"/output/",colonysites[i],"_HourlyBins_radar_L3.csv"),row.names=FALSE)}



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
files <- list.files(path = paste0(WD, "output/"), pattern = "*trips_L2.csv", recursive = TRUE)

# Read all files
GPS <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 

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
  dataGroup <- hourlybins %>% dplyr::filter(colonyName == colonysites[i])      
  
  # parse to factor and create a scale color palette 
  dataGroup$radar_presence = as.factor(dataGroup$radar_presence)
  
  cols_typeLoc <- scale_colour_manual(
    name="radar_presence",
    values = c("0"= "blue",
               "1"= "red"))
  
  # plot per trip
  tripvector <- unique(dataGroup$tripID)

  plotlist <- list()
  for (p in 1:length(tripvector)){
    
    # p = 4
    
    trip <-  GPS %>%
      dplyr::filter(tripID == tripvector[p])
    
    radar_trip <-  dataGroup %>%
      dplyr::filter(tripID == tripvector[p]) %>%
      dplyr::rename(latitude = latitude1,
                    longitude = longitude1)
    
    # plot
    tripZoom <- ggplot() +
      # land mask
      geom_sf(data = msk_sf) +
      # add tracks
      geom_path(data = trip, aes(x = longitude, y = latitude), colour = "black") +      
      geom_point(data = trip, aes(x = longitude, y = latitude), colour = "black", size=1, alpha= 0.5) +
      geom_point(data = radar_trip, aes(x=longitude, y=latitude, colour =  radar_presence),alpha= 0.5, size=2) +
      cols_typeLoc +
      # set spatial bounds
      coord_sf(xlim = range(trip$longitude), ylim = range(trip$latitude), expand=T) +
      # colony location
      geom_point(data = colony, aes(x=Longitude, y=Latitude), size=3, shape=24, fill = "yellow") + 
      # theme
      theme_classic() +
      ggtitle(paste0("First coordinate - hourly bins"))
    
    # list plots
    
    plotlist[[p]] <- list(tripZoom)
    
  }
  
  plotlist_f <- flatten(plotlist)
  
  setwd(paste0(WD,"/output/plots/"))
  ggsave(
    filename = paste0("hourlybins_",colonysites[i],"_L3.pdf"), 
    plot = gridExtra::marrangeGrob(grobs=plotlist_f, nrow=4, ncol=2), 
    width = 9, height = 15)
  
}
