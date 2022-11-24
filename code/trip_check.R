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

trips <- unique(GPS$tripID)
chunklist <- list()

for(i in 1:length(trips)){
  
  print(i)
  #i=112
  
  #~~~~~~~~~~~~~~
  # GPS
  #~~~~~~~~~~~~~~
  
  # filter organism
  ss <- GPS %>% dplyr::filter(tripID == trips[i])
  
  # create identification number of gps chunks
  ss$chunk <- timedif.segment(ss$time, 15)
  
  print(table(ss$chunk))
  
  ss$chunk = as.factor(ss$chunk)
  
  if(length(unique(ss$chunk))>1){
    # plot
    ggplot() +
      # add tracks
      geom_path(data = ss, aes(x = longitude, y = latitude), colour = "black") +      
      geom_point(data = ss, aes(x=longitude, y=latitude, colour = chunk), size=2, alpha= 0.5) +
      # set spatial bounds
      coord_sf(xlim = range(ss$longitude), ylim = range(ss$latitude), expand=T) +
      # theme
      theme_classic() +
      ggtitle(paste0("GPS"))
  }
  
  ss <- ss %>%
    group_by(chunk) %>%
    summarise(
      difftime = as.numeric(difftime(max(time), min(time), units = "days"))) 
  
  # working days of the tag
  working <- sum(ss$difftime)
  
  # trip length
  total <- GPS %>%
    dplyr::filter(tripID == trips[i]) %>%
    summarize(
      duration_days = as.numeric(difftime(max(time), min(time), units = "days"))) %>%
    pull(duration_days)
  
  # summarizing info
  gps_sz <- tibble(tripID = trips[i], 
               total_days_gps = total, 
               functioning_days_gps = working)

  
  #~~~~~~~~~~~~~~
  # RADAR
  #~~~~~~~~~~~~~~
  
  # filter organism
  ss <- RADAR %>% dplyr::filter(tripID == trips[i])
  
  # create identification number of gps chunks
  ss$chunk <- timedif.segment(ss$time, 15)
  
  print(table(ss$chunk))
  
  ss$chunk = as.factor(ss$chunk)
  
  if(length(unique(ss$chunk))>1){
    # plot
    ggplot() +
      # add tracks
      geom_path(data = ss, aes(x = longitude, y = latitude), colour = "black") +      
      geom_point(data = ss, aes(x=longitude, y=latitude, colour = chunk), size=2, alpha= 0.5) +
      # set spatial bounds
      coord_sf(xlim = range(ss$longitude), ylim = range(ss$latitude), expand=T) +
      # theme
      theme_classic() +
      ggtitle(paste0("RADAR"))
  }
  
  ss <- ss %>%
    group_by(chunk) %>%
    summarise(
      difftime = as.numeric(difftime(max(time), min(time), units = "days"))) 
  
  # working days of the tag
  working <- sum(ss$difftime)
  
  # trip length
  total <- GPS %>%
    dplyr::filter(tripID == trips[i]) %>%
    summarize(
      duration_days = as.numeric(difftime(max(time), min(time), units = "days"))) %>%
    pull(duration_days)
  
  # summarizing info
  rad_sz <- tibble(tripID = trips[i], 
                   total_days_rad = total, 
                   functioning_days_rad = working)
  
  #~~~~~~~~~~~~~~
  # SUMMARIZED
  #~~~~~~~~~~~~~~
  
  # merge both data 
  sz <- merge(gps_sz, rad_sz, by="tripID", all.x = T)
  
  chunklist[i] <- list(sz)
}

chunks <- do.call(bind_rows, chunklist)


# read summarized trips and merge info 


# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*_sumTrips.csv", recursive = TRUE)

# Read all files
summtrips <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

summtrips <- merge(summtrips, chunks, by="tripID", all.x=TRUE)

# add colony name
labels <- GPS %>%
  dplyr::select(colonyName, tripID) %>%
  distinct()

# merge info
summtrips <- merge(summtrips, labels, by="tripID", all.x= T)

# summarize info option a
sz <- summtrips %>%
  dplyr::mutate(
    percentGPSworking = (functioning_days_gps/total_days_gps)*100,
    percentRADworking = (functioning_days_rad/total_days_rad)*100,
    RADworking_trip = (functioning_days_rad/total_days_gps)*100
  ) %>%
  summarize(
    mean_percentGPSworking = round(mean(percentGPSworking),1),
    sd_percentGPSworking = round(sd(percentGPSworking),1),
    mean_percentRADworking = round(mean(percentRADworking),1),
    sd_percentRADworking = round(sd(percentRADworking),1),
    mean_RADworking_trip = round(mean(RADworking_trip),1),
    sd_RADworking_trip = round(sd(RADworking_trip),1)
  )

write.csv(sz, paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/general_radartag_functioning.csv"))

# summarize info option b
sz <- summtrips %>%
  dplyr::mutate(
    percentGPSworking = (functioning_days_gps/total_days_gps)*100,
    percentRADworking = (functioning_days_rad/total_days_rad)*100,
    RADworking_trip = (functioning_days_rad/total_days_gps)*100
  ) %>%
  group_by(colonyName) %>%
  summarize(
    mean_percentGPSworking = round(mean(percentGPSworking),1),
    sd_percentGPSworking = round(sd(percentGPSworking),1),
    mean_percentRADworking = round(mean(percentRADworking),1),
    sd_percentRADworking = round(sd(percentRADworking),1),
    mean_RADworking_trip = round(mean(RADworking_trip),1),
    sd_RADworking_trip = round(sd(RADworking_trip),1)
  )

write.csv(sz, paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/radartag_functioning.csv"))

# summarize info option C
sz <- summtrips %>%
  dplyr::mutate(
    percentGPSworking = (functioning_days_gps/total_days_gps)*100,
    percentRADworking = (functioning_days_rad/total_days_rad)*100,
    RADworking_trip = (functioning_days_rad/total_days_gps)*100,
    area = if_else(colonyName == "CalaMorell", "MED", "CCLME")
  ) %>%
  group_by(area) %>%
  summarize(
    mean_percentGPSworking = round(mean(percentGPSworking),1),
    sd_percentGPSworking = round(sd(percentGPSworking),1),
    mean_percentRADworking = round(mean(percentRADworking),1),
    sd_percentRADworking = round(sd(percentRADworking),1),
    mean_RADworking_trip = round(mean(RADworking_trip),1),
    sd_RADworking_trip = round(sd(RADworking_trip),1)
  )


#write.csv(sz, paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/radartag_functioning.csv"))


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
  
  setwd(paste0(WD,"GitData/Bird-borne-radar-detection/output/plots/"))
  ggsave(
    filename = paste0("splitTrips_II_",colonysites[i],"_L2.pdf"), 
    plot = gridExtra::marrangeGrob(grobs=plotlist_f, nrow=4, ncol=1), 
    width = 9, height = 15)
  
}
