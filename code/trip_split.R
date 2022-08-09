#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
#1. Make a vector of deployment identifications desired
#2. Read raw data and filter deployments desired
#3. Remove pseudoduplicates
#4. Remove overspeeded locations
#5. Interpolate locations each 5 minutes
#6. Split trips
#7. Remove group of consecutive interpolated locations larger than a threshold
#8. Write output
#9. Plot trips
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

deployment_v <- fread(file=paste0(WD,"output/filenames_L0.csv")) %>%
  # select deploymentID 
  dplyr::select(deploymentID) %>%
  pull(deploymentID)


########
#Step 2#
########

# Make a list of available csv files 
setwd(paste0(WD,"input/GPS"))
files <- list.files(pattern = '.csv') 

# parse to dataframe
files <- as.data.frame(files)

files <- files %>%
  rename(filename = files)  %>% 
  # construct deploymentID
  separate(filename, into = c("organismID", "recovery"), sep="_", extra = "drop", remove = F) %>%
  dplyr::mutate(
    deploymentID = paste(organismID, recovery, sep = "_")
  ) %>%
  # filter deploymentID desired
  dplyr::filter(deploymentID %in% deployment_v) %>%
  pull(filename)

# apply the function to import all files
GPS <- map_df(files, gps_import)

# check that dates have sense
tt <- GPS %>%
  dplyr::mutate(month = month(time),
                year = year(time)) %>%
  dplyr::group_by(colonyName, year, month) %>%
  summarize(n=length(unique(deploymentID)))

# view(tt)

rm(tt)


#-----------------#
# Prepare cluster #
#-----------------#

# let's %dopar% per colony site 
colonysites <- unique(c$colonyName)

cores <- length(colonysites) #Define number of cores
cl <- makeCluster(cores)
registerDoParallel(cl)

#-----------------------#
# Parallel foreach loop #
#-----------------------#

#for(i in seq_along(colonysites)) {
  
#  print(colonysites[i])

foreach(i=1:length(colonysites), .packages=c("track2KBA", "dplyr" ,"sf", "SDLfilter", "gridExtra", "tidyverse","cowplot", "data.table", "lubridate", "move", "moveVis", "purrr")) %dopar% {
  
  # i = 1
  
  #---------------------------------------#
  # Set the colony coordinates and extent #
  #---------------------------------------#
  
  c_i <- c %>%
    dplyr::filter(colonyName == colonysites[i]) %>%
    dplyr::rename(Longitude = longitude,
                  Latitude = latitude)
  
  colony <- c_i %>%
    dplyr::select(Longitude, Latitude)
  
  extent <- coord_sf(xlim = c(c_i[1,'extent_r'],
                              c_i[1,'extent_l']), 
                     ylim = c(c_i[1,'extent_b'],
                              c_i[1,'extent_t']))
 
                                               
  # filter the colony
  dataGroup <- GPS %>% dplyr::filter(colonyName == colonysites[i])      
  
  
  #---------------------#
  # Cleaning trajectory #
  #---------------------#
  
  deployment_v <- unique(dataGroup$deploymentID)
  
  #deployment_v <- deployment_v[20:21]
  dataGroup_interpolation <- list()
  
  for (d in seq_along(deployment_v)){#seq_along(deployment_v)
    
    #d = 3
     print(paste0("d=",d))
    # select deployment
    
    deployment_ss <- dataGroup %>%
      dplyr::filter(deploymentID == deployment_v[d])
    
    # Need to add this information to remove near-duplicate positions and spikes 
    
    deployment_ss$argosLC <- "G"
    
    ########
    #Step 4#
    ########
    
    #---------------------------------#
    # Remove near-duplicate positions #
    #---------------------------------#
    
    deployment_ss <- filter_dup(data = deployment_ss, step.time = step.time_v, step.dist = step.dist_v)
    
    ########
    #Step 5#
    ########
    
    #------------------------------#
    # Remove spikes as high speeds #
    #------------------------------#
  
    deployment_ss <- filter_speed(data = deployment_ss, vmax = vmax_v, method = method_v)   
    
    ########
    #Step 6#
    ########
    
    #-----------------------#
    # Interpolate locations #
    #-----------------------#
    
    # Convert to a move object
    
    GPS_dep <- moveVis::df2move(deployment_ss,
                                proj = "+proj=longlat +datum=WGS84", 
                                x = "longitude", 
                                y = "latitude", 
                                time = "time", 
                                track_id = "deploymentID")
    
    # Visualization of the interpolation
    
    # Plot in red where the timestamps will be located
    plot(GPS_dep$x,GPS_dep$y, col="black",pch=20)
    points(map<-move::interpolateTime(GPS_dep,time=as.difftime(IntThr_v, units="mins"), spaceMethod='greatcircle'), col="red", pch=21)
  
    
    # interpolate locations providing a time interval
    GPS_dep <- move::interpolateTime(GPS_dep, 
                                     time = as.difftime(IntThr_v, units="mins"),
                                     spaceMethod='greatcircle')
    
    # parse to dataframe
    
    GPS_dep_df <-as.data.frame(GPS_dep@coords)
    GPS_dep_df$time<- GPS_dep@timestamps
    

    # reshape dataset
    GPS_dep <- GPS_dep_df %>% 
      dplyr::select(coords.x1,coords.x2,time)%>%
      dplyr::rename(longitude = coords.x1,
                    latitude = coords.x2) %>%
      dplyr::mutate(
        organismID = unique(deployment_ss$organismID),
        deploymentID = unique(deployment_ss$deploymentID),
        colonyName = unique(deployment_ss$colonyName))
    
    # Label as original or interpolate position
    
    # interval
    timeinterval <- deployment_ss %>%
      dplyr::mutate(int = interval(time - TimeThr_v, time + TimeThr_v)) %>%
      pull(int)
    
    # loop it
    
    list_deployment_ss <- list()
    
    for (o in 1:nrow(GPS_dep)){
      #o = 75
      
      test <- GPS_dep %>%
        slice(o) %>%
        dplyr::mutate(
          type = if_else(sum(time %within% timeinterval) > 0, "raw", "interpolated"))
      
      list_deployment_ss[o] <- list(test)
      
    }

    locs <- do.call(rbind.data.frame, list_deployment_ss)
    dataGroup_interpolation [d] <- list(locs)
    
  }
  
  dataGroup <- do.call(rbind.data.frame, dataGroup_interpolation)
  

  ########
  #Step 7#
  ########
  
  #-------------------------------#
  # Split trajectories into trips #
  #-------------------------------#
  
  # prepare dataset 
  dataGroup  <- dataGroup  %>%
    # rename vars for tripSplit function
    rename(DateTime = time, 
           ID = deploymentID,
           Longitude = longitude,
           Latitude = latitude) 
  
  # innerBuff: here you specify the radius around the colony that if the bird enters we consider it has "returned to the nest"
  # returnBuff: trips getting closer than this distance (in km) to the colony will not considered incomplete. Further than this distance, they will
  # duration: trips lasting less than this time (in hours) will not be considered foraging trips
  # nests: set this to TRUE if you have a specific location for each nest
  
  splitted <- tripSplit(dataGroup, colony, innerBuff = innerBuff_v, returnBuff = returnBuff_v, duration = duration_v, nests = nests_v)
  
  # turn GPS to sf data
  trips_sf <- st_as_sf(splitted)
  
  # turn GPS to sf data
  trips_df <- as.data.frame(trips_sf)
  
  # reshape
  trips_df <- trips_df %>%
    dplyr::select(-c(X,Y,geometry)) %>%
    rename(deploymentID = ID,
           longitude = Longitude,
           latitude = Latitude,
           time = DateTime) 
  
  ########
  #Step 8#
  ########
  
  interpolateFilter <- trips_df %>%
    dplyr::filter(type == "interpolated")
  
  # label consecutive interpolated locations that were at least 1 hour larger
  
  interpolateFilter$gapID <- timedif.segment(interpolateFilter$time, SplitThr_v) # 1  HOUR THR.
  
  # summarize interval time of each gap
  
  interpolateFilter_sz <- interpolateFilter %>%
    dplyr::group_by(gapID) %>%
    summarize(
      duration = as.numeric(difftime(max(time),min(time)), units = "hours")
    ) %>%
    dplyr::filter(
      duration < SplitThr_v
    ) %>%
    pull(gapID)
  
  # select those interpolate events 
  interpolateFilter <- interpolateFilter %>%
    dplyr::filter(gapID %in% interpolateFilter_sz) %>%
    dplyr::select(time) %>%
    mutate(keep = T)
  
  # merge to original dataset
  trips_df <- merge(trips_df, interpolateFilter, by = "time", all.x = T)
  
  # rm locations not desired
  trips_df <- trips_df %>%
    dplyr::mutate(test = if_else(type == "interpolated" & is.na(keep), F, T)) %>%
    dplyr::filter(test == T) %>%
    dplyr::select(-c(keep, test))
  
  ########
  #Step 9#
  ########
  
  # write dataset
  fwrite(trips_df, file=paste0(WD,"/output/",colonysites[i],"_trips_L1.csv"),row.names=FALSE)
  
  #########
  #Step 10#
  #########
  
  cols_typeLoc <- scale_colour_manual(
    name="type",
    values = c("raw"= "blue",
               "interpolated"= "red"))

  table(trips_df$tripID)
  
  trips_df <- trips_df %>%
    dplyr::mutate(tripID2 = if_else(tripID == "-1", paste0(deploymentID, tripID), tripID))
    
  tripvector <- unique(trips_df$tripID2)
  
  plotlist <- list()
  for (p in 1:length(tripvector)){
    
    # p = 1
    
    trip <-  trips_df %>%
      dplyr::filter(tripID2 == tripvector[p])
    
    # plot
    tripView <- ggplot() +
      # land mask
      geom_sf(data = msk_sf) +
      # add tracks
      geom_path(data = trip, aes(x = longitude, y = latitude), colour = "red") +
      # set spatial bounds
      extent +
      # theme
      theme_bw() +
      ggtitle(paste0(tripvector[p]))
    
    # plot
    tripZoom <- ggplot() +
      # land mask
      geom_sf(data = msk_sf) +
      # add tracks
      geom_path(data = trip, aes(x = longitude, y = latitude), colour = "black") +
      geom_point(data = trip, aes(x=longitude, y=latitude, colour = type), size=1) +
      cols_typeLoc + 
      # set spatial bounds
      coord_sf(xlim = range(trip$longitude), ylim = range(trip$latitude), expand=T) +
      # colony location
      geom_point(data = colony, aes(x=Longitude, y=Latitude), size=3, shape=24, fill = "yellow") + 
      # theme
      theme_classic() +
      ggtitle(paste0("Returns = " = unique(trip$Returns)))
    
    # arrange 
    
    plot <- cowplot::plot_grid(tripView, tripZoom, 
              ncol = 2, nrow = 1)
    
    # list plots
    
    plotlist[[p]] <- list(plot)
    
  }
  
  plotlist_f <- flatten(plotlist)

  setwd(paste0(WD,"/output/plots/"))
  ggsave(
    filename = paste0("splitTrips_",colonysites[i],"_L1.pdf"), 
    plot = gridExtra::marrangeGrob(grobs=plotlist_f, nrow=4, ncol=1), 
    width = 9, height = 15)
  }

#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------

stopCluster(cl)



