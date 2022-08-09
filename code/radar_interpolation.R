#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read _L1 GPS files
# 2: read raw Radar data
# 3: interpolate radar data location
# 4: calculate per each radar scan time diff from previous and after 
# 5: label those radar scans interpolated between GPS locations that are 'threshold' minutes apart from radar scan timestamp
# 6. save data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

##########################
# general objects needed #
##########################

# load colony locations

c <- read.csv2(paste0(WD,"input/colonysites.csv"))

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

# setwd
setwd(paste0(WD, "input/RADAR"))

# List radar raw files
files <- list.files(path = paste0(WD, "input/RADAR"), pattern = "*.csv", recursive = TRUE)

# apply the function to all files
RAD <- map_df(files, radar_import)

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

#-----------------------#
# Parallel foreach loop #
#-----------------------#

l <- foreach(i=1:length(colonysites), .packages=c("dplyr" ,"tidyverse","data.table", "lubridate", "purrr", "moveVis", "move")) %dopar% {
  
#for (i in seq_along (colonysites)){
  
  # i=1
  print(i)
  
  colony <-  colonysites[[i]]
  
  # read radar data
  
  radar_group <- RAD %>%
    #filter colony group
    dplyr::filter(colonyName == colony)
  
  # list deploymentID
  
  deployments <- unique(radar_group$deploymentID)
  
  # empty list to save the outputs
  deployment_level <- list()
  
  for (j in seq_along(deployments)){
    
    # j=1
    
    if ( deployments[[j]] %in% unique(GPS$deploymentID) ) {
      #--------------------------------
      # Filter GPS file deployment
      #--------------------------------
      
      GPS_dep <- GPS %>%
        # filter desired deploymentID
        dplyr::filter(deploymentID == deployments[[j]]) 
      
      # empty list to save the outputs
      trip_level <- list()
      
      trips <- unique(GPS_dep$tripID)
      
      for (k in seq_along(trips)){
        
        print(k)
        #k = 1
        
        GPS_trip <- GPS_dep %>%
          # filter desired tripID
          dplyr::filter(tripID == trips[[k]]) 
        
        # Convert to a move object
        GPS_trip <- moveVis::df2move(GPS_trip,
                                  proj = "+proj=longlat +datum=WGS84", 
                                  x = "longitude", 
                                  y = "latitude", 
                                  time = "time", 
                                  track_id = "tripID")
      
        #--------------------------------
        # Filter Radar file deployment
        #--------------------------------
        
        RADAR_dep <- radar_group %>% 
        # filter desired deploymentID
        dplyr::filter(deploymentID == deployments[[j]]) %>%
        # remove those scans before and after GPS data
        dplyr::filter (time > min(GPS_trip$time) & 
                         time < max(GPS_trip$time)) %>%
        # sometimes more than one radar scan can be registered at same moment. Let's remove those pseudoduplicates.
        dplyr::group_by(time) %>%
        # filter that row with highest radar_level 
        slice(which.max(Radar_level))
      
        # Create a vector with all timestamps
        ts <- as.POSIXct(RADAR_dep$time,  format="%Y-%m-%d %H:%M:%S", tz="UTC")
      
        #--------------------------------
        # Interpolate radar data
        #--------------------------------
      
        # Plot in red where the timestamps will be located
        plot(GPS_trip$x,GPS_trip$y, col="black",pch=20)
        points(map<-move::interpolateTime(GPS_trip, time=ts, spaceMethod='greatcircle'), col="red", pch=21)
      
        # Save a move object from that interpolation
        interpolation <- move::interpolateTime(GPS_trip, time=ts, spaceMethod='greatcircle')
      
        # Parse to data frame
        interpolation <-  methods::as(interpolation, "data.frame")
      
        # Fix vars
        interpolation <- interpolation %>% 
          # select desired columns
          dplyr::select(coords.x1,coords.x2,timestamps) %>% 
          # rename them
          dplyr::rename(longitude=coords.x1, latitude=coords.x2, time=timestamps)
        
        # Join original radar dataset with the longitude and latitude obtained after interpolation
        interpolation <- left_join(RADAR_dep, interpolation, by="time")
      
      # add tripID
      interpolation <- interpolation %>%
        dplyr::mutate(
          tripID = paste0(trips[[k]]))
      
      trip_level[k] <-  list(interpolation)
      }
    }
    alltrips <- do.call(bind_rows,trip_level)
    deployment_level[j] <-  list(alltrips)
  }
  alldeployments <- do.call(bind_rows,deployment_level )
  deployment_level
}

output <- do.call(bind_rows,l)

#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

########
#Step 4#
########

# parse to dataframe 
output <- as.data.frame(output)

#---------------#
#Prepare cluster#
#---------------#

cores <- 5 #Define number of cores
cl <- makeCluster(cores)
registerDoParallel(cl)

#---------------------#
#Parallel foreach loop#
#---------------------#

#next takes a lot of time!

organisms <- unique(output$organismID)

l <- foreach(k=1:length(organisms), .packages=c("tidyverse", "lubridate", "dplyr", "data.table")) %dopar% {
  
  #for (k in 1:length(organisms)){  
  #k=53
  intervalcalc <- list()
  
  output_k <- output %>%
    # filter the dopar var
    dplyr::filter(organismID == organisms[[k]])
  
  for (i in 1:nrow(output_k)){
    
    print(nrow(output_k)-i)
    #i=1
    
    # Read row/scan per row/scan
    scan <- output_k %>% 
      dplyr::slice (i)
    
    # Closest GPS location before scan timestamp
    before <- GPS %>%
      # filter same deployment than scan register
      dplyr::filter (deploymentID == scan[1,'deploymentID']) %>%
      # filter below scan timestamp
      dplyr::filter (time < scan[1,'time']) %>%
      # group by deploymentID
      group_by(deploymentID) %>%
      # slice the max value 
      slice(which.max(time)) %>%
      # parse to vector
      pull(time)
    
    # Closest GPS location after scan timestamp
    after <- GPS %>%
      # filter same deployment than scan register
      dplyr::filter (deploymentID == scan[1,'deploymentID']) %>%
      # filter upper scan timestamp
      dplyr::filter (time > scan[1,'time']) %>%
      # group by deploymentID
      group_by(deploymentID) %>%
      # slice the minimum value 
      slice(which.min(time)) %>%
      # parse to vector
      pull(time)
    
    # Calculate difftime of each radar scan from the previous and following GPS location
    scan <- scan %>%
      dplyr::mutate(
        minBef = as.numeric(difftime(time,
                           before,
                           units="mins")),
        minAft = as.numeric(difftime(after,
                           time,
                           units="mins")))
    
    # list result
    intervalcalc[i] <- list(scan)
  }
  
  output_k <- do.call(bind_rows,intervalcalc)
  
  ########
  #Step 5#
  ########
  
  # Label reliability of radar scan locations
  output_k <- output_k %>%
    dplyr::mutate(
      # if radar scan timestamp is located between GPS over threshold time, label them as FALSE
      reliability = case_when(
        minBef > TimeThr_v & minAft < TimeThr_v ~ T,
        minAft > TimeThr_v & minBef < TimeThr_v ~ T,
        minAft < TimeThr_v & minBef < TimeThr_v ~ T,
        T ~ F)) %>%
    dplyr::filter(reliability == TRUE) %>%
    dplyr::select(-c(reliability,minBef,minAft))
  
  output_k
}

final_output <- do.call(bind_rows, l)

#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)


########
#Step 6#
########

colonysites <- unique(c$colonyName)

for (i in seq_along(colonysites)){
  
  colony <-  colonysites[[i]]
  
  # read radar data
  radar_group <- final_output %>%
    #filter colony group
    dplyr::filter(colonyName == colony) %>%
    #remove duplicates
    distinct()
  
  # write dataset
  fwrite(radar_group, file=paste0(WD,"/output/",colonysites[i],"_radar_L2.csv"),row.names=FALSE)}
