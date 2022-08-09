#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read _HourlyBins_radar_evaluation radar files 
# 2. substract light and week values
# 3. save data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*HourlyBins_radar_evaluation.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 


########
#Step 2#
########

#-----------------#
# Prepare cluster #
#-----------------#

# let's %dopar% per colony site 
colonysites <- unique(RAD$colonyName)

cores <- length(colonysites) #Define number of cores
cl <- makeCluster(cores)
registerDoParallel(cl)

#-----------------------#
# Parallel foreach loop #
#-----------------------#

foreach(i=1:length(colonysites), .packages=c("dplyr" ,"tidyverse","data.table", "lubridate", "purrr", "maptools", "sp")) %dopar% {
  
  # i=1
  print(i)
  
  colony <-  colonysites[[i]]
  
  # read radar data
  
  radar_group <- RAD %>%
    #filter colony group
    dplyr::filter(colonyName == colony) 
  
  # here we will store in a list every radar location temporal covar
  
  output <- list()
  
  for (j in 1:nrow(radar_group)){
    
    #j=1
    
    radar_loc <- radar_group %>%
      slice(j)
    
    # ~~~~~~~~~~~~~~~ #
    # Daylight effect #
    # ~~~~~~~~~~~~~~~ #
    
    # subset timestamp
    timestamp <- lubridate::ymd_hms(paste(radar_loc$Date, radar_loc$Hour, ":00:00"))
    
    # subset de coordinates
    coords <- radar_loc %>% dplyr::select(longitude1, latitude1)
    
    # parse to sp object
    loc <- sp::SpatialPoints(coords, proj4string=sp::CRS("+proj=longlat +datum=WGS84"))
    
    # identify sunrise time
    sunrise <- maptools::sunriset(loc, timestamp, direction="sunrise", POSIXct.out=TRUE)
    
    # identify sunset time
    sunset <- maptools::sunriset(loc, timestamp, direction="sunset", POSIXct.out=TRUE)
    
    # label as daylight or night
    
    if(timestamp > sunrise$time & timestamp < sunset$time ){
      
      radar_loc$Light <- "daylight"
      
    } else {
      
      radar_loc$Light <- "night"
    }
    
    # ~~~~~~~~~~~ #
    # Week effect #
    # ~~~~~~~~~~~ #
    
    # Find Numeric Day of Week (Assuming Week Starts on Monday)
    radar_loc$Week <- as.numeric(lubridate::wday(timestamp, week_start=1))
    
    radar_loc$Week <- if_else(radar_loc$Week > 5, "weekend", "weekday")
     
    # list output
    output[j] <- list(radar_loc)
  }
  
  final_output <- do.call(bind_rows, output)
  
  ########
  #Step 3#
  ########
  
  # write dataset
  fwrite(final_output, file=paste0(WD,"/output/",colonysites[i],"_HourlyBins_radar_evaluation.csv"),row.names=FALSE)
  
}


#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)   
