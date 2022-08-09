#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read HourlyBins_radar_L3 radar files 
# 2. substract non-fishing, domestic, others and fishing raster values per location 
# 3. save data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*HourlyBins_radar_L3.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 

# effort months

RAD <- RAD %>%
  mutate(
    month = lubridate::month(Date),
    date = paste0(year, "0", month, "01")
  ) 


effort_months <- RAD %>%
  dplyr::select(date) %>%
  distinct() %>%
  pull()


# effort months available
sAIS_months <- list.files(path = paste0(WD, "input/sAIS")) 

# filter months that coincide with radar data
effort_months <- effort_months[effort_months %in% sAIS_months]

RAD <- RAD %>%
  dplyr::filter(
    date %in% effort_months)
  

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

foreach(i=1:length(colonysites), .packages=c("dplyr" ,"tidyverse","data.table", "lubridate", "purrr", "raster", "sf", "exactextractr")) %dopar% {
  
  # i=1
  print(i)
  
  colony <-  colonysites[[i]]
  
  # read radar data
  
  radar_group <- RAD %>%
    #filter colony group
    dplyr::filter(colonyName == colony) 
  
  # here we will store in a list every 5 km buffer of each radar location within a radarID
  
  output <- list()
  
  for (j in 1:nrow(radar_group)){
    
    #j=1
    
    radar_loc <- radar_group %>%
        slice(j)
      
    # create buffer
    buf <- sf::st_as_sf(radar_loc, coords = c("longitude1", "latitude1"), crs = 4326, agr = "constant") %>% # radar location point
        sf::st_transform(crs = sf::st_crs('+proj=moll')) %>%  # transform to projected
        sf::st_buffer(dist = distance) %>%  # buffer, distance in m
        sf::st_transform(4326) # backtransform to geographic projection
  
    #plot(buf)
    
    # ~~~~~~~~~~~~~~~~~~~ #
    # Fishing vessel data #
    # ~~~~~~~~~~~~~~~~~~~ #
    
    fishing <- raster::raster(paste0(WD,"output/",radar_loc$date[1],"_GFW_NavigationHours.tif"))
    
    navigation_hours_sum <- exactextractr::exact_extract(fishing[[1]], buf, 'sum')
    
    fishing <- raster::raster(paste0(WD,"output/",radar_loc$date[1],"_GFW_FishingHours.tif"))
    
    fishing_hours_sum <- exactextractr::exact_extract(fishing[[1]], buf, 'sum')
    
    # ~~~~~~~~~~~~~~~~~~~~~~~ #
    # Non-fishing vessel data #
    # ~~~~~~~~~~~~~~~~~~~~~~~ #
    
    nonfishing <- raster::raster(paste0(WD,"output/",radar_loc$date[1],"_sAISnonfishing.tif"))
    
    nonfishing_sum <- exactextractr::exact_extract(nonfishing[[1]], buf, 'sum')
    
    # ~~~~~~~~~~~~~~~~~~~~ #
    # Domestic vessel data #
    # ~~~~~~~~~~~~~~~~~~~~ #
    
    domestic <- raster::raster(paste0(WD,"output/",radar_loc$date[1],"_sAISdomestic.tif"))
    
    domestic_sum <- exactextractr::exact_extract(domestic[[1]], buf, 'sum')
    
    # ~~~~~~~~~~~~~~~~~~ #
    # Others vessel data #
    # ~~~~~~~~~~~~~~~~~~ #
    
    other <- raster::raster(paste0(WD,"input/sAIS/",radar_loc$date[1],"/",radar_loc$date[1],"_OTHER_dens.tif"))
    
    other_sum <- exactextractr::exact_extract(other[[1]], buf, 'sum')
    
    # ~~~~~~~~~~~~~~ #
    # sz information #
    # ~~~~~~~~~~~~~~ #
    
    radar_loc$sumNavigatingHours_MonthlyGFW <-   navigation_hours_sum
    radar_loc$sumNonFishing_sAIS <-    nonfishing_sum
    radar_loc$sumFishingHours_MonthlyGFW <-    fishing_hours_sum
    radar_loc$sumDomestic_sAIS <-    domestic_sum
    radar_loc$sumOther_sAIS <-    other_sum
    
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

