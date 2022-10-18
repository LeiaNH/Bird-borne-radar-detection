#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read events_radar_GFWovr_L4 radar files 
# 2. substract non-fishing, domestic, others and fishing raster values per location 
# 3. summarize marine traffic values per radar event 
# 4. save data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*events_radar_GFWovr_L4.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

# Filter radar events that did not overlap with GFW datasets
# RAD <- RAD %>%
#  dplyr::filter(GFWovr == 0 | is.na(GFWovr))

# effort months

RAD <- RAD %>%
  mutate(
    month = lubridate::month(time),
    date = paste0(year, "0", month, "01")
  ) 

effort_months <- RAD %>%
  dplyr::select(date) %>%
  distinct() %>%
  pull()


# effort months available
sAIS_months <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/input/sAIS")) 

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
    dplyr::filter(colonyName == colony) %>%
    # create new radarID
    dplyr::mutate(radarID2 = paste0(tripID, "_",radarID))
  
  # here we will store in a list every 5 km buffer of each radar location within a radarID
  
  output <- list()
  
  buffs_v <- unique(radar_group$radarID2)
  
  for (j in seq_along(buffs_v)){
    
    #j=1
    
    radar_group_event <- radar_group %>%
      dplyr::filter(radarID2 == buffs_v[j])
  
    sf_list <- list()
    
    for (k in 1:nrow(radar_group_event)){
      
      #k=1
      
      radar_loc <- radar_group_event %>%
        slice(k)
      
      # create buffer
      buf <- sf::st_as_sf(radar_loc, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% # radar location point
        sf::st_transform(crs = sf::st_crs('+proj=moll')) %>%  # transform to projected
        sf::st_buffer(dist = distance) %>%  # buffer, distance in m
        sf::st_transform(4326) # backtransform to geographic projection
      
      sf_list[k] <- list(buf)
      
    }
    
    # group all buffers generated per radar event
    single_sf <- dplyr::bind_rows(sf_list)
    
    # merge all buffers into one
    dissolve_sf <- st_union(single_sf)
    
    #plot(dissolve_sf)
    
    ########
    #Step 3#
    ########
    
    # ~~~~~~~~~~~~~~~~~~~ #
    # Fishing vessel data #
    # ~~~~~~~~~~~~~~~~~~~ #
    
    fishing <- raster::raster(paste0(WD,"GitData/Bird-borne-radar-detection/output/",radar_loc$date[1],"_GFW_NavigationHours.tif"))
    
    navigation_hours_sum <- exactextractr::exact_extract(fishing[[1]], dissolve_sf, 'sum')
    
    fishing <- raster::raster(paste0(WD,"GitData/Bird-borne-radar-detection/output/",radar_loc$date[1],"_GFW_FishingHours.tif"))
    
    fishing_sum <- exactextractr::exact_extract(fishing[[1]], dissolve_sf, 'sum')
    
    # ~~~~~~~~~~~~~~~~~~~~~~~ #
    # Non-fishing vessel data #
    # ~~~~~~~~~~~~~~~~~~~~~~~ #
    
    nonfishing <- raster::raster(paste0(WD,"GitData/Bird-borne-radar-detection/output/",radar_loc$date[1],"_sAISnonfishing.tif"))
    
    nonfishing_sum <- exactextractr::exact_extract(nonfishing[[1]], dissolve_sf, 'sum')
    
    # ~~~~~~~~~~~~~~~~~~~~ #
    # Domestic vessel data #
    # ~~~~~~~~~~~~~~~~~~~~ #
    
    domestic <- raster::raster(paste0(WD,"GitData/Bird-borne-radar-detection/output/",radar_loc$date[1],"_sAISdomestic.tif"))
    
    domestic_sum <- exactextractr::exact_extract(domestic[[1]], dissolve_sf, 'sum')
    
    # ~~~~~~~~~~~~~~~~~~ #
    # Others vessel data #
    # ~~~~~~~~~~~~~~~~~~ #
    
    other <- raster::raster(paste0(WD,"GitData/Bird-borne-radar-detection/input/sAIS/",radar_loc$date[1],"/",radar_loc$date[1],"_OTHER_dens.tif"))
    
    other_sum <- exactextractr::exact_extract(other[[1]], dissolve_sf, 'sum')
    
    # ~~~~~~~~~~~~~~ #
    # sz information #
    # ~~~~~~~~~~~~~~ #
    
    radar_group_event$sumNavigatingHours_MonthlyGFW <-   navigation_hours_sum
    radar_group_event$FishingHours_GFW <-   fishing_sum
    radar_group_event$NonFishing_sAIS <-    nonfishing_sum
    radar_group_event$Domestic_sAIS <-    domestic_sum
    radar_group_event$Other_sAIS <-    other_sum
    
    output[j] <- list(radar_group_event)
  }
  
  final_output <- do.call(bind_rows, output)
  
  ########
  #Step 4#
  ########
  
  # write dataset
  fwrite(final_output, file=paste0(WD,"GitData/Bird-borne-radar-detection/output/",colonysites[i],"_events_radar_unreportedGFW_evaluation.csv"),row.names=FALSE)
  
  }
  
 
#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)   

