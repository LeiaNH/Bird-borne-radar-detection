#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read events_radar_L3 radar files
# 2. substract mmsi presence per 5 km buffer of each radar detection
# 3. save it
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L3.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*events_radar_L3.csv", recursive = TRUE)

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

foreach(i=1:length(colonysites), .packages=c("dplyr" ,"tidyverse","data.table", "lubridate", "purrr", "raster", "terra")) %dopar% {
 
  # i=1
  print(i)
  
  colony <-  colonysites[[i]]
  
  # read radar data
  
  radar_group <- RAD %>%
    #filter colony group
    dplyr::filter(colonyName == colony)
  
  # here we will store per radar location if any vessel was within the 5 km buffer of each radar location or not
  GFWmatch <- list()
  
  # read row per row
 for (j in 1:nrow(radar_group)){
    print(j)
    # j=10
    
    radar_loc <- radar_group %>%
      slice(j)
    
    # create buffer
    buf <- sf::st_as_sf(radar_loc, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% # radar location point
      sf::st_transform(crs = sf::st_crs('+proj=moll')) %>%  # transform to projected
      sf::st_buffer(dist = distance) %>%  # buffer, distance in m
      sf::st_transform(4326) # backtransform to geographic projection
    
    # ~~~~~~~~~ #
    # fleet data #
    # ~~~~~~~~~ #
    
    # read mmsi data
    fleet <- map_dfr(
      paste0(WD,"input/GFW/fleet-daily-csvs-100-v2/fleet-daily-csvs-100-v2-", 
             radar_loc$year, "/",
             as.Date(ymd_hms(radar_loc$time)), ".csv"), 
      .f = read_csv) 
    
    # Crop study area
    
    fleet <- fleet %>% 
      dplyr::filter(cell_ll_lon > -28 & cell_ll_lon < 7 & 
                      cell_ll_lat >  11 & cell_ll_lat < 46 )
    
    if (nrow(fleet) == 0){
      radar_loc <- radar_loc %>%
        dplyr::mutate(GFWovr = NA)
    }
  
    if (nrow(fleet) > 0){
      
    # Create an ID row per cell
    fleet <- fleet %>% 
      # each cell is defined by longitude and latitude
      group_by(cell_ll_lon, cell_ll_lat) %>%
      dplyr::mutate(
        # that defines an identificator per group_by level
        cellID = cur_group_id()
      ) %>%
      ungroup()
    
    # Select those vars identifying each cell location and id
    raster <- fleet %>% dplyr::select(cell_ll_lon,cell_ll_lat, cellID) 
    
    # Rasterize that info
    raster <- raster::rasterFromXYZ(raster, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    
    # Crop raster with polygon
    raster_extract <- terra::extract(raster, buf, exact = TRUE)
    
    # Substract ID cells intersecting the buffer
    values <- raster_extract[[1]]
    values <- values[!is.na(values)]
    
    # Filter that cells intersecting the buffer
    fleet <- fleet %>%
      dplyr::mutate(
        intersect = if_else(cellID %in% values, T, F))

    # if you want to check how it looks, set QP to TRUE
    QP <- FALSE
    
    if(QP == TRUE){

    # World polygons from rnaturalearthdata
    world <- ne_countries(scale = "medium", returnclass = "sf")
    # open ggplot
    ggplot() +
      # plot and color world land mask  
      geom_sf(data = world, fill= "antiquewhite") +
      # theme 
      theme(
        # color water
        panel.background = element_rect(fill="aliceblue"),
        # plot grid lines
        panel.grid.major = element_line(color = gray(.5), linetype = "dashed", size = 0.5),
        # remove axes title
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
      # plot radar location
      geom_point(
        data = radar_loc,
        aes(x = longitude, y = latitude), 
        colour= "red",
        alpha = 0.5)+  
      # plot buffer around radar location
      geom_sf(data = buf, color="purple", fill=NA,  linetype = 2) +
      # plot GFW data
      geom_raster(data= fleet, aes(x = cell_ll_lon, y = cell_ll_lat, fill= intersect), alpha = 0.5) +
      # define extent of area
      coord_sf(xlim = c(radar_loc$longitude-0.1, radar_loc$longitude+0.1), ylim = c(radar_loc$latitude-0.1,
                                                                                    radar_loc$latitude+0.1))}

    if (sum(fleet$intersect) == 0){
      
      # annotate that no vessel was within 5 km buffer
      radar_loc <- radar_loc %>%
        dplyr::mutate(GFWovr = 0)}
    
    if (sum(fleet$intersect) > 0){
      
      # annotate that at least one vessel was within 5 km buffer
      radar_loc <- radar_loc %>%
        dplyr::mutate(GFWovr = 1)
      
      geartype <- fleet %>%
        dplyr::filter(intersect == TRUE) %>%
        dplyr::select(geartype) %>%
        distinct() %>%
        dplyr::mutate(
          value = 1) %>%
        pivot_wider(names_from = geartype, values_from = value)
      
      flags <- fleet %>%
        dplyr::filter(intersect == TRUE) %>%
        dplyr::select(flag) %>%
        distinct() %>%
        dplyr::mutate(
          value = 1) %>%
        pivot_wider(names_from = flag, values_from = value)
      
      radar_loc <- bind_cols(radar_loc, geartype, flags)
      
      }
    
    }
    GFWmatch[j] <- list(radar_loc)
    
    }
  
  output_match <-  do.call(bind_rows, GFWmatch)
  
  ########
  #Step 3#
  ########
  
  # write dataset
  fwrite(output_match, file=paste0(WD,"/output/",colonysites[i],"_events_radar_GFWovr_L4.csv"),row.names=FALSE)
  
 }
  
#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)   

 