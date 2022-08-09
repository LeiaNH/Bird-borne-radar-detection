#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# Take a look to trips of _L1.pdf extention and
# 1. Read _L1 GPS files
# 2. Remove all -1 tripIDs
# 3. Remove trips with very few raw locations
# 5. Write output
# 4. Plot trips
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

# List L1.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*trips_L1.csv", recursive = TRUE)

# Read all files
GPS <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 

########
#Step 2#
########

GPS <- GPS %>%
  dplyr::filter(tripID != "-1") 

########
#Step 3#
########

trips <- GPS %>%
  group_by(tripID) %>%
  summarize(duration = as.numeric(difftime(max(time), min(time), units = "hours"))) %>%
  dplyr::filter(duration >= duration_v) %>%
  pull(tripID)

GPS <- GPS %>%
  dplyr::filter(tripID %in% trips) 


########
#Step 4#
########

#trips <- c("7501336_rec30072019_01") 

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

foreach(i=1:length(colonysites), .packages=c("dplyr" ,"sf","gridExtra", "tidyverse","cowplot", "data.table", "lubridate", "purrr")) %dopar% {
  
#for (i in seq_along(colonysites)){
  
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
  
  ########
  #Step 2#
  ########

  # write dataset
  fwrite(dataGroup, file=paste0(WD,"/output/",colonysites[i],"_trips_L2.csv"),row.names=FALSE)
  
  ########
  #Step 3#
  ########
  
  cols_typeLoc <- scale_colour_manual(
    name="type",
    values = c("raw"= "blue",
               "interpolated"= "red"))
  
  tripvector <- unique(dataGroup$tripID)
  
  plotlist <- list()
  for (p in 1:length(tripvector)){
    
    # p = 1
    
    trip <-  dataGroup %>%
      dplyr::filter(tripID == tripvector[p])
    
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
    filename = paste0("splitTrips_",colonysites[i],"_L2.pdf"), 
    plot = gridExtra::marrangeGrob(grobs=plotlist_f, nrow=4, ncol=1), 
    width = 9, height = 15)
  
}

#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)