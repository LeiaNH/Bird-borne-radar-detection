#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read radar events files 
# 2. read core areas
# 3. Overlap radar events per core area per population
# 4. sz
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*_events_radar_L3.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

# Add population label
RAD <- RAD %>%
  dplyr::mutate(population = recode(colonyName, 
                                    "CalaMorell" = "BalearicIs",
                                    "CVelho" = "CaboVerde",
                                    "MClara" = "CanaryIs",
                                    "Veneguera" = "CanaryIs")) 
########
#Step 2#
########

CA <- readRDS(paste0(WD, "GitData/Bird-borne-radar-detection/output/CoreAreas.rds"))


########
#Step 3#
########

populations <- unique(RAD$population)

sz <- list()

for (i in seq_along(populations)){
  
  #i = 1
  print(i)
  if(populations[[i]] == "BalearicIs"){
    poly <- CA[[1]]}
  
  if(populations[[i]] == "CaboVerde"){
    poly <- CA[[2]]}
  
  if(populations[[i]] == "CanaryIs"){
    poly <- CA[[3]]}
  
  plot(poly)
  
  # subset population 
  radar_pop <- RAD %>% 
    dplyr::filter(population == populations[[i]]) 
  
  individuals <-  unique(radar_pop$organismID)
  
  ovr_l <- list()
  
  for (j in seq_along(individuals)){
    
    #print(j)
    #j=1
  
  # subset coordinates
    radar_group <- radar_pop %>%
        dplyr::filter(organismID == individuals[j])

  coord <- radar_group %>%  
    dplyr::select(longitude,latitude)
  
  # parse to spatialpointsdataframe
  point <- SpatialPointsDataFrame (coord,proj4string=CRS("+proj=longlat +datum=WGS84"), data=radar_group)
  
  # plot it
  plot(point, col="red")
  plot(poly,add=T)
  
  # over function
  over <- over(point, poly)
  
  # merge info
  radar_group <- cbind (radar_group, over)
  
  # check overlap
  point_ovr <- radar_group %>% drop_na(over) %>% dplyr::select(longitude, latitude)
  
  if(nrow(point_ovr)>0) {
  point_ovr <- SpatialPointsDataFrame (point_ovr,proj4string=CRS("+proj=longlat +datum=WGS84"), data=point_ovr)
  plot(point_ovr, col="blue", add=T)}
  
  ########
  #Step 4#
  ########
  
  # summarize number of radar events (partially or completely) within core area 
  total_ca <- radar_group %>%
    group_by(radarID) %>%
    summarize(
      over = sum(over, na.rm= T))
  
  if(sum(total_ca$over) == 0){
    total_ca = 0
  }else{
    
    total_ca <- total_ca %>%
      dplyr::filter(over > 0) %>%
      dplyr::select(radarID) %>%
      distinct() 
    
    total_ca <- length(unique(total_ca$radarID))
  }
  
     
  # summarize number of radar events
  total <- length(unique(radar_group$radarID))
  
  # summarized table
  
  ovr <- tibble(
    population = unique(radar_group$population), 
    individual = unique(radar_group$organismID),
    n_radarevents = total, 
    n_radarevents_ca = total_ca,
    percent_radarevents_ca = round(((total_ca/total)*100),1))
  
  ovr_l[j] <- list(ovr)
  
  
  }
 
  ovr_output <- do.call(bind_rows, ovr_l)
  
  sz[i] <- list(ovr_output)
  
  
}

sz <- do.call(bind_rows, sz)

sz <- sz %>%
  dplyr::group_by(population) %>%
  summarize(
    mean = round(mean(percent_radarevents_ca),1),
    sd = round(sd(percent_radarevents_ca),1),
    n_radarevents = sum(n_radarevents),
    nradarevents_ca = sum(n_radarevents_ca)
  )

sz

fwrite(sz, file=paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/corearea_radarevents_sz.csv"),row.names=FALSE)
