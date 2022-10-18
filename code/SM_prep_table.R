#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# RADAR EVENTS WITHIN AND OUTSIDE FORAGING GROUNDS AT LEVEL TRIP
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
  
  trips <-  unique(radar_pop$tripID)
  
  ovr_l <- list()
  
  for (j in seq_along(trips)){
    
    #print(j)
    #j=1
    
    # subset coordinates
    radar_group <- radar_pop %>%
      dplyr::filter(tripID == trips[j])
    
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
      tripID = unique(radar_group$tripID),
      n_radarevents = total, 
      n_radarevents_ca = total_ca,
      percent_radarevents_ca = round(((total_ca/total)*100),1))
    
    ovr_l[j] <- list(ovr)
    
    
  }
  
  ovr_output <- do.call(bind_rows, ovr_l)
  
  sz[i] <- list(ovr_output)
  
  
}

table1 <- do.call(bind_rows, sz)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Trips recorded per deployment 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*_sumTrips.csv", recursive = TRUE)

# Read all files
table2 <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# deployments
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

table3 <- read_excel(paste0(WD,"GitData/Bird-borne-radar-detection/input/matrix_gps_deployment/GPS_tracking_deployments_radar_2018_2020_ss.xlsx"), 
                     sheet = "datav2") %>%
  dplyr::select(Colony, Year, Species, Ring, Breeding_phase, Deployment_Date, Recovery, Recovery_Date) %>%
  dplyr::mutate(deploymentID = paste0(Ring, "_rec",
                                      substr(as.character(Recovery_Date),9,10),
                                      substr(as.character(Recovery_Date),6,7),
                                      Year))

table(table3$Recovery)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# merge
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

head(table1)
head(table2)

table_1_2 <- merge(table1, table2, by = "tripID", all= T)

head(table3)

nchar(table_1_2$tripID)

table_1_2 <- table_1_2 %>%
  dplyr::mutate(deploymentID = substr(tripID,1,19))


table_1_2_3 <- merge(table_1_2, table3, by = "deploymentID", all = T)

table_1_2_3 <- table_1_2_3 %>%
  dplyr::mutate(ring2 = substr(deploymentID,1,7))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# add sex
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

sex <- read_csv2(paste0(WD,"GitData/Bird-borne-radar-detection/input/matrix_gps_deployment/Sexat_Molecular.csv")) %>%
  drop_na(Sex) %>%
  dplyr::rename(ring2 = Ring)

table_1_2_3 <- merge(table_1_2_3, sex, by = "ring2", all.x = T)
 
str(table_1_2_3)

output <- table_1_2_3 %>%
  rename(Organism = ring2) %>%
  dplyr::mutate(Data = if_else(is.na(tripID), "0", "1")) %>%
  dplyr::select(Organism, Colony, Species, Sex, Year, deploymentID, Deployment_Date, Recovery, Data, Recovery_Date, Breeding_phase, tripID, n_radarevents, n_radarevents_ca, percent_radarevents_ca, complete)

glimpse(output)

output$Breeding_phase = gsub("2","Incubation",output$Breeding_phase)
output$Breeding_phase = gsub("3","Chick rearing",output$Breeding_phase)
output$Sex = gsub("1","Male",output$Sex)
output$Sex = gsub("2","Female",output$Sex)
output$Species = gsub("CALDIO","Scopoli's shearwater",output$Species)
output$Species = gsub("CALBOR","Cory's shearwater",output$Species)
output$Species = gsub("CALEDW","Cabo Verde shearwater",output$Species)

write.csv(output, paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/summarytable.csv"), row.names = F)
