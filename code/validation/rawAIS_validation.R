# -----------------------------------------------------
# Read radar detections with vessel traffic information
# -----------------------------------------------------

# read radar detections with GFW and AIS data associated

predicted <- read.csv(paste0(WD, "GitData/Bird-borne-radar-detection/output/radarDetsummary.csv")) %>%
  dplyr::select(time, organismID, deploymentID, colonyName, year, longitude, latitude, tripID, radarID, GFWovr, GFWlevel, AISlevel, Risk, radarID2)   %>%
  mutate(sp = recode(colonyName, 
                     "CalaMorell" = "CALDIO",
                     "CVelho" = "CALEDW",
                     "MClara" = "CALBOR",
                     "Veneguera" = "CALBOR"))

# relabel risk if GFWovr is equal to 1
predicted$Risk = ifelse(predicted$GFWovr == 1, "null", predicted$Risk)

# sample size
predicted %>%
  dplyr::group_by(sp, Risk, year) %>%
  summarize(n=n())  %>%
  pivot_wider(names_from=Risk, values_from=n)

# prepare input
predicted <- predicted %>% 
  # remove species with no high IUU observations 
  dplyr::filter(sp != "CALDIO") %>%
  # remove null risk
  dplyr::filter(Risk != "null")

# sample size
predicted %>%
  dplyr::group_by(sp, Risk, year) %>%
  summarize(n=n())  %>%
  pivot_wider(names_from=Risk, values_from=n)

# add a id row
predicted <- predicted %>%
  dplyr::mutate(ObsID = row_number())

predicted %>% dplyr::filter(Risk == "high") %>% group_by(sp, year) %>% summarize(n=length(unique(radarID2)))

# -----------------------------------------------------
# Let's find match with raw AIS
# -----------------------------------------------------

#-----------------#
# Prepare cluster #
#-----------------#

# let's %dopar% per year
years <- unique(predicted$year)

cores <- length(year) #Define number of cores
cl <- makeCluster(cores)
registerDoParallel(cl)

#-----------------------#
# Parallel foreach loop #
#-----------------------#

l <- foreach(y=seq_along(years), .packages=c("dplyr" ,"tidyverse","data.table", "lubridate", "purrr", "geosphere")) %dopar% {
  
  #y=2
  
  # filter data
  data <- predicted %>%
    dplyr::mutate(year = as.numeric(year)) %>%
    dplyr::filter(year == years[y])
  
  # read AIS data
  AIS <- readRDS(paste0("D:/GitData/mava-ais/output/data4interaction/", years[y], "AIS.rds"))
  
  list_data <- list()
  
  for (i in 1:nrow(data)){
    
    print(i)
    
    #i=11
    
    ss <- data %>% slice(i)
    
    # time
    timeInput <- lubridate::ymd_hms(ss$time)
    # date
    dateInput <- as.Date(timeInput)
    
    class(dateInput)
    class(AIS$day)
    
    # filter AIS
    AISss <- AIS %>% 
      filter(day == dateInput) %>%
      #dplyr::filter(loctype == "raw") %>%
      filter(time > timeInput-300 & time < timeInput + 300) %>%
      dplyr::select(mmsi, longitude, latitude) 
    
    if(nrow(AISss) > 0){
      
      # return distance, in meters.
      AISss$dist <- geosphere::distGeo(p1 = c(ss$longitude, ss$latitude), p2 = as.matrix(AISss[,c("longitude", "latitude")]))  
      
      # filter less than 5 km
      AISss <- AISss %>% dplyr::filter(dist <= 5000)
      
      if(nrow(AISss)>0){
        output <- AISss %>%
          dplyr::select(mmsi) %>%
          dplyr::distinct() %>%
          dplyr::mutate(ObsID = paste(ss$ObsID),
                        mmsi = as.character(mmsi))
      }
      if(nrow(AISss) == 0){
        output <- tibble(
          ObsID = paste(ss$ObsID),
          mmsi = "nodata"
        )
      }
    } else {
      output <- tibble(
        ObsID = paste(ss$ObsID),
        mmsi = "nodata"
      )
    }
    
    output
    
    list_data[i] <- list(output)
    
  }
  
  # unlist
  final <- do.call(bind_rows,list_data)
  
  # mmsi
  ships <- unique(final$mmsi)
  
  # Read AIS summary file
  summary <- read_csv(paste0(WD, "GitData/West-Africa-seabird-fishery/input/AISsummary/", 
                             years[y], "AISsummary.csv")) %>%
    dplyr::select(mmsi, vesseltype) %>%
    dplyr::mutate(mmsi = as.character(mmsi)) %>%
    dplyr::filter(mmsi %in% ships)
  
  if(nrow(summary)>0){
    
    # recode non-fishing per nonfishing
    summary$vesseltype = gsub("-", "", summary$vesseltype)
    
    # merge data
    final <- left_join(final, summary, by = "mmsi")
  
  }
  
  # print it in foreach
  final
  }

#---------------------------------------------------------------
# Stop cluster
#---------------------------------------------------------------
stopCluster(cl)

table(predicted$year)

final_output <- do.call(bind_rows, l)

nrow(final_output) == nrow(predicted)

# merge with original data
final_output$ObsID = as.character(final_output$ObsID)
predicted$ObsID = as.character(predicted$ObsID)

final_output <- left_join(final_output, predicted, by = "ObsID")

rm(predicted)

saveRDS(final_output, paste0(WD,"GitData/Bird-borne-radar-detection/output/matchRawIntAIS.rds")) 

length(unique(final_output$radarID2))

#---------------------------------------------------------------
# Summarize
#---------------------------------------------------------------

final_output <- readRDS(paste0(WD,"GitData/Bird-borne-radar-detection/output/matchRawIntAIS.rds")) 

# summary per radarID

final_output <- final_output %>%
   dplyr::mutate(vesseltype = if_else(mmsi == "nodata", "nodata", vesseltype)) 


sz <- final_output %>%
  dplyr::select(Risk, vesseltype, radarID2, sp, year) %>%
  dplyr::mutate(value = "1") %>%
  distinct() %>%
  pivot_wider(names_from=vesseltype, values_from=value) 

names(sz) # there are no match with fishing vessels

sz <- sz %>%
  dplyr::mutate(match = if_else(!is.na(nonfishing), "nonfishing", "nodata")) %>%
   dplyr::group_by(Risk, match, sp) %>%
   dplyr::summarise(n=length(unique(radarID2)))  %>%
   pivot_wider(names_from=match, values_from=n)

sz   
  