# -----------------------------------------------------
# Read radar detections with vessel traffic information
# -----------------------------------------------------

# read radar detections with GFW and AIS data associated

predicted <- read.csv(paste0(WD, "GitData/Bird-borne-radar-detection/output/radarDetsummary.csv")) %>%
  dplyr::select(time, organismID, deploymentID, colonyName, year, longitude, latitude, tripID, radarID, GFWovr, GFWlevel, AISlevel, Risk) 

# relabel risk if GFWovr is equal to 1
predicted$Risk = ifelse(predicted$GFWovr == 1, "null", predicted$Risk)

# sample size
table(predicted$colonyName, predicted$year)


# let's do it only for CALEDW Curral Velho. Advantages to do so:
# higuest sample size
# only one year to process
# sp with high risk of IUU 

# prepare input

#colonies <- c("CVelho")
colonies <- c("MClara", "Veneguera")

input <- predicted %>% 
  dplyr::filter(colonyName %in% colonies)

# add a id row
input <- input %>%
  dplyr::mutate(ObsID = row_number())

year <- unique(input$year)
#year <- 2020

#sp <- "CALEDW"
sp <- "CALBOR"

# -----------------------------------------------------
# Let's find match with raw AIS
# -----------------------------------------------------

AIS <- readRDS(paste0("D:/GitData/mava-ais/output/data4interaction/", year, "AIS.rds"))

l <- list()

for (i in 1:nrow(input)){
  
  print(i)
  
  #i=2
  
  ss <- input %>% slice(i)
  
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
  
  l[i] <- list(output)
  
  }

final <- do.call(bind_rows,l)

final <- final %>%
  distinct()

final %>%
  dplyr::filter(mmsi == "nodata") %>%
  summarize(
    n = n()
  )

saveRDS(final, paste0(WD,"GitData/Bird-borne-radar-detection/output/matchRawIntAIS",sp, ".rds")) # aqui he tingut en compte les posicions originals (raw) y les interpol·lades. No obstant, crec que faré servir nomes les raw per no complicar els mètodes

#saveRDS(final, paste0(WD,"GitData/Bird-borne-radar-detection/output/matchRawAIS.rds")) # aqui nomes tinc en compte les posicions originals (Raw) AIS, per tant les posicions interpolades no han estat incloses. Ho he fet aixi, perque l'anterior dataset feia match sempre amb alguna embarcació. Entenc que ara no passará. 

# ----------------------------------------------
# Bind vessel summary information
# ----------------------------------------------

data <- readRDS(paste0(WD,"GitData/Bird-borne-radar-detection/output/matchRawIntAIS", sp, ".rds"))

year = 2019
year = 2020

# Read AIS summary file
summary <- read_csv(paste0(WD, "GitData/West-Africa-seabird-fishery/input/AISsummary/", year, "AISsummary.csv")) %>%
  dplyr::select(mmsi, vesseltype) %>%
  dplyr::mutate(mmsi = as.character(mmsi))

table(summary$vesseltype)

summary$vesseltype = gsub("-", "", summary$vesseltype)

# merge data
data <- left_join(data, summary, by = "mmsi")

# sz
data <- data %>%
  dplyr::mutate(vesseltype = if_else(mmsi == "nodata", "nodata", vesseltype)) %>%
  dplyr::select(ObsID, vesseltype) %>%
  distinct() %>%
  dplyr::mutate(value = 1) %>%
  pivot_wider(names_from=vesseltype, values_from=value)

# merge to original data 
input$ObsID = as.numeric(input$ObsID)
data$ObsID = as.numeric(data$ObsID)

output <- left_join(input, data, by = "ObsID")

# summary per radarID

(sz <- output %>%
  dplyr::mutate(
    # create an unique radar ID per trip
    radarID2 = paste0(tripID,"_", radarID)) %>%
  dplyr::group_by(radarID2, Risk) %>%
  dplyr::summarise(nonfishing = sum(nonfishing, na.rm = T),
                   fishing = sum(fishing, na.rm = T),
                   nodata = sum(nodata, na.rm = T)) %>%
  dplyr::mutate(
    nonfishing = if_else(nonfishing>0, 1, 0),
    fishing =if_else(fishing>0, 1, 0),
    nodata = if_else(nodata>0, 1, 0)
    
  ) %>%
  dplyr::group_by(Risk) %>%
  dplyr::summarise(nonfishing = sum(nonfishing, na.rm = T),
                   fishing = sum(fishing, na.rm = T),
                   nodata = sum(nodata, na.rm = T)))
  
dupes <- sz %>% janitor::get_dupes(radarID2)  
