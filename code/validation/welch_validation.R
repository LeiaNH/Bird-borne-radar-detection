# ----------------
# read welch data
# ----------------

# metadata can be checked here: https://globalfishingwatch.org/data-download/datasets/public-welch-et-al-disabling-events:v20221102
data <- read.csv(paste0(WD, "GitData/Bird-borne-radar-detection/input/ais_disabling_events.csv")) %>%
  # parse to date
  dplyr::mutate(gap_start_timestamp = lubridate::ymd_hms(substr(gap_start_timestamp,1,19)),
                gap_end_timestamp = lubridate::ymd_hms(substr(gap_end_timestamp,1,19)))

min(data$gap_start_timestamp)
max(data$gap_start_timestamp)

# though, we can only work with 2019 radar locations
data <- data %>%
  dplyr::mutate(year = lubridate::year(gap_end_timestamp)) %>%
  dplyr::filter(year == 2019) 



# ----------------
# read radar data
# ----------------

radar <- read.csv(paste0(WD, "GitData/Bird-borne-radar-detection/output/radarDetsummary.csv")) %>%
  # parse to date format
  dplyr::mutate(time = lubridate::ymd_hms(time)) %>%
  dplyr::filter(
    # only 2019 and in west africa
    year == 2019, 
    colonyName != "CalaMorell",
    # also with high risk
    Risk == "high")

table(radar$colonyName)
length(unique(radar$radarID2))

min(radar$time)
max(radar$time)

# create a vector of dates
radarTimes <- unique(radar$time)
class(radarTimes)

# ----------------
# temporal overlap 
# ----------------

# filter those ais disabling events that happened between radar detections dates
data <- data %>%
  dplyr::mutate(
    int = lubridate::interval(gap_start_timestamp, gap_end_timestamp))

# check if interval overlaps with any of the dates

data$keep <- NA

for(i in 1:nrow(data)) {
  
  print(nrow(data)-i)
  #i=1
  intervalAIS = data %>% dplyr::slice(i) %>% pull(int)
  class(intervalAIS)
  ovr = any(radarTimes %within% intervalAIS)
  data[i,18] = ovr
  }

# filter true overlaps
data <- data %>% dplyr::filter(keep == TRUE)

# --------------
# interpolation 
# --------------

# interpolate locations withing AIS gaps

l <- list()

for(i in 1:nrow(data)){
  
  print(nrow(data)-i)
  #i=50
  
  ss <- data %>% dplyr::slice(i) 
  
  # id gap
  label <- ss$gap_id
  
  # subset
  ss <- tibble(
    x = c(ss$gap_start_lon, ss$gap_end_lon),
    y = c(ss$gap_start_lat, ss$gap_end_lat),
    time = c(ss$gap_start_timestamp, ss$gap_end_timestamp)
  )
  
  # Convert to a move object
  ssMove <- moveVis::df2move(ss,
                             proj = "+proj=longlat +datum=WGS84", 
                             x = "x", 
                             y = "y", 
                             time = "time")
  
  
  # interpolate locations providing a time interval
  ssMove <- move::interpolateTime(ssMove, 
                                  time = as.difftime(5, units="mins"),
                                  spaceMethod='greatcircle')
  
  # parse to dataframe
  AISgaplocs <- as.data.frame(ssMove@coords)
  AISgaplocs$time<- ssMove@timestamps
  
  # reshape dataset
  AISgaplocs <- AISgaplocs %>% 
    dplyr::select(coords.x1,coords.x2,time)%>%
    dplyr::rename(lon = coords.x1,
                  lat = coords.x2) 

  # remove locations outside the radar detections range
  AISgaplocs <- AISgaplocs %>%
    dplyr::filter(lon >= min(radar$longitude) - 3 & lon <= max(radar$longitude) + 3,
                  lat >= min(radar$latitude) - 3 & lat <= max(radar$latitude) + 3 )
  
  if(nrow(AISgaplocs)>0){
    
    AISgaplocs$gap_id <- label
    l[i] <- list(AISgaplocs)
  }
  
  }

dataInt <- data.table::rbindlist(l)

saveRDS(dataInt, paste0(WD,"GitData/Bird-borne-radar-detection/output/intAISgaps.rds")) # aqui he tingut en compte les posicions originals (raw) y les interpol·lades. No obstant, crec que faré servir nomes les raw per no complicar els mètodes

length(unique(dataInt$gap_id))

# --------------
# ST overlap 
# --------------

# Define the extent of the raster
ext <- extent(min(radar$longitude) - 1, 
              max(radar$longitude) + 1, 
              min(radar$latitude) - 1, 
              max(radar$latitude) + 1)

# Define the resolution of the raster
res <- 1

# Define the CRS of the raster
crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Create an empty raster with the defined extent, resolution, and CRS
r <- raster(ext, res, crs=crs)

# Set the values of the raster to NA
values(r) <- NA

r

plot(r)

# read AIS gaps interpolated 
dataInt <- readRDS(paste0(WD,"GitData/Bird-borne-radar-detection/output/intAISgaps.rds"))

# list of interval times 
intervalGAPS <- dataInt %>%
  group_by(gap_id) %>%
  summarise(
    min = min(time),
    max = max(time),
    int = lubridate::interval(min, max)
  ) %>%
  dplyr::select(-min, -max)


# read radar event by radar event

radar_ovr <- list()

iteration <- unique(radar$radarID2)

for(i in seq_along(iteration)){
  
  print(length(iteration)-i)
  #i=175
  
  # radar event label
  radarEv <- iteration[i]
  
  # radar event locations
  radarEvLocs <- radar %>%
    dplyr::filter(radarID2 == radarEv) %>%
    dplyr::select(time, longitude, latitude)
  
  # time
  radarEvInterval <- radarEvLocs %>%
    summarise(
      min = min(time),
      max = max(time),
      int = lubridate::interval(min, max)
    ) %>%
    pull(int)
  
  # Select the intervals from ais gaps that overlap with radar interval
  
  intervals <- unique(intervalGAPS$int)
  overlaps <- intervals[which(int_overlaps(radarEvInterval, intervals))]
  
  # identify those gaps
  intervalGAPS_ss <- intervalGAPS %>% dplyr::filter(int %in% overlaps) %>%
    pull(gap_id)
  
  # -----------------------------------------------------------------------------------------
  # now rasterize gap_id per gap_id and overlap with radar event to see if it match spatially
  # -----------------------------------------------------------------------------------------
  
  match <- list()
  
  for (j in seq_along(intervalGAPS_ss)){
    
    #j=1
    
    # subset
    AISgaplocs <- dataInt %>%
      dplyr::filter(gap_id == intervalGAPS_ss[j])
      
    # convert to trip class
    coordinates(AISgaplocs) = ~ lon + lat
    proj4string(AISgaplocs) <- CRS("+proj=longlat +datum=WGS84")
    
    # if a dataset doesn't contain at least two locations at the same cell 
    test <- tryCatch(trip::trip(AISgaplocs, c("time", "gap_id")), error=function(err) FALSE)
    
    if (class(test) == "trip"){
      
      tr <- trip::trip(AISgaplocs, c("time", "gap_id"))
      
      rGap <- trip::rasterize(tr, r) #  time-stamp between trip locations (in seconds)
      rGap[rGap==0]<-NA
      rGap[rGap>0]<- 1
      rGap <- crop(rGap, extent(r))
      
      # Plot the raster
      plot(rGap)
      
      # Add the coordinates to the plot
      locations <- radarEvLocs %>% dplyr::select(longitude, latitude) %>%
        dplyr::rename(lon = longitude, lat = latitude)
      points(locations$lon, locations$lat, col="red", pch=16)
      
      # Check if coordinates overlap with raster cells
      overlap <- any(!is.na(extract(rGap, locations)))
      
      if(overlap == TRUE){
        
        table <- tibble::tibble(
          gap_id = intervalGAPS_ss[j]
        )
        match[j] <- list(table)
      }
      

    }
    }
  
  match <- do.call(bind_rows, match)
  
  if(nrow(match)>0){
    
     # merge info
     match <- left_join(match, data, by = "gap_id")
     
     # merge radarID
     match$radarID2 = radarEv
     

  }
  
  if(nrow(match) == 0){
    
    # merge radarID
    match <- tibble(
      radarID2 = radarEv,
      gap_id = "none"
    )
    
    
  }
  
  radar_ovr[i] <- list(match)

}


output <- do.call(bind_rows, radar_ovr)


radar <- radar %>%
  dplyr::select(radarID2, organismID, colonyName, Risk)

final <- left_join(output, radar, by = "radarID2")

final <- final %>% distinct()

length(unique(final$radarID2)) == length(iteration)

# parse hours to weeks

hoursWeek <- 24*7   # 1 weeks in hours

final <- final %>%
  mutate(gap_weeks = gap_hours/hoursWeek)


saveRDS(final, paste0(WD,"GitData/Bird-borne-radar-detection/output/RadarAISgapsOVR.rds")) # aqui he tingut en compte les posicions originals (raw) y les interpol·lades. No obstant, crec que faré servir nomes les raw per no complicar els mètodes

table(final$gap_id)




# summary

# numero d'events amb alt IUU risk
length(unique(final$radarID2))

# numero d'events que no han fet match amb cap AIS gap
final %>% dplyr::filter(gap_id == "none") %>% summarize(n=n())

# numero de ais gaps ID relacionats per cada radarID2
final %>% 
  filter(gap_id != "none") %>%
  group_by(radarID2) %>%
  summarize(
    gaps = length(unique(gap_id))
  ) %>%
  ungroup() %>%
  group_by(gaps) %>%
  summarize(
    nradarID = length(unique(radarID2))
  )

# events below and over 2 weeks
final %>% 
  filter(gap_id != "none") %>%
  dplyr::select(radarID2, gap_id, gap_weeks) %>%
  distinct() %>%
  ggplot(., aes(gap_weeks)) + geom_histogram() +
  geom_vline(xintercept = 2, linetype="dotted", 
               color = "blue", size=1.5)

# proportion of radar events and ais gaps over and below 2 weeks
final %>% 
  filter(gap_id != "none") %>%
  dplyr::select(radarID2, gap_id, gap_weeks) %>%
  dplyr::mutate(over2weeks = if_else(gap_weeks>2, "y", "n")) %>%
  dplyr::group_by(over2weeks) %>%
  summarize(
    nAISgapsID = length(unique(gap_id)),
    nradarEv = length(unique(radarID2))
  )
  
# proportion of radar events and ais gaps over and below 2 weeks
final %>% 
  filter(gap_id != "none") %>%
  dplyr::select(radarID2, gap_id, gap_weeks, flag, vessel_class) %>%
  dplyr::mutate(over2weeks = if_else(gap_weeks>2, "y", "n")) %>%
  dplyr::group_by(over2weeks, flag, vessel_class) %>%
  summarize(
    nAISgapsID = length(unique(gap_id)),
    nradarEv = length(unique(radarID2))
  )

