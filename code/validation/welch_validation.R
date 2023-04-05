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
# filtering 
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

# from hours to weeks 
#hoursWeek <- 24*7   # 1 weeks in hours

#data <- data %>%
#  mutate(gap_weeks = gap_hours/hoursWeek)

# Do linear interpolation

l <- list()
for(i in 1:nrow(data)){
  
  print(nrow(data)-i)
  #i=1
  
  ss <- data %>% dplyr::slice(i) 
  
  AISgap <- tibble(
    id = c(ss$gap_id, ss$gap_id),
    x = c(ss$gap_start_lon, ss$gap_end_lon),
    y = c(ss$gap_start_lat, ss$gap_end_lat),
    time = c(ss$gap_start_timestamp, ss$gap_end_timestamp)
  )
  
  # Convert to a move object
  AISgap <- moveVis::df2move(AISgap,
                               proj = "+proj=longlat +datum=WGS84", 
                               x = "x", 
                               y = "y", 
                               time = "time", 
                               track_id = "id")
  
  
  # interpolate locations providing a time interval
  AISgap <- move::interpolateTime(AISgap, 
                                  time = as.difftime(5, units="mins"),
                                  spaceMethod='greatcircle')
  
  # parse to dataframe
  output <- as.data.frame(AISgap@coords)
  output$time<- AISgap@timestamps
  
  # reshape dataset
  output <- output %>% 
    dplyr::select(coords.x1,coords.x2,time)%>%
    dplyr::rename(lon = coords.x1,
                  lat = coords.x2) %>%
    dplyr::mutate(
      gap_id = unique(ss$gap_id),
      gap_hours = unique(ss$gap_hours))
  
l[i] <- list(output)
}

output <- data.table::rbindlist(l)



### overlap

l <- list()

for (i in 1:nrow(radar)){
  
  print(i)
  
  #i=8
  
  ss <- radar %>% slice(i)
  
  # time
  timeInput <- lubridate::ymd_hms(ss$time)
  # date
  dateInput <- as.Date(timeInput)
  
  class(dateInput)
  class(output$time)
  
  # filter AIS
  AISss <- output %>% 
    filter(time > timeInput-60*60 & time < timeInput + 60*60) %>%
    dplyr::select(gap_id, lon, lat, gap_hours) 
  
  if(nrow(AISss) > 0){
    
    # return distance, in meters.
    AISss$dist <- geosphere::distGeo(p1 = c(ss$longitude, ss$latitude), p2 = as.matrix(AISss[,c("lon", "lat")]))  
    
    # filter less than 5 km
    AISss <- AISss %>% dplyr::filter(dist <= 50*1000)
    
    if(nrow(AISss)>0){
      final <- AISss %>%
        dplyr::select(gap_id) %>%
        dplyr::distinct() %>%
        dplyr::mutate(radarID = paste(ss$radarID2))
    }
    if(nrow(AISss) == 0){
      final <- tibble(
        ObsID = paste(ss$radarID2),
        gap_id = "nodata"
      )
    }
  } else {
    final <- tibble(
      ObsID = paste(ss$radarID2),
      gap_id = "nodata"
    )
  }
  
  final
  
  l[i] <- list(final)
  
}

final <- do.call(bind_rows,l)

final <- final %>%
  distinct()


# World polygons from rnaturalearthdata
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# Bounding box
extent <- coord_sf(xlim = c(min(output$lon), max(output$lon)), ylim = c(min(output$lat), max(output$lat)))

# plot it

p <- 
    # open ggplot
    ggplot() +
    # plot and color world land mask  
    geom_sf(data = world, fill= "antiquewhite") +
    # geom_sf_label(data = world,aes(label = name))+
    geom_point(data=output, aes(x=lon, y=lat)) +
    extent
