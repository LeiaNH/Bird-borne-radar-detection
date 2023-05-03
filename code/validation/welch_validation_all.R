# ----------------
# read welch data
# ----------------

# metadata can be checked here: https://globalfishingwatch.org/data-download/datasets/public-welch-et-al-disabling-events:v20221102
data <- read.csv(paste0(WD, "GitData/Bird-borne-radar-detection/input/ais_disabling_events.csv")) %>%
  # parse to date
  dplyr::mutate(gap_start_timestamp = lubridate::ymd_hms(substr(gap_start_timestamp,1,19)),
                gap_end_timestamp = lubridate::ymd_hms(substr(gap_end_timestamp,1,19))) %>%
  # filter gaps below 2 weeks 
  dplyr::filter(gap_hours <= 336)

min(data$gap_start_timestamp)
max(data$gap_start_timestamp)

# ----------------
# read radar data
# ----------------

radar <- read.csv(paste0(WD, "GitData/Bird-borne-radar-detection/output/radarDetsummary.csv")) %>%
  dplyr::select(time, organismID, deploymentID, colonyName, year, longitude, latitude, tripID, radarID, GFWovr, GFWlevel, AISlevel, Risk, radarID2)   %>%
  mutate(sp = recode(colonyName, 
                     "CalaMorell" = "CALDIO",
                     "CVelho" = "CALEDW",
                     "MClara" = "CALBOR",
                     "Veneguera" = "CALBOR"))

# relabel risk if GFWovr is equal to 1
radar$Risk = ifelse(radar$GFWovr == 1, "null", radar$Risk)

radar <- radar %>%
  # parse to date format
  dplyr::mutate(time = lubridate::ymd_hms(time)) %>%
  dplyr::filter(
    # also with high risk
    Risk != "null") 

min(radar$time)
max(radar$time)

table(radar$Risk)

# --------------
# interpolation 
# --------------

# set your extent
latmin <- min(radar$latitude) - 3
latmax <- max(radar$latitude) + 3
lonmin <- min(radar$longitude) - 3
lonmax <- max(radar$longitude) + 3

run <- "NO"
#run <- "YES"

if(run == "YES"){
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
    dplyr::filter(lon >= lonmin & lon <= lonmax,
                  lat >= latmin & lat <= latmax )
  
  if(nrow(AISgaplocs)>0){
    
    AISgaplocs$gap_id <- label
    l[i] <- list(AISgaplocs)
  }
  
}

dataInt <- data.table::rbindlist(l)

saveRDS(dataInt, paste0(WD,"GitData/Bird-borne-radar-detection/output/intAISgaps_all.rds")) # aqui he tingut en compte les posicions originals (raw) y les interpol·lades. No obstant, crec que faré servir nomes les raw per no complicar els mètodes
}

# -------------
# now rasterize 
# -------------

dataInt <-  readRDS(paste0(WD,"GitData/Bird-borne-radar-detection/output/intAISgaps_all.rds")) %>%
  #keep gap_id
  dplyr::select(lon, lat, gap_id)

# parse to factor
dataInt$gap_id = as.factor(dataInt$gap_id)

#-------------------------------------------------------------
plots <- "YES"
#plots <- "NO"

if(plots == "YES"){
  
  # landmask
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

  ggplot() +
    # add AIS gap paths
    geom_path(data = dataInt, aes(x=lon, y=lat, group=gap_id, colour=gap_id), 
              lwd  = 0.1, alpha=0.8, show.legend = F) +
    # plot land mask
    geom_sf(data = world, 
            color = "gray30", fill = "gray90",lwd  = 0.05) +
    # extent
    coord_sf(xlim = c(lonmin, lonmax), ylim = c(latmin, latmax))+
    theme_classic()
  
  }

#-------------------------------------------------------------


# create empty raster 
r <- raster(xmn=floor(min(dataInt$lon))-0.5, 
            xmx=ceiling(max(dataInt$lon)+0.5), 
            ymn=floor(min(dataInt$lat)-0.5), 
            ymx=ceiling(max(dataInt$lat)+0.5),
            crs=CRS("+proj=longlat +datum=WGS84"),
            resolution=c(1, 1), vals=NULL)

# rasterize number of individuals per grid cell
rcnt <- rasterize(cbind(dataInt$lon, dataInt$lat), r, field = dataInt$gap_id, fun = function(x, ...) {length(unique(na.omit(x)))})


#-------------------------------------------------------------
plot(rcnt)

# Plot the resulting raster

if(plots == "YES"){
  
  # landmask
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # View the first few rows of the dataframe
  df <- as.data.frame(rcnt, xy=TRUE, na.rm=TRUE)

  ggplot(df) +
    geom_tile(aes(x=x, y=y, fill=layer))  +
     scale_fill_viridis_c(name = "N AIS disablings", option = "magma")+
    #scale_fill_gradient(low = "cyan", high = "blue",name = "N AIS disablings") +
    # plot land mask
    geom_sf(data = world, 
            color = "gray30", fill = "gray90",lwd  = 0.05) +
    # extent
    coord_sf(xlim = c(lonmin+0.5, lonmax-0.8), ylim = c(latmin+0.5, latmax-1))+
    theme_classic() +
    geom_point(data=radar, aes(x=longitude, y=latitude), colour = "white", show.legend = F) +
    geom_point(data=radar, aes(x=longitude, y=latitude, colour = Risk),  alpha = 0.8, show.legend = F) +
    cols_risk + 
    xlab("") +
    ylab("") +
    theme_bw()+
    theme(strip.text = element_text(size=3),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),        
          plot.margin = unit(c(0, 0, 0, 0), "lines"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.position="bottom"
    )
  
}


#-------------------------------------------------------------
# ------------------------------
# extract values per radar event 
# ------------------------------

# Create a SpatialPointsDataFrame object from the new trajectory data
radar_sp <- SpatialPointsDataFrame(coords = radar[,c("longitude", "latitude")], data = radar, proj4string = CRS("+proj=longlat +datum=WGS84"))

# Extract the values of the raster cells that correspond to the new trajectory data
cell_values <- extract(rcnt, radar_sp)

# Create a new column in new_traj_data with the cell values
radar$cell_value <- cell_values

# parse na to 0
radar[is.na(radar)] <- 0

# ------------------------------
# violin plot 
# ------------------------------

length(unique(radar$radarID2))

sz <-  radar %>%
  dplyr::group_by(radarID2) %>%
  slice(which.max(cell_value))

# Calculate the mean and standard deviation for each category
summary_df <- sz %>%
  group_by(Risk) %>%
  summarize(
    mean=mean(cell_value),
      sd= sd(cell_value))

# Create a ggplot with error bars
ggplot(data = summary_df, aes(x=factor(Risk, level=c('high', 'medium', 'low')), y=mean, colour=Risk)) +
  geom_point(size=5) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2) +
  theme_classic() + 
  xlab("Radar events IUU Risk") + 
  ylab("N AIS disablings") +
  theme(legend.position = "none")  +
  geom_dotplot(data=sz, aes(x=Risk, y= cell_value, fill=Risk), 
               size = 0.5, binaxis = "y", stackdir = "center", alpha=0.2) +
  theme(
    axis.text.x = element_text(size = 13),
    axis.title.x = element_text(size = 15),
    axis.text.y = element_text(size = 13), 
    axis.title.y = element_text(size = 15))+
  cols_risk + 
  fills_risk
  
  

  
ss$Risk = as.factor(ss$Risk)  

# Add violin plots
ggpubr::ggerrorplot(ss, x = "Risk", y = "cell_value", 
            desc_stat = "mean_sd", color = "black",
            add = "violin", add.params = list(color = "darkgray")
)

# Add dot plots
ggpubr::ggerrorplot(ss, x = "Risk", y = "cell_value", 
            desc_stat = "mean_sd", color = "black",
            add = "dotplot", add.params = list(color = "darkgray")
)
