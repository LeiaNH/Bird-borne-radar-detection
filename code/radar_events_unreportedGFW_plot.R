#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read HourlyBins_radar_evaluation radar files
# 2. define marine traffic levels
# 3. label radar locations not matching daily with GFW with marine traffic level
# 4. plot it
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*HourlyBins_radar_evaluation.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) %>%
  # create unique radar events identification
  dplyr::mutate(
    population = recode(colonyName, 
                        "CalaMorell" = "BalearicIs",
                        "CVelho" = "CaboVerde",
                        "MClara" = "CanaryIs",
                        "Veneguera" = "CanaryIs")) %>%
  # parse NA values to 0
  replace(is.na(.), 0) %>%
  # remove 0
 dplyr::filter(
   sumFishingHours_MonthlyGFW > 0,
   sumNonFishing_sAIS > 0)

########
#Step 2#
########

quantiles <-  RAD %>%
  summarize(
    lowGFW =  quantile(sumFishingHours_MonthlyGFW, probs=0.25, na.rm=TRUE),
    lowAIS =  quantile(sumNonFishing_sAIS, probs=0.25, na.rm=TRUE),
    highGFW =  quantile(sumFishingHours_MonthlyGFW, probs=0.75, na.rm=TRUE),
    highAIS =  quantile(sumNonFishing_sAIS, probs=0.75, na.rm=TRUE))

########
#Step 3#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*unreportedGFW_evaluation.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) %>%
  # create unique radar events identification
  dplyr::mutate(
    population = recode(colonyName, 
                        "CalaMorell" = "BalearicIs",
                        "CVelho" = "CaboVerde",
                        "MClara" = "CanaryIs",
                        "Veneguera" = "CanaryIs")) %>%
  # parse NA values to 0
  replace(is.na(.), 0) 


########
#Step 3#
########

#sz <- merge(RAD, quantiles, by = "population", all.x = TRUE)
sz <- merge(RAD, quantiles, all.x = TRUE)

sz <- sz %>% 
  mutate(
    GFWlevel = case_when(
      FishingHours_GFW < lowGFW ~ "low",
      FishingHours_GFW >= lowGFW & FishingHours_GFW <= highGFW ~"medium",
      FishingHours_GFW > highGFW ~ "high"),
    AISlevel = case_when(
      NonFishing_sAIS < lowAIS ~ "low",
      NonFishing_sAIS >= lowAIS & NonFishing_sAIS <= highAIS ~"medium",
      NonFishing_sAIS > highAIS ~ "high"),
  ) %>%
  dplyr::filter(
    GFWovr == 0) %>%
  group_by(radarID2) %>%
  slice(1) 

sz$population <- recode_factor(sz$population, 
                               BalearicIs = "Balearic Is.", CanaryIs = "Canary Is.", CaboVerde = "Cabo Verde")

sz <- sz %>%
  dplyr::mutate(
    Risk = case_when(
      GFWlevel == "high" & AISlevel =="low" ~ "high",
      GFWlevel == "medium" & AISlevel =="low" ~ "high",
      GFWlevel == "low" & AISlevel =="low" ~ "low",
      GFWlevel == "high" & AISlevel =="medium" ~ "medium",
      GFWlevel == "medium" & AISlevel =="medium" ~ "medium",
      GFWlevel == "low" & AISlevel =="medium" ~ "low",
      GFWlevel == "high" & AISlevel =="high" ~ "medium",
      GFWlevel == "medium" & AISlevel =="high" ~ "medium",
      GFWlevel == "low" & AISlevel =="high" ~ "low"
    ))

sz$Risk = as.factor(sz$Risk)


ggplot(sz, aes(x = GFWlevel, y = AISlevel, fill= Risk)) +
  fills_risk + 
  geom_bin2d() +
  theme_bw() +
  scale_x_discrete(limits = c("low", "medium", "high")) +
  scale_y_discrete(limits = c("low","medium",  "high")) + 
  stat_bin2d(geom = "text", aes(label = ..count..)) + 
  facet_wrap( ~factor(population, levels=c('Balearic Is.','Canary Is.','Cabo Verde'))) +
  xlab ("Fishing") +
  theme_bw()+
  ylab ("Non-fishing") 


sz <- sz %>%
  dplyr::mutate(
    FishingHours_GFW_log1p = log1p(FishingHours_GFW),
    NonFishing_sAIS_log1p = log1p(NonFishing_sAIS)
  )

ggplot(sz, aes(x = FishingHours_GFW_log1p, y = NonFishing_sAIS_log1p)) +
  geom_point()+
  #scale_x_log10()+
  #scale_y_log10()+
  geom_density_2d_filled(contour_var = "ndensity", alpha= 0.8)+
  theme_minimal() + 
  xlim(c(0, max(sz$FishingHours_GFW_log1p))) +
  ylim(c(0, max(sz$NonFishing_sAIS_log1p))) +
  facet_wrap( ~factor(population, levels=c('Balearic Is.','Canary Is.','Cabo Verde'))) +
  xlab ("Fishing") +
  ylab ("Non-fishing") +
  theme(panel.grid = element_blank())



# amb max
maxfish <- log1p(max(RAD$FishingHours_GFW))
maxNONfish <- log1p(max(RAD$NonFishing_sAIS))


ggplot(sz, aes(x = FishingHours_GFW_log1p, y = NonFishing_sAIS_log1p)) +
  geom_point()+
  #scale_x_log10()+
  #scale_y_log10()+
  geom_density_2d_filled(contour_var = "ndensity", alpha= 0.8)+
  theme_minimal() + 
  xlim(c(0, maxfish)) +
  ylim(c(0, maxNONfish)) +
  facet_wrap( ~factor(population, levels=c('Balearic Is.','Canary Is.','Cabo Verde'))) +
  xlab ("Fishing") +
  ylab ("Non-fishing") +
  theme(panel.grid = element_blank())

#####
#MAP#
#####

  
##########################
# general objects needed #
##########################

# load bathymetry
bath <- marmap::getNOAA.bathy(
  lon1 = -28, lon2 = 7,
  lat1 = 11, lat2 = 46, 
  resolution = 4)

# load worldmap as sf
load(paste0(WD, "GitData/Bird-borne-radar-detection/input/valid_world_map.Rdata"))
msk_valid %>% 
  dplyr::select(Name) -> msk_sf  

rm(msk_valid)

# Loading colonies
c <- read.csv2(paste0(WD,"GitData/Bird-borne-radar-detection/input/colonysites.csv"))

# Loading EEZ data
eezs <- read_sf(paste0(WD,"GitData/Bird-borne-radar-detection/input/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp")) 

# ~~~~~~~~~~~~~~~~~~~~~~~~ #
# Mean Non-Fishing traffic #
# ~~~~~~~~~~~~~~~~~~~~~~~~ #

# List _sAISnonfishing.tif extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*_sAISnonfishing.tif", recursive = TRUE)

r1 <- raster::raster(paste0(WD,"GitData/Bird-borne-radar-detection/output/",files[1]))
##x11();plot(r1, xlim=c(-25,5),ylim=c(14,43))
r2 <- raster::raster(paste0(WD,"GitData/Bird-borne-radar-detection/output/",files[2]))
##x11();plot(r2, xlim=c(-25,5),ylim=c(14,43))
r3 <- raster::raster(paste0(WD,"GitData/Bird-borne-radar-detection/output/",files[3]))
##x11();plot(r3, xlim=c(-25,5),ylim=c(14,43))

# mean of all rasters
nonfishves <- mean(r1, r2, r3, na.rm = T) 
##x11();plot(nonfishves, xlim=c(-25,5),ylim=c(14,43))

#Density estimates plotted on a logarithmic (log 10) colour scale.

# ~~~~~~~~~~~~~~~~~~~~ #
# Mean Fishing traffic #
# ~~~~~~~~~~~~~~~~~~~~ #
ext <- raster::extent(-40,15,8,50)

# List _GFW_NavigationHours.tif extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*_GFW_FishingHours.tif", recursive = TRUE)

r1 <- raster::raster(paste0(WD,"GitData/Bird-borne-radar-detection/output/",files[1]))
r1 <- crop(r1,ext)
##x11();plot(r1)

r2 <- raster::raster(paste0(WD,"GitData/Bird-borne-radar-detection/output/",files[2]))
r2 <- crop(r2,ext)
##x11();plot(r2)

r3 <- raster::raster(paste0(WD,"GitData/Bird-borne-radar-detection/output/",files[3]))
r3 <- crop(r3,ext)
##x11();plot(r1)

# mean of all rasters
fishves <- mean(r1, r2, r3, na.rm = T) 
##x11();plot(fishves)

# ~~~~~~~ #
# plot it #
# ~~~~~~~ #

populations <- unique(sz$population)

sz$GFWlevel = as.factor(sz$GFWlevel)
sz$AISlevel = as.factor(sz$AISlevel)

# ~~~~~~~~~ #
# mapamundi #
# ~~~~~~~~~ #
ggplot() +
  geom_sf(data = msk_sf, 
          color = "gray80", fill = "gray80",lwd  = 0) +
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))


# ~~~~~~~~~~~~~~ #
# population = 1 #
# ~~~~~~~~~~~~~~ #

# population i = 1

i = 1

# SUBSET
radar_group <- sz %>%
  dplyr::filter(population == populations[[i]]) 

### define lon/lat bounds
# range from data
xl <- range(radar_group$longitude)
yl <- range(radar_group$latitude)
# get centroid
zoom_to <- c(mean(xl), mean(yl))  # center of the range
# define zoom level
lon_span <- xl[2]-xl[1]
lat_span <- yl[2]-yl[1]
zoom_lon <- floor(log2(360/lon_span))
zoom_lat <- floor(log2(180/lat_span))
zoom_level <- min(zoom_lon, zoom_lat)
# define span
lon_span <- 360 / 2^zoom_level
lat_span <- 180 / 2^zoom_level
# define boundaries
lon_bounds <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
lat_bounds <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

# let's plot it

#############
# Locations #
#############



loc1 <- ggplot() +
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  geom_sf(data = eezs,
          color = alpha("gray30",0.4),
          fill = NA,
          lwd  = 0.05,
          lty = 1)+   
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +    
  geom_point(aes(x=2.18, y=41.38), size=3, shape=16, colour="black")+
  geom_point(aes(x=2.18, y=41.38), size=1.5, shape=16, colour="white")+
  geom_point(aes(x=3.88, y=43.59), size=3, shape=16, colour="black")+
  geom_point(aes(x=3.88, y=43.59), size=1.5, shape=16, colour="white")+ 
  geom_point(data=radar_group,aes(x=longitude, y=latitude, fill = Risk), 
             size=2,show.legend = F, shape = 21, colour = "black", alpha = 0.5) + 
  fills_risk+  

  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.3,
                              text_cex = 0.8,
                              style = "ticks") +
  annotate(
    geom = "text",
    x = 4.3,
    y = 40.5,
    label = "Balearic Is. pop.",
    fontface = "italic",
    color = "black",
    size = 4.5+0.2
  )+
  annotate(
    geom = "text",
    x = 1.5,
    y = 41.4,
    label = "Barcelona",
    fontface = "italic",
    color = "black",
    size = 3.5+0.2
  )+
  annotate(
    geom = "text",
    x = 3.15-0.5,
    y = 43.59,
    label = "Montpellier",
    fontface = "italic",
    color = "black",
    size = 3.5+0.2
  )+
  annotate(
    geom = "text",
    x = 4.5,
    y = 41.5,
    label = "ES",
    fontface = "italic",
    color = "black",
    size = 4
  )

x11();loc1

#######
# GFW #
#######

fishves_i <- raster::rasterToPoints(fishves, spatial = TRUE)
fishves_i  <- data.frame(fishves_i)
fishves_i <- fishves_i %>%
  dplyr::select(-optional)
colnames(fishves_i)[1] <- "dens"
fishves_i <- fishves_i%>%
  filter(x >= -5 & x <= 9 | x>= -35 & x <= 9,
         y>=38  & y<=46 |  y>=10 & y<=32)%>%
  filter(x>-1 | y<38) %>%
  filter(y>38 | y<31) 
fishves_i <- fishves_i %>% filter(dens>0)
fishves_i <- fishves_i %>% mutate(denslog = log1p(dens))

##x11()

GFW1 <-  ggplot() +
  geom_raster(data = fishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
  geom_point(data=radar_group,aes(x=longitude, y=latitude), 
             size=2,show.legend = F,  colour="black", shape = 1) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

#######
# AIS #
#######

nonfishves_i <- raster::rasterToPoints(nonfishves, spatial = TRUE)
nonfishves_i  <- data.frame(nonfishves_i)
nonfishves_i <- nonfishves_i %>%
  dplyr::select(-optional)
nonfishves_i <- nonfishves_i %>%
  filter(x >= -5 & x <= 9 | x>= -35 & x <= 9,
         y>=38  & y<=46 |  y>=10 & y<=32)%>%
  filter(x>-1 | y<38) %>%
  filter(y>38 | y<31) 
colnames(nonfishves_i)[1] <- "dens"
#nonfishves_i <- nonfishves_i %>% filter(dens>0)
nonfishves_i[nonfishves_i == 0] <- NA 
nonfishves_i <- nonfishves_i %>% mutate(denslog = log1p(dens))


AIS1 <- ggplot() +
  geom_raster(data = nonfishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
  geom_point(data=radar_group,aes(x=longitude, y=latitude), 
             size=2,show.legend = F,  colour="black", shape = 1) + 
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))
#x11()
#plot(AIS1)


# ~~~~~~~~~~~~~~ #
# population = 2 #
# ~~~~~~~~~~~~~~ #

# population i = 2

i = 2

# SUBSET
radar_group <- sz %>%
  dplyr::filter(population == populations[[i]]) 

### define lon/lat bounds
# range from data
xl <- range(radar_group$longitude)
yl <- range(radar_group$latitude)
# get centroid
zoom_to <- c(mean(xl), mean(yl))  # center of the range
# define zoom level
lon_span <- xl[2]-xl[1]
lat_span <- yl[2]-yl[1]
zoom_lon <- floor(log2(360/lon_span))
zoom_lat <- floor(log2(180/lat_span))
zoom_level <- min(zoom_lon, zoom_lat)
# define span
lon_span <- 360 / 2^zoom_level
lat_span <- 180 / 2^zoom_level
# define boundaries
lon_bounds <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
lat_bounds <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

# let's plot it

#############
# Locations #
#############

loc2 <- ggplot() +
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  geom_sf(data = eezs,
          color = alpha("gray30",0.4),
          fill = NA,
          lwd  = 0.05,
          lty = 1)+   
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +    
  geom_point(data=radar_group,aes(x=longitude, y=latitude, fill = Risk, alpha = 0.5), 
             size=2,show.legend = F, shape = 21, colour = "black") + 
  fills_risk+  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))+
  geom_point(aes(x=-17.05,y=20.99), size=3, shape=16, colour="black")+
  geom_point(aes(x=-17.05,y=20.99), size=1.5, shape=16, colour="white")+
  geom_point(aes(x=-17.46,y=14.72), size=3, shape=16, colour="black")+
  geom_point(aes(x=-17.46,y=14.72), size=1.5, shape=16, colour="white")+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.1,
                              text_cex = 0.8,
                              style = "ticks") +
  annotate(
    geom = "text",
    x = -20,
    y = 26+0.2,
    label = "Canary Is. pop.",
    fontface = "italic",
    color = "black",
    size = 4.5+0.2
  ) +
  annotate(
    geom = "text",
    x = -15.1+0.8,
    y = 21,
    label = "Cabo Blanco",
    fontface = "italic",
    color = "black",
    size = 3.5+0.2
  )+
  annotate(
    geom = "text",
    x = -20,
    y = 28,
    label = "ES",
    fontface = "italic",
    color = "black",
    size = 4
  )+
  annotate(
    geom = "text",
    x = -9.5,
    y = 28,
    label = "MA",
    fontface = "italic",
    color = "black",
    size = 4
  )+
  annotate(
    geom = "text",
    x = -14.5,
    y = 24,
    label = "WS",
    fontface = "italic",
    color = "black",
    size = 4
  )

#x11();loc2

#######
# GFW #
#######

fishves_i <- raster::rasterToPoints(fishves, spatial = TRUE)
fishves_i  <- data.frame(fishves_i)
fishves_i <- fishves_i %>%
  dplyr::select(-optional)
colnames(fishves_i)[1] <- "dens"
fishves_i <- fishves_i%>%
  filter(x >= -5 & x <= 9 | x>= -35 & x <= 9,
         y>=38  & y<=46 |  y>=10 & y<=32)%>%
  filter(x>-1 | y<38)
fishves_i <- fishves_i %>% filter(dens>0)
fishves_i <- fishves_i %>% mutate(denslog = log1p(dens))

##x11()

GFW2 <-  ggplot() +
  geom_raster(data = fishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
  geom_point(data=radar_group,aes(x=longitude, y=latitude), 
             size=2,show.legend = F,  colour="black", shape = 1) + 
  theme_bw()+
  theme(legend.position = c(0.13,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))


#x11()
#plot(GFW2)


#######
# AIS #
#######

nonfishves_i <- raster::rasterToPoints(nonfishves, spatial = TRUE)
nonfishves_i  <- data.frame(nonfishves_i)
nonfishves_i <- nonfishves_i %>%
  dplyr::select(-optional)
nonfishves_i <- nonfishves_i %>%
  filter(x >= -5 & x <= 9 | x>= -35 & x <= 9,
         y>=38  & y<=46 |  y>=10 & y<=32)%>%
  filter(x>-1 | y<38)
colnames(nonfishves_i)[1] <- "dens"
nonfishves_i <- nonfishves_i %>% filter(dens>0)
nonfishves_i <- nonfishves_i %>% mutate(denslog = log1p(dens))


AIS2 <- ggplot() +
  geom_raster(data = nonfishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  scale_fill_gradientn(colours = marinetraffic)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
  geom_point(data=radar_group,aes(x=longitude, y=latitude), 
             size=2,show.legend = F,  colour="black", shape = 1) + 
  theme_bw()+
  theme(legend.position = c(0.13,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

#x11()
#plot(AIS2)


# ~~~~~~~~~~~~~~ #
# population = 3 #
# ~~~~~~~~~~~~~~ #

# population i = 3

i = 3

# SUBSET
radar_group <- sz %>%
  dplyr::filter(population == populations[[i]]) 

### define lon/lat bounds
# range from data
xl <- range(radar_group$longitude)
yl <- range(radar_group$latitude)
# get centroid
zoom_to <- c(mean(xl), mean(yl))  # center of the range
# define zoom level
lon_span <- xl[2]-xl[1]
lat_span <- yl[2]-yl[1]
zoom_lon <- floor(log2(360/lon_span))
zoom_lat <- floor(log2(180/lat_span))
zoom_level <- min(zoom_lon, zoom_lat)
# define span
lon_span <- 360 / 2^zoom_level
lat_span <- 180 / 2^zoom_level
# define boundaries
lon_bounds <- c(zoom_to[1] - lon_span / 2, zoom_to[1] + lon_span / 2)
lat_bounds <- c(zoom_to[2] - lat_span / 2, zoom_to[2] + lat_span / 2)

# let's plot it

#############
# Locations #
#############

loc3 <- ggplot() +
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  geom_sf(data = eezs,
          color = alpha("gray30",0.4),
          fill = NA,
          lwd  = 0.05,
          lty = 1)+   
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +    
  geom_point(data=radar_group,aes(x=longitude, y=latitude, fill = Risk, alpha = 0.5), 
             size=2,show.legend = F, shape = 21, colour = "black") + 
  fills_risk+  
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))+
  geom_point(aes(x=-17.05,y=20.99), size=3, shape=16, colour="black")+
  geom_point(aes(x=-17.05,y=20.99), size=1.5, shape=16, colour="white")+
  geom_point(aes(x=-17.46,y=14.72), size=3, shape=16, colour="black")+
  geom_point(aes(x=-17.46,y=14.72), size=1.5, shape=16, colour="white")+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.1,
                              text_cex = 0.8,
                              style = "ticks")+
  annotate(
    geom = "text",
    x = -19,
    y = 16.3,
    label = "Cabo Verde pop.",
    fontface = "italic",
    color = "black",
    size = 4.5+0.2
  ) +
  annotate(
    geom = "text",
    x = -17 + 0.1,
    y = 14.8,
    label = "Dakar",
    fontface = "italic",
    color = "black",
    size = 3.5+0.2
  )+
  annotate(
    geom = "text",
    x = -15.1+0.8,
    y = 21,
    label = "Cabo Blanco",
    fontface = "italic",
    color = "black",
    size = 3.5+0.2
  )+
  annotate(
    geom = "text",
    x = -16.1,
    y = 16.5,
    label = "MR",
    fontface = "italic",
    color = "black",
    size = 4
  )+
  annotate(
    geom = "text",
    x = -16,
    y = 15,
    label = "SN",
    fontface = "italic",
    color = "black",
    size = 4
  )


#x11();loc3

#######
# GFW #
#######
fishves_i <- raster::rasterToPoints(fishves, spatial = TRUE)
fishves_i  <- data.frame(fishves_i)
fishves_i <- fishves_i %>%
  dplyr::select(-optional)
colnames(fishves_i)[1] <- "dens"
fishves_i <- fishves_i%>%
  filter(x >= -5 & x <= 9 | x>= -35 & x <= 9,
         y>=38  & y<=46 |  y>=10 & y<=32)%>%
  filter(x>-1 | y<38)
fishves_i <- fishves_i %>% filter(dens>0)
fishves_i <- fishves_i %>% mutate(denslog = log1p(dens))


##x11()

GFW3 <-  ggplot() +
  geom_raster(data = fishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
  geom_point(data=radar_group,aes(x=longitude, y=latitude), 
             size=2,show.legend = F,  colour="black", shape = 1) + 
  theme_bw()+
  theme(legend.position = c(0.13,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

#x11()
#plot(GFW3)

GFW3_leg <-  ggplot() +
  geom_raster(data = fishves_i , aes(x=x,y=y,fill = denslog),show.legend = T)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
  theme(legend.position = c(0.13,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

legGFW <- cowplot::get_legend(GFW3_leg)
plot(legGFW)

#######
# AIS #
#######

nonfishves_i <- raster::rasterToPoints(nonfishves, spatial = TRUE)
nonfishves_i  <- data.frame(nonfishves_i)
nonfishves_i <- nonfishves_i %>%
  dplyr::select(-optional)
nonfishves_i <- nonfishves_i %>%
  filter(x >= -5 & x <= 9 | x>= -35 & x <= 9,
         y>=38  & y<=46 |  y>=10 & y<=32)%>%
  filter(x>-1 | y<38)
colnames(nonfishves_i)[1] <- "dens"
nonfishves_i <- nonfishves_i %>% filter(dens>0)
nonfishves_i <- nonfishves_i %>% mutate(denslog = log1p(dens))

AIS3 <- ggplot() +
  geom_raster(data = nonfishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  scale_fill_gradientn(colours = marinetraffic)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
  geom_point(data=radar_group,aes(x=longitude, y=latitude), 
             size=2,show.legend = F,  colour="black",  shape = 1) + 
  theme_bw()+
  theme(legend.position = c(0.13,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

#x11()
#plot(AIS3)

AIS3_leg <- ggplot() +
  geom_raster(data = nonfishves_i , aes(x=x,y=y,fill = denslog),show.legend = T)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
  theme(legend.position = c(0.13,0.6),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=15),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        legend.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

legAIS <- cowplot::get_legend(AIS3_leg)
plot(legAIS)

#############
# composite #
#############

composite <- ggpubr::ggarrange(
  loc1, loc2, loc3,
  GFW1,GFW2,GFW3, 
  AIS1,AIS2,AIS3,
  labels  = c("","","",
              "","","",
              "", "", ""),
  ncol = 3, nrow = 3,
  align="hv",
  common.legend = F)
##x11();composite

setwd(paste0(WD,"GitData/Bird-borne-radar-detection/output/figures"))

Cairo::Cairo(file = "GFW0.png",
             type = "png",
             units = "mm",
             width = 250+100,
             height = 150+90,
             dpi = 100,
             bg = "transparent")

composite


dev.off()


#### legends

blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

legends=ggpubr::ggarrange(blankPlot,
                          legGFW,
                          legAIS, 
                          ncol=1,nrow=2)

setwd(paste0(WD,"GitData/Bird-borne-radar-detection/output/figures"))

Cairo::Cairo(file = "leg.png",
             type = "png",
             units = "mm",
             width = 250+100,
             height = 150+90,
             dpi = 100,
             bg = "transparent")

legends


dev.off()

