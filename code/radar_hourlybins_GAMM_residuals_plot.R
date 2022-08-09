##########################
# general objects needed #
##########################

# load bathymetry
bath <- marmap::getNOAA.bathy(
  lon1 = -28, lon2 = 7,
  lat1 = 11, lat2 = 46, 
  resolution = 4)

# load worldmap as sf
load(paste0(WD, "input/valid_world_map.Rdata"))
msk_valid %>% 
  dplyr::select(Name) -> msk_sf  

rm(msk_valid)

# Loading colonies
c <- read.csv2(paste0(WD,"input/colonysites.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~ #
# Mean Non-Fishing traffic #
# ~~~~~~~~~~~~~~~~~~~~~~~~ #

# List _sAISnonfishing.tif extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*_sAISnonfishing.tif", recursive = TRUE)

r1 <- raster::raster(paste0(WD,"output/",files[1]))
##x11();plot(r1, xlim=c(-25,5),ylim=c(14,43))
r2 <- raster::raster(paste0(WD,"output/",files[2]))
##x11();plot(r2, xlim=c(-25,5),ylim=c(14,43))
r3 <- raster::raster(paste0(WD,"output/",files[3]))
##x11();plot(r3, xlim=c(-25,5),ylim=c(14,43))

# mean of all rasters
nonfishves <- mean(r1, r2, r3, na.rm = T) 
##x11();plot(nonfishves, xlim=c(-25,5),ylim=c(14,43))


# ~~~~~~~~~~~~~~~~~~~~ #
# Mean Fishing traffic #
# ~~~~~~~~~~~~~~~~~~~~ #
ext <- raster::extent(-40,15,8,50)

# List _GFW_NavigationHours.tif extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*_GFW_FishingHours.tif", recursive = TRUE)

r1 <- raster::raster(paste0(WD,"output/",files[1]))
r1 <- crop(r1,ext)
##x11();plot(r1)

r2 <- raster::raster(paste0(WD,"output/",files[2]))
r2 <- crop(r2,ext)
##x11();plot(r2)

r3 <- raster::raster(paste0(WD,"output/",files[3]))
r3 <- crop(r3,ext)
##x11();plot(r1)

# mean of all rasters
fishves <- mean(r1, r2, r3, na.rm = T) 
##x11();plot(fishves)

# ~~~~~~~~~ #
# Residuals #
# ~~~~~~~~~ #

# List _GFW_NavigationHours.tif extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*_residuals_bestGAMM.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 

# ~~~~~~~ #
# plot it #
# ~~~~~~~ #

populations <- unique(RAD$population)


# ~~~~~~~~~~~~~~ #
# population = 1 #
# ~~~~~~~~~~~~~~ #

# population i = 1

i = 1

# residuals
radar_group <- RAD %>%
  dplyr::filter(population == populations[[i]],
                residuals >= 0.5) 

### define lon/lat bounds
# range from data
xl <- range(radar_group$longitude1)
yl <- range(radar_group$latitude1)
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
# residuals #
#############

##x11()

r1 <- ggplot(msk_sf) +
  geom_sf(color = "black", fill = "gray90",lwd  = 0.2) +  
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  geom_point(data=radar_group,aes(x=longitude1, y=latitude1), colour="red", fill="red",alpha=.5,shape=21, 
             size=.7,show.legend = T)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
  theme_bw()+
  geom_point(x=2.18, y=41.38, size=3, shape=16, colour="black")+
  geom_point(x=2.18, y=41.38, size=1.5, shape=16, colour="white")+
  geom_point(x=3.88, y=43.59, size=3, shape=16, colour="black")+
  geom_point(x=3.88, y=43.59, size=1.5, shape=16, colour="white")+
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.3,
                              text_cex = 0.8,
                              style = "ticks") +
  annotate(
    geom = "text",
    x = 4.8+0.2,
    y = 39.3,
    label = "Balearic Is.",
    fontface = "italic",
    color = "black",
    size = 4.5+0.2
  )+
  annotate(
    geom = "text",
    x = 1.5-0.5,
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
  )


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


GFW1 <- ggplot() +
  geom_raster(data = fishves_i , aes(x=x,y=y,fill = denslog),show.legend = T)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  labs(fill = expression("Fishing hours "~month^-1))

#x11()
#plot(GFW1)

GFW_leg1 <- cowplot::get_legend(GFW1)
plot(GFW_leg1)

GFW1 <- ggplot() +
  geom_raster(data = fishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  labs(fill = expression("Fishing hours "~month^-1))

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


AIS1 <- ggplot() +
  geom_raster(data = nonfishves_i , aes(x=x,y=y,fill = denslog),show.legend = T)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(nonfishves_i$denslog), max(nonfishves_i$denslog)))+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  labs(fill = expression("vessels"~km^-2 ~month^-1))
  
#x11()
plot(AIS1)

AIS_leg1 <- cowplot::get_legend(AIS1)
plot(AIS_leg1)

AIS1 <- ggplot() +
  geom_raster(data = nonfishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(nonfishves_i$denslog), max(nonfishves_i$denslog)))+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA)) +
  labs(fill = expression("vessels"~km^-2 ~month^-1))


# ~~~~~~~~~~~~~~ #
# population = 2 #
# ~~~~~~~~~~~~~~ #

# population i = 2

i = 2

# residuals
radar_group <- RAD %>%
  dplyr::filter(population == populations[[i]],
                residuals >= 0.5) 

### define lon/lat bounds
# range from data
xl <- range(radar_group$longitude1)
xl <- c(-20, xl[2])

yl <- range(radar_group$latitude1)
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
# residuals #
#############

##x11()

r2 <- ggplot(msk_sf) +  
  geom_sf(color = "black", fill = "gray90",lwd  = 0.2) + 
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  geom_point(data=radar_group,aes(x=longitude1, y=latitude1), colour="red", fill="red",alpha=.5,shape=21, 
             size=.7,show.legend = T)+
 
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  geom_point(x=-17.05,y=20.99, size=3, shape=16, colour="black")+
  geom_point(x=-17.05,y=20.99, size=1.5, shape=16, colour="white")+
  geom_point(x=-17.46,y=14.72, size=3, shape=16, colour="black")+
  geom_point(x=-17.46,y=14.72, size=1.5, shape=16, colour="white")+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.1,
                              text_cex = 0.8,
                              style = "ticks")+
  annotate(
    geom = "text",
    x = -22,
    y = 17.7,
    label = "Cabo Verde",
    fontface = "italic",
    color = "black",
    size = 4.5+0.2
  ) +
  annotate(
    geom = "text",
    x = -16.4+0.5,
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
  )


#x11()
#plot(r2)

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


GFW2 <- ggplot() +
  geom_raster(data = fishves_i , aes(x=x,y=y,fill = denslog),show.legend = T)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  labs(fill = expression("Fishing hours "~month^-1))

#x11()
#plot(GFW2)

GFW_leg2 <- cowplot::get_legend(GFW2)
plot(GFW_leg2)

GFW2 <- ggplot() +
  geom_raster(data = fishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  labs(fill = expression("Fishing hours "~month^-1))

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
  geom_raster(data = nonfishves_i , aes(x=x,y=y,fill = denslog),show.legend = T)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(nonfishves_i$denslog), max(nonfishves_i$denslog)))+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  labs(fill = expression("vessels"~km^-2 ~month^-1))

#x11()
#plot(AIS2)

AIS_leg2 <- cowplot::get_legend(AIS2)
#plot(AIS_leg2)

AIS2 <- ggplot() +
  geom_raster(data = nonfishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(nonfishves_i$denslog), max(nonfishves_i$denslog)))+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  labs(fill = expression("vessels/"~km^2))

#x11()
#plot(AIS2)

# ~~~~~~~~~~~~~~ #
# population = 3 #
# ~~~~~~~~~~~~~~ #

# population i = 3

i = 3

# residuals
radar_group <- RAD %>%
  dplyr::filter(population == populations[[i]],
                residuals >= 0.5) 

### define lon/lat bounds
# range from data
xl <- range(radar_group$longitude1)
yl <- range(radar_group$latitude1)
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
# residuals #
#############

##x11()

r3 <- ggplot(msk_sf) +  
  geom_sf(color = "black", fill = "gray90",lwd  = 0.2) +  
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+   
  geom_point(data=radar_group,aes(x=longitude1, y=latitude1), colour="red", fill="red",alpha=.5,shape=21, 
             size=.7,show.legend = T)+

  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  geom_point(x=-17.05,y=20.99, size=3, shape=16, colour="black")+
  geom_point(x=-17.05,y=20.99, size=1.5, shape=16, colour="white")+
  geom_point(x=-7.62,y=33.60, size=3, shape=16, colour="black")+
  geom_point(x=-7.62,y=33.60, size=1.5, shape=16, colour="white")+
  geom_point(x=-14.499167,y=26.126944, size=3, shape=16, colour="black")+
  geom_point(x=-14.499167,y=26.126944, size=1.5, shape=16, colour="white")+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.1,
                              text_cex = 0.8,
                              style = "ticks") +
  annotate(
    geom = "text",
    x = -17.5-0.6,
    y = 27,
    label = "Canary Is.",
    fontface = "italic",
    color = "black",
    size = 4.5+0.2
  ) +
  annotate(
    geom = "text",
    x = -15.2+0.8,
    y = 21,
    label = "Cabo Blanco",
    fontface = "italic",
    color = "black",
    size = 3.5+0.2
  )+
  annotate(
    geom = "text",
    x = -12.5+0.8,
    y = 26,
    label = "Cabo Bojador",
    fontface = "italic",
    color = "black",
    size = 3.5+0.2
  )


#x11()
#plot(r3)

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


GFW3 <- ggplot() +
  geom_raster(data = fishves_i , aes(x=x,y=y,fill = denslog),show.legend = T)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  labs(fill = expression("Fishing hours "~month^-1))

#x11()
#plot(GFW3)

GFW_leg3 <- cowplot::get_legend(GFW3)
plot(GFW_leg3)

GFW3 <- ggplot() +
  geom_raster(data = fishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  labs(fill = expression("Fishing hours "~month^-1))

#x11()
#plot(GFW3)

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
  geom_raster(data = nonfishves_i , aes(x=x,y=y,fill = denslog),show.legend = T)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(nonfishves_i$denslog), max(nonfishves_i$denslog)))+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  labs(fill = expression("vessels"~km^-2 ~month^-1))

#x11()
#plot(AIS3)

AIS_leg3 <- cowplot::get_legend(AIS3)
#plot(AIS_leg3)

AIS3 <- ggplot() +
  geom_raster(data = nonfishves_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(nonfishves_i$denslog), max(nonfishves_i$denslog)))+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  labs(fill = expression("vessels"~km^-2 ~month^-1))

#x11()
#plot(AIS3)


# SAME SCALE, IT'S OKEY
plot(GFW_leg1)
plot(GFW_leg2)
plot(GFW_leg3)

# SAME SCALE, IT'S OKEY
plot(AIS_leg1)
plot(AIS_leg2)
plot(AIS_leg3)

#############
# composite #
#############

composite <- ggpubr::ggarrange(
  r1,r3,r2, 
  GFW1,GFW3,GFW2, 
  AIS1,AIS3,AIS2,
  #labels  = c("A","","",
  #            "B","","",
  #            "C","",""),
  ncol = 3, nrow = 3,
  align="hv",
  common.legend = F)
##x11();composite

setwd(paste0(WD,"outputs/figures"))
Cairo::Cairo(file = "residualsmap_fishingvsnonfishing.png",
             type = "png",
             units = "mm",
             width = 330,
             height = 210,
             dpi = 100,
             bg = "transparent")

composite


dev.off()


###########
# legends #
###########

legends <- ggpubr::ggarrange(
  GFW_leg1,
  AIS_leg2, 
  ncol=2)


setwd(paste0(WD,"outputs/figures"))
Cairo::Cairo(file = "legendsmap_fishingvsnonfishing.png",
             type = "png",
             units = "mm",
             width = 350,
             height = 100,
             dpi = 100,
             bg = "transparent")

legends


dev.off()
