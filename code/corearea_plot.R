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


# ~~~~~~~~~~~~~~~~~~~ #
# Mean marine traffic #
# ~~~~~~~~~~~~~~~~~~~ #

# List _sAISnonfishing.tif extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*_sAISnonfishing.tif", recursive = TRUE)
files <- gsub("_sAISnonfishing.tif","", files)

r1 <- raster::raster(paste0(WD,"input/sAIS/",files[1],"/",files[1],"_COUNT_dens.tif"))
##x11();plot(r1, xlim=c(-25,5),ylim=c(14,43))
r2 <- raster::raster(paste0(WD,"input/sAIS/",files[2],"/",files[2],"_COUNT_dens.tif"))
##x11();plot(r2, xlim=c(-25,5),ylim=c(14,43))
r3 <- raster::raster(paste0(WD,"input/sAIS/",files[3],"/",files[3],"_COUNT_dens.tif"))
##x11();plot(r3, xlim=c(-25,5),ylim=c(14,43))

# mean of all rasters
r <- mean(r1, r2, r3, na.rm = T) 
##x11();plot(r, xlim=c(-25,5),ylim=c(14,43))

# ~~~~~~~~~~~~ #
# Radar events #
# ~~~~~~~~~~~~ #

# List L2.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*_events_radar_L3.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 

# Add population label
RAD <- RAD %>%
  dplyr::mutate(population = recode(colonyName, 
                                    "CalaMorell" = "BalearicIs",
                                    "CVelho" = "CaboVerde",
                                    "MClara" = "CanaryIs",
                                    "Veneguera" = "CanaryIs")) 
# ~~~~~~~~~~ #
# Core Areas #
# ~~~~~~~~~~ #

CA <- readRDS(paste0(WD, "output/CoreAreas.rds"))

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

#######
# AIS #
#######

r_i <- raster::rasterToPoints(r, spatial = TRUE)
r_i  <- data.frame(r_i)
r_i <- r_i %>%
  dplyr::select(-optional)
r_i <- r_i %>%
  filter(x >= -5 & x <= 9 | x>= -35 & x <= 9,
         y>=38  & y<=46 |  y>=10 & y<=32)%>%
  filter(x>-1 | y<38)
colnames(r_i)[1] <- "dens"
r_i <- r_i %>% filter(dens>0)
r_i <- r_i %>% mutate(denslog = log1p(dens))


ca <-sf::st_as_sf(CA[[i]])

p1 <- ggplot() +
  geom_raster(data = r_i , aes(x=x,y=y,fill = denslog),show.legend = T)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(r_i$denslog), max(r_i$denslog)))+
  geom_sf(data= ca, colour="white", fill="white",alpha=0.2,size=0.8)+  
  geom_sf(data = ca, colour="black", alpha=0.2,size=0.5)+  
  geom_sf(color = "black", fill = "gray90",size = 0.2) + 
  geom_point(data=radar_group,aes(x=longitude, y=latitude), colour="#CF202E", fill="red",alpha=1,shape=21,size=1,show.legend = F) +
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
  geom_point(x=4,y=40, size=4, shape=17, colour="black")+
  geom_point(x=4,y=40, colour="#0072B2", size=3, shape=17 )+
  
  geom_point(x=2.18,y=41.38, size=3, shape=16, colour="black")+
  geom_point(x=2.18,y=41.38, size=1.5, shape=16, colour="white")+
  geom_point(x=3.88,y=43.59, size=3, shape=16, colour="black")+
  geom_point(x=3.88,y=43.59, size=1.5, shape=16, colour="white")+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.2,
                              text_cex = 1,
                              style = "ticks") +
  annotate(
    geom = "text",
    x = 4.8+0.5,
    y = 39.3,
    label = "Balearic Is. pop.",
    fontface = "italic",
    color = "black",
    size = 4.5+1
  )+
  annotate(
    geom = "text",
    x = 1.5-0.5,
    y = 41.4,
    label = "Barcelona",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )+
  annotate(
    geom = "text",
    x = 3.15-0.5,
    y = 43.59,
    label = "Montpellier",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )


#x11()
#plot(p1)

p1_leg1 <- cowplot::get_legend(p1)
plot(p1_leg1)

p1 <- ggplot() +
  geom_raster(data = r_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(r_i$denslog), max(r_i$denslog)))+
  geom_sf(data= ca, colour="white", fill="white",alpha=0.2,size=0.8)+  
  geom_sf(data = ca, colour="black", alpha=0.2,size=0.5)+  
  geom_sf(color = "black", fill = "gray90",size = 0.2) + 
  geom_point(data=radar_group,aes(x=longitude, y=latitude), colour="#CF202E", fill="red",alpha=1,shape=21,size=1,show.legend = F) +
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+
  coord_sf(xlim = lon_bounds, ylim=lat_bounds, expand = F, ndiscr = 1000) +   
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
        plot.background = element_rect(fill = "transparent", colour = NA))+
  geom_point(data=c,aes(longitude,latitude), size=6, shape=17, colour="black", show.legend = F)+  
  geom_point(data=c,aes(longitude,latitude), size=4, shape=17, colour="white", show.legend = F)+  
  geom_point(data=c,aes(longitude,latitude), colour="#0072B2", size=3, shape=17)+
  geom_point(aes(x=2.18,y=41.38), size=3, shape=16, colour="black")+
  geom_point(aes(x=2.18,y=41.38), size=1.5, shape=16, colour="white")+
  geom_point(aes(x=3.88,y=43.59), size=3, shape=16, colour="black")+
  geom_point(aes(x=3.88,y=43.59), size=1.5, shape=16, colour="white")+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.2,
                              text_cex = 1,
                              style = "ticks") +
  annotate(
    geom = "text",
    x = 4.8+0.5,
    y = 39.3,
    label = "Balearic Is. pop.",
    fontface = "italic",
    color = "black",
    size = 4.5+1
  )+
  annotate(
    geom = "text",
    x = 1.5-0.5,
    y = 41.4,
    label = "Barcelona",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )+
  annotate(
    geom = "text",
    x = 3.15-0.5,
    y = 43.59,
    label = "Montpellier",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )

#x11()
#plot(p1)

# ~~~~~~~~~~~~~~ #
# population = 2 #
# ~~~~~~~~~~~~~~ #

# population i = 2

i = 2

# residuals
radar_group <- RAD %>%
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

#######
# AIS #
#######

r_i <- raster::rasterToPoints(r, spatial = TRUE)
r_i  <- data.frame(r_i)
r_i <- r_i %>%
  dplyr::select(-optional)
r_i <- r_i %>%
  filter(x >= -5 & x <= 9 | x>= -35 & x <= 9,
         y>=38  & y<=46 |  y>=10 & y<=32)%>%
  filter(x>-1 | y<38)
colnames(r_i)[1] <- "dens"
r_i <- r_i %>% filter(dens>0)
r_i <- r_i %>% mutate(denslog = log1p(dens))


ca <-sf::st_as_sf(CA[[i]])

p2 <- ggplot() +
  geom_raster(data = r_i , aes(x=x,y=y,fill = denslog),show.legend = T)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(r_i$denslog), max(r_i$denslog)))+
  geom_sf(data= ca, colour="white", fill="white",alpha=0.2,size=0.8)+  
  geom_sf(data = ca, colour="black", alpha=0.2,size=0.5)+  
  geom_sf(color = "black", fill = "gray90",size = 0.2) + 
  geom_point(data=radar_group,aes(x=longitude, y=latitude), colour="#CF202E", fill="red",alpha=1,shape=21,size=1,show.legend = F) +
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
  geom_point(x=-22.7,y=15.9, size=3.9, shape=17, colour="black")+
  geom_point(x=-22.7,y=15.9, size=2.9, shape=17, colour="#009E73")+
  
  geom_point(x=-17.05,y=20.99, size=3, shape=16, colour="black")+
  geom_point(x=-17.05,y=20.99, size=1.5, shape=16, colour="white")+
  geom_point(x=-17.46,y=14.72, size=3, shape=16, colour="black")+
  geom_point(x=-17.46,y=14.72, size=1.5, shape=16, colour="white")+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.1,
                              text_cex = 1,
                              style = "ticks") +
  annotate(
    geom = "text",
    x = -22+0.5,
    y = 17.7,
    label = "Cabo Verde pop.",
    fontface = "italic",
    color = "black",
    size = 4.5+1
  ) +
  annotate(
    geom = "text",
    x = -16.4+0,
    y = 14.8,
    label = "Dakar",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )+
  annotate(
    geom = "text",
    x = -15.1+0.5,
    y = 21,
    label = "Cabo Blanco",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )

#x11()
#plot(p2)

p2_leg1 <- cowplot::get_legend(p2)
plot(p2_leg1)

p2 <- ggplot() +
  geom_raster(data = r_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(r_i$denslog), max(r_i$denslog)))+
  geom_sf(data= ca, colour="white", fill="white",alpha=0.2,size=0.8)+  
  geom_sf(data = ca, colour="black", alpha=0.2,size=0.5)+  
  geom_sf(color = "black", fill = "gray90",size = 0.2) + 
  geom_point(data=radar_group,aes(x=longitude, y=latitude), colour="#CF202E", fill="red",alpha=1,shape=21,size=1,show.legend = F) +
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+
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
  geom_point(data=c,aes(longitude,latitude), size=6, shape=17, colour="black", show.legend = F)+  
  geom_point(data=c,aes(longitude,latitude), size=4, shape=17, colour="white", show.legend = F)+  
  geom_point(data=c,aes(longitude,latitude), colour="#009E73", size=3, shape=17)+
  geom_point(aes(x=-17.05,y=20.99), size=3, shape=16, colour="black")+
  geom_point(aes(x=-17.05,y=20.99), size=1.5, shape=16, colour="white")+
  geom_point(aes(x=-17.46,y=14.72), size=3, shape=16, colour="black")+
  geom_point(aes(x=-17.46,y=14.72), size=1.5, shape=16, colour="white")+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.1,
                              text_cex = 1,
                              style = "ticks") +
  annotate(
    geom = "text",
    x = -22+0.5,
    y = 17.7,
    label = "Cabo Verde pop.",
    fontface = "italic",
    color = "black",
    size = 4.5+1
  ) +
  annotate(
    geom = "text",
    x = -16.4+0,
    y = 14.8,
    label = "Dakar",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )+
  annotate(
    geom = "text",
    x = -15.1+0.5,
    y = 21,
    label = "Cabo Blanco",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )

#x11()
#plot(p2)

# ~~~~~~~~~~~~~~ #
# population = 2 #
# ~~~~~~~~~~~~~~ #

# population i = 2

i = 3

# residuals
radar_group <- RAD %>%
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

#######
# AIS #
#######

r_i <- raster::rasterToPoints(r, spatial = TRUE)
r_i  <- data.frame(r_i)
r_i <- r_i %>%
  dplyr::select(-optional)
r_i <- r_i %>%
  filter(x >= -5 & x <= 9 | x>= -35 & x <= 9,
         y>=38  & y<=46 |  y>=10 & y<=32)%>%
  filter(x>-1 | y<38)
colnames(r_i)[1] <- "dens"
r_i <- r_i %>% filter(dens>0)
r_i <- r_i %>% mutate(denslog = log1p(dens))


ca <-sf::st_as_sf(CA[[i]])

p3 <- ggplot() +
  geom_raster(data = r_i , aes(x=x,y=y,fill = denslog),show.legend = T)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(r_i$denslog), max(r_i$denslog)))+
  geom_sf(data= ca, colour="white", fill="white",alpha=0.2,size=0.8)+  
  geom_sf(data = ca, colour="black", alpha=0.2,size=0.5)+  
  geom_sf(color = "black", fill = "gray90",size = 0.2) + 
  geom_point(data=radar_group,aes(x=longitude, y=latitude), colour="#CF202E", fill="red",alpha=1,shape=21,size=1,show.legend = F) +
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
  geom_point(data=c,aes(longitude,latitude), size=6, shape=17, colour="black", show.legend = F)+  
  geom_point(data=c,aes(longitude,latitude), size=4, shape=17, colour="white", show.legend = F)+  
  geom_point(data=c,aes(longitude,latitude), colour="#f5c014", size=3, shape=17)+
  geom_point(x=-13.5,y=29, size=3.9, shape=17, colour="black")+
  geom_point(x=-13.5,y=29,  size=2.9, shape=17, colour="#f5c014")+
  geom_point(x=-15.7,y=28, size=3.9, shape=17, colour="black")+
  geom_point(x=-15.7,y=28, size=2.9, shape=17, colour="#f5c014")+
  geom_point(x=-17.05,y=20.99, size=3, shape=16, colour="black")+
  geom_point(x=-17.05,y=20.99, size=1.5, shape=16, colour="white")+
  geom_point(x=-7.62,y=33.60, size=3, shape=16, colour="black")+
  geom_point(x=-7.62,y=33.60, size=1.5, shape=16, colour="white")+
  geom_point(x=-14.499167,y=26.126944, size=3, shape=16, colour="black")+
  geom_point(x=-14.499167,y=26.126944, size=1.5, shape=16, colour="white")+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.08,
                              text_cex = 1,
                              style = "ticks") +
  annotate(
    geom = "text",
    x = -17.5-0.8,
    y = 27-0.5,
    label = "Canary Is. pop.",
    fontface = "italic",
    color = "black",
    size = 4.5+1
  ) +
  annotate(
    geom = "text",
    x = -15.2+0.5,
    y = 21,
    label = "Cabo Blanco",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )+
  annotate(
    geom = "text",
    x = -12.5+0.5,
    y = 26,
    label = "Cabo Bojador",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )

#x11()
#plot(p3)

p3_leg1 <- cowplot::get_legend(p3)
plot(p3_leg1)

p3 <- ggplot() +
  geom_raster(data = r_i , aes(x=x,y=y,fill = denslog),show.legend = F)+
  geom_sf(data = msk_sf, 
          color = "black", fill = "gray90",lwd  = 0.2) +  
  scale_fill_gradientn(colours = marinetraffic, label = function(x) sprintf("%.2f", x),limits=c(min(r_i$denslog), max(r_i$denslog)))+
  geom_sf(data= ca, colour="white", fill="white",alpha=0.2,size=0.8)+  
  geom_sf(data = ca, colour="black", alpha=0.2,size=0.5)+  
  geom_sf(color = "black", fill = "gray90",size = 0.2) + 
  geom_point(data=radar_group,aes(x=longitude, y=latitude), colour="#CF202E", fill="red",alpha=1,shape=21,size=1,show.legend = F) +
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="black", lwd  = 0.3 ,
               lty = 2)+
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
  geom_point(data=c,aes(longitude,latitude), size=6, shape=17, colour="black", show.legend = F)+  
  geom_point(data=c,aes(longitude,latitude), size=4, shape=17, colour="white", show.legend = F)+  
  geom_point(data=c,aes(longitude,latitude), colour="#f5c014", size=3, shape=17)+
  geom_point(aes(x=-17.05,y=20.99), size=3, shape=16, colour="black")+
  geom_point(aes(x=-17.05,y=20.99), size=1.5, shape=16, colour="white")+
  geom_point(aes(x=-7.62,y=33.60), size=3, shape=16, colour="black")+
  geom_point(aes(x=-7.62,y=33.60), size=1.5, shape=16, colour="white")+
  geom_point(aes(x=-14.499167,y=26.126944), size=3, shape=16, colour="black")+
  geom_point(aes(x=-14.499167,y=26.126944), size=1.5, shape=16, colour="white")+
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              width_hint = 0.08,
                              text_cex = 1,
                              style = "ticks") +
  annotate(
    geom = "text",
    x = -17.5-0.8,
    y = 27-0.5,
    label = "Canary Is. pop.",
    fontface = "italic",
    color = "black",
    size = 4.5+1
  ) +
  annotate(
    geom = "text",
    x = -15.2+0.5,
    y = 21,
    label = "Cabo Blanco",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )+
  annotate(
    geom = "text",
    x = -12.5+0.5,
    y = 26,
    label = "Cabo Bojador",
    fontface = "italic",
    color = "black",
    size = 3.5+1
  )




###########
# legends #
###########

plot(p1_leg1)
plot(p2_leg1)
plot(p3_leg1)

legends <- ggpubr::ggarrange(
  p1_leg1,
  p2_leg1, 
  ncol=2)


setwd(paste0(WD,"outputs/figures"))
Cairo::Cairo(file = "legendsmap_corearea.png",
             type = "png",
             units = "mm",
             width = 350,
             height = 100,
             dpi = 100,
             bg = "transparent")

legends


dev.off()


#############
# composite #
#############
setwd(paste0(WD,"outputs/figures"))

composite <- ggpubr::ggarrange(
  p1,p3,p2, 
  #labels  = c("A","B","C"),
  ncol = 3, nrow = 1,
  align="hv",
  common.legend = F)

setwd(paste0(WD,"outputs/figures"))

Cairo::Cairo(file = "CoreArea.png",
             type = "png",
             units = "mm",
             width = 510,
             height = 110,
             dpi = 100,
             bg = "white")
composite
dev.off()
