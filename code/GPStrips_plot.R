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

# Loading EEZ data
eezs <- read_sf(paste0(WD,"input/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp")) 

# Loading colonies
c <- read.csv2(paste0(WD,"input/colonysites.csv"))

#########################
# load GPS data from L2 #
#########################

files <- list.files(path = paste0(WD, "output/"), pattern = "*trips_L2.csv", recursive = TRUE)

GPS <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) %>%
  # select coordinates and colonyname
  dplyr::select(longitude, latitude, colonyName, tripID)

###########
# plot it #
###########


p <- ggplot(bath) +
  geom_raster(aes(x, y),fill="white")+ 
  xlab("Longitude") + ylab("Latitude")+
  geom_path(data = GPS, aes(x=longitude, y=latitude, group=tripID, colour = colonyName), lwd  = 0.1, alpha=0.8, show.legend = F)+
  cols_colony +
  geom_sf(data = msk_sf, 
          color = "gray30", fill = "gray30",lwd  = 0.05) +
  geom_sf(data = eezs,
          color = alpha("gray90"),
          fill = NA,
          lwd  = 0.05,
          lty = 1)+     
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="gray10", lwd  = 0.1 ,
               lty = 1) + 
  geom_point(data=c,aes(longitude,latitude), size=1.3, shape=17, colour="black", show.legend = F)+  
  geom_point(data=c,aes(longitude,latitude), size=0.8, shape=17, colour="white", show.legend = F)+  
  geom_point(data=c,aes(longitude,latitude, colour = colonyName), size=0.4, shape=17)+
  cols_colony +
  coord_sf(xlim = c(-25.5, 5.5), ylim = c(14, 45)) + 
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              height = unit(0.1, "cm"),
                              line_width = 0.5,
                              text_cex= 0.5,
                              style = "ticks") +
  theme_bw()+
  theme(strip.text = element_text(size=3),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),        
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position="none"
  )+
  annotate(
    geom = "text",
    x = -23,
    y = 19,
    label = "Cabo Verde",
    fontface = "italic",
    color = "black",
    size = 1.7
  ) +
  annotate(
    geom = "text",
    x = -19.5,
    y = 30.2,
    label = "Canary Is.",
    fontface = "italic",
    color = "black",
    size = 1.7
  ) +
  annotate(
    geom = "text",
    x = 3.8,
    y = 38.2,
    label = "Balearic Is.",
    fontface = "italic",
    color = "black",
    size = 1.7
  ) +
  annotate(
    geom = "text",
    x = 5.2,
    y = 39.3,
    label = "Cala\nMorell",
    fontface = "italic",
    color = "#0072B2",
    size = 1.4
  ) +
  annotate(
    geom = "text",
    x = -15,
    y = 30,
    label = "Monta?a\nClara",
    fontface = "italic",
    color = "#E69F00",
    size = 1.4
  ) +
  annotate(
    geom = "text",
    x = -18-0.1,
    y = 27,
    label = "Veneguera",
    fontface = "italic",
    color = "#D55E00",
    size = 1.4
  ) +
  annotate(
    geom = "text",
    x = -22.5,
    y = 14.2,
    label = "Curral Velho",
    fontface = "italic",
    color = "#009E73",
    size = 1.4
  ) +
  geom_point(x=-17.05,y=20.99, size=0.8, shape=16, colour="black")+
  geom_point(x=-17.05,y=20.99, size=0.05, shape=16, colour="white")+
  geom_point(x=-7.62,y=33.60, size=0.8, shape=16, colour="black")+
  geom_point(x=-7.62,y=33.60, size=0.05, shape=16, colour="white")+
  geom_point(x=2.18,y=41.38, size=0.8, shape=16, colour="black")+
  geom_point(x=2.18,y=41.38, size=0.05, shape=16, colour="white")+
  geom_point(x=3.88,y=43.59, size=0.8, shape=16, colour="black")+
  geom_point(x=3.88,y=43.59, size=0.05, shape=16, colour="white")+
  geom_point(x=-17.46,y=14.72, size=0.8, shape=16, colour="black")+
  geom_point(x=-17.46,y=14.72, size=0.05, shape=16, colour="white")+
  geom_point(x=-14.499167,y=26.126944, size=0.8, shape=16, colour="black")+
  geom_point(x=-14.499167,y=26.126944, size=0.05, shape=16, colour="white")+
  annotate(
    geom = "text",
    x = -12.5+0.5,
    y = 26.2,
    label = "Cabo Bojador",
    fontface = "italic",
    color = "black",
    size = 1.4
  ) +
  annotate(
    geom = "text",
    x = 1,
    y = 42,
    label = "Barcelona",
    fontface = "italic",
    color = "black",
    size = 1.4
  ) +
  annotate(
    geom = "text",
    x = 4,
    y = 44.27,
    label = "Montpellier",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -5.8+0.4,
    y = 33.2+0.1,
    label = "Casablanca",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -16,
    y = 14.8,
    label = "Dakar",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -15+0.5,
    y = 21,
    label = "Cabo Blanco",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -5,
    y = 40,
    label = "ES",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -8,
    y = 32,
    label = "MA",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -14,
    y = 19,
    label = "MR",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -14,
    y = 15,
    label = "SN",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = -14,
    y = 23,
    label = "WS",
    fontface = "italic",
    color = "black",
    size = 1.4
  )+
  annotate(
    geom = "text",
    x = 0,
    y = 44,
    label = "FR",
    fontface = "italic",
    color = "black",
    size = 1.4
  )

x11();p


setwd(paste0(WD,"output/figures"))

Cairo::Cairo(file = "StudyArea.png",
             type = "png",
             units = "mm",
             width = 80,
             height = 100,
             dpi = 1000,
             bg = "white")
p
dev.off()

