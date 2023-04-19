# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*HourlyBins_radar_evaluation.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

# others

OTH <- RAD %>% 
  dplyr::filter(
  colonyName == "CVelho")

# EAtlantic_map.R
# aim: Map of animal trajectories at-sea in Eastern Atlantic ocean

# Step 1. Set your working directory
# Step 2. Install and load libraries required.
# Step 3. Define your study area
# Step 4. Load bathymetry
# Step 5. Load land mask
# Step 6. Load EEZ 
# Step 7. Load tag deployment sites 
# Step 8. Load GPS files 
# Step 9. Plot it 
# Step 10. Save it


# -------------------- #
# Step 2. Requirements #
# -------------------- #

# install packages
#install.packages("marmap")
#install.packages("rnaturalearthdata")
#install.packages("sf")
#install.packages("ggplot2")
#install.packages("Cairo")
#install.packages("purrr")
#install.packages("readr")

# load libraries
library(marmap)
library(rnaturalearthdata)
library(sf)
library(ggplot2)
library(Cairo)
library(purrr)
library(readr)

# ------------------ #
# Step 3. Study area #
# ------------------ #

# set your extent
latmin <- min(GPS$latitude1)
latmax <- max(GPS$latitude1)
lonmin <- min(GPS$longitude1)
lonmax <- max(GPS$longitude1)

# ----------------------- #
# Step 4. Load bathymetry #
# ----------------------- #

bath <- marmap::getNOAA.bathy(
  lon1 = lonmin-1, lon2 = lonmax+1,
  lat1 = latmin-1, lat2 = latmax+1, 
  resolution = 4)

# ---------------------- #
# Step 5. Load land mask #
# ---------------------- #

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# ---------------- #
# Step 6. Load EEZ #
# ---------------- #

# from marineregions
# eezs <- sf::read_sf(paste0(WD,"input/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp")) 

# --------------------------------- #
# Step 7. Load tag deployment sites #
# --------------------------------- #

#sites <- read.csv2(paste0(WD,"input/sites.csv"))

# ---------------------- #
# Step 8. Load GPS files #
# ---------------------- #


GPS <- OTH

# --------------- #
# Step 9. Plot it #
# --------------- #

p <- 
  # plot bathymetry
  ggplot(bath) +
  # fill background in white
  geom_raster(aes(x, y),fill="white")+ 
  # add GPS paths
  geom_point(data = GPS, aes(x=longitude1, y=latitude1), alpha=0.8, show.legend = T) +
  # plot land mask
  geom_sf(data = world, 
          color = "gray30", fill = "gray90",lwd  = 0.05) +
  # plot EEZ
  #geom_sf(data = eezs,
  #        color = alpha("gray30",0.4),
  #        fill = NA,
  #        lwd  = 0.05,
  #        lty = 1)+     
  # add bathymetry -200 line 
  geom_contour(data = bath, aes(x, y, z = z),
               breaks=c(-200),
               colour="gray10", lwd  = 0.1 ,
               lty = 1) + 
  # extent
  coord_sf(xlim = c(lonmin, lonmax), ylim = c(latmin, latmax))+
  # add scale
  ggspatial::annotation_scale(location = "br", 
                              line_col ="black",
                              text_col="black",
                              height = unit(0.1, "cm"),
                              line_width = 0.5,
                              text_cex= 0.5,
                              style = "ticks") +
  # define theme
  theme_bw()+
  theme(strip.text = element_text(size=3),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),        
        plot.margin = unit(c(0, 0, 0, 0), "lines"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.position="none"
  )+
  # define x and y labels
  xlab("Longitude") + ylab("Latitude") +
  facet_wrap(~radar_presence)


#x11();p

# ---------------- #
# Step 10. Save it #
# ---------------- #

setwd(paste0(WD,"output/"))

Cairo::Cairo(file = "EAtlantic.png",
             type = "png",
             units = "mm",
             width = 80,
             height = 100,
             dpi = 1000,
             bg = "white")
p
dev.off()