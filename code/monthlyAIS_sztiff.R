#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read events_radar_L3 radar files and create a vector of months needed
# 2. summarize non-fishing vessel
# 3. save it
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*HourlyBins_radar_L3.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 

# effort months

effort_months <- RAD %>%
  mutate(
    month = lubridate::month(Date),
    date = paste0(year, "0", month, "01")
  ) %>%
  dplyr::select(date) %>%
  distinct() %>%
  pull()


# effort months available

sAIS_months <- list.files(path = paste0(WD, "input/sAIS")) 

# filter months that coincide with radar data

effort_months <- effort_months[effort_months %in% sAIS_months]

########
#Step 2#
########


for (i in seq_along(effort_months)){
  
  #i=1
  
  effort_months[i]
  
  cargo <- raster::raster(paste0(WD,"input/sAIS/",effort_months[i],"/",effort_months[i],"_CARGO_dens.tif"))
  
  passenger <- raster::raster(paste0(WD,"input/sAIS/",effort_months[i],"/",effort_months[i], "_PASSENGER_dens.tif"))
  
  tanker <- raster::raster(paste0(WD,"input/sAIS/",effort_months[i],"/",effort_months[i],"_TANKER_dens.tif"))
  
  other <- raster::raster(paste0(WD,"input/sAIS/",effort_months[i],"/",effort_months[i],"_OTHER_dens.tif"))
  
  
  nonfishves <- sum(cargo, passenger, tanker, other, na.rm = T) #, other
  domestic <- sum(cargo, passenger, tanker, na.rm = T) #, other
  
  ########
  #Step 3#
  ########
  
  # Save GeoTiff
  
  writeRaster(nonfishves, filename = paste0(WD,"output/",effort_months[i],"_sAISnonfishing.tif"),
              overwrite=TRUE)
  
  writeRaster(domestic, filename = paste0(WD,"output/",effort_months[i],"_sAISdomestic.tif"),
              overwrite=TRUE)
  }
  