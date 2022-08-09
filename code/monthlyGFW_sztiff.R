#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read events_radar_L3 radar files and create a vector of months needed
# 2. summarize fishing vessel
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
  
  # i=1
  
  effort_months[i]
  
  # Create dataframe of filenames dates
  effort_files <- tibble(
    file = list.files(paste0(WD, 'input/GFW/fleet-daily-csvs-100-v2'), 
                      pattern = '.csv', recursive = T, full.names = T),
    date = ymd(str_extract(file, 
                           pattern = '[[:digit:]]{4}-[[:digit:]]{2}-[[:digit:]]{2}')))
  
  # create a new variable with the same format as effort_months vector
  
  effort_files <- effort_files %>%
    dplyr::mutate(
      monthlydate = paste0(substr(as.character(date),1,4),substr(as.character(date),6,7),"01")) %>%
    dplyr::filter(monthlydate == effort_months[i])
  
  # Read in data
  fleet <- map_dfr(effort_files$file, .f = read_csv) 
  
  # The original data are at a resolution of 0.01 degrees
  summary(fleet$cell_ll_lat)
  
  # We're interested in making global maps and 0.01 degrees is a much finer resolution than necessary
  # Specify new (lower) resolution in degrees for aggregating data setting res value.
  
  # Transform data across all fleets and geartypes
  effort_fleet <- fleet %>% 
    mutate(
      # calculate new lat lon bins with desired resolution
      lat_bin = floor(cell_ll_lat/res) * res + 0.5 * res, 
      lon_bin = floor(cell_ll_lon/res) * res + 0.5 * res)
  
  # Re-aggregate the data to 0.25 degrees for navigation hours value
  effort_hours <- effort_fleet %>% 
    group_by(lon_bin, lat_bin) %>% 
    summarize(hours = sum(hours, na.rm = T)) 
  
  effort_hours <- raster::rasterFromXYZ(effort_hours, 
                                     crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  # Re-aggregate the data to 0.25 degrees for fishing hours value
  effort_fishhours <- effort_fleet %>% 
    group_by(lon_bin, lat_bin) %>% 
    summarize(hours = sum(fishing_hours, na.rm = T)) 
  
  effort_fishhours <- raster::rasterFromXYZ(effort_fishhours, 
                                crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  ########
  #Step 3#
  ########
  
  # Save GeoTiff
  
  writeRaster(effort_hours, filename = paste0(WD,"output/",effort_months[i],"_GFW_NavigationHours.tif"),
              overwrite=TRUE)
  
  writeRaster(effort_fishhours, filename = paste0(WD,"output/",effort_months[i],"_GFW_FishingHours.tif"),
              overwrite=TRUE)

}
