#---------------------------------------------------------------
# Libraries and functions
#---------------------------------------------------------------

# to general coding
pacman::p_load("plyr", "purrr", "ggplot2", "stringr", "lubridate","readxl", "tidyverse", "tidylog", "data.table", "cowplot", "ggpubr")

# to manipulate track data
pacman::p_load("maptools", "rgdal", "sf", "fossil", "track2KBA", "bcpa", "scales", 
               "adehabitatLT", "rnaturalearth", "marmap", "terra", "adehabitatHR", "rgeos", "track2KBA")

# to perform and plot GAMM models
pacman::p_load("MuMIn", "gamm4", "PresenceAbsence", "broom", "ggeffects")

# to parallelize
pacman::p_load("parallel", "doParallel", "foreach")


#---------------------------------------------------------------
# Functions
#---------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Function to import all GPS files and removing points over land #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

sf::sf_use_s2(FALSE)# I need it to silence the  error in s2_geography_from_wkb(x, oriented = oriented, check = check) 

gps_import <- function(file_name) {
  #file_name <- files[1]
  cat(file_name, "\n")
  
  # Import the data, allow for old and new gps files (new ones have 6 empty rows on top)
  gps_data <- read_csv(file_name) #,fileEncoding= "UTF-8")
  
  # add ring, gps_deployment and colony info
  gps_data <- gps_data %>% 
    dplyr::select(Date, Time, Latitude, Longitude) %>% 
    rename(longitude = Longitude,
           latitude = Latitude) %>%
    drop_na() %>% 
    mutate(organismID = unlist(strsplit(file_name,"_"))[1], 
           deploymentID = paste(unlist(strsplit(file_name,"_"))[1], unlist(strsplit(file_name,"_"))[2], sep = "_"),
           colonyName = unlist(strsplit(file_name,"_"))[3],
           yearFile = substr(unlist(strsplit(file_name,"_"))[2],8,12))
  
  # Put the date and time together and parse
  gps_data <- gps_data %>% 
    mutate(
      time = dmy_hms(paste0(Date, Time)),
      year = lubridate::year(time)) %>%
    dplyr::filter(year == yearFile)%>%
    dplyr::select(-Date, -Time, -yearFile) 
  
  # remove duplicates rows (just works in case some repeated files were stored in the gps folder)
  gps_data <- gps_data %>%
    distinct()
  
  # overlay our data and the colony map to remove points on land 
  
  ## turn points to sf
  gps_data %>% 
    st_as_sf(coords = c("longitude", "latitude")) %>% 
    st_set_crs(st_crs(msk_sf)) -> gps_sf
  
  ## crop mask to sf extent
  msk_sf %>% 
    st_crop(gps_sf) %>% 
    mutate(land = "land") -> msk_sf_crop
  
  ## over
  gps_sf %>% 
    st_join(msk_sf_crop, join = st_intersects) -> gps_filtered
  
  ## turn back to sp
  gps_filtered %>% 
    filter(is.na(land)) %>% 
    dplyr::select(-land, -Name) %>% 
    mutate(latitude = sf::st_coordinates(.)[,2],
           longitude = sf::st_coordinates(.)[,1]) %>% 
    st_set_geometry(NULL) -> gps_data
  
  print(nrow(gps_sf)); print(nrow(gps_data))  
  
  #Output this new table.
  return(gps_data)}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Function to import radar files  #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

radar_import <- function(file_name) {
  #file_name <- files[1]
  cat(file_name, "\n")
  
  # Import the data, allow for old and new gps files (new ones have 6 empty rows on top)
  radar_data <- read_csv(file_name) #,fileEncoding= "UTF-8")
  
  # add ring, gps_deployment and colony info
  radar_data <- radar_data %>% 
    dplyr::select(Date, Time, Radar_level) %>% 
    drop_na() %>% 
    mutate(organismID = unlist(strsplit(file_name,"_"))[1], 
           deploymentID = paste(unlist(strsplit(file_name,"_"))[1], unlist(strsplit(file_name,"_"))[2], sep = "_"),
           colonyName = unlist(strsplit(file_name,"_"))[3],
           yearFile = substr(unlist(strsplit(file_name,"_"))[2],8,12))
  
  # Put the date and time together and parse
  radar_data <- radar_data %>% 
    mutate(
      time = dmy_hms(paste0(Date, Time)),
      year = lubridate::year(time)) %>%
    dplyr::filter(year == yearFile)%>%
    dplyr::select(-Date, -Time, -yearFile) 
  
  # remove duplicates rows (just works in case some repeated files were stored in the radar folder)
  radar_data <- radar_data %>%
    distinct()
  
  #Output this new table.
  return(radar_data)}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Function to remove pseudoduplicates - AnimalSensor GitHub #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

filter_dup  <- function (data, step.time = 2/60, step.dist = 0.001){
  
  ## Convert standard format to SDLfilter
  
  # Standardize Location clasess
  data$argosLC <- as.character(data$argosLC)
  data$argosLC[data$argosLC == "A"] <- -1
  data$argosLC[data$argosLC == "B"] <- -2
  data$argosLC[data$argosLC == "Z"] <- -9
  data$argosLC[data$argosLC == "G"] <- 4
  data$argosLC <- as.numeric(data$argosLC)
  
  # Rename columns
  names(data)[names(data)=="time"] <- "DateTime"
  names(data)[names(data)=="argosLC"] <- "qi"
  names(data)[names(data)=="longitude"] <- "lon"
  names(data)[names(data)=="latitude"] <- "lat"
  names(data)[names(data)=="organismID"] <- "id"
  
  ### Remove duplicated locations, based on both time and space criteria
  data <- SDLfilter::dupfilter(data.frame(data), step.time=step.time, step.dist=step.dist, conditional = FALSE)
  
  ## Back transform data.frame to standard format
  
  # Rename columns
  names(data)[names(data)=="DateTime"] <- "time"
  names(data)[names(data)=="qi"] <- "argosLC"
  names(data)[names(data)=="lon"] <- "longitude"
  names(data)[names(data)=="lat"] <- "latitude"
  names(data)[names(data)=="id"] <- "organismID"
  
  # Standardize Location clasess
  data$argosLC[data$argosLC == -1] <- "A"
  data$argosLC[data$argosLC == -2] <- "B"
  data$argosLC[data$argosLC == 4] <- "G"
  
  ## Prepare output
  return(data)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Function to remove overspeeded locations - AnimalSensor GitHub #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

filter_speed  <- function (data, vmax = 10.8, method = 1){
  
  ## Convert standard format to SDLfilter
  
  # Standardize Location clasess
  data$argosLC <- as.character(data$argosLC)
  data$argosLC[data$argosLC == "A"] <- -1
  data$argosLC[data$argosLC == "B"] <- -2
  data$argosLC[data$argosLC == "Z"] <- -9
  data$argosLC[data$argosLC == "G"] <- 4
  data$argosLC <- as.numeric(data$argosLC)
  
  # Rename columns
  names(data)[names(data)=="time"] <- "DateTime"
  names(data)[names(data)=="argosLC"] <- "qi"
  names(data)[names(data)=="longitude"] <- "lon"
  names(data)[names(data)=="latitude"] <- "lat"
  names(data)[names(data)=="organismID"] <- "id"
  
  
  ## Filter out values above speed threshold, considering both previous and subsequent positions
  data <- SDLfilter::ddfilter_speed(data, vmax = vmax, method = method)
  
  ## Back transform data.frame to standard format
  
  # Rename columns
  names(data)[names(data)=="DateTime"] <- "time"
  names(data)[names(data)=="qi"] <- "argosLC"
  names(data)[names(data)=="lon"] <- "longitude"
  names(data)[names(data)=="lat"] <- "latitude"
  names(data)[names(data)=="id"] <- "organismID"
  
  # Standardize Location clasess
  data$argosLC[data$argosLC == -1] <- "A"
  data$argosLC[data$argosLC == -2] <- "B"
  data$argosLC[data$argosLC == 4] <- "G"
  
  ## Prepare output
  return(data)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Function to group consecutive locations - AnimalSensor GitHub #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

timedif.segment <- function(x, thrs){
  
  # order time series
  x <- x[order(x)]
  
  # calculate difference between time steps
  tdif <- c(NA,as.numeric(difftime(x[-1], x[-length(x)], units="hours" )))
  
  # get breakpoints based on time threshold
  breaks <- c(0, which(tdif >= thrs), length(tdif)+1)
  
  # split into segments
  intervals <- cut(order(x), breaks, right=FALSE)
  segments <- as.numeric(intervals)
  return(segments)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Function to summarize general info from a tag file #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

glimpse_file <- function(file_name) {
  #file_name <- files[1]
  cat(file_name, "\n")
  
  # Import the data
  d <- read_csv(file_name) %>%
    # remove dates not filled
    drop_na(Date, Time) %>%
    # remove duplicates
    distinct() %>%
    # parse to date format
    dplyr::mutate(
      time = dmy_hms(paste0(Date, Time))) %>%
    summarize(
      # add deployment identification
      deploymentID = paste(unlist(strsplit(file_name,"_"))[1], unlist(strsplit(file_name,"_"))[2], sep = "_"),
      # add colony name
      colonyName = unlist(strsplit(file_name,"_"))[3],
      # add the year registered in file name
      yearFile = substr(unlist(strsplit(file_name,"_"))[2],8,12),
      # add first date registered
      time0 = min(time),
      # add last date registered
      time1 = max(time),
      # number of locations
      nrows = n())
  
  #Output this new table.
  return(d)
}
