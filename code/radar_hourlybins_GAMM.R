#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read HourlyBins_radar_evaluation radar files
# 2. prepare dataset to GAMM function
# 3. run model per colony
# 4. save results
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*HourlyBins_radar_evaluation.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

########
#Step 2#
########

visdat::vis_dat(RAD)


# replace NA values from marine traffic data to 0
RAD <- RAD %>%
  replace(is.na(.), 0) 
  
names(RAD)

# Log1p transform marine traffic data
RAD <- RAD %>%
  dplyr::mutate(
    fishves_log1p = log1p(sumNavigatingHours_MonthlyGFW),
    domeves_log1p = log1p(sumDomestic_sAIS),
    otheves_log1p = log1p(sumOther_sAIS))

# Scale (z-scoring) marine traffic data

covariates <- RAD %>%
  dplyr::select(fishves_log1p,domeves_log1p,otheves_log1p)  

scaled <- scale(covariates, center= TRUE, scale=TRUE)

# Save scaled attributes

scaleList <- list(scale = attr(scaled, "scaled:scale"),
                  center = attr(scaled, "scaled:center"))

# parse to dataframe

scaled <- as.data.frame(scaled)

# rename 

scaled <- scaled %>%
  dplyr::rename(
    fishves_scaled = fishves_log1p,
    domeves_scaled = domeves_log1p,
    otheves_scaled = otheves_log1p)

# bind covariates 

RAD <- bind_cols(RAD, scaled)


########
#Step 3#
########

#-----------------#
# Prepare cluster #
#-----------------#

RAD <- RAD %>%
  dplyr::mutate(population = recode(colonyName, 
                    "CalaMorell" = "BalearicIs",
                    "CVelho" = "CaboVerde",
                    "MClara" = "CanaryIs",
                    "Veneguera" = "CanaryIs")) 

# let's %dopar% per colony site 
populations <- unique(RAD$population)

cores <- length(populations) #Define number of cores
cl <- makeCluster(cores)
registerDoParallel(cl)

#-----------------------#
# Parallel foreach loop #
#-----------------------#

foreach(i=1:length(populations), .packages=c("dplyr" ,"tidyverse","data.table", "lubridate", "stats", "MuMIn")) %dopar% {
  
  # i=2
  print(i)
  
  # read radar data
  radar_group <- RAD %>%
    #filter colony group
    dplyr::filter(population == populations[[i]]) 
  
  #------------------------------------#
  # spearman's correlation coefficient #
  #------------------------------------#
  
  rho <- radar_group %>% rstatix::cor_test(fishves_scaled, domeves_scaled, otheves_scaled, method = "spearman")
  
  rho <- rho %>%
    dplyr::mutate(rm = if_else(var1 == var2, T,F)) %>%
    dplyr::filter(rm == F) 

  #library("ggpubr")
  #ggscatter(radar_group, x = "otheves_scaled", y = "fishves_scaled", 
   #         add = "reg.line", conf.int = TRUE, 
    #        cor.coef = TRUE, cor.method = "spearman")
  
  # write dataset
  fwrite(rho, file=paste0(WD,"GitData/Bird-borne-radar-detection/output/",populations[[i]] ,"_spearmancorr.csv"),row.names=FALSE)
  
  #------#
  # GAMM #
  #------#
  
  # parse as factor those covariates that need to
  
  radar_group$year = as.factor(radar_group$year)
  radar_group$Light = as.factor(radar_group$Light)
  radar_group$Week = as.factor(radar_group$Week)
  radar_group$organismID = as.factor(radar_group$organismID)
  radar_group$tripID = as.factor(radar_group$tripID)
  
  
  if(populations[[i]] == "CaboVerde"){
    
    # run a full model set from the global model 
    
    radar_group <- radar_group %>% dplyr::select(-otheves_scaled) 
    
    model <-MuMIn::uGamm(radar_presence ~
                           Light+
                           Week+
                           s(fishves_scaled)+
                           s(domeves_scaled),
                         random = ~(1| organismID/tripID),
                         family = "binomial", radar_group, REML=TRUE)
    
    }else{
      
      # run a full model set from the global model 
      
      model <-MuMIn::uGamm(radar_presence ~
             Light+
             Week+
             s(fishves_scaled)+
             s(domeves_scaled)+
             s(otheves_scaled),
           random = ~(1| organismID/tripID)+(1|year),
           family = "binomial", radar_group, REML=TRUE)}
  
  
  # perform all combinations
  
  models <- MuMIn::dredge(model, rank = "AIC")
  
  # add population label
  
  models$population <- paste(populations[[i]])
  
  ########
  #Step 4#
  ########
  
  # write dataset
  fwrite(models, file=paste0(WD,"GitData/Bird-borne-radar-detection/output/", populations[[i]],"_gamm_models.csv"),row.names=FALSE)

}




