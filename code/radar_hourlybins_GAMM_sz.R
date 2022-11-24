#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read gamm_models radar files and filter best model per population
# 2. Prepare again the datasets to perform the best GAMM model
# 3. Perform the best GAMM model per population
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*_gamm_models.csv", recursive = TRUE)

# Read all files
models_sz <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) %>%
  # group by per population
  group_by(population) %>%
  # filter lowest AIC
  slice(which.min(AIC))

########
#Step 2#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*HourlyBins_radar_evaluation.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*HourlyBins_radar_evaluation.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

# explore NAs
visdat::vis_dat(RAD)

# replace NA values from marine traffic data to 0
RAD <- RAD %>%
  replace(is.na(.), 0) 

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

# Add population label

RAD <- RAD %>%
  dplyr::mutate(population = recode(colonyName, 
                                    "CalaMorell" = "BalearicIs",
                                    "CVelho" = "CaboVerde",
                                    "MClara" = "CanaryIs",
                                    "Veneguera" = "CanaryIs")) 

populations <- unique(RAD$population)

for (i in seq_along(populations)) {
  
  #i = 1
  
  # ~~~~~~~~~~~~~~~~ #
  # Balearic Is. pop #
  # ~~~~~~~~~~~~~~~~ #
  
  if (populations[i] == "BalearicIs") {
    
    radar_group <- RAD %>%
      dplyr::filter(population == populations[i])
    
    m <- gamm4::gamm4( 
      radar_presence ~
        Light+
        Week+
        s(fishves_scaled ) +
        s(otheves_scaled ),
      random = ~(1| organismID/tripID) + (1|year),
      family = "binomial", radar_group, REML=TRUE)

    }
  
  # ~~~~~~~~~~~~~~ #
  # Canary Is. pop #
  # ~~~~~~~~~~~~~~ #
  
  if (populations[i] == "CanaryIs") {
    
    radar_group <- RAD %>%
      dplyr::filter(population == populations[i])
    
    m <- gamm4::gamm4( 
      radar_presence ~
        s(fishves_scaled ) +
        s(otheves_scaled ),
      random = ~(1| organismID/tripID) + (1|year),
      family = "binomial", radar_group, REML=TRUE)
  }
  
  # ~~~~~~~~~~~~~~ #
  # Cabo Verde pop #
  # ~~~~~~~~~~~~~~ #
  
  if (populations[i] == "CaboVerde") {
    
    radar_group <- RAD %>%
      dplyr::filter(population == populations[i])
    
    m <- gamm4::gamm4( 
      radar_presence ~
        Light+
        s(fishves_scaled ) +
        s(otheves_scaled ) + 
        s(domeves_scaled),
      random = ~(1| organismID/tripID), #+ (1|year)
      family = "binomial", radar_group, REML=TRUE)
  }
  
  # save model
  saveRDS(m, paste0(WD,"output/", unique(radar_group$population) ,"_bestGAMM.rds"))
  
  #---------------------------------------------------------------
  # Summary of the model and AUC calculation
  #---------------------------------------------------------------
  
  # Perform the prediction response
  predicted <- predict(m$gam, type = "response",radar_group)

  # parse to data frame
  predicted <-  as.data.frame(predicted)

  # calculate AUC
  observed <- radar_group %>%
    dplyr::select(population, radar_presence)

  # bind it
  data <- bind_cols(observed, predicted)

  # Calculate AUC and other accuracy parameters 
  accu <- PresenceAbsence::presence.absence.accuracy(data)

  #The plot from a good model will rise steeply to the upper left corner then level off quickly,resulting   in an AUC near 1.0.  A poor model (i.e. a model that is no better than random assignment) will have a     ROC plot lying along the diagonal, with an AUC near 0.5
  PresenceAbsence::auc.roc.plot(data, color = TRUE, legend.cex = 1.4, main = "")
  
  # save AUC results
  Fit <- accu %>%
    mutate(
      AUC = paste(round(AUC,2),round(AUC.sd,3),sep="?")) %>%
    dplyr::select(AUC)
  
  # Save summary 
  summary <- capture.output(summary(m$gam), file=NULL,append=FALSE, type="output",split=TRUE)
  summary <-as.data.frame(summary)

  summary[nrow(summary)+1,] <- paste("AUC=", Fit)
  
  # write it
  fwrite(summary, file=paste0(WD,"output/", unique(radar_group$population) ,"_bestGAMM_rawtable.csv"),row.names=FALSE)

  # Second summarized table to print
  
  # AUC sz
  AUC <- accu %>%
    mutate(
      AUC = paste(round(AUC,1),round(AUC.sd,3),sep="?")) %>%
    dplyr::select(AUC) %>%
    dplyr::mutate(Population = unique(radar_group$population))
  
  # Fixed sz
  FIXED <- broom::tidy(m$gam, parametric = TRUE) %>%
    dplyr::mutate(Population = unique(radar_group$population),
                Factor = "Fixed")
  
  # Smoothed sz
  SMOOTHED  <- broom::tidy(m$gam) %>%
    dplyr::mutate(Population = unique(radar_group$population),
                Factor = "Smoothed")
  
  # Bind everything
  summary <- bind_rows(FIXED, SMOOTHED, AUC)
  
  # write it
  fwrite(summary, file=paste0(WD,"output/", unique(radar_group$population) ,"_summary_bestGAMM.csv"),row.names=FALSE)
  
  #---------------------------------------------------------------
  # Substracting residuals
  #---------------------------------------------------------------
  
  # residuals from the model 
  residuals <- residuals(m$gam, type = "response",radar_group)
  
  # parse to dataframe
  residuals <- as.data.frame(residuals)
  
  # add info to the original dataset
  radar_group <- bind_cols(radar_group, residuals)
  
  # write it
  fwrite(radar_group, file=paste0(WD,"output/", unique(radar_group$population) ,"_residuals_bestGAMM.csv"),row.names=FALSE)
  
}
