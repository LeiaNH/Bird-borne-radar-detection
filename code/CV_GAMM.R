#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read gamm_models radar files and filter best model per population
# 2. Prepare again the datasets to perform the best GAMM model
# 3. Data partition (CV) per population
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
  slice(which.min(AICc))

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
pop_sz <- list()

for (i in seq_along(populations)) {

  print(populations[i])  
  #  i=1
  
  # filter the population
  radar_group <- RAD %>%
    dplyr::filter(population == populations[i]) 
  
  radar_group$organismID = as.factor(radar_group$organismID)
  
  length(unique(radar_group$organismID))
  
  #   set seed for reproducibility
  xpectr::set_test_seed(1)  
  
  foldset <- groupdata2::fold(radar_group, k = k_value, id_col = 'organismID') %>%
    rename(folds = .folds)
  
  foldset %>% 
    dplyr::group_by(folds) %>%
    summarize(n = length(unique(organismID)))
  
  
  accu_sz <- list()
  
    for (f in 1:k_value){
      
      #f=2
      
      testfold <- f
        
      train <- foldset %>%
        dplyr::filter(folds != testfold)
      
      test <- foldset %>%
        dplyr::filter(folds == testfold)
      
      
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
      
      
      # Perform the prediction response
      predicted <- predict(m$gam, type = "response", test)
      
      # parse to data frame
      predicted <-  as.data.frame(predicted)
      
      # calculate AUC
      observed <- test %>%
        dplyr::select(radar_presence)
      
      # bind it
      data <- bind_cols(observed, predicted)
      
      # Calculate AUC and other accuracy parameters 
      accu <- PresenceAbsence::presence.absence.accuracy(data)
      
      # parse to data frame
      accu <-  as.data.frame(accu)
      
      #The plot from a good model will rise steeply to the upper left corner then level off quickly,resulting   in an AUC near 1.0.  A poor model (i.e. a model that is no better than random assignment) will have a     ROC plot lying along the diagonal, with an AUC near 0.5
      PresenceAbsence::auc.roc.plot(data, color = TRUE, legend.cex = 1.4, main = "")
      
      # save data 
      accu <- accu %>%
        dplyr::select(AUC, AUC.sd) %>%
        dplyr::mutate(
          population = populations[i],
          test_iteration = testfold)
      
      
      accu_sz[f] <- list(accu)
      }
    
  accu_binded <- do.call(bind_rows, accu_sz)
  
  pop_sz[i] <- list(accu_binded)
    
  }
  
  
pop_binded <- do.call(bind_rows, pop_sz)

acc_sz <- pop_binded %>%
  group_by(population) %>%
  summarize(
    mean = round(mean(AUC),1),
    sd = round(sd(AUC),2)
  )

fwrite(acc_sz, file=paste0(WD,"/GitData/Bird-borne-radar-detection/output/","CV_gamm_acc.csv"),row.names=FALSE)

# Read original AUC from best models
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*_summary_bestGAMM.csv", recursive = TRUE)

# Read all files
(models_sz <- files %>%
   #read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))))


ggplot(acc_sz, aes(x= factor(population, level = c('BalearicIs', 'CanaryIs', 'CaboVerde')), y = mean, colour = population)) +  
   geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), show.legend = F)+
  geom_point(show.legend = F, alpha = 0.5, size=5) + 
  cols_pop + 
  geom_point (x = "BalearicIs", y = 0.7, colour= "black", shape = 4, size = 5) +
  geom_point (x = "CaboVerde", y = 0.9, colour= "black", shape = 4, size = ) +
  geom_point (x = "CanaryIs", y = 0.7, colour= "black", shape = 4, size = ) +
  ylim(c(0,1)) +
  theme_classic() +
  ylab("AUC") +
  xlab("") +
  scale_x_discrete(labels = c('Balearic Is.','Canary Is.','Cabo Verde'))

