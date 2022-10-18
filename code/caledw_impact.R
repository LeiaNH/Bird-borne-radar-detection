#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. Breeding success test for incubation period
# 2. Breeding success test for all breeding period
# 2. Trip duration
# 3. Weight difference
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


# load deployments data of breeders
deployments <- readxl::read_excel("D:/Dropbox/GitData/Bird-borne-radar-detection/input/CurralVelho2019Impact/CALEDW_Impact.xlsx", sheet = "deployments") %>%
  dplyr::filter(
    # Filter deployments replaced
    RemovedLNH == 0,
    # Remove non-breeders
    Breeding_phase != 4) %>%
  dplyr::select(-c(RemovedLNH, TAG_model))

# load b success data
bsuccess <- readxl::read_excel("D:/Dropbox/GitData/Bird-borne-radar-detection/input/CurralVelho2019Impact/CALEDW_Impact.xlsx", sheet = "bsuccess") %>%
  dplyr::select(Burrow, EggSuccLNH, ChickSuccLNH, NaturalChickDeath)

# merge both data
d <- merge(bsuccess, deployments, by = "Burrow", all.x = T)

########
#Step 1#
########

# ~~~~~~~~~~~~~~~~~~~~~~
# For Incubation period
# ~~~~~~~~~~~~~~~~~~~~~~

# filter incubation period and control nests (where individual type is equal to NA)
bs <- d %>%
  dplyr::filter(
    Breeding_phase == 2 | is.na(Individual_type))

# Select those Burrows that were at least once radar burrow
RadarBurrows <- bs %>%
  dplyr::filter(Individual_type == "Radar") %>%
  dplyr::select(Burrow) %>%
  distinct() %>%
  dplyr::mutate(Burrow_type = "Radar")

# Merge inFo
bs <- merge(bs, RadarBurrows, by = "Burrow", all.x = TRUE)

# Complete other burrow types
bs <- bs %>%
  dplyr::mutate(
    Burrow_type = case_when(Burrow_type == "Radar" ~ "Radar",
                   is.na(Individual_type) ~ "Control",
                   TRUE ~ "CatAxy")) 

# Select desired vars
bs <- bs %>%
  dplyr::select(Burrow_type, Burrow, EggSuccLNH) %>%
  distinct()

# Check if any egg did not hatch because of natural reasons

# parse to categorical 
bs$Burrow_type = as.factor(bs$Burrow_type)
bs$EggSuccLNH = as.factor(bs$EggSuccLNH)

# sample size
table(bs$Burrow_type)

# Chi2
test <- chisq.test(table(bs$Burrow_type, bs$EggSuccLNH))

#If a warning such as “Chi-squared approximation may be incorrect” appears, it means that the smallest expected frequencies is lower than 5. 
table(bs$Burrow_type, bs$EggSuccLNH)

#to avoid this issue, you can use the Fisher’s exact test

test <- fisher.test(table(bs$Burrow_type, bs$EggSuccLNH))

test



########
#Step 2#
########

# ~~~~~~~~~~~~~~~~~~~~~~~~
# For all breeding period
# ~~~~~~~~~~~~~~~~~~~~~~~~

# filter incubation period and control nests (where individual type is equal to NA)
bs <- d %>%
  dplyr::filter(
    Breeding_phase == 2 | is.na(Individual_type) | Breeding_phase == 3)

# Select those Burrows that were at least once radar burrow
RadarBurrows <- bs %>%
  dplyr::filter(Individual_type == "Radar") %>%
  dplyr::select(Burrow) %>%
  distinct() %>%
  dplyr::mutate(Burrow_type = "Radar")

# Merge info
bs <- merge(bs, RadarBurrows, by = "Burrow", all.x = TRUE)

# Complete other burrow types
bs <- bs %>%
  dplyr::mutate(
    Burrow_type = case_when(Burrow_type == "Radar" ~ "Radar",
                            is.na(Individual_type) ~ "Control",
                            TRUE ~ "CatAxy")) 

# Select desired vars
bs <- bs %>%
  dplyr::select(Burrow_type, Burrow, ChickSuccLNH, NaturalChickDeath) %>%
  distinct()

# Check if any chick did not succed because of natural reasons

bs %>%
  dplyr::filter(!is.na(NaturalChickDeath)) %>%
  group_by(Burrow_type) %>%
  summarize(
    burrows = n()
  )

bs <- bs %>%
  dplyr::filter(is.na(NaturalChickDeath))

# parse to categorical 
bs$Burrow_type = as.factor(bs$Burrow_type)
bs$ChickSuccLNH = as.factor(bs$ChickSuccLNH)

# sample size
table(bs$Burrow_type)

# Chi2
test <- chisq.test(table(bs$Burrow_type, bs$ChickSuccLNH))
test

#If a warning such as “Chi-squared approximation may be incorrect” appears, it means that the smallest expected frequencies is lower than 5. 
table(bs$Burrow_type, bs$ChickSuccLNH)

#to avoid this issue, you can use the Fisher’s exact test

test <- fisher.test(table(bs$Burrow_type, bs$ChickSuccLNH))

test

########
#Step 3#
########

# weight loss

replacements <- readxl::read_excel("D:/Dropbox/GitData/Bird-borne-radar-detection/input/CurralVelho2019Impact/CALEDW_Impact.xlsx", sheet = "deployments") %>%
  dplyr::filter(
    # Filter deployments removed
    RemovedLNH == 1) %>%
  # remove birds that were not weighted after that
  drop_na(Bodymass_after) %>%
  dplyr::mutate(
    #calculate number of days between manipulations
    diffDays = as.numeric(difftime(Recovery_Date, Deployment_Date)),
    #calculate diff weight
    diffBM = Bodymass_before - Bodymass_after,
    #calculate weight loss per day
    BMlost = diffBM/diffDays) %>%
  #summarize mean and sd
  summarize(
    median = median(BMlost),
    mean = mean(BMlost),
    sd = sd(BMlost),
    n = n()
  )

BDlost = replacements$median

# BM gain/lost 

bm <- d %>%
  # filter incubation period
  dplyr::filter(
    Breeding_phase == 2,
    LostLNH == 0) %>%
  # calculate weight departure corrected
  dplyr::mutate(
    Bodymass_before_corr = if_else(
      Departure_Date == 0,
      Bodymass_before,
      Bodymass_before - BDlost*Departure_Date
    )
    )

# recalculate the percentage of the tag for radar individuals

weightINC <- bm %>%
  dplyr::filter(Individual_type == "Radar") %>%
  dplyr::mutate(
    percent = (20/Bodymass_before_corr)*100) %>%
  pull(percent)

weightCR <- d %>%
  dplyr::filter(Individual_type == "Radar") %>%
  # filter incubation period
  dplyr::filter(
    Breeding_phase == 3) %>%
  dplyr::mutate(
  percent = (20/Bodymass_before)*100) %>%
  pull()
    
weigth <- c(weightINC,weightCR)   

mean(weigth)
sd(weigth)
max(weigth)

#it's the same as before 

library(lmerTest)


bm$Ring = as.factor(bm$Ring)
bm$Individual_type = as.factor(bm$Individual_type)


# calculate diffdays and model it
days <- bm %>%
  dplyr::mutate(
    Diffdays = as.numeric(difftime(Recovery_Date, Last_Date_Nest, units = "days"))
  )

ggplot(days, aes(y=Diffdays, x=Individual_type )) + geom_boxplot()

m1 <- glmer(Diffdays ~ Individual_type + (1|Ring), family = poisson, data = days)

# provar sense alguna anella repetida
summary(m1)

#performance
performance::model_performance(m1)
#assumptions
performance::check_model(m1)
#performance::check_normality(m1)
#performance::check_heteroscedasticity(m1)
performance::check_singularity(m1)
#power test
#sim_treat_m1 <- simr::powerSim(m1, nsim=1000, seed=25)
#print(sim_treat_m1)
#summary(sim_treat_m1)

# try to solve singularity 
days %>% janitor::get_dupes(Ring)

rings <- unique(days$Ring)

l <- list()
for (i in 1:length(rings)){
  print(i)
  #i=3
  ring <- days %>%
    dplyr::filter(Ring == rings[[i]])
  
  if(nrow(ring)>1){
    if("Radar" %in% unique(ring$Individual_type)){
      ring <- ring %>% dplyr::filter(Individual_type == "Radar") %>% slice_sample(n = 1)
    }else{
      ring <- ring %>% slice_sample(n = 1)
    }
  }
  
  l[i] <- list(ring)
  }

days <- do.call(rbind, l)

days %>% janitor::get_dupes(Ring)

m1 <- glmer(Diffdays ~ Individual_type + (1|Ring), family = poisson, data = days)


########
#Step 4#
########

library(lmerTest)

bm <- bm %>%
  dplyr::mutate(
    Diffmass = Bodymass_after - Bodymass_before_corr,
    Percentdiffmass = (Diffmass/Bodymass_before_corr)*100) 
  
#How many cannot do it?
table(is.na(bm$Percentdiffmass))

#Let's remove them
bm <- bm %>% drop_na(Percentdiffmass)

bm$Ring = as.factor(bm$Ring)
bm$Individual_type = as.factor(bm$Individual_type)

ggplot(bm, aes(y=Diffmass, x=Individual_type )) + geom_boxplot() + geom_point()
ggplot(bm, aes(y=Percentdiffmass, x=Individual_type )) + geom_boxplot() + geom_point()

m2 <- lmer(Diffmass ~ Individual_type + (1|Ring), data = bm)
m2 <- lmer(Percentdiffmass ~ Individual_type + (1|Ring), data = bm)

summary(m2)

#performance
performance::model_performance(m2)
#assumptions
performance::check_model(m2)
performance::check_normality(m2)
performance::check_heteroscedasticity(m2)
performance::check_singularity(m2)
#power test
#sim_treat_m2 <- simr::powerSim(m2, nsim=1000, seed=25)
#print(sim_treat_m2)

# try to solve singularity 
bm %>% janitor::get_dupes(Ring)

rings <- unique(bm$Ring)

l <- list()
for (i in 1:length(rings)){
  print(i)
  #i=3
  ring <- bm %>%
    dplyr::filter(Ring == rings[[i]])
  
  if(nrow(ring)>1){
    if("Radar" %in% unique(ring$Individual_type)){
      ring <- ring %>% dplyr::filter(Individual_type == "Radar") %>% slice_sample(n = 1)
    }else{
      ring <- ring %>% slice_sample(n = 1)
    }
  }
  
  l[i] <- list(ring)
}

bm <- do.call(rbind, l)

