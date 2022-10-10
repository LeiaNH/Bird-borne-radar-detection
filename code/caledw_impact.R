#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. Breeding success test
# 2. Trip duration
# 3. Weight difference
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# Breeding success
d <- readr::read_csv2(paste0(WD,"GitData/Bird-borne-radar-detection/input/CurralVelho2019Impact/ImpactNEST_toR.csv"))

test <- chisq.test(table(d$Nest_type, d$Nest_Succ))

#If a warning such as “Chi-squared approximation may be incorrect” appears, it means that the smallest expected frequencies is lower than 5. To avoid this issue, you can use the Fisher’s exact test

test <- fisher.test(table(d$Nest_type, d$Nest_Succ))

########
#Step 2#
########

library(lmerTest)

d <- readr::read_csv2(paste0(WD,"GitData/Bird-borne-radar-detection/input/CurralVelho2019Impact/ImpactGPSINC_toR.csv")) %>% drop_na()

d %>% janitor::get_dupes(Ring)

d$Ring = as.factor(d$Ring)
d$Individual_type = as.factor(d$Individual_type)

m1 <- glmer(Diffdays ~ Individual_type + (1|Ring), family = poisson, data = d)

summary(m1)

#performance
performance::model_performance(m1)
#assumptions
performance::check_model(m1)
#performance::check_normality(m1)
#performance::check_heteroscedasticity(m1)
performance::check_singularity(m1)
#power test
sim_treat_m1 <- simr::powerSim(m1, nsim=1000, seed=25)
print(sim_treat_m1)
summary(sim_treat_m1)

########
#Step 3#
########

library(lmerTest)

d <- readr::read_csv2(paste0(WD,"GitData/Bird-borne-radar-detection/input/CurralVelho2019Impact/ImpactGPSINC_toR.csv")) %>% drop_na() %>%
  dplyr::mutate(logDiffBodymass = log10(DiffBodymass))

d$Ring = as.factor(d$Ring)
d$Individual_type = as.factor(d$Individual_type)


m2 <- lmer(DiffBodymass ~ Individual_type + (1|Ring), data = d)

summary(m2)

#performance
performance::model_performance(m2)
#assumptions
performance::check_model(m2)
performance::check_normality(m2)
performance::check_heteroscedasticity(m2)
performance::check_singularity(m2)
#power test
sim_treat_m2 <- simr::powerSim(m2, nsim=1000, seed=25)
print(sim_treat_m2)
