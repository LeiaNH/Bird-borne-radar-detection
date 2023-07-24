#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read events_radar_GFWovr_L4 radar files
# 2. summarize per radar event the presence or absence of vessels and geartype freq.
# 3. summarize per radar location the presence or absence of vessels and geartype freq.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

########
#Step 1#
########

# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*events_radar_GFWovr_L4.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) 

# Number of trips with radar events
RAD %>%
  mutate(population = recode(colonyName, 
                             "CalaMorell" = "BalearicIs",
                             "CVelho" = "CaboVerde",
                             "MClara" = "CanaryIs",
                             "Veneguera" = "CanaryIs")) %>%
  group_by(population) %>%
  summarize(
    n = length(unique(na.omit(tripID)))
  )

# Number of radar events per population
RAD %>%
  mutate(population = recode(colonyName, 
                             "CalaMorell" = "BalearicIs",
                             "CVelho" = "CaboVerde",
                             "MClara" = "CanaryIs",
                             "Veneguera" = "CanaryIs"),
         radarID2 = paste0(tripID, "_", radarID)) %>%
  group_by(population) %>%
  summarize(
    n = length(unique(na.omit(radarID2)))
  )

# Number of radar events per population overlapping with GFW
(kk <- RAD %>%
  filter(GFWovr == "1" ) %>%
  mutate(population = recode(colonyName, 
                             "CalaMorell" = "BalearicIs",
                             "CVelho" = "CaboVerde",
                             "MClara" = "CanaryIs",
                             "Veneguera" = "CanaryIs"),
         radarID2 = paste0(tripID, "_", radarID)) %>%
  group_by(population) %>%
  summarize(
    n = length(unique(na.omit(radarID2)))
  ))

# Number of radar events per population NOT overlapping with GFW
(kk<-RAD %>%
  filter(GFWovr == "0" ) %>%
  mutate(population = recode(colonyName, 
                             "CalaMorell" = "BalearicIs",
                             "CVelho" = "CaboVerde",
                             "MClara" = "CanaryIs",
                             "Veneguera" = "CanaryIs"),
         radarID2 = paste0(tripID, "_", radarID)) %>%
  group_by(population) %>%
  summarize(
    n = length(unique(na.omit(radarID2)))
  ))
  
########

flags <- RAD %>%
  dplyr::filter(GFWovr == 1) %>%
  dplyr::mutate(
    radarID2 = paste0(tripID, "_", radarID))  %>%
  dplyr::select(colonyName, radarID2, ESP, FRA, SEN, CHN, KOR, TUR, GEO, LTU, NLD, DEU, MAR, PRT, LVA, MRT)%>%
  group_by(colonyName, radarID2) %>%
  summarize(
    ESP = if_else(sum(ESP)>0,1,0),
    FRA = if_else(sum(FRA)>0,1,0),
    SEN = if_else(sum(SEN)>0,1,0),
    CHN = if_else(sum(CHN)>0,1,0),
    KOR = if_else(sum(KOR)>0,1,0),
    TUR = if_else(sum(TUR)>0,1,0),
    GEO = if_else(sum(GEO)>0,1,0),
    LTU = if_else(sum(LTU)>0,1,0),
    NLD = if_else(sum(NLD)>0,1,0),
    DEU = if_else(sum(DEU)>0,1,0),
    MAR = if_else(sum(MAR)>0,1,0),
    PRT = if_else(sum(PRT)>0,1,0),
    LVA = if_else(sum(LVA)>0,1,0),
    MRT = if_else(sum(MRT)>0,1,0)
  ) %>%
  group_by(colonyName) %>%
  summarize(
    ESP = sum(ESP, na.rm = T),
    FRA = sum(FRA, na.rm = T),
    SEN = sum(SEN, na.rm = T),
    CHN = sum(CHN, na.rm = T),
    KOR = sum(KOR, na.rm = T),
    TUR = sum(TUR, na.rm = T),
    GEO = sum(GEO, na.rm = T),
    LTU = sum(LTU, na.rm = T),
    NLD = sum(NLD, na.rm = T),
    DEU = sum(DEU, na.rm = T),
    MAR = sum(MAR, na.rm = T),
    PRT = sum(PRT, na.rm = T),
    LVA = sum(LVA, na.rm = T),
    MRT = sum(MRT, na.rm = T))%>%
  gather(., Flag, Count, ESP:MRT, factor_key=TRUE) %>%
  mutate(population = recode(colonyName, 
                    "CalaMorell" = "BalearicIs",
                    "CVelho" = "CaboVerde",
                    "MClara" = "CanaryIs",
                    "Veneguera" = "CanaryIs"))%>%
  dplyr::filter(Count>0) %>%
  mutate(Flag2 = recode(Flag, 
                             "ESP" = "Spain",
                             "FRA" = "France",
                             "SEN" = "Senegal",
                             "CHN" = "China",
                             "KOR" = "Korea",
                             "TUR" = "Turkey",
                             "GEO" = "Georgia",
                             "LTU" = "Lithuania",
                             "NLD" = "Netherlands",
                             "DEU" = "Germany",
                             "MAR" = "Morocco",
                             "PRT" = "Portugal",
                             "LVA" = "Latvia",
                             "MRT" = "Mauritania")) %>%
  mutate(country = fct_reorder(Flag2, Count,.desc = TRUE)) 


flags$population <- factor(flags$population,      # Reordering group factor levels
                         levels = c("BalearicIs",  "CanaryIs", "CaboVerde"),
                         labels = c("Balearic Is.", "Canary Is.", "Cabo Verde"))

ggplot(flags, aes(x=country, y=Count, fill=colonyName)) + 
  geom_col() + 
  scale_fill_manual(name="colonyName",
                    values = c("CalaMorell"= "#0072B2",
                               "MClara"= "#E69F00",
                               "Veneguera"= "#D55E00",
                               "CVelho"= "#009E73"), 
                    labels = c("CalaMorell"= "Cala Morell",
                               "MClara"= "Montaña Clara",
                               "Veneguera"= "Veneguera",
                               "CVelho"= "Curral Velho"))+
  theme_bw() + 
  facet_wrap(~population, ncol = 1) + 
  coord_flip() +
  guides(fill=guide_legend("Colony"))+
  ylab("Number of Radar events") + 
  xlab("Vessel Flag") 

total <- flags %>%
  group_by(population) %>%
  summarize(n=sum(Count))

flags2 = merge(flags, total, by="population")

########
#Step 2#
########

sz <- RAD %>%
  dplyr::select(-c(ESP, FRA, SEN, CHN, KOR, TUR, GEO, LTU, NLD, DEU, MAR, PRT, LVA, MRT)) %>%
  # label NA from GFW overlap
  dplyr::mutate(
    GFW_NA = if_else(is.na(GFWovr),T,F)) %>%
  # now you can parse NA values to 0
  replace(is.na(.), 0) %>%
  # create unique radar events identification
  dplyr::mutate(
    radarID2 = paste0(tripID, "_", radarID),
    # parse GFWovr as factor
    GFWovr = as.factor(GFWovr),
    # recode GFWovr
    GFWovr = recode(GFWovr, 
                        "0" ="absence",
                        "1"="presence"),
    # add population label
    population = recode(colonyName, 
                        "CalaMorell" = "BalearicIs",
                        "CVelho" = "CaboVerde",
                        "MClara" = "CanaryIs",
                        "Veneguera" = "CanaryIs")) 

sz1 <- sz%>%
  # group by colony
  group_by(population, GFWovr) %>%
  # summarize
  summarize(
    n_radarID=length(unique(radarID2))) %>%
  # pivot table from long to wide
  pivot_wider(names_from=GFWovr, values_from=n_radarID) %>%
  replace(is.na(.), 0) %>%
  # calculate percentage
  dplyr::mutate(
    perc_absence = round((absence / (presence + absence))*100,1))

sz1 <- sz%>%
  dplyr::filter(GFW_NA == "TRUE") %>%
  # group by colony
  group_by(population) %>%
  # summarize
  summarize(
    NAs=length(unique(radarID2))) %>%
  left_join(sz1,., by = "population") %>%
  dplyr::mutate(perc_absenceNA = round((NAs /absence)*100,1))

# write dataset
fwrite(sz1, file=paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/radarevents_dailyGFW_sz.csv"),row.names=FALSE)

head(sz1)

# from presence, which geartypes are registered
sz2 <- sz %>%
  dplyr::filter(GFWovr == "presence") %>%
  group_by(population, radarID2) %>%
  summarize(
    trawl = if_else(sum(trawlers)>0,1,0),
    purse = if_else(sum(tuna_purse_seines, purse_seines, other_purse_seines)>0,1,0),
    longlines = if_else(sum(set_longlines)>0,1,0),
    others = if_else(sum(fixed_gear, fishing, pole_and_line)>0,1,0)
  ) %>%
  group_by(population) %>%
  summarize(
    trawl = sum(trawl),
    purse = sum(purse),
    longlines = sum(longlines),
    others = sum(others))

sz2

sz1 <- sz1 %>% dplyr::select(population, presence)  

sz2 <- merge(sz2, sz1, by="population")

sz2 <-  sz2 %>%
  dplyr::mutate(
    trawl_perc = round((trawl / presence)*100,1),
    purse_perc = round((purse / presence)*100,1),
    longlines_perc = round((longlines / presence)*100,1),
    others_perc = round((others / presence)*100,1)
  ) %>%
  dplyr::select(population, trawl_perc, purse_perc, longlines_perc, others_perc)

# write dataset
fwrite(sz2, file=paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/radarevents_dailyGFW_geartype_sz.csv"),row.names=FALSE)

########
#Step 3#
########

sz1 <- sz%>%
  # group by colony
  group_by(population, GFWovr) %>%
  # summarize
  summarize(
    n_radarID=length(radarID2)) %>%
  # pivot table from long to wide
  pivot_wider(names_from=GFWovr, values_from=n_radarID) %>%
  replace(is.na(.), 0) %>%
  # calculate percentage
  dplyr::mutate(
    perc_absence = round((absence / (presence + absence))*100,1))

sz1 <- sz%>%
  dplyr::filter(GFW_NA == "TRUE") %>%
  # group by colony
  group_by(population) %>%
  # summarize
  summarize(
    NAs=length(radarID2)) %>%
  left_join(sz1,., by = "population") %>%
  dplyr::mutate(perc_absenceNA = round((NAs /absence)*100,1))

# write dataset
fwrite(sz1, file=paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/radardetections_dailyGFW_sz.csv"),row.names=FALSE)

head(sz1)

# from presence, which geartypes are registered
sz2 <- sz %>%
  dplyr::filter(GFWovr == "presence") %>%
  group_by(population) %>%
  summarize(
    trawl = sum(trawlers),
    purse = sum(tuna_purse_seines, purse_seines, other_purse_seines),
    longlines = sum(set_longlines),
    others = sum(fixed_gear, fishing, pole_and_line))

sz1 <- sz1 %>% dplyr::select(population, presence)  

sz2 <- merge(sz2, sz1, by="population")

sz2 <-  sz2 %>%
  dplyr::mutate(
    trawl_perc = round((trawl / presence)*100,1),
    purse_perc = round((purse / presence)*100,1),
    longlines_perc = round((longlines / presence)*100,1),
    others_perc = round((others / presence)*100,1)
  ) %>%
  dplyr::select(population, trawl_perc, purse_perc, longlines_perc, others_perc)

# write dataset
fwrite(sz2, file=paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/radardetections_dailyGFW_geartype_sz.csv"),row.names=FALSE)

