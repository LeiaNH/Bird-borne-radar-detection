# Read summarized file
SM <- read_csv2(paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/summarytable_edited.csv")) %>%
  # group by next levels
  dplyr::group_by(Species, Colony,  Organism, Sex, Breeding_phase, deploymentID, Deployment_Date, Recovery_Date) %>%
  dplyr::summarise(
    # count number of trips
    trips = length(unique(na.omit(tripID))),
    # count number of radar events
    radarEvents = sum(n_radarevents, na.rm = T),
    # count number of radar events within Core Areas
    radarEventsCA = sum(n_radarevents_ca, na.rm = T)
  )

# Add info of completeness of trips  
completeness <- read_csv2(paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/summarytable_edited.csv")) %>%
  # group by next levels
  dplyr::group_by(deploymentID, complete) %>%
  dplyr::summarise(
    # count number of trips
    n = length(unique(na.omit(tripID)))
    ) %>%
  ungroup() %>%
  # remove those deployments with no trip registered
  drop_na(complete) %>%
  # transpose table
  pivot_wider(names_from=complete, values_from=n)
  
# Merge both tables 
SM <- merge(SM, completeness, by = "deploymentID", all.x=T)

# remove this table not needed anymore
rm(completeness)

# Fix table 
SM <- SM %>%
  arrange(Species, Colony, Organism, Deployment_Date)

# Check that there is no duplicate in deploymentID
SM %>% janitor::get_dupes(deploymentID)

# Remove now this variable
SM <- SM %>%
  dplyr::select(-deploymentID)

# write table
write.csv(SM, paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/summarytable_final.csv"), row.names = F)
