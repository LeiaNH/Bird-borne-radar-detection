# List L2.csv extention files
files <- list.files(path = paste0(WD, "GitData/Bird-borne-radar-detection/output/"), pattern = "*gamm_models.csv", recursive = TRUE)

# Read all files
mod <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"GitData/Bird-borne-radar-detection/output/"), .))) %>%
  dplyr::select(-logLik, -weight) 

names(mod) <- c("intercept","light", "dom", "fish", "oth", "week", "df", "AIC", "delta", "population")

tab <- mod %>%
  dplyr::select(population, intercept, light, week, fish, dom, oth, df, AIC, delta) %>%
  dplyr::mutate(
    intercept = formatC( round(intercept, 2), format='f', digits=2),
    df = round(df, 0),
    AIC = formatC( round(AIC, 2), format='f', digits=2),
    delta = formatC( round(delta, 2), format='f', digits=2))

tab$population <- factor(tab$population,      # Reordering group factor levels
                           levels = c("BalearicIs",  "CanaryIs", "CaboVerde"),
                           labels = c("Balearic Islands", "Canary Islands", "Cabo Verde"))


# write dataset
fwrite(tab, file=paste0(WD,"GitData/Bird-borne-radar-detection/output/tables/allmodels.csv"),row.names=FALSE)
