#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Steps:
# 1. read bestGAMM.rds files 
# 2. plot smooth factor per colony
# 3. composite plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

############
#scale list#
############

# List L2.csv extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*HourlyBins_radar_evaluation.csv", recursive = TRUE)

# Read all files
RAD <- files %>%
  # read in all the files, appending the path before the filename
  map_df(~ read_csv(file.path(paste0(WD,"output/"), .))) 

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


########
#Step 1#
########

# List _bestGAMM.rds extention files
files <- list.files(path = paste0(WD, "output/"), pattern = "*_bestGAMM.rds", recursive = TRUE)


########
#Step 2#
########

# population i = 1

i = 1
print(files[i])

model <-  readRDS(paste0(WD,"output/", files[i]))

m <- mgcViz::getViz(model$gam)

p <- plot(mgcViz::sm(m, 1) )

p <- p[["data"]][["fit"]]

p <- p %>%
  dplyr::mutate(
    x_unscaled=t(t(x) * scaleList$scale[[1]] + scaleList$center[[1]]))

b1 <- ggplot(p,aes(x=x_unscaled,y=y))+
    geom_line(colour = "#0072B2", size=0.5)+
    geom_rug(alpha = 1/2, position = "jitter")+
    theme_classic()+
    ylab("Effect") +
    xlab ("Log1p (hours) of fishing vessels") +
    theme_classic()+
    ylim(-4,5)+
    ggtitle("Balearic Is. population")+
  geom_ribbon(aes(ymin = y-se, ymax = y+se),fill="#0072B2",alpha=0.2)+
  theme(axis.text.x = element_text(size = 11),
          axis.text.y = element_text(size = 11),
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA))

b1_d<-p

p <- plot(mgcViz::sm(m, 2) )

p <- p[["data"]][["fit"]]

p <- p %>%
  dplyr::mutate(
    x_unscaled=t(t(x) * scaleList$scale[[3]] + scaleList$center[[3]]))

b2 <- ggplot(p,aes(x=x_unscaled,y=y))+
  geom_line(colour = "#0072B2", size=0.5)+
  geom_rug(alpha = 1/2, position = "jitter")+
  theme_classic()+
  ylab("Effect") +
  xlab (expression("Log1p (transits km"^-2*") of other vessels")) +
  theme_classic()+
  ylim(-4,5)+
  ggtitle("Balearic Is. population")+
  geom_ribbon(aes(ymin = y-se, ymax = y+se),fill="#0072B2",alpha=0.2)+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

b2_d<-p

# population i = 2

i = 3
print(files[i])

model <-  readRDS(paste0(WD,"output/", files[i]))

m <- mgcViz::getViz(model$gam)

p <- plot(mgcViz::sm(m, 1) )

p <- p[["data"]][["fit"]]
p <- p %>%
  dplyr::mutate(
    x_unscaled=t(t(x) * scaleList$scale[[1]] + scaleList$center[[1]]))

c1 <- ggplot(p,aes(x=x_unscaled,y=y))+
  geom_line(colour = "#f5c014", size=0.5)+
  geom_rug(alpha = 1/2, position = "jitter")+
  theme_classic()+
  ylab("Effect") +
  xlab ("Log1p (hours) of fishing vessels") +
  theme_classic()+
  ylim(-4,5)+
  ggtitle("Canary Is. population")+
  geom_ribbon(aes(ymin = y-se, ymax = y+se),fill="#f5c014",alpha=0.2)+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

c1_d <- p
# population i = 3

i = 2
print(files[i])

model <-  readRDS(paste0(WD,"output/", files[i]))

m <- mgcViz::getViz(model$gam)

p <- plot(mgcViz::sm(m, 1) )

p <- p[["data"]][["fit"]]
p <- p %>%
  dplyr::mutate(
    x_unscaled=t(t(x) * scaleList$scale[[1]] + scaleList$center[[1]]))

cv1 <- ggplot(p,aes(x=x_unscaled,y=y))+
  geom_line(colour = "#009E73", size=0.5)+
  geom_rug(alpha = 1/2, position = "jitter")+
  theme_classic()+
  ylab("Effect") +
  xlab ("Log1p (hours) of fishing vessels") +
  ggtitle("")+
  theme_classic()+
  ylim(-4,5)+
  ggtitle("Cabo Verde population")+
  geom_ribbon(aes(ymin = y-se, ymax = y+se),fill="#009E73",alpha=0.2)+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))
  
cv1_d<-p

p <- plot(mgcViz::sm(m, 2) )

p <- p[["data"]][["fit"]]
p <- p %>%
  dplyr::mutate(
    x_unscaled=t(t(x) * scaleList$scale[[3]] + scaleList$center[[3]]))

cv2 <- ggplot(p,aes(x=x_unscaled,y=y))+
  geom_line(colour = "#009E73", size=0.5)+
  geom_rug(alpha = 1/2, position = "jitter")+
  theme_classic()+
  ylab("Effect") +
  xlab (expression("Log1p (transits km"^-2*") of other vessels")) +
  ggtitle("")+
  theme_classic()+
  ylim(-4,5)+
  ggtitle("Cabo Verde population")+
  geom_ribbon(aes(ymin = y-se, ymax = y+se),fill="#009E73",alpha=0.2)+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

cv2_d<-p

p <- plot(mgcViz::sm(m, 3) )

p <- p[["data"]][["fit"]]
p <- p %>%
  dplyr::mutate(
    x_unscaled=t(t(x) * scaleList$scale[[2]] + scaleList$center[[2]]))

cv3 <- ggplot(p,aes(x=x_unscaled,y=y))+
  geom_line(colour = "#009E73", size=0.5)+
  geom_rug(alpha = 1/2, position = "jitter")+
  theme_classic()+
  ylab("Effect") +
  xlab (expression("Log1p (transits km"^-2*") of domestic vessels")) +
  ggtitle("")+
  theme_classic()+
  ylim(-4,5)+
  ggtitle("Cabo Verde population")+
  geom_ribbon(aes(ymin = y-se, ymax = y+se),fill="#009E73",alpha=0.2)+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))

cv3_d <-p


# plot 


composite <- ggpubr::ggarrange(b1,c1, cv1, b2, cv2, cv3,
                            #labels=c("A","B","C"), 
                            ncol = 3, nrow = 2)

setwd(paste0(WD,"output/figures"))

Cairo::Cairo(file = "ggeffects_FishingvsNonFishing.png",
             type = "png",
             units = "mm",
             width = 260,
             height = 130,
             dpi = 100,
             bg = "white")


composite

dev.off()


ggplot(cv1_d,aes(x=x_unscaled,y=y))+
  geom_line(colour = "#009E73", size=0.5)+
  geom_rug(alpha = 1/2, position = "jitter")+
  geom_line(data = b1_d, aes(x=x_unscaled,y=y),colour = "blue", size=0.5)+
  geom_line(data = c1_d, aes(x=x_unscaled,y=y),colour = "yellow", size=0.5)+
  theme_classic()+
  ylab("Effect") +
  xlab ("Log1p (hours) of fishing vessels") +
  ggtitle("")+
  theme_classic()+
  ylim(-4,5)+
  ggtitle("Balearic Is. & Cabo Verde pop")+
  geom_ribbon(aes(ymin = y-se, ymax = y+se),fill="#009E73",alpha=0.2)+
  geom_ribbon(data = b1_d, aes(ymin = y-se, ymax = y+se),fill="blue",alpha=0.2)+
  geom_ribbon(data = c1_d, aes(ymin = y-se, ymax = y+se),fill="yellow",alpha=0.2)+
  theme(axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA))
