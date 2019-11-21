##################################################

###   Making Maps with Fish and Enviro Data   ###

#################################################

library(tidyverse)

#load sampling locations -- this will include environmental covariates in case we want to map variability in those as well
locs <- read_csv("Data/Thesis/Tidy/AllCovariates.csv", col_names = T)

#load fish data
fish <- read_csv("Data/Thesis/Tidy/tidyfish1.csv", col_names=T)

#remove unwanted columns from environmental data
names(locs)
locs <- locs %>%
  select(uid, HUC_Site, HUC8, Site, Year, 82:87, Easting, Northing)

#prep fish data
names(fish2)

fish2 <- fish %>%
  mutate(SGCN_pres = ifelse(BKT+SRD+LND+Cottus+ABL+SPS>0,1,0), SGCN_rich = (BKT + SRD + LND + Cottus + ABL + SPS),
         SGCN_ab = (BKT_ab+SRD_ab+LND_ab+Cottus_ab+ABL_ab+SPS_ab)) %>%
  mutate(BKT_BRT = ifelse(BKT+BRT>1,1,0), BKT_RBT = ifelse(BKT+RBT>1,1,0), BRT_RBT = ifelse(BRT+RBT>1,1,0),
         BK_BR_RB = ifelse(BKT+BRT+RBT>2,1,0)) %>%
  select(uid, HUC8, site, richness, total_ab, SGCN_pres, SGCN_rich, SGCN_ab, BKT, BRT, BKT_BRT, BK_BR_RB)
