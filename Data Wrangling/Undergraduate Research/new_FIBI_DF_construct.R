library(tidyverse)
library(skimr)
library(forcats)


#----------------------------------
#     Remake FIBI dataframe
#----------------------------------

# we would like to add in the year 
# that each site was sampled, the local
# (HUC12) watershed, and the rating
#----------------------------------

#load data
mydat <- read.csv("Data/Thesis/Tidy/FIBI_and_covariates.csv", header = T)
huc <- read.csv("Data/Thesis/Spatial/sites_with_HUC12.csv", header = T)
bigdat <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header = T)

#subset bigdat
names(bigdat)
bd2 <- bigdat %>%
  select(HUC_Site, Year) %>%
  rename(newID=HUC_Site)

#subset huc
names(huc)
huc12 <- huc %>%
  select(newID, HUC_10, HUC_12)
huc12$newID <- as.character(huc12$newID)
huc12[47,1] <- "YEL_97b"

#inspect mydat
names(mydat)
skim(mydat)

mydat <- mydat %>%
  rename(HUC8=HUC8.x)

#join bigdat to huc12
bdhuc <- left_join(huc12, bd2, by = "newID")

#join bdhuc to mydat
newDF <- left_join(mydat, bdhuc, by = "newID")
skim(newDF)
names(newDF)

#organize & subset
newDF2 <- newDF %>%
  select(newID, HUC8, HUC_10, HUC_12, Year, M6_BKTsalmonid, IBIScore_M, Rating_M, IBIScore_L, Rating_L,
         MEANT, pctrun, pctrock, pctShade, boulder, pctBrBnk, HAiFLS_dev, HAiFLS_for)

#Write csvs
write.csv(newDF2, "Data/Thesis/Tidy/FIBI_tidy2.csv", row.names = F)



