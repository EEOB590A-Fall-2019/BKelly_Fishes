############################################################
############################################################
## Create Catchment Scale Environmental Covariate Dataset ##
############################################################
############################################################

library(tidyverse)
library(skimr)
library(forcats)

#read data
catch <- read_csv("Data/Thesis/Raw/All Years/Catchment_Attributes.csv", col_names = T) %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = F) %>%
  select(-FID, -OBJECTID)

FlowAcc <- read.csv("data/Thesis/Tidy/Watershed_Accumulation.csv") %>%
  select(HUC8, Site, floAcc_utm) %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = T)

#Examine structure, correct any data types, look for NA's
skim(catch)

skim(FlowAcc)

## Catch df 
#site number to character
catch$Site <- as.character(catch$Site)

#rename site numbers to include characters
catch[9,3] <- '57b'
catch[13,3] <- '14b'
catch[27,3] <- '78b'
catch[58,3] <- '118b'
catch[68,3] <- '32b'
catch[134,3] <- '28b'
catch[106,3] <- "97b"
catch[133,3] <- "48"
catch[133,1] <- "LMAQ_48"
##-----------------------------------------

#Left Join
catch2 <- left_join(catch, FlowAcc, by="newID") %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = F)

skim(catch2)
names(catch2)


#remove unwanted variables, rename variables, calculate density (per watershed area) instead of counts
catch3 <- catch2 %>%
  select(newID, floAcc_utm, Avg_Percen, Count_1, Count_2)%>%
  mutate(Acc_m2 = (floAcc_utm*79.636)) %>%
  mutate(Area_km2 = (Acc_m2/1000000)) %>%
  rename(AvgSlope = Avg_Percen, EnvFac = Count_1, CrossCat = Count_2)%>%
  mutate(EFac_Cat = (EnvFac/Area_km2), Cross_Cat = (CrossCat/Area_km2))%>%
  select(-EnvFac, -CrossCat, -floAcc_utm, -Acc_m2, -EnvFac)

summary(catch3$Area_km2)
hist(catch3$Area_km2)

#delete columns we will not use for analysis (YEL_33 and UPI_29 because of fishless status)
catch4 <- catch3 %>%
  filter(newID != "YEL_33" & newID != "UPI_29")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
##              Making a DF ready for occupancy analysis               ##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
brook <- read_csv("Data/Thesis/Tidy/BKT_occDF_RMARK.csv", col_names = T)
names(brook)
brook$freq <- 1
brook <- brook[,c(1,74,2:73)]
names(brook)

brook2 <- brook %>%
  rename(effort1 = t1_eff, effort2 = t2_eff, effort3 = t3_eff, pctpool = pctslow)%>%
  select(1:3, effort1, effort2, effort3, pctex21, MEANT, MAXT, RNGT, avgT, pctpool, 
         pctrock, pctBrBnk, pctShade, BRT_100m, HAiFLS_alt, HAiFLS_for)

#Join to new DF
occupancy_input <- full_join(brook2, catch3, by="newID")%>%
  filter(newID != "YEL_33" & newID != "UPI_29")

write.csv(occupancy_input, "Data/Thesis/Tidy/BKT_Occu_File.csv", row.names = F)




