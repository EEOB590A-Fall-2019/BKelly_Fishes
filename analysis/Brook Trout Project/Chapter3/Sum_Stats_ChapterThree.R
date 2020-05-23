### summary stats for Chapter Three

library(tidyverse)
library(skimr)

#load data
newdat <- read.csv("Data/Thesis/Tidy/chpt3_tidy.csv", header=T)
mydat <- read.csv("Data/Thesis/Tidy/SGCN_AllCovariates.csv", header=T)
ef <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header=T) %>%
  select(HUC_Site, effsec) %>%
  rename(newID = HUC_Site)

names(newdat)
names(mydat)

cobble <- mydat %>%
  select(newID, pctcbbl, SegLen, LND_ab, SRD_ab, Cottus_ab, Area_km2=CatArea_km2, HUC8)

cobble[97,7] = 28.8615

newdata <- left_join(newdat, cobble, by="newID")
newdata <- left_join(newdata, ef, by="newID")

names(newdata)

#fish stats
fish <- newdata %>%
  select(HUC8, Cottus_CPUE, SRD_CPUE, LND_CPUE, BRT, BRT_100m, med_len) %>%
  mutate(LND = ifelse(LND_CPUE>0,1,0), Cott = ifelse(Cottus_CPUE>0,1,0),
         SRD = ifelse(SRD_CPUE>0,1,0))

#-----LND
huc.lnd <- fish %>%
  group_by(HUC8) %>%
  tally(LND)

22/68*100
9/57*100
2/14*100

ldace <- fish %>%
  filter(LND == 1)

#-----SRD
huc.srd <- fish %>%
  group_by(HUC8) %>%
  tally(SRD)

19/68*100
10/57*100
4/14*100

sdace <- fish %>%
  filter(SRD == 1)

#-----SRD
huc.cot <- fish %>%
  group_by(HUC8) %>%
  tally(Cott)

4/68*100
13/57*100
3/14*100 

scul <- fish %>%
  filter(Cott ==1)

#-----BRT
74/139*100
9/14*100
31/57*100
34/68*100

#-----Sympat
37/139*100 #total
2/20*100 #sculp
19/33*100 #lnd
21/33*100 #srd

## habitat
means <- newdata %>%
  group_by(HUC8) %>%
  summarise(mean_for = mean(HAiFLS_for), sd_for = sd(HAiFLS_for),
            mean_flow = mean(mFlow), sd_flow = sd(mFlow),
            mean_cob = mean(pctcbbl), sd_cob = sd(pctcbbl),
            mean_fines = mean(pctfines), sd_fines = sd(pctfines),
            mean_dep = mean(avdep), sd_dep = sd(avdep))


-0.002 > -0.01










