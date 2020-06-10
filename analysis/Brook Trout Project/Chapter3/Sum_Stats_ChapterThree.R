### summary stats for Chapter Three
library(tidyverse)
library(skimr)

#load data
newdat <- read.csv("Data/Thesis/Tidy/chpt3_tidy.csv", header=T)
mydat <- read.csv("Data/Thesis/Tidy/SGCN_AllCovariates.csv", header=T)
ef <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header=T) %>%
  select(HUC_Site, effsec, avgT) %>%
  rename(newID = HUC_Site)

skim(ef)


names(newdat)
names(mydat)

cobble <- mydat %>%
  select(newID, pctcbbl, SegLen, LND_ab, SRD_ab, Cottus_ab, Area_km2=CatArea_km2, HUC8)

cobble[97,7] = 28.8615

newdata <- left_join(newdat, cobble, by="newID")
newdata <- left_join(newdata, ef, by="newID") %>%
  filter(newID != "UPI_165")

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
33/138*100
22/68*100
9/56*100
2/14*100

ldace <- fish %>%
  filter(LND == 1)

#-----SRD
huc.srd <- fish %>%
  group_by(HUC8) %>%
  tally(SRD)
32/138*100
19/68*100
9/56*100
4/14*100

sdace <- fish %>%
  filter(SRD == 1)

#-----Cott
huc.cot <- fish %>%
  group_by(HUC8) %>%
  tally(Cott)
20/138*100
4/68*100
13/56*100
3/14*100 

scul <- fish %>%
  filter(Cott ==1)

#-----BRT
74/138*100
9/14*100
31/56*100
34/68*100

#-----Sympat
37/138*100 #total
2/20*100 #sculp
19/33*100 #lnd
21/33*100 #srd




20/32*100



## habitat
means <- newdata %>%
  group_by(HUC8) %>%
  summarise(mean_area = mean(Area_km2), sd_area = sd(Area_km2), min_ar=min(Area_km2), max_ar=max(Area_km2),
            mean_for = mean(HAiFLS_for), sd_for = sd(HAiFLS_for), min_for=min(Area_km2), max_for=max(Area_km2),
            mean_flow = mean(mFlow), sd_flow = sd(mFlow), min_flo=min(mFlow), max_flo=max(mFlow),
            mean_cob = mean(pctcbbl), sd_cob = sd(pctcbbl),
            mean_fines = mean(pctfines), sd_fines = sd(pctfines),
            mean_dep = mean(avdep), sd_dep = sd(avdep))


-0.002 > -0.01



cpue <- read.csv("Data/Thesis/Tidy/cpue_data.csv", header=T) %>%
  filter(newID != "UPI_165") %>%
  separate(newID, into = c("HUC8","Site"), remove = T)

skim(cpue)

means2 <- cpue %>%
  group_by(HUC8) %>%
  summarise(mean_area = mean(Area_km2), sd_area = sd(Area_km2),
            mean_for = mean(HAiFLS_for), sd_for = sd(HAiFLS_for),
            mean_avgT = mean(avgT), sd_avgT = sd(avgT),
            mean_flow = mean(mFlow), sd_flow = sd(mFlow),
            mean_cob = mean(pctcbbl), sd_cob = sd(pctcbbl),
            mean_fines = mean(pctfines), sd_fines = sd(pctfines),
            mean_dep = mean(avdep), sd_dep = sd(avdep))



