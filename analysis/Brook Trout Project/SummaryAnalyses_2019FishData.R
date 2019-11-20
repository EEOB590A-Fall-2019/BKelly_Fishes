##-----------------------------------##
##    Fish data Exploration 2019     ##
##___________________________________##
## we have fish data for all sites 
## sampled in 2019, in wadable 
## streams. We want to summarize the 
## data for use in reports and 
## manuscripts. 
##-----------------------------------##

#libraries
library(tidyverse)
library(skimr)
library(ggplot2)

#load data
getwd()
fish <- read_csv("Data/Thesis/Tidy/tidyfish1.csv", col_names = T)

#skim data
skim(fish) #only missing values are for the 88 sites where we did not account for SLS vs MTS. (2018 data regardless)

#trim to 2019 data
f2 <- fish %>%
  filter(uid > 88) #53 sites with at least 1 fish. UPI_58 and YEL_98 were fishless. so 55 sampled in report, only 53 to summarize

#Let's list species by number of occurrences, and total abundance 

##occurrences
presence <- f2 %>%
  select_at(vars(-contains("_ab")))

occ.sums <- colSums(presence[,4:39])  

presence[54,] <- c(142, "ALL", 9999, occ.sums)

occ.summed <- presence %>%
  filter(uid == 142)

occ.wide <- occ.summed %>%
  select(-c(uid,HUC8,site))%>%
  pivot_longer(cols = 1:35,names_to = "Species", values_to = "Occurrence")
occ.wide$Occurrence <- as.numeric(occ.wide$Occurrence)

occ.wide <- occ.wide[,c(2,3)]

##abundances
abundance <- f2%>%
  select_at(vars(c(1:3,contains("_ab"))))

class(abundance$BKT_ab)
str(abundance)
ab.sums <- colSums(abundance[,4:40])

abundance[54,] <- c(142, "ALL", 9999, ab.sums)

ab.wide <- abundance %>%
  filter(BKT_ab == 47)%>%
  pivot_longer(cols = 4:40,names_to = "Species",values_to = "Count")%>%
  separate(col = Species, into = c("Species", NA), sep = "_", remove = T)%>%
  select(-uid,-HUC8,-site)


##join together
summary.fish19 <- left_join(occ.wide, ab.wide, by = "Species")

##Remove species that were not encountered in 2019
summary_fish19 <- summary.fish19 %>%
  filter(Occurrence > 0)

##exploration of richness
rich <- presence %>%
  filter(HUC8 != "ALL")%>%
  select(HUC8, site, richness)
rich$richness <- as.numeric(rich$richness)
summary(rich)

##exploration of SGCN
#Species of Greatest Conservation Need: BKT, SRD, LND, Cottus, ABL
sgcn <- f2 %>%
  select(uid, HUC8, site, BKT, BKT_ab, SRD, SRD_ab, LND, LND_ab, Cottus, Cottus_ab, ABL, ABL_ab,SPS,SPS_ab)%>%
  mutate(SGCN_pres = ifelse(BKT+SRD+LND+Cottus+ABL+SPS>0,1,0), SGCN_rich = (BKT + SRD + LND + Cottus + ABL + SPS),
         SGCN_ab = (BKT_ab+SRD_ab+LND_ab+Cottus_ab+ABL_ab+SPS_ab))%>%
  select(HUC8, site, SGCN_pres, SGCN_rich, SGCN_ab)

sgcn_summary <- sgcn %>%
  group_by(HUC8)%>%
  summarise(SGCN_sites = sum(SGCN_pres), SGCN_totals = sum(SGCN_ab))

##trout summary
trout <- f2 %>%
  select(uid, HUC8, site, BKT, BKT_ab, BRT, BRT_ab, RBT, RBT_ab)%>%
  mutate(BKT_BRT = ifelse(BKT+BRT>1,1,0), BKT_RBT = ifelse(BKT+RBT>1,1,0), BRT_RBT = ifelse(BRT+RBT>1,1,0),
         BK_BR_RB = ifelse(BKT+BRT+RBT>2,1,0))

tws <- trout %>%
  group_by(HUC8) %>%
  summarise(BK_total = sum(BKT_ab), BR_total = sum(BRT_ab), RB_total = sum(RBT_ab), 
            BkBr_Coex = sum(BKT_BRT), BkRb_Coex = sum(BKT_RBT), BrRb_Coex = sum(BRT_RBT),
            All_coex = sum(BK_BR_RB), PCT_BKT = (sum(BKT_ab)/(sum(BRT_ab+RBT_ab))*100))
ts <- trout %>%
  summarise(BK_total = sum(BKT_ab), BR_total = sum(BRT_ab), RB_total = sum(RBT_ab), 
            BkBr_Coex = sum(BKT_BRT), BkRb_Coex = sum(BKT_RBT), BrRb_Coex = sum(BRT_RBT),
            All_coex = sum(BK_BR_RB), PCT_BKT = (sum(BKT_ab)/(sum(BRT_ab+RBT_ab))*100))
