##-----------------------------------##
##    Fish data Exploration Final    ##
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

#load data
#getwd()
fish <- read_csv("Data/Thesis/Tidy/tidyfish1.csv", col_names = T)

#skim data
skim(fish) #only missing values are for the 88 sites where we did not account for SLS vs MTS. (2018 data regardless)

#remove fishless sites
fish[13,] #UPI_29
fish[54,] #YEL_33

fish2 <- fish %>%
  filter(uid != 13 & uid != 54)
#Let's list species by number of occurrences, and total abundance 

##occurrences
presence <- fish2 %>%
  select_at(vars(-contains("_ab")))

occ.sums <- colSums(presence[,4:39])  

presence[140,] <- c(142, "ALL", 9999, occ.sums)

occ.summed <- presence %>%
  filter(uid == 142)

occ.wide <- occ.summed %>%
  select(-c(uid,HUC8,site))%>%
  pivot_longer(cols = 1:35,names_to = "Species", values_to = "Occurrence")
occ.wide$Occurrence <- as.numeric(occ.wide$Occurrence)

occ.wide <- occ.wide[,c(2,3)]

##abundances
abundance <- fish2 %>%
  select_at(vars(c(1:3,contains("_ab"))))

class(abundance$BKT_ab)
str(abundance)
ab.sums <- colSums(abundance[,4:40])

abundance[140,] <- c(142, "ALL", 9999, ab.sums)

ab.wide <- abundance %>%
  filter(BKT_ab == 450)%>%
  pivot_longer(cols = 4:40,names_to = "Species",values_to = "Count")%>%
  separate(col = Species, into = c("Species", NA), sep = "_", remove = T)%>%
  select(-uid,-HUC8,-site)


##join together
summary.fish <- left_join(occ.wide, ab.wide, by = "Species")
  #select(-richness)

summary.fish <- summary.fish %>%
  filter(Species != "MTS" & Species != "SLS")

write.csv(summary.fish, "Data/Thesis/Tidy/AllFish_SummaryTable_2020.csv", row.names = F)

##exploration of richness
rich <- presence %>%
  filter(HUC8 != "ALL")%>%
  select(HUC8, site, richness)
rich$richness <- as.numeric(rich$richness)
summary(rich)

##exploration of SGCN
#Species of Greatest Conservation Need: BKT, SRD, LND, Cottus, ABL
sgcn <- fish2 %>%
  select(uid, HUC8, site, BKT, BKT_ab, SRD, SRD_ab, LND, LND_ab, Cottus, Cottus_ab, ABL, ABL_ab)%>%
  mutate(SGCN_pres = ifelse(BKT+SRD+LND+Cottus+ABL>0,1,0), SGCN_rich = (BKT + SRD + LND + Cottus + ABL),
         SGCN_ab = (BKT_ab+SRD_ab+LND_ab+Cottus_ab+ABL_ab))%>%
  select(HUC8, site, SGCN_pres, SGCN_rich, SGCN_ab)

sgcn_summary <- sgcn %>%
  group_by(HUC8)%>%
  summarise(SGCN_sites = sum(SGCN_pres), SGCN_totals = sum(SGCN_ab))

##trout summary
trout <- fish2 %>%
  select(uid, HUC8, site, BKT, BKT_ab, BRT, BRT_ab, RBT, RBT_ab, TGT, TGT_ab)%>%
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



####################################################################

## Make csv for mapping spatial distribution of Trout (BKT and BRT)

###################################################################

#make a "Trout Status" variable for the fill argument
trout2 <- trout %>%
  mutate(Trout_Status = ifelse(BKT+BRT>1,3,ifelse(BKT-BRT>0,2,ifelse(BKT-BRT<0,1,0))))

trout2$Trout_Status <- as.factor(trout2$Trout_Status)

skim(trout2)

#Load csv with sampled site list and UTM coords for each
sites <- read_csv("Data/Thesis/Tidy/Sampled_LocsUTM_tidy.csv")
skim(sites)

sites[48,2] <- '14b'
sites[70,2] <- '32b'
sites[71,2] <- '57b'
sites[87,2] <- '28b'
sites[105,2] <- '78b'
sites[109,2] <- '118b'
sites[85,2] <- '48'


#create new column with newID
sites2 <- sites %>%
  unite(newID, c(HUC8,Site), sep = "_", remove = F) %>%
  filter(newID != "UPI_29" & newID != "YEL_33")

sites2$newID <- as.factor(sites2$newID)

trout2[45,3] <- 201
trout2[46,3] <- 202

trout3 <- trout2 %>%
  unite(newID, c(HUC8, site), sep = "_", remove = F) %>%
  filter(newID != "UPI_165")

#match up the levels to make sure the lists of sites are the same 
trout3$newID <- as.factor(trout3$newID)
levels(sites2$newID)
levels(trout3$newID)

#reduce down vars on sites DF
sites3 <- sites2 %>%
  select(newID, Xcoord, Ycoord)

#reduce down vars on trout DF
trout4 <- trout3 %>%
  select(newID, HUC8, site, BKT, BRT, Trout_Status)


#combine into new DF
combo <- full_join(sites3, trout4, by="newID")
combo$HUC8 <- as.factor(combo$HUC8)
class(combo$BRT)
combo$BRT <- as.factor(combo$BRT)

combo2 <- combo %>%
  group_by(HUC8) %>%
  count(BRT)
  

#Write csv's
write.csv(combo, "Data/Thesis/Tidy/TroutStatus_sites.csv", row.names = F)
