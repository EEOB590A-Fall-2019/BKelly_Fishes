#Chapter 2: BRT influence on SGCN

#Need dataset with occurrence and abundance data for BRT, BKT, Sculpins, LND, & SRD,
#and any relevant habitat covariates

library(tidyverse)
library(skimr)

#load data
fish <- read_csv("Data/Thesis/Tidy/tidyfish1.csv", col_names = T)

#skim data
skim(fish) #only missing values are for the 88 sites where we did not account for SLS vs MTS. (2018 data regardless)

#remove fishless sites and non-project sites
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
summary.fish <- left_join(occ.wide, ab.wide, by = "Species") #%>%
  #select(-richness)


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
#####################################################################################################################
#####################################################################################################################

#More summary stats involing naive cooccurrence
#-----------------------------------------------


#subset data to BRT and SGCNs
names(fish)
mydat <- fish2 %>%
  select(uid, HUC8, site, BRT, BRT_ab, BKT, BKT_ab, LND, LND_ab, SRD, SRD_ab, Cottus, Cottus_ab)

#change presence variables and watersheds to factors
class(mydat$BRT)

mydat$HUC8 <- as.factor(mydat$HUC8) #watersheds
mydat$BRT <- as.factor(mydat$BRT) #BRT
mydat$BKT <- as.factor(mydat$BKT) #BKT
mydat$LND <- as.factor(mydat$LND) #LND
mydat$SRD <- as.factor(mydat$SRD) #SRD
mydat$Cottus <- as.factor(mydat$Cottus) #Cottus

#--------------------------------------------------
# display count data in sites with and without BRT
#--------------------------------------------------

#Brook Trout
ggplot(data = mydat, aes(x=BRT,y=BKT_ab)) +
  geom_violin() +
  geom_jitter() #remove zeros?

#Longnose Dace
ggplot(data = mydat, aes(x=BRT,y=LND_ab)) +
  geom_violin() +
  geom_jitter() #remove zeros?

#Southern Redbelly Dace
ggplot(data = mydat, aes(x=BRT,y=SRD_ab)) +
  geom_violin() +
  geom_jitter() #remove zeros?

#Cottus genus
ggplot(data = mydat, aes(x=BRT,y=Cottus_ab)) +
  geom_violin() +
  geom_jitter() #remove zeros?


#---------------------------------------------------------------
# display count data in sites with and without BRT
# for the sake of display, sites where the SGCN does NOT occur
#---------------------------------------------------------------

#------------
#Brook Trout
#------------

#filter
brk <- mydat %>%
  filter(BKT==1)

#graph
ggplot(data = brk, aes(x=BRT,y=BKT_ab)) +
  geom_boxplot()

ggplot(data = brk, aes(x=BRT,y=BKT_ab)) +
  geom_boxplot(aes(fill = HUC8))

#summary table(s)
brk2 <- brk %>%
  group_by(BRT) %>%
  summarise(total_brook = sum(BKT_ab), total_brown = sum(BRT_ab),
            avg_bkt = mean(BKT_ab), min_bkt = min(BKT_ab), max_bkt = max(BKT_ab))

#-------------
#Longnose Dace
#-------------

longn <- mydat %>%
  filter(LND==1)

ggplot(data = longn, aes(x=BRT,y=LND_ab)) +
  geom_boxplot()

ggplot(data = longn, aes(x=BRT,y=LND_ab)) +
  geom_boxplot(aes(fill = HUC8))

#summary table(s)
longn2 <-longn %>%
  group_by(BRT) %>%
  summarise(total_LND = sum(LND_ab), total_brown = sum(BRT_ab),
            avg_lnd = mean(LND_ab), min_lnd = min(LND_ab), max_lnd = max(LND_ab))

longn3 <- longn %>%
  modify_at(c(4,8), as.character) %>%
  modify_at(c(4,8), as.numeric) %>%
  mutate(status = if_else(BRT+LND > 1, 3, if_else(LND-BRT > 0, 2, ifelse(BRT-LND == 1, 1, 0)))) %>%
  group_by(status) %>%
  count()

#-----------------------
#Southern Redbelly Dace
#-----------------------

redbelly <- mydat %>%
  filter(SRD==1)

ggplot(data = redbelly, aes(x=BRT,y=SRD_ab)) +
  geom_boxplot()

ggplot(data = redbelly, aes(x=BRT,y=SRD_ab)) +
  geom_boxplot(aes(fill = HUC8))

#summary table(s)
red <- redbelly %>%
  group_by(BRT) %>%
  summarise(total_SRD = sum(SRD_ab), total_brown = sum(BRT_ab),
            avg_srd = mean(SRD_ab), min_srd = min(SRD_ab), max_srd = max(SRD_ab))

red2 <- redbelly %>%
  modify_at(c(4,10), as.character) %>%
  modify_at(c(4,10), as.numeric) %>%
  mutate(status = if_else(BRT+SRD > 1, 3, 2)) %>%
  group_by(status) %>%
  count()

#------------
#Cottus genus
#------------

sculp <- mydat %>%
  filter(Cottus==1)

ggplot(data = sculp, aes(x=BRT,y=Cottus_ab)) +
  geom_boxplot()

ggplot(data = sculp, aes(x=BRT,y=Cottus_ab)) +
  geom_boxplot(aes(fill = HUC8))

#summary table(s)
cott1 <- sculp %>%
  group_by(BRT) %>%
  summarise(total_cott = sum(Cottus_ab), total_brown = sum(BRT_ab),
            avg_cott = mean(Cottus_ab), min_cott = min(Cottus_ab), max_cott = max(Cottus_ab))

cott2 <- sculp %>%
  modify_at(c(4,12), as.character) %>%
  modify_at(c(4,12), as.numeric) %>%
  mutate(status = if_else(BRT+Cottus > 1, 3, 2)) %>%
  group_by(status) %>%
  count()












