#Chapter 2: BRT influence on SGCN

#Need dataset with occurrence and abundance data for BRT, BKT, Sculpins, LND, & SRD,
#and any relevant habitat covariates

library(tidyverse)
library(skimr)
library(coin)

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

###########################################################

# Mann Whitney U tests and permutation tests of differences
# in distributions of CPUE/100m for LND, SRD, and Cottus

###########################################################


#-----
#We first need to create the CPUE (count/100m) variable 
#-----

#bring in environmental data
hab <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header = T)
names(hab)
skim(hab)

#add in segment length (this is the total stream distance each site was sampled)
len <- hab %>%
  select(HUC8, Site, RchLength) %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = T) %>%
  mutate(SegLen = (3*RchLength)) %>%
  select(-RchLength)

fish <- mydat %>%
  unite(newID, c(HUC8, site), sep = "_", remove = T) %>%
  select(-uid)

fish[45,1] <- "UPI_201"
fish[46,1] <- "UPI_202"
fish[115,1] <- "YEL_97b"

newdf <- left_join(fish, len, by="newID")

skim(newdf)

#Now we have segment legnth as a variable, let's calculate CPUE(count/100m)
#the formula we will use is (ab/SegLen)*100
#Also, remove BKT from further calculations

new2 <- newdf %>%
  mutate(LND_CPUE = (LND_ab/SegLen)*100, SRD_CPUE = (SRD_ab/SegLen)*100, Cottus_CPUE = (Cottus_ab/SegLen)*100, BRT_CPUE = (BRT_ab/SegLen)*100)

#############################################################################

#----------------------------Boxplots of CPUE------------------------------#

#-----------------------Filter by presence of SGCN-------------------------#

#LND
ldace <- new2 %>%
  filter(LND == 1)
ggplot(data = ldace, aes(x=BRT,y=LND_CPUE)) +
  geom_boxplot(aes(fill=BRT))+
  labs(x="Brown Trout Presence", y="Longnose Dace CPUE (fish/100m)")+
  theme_minimal()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")

#SRD
sdace <- new2 %>%
  filter(SRD == 1)
ggplot(data = sdace, aes(x=BRT,y=SRD_CPUE)) +
  geom_boxplot(aes(fill=BRT))+
  labs(x="Brown Trout Presence", y="Southern Redbelly Dace CPUE (fish/100m)")+
  theme_minimal()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")

#Sculpins
cott <- new2 %>%
  filter(Cottus == 1)
ggplot(data = cott, aes(x=BRT,y=Cottus_CPUE)) +
  geom_boxplot(aes(fill=BRT))+
  labs(x="Brown Trout Presence", y="Sculpin CPUE (fish/100m)")+
  theme_minimal()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")


#############################################################################

# Mann Whitney U / Wilcox Sign Rank Test 

# using subsetted data -- only when SGCNs of interest are present

help("wilcox.test")

# Ho: Median CPUE of SGCN when BRT are present = CPUE when BRT are absent
# two-sided

#-----
#LND
#-----
class(ldace$BRT)
wilcox.test(ldace$LND_CPUE ~ ldace$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# no difference

#-----
#SRD
#-----
class(sdace$BRT)
wilcox.test(sdace$SRD_CPUE ~ sdace$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
#no difference
#-----
#Cottus
#-----
class(cott$BRT)
wilcox.test(cott$Cottus_CPUE ~ cott$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
#no difference

#############################################################################

# Using the "coin" package
library(coin)
# Exact Wilcoxon Mann Whitney Rank Sum Test
# where y is numeric and A is a binary factor

#LND
wilcox_test(LND_CPUE~BRT, data=ldace, distribution="exact") #p = 0.95
#SRD
wilcox_test(SRD_CPUE~BRT, data=sdace, distribution="exact") #p = 0.25
#Cottus
wilcox_test(Cottus_CPUE~BRT, data=cott, distribution="exact") #p = 1

# One-Way Permutation Test based on 9999 Monte-Carlo
# resamplings. y is numeric and A is a categorical factor

#LND
oneway_test(LND_CPUE~BRT, data=ldace,
            distribution=approximate(B=9999)) #p = 0.95
#SRD
oneway_test(SRD_CPUE~BRT, data=sdace,
            distribution=approximate(B=9999)) #p = 0.16
#Cottus
oneway_test(Cottus_CPUE~BRT, data=cott,
            distribution=approximate(B=9999)) #p = 0.91
#############################################################################

# Results:
  # Longnose Dace:
    # occurr = 33
    # sympatry = 19, allopatry = 14
    # no significant difference in CPUE when BRT present vs. absent
  # Southern Redbelly Dace:
    # occurr = 33
    # sympatry = 12, allopatry = 21
    # no significant difference in CPUE when BRT present vs. absent
  # Sculpins:
    # occurr = 20
    # sympatry = 18, allopatry = 2
    # no significant difference in CPUE when BRT present vs. absent

#############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Let's build some CPUE models for each non-game fish with a combination of 
# hypotheses: 1) only environment, 2) Brown Trout CPUE, 3) other "top carnivore" CPUE, 
# 4) BRT+environment, 5) OTC+environment, 6) null
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Longnose Dace
  # Intermediate tolerance, benthic, not TC, native eurythermal
  # Hypotheses:
    # Environment: MEANT(+), %riffle(+), %cobble(+), mflow(+), avwidth(+), HAiFLS_alt(-)
    # BRT influence: (~) Habitat overlap seemingly with BRT, benthic sp. 
    # maybe less susceptible to predation 

# Southern Redbelly Dace
  # Intermediate tolerance, column, not TC, native eurythermal
  # Hypotheses:
    # Environment: MEANT(+), %fines(+), %run(+), mdepth(m), HAiFLS_alt(-)
    # BRT influence: (-) column dwelling cyprinid - seems
    # susceptible to predation

#Sculpins
  # Intolerant, not TC, native coldwater, benthic
  # Hypotheses:
    # Environment: MEANT(+), %cobble(+), HAiFLS_for(-), pctBrBnk(-), pctShade(+), pctpool(+)
    # BRT influence: (+) habitat overlap, BRT invade -- abundance more
    # of a function of habitat since intolerant, and coldwater

#############################################################################

# Create dataframe with habitat variables

names(hab)
huc12 <- read.csv("Data/Thesis/Spatial/sites_with_HUC12.csv", header = T) #data with HUC10 and HUC12 info
summary(huc12)

habby <- hab %>%
  select(HUC_Site, HUC8, avwid, avdep, pctfines, pctcbbl, pctrock,
         mFlow, pctRiffle, pctrun, pctslow, bnkbare.,
         AvChnlShd., MEANT, HAiFLS_alt, HAiFLS_for, pctSlope, avgT, CatArea_km2) %>%
  rename(newID=HUC_Site, pctriffle = pctRiffle, pctpool=pctslow,
         BrBank = bnkbare., Canopy=AvChnlShd.) #trim the fat


sgcn <- left_join(new2,habby, by="newID") #join hab to fish data
skim(sgcn)

sgcn[97,30:31]
sgcn[97,30:31] <- c(89.50991,1.911236) #-- missing HAiFLS values for UPI_165

x <- mean(sgcn$MEANT, na.rm = T) #mean value of MEANT
x

y <- mean(sgcn$avgT, na.rm = T)
y

sgcn2 <- sgcn %>%
  mutate_at(vars(MEANT), ~replace(.,is.na(.), x)) #replace NAs with mean value

sgcn2 <- sgcn2 %>%
  mutate_at(vars(avgT), ~replace(.,is.na(.), y)) #replace NAs with mean value

basins <- huc12 %>%
  select(newID, HUC_10, HUC_12) #trim excess info
basins$newID <- as.character(basins$newID)
basins[47,1] <- "YEL_97b"

sgcn3 <- left_join(sgcn2, basins, by="newID") #join basin info to fish&hab data

names(sgcn3)

sgcn4 <- sgcn3 %>%
  select(newID, HUC8, HUC_10, HUC_12, everything()) #organize

sgcn4[97,3:4] <- sgcn4[88,3:4] ##missing values for UPI_165
sgcn4[88,3:4] ##UPI_165 is in the same HUC_12 and HUC_10 as UPI_23 & UPI_25
sgcn4[97,36] <- 28.827

z = mean(sgcn4$pctSlope, na.rm = T)
sgcn4[97,34] <- z

skim(sgcn4)



#Write tidy csv
write.csv(sgcn4, "Data/Thesis/Tidy/SGCN_AllCovariates.csv", row.names = F)
#############################################################################
library(tidyverse)
library(lme4)
library(MASS)
library(car)
library(ggResidpanel)
library(corrplot)
library(GLMMadaptive)

#load data
mydat <- read.csv("Data/Thesis/Tidy/SGCN_AllCovariates.csv", header=T)
skim(mydat)

#arrange watersheds as factors
mydat$HUC10 <- as.factor(sub('.*(?=.{3}$)', '', mydat$HUC_10, perl=T))
mydat$HUC12 <- as.factor(sub('.*(?=.{2}$)', '', mydat$HUC_12, perl=T))
mydat$watershed_med <- as.factor(paste(mydat$HUC8, mydat$HUC10, sep = "_"))
mydat$watershed_sm <- as.factor(paste(mydat$watershed_med, mydat$HUC12, sep = "_"))
mydat$HUC8 <- as.factor(mydat$HUC8)


#inspect response variable(s)

ggplot(mydat, aes(LND_CPUE))+
  geom_histogram(binwidth = 10) #Longnose Dace CPUE

ggplot(mydat, aes(SRD_CPUE))+
  geom_histogram(binwidth = 10) #Southern Redbelly Dace CPUE

ggplot(mydat, aes(Cottus_CPUE))+
  geom_histogram(binwidth = 10) #Sculpins CPUE


#inspect collinearity of predictors
predictors <- mydat %>%
  select_at(vars(20:33))

M <- cor(predictors)
corrplot(M, method = "number", type = "upper")

#correlated predictors to not include in same model (|r| > 0.6)

  #Local:
    #pctfines & pctcbbl
    #pctfines & pctrock
    #pctfines & pctriffle
    #pctcbbl & pctrock
    #pctcbbl & pctriffle
    #pctrock & pctriffle
    #mFlow & pctpool
  #Catchment:
    #HAiFLS_alt & HAiFLS_for

#Honorable mentions (|r| > 0.5):
    #avwid & avdep (0.5)
    #pctriffle & pctrun (-0.54)
    #pctrun & pctpool (-0.52)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Let's build some CPUE models for each non-game fish with a combination of 
# hypotheses: 1) only environment, 2) Brown Trout CPUE, 3) other "top carnivore" CPUE, 
# 4) BRT+environment, 5) OTC+environment, 6) null
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Longnose Dace
# Intermediate tolerance, benthic, not TC, native eurythermal
# Hypotheses:
# Environment: MEANT(+), %riffle(+), %cobble(+), mflow(+), avwidth(+), HAiFLS_alt(-)
# BRT influence: (~) Habitat overlap seemingly with BRT, benthic sp. 
# maybe less susceptible to predation 


lnd_GLMM <- mixed_model(fixed = LND_CPUE ~ MEANT+pctcbbl+mFlow+avwid+HAiFLS_alt+BRT_CPUE,
                        random = ~ 1 | watershed_sm, data = mydat,
                        family = zi.negative.binomial(), zi_fixed = ~ 1, iter_EM=0)


summary(lnd_GLMM)























