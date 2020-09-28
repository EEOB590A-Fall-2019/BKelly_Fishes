#################################################
# We have data from 138 GRTS selected streams in
# NE Iowa from 3 HUC8 watersheds (Uppper Iowa, 
# Yellow, and Graint-Little Maquoketa). 

# We will use Mundahl's coldwater FIBI
# to score these streams and then regress
# them against environmental variables. 

# First we need to bring in all the data, 
# remove sites with <25 individuals and 
# that have >22 degrees C during summer

################################################

library(tidyverse)
library(skimr)
library(forcats)
library(readxl)

#-----
# load fish data
#-----
getwd()
#working with tidyfish1.xlsx data
metrics <- read_excel("Data/Thesis/Tidy/tidyfish1.xlsx") 
names(metrics)

#-----
# load temperature data
#-----
temps <- read_csv("Data/Thesis/Tidy/TmpVars_all.csv")


#remove MTS and SLS columns - use select function
metrics <- metrics %>%
  select(-"SLS", -"SLS_ab", -"MTS", -"MTS_ab")
names(metrics)

#filter data by sites where total_ab >= 25 individuals (the minimum # of fish needed to score a site)
metrics <- metrics %>%
  filter(total_ab >= 25)
dim(metrics)

#filter data by sites where maximum daily mean <22 degrees Celsius
names(metrics)
names(temps)

#make common column in order to join data 
metrics <- metrics %>%
  unite(newID, c(HUC8, site), sep = "_", remove = F)

temps <- temps %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = T)

k <- left_join(metrics, temps, by="newID")
hist(k$MEANT)

metrics2 <- k %>%
  select(-avgT,-sdT,-MAXT,-RNGT)%>%
  filter(MEANT<22)

##############################################################################################################
#Metric 1: rename "richness" to "numspecies" to match the metrics names using rename function
#new column name on left, old on right
metrics <- metrics2 %>%
  rename(numspecies = "richness")
summary(metrics$numspecies)

#Metric 2: create column with number of coldwater species
#CW species = trout + Cottus + BSB + ABL
cold <- metrics %>%
  select(BKT, BRT, RBT, TGT, Cottus, BSB, ABL)
#Add column with rowSums()
metrics <- metrics %>%
  mutate(numCWspp = rowSums(cold))
summary(metrics$numCWspp)

#Metric 3: number of tolerant species
#tolerant species: CMM, BNM, FHM, WBD, CRC, WSU, GSF
#in order of select: CMM, WSU, CRC, WBD, FHM, BNM, GSF
tolerant <- metrics %>%
  select(CMM, WSU, CRC, WBD, FHM, BNM, GSF)
summary(tolerant)
#Add column with rowSums()
metrics <- metrics %>%
  mutate(numTOLspp = rowSums(tolerant))
summary(metrics$numTOLspp)

#Metric 4: number of minnow species
#Minnows: LND, SRD, SMM, MSM, CRC, WBD, CSR, FHM, BNM, CSH, SPS, HHC, SSH, BMS - RERUN with BMS
minnow <- metrics %>%
  select(LND, SRD, SMM, MSM, CRC, WBD, CSR, FHM, BNM, CSH, SPS, HHC, SSH, BMS)
summary(minnow)
#Add column with rowSums()
metrics <- metrics %>%
  mutate(numMINspp = rowSums(minnow))
summary(metrics$numMINspp)

#Metric 5: number of benthic species
#benthic: LND, Cottus, WSU, JOD, FTD, GRH, SHR, BLB, STC  
benthic <- metrics %>%
  select(LND, Cottus, WSU, JOD, FTD, GRH, SHR, BLB, STC)
summary(benthic)
#Add column with rowSums()
metrics <- metrics %>%
  mutate(numBENspp = rowSums(benthic))
summary(metrics$numBENspp)

#Metric 6: Percent coldwater individuals
#CW species = trout + Cottus + ABL +BSB
cold_abundance <- metrics %>%
  select(BKT_ab, BRT_ab, RBT_ab, TGT_ab, Cottus_ab, BSB_ab, ABL_ab)
summary(cold_abundance)
#Add column with rowSums()
metrics <- metrics %>%
  mutate(CW_ab = rowSums(cold_abundance))
summary(metrics$CW_ab)
#add column for %CW individuals
metrics <- metrics %>%
  mutate(pctCWindv = (CW_ab)/(total_ab)*100)
summary(metrics$pctCWindv)

ggplot(metrics, aes(HUC8, pctCWindv))+
  geom_boxplot()

#Metric 7: Percent Intolerant Indv
#Intolerant species numbers needed: ABL, BKT, SPS, Cottus

Intol <- metrics %>%
  select(BKT_ab, Cottus_ab, ABL_ab, SPS_ab)
head(Intol)
#Add column with rowSums()
metrics <- metrics %>%
  mutate(Intol_ab = rowSums(Intol))
summary(metrics$Intol_ab)
#add column for %Intolerant individuals
metrics <- metrics %>%
  mutate(pctINTOLindv = (Intol_ab)/(total_ab)*100)
summary(metrics$pctINTOLindv)

ggplot(metrics, aes(HUC8, pctINTOLindv))+
  geom_boxplot()

#Metric 8: Percent Salmondis as BKT

#add trout_ab column
salmonids <- metrics %>%
  select(BKT_ab, BRT_ab, RBT_ab, TGT_ab)

metrics <- metrics %>%
  mutate(trout_ab = rowSums(salmonids))
summary(metrics$trout_ab)

#pctBKT column
metrics <- metrics %>%
  mutate(pctBKTsalmon = (BKT_ab)/(trout_ab)*100)
summary(metrics$pctBKTsalmon)
#produces NA's for columns without trout, this should be okay? If problems arise remember this. 

#Metric 9 - Percent White Sucker
metrics <- metrics %>%
  mutate(pctWSUindv = (WSU_ab)/(total_ab)*100)
summary(metrics$pctWSUindv)

#Metric 10 - Percent Top Carn
names(metrics)
metrics <- metrics %>%
  mutate(pctTOPCARNindv = (TopCarn_ab)/(total_ab)*100)
summary(metrics$pctTOPCARNindv)

#Metric 11 - Number CW individuals per 150m
#need reach length for each site
#add column with total shocked length
#going to need to need to divide (cw_ab)/(length of effort)*(150)

#join df "reach" to "metrics" 
getwd()
reach <- read_csv("Data/Thesis/Raw/All Years/reachlengths.csv", col_names = T)
reach
metrics2 <- metrics %>%
  left_join(reach, by = "uid")
summary(metrics2)

metrics2 <- metrics2 %>%
  mutate(SegLength = (RchLength)*3)
summary(metrics2$SegLength)

metrics2 <- metrics2 %>%
  mutate(CWindv150 = (CW_ab)/(SegLength)*150)
summary(metrics2$CWindv150)

#Metric 12 - Number WW individuals per 150m
#create WW_ab column
#divide (Ww_ab)/(length of effort)*(150)

#WW species: everything except trout, sculpins, BSB and ABL
metrics2 <- metrics2 %>%
  mutate(WW_ab = (total_ab)-(CW_ab))
summary(metrics2$WW_ab)

metrics2 <- metrics2 %>%
  mutate(WWindv150 = (WW_ab)/(SegLength)*150)
summary(metrics2$WWindv150)

names(metrics2)

#----------------------------------------------------------------
#NEW - Lyons metrics to add to the Mundahl metrics
#________________________________________________________________
#Metric 1: number of intolerant species: ABL, BKT, SPS, Cottus
IntolSP <- metrics2 %>%
  select(Cottus, SPS, BKT, ABL)
summary(IntolSP)
#Add column with rowSums()
metrics2 <- metrics2 %>%
  mutate(numIntolSP = rowSums(IntolSP))
summary(metrics2$numIntolSP)

#Metric 2: Percent Tolerant Indv
#Tolerant species numbers needed: CMM, BNM, FHM, WBD, CRC, WSU, GSF
#in order of select: CMM, WSU, CRC, WBD, FHM, BNM, GSF
tol_ab <- metrics2 %>%
  select(CMM_ab, WSU_ab, CRC_ab, WBD_ab, FHM_ab, BNM_ab, GSF_ab)
summary(tol_ab)
#Add column with rowSums()
metrics2 <- metrics2 %>%
  mutate(Tolerant_ab = rowSums(tol_ab))
summary(metrics2$Tolerant_ab)

#add column for %Tolerant individuals
metrics2 <- metrics2 %>%
  mutate(pctTOLindv = (Tolerant_ab)/(total_ab)*100)
summary(metrics2$pctTOLindv)

ggplot(metrics2, aes(HUC8, pctTOLindv))+
  geom_boxplot()

#Metric 3: % Top Carnivore
metrics2 <- metrics2 %>%
  mutate(TC_noRBT = (TopCarn_ab-RBT_ab)) %>%
  mutate(pctTC_noRBT = (TC_noRBT/total_ab)*100)
summary(metrics2$pctTOPCARNindv)
summary(metrics2$pctTC_noRBT)

#Metric 4: % stenothermal coolwater and coldwater
#stenothermal: BKT, BRT, TGT, Cottus, ABL
steno_ab <- metrics2 %>%
  select(BKT_ab, BRT_ab, TGT_ab, Cottus_ab, BSB_ab, ABL_ab)
summary(steno_ab)
#Add column with rowSums()
metrics2 <- metrics2 %>%
  mutate(CWnoRBT_ab = rowSums(steno_ab))
summary(metrics2$CWnoRBT_ab)

#add column for %CW individuals
metrics2 <- metrics2 %>%
  mutate(pctCW_noRBT = (CWnoRBT_ab)/(total_ab)*100)
summary(metrics2$pctCWindv)
summary(metrics2$pctCW_noRBT)

#Metric 5: % salmonids as BKT (no RBT in salmonids total!)
#add trout_ab column
salmos <- metrics %>%
  select(BKT_ab, BRT_ab, TGT_ab)

metrics2 <- metrics2 %>%
  mutate(troutnoRBT_ab = rowSums(salmos))
summary(metrics2$troutnoRBT_ab)

#pctBKT column
metrics2 <- metrics2 %>%
  mutate(pctBKT_noRBT = (BKT_ab)/(troutnoRBT_ab)*100)
summary(metrics$pctBKT_noRBT)

#replace NAs in pctBKT_noRBT column with zero
#metrics2$pctBKT_noRBT[is.na(metrics2$pctBKT_noRBT)]=0
#skim(metrics2)
##############################################################################################################################



#create new df without raw fish data to use for IBI calculation, then write this as a csv as an intermediate step
IBI <- metrics2 %>%
  select(uid, HUC8, site, numspecies, TopCarn_ab, total_ab, numCWspp, numTOLspp, numMINspp, numBENspp, CW_ab, 
         pctCWindv, Intol_ab, pctINTOLindv, trout_ab, pctBKTsalmon, pctWSUindv, pctTOPCARNindv, CWindv150, WW_ab, WWindv150,
         numIntolSP, pctTOLindv, pctTC_noRBT, pctCW_noRBT, pctBKT_noRBT)
head(IBI)
skim(IBI)
IBI[72,3] <- "97b"
#create new df or columns with the actual scores of each metric using ifelse statements

#M1
IBI <- IBI %>%
  mutate(M1_spp = ifelse(IBI$numspecies<5, 10, ifelse(IBI$numspecies<10, 5, 0)))
summary(IBI$M1_spp)
check_M1 <- IBI %>%
  select(numspecies, M1_spp)
check_M1 #worked

#M2
IBI <- IBI %>%
  mutate(M2_CWspp = ifelse(IBI$numCWspp>3, 10, ifelse(IBI$numCWspp>1, 5, 0)))
summary(IBI$M2_CWspp)
check_M2 <- IBI %>%
  select(numCWspp, M2_CWspp)
check_M2 #worked

#M3
IBI <- IBI %>%
  mutate(M3_MINspp = ifelse(IBI$numMINspp>3, 0, ifelse(IBI$numMINspp>1, 5, 10)))
summary(IBI$M3_MINspp)
check_M3 <- IBI %>%
  select(numMINspp, M3_MINspp)
check_M3 #worked

#M4 
IBI <- IBI %>%
  mutate(M4_BENspp = ifelse(IBI$numBENspp>2, 0, ifelse(IBI$numBENspp<2, 10, 5)))
summary(IBI$M4_BENspp)
check_M4 <- IBI %>%
  select(numBENspp, M4_BENspp)
check_M4 #worked

#M5 
IBI <- IBI %>%
  mutate(M5_TOLspp = ifelse(IBI$numTOLspp>3, 0, ifelse(IBI$numTOLspp<2, 10, 5)))
summary(IBI$M5_TOLspp)
check_M5 <- IBI %>%
  select(numTOLspp, M5_TOLspp)
check_M5 #worked

#M6 
IBI <- IBI %>%
  mutate(M6_BKTsalmonid = ifelse(IBI$pctBKTsalmon<12, 0, ifelse(IBI$pctBKTsalmon>92, 10, 5)))
summary(IBI$M6_BKTsalmonid)
check_M6 <- IBI %>%
  select(pctBKTsalmon, M6_BKTsalmonid)
check_M6 #worked, produced NA's in sites without trout at all

#M7 
IBI <- IBI %>%
  mutate(M7_pctIntol = ifelse(IBI$pctINTOLindv<10, 0, ifelse(IBI$pctINTOLindv>43, 10, 5)))
summary(IBI$M7_pctIntol)
check_M7 <- IBI %>%
  select(pctINTOLindv, M7_pctIntol)
check_M7 #worked

#M8
IBI <- IBI %>%
  mutate(M8_pctCW = ifelse(IBI$pctCWindv<42, 0, ifelse(IBI$pctCWindv>88, 10, 5)))
summary(IBI$M8_pctCW)
check_M8 <- IBI %>%
  select(pctCWindv, M8_pctCW)
check_M8 #worked

#M9
IBI <- IBI %>%
  mutate(M9_pctWSU = ifelse(IBI$pctWSUindv>1.5, 0, ifelse(IBI$pctWSUindv>0, 5, 10)))
summary(IBI$M9_pctWSU)
check_M9 <- IBI %>%
  select(pctWSUindv, M9_pctWSU)
check_M9 #worked

#M10
IBI <- IBI %>%
  mutate(M10_pctTC = ifelse(IBI$pctTOPCARNindv<30, 0, ifelse(IBI$pctTOPCARNindv>72, 10, 5)))
summary(IBI$M10_pctTC)
check_M10 <- IBI %>%
  select(pctTOPCARNindv, M10_pctTC)
check_M10 #worked

#M11
IBI <- IBI %>%
  mutate(M11_CWindv150 = ifelse(IBI$CWindv150<32, 0, ifelse(IBI$CWindv150>75, 10, 5)))
summary(IBI$M11_CWindv150)
check_M11 <- IBI %>%
  select(CWindv150, M11_CWindv150)
check_M11 #worked

#M12
IBI <- IBI %>%
  mutate(M12_WWindv150 = ifelse(IBI$WWindv150>60, 0, ifelse(IBI$WWindv150<16, 10, 5)))
summary(IBI$M12_WWindv150)
check_M12 <- IBI %>%
  select(WWindv150, M12_WWindv150)
check_M12 #worked

#ML1
IBI <- IBI %>%
  mutate(ML1_numIntolSp = ifelse(IBI$numIntolSP>1, 20, ifelse(IBI$numIntolSP==1, 10, 0)))
summary(IBI$ML1_numIntolSp)
check_ML1 <- IBI %>%
  select(numIntolSP, ML1_numIntolSp)
check_ML1 #worked

#ML2
IBI <- IBI %>%
  mutate(ML2_pctTolindv = ifelse(IBI$pctTOLindv<6, 20, ifelse(IBI$pctTOLindv<23, 10, 0)))
summary(IBI$ML2_pctTolindv)
check_ML2 <- IBI %>%
  select(pctTOLindv, ML2_pctTolindv)
check_ML2 #worked

#ML3
IBI <- IBI %>%
  mutate(ML3_pctTCnoRBT = ifelse(IBI$pctTC_noRBT>45, 20, ifelse(IBI$pctTC_noRBT>14, 10, 0)))
summary(IBI$ML3_pctTCnoRBT)
check_ML3 <- IBI %>%
  select(pctTC_noRBT, ML3_pctTCnoRBT)
check_ML3 #worked

#ML4
IBI <- IBI %>%
  mutate(ML4_pctCWnoRBT = ifelse(IBI$pctCW_noRBT>85, 20, ifelse(IBI$pctCW_noRBT>42.0, 10, 0)))
summary(IBI$ML4_pctCWnoRBT)
check_ML4 <- IBI %>%
  select(pctCW_noRBT, ML4_pctCWnoRBT)
check_ML4 #worked

#ML5
IBI <- IBI %>%
  mutate(ML5_pctBKTnoRBT = ifelse(IBI$pctBKT_noRBT>95, 20, ifelse(IBI$pctBKT_noRBT>4, 10, 0)))
summary(IBI$ML5_pctBKTnoRBT)
check_ML5 <- IBI %>%
  select(pctBKT_noRBT, ML5_pctBKTnoRBT)
check_ML5 #worked



#############################################
#new DF with M1 --> M12 
names(IBI)
scores_Mundahl <- IBI %>%
  select(M1_spp, M2_CWspp, M3_MINspp, M4_BENspp, M5_TOLspp, M6_BKTsalmonid, M7_pctIntol, 
         M8_pctCW, M9_pctWSU, M10_pctTC, M11_CWindv150, M12_WWindv150)
head(scores_Mundahl)

IBI_Mun <- IBI %>%
  mutate(IBIScore_M = rowSums(scores_Mundahl, na.rm = T))

ggplot(IBI_Mun, aes(HUC8, IBIScore_M, color=HUC8))+
  geom_boxplot()

#Add column for Rating (Very Poor, Poor, Fair, Good, Excellent)
IBI_Mun <- IBI_Mun %>%
  mutate(Rating_M = ifelse(IBIScore_M>104, "Excellent", ifelse(
    IBIScore_M>69, "Good", ifelse(IBIScore_M>34, "Fair", ifelse(
      IBIScore_M>9, "Poor", ifelse(IBIScore_M<6, "Very Poor", "No Score")
    ))
  )))
class(IBI_Mun$Rating_M)

IBI_Mun$Rating_M <- as.factor(IBI_Mun$Rating_M)
class(IBI_Mun$Rating_M)
levels(IBI_Mun$Rating_M)

g <- ggplot(IBI_Mun, aes(Rating_M, color = HUC8)) +
  geom_bar()
g

max(IBI_Mun$IBIScore_M)

#######################################################################################

scores_Lyons <- IBI %>%
  select(ML1_numIntolSp, ML2_pctTolindv, ML3_pctTCnoRBT, ML4_pctCWnoRBT, ML5_pctBKTnoRBT)
head(scores_Lyons)

IBI_Lyons <- IBI %>%
  mutate(IBIScore_L = rowSums(scores_Lyons, na.rm = T))

ggplot(IBI_Lyons, aes(HUC8, IBIScore_L, color=HUC8))+
  geom_boxplot()

#Add column for Rating (Very Poor, Poor, Fair, Good, Excellent)
IBI_Lyons <- IBI_Lyons %>%
  mutate(Rating_L = ifelse(IBIScore_L>80, "Excellent", ifelse(
    IBIScore_L>50, "Good", ifelse(IBIScore_L>20, "Fair", ifelse(
      IBIScore_L>0, "Poor", ifelse(IBIScore_L<1, "Very Poor", "No Score")
    ))
  )))
class(IBI_Lyons$Rating_L)

IBI_Lyons$Rating_L <- as.factor(IBI_Lyons$Rating_L)
class(IBI_Lyons$Rating_L)
levels(IBI_Lyons$Rating_L)

gg <- ggplot(IBI_Lyons, aes(Rating_L, color = HUC8)) +
  geom_bar()
gg

###################################
### REDUCE AND COMBINE DATASETS ###
###################################
names(IBI_Lyons)

ibi_lyons_short <- IBI_Lyons %>%
  select(uid, IBIScore_L, Rating_L)

full.ibi.df <- left_join(IBI_Mun, ibi_lyons_short, by="uid")


#write tidy csv of IBI 
getwd()
write.csv(full.ibi.df, "Data/Thesis/Tidy/tidy_FIBI_FULL.csv", row.names = F)

###################################################
###################################################

# CW FIBI Dataset Wrangling and Exploration

##################################################
##################################################

#load packages
library(tidyverse)
library(skimr)
library(forcats)
library(readxl)
library(corrplot)
library(ggResidpanel)

#read in data
IBIf <- read.csv("Data/Thesis/Tidy/tidy_FIBI_FULL.csv", header = T)

names(IBIf)

#subset data
IBI2 <- IBIf %>%
  select(uid,HUC8,site,M1_spp,M2_CWspp,M3_MINspp,M4_BENspp,M5_TOLspp,M6_BKTsalmonid,
         M7_pctIntol,M8_pctCW,M9_pctWSU,M10_pctTC,M11_CWindv150,
         M12_WWindv150,ML1_numIntolSp, ML2_pctTolindv, ML3_pctTCnoRBT, ML4_pctCWnoRBT,
         ML5_pctBKTnoRBT, IBIScore_M, Rating_M, IBIScore_L, Rating_L)

#explore data
skim(IBI2)

#visualize data


#summarize data
IBI_table <- IBI2 %>%
  group_by(HUC8) %>%
  summarize(mean(IBIScore))

#-----------------------------------------------

#             load environmental data

#-----------------------------------------------

#load data
hab <- read.csv("Data/Thesis/Tidy/enviro_tidy.csv", header=T)

#inspect data
names(hab)

## Hypotheses

## Preliminary Analysis: boulder hab (+), stream temp (-), bare bank (-), % run macrohabitat (-) 

# 1) Catchment Scale
# > HAiFLS_ag (row crops) #heavily cited (-)
# > HAiFLS_dev (developed) (-)
# > HAiFLS_alt (ag + pasture + developed) (-)   ##only retained land cover metric post correlation test
# > HAiFLS_for (forest - all types) (+)
# > HAiFLS_nat (forest + grassland, etc.) (+)

# 2) Local Scale
# > MEANT (-)
# > pctrun (-)
# > pctrock (+)
# > pctShade (+)
# > boulder (+) ## remove post correlation test
# > pctBrBnk (-)

## subset habitat dataset

hab2 <- hab %>%
  select(HUC8, Site, MEANT, pctrun, pctrock, pctShade, boulder, pctBrBnk, HAiFLS_ag, HAiFLS_dev, HAiFLS_alt, HAiFLS_for, HAiFLS_nat) %>%
  rename(site = Site) %>%
  unite(newID, c(HUC8, site), sep = "_", remove = F)

summary(hab2$HAiFLS_dev)
hist(hab2$HAiFLS_dev)
#----------------#
#correlation test
#----------------#
c <- cor(hab2[,4:14])
head(round(c,2)) 

#round down
cround <- round(c,3)

#visualize these correlations
corrplot(c, method = "number")

#-------------------------#
#Final covariate selection
#-------------------------#

hab3 <- hab2 %>%
  select(-HAiFLS_ag, -HAiFLS_alt, -HAiFLS_nat)


#---------------
#join datasets together to have one common DF moving forward
#---------------

IBI2$site <- as.character(IBI2$site)


#create newID column for IBI df 
IBI3 <- IBI2 %>%
  unite(newID, c(HUC8, site), sep = "_", remove = F)

data <- left_join(IBI3, hab3, by='newID') %>%
  select(-HUC8.y, -site.x, -site.y, -uid)

#inspect final df
skim(data)
names(data)

#graph response variable
hist(data$IBIScore)

#plot all remaining variables
pairs(data[,2:10])

#----------
#export data
write.csv(data, "Data/Thesis/Tidy/FIBI_and_Hab.csv", row.names = F)
#----------

###################################################
###################################################

#             CW FIBI Dataset Analysis

##################################################
##################################################


#consider log transformation of response?
data2 <- data %>%
  mutate(log_IBI = log(1+IBIScore))
hist(data2$log_IBI)

#data$IBIScore_log <- log(1+data$IBIScore)
#hist(data$IBIScore_log)

#########################################################
#########################################################

###         Regression Analysis Using GLMs 

#########################################################
#########################################################

#response: log IBI Score

#Predictors: 
# 1) Catchment Scale
# > HAiFLS_alt (ag + pasture + developed) (-)   

# 2) Local Scale
# > MEANT (-)
# > pctrun (-)
# > pctrock (+)
# > pctShade (+)
# > pctBrBnk (-)

#########################################################
require(MuMIn)
options(na.action = "na.omit") # change the default "na.omit" to prevent models
# from being fitted to different datasets in
# case of missing values.
#lm(thickness ~ island*species, data = traits)
global <- glm(IBIScore_log ~ HAiFLS_alt+MEANT+pctrun+pctrock+pctShade+pctBrBnk, data = data, na.action = "na.fail")

combinations <- dredge(global, beta = "none", rank = "AICc", evaluate = T)

print(combinations)

combinations_sd <- dredge(global, beta = "sd", rank = "AICc", evaluate = F)


print(combinations_sd)


top <- glm(IBIScore_log ~ MEANT+pctrock+pctShade+pctBrBnk, data = data)
summary(top)

resid_panel(top)


plot(data$IBIScore, data$pctrock)
plot(data$IBIScore_log, data$pctrock)

plot(data$pctrock, data$IBIScore)
plot(data$pctrock, data$IBIScore_log)

plot(data$MEANT, data$IBIScore_log)


1 - (85.452/153.190) #R^2 value = 0.442


#########################################
# Random effects of site/HUC10 or HUC12?
# Incorporate more land cover variables?
# Data transformation? If so, which one?
# Error distribution? Which is best?
# GOF test for glm?
########################################











