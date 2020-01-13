#------------------------------------------------#
# Undergrad Research: Coldwater Fish IBI Analysis#
#------------------------------------------------#

#working with tidyfish1.xlsx data
#goal for 10/17/19 is to quantify metrics of IBI
library(tidyverse)
library(ggplot2)
library(forcats)
library(readxl)
#load data
getwd()
#metrics <- read_excel("/Users/brettkelly/Documents/MyFiles/Iowa State/EEOB590B/BKelly_Fishes/Data/Thesis/Tidy/tidyfish1.xlsx") #laptop
metrics <- read_excel("Data/Thesis/Tidy/tidyfish1.xlsx") #work computer
names(metrics)

#remove MTS and SLS columns - use select function
metrics <- metrics %>%
  select(-"SLS", -"SLS_ab", -"MTS", -"MTS_ab")
names(metrics)

#filter data by sites where total_ab >= 25 individuals (the minimum # of fish needed to score a site)
metrics <- metrics %>%
  filter(total_ab >= 25)
dim(metrics)

#Metric 1: rename "richness" to "numspecies" to match the metrics names using rename function
#new column name on left, old on right
metrics <- metrics %>%
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
reach <- read_csv("/Users/brettkelly/Documents/MyFiles/Iowa State/EEOB590B/BKelly_Fishes/Data/Thesis/Raw/All Years/reachlengths.csv", col_names = T)
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



#creat new df without raw fish data to use for IBI calculation, then write this as a csv as an intermediate step
IBI <- metrics2 %>%
  select(uid, HUC8, site, numspecies, TopCarn_ab, total_ab, numCWspp, numTOLspp, numMINspp, numBENspp, CW_ab, 
         pctCWindv, Intol_ab, pctINTOLindv, trout_ab, pctBKTsalmon, pctWSUindv, pctTOPCARNindv, CWindv150, WW_ab, WWindv150)
head(IBI)

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

#new DF with M1 --> M12
names(IBI)
scores <- IBI %>%
  select(M1_spp, M2_CWspp, M3_MINspp, M4_BENspp, M5_TOLspp, M6_BKTsalmonid, M7_pctIntol, 
         M8_pctCW, M9_pctWSU, M10_pctTC, M11_CWindv150, M12_WWindv150)
head(scores)

IBI <- IBI %>%
  mutate(IBIScore = rowSums(scores, na.rm = T))

ggplot(IBI, aes(HUC8, IBIScore, color=HUC8))+
  geom_boxplot()

#Add column for Rating (Very Poor, Poor, Fair, Good, Excellent)
IBI <- IBI %>%
  mutate(Rating = ifelse(IBIScore>104, "Excellent", ifelse(
    IBIScore>69, "Good", ifelse(IBIScore>34, "Fair", ifelse(
      IBIScore>9, "Poor", ifelse(IBIScore<6, "Very Poor", "No Score")
    ))
  )))
class(IBI$Rating)

IBI$Rating <- as.factor(IBI$Rating)
class(IBI$Rating)
levels(IBI$Rating)

g <- ggplot(IBI, aes(Rating, color = HUC8)) +
  geom_bar()
g

max(IBI$IBIScore)
IBI$IBIScore[105]

#write tidy csv of IBI 
getwd()
write.csv(IBI, "/Users/brettkelly/Documents/MyFiles/Iowa State/EEOB590B/BKelly_Fishes/Data/Thesis/Tidy/tidy_IBI1.csv", row.names = F)
  