################################################
# Fish and Habitat Data Wrangling (2018 & 2019)#
################################################

#packages, will need tidyverse, etc
install.packages('tidyverse')
library(tidyverse)
library(ggplot2)
library(forcats)
library(readxl)


##----------------##
## Tidy Fish Data ##
##----------------##

#read in data, inspect for errors

#fish data 2018
getwd()
?read_excel
fishy18 <- read_excel("C:/Users/bbkelly/Documents/Brook Trout_Brett/Thesis/data/Data 2018/FishToBeTidy_2018.xlsx")

names(fishy18)
head(fishy18)
str(fishy18)
summary(fishy18)

#UID to character 2018
fishy18$UID <- as.character(fishy18$UID)
class(fishy18$UID)

#fish data 2019
fishy19 <- read_excel("C:/Users/bbkelly/Documents/Brook Trout_Brett/Thesis/data/Data 2019/Fish_Data_2019.xlsx", 
                      sheet = 2)
names(fishy19)
str(fishy19)
summary(fishy19)

#UID to character 2019
fishy19$UID <- as.character(fishy19$UID)
class(fishy19$UID)

#Merge two data sets together without losing information, should be 141 obs. of 75 vars with only na's in TopCarn fishy19

bigfish1 <- full_join(fishy18, fishy19, by=NULL)
names(bigfish1)
summary(bigfish1)

#change column names using rename(), NEW name is on LEFT, OLD is on RIGHT
bigfish <- bigfish1 %>%
  rename(uid = UID, HUC8 = `Watershed (HUC 8)`, site = `Site #`, BKT_ab = 'BKT #', BRT_ab = 'BRT #', RBT_ab = 'RBT #', 
         TGT_ab = `TGT #`, LND_ab = 'LND #', SRD_ab = 'SRD #', Cottus_ab = 'Cottus #', CMM_ab = 'CMM #', SMM_ab = 'SMM #',
         MSM_ab = 'MSM #', WSU_ab = 'WSU #', CRC_ab = 'CRC #', WBD_ab = 'WBD #', CSR_ab = 'CSR #', JOD_ab = 'JOD #',
         FTD_ab = "FTD #", BSB_ab = 'BSB #', FHM_ab = 'FHM #', BNM_ab = 'BNM #', CSH_ab = 'CSH #', GRH_ab = 'GRH #',
         GSF_ab = 'GSF #', SSH_ab = 'SSH #', PPS_ab = 'PPS #', HHC_ab = 'HHC #', BMS_ab = 'BMS #', LMB_ab = 'LMB #',
         BLG_ab = 'BLG #', SHR_ab = `SHR #`, ABL_ab = 'ABL #', SLS_ab = 'SLS #', MTS_ab = 'MTS#', SPS_ab = 'SPS #',
         BLB_ab = 'BLB #', STC_ab = 'STC #')
colnames(bigfish)

#replace na's from the 2018 sites with zeros, only na's should be in TopCarn, Richness, MTS, and SLS
#species not seen in 2018 = SPS, BLB, and STC
summary(bigfish)
#SPS
bigfish$SPS <- bigfish$SPS %>% 
  replace_na(0)
summary(bigfish$SPS)
bigfish$SPS_ab <- bigfish$SPS_ab %>%
  replace_na(0)
#BLB
bigfish$BLB <- bigfish$BLB %>% 
  replace_na(0)

bigfish$BLB_ab <- bigfish$BLB_ab %>%
  replace_na(0)
#STC
bigfish$STC <- bigfish$STC %>% 
  replace_na(0)

bigfish$STC_ab <- bigfish$STC_ab %>%
  replace_na(0)

#did it work? Y/N
summary(bigfish)
#Y!

#Change HUC8 to a factor with three levels: UPI, YEL, LMAQ
class(bigfish$HUC8)

bigfish$HUC8["Upper Iowa River"]

bigfish$HUC8[bigfish$HUC8 == "Upper Iowa River"] <- "UPI"
bigfish$HUC8[bigfish$HUC8 == "Yellow River"] <- "YEL"
bigfish$HUC8[bigfish$HUC8 == "Little Maquoketa"] <- "LMAQ"

bigfish$HUC8 <- as.factor(bigfish$HUC8)
class(bigfish$HUC8)
levels(bigfish$HUC8)

#------------------------------------------------------------------------------------------
#remove columns: Top Carnivore and Richness; then recalculate and add them back using dplyr
fish <- bigfish %>%
  select(-Richness, -"Top Carnivore")
names(fish)

str(fish)


#make new df with only presence columns
fish_presence <- fish %>%
  select(BKT, BRT, RBT, TGT, LND, SRD, Cottus, CMM, SMM, MSM, WSU, CRC, WBD, CSR, JOD, FTD, BSB,
         FHM, BNM, CSH, GRH, GSF, SSH, PPS, HHC, BMS, LMB, BLG, SHR, ABL, SPS, BLB, STC)
names(fish_presence)

#Add richness column
rich <- fish %>%
  mutate(richness = rowSums(fish_presence))
summary(rich$richness)

ggplot(rich, aes(HUC8, richness)) +
  geom_boxplot(aes(color = HUC8))

#add Top Carn column
#select only top carn columns
predator <- fish %>%
  select(BKT_ab, BRT_ab, RBT_ab, TGT_ab, LMB_ab)
head(predator)

p_trial <- rich %>%
  mutate(TopCarn_ab = rowSums(predator))
summary(p_trial$TopCarn_ab)

ggplot(p_trial, aes(HUC8, TopCarn_ab, color = HUC8))+
  geom_boxplot()

#------------------------------------------------------------------------------------------
# Add totals columns, make totals table by species, write tidy CSV for Sam 
#------------------------------------------------------------------------------------------

totals <- p_trial %>%
  select(BKT_ab, BRT_ab, RBT_ab, TGT_ab, LND_ab, SRD_ab, Cottus_ab, CMM_ab, SMM_ab, MSM_ab, WSU_ab,
         CRC_ab, WBD_ab, CSR_ab, JOD_ab, FTD_ab, BSB_ab, FHM_ab, BNM_ab, CSH_ab, GRH_ab, GSF_ab,
         SSH_ab, PPS_ab, HHC_ab, BMS_ab, LMB_ab, BLG_ab, SHR_ab, ABL_ab, SPS_ab, BLB_ab, STC_ab)
names(totals)

Fish <- p_trial %>%
  mutate(total_ab = rowSums(totals))
names(Fish)
str(Fish)
summary(Fish)

sum(Fish$total_ab) #23,810 total fish

fish_table <- colSums(totals)
fish_table
hist(fish_table) #top three abundance: 1) White Sucker (n=3480), 2) Creek Chub (n=3359), 3) Brown Trout (n=3267)

#write tidy csv
getwd()
?write.csv
write.csv(Fish, "C:/Users/bbkelly/Documents/Brook Trout_Brett/Thesis/data/tidyfish1.csv", row.names = F)





##------------------------##
## tidy habitat data set  ##
##------------------------##

#load data from 2018
#getwd()
hab_18 <- read_excel("Data/Thesis/Raw/Data 2018/Habitat/HabToBeTidy_2018.xlsx")
str(hab_18)

#change column names using rename(), NEW name is on LEFT, OLD is on RIGHT
hab_18 <- hab_18 %>%
  rename(uid = UID, HUC8 = `Watershed (HUC 8)`)
names(hab_18)

#change HUC8 to factor
hab_18$HUC8 <- as.factor(hab_18$HUC8)
class(hab_18$HUC8)
levels(hab_18$HUC8)

#Change HUC8 levels: UPI, YEL, LMAQ
levels(hab_18$HUC8)[levels(hab_18$HUC8)=="Upper Iowa River"] <- "UPI"
levels(hab_18$HUC8)[levels(hab_18$HUC8)=="Yellow River"] <- "YEL"
levels(hab_18$HUC8)[levels(hab_18$HUC8)=="Little Maquoketa"] <- "LMAQ"
levels(hab_18$HUC8)

summary(hab_18)

#load data from 2019
hab_19 <- read_excel("Data/Thesis/Raw/Data 2019/Master_Hab_DataSet_2019.xlsx")
summary(hab_19)

#change column names using rename(), NEW name is on LEFT, OLD is on RIGHT
hab_19 <- hab_19 %>%
  rename(uid = UID, HUC8 = `Watershed (HUC 8)`)
names(hab_19)

#change HUC8 to factor
hab_19$HUC8 <- as.factor(hab_19$HUC8)
class(hab_19$HUC8)
levels(hab_19$HUC8)

#Data should be ready to join 
#Merge two data sets together without losing information, should be 141 obs. of 86 vars
str(hab_18)
str(hab_19) ##dissolved oxygen is a character for some odd reason, change it to numeric

hab_19$do <- as.numeric(hab_19$do)
class(hab_19$do)
summary(hab_19$do)

#get rid of BRTcpue on both habitat data sets
hab_18 <- hab_18 %>%
  select(-`BRTcpue(fish/min)`)
hab_19 <- hab_19 %>%
  select(-`BRTcpue(fish/min)`)

habitat <- full_join(hab_18, hab_19, by=NULL)
names(habitat)
summary(habitat)

#we will most likely not need the "extra" columns due to running models with new covariates, so eliminate those
habitat <- habitat %>%
  select(-extra1, -extra2, -extra3, -extra4, -extra5)
names(habitat)  
summary(habitat)

#Julian date needs to be numeric
habitat$JD <- as.numeric(habitat$JD)

#create new df with uid and RchLength to transfer over to fish data and IBI calculations
reach <- habitat %>%
  select(uid, RchLength)
head(reach)

#write csv of reach lengths for Sam so he can calculate metrics 11 and 12 of fish IBI
write.csv(reach, "C:/Users/bbkelly/Documents/Brook Trout_Brett/Thesis/data/reachlengths.csv", row.names = F)

#Write tidy csv of instream habitat
write.csv(habitat, "C:/Users/bbkelly/Documents/Brook Trout_Brett/Thesis/data/tidyhabitat1.csv", row.names = F)

