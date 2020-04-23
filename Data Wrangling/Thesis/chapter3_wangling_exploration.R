## Thesis: Chapter 3 - BRT vs. SGCN analysis

### Data Organization:
    #> make sure data deminsions are consistent
    #> check for outliers, many NAs
    #> make sure units are consistent
    #> summarize to make new column variables

#libraries
library(tidyverse)
library(skimr)

#load data
brt <- read.csv("Data/Thesis/Raw/All Years/browntrout_LW_data_2020.csv", header=T)
sgcn <- read.csv("Data/Thesis/Raw/All Years/sgcn_abun_length_2020.csv", header=T)

#inspect data
skim(brt) # seems to be the correct data type for all variables, 8 missing weights
names(brt)

skim(sgcn)

#let's work with the brown trout data first

#change length measurements for UPI_149
inches <- brt[574:871,] %>%
  mutate(len_mm = (length*25.4))
x <- inches$len_mm
summary(x)

brt[574:871,4] <- x

br2 <- brt %>%
  filter(species == "BRT")

#abundance covariate - 6 inch minimum length for inclusion
adults <- br2 %>%
  filter(length > 152.3) 

num_adults <- adults %>%
  count(HUC8, site) %>%
  rename(adult_count = n) %>%
  unite(col = "newID", c(HUC8,site), sep = "_", remove = F)

#size covariate
br3 <- br2 %>%
  group_by(HUC8, site) %>%
  summarise(mean_len = mean(length), med_len = median(length)) %>%
  unite(col = "newID", c(HUC8,site), sep = "_", remove = F)
  
#combine previous 2 dfs
bdat <- left_join(br3, num_adults, by="newID") %>%
  select(-HUC8.y, -site.y) %>%
  rename(HUC8 = HUC8.x, site = site.x) %>%
  replace_na(list(adult_count = 0))

#load environmental covariates
allcov <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header = T)
env <- read.csv("Data/Thesis/Tidy/enviro_tidy.csv", header = T)
reach <- read.csv("Data/Thesis/Tidy/reachlengths.csv", header = T)
etc <- read.csv("Data/Thesis/Tidy/BKT_Occu_File.csv", header = T)
misc <- read.csv("Data/Thesis/Tidy/BKT_occDF_RMARK.csv", header = T)
ndata <- read.csv("Data/Thesis/Tidy/SGCN_AllCovariates.csv", header=T)

names(misc)
cov1 <- misc %>%
  select(newID, pctSlope, pctRiffle, pctfines, avwid, MEANT, pctBrBnk, HAiFLS_for, HAiFLS_ag, BRT_100m) %>%
  rename(BrBnk = pctBrBnk)

cov2 <- allcov %>%
  select(HUC_Site, RchLength) %>%
  rename(newID = HUC_Site)

#add reach lengths to bdat
bdat2 <- left_join(bdat, cov2, by = "newID") %>%
  mutate(seg_length = (RchLength*3), adult_100m = (adult_count/seg_length)*100)

#add environmental covariates to bdat2
bdat3 <- left_join(bdat2, cov1, by = "newID")

write.csv(bdat3, "Data/Thesis/Tidy/chapt3_data_full.csv", row.names = F)

#trim down
names(bdat3)
bdat4 <- bdat3 %>%
  select(newID, BRT_100m, adult_100m, mean_len, med_len, pctSlope, HAiFLS_for, HAiFLS_ag,
         avwid, pctRiffle, pctfines, BrBnk, MEANT)

names(ndata)

nd2 <- ndata %>%
  select(newID, HUC_12, BRT, LND_CPUE, SRD_CPUE, Cottus_CPUE, avwid, pctfines, pctriffle, BrBank, MEANT, HAiFLS_ag, HAiFLS_for, 
         pctSlope)

names(bdat4)
bdat5 <- bdat4 %>%
  select(-HAiFLS_for, -HAiFLS_ag, -avwid, -pctRiffle, -pctfines, -BrBnk, -MEANT, -pctSlope)

cpue_mod_data <- left_join(nd2, bdat5, by="newID") %>%
  select(-HUC8) %>%
  replace_na(list("BRT_100m" = 0, "adult_100m" = 0, "mean_len" = 0, "med_len" = 0))

cpue_mod_data[97,14]
s <- mean(cpue_mod_data$pctSlope, na.rm = T)
cpue_mod_data[97,14] <- s

#export
write.csv(cpue_mod_data, "Data/Thesis/Tidy/chpt3_tidy.csv", row.names = F)


#----------
# SGCN Data
#----------
names(sgcn)

sgcn2 <- sgcn %>%
  unite(col = "newID", c(HUC8,site), sep = "_", remove = F) %>%
  replace_na(list("X30_60" = 0, "X60_90" = 0, "X90_120" = 0, "X120_150" = 0, 
                  "X150_180" = 0, "X180_210" = 0, "X210_240" = 0, "X240plus" = 0))

#recode to Cottus
levels(sgcn2$species)
sgcn2$species <- recode(sgcn2$species, MTS = "Cottus", SLS = "Cottus", SCULPIN = "Cottus")
levels(sgcn2$species)



#------------------------------------------------------------------------------------------------------------
#                               Encounter History for Occupancy Modeling
#------------------------------------------------------------------------------------------------------------
names(sgcn2)

ehist <- sgcn2 %>%
  select(newID, HUC8, site, reach, species, X30_60, X60_90, X90_120, X120_150, X150_180) %>%
  filter(species %in% c("LND","SRD","Cottus")) %>%
  droplevels()

#split by species
class(ehist$reach)

#-----
#longnose dace
#-----
lnd <- ehist %>%
  filter(species == "LND") %>%
  select(newID, reach, species) %>%
  pivot_wider(names_from = reach, values_from = species) %>%
  rename(p1=2, p2=3, p3=4) %>%
  mutate(ch1 = ifelse(p1 == "LND",1,0), ch2 = ifelse(p2 == "LND",1,0), ch3 = ifelse(p3 == "LND",1,0)) %>%
  replace_na(list("ch1"=0, "ch2"=0, "ch3"=0)) %>%
  select(-p1,-p2,-p3)

#occupancy modeling ready dataframe
lnd.occu.frame <- left_join(cpue_mod_data, lnd, by="newID") %>%
  replace_na(list("ch1"=0, "ch2"=0, "ch3"=0)) %>%
  unite(ch, c(ch1,ch2,ch3), sep = "", remove = T) %>%
  mutate(freq = 1) %>%
  select(ch, freq, everything()) %>%
  select(-HUC_12, -BRT, -LND_CPUE, -SRD_CPUE, -Cottus_CPUE)

write.csv(lnd.occu.frame, "Data/Thesis/Tidy/lnd_occu_data.csv", row.names = F)

#-----
#southern redbelly dace
#-----
srd <- ehist %>%
  filter(species == "SRD") %>%
  select(newID, reach, species) %>%
  pivot_wider(names_from = reach, values_from = species) %>%
  rename(p1=2, p2=3, p3=4) %>%
  mutate(ch1 = ifelse(p1 == "SRD",1,0), ch2 = ifelse(p2 == "SRD",1,0), ch3 = ifelse(p3 == "SRD",1,0)) %>%
  replace_na(list("ch1"=0, "ch2"=0, "ch3"=0)) %>%
  select(-p1,-p2,-p3)

#occupancy modeling ready dataframe
srd.occu.frame <- left_join(cpue_mod_data, srd, by="newID") %>%
  replace_na(list("ch1"=0, "ch2"=0, "ch3"=0)) %>%
  unite(ch, c(ch1,ch2,ch3), sep = "", remove = T) %>%
  mutate(freq = 1) %>%
  select(ch, freq, everything()) %>%
  select(-HUC_12, -BRT, -LND_CPUE, -SRD_CPUE, -Cottus_CPUE)
  
write.csv(srd.occu.frame, "Data/Thesis/Tidy/srd_occu_data.csv", row.names = F)

#-----
#Sculpins - "Cottus"
#-----
cott <- ehist %>%
  filter(species == "Cottus") %>%
  select(newID, reach, species) %>%
  droplevels() %>%
  mutate(p1 = ifelse(reach == 1, 1,0), p2 = ifelse(reach == 2, 1,0), p3 = ifelse(reach == 3, 1,0)) %>%
  group_by(newID) %>%
  summarise(ch1 = max(p1), ch2 = max(p2), ch3 = max(p3))


#occupancy modeling ready dataframe
cott.occu.frame <- left_join(cpue_mod_data, cott, by="newID") %>%
  replace_na(list("ch1"=0, "ch2"=0, "ch3"=0)) %>%
  unite(ch, c(ch1,ch2,ch3), sep = "", remove = T) %>%
  mutate(freq = 1) %>%
  select(ch, freq, everything()) %>%
  select(-HUC_12, -BRT, -LND_CPUE, -SRD_CPUE, -Cottus_CPUE)


write.csv(cott.occu.frame, "Data/Thesis/Tidy/cott_occu_data.csv", row.names = F)

#------------------------------------------------------------------------------------------------------------
#                               Count data for length frequency distributions
#------------------------------------------------------------------------------------------------------------
sgcn_counts <- sgcn2 %>% #sum across size bins for 3 sgcns
  group_by(newID, species) %>%
  summarise(n_30_60 = sum(X30_60), n_60_90 = sum(X60_90), n_90_120 = sum(X90_120),
            n_120_150 = sum(X120_150), n_150_180 = sum(X150_180)) %>%
  filter(species %in% c("LND","SRD","Cottus")) %>%
  droplevels()

new_dat <- as.data.frame(sgcn_counts)
summary(new_dat$n_150_180)
which(new_dat$n_150_180==1)

new_dat2 <- new_dat %>% #trim the unwanted 150-180 bin - outliers in SRD! no way two individuals >5 inches
  select(-n_150_180) %>%
  rename(bin1 = n_30_60, bin2 = n_60_90, bin3 = n_90_120, bin4 = n_120_150)

trial <- new_dat2 %>% #long to wide format data
  pivot_wider(names_from = species, values_from = c(bin1, bin2, bin3, bin4))
summary(trial)

trial[is.na(trial)] <- 0 #replace NAs with zeros
summary(trial)

brt_col <- cpue_mod_data %>%
  select(newID, BRT, BRT_100m, adult_100m) %>%
  mutate(prop_adults = (adult_100m/BRT_100m)) %>%
  mutate(adult_status = ifelse(prop_adults>0.49,1,0)) %>%
  replace_na(list("adult_status" = 0)) %>%
  select(newID, BRT, adult_status)

tidy.data <- left_join(trial, brt_col, by="newID") %>%
  select(-bin4_LND, -bin4_SRD)

summary(tidy.data)

#write tidy csv
write.csv(tidy.data, "Data/Thesis/Tidy/sgcn_size_tidy.csv", row.names = F)





