############################################################
# Create occasion-specific encounter history for Brown Trout
############################################################
library(tidyverse)

#bring in fish data, and isolate BRT
fishdat <- read_csv("Data/Thesis/Tidy/tidyfish1.csv", col_names = T)
brown <- fishdat %>%
  select(1:3,BRT)

#change names to match environmental covariate data set
brown[46,3] <- 201
brown[47,3] <- 202

#remove sites deemed unfit for alaysis (fishless or area not randomly selected)
brown <- brown %>%
  unite(newID, c(HUC8, site), sep = "_", remove = F)%>%
  filter(!newID %in% c("UPI_29", "UPI_165", "YEL_33", "YEL_98"))

#create new columns with occ1, occ2, occ3
brown <- brown %>%
  mutate(occ1=NA, occ2=NA, occ3=NA)

#isolate sites where BKT were present and absent
present <- brown %>%
  filter(BRT == 1)

absent <- brown %>%
  filter(BRT == 0)

#update values of occasion specific occupancy in "present" df
present[1,6:8] <- c(1,0,1)
present[2,6:8] <- c(0,0,1)
present[3,6:8] <- c(0,1,1)
present[4,6:8] <- c(0,1,0)
present[5,6:8] <- c(1,0,0)
present[6,6:8] <- c(1,1,1)
present[7,6:8] <- c(1,0,1)
present[8,6:8] <- c(0,0,1)
present[9,6:8] <- c(1,1,1)
present[10,6:8] <- c(1,1,1)
present[11,6:8] <- c(1,0,1)
present[12,6:8] <- c(1,1,1)
present[13,6:8] <- c(1,1,1)
present[14,6:8] <- c(1,0,0)
present[15,6:8] <- c(1,1,1)
present[16,6:8] <- c(1,1,1)
present[17,6:8] <- c(1,1,1)
present[18,6:8] <- c(1,1,1)
present[19,6:8] <- c(1,1,1)
present[20,6:8] <- c(1,1,1)
present[21,6:8] <- c(1,1,1)
present[22,6:8] <- c(1,1,1)
present[23,6:8] <- c(1,1,1)
present[24,6:8] <- c(1,1,1)
present[25,6:8] <- c(1,1,1)
present[26,6:8] <- c(1,1,1)
present[27,6:8] <- c(1,1,1)
present[28,6:8] <- c(0,0,1)
present[29,6:8] <- c(1,1,1)
present[30,6:8] <- c(1,0,1)
present[31,6:8] <- c(1,1,1)
present[32,6:8] <- c(0,1,0)
present[33,6:8] <- c(1,1,1)
present[34,6:8] <- c(1,1,1)
present[35,6:8] <- c(1,0,1)
present[36,6:8] <- c(1,1,0)
present[37,6:8] <- c(1,1,1)
present[38,6:8] <- c(1,0,0)
present[39,6:8] <- c(1,1,1)
present[40,6:8] <- c(0,1,1)
present[41,6:8] <- c(1,1,1)
present[42,6:8] <- c(1,1,1)
present[43,6:8] <- c(1,1,1)
present[44,6:8] <- c(1,1,1)
present[45,6:8] <- c(1,1,1)
present[46,6:8] <- c(1,0,1)
present[47,6:8] <- c(0,1,1)
present[48,6:8] <- c(1,1,1)
present[49,6:8] <- c(0,1,0)
present[50,6:8] <- c(1,0,0)
present[51,6:8] <- c(1,0,0)
present[52,6:8] <- c(1,1,1)
present[53,6:8] <- c(1,0,0)
present[54,6:8] <- c(1,0,0)
present[55,6:8] <- c(1,1,1)
present[56,6:8] <- c(1,1,1)
present[57,6:8] <- c(1,1,1)
present[58,6:8] <- c(1,1,1)
present[59,6:8] <- c(1,1,1)
present[60,6:8] <- c(0,1,1)
present[61,6:8] <- c(1,1,1)
present[62,6:8] <- c(1,1,1)
present[63,6:8] <- c(1,1,1)
present[64,6:8] <- c(1,1,1) #change to 97b for later communication with other data sets
present[64,2] <- "YEL_97b"
present[64,4] <- "97b"
present[65,6:8] <- c(1,1,1)
present[66,6:8] <- c(1,1,1)
present[67,6:8] <- c(1,1,1)
present[68,6:8] <- c(1,1,1)
present[69,6:8] <- c(0,1,0)
present[70,6:8] <- c(1,1,1)
present[71,6:8] <- c(1,1,1)
present[72,6:8] <- c(1,1,0)
present[73,6:8] <- c(1,1,1)
present[74,6:8] <- c(1,1,1)



#update values in "absent" df
absent[,6:8] <- 0

#join dataset back together
enc <- full_join(absent, present, by=NULL)

#create column for enc history in string format ("001" for example)
brown_ch <- enc %>%
  unite(ch, c(6:8), sep = "", remove = F)

#create RMark format dataset
brt_ch <- brown_ch %>%
  mutate(freq = 1) %>%
  select(ch, freq, newID)
class(brt_ch$newID) #confirm it is a character


######################################
## Write csv for BKT encounter history
######################################

write.csv(brt_ch, "Data/Thesis/Tidy/BRT_ch_data.csv", row.names = F)
write.csv(brown_ch, "Data/Thesis/Tidy/BKT_Full_Occ_Hist.csv", row.names = F)














