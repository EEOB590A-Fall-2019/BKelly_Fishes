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
present[1,6:8] <- c(,,)


#update values in "absent" df
absent[,6:8] <- 0

#join dataset back together
enc <- full_join(absent, present, by=NULL)

#create column for enc history in string format ("001" for example)
brown_enc <- enc %>%
  unite(ch, c(5:7), sep = "", remove = F)
brown_enc <- Brook_enc[,c(1:3,5,4,6:8)]

######################################
## Write csv for BKT encounter history
######################################

write.csv(brown_enc, "Data/Thesis/Tidy/BRT_ch_data.csv", row.names = F)
write.csv(enc, "Data/Thesis/Tidy/BKT_Occ_Hist.csv", row.names = F)