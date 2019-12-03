############################################################
# Create occasion-specific encounter history for Brook Trout
############################################################
library(tidyverse)




#bring in fish data, and isolate BKT
fishdat <- read_csv("Data/Thesis/Tidy/tidyfish1.csv", col_names = T)
brook <- fishdat %>%
  select(1:4)

#change names to match environmental covariate data set
brook[46,3] <- 201
brook[47,3] <- 202

#remove sites deemed unfit for alaysis (fishless or area not randomly selected)
brook <- brook %>%
  unite(newID, c(HUC8, site), sep = "_", remove = F)%>%
  filter(!newID %in% c("UPI_29", "UPI_165", "YEL_33", "YEL_98"))%>%
  select(-newID)

#create new columns with occ1, occ2, occ3
brook <- brook %>%
  mutate(occ1=NA, occ2=NA, occ3=NA)

#isolate sites where BKT were present and absent
present <- brook %>%
  filter(BKT == 1)

absent <- brook %>%
  filter(BKT == 0)

#update values of occasion specific occupancy in "present" df
present[1,5:7] <- c(0,1,0)
present[2,5:7] <- c(1,1,1)
present[3,5:7] <- c(1,1,0)
present[4,5:7] <- c(1,0,0)
present[5,5:7] <- c(0,0,1)
present[6,5:7] <- c(1,1,1)
present[7,5:7] <- c(1,1,0)
present[8,5:7] <- c(1,1,1)
present[9,5:7] <- c(1,1,1)
present[10,5:7] <- c(1,1,1)
present[11,5:7] <- c(0,1,0)
present[12,5:7] <- c(1,1,1)
present[13,5:7] <- c(1,1,1)
present[14,5:7] <- c(1,1,1)
present[15,5:7] <- c(0,1,1)
present[16,5:7] <- c(1,1,1)
present[17,5:7] <- c(1,1,1)
present[18,5:7] <- c(1,0,0)
present[19,5:7] <- c(0,0,1)


#update values in "absent" df
absent[,5:7] <- 0

#join dataset back together
Enc <- full_join(absent, present, by=NULL)

#create column for enc history in string format ("001" for example)
Brook_enc <- Enc %>%
  unite(EncHist, c(5:7), sep = "", remove = F)
Brook_enc <- Brook_enc[,c(1:3,5,4,6:8)]

######################################
## Write csv for BKT encounter history
######################################

write.csv(Brook_enc, "Data/Thesis/Tidy/BKT_EncHist.csv", row.names = F)







