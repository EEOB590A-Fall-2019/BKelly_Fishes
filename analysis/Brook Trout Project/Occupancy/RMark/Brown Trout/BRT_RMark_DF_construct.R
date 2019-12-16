##############################################################################################################################

# Create an RMARK ready dataframe for Brown Trout
# Combine current Environmental Covariates and Brown Trout Encounter history
#   > trim covariates to those with biologically/ecologically relevant a priori hypotheses
#   > run correlation tests on covariates to know which predictor variables should and should not be included wthin same model

##############################################################################################################################

library(tidyverse)
library(skimr)

#read in Brook Trout encounter history and environmental covariates

Brk <- read_csv("Data/Thesis/Tidy/BKT_EncHist.csv", col_names = T)
covars <- read_csv("Data/Thesis/Tidy/enviro_tidy.csv", col_names = T)
fishdat <- read_csv("Data/Thesis/Tidy/tidyfish1.csv", col_names = T)


#----------------------------------------------------------------------
#need BRT CPUE as covariate
#first we need to create new vector just for BRT_ab

trutta <- fishdat %>%
  unite(newID, c(HUC8, site),sep = "_", remove = F)%>%
  select(newID, BRT_ab)

#rename to common identifiers
trutta[46,1] <- "UPI_201"
trutta[47,1] <- "UPI_202"
trutta[117,1] <- "YEL_97b"

#remove fishless and non-randomly selected sites
trutta <- trutta %>%
  filter(!newID %in% c("UPI_29", "UPI_165", "YEL_33", "YEL_98"))

#join with other covariates -- run lines below **FIRST!**
hab <- covars%>%
  unite(newID, c(HUC8, Site),sep = "_", remove = F)
habby <- left_join(hab, trutta, by = "newID")
habby <- habby %>%
  mutate(SampleRch = (RchLen*3)) %>%
  mutate(BRT_100m = (BRT_ab/SampleRch)*100)

#---------------------------------------------------------------------
#create reduced Brook Trout encounter history tbl
enc <- Brk %>%
  unite(newID, c(HUC8, site),sep = "_", remove = F)%>%
  select(EncHist, newID)%>%
  rename(ch = EncHist)
enc[96,2] <- "YEL_97b"

#join and remove identifiers and covariates unrelated to hypotheses
names(habby)
habby <- habby %>%
  select(-uid, -HUC8, -Site, -Order, -SegLen, -RchLen, -pH, -pctclay, -pctsilt, -pctsand, -pctbldr, 
         -FWD) %>%
  select(-BRT_ab, -SampleRch)


########################################################
#merged data
brook <- full_join(enc, habby, by = "newID")
########################################################

#inspect correlations between covariates
#install.packages("corrplot")
library(corrplot)

#correlation test
c <- cor(brook[,3:73])
head(round(c,2)) 


#round down
cround <- round(c,3)

#visualize these correlations
corrplot(c, type = "upper", order = "hclust", col = c("black", "white"),
         bg="lightblue")

#--------------------------------------------------------------------------
#extract high and low values
i=1
j=1
k=1

corr <- data.frame(var1 = character(),
                   var2 = character(),
                   corr = character(),
                   stringsAsFactors = F)

for(i in 1:nrow(cround)){
  for(j in 1:ncol(cround)){
    if(cround[i,j]>.6 | cround[i,j] < -.6){
      corr[k,1] <- as.character(row.names(cround)[i])
      corr[k,2] <- as.character(colnames(cround)[j])
      corr[k,3] <- as.character(cround[i,j])
      k = k+1
    }
  }
}
#-------------------------------------------------------------------------

#remove rows where variables are compared with themselves
correfilt <- corr%>%
  filter(var1 != var2)


###########################################################################
#Write CSV's

#RMARK dataframe
write.csv(brook,"Data/Thesis/Tidy/BKT_occDF_RMARK.csv", row.names = F)

#Correlation's Over .6
write.csv(correfilt,"Data/Thesis/Tidy/StrCorrelations_Covariates.csv", row.names = F)
