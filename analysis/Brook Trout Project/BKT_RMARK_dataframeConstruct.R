##############################################################################################################################

# Create an RMARK ready dataframe for Brook Trout
# Combine current Environmental Covariates and Brook Trout Encounter history
#   > trim covariates to those with biologically/ecologically relevant a priori hypotheses
#   > run correlation tests on covariates to know which predictor variables should and should not be included wthin same model

##############################################################################################################################

library(tidyverse)
library(skimr)
library(ggplot2)

#read in Brook Trout encounter history and environmental covariates

Brk <- read_csv("Data/Thesis/Tidy/BKT_EncHist.csv", col_names = T)
covars <- read_csv("Data/Thesis/Tidy/enviro_tidy.csv", col_names = T)

#create reduced Brook Trout encounter history tbl
enc <- Brk %>%
  unite(newID, c(HUC8, site),sep = "_", remove = F)%>%
  select(EncHist, newID)%>%
  rename(ch = EncHist)

#join and remove identifiers and covariates unrelated to hypotheses
names(covars)
hab <- covars%>%
  unite(newID, c(HUC8, Site),sep = "_", remove = F)%>%
  select(-uid, -HUC8, -Site, -Order, -SegLen, -RchLen, -pH, -pctclay, -pctsilt, -pctsand, -pctbldr, 
         -FWD)

enc[96,2] <- "YEL_97b"

########################################################
#merged data
brook <- full_join(enc, hab, by = "newID")
########################################################

#inspect correlations between covariates
install.packages("corrplot")
library(corrplot)

#correlation test
c <- as.data.frame(cor(brook[,3:72]))
head(round(c,2)) 


#round down
cround <- round(c,3)%>%
  (filter(1:70 > .6))
str(cround)

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



