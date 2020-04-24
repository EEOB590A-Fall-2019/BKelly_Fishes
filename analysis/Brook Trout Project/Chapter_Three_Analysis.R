## Thesis: Chapter 3 - BRT vs. SGCN analysis

### Data Exploration and Analysis:
## Three main analyses:
#> Occupancy - 4 models: 1) all BRT, 2) all environment, 3) full model, 4) null model

#> Density 
  #1.) comparison of density b/w sites with and without BRT - permutation tests
  #2.) CPUE models - 1) all BRT, 2) all environment, 3) full model, 4) null model

#> Size - compare length frequency bins with permutation tests

library(tidyverse)
library(skimr)

######################################################################################
#> Occupancy - 4 models: 1) all BRT, 2) all environment, 3) full model, 4) null model
library(RMark)
library(corrplot)
#-------------------------------------------------------------------------------------

#load encounter history data 
lnd <- read.csv("Data/Thesis/Tidy/lnd_occu_data.csv", header = T) %>%
  unite(ch, c(ch1,ch2,ch3), sep = "", remove = T) %>%
  select(ch, freq, everything())

srd <- read.csv("Data/Thesis/Tidy/srd_occu_data.csv", header = T) %>%
  unite(ch, c(ch1,ch2,ch3), sep = "", remove = T) %>%
  select(ch, freq, everything())

cott <- read.csv("Data/Thesis/Tidy/cott_occu_data.csv", header = T) %>%
  unite(ch, c(ch1,ch2,ch3), sep = "", remove = T) %>%
  select(ch, freq, everything())

#load environmental data
env <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header = T)

#extract vars for detection probability
dp.cov <- env %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = T) %>%
  select(newID, effsec, mFlow, avdep, pctcbbl)

#add detection probability vars to enc histories

#lnd
lnd2 <- left_join(lnd, dp.cov, by="newID")
#lnd
srd2 <- left_join(srd, dp.cov, by="newID")
#cott
cott2 <- left_join(cott, dp.cov, by="newID")

#----------
#Hypotheses 
#----------
##########################################################################################
#Longnose Dace
#Occupancy
# Habitat:
#> avwid (+)
#> AvgSlope (+) "Mean Slope of the catchment"
#> pctriffle (+)

# Brown Trout:
#> BRT_100m (-/+) "Brown Trout Catch-Per 100m of stream sampled"
#> adult_100m (-) "Brown Trout adult Catch-Per 100m of stream sampled"
#> med_len (-/+) "median TL of brown trout"

#Detection
#> effsec (+)
#> mFlow (+)
#---

#Southern Redbelly Dace
#Occupancy
# Habitat:
#> HAiFLS_ag (-) "Hydrologically Active inserve flow length to the stream of crop LULC"
#> MEANT (+)
#> % fines (+)
#> avwid (-)

# Brown Trout:
#> BRT_100m (-) "Brown Trout Catch-Per 100m of stream sampled"
#> adult_100m (-) "Brown Trout adult Catch-Per 100m of stream sampled"
#> med_len (-) "median TL of brown trout"

#Detection
#> effsec (+)
#> avdep (+)
#---

#Cottus
#Occupancy
# Habitat:
#> HAiFLS_for (+) "Hydrologically Active inserve flow length to the stream of forest LULC"
#> MEANT (-)
#> BrBnk (-)

# Brown Trout:
# Brown Trout:
#> BRT_100m (+) "Brown Trout Catch-Per 100m of stream sampled"
#> adult_100m (-/+) "Brown Trout adult Catch-Per 100m of stream sampled"
#> med_len (-/+) "median TL of brown trout"

#Detection
#> effsec (+)
#> pctcbbl (+)
#---
##########################################################################################

#----------
#collinearity assessment 
#----------
c <- cor(lnd2[,4:19])
head(round(c,2)) 

#round down
cround <- round(c,3)

#visualize these correlations
corrplot(c, method = "number")

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(c)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(lnd2[,4:19])

#correlogram
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(c, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
##########################################################################################










