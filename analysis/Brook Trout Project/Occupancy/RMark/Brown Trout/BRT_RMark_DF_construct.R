##############################################################################################################################

# Create an RMARK ready dataframe for Brown Trout
# Combine current Environmental Covariates and Brown Trout Encounter history
#   > trim covariates to those with biologically/ecologically relevant a priori hypotheses
#   > run correlation tests on covariates to know which predictor variables should and should not be included wthin same model

##############################################################################################################################

library(tidyverse)
library(skimr)
library(corrplot)
#setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")

#read in Brown Trout encounter history and environmental covariates

brt <- read_csv("Data/Thesis/Tidy/BRT_ch_data.csv", col_names = T)
covars <- read_csv("Data/Thesis/Tidy/enviro_tidy.csv", col_names = T)
spc <- read_csv("Data/Thesis/Raw/All Years/Catchment_Attributes.csv", col_names = T)

#----------------------------------------------------------------------

#join and remove identifiers and covariates unrelated to hypotheses
names(covars)
covars2 <- covars %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = T) %>%
  select(newID, t1_eff, t2_eff, t3_eff, avgT, MAXT, MEANT, RNGT, avwid, avdep, mFlow,
         pctrun, pctslow, pctBrBnk, HAiFLS_alt, HAiFLS_for, HAiFLS_nat) %>%
  rename(effort1 = t1_eff, effort2 = t2_eff, effort3 = t3_eff)

names(spc)
spc2 <- spc %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = T) %>%
  select(newID, POLY_AREA, Avg_Percen, Count_2) %>%
  rename(Area_km2=POLY_AREA, AvgSlope=Avg_Percen, Cross_Cat=Count_2) %>%
  filter(newID != "UPI_29" & newID != "YEL_33") 

spc2[9,1] <- "UPI_57b"
spc2[12,1] <- "UPI_14b"
spc2[26,1] <- "UPI_78b"
spc2[57,1] <- "YEL_118b"
spc2[67,1] <- "UPI_32b"
spc2[104,1] <- "YEL_97b"
spc2[132,1] <- "LMAQ_28b"
spc2[129,1] <- "LMAQ_48"

envc <- full_join(covars2, spc2, by = "newID")

envc <- envc %>%
  mutate(HAiFLS_al2=(HAiFLS_alt^2))

########################################################
#merge ch and covariate data
brown <- full_join(brt, envc, by = "newID")
########################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#inspect correlations between covariates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
psi.vars <- brown[,7:22]

#correlation test
c <- cor(psi.vars)
head(round(c,2)) 

#round down
cround <- round(c,3)

#visualize these correlations
corrplot(c, type = "upper", order = "hclust", col = c("black", "white"),
         bg="lightblue")


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
p.mat <- c

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

pairs(psi.vars)

############################################################################
#         Remove covars based on correlation and lit review               #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

brown <- brown %>%
  select(-RNGT)

###########################################################################
#Write CSV's

#RMARK dataframe
write.csv(brown,"Data/Thesis/Tidy/BRT_RMark.csv", row.names = F)


