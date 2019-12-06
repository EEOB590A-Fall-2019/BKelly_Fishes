##########################################################################################################################
#                                       Preliminary BKT Occupancy Analysis in RMark -- Heirachical
##########################################################################################################################

# We have data on Brook Trout occurence across 138 unique sites (stream segments of 1st-4th order), 
# in northeast Iowa (driftless area ecoregion)
# Each site had three spatial replicates or occasions
# Naive occurence of Brook Trout: 19/138 sites or ~13.8% of sites
# sites were randomly selected using GRTS site selection 


# We will use package RMark to run single-species single-season occupancy models with a suite of environmental covariates
# some covariates are site specific while others are occasion specific
# Covariates cover instream parameters, riparian parameters, and catchment level parameters

##########################################################################################################################

#install.packages("RMark")
library(RMark)
library(tidyverse)
library(skimr)
library(psycho)
library(corrplot)

##Occupancy example
#?weta


##--------------------------------------------------------------------------------------------------------------------------------##
#read in data, rearrange and change some labels to work with grouping ("freq"), and time-varying covariates ("Effort1 --> Effort3")
brook <- read_csv("Data/Thesis/Tidy/BKT_occDF_RMARK.csv", col_names = T)
names(brook)
brook$freq <- 1
brook <- brook[,c(1,74,2:73)]
names(brook)

#reduce df down to variables of interest
brook2 <- brook %>%
  rename(effort1 = t1_eff, effort2 = t2_eff, effort3 = t3_eff, pctpool = pctslow)%>%
  select(1:3, effort1, effort2, effort3, pctex21, pctpool, machabprop, 
         pctrock, LWD, pctBrBnk, pctShade, BRT_100m)
#examine
skim(brook2)

#standardize covariates to have center 0 --- should we do this? (subtract mean and divide by sd)
x <- brook2 %>%
  select(4:6) %>%
  psycho::standardize()
summary(x)
skim(x)

brook3 <- brook2 
brook3[,4:6]=x

#correlation test
c <- cor(brook3[,4:14])
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
p.mat <- cor.mtest(brook3[,4:14])

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
#weak evidence that pctShade and LWD should not be in same model due to correlation (0.57)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Variables of interest and Data Dictionary ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Detection Probability: probability of detecting Brook Trout at a site
#> time varying effort (+) "amount (seconds) of time spent sampling (aka shocking)"

#Occupancy Probability: probability of Brook Trout occurrence among sites
# Local Scale: instream and immediate riparian area
#> pctex21 (-) "percentage of summer temperature observations that exceed 21 degrees C"
#> pctpool (+) "percentage of pool habitats"
#> pctrock (+) "percentage of rocky substrates (gravel, cobble, boulder)"
#> LWD (+) "percentage of large woody debris"
#> pctBrBnk (-) "percentage of bank that is bare soil"
#> pctShade (+) "average percent canopy cover"
#> BRT_100m (-) "Brown Trout Catch-Per 100m of stream sampled"
##########################################################################################
getwd()
#set wd to scratch folder because MARK outputs an insane amount of files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Analysis/Brook Trout Project/RMark/BrookTrout_ScratchFolder") #because MARK loves output files


#Process Data
#?process.data
#?make.design.data
brook.process = process.data(brook3, model="Occupancy", groups = "freq")
bkt.ddl = make.design.data(brook.process)


#############################################################
## For this portion of the analysis we will focus on       ##
## local covariates (instream and immediate riparian area) ##
############################################################

###~~~~~~~~~~~~~##
#### Round 1 ####
##~~~~~~~~~~~~~##

run.occ=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.tv.effort = list(formula = ~effort)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #direct effects - hypothesized larger effects (factors that may directly influence BKT persistence)
  Psi.biology = list(formula = ~pctex21+pctpool+pctrock+BRT_100m)
  Psi.p21_pool_rck = list(formula = ~pctex21+pctpool+pctrock)
  Psi.p21_pool_brt = list(formula = ~pctex21+pctpool+BRT_100m)
  Psi.p21_rck_brt = list(formula = ~pctex21+pctrock+BRT_100m)
  Psi.pct21_pool = list(formula = ~pctex21 + pctpool) 
  Psi.pct21_rock = list(formula = ~pctex21 + pctrock) 
  Psi.pct21_brt = list(formula = ~pctex21 + BRT_100m)
  #indirect effects - hypothesized smaller effects (features of high quality stream habitats (for trout))
  Psi.lwd_pctBrBnk = list(formula = ~LWD + pctBrBnk)
  Psi.lwd_shade = list(formula = ~LWD + pctShade)
  Psi.BBnk_shade = list(formula = ~pctBrBnk+pctShade)
  #combination of effects
  Psi.global = list(formula = ~pctex21+pctpool+pctrock+LWD+pctBrBnk+BRT_100m)
  Psi.global2 = list(formula = ~pctex21+pctpool+pctrock+pctBrBnk+pctShade+BRT_100m)
  Psi.mixed1 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+LWD)
  Psi.mixed2 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctBrBnk)
  Psi.mixed3 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctShade)
  Psi.mixed4 = list(formula = ~pctex21+pctpool+pctrock+LWD+pctBrBnk)
  Psi.mixed5 = list(formula = ~pctex21+pctpool+pctrock+pctShade+pctBrBnk)
  Psi.mixed6 = list(formula = ~pctex21+pctpool+BRT_100m+LWD+pctBrBnk)
  Psi.mixed7 = list(formula = ~pctex21+pctpool+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed8 = list(formula = ~pctex21+pctrock+BRT_100m+LWD+pctBrBnk)
  Psi.mixed9 = list(formula = ~pctex21+pctrock+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed10 = list(formula = ~pctpool+pctrock+BRT_100m+LWD+pctBrBnk)
  Psi.mixed11 = list(formula = ~pctpool+pctrock+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed12 = list(formula = ~pctex21+pctpool+LWD+pctBrBnk)
  Psi.mixed13 = list(formula = ~pctex21+pctpool+pctShade+pctBrBnk)
  Psi.mixed14 = list(formula = ~pctex21+pctpool+LWD+pctBrBnk)
  Psi.mixed15 = list(formula = ~pctex21+pctpool+pctShade+pctBrBnk)
  Psi.mixed16 = list(formula = ~pctex21+BRT_100m+LWD+pctBrBnk)
  Psi.mixed17 = list(formula = ~pctex21+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed18 = list(formula = ~pctrock+BRT_100m+LWD+pctBrBnk)
  Psi.mixed19 = list(formula = ~pctrock+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed20 = list(formula = ~pctpool+pctrock+LWD+pctBrBnk)
  Psi.mixed21 = list(formula = ~pctpool+pctrock+pctShade+pctBrBnk)
  Psi.mixed22 = list(formula = ~pctex21+pctrock+LWD+pctBrBnk)
  Psi.mixed23 = list(formula = ~pctex21+pctrock+pctShade+pctBrBnk)
  Psi.mixed24 = list(formula = ~pctpool+BRT_100m+LWD+pctBrBnk)
  Psi.mixed25 = list(formula = ~pctpool+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed26 = list(formula = ~pctex21+LWD+pctBrBnk)
  Psi.mixed27 = list(formula = ~pctex21+pctShade+pctBrBnk)
  Psi.mixed28 = list(formula = ~pctpool+LWD+pctBrBnk)
  Psi.mixed29 = list(formula = ~pctpool+pctShade+pctBrBnk)
  Psi.mixed30 = list(formula = ~pctrock+LWD+pctBrBnk)
  Psi.mixed31 = list(formula = ~pctrock+pctShade+pctBrBnk)
  Psi.mixed32 = list(formula = ~BRT_100m+LWD+pctBrBnk)
  Psi.mixed33 = list(formula = ~BRT_100m+pctShade+pctBrBnk)
  Psi.pct21_lwd = list(formula = ~pctex21 + LWD) 
  Psi.pct21_bare = list(formula = ~pctex21 + pctBrBnk) 
  Psi.pct21_shade = list(formula = ~pctex21 + pctShade)
  Psi.pool_lwd = list(formula = ~pctpool + LWD) 
  Psi.pool_bare = list(formula = ~pctpool + pctBrBnk) 
  Psi.pool_shade = list(formula = ~pctpool + pctShade) 
  Psi.rock_lwd = list(formula = ~pctrock + LWD) 
  Psi.rock_bare = list(formula = ~pctrock + pctBrBnk) 
  Psi.rock_shade = list(formula = ~pctrock + pctShade) 
  Psi.BRT_lwd = list(formula = ~BRT_100m + LWD) 
  Psi.BRT_bare = list(formula = ~BRT_100m + pctBrBnk) 
  Psi.BRT_shade = list(formula = ~BRT_100m + pctShade) 
  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  Psi.pctex21 = list(formula=~pctex21) 
  Psi.pool = list(formula=~pctpool) 
  Psi.MacroProp = list(formula=~machabprop) 
  Psi.rock = list(formula=~pctrock) 
  Psi.lwd = list(formula = ~LWD) 
  Psi.bare = list(formula=~pctBrBnk) 
  Psi.shade = list(formula=~pctShade) 
  Psi.brt = list(formula=~BRT_100m) 
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml=create.model.list("Occupancy")
  results=mark.wrapper(cml, data=brook.process, ddl=bkt.ddl, output=F)
  return(results)
}

bkt.results = run.occ()


##Examine model list and look at model comparisons
bkt.results
##Model Table
bkt.results$model.table = model.table(bkt.results, use.lnl = T)
bkt.results$model.table

#look at summary of top model
summary(bkt.results$p.tv.effort.Psi.pct21_brt)
names(bkt.results)
round1.top <- bkt.results$p.tv.effort.Psi.pct21_brt
summary(round1.top, showall = F)
round1.top$results$real

summary(bkt.results$p.tv.effort.Psi.biology)
summary(bkt.results$p.tv.effort.Psi.global)


cleanup(ask = FALSE)


#---------------------------------------------------------------------------------------------------#
#export MARK data with models
#export ch data to an .inp file
#str(brook2)
#export.chdata(brook.process, filename = "BrookOccu", covariates = "all")
#export.MARK(brook.process, "BKT_OccPrelim", model = bkt.results, ind.covariates = "all", replace = T)
#---------------------------------------------------------------------------------------------------#
