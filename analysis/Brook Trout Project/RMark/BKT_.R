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
#library(psycho)

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
  rename(effort1 = t1_eff, effort2 = t2_eff, effort3 = t3_eff)%>%
  select(1:3, effort1, effort2, effort3, pctex21, pctslow, machabprop, 
         pctrock, LWD, pctBrBnk, pctShade, BRT_100m)
#examine
skim(brook2)

#standardize covariates to have center 0 --- should we do this? (subtract mean and divide by sd)
#x <- brook2 %>%
#  select(4:17) %>%
#  psycho::standardize()
#summary(x)
#skim(x)

#brook3 <- brook2 
#brook3[,4:17]=x

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Variables of interest and Data Dictionary ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Detection Probability: probability of detecting Brook Trout at a site
#> time varying effort (+) "amount (seconds) of time spent sampling (aka shocking)"

#Occupancy Probability: probability of Brook Trout occurrence among sites
# Local Scale: instream and immediate riparian area
#> pctex21 (-) "percentage of summer temperature observations that exceed 21 degrees C"
#> pctslow (+) "percentage of slow moving deep habiatas aka pools and glides"
#> machabprop (-) "proportion of macrohabitat type"
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
brook.process = process.data(brook2, model="Occupancy", groups = "freq")
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
  #~~~~~~~~~~~~~ Occupancy - global model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.global = list(formula = ~pctex21+pctslow+machabprop+pctrock+LWD+pctBrBnk+pctShade+BRT_100m)
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #direct effects - hypothesized larger effects
  Psi.biology = list(formula = ~pctex21+pctslow+pctrock+BRT_100m) #factors that may directly influence BKT persistence
  Psi.p21_slo_rck = list(formula = ~pctex21+pctslow+pctrock)
  Psi.p21_slo_brt = list(formula = ~pctex21+pctslow+BRT_100m)
  Psi.p21_rck_brt = list(formula = ~pctex21+pctrock+BRT_100m)
  Psi.pct21_slo = list(formula = ~pctex21 + pctslow) 
  Psi.pct21_rock = list(formula = ~pctex21 + pctrock) 
  Psi.pct21_brt = list(formula = ~pctex21 + BRT_100m)
  #Psi.pct21_brt_int = list(formula = ~pctex21*BRT_100m) temperature and brown trout interaction
  #indirect effects - hypothesized smaller effects
  Psi.habitat = list(formula = ~machabprop+LWD+pctBrBnk+pctShade) #features of high quality stream habitats (for trout)
  Psi.mhp_lwd_BBnk = list(formula = ~machabprop+LWD+pctBrBnk)
  Psi.mhp_lwd_shde = list(formula = ~machabprop+LWD+pctShade)
  Psi.mhp_BBnk_shde = list(formula = ~machabprop+pctBrBnk+pctShade)
  Psi.lwd_BBnk_shde = list(formula = ~LWD+pctBrBnk+pctShade)
  Psi.mhp_lwd = list(formula = ~machabprop + LWD)
  Psi.mhp_BBnk = list(formula = ~machabprop + pctBrBnk)
  Psi.mhp_shade = list(formula = ~machabprop + pctShade)
  Psi.lwd_pctBrBnk = list(formula = ~LWD + pctBrBnk)
  Psi.lwd_shade = list(formula = ~LWD + pctShade)
  Psi.BBnk_shade = list(formula = ~pctBrBnk+pctShade)
  #combination of effects
  #~pctex21+pctslow+machabprop+pctrock+LWD+pctBrBnk+pctShade+BRT_100m
  Psi.p21_hab = list(formula = ~pctex21+pctslow+machabprop+pctrock+LWD+pctBrBnk+pctShade)
  Psi.p21_hab2 = list(formula = ~pctex21+pctslow+machabprop+pctrock+LWD+pctBrBnk+BRT_100m)
  Psi.p21_hab3 = list(formula = ~pctex21+pctslow+machabprop+pctrock+LWD+pctShade+BRT_100m)
  Psi.p21_hab4 = list(formula = ~pctex21+pctslow+machabprop+pctrock+pctBrBnk+pctShade+BRT_100m)
  Psi.p21_hab5 = list(formula = ~pctex21+pctslow+machabprop+LWD+pctBrBnk+pctShade+BRT_100m)
  Psi.pct21_MHP = list(formula = ~pctex21 + machabprop) 
  Psi.pct21_lwd = list(formula = ~pctex21 + LWD) 
  Psi.pct21_bare = list(formula = ~pctex21 + pctBrBnk) 
  Psi.pct21_shade = list(formula = ~pctex21 + pctShade) 
  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  Psi.pctex21 = list(formula=~pctex21) 
  Psi.slow = list(formula=~pctslow) 
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
summary(bkt.results$p.tv.effort.Psi.biology)
names(bkt.results)
round1.top <- bkt.results$p.tv.effort.Psi.pctex21
summary(round1.top, showall = F)
round1.top$results$real



#---------------------------------------------------------------------------------------------------#
#export MARK data with models
#export ch data to an .inp file
#str(brook2)
#export.chdata(brook.process, filename = "BrookOccu", covariates = "all")
#export.MARK(brook.process, "BKT_OccPrelim", model = bkt.results, ind.covariates = "all", replace = T)
#---------------------------------------------------------------------------------------------------#
