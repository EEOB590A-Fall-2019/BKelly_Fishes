##########################################################################################################################
#                           Preliminary BKT Occupancy Analysis in RMark -- Catchment Scale Only
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

##Occupancy example
#?weta



##--------------------------------------------------------------------------------------------------------------------------------##
#read in data, rearrange and change some labels to work with grouping ("freq"), and time-varying covariates ("Effort1 --> Effort3")
brook <- read_csv("Data/Thesis/Tidy/BKT_occDF_RMARK.csv", col_names = T)
names(brook)
brook$freq <- 1
brook <- brook[,c(1,74,2:73)]
names(brook)

brook2 <- brook %>%
  rename(effort1 = t1_eff, effort2 = t2_eff, effort3 = t3_eff, undr1 = t1_under, undr2=t2_under, undr3=t3_under, mxdep1 = t1_maxdep,
         mxdep2 = t2_maxdep, mxdep3 = t3_maxdep, mxflow1=t1_maxflow, mxflow2=t2_maxflow, mxflow3=t3_maxflow,
         lwd1=t1_LWD, lwd2=t2_LWD, lwd3=t3_LWD)%>%
  select(-temp,-avwid,-sdwid,-t1_avwid, -t2_avwid, -t3_avwid, -t1_avdep, -t2_avdep, -t3_avdep,
         -mFlow, -sdFlow, -t1_flow, -t2_flow, -t3_flow, -pctEmb1, -boulder, -Avbnka, -pctShadeSD)


##~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Variables of interest ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Detection Probability:
#> time varying effort (+) and site level effort (+)
#> time varying large woody debris (-) and site level LWD (-)
#> time varying undercut bank (-) and site level undercut banks (-)
#> time varying max depth (-)
#> time varying max flow (-)

#Occupancy Probability
# Landscape:
#>pctSlope (+)
#>CatArea_km2 (-)
#>HAiFLS_alt, HAiFLS_for, HAiFLS_ag (-, +, -)
##########################################################################################
getwd()
#set wd to scratch folder because MARK outputs a stupid amount of files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Analysis/Brook Trout Project/MARK/BrookTrout_ScratchFolder") #because MARK loves output files


#Process Data
?process.data
?make.design.data

brook.process = process.data(brook2, model="Occupancy", groups = "freq")
bkt.ddl = make.design.data(brook.process)

#export ch data to an .inp file
str(brook2)
export.chdata(brook.process, filename = "BrookOccu", covariates = "all")

###~~~~~~~~~~~~~##
#### Round 1 ####
##~~~~~~~~~~~~~##
run.occ=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  p.effort = list(formula= ~effsec) 
  p.tv.effort = list(formula = ~effort)
  p.tv.mxdep = list(formula = ~mxdep)
  p.tv.mxflow = list(formula = ~mxflow)
  p.LWD = list(formula = ~LWD)
  p.tv.lwd = list(formula = ~lwd)
  p.under = list(formula = ~under)
  p.tv.under = list(formula = ~undr)
  
  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  #---Local (instream and riparian) Scale Variables---
  Psi.Dot        = list(formula=~1)
  
  #Temperature covariates
  Psi.pctex21 = list(formula=~pctex21)
  #Psi.avgT       = list(formula=~avgT)
  #Psi.MAXT  = list(formula=~MAXT)
  #Psi.MEANT  = list(formula=~MEANT)
  #Psi.RNGT = list(formula=~RNGT)
  
  #Stream Dimensions
  Psi.depth = list(formula=~avdep)
  Psi.WD = list(formula=~WidDep)
  
  #Macrohabitat
  Psi.slow = list(formula=~pctslow)
  Psi.MacroProp = list(formula=~machabprop)
  
  #Substrate
  Psi.rock = list(formula=~pctrock)
  
  #Instream Complexity
  Psi.under = list(formula=~under) #also covariate on p
  Psi.pool = list(formula=~depool)
  Psi.lwd = list(formula = ~LWD) #also covariate on p
  
  #Bank Condition
  Psi.Vertical = list(formula=~pctVertbnk)
  Psi.bare = list(formula=~pctBrBnk)
  Psi.shade = list(formula=~pctShade)
  
  #Fish Covariates -- only interested in trout density
  Psi.brt = list(formula=~BRT_100m)
  
  #---Catchment Scale Variables---
  #Landcover
  Psi.HAiFLS.alt = list(formula=~HAiFLS_alt)
  Psi.HAiFLS.nat = list(formula=~HAiFLS_nat)
  
  #Catchment Attributes
  Psi.cat = list(formula = ~CatArea_km2)
  
  #Stream Segment Attributes
  Psi.slope = list(formula = ~pctSlope)
  Psi.sinu = list(formula = ~Sinuosity)
  
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml=create.model.list("Occupancy")
  results=mark.wrapper(cml, data=brook.process, ddl=bkt.ddl, output=F)
  return(results)
}

bkt.results = run.occ()

#export MARK data with models
export.MARK(brook.process, "BKT_OccPrelim", model = bkt.results, ind.covariates = "all", replace = T)

##Examine model list and look at model comparisons
bkt.results
##use AIC instead of AICc
AIC.table = bkt.results
AIC.table

#look at summary of top model
names(bkt.results)
round1.top <- bkt.results$p.tv.effort.Psi.pctex21
summary(round1.top, showall = F)
round1.top$results$real

#                 model          npar AICc     DeltaAICc   weight     Deviance
#63     p(~effort)Psi(~pctex21)    4 154.9750  0.000000 4.255411e-01 146.67423
#99      p(~mxdep)Psi(~pctex21)    4 155.7266  0.751620 2.922331e-01 147.42585
#27     p(~effsec)Psi(~pctex21)    4 157.1714  2.196430 1.419034e-01 148.87066
#9           p(~1)Psi(~pctex21)    3 159.6379  4.662933 4.134268e-02 153.45881
#135      p(~undr)Psi(~pctex21)    4 160.2502  5.275180 3.044043e-02 151.94941
#153     p(~under)Psi(~pctex21)    4 161.2186  6.243660 1.875634e-02 152.91789
#81        p(~lwd)Psi(~pctex21)    4 161.2767  6.301740 1.821949e-02 152.97597
#45        p(~LWD)Psi(~pctex21)    4 161.6283  6.653340 1.528224e-02 153.32757
#117    p(~mxflow)Psi(~pctex21)    4 161.6930  6.717990 1.479614e-02 153.39222