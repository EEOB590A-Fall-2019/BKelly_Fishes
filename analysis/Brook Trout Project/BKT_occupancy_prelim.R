##########################################################################################################################
#                                       Preliminary BKT Occupancy Analysis in RMark
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
library(tidyselect)
library(tidyverse)

##Occupancy example
?weta



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
  #> time varying effort (+)
  #> time varying large woody debris (+)
  #> time varying undercut bank (-)

#Occupancy Probability
# Local:
  #> DO (+)
  #> temp, avgT, sdT, MAXT, MEANT, RNGT, pctex21 (-)
  #> avwid, avdep, maxdep, sdDep, WidDep (- for width, + for depth)
  #> mFlow, sdFlow (null)
  #> pctslow, machabprop (+ for pct slow)
  #> pctrock, pctgrvl, pctcbbl, subprop (+)
  #> LWD, under, boulder, depool (+)
  #> pctVertbnk, pctBrBnk (-)
  #> pctShade, pctShadeSD (+)
  #> BRT_100m (-)
# Landscape:
  #>pctSlope (+)
  #>CatArea_km2 (-)
  #>HAiFLS_alt, HAiFLS_for, HAiFLS_ag (-, +, -)
##########################################################################################

#Process Data
?process.data
?make.design.data

brook.process = process.data(brook2, model="Occupancy", groups = "freq")
bkt.ddl = make.design.data(brook.process)



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
  p.maxdep = list(formula = ~maxdep)
  p.tv.mxdep = list(formula = ~mxdep)
  p.maxflow = list(formula = ~maxflow)
  p.tv.mxflow = list(formula = ~mxflow)
  p.LWD = list(formula = ~LWD)
  p.tv.lwd = list(formula = ~lwd)
  p.under = list(formula = ~under)
  p.tv.under = list(formula = ~undr)
  
  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  #---Local (instream and riparian) Scale Variables---
  #Temperature covariates
  Psi.Dot        = list(formula=~1)
  Psi.avgT       = list(formula=~avgT)
  Psi.MAXT  = list(formula=~MAXT)
  Psi.MEANT  = list(formula=~MEANT)
  Psi.RNGT = list(formula=~RNGT)
  Psi.pctex21 = list(formula=~pctex21)
  
  #Stream Dimensions
  Psi.depth = list(formula=~avdep)
  Psi.WD = list(formula=~WidDep)
  
  #Macrohabitat
  Psi.slow = list(formula=~pctslow)
  Psi.MacroProp = list(formula=~machabprop)
  
  #Substrate
  Psi.rock = list(formula=~pctrock)
  
  #Instream Complexity
  Psi.under = list(formula=~under)
  Psi.pool = list(formula=~depool)
  
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
  results=mark.wrapper(cml, data=brook.process, ddl=bkt.ddl)
  return(results)
}

bkt.models1 = run.occ()


##Examine model list and look at model comparisons
bkt.models1
##use AIC instead of AICc
AIC.table = bkt.models1
AIC.table$model.table = model.table(bkt.models1, use.AIC = T)
AIC.table

#look at summary of top model
names(bkt.models1)
round1.top <- bkt.models1$p.effort.Psi.pctex21
summary(round1.top, showall = F)





















