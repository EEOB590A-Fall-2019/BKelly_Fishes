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

##--------------------------------------------------------------------------------------------------------------------------------##
#read in data, rearrange and change some labels to work with grouping ("freq"), and time-varying covariates ("Effort1 --> Effort3")
brook <- read_csv("Data/Thesis/Tidy/BKT_occDF_RMARK.csv", col_names = T)
names(brook)
brook$freq <- 1
brook <- brook[,c(1,74,2:73)]
names(brook)

brook <- brook %>%
  rename(effort1 = t1_eff, effort2 = t2_eff, effort3 = t3_eff, under1 = t1_under, under2=t2_under, under3=t3_under, maxdep1 = t1_maxdep,
         maxdep2 = t2_maxdep, maxdep3 = t3_maxdep, avdep1 = t1_avdep, avdep2 = t2_avdep, avdep3 = t3_avdep)%>%
  select(-t1_avwid, -t2_avwid, -t3_avwid, -t1_LWD, -t2_LWD, -t3_LWD)


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
brook.process = process.data(brook, model="Occupancy")
bkt.ddl = make.design.data(brook.process)



###~~~~~~~~~~~~~##
#### Round 1 ####
##~~~~~~~~~~~~~##

run.occ=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1)
  Psi.avgT       = list(formula=~avgT)
  Psi.MAXT  = list(formula=~MAXT)
  Psi.MEANT  = list(formula=~MEANT)
  Psi.RNGT = list(formula=~RNGT)
  Psi.pctex21 = list(formula=~pctex21)
  
  #Psi.depth = list(formula=~avdep)
  #Psi.maxdep = list(formula=~maxdep)
  #Psi.WD = list(formula=~WidDep)
  
  #Psi.slow = list(formula=~pctslow)
  #Psi.MacroProp = list(formula=~machabprop)
  
  #Psi.rock = list(formula=~pctrock)
  #Psi.gravel = list(formula=~pctgrvl)
  
  #Psi.under = list(formula=~under)
  #Psi.boulder = list(formula=~boulder)
  #Psi.pool = list(formula=~depool)
  
  #Psi.Vertical = list(formula=~pctVertbnk)
  #Psi.bare = list(formula=~pctBrBnk)
  
  #Psi.shade = list(formula=~pctShade)
  
  #Psi.brt = list(formula=~BRT_100m)
  
  #Psi.cat = list(formula=~CatArea_km2)
  #Psi.HAiFLS.alt = list(formula=~HAiFLS_alt)
  #Psi.HAiFLS.for = list(formula=~HAiFLS_for)
  #~~~~~~~~~~~ Detection Probability ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p.Dot     = list(formula=~1)
  p.effort    = list(formula=~effsec) 
  p.tv.effort = list(formula = ~ effort)
 
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml=create.model.list("Occupancy")
  results=mark.wrapper(cml, data=brook.process, ddl=bkt.ddl, adjust=F, output=F, threads=-1)
  return(results)
}

occ.results=run.occ()
occ.results.initial <- occ.results


