##########################################################################################################################
#                                       Preliminary BKT Occupancy Analysis in unmarked
##########################################################################################################################

# We have data on Brook Trout occurence across 138 unique sites (stream segments of 1st-4th order), 
# in northeast Iowa (driftless area ecoregion)
# Each site had three spatial replicates or occasions
# Naive occurence of Brook Trout: 19/138 sites or ~13.8% of sites
# sites were randomly selected using GRTS site selection 


# We will use package unmarked to run single-species single-season occupancy models with a suite of environmental covariates
# some covariates are site specific while others are occasion specific
# Covariates cover instream parameters, riparian parameters, and catchment level parameters

##########################################################################################################################

#install.packages("RMark")
library(unmarked)
library(tidyverse)
getwd()

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
# Local:
#> DO (+)
#> temp, avgT, sdT, MAXT, MEANT, RNGT, pctex21 (-)
#> avdep, WidDep (- for widDep, + for depth)
#> pctslow, machabprop (+ for pct slow)
#> pctrock (+)
#> LWD, depool (+)
#> pctVertbnk, pctBrBnk (-)
#> pctShade (+)
#> BRT_100m (-)
# Landscape:
#>pctSlope (+)
#>CatArea_km2 (-)
#>HAiFLS_alt, HAiFLS_for, HAiFLS_ag (-, +, -)
##########################################################################################