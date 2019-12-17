##########################################################################################################################
#                  Brown Trout Occupancy Analysis in RMark -- Information Criteria Model Selection
##########################################################################################################################

# We have data on Brown Trout occurence across 138 unique sites (stream segments of 1st-4th order), 
# in northeast Iowa (driftless area ecoregion)
# Each site had three spatial replicates or occasions
# Naive occurence of Brown Trout: 74/138 sites or ~53.6% of sites
# sites were randomly selected using GRTS site selection 


# We will use package RMark to run single-species single-season occupancy models with a suite of environmental covariates
# some covariates are site specific while others are occasion specific
# Covariates cover instream parameters, riparian parameters, and catchment level parameters

##########################################################################################################################

#install.packages("RMark")
library(RMark)
library(tidyverse)
library(skimr)
library(corrplot)

##Occupancy example
#?weta


##--------------------------------------------------------------------------------------------------------------------------------##
#read in data, rearrange and change some labels to work with grouping ("freq"), and time-varying covariates ("Effort1 --> Effort3")
brown <- read_csv("Data/Thesis/Tidy/BRT_occDF_RMARK.csv", col_names = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#inspect correlations between covariates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
psi.vars <- brown[,7:20]
psi.cv <- brown[,16:20]
psi.lv <- brown[,7:15]

#correlation test
ct <- cor(psi.vars) #all
head(round(ct,2)) 

ctc <- cor(psi.cv) #catchment
ctl <- cor(psi.lv) #local


#visualize these correlations
corrplot(ct, method = "number", type = "upper") #all
corrplot(ctl, method = "number", type = "upper") #local
corrplot(ctc, method = "number", type = "upper") #catchment

pairs(psi.vars) #pairs method of visualizing relationships

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Colinear variables to not include in the same model (> 0.6):
# Strong #
#--------#
# Local Scale: 
  # avgT and MEANT
  # MAXT and MEANT
  # pctrun and pctslow
#Catchment Scale:
  #HAiFLS_alt and HAiFLS_al2
  #Area_km2 and Cross_Cat
  #Area_km2 and Area2
#Both:
  # none >0.6
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Weak - user discretion #
#------------------------#
  #Area_km2 and avwid (0.53) #don't include... likely tell same exact story
  #avwid and avdep (0.50)
  #mFlow and pctslow (-0.48)

## Remove any variables based on better biological/ecological knowledge and hypotheses and above correlations ^^
brown <- brown %>%
  select(-avgT) %>%
  mutate(MEANT2 = (MEANT^2), MAXT2 = (MAXT^2), Area2 = (Area_km2^2)) %>%
  rename(pctpool = pctslow)

names(brown)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Variables of interest and Data Dictionary ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#-----#
#Detection Probability: probability of detecting Brown Trout at a site
#-----#
#> time varying effort (+) "amount (seconds) of time spent sampling (aka shocking)"

#-----#
#Occupancy Probability: probability of Brook Trout occurrence among sites
#-----#
# Local Scale: instream and immediate riparian area
#> MAXT & MAXT2(+,quad) "maximum daily maximum stream temperature"
#> MEANT & MEANT2 (+,quad) "maximum daily mean stream temperature"
#> avwid (+) "mean wetted width"
#> avdep (+) "mean depth"
#> mFlow (+) "mean flow velocity"
#> pctrun (-) "percentage of run habitats"
#> pctpool (+) "percentage of pool habitats"
#> pctBrBnk (-) "percentage of bank that is bare soil"
# Catchment Scale: within the upstream land area that drains to the outlet of the sampled segment
#> HAiFLS_alt and HAiFLS_al2 (+) "Hydrologically Active inserve flow length to the stream of altered LULC"
#> Area_km2 & Area2 (+,quad) "Catchment Area"
#> AvgSlope (-) "Mean Slope of the catchment"
#> Cross_Cat (-) "Road Crossing density of upstream catchment (count/Area_km2)"
##########################################################################################
#set wd to scratch folder because MARK outputs an insane amount of files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Analysis/Brook Trout Project/Occupancy/RMark/Brown Trout/brt_output") #because MARK loves output files
getwd()

#Process Data
#?process.data
#?make.design.data
brown.df <- as.data.frame(brown)
brown.process = process.data(brown.df, model="Occupancy", groups = "freq")
brt.ddl = make.design.data(brown.process)


################################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################################################################################################################################
#set wd to scratch folder because MARK outputs an insane amount of files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Analysis/Brook Trout Project/RMark/output") #because MARK loves output files

# Catchment Scale: 
#> HAiFLS_alt and HAiFLS_al2 (+) "Hydrologically Active inserve flow length to the stream of altered LULC"
#> Area_km2 & Area2 (+,quad) "Catchment Area"
#> AvgSlope (-) "Mean Slope of the catchment"
#> Cross_Cat (-) "Road Crossing density of upstream catchment (count/Area_km2)"

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####   Catchment Scale covariates   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

run.occ.cat=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.tv.effort = list(formula = ~effort)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global1 = list(formula = ~HAiFLS_alt+Area_km2+AvgSlope+Cross_Cat)
  Psi.global2 = list(formula = ~HAiFLS_al2+Area_km2+AvgSlope+Cross_Cat)
  Psi.global3 = list(formula = ~HAiFLS_alt+Area2+AvgSlope+Cross_Cat)
  Psi.global4 = list(formula = ~HAiFLS_al2+Area2+AvgSlope+Cross_Cat)
  #3 Covariates
  Psi.alt_area_slpe = list(formula = ~HAiFLS_alt+Area_km2+AvgSlope)
  Psi.alt_area_cross = list(formula = ~HAiFLS_alt+Area_km2+Cross_Cat)
  Psi.alt_slpe_cross = list(formula = ~HAiFLS_alt+AvgSlope+Cross_Cat)
  Psi.area_slpe_crs = list(formula = ~Area_km2+AvgSlope+Cross_Cat)
  
  Psi.al2_area_slpe = list(formula = ~HAiFLS_al2+Area_km2+AvgSlope)
  Psi.al2_area_cross = list(formula = ~HAiFLS_al2+Area_km2+Cross_Cat)
  Psi.al2_slpe_cross = list(formula = ~HAiFLS_al2+AvgSlope+Cross_Cat)
  Psi.ar2_slpe_crs = list(formula = ~Area2+AvgSlope+Cross_Cat)
  Psi.alt_ar2_slpe = list(formula = ~HAiFLS_alt+Area2+AvgSlope)
  Psi.alt_ar2_cross = list(formula = ~HAiFLS_alt+Area2+Cross_Cat)
  Psi.al2_ar2_slpe = list(formula = ~HAiFLS_al2+Area2+AvgSlope)
  Psi.al2_ar2_cross = list(formula = ~HAiFLS_al2+Area2+Cross_Cat)
  #2 covariates
  Psi.alt_area = list(formula = ~HAiFLS_alt+Area_km2)
  Psi.alt_slpe = list(formula = ~HAiFLS_alt+AvgSlope)
  Psi.alt_cross = list(formula = ~HAiFLS_alt+Cross_Cat)
  Psi.area_slpe = list(formula = ~Area_km2+AvgSlope)
  Psi.area_cross = list(formula = ~Area_km2+Cross_Cat)
  Psi.slpe_cross = list(formula = ~AvgSlope+Cross_Cat)
  
  Psi.al2_area = list(formula = ~HAiFLS_al2+Area_km2)
  Psi.al2_slpe = list(formula = ~HAiFLS_al2+AvgSlope)
  Psi.al2_cross = list(formula = ~HAiFLS_al2+Cross_Cat)
  
  Psi.alt_ar2 = list(formula = ~HAiFLS_alt+Area2)
  Psi.ar2_slpe = list(formula = ~Area2+AvgSlope)
  Psi.ar2_cross = list(formula = ~Area2+Cross_Cat)
  
  Psi.al2_ar2 = list(formula = ~HAiFLS_al2+Area2)
  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  Psi.alt = list(formula = ~HAiFLS_alt)
  Psi.al2 = list(formula = ~HAiFLS_al2)
  Psi.area = list(formula = ~Area_km2)
  Psi.ar2 = list(formula = ~Area2)
  Psi.slope = list(formula = ~AvgSlope)
  Psi.cross = list(formula = ~Cross_Cat)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.cat=create.model.list("Occupancy")
  results.cat=mark.wrapper(cml.cat, data=brown.process, ddl=brt.ddl, output=F)
  return(results.cat)
}

brt.results.cat = run.occ.cat()


##Examine model list and look at model comparisons
brt.results.cat
##Model Table
AICc.Table = model.table(brt.results.cat, use.lnl = T)
AICc.Table

#look at summary of top model(s) (delta AICc < 2)
summary(brt.results.cat$p.tv.effort.Psi.al2_area_cross) #top
summary(brt.results.cat$p.tv.effort.Psi.al2_area) #2nd

brt.results.cat$p.tv.effort.Psi.al2_area_cross$results$real #top
brt.results.cat$p.tv.effort.Psi.al2_area$results$real #2nd

tm.cat <- brt.results.cat$p.tv.effort.Psi.al2_area_cross
tm2.cat <- brt.results.cat$p.tv.effort.Psi.al2_area


cleanup(ask = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing HAiFLS_al2 effect on psi ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
brt.ddl #par.index = 1, model.index = 4

#covariate.predictions method
#HAiFLS_al2
min.al2 <- min(brown.df$HAiFLS_al2)
max.al2 <- max(brown.df$HAiFLS_al2)
mean.al2 <- mean(brown.df$HAiFLS_al2)
al2.values <- seq(from = min.al2, to = max.al2, length = 100)
#Area_km2
min.area <- min(brown.df$Area_km2)
max.area <- max(brown.df$Area_km2)
mean.area <- mean(brown.df$Area_km2)
area.values <- seq(from = min.area, to = max.area, length = 100)
#Cross_Cat
min.cross <- min(brown.df$Cross_Cat)
max.cross <- max(brown.df$Cross_Cat)
mean.cross <- mean(brown.df$Cross_Cat)
cross.values <- seq(from = min.cross, to = max.cross, length = 100)

#####################################################
#predict across range of observed values (HAiFLS_al2)
#####################################################

#predictions of Psi for full range of p21 & -1SD of forest values (would be negative so just forest=0)
predictions_al2 <- covariate.predictions(tm.cat, 
                                         data = data.frame(HAiFLS_al2 = al2.values,
                                                           Area_km2 = mean.area,
                                                           Cross_Cat = mean.cross),
                                         indices = 4)

predictions_al2$estimates

cat.al2.preds <- predictions_al2$estimates

ggplot(data=cat.al2.preds, aes(x=HAiFLS_al2))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="red")+
  labs(x="% HAiFLS Altered Land Cover ^2",
       y="Occupancy Probability (Psi)")+
  theme_classic()+
  theme(axis.title = element_text(face = "bold"))

############## STOPPED HERE 12/17/2019 @ 5:40pm

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(catch.mod.predictions, "Data/Thesis/Tidy/BKT_Catchment_Model_Predictions.csv")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing effort effect on p   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
min.effort <- min(brook.df$effort1)
max.effort <- max(brook.df$effort1)
effort.values <- seq(min.effort, max.effort, length.out = 100)
mean.effort <- mean(brook.df$effort1) #906.231

#predictions of p for full range of effort1 values
p.pred.eff1 <- covariate.predictions(tm.cat, 
                                     data = data.frame(effort1 = effort.values),
                                     indices = 1)

p.pred.eff1$estimates


P.predictions.eff1 <- p.pred.eff1$estimates %>%
  select(covdata, estimate, se, lcl, ucl) %>%
  rename(Effort_sec = covdata) %>%
  round(digits = 4)

####################################################
##       Write tidy csv for P predictions         ## 
####################################################
write_csv(P.predictions.eff1, "Data/Thesis/Tidy/BKT_CatchMod_DProb_predictions.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              ###~~~~~~~~~~~~~~~~~~~~~~~##
                              ####   All covariates   ####
                              ##~~~~~~~~~~~~~~~~~~~~~~~##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
  Psi.biol = list(formula = ~pctex21+pctpool+pctrock+BRT_100m)
  Psi.p21_pool_rck = list(formula = ~pctex21+pctpool+pctrock)
  Psi.p21_pool_brt = list(formula = ~pctex21+pctpool+BRT_100m)
  Psi.p21_rck_brt = list(formula = ~pctex21+pctrock+BRT_100m)
  Psi.pct21_pool = list(formula = ~pctex21 + pctpool) 
  Psi.pct21_rock = list(formula = ~pctex21 + pctrock) 
  Psi.pct21_brt = list(formula = ~pctex21 + BRT_100m)
  #indirect effects - hypothesized smaller effects (features of high quality stream habitats (for Brook Trout))
  Psi.indEff = list(formula = ~pctBrBnk+pctShade+HAiFLS_for)
  Psi.BBnk_shade = list(formula = ~pctBrBnk+pctShade)
  Psi.BBnk_for = list(formula = ~pctBrBnk+HAiFLS_for)
  Psi.shade_for = list(formula = ~pctShade+HAiFLS_for)
  #combination of effects
  #all covariates
  Psi.global = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  #6 covariates
  Psi.mixed1 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctBrBnk+pctShade)
  Psi.mixed2 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctBrBnk+HAiFLS_for)
  Psi.mixed3 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed4 = list(formula = ~pctex21+pctpool+pctrock+pctBrBnk+pctShade+HAiFLS_for)
  Psi.mixed5 = list(formula = ~pctex21+pctpool+BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  Psi.mixed6 = list(formula = ~pctex21+pctrock+BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  Psi.mixed7 = list(formula = ~pctpool+pctrock+BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  #5 covariates
  Psi.mixed8 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctBrBnk)
  Psi.mixed9 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctShade)
  Psi.mixed10 = list(formula = ~pctex21+pctpool+pctrock+pctShade+pctBrBnk)
  Psi.mixed11 = list(formula = ~pctex21+pctpool+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed12 = list(formula = ~pctex21+pctrock+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed13 = list(formula = ~pctpool+pctrock+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed14 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+HAiFLS_for)
  Psi.mixed15 = list(formula = ~pctex21+pctpool+pctrock+pctShade+HAiFLS_for)
  Psi.mixed16 = list(formula = ~pctex21+pctpool+pctShade+pctBrBnk+HAiFLS_for)
  Psi.mixed17 = list(formula = ~pctex21+BRT_100m+pctShade+pctBrBnk+HAiFLS_for)
  Psi.mixed18 = list(formula = ~pctrock+BRT_100m+pctShade+pctBrBnk+HAiFLS_for)
  Psi.mixed19 = list(formula = ~pctpool+pctrock+BRT_100m+pctBrBnk+HAiFLS_for)
  Psi.mixed20 = list(formula = ~pctex21+pctrock+BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed21 = list(formula = ~pctpool+pctrock+BRT_100m+pctBrBnk+HAiFLS_for)
  Psi.mixed22 = list(formula = ~pctex21+pctpool+pctrock+pctBrBnk+HAiFLS_for)
  Psi.mixed23 = list(formula = ~pctex21+pctpool+BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed24 = list(formula = ~pctex21+pctrock+pctBrBnk+pctShade+HAiFLS_for)
  Psi.mixed25 = list(formula = ~pctpool+BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  Psi.mixed26 = list(formula = ~pctex21+pctpool+BRT_100m+pctBrBnk+HAiFLS_for)
  Psi.mixed27 = list(formula = ~pctex21+pctrock+BRT_100m+pctBrBnk+HAiFLS_for)
  Psi.mixed28 = list(formula = ~pctpool+pctrock+pctBrBnk+pctShade+HAiFLS_for)
  Psi.mixed29 = list(formula = ~pctpool+pctrock+BRT_100m+pctShade+HAiFLS_for)
  #4 covariates
  Psi.mixed30 = list(formula = ~pctex21+pctpool+pctShade+pctBrBnk)
  Psi.mixed31 = list(formula = ~pctex21+pctrock+pctShade+pctBrBnk)
  Psi.mixed32 = list(formula = ~pctex21+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed33 = list(formula = ~pctrock+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed34 = list(formula = ~pctpool+pctrock+pctShade+pctBrBnk)
  Psi.mixed35 = list(formula = ~pctpool+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed36 = list(formula = ~pctex21+pctpool+pctrock+HAiFLS_for)
  Psi.mixed37 = list(formula = ~pctex21+pctpool+pctShade+HAiFLS_for)
  Psi.mixed38 = list(formula = ~pctex21+pctBrBnk+pctShade+HAiFLS_for)
  Psi.mixed39 = list(formula = ~BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  Psi.mixed40 = list(formula = ~pctex21+pctpool+BRT_100m+HAiFLS_for)
  Psi.mixed41 = list(formula = ~pctpool+pctrock+BRT_100m+HAiFLS_for)
  Psi.mixed42 = list(formula = ~pctex21+pctrock+BRT_100m+HAiFLS_for)
  Psi.mixed43 = list(formula = ~pctex21+pctpool+pctBrBnk+HAiFLS_for)
  Psi.mixed44 = list(formula = ~pctex21+pctrock+pctBrBnk+HAiFLS_for)
  Psi.mixed45 = list(formula = ~pctpool+pctrock+pctBrBnk+HAiFLS_for)
  Psi.mixed46 = list(formula = ~pctex21+pctrock+pctBrBnk+HAiFLS_for)
  Psi.mixed47 = list(formula = ~pctex21+pctrock+pctShade+HAiFLS_for)
  Psi.mixed48 = list(formula = ~pctpool+pctrock+pctShade+HAiFLS_for)
  Psi.mixed49 = list(formula = ~pctpool+BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed50 = list(formula = ~pctrock+BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed51 = list(formula = ~pctrock+pctBrBnk+pctShade+HAiFLS_for)
  Psi.mixed52 = list(formula = ~pctpool+pctBrBnk+pctShade+HAiFLS_for)
  Psi.mixed53 = list(formula = ~pctpool+BRT_100m+pctBrBnk+HAiFLS_for)
  Psi.mixed54 = list(formula = ~pctpool+BRT_100m+pctShade+HAiFLS_for)
  #3 covariates
  Psi.mixed55 = list(formula = ~pctex21+pctShade+pctBrBnk)
  Psi.mixed56 = list(formula = ~pctpool+pctShade+pctBrBnk)
  Psi.mixed57 = list(formula = ~pctrock+pctShade+pctBrBnk)
  Psi.mixed58 = list(formula = ~BRT_100m+pctShade+pctBrBnk)
  Psi.mixed59 = list(formula = ~pctex21+pctpool+pctShade)
  Psi.mixed60 = list(formula = ~pctpool+pctrock+pctShade)
  Psi.mixed61 = list(formula = ~pctrock+BRT_100m+pctShade)
  Psi.mixed62 = list(formula = ~pctex21+BRT_100m+pctShade)
  Psi.mixed63 = list(formula = ~pctex21+pctrock+pctShade)
  Psi.mixed64 = list(formula = ~pctpool+BRT_100m+pctShade)
  Psi.mixed65 = list(formula = ~pctex21+pctpool+pctBrBnk)
  Psi.mixed66 = list(formula = ~pctpool+pctrock+pctBrBnk)
  Psi.mixed67 = list(formula = ~pctrock+BRT_100m+pctBrBnk)
  Psi.mixed68 = list(formula = ~pctex21+BRT_100m+pctBrBnk)
  Psi.mixed69 = list(formula = ~pctex21+pctrock+pctBrBnk)
  Psi.mixed70 = list(formula = ~pctpool+BRT_100m+pctBrBnk)
  Psi.mixed71 = list(formula = ~pctex21+pctShade+HAiFLS_for)
  Psi.mixed72 = list(formula = ~pctpool+pctShade+HAiFLS_for)
  Psi.mixed73 = list(formula = ~pctrock+pctShade+HAiFLS_for)
  Psi.mixed74 = list(formula = ~BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed75 = list(formula = ~pctex21+pctBrBnk+HAiFLS_for)
  Psi.mixed76 = list(formula = ~pctpool+pctBrBnk+HAiFLS_for)
  Psi.mixed77 = list(formula = ~pctrock+pctBrBnk+HAiFLS_for)
  Psi.mixed78 = list(formula = ~BRT_100m+pctBrBnk+HAiFLS_for)
  Psi.mixed79 = list(formula = ~pctex21+pctpool+HAiFLS_for)
  Psi.mixed80 = list(formula = ~pctpool+pctrock+HAiFLS_for)
  Psi.mixed81 = list(formula = ~pctrock+BRT_100m+HAiFLS_for)
  Psi.mixed82 = list(formula = ~pctex21+BRT_100m+HAiFLS_for)
  Psi.mixed83 = list(formula = ~pctex21+pctrock+HAiFLS_for)
  Psi.mixed84 = list(formula = ~pctpool+BRT_100m+HAiFLS_for)
  #2 covariates
  Psi.pct21_bare = list(formula = ~pctex21 + pctBrBnk) 
  Psi.pct21_shade = list(formula = ~pctex21 + pctShade)
  Psi.pool_bare = list(formula = ~pctpool + pctBrBnk) 
  Psi.pool_shade = list(formula = ~pctpool + pctShade) 
  Psi.rock_bare = list(formula = ~pctrock + pctBrBnk) 
  Psi.rock_shade = list(formula = ~pctrock + pctShade) 
  Psi.BRT_bare = list(formula = ~BRT_100m + pctBrBnk) 
  Psi.BRT_shade = list(formula = ~BRT_100m + pctShade) 
  Psi.pct21_for = list(formula = ~pctex21 + HAiFLS_for)
  Psi.pool_for = list(formula = ~pctpool + HAiFLS_for)
  Psi.rock_for = list(formula = ~pctrock + HAiFLS_for) 
  Psi.BRT_for = list(formula = ~BRT_100m + HAiFLS_for) 
  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  Psi.pctex21 = list(formula=~pctex21) 
  Psi.pool = list(formula=~pctpool) 
  Psi.rock = list(formula=~pctrock) 
  Psi.bare = list(formula=~pctBrBnk) 
  Psi.shade = list(formula=~pctShade) 
  Psi.brt = list(formula=~BRT_100m)
  Psi.forest = list(formula = ~HAiFLS_for)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml=create.model.list("Occupancy")
  results=mark.wrapper(cml, data=brook.process, ddl=bkt.ddl, output=F)
  return(results)
}

bkt.results = run.occ()


##Examine model list and look at model comparisons
bkt.results
##Model Table
AICc.Table = model.table(bkt.results, use.lnl = T)
AICc.Table

#look at summary of top model(s)
summary(bkt.results$p.tv.effort.Psi.pct21_for)
summary(bkt.results$p.tv.effort.Psi.forest)
summary(bkt.results$p.tv.effort.Psi.mixed82)
bkt.results$p.tv.effort.Psi.pct21_for$results$real
top.mod <- bkt.results$p.tv.effort.Psi.pct21_for


cleanup(ask = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing pctex21 effect on psi ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

#covariate.predictions method
min.temp <- min(brook.df$pctex21)
max.temp <- max(brook.df$pctex21)
temp.values <- seq(from = min.temp, to = max.temp, length = 100)
min.for <- min(brook.df$HAiFLS_for)
max.for <- max(brook.df$HAiFLS_for)
for.values <- seq(from = min.for, to = max.for, length = 100)

bkt.ddl #par.index = 1, model.index = 4

##################################################
#predict while holding one value constant (forest)
##################################################
for.mean <- rep(mean(brook.df$HAiFLS_for), 100)
for.minus <- rep(0, 100)
for.plus <- rep(mean(brook.df$HAiFLS_for)+sd(brook.df$HAiFLS_for), 100)

#predictions of Psi for full range of p21 & -1SD of forest values (would be negative so just forest=0)
p21.pred.minus <- covariate.predictions(top.mod, 
                                        data = data.frame(pctex21 = temp.values,
                                                          HAiFLS_for = for.minus),
                                        indices = 4)

p21.pred.minus$estimates

#predictions of Psi for full range of p21 & mean of forest values 
p21.pred.mean <- covariate.predictions(top.mod, 
                                       data = data.frame(pctex21 = temp.values,
                                                         HAiFLS_for = for.mean),
                                       indices = 4)

p21.pred.mean$estimates

#predictions of Psi for full range of p21 & +1SD of forest values 
p21.pred.plus <- covariate.predictions(top.mod, 
                                       data = data.frame(pctex21 = temp.values,
                                                         HAiFLS_for = for.plus),
                                       indices = 4)

p21.pred.plus$estimates

Psi.Predictions.P21 <- rbind(p21.pred.minus$estimates, p21.pred.mean$estimates, p21.pred.plus$estimates)%>%
  select(pctex21, HAiFLS_for, estimate, se, lcl, ucl)%>%
  round(digits = 4)

##########################################################
#predict while holding one value constant (temp this time)
##########################################################
temp.mean <- rep(mean(brook.df$pctex21), 100)
temp.minus <- rep(0, 100)
temp.plus <- rep(mean(brook.df$pctex21)+sd(brook.df$pctex21), 100)

#predictions of Psi for full range of HAiFLS_for & -1SD of pctex21 values (would be negative so just =0)
for.pred.minus <- covariate.predictions(top.mod, 
                                        data = data.frame(pctex21 = temp.minus,
                                                          HAiFLS_for = for.values),
                                        indices = 4)

for.pred.minus$estimates

#predictions of Psi for full range of HAiFLS_for & mean of pctex21 values 
for.pred.mean <- covariate.predictions(top.mod, 
                                       data = data.frame(pctex21 = temp.mean,
                                                         HAiFLS_for = for.values),
                                       indices = 4)

for.pred.mean$estimates

#predictions of Psi for full range of HAiFLS_for & +1SD of pctex21 values
for.pred.plus <- covariate.predictions(top.mod, 
                                       data = data.frame(pctex21 = temp.plus,
                                                         HAiFLS_for = for.values),
                                       indices = 4)

for.pred.plus$estimates

Psi.Predictions.for <- rbind(for.pred.minus$estimates, for.pred.mean$estimates, for.pred.plus$estimates)%>%
  select(pctex21, HAiFLS_for, estimate, se, lcl, ucl)%>%
  round(digits = 4)

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(Psi.Predictions.P21, "Data/Thesis/Tidy/Psi_predictions_pctex21.csv")
write_csv(Psi.Predictions.for, "Data/Thesis/Tidy/Psi_predictions_HAiFLS_for.csv")




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing effort effect on p   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
min.effort <- min(brook.df$effort1)
max.effort <- max(brook.df$effort1)
effort.values <- seq(min.effort, max.effort, length.out = 100)
mean.effort <- mean(brook.df$effort1) #906.231

#predictions of p for full range of effort1 values
p.pred.effort <- covariate.predictions(top.mod, 
                                       data = data.frame(effort1 = effort.values),
                                       indices = 1)

p.pred.effort$estimates


P.predictions.effort <- p.pred.effort$estimates %>%
  select(covdata, estimate, se, lcl, ucl) %>%
  rename(Effort_sec = covdata) %>%
  round(digits = 4)

####################################################
##       Write tidy csv for P predictions         ## 
####################################################
write_csv(P.predictions.effort, "Data/Thesis/Tidy/P_predictions_effort.csv")

