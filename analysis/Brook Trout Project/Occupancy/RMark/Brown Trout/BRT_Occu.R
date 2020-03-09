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
library(cowplot)

##Occupancy example
#?weta

setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
##--------------------------------------------------------------------------------------------------------------------------------##
#read in data, rearrange and change some labels to work with grouping ("freq"), and time-varying covariates ("Effort1 --> Effort3")
brown <- read_csv("Data/Thesis/Tidy/BRT_RMark.csv", col_names = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#inspect correlations between covariates
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
psi.vars <- brown[,7:22]
psi.cv <- brown[,16:22]
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
  #HAiFLS_nat and HAiFLS_alt and HAiFLS_al2
  #HAiFLS_for and HAiFLS_alt and HAiFLS_al2 and HAiFLS_nat
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
  mutate(Area2 = (Area_km2^2)) %>%
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
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Analysis/Brook Trout Project/Occupancy/RMark/Brown Trout") #because MARK loves output files
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
# Catchment Scale: 
#> HAiFLS_alt and HAiFLS_al2 (+) "Hydrologically Active inserve flow length to the stream of altered LULC"
#> Area_km2 & Area2 (+,quad) "Catchment Area"
#> AvgSlope (-) "Mean Slope of the catchment"
#> Cross_Cat (-) "Road Crossing density of upstream catchment (count/Area_km2)"

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####   Catchment Scale covariates   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

#Compare dot model vs effort for d-prob

run.occ.catc=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.tv.effort = list(formula = ~effort)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #Full Psi models
  Psi.global1 = list(formula = ~HAiFLS_alt+Area_km2+AvgSlope+Cross_Cat)
  #Psi.global2 = list(formula = ~HAiFLS_alt+Area2+AvgSlope+Cross_Cat)
  #Psi.global3 = list(formula = ~HAiFLS_for+Area_km2+AvgSlope+Cross_Cat)
  #Psi.global4 = list(formula = ~HAiFLS_for+Area2+AvgSlope+Cross_Cat)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.catc=create.model.list("Occupancy")
  results.catc=mark.wrapper(cml.catc, data=brown.process, ddl=brt.ddl, output=F)
  return(results.catc)
}

brt.results.catc = run.occ.catc()

##Examine model list and look at model comparisons -- effort versus dot model
brt.results.catc

##Model Table
AICc.Table.BTdot = model.table(brt.results.catc, use.lnl = T)
AICc.Table.BTdot

#continue with effort on p

run.occ.catEFF=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  #p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.tv.effort = list(formula = ~effort)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global1 = list(formula = ~HAiFLS_alt+Area_km2+AvgSlope+Cross_Cat)
  Psi.global3 = list(formula = ~HAiFLS_alt+Area2+AvgSlope+Cross_Cat)
  Psi.global5 = list(formula = ~HAiFLS_for+Area_km2+AvgSlope+Cross_Cat)
  Psi.global7 = list(formula = ~HAiFLS_for+Area2+AvgSlope+Cross_Cat)
  #3 Covariates
  Psi.alt_area_slpe = list(formula = ~HAiFLS_alt+Area_km2+AvgSlope)
  Psi.alt_area_cross = list(formula = ~HAiFLS_alt+Area_km2+Cross_Cat)
  Psi.alt_slpe_cross = list(formula = ~HAiFLS_alt+AvgSlope+Cross_Cat)
  Psi.area_slpe_crs = list(formula = ~Area_km2+AvgSlope+Cross_Cat)
  Psi.for_area_slpe = list(formula = ~HAiFLS_for+Area_km2+AvgSlope)
  Psi.for_area_cross = list(formula = ~HAiFLS_for+Area_km2+Cross_Cat)
  Psi.for_slpe_cross = list(formula = ~HAiFLS_for+AvgSlope+Cross_Cat)
  Psi.ar2_slpe_crs = list(formula = ~Area2+AvgSlope+Cross_Cat)
  Psi.alt_ar2_slpe = list(formula = ~HAiFLS_alt+Area2+AvgSlope)
  Psi.alt_ar2_cross = list(formula = ~HAiFLS_alt+Area2+Cross_Cat)
  Psi.for_ar2_slpe = list(formula = ~HAiFLS_for+Area2+AvgSlope)
  #2 covariates
  Psi.alt_area = list(formula = ~HAiFLS_alt+Area_km2)
  Psi.alt_slpe = list(formula = ~HAiFLS_alt+AvgSlope)
  Psi.alt_cross = list(formula = ~HAiFLS_alt+Cross_Cat)
  Psi.area_slpe = list(formula = ~Area_km2+AvgSlope)
  Psi.area_cross = list(formula = ~Area_km2+Cross_Cat)
  Psi.slpe_cross = list(formula = ~AvgSlope+Cross_Cat)
  Psi.for_area = list(formula = ~HAiFLS_for+Area_km2)
  Psi.for_slpe = list(formula = ~HAiFLS_for+AvgSlope)
  Psi.for_cross = list(formula = ~HAiFLS_for+Cross_Cat)
  Psi.alt_ar2 = list(formula = ~HAiFLS_alt+Area2)
  Psi.ar2_slpe = list(formula = ~Area2+AvgSlope)
  Psi.ar2_cross = list(formula = ~Area2+Cross_Cat)
  Psi.for_ar2 = list(formula = ~HAiFLS_for+Area2)

  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  Psi.alt = list(formula = ~HAiFLS_alt)
  Psi.for = list(formula = ~HAiFLS_for)
  Psi.area = list(formula = ~Area_km2)
  Psi.ar2 = list(formula = ~Area2)
  Psi.slope = list(formula = ~AvgSlope)
  Psi.cross = list(formula = ~Cross_Cat)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.catEFF=create.model.list("Occupancy")
  results.catEFF=mark.wrapper(cml.catEFF, data=brown.process, ddl=brt.ddl, output=F)
  return(results.catEFF)
}
brt.results.catEFF = run.occ.catEFF()


##Examine model list and look at model comparisons
brt.results.catEFF


##Model Table
AICc.Table.BTcat = model.table(brt.results.catEFF, use.lnl = T)
AICc.Table.BTcat

setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write.csv(AICc.Table.BTcat, "Data/Thesis/Tidy/BRT_OccuModTable_Cat_new.csv", row.names = F)
#-----
#look at summary of top model(s) (delta AICc < 2)
#-----

#without quadratic term for HAiFLS_alt
summary(brt.results.catEFF$p.tv.effort.Psi.for_area) #top
summary(brt.results.catEFF$p.tv.effort.Psi.for_area_slpe) #2nd


#-----
#real parameter values
#-----

#without quadratic term
brt.results.catEFF$p.tv.effort.Psi.for_area$results$real 
tmnq.cat <- brt.results.catEFF$p.tv.effort.Psi.for_area 


cleanup(ask = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing HAiFLS_al2 effect on psi ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
brt.ddl #par.index = 1, model.index = 4

#covariate.predictions method
#HAiFLS_for
min.for <- min(brown.df$HAiFLS_for)
max.for <- max(brown.df$HAiFLS_for)
mean.for <- mean(brown.df$HAiFLS_for)
for.values <- seq(from = min.for, to = max.for, length = 100)
#Area_km2
min.area <- min(brown.df$Area_km2)
max.area <- max(brown.df$Area_km2)
mean.area <- mean(brown.df$Area_km2)
area.values <- seq(from = min.area, to = max.area, length = 100)

###########################################################################
#predict across range of observed values (HAiFLS_for, Area_km2, Cross_Cat)
##########################################################################

#predictions of Psi for full range of HAiFLS_for and mean values of other covars
predictions_for <- covariate.predictions(tmnq.cat, 
                                         data = data.frame(HAiFLS_for = for.values,
                                                           Area_km2 = mean.area),
                                         indices = 4)

head(predictions_for$estimates)

cat.for.preds <- predictions_for$estimates

ap <- 
  ggplot(data=cat.for.preds, aes(x=HAiFLS_for))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="% HAiFLS Forest Land Cover",
       y="Occupancy Probability")+
  theme_bw()+
    theme(panel.grid = element_blank())+
    scale_y_continuous(limits = c(0.00,1.00),
                       breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                       labels = c("0.00", "0.25", "0.50", "0.75", "1.00"))+
    theme(axis.title = element_text(size = 12, face = "bold"))
#ap



#predictions of Psi for full range of Area_km2 and mean values of other covars
predictions_area <- covariate.predictions(tmnq.cat, 
                                         data = data.frame(HAiFLS_for = mean.for,
                                                           Area_km2 = area.values),
                                         indices = 4)

head(predictions_area$estimates)

cat.area.preds <- predictions_area$estimates

bp <- 
  ggplot(data=cat.area.preds, aes(x=Area_km2))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x= bquote(bold('Upstream Catchment Area' ~(km^2))),
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_y_continuous(limits = c(0.00,1.00),
                     breaks = c(0.00, 0.25, 0.50, 0.75, 1.00),
                     labels = c("0.00", "0.25", "0.50", "0.75", "1.00"))+
  theme(axis.title = element_text(size = 12, face = "bold"))

plot_grid(ap, bp, labels = NULL, ncol = 2)
ggsave("brt_occuCAT_new.png", dpi = 350)


####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(cat.for.preds, "Data/Thesis/Tidy/BRT_cat_for_preds.csv")
write_csv(cat.area.preds, "Data/Thesis/Tidy/BRT_cat_area_preds.csv")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing effort effect on p   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
min.effort <- min(brown.df$effort1)
max.effort <- max(brown.df$effort1)
effort.values <- seq(min.effort, max.effort, length.out = 100)
mean.effort <- mean(brown.df$effort1) #906.231

#predictions of p for full range of effort1 values
p.pred.eff1 <- covariate.predictions(tmnq.cat, 
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
write_csv(P.predictions.eff1, "Data/Thesis/Tidy/BRT_cat_DProb_preds.csv")




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                              ###~~~~~~~~~~~~~~~~~~~~~~~##
                              ####   All covariates   ####
                              ##~~~~~~~~~~~~~~~~~~~~~~~##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Local Scale: instream and immediate riparian area
#> MEANT & MEANT2 (+,quad) "maximum daily mean stream temperature"
#> avdep (+) "mean depth"
#> mFlow (+) "mean flow velocity"
#> pctrun (-) "percentage of run habitats"
#> pctpool (+) "percentage of pool habitats"
#> pctBrBnk (-) "percentage of bank that is bare soil"

# Catchment Scale: within the upstream land area that drains to the outlet of the sampled segment
#> HAiFLS_for (+) "Hydrologically Active inserve flow length to the stream of forest LULC"
#> Area_km2 (+,quad) "Catchment Area"

# Weak collinearity - user discretion #
#------------------------#
#Area_km2 and avwid (0.53) #don't include... likely tell same exact story
#avwid and avdep (0.50)
#mFlow and pctslow (-0.48)
## test vs null detection model

#set wd to scratch folder because MARK outputs an insane amount of files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Analysis/Brook Trout Project/Occupancy/RMark/Brown Trout") #because MARK loves output files
getwd()

run.occ.btc=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.tv.effort = list(formula = ~effort)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) #dAICc > 37
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #7 covariates
  Psi.global1 = list(formula = ~MEANT+avdep+mFlow+pctrun+pctBrBnk+HAiFLS_for+Area_km2) #dAICc > 3
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.btc=create.model.list("Occupancy")
  results.btc=mark.wrapper(cml.btc, data=brown.process, ddl=brt.ddl, output=F)
  return(results.btc)
}

brt.results.c = run.occ.btc()
cleanup(ask = F)

##Examine model list and look at model comparisons
brt.results.c

#--------------------------------------------------------------------------------------------------------

run.occ.btf=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  #p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.tv.effort = list(formula = ~effort)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  
  #7 covariates
  Psi.global1 = list(formula = ~MEANT+avdep+mFlow+pctrun+pctBrBnk+HAiFLS_for+Area_km2)
  #6 covariates
  Psi.six.1 = list(formula = ~MEANT+avdep+mFlow+pctrun+pctBrBnk+HAiFLS_for)
  Psi.six.3 = list(formula = ~MEANT+avdep+mFlow+pctrun+pctBrBnk+Area_km2)
  Psi.six.5 = list(formula = ~MEANT+avdep+mFlow+pctrun+HAiFLS_for+Area_km2)
  Psi.six.7 = list(formula = ~MEANT+avdep+mFlow+pctBrBnk+HAiFLS_for+Area_km2)
  Psi.six.9 = list(formula = ~MEANT+avdep+pctrun+pctBrBnk+HAiFLS_for+Area_km2)
  Psi.six.11 = list(formula = ~MEANT+mFlow+pctrun+pctBrBnk+HAiFLS_for+Area_km2)  
  Psi.six.13 = list(formula = ~avdep+mFlow+pctrun+pctBrBnk+HAiFLS_for+Area_km2)
  
  #5 covariates
  Psi.five.1 = list(formula = ~MEANT+avdep+mFlow+pctrun+pctBrBnk)
  Psi.five.5 = list(formula = ~MEANT+avdep+mFlow+pctrun+HAiFLS_for)
  Psi.five.7 = list(formula = ~MEANT+avdep+mFlow+pctBrBnk+HAiFLS_for)
  Psi.five.9 = list(formula = ~MEANT+avdep+pctrun+pctBrBnk+HAiFLS_for)
  Psi.five.11 = list(formula = ~MEANT+mFlow+pctrun+pctBrBnk+HAiFLS_for)
  Psi.five.13 = list(formula = ~avdep+mFlow+pctrun+pctBrBnk+HAiFLS_for)
  Psi.five.14 = list(formula = ~MEANT+avdep+mFlow+pctrun+Area_km2)
  Psi.five.16 = list(formula = ~MEANT+avdep+mFlow+pctBrBnk+Area_km2)
  Psi.five.18 = list(formula = ~MEANT+avdep+pctrun+pctBrBnk+Area_km2)
  Psi.five.20 = list(formula = ~MEANT+mFlow+pctrun+pctBrBnk+Area_km2)
  Psi.five.22 = list(formula = ~avdep+mFlow+pctrun+pctBrBnk+Area_km2)
  Psi.five.23 = list(formula = ~MEANT+avdep+mFlow+HAiFLS_for+Area_km2)
  Psi.five.25 = list(formula = ~MEANT+avdep+pctrun+HAiFLS_for+Area_km2)
  Psi.five.27 = list(formula = ~MEANT+mFlow+pctrun+HAiFLS_for+Area_km2)  
  Psi.five.29 = list(formula = ~avdep+mFlow+pctrun+HAiFLS_for+Area_km2)
  Psi.five.30 = list(formula = ~MEANT+avdep+pctBrBnk+HAiFLS_for+Area_km2)
  Psi.five.32 = list(formula = ~MEANT+mFlow+pctBrBnk+HAiFLS_for+Area_km2)
  Psi.five.34 = list(formula = ~avdep+mFlow+pctBrBnk+HAiFLS_for+Area_km2)
  Psi.five.35 = list(formula = ~MEANT+pctrun+pctBrBnk+HAiFLS_for+Area_km2)
  Psi.five.37 = list(formula = ~avdep+pctrun+pctBrBnk+HAiFLS_for+Area_km2)
  Psi.five.37 = list(formula = ~mFlow+pctrun+pctBrBnk+HAiFLS_for+Area_km2) ##Top Model##
  
  #4 covariates
  Psi.four.1 = list(formula = ~MEANT+mFlow+pctrun+pctBrBnk)
  Psi.four.3 = list(formula = ~MEANT+mFlow+pctrun+HAiFLS_for)
  Psi.four.5 = list(formula = ~MEANT+mFlow+pctBrBnk+HAiFLS_for)
  Psi.four.7 = list(formula = ~MEANT+pctrun+pctBrBnk+HAiFLS_for)
  Psi.four.9 = list(formula = ~mFlow+pctrun+pctBrBnk+HAiFLS_for)
  Psi.four.10 = list(formula = ~MEANT+mFlow+pctrun+Area_km2)
  Psi.four.12 = list(formula = ~MEANT+mFlow+pctBrBnk+Area_km2)
  Psi.four.14 = list(formula = ~MEANT+pctrun+pctBrBnk+Area_km2)
  Psi.four.16 = list(formula = ~mFlow+pctrun+pctBrBnk+Area_km2)
  Psi.four.17 = list(formula = ~MEANT+mFlow+HAiFLS_for+Area_km2)
  Psi.four.19 = list(formula = ~MEANT+pctrun+HAiFLS_for+Area_km2)
  Psi.four.21 = list(formula = ~mFlow+pctrun+HAiFLS_for+Area_km2) ##2nd Top Model - Delta AICc = 0.022
  Psi.four.22 = list(formula = ~MEANT+pctBrBnk+HAiFLS_for+Area_km2)
  Psi.four.24 = list(formula = ~mFlow+pctBrBnk+HAiFLS_for+Area_km2)
  Psi.four.25 = list(formula = ~pctrun+pctBrBnk+HAiFLS_for+Area_km2)

  #3 covariates
  Psi.three.1 = list(formula = ~MEANT+mFlow+pctrun)
  Psi.three.3 = list(formula = ~MEANT+mFlow+pctBrBnk)
  Psi.three.5 = list(formula = ~MEANT+pctrun+pctBrBnk)
  Psi.three.7 = list(formula = ~mFlow+pctrun+pctBrBnk)
  Psi.three.8 = list(formula = ~MEANT+mFlow+HAiFLS_for)
  Psi.three.10 = list(formula = ~MEANT+pctBrBnk+HAiFLS_for)
  Psi.three.12 = list(formula = ~pctrun+pctBrBnk+HAiFLS_for)
  Psi.three.13 = list(formula = ~MEANT+mFlow+Area_km2)
  Psi.three.15 = list(formula = ~MEANT+pctrun+Area_km2)
  Psi.three.17 = list(formula = ~mFlow+pctrun+Area_km2)
  Psi.three.18 = list(formula = ~MEANT+pctBrBnk+Area_km2)
  Psi.three.20 = list(formula = ~pctrun+pctBrBnk+Area_km2)
  Psi.three.21 = list(formula = ~MEANT+HAiFLS_for+Area_km2)
  Psi.three.23 = list(formula = ~mFlow+HAiFLS_for+Area_km2)
  Psi.three.24 = list(formula = ~pctrun+HAiFLS_for+Area_km2)
  Psi.three.25 = list(formula = ~pctBrBnk+HAiFLS_for+Area_km2)
  Psi.three.26 = list(formula = ~MEANT+pctrun+HAiFLS_for)
  Psi.three.28 = list(formula = ~mFlow+pctrun+HAiFLS_for)
  Psi.three.29 = list(formula = ~mFlow+pctBrBnk+Area_km2)
  
  #2 covariates
  Psi.two.1 = list(formula = ~MEANT+avdep)
  Psi.two.3 = list(formula = ~MEANT+mFlow)
  Psi.two.5 = list(formula = ~MEANT+pctrun)
  Psi.two.9 = list(formula = ~MEANT+pctBrBnk)
  Psi.two.11 = list(formula = ~MEANT+HAiFLS_for)
  Psi.two.13 = list(formula = ~MEANT+Area_km2)
  Psi.two.15 = list(formula = ~avdep+mFlow)
  Psi.two.16 = list(formula = ~avdep+pctrun)
  Psi.two.18 = list(formula = ~avdep+pctBrBnk)
  Psi.two.19 = list(formula = ~avdep+HAiFLS_for)
  Psi.two.20 = list(formula = ~avdep+Area_km2)
  Psi.two.21 = list(formula = ~mFlow+pctrun)
  Psi.two.23 = list(formula = ~mFlow+pctBrBnk)
  Psi.two.24 = list(formula = ~mFlow+HAiFLS_for)
  Psi.two.25 = list(formula = ~mFlow+Area_km2)
  Psi.two.26 = list(formula = ~pctrun+pctBrBnk)
  Psi.two.27 = list(formula = ~pctrun+HAiFLS_for)
  Psi.two.28 = list(formula = ~pctrun+Area_km2)
  Psi.two.32 = list(formula = ~pctBrBnk+HAiFLS_for)
  Psi.two.33 = list(formula = ~pctBrBnk+Area_km2)
  Psi.two.34 = list(formula = ~HAiFLS_for+Area_km2)

  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  Psi.MEANT = list(formula = ~MEANT)
  Psi.depth = list(formula = ~avdep)
  Psi.flow = list(formula = ~mFlow)
  Psi.run = list(formula = ~pctrun)
  Psi.bare = list(formula = ~pctBrBnk) 
  Psi.forest = list(formula = ~HAiFLS_for)
  Psi.area = list(formula = ~Area_km2)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.btf=create.model.list("Occupancy")
  results.btf=mark.wrapper(cml.btf, data=brown.process, ddl=brt.ddl, output=F)
  return(results.btf)
}

brt.results.f = run.occ.btf()
cleanup(ask = F)

##Examine model list and look at model comparisons
brt.results.f


##########################################################################################################

##Model Table
AICc.Table.BRT = model.table(brt.results.f, use.lnl = T)
AICc.Table.BRT
class(AICc.Table)
#export model table 
getwd()
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
#write csv for model table
write.csv(AICc.Table.BRT, "Data/Thesis/Tidy/BrownTrout_OccuMod_Table_new.csv", row.names = F)

#look at summary of top model(s)
summary(brt.results.f$p.tv.effort.Psi.five.37) #top
summary(brt.results.f$p.tv.effort.Psi.four.21) #2nd
summary(brt.results.f$p.tv.effort.Psi.five.27) #3rd
summary(brt.results.f$p.tv.effort.Psi.six.11) #4th -- same as 3rd but with BareBank

#real parameter values
brt.results.f$p.tv.effort.Psi.four.21$results$real

#save top model into new object for later projections
p.mod <- brt.results.f$p.tv.effort.Psi.four.21

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing covariate effects on psi ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

#Covars to predict on: mFlow, pctrun, HAiFLS_for, Area_km2
#covariate.predictions method

#-----
#mFlow
#-----
min.flow <- min(brown.df$mFlow)
max.flow <- max(brown.df$mFlow)
mean.flow <- mean(brown.df$mFlow)
flow.values <- seq(from = min.flow, to = max.flow, length = 100)
#-----
#pctrun
#-----
min.run <- min(brown.df$pctrun)
max.run <- max(brown.df$pctrun)
mean.run <- mean(brown.df$pctrun)
run.values <- seq(from = min.run, to = max.run, length = 100)
#-----
#HAiFLS_for
#-----
min.for <- min(brown.df$HAiFLS_for)
max.for <- max(brown.df$HAiFLS_for)
mean.for <- mean(brown.df$HAiFLS_for)
for.values <- seq(from = min.for, to = max.for, length = 100)
#-----
#Area_km2
#-----
min.area <- min(brown.df$Area_km2)
max.area <- max(brown.df$Area_km2)
mean.area <- mean(brown.df$Area_km2)
area.values <- seq(from = min.area, to = max.area, length = 100)


#########################################################################
#predict while holding all other covariate values constant at their mean
#########################################################################
brt.ddl #model.index=4

#-----
#mFlow
#-----
predictions_flow <- covariate.predictions(p.mod, 
                                           data = data.frame(mFlow = flow.values,
                                                             pctrun = mean.run,
                                                             HAiFLS_for = mean.for,
                                                             Area_km2 = mean.area),
                                           indices = 4)

predictions_flow$estimates

flow.preds <- predictions_flow$estimates

a <- ggplot(data=flow.preds, aes(x=mFlow))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Mean Flow Velocity (m/sec)",
       y="Occupancy Probability (Psi)")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
a

#-----
#pctrun
#-----
predictions_run <- covariate.predictions(p.mod, 
                                          data = data.frame(mFlow = mean.flow,
                                                            pctrun = run.values,
                                                            HAiFLS_for = mean.for,
                                                            Area_km2 = mean.area),
                                          indices = 4)

predictions_run$estimates

run.preds <- predictions_run$estimates

b <- ggplot(data=run.preds, aes(x=pctrun))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Percentage of Run Macrohabitat",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
b

#-----
#HAiFLS_for
#-----
predictions_for <- covariate.predictions(p.mod, 
                                         data = data.frame(mFlow = mean.flow,
                                                           pctrun = mean.run,
                                                           HAiFLS_for = for.values,
                                                           Area_km2 = mean.area),
                                         indices = 4)

predictions_for$estimates

for.preds <- predictions_for$estimates
class(for.preds)
names(for.preds)
class(for.preds$lcl)

c <- ggplot(data=for.preds, aes(x=HAiFLS_for))+
  geom_ribbon(aes(ymin=for.preds$lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0.2,1), breaks = c(0.20,0.40,0.60,0.80,1.00), labels = c("0.20","0.40","0.60","0.80","1.00"))+
  labs(x="% HAiFLS Forest Land Cover",
       y="Occupancy Probability (Psi)")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
c


#-----
#Area_km2
#-----
predictions_area <- covariate.predictions(p.mod, 
                                         data = data.frame(mFlow = mean.flow,
                                                           pctrun = mean.run,
                                                           HAiFLS_for = mean.for,
                                                           Area_km2 = area.values),
                                         indices = 4)

predictions_area$estimates

area.preds <- predictions_area$estimates

d <- ggplot(data=area.preds, aes(x=Area_km2))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0.2,1), breaks = c(0.20,0.40,0.60,0.80,1.00), labels = c("0.20","0.40","0.60","0.80","1.00"))+
  labs(x="Upstream Catchment Area (km^2)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
d

#cowplot
plot_grid(a,b,c,d, labels = NULL, ncol = 2)
ggsave("brt_OccuProb_NEW_02_28.png",
       dpi = 350)

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(flow.preds, "Data/Thesis/Tidy/BRT_psi_flow_preds.csv")
write_csv(run.preds, "Data/Thesis/Tidy/BRT_psi_run_preds.csv")
write_csv(for.preds, "Data/Thesis/Tidy/BRT_psi_for_preds.csv")
write_csv(area.preds, "Data/Thesis/Tidy/BRT_psi_area_preds.csv")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing effort effect on p   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
min.effort <- min(brown.df$effort1)
max.effort <- max(brown.df$effort1)
effort.values <- seq(min.effort, max.effort, length.out = 100)
mean.effort <- mean(brown.df$effort1) #906.231

#predictions of p for full range of effort1 values
predictions_effort <- covariate.predictions(p.mod, 
                                       data = data.frame(effort1 = effort.values),
                                       indices = 1)

predictions_effort$estimates


effort.preds <- predictions_effort$estimates %>%
  select(covdata, estimate, se, lcl, ucl) %>%
  rename(Effort_sec = covdata) %>%
  round(digits = 4)

ggplot(data=effort.preds, aes(x=Effort_sec))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Electrofishing Effort (sec)",
       y="Detection Probability")+
  scale_y_continuous(limits = c(0.4,1), breaks = c(0.40,0.60,0.80,1.00), labels = c("0.40","0.60","0.80","1.00"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))

ggsave("brt_DetProb_3_8_2020.png",
       dpi = 350)

####################################################
##       Write tidy csv for P predictions         ## 
####################################################
write_csv(effort.preds, "Data/Thesis/BRT_p_effort_preds.csv")


run.occ.BEST=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.tv.effort = list(formula = ~effort)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  
  #4 covariates
  Psi.four.21 = list(formula = ~mFlow+pctrun+HAiFLS_for+Area_km2) ##2nd Top Model - Delta AICc = 

  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.BEST=create.model.list("Occupancy")
  results.BEST=mark.wrapper(cml.BEST, data=brown.process, ddl=brt.ddl, output=F)
  return(results.BEST)
}

brt.results.BEST = run.occ.BEST()
cleanup(ask = F)

summary(brt.results.BEST$p.Dot.Psi.four.21)
brt.results.BEST$p.tv.effort.Psi.four.21$results$real



























sd(brown$mFlow)
mean(brown$mFlow)
mean(brown$mFlow)+sd(brown$mFlow)
