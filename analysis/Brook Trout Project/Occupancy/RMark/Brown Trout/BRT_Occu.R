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


##--------------------------------------------------------------------------------------------------------------------------------##
#read in data, rearrange and change some labels to work with grouping ("freq"), and time-varying covariates ("Effort1 --> Effort3")
brown <- read_csv("Data/Thesis/Tidy/BRT_occDF_RMARK.csv", col_names = T)

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
  #Psi.global2 = list(formula = ~HAiFLS_al2+Area_km2+AvgSlope+Cross_Cat)
  Psi.global3 = list(formula = ~HAiFLS_alt+Area2+AvgSlope+Cross_Cat)
  #Psi.global4 = list(formula = ~HAiFLS_al2+Area2+AvgSlope+Cross_Cat)
  Psi.global5 = list(formula = ~HAiFLS_for+Area_km2+AvgSlope+Cross_Cat)
  Psi.global6 = list(formula = ~HAiFLS_nat+Area_km2+AvgSlope+Cross_Cat)
  Psi.global7 = list(formula = ~HAiFLS_for+Area2+AvgSlope+Cross_Cat)
  Psi.global8 = list(formula = ~HAiFLS_nat+Area2+AvgSlope+Cross_Cat)
  #3 Covariates
  Psi.alt_area_slpe = list(formula = ~HAiFLS_alt+Area_km2+AvgSlope)
  Psi.alt_area_cross = list(formula = ~HAiFLS_alt+Area_km2+Cross_Cat)
  Psi.alt_slpe_cross = list(formula = ~HAiFLS_alt+AvgSlope+Cross_Cat)
  Psi.area_slpe_crs = list(formula = ~Area_km2+AvgSlope+Cross_Cat)
  Psi.for_area_slpe = list(formula = ~HAiFLS_for+Area_km2+AvgSlope)
  Psi.for_area_cross = list(formula = ~HAiFLS_for+Area_km2+Cross_Cat)
  Psi.for_slpe_cross = list(formula = ~HAiFLS_for+AvgSlope+Cross_Cat)
  Psi.nat_area_slpe = list(formula = ~HAiFLS_nat+Area_km2+AvgSlope)
  Psi.nat_area_cross = list(formula = ~HAiFLS_nat+Area_km2+Cross_Cat)
  Psi.nat_slpe_cross = list(formula = ~HAiFLS_nat+AvgSlope+Cross_Cat)
  
  #Psi.al2_area_slpe = list(formula = ~HAiFLS_al2+Area_km2+AvgSlope)
  #Psi.al2_area_cross = list(formula = ~HAiFLS_al2+Area_km2+Cross_Cat)
  #Psi.al2_slpe_cross = list(formula = ~HAiFLS_al2+AvgSlope+Cross_Cat)
  Psi.ar2_slpe_crs = list(formula = ~Area2+AvgSlope+Cross_Cat)
  Psi.alt_ar2_slpe = list(formula = ~HAiFLS_alt+Area2+AvgSlope)
  Psi.alt_ar2_cross = list(formula = ~HAiFLS_alt+Area2+Cross_Cat)
  #Psi.al2_ar2_slpe = list(formula = ~HAiFLS_al2+Area2+AvgSlope)
  #Psi.al2_ar2_cross = list(formula = ~HAiFLS_al2+Area2+Cross_Cat)
  Psi.for_ar2_slpe = list(formula = ~HAiFLS_for+Area2+AvgSlope)
  Psi.nat_ar2_cross = list(formula = ~HAiFLS_nat+Area2+Cross_Cat)
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
  Psi.nat_area = list(formula = ~HAiFLS_nat+Area_km2)
  Psi.nat_slpe = list(formula = ~HAiFLS_nat+AvgSlope)
  Psi.nat_cross = list(formula = ~HAiFLS_nat+Cross_Cat)
  
  #Psi.al2_area = list(formula = ~HAiFLS_al2+Area_km2)
  #Psi.al2_slpe = list(formula = ~HAiFLS_al2+AvgSlope)
  #Psi.al2_cross = list(formula = ~HAiFLS_al2+Cross_Cat)
  
  Psi.alt_ar2 = list(formula = ~HAiFLS_alt+Area2)
  Psi.ar2_slpe = list(formula = ~Area2+AvgSlope)
  Psi.ar2_cross = list(formula = ~Area2+Cross_Cat)
  Psi.for_ar2 = list(formula = ~HAiFLS_for+Area2)
  Psi.nat_ar2 = list(formula = ~HAiFLS_nat+Area2)
  
  #Psi.al2_ar2 = list(formula = ~HAiFLS_al2+Area2)
  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  Psi.alt = list(formula = ~HAiFLS_alt)
  #Psi.al2 = list(formula = ~HAiFLS_al2)
  Psi.for = list(formula = ~HAiFLS_for)
  Psi.nat = list(formula = ~HAiFLS_nat)
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

#-----
#look at summary of top model(s) (delta AICc < 2)
#-----

#without quadratic term
summary(brt.results.cat$p.tv.effort.Psi.for_area_cross) #top
summary(brt.results.cat$p.tv.effort.Psi.alt_area_cross) #2nd

#With quadratic term
summary(brt.results.cat$p.tv.effort.Psi.al2_area_cross) #top
summary(brt.results.cat$p.tv.effort.Psi.al2_area) #2nd
summary(brt.results.cat$p.tv.effort.Psi.alt_area_cross) 

#-----
#real parameter values
#-----

#without quadratic term
brt.results.cat$p.tv.effort.Psi.for_area_cross$results$real 
tmnq.cat <- brt.results.cat$p.tv.effort.Psi.for_area_cross 

#With quadratic term
brt.results.cat$p.tv.effort.Psi.al2_area_cross$results$real #top
brt.results.cat$p.tv.effort.Psi.al2_area$results$real #2nd
brt.results.cat$p.tv.effort.Psi.alt_area_cross$results$real 
tm.cat <- brt.results.cat$p.tv.effort.Psi.al2_area_cross
tm2.cat <- brt.results.cat$p.tv.effort.Psi.al2_area



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
#HAiFLS_alt
min.alt <- min(brown.df$HAiFLS_alt)
max.alt <- max(brown.df$HAiFLS_alt)
mean.alt <- mean(brown.df$HAiFLS_alt)
alt.values <- seq(from = min.alt, to = max.alt, length = 100)
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

###########################################################################
#predict across range of observed values (HAiFLS_al2, Area_km2, Cross_Cat)
##########################################################################

#predictions of Psi for full range of HAiFLS_for and mean values of other covars
predictions_for <- covariate.predictions(tmnq.cat, 
                                         data = data.frame(HAiFLS_for = for.values,
                                                           Area_km2 = mean.area,
                                                           Cross_Cat = mean.cross),
                                         indices = 4)

predictions_for$estimates

cat.for.preds <- predictions_for$estimates

ap <- ggplot(data=cat.for.preds, aes(x=HAiFLS_for))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="% HAiFLS Forest Land Cover",
       y="Occupancy Probability (Psi)")+
  theme_minimal_grid()

#predictions of Psi for full range of HAiFLS_alt and mean values of other covars
predictions_alt <- covariate.predictions(tm.alt.cat, 
                                         data = data.frame(HAiFLS_alt = alt.values,
                                                           Area_km2 = mean.area,
                                                           Cross_Cat = mean.cross),
                                         indices = 4)

predictions_alt$estimates

cat.alt.preds <- predictions_alt$estimates

ggplot(data=cat.alt.preds, aes(x=HAiFLS_alt))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="% HAiFLS Altered Land Cover",
       y="Occupancy Probability (Psi)")+
  theme_minimal_grid()


#predictions of Psi for full range of HAiFLS_al2 and mean values of other covars
predictions_al2 <- covariate.predictions(tm.cat, 
                                         data = data.frame(HAiFLS_al2 = al2.values,
                                                           Area_km2 = mean.area,
                                                           Cross_Cat = mean.cross),
                                         indices = 4)

predictions_al2$estimates

cat.al2.preds <- predictions_al2$estimates

ggplot(data=cat.al2.preds, aes(x=HAiFLS_al2))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="% HAiFLS Altered Land Cover ^2",
       y="Occupancy Probability (Psi)")+
  theme_minimal_grid()#+
  #theme(axis.title = element_text(face = "bold"))


#predictions of Psi for full range of Area_km2 and mean values of other covars
predictions_area <- covariate.predictions(tm.cat, 
                                         data = data.frame(HAiFLS_al2 = mean.al2,
                                                           Area_km2 = area.values,
                                                           Cross_Cat = mean.cross),
                                         indices = 4)

predictions_area$estimates

cat.area.preds <- predictions_area$estimates

bp <- ggplot(data=cat.area.preds, aes(x=Area_km2))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Upstream Catchment Area (km^2)",
       y="Occupancy Probability (Psi)")+
  theme_minimal_grid()#+
  #theme(axis.title = element_text(face = "bold"))

#predictions of Psi for full range of Cross_Cat and mean values of other covars
predictions_cross <- covariate.predictions(tm.cat, 
                                          data = data.frame(HAiFLS_al2 = mean.al2,
                                                            Area_km2 = mean.area,
                                                            Cross_Cat = cross.values),
                                          indices = 4)

predictions_cross$estimates

cat.cross.preds <- predictions_cross$estimates

cp <- ggplot(data=cat.cross.preds, aes(x=Cross_Cat))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="red")+
  labs(x="Road Crossing Density in Catchment (#/km^2)",
       y="Occupancy Probability (Psi)")+
  theme_minimal_grid()#+
  #theme(axis.title = element_text(face = "bold"))

#cowplot
# now add the title
title <- ggdraw() + 
  draw_label(
    "Catchment Scale Occupancy of Brown Trout",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )
plot_row <- plot_grid(ap, bp, cp, ncol = 3)
plot_grid(title, plot_row, labels = NULL, ncol = 1,
          rel_heights = c(0.1, 1))



####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(cat.for.preds, "Data/Thesis/Tidy/BRT_cat_for_preds.csv")
write_csv(cat.area.preds, "Data/Thesis/Tidy/BRT_cat_area_preds.csv")
write_csv(cat.cross.preds, "Data/Thesis/Tidy/BRT_cat_cross_preds.csv")

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
#> MAXT & MAXT2(+,quad) "maximum daily maximum stream temperature"
#> MEANT & MEANT2 (+,quad) "maximum daily mean stream temperature"
#> avwid (+) "mean wetted width"
#> avdep (+) "mean depth"
#> mFlow (+) "mean flow velocity"
#> pctrun (-) "percentage of run habitats"
#> pctpool (+) "percentage of pool habitats"
#> pctBrBnk (-) "percentage of bank that is bare soil"

# Catchment Scale: within the upstream land area that drains to the outlet of the sampled segment
#> HAiFLS_for (+) "Hydrologically Active inserve flow length to the stream of forest LULC"
#> Area_km2 & Area2 (+,quad) "Catchment Area"

# Weak collinearity - user discretion #
#------------------------#
#Area_km2 and avwid (0.53) #don't include... likely tell same exact story
#avwid and avdep (0.50)
#mFlow and pctslow (-0.48)

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
  
  #all covariates
  #Psi.global = list(formula = ~)
  #6 covariates
 
  #5 covariates

  #4 covariates

  #3 covariates

  #2 covariates

  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  Psi.pool = list(formula=~pctpool) 
  Psi.bare = list(formula=~pctBrBnk) 
  Psi.forest = list(formula = ~HAiFLS_for)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml=create.model.list("Occupancy")
  results=mark.wrapper(cml, data=brook.process, ddl=bkt.ddl, output=F)
  return(results)
}

brt.results = run.occ()


##Examine model list and look at model comparisons
brt.results
##Model Table
AICc.Table = model.table(brt.results, use.lnl = T)
AICc.Table

#look at summary of top model(s)


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

