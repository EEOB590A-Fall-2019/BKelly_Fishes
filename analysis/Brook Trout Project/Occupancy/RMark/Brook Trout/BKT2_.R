##########################################################################################################################
#                  Brook Trout Occupancy Analysis in RMark -- Information Criteria Model Selection
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
library(corrplot)

##Occupancy example
#?weta


##--------------------------------------------------------------------------------------------------------------------------------##
#read in data, rearrange and change some labels to work with grouping ("freq"), and time-varying covariates ("Effort1 --> Effort3")
brook <- read_csv("Data/Thesis/Tidy/BKT_Occu_File.csv", col_names = T)
brook.df <- as.data.frame(brook) %>%
  rename(BrBnk = pctBrBnk) %>%
  select(-pctex21, -RNGT, -EFac_Cat)
#examine
skim(brook.df)
names(brook.df)

cat <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header=T) %>%
  select(newID = HUC_Site, elev_m)
#examine
skim(cat)
names(cat)

brook.df <- left_join(brook.df, cat, by="newID")

#write_csv(brook.df, "Data/Thesis/Tidy/BrookTrout_dataframe.csv")

#need random summaries for manuscript table - canopy cover, rock, BRT_CPUE
#summary(brook.df$pctrock)
#sd(brook.df$pctrock)
#summary(brook.df$pctShade)
#sd(brook.df$pctShade)
#summary(brook.df$BRT_100m)
#sd(brook.df$BRT_100m)
##########################################################################################
# Local Scale: instream and immediate riparian area
#> pctex21 (-) "percentage of summer temperature observations that exceed 21 degrees C"
#> pctpool (+) "percentage of pool habitats"
#> pctrock (+) "percentage of rocky substrates (gravel, cobble, boulder)"
#> pctBrBnk (-) "percentage of bank that is bare soil"
#> pctShade (+) "average percent canopy cover"
#> BRT_100m (-) "Brown Trout Catch-Per 100m of stream sampled"
# Catchment Scale: within the upstream land area that drains to the outlet of the sampled segment
#> HAiFLS_for (+) "Hydrologically Active inserve flow length to the stream of forest LULC"
#> Area_km2 (-) "Catchment Area"
#> AvgSlope (+) "Mean Slope of the catchment"
#> EFac_Cat (-) "Environmental Facility density of upstream catchment (count/Area_km2)"
#> Cross_Cat (-) "Road Crossing density of upstream catchment (count/Area_km2)"
##########################################################################################

#----------------#
#correlation test
#----------------#
c <- cor(brook.df[,4:20])
head(round(c,2)) 

#round down
cround <- round(c,3)

#visualize these correlations
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "gray", "#007FFF", "blue", "#00007F"))

corrplot(c, type = "upper", order = "alphabet", method = "number", col = col4(5))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
## Colinear variables to not include in the same model (> 0.6):
# Strong #
#HAiFLS_for and HAiFLS_alt (-0.96)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Weak - user discretion #
#------------------------#
#Area_km2 and effort (0.43)
#pctShade and HAiFLS_for (0.4)
#pctShade and HAiFLS_alt (-0.37)
#HAiFLS_for and Cross_Cat (-0.36)
#HAiFLS_alt and Cross_Cat (0.35)
#pctShade and Cross_Cat (-0.25)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Variables of interest and Data Dictionary ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#-----#
#Detection Probability: probability of detecting Brook Trout at a site
#-----#
#> time varying effort (+) "amount (seconds) of time spent sampling (aka shocking)"

#-----#
#Occupancy Probability: probability of Brook Trout occurrence among sites
#-----#
# Local Scale: instream and immediate riparian area
#> pctpool (+) "percentage of pool habitats"
#> pctrock (+) "percentage of rocky substrates (gravel, cobble, boulder)"
#> pctBrBnk (-) "percentage of bank that is bare soil"
#> pctShade (+) "average percent canopy cover"
#> BRT_100m (-) "Brown Trout Catch-Per 100m of stream sampled"
# Catchment Scale: within the upstream land area that drains to the outlet of the sampled segment
#> HAiFLS_for (+) "Hydrologically Active inserve flow length to the stream of forest LULC"
#> Area_km2 (-) "Catchment Area"
#> AvgSlope (+) "Mean Slope of the catchment"
#> EFac_Cat (-) "Environmental Facility density of upstream catchment (count/Area_km2)"
#> Cross_Cat (-) "Road Crossing density of upstream catchment (count/Area_km2)"
##########################################################################################
################################################################################################################################
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
################################################################################################################################
#set wd to scratch folder because MARK outputs an insane amount of files
setwd("C:/Users/brett/OneDrive/Documents/Iowa State University/BKelly_Fishes/Analysis/Brook Trout Project/Occupancy/RMark/Brook Trout") #because MARK loves output files
getwd()
#Process Data
#?process.data
#?make.design.data
brook.process = process.data(brook.df, model="Occupancy", groups = "freq")
bkt.ddl = make.design.data(brook.process)

# Catchment Scale: within the upstream land area that drains to the outlet of the sampled segment
#> HAiFLS_for (+) "Hydrologically Active inserve flow length to the stream of forest LULC"
#> Area_km2 (-) "Catchment Area"
#> AvgSlope (+) "Mean Slope of the catchment"
#> Cross_Cat (-) "Road Crossing density of upstream catchment (count/Area_km2)"

##-------------------------------------------------------------------------------------------##
## --------------------------- Check constant p hypothesis --------------------------------- ##
##-------------------------------------------------------------------------------------------##

run.occ.cat.dp1=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.tv.effort = list(formula = ~effort)
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~HAiFLS_for+Area_km2+AvgSlope+Cross_Cat)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.cat.dp1=create.model.list("Occupancy")
  results.cat.dp1=mark.wrapper(cml.cat.dp1, data=brook.process, ddl=bkt.ddl, output=F)
  return(results.cat.dp1)
}

bkt.results.dp1 = run.occ.cat.dp1()

bkt.results.dp1

#Model Results
summary(bkt.results.dp1$p.tv.effort.Psi.global) #top model
bkt.results.dp1$p.tv.effort.Psi.global$results$real


summary(bkt.results.dp1$p.Dot.Psi.global) #2nd model 
bkt.results.dp1$p.Dot.Psi.global$results$real

## continue with effort on p

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####   Catchment Scale covariates   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

run.occ.cat=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.tv.effort = list(formula = ~effort)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~HAiFLS_for+Area_km2+AvgSlope+Cross_Cat)
  #3 Covariates
  Psi.for_area_slpe = list(formula = ~HAiFLS_for+Area_km2+AvgSlope)
  Psi.for_area_cross = list(formula = ~HAiFLS_for+Area_km2+Cross_Cat)
  Psi.for_slpe_cross = list(formula = ~HAiFLS_for+AvgSlope+Cross_Cat)
  Psi.area_slpe_crs = list(formula = ~Area_km2+AvgSlope+Cross_Cat)
  #2 covariates
  Psi.for_area = list(formula = ~HAiFLS_for+Area_km2)
  Psi.for_slpe = list(formula = ~HAiFLS_for+AvgSlope)
  Psi.for_cross = list(formula = ~HAiFLS_for+Cross_Cat)
  Psi.area_slpe = list(formula = ~Area_km2+AvgSlope)
  Psi.area_cross = list(formula = ~Area_km2+Cross_Cat)
  Psi.slpe_cross = list(formula = ~AvgSlope+Cross_Cat)
  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  Psi.for = list(formula = ~HAiFLS_for)
  Psi.area = list(formula = ~Area_km2)
  Psi.slope = list(formula = ~AvgSlope)
  Psi.cross = list(formula = ~Cross_Cat)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.cat=create.model.list("Occupancy")
  results.cat=mark.wrapper(cml.cat, data=brook.process, ddl=bkt.ddl, output=F)
  return(results.cat)
}

bkt.results.cat = run.occ.cat()


##Examine model list and look at model comparisons
bkt.results.cat
##Model Table
AICc.Table.cat = model.table(bkt.results.cat, use.lnl = T)
AICc.Table.cat

#save model table output
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write.csv(AICc.Table.cat, "BrookTrout_CatModTable.csv", row.names = F)

#look at summary of top model(s)
#top model
summary(bkt.results.cat$p.tv.effort.Psi.for)
bkt.results.cat$p.tv.effort.Psi.for$results$real

#2nd ranked model
summary(bkt.results.cat$p.tv.effort.Psi.for_slpe)
bkt.results.cat$p.tv.effort.Psi.for_slpe$results$real

#designate top cat model
tm.cat <- bkt.results.cat$p.tv.effort.Psi.for


cleanup(ask = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing HAiFLS_for effect on psi ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
bkt.ddl #par.index = 1, model.index = 4

#covariate.predictions method
min.for <- min(brook.df$HAiFLS_for)
max.for <- max(brook.df$HAiFLS_for)
for.values <- seq(from = min.for, to = max.for, length = 100)

##################################################
#predict across range of observed values (forest)
##################################################

#predictions of Psi for full range of p21 & -1SD of forest values (would be negative so just forest=0)
predictions_for <- covariate.predictions(tm.cat, 
                                         data = data.frame(HAiFLS_for = for.values),
                                         indices = 4)

head(predictions_for$estimates)

catch.mod.predictions <- predictions_for$estimates
names(catch.mod.predictions)

#-----ggplot-----#

#---Occupancy---#

ggplot(data=catch.mod.predictions, aes(x=covdata))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="% HAiFLS Forest Land Cover",
       y="Occupancy Probability")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  scale_y_continuous(limits = c(0.00,0.75),
                     breaks = c(0.00, 0.25, 0.50, 0.75),
                     labels = c("0.00", "0.25", "0.50", "0.75"))+
  theme(axis.title = element_text(size = 12, face = "bold"))
#ggsave("bkt_Psi_CatScale.png", dpi = 350)

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(catch.mod.predictions, "Data/Thesis/Tidy/BKT_Catchment_Model_Predictions.csv")



#-----------------------------------------------------------------------------------------------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing HAiFLS_for effect on psi -- prediction surface map ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
gis <- read.csv("Data/Thesis/Tidy/gis_points.csv", header = T)

bkt.ddl #par.index = 1, model.index = 4

#covariate.predictions method
for.valuesg <- gis$HAiFLS_for

##################################################
#predict across range of observed values (forest)
##################################################

#predictions of Psi for full range of forest values
predictions_forg <- covariate.predictions(tm.cat, 
                                         data = data.frame(HAiFLS_for = for.valuesg),
                                         indices = 4)

head(predictions_forg$estimates)

gis2 <- as.data.frame(predictions_forg$estimates) %>%
  select(HAiFLS_for = covdata, estimate, se, lcl, ucl)

gis2$OBJECTID <- seq(from=1, to=4664)

gis3 <- left_join(gis, gis2, by="OBJECTID") %>%
  select(-HAiFLS_for.y) %>%
  rename(HAiFLS_for = HAiFLS_for.x)
summary(gis3$estimate)

write.csv(gis3, "Data/Thesis/Tidy/BrookTrout_Psi_Points.csv", row.names = F)
#-----------------------------------------------------------------------------------------------


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

names(P.predictions.eff1)
summary(P.predictions.eff1)


####################################################
##       Write tidy csv for P predictions         ## 
####################################################
write_csv(P.predictions.eff1, "Data/Thesis/Tidy/BKT_CatchMod_DProb_predictions.csv")

##########################################################################################
##### ----------------- Local and Catchment Scale Models -------------------------- ######
##########################################################################################
getwd()
#set wd to scratch folder because MARK outputs an insane amount of files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Analysis/Brook Trout Project/Occupancy/RMark/Brook Trout") #because MARK loves output files

#Process Data
#?process.data
#?make.design.data
brook.process = process.data(brook.df, model="Occupancy", groups = "freq")
bkt.ddl = make.design.data(brook.process)

#---------------------------------------------------------------------------------------------------------------------------------------#

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####   dot versus effort on detection probability   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
run.occ.dp2=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.tv.effort = list(formula = ~effort)
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~avgT+pctpool+pctrock+BRT_100m+BrBnk+pctShade+HAiFLS_for)

  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.dp2=create.model.list("Occupancy")
  results.dp2=mark.wrapper(cml.dp2, data=brook.process, ddl=bkt.ddl, output=F)
  return(results.dp2)
}

bkt.results.dp2 = run.occ.dp2()


##Examine model list and look at model comparisons
bkt.results.dp2

## Move forward with effort on p

#---------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------------------------------------------------------------#

###~~~~~~~~~~~~~~~~~~~~~~~##
####   All covariates   ####
##~~~~~~~~~~~~~~~~~~~~~~~##

run.occ=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - single covariate ~~~~~~~~~~~~
  p.tv.effort = list(formula = ~effort)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #Global Model
  Psi.global = list(formula = ~avgT+pctpool+pctrock+BRT_100m+BrBnk+pctShade+HAiFLS_for)
  #6 covariates
  Psi.mixed1 = list(formula = ~avgT+pctpool+pctrock+BRT_100m+BrBnk+pctShade)
  Psi.mixed2 = list(formula = ~avgT+pctpool+pctrock+BRT_100m+BrBnk+HAiFLS_for)
  Psi.mixed3 = list(formula = ~avgT+pctpool+pctrock+BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed4 = list(formula = ~avgT+pctpool+pctrock+BrBnk+pctShade+HAiFLS_for)
  Psi.mixed5 = list(formula = ~avgT+pctpool+BRT_100m+BrBnk+pctShade+HAiFLS_for)
  Psi.mixed6 = list(formula = ~avgT+pctrock+BRT_100m+BrBnk+pctShade+HAiFLS_for)
  Psi.mixed7 = list(formula = ~pctpool+pctrock+BRT_100m+BrBnk+pctShade+HAiFLS_for)
  #5 covariates
  Psi.mixed8 = list(formula = ~avgT+pctpool+pctrock+BRT_100m+BrBnk)
  Psi.mixed9 = list(formula = ~avgT+pctpool+pctrock+BRT_100m+pctShade)
  Psi.mixed10 = list(formula = ~avgT+pctpool+pctrock+pctShade+BrBnk)
  Psi.mixed11 = list(formula = ~avgT+pctpool+BRT_100m+pctShade+BrBnk)
  Psi.mixed12 = list(formula = ~avgT+pctrock+BRT_100m+pctShade+BrBnk)
  Psi.mixed13 = list(formula = ~pctpool+pctrock+BRT_100m+pctShade+BrBnk)
  Psi.mixed14 = list(formula = ~avgT+pctpool+pctrock+BRT_100m+HAiFLS_for)
  Psi.mixed15 = list(formula = ~avgT+pctpool+pctrock+pctShade+HAiFLS_for)
  Psi.mixed16 = list(formula = ~avgT+pctpool+pctShade+BrBnk+HAiFLS_for)
  Psi.mixed17 = list(formula = ~avgT+BRT_100m+pctShade+BrBnk+HAiFLS_for)
  Psi.mixed18 = list(formula = ~pctrock+BRT_100m+pctShade+BrBnk+HAiFLS_for)
  Psi.mixed19 = list(formula = ~pctpool+pctrock+BRT_100m+BrBnk+HAiFLS_for)
  Psi.mixed20 = list(formula = ~avgT+pctrock+BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed22 = list(formula = ~avgT+pctpool+pctrock+BrBnk+HAiFLS_for)
  Psi.mixed23 = list(formula = ~avgT+pctpool+BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed24 = list(formula = ~avgT+pctrock+BrBnk+pctShade+HAiFLS_for)
  Psi.mixed25 = list(formula = ~pctpool+BRT_100m+BrBnk+pctShade+HAiFLS_for)
  Psi.mixed26 = list(formula = ~avgT+pctpool+BRT_100m+BrBnk+HAiFLS_for)
  Psi.mixed27 = list(formula = ~avgT+pctrock+BRT_100m+BrBnk+HAiFLS_for)
  Psi.mixed28 = list(formula = ~pctpool+pctrock+BrBnk+pctShade+HAiFLS_for)
  Psi.mixed29 = list(formula = ~pctpool+pctrock+BRT_100m+pctShade+HAiFLS_for)
  #4 covariates
  Psi.biol = list(formula = ~avgT+pctpool+pctrock+BRT_100m)
  Psi.mixed30 = list(formula = ~avgT+pctpool+pctShade+BrBnk)
  Psi.mixed31 = list(formula = ~avgT+pctrock+pctShade+BrBnk)
  Psi.mixed32 = list(formula = ~avgT+BRT_100m+pctShade+BrBnk)
  Psi.mixed33 = list(formula = ~pctrock+BRT_100m+pctShade+BrBnk)
  Psi.mixed34 = list(formula = ~pctpool+pctrock+pctShade+BrBnk)
  Psi.mixed35 = list(formula = ~pctpool+BRT_100m+pctShade+BrBnk)
  Psi.mixed36 = list(formula = ~avgT+pctpool+pctrock+HAiFLS_for)
  Psi.mixed37 = list(formula = ~avgT+pctpool+pctShade+HAiFLS_for)
  Psi.mixed38 = list(formula = ~avgT+BrBnk+pctShade+HAiFLS_for)
  Psi.mixed39 = list(formula = ~BRT_100m+BrBnk+pctShade+HAiFLS_for)
  Psi.mixed40 = list(formula = ~avgT+pctpool+BRT_100m+HAiFLS_for)
  Psi.mixed41 = list(formula = ~pctpool+pctrock+BRT_100m+HAiFLS_for)
  Psi.mixed42 = list(formula = ~avgT+pctrock+BRT_100m+HAiFLS_for)
  Psi.mixed43 = list(formula = ~avgT+pctpool+BrBnk+HAiFLS_for)
  Psi.mixed44 = list(formula = ~avgT+pctrock+BrBnk+HAiFLS_for)
  Psi.mixed45 = list(formula = ~pctpool+pctrock+BrBnk+HAiFLS_for)
  Psi.mixed46 = list(formula = ~avgT+pctrock+BrBnk+HAiFLS_for)
  Psi.mixed47 = list(formula = ~avgT+pctrock+pctShade+HAiFLS_for)
  Psi.mixed48 = list(formula = ~pctpool+pctrock+pctShade+HAiFLS_for)
  Psi.mixed49 = list(formula = ~pctpool+BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed50 = list(formula = ~pctrock+BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed51 = list(formula = ~pctrock+BrBnk+pctShade+HAiFLS_for)
  Psi.mixed52 = list(formula = ~pctpool+BrBnk+pctShade+HAiFLS_for)
  Psi.mixed53 = list(formula = ~pctpool+BRT_100m+BrBnk+HAiFLS_for)
  Psi.mixed54 = list(formula = ~pctpool+BRT_100m+pctShade+HAiFLS_for)
  #3 covariates
  Psi.avgT_pool_rck = list(formula = ~avgT+pctpool+pctrock)
  Psi.avgT_pool_brt = list(formula = ~avgT+pctpool+BRT_100m)
  Psi.avgT_rck_brt = list(formula = ~avgT+pctrock+BRT_100m)
  Psi.mixed55 = list(formula = ~avgT+pctShade+BrBnk)
  Psi.mixed56 = list(formula = ~pctpool+pctShade+BrBnk)
  Psi.mixed57 = list(formula = ~pctrock+pctShade+BrBnk)
  Psi.mixed58 = list(formula = ~BRT_100m+pctShade+BrBnk)
  Psi.mixed59 = list(formula = ~avgT+pctpool+pctShade)
  Psi.mixed60 = list(formula = ~pctpool+pctrock+pctShade)
  Psi.mixed61 = list(formula = ~pctrock+BRT_100m+pctShade)
  Psi.mixed62 = list(formula = ~avgT+BRT_100m+pctShade)
  Psi.mixed63 = list(formula = ~avgT+pctrock+pctShade)
  Psi.mixed64 = list(formula = ~pctpool+BRT_100m+pctShade)
  Psi.mixed65 = list(formula = ~avgT+pctpool+BrBnk) # Top Model
  Psi.mixed66 = list(formula = ~pctpool+pctrock+BrBnk)
  Psi.mixed67 = list(formula = ~pctrock+BRT_100m+BrBnk)
  Psi.mixed68 = list(formula = ~avgT+BRT_100m+BrBnk) #4th top model
  Psi.mixed69 = list(formula = ~avgT+pctrock+BrBnk)
  Psi.mixed70 = list(formula = ~pctpool+BRT_100m+BrBnk)
  Psi.mixed71 = list(formula = ~avgT+pctShade+HAiFLS_for)
  Psi.mixed72 = list(formula = ~pctpool+pctShade+HAiFLS_for)
  Psi.mixed73 = list(formula = ~pctrock+pctShade+HAiFLS_for)
  Psi.mixed74 = list(formula = ~BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed75 = list(formula = ~avgT+BrBnk+HAiFLS_for) #3rd top model
  Psi.mixed76 = list(formula = ~pctpool+BrBnk+HAiFLS_for)
  Psi.mixed77 = list(formula = ~pctrock+BrBnk+HAiFLS_for)
  Psi.mixed78 = list(formula = ~BRT_100m+BrBnk+HAiFLS_for)
  Psi.mixed79 = list(formula = ~avgT+pctpool+HAiFLS_for)
  Psi.mixed80 = list(formula = ~pctpool+pctrock+HAiFLS_for)
  Psi.mixed81 = list(formula = ~pctrock+BRT_100m+HAiFLS_for)
  Psi.mixed82 = list(formula = ~avgT+BRT_100m+HAiFLS_for)
  Psi.mixed83 = list(formula = ~avgT+pctrock+HAiFLS_for)
  Psi.mixed84 = list(formula = ~pctpool+BRT_100m+HAiFLS_for)
  Psi.indEff = list(formula = ~BrBnk+pctShade+HAiFLS_for)
  #2 covariates
  Psi.avgT_pool = list(formula = ~avgT + pctpool) 
  Psi.avgT_rock = list(formula = ~avgT + pctrock) 
  Psi.avgT_brt = list(formula = ~avgT + BRT_100m)
  Psi.avgT_bare = list(formula = ~avgT + BrBnk) #2nd top model 
  Psi.avgT_shade = list(formula = ~avgT + pctShade)
  Psi.pool_bare = list(formula = ~pctpool + BrBnk) 
  Psi.pool_shade = list(formula = ~pctpool + pctShade) 
  Psi.rock_bare = list(formula = ~pctrock + BrBnk) 
  Psi.rock_shade = list(formula = ~pctrock + pctShade) 
  Psi.BRT_bare = list(formula = ~BRT_100m + BrBnk) 
  Psi.BRT_shade = list(formula = ~BRT_100m + pctShade) 
  Psi.avgT_for = list(formula = ~avgT + HAiFLS_for)
  Psi.pool_for = list(formula = ~pctpool + HAiFLS_for)
  Psi.rock_for = list(formula = ~pctrock + HAiFLS_for) 
  Psi.BRT_for = list(formula = ~BRT_100m + HAiFLS_for) 
  Psi.BBnk_shade = list(formula = ~BrBnk+pctShade)
  Psi.BBnk_for = list(formula = ~BrBnk+HAiFLS_for)
  Psi.shade_for = list(formula = ~pctShade+HAiFLS_for)
  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  Psi.avgT = list(formula=~avgT) 
  Psi.pool = list(formula=~pctpool) 
  Psi.rock = list(formula=~pctrock) 
  Psi.bare = list(formula=~BrBnk) 
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
BKT.AICc.Table = model.table(bkt.results, use.lnl = T)
BKT.AICc.Table

class(BKT.AICc.Table)
getwd()
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
#write csv for model table
write.csv(BKT.AICc.Table, "Data/Thesis/Tidy/BrookTrout_OccuMod_Table.csv", row.names = F)

#look at summary of top model(s)

## Top model (dAIC=0) -- num parms = 6 
summary(bkt.results$p.tv.effort.Psi.mixed65) 
bkt.results$p.tv.effort.Psi.mixed65$results$real
## Second top model (dAIC=0.33) -- num parms = 5 (excludes pctpool from top model -- top most parsimonious model)
summary(bkt.results$p.tv.effort.Psi.avgT_bare) 
bkt.results$p.tv.effort.Psi.avgT_bare$results$real
## 3rd top model (dAIC=1.55) -- num parms = 6 (replaces pctpool in top model with HAiFLS_for)
summary(bkt.results$p.tv.effort.Psi.mixed75) 
bkt.results$p.tv.effort.Psi.mixed75$results$real
## 4th top model (dAIC=1.69) -- num parms = 6 (avgT+BrBank+BRT_100m)
summary(bkt.results$p.tv.effort.Psi.mixed68) 
bkt.results$p.tv.effort.Psi.mixed68$results$real
## 5th top model (dAIC=1.81) -- num parms = 6 (avgT+BrBank+BRT_100m)
summary(bkt.results$p.tv.effort.Psi.avgT) 
bkt.results$p.tv.effort.Psi.avgT$results$real

## Exploratory -- avgT+HAiFLS_for
#summary(bkt.results$p.tv.effort.Psi.avgT_for) 
#bkt.results$p.tv.effort.Psi.avgT_for$results$real



top.mod <- bkt.results$p.tv.effort.Psi.mixed65

cleanup(ask = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####  Visualizing avgT effect on psi  ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

#covariate.predictions method
min.temp <- min(brook.df$avgT)
max.temp <- max(brook.df$avgT)
temp.values <- seq(from = min.temp, to = max.temp, length = 100)
#
#min.for <- min(brook.df$HAiFLS_for)
#max.for <- max(brook.df$HAiFLS_for)
#for.values <- seq(from = min.for, to = max.for, length = 100)
min.bare <- min(brook.df$BrBnk)
max.bare <- max(brook.df$BrBnk)
bare.values <- seq(from = min.bare, to = max.bare, length = 100)
#
min.pool <- min(brook.df$pctpool)
max.pool <- max(brook.df$pctpool)
pool.values <- seq(from = min.pool, to = max.pool, length = 100)

bkt.ddl #par.index = 1, model.index = 4

###############################################################
#predict while holding other values constant (pctBrBnk)
##############################################################
#for.mean <- rep(mean(brook.df$HAiFLS_for), 100)
#for.minus <- rep(0, 100)
#for.plus <- rep(mean(brook.df$HAiFLS_for)+sd(brook.df$HAiFLS_for), 100)
summary(brook.df$BrBnk)
bare.mean <- rep(mean(brook.df$BrBnk), 100)
#bare.lower <- rep(0.4271, 100)
#bare.upper <- rep(1.197, 100)
pool.mean <- rep(mean(brook.df$pctpool), 100)

#predictions of Psi for full range of p21 & -1SD of forest values (would be negative so just forest=0)
#avgT.pred.lower <- covariate.predictions(top.mod, 
                                  #data = data.frame(avgT = temp.values,
                                                    #HAiFLS_for = for.minus,
                                  #                  BrBnk = bare.lower),
                                  #indices = 4)

#head(avgT.pred.lower$estimates)

#predictions of Psi for full range of avgT & mean values of Brbnk & pctpool 
avgT.pred.mean <- covariate.predictions(top.mod, 
                                        data = data.frame(avgT = temp.values,
                                                          BrBnk = bare.mean,
                                                          pctpool = pool.mean),
                                        indices = 4)

head(avgT.pred.mean$estimates)

#predictions of Psi for full range of p21 & +1SD of forest values 
#avgT.pred.upper <- covariate.predictions(top.mod, 
#                                        data = data.frame(avgT = temp.values,
#                                                          pctBrBnk = bare.upper),
#                                        indices = 4)

#head(avgT.pred.upper$estimates)

#Psi.Predictions.avgT <- rbind(avgT.pred.lower$estimates, avgT.pred.mean$estimates, avgT.pred.upper$estimates)%>%
#  select(avgT, pctBrBnk, estimate, se, lcl, ucl)%>%
#  round(digits = 4)
#head(Psi.Predictions.avgT)

##########################################################
#predict while holding one value constant (temp this time)
##########################################################
summary(brook.df$avgT)
temp.mean <- rep(mean(brook.df$avgT), 100)
#temp.lower <- rep(14.391, 100)
#temp.upper <- rep(16.790, 100)

#predictions of Psi for full range of BrBnk
#bare.pred.lower <- covariate.predictions(top.mod, 
#                                        data = data.frame(avgT = temp.lower,
#                                                          pctBrBnk = bare.values),
#                                        indices = 4)

#head(bare.pred.lower$estimates)

#predictions of Psi for full range of bare & mean of other values 
bare.pred.mean <- covariate.predictions(top.mod, 
                                       data = data.frame(avgT = temp.mean,
                                                         BrBnk = bare.values,
                                                         pctpool = pool.mean),
                                       indices = 4)

head(bare.pred.mean$estimates)

#predictions of Psi for full range of HAiFLS_for & +1SD of pctex21 values
#bare.pred.upper <- covariate.predictions(top.mod, 
                                       #data = data.frame(avgT = temp.upper,
                                      #                   pctBrBnk = bare.values),
                                      # indices = 4)

#head(bare.pred.upper$estimates)

#Psi.Predictions.bare <- rbind(bare.pred.lower$estimates, bare.pred.mean$estimates, bare.pred.upper$estimates)%>%
#  select(avgT, pctBrBnk, estimate, se, lcl, ucl)%>%
#  round(digits = 4)

#predictions of Psi for full range of pool & mean of other values 
pool.pred.mean <- covariate.predictions(top.mod, 
                                        data = data.frame(avgT = temp.mean,
                                                          BrBnk = bare.mean,
                                                          pctpool = pool.values),
                                        indices = 4)

head(pool.pred.mean$estimates)

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################

# Convert to df
avgT.predictions <- as.data.frame(avgT.pred.mean) #temp
bare.predictions <- as.data.frame(bare.pred.mean) #bare
pool.predictions <- as.data.frame(pool.pred.mean) #pool



setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(avgT.predictions, "Data/Thesis/Tidy/Psi_predictions_avgT.csv")
write_csv(bare.predictions, "Data/Thesis/Tidy/Psi_predictions_BrBnk.csv")
write_csv(pool.predictions, "Data/Thesis/Tidy/Psi_predictions_pctpool.csv")




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

head(p.pred.effort$estimates)


P.predictions.effort <- p.pred.effort$estimates %>%
  select(covdata, estimate, se, lcl, ucl) %>%
  rename(Effort_sec = covdata) %>%
  round(digits = 4)

####################################################
##       Write tidy csv for P predictions         ## 
####################################################
write_csv(P.predictions.effort, "Data/Thesis/Tidy/P_predictions_effort.csv")










library(cowplot)
#########################################################################################################
pred.temps <- as.data.frame(avgT.pred.mean) %>%
  select(avgT=estimates.avgT, BrBnk=estimates.BrBnk, pctpool=estimates.pctpool,
         estimate=estimates.estimate, se=estimates.se, lcl=estimates.lcl,
         ucl=estimates.ucl)
head(pred.temps)


#Make ggplot for predicted occupancy probabilies 
Psi1 <- ggplot(data = pred.temps, aes(x=avgT))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), colour="black", size=1)+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00))+
  labs(x="Average Summer Stream Temperature (°C)",
       y="Occupancy Probability")+
  theme_bw()+
  theme(axis.title = element_text(face = "bold"))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))
Psi1
###############################################################################
pred.bare <- as.data.frame(bare.pred.mean) %>%
  select(avgT=estimates.avgT, BrBnk=estimates.BrBnk, pctpool=estimates.pctpool,
         estimate=estimates.estimate, se=estimates.se, lcl=estimates.lcl,
         ucl=estimates.ucl)
head(pred.bare)

Psi2 <- ggplot(data = pred.bare, aes(x=BrBnk))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), colour="black", size=1)+
  scale_y_continuous(breaks = c(0.00,0.10,0.20, 0.30, 0.40),
                     labels = c("0.00","0.10","0.20","0.30","0.40"))+
  labs(x="Bare Bank Index",
       y=NULL)+
  theme_bw()+
  theme(axis.title = element_text(face = "bold"))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))
Psi2

###############################################################################
pred.pool <- as.data.frame(pool.pred.mean) %>%
  select(avgT=estimates.avgT, BrBnk=estimates.BrBnk, pctpool=estimates.pctpool,
         estimate=estimates.estimate, se=estimates.se, lcl=estimates.lcl,
         ucl=estimates.ucl)
head(pred.pool)

Psi3 <- ggplot(data = pred.pool, aes(x=pctpool))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), colour="black", size=1)+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00))+
  labs(x="Percent Pool Macrohabitat",
       y=NULL)+
  theme_bw()+
  theme(axis.title = element_text(face = "bold"))+
  theme(panel.grid = element_blank())+
  theme(strip.text.x = element_text(size=10,face = "bold"))
Psi3

#cowplot
plot_grid(Psi1,Psi3,Psi2, align = "h", labels = c(NA,NA,NA), nrow = 1)

ggsave("bkt_OccuProb_AvgT_Bnk_Pool.png",
       dpi = 350)


############################










