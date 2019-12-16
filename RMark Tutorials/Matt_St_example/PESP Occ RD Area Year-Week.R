
##~~~~~~~~~~~~~~~~~~~~~~##
#### Set up workspace ####
##~~~~~~~~~~~~~~~~~~~~~~##

library(RMark)
library(beepr)
library(dplyr)
library(tidyr)
#data("salamander")
#data("RDSalamander") #For example inp format
#data(RDOccupancy)

rm(list = ls())
cleanup(ask=FALSE)

##~~~~~~~~~~~~~~~~~##
#### Import data ####
##~~~~~~~~~~~~~~~~~##

dataset_coverboard_area_inp_PESP_weekly <- readRDS(file="L:/STRIPS Birds/Analysis/STRIPS2Schulte/data/dataset_coverboard_area_inp_PESP_weekly.rda")
load(file="L:/STRIPS Birds/Analysis/STRIPS2Schulte/data/dataset_coverboard_area_patch_metrics.rda")
load(file="L:/STRIPS Birds/Analysis/STRIPS2Schulte/data/dataset_coverboard_landcover_areas.rda")
#load(file="L:/STRIPS Birds/Analysis/STRIPS2Schulte/data/dataset_coverboard_captures.rda")
load(file="L:/STRIPS Birds/Analysis/STRIPS2Schulte/data/CoverboardID.rda")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Flattening landcover datasets ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
dataset_coverboard_landcover_areas <- left_join(dataset_coverboard_landcover_areas, subset(CoverboardID, select=c(coverboardID, coverboard_sample_unitID)), by="coverboardID")

dataset_coverboard_landcover_areas <- dataset_coverboard_landcover_areas %>%
  select(-coverboardID) %>%
  group_by(coverboard_sample_unitID) %>%
  summarize_all(funs(mean), na.rm=T)

inp.file <- left_join(dataset_coverboard_area_inp_PESP_weekly, dataset_coverboard_area_patch_metrics, by="coverboard_sample_unitID")

# Replace NA values in landcover areas with column means
areas <- left_join(subset(inp.file, select=c(coverboard_sample_unitID)), dataset_coverboard_landcover_areas)
for(i in 2:ncol(areas)){
  areas[is.na(areas[,i]), i] <- mean(areas[,i], na.rm = TRUE)
}

inp.file <- left_join(inp.file, areas, by="coverboard_sample_unitID")



##~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Variables of interest ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#p
#site
#treatment
#area (sq meters)
#effort (board flips)
#time since sunrise           #problem with missing values
#temperature                  #need to calculate
#cloudiness                   #need to calculate
#Psi
#site
#treatment
#area (sq meters)
#PAR

##~~~~~~~~~~~~~~~~~##
#### Design data ####
##~~~~~~~~~~~~~~~~~##

# Determining time intervals
season.dates <- readRDS(file="L:/STRIPS Birds/Analysis/STRIPS2Schulte/data/coverboard_occupancy_season_dates.rda")
sp <- "PESP" # Species of interest

time_intervals = unlist(list(rep(0,season.dates$week_count[season.dates$species_abbreviation==sp & season.dates$field_season==2015]-1), 
                             1, 
                             rep(0,season.dates$week_count[season.dates$species_abbreviation==sp & season.dates$field_season==2016]-1),
                             1,
                             rep(0,season.dates$week_count[season.dates$species_abbreviation==sp & season.dates$field_season==2017]-1),
                             1, 
                             rep(0,season.dates$week_count[season.dates$species_abbreviation==sp & season.dates$field_season==2018]-1)))

nn=process.data(inp.file, model="RDOccupEG", groups=c("LCov"), 
                time.intervals=time_intervals, begin.time=2015)
nn.ddl=make.design.data(nn)

# Still need to figure out how to make LCov group time-varying





##~~~~~~~~~~~~~##
#### Round 1 ####
##~~~~~~~~~~~~~##

start.time <- Sys.time()
run.occ=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~~~ Occupancy ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1)
  #Psi.Site       = list(formula=~site)
  Psi.LCov       = list(formula=~LCov)
  Psi.Yrs  = list(formula=~yrs)
  Psi.Lyr  = list(formula=~lyr)
  
  Psi.PAR10m   = list(formula=~PAR10m)
  Psi.Area2m   = list(formula=~ara2m)
  Psi.LArea10m = list(formula=~lar10m)
  Psi.Hdiv500  = list(formula=~hdiv5m)
  #~~~~~~~~~~~ Detection Probability ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p.Dot     = list(formula=~1)
  p.Time    = list(formula=~Time)
  p.Session = list(formula=~session)
  p.Sunmins = list(formula=~sun)
  p.Tmp     = list(formula=~tmp)
  p.Cld     = list(formula=~cld)
  p.Rain    = list(formula=~pcp)
  p.Vst     = list(formula=~vst)
  # p models that are also Psi models
  #p.Site       = list(formula=~site)
  p.LCov       = list(formula=~LCov)
  p.PAR6m      = list(formula=~PAR6m)
  p.Yrs        = list(formula=~yrs)
  p.Lyr        = list(formula=~lyr)
  p.LogArea10m = list(formula=~lar10m)
  p.Area10m    = list(formula=~ara10m)
  p.Hdiv100    = list(formula=~hdiv1m)
  #~~~~~~~~~~~ Colonization/Extinction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Gamma.Dot     = list(formula=~1)
  Gamma.PAR8m   = list(formula=~PAR8m)
  Epsilon.Dot   = list(formula=~1)
  Epsilon.PAR0m = list(formula=~PAR0m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml=create.model.list("RDOccupEG")
  results=mark.wrapper(cml, data=nn, ddl=nn.ddl, adjust=F, output=F, threads=-1)
  return(results)
}

occ.results=run.occ()
occ.results.initial <- occ.results

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Checking model results ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
occ.results$model.table <- model.table(occ.results, use.lnl=TRUE, model.name=F)
model.table <- model.table(occ.results, use.lnl=TRUE, model.name=F)
model.names <- as.character(occ.results$model.table$model) # Create the list of model names to iterate over

# Create a data frame to hold the results of tests
model.validation <- data.frame(model = model.names, CI.cross.zero = rep(TRUE, length(model.names)), SE.zero = rep(FALSE, length(model.names)))

# Test confidence intervals for all parameters in every model
for (i in model.names){
  if(isTRUE(all.equal(sign(occ.results[[i]]$results$beta$lcl), sign(occ.results[[i]]$results$beta$ucl)))){
    model.validation$CI.cross.zero[model.validation$model==i] <- FALSE
  }
}

# Test standard errors for every parameter in every model
for (i in model.names){
  if (0 %in% occ.results[[i]]$results$beta$se){
    model.validation$SE.zero[model.validation$model==i] <- TRUE
  }
}

model.table <- left_join(model.table, model.validation, by="model")
valid.models <- model.table[model.table$CI.cross.zero==F,] # Filtering for only well-defined models

# Adjusting delta AIC values
if(nrow(valid.models)>1){
  valid.models$DeltaAICc[1] <- 0
  for (i in 2:nrow(valid.models)){
    valid.models$DeltaAICc[i] = valid.models$AICc[i] - valid.models$AICc[1]
  }
}

# Present results for review
end.time <- Sys.time()
runtime <- end.time - start.time
runtime
beep("fanfare")


View(valid.models)

View(model.table)

# Run 1
occ.results.1 <- occ.results
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.Lyr$results$real # Model 1 2276.977 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.Lyr$results$real # Model 2 2293.680 AICc
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.Yrs$results$real # Model 3 2328.402 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.Yrs$results$real # Model 4 2348.180 AICc
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.Time$results$real # Model 5 2400.939 AICc

# Run 1
occ.results.1 <- occ.results
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.Lyr$results$real # Model 1 2276.977 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.Lyr$results$real # Model 2 2293.680 AICc
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.Yrs$results$real # Model 3 2328.402 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.Yrs$results$real # Model 4 2348.180 AICc
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.Time$results$real # Model 5 2400.939 AICc

# All models the same in run #2! Proceed to round 2!

##~~~~~~~~~~~~~##
#### Round 2 ####
##~~~~~~~~~~~~~##

start.time <- Sys.time()
run.occ=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Psi.Lyr  = list(formula=~lyr)
  Epsilon.Dot   = list(formula=~1)
  Gamma.Dot     = list(formula=~1)
  p.Lyr        = list(formula=~lyr)
  #~~~~~~~~~~~~~ Occupancy ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1)
  Psi.LCov       = list(formula=~LCov)
  Psi.Yrs  = list(formula=~yrs)
  Psi.PAR10m   = list(formula=~PAR10m)
  Psi.Area2m   = list(formula=~ara2m)
  Psi.LArea10m = list(formula=~lar10m)
  Psi.Hdiv500  = list(formula=~hdiv5m)
  #~~~~~~~~~~~~~ Occupancy 2 variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Psi.LyrLCov       = list(formula=~lyr + LCov)
  Psi.LyrYrs  = list(formula=~lyr + yrs)
  Psi.LyrPAR10m   = list(formula=~lyr + PAR10m)
  Psi.LyrArea2m   = list(formula=~lyr + ara2m)
  Psi.LyrLArea10m = list(formula=~lyr + lar10m)
  Psi.LyrHdiv500  = list(formula=~lyr + hdiv5m)
  #~~~~~~~~~~~ Detection Probability ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p.Dot     = list(formula=~1)
  p.Time    = list(formula=~Time)
  p.Session = list(formula=~session)
  p.Sunmins = list(formula=~sun)
  p.Tmp     = list(formula=~tmp)
  p.Cld     = list(formula=~cld)
  p.Rain    = list(formula=~pcp)
  p.Vst     = list(formula=~vst)
  # p models that are also Psi models
  p.LCov       = list(formula=~LCov2018)
  p.PAR6m      = list(formula=~PAR6m)
  p.Yrs        = list(formula=~yrs)
  p.LogArea10m = list(formula=~lar10m)
  p.Area10m    = list(formula=~ara10m)
  p.Hdiv100    = list(formula=~hdiv1m)
  #~~~~~~~~~~~ Detection Probability 2 variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p.LyrTime    = list(formula=~lyr + Time)
  p.LyrSession = list(formula=~lyr + session)
  p.LyrSunmins = list(formula=~lyr + sun)
  p.LyrTmp     = list(formula=~lyr + tmp)
  p.LyrCld     = list(formula=~lyr + cld)
  p.LyrRain    = list(formula=~lyr + pcp)
  p.LyrVst     = list(formula=~lyr + vst)
  # p models that are also Psi models
  p.LyrLCov       = list(formula=~lyr + LCov)
  p.LyrPAR6m      = list(formula=~lyr + PAR6m)
  p.LyrYrs        = list(formula=~lyr + yrs)
  p.LyrLogArea10m = list(formula=~lyr + lar10m)
  p.LyrArea10m    = list(formula=~lyr + ara10m)
  p.LyrHdiv100    = list(formula=~lyr + hdiv1m)
  #~~~~~~~~~~~ Colonization/Extinction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Gamma.PAR8m   = list(formula=~PAR8m)
  Epsilon.PAR0m = list(formula=~PAR0m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml=create.model.list("RDOccupEG")
  results=mark.wrapper(cml, data=nn, ddl=nn.ddl, adjust=F, output=F, threads=-1)
  return(results)
}

occ.results=run.occ()
occ.results.initial <- occ.results

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Checking model results ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
occ.results$model.table <- model.table(occ.results, use.lnl=TRUE, model.name=F)
model.table <- model.table(occ.results, use.lnl=TRUE, model.name=F)
model.names <- as.character(occ.results$model.table$model) # Create the list of model names to iterate over

# Create a data frame to hold the results of tests
model.validation <- data.frame(model = model.names, CI.cross.zero = rep(TRUE, length(model.names)), SE.zero = rep(FALSE, length(model.names)))

# Test confidence intervals for all parameters in every model
for (i in model.names){
  if(isTRUE(all.equal(sign(occ.results[[i]]$results$beta$lcl), sign(occ.results[[i]]$results$beta$ucl)))){
    model.validation$CI.cross.zero[model.validation$model==i] <- FALSE
  }
}

# Test standard errors for every parameter in every model
for (i in model.names){
  if (0 %in% occ.results[[i]]$results$real$se){
    model.validation$SE.zero[model.validation$model==i] <- TRUE
  }
}

model.table <- left_join(model.table, model.validation, by="model")
valid.models <- model.table[model.table$CI.cross.zero==F,] # Filtering for only well-defined models

# Adjusting delta AIC values
if(nrow(valid.models)>1){
  valid.models$DeltaAICc[1] <- 0
  for (i in 2:nrow(valid.models)){
    valid.models$DeltaAICc[i] = valid.models$AICc[i] - valid.models$AICc[1]
  }
}

# Present results for review
end.time <- Sys.time()
runtime <- end.time - start.time
runtime
beep("fanfare")


# Run 1
occ.results.2.1 <- occ.results
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.LyrVst$results$real # Model 1 2268.228 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.LyrVst$results$real # Model 2 2282.155 AICc
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.Lyr$results$real # Model 3 2291.578 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.Lyr$results$real # Model 4 2305.840 AICc
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.Yrs$results$real # Model 5 2342.215 AICc


##~~~~~~~~~~~~~##
#### Round 3 ####
##~~~~~~~~~~~~~##

start.time <- Sys.time()
run.occ=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Psi.Lyr      = list(formula=~lyr) # Best Psi round 1 & 2
  Epsilon.Dot  = list(formula=~1)   # Best Epsilon round 1 & 2
  Gamma.Dot    = list(formula=~1)   # Best Gamma round 1 & 2
  p.Lyr        = list(formula=~lyr) # Best p round 1
  p.LyrVst     = list(formula=~lyr + vst) # Best p round 2
  #~~~~~~~~~~~~~ Occupancy ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1)
  Psi.LCov       = list(formula=~LCov)
  Psi.Yrs  = list(formula=~yrs)
  Psi.PAR10m   = list(formula=~PAR10m)
  Psi.Area2m   = list(formula=~ara2m)
  Psi.LArea10m = list(formula=~lar10m)
  Psi.Hdiv500  = list(formula=~hdiv5m)
  #~~~~~~~~~~~~~ Occupancy 2 variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Psi.LyrLCov       = list(formula=~lyr + LCov)
  Psi.LyrYrs  = list(formula=~lyr + yrs)
  Psi.LyrPAR10m   = list(formula=~lyr + PAR10m)
  Psi.LyrArea2m   = list(formula=~lyr + ara2m)
  Psi.LyrLArea10m = list(formula=~lyr + lar10m)
  Psi.LyrHdiv500  = list(formula=~lyr + hdiv5m)
  #~~~~~~~~~~~ Detection Probability ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability 3 variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p.VstLyrTime    = list(formula=~vst + lyr + Time)
  p.VstLyrSession = list(formula=~vst + lyr + session)
  p.VstLyrSunmins = list(formula=~vst + lyr + sun)
  p.VstLyrTmp     = list(formula=~vst + lyr + tmp)
  p.VstLyrCld     = list(formula=~vst + lyr + cld)
  p.VstLyrRain    = list(formula=~vst + lyr + pcp)

  # p models that are also Psi models
  p.VstLyrLCov       = list(formula=~vst + lyr + LCov)
  p.VstLyrPAR6m      = list(formula=~vst + lyr + PAR6m)
  p.VstLyrLogArea10m = list(formula=~vst + lyr + lar10m)
  p.VstLyrArea10m    = list(formula=~vst + lyr + ara10m)
  p.VstLyrHdiv100    = list(formula=~vst + lyr + hdiv1m)
  #~~~~~~~~~~~ Colonization/Extinction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Gamma.PAR8m   = list(formula=~PAR8m)
  Epsilon.PAR0m = list(formula=~PAR0m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml=create.model.list("RDOccupEG")
  results=mark.wrapper(cml, data=nn, ddl=nn.ddl, adjust=F, output=F, threads=-1)
  return(results)
}

occ.results=run.occ()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Checking model results ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
occ.results$model.table <- model.table(occ.results, use.lnl=TRUE, model.name=F)
model.table <- model.table(occ.results, use.lnl=TRUE, model.name=F)
model.names <- as.character(occ.results$model.table$model) # Create the list of model names to iterate over

# Create a data frame to hold the results of tests
model.validation <- data.frame(model = model.names, CI.cross.zero = rep(TRUE, length(model.names)), SE.zero = rep(FALSE, length(model.names)))

# Test confidence intervals for all parameters in every model
for (i in model.names){
  if(isTRUE(all.equal(sign(occ.results[[i]]$results$beta$lcl), sign(occ.results[[i]]$results$beta$ucl)))){
    model.validation$CI.cross.zero[model.validation$model==i] <- FALSE
  }
}

# Test standard errors for every parameter in every model
for (i in model.names){
  if (0 %in% occ.results[[i]]$results$real$se){
    model.validation$SE.zero[model.validation$model==i] <- TRUE
  }
}

model.table <- left_join(model.table, model.validation, by="model")
valid.models <- model.table[model.table$CI.cross.zero==F,] # Filtering for only well-defined models

# Adjusting delta AIC values
if(nrow(valid.models)>1){
  valid.models$DeltaAICc[1] <- 0
  for (i in 2:nrow(valid.models)){
    valid.models$DeltaAICc[i] = valid.models$AICc[i] - valid.models$AICc[1]
  }
}

# Present results for review
end.time <- Sys.time()
runtime <- end.time - start.time
runtime
beep("fanfare")


# Run 3.1
occ.results.3.1 <- occ.results
models.round.3.1 <- as.character(valid.models$model)

occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.VstLyrTime$results$real # Model 1 2256.242 AICc
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.LyrVst$results$real     # Model 2 2268.228 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.VstLyrTime$results$real # Model 3 2270.429 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.LyrVst$results$real     # Model 4 2282.155 AICc
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.Lyr$results$real        # Model 5 2291.578 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.Lyr$results$real        # Model 6  2305.840AICc



##~~~~~~~~~~~~~##
#### Round 4 ####
##~~~~~~~~~~~~~##

start.time <- Sys.time()
run.occ=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Psi.Lyr      = list(formula=~lyr) # Best Psi round 1 & 2 & 3
  Epsilon.Dot  = list(formula=~1)   # Best Epsilon round 1 & 2 & 3
  Gamma.Dot    = list(formula=~1)   # Best Gamma round 1 & 2 & 3
  p.Lyr        = list(formula=~lyr) # Best p round 1
  p.LyrVst     = list(formula=~lyr + vst) # Best p round 2
  p.VstLyrTime    = list(formula=~vst + lyr + Time) # Best p round 3
  #~~~~~~~~~~~~~ Occupancy ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot      = list(formula=~1)
  Psi.LCov     = list(formula=~LCov)
  Psi.Yrs      = list(formula=~yrs)
  Psi.PAR10m   = list(formula=~PAR10m)
  Psi.Area2m   = list(formula=~ara2m)
  Psi.LArea10m = list(formula=~lar10m)
  Psi.Hdiv500  = list(formula=~hdiv5m)
  #~~~~~~~~~~~~~ Occupancy 2 variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Psi.LyrLCov     = list(formula=~lyr + LCov)
  Psi.LyrYrs      = list(formula=~lyr + yrs)
  Psi.LyrPAR10m   = list(formula=~lyr + PAR10m)
  Psi.LyrArea2m   = list(formula=~lyr + ara2m)
  Psi.LyrLArea10m = list(formula=~lyr + lar10m)
  Psi.LyrHdiv500  = list(formula=~lyr + hdiv5m)
  #~~~~~~~~~~~ Detection Probability ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability 3 variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p.VstLyrSession = list(formula=~vst + lyr + session)
  p.VstLyrSunmins = list(formula=~vst + lyr + sun)
  p.VstLyrTmp     = list(formula=~vst + lyr + tmp)
  p.VstLyrCld     = list(formula=~vst + lyr + cld)
  p.VstLyrRain    = list(formula=~vst + lyr + pcp)
  p.VstLyrLCov       = list(formula=~vst + lyr + LCov)
  p.VstLyrPAR6m      = list(formula=~vst + lyr + PAR6m)
  p.VstLyrLogArea10m = list(formula=~vst + lyr + lar10m)
  p.VstLyrArea10m    = list(formula=~vst + lyr + ara10m)
  p.VstLyrHdiv100    = list(formula=~vst + lyr + hdiv1m)
  #~~~~~~~~~~~ Detection Probability 4 variables ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  p.TimeVstLyrSession = list(formula=~Time + vst + lyr + session)
  p.TimeVstLyrSunmins = list(formula=~Time + vst + lyr + sun)
  p.TimeVstLyrTmp     = list(formula=~Time + vst + lyr + tmp)
  p.TimeVstLyrCld     = list(formula=~Time + vst + lyr + cld)
  p.TimeVstLyrRain    = list(formula=~Time + vst + lyr + pcp)
  p.TimeVstLyrLCov       = list(formula=~Time + vst + lyr + LCov2018)
  p.TimeVstLyrPAR6m      = list(formula=~Time + vst + lyr + PAR6m)
  p.TimeVstLyrLogArea10m = list(formula=~Time + vst + lyr + lar10m)
  p.TimeVstLyrArea10m    = list(formula=~Time + vst + lyr + ara10m)
  p.TimeVstLyrHdiv100    = list(formula=~Time + vst + lyr + hdiv1m)
  #~~~~~~~~~~~ Colonization/Extinction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  Gamma.PAR8m   = list(formula=~PAR8m)
  Epsilon.PAR0m = list(formula=~PAR0m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml=create.model.list("RDOccupEG")
  results=mark.wrapper(cml, data=nn, ddl=nn.ddl, adjust=F, output=F, threads=-1)
  return(results)
}

occ.results=run.occ()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Checking model results ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
occ.results$model.table <- model.table(occ.results, use.lnl=TRUE, model.name=F)
model.table <- model.table(occ.results, use.lnl=TRUE, model.name=F)
model.names <- as.character(occ.results$model.table$model) # Create the list of model names to iterate over

# Create a data frame to hold the results of tests
model.validation <- data.frame(model = model.names, CI.cross.zero = rep(TRUE, length(model.names)), SE.zero = rep(FALSE, length(model.names)))

# Test confidence intervals for all parameters in every model
for (i in model.names){
  if(isTRUE(all.equal(sign(occ.results[[i]]$results$beta$lcl), sign(occ.results[[i]]$results$beta$ucl)))){
    model.validation$CI.cross.zero[model.validation$model==i] <- FALSE
  }
}

# Test standard errors for every parameter in every model
for (i in model.names){
  if (0 %in% occ.results[[i]]$results$real$se){
    model.validation$SE.zero[model.validation$model==i] <- TRUE
  }
}

model.table <- left_join(model.table, model.validation, by="model")
valid.models <- model.table[model.table$CI.cross.zero==F,] # Filtering for only well-defined models

# Adjusting delta AIC values
if(nrow(valid.models)>1){
  valid.models$DeltaAICc[1] <- 0
  for (i in 2:nrow(valid.models)){
    valid.models$DeltaAICc[i] = valid.models$AICc[i] - valid.models$AICc[1]
  }
}

# Present results for review
end.time <- Sys.time()
runtime <- end.time - start.time
runtime
beep("fanfare")


# Run 4.1
occ.results.4.1 <- occ.results
models.round.4.1 <- as.character(valid.models$model)

occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.VstLyrTime$results$real # Model 1 2256.242 AICc # Best model 2 rounds in a row!
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.LyrVst$results$real     # Model 2 2268.228 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.VstLyrTime$results$real # Model 3 2270.429 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.LyrVst$results$real     # Model 4 2282.155 AICc
occ.results$Psi.Lyr.Epsilon.Dot.Gamma.Dot.p.Lyr$results$real        # Model 5 2291.578 AICc
occ.results$Psi.Yrs.Epsilon.Dot.Gamma.Dot.p.Lyr$results$real        # Model 6 2305.840 AICc



