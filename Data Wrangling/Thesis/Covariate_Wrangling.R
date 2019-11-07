###################################################
###################################################

# Create Centralized dataset for occupancy analysis

###################################################
###################################################

#combine all habitat variables into wide-format dataset
# pulling data from three sources: in-situ habitat classification, in-stream temperature summaries,
# and GIS-derived geospatial data

library(tidyverse)
library(skimr)
library(forcats)
library(ggplot2)

#in-stream variables
getwd()
local <- read_csv("Data/Thesis/Tidy/tidyhabitat1.csv", col_names = T)

#temperature
temp <- read_csv("Data/Thesis/Tidy/TmpVars_all.csv", col_names = T)

#landscape
gis <- read_csv("Data/Thesis/Tidy/LandscapeCovars_tidy.csv", col_names = T)

#-------------------------------------------------------------------------------------------------

# These three data sets have slight differences in column structure, specifically in "uid" columns 
# and the resulting columns of uniting "HUC8" and "Site" 
# additionally, the data sets from gis have "78b" as "7878" and etc. due to how it reads in data

# We need one common field across all three to use as a joining factor, so let's work on this now
# uid seems the "best" because it is an integer, but "HucSite" seems easiest to do


#create "HUC_Site" 

#gis data
class(gis$Site)
gis$Site <- as.character(gis$Site)
gis[11,3] <- '57b'
gis[28,3] <- '32b'
gis[42,3] <- '14b'
gis[57,3] <- '78b'
gis[95,3] <- '118b'
gis[132,3] <- '28b'

#delete columns we will not use for analysis
names(gis)
g2 <- gis %>%
  select(-uid, -YrSmpld)%>%
  unite(HUC_Site, c(HUC8, Site), sep = "_", remove = T)
g2[125,1] <- "YEL_97b"

##########
#temp data
##########
names(temp)
t2 <- temp %>%
  select(-uid, -HUCsite)%>%
  unite(HUC_Site, c(HUC8, Site), sep = "_", remove = T)
t2[122,1] <- "YEL_97b"
t2[122,]

###########
#local Data
###########
local[46,3] <- 201
local[47,3] <- 202
l2 <- local %>%
  unite(HUC_Site, c(HUC8, Site), sep = "_", remove = F)

########
# Join
########

big1 <- left_join(l2, t2, by = "HUC_Site")
big2 <- left_join(big1, g2, by = "HUC_Site")

##########
#Write csv
##########

write.csv(big2, "Data/Thesis/Tidy/AllCovariates.csv", row.names = F)


#---------------------------------------------------------------------------------------------------

# Now we should trim down our habitat data set and remove columns not to be used in analysis
# Then organize in order by type of covariate: local (abiotic and biotic), landscape

#compare with fish data
fish <- read_csv("Data/Thesis/Tidy/tidyfish1.csv", col_names = T)
#retain HUC8 and Site as indiv. columns

names(big2)
test <- big2 %>%
  select(`W/D`, `bnkahz%`)

envars <- big2 %>%
  select(uid, HUC8, Site, Order, SegLen, elev_m, RchLength, effsec, t1_eff, t2_eff, t3_eff, ph, do, temp, avgT, sdT, MAXT,
         MEANT, RNGT, pctex21, avwid, sdwid, t1_avwid, t2_avwid, t3_avwid, avdep, t1_avdep, t2_avdep, t3_avdep,
         maxdep, t1_maxdep, t2_maxdep, t3_maxdep, sdDep, `W/D`, mFlow, sdFlow, t1_flow, t2_flow, t3_flow, maxflow,
         t1_maxflow, t2_maxflow, t3_maxflow, pctRiffle, pctrun, pctslow, machabprop, pctclay, pctsilt, pctsand,
         pctfines, pctgrvl, pctcbbl, pctbldr, pctrock, subprop, pctEmb1, LWD, t1_LWD, t2_LWD, t3_LWD, FWD, under, 
         t1_under, t2_under, t3_under, boulder, depool, `bnkavr%`, Avbnka, `bnkbare%`,`AvChnlShd%`, `chshdSD%`,
         Sinuosity, pctSlope, CatArea_km2, HAiFLS_alt, HAiFLS_bmp, HAiFLS_ag, HAiFLS_dev, HAiFLS_for)%>%
  rename(RchLen = RchLength, pH = ph, DO = do, WidDep = `W/D`, pctVertbnk = `bnkavr%`, pctBrBnk = `bnkbare%`,
         pctShade = `AvChnlShd%`, pctShadeSD = `chshdSD%`, HAiFLS_nat = HAiFLS_bmp)

#explore data
skim(envars)

#missing values seem to be accounted for:
  # i.e. loggers were lost, or broken, 
  # or UPI_165 not included due to the site being thrown out. 

#Let's remove sites we won't analyze for various reasons
EnVars <- envars %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = F)%>%
  filter(!newID %in% c("UPI_29", "UPI_165", "YEL_33", "YEL_98"))%>%
  select(-newID)

#now let's check for NA's again
skim(EnVars)

## Only NA's are for missing temp logger data, and one missing Dissolved Oxygen datapoint
## For occupancy analysis we will plug in the overall mean of the data for these values


########################################################
#Replace NA's with averages of variable across all sites
########################################################

pctmiss = 8/138*100 #~6% of obs have data missing for instream temp variables (8/138)

notemp <- EnVars %>%
  filter(is.na(avgT))
means <- c(mean(EnVars$avgT, na.rm = T), mean(EnVars$sdT, na.rm = T), mean(EnVars$MAXT, na.rm = T), mean(EnVars$MEANT, na.rm = T),
           mean(EnVars$RNGT, na.rm = T), mean(EnVars$pctex21, na.rm = T))
means

names(EnVars)
EnVars[18,15:20] <- means
EnVars[28,15:20] <- means
EnVars[53,15:20] <- means
EnVars[58,15:20] <- means
EnVars[63,15:20] <- means
EnVars[70,15:20] <- means
EnVars[81,15:20] <- means
EnVars[83,15:20] <- means
skim(EnVars)

EnVars[115,13] <- mean(EnVars$DO, na.rm = T)
EnVars[115,]

################
#Write tidy csv
################
write.csv(EnVars, "Data/Thesis/Tidy/enviro_tidy.csv", row.names = F)


