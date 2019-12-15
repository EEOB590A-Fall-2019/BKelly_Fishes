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
library(skimr)
library(psycho)
library(cowplot)

##--------------------------------------------------------------------------------------------------------------------------------##
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
#read in data, rearrange and change some labels to work with grouping ("freq"), and time-varying covariates ("Effort1 --> Effort3")
brook <- read_csv("Data/Thesis/Tidy/BKT_Occu_File.csv", col_names = T)
brook.df <- as.data.frame(brook)
#examine
skim(brook.df)

#extract encounter history into three separate columns
y <- read_csv("Data/Thesis/Tidy/BKT_DetectionHist.csv", col_names = T)

y2 <- y %>%
  unite(newID, c(HUC8, site), sep = "_", remove = T)
y2[96,2] <- "YEL_97b" 

full.data <- full_join(brook.df, y2, by="newID")

#BKT det/nondet data in matrix format
y <- full.data %>%
  select(occ1, occ2, occ3) 
y <- as.matrix(y) 


#enviro covars
temp.og <- full.data[,"pctex21"] #unstandardised, original values of covariate
forest.og <- full.data[,"HAiFLS_for"] #unstandardised, original values of covariate

covs <- full.data %>%
  select(pctex21, HAiFLS_for)
covs <- as.matrix(covs) #site covars in matrix format

#detection covars
effort.og <- as.matrix(full.data[,4:6])
time <- matrix(as.character(1:3), nrow = 138, ncol = 3, byrow = T) #since we have three occasions

#overview of covars
covs2 <- cbind(covs, effort.og)
pairs(covs2, smooth=T, density=T)
#standardize covars
#covs.scaled <- psycho::standardize(as.data.frame(covs2))
#temp21 <- covs.scaled$pctex21
#forest <- covs.scaled$HAiFLS_for
#effort <- as.matrix(covs.scaled[,3:5])

#UNstandardize covars
temp21 <- temp.og
forest <- forest.og
effort <- effort.og



#format data and summarize
umf <- unmarkedFrameOccu(y=y, siteCovs = data.frame(temp21=temp21, forest=forest),
                         obsCovs = list(time=time, effort=effort))
summary(umf)


summary(fm1 <- occu(~1~1, data = umf))
summary(fm2 <- occu(~1~temp21, data = umf))
summary(fm3 <- occu(~effort~temp21, data = umf))
summary(fm4 <- occu(~1~forest, data = umf))
summary(fm5 <- occu(~effort~forest, data = umf))
summary(fm6 <- occu(~1~temp21+forest, data = umf))
summary(fm7 <- occu(~effort ~temp21+forest, data = umf))



#put fitted models in a "fitList" and rank by AIC
fms <- fitList(
  "p(.)Psi(.)"                  =fm1,
  "p(.)Psi(temp21)"             =fm2,
  "p(effort)Psi(temp21)"        =fm3,
  "p(.)Psi(forest)"             =fm4,
  "p(effort)Psi(forest)"        =fm5,
  "p(.)Psi(temp21+forest)"      =fm6,
  "p(effort)Psi(temp21+forest)" =fm7
  
)

(ms <- modSel(fms))

#zero pctex21
newdat <- data.frame(temp21=0, forest=c(min(forest.og), mean(forest.og), max(forest.og)))
predict(fm7, type="state", newdata=newdat, appendData=T)
#one pctex21
newdat <- data.frame(temp21=1, forest=c(min(forest.og), mean(forest.og), max(forest.og)))
predict(fm7, type="state", newdata=newdat, appendData=T)
#two pctex21
newdat <- data.frame(temp21=2, forest=c(min(forest.og), mean(forest.og), max(forest.og)))
predict(fm7, type="state", newdata=newdat, appendData=T)
#three pctex21
newdat <- data.frame(temp21=3, forest=c(min(forest.og), mean(forest.og), max(forest.og)))
predict(fm7, type="state", newdata=newdat, appendData=T)
#four pctex21
newdat <- data.frame(temp21=4, forest=c(min(forest.og), mean(forest.og), max(forest.og)))
predict(fm7, type="state", newdata=newdat, appendData=T)
#mean pctex21
newdat <- data.frame(temp21=mean(temp.og), forest=c(min(forest.og), mean(forest.og), max(forest.og)))
predict(fm7, type="state", newdata=newdat, appendData=T)
#five pctex21
newdat <- data.frame(temp21=5, forest=c(min(forest.og), mean(forest.og), max(forest.og)))
predict(fm7, type="state", newdata=newdat, appendData=T)
#ten pctex21
newdat <- data.frame(temp21=10, forest=c(min(forest.og), mean(forest.og), max(forest.og)))
predict(fm7, type="state", newdata=newdat, appendData=T)
#15 pctex21
newdat <- data.frame(temp21=20, forest=c(min(forest.og), mean(forest.og), max(forest.og)))
predict(fm7, type="state", newdata=newdat, appendData=T)
#20 pctex21
newdat <- data.frame(temp21=30, forest=c(min(forest.og), mean(forest.og), max(forest.og)))
predict(fm7, type="state", newdata=newdat, appendData=T)




#zero pctex21
newdat <- data.frame(temp21=0, forest=0)
predict(fm7, type="state", newdata=newdat, appendData=T)

mean(forest.og)-sd(forest.og)
sd(forest.og)
summary(forest.og)
hist(forest.og)

summary(forest.og)

newdat <- data.frame(temp21=seq(min(temp.og),max(temp.og),length.out = 100), forest=3.178)
pred.occ.p21.for_Q1 <- predict(fm7, type="state", newdata=newdat, appendData=T)

newdat1 <- data.frame(temp21=seq(min(temp.og),max(temp.og),length.out = 100), forest=mean(forest.og))
pred.occ.p21.for_mean <- predict(fm7, type="state", newdata=newdat1, appendData=T)

newdat2 <- data.frame(temp21=seq(min(temp.og),max(temp.og),length.out = 100), forest=48.403)
pred.occ.p21.for_Q3 <- predict(fm7, type="state", newdata=newdat2, appendData=T)

p <- ggplot(pred.occ.p21.for_Q1, aes(x=temp21, y=Predicted))+
  geom_line(size =1, color="blue")+
  geom_ribbon(data = pred.occ.p21.for_Q1, aes(ymin=lower, ymax=upper), alpha=0.1, colour=NA)+
  labs(x="% Summer Stream Temp > 21 (C)",
       y="Occupancy (Psi)")+
  theme_minimal_grid()+
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30))
p

p2 <- ggplot(data = pred.occ.p21.for_mean, aes(x=temp21))+
  geom_line(aes(y=Predicted), size=1, color="blue")+
  geom_ribbon(aes(ymin=pred.occ.p21.for_mean$lower, ymax=pred.occ.p21.for_mean$upper), alpha=0.1, color=NA)+
  labs(x="% Summer Stream Temp > 21 (C)",
       y="Occupancy (Psi)")+
  theme_minimal_grid()+
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30))
p2

p3 <- ggplot(data = pred.occ.p21.for_Q3, aes(x=temp21))+
  geom_line(aes(y=Predicted), size=1, color="blue")+
  geom_ribbon(aes(ymin=pred.occ.p21.for_mean$lower, ymax=pred.occ.p21.for_mean$upper), alpha=0.1, color=NA)+
  labs(x="% Summer Stream Temp > 21 (C)",
       y="Occupancy (Psi)")+
  theme_minimal_grid()+
  scale_x_continuous(breaks = c(0,5,10,15,20,25,30))
p3


plot_grid(p,p2,p3, labels = "AUTO", label_size = 12, nrow = 1)



par(mfrow=c(1,2), cex.lab=1.2)
mapPalette <- colorRampPalette(c("grey","yellow","orange","red"))
image(x=temp.og,y=forest.og)



library(lattice)

newdat3 <- data.frame(temp21=seq(min(temp.og),max(temp.og),length.out = 100),
                      forest=seq(min(forest.og), max(forest.og), length.out = 100))
pred.occ.p21.for <- predict(fm7, type="state", newdata=newdat3, appendData=T)
pred.occ.p21_for <- 
x = temp.og
y = forest.og
z = pred
levelplot(Predicted~temp21*forest, data = pred.occ.p21.for,
          xlab = "% Stream Temp > 21C", ylab = "% Forested Catchment")


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