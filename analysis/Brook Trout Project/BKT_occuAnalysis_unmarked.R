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


##--------------------------------------------------------------------------------------------------------------------------------##
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
pairs(covs2)

#standardize covars
covs.scaled <- psycho::standardize(as.data.frame(covs2))
temp21 <- covs.scaled$pctex21
forest <- covs.scaled$HAiFLS_for
effort <- as.matrix(covs.scaled[,3:5])

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
summary(fm7 <- occu(~effort~temp21+forest, data = umf))

#get estimates on probability scale
backTransform(fm7, "state") #psi estimate
backTransform(fm7, "det") #p estimate

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

orig.temp21 <- seq(0,30,1)
orig.forest <- seq(0,100,1)

tp <- psycho::standardize(orig.temp21)
fp <- psycho::standardize(orig.forest)

newData <- data.frame(temp21=tp, forest=0)
pred.occ.temp21 <- predict(fm7, type="state", newdata=newData, appendData=T)
newData2 <- data.frame(temp21=0, forest=fp)
pred.occ.forest <- predict(fm7, type="state", newdata=newData2, appendData=T)

plot(pred.occ.temp21[[1]]~orig.temp21, type="l", lwd=3, col="blue", ylim=c(0,1))
matlines(orig.temp21,pred.occ.temp21[,3:4], lty = 1, lwd = 1, col = "grey")

#----------------#
#correlation test
#----------------#
c <- cor(brook2[,4:18])
head(round(c,2)) 

#round down
cround <- round(c,3)

#visualize these correlations
corrplot(c, method = "number")

# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(c)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(brook3[,4:18])

#correlogram
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(c, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)

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