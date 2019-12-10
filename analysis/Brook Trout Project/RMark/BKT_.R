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
library(psycho)
library(corrplot)

##Occupancy example
#?weta


##--------------------------------------------------------------------------------------------------------------------------------##
#read in data, rearrange and change some labels to work with grouping ("freq"), and time-varying covariates ("Effort1 --> Effort3")
brook <- read_csv("Data/Thesis/Tidy/BKT_Occu_File.csv", col_names = T)
brook.df <- as.data.frame(brook)
#examine
skim(brook.df)

#---------------------------------------------#
#standardize covariates to have center 0 --- Only effort variables (all high and 3-4 digits) (subtract mean and divide by sd)
#---------------------------------------------#
x <- brook.df %>%
  select(4:6) %>%
  psycho::standardize()
summary(x)
skim(x)

brook2 <- brook.df 
brook2[,4:6]=x


brook3 <- brook2 %>%
  mutate(left = "/*", right = "*/") %>%
  unite(comment, c(left,newID,right), sep = "", remove = F)
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
getwd()
#set wd to scratch folder because MARK outputs an insane amount of files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Analysis/Brook Trout Project/RMark/BrookTrout_ScratchFolder") #because MARK loves output files

#Process Data
#?process.data
#?make.design.data
brook.process = process.data(brook2, model="Occupancy", groups = "freq")
bkt.ddl = make.design.data(brook.process)


###~~~~~~~~~~~~~~~~~~~~~~~##
####   All covariates   ####
##~~~~~~~~~~~~~~~~~~~~~~~##

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
  #Psi.global = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  #6 covariates
  #Psi.mixed1 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctBrBnk+pctShade)
  #Psi.mixed2 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctBrBnk+HAiFLS_for)
  #Psi.mixed3 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctShade+HAiFLS_for)
  #Psi.mixed4 = list(formula = ~pctex21+pctpool+pctrock+pctBrBnk+pctShade+HAiFLS_for)
  #Psi.mixed5 = list(formula = ~pctex21+pctpool+BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  #Psi.mixed6 = list(formula = ~pctex21+pctrock+BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  #Psi.mixed7 = list(formula = ~pctpool+pctrock+BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  #5 covariates
  Psi.mixed8 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctBrBnk)
  Psi.mixed9 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+pctShade)
  #Psi.mixed10 = list(formula = ~pctex21+pctpool+pctrock+pctShade+pctBrBnk)
  #Psi.mixed11 = list(formula = ~pctex21+pctpool+BRT_100m+pctShade+pctBrBnk)
  #Psi.mixed12 = list(formula = ~pctex21+pctrock+BRT_100m+pctShade+pctBrBnk)
  #Psi.mixed13 = list(formula = ~pctpool+pctrock+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed14 = list(formula = ~pctex21+pctpool+pctrock+BRT_100m+HAiFLS_for)
  #Psi.mixed15 = list(formula = ~pctex21+pctpool+pctrock+pctShade+HAiFLS_for)
  #Psi.mixed16 = list(formula = ~pctex21+pctpool+pctShade+pctBrBnk+HAiFLS_for)
  #Psi.mixed17 = list(formula = ~pctex21+BRT_100m+pctShade+pctBrBnk+HAiFLS_for)
  #Psi.mixed18 = list(formula = ~pctrock+BRT_100m+pctShade+pctBrBnk+HAiFLS_for)
  #Psi.mixed19 = list(formula = ~pctpool+pctrock+BRT_100m+pctBrBnk+HAiFLS_for)
  #Psi.mixed20 = list(formula = ~pctex21+pctrock+BRT_100m+pctShade+HAiFLS_for)
  #Psi.mixed21 = list(formula = ~pctpool+pctrock+BRT_100m+pctBrBnk+HAiFLS_for)
  #Psi.mixed22 = list(formula = ~pctex21+pctpool+pctrock+pctBrBnk+HAiFLS_for)
  #Psi.mixed23 = list(formula = ~pctex21+pctpool+BRT_100m+pctShade+HAiFLS_for)
  #Psi.mixed24 = list(formula = ~pctex21+pctrock+pctBrBnk+pctShade+HAiFLS_for)
  #Psi.mixed25 = list(formula = ~pctpool+BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  #Psi.mixed26 = list(formula = ~pctex21+pctpool+BRT_100m+pctBrBnk+HAiFLS_for)
  #Psi.mixed27 = list(formula = ~pctex21+pctrock+BRT_100m+pctBrBnk+HAiFLS_for)
  #Psi.mixed28 = list(formula = ~pctpool+pctrock+pctBrBnk+pctShade+HAiFLS_for)
  #Psi.mixed29 = list(formula = ~pctpool+pctrock+BRT_100m+pctShade+HAiFLS_for)
  #4 covariates
  Psi.mixed30 = list(formula = ~pctex21+pctpool+pctShade+pctBrBnk)
  Psi.mixed31 = list(formula = ~pctex21+pctrock+pctShade+pctBrBnk)
  Psi.mixed32 = list(formula = ~pctex21+BRT_100m+pctShade+pctBrBnk)
  #Psi.mixed33 = list(formula = ~pctrock+BRT_100m+pctShade+pctBrBnk)
  #Psi.mixed34 = list(formula = ~pctpool+pctrock+pctShade+pctBrBnk)
  #Psi.mixed35 = list(formula = ~pctpool+BRT_100m+pctShade+pctBrBnk)
  Psi.mixed36 = list(formula = ~pctex21+pctpool+pctrock+HAiFLS_for)
  Psi.mixed37 = list(formula = ~pctex21+pctpool+pctShade+HAiFLS_for)
  Psi.mixed38 = list(formula = ~pctex21+pctBrBnk+pctShade+HAiFLS_for)
  #Psi.mixed39 = list(formula = ~BRT_100m+pctBrBnk+pctShade+HAiFLS_for)
  Psi.mixed40 = list(formula = ~pctex21+pctpool+BRT_100m+HAiFLS_for)
  #Psi.mixed41 = list(formula = ~pctpool+pctrock+BRT_100m+HAiFLS_for)
  Psi.mixed42 = list(formula = ~pctex21+pctrock+BRT_100m+HAiFLS_for)
  Psi.mixed43 = list(formula = ~pctex21+pctpool+pctBrBnk+HAiFLS_for)
  Psi.mixed44 = list(formula = ~pctex21+pctrock+pctBrBnk+HAiFLS_for)
  #Psi.mixed45 = list(formula = ~pctpool+pctrock+pctBrBnk+HAiFLS_for)
  Psi.mixed46 = list(formula = ~pctex21+pctrock+pctBrBnk+HAiFLS_for)
  Psi.mixed47 = list(formula = ~pctex21+pctrock+pctShade+HAiFLS_for)
  #Psi.mixed48 = list(formula = ~pctpool+pctrock+pctShade+HAiFLS_for)
  #Psi.mixed49 = list(formula = ~pctpool+BRT_100m+pctShade+HAiFLS_for)
  #Psi.mixed50 = list(formula = ~pctrock+BRT_100m+pctShade+HAiFLS_for)
  #Psi.mixed51 = list(formula = ~pctrock+pctBrBnk+pctShade+HAiFLS_for)
  #Psi.mixed52 = list(formula = ~pctpool+pctBrBnk+pctShade+HAiFLS_for)
  #Psi.mixed53 = list(formula = ~pctpool+BRT_100m+pctBrBnk+HAiFLS_for)
  #Psi.mixed54 = list(formula = ~pctpool+BRT_100m+pctShade+HAiFLS_for)
  #3 covariates
  Psi.mixed55 = list(formula = ~pctex21+pctShade+pctBrBnk)
  #Psi.mixed56 = list(formula = ~pctpool+pctShade+pctBrBnk)
  #Psi.mixed57 = list(formula = ~pctrock+pctShade+pctBrBnk)
  #Psi.mixed58 = list(formula = ~BRT_100m+pctShade+pctBrBnk)
  Psi.mixed59 = list(formula = ~pctex21+pctpool+pctShade)
  #Psi.mixed60 = list(formula = ~pctpool+pctrock+pctShade)
  #Psi.mixed61 = list(formula = ~pctrock+BRT_100m+pctShade)
  Psi.mixed62 = list(formula = ~pctex21+BRT_100m+pctShade)
  Psi.mixed63 = list(formula = ~pctex21+pctrock+pctShade)
  #Psi.mixed64 = list(formula = ~pctpool+BRT_100m+pctShade)
  Psi.mixed65 = list(formula = ~pctex21+pctpool+pctBrBnk)
  #Psi.mixed66 = list(formula = ~pctpool+pctrock+pctBrBnk)
  #Psi.mixed67 = list(formula = ~pctrock+BRT_100m+pctBrBnk)
  Psi.mixed68 = list(formula = ~pctex21+BRT_100m+pctBrBnk)
  Psi.mixed69 = list(formula = ~pctex21+pctrock+pctBrBnk)
  #Psi.mixed70 = list(formula = ~pctpool+BRT_100m+pctBrBnk)
  Psi.mixed71 = list(formula = ~pctex21+pctShade+HAiFLS_for)
  #Psi.mixed72 = list(formula = ~pctpool+pctShade+HAiFLS_for)
  #Psi.mixed73 = list(formula = ~pctrock+pctShade+HAiFLS_for)
  #Psi.mixed74 = list(formula = ~BRT_100m+pctShade+HAiFLS_for)
  Psi.mixed75 = list(formula = ~pctex21+pctBrBnk+HAiFLS_for)
  #Psi.mixed76 = list(formula = ~pctpool+pctBrBnk+HAiFLS_for)
  #Psi.mixed77 = list(formula = ~pctrock+pctBrBnk+HAiFLS_for)
  #Psi.mixed78 = list(formula = ~BRT_100m+pctBrBnk+HAiFLS_for)
  Psi.mixed79 = list(formula = ~pctex21+pctpool+HAiFLS_for)
  #Psi.mixed80 = list(formula = ~pctpool+pctrock+HAiFLS_for)
  #Psi.mixed81 = list(formula = ~pctrock+BRT_100m+HAiFLS_for)
  Psi.mixed82 = list(formula = ~pctex21+BRT_100m+HAiFLS_for)
  Psi.mixed83 = list(formula = ~pctex21+pctrock+HAiFLS_for)
  #Psi.mixed84 = list(formula = ~pctpool+BRT_100m+HAiFLS_for)
  #2 covariates
  Psi.pct21_bare = list(formula = ~pctex21 + pctBrBnk) 
  Psi.pct21_shade = list(formula = ~pctex21 + pctShade)
  #Psi.pool_bare = list(formula = ~pctpool + pctBrBnk) 
  #Psi.pool_shade = list(formula = ~pctpool + pctShade) 
  #Psi.rock_bare = list(formula = ~pctrock + pctBrBnk) 
  #Psi.rock_shade = list(formula = ~pctrock + pctShade) 
  #Psi.BRT_bare = list(formula = ~BRT_100m + pctBrBnk) 
  #Psi.BRT_shade = list(formula = ~BRT_100m + pctShade) 
  Psi.pct21_for = list(formula = ~pctex21 + HAiFLS_for)
  #Psi.pool_for = list(formula = ~pctpool + HAiFLS_for)
  #Psi.rock_for = list(formula = ~pctrock + HAiFLS_for) 
  #Psi.BRT_for = list(formula = ~BRT_100m + HAiFLS_for) 
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
top.mod <- bkt.results.loc$p.tv.effort.Psi.pct21_brt 


summary(top.mods)
cleanup(ask = F)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#




###~~~~~~~~~~~~~~~~~~##
####  Catchment   ####
##~~~~~~~~~~~~~~~~~~##

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
  #"global" catchment models
  Psi.cat = list(formula = ~HAiFLS_for+Area_km2+AvgSlope+EFac_Cat+Cross_Cat)
  #submodels
  #4 covariates
  Psi.for1 = list(formula = ~HAiFLS_for+Area_km2+AvgSlope+EFac_Cat)
  Psi.for2 = list(formula = ~HAiFLS_for+Area_km2+AvgSlope+Cross_Cat)
  Psi.for3 = list(formula = ~HAiFLS_for+Area_km2+EFac_Cat+Cross_Cat)
  Psi.for4 = list(formula = ~HAiFLS_for+AvgSlope+EFac_Cat+Cross_Cat)
  Psi.for5 = list(formula = ~Area_km2+AvgSlope+EFac_Cat+Cross_Cat)
  #3 covariates
  Psi.for6 = list(formula = ~HAiFLS_for+Area_km2+AvgSlope)
  Psi.for7 = list(formula = ~HAiFLS_for+Area_km2+EFac_Cat)
  Psi.for8 = list(formula = ~HAiFLS_for+Area_km2+Cross_Cat)
  Psi.for9 = list(formula = ~HAiFLS_for+AvgSlope+EFac_Cat)
  Psi.for10 = list(formula = ~HAiFLS_for+AvgSlope+Cross_Cat)
  Psi.for11 = list(formula = ~Area_km2+AvgSlope+EFac_Cat)
  Psi.for12 = list(formula = ~Area_km2+AvgSlope+Cross_Cat)
  Psi.for13 = list(formula = ~AvgSlope+EFac_Cat+Cross_Cat)
  #2 covariates
  Psi.for_area = list(formula = ~HAiFLS_for+Area_km2)
  Psi.for_slope = list(formula = ~HAiFLS_for+AvgSlope)
  Psi.for_EFac = list(formula = ~HAiFLS_for+EFac_Cat)
  Psi.for_cross = list(formula = ~HAiFLS_for+Cross_Cat)
  Psi.area_slope = list(formula = ~Area_km2+AvgSlope)
  Psi.area_EFac = list(formula = ~Area_km2+EFac_Cat)
  Psi.area_cross = list(formula = ~Area_km2+Cross_Cat)
  Psi.slope_EFac = list(formula = ~AvgSlope+EFac_Cat)
  Psi.slope_cross = list(formula = ~AvgSlope+Cross_Cat)
  Psi.EFac_cross = list(formula = ~EFac_Cat+Cross_Cat)
  #~~~~~~~~~~~~~ Occupancy - single covariate ~~~~~~~~~~~~~~~~~~~~~~
  Psi.for = list(formula = ~HAiFLS_for)
  Psi.area = list(formula = ~Area_km2)
  Psi.slope = list(formula = ~AvgSlope)
  Psi.EFac = list(formula = ~EFac_Cat)
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
AICc.Table.Cat = model.table(bkt.results.cat, use.lnl = T)
AICc.Table.Cat

#look at summary of top model(s)
summary(bkt.results.cat$p.tv.effort.Psi.for)
bkt.results.cat$p.tv.effort.Psi.for$results$real


cleanup(ask = FALSE)

#---------------------------------------------------------------------------------------------------#
#export MARK data with models
#export ch data to an .inp file
#str(brook2)
#export.chdata(brook.process, filename = "BrookOccu", covariates = "all")

#overall analysis results
names(brook2)
export.MARK(brook.process, "BKT_OccPrelim", model = bkt.results, ind.covariates = c("effort1", "effort2", 
                                                                                    "effort3", "pctex21",
                                                                                    "pctpool", "pctrock",
                                                                                    "pctBrBnk", "pctShade",
                                                                                    "BRT_100m", "HAiFLS_for",
                                                                                    "Area_km2", "AvgSlope",
                                                                                    "EFac_Cat", "Cross_Cat"), replace = T)
export.MARK(brook.process, "BKT_OccCat_Prelim", model = bkt.results.cat, ind.covariates = c("effort1", "effort2", 
                                                                                    "effort3", "HAiFLS_for",
                                                                                    "Area_km2", "AvgSlope",
                                                                                    "EFac_Cat", "Cross_Cat"), replace = T)

#---------------------------------------------------------------------------------------------------#
