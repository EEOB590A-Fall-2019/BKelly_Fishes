## Thesis: Chapter 3 - BRT vs. SGCN analysis

### Data Exploration and Analysis:
## Three main analyses:
#> Occupancy - 4 models: 1) all BRT, 2) all environment, 3) full model, 4) null model

#> Density 
  #1.) comparison of density b/w sites with and without BRT - permutation tests
  #2.) CPUE models - 1) all BRT, 2) all environment, 3) full model, 4) null model

#> Size - compare length frequency bins with permutation tests

library(tidyverse)
library(skimr)

######################################################################################
#> Occupancy - 4 models: 1) all BRT, 2) all environment, 3) full model, 4) null model
library(RMark)
library(corrplot)
#-------------------------------------------------------------------------------------

#load encounter history data 
lnd <- read.csv("Data/Thesis/Tidy/lnd_occu_data.csv", header = T) %>%
  unite(ch, c(ch1,ch2,ch3), sep = "", remove = T) %>%
  select(ch, freq, everything())

srd <- read.csv("Data/Thesis/Tidy/srd_occu_data.csv", header = T) %>%
  unite(ch, c(ch1,ch2,ch3), sep = "", remove = T) %>%
  select(ch, freq, everything())

cott <- read.csv("Data/Thesis/Tidy/cott_occu_data.csv", header = T) %>%
  unite(ch, c(ch1,ch2,ch3), sep = "", remove = T) %>%
  select(ch, freq, everything())

#load environmental data
env <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header = T)

#extract vars for detection probability
dp.cov <- env %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = T) %>%
  select(newID, pctcbbl, CatArea_km2)

#add detection probability vars to enc histories

#lnd
lnd2 <- left_join(lnd, dp.cov, by="newID")

#srd
srd2 <- left_join(srd, dp.cov, by="newID") %>%
  mutate(BRT = ifelse(BRT_100m > 0,1,0)) %>%
  replace_na(list("CatArea_km2" = 28.827)) %>%
  rename(Area_km2 = CatArea_km2)

#cott
cott2 <- left_join(cott, dp.cov, by="newID")

#----------
#Hypotheses 
#----------
##########################################################################################
#Longnose Dace
#Occupancy
# Habitat:
#> avwid (+)
#> AvgSlope (+) "Mean Slope of the catchment"
#> pctriffle (+)

# Brown Trout:
#> BRT_100m (-/+) "Brown Trout Catch-Per 100m of stream sampled"
#> adult_100m (-) "Brown Trout adult Catch-Per 100m of stream sampled"
#> med_len (-/+) "median TL of brown trout"

#Detection
#> effsec (+)
#> mFlow (+)
#---

#Southern Redbelly Dace
#Occupancy
# Habitat:
#> HAiFLS_ag (-) "Hydrologically Active inserve flow length to the stream of crop LULC"
#> MEANT (+)
#> % fines (+)
#> avwid (-)

# Brown Trout:
#> BRT_100m (-) "Brown Trout Catch-Per 100m of stream sampled"
#> adult_100m (-) "Brown Trout adult Catch-Per 100m of stream sampled"
#> med_len (-) "median TL of brown trout"

#Detection
#> effsec (+)
#> avdep (+)
#---

#Cottus
#Occupancy
# Habitat:
#> HAiFLS_for (+) "Hydrologically Active inserve flow length to the stream of forest LULC"
#> MEANT (-)
#> BrBnk (-)

# Brown Trout:
# Brown Trout:
#> BRT_100m (+) "Brown Trout Catch-Per 100m of stream sampled"
#> adult_100m (-/+) "Brown Trout adult Catch-Per 100m of stream sampled"
#> med_len (-/+) "median TL of brown trout"

#Detection
#> effsec (+)
#> pctcbbl (+)
#---
##########################################################################################

#----------
#collinearity assessment 
#----------
c <- cor(lnd2[,4:19])
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
p.mat <- cor.mtest(lnd2[,4:19])

#correlogram
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(c, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE)

##########################################################################################
################################################################################################################################
#set wd to scratch folder because MARK outputs an insane amount of files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Analysis/Brook Trout Project/Chapter3") 

#Process Data
#?process.data
#?make.design.data
lnd.process = process.data(lnd2, model="Occupancy", groups = "freq")
lnd.ddl = make.design.data(lnd.process)


##-------------------------------------------------------------------------------------------##
## --------------------------- Check constant p hypothesis --------------------------------- ##
##-------------------------------------------------------------------------------------------##

run.occ.lnd.p=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - covariates ~~~~~~~~~~~~
  p.full = list(formula = ~pctriffle + mFlow)
  p.riffle = list(formula = ~pctriffle)
  p.flow = list(formula = ~mFlow)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~avwid+pctcbbl+pctSlope+med_len+adult_100m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.lnd.p=create.model.list("Occupancy")
  results.lnd.p=mark.wrapper(cml.lnd.p, data=lnd.process, ddl=lnd.ddl, output=F)
  return(results.lnd.p)
}

lnd.results.p = run.occ.lnd.p()

lnd.results.p

#only one model <2 DeltaAICc 
summary(lnd.results.p$p.Dot.Psi.global) #top model 

## continue with null d-prob

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####        Occupancy models        ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

run.occ.lnd=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~avwid+pctcbbl+pctSlope+med_len+adult_100m)
  #Habitat Only
  Psi.habitat = list(formula = ~avwid+pctcbbl+pctSlope)
  #Brown Trout only
  Psi.trout = list(formula = ~med_len+adult_100m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.lnd=create.model.list("Occupancy")
  results.lnd=mark.wrapper(cml.lnd, data=lnd.process, ddl=lnd.ddl, output=F)
  return(results.lnd)
}

lnd.results.psi = run.occ.lnd()

##Examine model list and look at model comparisons
lnd.results.psi

##Model Table
AICc.Table.lnd = model.table(lnd.results.psi, use.lnl = T)
AICc.Table.lnd

#save model table output
write.csv(AICc.Table.lnd, "Longnose_ModTable.csv", row.names = F)

#look at summary of top model(s)
summary(lnd.results.p$p.Dot.Psi.global) #top model 

lnd.results.p$p.Dot.Psi.global$results$real

#designate top model
tm.lnd <- lnd.results.p$p.Dot.Psi.global
cleanup(ask = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing relative effects on psi  ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
lnd.ddl #par.index = 1, model.index = 4

#width
min.width <- min(lnd2$avwid)
max.width <- max(lnd2$avwid)
width.values <- seq(from = min.width, to = max.width, length = 100)
mean.width <- mean(lnd2$avwid)
#cobble
min.cobble <- min(lnd2$pctcbbl)
max.cobble <- max(lnd2$pctcbbl)
cobble.values <- seq(from = min.cobble, to = max.cobble, length = 100)
mean.cobble <- mean(lnd2$pctcbbl)
#slope
min.slope <- min(lnd2$pctSlope)
max.slope <- max(lnd2$pctSlope)
slope.values <- seq(from = min.slope, to = max.slope, length = 100)
mean.slope <- mean(lnd2$pctSlope)
#median TL
min.length <- min(lnd2$med_len)
max.length <- max(lnd2$med_len)
length.values <- seq(from = min.length, to = max.length, length = 100)
mean.length <- mean(lnd2$med_len)
#adult brown trout density
min.adult <- min(lnd2$adult_100m)
max.adult <- max(lnd2$adult_100m)
adult.values <- seq(from = min.adult, to = max.adult, length = 100)
mean.adult <- mean(lnd2$adult_100m)

##################################################
#covariate.predictions method
##################################################

#predictions of Psi for full range of average width
preds.lnd.width <- covariate.predictions(tm.lnd, 
                                         data = data.frame(avwid = width.values,
                                                           pctcbbl = mean.cobble,
                                                           pctSlope = mean.slope,
                                                           med_len = mean.length,
                                                           adult_100m = mean.adult),
                                         indices = 4)

head(preds.lnd.width$estimates)

lnd.wid.preds <- preds.lnd.width$estimates %>%
  select(avwid, pctcbbl, pctSlope, med_len, adult_100m, estimate, se, lcl, ucl)
  
names(lnd.wid.preds)

#predictions of Psi for full range of pctcbbl
preds.lnd.cobble <- covariate.predictions(tm.lnd, 
                                         data = data.frame(avwid = mean.width,
                                                           pctcbbl = cobble.values,
                                                           pctSlope = mean.slope,
                                                           med_len = mean.length,
                                                           adult_100m = mean.adult),
                                         indices = 4)

head(preds.lnd.cobble$estimates)

lnd.cbl.preds <- preds.lnd.cobble$estimates %>%
  select(avwid, pctcbbl, pctSlope, med_len, adult_100m, estimate, se, lcl, ucl)

names(lnd.cbl.preds)

#predictions of Psi for full range of slope
preds.lnd.slope <- covariate.predictions(tm.lnd, 
                                          data = data.frame(avwid = mean.width,
                                                            pctcbbl = mean.cobble,
                                                            pctSlope = slope.values,
                                                            med_len = mean.length,
                                                            adult_100m = mean.adult),
                                          indices = 4)

head(preds.lnd.slope$estimates)

lnd.slp.preds <- preds.lnd.slope$estimates %>%
  select(avwid, pctcbbl, pctSlope, med_len, adult_100m, estimate, se, lcl, ucl)

names(lnd.slp.preds)

#predictions of Psi for full range of length
preds.lnd.length <- covariate.predictions(tm.lnd, 
                                         data = data.frame(avwid = mean.width,
                                                           pctcbbl = mean.cobble,
                                                           pctSlope = mean.slope,
                                                           med_len = length.values,
                                                           adult_100m = mean.adult),
                                         indices = 4)

head(preds.lnd.length$estimates)

lnd.len.preds <- preds.lnd.length$estimates %>%
  select(avwid, pctcbbl, pctSlope, med_len, adult_100m, estimate, se, lcl, ucl)

names(lnd.len.preds)

#predictions of Psi for full range of adult_100m
preds.lnd.adults <- covariate.predictions(tm.lnd, 
                                          data = data.frame(avwid = mean.width,
                                                            pctcbbl = mean.cobble,
                                                            pctSlope = mean.slope,
                                                            med_len = mean.length,
                                                            adult_100m = adult.values),
                                          indices = 4)

head(preds.lnd.adults$estimates)

lnd.ads.preds <- preds.lnd.adults$estimates %>%
  select(avwid, pctcbbl, pctSlope, med_len, adult_100m, estimate, se, lcl, ucl)

names(lnd.ads.preds)

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(lnd.wid.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_width.csv")
write_csv(lnd.cbl.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_cobble.csv")
write_csv(lnd.slp.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_slope.csv")
write_csv(lnd.len.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_length.csv")
write_csv(lnd.ads.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_adults.csv")

#-----
#avwid
#-----
a <- ggplot(data=lnd.wid.preds, aes(x=avwid))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Mean Wetted Width (m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))+
  scale_x_continuous(breaks = c(2,4,6,8,10),
                     labels = c("2","4","6","8","10"))+
  ggtitle("(a)")+
  theme(plot.title = element_text(size=14))+
  theme(plot.title = element_text(vjust = -6, hjust = 0.02))
a

#-----
#pctcbbl
#-----
b <- ggplot(data=lnd.cbl.preds, aes(x=pctcbbl))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="% Cobble Substrate",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))
b

#-----
#pctslope
#-----
c <- ggplot(data=lnd.slp.preds, aes(x=pctSlope))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="% Catchment Slope",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))
c

#-----
#med_len
#-----
d <- ggplot(data=lnd.len.preds, aes(x=med_len))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Median Brown Trout TL (mm)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
d


#-----
#adult_100m
#-----
e <- ggplot(data=lnd.ads.preds, aes(x=adult_100m))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Brown Trout Density (n/100m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
e

#cowplot
library(cowplot)
vert.lnd <- plot_grid(a,b,c,d,e, labels = NULL, ncol = 1)

#create common y axis label
library(gridExtra)
library(grid)
y.grob <- textGrob("Occupancy Probability (Ψ)", 
                   gp=gpar(fontface="bold", col="black", fontsize=14), rot=90)
#add to plot
lnd.f <- grid.arrange(arrangeGrob(vert.lnd, left = y.grob))

getwd()
#setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
ggsave("lnd_occu.png", plot=lnd.f, dpi = 600)












##########################################################################################
################################################################################################################################
#set wd to scratch folder because MARK outputs an insane amount of files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Analysis/Brook Trout Project/Chapter3") 

#Process Data
#?process.data
#?make.design.data
srd.process = process.data(srd2, model="Occupancy", groups = "freq")
srd.ddl = make.design.data(srd.process)

##-------------------------------------------------------------------------------------------##
## --------------------------- Check constant p hypothesis --------------------------------- ##
##-------------------------------------------------------------------------------------------##

run.occ.srd.p=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - covariates ~~~~~~~~~~~~
  p.full = list(formula = ~avdep + mFlow)
  p.depth = list(formula = ~avdep)
  p.flow = list(formula = ~mFlow)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~MEANT+HAiFLS_alt+med_len+BRT)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.srd.p=create.model.list("Occupancy")
  results.srd.p=mark.wrapper(cml.srd.p, data=srd.process, ddl=srd.ddl, output=F)
  return(results.srd.p)
}

srd.results.p = run.occ.srd.p()

srd.results.p

#only one model <2 DeltaAICc 
summary(srd.results.p$p.depth.Psi.global) #top model 
srd.results.p$p.depth.Psi.global$results$real
## continue with depth on d-prob

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####        Occupancy models        ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

run.occ.srd=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.depth = list(formula= ~avdep)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~MEANT+HAiFLS_alt+med_len+BRT)
  #Habitat Only
  Psi.habitat = list(formula = ~MEANT+HAiFLS_alt)
  #Brown Trout only
  Psi.trout = list(formula = ~med_len+BRT)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.srd=create.model.list("Occupancy")
  results.srd=mark.wrapper(cml.srd, data=srd.process, ddl=srd.ddl, output=F)
  return(results.srd)
}

srd.results.psi = run.occ.srd()

##Examine model list and look at model comparisons
srd.results.psi

##Model Table
AICc.Table.srd = model.table(srd.results.psi, use.lnl = T)
AICc.Table.srd


##################################################
#save model table output
getwd()
write.csv(AICc.Table.srd, "Redbelly_ModTable.csv", row.names = F)

#look at summary of top model(s)
summary(srd.results.psi$p.depth.Psi.habitat) #top model 
srd.results.psi$p.depth.Psi.habitat$results$real

summary(srd.results.psi$p.depth.Psi.global) #2nd top model 
srd.results.psi$p.depth.Psi.global$results$real

#designate top model(s)
tm.srd <- srd.results.psi$p.depth.Psi.habitat
cleanup(ask = F)

t2.srd <- srd.results.psi$p.depth.Psi.global

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing relative effects on psi  ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
srd.ddl #par.index = 1, model.index = 4

#----------
#Detection:
#----------
#avdep
min.depth <- min(srd2$avdep)
max.depth <- max(srd2$avdep)
depth.values <- seq(from = min.depth, to = max.depth, length = 100)
mean.depth <- mean(srd2$avdep)

#---------
#Occupancy:
#---------
#MEANT
min.MEANT <- min(srd2$MEANT)
max.MEANT <- max(srd2$MEANT)
MEANT.values <- seq(from = min.MEANT, to = max.MEANT, length = 100)
mean.MEANT <- mean(srd2$MEANT)
#HAiFLS_alt
min.alt <- min(srd2$HAiFLS_alt)
max.alt <- max(srd2$HAiFLS_alt)
alt.values <- seq(from = min.alt, to = max.alt, length = 100)
mean.alt <- mean(srd2$HAiFLS_alt)
#BRT
min.BRT <- min(srd2$BRT)
max.BRT <- max(srd2$BRT)
BRT.values <- seq(from = min.BRT, to = max.BRT, length = 2)
BRT.values
mean.BRT <- mean(srd2$BRT)

##################################################
#covariate.predictions method - top model
##################################################

#predictions of Psi for full range of HAiFLS_alt
preds.srd.alt <- covariate.predictions(tm.srd, 
                                         data = data.frame(HAiFLS_alt = alt.values,
                                                           MEANT = mean.MEANT),
                                         indices = 4)

head(preds.srd.alt$estimates)

srd.alt.preds <- preds.srd.alt$estimates %>%
  select(HAiFLS_alt, MEANT, estimate, se, lcl, ucl)

names(srd.alt.preds)


#predictions of Psi for full range of MEANT 
preds.srd.MEANT <- covariate.predictions(tm.srd, 
                                          data = data.frame(HAiFLS_alt = mean.alt,
                                                            MEANT = MEANT.values),
                                          indices = 4)

head(preds.srd.MEANT$estimates)

srd.MNT.preds <- preds.srd.MEANT$estimates %>%
  select(HAiFLS_alt, MEANT, estimate, se, lcl, ucl)

names(srd.MNT.preds)

##################################################
#covariate.predictions method - 2nd model
##################################################

#predictions of Psi for full range of HAiFLS_alt and no BRT
preds.srd.alt <- covariate.predictions(tm.srd, 
                                       data = data.frame(HAiFLS_alt = alt.values,
                                                         MEANT = mean.MEANT),
                                       indices = 4)

head(preds.srd.alt$estimates)

srd.alt.preds <- preds.srd.alt$estimates %>%
  select(HAiFLS_alt, MEANT, estimate, se, lcl, ucl)

names(srd.alt.preds)

#predictions of Psi for full range of HAiFLS_alt and  BRT
preds.srd.alt <- covariate.predictions(tm.srd, 
                                       data = data.frame(HAiFLS_alt = alt.values,
                                                         MEANT = mean.MEANT),
                                       indices = 4)

head(preds.srd.alt$estimates)

srd.alt.preds <- preds.srd.alt$estimates %>%
  select(HAiFLS_alt, MEANT, estimate, se, lcl, ucl)

names(srd.alt.preds)



#predictions of Psi for full range of MEANT and no BRT
preds.srd.MEANT <- covariate.predictions(tm.srd, 
                                         data = data.frame(HAiFLS_alt = mean.alt,
                                                           MEANT = MEANT.values),
                                         indices = 4)

head(preds.srd.MEANT$estimates)

srd.MNT.preds <- preds.srd.MEANT$estimates %>%
  select(HAiFLS_alt, MEANT, estimate, se, lcl, ucl)

names(srd.MNT.preds)

#predictions of Psi for full range of MEANT and BRT
preds.srd.MEANT <- covariate.predictions(tm.srd, 
                                         data = data.frame(HAiFLS_alt = mean.alt,
                                                           MEANT = MEANT.values),
                                         indices = 4)

head(preds.srd.MEANT$estimates)

srd.MNT.preds <- preds.srd.MEANT$estimates %>%
  select(HAiFLS_alt, MEANT, estimate, se, lcl, ucl)

names(srd.MNT.preds)

#predictions of Psi for full range of length
preds.srd.length <- covariate.predictions(tm.srd, 
                                            data = data.frame(Area_km2 = mean.Area_km2,
                                                              MEANT = mean.MEANT,
                                                              pctfines = mean.fines,
                                                              med_len = length.values,
                                                              BRT_100m = mean.BRT),
                                            indices = 4)

head(preds.srd.length$estimates)

srd.len.preds <- preds.srd.length$estimates %>%
  select(Area_km2, MEANT, pctfines, med_len, BRT_100m, estimate, se, lcl, ucl)

names(srd.len.preds)

#predictions of Psi for Brown Trout presence
preds.srd.brown <- covariate.predictions(tm.srd, 
                                          data = data.frame(Area_km2 = mean.Area_km2,
                                                            MEANT = mean.MEANT,
                                                            pctfines = mean.fines,
                                                            med_len = mean.length,
                                                            BRT_100m = BRT.values),
                                          indices = 4)

head(preds.srd.brown$estimates)

srd.brt.preds <- preds.srd.brown$estimates %>%
  select(Area_km2, MEANT, pctfines, med_len, BRT_100m, estimate, se, lcl, ucl)

names(srd.brt.preds)

#predictions of Psi for Brown Trout absence
preds.srd.brown <- covariate.predictions(tm.srd, 
                                         data = data.frame(Area_km2 = mean.Area_km2,
                                                           MEANT = mean.MEANT,
                                                           pctfines = mean.fines,
                                                           med_len = mean.length,
                                                           BRT_100m = BRT.values),
                                         indices = 4)

head(preds.srd.brown$estimates)

srd.brt.preds <- preds.srd.brown$estimates %>%
  select(Area_km2, MEANT, pctfines, med_len, BRT_100m, estimate, se, lcl, ucl)

names(srd.brt.preds)

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(srd.alt.preds, "Data/Thesis/Tidy/SRD_OccuMod_Predictions_HAiFLS_alt.csv")
write_csv(srd.MNT.preds, "Data/Thesis/Tidy/SRD_OccuMod_Predictions_meant.csv")
#write_csv(srd.len.preds, "Data/Thesis/Tidy/SRD_OccuMod_Predictions_length.csv")
#write_csv(srd.brt.preds, "Data/Thesis/Tidy/SRD_OccuMod_Predictions_BRT_100m.csv")

#-----
#HAiFLS_alt
#-----
aa <- ggplot(data=srd.alt.preds, aes(x=HAiFLS_alt))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="% Human-Altered Land Cover",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))+
  #scale_x_continuous(breaks = c(2,4,6,8,10),
   #                  labels = c("2","4","6","8","10"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,.25,.50,.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))+
  ggtitle("(c)")+
  theme(plot.title = element_text(size=14))+
  theme(plot.title = element_text(vjust = -6, hjust = 0.02))
aa

#-----
#MEANT
#-----
bb <- ggplot(data=srd.MNT.preds, aes(x=MEANT))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Max Daily Mean Temperature (°C)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))+
  scale_x_continuous(limits = c(min(srd2$MEANT),max(srd2$MEANT)),
                     breaks = c(10,12,14,16,18,20,22,24),
                     labels = c("10","12","14","16","18","20","22","24"))
bb

#cowplot
vert.srd <- plot_grid(aa,bb, labels = NULL, ncol = 1, nrow = 5)
vert.srd
#create common y axis label
y.grob <- textGrob("Occupancy Probability (Ψ)", 
                   gp=gpar(fontface="bold", col="black", fontsize=14), rot=90)
#add to plot
srd.f <- grid.arrange(arrangeGrob(vert.srd, left = y.grob))

setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
ggsave("srd_occu.png", plot=srd.f, dpi = 600)






#-----
#med_len
#-----
cc <- ggplot(data=srd.len.preds, aes(x=med_len))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Median Brown Trout TL (mm)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
cc


#-----
#adult_100m
#-----
dd <- ggplot(data=srd.brt.preds, aes(x=BRT_100m))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  #scale_x_continuous(limits = c(0,1), breaks = c(0,1), labels = c("0", "1"))+
  labs(x="Brown Trout Density (n/100m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
dd

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing effort effect on p   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
min.depth <- min(srd2$avdep)
max.depth <- max(srd2$avdep)
depth.values <- seq(min.depth, max.depth, length.out = 100)
mean.depth <- mean(srd2$avdep) 

#predictions of p for full range of effort1 values
p.pred.srd <- covariate.predictions(tm.srd, 
                                     data = data.frame(avdep = depth.values),
                                     indices = 1)

head(p.pred.srd$estimates)


P.predictions.srd <- p.pred.srd$estimates %>%
  select(covdata, estimate, se, lcl, ucl) %>%
  rename(avdep = covdata) %>%
  round(digits = 3)

srd.dp <- ggplot(data=P.predictions.srd, aes(x=avdep))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Mean Stream Depth (m)",
       y=NULL,
       title = "")+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 12))+
  ggtitle("Southern Redbelly Dace")+
  theme(plot.title = element_text(size=12))
srd.dp

#ggsave("srd_dprob.png", dpi = 600)










##########################################################################################
################################################################################################################################
#set wd to scratch folder because MARK outputs an insane amount of files
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos/Analysis/Brook Trout Project/Chapter3") 

#Process Data
#?process.data
#?make.design.data
cott.process = process.data(cott2, model="Occupancy", groups = "freq")
cott.ddl = make.design.data(cott.process)


##-------------------------------------------------------------------------------------------##
## --------------------------- Check constant p hypothesis --------------------------------- ##
##-------------------------------------------------------------------------------------------##

run.occ.cott.p=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~ Detection Probability - covariates ~~~~~~~~~~~~
  p.full = list(formula = ~pctcbbl + mFlow)
  p.cobble = list(formula = ~pctcbbl)
  p.flow = list(formula = ~mFlow)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~avgT+BrBank+HAiFLS_for+med_len+adult_100m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.cott.p=create.model.list("Occupancy")
  results.cott.p=mark.wrapper(cml.cott.p, data=cott.process, ddl=cott.ddl, output=F)
  return(results.cott.p)
}

cott.results.p = run.occ.cott.p()

cott.results.p

#only one model <2 DeltaAICc 
summary(cott.results.p$p.flow.Psi.global) #top model 

## continue with d-prob as a function of mFlow

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####        Occupancy models        ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

run.occ.cott=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.flow = list(formula= ~mFlow)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~avgT+BrBank+HAiFLS_for+med_len+adult_100m)
  #Habitat Only
  Psi.habitat = list(formula = ~avgT+BrBank+HAiFLS_for)
  #Brown Trout only
  Psi.trout = list(formula = ~med_len+adult_100m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.cott=create.model.list("Occupancy")
  results.cott=mark.wrapper(cml.cott, data=cott.process, ddl=cott.ddl, output=F)
  return(results.cott)
}

cott.results.psi = run.occ.cott()

##Examine model list and look at model comparisons
cott.results.psi

##Model Table
AICc.Table.cott = model.table(cott.results.psi, use.lnl = T)
AICc.Table.cott

#save model table output
write.csv(AICc.Table.cott, "Cottus_ModTable.csv", row.names = F)

#look at summary of top model(s)
summary(cott.results.psi$p.flow.Psi.global) #top model 

cott.results.psi$p.flow.Psi.global$results$real

#designate top model
tm.cott <- cott.results.psi$p.flow.Psi.global
cleanup(ask = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing relative effects on psi  ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
cott.ddl #par.index = 1, model.index = 4

#BrBank
min.bare <- min(cott2$BrBank)
max.bare <- max(cott2$BrBank)
bare.values <- seq(from = min.bare, to = max.bare, length = 100)
mean.bare <- mean(cott2$BrBank)
#HAiFLS_for
min.forest <- min(cott2$HAiFLS_for)
max.forest <- max(cott2$HAiFLS_for)
forest.values <- seq(from = min.forest, to = max.forest, length = 100)
mean.forest <- mean(cott2$HAiFLS_for)
#avgT
min.avgT <- min(cott2$avgT)
max.avgT <- max(cott2$avgT)
avgT.values <- seq(from = min.avgT, to = max.avgT, length = 100)
mean.avgT <- mean(cott2$avgT)
#median TL
min.length <- min(lnd2$med_len)
max.length <- max(lnd2$med_len)
length.values <- seq(from = min.length, to = max.length, length = 100)
mean.length <- mean(lnd2$med_len)
#adult brown trout density
min.adult <- min(lnd2$adult_100m)
max.adult <- max(lnd2$adult_100m)
adult.values <- seq(from = min.adult, to = max.adult, length = 100)
mean.adult <- mean(lnd2$adult_100m)


##################################################
#covariate.predictions method
##################################################

#predictions of Psi for full range of avgT
preds.cott.avgT <- covariate.predictions(tm.cott, 
                                         data = data.frame(avgT = avgT.values,
                                                           BrBank = mean.bare,
                                                           HAiFLS_for = mean.forest,
                                                           med_len = mean.length,
                                                           adult_100m = mean.adult),
                                         indices = 4)

head(preds.cott.avgT$estimates)

cott.avgT.preds <- preds.cott.avgT$estimates %>%
  select(avgT, BrBank, HAiFLS_for, med_len, adult_100m, estimate, se, lcl, ucl)

names(cott.avgT.preds)

#predictions of Psi for full range of BrBank
preds.cott.bare <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            BrBank = bare.values,
                                                            HAiFLS_for = mean.forest,
                                                            med_len = mean.length,
                                                            adult_100m = mean.adult),
                                          indices = 4)

head(preds.cott.bare$estimates)

cott.bare.preds <- preds.cott.bare$estimates %>%
  select(avgT, BrBank, HAiFLS_for, med_len, adult_100m, estimate, se, lcl, ucl)

names(cott.bare.preds)

#predictions of Psi for full range of forest
preds.cott.forest <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            BrBank = mean.bare,
                                                            HAiFLS_for = forest.values,
                                                            med_len = mean.length,
                                                            adult_100m = mean.adult),
                                          indices = 4)

head(preds.cott.forest$estimates)

cott.forest.preds <- preds.cott.forest$estimates %>%
  select(avgT, BrBank, HAiFLS_for, med_len, adult_100m, estimate, se, lcl, ucl)

names(cott.forest.preds)

#predictions of Psi for full range of length
preds.cott.length <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            BrBank = mean.bare,
                                                            HAiFLS_for = mean.forest,
                                                            med_len = length.values,
                                                            adult_100m = mean.adult),
                                          indices = 4)

head(preds.cott.length$estimates)

cott.length.preds <- preds.cott.length$estimates %>%
  select(avgT, BrBank, HAiFLS_for, med_len, adult_100m, estimate, se, lcl, ucl)

names(cott.length.preds)

#predictions of Psi for full range of adult_100m
preds.cott.adults <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            BrBank = mean.bare,
                                                            HAiFLS_for = mean.forest,
                                                            med_len = mean.length,
                                                            adult_100m = adult.values),
                                          indices = 4)

head(preds.cott.adults$estimates)

cott.adults.preds <- preds.cott.adults$estimates %>%
  select(avgT, BrBank, HAiFLS_for, med_len, adult_100m, estimate, se, lcl, ucl)

names(cott.adults.preds)

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(cott.avgT.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_avgT.csv")
write_csv(cott.bare.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_bare.csv")
write_csv(cott.forest.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_forest.csv")
write_csv(cott.length.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_length.csv")
write_csv(cott.adults.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_adults.csv")

#-----
#avgT
#-----
aaa <- ggplot(data=cott.avgT.preds, aes(x=avgT))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Mean Stream Temperature (°C)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))+
  ggtitle("(b)")+
  theme(plot.title = element_text(size=14))+
  theme(plot.title = element_text(vjust = -6, hjust = 0.02))
aaa

#-----
#BrBank
#-----
bbb <- ggplot(data=cott.bare.preds, aes(x=BrBank))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Bare Bank Index Rating",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))
bbb

#-----
#HAiFLS_for
#-----
ccc <- ggplot(data=cott.forest.preds, aes(x=HAiFLS_for))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="% HAiFLS Forest Land Cover",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))
ccc

#-----
#med_len
#-----
ddd <- ggplot(data=cott.length.preds, aes(x=med_len))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Median Brown Trout TL (mm)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
ddd


#-----
#adult_100m
#-----
eee <- ggplot(data=cott.adults.preds, aes(x=adult_100m))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Brown Trout Density (n/100m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
eee

#cowplot
library(cowplot)
vert.cott <- plot_grid(aaa,bbb,ccc,ddd,eee, labels = NULL, ncol = 1)
vert.cott

#create common y axis label
library(gridExtra)
library(grid)
y.grob <- textGrob("Occupancy Probability (Ψ)", 
                   gp=gpar(fontface="bold", col="black", fontsize=14), rot=90)
#add to plot
ff <- grid.arrange(arrangeGrob(vert3, left = y.grob))

getwd()
#setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
ggsave("cott_occu.png", plot=ff, dpi = 600)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### overall Psi figure for thesis   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
occupancy.figure <- plot_grid(lnd.f,vert.cott,vert.srd, labels = NULL, ncol = 3)
occupancy.figure
ggsave("Figure_2.png", plot = occupancy.figure, dpi = 600)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing effort effect on p   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
min.mFlow <- min(cott2$mFlow)
max.mFlow <- max(cott2$mFlow)
mFlow.values <- seq(min.mFlow, max.mFlow, length.out = 100)
mean.mFlow <- mean(cott2$mFlow) 

#predictions of p for full range of effort1 values
p.pred.cott <- covariate.predictions(tm.cott, 
                                     data = data.frame(mFlow = mFlow.values),
                                     indices = 1)

head(p.pred.cott$estimates)


P.predictions.cott <- p.pred.cott$estimates %>%
  select(covdata, estimate, se, lcl, ucl) %>%
  rename(mFlow = covdata) %>%
  round(digits = 3)

cott.dp <- ggplot(data=P.predictions.cott, aes(x=mFlow))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Mean Stream Velocity (m/s)",
       y="Detection Probability (p)",
       title = "")+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 12))+
  ggtitle("Sculpins")+
  theme(plot.title = element_text(size=12))
cott.dp

ggsave("cott_dprob.png", dpi = 600)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### overall (p) figure for thesis   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
detection.figure <- plot_grid(cott.dp,srd.dp, labels = NULL, ncol = 2)
detection.figure
ggsave("Figure_1.png", plot = detection.figure, dpi = 600)




