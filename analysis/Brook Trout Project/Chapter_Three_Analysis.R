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
names(env)

#extract vars for detection probability
dp.cov <- env %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = T) %>%
  select(newID, pctcbbl, pctpool = pctslow)

#add detection probability vars to enc histories

#lnd
lnd2 <- left_join(lnd, dp.cov, by="newID")

#srd
srd2 <- left_join(srd, dp.cov, by="newID") %>%
  mutate(BRT = ifelse(BRT_100m > 0,1,0)) #%>%
  #replace_na(list("CatArea_km2" = 28.827)) %>%
  #rename(Area_km2 = CatArea_km2)

#outlier in BRT_100m
srd2[115,]

#checking something
srd3 <- srd2 %>%
  filter(BRT_100m > 3) %>%
  select(ch, newID, avgT, pctfines, avdep, BRT_100m)

#changing false-presence of SRD in site 20
#srd2[136,1] <- "000"
#srd2[136,]

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
#> pctcbbl (+)

# Brown Trout:
#> BRT_100m (-/+) "Brown Trout Catch-Per 100m of stream sampled"
#> adult_100m (-) "Brown Trout adult Catch-Per 100m of stream sampled"
#> med_len (-/+) "median TL of brown trout"

#Detection
#> pctcbbl (+)
#> mFlow (+)
#---

#Southern Redbelly Dace
#Occupancy
# Habitat:
#> HAiFLS_for (+) "Hydrologically Active inserve flow length to the stream of forest LULC"
#> avgT (+)
#> % fines (-)
#> AvgSlope (+) "Mean Slope of the catchment"

# Brown Trout:
#> BRT_100m (-) "Brown Trout Catch-Per 100m of stream sampled"
#> adult_100m (-) "Brown Trout adult Catch-Per 100m of stream sampled"
#> med_len (-) "median TL of brown trout"

#Detection
#> mFlow (-)
#> avdep (+)
#---

#Cottus
#Occupancy
# Habitat:
#> HAiFLS_for (+) "Hydrologically Active inserve flow length to the stream of forest LULC"
#> avgT (-)
#> BrBnk (-)

# Brown Trout:
# Brown Trout:
#> BRT_100m (+) "Brown Trout Catch-Per 100m of stream sampled"
#> adult_100m (-/+) "Brown Trout adult Catch-Per 100m of stream sampled"
#> med_len (-/+) "median TL of brown trout"

#Detection
#> mFlow (-)
#> pctcbbl (+)
#---
##########################################################################################

#----------
#collinearity assessment 
#----------
c <- cor(lnd2[,4:20])
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
p.mat <- cor.mtest(lnd2[,4:20])

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
  p.full = list(formula = ~pctcbbl + mFlow)
  p.cobble = list(formula = ~pctcbbl)
  p.flow = list(formula = ~mFlow)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~avwid+pctcbbl+pctSlope+med_len+BRT_100m)
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
  Psi.global = list(formula = ~avwid+pctcbbl+pctSlope+med_len+BRT_100m)
  #Habitat Only
  Psi.habitat = list(formula = ~avwid+pctcbbl+pctSlope)
  #Brown Trout only
  Psi.trout = list(formula = ~med_len+BRT_100m)
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
#brown trout density
min.trout <- min(lnd2$BRT_100m)
max.trout <- max(lnd2$BRT_100m)
trout.values <- seq(from = min.trout, to = max.trout, length = 100)
mean.trout <- mean(lnd2$BRT_100m)
med.trout <- median(lnd2$BRT_100m)

##################################################
#covariate.predictions method
##################################################

#predictions of Psi for full range of average width
preds.lnd.width <- covariate.predictions(tm.lnd, 
                                         data = data.frame(avwid = width.values,
                                                           pctcbbl = mean.cobble,
                                                           pctSlope = mean.slope,
                                                           med_len = mean.length,
                                                           BRT_100m = mean.trout),
                                         indices = 4)

head(preds.lnd.width$estimates)

lnd.wid.preds <- preds.lnd.width$estimates %>%
  select(avwid, pctcbbl, pctSlope, med_len, BRT_100m, estimate, se, lcl, ucl)
  
names(lnd.wid.preds)

#predictions of Psi for full range of pctcbbl
preds.lnd.cobble <- covariate.predictions(tm.lnd, 
                                         data = data.frame(avwid = mean.width,
                                                           pctcbbl = cobble.values,
                                                           pctSlope = mean.slope,
                                                           med_len = mean.length,
                                                           BRT_100m = mean.trout),
                                         indices = 4)

head(preds.lnd.cobble$estimates)

lnd.cbl.preds <- preds.lnd.cobble$estimates %>%
  select(avwid, pctcbbl, pctSlope, med_len, BRT_100m, estimate, se, lcl, ucl)

names(lnd.cbl.preds)

#predictions of Psi for full range of slope
preds.lnd.slope <- covariate.predictions(tm.lnd, 
                                          data = data.frame(avwid = mean.width,
                                                            pctcbbl = mean.cobble,
                                                            pctSlope = slope.values,
                                                            med_len = mean.length,
                                                            BRT_100m = mean.trout),
                                          indices = 4)

head(preds.lnd.slope$estimates)

lnd.slp.preds <- preds.lnd.slope$estimates %>%
  select(avwid, pctcbbl, pctSlope, med_len, BRT_100m, estimate, se, lcl, ucl)

names(lnd.slp.preds)

#predictions of Psi for full range of length
preds.lnd.length <- covariate.predictions(tm.lnd, 
                                         data = data.frame(avwid = mean.width,
                                                           pctcbbl = mean.cobble,
                                                           pctSlope = mean.slope,
                                                           med_len = length.values,
                                                           BRT_100m = mean.trout),
                                         indices = 4)

head(preds.lnd.length$estimates)

lnd.len.preds <- preds.lnd.length$estimates %>%
  select(avwid, pctcbbl, pctSlope, med_len, BRT_100m, estimate, se, lcl, ucl)

names(lnd.len.preds)

#predictions of Psi for full range of adult_100m
preds.lnd.trout <- covariate.predictions(tm.lnd, 
                                          data = data.frame(avwid = mean.width,
                                                            pctcbbl = mean.cobble,
                                                            pctSlope = mean.slope,
                                                            med_len = mean.length,
                                                            BRT_100m = trout.values),
                                          indices = 4)

head(preds.lnd.trout$estimates)

lnd.brt.preds <- preds.lnd.trout$estimates %>%
  select(avwid, pctcbbl, pctSlope, med_len, BRT_100m, estimate, se, lcl, ucl)

names(lnd.brt.preds)

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(lnd.wid.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_width.csv")
write_csv(lnd.cbl.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_cobble.csv")
write_csv(lnd.slp.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_slope.csv")
write_csv(lnd.len.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_length.csv")
write_csv(lnd.brt.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_trout.csv")
###################################################################################
library(extrafont)
#font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts() 

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
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_x_continuous(breaks = c(2,4,6,8,10),
                     labels = c("2","4","6","8","10"))+
  ggtitle("Longnose Dace")+
  theme(plot.title = element_text(size = 16, family = "Times New Roman"))
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
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
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
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
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
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
d


#-----
#brt_100m
#-----
e <- ggplot(data=lnd.brt.preds, aes(x=BRT_100m))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  scale_x_continuous(limits = c(0,40), breaks = c(0,10,20,30,40), labels = c("0","10","20","30","40"))+
  labs(x="Brown Trout Density (n/100m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
e

#cowplot
library(cowplot)
vert.lnd <- plot_grid(a,b,c,d,e, labels = NULL, ncol = 1)
vert.lnd

#create common y axis label
library(gridExtra)
library(grid)
y.grob <- textGrob("Occupancy Probability (Ψ)", 
                   gp=gpar(fontface="bold", col="black", fontsize=14, fontfamily="Times New Roman"), rot=90)
#add to plot
lnd.f <- grid.arrange(arrangeGrob(vert.lnd, left = y.grob))

getwd()
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
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
  p.flow = list(formula = ~mFlow)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~avgT+avdep+pctfines+med_len+BRT_100m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.srd.p=create.model.list("Occupancy")
  results.srd.p=mark.wrapper(cml.srd.p, data=srd.process, ddl=srd.ddl, output=F)
  return(results.srd.p)
}

srd.results.p = run.occ.srd.p()

srd.results.p

#Two models <2 DeltaAICc 
summary(srd.results.p$p.Dot.Psi.global) #top model 
srd.results.p$p.Dot.Psi.global$results$real

summary(srd.results.p$p.flow.Psi.global) #2nd model 
srd.results.p$p.flow.Psi.global$results$real


## continue with null on d-prob

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
####        Occupancy models        ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

run.occ.srd=function()
{
  #~~~~~~~~~~~~~ Model List ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #~~~~~~~~~~~ Detection Probability - null model ~~~~~~~~~~~~
  p.Dot = list(formula= ~1)
  #~~~~~~~~~~~~~ Occupancy - null model ~~~~~~~~~~~~~~~~~~~~~~
  Psi.Dot        = list(formula=~1) 
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~avgT+avdep+pctfines+med_len+BRT_100m)
  #Habitat Only
  Psi.habitat = list(formula = ~avgT+avdep+pctfines)
  #Brown Trout only
  Psi.trout = list(formula = ~med_len+BRT_100m)
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
summary(srd.results.psi$p.Dot.Psi.global) #top model 
srd.results.psi$p.Dot.Psi.global$results$real

#look at summary of top model(s)
summary(srd.results.psi$p.Dot.Psi.habitat) #2nd model (delta AIC = 6.9) 
srd.results.psi$p.Dot.Psi.habitat$results$real

#designate top model(s)
tm.srd <- srd.results.psi$p.Dot.Psi.global
cleanup(ask = F)

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
#avgT
min.avgT <- min(srd2$avgT)
max.avgT <- max(srd2$avgT)
avgT.values <- seq(from = min.avgT, to = max.avgT, length = 100)
mean.avgT <- mean(srd2$avgT)
#pctfines
min.fines <- min(srd2$pctfines)
max.fines <- max(srd2$pctfines)
fines.values <- seq(from = min.fines, to = max.fines, length = 100)
mean.fines <- mean(srd2$pctfines)


##################################################
#covariate.predictions method - top model
##################################################

#predictions of Psi for full range of avgT
preds.srd.avgT <- covariate.predictions(tm.srd, 
                                         data = data.frame(avgT = avgT.values,
                                                           avdep = mean.depth,
                                                           pctfines = mean.fines,
                                                           med_len = mean.length,
                                                           BRT_100m = mean.trout),
                                         indices = 4)

head(preds.srd.avgT$estimates)

srd.avgT.preds <- preds.srd.avgT$estimates %>%
  select(avgT, avdep, pctfines, med_len, BRT_100m, estimate, se, lcl, ucl)

names(srd.avgT.preds)


#predictions of Psi for full range of pctfines 
preds.srd.fines <- covariate.predictions(tm.srd, 
                                        data = data.frame(avgT = mean.avgT,
                                                          avdep = mean.depth,
                                                          pctfines = fines.values,
                                                          med_len = mean.length,
                                                          BRT_100m = mean.trout),
                                        indices = 4)

head(preds.srd.fines$estimates)

srd.fine.preds <- preds.srd.fines$estimates %>%
  select(avgT, avdep, pctfines, med_len, BRT_100m, estimate, se, lcl, ucl)

names(srd.fine.preds)

#predictions of Psi for full range of pctSlope
preds.srd.depth <- covariate.predictions(tm.srd, 
                                         data = data.frame(avgT = mean.avgT,
                                                           avdep = depth.values,
                                                           pctfines = mean.fines,
                                                           med_len = mean.length,
                                                           BRT_100m = mean.trout),
                                         indices = 4)

head(preds.srd.depth$estimates)

srd.depth.preds <- preds.srd.depth$estimates %>%
  select(avgT, avdep, pctfines, med_len, BRT_100m, estimate, se, lcl, ucl)

names(srd.depth.preds)

#predictions of Psi for full range of med_len
preds.srd.length <- covariate.predictions(tm.srd, 
                                         data = data.frame(avgT = mean.avgT,
                                                           avdep = mean.depth,
                                                           pctfines = mean.fines,
                                                           med_len = length.values,
                                                           BRT_100m = mean.trout),
                                         indices = 4)

head(preds.srd.length$estimates)

srd.length.preds <- preds.srd.length$estimates %>%
  select(avgT, avdep, pctfines, med_len, BRT_100m, estimate, se, lcl, ucl)

names(srd.length.preds)


#predictions of Psi for full range of BRT_100m
preds.srd.trout <- covariate.predictions(tm.srd, 
                                          data = data.frame(avgT = mean.avgT,
                                                            avdep = mean.depth,
                                                            pctfines = mean.fines,
                                                            med_len = mean.length,
                                                            BRT_100m = trout.values),
                                          indices = 4)

head(preds.srd.trout$estimates)

srd.trout.preds <- preds.srd.trout$estimates %>%
  select(avgT, avdep, pctfines, med_len, BRT_100m, estimate, se, lcl, ucl)

names(srd.trout.preds)



####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(srd.avgT.preds, "Data/Thesis/Tidy/SRD_OccuMod_Predictions_avgT.csv")
write_csv(srd.fine.preds, "Data/Thesis/Tidy/SRD_OccuMod_Predictions_fines.csv")
write_csv(srd.depth.preds, "Data/Thesis/Tidy/SRD_OccuMod_Predictions_depth.csv")
write_csv(srd.length.preds, "Data/Thesis/Tidy/SRD_OccuMod_Predictions_length.csv")
write_csv(srd.trout.preds, "Data/Thesis/Tidy/SRD_OccuMod_Predictions_BRT_100m.csv")

#-----
#avgT
#-----
aa <- ggplot(data=srd.avgT.preds, aes(x=avgT))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Mean Stream Temperature (°C)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  #scale_x_continuous(breaks = c(2,4,6,8,10),
   #                  labels = c("2","4","6","8","10"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,.25,.50,.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))+
  ggtitle("Southern Redbelly Dace")+
  theme(plot.title = element_text(size = 16, family = "Times New Roman"))
aa

#-----
#fines
#-----
bb <- ggplot(data=srd.fine.preds, aes(x=pctfines))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="% Fine Substrates (clay, silt, sand)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))#+
  #scale_x_continuous(limits = c(min(srd2$MEANT),max(srd2$MEANT)),
  #                   breaks = c(10,12,14,16,18,20,22,24),
  #                   labels = c("10","12","14","16","18","20","22","24"))
bb

#-----
#depth
#-----
cc <- ggplot(data=srd.depth.preds, aes(x=avdep))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Mean Stream Depth (m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))#+
#scale_x_continuous(limits = c(min(srd2$MEANT),max(srd2$MEANT)),
#                   breaks = c(10,12,14,16,18,20,22,24),
#                   labels = c("10","12","14","16","18","20","22","24"))
cc


#-----
#med_len
#-----
dd <- ggplot(data=srd.length.preds, aes(x=med_len))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Median Brown Trout TL (mm)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
dd


#-----
#adult_100m
#-----
ee <- ggplot(data=srd.trout.preds, aes(x=BRT_100m))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  scale_x_continuous(limits = c(0,30), breaks = c(0,10,20,30), labels = c("0","10","20","30"))+
  labs(x="Brown Trout Density (n/100m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
ee



#cowplot
vert.srd <- plot_grid(aa,bb,cc,dd,ee, labels = NULL, ncol = 1, nrow = 5)
vert.srd
#create common y axis label
#y.grob <- textGrob("Occupancy Probability (Ψ)", 
 #                  gp=gpar(fontface="bold", col="black", fontsize=14), rot=90)
#add to plot
srd.f <- grid.arrange(arrangeGrob(vert.srd, left = y.grob))

#setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
ggsave("srd_occu.png", plot=srd.f, dpi = 600)







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
  Psi.global = list(formula = ~avgT+mFlow+HAiFLS_for+med_len+BRT_100m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.cott.p=create.model.list("Occupancy")
  results.cott.p=mark.wrapper(cml.cott.p, data=cott.process, ddl=cott.ddl, output=F)
  return(results.cott.p)
}

cott.results.p = run.occ.cott.p()

cott.results.p

#only one model <2 DeltaAICc 
summary(cott.results.p$p.flow.Psi.global) #top model 
#only one model <2 DeltaAICc 
summary(cott.results.p$p.full.Psi.global) #2nd model 

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
  Psi.global = list(formula = ~avgT+mFlow+HAiFLS_for+med_len+BRT_100m)
  #Habitat Only
  Psi.habitat = list(formula = ~avgT+mFlow+HAiFLS_for)
  #Brown Trout only
  Psi.trout = list(formula = ~med_len+BRT_100m)
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

#HAiFLS_for
min.forest <- min(cott2$HAiFLS_for)
max.forest <- max(cott2$HAiFLS_for)
forest.values <- seq(from = min.forest, to = max.forest, length = 100)
mean.forest <- mean(cott2$HAiFLS_for)

#mFlow
min.flow <- min(cott2$mFlow)
max.flow <- max(cott2$mFlow)
flow.values <- seq(from = min.flow, to = max.flow, length = 100)
mean.flow <- mean(cott2$mFlow)

##################################################
#covariate.predictions method
##################################################

#predictions of Psi for full range of avgT
preds.cott.avgT <- covariate.predictions(tm.cott, 
                                         data = data.frame(avgT = avgT.values,
                                                           mFlow = mean.flow,
                                                           HAiFLS_for = mean.forest,
                                                           med_len = mean.length,
                                                           BRT_100m = mean.trout),
                                         indices = 4)

head(preds.cott.avgT$estimates)

cott.avgT.preds <- preds.cott.avgT$estimates %>%
  select(avgT, mFlow, HAiFLS_for, med_len, BRT_100m, estimate, se, lcl, ucl)

names(cott.avgT.preds)

#predictions of Psi for full range of mFlow
preds.cott.flow <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            mFlow = flow.values,
                                                            HAiFLS_for = mean.forest,
                                                            med_len = mean.length,
                                                            BRT_100m = mean.trout),
                                          indices = 4)

head(preds.cott.flow$estimates)

cott.flow.preds <- preds.cott.flow$estimates %>%
  select(avgT, mFlow, HAiFLS_for, med_len, BRT_100m, estimate, se, lcl, ucl)

names(cott.flow.preds)

#predictions of Psi for full range of forest
preds.cott.forest <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            mFlow = mean.flow,
                                                            HAiFLS_for = forest.values,
                                                            med_len = mean.length,
                                                            BRT_100m = mean.trout),
                                          indices = 4)

head(preds.cott.forest$estimates)

cott.forest.preds <- preds.cott.forest$estimates %>%
  select(avgT, mFlow, HAiFLS_for, med_len, BRT_100m, estimate, se, lcl, ucl)

names(cott.forest.preds)

#predictions of Psi for full range of length
preds.cott.length <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            mFlow = mean.flow,
                                                            HAiFLS_for = mean.forest,
                                                            med_len = length.values,
                                                            BRT_100m = mean.trout),
                                          indices = 4)

head(preds.cott.length$estimates)

cott.length.preds <- preds.cott.length$estimates %>%
  select(avgT, mFlow, HAiFLS_for, med_len, BRT_100m, estimate, se, lcl, ucl)

names(cott.length.preds)

#predictions of Psi for full range of adult_100m
preds.cott.trout <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            mFlow = mean.flow,
                                                            HAiFLS_for = mean.forest,
                                                            med_len = mean.length,
                                                            BRT_100m = trout.values),
                                          indices = 4)

head(preds.cott.trout$estimates)

cott.trout.preds <- preds.cott.trout$estimates %>%
  select(avgT, mFlow, HAiFLS_for, med_len, BRT_100m, estimate, se, lcl, ucl)

names(cott.trout.preds)

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(cott.avgT.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_avgT.csv")
write_csv(cott.flow.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_flow.csv")
write_csv(cott.forest.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_forest.csv")
write_csv(cott.length.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_length.csv")
write_csv(cott.trout.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_trout.csv")

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
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))+
  ggtitle("Sculpin")+
  theme(plot.title = element_text(size = 16, family = "Times New Roman"))
aaa

#-----
#BrBank
#-----
bbb <- ggplot(data=cott.flow.preds, aes(x=mFlow))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Mean Stream Velocity (m/s)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
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
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
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
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
ddd


#-----
#adult_100m
#-----
eee <- ggplot(data=cott.trout.preds, aes(x=BRT_100m))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Brown Trout Density (n/100m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
eee

#cowplot
#library(cowplot)
vert.cott <- plot_grid(aaa,bbb,ccc,ddd,eee, labels = NULL, ncol = 1)
vert.cott

#create common y axis label
#library(gridExtra)
#library(grid)
#y.grob <- textGrob("Occupancy Probability (Ψ)", 
                   #gp=gpar(fontface="bold", col="black", fontsize=14), rot=90)
#add to plot
ff <- grid.arrange(arrangeGrob(vert.cott, left = y.grob))

getwd()
#setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
ggsave("cott_occu.png", plot=ff, dpi = 600)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### overall Psi figure for thesis   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
occupancy.figure <- plot_grid(ff,vert.lnd,vert.srd, labels = NULL, ncol = 3)
occupancy.figure
ggsave("Figure_3_occupancy.png", plot = occupancy.figure, dpi = 600)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing effort effect on p   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#mFlow
min.flow <- min(cott2$mFlow)
max.flow <- max(cott2$mFlow)
flow.values <- seq(from = min.flow, to = max.flow, length = 100)
mean.flow <- mean(cott2$mFlow)

#predictions of p for full range of effort1 values
p.pred.cott <- covariate.predictions(tm.cott, 
                                     data = data.frame(mFlow = flow.values),
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
       y=expression(bold('Detection Probability '~bold(italic((p)))~'')),
       title = "")+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title.y = element_text(margin = margin(r=5)))+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
cott.dp
  #ggtitle("Sculpins")+
  #theme(plot.title = element_text(size=12))


ggsave("cott_dprob.png", dpi = 600)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### overall (p) figure for thesis   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#Sculpin Cumulative Detection Probability

#Chosen Model results
cott.results.psi$p.flow.Psi.global$results$real

#estimate
cpe <- 0.7949061
cpe2 <- 1-(1-cpe)^2
cpe3 <- 1-(1-cpe)^3
#lower confidence limit
clc <- 0.6498150
clc2 <- 1-(1-clc)^2
clc3 <- 1-(1-clc)^3
#upper confidence limit
cuc <- 0.8900533
cuc2 <- 1-(1-cuc)^2
cuc3 <- 1-(1-cuc)^3
#Dataframe
cott_cdp <- data.frame(reach = 1:3, p = c(cpe,cpe2,cpe3), lcl = c(clc,clc2,clc3),
                      ucl = c(cuc,cuc2,cuc3))

cott.cdp <- 
  ggplot(data = cott_cdp, aes(x=reach))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), colour="black", width=.1) +
  geom_line(aes(y=p), size=1, color="black")+
  geom_point(aes(y=p))+
  labs(x="Sampling Occassions",
       y="Cumulative Detection Probability")+
  ggtitle("Sculpin")+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  scale_x_continuous(breaks = c(1,2,3), labels = c("1","2","3"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  theme(axis.title.y = element_text(margin = margin(r=5)))+
  theme(plot.title = element_text(size = 16, family = "Times New Roman"))
cott.cdp


#Longnose Dace Cumulative Detection Probability

#Chosen Model results
lnd.results.psi$p.Dot.Psi.global$results$real

#estimate
lpe <- 0.6660873
lpe2 <- 1-(1-lpe)^2
lpe3 <- 1-(1-lpe)^3
#lower confidence limit
llc <- 0.5600865
llc2 <- 1-(1-llc)^2
llc3 <- 1-(1-llc)^3
#upper confidence limit
luc <- 0.7576005
luc2 <- 1-(1-luc)^2
luc3 <- 1-(1-luc)^3
#Dataframe
lnd_cdp <- data.frame(reach = 1:3, p = c(lpe,lpe2,lpe3), lcl = c(llc,llc2,llc3),
                       ucl = c(luc,luc2,luc3))

lnd.cdp <- 
  ggplot(data = lnd_cdp, aes(x=reach))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), colour="black", width=.1) +
  geom_line(aes(y=p), size=1, color="black")+
  geom_point(aes(y=p))+
  labs(x="Sampling Occassions",
       y="")+
  ggtitle("Longnose Dace")+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  scale_x_continuous(breaks = c(1,2,3), labels = c("1","2","3"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  theme(plot.title = element_text(size = 16, family = "Times New Roman"))
lnd.cdp



#Southern Redbelly Dace Cumulative Detection Probability

#Chosen Model results
srd.results.psi$p.Dot.Psi.global$results$real

#estimate
spe <- 0.5405216
spe2 <- 1-(1-spe)^2
spe3 <- 1-(1-spe)^3
#lower confidence limit
slc <- 0.4235900
slc2 <- 1-(1-slc)^2
slc3 <- 1-(1-slc)^3
#upper confidence limit
suc <- 0.6531555
suc2 <- 1-(1-suc)^2
suc3 <- 1-(1-suc)^3
#Dataframe
srd_cdp <- data.frame(reach = 1:3, p = c(spe,spe2,spe3), lcl = c(slc,slc2,slc3),
                      ucl = c(suc,suc2,suc3))

srd.cdp <- 
  ggplot(data = srd_cdp, aes(x=reach))+
  geom_errorbar(aes(ymin=lcl, ymax=ucl), colour="black", width=.1) +
  geom_line(aes(y=p), size=1, color="black")+
  geom_point(aes(y=p))+
  labs(x="Sampling Occassions",
       y="")+
  ggtitle("Southern Redbelly Dace")+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  scale_x_continuous(breaks = c(1,2,3), labels = c("1","2","3"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  theme(plot.title = element_text(size = 16, family = "Times New Roman"))
srd.cdp


#Plot full grided figure
plot_grid(cott.cdp, lnd.cdp, srd.cdp, cott.dp, ncol = 3)
ggsave("Chapt3_Figure_Dprob.png", dpi = 600)
