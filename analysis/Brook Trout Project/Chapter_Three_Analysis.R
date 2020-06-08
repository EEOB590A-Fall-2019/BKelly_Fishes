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

habby <- read.csv("Data/Thesis/Tidy/enviro_tidy.csv", header = T)

#extract vars for detection probability
dp.cov <- env %>%
  unite(newID, c(HUC8, Site), sep = "_", remove = T) %>%
  select(newID, pctcbbl, pctpool = pctslow, Order, Area_km2=CatArea_km2, boulder, elev_m)

dp.cov[99,]
dp.cov[99,4] = 4
dp.cov[99,5] = 28.8615
dp.cov[99,7] = 355.164337
dp.cov[99,]

#add detection probability vars to enc histories

#lnd
lnd2 <- left_join(lnd, dp.cov, by="newID")
names(lnd2)
lnd2 <- lnd2 %>%
  select(-avwid, -HAiFLS_alt, -pctSlope, -adult_100m, -MEANT, -BrBank)

#srd
srd2 <- left_join(srd, dp.cov, by="newID") %>%
  mutate(BRT = ifelse(BRT_100m > 0,1,0))
names(srd2)
srd2 <- srd2 %>%
  select(-avwid, -HAiFLS_alt, -pctSlope, -adult_100m, -MEANT, -BrBank)

#outlier in BRT_100m
srd2[115,]

#checking something
srd3 <- srd2 %>%
  filter(BRT == 1) %>%
  summarise(mean_cpue = mean(BRT_100m), min_cpue = min(BRT_100m),
            max_cpue = max(BRT_100m), sd_cpue = sd(BRT_100m))

#changing false-presence of SRD in site 20
#srd2[136,1] <- "000"
#srd2[136,]

#cott
cott2 <- left_join(cott, dp.cov, by="newID")
names(cott2)
cott2 <- cott2 %>%
  select(-avwid, -HAiFLS_alt, -pctSlope, -adult_100m, -MEANT, -BrBank)
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
c <- cor(lnd2[,4:18])
head(round(c,2)) 

#round down
cround <- round(c,3)

#visualize these correlations
col4 <- colorRampPalette(c("#7F0000", "red", "#FF7F00", "gray", "#007FFF", "blue", "#00007F"))

corrplot(c, type = "upper", order = "alphabet", method = "number", col = col4(5))

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
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~Area_km2+pctcbbl+elev_m+avgT+med_len+BRT_100m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.lnd.p=create.model.list("Occupancy")
  results.lnd.p=mark.wrapper(cml.lnd.p, data=lnd.process, ddl=lnd.ddl, output=F)
  return(results.lnd.p)
}

lnd.results.p = run.occ.lnd.p()

lnd.results.p

##Model Table
AICc.Table.lnd.p = model.table(lnd.results.p, use.lnl = T)
AICc.Table.lnd.p

#save model table output
write.csv(AICc.Table.lnd.p, "Longnose_ModTable_DProb.csv", row.names = F)

#Two models <2 DeltaAICc 
summary(lnd.results.p$p.Dot.Psi.global)#top model 
lnd.results.p$p.Dot.Psi.global$results$real
summary(lnd.results.p$p.cobble.Psi.global)#2nd model 
lnd.results.p$p.cobble.Psi.global$results$real
summary(lnd.results.p$p.flow.Psi.global)#3rd model 
lnd.results.p$p.flow.Psi.global$results$real
summary(lnd.results.p$p.flow.Psi.global) # 4th model
lnd.results.p$p.flow.Psi.global$results$real
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
  #Combination models
  Psi.global = list(formula = ~Area_km2+pctcbbl+elev_m+avgT+med_len+BRT_100m)
  
  Psi.combo1 = list(formula = ~Area_km2+pctcbbl+elev_m+avgT+med_len)
  Psi.combo1.1 = list(formula = ~Area_km2+pctcbbl+elev_m+med_len)
  Psi.combo1.2 = list(formula = ~Area_km2+pctcbbl+avgT+med_len)
  Psi.combo1.3 = list(formula = ~pctcbbl+elev_m+avgT+med_len)
  Psi.combo1.4 = list(formula = ~Area_km2+pctcbbl+med_len)
  Psi.combo1.5 = list(formula = ~Area_km2+elev_m+med_len)
  Psi.combo1.6 = list(formula = ~Area_km2+avgT+med_len)
  Psi.combo1.7 = list(formula = ~pctcbbl+elev_m+med_len)
  Psi.combo1.8 = list(formula = ~pctcbbl+avgT+med_len)
  Psi.combo1.9 = list(formula = ~elev_m+avgT+med_len)
  Psi.combo1.10 = list(formula = ~Area_km2+med_len)
  Psi.combo1.11 = list(formula = ~pctcbbl+med_len)
  Psi.combo1.12 = list(formula = ~elev_m+med_len)
  Psi.combo1.13 = list(formula = ~avgT+med_len)
  
  Psi.combo2 = list(formula = ~Area_km2+pctcbbl+elev_m+avgT+BRT_100m)
  Psi.combo2.1 = list(formula = ~Area_km2+pctcbbl+elev_m+BRT_100m)
  Psi.combo2.2 = list(formula = ~Area_km2+pctcbbl+avgT+BRT_100m)
  Psi.combo2.3 = list(formula = ~pctcbbl+elev_m+avgT+BRT_100m)
  Psi.combo2.4 = list(formula = ~Area_km2+pctcbbl+BRT_100m)
  Psi.combo2.5 = list(formula = ~Area_km2+elev_m+BRT_100m)
  Psi.combo2.6 = list(formula = ~Area_km2+avgT+BRT_100m)
  Psi.combo2.7 = list(formula = ~pctcbbl+elev_m+BRT_100m)
  Psi.combo2.8 = list(formula = ~pctcbbl+avgT+BRT_100m)
  Psi.combo2.9 = list(formula = ~elev_m+avgT+BRT_100m)
  Psi.combo2.10 = list(formula = ~Area_km2+BRT_100m)
  Psi.combo2.11 = list(formula = ~pctcbbl+BRT_100m)
  Psi.combo2.12 = list(formula = ~elev_m+BRT_100m)
  Psi.combo2.13 = list(formula = ~avgT+BRT_100m)
  #Habitat Only models
  Psi.habitat = list(formula = ~Area_km2+pctcbbl+elev_m+avgT)
  Psi.hab1 = list(formula = ~Area_km2+pctcbbl)
  Psi.hab2 = list(formula = ~Area_km2+avgT)
  Psi.hab3 = list(formula = ~pctcbbl+elev_m)
  Psi.hab4 = list(formula = ~elev_m+avgT)
  
  Psi.watershed = list(formula = ~Area_km2+elev_m)
  Psi.area = list(formula = ~Area_km2)
  Psi.elev = list(formula = ~elev_m)
  
  Psi.local = list(formula = ~pctcbbl+avgT)
  Psi.cobble = list(formula = ~pctcbbl)
  Psi.temp = list(formula = ~avgT)
  
  #Brown Trout only models
  Psi.trout = list(formula = ~med_len+BRT_100m)
  Psi.len = list(formula = ~med_len)
  Psi.abun = list(formula = ~BRT_100m)
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

#look at summary of top model(s) -- 3 within 2AICc & hold double digit weight
summary(lnd.results.psi$p.Dot.Psi.combo2) # top model
lnd.results.psi$p.Dot.Psi.combo2$results$real

summary(lnd.results.psi$p.Dot.Psi.habitat) # 2nd model
lnd.results.psi$p.Dot.Psi.habitat$results$real

summary(lnd.results.psi$p.Dot.Psi.global) # 3rd model
lnd.results.psi$p.Dot.Psi.global$results$real

summary(lnd.results.psi$p.Dot.Psi.combo2.3) #FMO
lnd.results.psi$p.Dot.Psi.combo2.3$results$real

#designate top model
tm.lnd <- lnd.results.psi$p.Dot.Psi.combo2
sm.lnd <- lnd.results.psi$p.Dot.Psi.habitat
cleanup(ask = F)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing relative effects on psi  ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
lnd.ddl #par.index = 1, model.index = 4

#area
min.area <- min(lnd2$Area_km2)
max.area <- max(lnd2$Area_km2)
area.values <- seq(from = min.area, to = max.area, length = 100)
mean.area <- mean(lnd2$Area_km2)
#cobble
min.cobble <- min(lnd2$pctcbbl)
max.cobble <- max(lnd2$pctcbbl)
cobble.values <- seq(from = min.cobble, to = max.cobble, length = 100)
mean.cobble <- mean(lnd2$pctcbbl)
#elev_m
min.elev <- min(lnd2$elev_m)
max.elev <- max(lnd2$elev_m)
elev.values <- seq(from = min.elev, to = max.elev, length = 100)
mean.elev <- mean(lnd2$elev_m)
#avgT
min.avgT <- min(srd2$avgT)
max.avgT <- max(srd2$avgT)
avgT.values <- seq(from = min.avgT, to = max.avgT, length = 100)
mean.avgT <- mean(srd2$avgT)
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

#predictions of Psi for full range of area
preds.lnd.area <- covariate.predictions(tm.lnd, 
                                         data = data.frame(Area_km2 = area.values,
                                                           pctcbbl = mean.cobble,
                                                           elev_m = mean.elev,
                                                           avgT = mean.avgT,
                                                           BRT_100m = mean.trout),
                                         indices = 4)

head(preds.lnd.area$estimates)

lnd.area.preds <- preds.lnd.area$estimates %>%
  select(Area_km2, pctcbbl, elev_m, avgT, BRT_100m, estimate, se, lcl, ucl)
  
names(lnd.area.preds)

#predictions of Psi for full range of pctcbbl
preds.lnd.cobble <- covariate.predictions(tm.lnd, 
                                         data = data.frame(Area_km2 = mean.area,
                                                           pctcbbl = cobble.values,
                                                           elev_m = mean.elev,
                                                           avgT = mean.avgT,
                                                           BRT_100m = mean.trout),
                                         indices = 4)

head(preds.lnd.cobble$estimates)

lnd.cbl.preds <- preds.lnd.cobble$estimates %>%
  select(Area_km2, pctcbbl, elev_m, avgT, BRT_100m, estimate, se, lcl, ucl)

names(lnd.cbl.preds)

#predictions of Psi for full range of elevation
preds.lnd.elev <- covariate.predictions(tm.lnd, 
                                          data = data.frame(Area_km2 = mean.area,
                                                            pctcbbl = mean.cobble,
                                                            elev_m = elev.values,
                                                            avgT = mean.avgT,
                                                            BRT_100m = mean.trout),
                                          indices = 4)

head(preds.lnd.elev$estimates)

lnd.elev.preds <- preds.lnd.elev$estimates %>%
  select(Area_km2, pctcbbl, elev_m, avgT, BRT_100m, estimate, se, lcl, ucl)

names(lnd.elev.preds)

#predictions of Psi for full range of temperature
preds.lnd.temp <- covariate.predictions(tm.lnd, 
                                        data = data.frame(Area_km2 = mean.area,
                                                          pctcbbl = mean.cobble,
                                                          elev_m = mean.elev,
                                                          avgT = avgT.values,
                                                          BRT_100m = mean.trout),
                                        indices = 4)

head(preds.lnd.temp$estimates)

lnd.avgt.preds <- preds.lnd.temp$estimates %>%
  select(Area_km2, pctcbbl, elev_m, avgT, BRT_100m, estimate, se, lcl, ucl)

names(lnd.avgt.preds)


#predictions of Psi for full range of BRT_100m
preds.lnd.trout <- covariate.predictions(tm.lnd, 
                                          data = data.frame(Area_km2 = mean.area,
                                                            pctcbbl = mean.cobble,
                                                            elev_m = mean.elev,
                                                            avgT = mean.avgT,
                                                            BRT_100m = trout.values),
                                          indices = 4)

head(preds.lnd.trout$estimates)

lnd.brt.preds <- preds.lnd.trout$estimates %>%
  select(Area_km2, pctcbbl, elev_m, avgT, BRT_100m, estimate, se, lcl, ucl)

names(lnd.brt.preds)

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
#top mod
write_csv(lnd.area.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_area_km2.csv")
write_csv(lnd.cbl.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_cobble.csv")
write_csv(lnd.elev.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_elev.csv")
write_csv(lnd.avgt.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_avgT.csv")
write_csv(lnd.brt.preds, "Data/Thesis/Tidy/LND_OccuMod_Predictions_trout.csv")
###################################################################################
library(extrafont)
#font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts() 

#-----
#area
#-----
ar <- ggplot(data=lnd.area.preds, aes(x=Area_km2))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black", linetype="longdash")+
  labs(x="Total Catchment Area (Km )",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))+
  theme(axis.title.x = element_text(margin = margin(t=0)))
ar

#-----
#pctcbbl
#-----
cob <- ggplot(data=lnd.cbl.preds, aes(x=pctcbbl))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black",linetype="longdash")+
  labs(x= "% Cobble Substrate",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))
cob

#-----
#elevation
#-----
elev <- ggplot(data=lnd.elev.preds, aes(x=elev_m))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black", linetype="longdash")+
  labs(x="Elevation (m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))+
  scale_x_continuous(breaks = c(200,240,280,320,360),
                     labels = c("200","240","280","320","360"))
elev

#-----
#avgT
#-----
avt <- ggplot(data=lnd.avgt.preds, aes(x=avgT))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black", linetype="longdash")+
  labs(x="Mean Stream Temperature (°C)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))
avt


#-----
#brt_100m
#-----
bt <- ggplot(data=lnd.brt.preds, aes(x=BRT_100m))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black", linetype="longdash")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Brown Trout CPUE (fish/100m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
bt

#cowplot
library(cowplot)
plot.lnd <- plot_grid(avt,cob,elev,ar,bt, labels = NULL, ncol = 3)
plot.lnd

#create common y axis label
library(gridExtra)
library(grid)
y.grob <- textGrob("Occupancy Probability (Ψ)", 
                   gp=gpar(fontface="bold", col="black", fontsize=14, fontfamily="Times New Roman"), rot=90)
#add to plot
lnd.f <- grid.arrange(arrangeGrob(plot.lnd, left = y.grob))

getwd()
#setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
ggsave("lnd_occu.png", plot=lnd.f, dpi = 600)




#not run!
#----------------------------------------------------
# now add the title
title1 <- ggdraw() + 
  draw_label(
    "Longnose Dace",
    size = 16,
    fontfamily = "Times New Roman",
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

final.lnd <- plot_grid(
  title1, plot.lnd,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
#----------------------------------------------------






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
  p.full = list(formula = ~mFlow+avdep)
  p.flow = list(formula = ~mFlow)
  p.depth = list(formula = ~avdep)
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

##Model Table
AICc.Table.srd.p = model.table(srd.results.p, use.lnl = T)
AICc.Table.srd.p

#save model table output
write.csv(AICc.Table.srd.p, "Redbelly_ModTable_DProb.csv", row.names = F)

#Compare model results - 1 model <2deltaAICc 
summary(srd.results.p$p.depth.Psi.global) #top model 
srd.results.p$p.depth.Psi.global$results$real

summary(srd.results.p$p.full.Psi.global) #2nd model 
srd.results.p$p.full.Psi.global$results$real


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
  Psi.global = list(formula = ~avgT+avdep+pctfines+med_len+BRT_100m)
  
  Psi.combo1 = list(formula = ~avgT+avdep+pctfines+med_len)
  Psi.combo1.1 = list(formula = ~avgT+avdep+med_len)
  Psi.combo1.2 = list(formula = ~avgT+pctfines+med_len)
  Psi.combo1.3 = list(formula = ~avdep+pctfines+med_len)
  Psi.combo1.4 = list(formula = ~avgT+med_len)
  Psi.combo1.5 = list(formula = ~avdep+med_len)
  Psi.combo1.6 = list(formula = ~pctfines+med_len)
  
  Psi.combo2 = list(formula = ~avgT+avdep+pctfines+BRT_100m)
  Psi.combo2.1 = list(formula = ~avgT+avdep+BRT_100m)
  Psi.combo2.2 = list(formula = ~avgT+pctfines+BRT_100m)
  Psi.combo2.3 = list(formula = ~avdep+pctfines+BRT_100m)
  Psi.combo2.4 = list(formula = ~avgT+BRT_100m)
  Psi.combo2.5 = list(formula = ~avdep+BRT_100m)
  Psi.combo2.6 = list(formula = ~pctfines+BRT_100m)
  
  #Habitat Only
  Psi.habitat = list(formula = ~avgT+avdep+pctfines)
  Psi.hab1 = list(formula = ~avgT+avdep)
  Psi.hab2 = list(formula = ~avgT+pctfines)
  Psi.hab3 = list(formula = ~avdep+pctfines)
  Psi.avgt = list(formula = ~avgT)
  Psi.avdep = list(formula = ~avdep)
  Psi.pctfines = list(formula = ~pctfines)
  
  #Brown Trout only
  Psi.trout = list(formula = ~med_len+BRT_100m)
  Psi.len = list(formula = ~med_len)
  Psi.abun = list(formula = ~BRT_100m)
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
summary(srd.results.psi$p.depth.Psi.combo2.4) #top model 
srd.results.psi$p.depth.Psi.combo2.4$results$real

##2nd model (delta AIC = 1.47)
summary(srd.results.psi$p.depth.Psi.combo2.1) 
srd.results.psi$p.depth.Psi.combo2.1$results$real

##3rd model (delta AIC = 1.90)
summary(srd.results.psi$p.depth.Psi.combo2.2) 
srd.results.psi$p.depth.Psi.combo2.2$results$real

#designate top model(s)
tm.srd <- srd.results.psi$p.depth.Psi.combo2.4
cleanup(ask = F)


summary(srd.results.psi$p.depth.Psi.combo2exp4)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### Visualizing relative effects on psi  ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
srd.ddl #par.index = 1, model.index = 4


#avdep
min.depth <- min(srd2$avdep)
max.depth <- max(srd2$avdep)
depth.values <- seq(from = min.depth, to = max.depth, length = 100)
mean.depth <- mean(srd2$avdep)
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
                                                           BRT_100m = mean.trout),
                                         indices = 4)

head(preds.srd.avgT$estimates)

srd.avgT.preds <- preds.srd.avgT$estimates %>%
  select(avgT, BRT_100m, estimate, se, lcl, ucl)

names(srd.avgT.preds)

#predictions of Psi for full range of BRT_100m
preds.srd.trout <- covariate.predictions(tm.srd, 
                                         data = data.frame(avgT = mean.avgT,
                                                           BRT_100m = trout.values),
                                         indices = 4)

head(preds.srd.trout$estimates)

srd.trout.preds <- preds.srd.trout$estimates %>%
  select(avgT, BRT_100m, estimate, se, lcl, ucl)

names(srd.trout.preds)

#-----Detection Probability-----#
#predictions of p for full range of depth
preds.srd.deprob <- covariate.predictions(tm.srd, 
                                         data = data.frame(avdep = depth.values),
                                         indices = 1)

head(preds.srd.deprob$estimates)

srd.dprob.preds <- preds.srd.deprob$estimates %>%
  select(avdep=covdata, estimate, se, lcl, ucl)

names(srd.dprob.preds)


##################################################
#  NOT RUN -- COVARS NOT INCLUDED IN TOP MODEL   #
##################################################
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

#predictions of Psi for full range of depth
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
##################################################
#  NOT RUN -- COVARS NOT INCLUDED IN TOP MODEL   #
##################################################






####################################################
##     Write tidy csv's for p & Psi predictions   ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(srd.avgT.preds, "Data/Thesis/Tidy/SRD_OccuMod_Predictions_avgT.csv")
write_csv(srd.trout.preds, "Data/Thesis/Tidy/SRD_OccuMod_Predictions_BRT_100m.csv")
write_csv(srd.dprob.preds, "Data/Thesis/Tidy/SRD_DetectMod_Predictions_depth.csv")

#-----
#avgT
#-----
srd.avt <- ggplot(data=srd.avgT.preds, aes(x=avgT))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black", linetype="dotdash")+
  labs(x="Mean Stream Temperature (°C)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  #scale_x_continuous(breaks = c(2,4,6,8,10),
   #                  labels = c("2","4","6","8","10"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,.25,.50,.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))
srd.avt

#-----
#BRT_100m
#-----
srd.bt <- ggplot(data=srd.trout.preds, aes(x=BRT_100m))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black", linetype="dotdash")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  #scale_x_continuous(limits = c(0,50), breaks = c(0,10,20,30,40,50), labels = c("0","10","20","30","40","50"))+
  labs(x="Brown Trout CPUE (fish/100m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
srd.bt






#cowplot
plot.srd <- plot_grid(srd.avt,srd.bt, labels = NULL, ncol = 3, nrow = 1)
plot.srd

# now add the title
title2 <- ggdraw() + 
  draw_label(
    "Southern Redbelly Dace",
    size = 16,
    fontfamily = "Times New Roman",
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

final.srd <- plot_grid(
  title2, plot.srd,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
final.srd

srd.f <- grid.arrange(arrangeGrob(final.srd, left = y.grob))

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
  #~~~~~~~~~~~~~ Occupancy - multiple covariates ~~~~~~~~~~~~~~~~~~~~~~
  #all covariates
  Psi.global = list(formula = ~avgT+mFlow+HAiFLS_for+boulder+med_len+BRT_100m)
  #~~~~~~~~~~~~ model list & wrapper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cml.cott.p=create.model.list("Occupancy")
  results.cott.p=mark.wrapper(cml.cott.p, data=cott.process, ddl=cott.ddl, output=F)
  return(results.cott.p)
}

cott.results.p = run.occ.cott.p()

cott.results.p

##Model Table
AICc.Table.cott.p = model.table(cott.results.p, use.lnl = T)
AICc.Table.cott.p

#save model table output
write.csv(AICc.Table.cott.p, "Cottus_ModTable_DProb.csv", row.names = F)

#only one model <2 DeltaAICc 
summary(cott.results.p$p.flow.Psi.global) #top model 
cott.results.p$p.flow.Psi.global$results$real
#only one model <2 DeltaAICc 
summary(cott.results.p$p.full.Psi.global) #2nd model 
cott.results.p$p.full.Psi.global$results$real
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
  Psi.global = list(formula = ~avgT+mFlow+HAiFLS_for+boulder+med_len+BRT_100m)
  
  Psi.combo1 = list(formula = ~avgT+mFlow+HAiFLS_for+boulder+med_len)
  Psi.combo1.1 = list(formula = ~avgT+mFlow+HAiFLS_for+med_len)
  Psi.combo1.2 = list(formula = ~avgT+mFlow+boulder+med_len)
  Psi.combo1.3 = list(formula = ~mFlow+HAiFLS_for+boulder+med_len)
  Psi.combo1.4 = list(formula = ~avgT+mFlow+med_len)
  Psi.combo1.5 = list(formula = ~avgT+HAiFLS_for+med_len)
  Psi.combo1.6 = list(formula = ~avgT+boulder+med_len)
  Psi.combo1.7 = list(formula = ~mFlow+HAiFLS_for+med_len)
  Psi.combo1.8 = list(formula = ~mFlow+boulder+med_len)
  Psi.combo1.9 = list(formula = ~HAiFLS_for+boulder+med_len)
  Psi.combo1.10 = list(formula = ~avgT+med_len)
  Psi.combo1.11 = list(formula = ~mFlow+med_len)
  Psi.combo1.12 = list(formula = ~HAiFLS_for+med_len)
  Psi.combo1.13 = list(formula = ~boulder+med_len)
  
  Psi.combo2 = list(formula = ~avgT+mFlow+HAiFLS_for+boulder+BRT_100m)
  Psi.combo2.1 = list(formula = ~avgT+mFlow+HAiFLS_for+BRT_100m)
  Psi.combo2.2 = list(formula = ~avgT+mFlow+boulder+BRT_100m)
  Psi.combo2.3 = list(formula = ~mFlow+HAiFLS_for+boulder+BRT_100m)
  Psi.combo2.4 = list(formula = ~avgT+mFlow+BRT_100m)
  Psi.combo2.5 = list(formula = ~avgT+HAiFLS_for+BRT_100m)
  Psi.combo2.6 = list(formula = ~avgT+boulder+BRT_100m)
  Psi.combo2.7 = list(formula = ~mFlow+HAiFLS_for+BRT_100m)
  Psi.combo2.8 = list(formula = ~mFlow+boulder+BRT_100m)
  Psi.combo2.9 = list(formula = ~HAiFLS_for+boulder+BRT_100m)
  Psi.combo2.10 = list(formula = ~avgT+BRT_100m)
  Psi.combo2.11 = list(formula = ~mFlow+BRT_100m)
  Psi.combo2.12 = list(formula = ~HAiFLS_for+BRT_100m)
  Psi.combo2.13 = list(formula = ~boulder+BRT_100m)
  
  #Habitat Only
  Psi.habitat = list(formula = ~avgT+mFlow+HAiFLS_for+boulder)
  Psi.hab1 = list(formula = ~avgT+mFlow+HAiFLS_for)
  Psi.hab2 = list(formula = ~avgT+HAiFLS_for+boulder)
  Psi.hab3 = list(formula = ~mFlow+HAiFLS_for+boulder)
  Psi.hab4 = list(formula = ~avgT+HAiFLS_for)
  Psi.hab5 = list(formula = ~mFlow+HAiFLS_for)
  Psi.hab6 = list(formula = ~boulder+HAiFLS_for)
  #watershed
  Psi.forest = list(formula = ~HAiFLS_for)
  #local
  Psi.local = list(formula = ~avgT+mFlow+boulder)
  Psi.local = list(formula = ~avgT+mFlow)
  Psi.local = list(formula = ~avgT+boulder)
  Psi.local = list(formula = ~mFlow+boulder)
  
  
  #Brown Trout only
  Psi.trout = list(formula = ~med_len+BRT_100m)
  Psi.len = list(formula = ~med_len)
  Psi.abun = list(formula = ~BRT_100m)
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

summary(cott.results.psi$p.flow.Psi.combo2.9) #2nd model 

cott.results.psi$p.flow.Psi.combo2.9$results$real

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

#boulder
min.bldr <- min(cott2$boulder)
max.bldr <- max(cott2$boulder)
bldr.values <- seq(from = min.bldr, to = max.bldr, length = 100)
mean.bldr <- mean(cott2$boulder)

##################################################
#covariate.predictions method
##################################################

#predictions of Psi for full range of avgT
preds.cott.avgT <- covariate.predictions(tm.cott, 
                                         data = data.frame(avgT = avgT.values,
                                                           mFlow = mean.flow,
                                                           HAiFLS_for = mean.forest,
                                                           boulder = mean.bldr,
                                                           med_len = mean.length,
                                                           BRT_100m = mean.trout),
                                         indices = 4)

head(preds.cott.avgT$estimates)

cott.avgT.preds <- preds.cott.avgT$estimates %>%
  select(avgT, mFlow, HAiFLS_for, boulder, med_len, BRT_100m, estimate, se, lcl, ucl)

names(cott.avgT.preds)

#predictions of Psi for full range of mFlow
preds.cott.flow <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            mFlow = flow.values,
                                                            HAiFLS_for = mean.forest,
                                                            boulder = mean.bldr,
                                                            med_len = mean.length,
                                                            BRT_100m = mean.trout),
                                          indices = 4)

head(preds.cott.flow$estimates)

cott.flow.preds <- preds.cott.flow$estimates %>%
  select(avgT, mFlow, HAiFLS_for, boulder, med_len, BRT_100m, estimate, se, lcl, ucl)

names(cott.flow.preds)

#predictions of Psi for full range of forest
preds.cott.forest <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            mFlow = mean.flow,
                                                            HAiFLS_for = forest.values,
                                                            boulder = mean.bldr,
                                                            med_len = mean.length,
                                                            BRT_100m = mean.trout),
                                          indices = 4)

head(preds.cott.forest$estimates)

cott.forest.preds <- preds.cott.forest$estimates %>%
  select(avgT, mFlow, HAiFLS_for, boulder, med_len, BRT_100m, estimate, se, lcl, ucl)

names(cott.forest.preds)

#predictions of Psi for full range of boulder
preds.cott.boulder <- covariate.predictions(tm.cott, 
                                           data = data.frame(avgT = mean.avgT,
                                                             mFlow = mean.flow,
                                                             HAiFLS_for = mean.forest,
                                                             boulder = bldr.values,
                                                             med_len = mean.length,
                                                             BRT_100m = mean.trout),
                                           indices = 4)

head(preds.cott.boulder$estimates)

cott.bldr.preds <- preds.cott.boulder$estimates %>%
  select(avgT, mFlow, HAiFLS_for, boulder, med_len, BRT_100m, estimate, se, lcl, ucl)

names(cott.bldr.preds)

#predictions of Psi for full range of length
preds.cott.length <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            mFlow = mean.flow,
                                                            HAiFLS_for = mean.forest,
                                                            boulder = mean.bldr,
                                                            med_len = length.values,
                                                            BRT_100m = mean.trout),
                                          indices = 4)

head(preds.cott.length$estimates)

cott.length.preds <- preds.cott.length$estimates %>%
  select(avgT, mFlow, HAiFLS_for, boulder, med_len, BRT_100m, estimate, se, lcl, ucl)

names(cott.length.preds)

#predictions of Psi for full range of BRT_100m
preds.cott.trout <- covariate.predictions(tm.cott, 
                                          data = data.frame(avgT = mean.avgT,
                                                            mFlow = mean.flow,
                                                            HAiFLS_for = mean.forest,
                                                            boulder = mean.bldr,
                                                            med_len = mean.length,
                                                            BRT_100m = trout.values),
                                          indices = 4)

head(preds.cott.trout$estimates)

cott.trout.preds <- preds.cott.trout$estimates %>%
  select(avgT, mFlow, HAiFLS_for, boulder, med_len, BRT_100m, estimate, se, lcl, ucl)

names(cott.trout.preds)

####################################################
##     Write tidy csv's for Psi predictions       ## 
####################################################
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write_csv(cott.avgT.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_avgT.csv")
write_csv(cott.flow.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_flow.csv")
write_csv(cott.forest.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_forest.csv")
write_csv(cott.bldr.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_boulder.csv")
write_csv(cott.length.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_length.csv")
write_csv(cott.trout.preds, "Data/Thesis/Tidy/Cottus_OccuMod_Predictions_trout.csv")

#-----
#avgT
#-----
avt.cott <- ggplot(data=cott.avgT.preds, aes(x=avgT))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Mean Stream Temperature (°C)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))
avt.cott

#-----
#flow
#-----
flo.cott <- ggplot(data=cott.flow.preds, aes(x=mFlow))+
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
flo.cott

#-----
#HAiFLS_for
#-----
for.cott <- ggplot(data=cott.forest.preds, aes(x=HAiFLS_for))+
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
for.cott

#-----
#boulder
#-----
boul <- ggplot(data=cott.bldr.preds, aes(x=boulder))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Boulder Coverage Index",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))
boul

#-----
#med_len
#-----
len.cott <- ggplot(data=cott.length.preds, aes(x=med_len))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Median Brown Trout TL (mm)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
len.cott


#-----
#BRT_100m
#-----
bt.cott <- ggplot(data=cott.trout.preds, aes(x=BRT_100m))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  labs(x="Brown Trout CPUE (fish/100m)",
       y=NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
bt.cott

#cowplot
#library(cowplot)
plot.cott <- plot_grid(avt.cott,boul,flo.cott,for.cott,len.cott,bt.cott, labels = NULL, ncol = 3)
plot.cott

# now add the title
title3 <- ggdraw() + 
  draw_label(
    "Sculpin",
    size = 16,
    fontfamily = "Times New Roman",
    x = 0,
    hjust = 0
  ) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 7)
  )

final.cott <- plot_grid(
  title3, plot.cott,
  ncol = 1,
  # rel_heights values control vertical title margins
  rel_heights = c(0.1, 1)
)
final.cott

#add to plot
ff <- grid.arrange(arrangeGrob(final.cott, left = y.grob))

getwd()
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
ggsave("cott_occu.png", plot=ff, dpi = 600)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
#### overall Psi figure for thesis   ####
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
plot.lnd2 <- plot_grid(avt,cob,elev,ar,bt, labels = NULL, ncol = 2, nrow = 3)
plot.lnd2

plot.srd2 <- plot_grid(srd.avt,srd.bt, labels = NULL,nrow = 3, ncol = 1)
plot.srd2
blank <- plot_grid(plot.srd2,labels = NULL, ncol = 2)
blank

plot.cott2 <- plot_grid(avt.cott,boul,flo.cott,for.cott,len.cott,bt.cott, labels = NULL, ncol = 2, nrow = 3)
plot.cott2

occupancy.figure3 <- plot_grid(plot.cott2,plot.lnd2,blank,labels = NULL, ncol = 3)
occupancy.figure3

#create common y axis label
library(gridExtra)
library(grid)
y.grob <- textGrob("Occupancy Probability (Ψ)", 
                   gp=gpar(fontface="bold", col="black", fontsize=14, fontfamily="Times New Roman"), rot=90)


Fig3 <- grid.arrange(arrangeGrob(occupancy.figure3, left = y.grob))
#ggsave("Figure_3_occupancy.png", plot = Fig3, dpi = 600)
ggsave("Figure_3_occupancy2.png", plot = Fig3, dpi = 600)

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
       y=expression(bold('Detection Probability '~bold(italic((p)))~'')))+
  scale_y_continuous(limits = c(0,1), breaks = c(0.00,0.25,0.50,0.75,1.00), labels = c("0.00","0.25","0.50","0.75","1.00"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title.y = element_text(margin = margin(r=2)))+
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
cpe <- 0.7938250
cpe2 <- 1-(1-cpe)^2
cpe3 <- 1-(1-cpe)^3
#lower confidence limit
clc <- 0.6471543
clc2 <- 1-(1-clc)^2
clc3 <- 1-(1-clc)^3
#upper confidence limit
cuc <- 0.8899001
cuc2 <- 1-(1-cuc)^2
cuc3 <- 1-(1-cuc)^3
#Dataframe
cott_cdp <- data.frame(reach = 1:3, p = c(cpe,cpe2,cpe3), lcl = c(clc,clc2,clc3),
                      ucl = c(cuc,cuc2,cuc3))

write.csv(cott_cdp, "Data/Thesis/Tidy/Sculpin_CumulativeDetectionProb.csv", row.names = F)

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
  theme(axis.title.y = element_text(margin = margin(r=9)))+
  theme(plot.title = element_text(size = 16, family = "Times New Roman"))
cott.cdp


#Longnose Dace Cumulative Detection Probability

#Chosen Model results
lnd.results.psi$p.Dot.Psi.combo2$results$real

#estimate
lpe <- 0.6871504
lpe2 <- 1-(1-lpe)^2
lpe3 <- 1-(1-lpe)^3
#lower confidence limit
llc <- 0.5796786
llc2 <- 1-(1-llc)^2
llc3 <- 1-(1-llc)^3
#upper confidence limit
luc <- 0.7776817 
luc2 <- 1-(1-luc)^2
luc3 <- 1-(1-luc)^3
#Dataframe
lnd_cdp <- data.frame(reach = 1:3, p = c(lpe,lpe2,lpe3), lcl = c(llc,llc2,llc3),
                       ucl = c(luc,luc2,luc3))
getwd()
setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
write.csv(lnd_cdp, "Data/Thesis/Tidy/LND_CumulativeDetectionProb.csv", row.names = F)

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
srd.results.psi$p.depth.Psi.combo2.4$results$real

#estimate
spe <- 0.4129207
spe2 <- 1-(1-spe)^2
spe3 <- 1-(1-spe)^3
#lower confidence limit
slc <- 0.2904924
slc2 <- 1-(1-slc)^2
slc3 <- 1-(1-slc)^3
#upper confidence limit
suc <- 0.5471558
suc2 <- 1-(1-suc)^2
suc3 <- 1-(1-suc)^3
#Dataframe
srd_cdp <- data.frame(reach = 1:3, p = c(spe,spe2,spe3), lcl = c(slc,slc2,slc3),
                      ucl = c(suc,suc2,suc3))
write.csv(srd_cdp, "Data/Thesis/Tidy/SRD_CumulativeDetectionProb.csv", row.names = F)

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
  theme(axis.title.y = element_text(margin = margin(r=10)))+
  theme(plot.title = element_text(size = 16, family = "Times New Roman"))
srd.cdp

#-----
#depth
#-----
srd.dep <- ggplot(data=srd.dprob.preds, aes(x=avdep))+
  geom_ribbon(aes(ymin=lcl, ymax=ucl), fill="grey70", alpha=0.7)+
  geom_line(aes(y=estimate), size=1, color="black")+
  labs(x="Mean Stream Depth (m)",
       y=expression(bold('Detection Probability '~bold(italic((p)))~'')))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,1),
                     breaks = c(0,0.25,0.50,0.75,1),
                     labels = c("0.00","0.25","0.50","0.75","1.00"))
srd.dep


#Plot full grided figure
sculp.p <- plot_grid(cott.cdp,cott.dp,labels = NULL,nrow = 2)
long.p <- plot_grid(lnd.cdp,nrow = 2)
south.p <- plot_grid(srd.cdp,srd.dep,nrow = 2)
plot_grid(sculp.p,long.p,south.p, ncol = 3)
ggsave("Chapt3_Figure_Dprob.png", dpi = 600)
