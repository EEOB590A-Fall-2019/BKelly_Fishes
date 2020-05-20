#Chapter three statistical analysis:
##Adundace comparisons and models

library(tidyverse)
library(skimr)
library(cowplot)
library(coin)

#Terms to define:
#LND = Longnose Dace
#SRD = Southern Redbelly Dace
#Cottus = the genus of Sculpins 
#BRT = Brown Trout
#SGCN = Species of Greatest Conservation Need
#CPUE = Catch Per Unit Effort; in this case = fish/100m or "fish per 100 meter of stream distance sampled"
#TC = Top Carnivore
#Eurythermal = wide thermal tolerance
#Sympatry = alongside Brown Trout 
#Allopatry = in the absence of Brown Trout


#load data
newdat <- read.csv("Data/Thesis/Tidy/chpt3_tidy.csv", header=T)
mydat <- read.csv("Data/Thesis/Tidy/SGCN_AllCovariates.csv", header=T)
ef <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header=T) %>%
  select(HUC_Site, effsec) %>%
  rename(newID = HUC_Site)

names(newdat)
names(mydat)

cobble <- mydat %>%
  select(newID, pctcbbl, SegLen, LND_ab, SRD_ab, Cottus_ab, Area_km2=CatArea_km2)

cobble[97,7] = 28.8615

newdata <- left_join(newdat, cobble, by="newID")
newdata <- left_join(newdata, ef, by="newID")

summary(newdata)

write.csv(newdata, "Data/Thesis/Tidy/cpue_data.csv", row.names = F)

#inspect
skim(mydat)

#wrangle
mydat <- mydat %>%
  mutate_at(vars(c("BRT","LND","SRD","Cottus")), as.factor)




#############################################################################
#----------
#collinearity assessment 
#----------
library(corrplot)
correl <- newdata %>%
  select(Cottus_CPUE, SRD_CPUE, LND_CPUE, Area_km2, pctcbbl, pctSlope,
         avgT, pctfines, avdep, mFlow, HAiFLS_for, med_len, BRT_100m)
  
c <- cor(correl)
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
p.mat <- cor.mtest(lnd2[,4:22])

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





#############################################################################
###############For Map##################
rand <- read.csv("Data/Thesis/Tidy/AllCovariates.csv", header=T)
names(rand)
rand2 <- rand %>%
  select(newID=HUC_Site, Xcoord=Easting, Ycoord=Northing)

fish_part <- mydat %>%
  select(newID, BRT, LND, SRD, Cottus) %>%
  mutate_at(vars(c("LND","SRD","Cottus")), as.numeric)%>%
  mutate(SGCN=(LND+SRD+Cottus))

for_map <- left_join(rand2, fish_part, by="newID") %>%
  filter(newID != "UPI_29") %>%
  filter(newID != "YEL_33")

for_map[97,2] <- 569734
for_map[97,3] <- 4804779
for_map[97,]

for_map2 <- for_map %>%
  mutate(status = ifelse(SGCN < 1 & BRT == 0,0,ifelse(SGCN>0 & BRT==0,1,ifelse(SGCN<1 & BRT==1,2,ifelse(SGCN>0
                                                                                                        &BRT==1,3,NA)))))

write.csv(for_map2, "Data/Thesis/Spatial/Map_chpt3_pointsv2.csv", row.names = F)
########################################
#############################################################################
#----------------------------Boxplots of CPUE------------------------------#

#-----------------------Filter by presence of SGCN-------------------------#
library(extrafont)
#font_import()
loadfonts(device="win")   #Register fonts for Windows bitmap output
fonts() 

#LND
ldace <- mydat %>%
  filter(LND == 1)

p2 <- ggplot(data = ldace, aes(x=BRT,y=LND_CPUE)) +
  stat_boxplot(geom = 'errorbar', width=0.25)+
  geom_boxplot(aes(fill=BRT), width = 0.50)+
  labs(x=NULL, y=NULL)+
  theme_bw()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")+
  scale_fill_manual(values = c("white", "grey"))+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  ggtitle("Longnose Dace")+
  theme(plot.title = element_text(size=16, family = "Times New Roman"))+
  theme(axis.text.x = element_text(size=12, family = "Times New Roman"))
p2

#SRD
sdace <- mydat %>%
  filter(SRD == 1)

p3 <- ggplot(data = sdace, aes(x=BRT,y=SRD_CPUE)) +
  stat_boxplot(geom = 'errorbar', width=0.25)+
  geom_boxplot(aes(fill=BRT), width=0.50)+
  labs(x=NULL, y=NULL)+
  theme_bw()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")+
  scale_fill_manual(values = c("white", "grey"))+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  ggtitle("Southern Redbelly Dace")+
  theme(plot.title = element_text(size=16, family = "Times New Roman"))+
  theme(axis.text.x = element_text(size=12, family = "Times New Roman"))
p3
  

#Sculpins
cott <- mydat %>%
  filter(Cottus == 1)

p1 <- ggplot(data = cott, aes(x=BRT,y=Cottus_CPUE)) +
  stat_boxplot(geom = 'errorbar', width=0.25)+
  geom_boxplot(aes(fill=BRT), width=0.50)+
  labs(x=NULL, y="Catch Per Unit Effort (fish/100m)")+
  theme_bw()+
  scale_x_discrete(labels=c("Absent", "Present"))+
  theme(legend.position = "NULL")+
  scale_fill_manual(values = c("white", "grey"))+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  ggtitle("Sculpin")+
  theme(plot.title = element_text(size=16, family = "Times New Roman"))+
  theme(axis.text.x = element_text(size=12, family = "Times New Roman"))
p1


#Overall figure for thesis:
boxplot.figure <- plot_grid(p1, p2, p3, labels = NULL, ncol=3)
boxplot.figure

#create common x axis label
library(gridExtra)
library(grid)
x.grob <- textGrob("Brown Trout Status (Presence/Absence)", 
                   gp=gpar(col="black", fontsize=14, fontfamily="Times New Roman", fontface="bold"))
#add to plot
bf2 <- grid.arrange(arrangeGrob(boxplot.figure, bottom = x.grob))
bf2

ggsave("Figure_4_boxplots.png", plot = bf2, dpi = 600)






#############################################################################

# Mann Whitney U / Wilcox Sign Rank Test 

# using subsetted data -- only when SGCNs of interest are present

help("wilcox.test")

# Ho: Median CPUE of SGCN when BRT are present = CPUE when BRT are absent
# two-sided

#-----
#LND
#-----
class(ldace$BRT)
wilcox.test(ldace$LND_CPUE ~ ldace$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
# no difference

#-----
#SRD
#-----
class(sdace$BRT)
wilcox.test(sdace$SRD_CPUE ~ sdace$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
#no difference
#-----
#Cottus
#-----
class(cott$BRT)
wilcox.test(cott$Cottus_CPUE ~ cott$BRT, mu=0, alt="two.sided", conf.int=T, conf.level=0.95, paired=F,
            exact=F)
#no difference

#############################################################################

# Using the "coin" package
# Exact Wilcoxon Mann Whitney Rank Sum Test
# where y is numeric and A is a binary factor

#LND
wilcox_test(LND_CPUE~BRT, data=ldace, distribution="exact") #p = 0.95
#SRD
wilcox_test(SRD_CPUE~BRT, data=sdace, distribution="exact") #p = 0.25
#Cottus
wilcox_test(Cottus_CPUE~BRT, data=cott, distribution="exact") #p = 1

# One-Way Permutation Test based on 9999 Monte-Carlo
# resamplings. y is numeric and A is a categorical factor

#LND
oneway_test(LND_CPUE~BRT, data=ldace,
            distribution=approximate(B=9999)) #p = 0.95
#SRD
oneway_test(SRD_CPUE~BRT, data=sdace,
            distribution=approximate(B=9999)) #p = 0.16
#Cottus
oneway_test(Cottus_CPUE~BRT, data=cott,
            distribution=approximate(B=9999)) #p = 0.91
#############################################################################

# Results:
# Longnose Dace:
# occurr = 33
# sympatry = 19, allopatry = 14
# no significant difference in CPUE when BRT present vs. absent
# Southern Redbelly Dace:
# occurr = 33
# sympatry = 12, allopatry = 21
# no significant difference in CPUE when BRT present vs. absent
# Sculpins:
# occurr = 20
# sympatry = 18, allopatry = 2
# no significant difference in CPUE when BRT present vs. absent

#############################################################################





#############################################################################

#---------------------------------CPUE models--------------------------------

#############################################################################

#explore data
names(newdat)


#inspect response variable(s)
ggplot(newdat, aes(LND_CPUE))+
  geom_histogram(binwidth = 10) #Longnose Dace (CPUE)

ggplot(newdat, aes(SRD_CPUE))+
  geom_histogram(binwidth = 10) #Southern Redbelly Dace CPUE

ggplot(newdat, aes(Cottus_CPUE))+
  geom_histogram(binwidth = 10) #Sculpins CPUE

#inspect predictor variables
predictors <- newdat %>%
  select(avwid, pctfines, pctriffle, BrBank, MEANT, HAiFLS_alt, HAiFLS_for, pctSlope, avgT, mFlow, 
         adult_100m, med_len, mean_len)

ggplot(predictors, aes(avwid))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(pctfines))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(pctriffle))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(BrBank))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(MEANT))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(HAiFLS_alt))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(HAiFLS_for))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(pctSlope))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(avgT))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(mFlow))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(adult_100m))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(med_len))+
  geom_histogram(binwidth = 1)
ggplot(predictors, aes(mean_len))+
  geom_histogram(binwidth = 1)

#############################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Let's build some CPUE models for each non-game fish with a combination of 
# hypotheses: 1) full model, 2) environment only, 3) BRT variables only, 
# 4) null model
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

# Longnose Dace
# Intermediate tolerance, benthic, not TC, native eurythermal
# Hypotheses:
# Environment: %cobble(+), avwidth(+), pctSlope(-)
# BRT influence: (~) Habitat overlap seemingly with BRT, benthic sp. 
# maybe less susceptible to predation 

# Southern Redbelly Dace
# Intermediate tolerance, column, not TC, native eurythermal
# Hypotheses:
# Environment: MEANT(+), HAiFLS_alt(-), avdep(+)
# BRT influence: (-) column dwelling cyprinid - seems
# susceptible to predation

#Sculpins
# Intolerant, not TC, native coldwater, benthic
# Hypotheses:
# Environment: avgT(-), BrBank(-), HAiFLS_for(+), mFlow(-)
# BRT influence: (+) habitat overlap, BRT invade -- abundance more
# of a function of habitat since intolerant, and coldwater

#############################################################################
#libraries
library(pscl)
#library(MASS)
library(boot)


names(newdata)
summary(newdata)

#Longnose Dace Count = response, segment length = offset
#zero-inflated negative binomial model 

lnd.full.mod <- zeroinfl(LND_ab ~ Area_km2+pctcbbl+pctSlope+med_len+BRT_100m | 1,
               data = newdata,
               dist = "negbin",
               offset = log(SegLen))
summary(lnd.full.mod)

lnd.env.mod <- zeroinfl(LND_ab ~ Area_km2+pctcbbl+pctSlope | 1,
                        data = newdata,
                        dist = "negbin",
                        offset = log(SegLen)
)
summary(lnd.env.mod)

lnd.brt.mod <- zeroinfl(LND_ab ~ med_len+BRT_100m | 1,
                        data = newdata,
                        dist = "negbin",
                        offset = log(SegLen)
)
summary(lnd.brt.mod)

lnd.null.mod <- zeroinfl(LND_ab ~ 1 | 1,
                        data = newdata,
                        dist = "negbin",
                        offset = log(SegLen)
)
summary(lnd.null.mod)

#Compare models
library(MuMIn)
lnd.mod.AICc <- model.sel(lnd.full.mod, lnd.env.mod, lnd.brt.mod, lnd.null.mod)
lnd.mod.AICc

#export model table
lnd.cpue.table <- as.data.frame(lnd.mod.AICc) %>%
  select(df, logLik, AICc, delta, weight)

write.csv(lnd.cpue.table, "Data/Thesis/Tidy/lnd_cpue_ModelTable.csv", row.names = T)

#-----
#extracting confidence intervals for the parameters

#top model 
dput(round(coef(lnd.full.mod, "count"), 4)) #count process
dput(round(coef(lnd.full.mod, "zero"), 4)) #extra zero process

f <- function(data, i) {
  require(pscl)
  m <- zeroinfl(LND_ab ~ avwid+pctcbbl+pctSlope+med_len+BRT_100m | 1,
                data = data[i, ],
                dist = "negbin",
                offset = log(SegLen),
                start = list(count = c(-7.724,0.7464,0.0186,-0.0781,
                                       0.0027,-0.0808),
                             zero = c(-8.7639)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
(res <- boot(newdata, f, R = 10000))

## basic parameter estimates with percentile and bias adjusted CIs
parms <- t(sapply(c(1, 3, 5, 7, 9, 11, 15), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaUL = bca[5]))
}))

## add row names
row.names(parms) <- names(coef(lnd.full.mod))
## print results
parms

## compare with normal based approximation
confint(lnd.full.mod)

## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms <- t(sapply(c(1, 3, 5, 7, 9, 11, 15), function(i) {
  out <- boot.ci(res, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaUL = bca[5]))
}))

## add row names
row.names(expparms) <- names(coef(lnd.full.mod))
## print results
expparms



#########################################################################
lnd_log_dat <- newdata %>%
  mutate(log_lnd = log(1+LND_CPUE))

a <- ggplot(lnd_log_dat, aes(x = Area_km2, y = LND_CPUE)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = bquote(bold('Total Catchment Area'~(km^2))), y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))
  #scale_y_continuous(limits = c(0,4),
  #                   breaks = c(0,1,2,3,4),
  #                   labels = c("0","1","2","3","4"))
a


b <- ggplot(lnd_log_dat, aes(x = pctcbbl, y = LND_CPUE)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "% Cobble Substrate", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,4),
                     breaks = c(0,1,2,3,4),
                     labels = c("0","1","2","3","4"))


c <- ggplot(lnd_log_dat, aes(x = pctSlope, y = log_lnd)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "% Catchment Slope", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,4),
                     breaks = c(0,1,2,3,4),
                     labels = c("0","1","2","3","4"))


d <- ggplot(lnd_log_dat, aes(x = med_len, y = log_lnd)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "Median Brown Trout TL (mm)", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,4),
                     breaks = c(0,1,2,3,4),
                     labels = c("0","1","2","3","4"))



e <- ggplot(lnd_log_dat, aes(x = BRT_100m, y = LND_CPUE)) +
  geom_point(
    shape=2,
    color="black",
    fill="grey70",
    ) +
  labs(x = "Brown Trout CPUE (fish/100m)", y = "Longnose Dace CPUE (fish/100m)")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,4),
                     breaks = c(0,1,2,3,4),
                     labels = c("0","1","2","3","4"))
e

#cowplot
library(cowplot)
horiz.lnd <- plot_grid(a,b,c,d,e, labels = NULL, ncol = 5)
horiz.lnd

title1 <- ggdraw()+
  draw_label("Longnose Dace",
             fontfamily = "Times New Roman",
             size = 16,
             x=0,
             hjust = 0)+
  theme(plot.margin = margin(0,0,0,7))

cpue2 <- plot_grid(title1, horiz.lnd, ncol = 1,
          rel_heights = c(0.1,1))

#create common y axis label
library(gridExtra)
library(grid)
y.grob <- textGrob("Log CPUE (fish/100m)", 
                   gp=gpar(fontface="bold", col="black", fontsize=14, fontfamily="Times New Roman"), rot=90)
#add to plot
#lnd.f <- grid.arrange(arrangeGrob(vert.lnd, left = y.grob))

getwd()
#setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
ggsave("lnd_cpue.png", plot=lnd.f, dpi = 600)
#########################################################################





#Sculpin = response, segment length = offset
#zero-inflated negative binomial model 

cott.full.mod <- zeroinfl(Cottus_ab ~ avgT+HAiFLS_for+mFlow+med_len+BRT_100m | 1,
                         data = newdata,
                         dist = "negbin",
                         offset = log(SegLen))
summary(cott.full.mod)

cott.env.mod <- zeroinfl(Cottus_ab ~ avgT+HAiFLS_for+mFlow | 1,
                        data = newdata,
                        dist = "negbin",
                        offset = log(SegLen)
)
summary(cott.env.mod)

cott.brt.mod <- zeroinfl(Cottus_ab ~ med_len+BRT_100m | 1,
                        data = newdata,
                        dist = "negbin",
                        offset = log(SegLen)
)
summary(cott.brt.mod)

cott.null.mod <- zeroinfl(Cottus_ab ~ 1 | 1,
                         data = newdata,
                         dist = "negbin",
                         offset = log(SegLen)
)
summary(cott.null.mod)

#Compare models
cott.mod.AICc <- model.sel(cott.full.mod, cott.env.mod, cott.brt.mod, cott.null.mod)
cott.mod.AICc

#export model table
cott.cpue.table <- as.data.frame(cott.mod.AICc) %>%
  select(df, logLik, AICc, delta, weight)

write.csv(cott.cpue.table, "Data/Thesis/Tidy/cottus_cpue_ModelTable.csv", row.names = T)


#-----
#extracting confidence intervals for the parameters

#top model 
dput(round(coef(cott.full.mod, "count"), 4)) #count process
dput(round(coef(cott.full.mod, "zero"), 4)) #extra zero process

ff <- function(data, i) {
  require(pscl)
  m <- zeroinfl(Cottus_ab ~ avgT+HAiFLS_for+mFlow+med_len+BRT_100m | 1,
                data = data[i, ],
                dist = "negbin",
                offset = log(SegLen),
                start = list(count = c(-1.561,-0.4688, 
                                       0.0859,-13.0608,0.0186,0.0602),
                             zero = c(-5.1326)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
(res.cott <- boot(newdata, ff, R = 10000))

## basic parameter estimates with percentile and bias adjusted CIs
parms.cott <- t(sapply(c(1, 3, 5, 7, 9, 11, 15), function(i) {
  out <- boot.ci(res.cott, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaUL = bca[5]))
}))

## add row names
row.names(parms.cott) <- names(coef(cott.full.mod))
## print results
parms.cott

## compare with normal based approximation
confint(cott.full.mod)

## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms.cott <- t(sapply(c(1, 3, 5, 7, 9, 11, 15), function(i) {
  out <- boot.ci(res.cott, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaUL = bca[5]))
}))

## add row names
row.names(expparms.cott) <- names(coef(cott.full.mod))
## print results
expparms.cott



#########################################################################
cott_log_dat <- newdata %>%
  mutate(log_cott = log(1+Cottus_CPUE))

plot(newdata$avgT, newdata$Cottus_ab)
plot(newdata$avgT, newdata$Cottus_CPUE)

aa <- ggplot(cott_log_dat, aes(x = avgT, y = log_cott)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "Mean Summer Stream Temperature (°C)", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,5),
                     breaks = c(0,1,2,3,4,5),
                     labels = c("0","1","2","3","4","5"))
aa


bb <- ggplot(cott_log_dat, aes(x = mFlow, y = log_cott)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "Mean Stream Velocity (m/s)", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,5),
                     breaks = c(0,1,2,3,4,5),
                     labels = c("0","1","2","3","4","5"))
bb


cc <- ggplot(cott_log_dat, aes(x = HAiFLS_for, y = log_cott)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "% HAiFLS Forest Land Cover", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,5),
                     breaks = c(0,1,2,3,4,5),
                     labels = c("0","1","2","3","4","5"))
cc


dd <- ggplot(cott_log_dat, aes(x = med_len, y = log_cott)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "Median Brown Trout TL (mm)", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,5),
                     breaks = c(0,1,2,3,4,5),
                     labels = c("0","1","2","3","4","5"))
dd


ee <- ggplot(cott_log_dat, aes(x = BRT_100m, y = log_cott)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "Brown Trout Density (fish/100m)", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,5),
                     breaks = c(0,1,2,3,4,5),
                     labels = c("0","1","2","3","4","5"))
ee

#cowplot
horiz.cott <- plot_grid(aa,bb,cc,dd,ee, labels = NULL, ncol = 5)
horiz.cott

title2 <- ggdraw()+
  draw_label("Sculpin",
             fontfamily = "Times New Roman",
             size = 16,
             x=0,
             hjust = 0)+
  theme(plot.margin = margin(0,0,0,7))

cpue1 <- plot_grid(title2, horiz.cott, ncol = 1,
          rel_heights = c(0.1,1))
cpue1


#######################################################################################



#SRD = response, segment length = offset
#zero-inflated negative binomial model 

srd.full.mod <- zeroinfl(SRD_ab ~ avgT+pctfines+avdep+med_len+BRT_100m | 1,
                          data = newdata,
                          dist = "negbin",
                          offset = log(SegLen))
summary(srd.full.mod)

srd.env.mod <- zeroinfl(SRD_ab ~ avgT+pctfines+avdep | 1,
                         data = newdata,
                         dist = "negbin",
                         offset = log(SegLen)
)
summary(srd.env.mod)

srd.brt.mod <- zeroinfl(SRD_ab ~ med_len+BRT_100m | 1,
                         data = newdata,
                         dist = "negbin",
                         offset = log(SegLen)
)
summary(srd.brt.mod)

srd.null.mod <- zeroinfl(SRD_ab ~ 1 | 1,
                          data = newdata,
                          dist = "negbin",
                          offset = log(SegLen)
)
summary(srd.null.mod)

#Compare models
srd.mod.AICc <- model.sel(srd.full.mod, srd.env.mod, srd.brt.mod, srd.null.mod)
srd.mod.AICc

#export model table
srd.cpue.table <- as.data.frame(srd.mod.AICc) %>%
  select(df, logLik, AICc, delta, weight)

write.csv(srd.cpue.table, "Data/Thesis/Tidy/SRD_cpue_ModelTable.csv", row.names = T)


#-----
#extracting confidence intervals for the parameters

#top model 
dput(round(coef(srd.full.mod, "count"), 4)) #count process
dput(round(coef(srd.full.mod, "zero"), 4)) #extra zero process

f3 <- function(data, i) {
  require(pscl)
  m <- zeroinfl(SRD_ab ~ avgT+pctfines+avdep+med_len+BRT_100m | 1,
                data = data[i, ],
                dist = "negbin",
                offset = log(SegLen),
                start = list(count = c(-17.7018,0.8004,-0.0176, 3.6761,-0.0052, -0.3086),
                             zero = c(-9.1721)))
  as.vector(t(do.call(rbind, coef(summary(m)))[, 1:2]))
}

set.seed(10)
(res.srd <- boot(newdata, f3, R = 10000))

## basic parameter estimates with percentile and bias adjusted CIs
summary(srd.full.mod)

parms.srd <- t(sapply(c(1, 3, 5, 7, 9, 11, 15), function(i) {
  out <- boot.ci(res.srd, index = c(i, i + 1), type = c("perc", "bca"))
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaUL = bca[5]))
}))

## add row names
row.names(parms.srd) <- names(coef(srd.full.mod))
## print results
parms.srd

## compare with normal based approximation
confint(srd.full.mod)

## exponentiated parameter estimates with percentile and bias adjusted CIs
expparms.srd <- t(sapply(c(1, 3, 5, 7, 9, 11, 15), function(i) {
  out <- boot.ci(res.srd, index = c(i, i + 1), type = c("perc", "bca"), h = exp)
  with(out, c(Est = t0, pLL = percent[4], pUL = percent[5],
              bcaLL = bca[4], bcaUL = bca[5]))
}))

## add row names
row.names(expparms.srd) <- names(coef(srd.full.mod))
## print results
expparms.srd

############################################################################

srd_log_dat <- newdata %>%
  mutate(log_srd = log(1+SRD_CPUE))

aaa <- ggplot(srd_log_dat, aes(x = avgT, y = log_srd)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "Mean Summer Stream Temperature (°C)", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,4),
                     breaks = c(0,1,2,3,4),
                     labels = c("0","1","2","3","4"))
aaa


bbb <- ggplot(srd_log_dat, aes(x = pctfines, y = log_srd)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "% Fine Substrates (clay, silt, sand)", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,4),
                     breaks = c(0,1,2,3,4),
                     labels = c("0","1","2","3","4"))
bbb


ccc <- ggplot(srd_log_dat, aes(x = avdep, y = log_srd)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "Mean Stream Depth (m)", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,4),
                     breaks = c(0,1,2,3,4),
                     labels = c("0","1","2","3","4"))
ccc


ddd <- ggplot(srd_log_dat, aes(x = med_len, y = log_srd)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "Median Brown Trout TL (mm)", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,4),
                     breaks = c(0,1,2,3,4),
                     labels = c("0","1","2","3","4"))
ddd


eee <- ggplot(srd_log_dat, aes(x = BRT_100m, y = log_srd)) +
  geom_point(
    color="Black",
    fill="white",
    shape=21,
    alpha=0.75,
    size=2,
    stroke = 1
  ) +
  geom_smooth(method = "lm", se=F, color="black", size=1.25)+
  labs(x = "Brown Trout Density (fish/100m)", y = NULL)+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,4),
                     breaks = c(0,1,2,3,4),
                     labels = c("0","1","2","3","4"))
eee

#cowplot
horiz.srd <- plot_grid(aaa,bbb,ccc,ddd,eee, labels = NULL, ncol = 5)
horiz.srd

title3 <- ggdraw()+
  draw_label("Southern Redbelly Dace",
             fontfamily = "Times New Roman",
             size = 16,
             x=0,
             hjust = 0)+
  theme(plot.margin = margin(0,0,0,7))

cpue3 <- plot_grid(title3, horiz.srd, ncol = 1,
                   rel_heights = c(0.1,1))
cpue3


#########################################################################

cpue.plot <- plot_grid(cpue1,cpue2,cpue3, ncol = 1)
#cpue.plot

#add to plot
cpue.plot2 <- grid.arrange(arrangeGrob(cpue.plot, left = y.grob))
#cpue.plot2

getwd()
#setwd("C:/Users/bbkelly/Documents/Brook Trout_Brett/BKelly_Fishes_GithubRepos")
ggsave("chpt3_Figure_Five.png", plot=cpue.plot2, width = 19, height = 10.5, units = "in", dpi = 600)


















#########################################################################
library(emmeans)
library(skimr)
# emmeans on continuous predictors
# See Russ Lenth's long response to this question:
# https://stackoverflow.com/questions/52381434/emmeans-continuous-independant-variable
# Also the "basics" vignette to emmeans

ref_grid(lnd.full.mod) #these are the mean values for all the covariates

#
skim(newdata)

# Plot at quantile values
emmip(lnd.full.mod, BRT_100m~avwid, at = list(avwid = c(2,4,6,8,10), 
                                              BRT_100m = c(6.73)), type = "response")  +
  theme_bw()+
  theme(panel.grid = element_blank())
  

#emmip(negbin_mod, MEANT ~ BRT_CPUE, at = list(MEANT = c(9.76, 17.82, 18.98, 20.61, 24.08), BRT_CPUE = c(0, 41.5556)), type = "response") +
#  labs(title = "Effect of predator on response, at levels of temperature",
#       subtitle = "Levels of temperature are min = 9.76, 25% quantile = 17.82, mean = 18.94, 75% quantile = 20.61, max = 24.08\nValues of other covariates held at their means")

# Plot effect of temp only (other covariates at their mean)
width_rg <- ref_grid(lnd.full.mod, at = list(avwid = newdata$avwid))
width_val <- width_rg@grid$avwid
width_pred <- predict(width_rg, type = "response")

plot_df_width <- data.frame(avwid = width_val, effect = width_pred)
ggplot(plot_df_width, aes(x = avwid, y = effect)) + geom_line() +
  xlab("avwid") + ylab("Predicted effect") +
  labs(title = "Predicted effect of mean stream width on LND CPUE",
       subtitle = "Other covariates held at their mean values")

# Plot effect of predator
brt_rg <- ref_grid(negbin_mod, at = list(BRT_CPUE = mydat$BRT_CPUE))
brt_val <- brt_rg@grid$BRT_CPUE
brt_pred <- predict(brt_rg, type = "response")

plot_df_brt <- data.frame(BRT_CPUE = brt_val, effect = brt_pred)
ggplot(plot_df_brt, aes(x = BRT_CPUE, y = effect)) + geom_line() +
  xlab("BRT_CPUE") + ylab("Predicted effect") +
  labs(title = "Predicted effect of predator on response",
       subtitle = "Other covariates held at their mean values")

# Plot them together
plot_df_merge <- merge(plot_df_brt, plot_df_temp, by = "effect", all.x = T, all.y = T)
plot_df_both <- melt(plot_df_merge, id.vars = "effect"); rm(plot_df_merge)
ggplot(plot_df_both, aes(x = value, y = effect, color = variable)) + geom_line() +
  xlab("Predicted effect") + ylab("Value of covariate") +
  scale_color_discrete(name = "covariate") +
  labs(title = "Effect of individual covariates on response",
       subtitle = "All other covariate values held at their mean")
# You can extend this to keep adding your covariates





# Plot predicted vs observed
ld.df <- newdata %>%
  select(avwid, pctSlope, pctcbbl, med_len, BRT_100m, SegLen)

all_rg <- predict(lnd.full.mod, newdata = ld.df)# This gives you predictions at your data points

plot_df_compare <- data.frame(obs_value = newdata$LND_ab, estimate = all_rg)
ggplot(data = plot_df_compare, aes(x = obs_value, y = estimate)) + geom_point() +
  xlab("Observed CPUE") + ylab("Predicted CPUE")+
  scale_y_continuous(limits = c(0,200))
# This looks weird because you're using a two-part model.  The line of points at zero is the zero-inflated part of the model.









#########################################################################
#area
min.area <- min(newdata$Area_km2)
max.area <- max(newdata$Area_km2)
area.values <- seq(from = min.area, to = max.area, length = 100)
mean.area <- mean(newdata$Area_km2)
#cobble
min.cobble <- min(newdata$pctcbbl)
max.cobble <- max(newdata$pctcbbl)
cobble.values <- seq(from = min.cobble, to = max.cobble, length = 100)
mean.cobble <- mean(newdata$pctcbbl)
#slope
min.slope <- min(newdata$pctSlope)
max.slope <- max(newdata$pctSlope)
slope.values <- seq(from = min.slope, to = max.slope, length = 100)
mean.slope <- mean(newdata$pctSlope)
#median TL
min.length <- min(newdata$med_len)
max.length <- max(newdata$med_len)
length.values <- seq(from = min.length, to = max.length, length = 100)
mean.length <- mean(newdata$med_len)
#brown trout density
min.trout <- min(newdata$BRT_100m)
max.trout <- max(newdata$BRT_100m)
trout.values <- seq(from = min.trout, to = max.trout, length = 100)
mean.trout <- mean(newdata$BRT_100m)
med.trout <- median(newdata$BRT_100m)
#Segment Length
segment <- 100
#########################################################################

#new df for predictions: based on width
names(newdata)

df1 <- data.frame(Area_km2 = area.values,
                           pctcbbl = mean.cobble,
                           pctSlope = mean.slope,
                           SegLen = segment)



#################################################################################
#########################################################################

newdata$resp_lnd <- predict(lnd.env.mod)

plot(newdata$resp_lnd, newdata$LND_CPUE)

ggplot(aes(x=resp_lnd,y=LND_ab), data = newdata)+
  geom_point()+
  scale_x_continuous(limits=c(0,150))
#################################################################################
#########################################################################


c.1 <- ggplot(newdata, aes(x=BRT_100m, y=LND_CPUE)) +
  geom_point(
    color="black",
    fill="grey70",
    shape=18,
    alpha=0.5,
    size=3,
    stroke = 2
  ) +
  labs(x = "Brown Trout CPUE (fish/100m)", y = "Longnose Dace CPUE (fish/100m)")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,40),
                     breaks = c(0,10,20,30,40,50),
                     labels = c("0","10","20","30","40","50"))
c.1




c.2 <- ggplot(newdata, aes(x=BRT_100m, y=Cottus_CPUE)) +
  geom_point(
    color="black",
    fill="grey70",
    shape=18,
    alpha=0.5,
    size=3,
    stroke = 2
  ) +
  labs(x = "Brown Trout CPUE (fish/100m)", y = "Sculpin CPUE (fish/100m)")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,100),
                     breaks = c(0,25,50,75,100),
                     labels = c("0","25","50","75","100"))
c.2


c.3 <- ggplot(newdata, aes(x=BRT_100m, y=SRD_CPUE)) +
  geom_point(
    color="black",
    fill="grey70",
    shape=18,
    alpha=0.5,
    size=3,
    stroke = 2
  ) +
  labs(x = "Brown Trout CPUE (fish/100m)", y = "Southern Redbelly Dace CPUE (fish/100m)")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold", size = 14, family = "Times New Roman"))+
  scale_y_continuous(limits = c(0,50),
                     breaks = c(0,10,20,30,40,50),
                     labels = c("0","10","20","30","40","50"))
c.3

library(cowplot)
cp.plot <- plot_grid(c.2,c.1,c.3, ncol=3)
cp.plot

ggsave("chpt3_Figure_Five.png", plot=cp.plot, dpi = 600)


