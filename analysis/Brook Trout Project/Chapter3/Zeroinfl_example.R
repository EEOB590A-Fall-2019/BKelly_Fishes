# Zero-inflated negative binomial model example
# response = fish count
# predictors = environmental conditions (habitat and brown trout)
# offset or exposure variable = length of stream segment sampled


#libraries
library(tidyverse)
library(pscl)
library(boot)


#load data
data <- read.csv("Data/Thesis/Tidy/cpue_data.csv", header = T)
#----------------------------------------------------------------

#Longnose Dace Count = response
#offset variable = SegLen or segment length (m)
#Predictors:
# avwid - stream width
# pctcbbl - % cobble substrate
# pctSlope - average watershed gradient (%)
# med_len - median Brown Trout length for that site
# BRT_100m - Brown Trout density


lnd.full.mod <- zeroinfl(LND_ab ~ avwid+pctcbbl+pctSlope+med_len+BRT_100m | 1,
                         data = newdata,
                         dist = "negbin",
                         offset = log(SegLen))
summary(lnd.full.mod)
#----------------------------------------------------------------


library(emmeans)
library(skimr)
# emmeans on continuous predictors
# See Russ Lenth's long response to this question:
# https://stackoverflow.com/questions/52381434/emmeans-continuous-independant-variable
# Also the "basics" vignette to emmeans

ref_grid(lnd.full.mod) #these are the mean values for all the covariates

# look at temperature and BRT_CPUE
skim(newdata)

# Plot at quantile values
emmip(lnd.full.mod, BRT_100m~avwid, at = list(avwid = c(0.82, 11.21), BRT_100m = c()), type = "response")  +
  labs(title = "(a)")+
  theme_bw()+
  theme(panel.grid = element_blank())