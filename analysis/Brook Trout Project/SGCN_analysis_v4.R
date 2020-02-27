library(tidyverse)
library(GLMMadaptive)
library(emmeans)
library(reshape2)
library(skimr)
library(MuMIn)

#load data
#getwd()
mydat <- read.csv("Data/Thesis/Tidy/SGCN_AllCovariates.csv", header=T)

#arrange watersheds as factors
mydat$HUC10 <- as.factor(sub('.*(?=.{3}$)', '', mydat$HUC_10, perl=T))
mydat$HUC12 <- as.factor(sub('.*(?=.{2}$)', '', mydat$HUC_12, perl=T))
mydat$watershed_med <- as.factor(paste(mydat$HUC8, mydat$HUC10, sep = "_"))
mydat$watershed_sm <- as.factor(paste(mydat$watershed_med, mydat$HUC12, sep = "_"))
mydat$HUC8 <- as.factor(mydat$HUC8)
mydat <- mydat %>%
  mutate_at(vars(c("BRT","LND","SRD","Cottus")), as.factor)
skim(mydat)

# Look at covariates
mydat_org <- mydat
mydat2 <- mydat[, c("LND_CPUE", "watershed_sm", "BRT_CPUE", "MEANT", "pctcbbl", "mFlow", "avwid", "HAiFLS_alt")]

ggplot(mydat, aes(x = BRT_CPUE)) + geom_histogram(binwidth = 1) # note: one really big outlier here.  Is this a valid data point? -- Yes
  mydat[which(mydat$BRT_CPUE > 50),] # row 115
  ## this data point is valid, but mostly all were YOY - ask Mike about beefing up plain BRT_CPUE
  ## to potentially BRT_CPUE if > 6inches, or Avg Body Condition, etc.
ggplot(mydat, aes(x = MEANT)) + geom_histogram(binwidth = 1) 
ggplot(mydat, aes(x = pctcbbl)) + geom_histogram(binwidth = 0.1)
ggplot(mydat, aes(x = mFlow)) + geom_histogram(binwidth = 0.01)
ggplot(mydat, aes(x = avwid)) + geom_histogram(binwidth = 0.1)
ggplot(mydat, aes(x = HAiFLS_alt)) + geom_histogram(binwidth = 1)

# Remove outlier rows 115 and 84
mydat2 <- mydat2[-c(115), ]

#Longnose Dace all additive global model with watershed_small as random effect
#zero-inflated negative binomial model fitted using GLMMadaptive package, mixed_model() function

lnd1 <- mixed_model(fixed = LND_CPUE ~ BRT_CPUE + MEANT + pctcbbl + mFlow + avwid + HAiFLS_alt,
                        random = ~ 1 | watershed_sm, data = mydat,
                        family = zi.negative.binomial(), zi_fixed = ~ 1,
                        zi_random = ~ 1 | watershed_sm, iter_EM=0)
summary(lnd1)

lnd2 <- mixed_model(fixed = LND_CPUE ~ MEANT + pctcbbl + mFlow + avwid + HAiFLS_alt,
                              random = ~ 1 | watershed_sm, data = mydat,
                              family = zi.negative.binomial(), zi_fixed = ~ 1,
                              zi_random = ~ 1 | watershed_sm, iter_EM=0)

lnd3 <- mixed_model(fixed = LND_CPUE ~ BRT_CPUE + pctcbbl + mFlow + avwid + HAiFLS_alt,
                               random = ~ 1 | watershed_sm, data = mydat,
                               family = zi.negative.binomial(), zi_fixed = ~ 1,
                               zi_random = ~ 1 | watershed_sm, iter_EM=0)

lnd4 <- mixed_model(fixed = LND_CPUE ~ BRT_CPUE + MEANT + mFlow + avwid + HAiFLS_alt,
                               random = ~ 1 | watershed_sm, data = mydat,
                               family = zi.negative.binomial(), zi_fixed = ~ 1,
                               zi_random = ~ 1 | watershed_sm, iter_EM=0)

lnd5 <- mixed_model(fixed = LND_CPUE ~ BRT_CPUE + MEANT + pctcbbl + avwid + HAiFLS_alt,
                               random = ~ 1 | watershed_sm, data = mydat,
                               family = zi.negative.binomial(), zi_fixed = ~ 1,
                               zi_random = ~ 1 | watershed_sm, iter_EM=0)
summary(lnd5)

lnd6 <- mixed_model(fixed = LND_CPUE ~ BRT_CPUE + MEANT + pctcbbl + mFlow + HAiFLS_alt,
                               random = ~ 1 | watershed_sm, data = mydat,
                               family = zi.negative.binomial(), zi_fixed = ~ 1,
                               zi_random = ~ 1 | watershed_sm, iter_EM=0)

lnd7 <- mixed_model(fixed = LND_CPUE ~ BRT_CPUE + MEANT + pctcbbl + mFlow + avwid,
                               random = ~ 1 | watershed_sm, data = mydat,
                               family = zi.negative.binomial(), zi_fixed = ~ 1,
                               zi_random = ~ 1 | watershed_sm, iter_EM=0)

AIC(lnd1,lnd2,lnd3,lnd4,lnd5,lnd6,lnd7)

lnd_2.1 <- mixed_model(fixed = LND_CPUE ~ MEANT + pctcbbl + avwid + HAiFLS_alt,
                    random = ~ 1 | watershed_sm, data = mydat,
                    family = zi.negative.binomial(), zi_fixed = ~ 1,
                    zi_random = ~ 1 | watershed_sm, iter_EM=0)
lnd_2.2 <- mixed_model(fixed = LND_CPUE ~ BRT_CPUE + pctcbbl + avwid + HAiFLS_alt,
                       random = ~ 1 | watershed_sm, data = mydat,
                       family = zi.negative.binomial(), zi_fixed = ~ 1,
                       zi_random = ~ 1 | watershed_sm, iter_EM=0)
lnd_2.3 <- mixed_model(fixed = LND_CPUE ~ BRT_CPUE + MEANT + avwid + HAiFLS_alt,
                       random = ~ 1 | watershed_sm, data = mydat,
                       family = zi.negative.binomial(), zi_fixed = ~ 1,
                       zi_random = ~ 1 | watershed_sm, iter_EM=0)
lnd_2.4 <- mixed_model(fixed = LND_CPUE ~ BRT_CPUE + MEANT + pctcbbl + HAiFLS_alt,
                       random = ~ 1 | watershed_sm, data = mydat,
                       family = zi.negative.binomial(), zi_fixed = ~ 1,
                       zi_random = ~ 1 | watershed_sm, iter_EM=0)
lnd_2.5 <- mixed_model(fixed = LND_CPUE ~ BRT_CPUE + MEANT + pctcbbl + avwid,
                       random = ~ 1 | watershed_sm, data = mydat,
                       family = zi.negative.binomial(), zi_fixed = ~ 1,
                       zi_random = ~ 1 | watershed_sm, iter_EM=0)
AIC(lnd1,lnd2,lnd3,lnd4,lnd5,lnd6,lnd7,
    lnd_2.1,lnd_2.2,lnd_2.3,lnd_2.4,lnd_2.5)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#Southern Redbally Dace all additive global model with watershed_small as random effect
#zero-inflated negative binomial model fitted using GLMMadaptive package, mixed_model() function
# Intermediate tolerance, column, not TC, native eurythermal
# Hypotheses:
# Environment: MEANT(+), %fines(+), %run(+), avdep(m), HAiFLS_alt(-)
# BRT influence: (-) column dwelling cyprinid - seems
# susceptible to predation

srd.mod <- mixed_model(fixed = SRD_CPUE ~ BRT_CPUE + MEANT + pctfines + avdep + HAiFLS_alt,
                    random = ~ 1 | watershed_sm, data = mydat,
                    family = zi.negative.binomial(), zi_fixed = ~ 1,
                    zi_random = ~ 1 | watershed_sm, iter_EM=0)
summary(srd.mod)


# emmeans on continuous predictors
  # See Russ Lenth's long response to this question:
  # https://stackoverflow.com/questions/52381434/emmeans-continuous-independant-variable
  # Also the "basics" vignette to emmeans

ref_grid(lnd5) #these are the mean values for all the covariates

  # look at temperature and BRT_CPUE
summary(mydat$MEANT)
summary(mydat$BRT_CPUE)

  # Plot at quantile values
emmip(negbin_mod, BRT_CPUE ~ MEANT, at = list(MEANT = c(9.76, 24.08), BRT_CPUE = c(0, 6.13, 7.25, 41.56)), type = "response")  +
  labs(title = "Effect of temperature on response, at levels of predator",
       subtitle = "Levels of predator are min = 25% quantile = 0, mean = 6.13, 75% quantile = 7.25, max = 41.56\nValues of other covariates held at their means")

emmip(negbin_mod, MEANT ~ BRT_CPUE, at = list(MEANT = c(9.76, 17.82, 18.98, 20.61, 24.08), BRT_CPUE = c(0, 41.5556)), type = "response") +
  labs(title = "Effect of predator on response, at levels of temperature",
       subtitle = "Levels of temperature are min = 9.76, 25% quantile = 17.82, mean = 18.94, 75% quantile = 20.61, max = 24.08\nValues of other covariates held at their means")

  # Plot effect of temp only (other covariates at their mean)
temp_rg <- ref_grid(negbin_mod, at = list(MEANT = mydat$MEANT))
temp_val <- temp_rg@grid$MEANT
temp_pred <- predict(temp_rg, type = "response")

plot_df_temp <- data.frame(MEANT = temp_val, effect = temp_pred)
ggplot(plot_df_temp, aes(x = MEANT, y = effect)) + geom_line() +
  xlab("MEANT") + ylab("Predicted effect") +
  labs(title = "Predicted effect of mean temperature on response",
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
all_rg <- predict(negbin_mod, newdata = mydat[,c(3:8)]) # This gives you predictions at your data points
plot_df_compare <- data.frame(obs_value = mydat$LND_CPUE, estimate = all_rg)
ggplot(data = plot_df_compare, aes(x = obs_value, y = estimate)) + geom_point() +
  xlab("Observed CPUE") + ylab("Predicted CPUE")
# This looks weird because you're using a two-part model.  The line of points at zero is the zero-inflated part of the model.