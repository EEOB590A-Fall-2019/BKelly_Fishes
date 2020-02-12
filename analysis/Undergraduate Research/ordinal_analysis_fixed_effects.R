library(ordinal)
library(ggplot2)
library(car)

### Massage the dataset
mydat <- read.csv("Data/Thesis/Tidy/FIBI_tidy2.csv", header = T)
mydat$Rating <- factor(mydat$Rating, levels = c("Very Poor", "Poor", "Fair", "Good", "Excellent"))
mydat$Year <- factor(mydat$Year, levels = c("2018", "2019"))
table(mydat$Rating)
with(mydat, table(Rating, HUC8))

mydat$HUC10 <- as.factor(sub('.*(?=.{3}$)', '', mydat$HUC_10, perl=T))
mydat$HUC12 <- as.factor(sub('.*(?=.{2}$)', '', mydat$HUC_12, perl=T))
mydat$watershed_med <- as.factor(paste(mydat$HUC8, mydat$HUC10, sep = "_"))
mydat$watershed_sm <- as.factor(paste(mydat$watershed_med, mydat$HUC12, sep = "_"))

with(mydat, table(Rating, watershed_med))
with(mydat, table(Year, watershed_med))
temp_df <- as.data.frame(with(mydat, table(Year, watershed_sm)))
ggplot(temp_df, aes(x = watershed_sm, y = Freq, color = Year)) + geom_point()
rm(temp_df)

### Models

#ord_mod_eqi <- clm(Rating ~ Year + watershed_med, data = mydat, threshold = "equidistant")
#ord_mod <- clm(Rating ~ Year + watershed_med, data = mydat)

#ord_mod_eqi <- clm(Rating ~ Year + watershed_med + MEANT + pctrun + pctrock + pctShade + pctBrBnk, data = mydat, threshold = "equidistant")
#ord_mod <- clm(Rating ~ Year + watershed_med + MEANT + pctrun + pctrock + pctShade + pctBrBnk, data = mydat)

ord_mod_eqi <- clm(Rating ~ Year + watershed_med + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for, data = mydat, threshold = "equidistant")
ord_mod <- clm(Rating ~ Year + watershed_med + MEANT + pctrun + pctrock + pctShade + pctBrBnk + HAiFLS_dev + HAiFLS_for, data = mydat)

convergence(ord_mod_eqi)
convergence(ord_mod)

summary(ord_mod_eqi)
summary(ord_mod)

anova(ord_mod_eqi, ord_mod) # Are flexible thresholds better supported?  (LRT test of two models)
ord_mod <- ord_mod_eqi

Anova(ord_mod, type = 3)

### Model fit
pr1 <- profile(ord_mod)
plot(pr1)

#slice_mle <- slice(ord_mod, lambda = 1e-2)
#plot(slice_mle)

rm(pr1, slice_mle)

### Predictions
  # Plot estimated probability for data class (with s.e.)
predictions <- predict(ord_mod, se.fit = T, interval = T) # fit is the probability associated with the class given in the data (not necessarily the class with the highest probability - see next section)
predictions <- as.data.frame(do.call(cbind, predictions))
predictions <- cbind(watershed = mydat$watershed_med, year = mydat$Year, IBIscore = mydat$IBIScore, predictions)
predictions$obs <- rownames(predictions)
predictions$IBIrank <- rank(predictions$IBIscore, ties.method = "first")
predictions$fit_rank <- rank(predictions$fit, ties.method = "first")

ggplot(predictions, aes(x = IBIrank, y = fit, color = watershed)) + geom_point() +
  geom_segment(aes(x = IBIrank, y = lwr, xend = IBIrank, yend = upr), data = predictions) +
  ylab("probability") + xlab("rank of IBI score") + ggtitle("Fitted probabilities (95% profile CI) of observation landing in class identified by data")

ggplot(predictions, aes(x = fit_rank, y = fit, color = IBIscore)) + geom_point() +
  geom_segment(aes(x = fit_rank, y = lwr, xend = fit_rank, yend = upr), data = predictions) +
  ylab("probability") + xlab("rank of estimated fit") + ggtitle("Fitted probabilities (95% profile CI) of observation landing in class identified by data")

  # Plot data classes versus predicted classes
#mydat_xs <- mydat[,c("watershed_med", "Year")]
#mydat_xs <- mydat[,c("Year", "watershed_med", "MEANT", "pctrun", "pctrock", "pctShade", "pctBrBnk")]
mydat_xs <- mydat[,c("Year", "watershed_med", "MEANT", "pctrun", "pctrock", "pctShade", "pctBrBnk", "HAiFLS_dev", "HAiFLS_for")]

ord_predict <- predict(ord_mod, newdata = mydat_xs)$fit
pred_df <- cbind(mydat[,c("newID", "Year", "watershed_med", "IBIScore", "Rating")], 
                 "PredictedClass" = predict(ord_mod, newdata = mydat_xs, type = "class")$fit,
                 round(ord_predict, 3))

convert_df1 <- data.frame(Rating = c("Very Poor", "Poor", "Fair", "Good", "Excellent"),
                         data_class = seq(1:5))
pred_df <- merge(pred_df, convert_df1)
convert_df2 <- data.frame(PredictedClass = c("Very Poor", "Poor", "Fair", "Good", "Excellent"),
                          pred_class = seq(1:5))
pred_df <- merge(pred_df, convert_df2)
pred_df$diff <- pred_df$data_class - pred_df$pred_class
pred_df[order(pred_df$IBIScore),c("newID", "Year", "watershed_med", "IBIScore", "Rating", "data_class", "PredictedClass", "pred_class", "Very Poor", "Poor", "Fair", "Good", "Excellent", "diff")]

ggplot(pred_df, aes(x = IBIScore, y = diff, color = Rating)) +
  geom_jitter(position=position_jitter(width=0.1, height=0.1)) +
  ylab("movement from data class") +
  scale_y_continuous(labels = c("up 1 class", "same class", "down 1 class", "down 2 classes")) +
  scale_color_discrete(name = "Class in data") +
  ggtitle("Predicted class from class identified in data") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "blue2")

rm(convert_df1, convert_df2)
