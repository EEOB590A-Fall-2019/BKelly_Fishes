#ggplot exercise Day 1
#EEOB590

#We will use the forest trajectory dataset to make some graphs. These are from 25m transects conducted across three islands within 4 different forest types. We measured a bunch of things along each transect, so the dataframe is relatively complex. Be sure to use the ggplot_tutorial.R script to help you with this exercise. 

#Load libraries
library(tidyverse)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(viridis)

#Load data
getwd()
foresttraj <- read.csv("Data/EEOB590/tidy/foresttrajectory_site_tidy.csv")
names(foresttraj)
fore <- foresttraj %>%
  select(island, site, forest.type, num.adult.spp)%>%
  rename(Ftype = forest.type, Richness = num.adult.spp, Island=island)

#1) Replicate the figure in the graphics folder called spprich.adult.pdf. 
ggplot(fore, aes(Ftype, Richness, fill=Island))+
  geom_boxplot()+
  theme_classic()+
  labs(x = "Forest Type", y = "Species richness - adult trees")+
  scale_x_discrete(labels=c("Leucaena thicket", "Mixed introduced forest",
                            "Native limestone forest", "Scrub-Shrub"))+
  scale_fill_brewer(palette = "Blues",
                    labels = c("Guam","Rota","Saipan"))
  
##------------------------------------------------------------------------------------------------------
#2) Now, make a figure based on model output from the model below. The final figure should look like the one called num.adult.trees.pdf. Be sure to use the code in the ggplot_tutoria file for this. 

m1 <- glm(num.adults ~ island, data=foresttraj, family=poisson)
summary(m1)

#####based on model output#####
#create dataframe over which to predict model results
preddata <- with(foresttraj, expand.grid(island = levels(island)))

#predict model results
preddata2 <- as.data.frame(predict(m1, newdata=preddata, type="link", se.fit=TRUE))
preddata2<-cbind(preddata, preddata2)

#calculate upper and lower CI's
preddata2 <- within(preddata2, {
  pred <- exp(fit)
  lwr <- exp(fit - (1.96 * se.fit))
  upr <- exp(fit + (1.96 * se.fit))
})

ggplot(preddata2, aes(island, pred))+
  geom_point()+
  geom_errorbar(aes(ymin=lwr, ymax=upr), width=0.2)+
  theme_classic()+
  labs(x = "Island", y = "Number of adult trees per transect")+
  scale_x_discrete(labels = c("Guam","Rota","Saipan"))

#3) Come up with a cool way to visualize the relationship between the number of adult species and the number of seedling species across the islands and forest types. 
# New facet label names for supp variable
levels(foresttraj$island) <- c("Guam", "Rota", "Saipan")

ggplot(foresttraj, aes(num.adult.spp, num.seedling.spp, fill=forest.type))+
  geom_point(shape = 21,
             alpha = 0.8,
             size = 3,
             stroke = 1)+
  facet_grid(rows = vars(island))+
  theme_bw()+
  labs(x = "Number of adult tree species", 
       y = "Number of seedling tree species")+
  theme(legend.position = "top")+
  scale_fill_brewer(palette = "Set1",
                    name="Forest Type",
                    labels = c("Leucaena Thicket","Mixed Introduced",
                               "Native Limestone","Scrub Shrub"))+
  theme(legend.title = element_text(face = "bold"))+
  theme(axis.title.x = element_text(size = "14"))+
  theme(axis.title.y = element_text(size = "14"))+
  theme(strip.text = element_text(face = "bold"))+
  theme(legend.text = element_text(size = "12"))


#4) Find a cool graphical approach from the websites below, then create a graph of that type using data from the foresttraj dataset 
# http://www.r-graph-gallery.com/ 
# http://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html 


###Cool graph choice: Violin Chart###
ggplot(foresttraj,aes(x=forest.type, y=num.seedling.spp, fill=island)) + 
  geom_violin(position="dodge", alpha=0.75)+
  scale_fill_brewer(palette = "Dark2", 
                    name = "Island")+
  theme_bw()+
  theme(legend.position = "top")+
  labs(x="Forest Type", y="Number of seedling species")+
  scale_x_discrete(labels=c("Leucaena thicket", "Mixed introduced forest",
                            "Native limestone forest", "Scrub-Shrub"))+
  theme(axis.text.x = element_text(size = "10"))+
  theme(axis.text.y = element_text(size = "11"))



