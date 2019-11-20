###########################################
### Stream Temperature Data Exploration ###
###########################################
library(extrafont)
library(tidyverse)
library(skimr)
#install.packages("ggridges")
#install.packages("viridis")
#install.packages("hrbrthemes")
library(ggridges)
library(viridis)
library(hrbrthemes)
library(lubridate)
library(cowplot)
#hrbrthemes::import_roboto_condensed()
#hrbrthemes::font_an
################################

##load temp data##

#2019 logger list
logger.info <- read_csv("Data/Thesis/Tidy/tidy_temps19.csv", col_names = T) 
#2019 summarized variables
tv19 <- read_csv("Data/Thesis/Tidy/TempVars_2019_tidy.csv", col_names = T) 
#raw temperatures for all loggers
temp19 <- read_csv("Data/Thesis/Tidy/TempData2019_DateTimeCelcius.csv", col_names = T,
                   col_types = list(col_character(),col_character(),col_factor(),col_datetime(),
                                    col_time(),col_date(),col_double(),col_double())) 
temp18 <- read_csv("Data/Thesis/Tidy/TempData18_DateTimeCelcius.csv", col_names = T,
                   col_types = list(col_time(), col_double(), col_character(),
                                    col_character(), col_character(), col_factor(),
                                    col_character(),col_double()))


##make date and datetime class columns and arrage to match 2019 data
names(temp18)
temp18$Date <- mdy(temp18$Date)
class(temp18$Date)
temp18$DateTime <- mdy_hms(temp18$DateTime)
class(temp18$DateTime)
temp18 <- temp18[,c(4,5,6,7,1,3,2,8)]
#################################################################################################

##simple ridgeline plot
ggplot()+
  geom_density_ridges(data = tv19,
                      aes(x = avgT, y = HUC8, fill = HUC8), alpha = 0.75)+
  theme_ridges()+
  theme(legend.position = "bottom")+
  labs(x = "Mean August/September Temperature (Celcius)",
       y = "Watershed (HUC8)")+
  theme(axis.text.y = element_blank())+
  scale_fill_brewer(name = "Watershed",
                    labels = c("Grant-Little Maquoketa","Upper Iowa","Yellow"),
                    palette = "Dark2")


##Let's try violin plots instead
ggplot()+
  geom_violin(data = tv19,
              aes(x = HUC8, y = avgT, fill = HUC8),
              alpha = 0.5)+
  scale_fill_viridis(discrete = T, name = "Watershed",
                     labels = c("Grant-Little Maquoketa","Upper Iowa","Yellow"))+
  theme_ipsum()+
  labs(x = "", y = "Mean Aug/Sept Stream Temperature (Celcius)")+
  theme(legend.position = "top")+
  scale_x_discrete(labels = c("Grant-Little Maquoketa","Upper Iowa","Yellow"))+
  theme(axis.title.y = element_text(size = "12"))

ggplot()+
  geom_violin(data = tv19,
              aes(x = HUC8, y = MAXT, fill = HUC8),
              alpha = 0.5)+
  scale_fill_viridis(discrete = T, name = "Watershed",
                     labels = c("Grant-Little Maquoketa","Upper Iowa","Yellow"))+
  theme_ipsum()+
  labs(x = "", y = "Max Aug/Sept Stream Temperature (Celcius)")+
  theme(legend.position = "top")+
  scale_x_discrete(labels = c("Grant-Little Maquoketa","Upper Iowa","Yellow"))+
  theme(axis.title.y = element_text(size = "12"))

ggplot()+
  geom_violin(data = tv19,
              aes(x = HUC8, y = RNGT, fill = HUC8),
              alpha = 0.5)+
  scale_fill_viridis(discrete = T, name = "Watershed",
                     labels = c("Grant-Little Maquoketa","Upper Iowa","Yellow"))+
  theme_ipsum()+
  labs(x = "", y = "Maximum Daily Aug/Sept Stream Temperature Range (Celcius)")+
  theme(legend.position = "top")+
  scale_x_discrete(labels = c("Grant-Little Maquoketa","Upper Iowa","Yellow"))+
  theme(axis.title.y = element_text(size = "12"))

##try ridgeplot across time, faceted in rows by watershed

temp19.months <- temp19 %>%
  mutate(month_x = month(Date), day_x = day(Date))%>%
  filter(Date > "2019-6-20" & Date < "2019-9-23")
tmp19.summ <- temp19.months %>%
  group_by(HUC8, month_x)%>%
  summarise(meanT_C = mean(Temp_C), meanT_F = mean(Temp_F))%>%
  mutate(month_name = ifelse(month_x == 6,"June",ifelse(month_x == 7,"July", ifelse(month_x == 8, "August",
                                                                                    ifelse(month_x == 9, "September","NA")))))%>%
  mutate(Year_logger = 2019)

class(tmp19.summ$HUC8)
tmp19.summ$HUC8 <- as.factor(tmp19.summ$HUC8)


ggplot(data = tmp19.summ, aes(x = meanT_C, y = month_name, fill = ..x..))+
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01, alpha = 0.5)+
  scale_fill_viridis(name = "Temp [C]", option = "D")+
  theme_ipsum()
#+ facet_wrap(~ HUC8, ncol = 1) ***not working!***

##try geom_density instead
ggplot()+
  geom_density_ridges(data = tv19,
                      aes(x = avgT, y = HUC8, fill = HUC8), alpha = 0.5)+
  theme_ridges()+
  theme(legend.position = "bottom")+
  labs(x = "Mean August/September Temperature (Celcius)",
       y = "Watershed (HUC8)")+
  theme(axis.text.y = element_blank())+
  scale_fill_brewer(name = "Watershed",
                    labels = c("Grant-Little Maquoketa","Upper Iowa","Yellow"),
                    palette = "Dark2")+
  facet_grid(HUC8 ~ .)



tmp19.summ <- tmp19.summ %>%
  unite(HUC_month, c(HUC8,month_name), sep = "_", remove = F)

font_import(paths = NULL, recursive = TRUE, prompt = TRUE,
            pattern = NULL)



##################################
##### THE HOLY RIDGE -- 2019 #####
##################################
flabels <- c(LMAQ = "Grant-Little Maquoketa",
             UPI = "Upper Iowa",
             YEL = "Yellow")

plot2019 <-ggplot(data = temp19.months, aes(x = Temp_C, y = factor(month_x), fill = ..x..))+
  geom_density_ridges_gradient(scale = 0.9, rel_min_height = 0.01)+
  facet_grid(HUC8 ~ ., labeller = labeller(HUC8 = flabels))+
  theme_ipsum()+
  scale_fill_viridis(name = "Temp [C]",option = "D", begin = .4, end = 1)+
  scale_y_discrete(limits = c("9","8","7","6"),
                   labels = c("September", "August", "July", "June"))+
  xlim(10,30)+
  labs(x="Summer Stream Temperature (Celcius)", y="Month", title = '2019')+
  theme(legend.position = "none",
        axis.title.x = element_text(size = "14"),
        axis.title.y = element_text(size = "14"))
ggsave("plot2019.png",width = 10, height = 8, units = "in", dpi = 350)


##################################
##### THE HOLY RIDGE -- 2019 #####
##################################

temp18.months <- temp18 %>%
  mutate(month_x = month(Date), day_x = day(Date))%>%
  filter(Date > "2018-6-20" & Date < "2018-9-23")

plot2018 <-ggplot(data = temp18.months, aes(x = Temp_C, y = factor(month_x), fill = ..x..))+
  geom_density_ridges_gradient(scale = 0.9, rel_min_height = 0.01)+
  facet_grid(HUC8 ~ ., labeller = labeller(HUC8 = flabels))+
  theme_ipsum()+
  scale_fill_viridis(name = "Temp [C]",option = "D", begin = .4, end = 1)+
  scale_y_discrete(limits = c("9","8","7","6"),
                   labels = c("September", "August", "July", "June"))+
  xlim(10,30)+
  labs(x="Summer Stream Temperature (Celcius)", y="Month", title = 2018)+
  theme(legend.position = "none",
        axis.title.x = element_text(size = "14"),
        axis.title.y = element_text(size = "14"))
ggsave("plot2018.png",width = 10, height = 8, units = "in", dpi = 350)
#plot2018



####################
####################
## The Grand Finale 
####################
####################

TemperatureRidges <- plot_grid(plot2018,plot2019,
          labels = "auto")

ggsave("TemperatureRidges.png", 
       width = 10, height = 8, units = "in", dpi = 350)



