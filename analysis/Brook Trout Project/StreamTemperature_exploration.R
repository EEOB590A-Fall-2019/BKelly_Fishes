###########################################
### Stream Temperature Data Exploration ###
###########################################

library(tidyverse)
library(skimr)
#install.packages("ggridges")
#install.packages("viridis")
#install.packages("hrbrthemes")
library(ggridges)
library(viridis)
library(hrbrthemes)
library(lubridate)

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

ggplot(data = tmp19.summ, aes(x = meanT_C, y = month_name, fill = ..x..)) +  
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp [C]", option = "C")+
  theme_ipsum()#+
  #facet_grid(rows = vars(HUC8), scales = "free_x", space = "fixed")
##try cowplot instead??


