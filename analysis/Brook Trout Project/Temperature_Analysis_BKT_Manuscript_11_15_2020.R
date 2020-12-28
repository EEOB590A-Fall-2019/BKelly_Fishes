## Temperature Analysis for Occupancy Manuscript

#libraries
library(skimr)
library(tidyverse)
library(readxl)
library(lubridate)

#load data
temp.data.2018 <- read.csv("Data/Thesis/Tidy/TemperatureData_2018Sites.csv", header=T)
str(temp.data.2018)
temp.data.2018$SN18 <- as.numeric(temp.data.2018$SN18)
miss.temps <- temp.data.2018 %>%
  filter(is.na(SN18))

#filter data by Serial Numbers that have June and July Data
tdata.2018 <- temp.data.2018 %>%
  filter(SN18 == 9345 | SN18 == 4485 | SN18 == 4605 | SN18 == 4608 | SN18 == 20351163 |
           SN18==4599|SN18==4589|SN18==3221|SN18==4612|SN18==4606|SN18==4607|SN18==3219|
           SN18==4609|SN18==4586|SN18==4600|SN18==4604|SN18==4611|SN18==3211|SN18==4587|
           SN18==3218|SN18==3208|SN18==4594|SN18==4597|SN18==3188|SN18==4603|SN18==3212|
           SN18==3214|SN18==3213|SN18==3223)

#needed.sites <- c(9345x, 4485x, 4605x, 4608x, 20351163x, 4599x, 4589x, 3221x, 4612x, 
 #                 4606x, 4607x, 3219x, 4609x, 4586x, 4600x, 4604x, 4611x, 3211x, 
  #                4587x, 3218x, 3208x, 4594x, 4597x, 3188x, 4603x, 3212x, 3214x, 
   #               3213, 3223)

#make sure we have 29 sites
n_unique(tdata.2018$SN18) #filter worked!


#trim data to date range of interest: 6/1/2018 to 9/22/18
tdat.2018 <- tdata.2018 %>%
  separate(Date_Time, into = c("DOY", "TOD"), sep = " ") %>%
  select(-TOD) %>%
  mutate(LubDate = mdy(DOY)) %>%
  mutate(Month = month(LubDate)) %>%
  mutate(Day = day(LubDate)) %>%
  mutate(Yr = year(LubDate)) %>%
  filter(Month==6|Month==7|Month==8|Month==9) %>%
  filter(LubDate<"2018-09-23") %>%
  filter(Temp_F<86)

plot(tdat.2018$Temp_F)
summary(tdat.2018$Temp_F)


##############
#ADD MORE DATA 
##############
logger.list.2018 <- read.csv("Data/Thesis/Tidy/Loggers_2018_for_manuscript.csv", header=T) %>%
  mutate(Install_Date = mdy(DateOY)) %>%
  unite("uid", c("HUC8", "Site"), sep = "_", remove = T)
##########################################################

#combine new data to big dataset by SN18
newdat.2018 <- left_join(tdat.2018, logger.list.2018, by="SN18")

#reduce
names(newdat.2018)
newdat2k18 <- newdat.2018 %>%
  select(uid, SN18, Install_Date, LubDate, Month, Day, Yr, Temp_F)

#trial
x <- newdat2k18 %>%
  group_by(SN18) %>%
  filter(LubDate>Install_Date) %>%
  mutate(Temp_C = (Temp_F-32)/(9/5))



##-----
### SUMMARIES - 2018 ###
##-----

#monthly averages
temp.AVG.2018 <- x %>%
  group_by(uid, SN18, Month) %>%
  summarise(avg = mean(Temp_C))

#wide format dataset for linear regression
y <- temp.AVG.2018 %>%
  spread(Month, avg) %>%
  rename(June_avg="6",July_avg="7",Aug_avg="8",Sept_avg="9") %>%
  mutate(June_July = (June_avg+July_avg)/(2)) %>%
  mutate(Yr="2018")


## For table
#monthly max
x2 <- x %>%
  group_by(Month, uid) %>%
  summarise(Mini = min(Temp_C),
            Maxi = max(Temp_C),
            ) %>%
  group_by(Month) %>%
  summarise(Mini_mean = mean(Mini),
            Maxi_mean = mean(Maxi))

#-----
temp.summary.2018 <- x %>%
  group_by(Month) %>%
  summarise(AvgT = mean(Temp_C),
            sdT = sd(Temp_C)) %>%
  mutate(Yr = "2018") %>%
  mutate(MOY = ifelse(Month==6,"June",ifelse(Month==7,"July",ifelse(Month==8,"August",ifelse(Month==9,"September","NA"))))) %>%
  select(Yr,MOY,Month,AvgT,sdT)




################################################################################
################################################################################
################################################################################
################################################################################
##                                2019
################################################################################
################################################################################



#load data
temp.data.2019 <- read.csv("Data/Thesis/Tidy/TemperatureData_2019Sites.csv", header=T)
str(temp.data.2019)
skim(temp.data.2019)

#filter data by Serial Numbers that have June and July Data
tdata.2019 <- temp.data.2019 %>%
  filter(SN19 == 8158 | SN19 == 9680 | SN19 == 8106 | SN19 == 4642 | SN19 == 4639 |
           SN19 == 4592 | SN19 == 4613 | SN19 == 4635 | SN19 == 4602 | SN19 == 3186 |
           SN19 == 4627 | SN19 == 4600 | SN19 == 4633 | SN19 == 4607 | SN19 == 4624 |
           SN19 == 3220)

#make sure we have 16 sites
n_unique(tdata.2019$SN19) #filter worked!


#trim data to date range of interest: 6/1/2019 to 9/11/19
tdat.2019 <- tdata.2019 %>%
  separate(Date_Time, into = c("DOY", "TOD"), sep = " ") %>%
  select(-TOD) %>%
  mutate(LubDate = mdy(DOY)) %>%
  mutate(Month = month(LubDate)) %>%
  mutate(Day = day(LubDate)) %>%
  mutate(Yr = year(LubDate)) %>%
  filter(Month==6|Month==7|Month==8|Month==9) %>%
  filter(LubDate<"2019-09-12") %>%
  filter(Temp_F<86)

plot(tdat.2019$Temp_F) ## Some temps already in Degrees C!!

#find the sites in (C)
dc <- tdat.2019 %>%
  filter(Temp_F<30) #SN19 = 8158

#convert 8158 back to degrees F
DegC <- tdat.2019 %>%
  filter(SN19==8158) %>%
  mutate(Temp_F = Temp_F*(9/5)+32)

test <- tdat.2019[34521:35804,]

tdat.2019[34521:35804,] <- DegC
plot(tdat.2019$Temp_F) ## Everything in F, but that site is still weird!

plot(DegC$LubDate,DegC$Temp_F) #remove SN19 = 8158

tdat.2019 <- tdat.2019 %>%
  filter(SN19 != 8158) #good to go!


summary(tdat.2019$Temp_F) #looks normal!


##############
#ADD MORE DATA 
##############
logger.list.2019 <- read.csv("Data/Thesis/Tidy/Loggers_2019_for_manuscript.csv", header=T) %>%
  mutate(Install_Date = mdy(DateOY)) %>%
  unite("uid", c("HUC8", "Site"), sep = "_", remove = T)
##########################################################

#combine new data to big dataset by SN18
newdat.2019 <- left_join(tdat.2019, logger.list.2019, by="SN19")

#reduce
names(newdat.2019)
newdat2k19 <- newdat.2019 %>%
  select(uid, SN19, Install_Date, LubDate, Month, Day, Yr, Temp_F)

#trial
xx <- newdat2k19 %>%
  group_by(SN19) %>%
  filter(LubDate>Install_Date) %>%
  mutate(Temp_C = (Temp_F-32)/(9/5))



##-----
### SUMMARIES - 2019 ###
##-----

#monthly averages
temp.AVG.2019 <- xx %>%
  group_by(uid, SN19, Month) %>%
  summarise(avg = mean(Temp_C))

#wide format dataset for linear regression
yy <- temp.AVG.2019 %>%
  spread(Month, avg) %>%
  rename(June_avg="6",July_avg="7",Aug_avg="8",Sept_avg="9") %>%
  mutate(June_July = (June_avg+July_avg)/(2)) %>%
  mutate(Yr="2019")


## For table

#monthly max
xx2 <- xx %>%
  group_by(Month, uid) %>%
  summarise(Mini = min(Temp_C),
            Maxi = max(Temp_C),
  ) %>%
  group_by(Month) %>%
  summarise(Mini_mean = mean(Mini),
            Maxi_mean = mean(Maxi))

#monthly max
temp.summary.2019 <- xx %>%
  group_by(Month) %>%
  summarise(MINT = min(Temp_C),
            MAXT = max(Temp_C),
            AvgT = mean(Temp_C),
            sdT = sd(Temp_C)) %>%
  mutate(Yr = "2019") %>%
  mutate(MOY = ifelse(Month==6,"June",ifelse(Month==7,"July",ifelse(Month==8,"August",ifelse(Month==9,"September","NA"))))) %>%
  select(Yr,MOY,Month,MINT,MAXT,AvgT,sdT)



##-----
### Temperature Table - Final ###
##-----

t.table <- full_join(temp.summary.2018, temp.summary.2019)

write.csv(t.table, "Data/Thesis/Tidy/Temperature_Table_for_manuscript_11_15_2020.csv", row.names = F)



##-----
### Regression Analysis ###
##-----

early <- full_join(y,yy)
names(early)

early2 <- early %>%
  select(uid, June_July) %>%
  select(-SN18)

# join the covariate used in analysis!
covariate <- read.csv("Data/Thesis/Tidy/BKT_Occu_File.csv", header = T)
skim(covariate)

late <- covariate %>%
  select(newID, avgT) %>%
  rename(uid=newID)

summer <- left_join(early2, late, by="uid") %>%
  filter(uid != "UPI_29")

cor(summer$June_July, summer$avgT)

#formal linear model:
temp.lm <- lm(avgT~June_July, data=summer)

summary(temp.lm)

#reverse to see if values are the same
temp.lm.reverse <- lm(June_July~avgT, data=summer)

summary(temp.lm.reverse)


##-----
#Make plot for response letter
##-----
## 1-1 line PLUS stat smooth

ggplot(summer, aes(x=June_July, y=avgT)) + 
  geom_point(
    color="black",
    fill="grey70",
    shape=21,
    alpha=0.75,
    size=4,
    stroke = 1.5
  )+
  geom_abline(intercept = 0, slope = 1, color="black", linetype="dashed",
              size=1)+
  stat_smooth(method = "lm", se=F, color="blue", size=1.5)+
  theme_bw()+
  theme(legend.position = "bottom")+
  labs(y="August-September Mean Stream Temperature (C)", 
       x="June-July Mean Stream Temperature (C)")+
  theme(axis.title = element_text(size = 14, face = "bold"))+
  theme(axis.text = element_text(size = 12))+
  theme(panel.grid = element_blank())+
  annotate("text",x=13,y=20,label="R^2 Value = 0.75",
           fontface="bold", size=5)

getwd()
ggsave("Summer_Temp_Comp_11_15_2020.png", dpi = 350)
