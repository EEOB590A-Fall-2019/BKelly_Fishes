##################################
## Making Maps with Stream Data ##
##################################

library(tidyverse)
library(sp)
library(maps)
library(raster)
library(rgdal)
library(mapproj)



#polygons - watershed boundaries
huc12 <- shapefile("Data/Thesis/Spatial/HUC12_project.shp")
UPI_wbd <- shapefile("Data/Thesis/Spatial/UPI_by_HUC12.shp")
YEL_wbd <- shapefile("Data/Thesis/Spatial/Yellow_by_HUC12.shp")
LMAQ_wbd <- shapefile("Data/Thesis/Spatial/LMAQ_by_HUC12.shp")

#polylines - streams
streams12 <- shapefile("Data/Thesis/Spatial/streams_by_HUC12.shp")
UPI_strm <- shapefile("Data/Thesis/Spatial/UPI_streams.shp")
YEL_strm <- shapefile("Data/Thesis/Spatial/YEL_streams.shp")
LMAQ_strm <- shapefile("Data/Thesis/Spatial/LMAQ_streams.shp")

#--------
#plotting
#--------

#----------------------------------------------------------------
##Instructions on making a DF out of your spatial data

class(huc12) #check to make sure it is a spatialpolygondataframe

huc12.df <- fortify(huc12) #make df of spatial kind

class(huc12.df) #make sure it is a DF

head(huc12.df) #inspect the data

#produce plot!
ggplot(data = huc12.df,
       aes(x = long, y = lat, group=group))+
  geom_path()
#----------------------------------------------------------------

streams12.df <- fortify(streams12)

#Let's make a map of our sampled streams for 2019
sites <- read_csv("Data/Thesis/Spatial/Fish_Temp_Locs19.csv", col_names = T)%>%
  filter(HUC_Site != "UPI_165")%>%
  rename(long = Easting, lat = Northing)


basemap <- ggplot(data = huc12.df,
       aes(x = long, y = lat, group=group))+
  geom_path()+
  theme_bw()

basemap + geom_path(data = streams12.df,
            aes(x = long, y = lat, group = group),
            color = "lightblue")+
  geom_point(data = sites, aes(x = long, y = lat, group = NULL, fill=HUC8),
             shape = 21, size = 4)+
  theme(legend.position = "bottom")+
  labs(x = "Easting (UTM)", y = "Northing (UTM)")






###############################################
###############################################
##lets try with the tidy::broom funtion instead
###############################################
###############################################

library(broom)

#polygons - watershed boundaries
huc12v2 <- readOGR("Data/Thesis/Spatial/HUC12_project.shp")
UPI_wbdv2 <- readOGR("Data/Thesis/Spatial/UPI_by_HUC12.shp")
YEL_wbdv2 <- readOGR("Data/Thesis/Spatial/Yellow_by_HUC12.shp")
LMAQ_wbdv2 <- readOGR("Data/Thesis/Spatial/LMAQ_by_HUC12.shp")

#polylines - streams
streams12v2 <- readOGR("Data/Thesis/Spatial/streams_by_HUC12.shp")
UPI_strmv2 <- readOGR("Data/Thesis/Spatial/UPI_streams.shp")
YEL_strmv2 <- readOGR("Data/Thesis/Spatial/YEL_streams.shp")
LMAQ_strmv2 <- readOGR("Data/Thesis/Spatial/LMAQ_streams.shp")

#Read in sites
sites <- read_csv("Data/Thesis/Spatial/Fish_Temp_Locs19.csv", col_names = T)%>%
  filter(HUC_Site != "UPI_165")%>%
  rename(long = Easting, lat = Northing)

#convert spatial polygons data frame into tible
head(huc12v2)
huc12_broom <- tidy(huc12v2, region = NULL)
strm12_broom <- tidy(streams12v2, region = NULL)

# Plot it
base <- ggplot() +
  geom_polygon(data = huc12_broom, aes(x = long, y = lat, group = group),
               fill="gray60", color="black") +
  theme_bw()
base

base + geom_path(data = strm12_broom,
                           aes(x = long, y = lat, group = group),
                           color = "lightblue")+
  geom_point(data = sites, aes(x = long, y = lat, group = NULL, fill=HUC8),
             shape = 21, size = 4)+
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette = "PiYG", 
                    name="2019 Sampling Locations",
                    labels = c("Little Maquoketa (HUC8)",
                               "Upper Iowa (HUC8)",
                               "Yellow (HUC8)"))+
  labs(x = "Easting (UTM)", y = "Northing (UTM)")+
  theme(legend.justification = c(0,0), legend.position = c(0,0),
        legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid"),
        legend.title = element_text(size = "12", face = "bold"),
        legend.text = element_text(size = "12"))+
  theme(axis.title.x = element_text(size = "14"),
        axis.title.y = element_text(size = "14"))

ggsave("StudyAreaMap.png", 
       width = 12, height = 10, units = "in", dpi = 350)


##---------------
##Individual Summary Maps by HUC8
##---------------

##----
#Upper Iowa
##----
#UPI_wbdv2 <- readOGR("Data/Thesis/Spatial/UPI_by_HUC12.shp")
#UPI_strmv2 <- readOGR("Data/Thesis/Spatial/UPI_streams.shp")

#convert to tible
UPI_wbd_broom <- tidy(UPI_wbdv2, region = NULL)
UPI_streams_broom <- tidy(UPI_strmv2, region = NULL)

#filter sites to just those in the UPI
upi.sites <- sites %>%
  filter(HUC8 == "UPI")

#make basemap with Upper Iowa's HUC12 boundaries
base.upi <- ggplot() +
  geom_polygon(data = UPI_wbd_broom, aes(x = long, y = lat, group = group),
               fill="gray60", color="black") +
  theme_bw()
base.upi

bkt.upi <- base.upi + geom_path(data = UPI_streams_broom,
                 aes(x = long, y = lat, group = group),
                 color = "lightblue")+
  geom_point(data = upi.sites, aes(x = long, y = lat, group = NULL, fill = factor(BKT)),
             shape = 21, size = 4)+
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette = "Set1", 
                    name="Brook Trout Status",
                    labels = c("Absent", "Present"))+
  labs(x = "Easting (UTM)", y = "Northing (UTM)")+
  theme(legend.justification = c(0,0), legend.position = c(0,0),
        legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid"),
        legend.title = element_text(size = "12", face = "bold"),
        legend.text = element_text(size = "12"))+
  theme(axis.title.x = element_text(size = "14"),
        axis.title.y = element_text(size = "14"))


##voodoo magic to make a "Trout Status" variable for the fill argument
upi.sites.v2 <- upi.sites %>%
  mutate(Trout_Status = ifelse(BKT+BRT>1,3,ifelse(BKT-BRT>0,2,ifelse(BKT-BRT<0,1,0))))
######################################################################


bkt.brt.upi <- base.upi + geom_path(data = UPI_streams_broom,
                                aes(x = long, y = lat, group = group),
                                color = "lightblue")+
  geom_point(data = upi.sites.v2, aes(x = long, y = lat, group = NULL, fill = factor(Trout_Status)),
             shape = 21, size = 4)+
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette = "Set1", 
                    name="Trout Status",
                    labels = c("Neither Species Present", "Brown Trout Only",
                               "Brook Trout Only", "Brook & Brown Trout"))+
  labs(x = "Easting (UTM)", y = "Northing (UTM)", title = "Upper Iowa River Watershed")+
  theme(legend.justification = c(0,0), legend.position = c(0,0),
        legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid"),
        legend.title = element_text(size = "12", face = "bold"),
        legend.text = element_text(size = "12"))+
  theme(axis.title.x = element_text(size = "14"),
        axis.title.y = element_text(size = "14"),
        plot.title = element_text(size = "14"))
bkt.brt.upi

ggsave("UpperIowaMap.png", 
       width = 12, height = 6, units = "in", dpi = 350)
######################################################################




##----
#Yellow River
##----

#convert to tible
YEL_wbd_broom <- tidy(YEL_wbdv2, region = NULL)
YEL_streams_broom <- tidy(YEL_strmv2, region = NULL)

#filter sites to just those in the UPI
yel.sites <- sites %>%
  filter(HUC8 == "YEL")

#make basemap with Upper Iowa's HUC12 boundaries
base.yel <- ggplot() +
  geom_polygon(data = YEL_wbd_broom, aes(x = long, y = lat, group = group),
               fill="gray60", color="black") +
  theme_bw()

##voodoo magic to make a "Trout Status" variable for the fill argument
yel.sites.v2 <- yel.sites %>%
  mutate(Trout_Status = ifelse(BKT+BRT>1,3,ifelse(BKT-BRT>0,2,ifelse(BKT-BRT<0,1,0))))
######################################################################

bkt.brt.yel <- base.yel + geom_path(data = YEL_streams_broom,
                                    aes(x = long, y = lat, group = group),
                                    color = "lightblue")+
  geom_point(data = yel.sites.v2, aes(x = long, y = lat, group = NULL, fill = factor(Trout_Status)),
             shape = 21, size = 4)+
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette = "Set1", 
                    name="Trout Status",
                    labels = c("Neither Species Present", "Brown Trout Only",
                               "Brook Trout Only", "Brook & Brown Trout"))+
  labs(x = "Easting (UTM)", y = "Northing (UTM)", title = "Yellow River Watershed")+
  theme(legend.justification = c(0,0), legend.position = c(0,0),
        legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid"),
        legend.title = element_text(size = "12", face = "bold"),
        legend.text = element_text(size = "12"))+
  theme(axis.title.x = element_text(size = "14"),
        axis.title.y = element_text(size = "14"),
        plot.title = element_text(size = "14"))
#bkt.brt.yel

ggsave("YellowMap.png", 
       width = 10, height = 8, units = "in", dpi = 350)
######################################################################




##----
#Little Maq
##----

#convert to tible
LMAQ_wbd_broom <- tidy(LMAQ_wbdv2, region = NULL)
LMAQ_streams_broom <- tidy(LMAQ_strmv2, region = NULL)

#filter sites to just those in the UPI
lmaq.sites <- sites %>%
  filter(HUC8 == "LMAQ")

#make basemap with Upper Iowa's HUC12 boundaries
base.lmaq <- ggplot() +
  geom_polygon(data = LMAQ_wbd_broom, aes(x = long, y = lat, group = group),
               fill="gray60", color="black") +
  theme_bw()
base.lmaq

##voodoo magic to make a "Trout Status" variable for the fill argument
lmaq.sites.v2 <- lmaq.sites %>%
  mutate(Trout_Status = ifelse(BKT+BRT>1,3,ifelse(BKT-BRT>0,2,ifelse(BKT-BRT<0,1,0))))
######################################################################

bkt.brt.lmaq <- base.lmaq + geom_path(data = LMAQ_streams_broom,
                                    aes(x = long, y = lat, group = group),
                                    color = "lightblue")+
  geom_point(data = lmaq.sites.v2, aes(x = long, y = lat, group = NULL, fill = factor(Trout_Status)),
             shape = 21, size = 4)+
  theme(legend.position = "bottom")+
  scale_fill_brewer(palette = "Set1", 
                    name="Trout Status",
                    labels = c("Neither Species Present", "Brown Trout Only",
                               "Brook Trout Only", "Brook & Brown Trout"))+
  labs(x = "Easting (UTM)", y = "Northing (UTM)", title = "Little Maquoketa River Watershed")+
  theme(legend.justification = c(0,0), legend.position = c(0,0),
        legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "solid"),
        legend.title = element_text(size = "12", face = "bold"),
        legend.text = element_text(size = "12"))+
  theme(axis.title.x = element_text(size = "14"),
        axis.title.y = element_text(size = "14"),
        plot.title = element_text(size = "14"))
#bkt.brt.lmaq

ggsave("LittleMaqMap.png", 
       width = 8, height = 9, units = "in", dpi = 350)
######################################################################









