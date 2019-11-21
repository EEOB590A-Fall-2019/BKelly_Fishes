##################################
## Making Maps with Stream Data ##
##################################

library(tidyverse)
library(sp)
library(maps)
library(raster)
library(rgdal)
#install.packages("rgdal")


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






