################################################################################################
################################################################################################

# Data Wrangling: GIS derived stream and catchment attributes

#Description:
  # Data were collected using IDW, Hydrology, and Sinuosity and Gradient Toolsets in ArcMap10.6
  # Data needs to be inspected for weird values, missing values, make common column structure, 
  #   and joined into one wide-format data set where rows are sites and columns are attributes

###############################################################################################
###############################################################################################

library(tidyverse)
library(forcats)
library(skimr)
library(ggplot2)


#First let's work with the overall endpoint for the data: "StreamAttributes.csv"
getwd()
stvars <- read_csv("Data/Thesis/Raw/All Years/StreamAttributes.csv", col_names = T)

#inspect for missing values, extremes, unexpected 0's, etc. 
skim(stvars)

#change column names to logical labels with "_UnitsOfMeasure" -- ex. "Elev_m" = Elevation at sampling point in Meters
names(stvars)
stvars <- stvars %>%
  select(HUC8, Site, Xcoord, Ycoord, YrSmpld, grid_code, Shape_Leng, RASTERVALU, Sinuosity, Percent, Degrees, POLY_AREA) %>%
  rename(Easting = Xcoord, Northing = Ycoord, Order = grid_code, SegLen = Shape_Leng, elev_m = RASTERVALU, pctSlope = Percent,
         dgreSlope = Degrees, CatArea_km2 = POLY_AREA)

####################################################################
#Now lets join all of our IDW together, and then to the above data ^
####################################################################

#read data
UPIfls <- read_csv("Data/Thesis/Raw/All Years/UPI_IDW.csv", col_names = T)
UPI202fls <- read_csv("Data/Thesis/Raw/All Years/UPI202_IDW.csv", col_names = T)
YELfls <- read_csv("Data/Thesis/Raw/All Years/YEL_IDW.csv", col_names = T)
LMAQfls <- read_csv("Data/Thesis/Raw/All Years/LMAQ_IDW.csv", col_names = T)

#Issues:
# - When running HAiFLS tool in IDW Plus toolbox, had to break sites in YEL into different runs for processing feasibility
# - Site UPI_202 had a pour point in the wrong direction, so it was rerun, and put into another csv

#Make column structure the same (number and names equal among df's)
#remove OBJECTID's of all types

#UPI
names(UPIfls)
UPIfls <- UPIfls %>%
  select(-OBJECTID)%>%
  rename(OutletX = POINT_X, OutletY = POINT_Y)

#UPI202
names(UPI202fls)
UPI202 <- UPI202fls %>%
  select(-OBJECTID_1, -OBJECTID)%>%
  rename(OutletX = POINT_X, OutletY = POINT_Y)

#replace UPI_202 in larger dataset with new observation
UPIfls[1,] <- UPI202

#Yellow 
names(YELfls)
str(YELfls)
yel <- YELfls %>%
  select(-OBJECTID)%>%
  rename(OutletX = POINT_X, OutletY = POINT_Y, HAiFLS_alt1 = HAiFLS_alt)

#combine the data from complementary columns for HAiFLS vars
#alt
alt_test <- yel$HAiFLS_alt1[is.na(yel$HAiFLS_alt1)]
yel$HAiFLS_alt1[is.na(yel$HAiFLS_alt1)] <- yel$HAiFLS_alt2[is.na(yel$HAiFLS_alt1)]
#bmp
bmp_test <- yel$HAiFLS_bmp1[is.na(yel$HAiFLS_bmp1)]
yel$HAiFLS_bmp1[is.na(yel$HAiFLS_bmp1)] <- yel$HAiFLS_bmp2[is.na(yel$HAiFLS_bmp1)]
#ag
ag_test <- yel$HAiFLS_ag1[is.na(yel$HAiFLS_ag1)]
yel$HAiFLS_ag1[is.na(yel$HAiFLS_ag1)] <- yel$HAiFLS_ag2[is.na(yel$HAiFLS_ag1)]
#dev
d_test <- yel$HAiFLS_dev1[is.na(yel$HAiFLS_dev1)]
yel$HAiFLS_dev1[is.na(yel$HAiFLS_dev1)] <- yel$HAiFLS_dev2[is.na(yel$HAiFLS_dev1)]
#for
for_test <- yel$HAiFLS_for1[is.na(yel$HAiFLS_for1)]
yel$HAiFLS_for1[is.na(yel$HAiFLS_for1)] <- yel$HAiFLS_for2[is.na(yel$HAiFLS_for1)]
#pas
p_test <- yel$HAiFLS_pas1[is.na(yel$HAiFLS_pas1)]
yel$HAiFLS_pas1[is.na(yel$HAiFLS_pas1)] <- yel$HAiFLS_pas2[is.na(yel$HAiFLS_pas1)]

#remove unneeded cols and rename to be common
names(yel)
yel <- yel %>%
  select(-HAiFLS_alt2, -HAiFLS_bmp2, -HAiFLS_ag2, -HAiFLS_dev2, -HAiFLS_for2, -HAiFLS_pas2)%>%
  rename(HAiFLS_alt = HAiFLS_alt1, HAiFLS_bmp = HAiFLS_bmp1, HAiFLS_ag = HAiFLS_ag1, HAiFLS_dev = HAiFLS_dev1, HAiFLS_for = HAiFLS_for1, HAiFLS_pas = HAiFLS_pas1)

#LMAQ
names(LMAQfls)
lmaq <- LMAQfls %>%
  select(-OBJECTID)%>%
  rename(OutletX = POINT_X, OutletY = POINT_Y, HAiFLS_ag = HAiFLS_Ag)

IDW1 <- full_join(UPIfls, yel, by=NULL)
IDW <- full_join(IDW1, lmaq, by=NULL)

##Ghost levels
#Brook trout are ghosts #browntroutgang #unlockedcomputer

