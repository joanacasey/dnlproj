library(tidyverse) 
library(sf)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(htmlwidgets)

setwd("~/Downloads/simple_tracts100/ct2015_epsg5070/ct2015_epsg5070_20_35")

#ct<- st_read("ct2015_epsg5070.shp")  # THIS IS THE MAPSHAPER OUTPUT FILE
                                    # NEED TO RENAME TO AVOID CONFUSION:
#st_write(ct,"ct2015_epsg5070_20_35.geojson")
ct<- st_read("ct2015_epsg5070_20_35.geojson")
colnames(ct)
head(ct)
keepcols<-c("GISJOIN","STATEFP", "dnlmean","geometry")

ct2<- ct[keepcols]

summary(ct2$dnlmean)

#drop nas
ct3<- subset(ct2,!is.na(dnlmean))

# All cats & vals
# Categorical variable
ct3$cat <- "low"
table(ct3$cat)
ct3$cat <- ifelse(ct3$dnlmean >= 55, "med",  ct3$cat)
table(ct3$cat)
ct3$cat <- ifelse(ct3$dnlmean >= 58, "high", ct3$cat)
table(ct3$cat)

#Faux numeric var for testing
ct3$catnum <- 1
ct3$catnum <- ifelse(ct3$cat == "med", 2, 1)
ct3$catnum  <- ifelse(ct3$cat =="high", 3, ct3$catnum)
table(ct3$cat)
table(ct3$catnum)

head(ct3)
# rename our dnlmean col so noone knows what it ise
colnames(ct3) <-c("GISJOIN","STATEFP","mymean","geometry","cat","catnum")

# Order the data from lowest to highest so that the high values
# will display on the map on top of the lower vals
ct3 <- ct3[order(ct3$mymean, decreasing = FALSE),]

# Create a centroid version of the dataset
ct3_pts <- st_centroid(ct3)

# Transform data to WGS84 (4326)
ct_all_polys_4326 <- st_transform(ct3, 4326)
ct_all_pts_4326 <- st_transform(ct3_pts, 4326)

# Create a subset that only has med & high values
ct_medhigh_polys_4326 <- subset(ct_all_polys_4326, cat!= "low")
ct_medhigh_pts_4326 <- subset(ct_all_pts_4326 , cat!= "low")

#
# Write four data objects to files
## shps and geojson
#
st_write(ct_all_polys_4326, "ct_all_polys_4326.shp", delete_dsn=TRUE)
st_write(ct_all_pts_4326, "ct_all_pts_4326.shp",delete_dsn=TRUE)
#
st_write(ct_all_polys_4326, "ct_all_polys_4326.geojson",delete_dsn=TRUE)
st_write(ct_all_pts_4326, "ct_all_pts_4326.geojson",delete_dsn=TRUE)


st_write(ct_medhigh_polys_4326, "ct_medhigh_polys_4326.shp",delete_dsn=TRUE)
st_write(ct_medhigh_pts_4326, "ct_medhigh_pts_4326.shp", delete_dsn=TRUE)
#
st_write(ct_medhigh_polys_4326, "ct_medhigh_polys_4326.geojson",delete_dsn=TRUE)
st_write(ct_medhigh_pts_4326, "ct_medhigh_pts_4326.geojson",delete_dsn=TRUE)

#
# Create a detailed polygon shapefile for each state
## Let's start with CA
#
head(ct_all_polys_4326)

#capoly <- subset(ct_all_polys_4326, grepl("G06",GISJOIN))
capoly <- subset(ct_all_polys_4326, STATEFP=='06')

head(capoly)
tail(capoly)

# save to test before doing all states
st_write(capoly, "ct_all_polys_4326_cal.geojson",delete_dsn=TRUE)


summary(ct_all_pts_4326$mymean)
summary(ct_all_pts_4326$catnum)

x<-as.data.frame(table(ct_all_pts_4326$STATEFP))

#subset the top 10 states into their own shapefiles
sub_states<- c("06","48","36","12","42","17", "39", "26", "37","34")

for (i in sub_states){
  print(i)
  x <-subset(ct_all_polys_4326, STATEFP == i)
  fname <- paste0("ct_all_polys_4326_", i,".geojson")
  print(fname)
  st_write(x,fname ,delete_dsn=TRUE)
}

#merge what's left with census regions
#need a lookup table of statefps to regions from census
#

reglut <- read.csv("statefp_region_lut.csv", 
                   colClasses=c("character","character","character","character"), stringsAsFactors = F, strip.white = T)
head(reglut)

# make a geojson for each region
# then try
ct_reg<- subset(ct_all_polys_4326, !(STATEFP %in% sub_states))

head(ct_reg)
ct_reg_poly <- left_join(ct_reg, reglut, by="STATEFP")

# upload geojson to a github repo
head(ct_reg_poly)
unique(reglut$Division)
udiv <- c("1","2","3","4","5","6","7","8","9")
for (i in udiv){
  print(i)
  x <-subset(ct_reg_poly, Division == i)
  fname <- paste0("ct_all_polys_4326_div_", i,".geojson")
  print(fname)
  st_write(x,fname ,delete_dsn=TRUE)
}
########################################
# These ddata too big for R Leaflet
########################################
# conus <- st_read("conus_4326.shp")
# #medhi <- st_read("cttracts_gte55_4326_simp0001.shp")
# 
# # Create a qualitative color palette
# myColor_function <- colorFactor(c("red","pink"), ct_medhigh_pts_4326$cat) 
# #myColor_function(ct_medhigh_pts_4326$cat)
# 
# 
# map4 <- leaflet() %>%
#   addTiles() %>%
#   # addPolygons(data=conus, 
#   #             color="grey",  # Outline color
#   #             weight=1,      # Outline thickness
#   #             fillColor="grey", 
#   #             fillOpacity = 0.25,
#   #             popup="low concern"
#   # )  %>%
#   addCircleMarkers(data=ct_medhigh_pts_4326,
#               #color="grey",  # Outline color
#               #weight=0.25,      # Outline thickness
#               color=~myColor_function(ct_medhigh_pts_4326$cat), 
#               fillOpacity = 0.5,
#               radius=3,
#               stroke=F,
#               popup="concern"
#   )
# 
# map4
# 
# saveWidget(map4, file="medhigh_map.html")
