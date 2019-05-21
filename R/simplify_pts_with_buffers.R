library(sf)
library(rgdal)


setwd("~/Downloads/simple_tracts100/ct2015_epsg5070/ct2015_epsg5070_20_35")
dir()

#
# This was a data reduction test in which the census tract centroids where buffered and dissolved
# and points in the same buffer polygon had dnl values averaged.
# MBTiles seems to do this anyway so not used.
#
p1 <- st_read("ct_all_pts_5070.shp")
p2<- st_buffer(p1, 4000)  # within 4KM / ~2.5 miles

# Limit the object to just geometry
p3 <- st_geometry(p2)
p4 <- st_union(p3)

p5 = st_cast(p4, "POLYGON")
plot(p5)

head(p1)
p1_dnl = p1[c("mymean","geometry")]
p6_avg = aggregate(x=p1_dnl, by=p5, FUN=mean)

head(p6_avg)
summary(p6_avg)
# Categorical variable
p6_avg$cat <- "low"
table(p6_avg$cat)
p6_avg$cat <- ifelse(p6_avg$mymean >= 50, "med",  p6_avg$cat)
table(p6_avg$cat)
p6_avg$cat <- ifelse(p6_avg$mymean >= 54, "high", p6_avg$cat)
table(p6_avg$cat)

#reorder
p7 <- p6_avg[order(p6_avg$mymean, decreasing = FALSE),]
# compute centroids
# Create a centroid version of the dataset
p7_pts <- st_centroid(p7)
plot(p7_pts$geometry)
# Transform data to WGS84 (4326)
ctbuf10k_all_pts_4326 <- st_transform(p7_pts, 4326)

#
st_write(ctbuf10k_all_pts_4326, "ctbuf10k_all_pts_4326.geojson",delete_dsn=TRUE)
#

#
# Average the dnlmean census tract point data to county polygon data
## Do this is we want to show county polygons when zoomed way out (saves browser memory?)
#
setwd("~/Downloads/simple_tracts100/ct2015_epsg5070/ct2015_epsg5070_20_35/dnlproj/R")

c1<- st_read("conus_counties_5m_4326.geojson")

st_crs(c1)
c1geo <- st_set_crs(c1, 4326)
p1_dnl_geo <- st_transform(p1_dnl, 4326)
c1dnl <- aggregate(x=p1_dnl_geo, by=c1geo, FUN=mean)
head(c1dnl)
plot(c1dnl)

# Categorical variable - NOTE effect of countywide average
c1dnl$cat <- "low"
table(c1dnl$cat)
c1dnl$cat <- ifelse(c1dnl$mymean >= 50, "med",  c1dnl$cat)
table(c1dnl$cat)
c1dnl$cat <- ifelse(c1dnl$mymean >= 54, "high", c1dnl$cat)
table(c1dnl$cat)

st_write(c1dnl, "countymean_dnl.geojson",delete_dsn=TRUE)
summary(c1dnl$mymean)
