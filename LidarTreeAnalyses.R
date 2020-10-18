library(raster)
library(rgdal)
library(lidR)
library(sf)
lidar_dem <- readLAS("D:/Data/SmithTripp/Arkin_Data/lidar/gavin_lake_all.las")
fire_wBuffer <- readOGR("D:/Data/SmithTripp/Field_SiteData/fire_Severity_100mbuffer.shp")
fire_extent <- extent(fire_wBuffer[24,]) #Subset 2017 Gavin Lake Fire
polygon_extent <- as(fire_extent, 'SpatialPolygons')

lidar_fire_subset <- lasclip(lidar_dem, polygon_extent)
plot(lidar_dem)
plot(fire_wBuffer[24,])
class(fire_wBuffer[25,])
