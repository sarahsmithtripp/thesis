### Author: Sarah Smith-Tripp 
## July 21 2020 (half-birthday woot!)
# contact: sarahsmith.tripp@gmail.com

##This script reads in a set of lat-long coordinates from a CSV
## It then defines their extent and creates a bounding box around them 
## this bounding box is then used to clip the LAS file of interest 
setwd("D:/Data/SmithTripp/Gavin_Lake/3D_models")
library(readxl)
library(tidyverse)
library(rgdal)
library(sp)
library(rgeos)
library(raster)
library(dplyr)


# functions to use throughout ---------------------------------------------
#subset a data.frame based on a word or character or interest 
#grabber is the thing you are using to query
#data is the data frame 
#column is the column you want to query 
grabber <- function(grabber, data, column) { 
  column_to_grab <- which(grepl(column, names(data)))
  query_ID <- which(grepl(grabber, data[,column]))
  data_grabbed <- data[query_ID,]
  return(data_grabbed)
}

##
# Read in Lat-Long Coordinates of the plots  ------------------------------
options(digits = 22)
d <-read.csv("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/GPX_Waypoints/microclimate_locations_WGS84.csv")
d$point <- as.character(d$point)


#block data by area because the flights are flown by area
areas <- c("upperplots", "lowerplots", "midplots", "oldguy")

d_areas <- d %>% 
  mutate(area = as.factor(case_when( plot == '60.' | plot == 'cont' ~ "lowerplots",
                                     plot == "1.55"| plot == "cc" | plot == "3.52" | plot == "4.66" ~ "upperplots",
                                     plot == "1.83" | plot == "3.66" | plot == "bt" ~ "midplots",
                                     plot == "oldguy" ~ "oldguy")))

#load test LAS files 
library(lidR)
lowerplot_las <- readLAS("LowerPlots/LowerPlots.las")
lidar_las <- readLAS("D:/Data/SmithTripp/Gavin_Lake/IRSS_data/Arkin_Data/lidar/gavin_lake_all.las")
projection(lidar_las) <- CRS(
   "+init=epsg:26910 +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs
     +ellps=GRS80 +towgs84=0,0,0" )

area_extent <- function(flight_name, data, meters,
                        plot_las, lidar) {
  ## collect the area flown to then write the extent of the shapefile as the boundary
  flight_areas <- grabber(flight_name, data, "area")
  #create a list to store output shapefiles
  plot_names <- unique(flight_areas$plot)
  poly <- list()
  maxmin_ht <- list()
  ##write each area seperately into a shapefile
  for (i in 1:length(plot_names)) {
    plot <- plot_names[i]
    area_d <- grabber(plot, flight_areas, "plot")
    area_sf <-
      SpatialPointsDataFrame(
        area_d[, c("long", "lat", "hght")],
        area_d,
        proj4string = CRS(
          "+init=epsg:4326 +proj=longlat +ellps=WGS84
          +datum=WGS84 +no_defs +towgs84=0,0,0"))
    area_NAD83 <-
      spTransform(
        area_sf,
        CRSobj = CRS(
          "+init=epsg:26910 +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs
          +ellps=GRS80 +towgs84=0,0,0"
        )
      )
    # subset to a just the exterior
    coords_NAD83 <-
      as.data.frame(coordinates(area_NAD83)) #points are in the order of the data (verified by hand)
    coords_NAD83$point <- area_d$point
    corners <-  as.matrix(filter(coords_NAD83, point %in%  c(1, 3, 9, 7)))
    corners <- apply(corners, 2, as.numeric)
    corners_sort <- corners[order(corners[, 4]), ]
    plot_edge <- Polygon(coords = corners_sort[, 1:2], hole = F)
    print(i)
    poly[i] <- Polygons(list(plot_edge), plot)
    maxmin_ht[[i]] <- c(max(coords_NAD83$hght), min(coords_NAD83$hght))
  }
  maxmin_ht <- do.call(rbind.data.frame, maxmin_ht)
  names(maxmin_ht) <- c("max", "min")
  flight_area <- SpatialPolygons(poly)
  bounding_box <- as(extent(flight_area@bbox), "SpatialPolygons")
  proj4string(bounding_box) <-
    CRS(
      "+init=epsg:26910 +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs
      +ellps=GRS80 +towgs84=0,0,0"
    )
  bounding_box_df <-
    as.data.frame(bounding_box@polygons[[1]]@Polygons[[1]]@coords)
  ## create output dataframe to store with
  boundary <- data.frame(matrix(ncol = 7, nrow = 1))
  names(boundary) <-
    c("ymax", "ymin", "xmax", "xmin", "zmax", "zmin", "name")
  boundary[1, ] <- c(
    max(bounding_box_df$V2) + meters,
    min(bounding_box_df$V2) - meters,
    max(bounding_box_df$V1) + meters,
    min(bounding_box_df$V1) - meters,
    min(maxmin_ht$min),
    max(maxmin_ht$max),
    flight_name
  )
  plot_clip <- lasclipRectangle(plot_las, 
                               as.numeric(boundary$xmin),
                               as.numeric(boundary$ymin), 
                               as.numeric(boundary$xmax), 
                               as.numeric(boundary$ymax))
  lidar_clip <- lasclipRectangle(lidar, 
                                 as.numeric(boundary$xmin),
                                 as.numeric(boundary$ymin), 
                                 as.numeric(boundary$xmax), 
                                 as.numeric(boundary$ymax))
  plot_clip <- decimate_points(plot_clip, algorithm = homogenize(2000, res = 1)) #decrease point density to something managable
  return(list(flight_area, boundary, bounding_box, plot_clip, lidar_clip))
}

area_extent_onecloud <- function(flight_name, data, meters,
                                             las) {
  ## collect the area flown to then write the extent of the shapefile as the boundary
  flight_areas <- grabber(flight_name, data, "area")
  #create a list to store output shapefiles
  plot_names <- unique(flight_areas$plot)
  poly <- list()
  maxmin_ht <- list()
  ##write each area seperately into a shapefile
  for (i in 1:length(plot_names)) {
    plot <- plot_names[i]
    area_d <- grabber(plot, flight_areas, "plot")
    area_sf <-
      SpatialPointsDataFrame(
        area_d[, c("long", "lat", "hght")],
        area_d,
        proj4string = CRS(
          "+init=epsg:4326 +proj=longlat +ellps=WGS84
          +datum=WGS84 +no_defs +towgs84=0,0,0"))
    area_NAD83 <-
      spTransform(
        area_sf,
        CRSobj = CRS(
          "+init=epsg:26910 +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs
          +ellps=GRS80 +towgs84=0,0,0"
        )
      )
    # subset to a just the exterior
    coords_NAD83 <-
      as.data.frame(coordinates(area_NAD83)) #points are in the order of the data (verified by hand)
    coords_NAD83$point <- area_d$point
    corners <-  as.matrix(filter(coords_NAD83, point %in%  c(1, 3, 9, 7)))
    corners <- apply(corners, 2, as.numeric)
    corners_sort <- corners[order(corners[, 4]), ]
    plot_edge <- Polygon(coords = corners_sort[, 1:2], hole = F)
    print(i)
    poly[i] <- Polygons(list(plot_edge), plot)
    maxmin_ht[[i]] <- c(max(coords_NAD83$hght), min(coords_NAD83$hght))
  }
  maxmin_ht <- do.call(rbind.data.frame, maxmin_ht)
  names(maxmin_ht) <- c("max", "min")
  flight_area <- SpatialPolygons(poly)
  bounding_box <- as(extent(flight_area@bbox), "SpatialPolygons")
  proj4string(bounding_box) <-
    CRS(
      "+init=epsg:26910 +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs
      +ellps=GRS80 +towgs84=0,0,0"
    )
  bounding_box_df <-
    as.data.frame(bounding_box@polygons[[1]]@Polygons[[1]]@coords)
  ## create output dataframe to store with
  boundary <- data.frame(matrix(ncol = 7, nrow = 1))
  names(boundary) <-
    c("ymax", "ymin", "xmax", "xmin", "zmax", "zmin", "name")
  boundary[1, ] <- c(
    max(bounding_box_df$V2) + meters,
    min(bounding_box_df$V2) - meters,
    max(bounding_box_df$V1) + meters,
    min(bounding_box_df$V1) - meters,
    min(maxmin_ht$min),
    max(maxmin_ht$max),
    flight_name
  )

  las_clip <- lasclipRectangle(las, 
                                 as.numeric(boundary$xmin),
                                 as.numeric(boundary$ymin), 
                                 as.numeric(boundary$xmax), 
                                 as.numeric(boundary$ymax))
  plot_clip <- decimate_points(las_clip, algorithm = homogenize(3500, res = 1)) #decrease point density to something managable
  return(list(flight_area, boundary, bounding_box, plot_clip)) 
}

#lowerplots <- area_extent_onecloud("lowerplots",d_areas,100, lowerplot_las)
#lowerplots_lidar <- area_extent_onecloud("lowerplots", d_areas, 100, lidar_las )
##Apply to other plots
#upper plots
upperplot_las <- readLAS("TopPlots/TopPlots_Part1.las")
upperplot_las_2 <- readLAS("TopPlots/TopPlots_part2.las")
#extent(upperplot_las)
upperplots_las_clip_1 <- area_extent_onecloud("upperplots", d_areas, 20, upperplot_las)
rm(upperplot_las)
upperplots_las_clip_2 <- area_extent_onecloud("upperplots", d_areas, 20, upperplot_las_2)
rm(upperplot_las_2)
upperplot_las_lidar <- area_extent_onecloud("upperplots", d_areas, 20, lidar_las)

upperplot_las <- merge_spatial(upperplot_las_clip_1, upperplot_las_clip_2)
#write LAS to file 
writeLAS(lowerplots[[4]], file = "LowerPlots/LowerPlots_clip.las")
writeLAS(lowerplots_lidar[[4]], file = "LowerPlots/LowerPlots_lidar_clip.las")

#upper plots 
writeLAS(upperplots_las_clip_2[[4]], file = "TopPlots/UpperPlots_clip_2.las")
writeLAS(upperplots_las_clip_1[[4]], file = "TopPlots/UpperPlots_clip_1.las")
writeLAS(upperplot_las_lidar[[4]], file = "TopPlots/UpperPlots_lidar_clip.las")

#middple plots 
#middleplot_las <- readLAS("Midplots/midplots_las_redo.las")
#middleplot_clip <- area_extent_onecloud("midplots", d_areas, 20, middleplot_las)
#lidar_clip <- area_extent_onecloud("midplots", d_areas, 20, lidar_las)
#writeLAS(middleplot_clip[[4]], file = "Midplots/MidPlots_clip_redo.las")
#writeLAS(lidar_clip[[4]], file = "Midplots/MidPlots_lidar_clip.las")

##old guy plots
old_guy <- readLAS("Old_guy/OldGuy_ChunkScript.las")
lidar_clip <- area_extent_onecloud("oldguy", d_areas, 25, lidar_las) 
old_guy_clip <- area_extent_onecloud("oldguy", d_areas, 25, old_guy)

writeLAS(lidar_clip[[4]], file = "Old_guy/Oldguy_lidar_clip.las")
writeLAS(old_guy_clip[[4]], file = "Old_guy/Oldguy_clip.las")
plot_las_clip <- lasclipRectangle(plot_las, as.numeric(lowerplots[[2]]$xmin), as.numeric(lowerplots[[2]]$ymin), as.numeric(lowerplots[[2]]$xmax), as.numeric(lowerplots[[2]]$ymax))
plot(plot_las_clip)

