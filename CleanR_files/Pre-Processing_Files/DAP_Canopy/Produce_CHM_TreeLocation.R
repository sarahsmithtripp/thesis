#Sarah Smith-Tripp 
#September 10th 2020 

#Code written to make canopy height models from point cloud dat a

library(lidR)
library(raster)
library(lidRplugins)
library(rgdal)
setwd("D:/Data/SmithTripp/Gavin_Lake/3D_models")

# create list of file locatiosn  ------------------------------------------
query <- list.files(paste0(getwd()), full.names = T)
file_folders <-      list(query[[5]], ## lowerplots
                                     query[[6]], ## midplots
                                     query[[8]], ##old_guy
  
                          
                                                             query[[14]]) ## top plots

all_files <- unlist(lapply(file_folders, list.files, full.names = T))

aligned_list <- unlist(sapply(all_files, grepl, pattern = "aligned.las"))
aligned_las <- all_files[aligned_list]
#read LAS files 
aligned <- lapply(aligned_las, readLAS)
#aligned <- lapply(aligned_las[[4]], readLAS)                    
                     




#load DEM to normalize las files
dem <- raster("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/clip_DEM.tif")

#normalize all point clouds with 1-m Lidar DEM 
normalized <- lapply(aligned, normalize_height, dem)

 #write function to change all returns to first return (because DAP point cloud)
convert_return <- function(las){
  las@data$ReturnNumber <- 1 
  return(las)
}
#convert all returns to the same
normalized_1 <- lapply(normalized, convert_return)



#create CHM
chm_list <- lapply(normalized_1, grid_canopy, res = 0.2, algorithm = p2r(0.2, na.fill = tin()))
chm_list_dsmtin <- lapply(normalized_1, grid_canopy, res = 0.1, algorithm = dsmtin(max_edge = 2))

#Save normalized las files 
# writeRaster(chm_list_dsmtin[[1]], filename = paste0(getwd(), 
#                                                     "/LowerPlots/lower_plots_chm_10cm.tif"), overwrite = T,
#             format = "GTiff")
# writeRaster(chm_list_dsmtin[[2]], filename = paste0(getwd(), 
#                                              "/Old_guy/oldguy_chm_10cm.tif"), overwrite = T, 
#             format = "GTiff")
# writeRaster(chm_list_dsmtin[[3]], filename = paste0(getwd(), 
#                                                    "/midplots/midplots_chm_10cm.tif"), overwrite = T, 
#             format = "GTiff")
# writeRaster(chm_list_dsmtin[[4]], filename = paste0(getwd(), 
#                                                     "/TopPlots/UpperPlots_chm_10cm.tif"), overwrite = T, 
#             format = "GTiff")

#create CHM with values above 2.0 m
filter_points <- function (las, height) {
  new_las <- filter_poi(las, Z >= height)
  return(new_las)
}
normalized_greaterthan2m <- lapply(normalized_1, filter_points, 2)
chm_greatthan2m <- lapply(normalized_greaterthan2m, grid_canopy, res = 0.1, algorithm = dsmtin(max_edge = 2))

#save 2m data 
# #Save normalized las files 
# writeRaster(chm_greatthan2m[[1]], filename = paste0(getwd(),
#                                                     "/LowerPlots/lower_plots_chm2m_10cm_.tif"), overwrite = T,
#             format = "GTiff")
# writeRaster(chm_greatthan2m[[2]], filename = paste0(getwd(),
#                                              "/Old_guy/oldguy_chm2m_10cm.tif"), overwrite = T,
#             format = "GTiff")
# writeRaster(chm_greatthan2m[[3]], filename = paste0(getwd(),
#                                                    "/midplots/midplots_chm2m_10cm.tif"), overwrite = T,
#             format = "GTiff")
# writeRaster(chm_greatthan2m[[4]], filename = paste0(getwd(),
#                                                     "/TopPlots/UpperPlots_chm2m_10cm.tif"), overwrite = T,
#             format = "GTiff")


#add proj4string to the data 
CRS <- CRS("+init=epsg:26910 +proj=utm +zone=10 +datum=NAD83 +units=m +no_defs
      +ellps=GRS80 +towgs84=0,0,0")
convert_CRS <- function(spatial_item, CRS){
  projection(spatial_item) <- CRS 
  return(spatial_item)
}

chm_list_dsmtin <- lapply(chm_list_dsmtin, convert_CRS, CRS)


# Run canopy metrics  ------------------


#Save CHM
# writeRaster(chm_list_dsmtin[[1]], filename = paste0(getwd(), 
#                                                     "/LowerPlots/lower_plots_chm_10cm.tif"), overwrite = T,
#             format = "GTiff")
# writeRaster(chm_list_dsmtin[[2]], filename = paste0(getwd(), 
#                                              "/Old_guy/oldguy_chm_10cm.tif"), overwrite = T, 
#             format = "GTiff")
# writeRaster(chm_list_dsmtin[[3]], filename = paste0(getwd(), 
#                                                    "/midplots/midplots_chm_10cm.tif"), overwrite = T, 
#             format = "GTiff")
# writeRaster(chm_list_dsmtin[[4]], filename = paste0(getwd(), 
#                                                     "/TopPlots/UpperPlots_chm_10cm.tif"), overwrite = T, 
#             format = "GTiff")


#Or read in the files already processed 
 chm_list_dsmtin <- list()
# 
 chm_list_dsmtin[[1]] <-  raster(paste0(getwd(), "/LowerPlots/lower_plots_chm_10cm.tif"))
# chm_list_dsmtin[[2]] <-  raster(paste0(getwd(),"/Old_guy/chm_10cm.tif"))
# chm_list_dsmtin[[3]] <-  raster(paste0(getwd(),"/midplots/midplots_chm_10cm.tif"))
# chm_list_dsmtin[[4]] <-  raster(paste0(getwd(),"/TopPlots/UpperPlots_chm_10cm.tif"))

#write canopy rasters to file 
# writeRaster(chm_list_dsmtin[[2]], filename = paste0(getwd(),"/Old_guy/chm_10cm.tif"), overwrite = T, 
#             format = "GTiff")
# writeRaster(chm_list_dsmtin[[3]], filename = paste0(getwd(),"/midplots/midplots_chm_10cm.tif"), overwrite = T, 
#             format = "GTiff")
# writeRaster(chm_list_dsmtin[[4]], paste0(getwd(),"/TopPlots/UpperPlots_chm_10cm.tif"), overwrite = T, 
 #            format = "GTiff")

for(i in 1:length(chm_list_dsmtin)) {
  plot(chm_list_dsmtin[[i]])
}

#Define custom metric table
# Create a function to calculate Mode statistic
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

my_struct_metrics = function(Z)
{
  metrics = list(
    #Number of points, use to find the minimal resolution and/or gaps in the dataset
    npoint = length(Z), #                                                                    
    #Measures of central tendency and variability
    mean = mean(Z), #Mean hight                                                               
    modus = getmode(Z),#Modus                                                                 
    min = min(Z), #Minimal height                                                              
    max = max(Z), #Maximal height                                                           
    p10 = quantile(Z, prob = c(0.1)), #10th height percentile                                 
    p20 = quantile(Z, prob = c(0.2)), #20th height percentile                                 
    p30 = quantile(Z, prob = c(0.3)), #30th height percentile                                     
    p40 = quantile(Z, prob = c(0.4)), #40th height percentile                                      
    p50 = quantile(Z, prob = c(0.5)), #50th height percentile                                
    p60 = quantile(Z, prob = c(0.6)), #60th height percentile                                      
    p70 = quantile(Z, prob = c(0.7)), #70th height percentile                                        
    p80 = quantile(Z, prob = c(0.8)), #80th height percentile                             
    p90 = quantile(Z, prob = c(0.9)), #90th height percentile                                 
    p95 = quantile(Z, prob = c(0.95)), #95th height percentile                                           
    p99 = quantile(Z, prob = c(0.99)), #99th height percentile                                            
    #Measures of variability
    std = sd(Z), # standard deviation                                                                  , #Kurtosis                                                             
    cv = ((sd(Z) / mean(Z)) * 100), # Coefficient of variation                                     
    #Measures of canopy cover
    cc000 = length(Z[Z>0])/length(Z) * 100, #Percentage of points above 0 cm  
    cc130 = length(Z[Z>0])/length(Z) * 100,
    cc200 = length(Z[Z>2])/length(Z) * 100 #Percentage of points above 200 cm                      
  )
  return(metrics)
}

returns_forCanopy <- function(Z) { 
  metrics = list(
  cc200 = length(Z[Z>2])/length(Z) * 100 
  )
  return(metrics)
  }

#Create metrics for a raster grid
las_gridmetrics = lapply(normalized_1, grid_metrics, my_struct_metrics(Z), res = 0.1)

las_canopycov <- lapply(normalized_1, grid_metrics, returns_forCanopy(Z), res = 0.1)

#or read in files already processed 
# las_gridmetrics <- list()
# 
# las_gridmetrics[[1]] <-  raster(paste0(getwd(), "/LowerPlots/lower_plots_metrics.tif"))
# las_gridmetrics[[2]] <-  raster(paste0(getwd(),"/Old_guy/oldguy_metrics.tif"))
# las_gridmetrics[[3]] <-  raster(paste0(getwd(),"/midplots/midplots_metrics.tif"))
# las_gridmetrics[[4]] <-  raster(paste0(getwd(),"/TopPlots/UpperPlots_metrics.tif"))

las_gridmetrics <- lapply(las_gridmetrics, convert_CRS, CRS)
las_canopycov <- lapply(las_canopycov, convert_CRS, CRS)
# #write raster metrics to file 
# writeRaster(las_gridmetrics[[1]], filename = paste0(getwd(),
#                                                     "/LowerPlots/lower_plots_metrics.tif"), overwrite = T,
#             format = "GTiff")
# writeRaster(las_gridmetrics[[2]], filename = paste0(getwd(),
#                                              "/Old_guy/oldguy_metrics.tif"), overwrite = T,
#             format = "GTiff")
# writeRaster(las_gridmetrics[[3]], filename = paste0(getwd(),
#                                                    "/midplots/midplots_metrics.tif"), overwrite = T,
#             format = "GTiff")
# writeRaster(las_gridmetrics[[4]], filename = paste0(getwd(),
#                                                     "/TopPlots/UpperPlots_metrics.tif"), overwrite = T,
#             format = "GTiff")


# #write raster metrics to file 
# writeRaster(las_canopycov[[1]], filename = paste0(getwd(),
#                                                     "/LowerPlots/lower_plots_canopycov.tif"), overwrite = T,
#             format = "GTiff")
# writeRaster(las_canopycov[[2]], filename = paste0(getwd(),
#                                              "/Old_guy/oldguy_canopycov.tif"), overwrite = T,
#             format = "GTiff")
# writeRaster(las_canopycov[[3]], filename = paste0(getwd(),
#                                                    "/midplots/midplots_canopycov.tif"), overwrite = T,
#             format = "GTiff")
# writeRaster(las_canopycov[[4]], filename = paste0(getwd(),
#                                                     "/TopPlots/UpperPlots_canopycov.tif"), overwrite = T,
#             format = "GTiff")



# delineate trees and their heights for in-field data collection ----------
#write a function to change the window size depending on the canopy height 
#devtools::install_github("Jean-Romain/lidRplugins")
library(lidRplugins)

f <- function(x) {
  y <- 3 * (-(exp(-0.08*(x-2)) - 1)) + 0.5
  y[x < 2] <- 0.5
  y[x > 25] <- 3
  return(y)
}

heights <- seq(-5,30,0.5)
ws <- f(heights)
plot(heights, ws, type = "l",  ylim = c(0,5))


#would ideally use ptrees because it is more accurate but currently using a use defined function 
k = c(30,15)
trees <- lapply(normalized_1[[1]], find_trees, algorithm = lmf(ws = f, hmin = 1.3), uniqueness = 'bitmerge')
trees <- list()
trees[[1]] <- find_trees(normalized_1[[1]], algorithm = lmf(ws = f, hmin = 1.3), uniqueness = 'bitmerge')


test <- trees[[1]]
test@data$treeheight <- extract(chm_list_dsmtin[[1]], trees[[1]])
cols <- heat.colors(length(test@data$treeheight))

plot(test, col = cols)

tree_height_add <- function(chm_raster, tree_sp){
  tree_sp@data$treeheight <- extract(chm_raster, tree_sp)
  return(tree_sp)
}

tree_heights <- list()
tree_heights[[1]] <-  tree_height_add(chm_list_dsmtin[[1]], trees[[1]])
for(i in 1:4){
  tree_heights[[i]] <- tree_height_add(chm_list_dsmtin[[i]], trees[[i]])
}

#write trees to file 
#Save CHM
# library(rgdal)
# writeOGR(tree_heights
#          [[1]], layer = "lower_plots_treeloc", dsn = paste0(getwd(),
#                                                     "/LowerPlots/trees"), driver = "ESRI Shapefile")
# writeOGR(tree_heights[[2]], layer = "old_guy_treeloc", dsn = paste0(getwd(),
#                                              "/Old_guy/trees"), driver = "ESRI Shapefile")
# writeOGR(tree_heights[[3]], layer = "midplotS_treeloc", dsn = paste0(getwd(),
#                                                    "/midplots/trees"), driver = "ESRI Shapefile")
# writeOGR(tree_heights[[4]], layer = "topplots_treeloc", dsn = paste0(getwd(),
#                                                     "/TopPlots/trees"), driver = "ESRI Shapefile")

