###SST 
## 2020-Nov-03
## Election day (yikes)
library(raster)
library(rgdal)
library(sf)

##

## Output a CSV of slope and elevation and aspect metrics of inclusion in the metadata 


# Out   ------------------------------------------
setwd("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData")

#raster with no values below 2m -> possibly more accurate to canopy height 
dem_raster <- raster::raster("Clip_DEM.tif")


#Add in Microclimate locations 
microclimate_locations <- rgdal::readOGR("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/GPX_Waypoints/microclimate_location_oct-22-2020.shp")
raster::plot(microclimate_locations, add = T)



# Rename Microclimate Locations to Standard Format ------------------------
#Read in plots with fire severity to order the loggers 
#write sequence to name for numbers 
seq <- seq(from =1 , to = 10, by = 1)
fs_seq <- paste0("fs", seq)
fire <- which(grepl("fire", names(plots@data)))
plot <- which(grepl("plot", names(plots@data)))
grabber_cols <- c(fire, plot)

plots <- rgdal::readOGR("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/Plots.shp")
plots@data <- plots@data %>% 
  arrange(X_firemean) %>%
  mutate(
    plot_match = as.factor(case_when( plot == '60' ~ "60",
                                      plot == '4.66' ~ '4.66',
                                      plot == '3.52' ~ '3.52',
                                      plot == '1.83' ~ '1.83', 
                                      plot == '1.55' ~ '1.55',
                                      plot == '3.66' ~ '3.66',
                                      plot == 'oldguy' ~ 'oldguy', 
                                      plot == 'cc' ~ 'cc' ,
                                      plot == 'cont' ~ 'cont',
                                      plot == 'bt' ~ 'bt'))) 

plots@data <- plots@data[,grabber_cols]


plots@data$plot_num <- factor(fs_seq, levels = fs_seq)

## Bind Plots to Microclimate_locations 

microclimate_locations@data <- microclimate_locations@data %>% 
  left_join(plots@data[,c("X_firemean", "X_firestdev", "plot_match", "plot_num", "plot")]) %>% 
  distinct()

## Fix poorly named points, within a function to double check for errors 
fix_errors <- function(points) {
  errors <- which(points$point < 0 )
  if(length(errors) > 1)
    print("You have to fix some shit")
  else
    points[errors, c('point')] <- 5
  return(points)
}

microclimate_locations@data <- fix_errors(microclimate_locations@data)

microclimate_locations@data <- microclimate_locations@data %>%
  mutate(plot_point = paste0(plot_num, point))
microclimate_locations@data[,c("field_1", "plot", "point", "path", "layer", "z_DEM", "time", "FID", "name")] <- list(NULL)

microclimate_locations@data$hght_DEM <- raster::extract(dem_raster,microclimate_locations, fun = NULL)
                           sp = F, stringsAsFactors = F)


microclimate_locations <- sp::spTransform(microclimate_locations, CRS('+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'))

microclimate_locations@coords[,3] <- microclimate_locations@data$hght_DEM
microclimate_locations@data[,c("long", "lat")] <- microclimate_locations@coords[,1:2]

#writeOGR(microclimate_locations, "D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Model_Inputs", "microclimate_loc", driver = "ESRI Shapefile")

## WRite a function to derive canopy metrics
radii <- c(2,5,10,15,20)

## Parallel processing of canopy metrics 

library(doParallel)
library(parallel)
library(foreach)

## process for topography metrics (taken out of function because not working)
  names <- c("plot_point", paste("elev radius", radii), paste("aspect radius", radii), paste("slope radius", radii), paste("TRI radius", radii))
  length <- length(microclimate_locations$plot_point)
  
  cl <- parallel::makeCluster(detectCores())
  doParallel::registerDoParallel(cl)
  
  elev <- foreach::foreach(radii = radii,
                          .combine = cbind, .packages = 'raster') %dopar% 
    extract(dem_raster, microclimate_locations, 
            buffer = radii, fun = mean,
            sp = F, stringAsFactors = F)
   aspect <- foreach::foreach(radii = radii, .combine = cbind, .packages = 'raster') %dopar%
    extract(terrain(dem_raster, opt = "aspect"), microclimate_locations, 
            buffer = radii, fun = mean,
            sp = F, stringAsFactors = F)
   slope <- foreach::foreach(radii = radii, .combine = cbind, .packages = 'raster') %dopar%
     extract(terrain(dem_raster, opt = "slope"), microclimate_locations, 
             buffer = radii, fun = mean,
             sp = F, stringAsFactors = F)
   TRI <- foreach::foreach(radii = radii, .combine = cbind, .packages = 'raster') %dopar%
     extract(terrain(dem_raster, opt = "TRI"), microclimate_locations, 
             buffer = radii, fun = mean,
             sp = F, stringAsFactors = F)
  terrain_values <- data.frame(plot_point = microclimate_locations$plot_point, 
                     elev, aspect, slope, TRI)
  names(terrain_values) <- names 
  parallel::stopCluster(cl)


write.csv(terrain_values, "D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Model_Inputs/terrain_metrics.csv")

