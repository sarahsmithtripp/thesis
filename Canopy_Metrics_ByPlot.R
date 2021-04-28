###SST 
## 2020-Nov-03
## Election day (yikes)

##

## Output a CSV of canopy height and canopy cover at different distances from
## microclimate locations

library(tidyverse)

# Out   ------------------------------------------
query <- list.files(paste0("D:/Data/SmithTripp/Gavin_Lake/3D_models"), full.names = T)
file_folders <- file_folders <- list(query[[5]], ## lowerplots
                                     query[[6]], ## midplots
                                     query[[8]], ##old_guy
                                     query[[14]]) ## top plots
all_files <- unlist(lapply(file_folders, list.files, full.names = T))

#raster with no values below 2m -> possibly more accurate to canopy height 
chm_2m_raster <- raster::raster(paste0(query[[4]], 
                                       "/chm_2m_10cmres.tif"))

#raster with all values (heights = 0 to max) used to produce chm
chm_tif <- all_files[which(grepl("/Old_guy/chm_10cm.tif", all_files))][1]
chm_raster <- raster::raster(chm_tif)


# Load canopy Data 
canopy_cover_raster <- raster::raster("D:/Data/SmithTripp/Gavin_Lake/3D_models/Las_Catalog/canopycov_2m_10cmres.tif")

#Add in Microclimate locations 
#Add in Microclimate locations 
microclimate_locations <- rgdal::readOGR("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/GPX_Waypoints/microclimate_location_oct-22-2020.shp")
raster::plot(microclimate_locations)



# Rename Microclimate Locations to Standard Format ------------------------
#Read in plots with fire severity to order the loggers 
#write sequence to name for numbers 
seq <- seq(from =1 , to = 10, by = 1)
fs_seq <- paste0("fs", seq)
plots <- rgdal::readOGR("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/Plots.shp")
fire <- which(grepl("fire", names(plots@data)))
plot <- which(grepl("plot", names(plots@data)))
grabber_cols <- c(fire, plot)
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
  left_join(plots@data) %>% 
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
  mutate(plot_point = paste0(.$plot_num, .$point))
sub <- filter(microclimate_locations@data, plot_point == "fs107")
sub[2,c("plot_point")] <- "fs108"

microclimate_locations@data <- microclimate_locations@data %>% filter(plot_point != "fs107")
microclimate_locations@data <- rbind(microclimate_locations@data, sub)




## WRite a function to derive canopy metrics
radii <- c(2,5,10,15,20)

radii_1 <- raster::extract(chm_raster,microclimate_locations,
                           buffer = radii[1], fun =mean, 
                           sp = F, stringsAsFactors = F)

## Parallel processing of canopy metrics 
### Here is example of parallel processing in a loop - I apparently did not do it for my las processing. I just used lapply... 


library(doParallel)

library(parallel)
library(foreach)


  names <- c("plot_point", paste0("DAP_Canopy_Height_r", radii, "m"), paste0("DAP_Canopy_Cover_r", radii, "m"),paste0("DAP_Canopy_Max_r", radii, "m"))
             
  length <- length(microclimate_locations$plot_point)

  cl <- parallel::makeCluster(detectCores())
  doParallel::registerDoParallel(cl)
  chm <- foreach::foreach(radii = radii,
                          .combine = cbind, .packages = 'raster') %dopar% 
                            extract(chm_raster, microclimate_locations, 
                                            buffer = radii, fun = mean,
                                            sp = F, stringAsFactors = F)
  canopy_cover <- foreach::foreach(radii = radii, .combine = cbind, .packages = 'raster') %dopar%
    extract(canopy_cover_raster, microclimate_locations, 
            buffer = radii, fun = mean,
            sp = F, stringAsFactors = F)
  chm_max <- foreach::foreach(radii = radii, .combine = cbind, .packages = 'raster') %dopar%
    extract(canopy_cover_raster, microclimate_locations, 
            buffer = radii, fun = max,
            sp = F, stringAsFactors = F)
  data <- data.frame(plot_point = microclimate_locations$plot_point, 
                     chm,
                     canopy_cover, chm_max)
  names(data) <- names 
  parallel::stopCluster(cl)


  
write.csv(data, "D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Model_Inputs/canopy_metrics.csv")

###### Creating a raster polygon that is clipped to the cropped data
library(raster)
#load buffer area 
plot_buffer <- rgdal::readOGR("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/Site_Buffer_14Jan2020.shp")

#create canopy height mask 
chm_mask <- raster::mask(chm_raster, plot_buffer)
plot(chm_mask)

cov_mask <- raster::mask(canopy_cover_raster, plot_buffer)


#load fire severity data 
#fire <- raster::raster("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Fire_Severity_Shapes/New Folder/dnbr.tif")
fire <- raster::raster("D:/Data/SmithTripp/Gavin_Lake/3D_models/Las_Catalog/fire_severity_dnbr.tif")
proj4string(fire) <- proj4string(plot_buffer)
values(fire)[values(fire) < 0] <- 0
fire_mask <- raster::mask(fire, plot_buffer)

extent(fire) <- extent(chm_mask)
plot(fire_mask, add = T)

fire_poly <- rasterToPolygons(fire_mask, na.rm = F, dissolve = T)
base::plot(fire_poly, col = rainbow)

chm_mask_projected <- projectRaster(chm_mask, fire_mask)


chm_res_r <-  chm_mask  %>% 
  #aggregate(4) %>% 
  resample(fire_mask)

cov_res_r <- aggregate(cov_mask, 4) %>% 
  resample(fire_mask)

  fire_res_r <- resample(fire_mask, chm_mask)


# Stack covariates
fire_chm_s <- stack(fire, chm_res_r)
chm_fire_s <- stack(fire_res_r, chm_mask)
fire_cov_s <- stack(fire_mask, cov_res_r)

names(fire_chm_s) <- c("fire_sev", "canopy_ht_m")
names(chm_fire_s) <- c("fire_sev", "canopy_ht_mt")
names(fire_cov_s) <- c("fire_sev", "canopy_cov_per")

# Correlation between layers
cor(values(fire_chm_s)[,1],
    values(fire_chm_s)[,2],
    use = "na.or.complete")
cor(values(chm_fire_s)[,1],
    values(chm_fire_s)[,2],
    use = "na.or.complete")
cor(values(fire_cov_s)[,1],
    values(fire_cov_s)[,2],
    use = "na.or.complete")

data_comp <- as.data.frame(cbind(values(fire_chm_s)[,1], values(fire_chm_s)[,2]))
data_comp_chm <- as.data.frame(cbind(values(chm_fire_s)[,1], values(chm_fire_s)[,2]))
data_comp_cov <- as.data.frame(cbind(values(fire_cov_s)[,1], values(fire_cov_s)[,2]))
names(data_comp) <- c("fire_sev", "canopy_ht_m")
names(data_comp_chm) <- c("fire_sev","canopy_ht_m")
names(data_comp_cov) <- c("fire_sev","canopy_cov_per")

lm1 <- lm(canopy_ht_m ~ fire_sev, data = data_comp_chm,na.exclude)
lm2 <- lm(canopy_cov_per ~ fire_sev, data = data_comp_cov)

ggplot(data_comp, aes(fire_sev, canopy_ht_m)) + geom_point()
ggplot(data_comp_cov, aes(fire_sev, canopy_cov_per)) + geom_point()


## for each microclimate location 

length <- 1
cl <- parallel::makeCluster(detectCores())
doParallel::registerDoParallel(cl)
chm_extract <- foreach::foreach(length = length,
                        .combine = cbind, .packages = 'raster') %dopar% 
  extract(chm_mask, microclimate_locations, buffer = 15, fun = mean, stringAsFactors = F)
fire_extract <- foreach::foreach(length = length,
                                .combine = cbind, .packages = 'raster') %dopar% 
  extract(fire_mask, microclimate_locations, fun = mean, buffer = 15, stringAsFactors = F)


parallel::stopCluster(cl)

logger_data_comp <- as.data.frame(cbind(fire_extract, chm_extract))
names(logger_data_comp) <- c("fire_sev","canopy_ht_m")
ggplot(logger_data_comp, aes(fire_sev^0.5, canopy_ht_m)) + geom_point()
logger_data_comp$fire_sev_sqrt <- logger_data_comp$fire_sev ^ 0.5
lm3 <-  lm(canopy_ht_m ~ fire_sev_sqrt, logger_data_comp)

chm_canopy_zones <- raster::extract(chm_mask, fire_poly, fun = mean)


chm_mask_new <- raster::raster(vals=values(chm_mask),ext=extent(fire_mask),crs=crs(fire_mask),
                     nrows=dim(fire_mask)[1],ncols=dim(fire_mask)[2])

