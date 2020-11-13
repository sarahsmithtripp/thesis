###SST 
## 2020-Nov-03
## Election day (yikes)

##

## Output a CSV of canopy height and canopy cover at different distances from
## microclimate locations



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
canopy_cover <- raster::raster("D:/Data/SmithTripp/Gavin_Lake/3D_models/Las_Catalog/canopycov_2m_10cmres.tif")

#Add in Microclimate locations 
microclimate_locations <- readOGR("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Model_Inputs/microclimate_loc.shp")
raster::plot(microclimate_locations)


radii_1 <- raster::extract(chm,microclimate_locations,
                           buffer = radii[1], fun =mean, 
                           sp = F, stringsAsFactors = F)

## WRite a function to derive canopy metrics
radii <- c(2,5,10,15,20)

## Parallel processing of canopy metrics 

library(doParallel)
library(parallel)
library(foreach)


  names <- c("plot_point", paste0("DAP_Canopy_Height_r", radii, "m"), paste0("DAP_Canopy_Cover_r", radii, "m"))
  length <- length(microclimate_locations$plot_point)

  cl <- parallel::makeCluster(detectCores())
  doParallel::registerDoParallel(cl)
  chm <- foreach::foreach(radii = radii,
                          .combine = cbind, .packages = 'raster') %dopar% 
                            extract(chm_raster, microclimate_locations, 
                                            buffer = radii, fun = mean,
                                            sp = F, stringAsFactors = F)
  canopy_cover <- foreach::foreach(radii = radii, .combine = cbind, .packages = 'raster') %dopar%
    extract(canopy_cover, microclimate_locations, 
            buffer = radii, fun = mean,
            sp = F, stringAsFactors = F)
  data <- data.frame(plot_point = microclimate_locations$plot_point, 
                     chm,
                     canopy_cover)
  names(data) <- names 
  parallel::stopCluster(cl)


  
write.csv(data, "D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Model_Inputs/canopy_metrics.csv")

