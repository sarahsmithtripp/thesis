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
    extract(canopy_cover_raster, microclimate_locations, 
            buffer = radii, fun = mean,
            sp = F, stringAsFactors = F)
  data <- data.frame(plot_point = microclimate_locations$plot_point, 
                     chm,
                     canopy_cover)
  names(data) <- names 
  parallel::stopCluster(cl)


  
write.csv(data, "D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Model_Inputs/canopy_metrics.csv")

