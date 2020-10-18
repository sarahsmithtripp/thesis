#sarah Smith-Tripp
#September 9-21 
#Ortho Accuracy 
## Running to see the accuracy of the otho accuracy alignment to Canopy Height Model
#written independently for each raster because the scale is variable for all rasters 

#state pixel accuracy for the rasters 
pixel_mean_err <- as.data.frame(cbind(c("lowerplots", "midplots", "topplots_1", "top_plots_2", "old_guy"), c(40.3932,83.7998,11.6851,10.403, 40.52), 
                        c(0.0239249,0.026918708,0.0266349,0.0231879,0.017335927)))

names(pixel_mean_err) <- c("plots","mean_ref_err",
                          "pixel_size")
library(dplyr) 
pixel_mean_err$mean_ref_err <- as.numeric(pixel_mean_err$mean_ref_err)
pixel_mean_err$pixel_size <- as.numeric(pixel_mean_err$pixel_size)

#calculate accuracy of alignment in meters 
pixel_mean_err$accuracy_alignment <- pixel_mean_err$mean_ref_err * pixel_mean_err$pixel_size


#shift microclimate points based on locations in orthophotos 
options(digits = 22)
setwd("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/GPX_Waypoints")
list.files(getwd())
locations <- read.csv("Microclimate_locations_ts_NAD.csv")
w1.83_5 <- subset(locations, field_1 == 52)
w1.83_5$pointname <- c("1.83w.5")
locations <- subset(locations, field_1 != 52)
locations <- rbind(w1.83_5, locations)
ortho_points <- read.csv(list.files(getwd())[26])
distance_microclimates <- rgdal::readOGR("distance_othophotos.shp")             
distance_matrix <- as.matrix(distance_microclimates@data)
ind_min <- function(row){ 
  minimum = min(row)
  ID <- as.character(row[1])
  index = which(row == minimum)
  name <- names(index)
  return (c(name, minimum, ID))
}


errors <- as.data.frame(t(apply(distance_matrix, 1, ind_min)))
errors[,1] <- gsub( "X", "", as.character(errors[,1]), n) # remove X's from in front of points
#errors[,1] <-gsub("\\.*", "", as.character(errors[,1]), n)
errors <- subset(errors, V3 > 5 )
names(errors) <- c("pointname", "error_m", "ortho_ID")

location_errors <- full_join(locations, errors, by  = "pointname")
location_errors$T_block <- as.factor(location_errors$T_block)

names(ortho_points) <- c("ortho_ID", "long_otho", "lat_ortho")
ortho_points$ortho_ID <- as.character(ortho_points$ortho_ID)
location_errors <- left_join(location_errors, ortho_points, by = "ortho_ID")
location_errors$lat_dist <- location_errors$lat_NAD - location_errors$lat_ortho
location_errors$lon_dist <- location_errors$long_NAD - location_errors$long_otho
location_errors$error_check <- (location_errors$lat_dist^2 + location_errors$lon_dist^2)^0.5



location_errors_pts <- location_errors %>% 
  subset(error_m < 0) %>% 
  group_by(T_block)

T_block_2 <- c(-4.8, -1)  
T_block_3 <- c(-0.63, -0.96)
T_block_4 <- c(0.27, 1.14)
T_block_5 <- c(-7.79377426533028483, -3.228687839582562447)
T_block_6 <- c(-0.92567107302602381, 2.434133107773959637)
T_block_7 <- c(-0.96021664654836059, 2.205467282794415951)
T_block_8 <- c(-1.863473891280591488, 2.10809400491047194)

shift_df <- data.frame(T_block = as.factor(seq(1,8, by = 1)),
                       lat_shift = c(NA, -4.8, -0.63, 0.27, 
                                     -7.79377426533028483,
                                     -0.92567107302602381,
                                     -0.96021664654836059,
                                     -1.863473891280591488),
                       long_shift = c(NA, -1, -0.96, 1.14, 
                                      -3.228687839582562447,
                                      2.434133107773959637,
                                      2.205467282794415951,
                                      2.10809400491047194 ))
library(tidyverse)
location_errors <- left_join(location_errors, shift_df, by = "T_block")
  
location_errors$lat_new = location_errors$lat_NAD - location_errors$lat_shift
location_errors$long_new =  location_errors$long_NAD - location_errors$long_shift

write.csv(location_errors, "microclimate_ortho_correct.csv")
