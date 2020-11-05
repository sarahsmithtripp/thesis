##SST 
## Oct-21-2020
#Script written to develop a linear relationship between LAI estimates
#And DAP derived estimates of canopy cover 

#packages
#necessary packages to run 
library(ggplot2)
library(reshape2)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)
library(readxl)
library(cowplot)


#Set working directory to LAI files 
setwd("D:/Data/SmithTripp/Gavin_Lake/LAI_Data")

## Necessary Variables 
## LAI Measurements for all Microclimate locations 
## GPS of all Microclimate locations 
## Coverage values for all plots 


# Work with LAI to compute plot level metrics (similiar to soil pl --------
LAI <- readxl::read_excel("LAI_Plots.xlsx", sheet = "Cleaned")

LAI_PlotSummaries <- LAI %>% 
  group_by(Plot) %>% 
  summarise(mean_LAI = mean(LAI, na.rm =T),
            sd_LAI = sd(LAI, na.rm = T))

LAI <- left_join(LAI, LAI_PlotSummaries, by = "Plot")

View(LAI)

#Leaf Area Index Variability  
LAI_plot <- ggplot(LAI, aes(group = Plot, color = Plot)) + 
  geom_boxplot(aes(Plot, LAI), alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  #geom_point(aes(Plot, mean_LAI), alpha = 0.8, position = 'jitter')+
  ylab("LAI") + 
  labs(color = "Plot") +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  cowplot:: theme_cowplot()
LAI_plot


# Read in Microclimate Locations /Cover Raster (these are the same as the LAI locations)--------
microclimate_locations <- rgdal::readOGR("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/GPX_Waypoints/microclimate_location_oct-22-2020.shp")
raster::plot(microclimate_locations)


canopy_cover <- raster::raster("D:/Data/SmithTripp/Gavin_Lake/3D_models/Las_Catalog/canopycov_2m_10cmres.tif")
plot(canopy_cover)

#calculate buffer area 
angle <- 68 #angle of 5th ring in LAI 
height <- 1.2 #approximate height of measurments 
top <- 20.2 #define a maximum for now (technically this is dependent on the height of the canopy, and that determines the radius)

buffer_area <- tan((angle*pi /180)) *(top-height)

canopy_cov_metrics <- raster::extract(canopy_cover,microclimate_locations,
                                                       buffer = buffer_area, fun =mean, 
                                                       sp = TRUE, stringsAsFactors = F)
#this is the average % cover over the buffer (# of points above 2m)
canopy_cov_metrics@data$avgper_cov <- canopy_cov_metrics@data$canopycov_2m_10cmres


#clean naming between LAI and canopy covert to join 
canopy_cov_metrics@data$plot_point <- paste(canopy_cov_metrics@data$plot, canopy_cov_metrics@data$point, sep = "-")
LAI <- LAI %>% 
  mutate(plot_match = as.factor(case_when( Plot == '-60' ~ "60",
                                     Plot == '4.66' ~ '4.66',
                                     Plot == '3.52' ~ '3.52',
                                     Plot == '1.83' ~ '1.83', 
                                    Plot == '1.55' ~ '1.55',
                                    Plot == '3.66' ~ '3.66',
                                    Plot == 'OG' ~ 'oldguy', 
                                    Plot == 'CC' ~ 'cc' ,
                                    Plot == 'CONT' ~ 'cont',
                                    Plot == 'BTW' ~ 'bt')))
LAI$plot_point <- paste(LAI$plot_match, LAI$Point, sep = "-")

#Merge LAI with Canopy Cover Data 

CanopyCov_LAI <- merge(canopy_cov_metrics, LAI, by = "plot_point")

Canopy_LAI_plot <- ggplot(as.data.frame(CanopyCov_LAI), aes(x=LAI, y = avgper_cov)) +
  geom_point(aes(col = plot)) +
  theme_bw() +
  xlab("Field Measured LAI") +
  ylab("Avg % of pts above 2m") 
  #stat_smooth(method = "lm")

Mean_Height_Graph <- ggplot(as.data.frame(CanopyCov_LAI), aes(plot_num, mean_ht, group = plot_num, color = plot_num)) + 
  geom_boxplot(alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  geom_point(alpha = 0.8, position = 'jitter')+
  ylab("Mean Canopy Height (m)") + 
  xlab("plot") + 
  labs(color = "Plot") +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  cowplot:: theme_cowplot()
Canopy_LAI_plot

lm_canopy_LAI <- lm(avgper_cov ~ LAI, CanopyCov_LAI)
summary(lm_canopy_LAI)


