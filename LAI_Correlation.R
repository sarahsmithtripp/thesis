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

ggplot(LAI, aes(group = Plot)) 