##SST 
##OCt-18-2020
#Code written to plot output from the field tree height surveys
setwd("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Tree_data")


##write function to convert polar to cartesian 
polar2cart <- function(data) { 
  dist <- data[,c("Distance")]
  theta <- data[,c("Azimuth")]
  x <- dist * cos(theta)
  y <- dist * sin(theta)
  data_return <- cbind(data, x, y)
  names(data_return) <- c(paste0(names(data)), "x", "y")
  return(data_return)
}

## read in the data
library(readxl)
trees <- readxl::read_excel("Tree_Data.xlsx", sheet = "Cleaned")
trees_cart <- polar2cart(trees)
##explore the data 
length(unique(trees$Plot))
library(dplyr)
summary_trees <- trees %>% 
  group_by(Plot) %>% 
  summarise(mean_trees = mean(Adgjusted_height, na.rm = T),
            mean_dbh = mean(as.numeric(DHB), na.rm = T),
            sd_trees = sd(Adgjusted_height, na.rm = T),
            count = length(`TREE ID`))

#mean number of trees measured 
round(mean(summary_trees$count), 0)


## Plot the tree plots so you can look at them later 
library(ggplot2)
library(reshape2)
map <- ggplot(trees_cart, aes(x, y)) + 
  geom_point(aes(color=Adgjusted_height, size= as.numeric(DHB)))+
  #geom_text(hjust=0, size=4, nudge_y=0.5,nudge_x=0.5, angle=15, check_overlap = F)+
  xlab("X (meters)") + ylab("Y (meters)") + xlim(-10, 10) + ylim(-10, 10) + 
  geom_vline(xintercept=0) + geom_hline(yintercept = 0) + 
  facet_wrap(~as.factor(Plot))+
  scale_color_gradient(low = "grey74", high = "grey4")+ theme_bw()+
  theme(legend.text=element_text(size=12), legend.title=element_text(size=13)) + 
  scale_size_continuous(range = c(0, 3))+
 geom_segment(arrow=arrow(length=unit(5,"mm")), aes(x=25,xend=25,y=21,yend=25), 
               colour="darkblue") +
  annotate(x=25, y=20, label="North", colour="darkblue", geom="text", size=6)+
  geom_segment(arrow=arrow(length=unit(5,"mm")), aes(x=25,xend=25,y=-21,yend=-25), 
               colour="darkblue") +
  annotate(x=25, y=-20, label="South", colour="darkblue", geom="text", size=6)+
  labs(colour = "Height (m)", size = "DBH (cm)")
  guides(shape = guide_legend(override.aes = list(size = 1)))


map


library(cowplot)

save_plot("tree_maps.jpeg", map, base_height = 8, base_width = 8)


## currently using plot 11 as an example because I actually have the position 
plot_11_loc <- c(585207.253302,5811293.66831)

plot_11 <- subset(trees_cart, Plot == "11")

library(rgdal)
library(raster)
lower_plots_canopy <- readGDAL("D:/Data/SmithTripp/Gavin_Lake/3D_models/LowerPlots/lower_plots_chm_10cm.tif")

#write function to create CHM, clip the DAP CHm to the area around the plot, and and then calculate at difference raster 
chm_error <- function(fieldtrees, dap_trees){
  #field trees is the excel for the plot area 
  #DAP trees is the canopy height model
  #create an empty raster stack to store CHMS 
  raster_stack <- stack()
  
  dap_trees_clip <- extract(dap_trees, points, 
                         buffer = 6.5,
                         fun=max,
                         sp=TRUE,
                         stringsAsFactors=FALSE)
  

  #make a canopy height model for the tree plot
  field_chm <- rasterFromXYZ(fieldtrees[, c("x","y", "Adgjusted_height")], res = 2, digits = ) 
}
