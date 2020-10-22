##SST 
##OCt-18-2020
#Code written to plot output from the field tree height surveys
setwd("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Tree_data")

#necessary packages to run 
library(ggplot2)
library(reshape2)
library(rgeos)
library(rgdal)
library(raster)
library(dplyr)
library(readxl)
library(cowplot)

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
trees <- readxl::read_excel("Tree_Data.xlsx", sheet = "Cleaned")
trees_cart <- polar2cart(trees)
trees_cart$Plot <- as.factor(trees_cart$Plot)
##explore the data 
length(unique(trees$Plot))

summary_trees <- trees %>% 
  group_by(Plot) %>% 
  summarise(
    fieldhtmax_trees = max(Adgjusted_height, na.rm = T), 
    fieldhtmean_trees = mean(Adgjusted_height, na.rm = T),
            mean_dbh = mean(as.numeric(DHB), na.rm = T),
            fieldhtsd_trees = sd(Adgjusted_height, na.rm = T),
            count = length(`TREE ID`))
summary_trees <- trees %>% 
  group_by(Plot) %>% 
  summarise(
    fieldhtmax_trees = max(Height, na.rm = T), 
    fieldhtmean_trees = mean(Height, na.rm = T),
    mean_dbh = mean(as.numeric(DHB), na.rm = T),
    fieldhtsd_trees = sd(Height, na.rm = T),
    count = length(`TREE ID`))

summary_livetrees <- trees %>% 
  filter(State %in% c("A", "a")) %>% 
  group_by(Plot) %>% 
  summarise(
    fieldhtmean_trees = mean(Height, na.rm = T)
  )
head(summary_livetrees)
head(summary_trees)
#mean number of trees measured 
round(mean(summary_trees$count), 0)


## Plot the tree plots so you can look at them later 
  ## code based of of TFK tree plots by Marissa Goodwin 

map <- ggplot(trees_cart, aes(x, y)) + 
  geom_point(aes(color=Adgjusted_height, size= as.numeric(DHB)))+
  #geom_text(hjust=0, size=4, nudge_y=0.5,nudge_x=0.5, angle=15, check_overlap = F)+
  xlab("X (meters)") + ylab("Y (meters)") + xlim(-10, 10) + ylim(-10, 10) + 
  geom_vline(xintercept=0) + geom_hline(yintercept = 0) + 
  facet_wrap(~Plot)+
  scale_color_gradient(low = "grey74", high = "grey4")+ theme_bw()+
  theme(legend.text=element_text(size=12), legend.title=element_text(size=13)) + 
  scale_size_continuous(range = c(0, 3))+
 geom_segment(arrow=arrow(length=unit(5,"mm")), aes(x=25,xend=25,y=21,yend=25), 
               colour="darkblue") +
  annotate(x=25, y=20, label="North", colour="darkblue", geom="text", size=6)+
  geom_segment(arrow=arrow(length=unit(5,"mm")), aes(x=25,xend=25,y=-21,yend=-25), 
               colour="darkblue") +
  annotate(x=25, y=-20, label="South", colour="darkblue", geom="text", size=6)+
  labs(colour = "Height (m)", size = "DBH (cm)") +
  guides(shape = guide_legend(override.aes = list(size = 1)))


map

#save tree plot maps to file 
save_plot("tree_maps.jpeg", map, base_height = 8, base_width = 8)



# Read in tree plot gps locations  ----------------------------------------

# read in field data locations 
tree_locations_partial <- rgdal::readOGR(list.files(getwd())[which(grepl("Tree_plots_NAD27.shp",list.files(getwd())))])
plot_12_coords <- cbind(585179.4209196416,5811225.1762742680)
plot_12_data  <- data.frame(t(c(1, "tree_12", NA, NA, 2020-10-10,585164.4209196416, 5811240.1762742680)))
names(plot_12_data) <- names(tree_locations_partial@data)
#combine plot 12 with other plots 
data_merge <- rbind(tree_locations_partial@data, plot_12_data)

#create spatial polygons data frame to work with the data 
tree_locations <- sp::SpatialPointsDataFrame(cbind(as.numeric(data_merge[,'x']),as.numeric(data_merge[,'y'])) ,data = data_merge)
sp::proj4string(tree_locations)<- sp::CRS("+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs")
#export shapefile and double check locations in QGIS 
#rgdal::writeOGR(tree_locations, "tree_locations", layer= "tree_locations", driver = "ESRI Shapefile", overwrite_layer = T)


#create plot area points 
tree_location_buffer <- raster::buffer(tree_locations,6.5)
raster::plot(tree_location_buffer)



# Read in CHMs  -----------------------------------------------------------
#Here there are several raster approaches 
#the first in the code uses a raster with no values below 2 meters
#after exploring the data is was detemined this was likely the most accurate of the data points 
# the next uses a raster that goes to 0 
#finally there is a raster with a lower resolution (closer to what will be input in the microclimate models )
# create list of file locations  ------------------------------------------
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

#Alternative to aggregate to 2 meter resolution
#chm_raster_2m <- raster::aggregate(chm_raster, 20, fun = mean, na.rm = T)

## plot locations on CHMs
plot.new()
par(mar=c(0.2,0.2,0.2,0.2))
png(filename = "chm_w_pts.png", res = 300,
    width = 4, height = 5.5, units = "in")
raster::plot(chm_raster)
raster::plot(tree_location_buffer, add =T)
dev.off()

#function to clean up naming of plots for easier field vs. DAP deduction
clean_naming <- function(data){
  data@data <- tidyr::separate(data@data, name, c("Plot", "MicroclimatePt"), "-")
  data@data$Plot <- gsub("tree", "", data$Plot)
  
  #fix the four plots with pooly named values 
  data@data[12:15,2] <- rbind(c(22), c(11), c(10), c(12))
  return(data)
}

#merge data to field data and calculate values of interest
DAP_comp_Field <- function(raster, field_data,variable_of_interest,
                           summary_data,
                           clean_names) {
  #raster is the DAP derived field height 
  #field is the field tree data as a spatial polygons dataframe 
  raster[raster == 0] <- NA 
  
  raster_values <- raster::extract(raster,
                                   field_data,
                                   buffer = 6.5, fun =variable_of_interest, 
                                   sp = TRUE, stringsAsFactors = F)
  if(clean_names == T){
    raster_values <- clean_naming(raster_values)
  }
  out <- merge(raster_values, summary_data, by = "Plot")
  return(out) 
  }


chm_raster[chm_raster == 0] <- NA
chm_2m_raster[chm_2m_raster == 0] <- NA
chm_raster_2m[chm_raster_2m == 0]

# Insitu sampling took place within 6.5 radius plots
# Note that below will return a dataframe containing the max height
# calculated from all pixels in the buffer for each plot
CHM_max_height <- raster::extract(chm_2m_raster,
                       tree_locations,
                       buffer = 6.5,
                       fun=max,
                       sp=TRUE,
                       stringsAsFactors=FALSE)
CHM_mean_height <- raster::extract(chm_2m_raster,
                                  tree_locations,
                                  buffer = 6.5,
                                  fun=mean,
                                  sp=TRUE,
                                  stringsAsFactors=FALSE)
CHM_max_height_2m <- raster::extract(chm_raster_2m,
                                  tree_locations,
                                  buffer = 6.5,
                                  fun=max,
                                  sp=TRUE,
                                  stringsAsFactors=FALSE)


CHM_max_height <- clean_naming(CHM_max_height)
CHM_max_height_2m <- clean_naming(CHM_max_height_2m)
CHM_mean_height <- clean_naming(CHM_mean_height)

CHM_withField <- merge(CHM_max_height, summary_trees, by = "Plot")
CHM_field_mean_all <- merge(CHM_mean_height, summary_trees, by = "Plot")

#check to see if DAP better at detecting live trees 
CHM_field_mean_livetrees <- merge(CHM_mean_height, summary_livetrees, by = "Plot")
CHM_field_2m <- merge(CHM_max_height_2m, summary_trees, by = "Plot")
## plot the derived data
library(ggplot2)
max_plot <- ggplot(as.data.frame(CHM_withField), aes(x=chm_2m_10cmres, y = fieldhtmax_trees)) +
  geom_point() +
  theme_bw() +
  ylab("Maximum measured height (m)") +
  xlab("Maximum DAP Pixel (m)")+
  xlim(0,35) + 
  ylim(0,35) + 
  geom_abline(intercept = 0, slope=1) +
  ggtitle("Max DAP vs. Field Ht (m)")
mean_plot <- ggplot(as.data.frame(CHM_field_mean_all), aes(x=chm_2m_10cmres, y = fieldhtmean_trees)) +
  geom_point() +
  theme_bw() +
  ylab("Mean measured height (m)") +
  xlab("Mean DAP Pixel (m)")+
  xlim(0,35) + 
  ylim(0,35) +
  geom_abline(intercept = 0, slope=1) + 
  ggtitle("Mean DAP vs. Field Ht")

cowplot::plot_grid(max_plot, mean_plot, rel_widths = c(1.2, 1.3))

lm_tree_height_max <- lm(chm_10cm ~ fieldhtmax_trees, CHM_withField)
lm_tree_height_mean <- lm(chm_2m_10cmres ~ fieldhtmean_trees, CHM_field_mean_all)
mean_plot
