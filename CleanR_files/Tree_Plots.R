##SST 
##OCt-18-2020
#Code written to plot output from the field tree height surveys
# Set working directory to where metadata is stored 
#setwd("F:/SmithTripp_Metadata")

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
trees <- readxl::read_excel("_Field-Collected-Data/Tree_Plots/Tree_Data.xlsx", sheet = "Cleaned")
trees$Micro_plot_fact <- trees$`Microclimate Plot`
trees <- trees %>% 
  mutate(plot_match = dplyr::case_when(.$'Micro_plot_fact'== '60' ~ "60",
                                .$'Micro_plot_fact' == '4.66' ~ '4.66',
                                .$'Micro_plot_fact' == '3.52' ~ '3.52',
                                .$'Micro_plot_fact' == "1.83" ~ '1.83', 
                                .$'Micro_plot_fact' == '1.55' ~ '1.55',
                                .$'Micro_plot_fact' == '3.66' ~ '3.66',
                                .$'Micro_plot_fact' == 'Oldguy'|.$'Micro_plot_fact' == 'OldGuy' ~ 'oldguy',  
                                .$'Micro_plot_fact' == 'cc'| .$'Micro_plot_fact' == 'CC' ~ 'cc' ,
                                .$'Micro_plot_fact' == 'cont'|.$'Micro_plot_fact' == 'Cont' |.$'Micro_plot_fact' == "CONT" ~'cont',
                                .$'Micro_plot_fact' == 'BT' ~ 'bt'))

##Add in plot level data to determine if this could vary by fire severity
#Read in plots with fire severity to order the loggers 
#write sequence to name for numbers 
seq <- seq(from =1 , to = 10, by = 1)
fs_seq <- paste0(seq)
plots <- rgdal::readOGR("_field-site-data/Plots.shp")
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

plots@data$plot_num <- factor(fs_seq, levels = fs_seq)
#Drop all things note needed from plots before joining 
plots_df <- plots@data[, c("plot_match", "X_firemean","X_firestdev",
                           "plot_num")]
trees_plots <- left_join(trees, plots_df)
#View(trees_plots)

trees_cart <- polar2cart(trees_plots)
trees_cart$Plot <- as.factor(trees_cart$Plot)

#write.csv(trees_cart, "trees_cartesian.csv")
##explore the data 
length(unique(trees$Plot))

summary_trees <- trees_cart %>% 
  filter(Distance < 6.5) %>%
  #filter(CC %in% c("D", "d", "C", "c", "1", "5", "I", "i")) %>%# clip to the final area size 
  group_by(Plot) %>% 
  mutate(dom = case_when(CC == "D" | CC == "d" | CC == "C" | CC == "c" | CC == "1" | CC == "5" ~ 1)) %>% 
  summarise(dom_ct = sum(dom, na.rm =T),
    fieldhtmax_trees = max(Adgjusted_height, na.rm = T), 
    fieldhtmean_trees = mean(Adgjusted_height, na.rm = T),
            mean_dbh = mean(as.numeric(DHB), na.rm = T),
            fieldhtsd_trees = sd(Adgjusted_height, na.rm = T),
           count = length(`TREE ID`), 
    perc_dom = dom_ct / count) 

summary_trees <- summary_trees %>% left_join(trees_cart[,c("Plot", "plot_num")], by = "Plot") %>% 
  distinct()
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
#save_plot("tree_maps.jpeg", map, base_height = 8, base_width = 8)



# Read in tree plot gps locations  ----------------------------------------

# read in field data locations 
tree_locations_partial <- rgdal::readOGR("_Field-Collected-Data/Tree_Plots/tree_locations/tree_locations.shp")

plot_12_coords <- cbind(585179.4209196416,5811225.1762742680)
plot_12_data  <- data.frame(t(c(15, "tree_12",NA, NA, 2020-10-10,585164.4209196416, 5811240.1762742680)))
names(plot_12_data) <- names(tree_locations_partial@data)
#combine plot 12 with other plots 
data_merge <- rbind(tree_locations_partial@data, plot_12_data)

#create spatial polygons data frame to work with the data 
tree_locations <- sp::SpatialPointsDataFrame(cbind(as.numeric(data_merge[,'x']),as.numeric(data_merge[,'y'])) ,data = data_merge)

#set shapefile locations 
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
query <- list.files(paste0("_RPA-Aqcuired-Data/Canopy_Structure/Canopy_Height/"), full.names = T)
file_folders <- file_folders <- list(query[[1]], ## lowerplots
                                     query[[2]], ## midplots
                                     query[[3]], ##old_guy
                                     query[[4]]) ## top plots
all_files <- unlist(lapply(file_folders, list.files, full.names = T))

#raster with no values below 2m -> possibly more accurate to canopy height 
chm_raster <- raster::raster(paste0(query[[3]])) ## all of the values included here 

#Alternative to aggregate to 2 meter resolution
# chm_raster_2m <- raster::aggregate(chm_raster, 20, fun = mean, na.rm = T)

## plot locations on CHMs
# plot.new()
# par(mar=c(0.2,0.2,0.2,0.2))
# png(filename = "chm_w_pts.png", res = 300,
#     width = 4, height = 5.5, units = "in")
# raster::plot(chm_raster)
# raster::plot(tree_location_buffer, add =T)
# dev.off()



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
#chm_2m_raster[chm_2m_raster == 0] <- NA
#chm_raster_2m[chm_raster_2m == 0]

# Insitu sampling took place within 6.5 radius plots
# Note that below will return a dataframe containing the max height
# calculated from all pixels in the buffer for each plot
CHM_max_height <- raster::extract(chm_raster,
                       tree_locations,
                       buffer = 6.5,
                       fun=max,
                       sp=TRUE,
                       stringsAsFactors=FALSE)
CHM_mean_height <- raster::extract(chm_raster,
                                  tree_locations,
                                  buffer = 6.5,
                                  fun=mean,
                                  sp=TRUE,
                                  stringsAsFactors=FALSE)
# CHM_max_height_2m <- raster::extract(chm_raster_2m,
#                                   tree_locations,
#                                   buffer = 6.5,
#                                   fun=max,
#                                   sp=TRUE,
#                                   stringsAsFactors=FALSE)


#function to clean up naming of plots for easier field vs. DAP deduction
clean_naming <- function(data){
  data@data <- tidyr::separate(data@data, name, c("Plot", "MicroclimatePt"), "-")
  data@data$Plot <- gsub("tree", "", data$Plot)
  
  #fix the four plots with poorly named values 
  data@data[12:15,2] <- rbind(c(22), c(11), c(10), c(12))
  return(data)
}


CHM_max_height <- clean_naming(CHM_max_height)
#CHM_max_height_2m <- clean_naming(CHM_max_height_2m)
CHM_mean_height <- clean_naming(CHM_mean_height)


CHM_max_field <- merge(CHM_max_height, summary_trees, by = "Plot")

levels(CHM_max_field$plot_num) 
CHM_mean_field <- merge(CHM_mean_height, summary_trees, by = "Plot")
levels(CHM_mean_field$plot_num)
#check to see if DAP better at detecting live trees 
CHM_field_mean_livetrees <- merge(CHM_mean_height, summary_livetrees, by = "Plot")
CHM_field_2m <- merge(CHM_max_height_2m, summary_trees, by = "Plot")
## plot the derived data

## added values ot align field (which includes data taken at 1.6 m)
max_plot <- ggplot(CHM_max_field@data, aes(x=chm_2m_10cmres+1.6, y = fieldhtmax_trees)) +
  geom_point(size = 3) + 
  ylab("Maximum Measured Height (m)") +
  xlab("Maximum DAP Height (m)")+
  xlim(0,35) + 
  ylim(0,35) + 
  stat_smooth(method = "lm", formula = y~ x) + 
  geom_abline(intercept = 0, slope=1) +
  #ggthemes::scale_color_tableau(palette = "Classic Cyclic", na.value = T, drop = F) +
  #ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + 
  guides(color = F) + 
  cowplot::theme_cowplot() + 
  ggtitle("Max DAP vs. Field Ht (m)") 

# remove outlier
# chm_mean_field_clean <- mutate(CHM_mean_field@data, outlier = case_when(chm_2m_10cmres > 22 | chm_2m_10cmres < 5 ~ T,
#                                                                       chm_2m_10cmres < 22 | chm_2m_10cmres > 5 ~ F))
chm_mean_field_clean <- mutate(CHM_mean_field@data, outlier = case_when(chm_2m_10cmres > 22 ~ T, 
                                                                        chm_2m_10cmres < 22 ~ F))
lm_tree_height_mean_dirt <- lm((fieldhtmean_trees-1.6) ~ chm_2m_10cmres , chm_mean_field_clean)
lm_tree_height_mean_clean <- lm((fieldhtmean_trees-1.6) ~ chm_2m_10cmres ,filter(
  chm_mean_field_clean, outlier == F))
chm_mean_field_clean$pred_dirt_mod <- predict(lm_tree_height_mean_dirt) 
mean_plot <- ggplot(chm_mean_field_clean, aes(x=chm_2m_10cmres, y = fieldhtmean_trees-1.6)) +
  geom_point(aes(shape = outlier), size = 3) +
  geom_line(aes(chm_2m_10cmres, pred_dirt_mod), linetype = "dotted", size = 1) +
  stat_smooth(data = filter(chm_mean_field_clean, outlier == F),method = "lm") +
  scale_linetype_manual(values=c("dotted", NA))+ 
  labs(shape = "") +
  ylab("Mean measured height (m)") +
  xlab("Mean DAP Height (m)")+
  xlim(0,35) + 
  ylim(0,35) +
  geom_abline(intercept = 0, slope=1) + 
  #ggthemes::scale_color_tableau(palette = "Classic Cyclic", na.value = T, drop = F) +
  #ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + 
  labs(color = "Plot") + 
  cowplot::theme_cowplot() + ggtitle(c("Mean DAP vs. Field Ht"))
mean_plot

# save_plot('D:/Data/SmithTripp/Gavin_Lake/Figures/HeightVerification_Plots.jpg',
cowplot::plot_grid(max_plot, mean_plot, rel_widths = c(1.2, 1.5),base_width =7.5, base_height = 4)


lm_tree_height_max <- lm(fieldhtmax_trees ~ chm_2m_10cmres , CHM_max_field@data)
plot(lm_tree_height_max)

#Calculate RMSE because that is a measure of how well the model fits the data, rather than a simple measure of fit 
RSS <- c(crossprod(lm_tree_height_max$residuals))
MSE <- RSS / length(lm_tree_height_max$residuals)
RMSE <- sqrt(MSE)
lm_tree_height_mean <- lm((fieldhtmean_trees-1.2) ~ chm_2m_10cmres , chm_mean_field_clean)

summary(lm_tree_height_max)
summary(lm_tree_height_mean)

lm_tree_height_max_2m <- lm(CHM_field_2m$fieldhtmax_trees
                            ~ CHM_field_2m$chm_10cm)
