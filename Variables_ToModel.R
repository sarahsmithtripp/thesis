## SSt 

# This is a script written to calculate all of the variables to be inpuit 
# into microclimate modelign 

#Started Oct-22-2020
#Modeling (in order of inclusion)
#Canopy Height (Mean and Maximum)
#Canopy openness (% percent of points above 2m)
#Vegetation Height 
#Soil Density 
#Litter Depth 


#First read in the microclimate logger locations 
microclimate_locations <- rgdal::readOGR("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/GPX_Waypoints/microclimate_location_oct-22-2020.shp")


microclimate_locations@data <- microclimate_locations@data %>% 
  mutate(plot_match = as.factor(case_when( plot == '60' ~ "60",
                                  plot == '4.66' ~ '4.66',
                                  plot == '3.52' ~ '3.52',
                                  plot == '1.83' ~ '1.83', 
                                  plot == '1.55' ~ '1.55',
                                  plot == '3.66' ~ '3.66',
                                  plot == 'oldguy' ~ 'oldguy', 
                                  plot == 'cc' ~ 'cc' ,
                                  plot == 'cont' ~ 'cont',
                                  plot == 'bt' ~ 'bt')), 
         )

#Read in plots with fire severity to order the loggers 
#write sequence to name for numbers 
seq <- seq(from =1 , to = 10, by = 1)
fs_seq <- paste0("fs", seq)
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

plots@data$plot_num <- factor(fs_seq, levels = fs_seq)
#Drop all things note needed from plots before joining 
plots_df <- plots@data[, c("plot_match", "X_firemean","X_firestdev",
                           "plot_num")]

microclimate_locations <- merge(microclimate_locations, plots_df, by = "plot_match")


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


chm_2m_raster[chm_2m_raster == 0] <- NA


CHM_max_height <- raster::extract(chm_2m_raster,microclimate_locations,
                                                   buffer = 2,
                                                   fun=max,
                                                   sp=TRUE,
                                                   stringsAsFactors=FALSE)
CHM_mean_height <- raster::extract(chm_2m_raster,microclimate_locations,
                                  buffer = 2,
                                  fun=mean,
                                  sp=TRUE,
                                  stringsAsFactors=FALSE)
CHM_mean_height$mean_ht <- CHM_mean_height$chm_2m_10cmres

CHM_var<- cbind(CHM_max_height, CHM_mean_height@data[,c("mean_ht")])
CHM_var$mean_ht <- CHM_var$c.7.8335704716174..4.27007432610883..15.8263530985412..18.5441143467313..
View(CHM_max_height@data)

#Max Height
Max_Height_Graph <- ggplot(as.data.frame(CHM_max_height@data), aes(plot_num, chm_2m_10cmres, group = plot_num, color = plot_num)) + 
  geom_boxplot(alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  geom_point(alpha = 0.8, position = 'jitter')+
  ylab("Max Canopy Height (m)") + 
  xlab("plot") + 
  labs(color = "Plot") +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  cowplot:: theme_cowplot() 

#Mean Height  
Mean_Height_Graph <- ggplot(as.data.frame(CHM_var), aes(plot_num, mean_ht, group = plot_num, color = plot_num)) + 
  geom_boxplot(alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  geom_point(alpha = 0.8, position = 'jitter')+
  ylab("Mean Canopy Height (m)") + 
  xlab("plot") + 
  labs(color = "Plot") +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  cowplot:: theme_cowplot()

legend <- get_legend(Mean_Height_Graph)

Max_Height_Graph <- Max_Height_Graph + guides(color = F, fill = F)
Mean_Height_Graph <- Mean_Height_Graph + guides(color = F, fill = F)


partial <- plot_grid(Max_Height_Graph, Mean_Height_Graph, nrow = 2, ncol =1)
partial

full <- plot_grid(partial, legend, rel_widths = c(1,0.2))
full
