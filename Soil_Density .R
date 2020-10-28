## Script written to make figure for soil density 

library(dplyr)
library(tidyverse)

#Sarah's Laptop 
#setwd("~/Documents/UBC/Thesis/Methods")
#School Computer
setwd("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Microclimate_SiteData(veg-soil)")

#read in soil_data
soil_data <- readxl::read_excel("Sample_Sites_Soil.xlsx", sheet = "Sheet1")

soil_data_plots <- soil_data %>% 
  tidyr::separate(col = ID...1, into = c("plot", "point"), sep = '-w', extra = "merge")
good_boy_plots<- filter(soil_data_plots, !is.na(point))
## delineate plots that were not well named and have to be seperated by hand :( )
### write a function to deal with that bad boy data 
bad_plot_naming_sad <- function(delimiter, grabber, og_data) {
  data <- which(grepl(grabber, 
                      og_data$ID...1))
  bad_boy_df <- og_data[data,]
  bad_boy_df_tidy <- tidyr::separate(bad_boy_df, col = "ID...1", into = c("plot", "point"), sep = delimiter, extra = "merge")
  return(bad_boy_df_tidy)
}
### deal with that bad boy data god damn 
## clearcut
cc <- bad_plot_naming_sad(2, "CC", soil_data)
##oldguy 
oldguy <- bad_plot_naming_sad(6, "OLDGUY", soil_data)
## 60 
plot_60 <- bad_plot_naming_sad(4, "60.", soil_data)
#cont 
cont <- bad_plot_naming_sad(5, "CONT", soil_data)
w1.83 <- bad_plot_naming_sad(5, "1.83", soil_data)
bt <- bad_plot_naming_sad(4, "BT", soil_data)
w3.66 <- bad_plot_naming_sad(5, "3.66", soil_data)
w4.66 <- bad_plot_naming_sad(5, "4.66", soil_data)
w3.52 <- bad_plot_naming_sad(5, "3.52", soil_data)
w1.55 <- bad_plot_naming_sad(5, "1.55", soil_data)


all_plots <- rbind(cc, oldguy, plot_60, cont,
                   w1.83, w1.55, w3.52, w4.66, 
                   w3.66, bt)
all_plots <- all_plots %>% 
  mutate(area = as.factor(case_when( plot == '-60.' | plot == '-CONT' ~ "lowerplots",
                                     plot == "1.55-"| plot == "CC" | plot == "3.52-" | plot == "4.66-" ~ "upperplots",
                                     plot == "1.83-" | plot == "3.66-" | plot == "BT-W" ~ "midplots",
                                     plot == "OLDGUY" ~ "oldguy")),
         plot_match = as.factor(case_when( plot == '-60.' ~ "60",
                                           plot == '4.66-' ~ '4.66',
                                           plot == '3.52-' ~ '3.52',
                                           plot == '1.83-' ~ '1.83', 
                                           plot == '1.55-' ~ '1.55',
                                           plot == '3.66-' ~ '3.66',
                                           plot == 'OLDGUY' ~ 'oldguy', 
                                           plot == 'CC' ~ 'cc' ,
                                           plot == '-CONT' ~ 'cont',
                                           plot == 'BT-W' ~ 'bt')))

#bind poorly named plots to the main dataframe

  
vegetation_data <- readxl::read_excel("Vegetation_Data_Clean.xlsx")
litter <- which(grepl("Lit", names(vegetation_data)))
veg <- which(grepl("Veg", names(vegetation_data)))
vegetation_data[,c(veg, litter)] <- lapply(vegetation_data[,c(veg, litter)], as.numeric)
litter <- vegetation_data[,c(1, litter)] %>% 
  pivot_longer(cols = starts_with("Lit"), values_to = c("Lit_depth_cm"), names_to = "Litter_Measurement") %>% 
  tidyr::separate(col = "Litter_Measurement", into = c("drop_1", "Measurement","drop_2"), sep = "_", extra = "merge")
litter <- litter[,-c(which(grepl("drop", names(litter))))]
veg <- vegetation_data[,c(1, veg)] %>%
  pivot_longer(cols = starts_with("Veg"), values_to = c("Veg_depth_cm"), names_to = "Veg_Measurement") %>% 
  tidyr::separate(col = "Veg_Measurement", into = c("drop_1", "Measurement","drop_2"), sep = "_", extra = "merge")
veg <- veg[,-c(which(grepl("drop", names(veg))))]
veg_lit <- litter %>% 
  left_join(veg, by = c("Logger", "Measurement")) %>%
  left_join(vegetation_data[,c("Logger", "Comments Oct", "Comments june-july", "Original Logger")])
  


### Incorporating Replaced Loggers 

names(veg_lit)[1]<- "logger"
veg_all_plots<- all_plots %>% left_join(veg_lit, by = "logger")



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
all_plots <- left_join(all_plots, plots_df, by = "plot_match")
veg_all_plots <- left_join(veg_all_plots, plots_df, by = "plot_match")
write.csv(veg_all_plots, "Veg-Soil-PlotFireSev.csv")

veg_all_sum <- veg_all_plots %>% group_by(logger) %>% 
  summarize(mean_veg = mean(Veg_depth_cm),
            stdev_veg = sd(Veg_depth_cm),
            mean_lit = mean(Lit_depth_cm), 
            stdev_lit = sd(Lit_depth_cm)) %>% 
  inner_join(all_plots[, c("plot_num", "logger", "area")], by = "logger")%>%
  mutate(logger_fac = as.factor(logger))



library(ggplot2)

#bulk density 
bulk_density_graph <- ggplot(all_plots, aes(plot_num, `bulk density`, group = plot_num, color = plot_num)) + 
  geom_boxplot(alpha = 0.2, outlier.color = NA, position = position_dodge(0.8), aes(fill = plot_num)) + 
  geom_point(alpha = 0.7, sive = 1.2, position = 'jitter')+
  ylab("Bulk Density (g/cm^3)") + 
  labs(color = "Plot") +
  xlab("Plot")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot()

bulk_density_graph
#vegetation_data 

library(dplyr)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}


GeomFlatViolin <-
  ggproto(
    "GeomFlatViolin",
    Geom,
    setup_data = function(data, params) {
      data$width <- data$width %||%
        params$width %||% (resolution(data$x, FALSE) * 0.9)
      
      # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
      data %>%
        dplyr::group_by(.data = ., group) %>%
        dplyr::mutate(
          .data = .,
          ymin = min(y),
          ymax = max(y),
          xmin = x,
          xmax = x + width / 2
        )
    },
    
    draw_group = function(data, panel_scales, coord)
    {
      # Find the points for the line to go all the way around
      data <- base::transform(data,
                              xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
      
      # Make sure it's sorted properly to draw the outline
      newdata <-
        base::rbind(
          dplyr::arrange(.data = base::transform(data, x = xminv), y),
          dplyr::arrange(.data = base::transform(data, x = xmaxv), -y)
        )
      
      # Close the polygon: set first and last point the same
      # Needed for coord_polar and such
      newdata <- rbind(newdata, newdata[1,])
      
      ggplot2:::ggname("geom_flat_violin",
                       GeomPolygon$draw_panel(newdata, panel_scales, coord))
    },
    
    draw_key = draw_key_polygon,
    
    default_aes = ggplot2::aes(
      weight = 1,
      colour = "grey20",
      fill = "white",
      size = 0.5,
      alpha = NA,
      linetype = "solid"
    ),
    
    required_aes = c("x", "y")
  )



library(cowplot)
veg_all_sum$plot <- as.factor(veg_all_sum$plot_num)
veg_all_sum$plot_area <- do.call(paste0, veg_all_sum[, c("plot", "area")])
veg_all_plots$plot_area <- do.call(paste0, veg_all_plots[, c("plot", "area")])
lit_graph <- ggplot()+ 
  geom_violin(data = veg_all_plots, aes(plot_num, Lit_depth_cm, color = plot_num, fill = plot_num), alpha = 0.4) + 
  geom_point(data = veg_all_sum, aes(plot_num,mean_lit, group = logger, color = plot_num), alpha = 0.8, position = 'jitter') +
  ylab("Litter Depth (cm)") + 
  labs(color = "Area") + 
  xlab("Plot")+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + 
  guides(color = F, fill= F) + 
  cowplot::theme_cowplot()
veg_graph <- ggplot()+ 
  geom_violin(data = veg_all_plots, aes(plot_num, Veg_depth_cm, color = plot_num, fill = plot_num), alpha = 0.4 ) + 
  geom_point(data = veg_all_sum, aes(plot_num,mean_veg, group = logger, color = plot_num), alpha = 0.8, position = 'jitter') +
  ylab("Vegetation Height (cm)") + 
  labs(color = "Area") + 
  xlab("Plot")  +
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") +
  guides(color = F, fill = F) +
  cowplot::theme_cowplot()


legend <- get_legend(bulk_density_graph)
save_plot('D:/Data/SmithTripp/Gavin_Lake/Figures/veg_litter.jpg', 
          plot_grid(lit_graph, veg_graph, legend, nrow = 1, rel_widths = c(1,1,0.2)),
          base_width =7.5, base_height = 4)
save_plot('D:/Data/SmithTripp/Gavin_Lake/Figures/bulk_density.jpg', 
          bulk_density_graph,
          base_width =4.5, base_height = 3)

test <- subset(veg_all_sum, plot_area == "OLDGUYoldguy")
  

ggplot()+ 
  geom_flat_violin(data = veg_all_plots, aes(Veg_depth_cm, plot_num, color = plot_num, fill = plot_num), alpha = 0.4 ) + 
  geom_point(data = veg_all_sum, aes(mean_veg,plot_num, group = logger, color = plot_num), alpha = 0.8, position = 'jitter') +
  ylab("Vegetation Height (cm)") + 
  labs(color = "Area") + 
  xlab("Plot")  +
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") +
  guides(color = F, fill = F) +
  cowplot::theme_cowplot()

length(veg_all_plots$Veg_depth_cm)

veg_graph


View(all_plots %>% 
       group_by(plot_num) %>% 
       summarise(max = max(`bulk density`, na.rm = T),
              min = min(`bulk density`, na.rm = T), 
              mean = mean(`bulk density`, na.rm = T),
              range= max - min))

mean(all_plots$`bulk density`, na.rm = T)
anova(lm(all_plots$`bulk density` ~ all_plots$plot_num))

View(all_plots)
