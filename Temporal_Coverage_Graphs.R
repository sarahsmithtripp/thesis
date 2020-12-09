##SST 
#2020-12-01
#code develops a st of temporal coverage graphs for the microclimate loggers 


library(tidyverse)
library(cowplot)
seq <- seq(from =1 , to = 10, by = 1)
fs_seq <- paste0("fs", seq)

meta_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/CA_ST_SoilTempData/CA_ST_MetaData.csv", header = T)
meta_data$plot <- as.factor(meta_data$plot)
levels(meta_data$plot) <- seq(1,10, by = 1)

climate_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/Microclimate_Measurements/Microclimate_TMS_UserSoils_Nov-25-20.csv")
climate_data <- climate_data %>% 
  filter(DateTime_GMT > "2020-05-15" & DateTime_GMT < "2020-10-10") %>% 
  mutate(DateTime = as.Date(DateTime_GMT), 
         Plotcode = as.factor(Plotcode), 
         DateTime_GMT = lubridate::ymd_hms(DateTime_GMT),
         Hour =lubridate::hour(DateTime_GMT), 
         DateTime_Hour = lubridate::ymd_h(paste(DateTime, Hour))) %>%
  left_join(meta_data[, c('Plotcode', 'plot')])
  
levels(climate_data$plot)

### Add day and night to data frame 
#install.packages("StreamMetabolism")
library(StreamMetabolism)
#define latitude and longitude of the field site
lat <- meta_data$Latitude[1]
long <- meta_data$Longitude[1]

sunrise_set <- sunrise.set(date = as.Date('2020/05/13'), num.days =160, lat = as.numeric(lat), lon = as.numeric(long), timezone = "UTC+7")
sunrise_set$DateTime <- as.Date(sunrise_set$sunrise)

climate_data_sunrise_join <- climate_data %>% 
  inner_join(sunrise_set) %>% 
  mutate(day_night = ifelse(DateTime_GMT > sunrise & DateTime_GMT < sunset, 'day', 'night'))

climate_data <- climate_data_sunrise_join[, c(names(climate_data), 'day_night')]


meta_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/CA_ST_SoilTempData/CA_ST_MetaData.csv", header = T)
meta_data <- meta_data[2:nrow(meta_data), ]
# add a section to look at the variation between different soil type --------
soil_moisture_expl <- climate_data %>%
  left_join(meta_data[,c('Plotcode', 'perc_silt_r1m')]) %>% 
  group_by(DateTime_GMT, perc_silt_r1m) %>% 
  mutate(sm_mean_soiltype = mean(vol_sm, na.rm = T), 
         sm_mean_ct = mean(SM_Count, na.rm = T))  %>%
group_by(DateTime_Hour, Plotcode) %>% 
  summarize(plot  = plot,
            day_night = day_night, 
            sm_mean_plot_d = mean(vol_sm, na.rm = T), 
            T1_mean_h = mean(T1, na.rm = T), 
            T2_mean_h = mean(T2, na.rm = T), 
            T3_mean_h = mean(T3, na.rm = T)) 
soil_moisture_expl  <- distinct(soil_moisture_expl)



# Graph the Temporal length of the Data -----------------------------------

#first define some colors 
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 90
mycolors_day <- colorRampPalette(brewer.pal(10, "Spectral"))(nb.cols)
mycolors_night <- colorRampPalette(brewer.pal(10, "BrBG"))(nb.cols)
n <- seq(0, 90, by = 20)
graph_temps <- function(num, soil_moisture_expl, var, day) {
  subset_data_q <- levels(soil_moisture_expl$Plotcode)[num:(num + 19)]
  subset_data <-
    filter(soil_moisture_expl, Plotcode %in% subset_data_q)
  if (day == 'day') {
    subset_data <- filter(subset_data, day_night == 'day')
    if (var == 'T1') {
      graph <- ggplot(subset_data) +
        geom_point(
          aes(DateTime_Hour, T1_mean_h, color = Plotcode),
          size = 0.2,
          shape = 16, alpha = 0.4 
        ) +
        scale_x_datetime(date_labels = "%B") +
        theme_bw() +
        ylab("Temperature T1") + 
        scale_color_manual(values = mycolors_day) + 
        xlab(NULL)
      graph
      #cowplot::save_plot(graph)
    }
    else if (var == 'T2') {
      graph <- ggplot(subset_data) +
        geom_point(
          aes(DateTime_Hour, T2_mean_h, color = Plotcode),
          size = 0.2,
          shape = 17, alpha = 0.4
        ) +
        scale_x_datetime(date_labels = "%B") +
        theme_bw() +
        ylab("Temperature T2") + 
        scale_color_manual(values = mycolors_day) + 
        xlab(NULL)
      graph
    }
    
    else if (var == 'T3') {
      graph <- ggplot(subset_data) +
        geom_point(
          aes(DateTime_Hour, T3_mean_h, color = Plotcode),
          size = 0.2,
          shape = 18, alpha = 0.4 
        ) +
        scale_x_datetime(date_labels = "%B") +
        theme_bw() +
        ylab("Temperature T3") + 
        scale_color_manual(values = mycolors_day) + 
        xlab(NULL)
      graph
    }
  }
  else if (day == 'night') {
    subset_data <- filter(subset_data, day_night == 'night')
    if (var == 'T1') {
      graph <- ggplot(subset_data) +
        geom_point(
          aes(DateTime_Hour, T1_mean_h, color = Plotcode),
          size = 0.2,
          shape = 16, alpha = 0.4
        ) +
        scale_x_datetime(date_labels = "%B") +
        theme_bw() +
        ylab("Temperature T1") + 
        scale_color_manual(values = mycolors_night) + 
        xlab(NULL)
      graph
      #cowplot::save_plot(graph)
    }
    else if (var == 'T2') {
      graph <- ggplot(subset_data) +
        geom_point(
          aes(DateTime_Hour, T2_mean_h, color = Plotcode),
          size = 0.2,
          shape = 17, alpha = 0.4
        ) +
        scale_x_datetime(date_labels = "%B") +
        theme_bw() +
        ylab("Temperature T2") + 
        scale_color_manual(values = mycolors_night) + 
        xlab(NULL)
      graph
    }
    
    else if (var == 'T3') {
      graph <- ggplot(subset_data) +
        geom_point(aes(DateTime_Hour, T3_mean_h, color = Plotcode), size = 0.2,shape = 18, alpha = 0.5) +
        scale_x_datetime(date_labels = "%B") +
        theme_bw() +
        ylab("Temperature T3") + 
        scale_color_manual(values = mycolors_night) + 
        xlab(NULL)
      graph
    }
  }
  return(graph)
}


T3_day <- lapply(n, graph_temps, soil_moisture_expl = soil_moisture_expl, var = 'T3', day = 'day')
T1_day <- lapply(n, graph_temps, soil_moisture_expl = soil_moisture_expl, var = 'T1', day = 'day')
T2_day <- lapply(n, graph_temps, soil_moisture_expl = soil_moisture_expl, var = 'T2', day = 'day')

T3_night <- lapply(n, graph_temps, soil_moisture_expl = soil_moisture_expl, var = 'T3', day = 'night')
T2_night <- lapply(n, graph_temps, soil_moisture_expl = soil_moisture_expl, var = 'T2', day = 'night')
T1_night <- lapply(n, graph_temps, soil_moisture_expl = soil_moisture_expl, var = 'T1', day = 'night')


# library(cowplot)
# pdf(file = "D:/Data/SmithTripp/Gavin_Lake/Figures/Temperature_TimeSeries.pdf")
# ##T3
# cowplot::plot_grid(ggdraw()+draw_label("T3 Hourly Values - Daytime"), T3_day[[1]], T3_day[[2]], T3_day[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
# cowplot::plot_grid(ggdraw()+draw_label("T3 Hourly Values - Daytime"), T3_day[[4]], T3_day[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 
# cowplot::plot_grid(ggdraw()+draw_label("T3 Hourly Values - Nighttime"), T3_night[[1]], T3_night[[2]], T3_night[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
# cowplot::plot_grid(ggdraw()+draw_label("T3 Hourly Values - Nighttime"), T3_night[[4]], T3_night[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 
# 
# ##T2 
# cowplot::plot_grid(ggdraw()+draw_label("T2 Hourly Values - Daytime"), T2_day[[1]], T2_day[[2]], T2_day[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
# cowplot::plot_grid(ggdraw()+draw_label("T2 Hourly Values - Daytime"), T2_day[[4]], T2_day[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 
# cowplot::plot_grid(ggdraw()+draw_label("T2 Hourly Values - Nighttime"), T2_night[[1]], T2_night[[2]], T2_night[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
# cowplot::plot_grid(ggdraw()+draw_label("T2 Hourly Values - Nighttime"), T2_night[[4]], T2_night[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 
# 
# ##T1
# cowplot::plot_grid(ggdraw()+draw_label("T1 Hourly Values - Daytime"), T1_day[[1]], T1_day[[2]], T1_day[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
# cowplot::plot_grid(ggdraw()+draw_label("T1 Hourly Values - Daytime"), T1_day[[4]], T1_day[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 
# cowplot::plot_grid(ggdraw()+draw_label("T1 Hourly Values - Nighttime"), T1_night[[1]], T1_night[[2]], T1_night[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
# cowplot::plot_grid(ggdraw()+draw_label("T1 Hourly Values - Nighttime"), T1_night[[4]], T1_night[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 
# 
# dev.off()

## Different style temperature plots 
T1_plots <- ggplot(soil_moisture_expl) +
  geom_point(
    aes(DateTime_Hour, T1_mean_h, color = Plotcode),
    size = 0.2,
    shape = 16, alpha = 0.4 
  ) +
  scale_x_datetime(date_labels = "%B") +
  theme_bw() +
  ylab("Temperature T1") + 
  scale_color_manual(values = mycolors_day) + 
  xlab(NULL) + facet_wrap(~plot)
T2_plots <- ggplot(soil_moisture_expl) +
  geom_point(aes(DateTime_Hour, T2_mean_h, color = Plotcode),
             size = 0.2,
             shape = 16, alpha = 0.4) +
  scale_x_datetime(date_labels = "%B") +
  theme_bw() +
  ylab("Temperature T1") + 
  scale_color_manual(values = mycolors_day) + 
  xlab(NULL) + facet_wrap(~Plot)
T3_plots <-  ggplot(soil_moisture_expl) +
  geom_point(aes(DateTime_Hour, T3_mean_h, color = Plotcode),
             size = 0.2,
             shape = 16, alpha = 0.4) +
  scale_x_datetime(date_labels = "%B") +
  theme_bw() +
  ylab("Temperature T1") + 
  scale_color_manual(values = mycolors_day) + 
  xlab(NULL) + facet_wrap(~Plot)

# plot NA values in dataset 
Na_values <- climate_data %>% select(-c("Time", "X", "DateTime_GMT", "DateTime_Hour", "Hour")) %>% pivot_longer(starts_with("T"), names_to = "Sensor",  values_to = "Temperature") %>%
  subset(is.na(Temperature)) %>% distinct() %>% mutate(plot_num = substr(Plotcode, 9,11)) 
Nan_graph <- ggplot(Na_values, aes(DateTime, plot_num, group = plot)) + 
  geom_point(aes(color = plot)) + facet_wrap(~Sensor) +theme_bw() +xlab("Plot ID") + ylab("Month") + ggthemes::scale_color_tableau(palette = "Classic Cyclic") + 
  ggtitle("Values Excluded From Dataset")
Nan_sm <- climate_data %>% subset(is.na(vol_sm)) %>% distinct() %>% mutate(plot_num = substr(Plotcode, 9, 11))
Nan_sm_graph <- ggplot(Nan_sm, aes(DateTime, plot_num, group = plot)) + 
  geom_point(aes(color = plot)) +theme_bw() +xlab("Plot ID") + ylab("Month") + ggthemes::scale_color_tableau(palette = "Classic Cyclic") + 
  ggtitle("Values Excluded From Dataset")

ggsave(Nan_graph, filename  = "D:/Data/SmithTripp/Gavin_Lake/Figures/Dropped_Values.jpeg")
# soil Moisture plots  ----------------------------------------------------
soil_moisture_expl <- climate_data %>% 
  mutate(Plotcode = as.factor(Plotcode), 
         DateTime_GMT = lubridate::ymd_hms(DateTime_GMT),
         Hour =lubridate::hour(DateTime_GMT), 
         DateTime_Hour = lubridate::ymd_h(paste(DateTime, Hour))) %>%
  left_join(meta_data[,c('Plotcode', 'perc_silt_r1m')]) %>% 
  group_by(DateTime_GMT, perc_silt_r1m) %>% 
  mutate(sm_mean_soiltype = mean(vol_sm, na.rm = T), 
         sm_mean_ct = mean(SM_Count, na.rm = T))

sm_counts <- ggplot(soil_moisture_expl, aes(group = as.factor(perc_silt_r1m))) +
  geom_line(aes(DateTime_GMT, sm_mean_ct, color = as.factor(perc_silt_r1m))) + 
  theme_bw() + 
  ylab('Soil Moisture Count') + xlab("") + 
  labs(color = '% Silt') + theme(legend.position = "none")
sm_moist <- ggplot(soil_moisture_expl, aes(group = as.factor(perc_silt_r1m))) +
  geom_line(aes(DateTime_GMT, sm_mean_soiltype, color = as.factor(perc_silt_r1m))) + 
  theme_bw() + 
  ylab('Volumetric Soil Moisture (%)') + xlab("") + 
  labs(color = '% Silt') 
legend <- get_legend(sm_moist)
sm_moist_no_leg <- sm_moist + theme(legend.position = "none")
left_side_side <- plot_grid(ggdraw() + draw_label("Mean Values by Soil Type (all % Silt Values are Unique Soils)"), sm_counts, sm_moist_no_leg, 
                            nrow = 3, ncol = 1, rel_heights = c(0.15, 1,1))
sm_moisture <- plot_grid(left_side_side, legend, rel_widths = c(1, 0.1))
ggsave(sm_moisture, filename  = "D:/Data/SmithTripp/Gavin_Lake/Figures/soil_moisture_bysoiltype.jpeg")
