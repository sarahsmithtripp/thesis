## Creates first simple models of microclimate 

### reads in data submitted to soiltemp database that is clipped in the microclimate_logger_parsing - removing erroneous values 
library(tidyverse)



climate_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/Microclimate_Measurements/Microclimate_filtered_Vol_Sm_Nov-22-20.csv", header = T)
climate_data <- climate_data %>% 
  filter(DateTime_GMT > "2020-05-15" & DateTime_GMT < "2020-10-10") %>% 
  mutate(DateTime = as.Date(DateTime_GMT))


meta_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/CA_ST_SoilTempData/CA_ST_MetaData.csv", header = T)
meta_data <- meta_data[2:nrow(meta_data), ]


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

# add a section to look at the variation between different soil type --------
soil_moisture_expl <- climate_data %>% 
  mutate(Plotcode = as.factor(Plotcode), 
         DateTime_GMT = lubridate::ymd_hms(DateTime_GMT),
         Hour =lubridate::hour(DateTime_GMT), 
         DateTime_Hour = lubridate::ymd_h(paste(DateTime, Hour))) %>%
  group_by(DateTime_GMT, TMS_SoilType) %>% 
  mutate(sm_mean_soiltype = mean(vol_sm, na.rm = T), 
         sm_mean_ct = mean(SM_Count, na.rm = T)) %>%
  group_by(DateTime_Hour, Plotcode) %>% 
  summarize(
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
nb.cols <- 20
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
title <- ggdraw() + draw_label("MPG declines with displacement and horsepower", fontface='bold')
library(cowplot)
pdf(file = "D:/Data/SmithTripp/Gavin_Lake/Figures/Temperature_TimeSeries.pdf")
##T3
cowplot::plot_grid(ggdraw()+draw_label("T3 Hourly Values - Daytime"), T3_day[[1]], T3_day[[2]], T3_day[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
cowplot::plot_grid(ggdraw()+draw_label("T3 Hourly Values - Daytime"), T3_day[[4]], T3_day[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 
cowplot::plot_grid(ggdraw()+draw_label("T3 Hourly Values - Nighttime"), T3_night[[1]], T3_night[[2]], T3_night[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
cowplot::plot_grid(ggdraw()+draw_label("T3 Hourly Values - Nighttime"), T3_night[[4]], T3_night[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 

##T2 
cowplot::plot_grid(ggdraw()+draw_label("T2 Hourly Values - Daytime"), T2_day[[1]], T2_day[[2]], T2_day[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
cowplot::plot_grid(ggdraw()+draw_label("T2 Hourly Values - Daytime"), T2_day[[4]], T2_day[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 
cowplot::plot_grid(ggdraw()+draw_label("T2 Hourly Values - Nighttime"), T2_night[[1]], T2_night[[2]], T2_night[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
cowplot::plot_grid(ggdraw()+draw_label("T2 Hourly Values - Nighttime"), T2_night[[4]], T2_night[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 

##T1
cowplot::plot_grid(ggdraw()+draw_label("T1 Hourly Values - Daytime"), T1_day[[1]], T1_day[[2]], T1_day[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
cowplot::plot_grid(ggdraw()+draw_label("T1 Hourly Values - Daytime"), T1_day[[4]], T1_day[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 
cowplot::plot_grid(ggdraw()+draw_label("T1 Hourly Values - Nighttime"), T1_night[[1]], T1_night[[2]], T1_night[[3]], nrow = 4, rel_heights=c(0.1, 1,1,1)) 
cowplot::plot_grid(ggdraw()+draw_label("T1 Hourly Values - Nighttime"), T1_night[[4]], T1_night[[5]],  nrow = 3, rel_heights=c(0.15, 1,1)) 

dev.off()
# ggplot(soil_moisture_expl, aes(group = as.factor(TMS_SoilType))) + 
#          geom_line(aes(DateTime_GMT, sm_mean_ct, color = as.factor(TMS_SoilType)))
#   #geom_point(aes(DateTime_GMT, sm_mean_plot_d), size = 0.1, alpha = 0.4) + 
#   #facet_wrap(~TMS_SoilType)

# develop microclimate summary data  --------------------------------------

climate_modeling <- climate_data %>% 
  group_by(Plotcode, DateTime) %>% 
  mutate(range_T1_d = max(T1) - min(T1), 
         max_T1_d = max(T1), 
         min_T1_d = min(T1))%>% 
  group_by(Plotcode, month) %>% 
  summarise(range_T1_m = mean(range_T1_d, na.rm = T), 
            max_T1_m = mean(max_T1_d, na.rm = T),
            min_T1_m = mean(min_T1_d, na.rm = T)) %>% 
  left_join(meta_data, by = 'Plotcode') %>% 
  mutate(plot = as.factor(gsub('.{1}$', '', plot_point))) %>%
  filter(range_T1_m < 10)


weird_data <- climate_modeling %>% 
  filter(range_T1_m > 10)

ggplot(climate_modeling, aes(log(as.numeric(DAP_Canopy_Height_r2m)+1))) + 
  geom_density()




#this function currently does NOT work 
plotter <- function(data,det_variable, resp_variable, transform, label) { 
  det_variable_list <- names(data)[which(grepl(det_variable, names(data)))]
  if(transform == T) { 
    data_det_vars <- data[, c(det_variable_list)]
    data_det_vars <- apply(data_det_vars, 2, as.numeric) + 1
    data_det_vars_log <- apply(data_det_vars, 2, log)
    data_transform <- data.frame(data[, c(resp_variable, 'Plotcode', 'month')], data_det_vars_log)
    data_transform <- distinct(data_transform)
    data_1 <- data_transform[,c(resp_variable, det_variable_list[1], 'month')] 
    names(data_1) <- c('y','x', 'month')
    data_2 <- data_transform[,c(resp_variable, det_variable_list[2], 'month')] 
    names(data_2) <- c('y','x', 'month')
    data_3 <- data_transform[,c(resp_variable, det_variable_list[3], 'month')] 
    
    names(data_3) <- c('y','x', 'month')
    data_4 <- data_transform[,c(resp_variable, det_variable_list[4], 'month')] 
    names(data_4) <- c('y','x', 'month')
    data_5 <- data_transform[,c(resp_variable, det_variable_list[5], 'month')] 
    names(data_5) <- c('y','x', 'month')
    range_r2m <- ggplot(data_1, aes(x,y, group = month)) +
      geom_point(aes(color = as.factor(month))) + 
      xlab(det_variable_list[1]) + 
      ylab(resp_variable) + 
      theme_bw() +
      theme(legend.position = "none")
    
    range_r5m <- ggplot(data_2, aes(x, y, group = month)) +
      geom_point(aes(color = as.factor(month))) + 
      xlab(det_variable_list[2]) + 
      ylab(resp_variable) + 
      theme_bw() +
      theme(legend.position = "none")
    range_r10m <- ggplot(data_3, aes(x,y, group = month)) +
      geom_point(aes(color = as.factor(month))) +
      xlab(det_variable_list[3]) + 
      ylab(resp_variable) + 
      theme_bw() +
      theme(legend.position = "none")
    range_r15m <- ggplot(data_4, aes(x,y, group = month)) +
      geom_point(aes(color = as.factor(month)))  + 
      xlab(det_variable_list[4]) + 
      ylab(resp_variable) + 
      theme_bw() +
      theme(legend.position = "none")
    range_r20m <- ggplot(data_5, aes(x,y, group = month)) +
      xlab(det_variable_list[5]) + 
      ylab(resp_variable) + 
      geom_point(aes(color = as.factor(month))) + 
      theme_bw() 
    
    legend <- get_legend(range_r20m)
    labeller <- ggdraw() + draw_label(label)
    plots <- plot_grid(range_r2m, range_r5m, range_r10m, range_r15m, range_r20m + theme(legend.position = "none"), legend)
    out_plots <- plot_grid(labeller, plots, ncol = 1, nrow=2, rel_heights= c(0.1, 1))
  } 
  
  else if(transform == F) { 
    data_det_vars <- data[, c(det_variable_list)]
    data_det_vars <- apply(data_det_vars, 2, as.numeric)
    
    data_vars <- data.frame(data[, c(resp_variable, 'Plotcode', 'month')], data_det_vars)
    data_vars_gather <- pivot_longer(data_vars, cols = det_variable_list, names_to ="Canopy_Radius", values_to = "Mean_Canopy_Height") 
    graph <- ggplot(data_vars_gather, aes(Mean_Canopy_Height, range_T1_m, group = as.factor(month))) + 
      geom_point(aes(color = as.factor(month))) + 
      theme_bw() + 
      facet_wrap(~Canopy_Radius)
    # #data_vars <- distinct(data_transform)
    # data_1a <- data_vars[,c(resp_variable, det_variable_list[1], 'month')] 
    # names(data_1a) <- c('y','x', 'month')
    # data_2a <- data_vars[,c(resp_variable, det_variable_list[2], 'month')] 
    # names(data_2a) <- c('y','x', 'month')
    # data_3a <- data_vars[,c(resp_variable, det_variable_list[3], 'month')] 
    # 
    # names(data_3a) <- c('y','x', 'month')
    # data_4a <- data_vars[,c(resp_variable, det_variable_list[4], 'month')] 
    # names(data_4a) <- c('y','x', 'month')
    # data_5a <- data_vars[,c(resp_variable, det_variable_list[5], 'month')] 
    # names(data_5a) <- c('y','x', 'month')
    # range_r2m <- ggplot(data_1a, aes(x,y, group = month)) +
    #   geom_point(aes(color = as.factor(month))) + 
    #   xlab(det_variable_list[1]) + 
    #   ylab(resp_variable) + 
    #   theme_bw() +
    #   theme(legend.position = "none")
    # 
    # range_r5m <- ggplot(data_2a, aes(x, y, group = month)) +
    #   geom_point(aes(color = as.factor(month))) + 
    #   xlab(det_variable_list[2]) + 
    #   ylab(resp_variable) + 
    #   theme_bw() +
    #   theme(legend.position = "none")
    # range_r10m <- ggplot(data_3a, aes(x,y, group = month)) +
    #   geom_point(aes(color = as.factor(month))) +
    #   xlab(det_variable_list[3]) + 
    #   ylab(resp_variable) + 
    #   theme_bw() +
    #   theme(legend.position = "none")
    # range_r15m <- ggplot(data_4a, aes(x,y, group = month)) +
    #   geom_point(aes(color = as.factor(month)))  + 
    #   xlab(det_variable_list[4]) + 
    #   ylab(resp_variable) + 
    #   theme_bw() +
    #   theme(legend.position = "none")
    # range_r20m <- ggplot(data_5a, aes(x,y, group = month)) +
    #   xlab(det_variable_list[5]) + 
    #   ylab(resp_variable) + 
    #   geom_point(aes(color = as.factor(month))) + 
    #   theme_bw() 
    #   
    # legend <- get_legend(range_r20m)
    # labeller <- ggdraw() + draw_label(label)
    # plots <- plot_grid(range_r2m, range_r5m, range_r10m, range_r15m, range_r20m + theme(legend.position = "none"), legend)
    # out_plots <- plot_grid(labeller, plots, ncol = 1, nrow=2, rel_heights= c(0.1, 1))
    }
    
}

Range_T_graphs <- plotter(climate_modeling, resp_variable = 'range_T1_m', det_variable =  "DAP_Canopy_Height", transform = T, label = "Mean Daily Range in Temperature by Month")
max_T_graphs <- plotter(climate_modeling, resp_variable =  'max_T1_m', det_variable = "DAP_Canopy_Height", transform = F, label = "Mean Maximum Daily Temperature by Month")
min_T_graphs <- plotter(climate_modeling, resp_variable = 'min_T1_m', det_variable = "DAP_Canopy_Height", transform = T, label = "Mean Minimum Daily Temperature by Month")

pdf(file = "D:/Data/SmithTripp/Gavin_Lake/Figures/SoilTemp_DataExpl.pdf", width = 14, height = 9)

Range_T_graphs

max_T_graphs

min_T_graphs

dev.off()

model_graphs <- function(data, model, y) {
  data$yhat.0 <- fitted(model, level = 0 ) #population averaged estimates
  data$yhat.1 <- fitted(model, level = 1) #plot level estimates
  data$resid.0 <- resid(model, level = 0) #estimate residuals 
  data$resid.1 <- resid(model, level = 1) # estimate the models final residuals (i.e. actual error in the model) 
  # get diagnostic plots
  lev1_residuals <- ggplot(data, aes(yhat.0, resid.0)) + geom_point() + ggtitle("Residual Plot, Population level") + theme_bw()
  fit_plot <- ggplot(data, aes(yhat.1, resid.1)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
  qnorm <- ggplot(data, aes(sample = resid.1)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
  hist <- ggplot(data, aes(resid.1)) + geom_density() + ggtitle("Error Distribution") +theme_bw()
  stats_plots <- plot_grid(lev1_residuals, fit_plot, qnorm, hist)
  return(stats_plots)
}

package <- function(name, model, graphs) { 
  packaged_data <- list(name, model, graphs, list(anova(model), summary(model)))
  return(packaged_data)
}
library(nlme)
lm_random_plots_graphs <- function(climate_modeling, resp_variable, det_variable) {
  det_variable_list <- names(data)[which(grepl(det_variable, names(data)))]
  data_det_vars <- data[, c(det_variable_list)]
  data_det_vars <- apply(data_det_vars, 2, as.numeric)
  
  data_vars <- data.frame(data[, c(resp_variable, 'Plotcode', 'month', 'plot')], data_det_vars)
  #data_vars <- distinct(data_transform)
  data_1a <- data_vars[,c(resp_variable, det_variable_list[1], 'month', 'plot')] 
  names(data_1a) <- c('y','x', 'month', 'plot')
  data_2a <- data_vars[,c(resp_variable, det_variable_list[2], 'month', 'plot')] 
  names(data_2a) <- c('y','x', 'month', 'plot')
  data_3a <- data_vars[,c(resp_variable, det_variable_list[3], 'month', 'plot')] 
  
  names(data_3a) <- c('y','x', 'month', 'plot')
  data_4a <- data_vars[,c(resp_variable, det_variable_list[4], 'month', 'plot')] 
  names(data_4a) <- c('y','x', 'month', 'plot')
  
  data_5a <- data_vars[,c(resp_variable, det_variable_list[5], 'month', 'plot')] 
  names(data_5a) <- c('y','x', 'month', 'plot')
  
  
   #write models
 lm1 <- lme(y~ x + month, data = data_1a, method = "REML", random = ~ 1 |as.factor(plot), na.action = na.exclude, correlation = corAR1(form = 1|month))
 lm2 <- lme(y~x +as.factor(month), data = data_2a, method = "REML", random = ~ 1 |as.factor(plot), na.action = na.exclude)
 lm3 <- lme(y~x +as.factor(month), data = data_3a, method = "REML", random = ~ 1 |as.factor(plot), na.action = na.exclude)
 
 lm4 <- lme(y~x +as.factor(month), data = data_4a, method = "REML", random = ~ 1 |as.factor(plot), na.action = na.exclude)
 lm5 <- lme(y~x +as.factor(month), data = data_5a, method = "REML", random = ~ 1 |as.factor(plot), na.action = na.exclude)
 
 #get model graphs 
 graphs_lm1 <- model_graphs(data_1a, lm1, y = y)
 graphs_lm2 <- model_graphs(data_2a, lm2, y = y)
 graphs_lm3 <- model_graphs(data_3a, lm3, y = y)
 graphs_lm4 <- model_graphs(data_4a, lm4, y = y)
 graphs_lm5 <- model_graphs(data_5a, lm5, y = y)
 

 
 range_1 <- package(det_variable_list[1], lm1, graphs_lm1)
 range_2 <- package(det_variable_list[2], lm2, graphs_lm2)
 range_3 <- package(det_variable_list[3], lm3, graphs_lm3)
 range_4 <- package(det_variable_list[4], lm4, graphs_lm4)
 range_5 <- package(det_variable_list[5], lm5, graphs_lm5)
 
 return(list(range_1, range_2, range_3, range_4, range_5))
}

lm_random_plots_graphs_log <- function(climate_modeling, resp_variable, det_variable) {
  det_variable_list <- names(data)[which(grepl(det_variable, names(data)))]
  data_det_vars <- data[, c(det_variable_list)]
  data_det_vars <- apply(data_det_vars, 2, as.numeric) + 1
  data_det_vars_log <- apply(data_det_vars, 2, log)
  data_transform <- data.frame(data[, c(resp_variable, 'Plotcode', 'month')], data_det_vars_log)
  data_transform <- distinct(data_transform)
  data_1a <- data_transform[,c(resp_variable, det_variable_list[1], 'month')] 
  names(data_1a) <- c('y','x', 'month')
  data_2a <- data_transform[,c(resp_variable, det_variable_list[2], 'month')] 
  names(data_2a) <- c('y','x', 'month')
  data_3a <- data_transform[,c(resp_variable, det_variable_list[3], 'month')] 
  
  names(data_3a) <- c('y','x', 'month')
  data_4a <- data_transform[,c(resp_variable, det_variable_list[4], 'month')] 
  names(data_4a) <- c('y','x', 'month')
  data_5a <- data_transform[,c(resp_variable, det_variable_list[5], 'month')] 
  names(data_5a) <- c('y','x', 'month')
  
  
  #write models
  lm1 <- lme(y~x +as.factor(month), data = data_1a, method = "REML", random = ~ 1 |as.factor(plot), na.action = na.exclude)
  lm2 <- lme(y~x +as.factor(month), data = data_2a, method = "REML", random = ~ 1 |as.factor(plot), na.action = na.exclude)
  lm3 <- lme(y~x +as.factor(month), data = data_3a, method = "REML", random = ~ 1 |as.factor(plot), na.action = na.exclude)
  
  lm4 <- lme(y~x +as.factor(month), data = data_4a, method = "REML", random = ~ 1 |as.factor(plot), na.action = na.exclude)
  lm5 <- lme(y~x +as.factor(month), data = data_5a, method = "REML", random = ~ 1 |as.factor(plot), na.action = na.exclude)
  
  #get model graphs 
  graphs_lm1 <- model_graphs(data_1a, lm1, y = y)
  graphs_lm2 <- model_graphs(data_2a, lm2, y = y)
  graphs_lm3 <- model_graphs(data_3a, lm3, y = y)
  graphs_lm4 <- model_graphs(data_4a, lm4, y = y)
  graphs_lm5 <- model_graphs(data_5a, lm5, y = y)
  
  
  
  range_1 <- package(det_variable_list[1], lm1, graphs_lm1)
  range_2 <- package(det_variable_list[2], lm2, graphs_lm2)
  range_3 <- package(det_variable_list[3], lm3, graphs_lm3)
  range_4 <- package(det_variable_list[4], lm4, graphs_lm4)
  range_5 <- package(det_variable_list[5], lm5, graphs_lm5)
  
  return(list(range_1, range_2, range_3, range_4, range_5))
}


range_T_mods <- lm_random_plots_graphs(climate_modeling, resp_variable = 'range_T1_m', det_variable = 'DAP_Canopy_Height')
range_T_mods_log <- lm_random_plots_graphs(climate_modeling, resp_variable = 'range_T1_m', det_variable = 'DAP_Canopy_Height')

