## Creates first simple models of microclimate 

### reads in data submitted to soiltemp database that is clipped in the microclimate_logger_parsing - removing erroneous values 
library(tidyverse)
library(cowplot)
seq <- seq(from =1 , to = 10, by = 1)
fs_seq <- paste0("fs", seq)

climate_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/Microclimate_Measurements/Microclimate_TMS_UserSoils_Nov-25-20.csv")
climate_data <- climate_data %>% 
  filter(DateTime_GMT > "2020-05-15" & DateTime_GMT < "2020-10-10") %>% 
  mutate(DateTime = as.Date(DateTime_GMT), 
         Plotcode = as.factor(Plotcode), 
         DateTime_GMT = lubridate::ymd_hms(DateTime_GMT),
         Hour =lubridate::hour(DateTime_GMT), 
         DateTime_Hour = lubridate::ymd_h(paste(DateTime, Hour)))



meta_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/CA_ST_SoilTempData/CA_ST_MetaData.csv", header = T)

filter(meta_data, plot == "2")

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


# Data Spread (for committee) ---------------------------------------------

climate_sums <-climate_data %>% mutate(yday = lubridate::yday(DateTime_GMT)) %>% 
  group_by(Plotcode, yday) %>% summarise(month = month, max_daily_T1 = max(T1), max_daily_T2 = max(T2),max_daily_T3 = max(T2)) %>% 
  group_by(Plotcode, month) %>% summarise(mean_max_m_T1 = mean(max_daily_T1, na.rm = T),
                                          mean_max_m_T2 = mean(max_daily_T2, na.rm = T),
                                          mean_max_m_T3 = mean(max_daily_T3, na.rm = T)) %>% 
  pivot_longer(cols = contains("mean_max"), names_to = "Temp_Sensor", values_to = "Mean_max_m") %>%
  left_join(meta_data[,c("Plotcode", "plot")])
quick_data_fix <- climate_data %>% 
  filter(Plotcode %in% 
           filter(climate_sums, Mean_max_m>30 & Temp_Sensor == "mean_max_m_T1")$Plotcode) %>% 
  filter(DateTime_GMT > "2020-07-04")

climate_data_fix <- climate_data %>%  filter(!Plotcode %in% 
                                               c("CA_ST_fs78", "CA_ST_fs81", "CA_ST_fs85" ,"CA_ST_fs86", "CA_ST_fs96", "CA_ST_fs96")) %>% 
  full_join(quick_data_fix)

climate_sums <- climate_data_fix %>% mutate(yday = lubridate::yday(DateTime_GMT)) %>% 
  group_by(Plotcode, yday) %>% summarise(month = month, max_daily_T1 = max(T1), max_daily_T2 = max(T2),max_daily_T3 = max(T2)) %>% 
  group_by(Plotcode, month) %>% summarise("Soil (-8 cm)" = mean(max_daily_T1, na.rm = T),
                                          "Surface (0 cm)" = mean(max_daily_T2, na.rm = T),
                                          "Near-Surface (+15 cm)" = mean(max_daily_T3, na.rm = T)) %>% 
  pivot_longer(cols = contains("cm"), names_to = "Temp_Sensor", values_to = "Mean_max_m") %>%
  left_join(meta_data[,c("Plotcode", "plot")])
Temperature_plots <- ggplot(climate_sums %>% filter(Temp_Sensor %in% c("Soil (-8 cm)", "Near-Surface (+15 cm)")), aes(month, Mean_max_m, group = month))+ 
  geom_boxplot(alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  geom_point(aes(color = as.factor(plot)), alpha = 0.4, position = 'jitter', size = 1)+
  facet_wrap(~Temp_Sensor, ncol = 2) +
  ylab("Average Daily Maximum Temperature °C") + 
  labs(color = "Plot") +
  xlab("Month")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") +
  theme_bw(base_size = 15)


# develop microclimate summary data  --------------------------------------

climate_modeling <- climate_data_fix %>% 
  group_by(Plotcode, DateTime) %>% 
  mutate(range_T1_d = max(T1) - min(T1), 
         max_T1_d = max(T1), 
         min_T1_d = min(T1))%>% 
  group_by(Plotcode, month) %>% 
  summarise(range_T1_m = mean(range_T1_d, na.rm = T), 
            max_T1_m = mean(max_T1_d, na.rm = T),
            min_T1_m = mean(min_T1_d, na.rm = T)) %>% 
  left_join(meta_data, by = 'Plotcode') %>% 
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
    data_transform <- data.frame(data[, c(resp_variable, 'Plotcode', 'month', 'plot')], data_det_vars_log)
    levels(data_transform$plot) <- fs_seq
    data_transform_gather <- data_transform %>% distinct() %>% pivot_longer(cols = det_variable_list, names_to = "Radius", values_to = "Value")
    names(data_transform_gather )[names(data_transform_gather) == resp_variable ] <- 'Y'
    graph <- ggplot(data_transform_gather, aes(Value, Y, group = as.factor(month))) + 
      geom_point(aes(color = as.factor(plot))) + 
      xlab(det_variable) + ylab(resp_variable) + ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
      theme_bw() + 
      facet_wrap(~Radius)
  } 
  
  else if(transform == F) { 
    data_det_vars <- data[, c(det_variable_list)]
    data_det_vars <- apply(data_det_vars, 2, as.numeric)
    
    data_vars <- data.frame(data[, c(resp_variable, 'Plotcode', 'month')], data_det_vars)
    names(data_vars) <- c('resp_variable', 'Plotcode', 'month', det_variable_list)
    data_vars_gather <- pivot_longer(data_vars, cols = det_variable_list, names_to ="Canopy_Radius", values_to = "Mean_Canopy_Height") 
    graph <- ggplot(data_vars_gather, aes(Mean_Canopy_Height, resp_variable, group = as.factor(month))) + 
      geom_point(aes(color = as.factor(month))) + 
      ylab(paste(resp_variable)) +
      facet_wrap(~Canopy_Radius) +
      theme_bw()
      
    }
    
}

Range_T_graphs <- plotter(climate_modeling, resp_variable = 'range_T1_m', det_variable =  "elevation", transform = F, label = "Mean Daily Range in Temperature by Month")
max_T_graphs <- plotter(climate_modeling, resp_variable =  'max_T1_m', det_variable = "DAP_Canopy_Height", transform = F, label = "Mean Maximum Daily Temperature by Month")
min_T_graphs <- plotter(climate_modeling, resp_variable = 'min_T1_m', det_variable = "DAP_Canopy_Height", transform = T, label = "Mean Minimum Daily Temperature by Month")

pdf(file = "D:/Data/SmithTripp/Gavin_Lake/Figures/SoilTemp_DataExpl.pdf", width = 14, height = 9)

Range_T_graphs

max_T_graphs

min_T_graphs

dev.off()

climate_modeling_annual <- climate_data_fix %>% 
  pivot_longer(cols = c("T1", "T2", "T3"), names_to = 'sensor', values_to = 'Temp_C') %>% 
  group_by(Plotcode, sensor, DateTime) %>% 
  mutate(range_d = max(Temp_C) - min(Temp_C), 
         min_d = min(Temp_C), 
         max_d = max(Temp_C))%>% 
  group_by(Plotcode, sensor) %>% 
  summarise(range_T = mean(range_d, na.rm = T), 
            max_T = mean(max_d, na.rm = T),
            min_T = mean(min_d, na.rm = T), 
            mean_T = mean(Temp_C, na.rm = T)) %>% 
  left_join(meta_data, by = 'Plotcode')
climate_modeling_Ht_long <-climate_modeling_annual %>% pivot_longer(cols = contains("Dap_Canopy_Height"), names_to ="Canopy_Radius", values_to = "Mean_Canopy_Height") 
ggplot(climate_modeling_Ht_long %>% filter(Canopy_Radius %in% c("DAP_Canopy_Height_r2m", "DAP_Canopy_Height_r15m")),
       aes(Mean_Canopy_Height,mean_T)) + 
  geom_point(aes(color = as.factor(plot))) + 
  #geom_smooth(method = "lm", alpha = 0.2, color = "grey") +
  ylab("Average Daily Maximum Temperature °C") + 
  xlab("Average Canopy Height (m)") +
  labs(color = "Plot") + 
  facet_grid(Canopy_Radius ~ sensor) +   theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +theme_bw(base_size = 15)


library(cowplot)
library(lme4)
library(lmtest)
library(ggeffects)



# Annual models  ----------------------------------------------------------
soil_annual_lm <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m + elevation_r15m + (1|plot), 
                       data = filter(climate_modeling_annual, sensor == "T1"))
plot(soil_annual_lm)

#Nested model 1 - remove elevation because should be dealt with T1 
soil_annual_lm_n1b <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m + (1|plot), 
                          data = filter(climate_modeling_annual, sensor == "T1"))
#Nested model 2 - remove aspect
soil_annual_lm_n1a <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m+ (1|plot), 
                          data = filter(climate_modeling_annual, sensor == "T1"))
#remove all variables not Canopy Height 
soil_annual_lm_n2 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (1|plot), 
                          data = filter(climate_modeling_annual, sensor == "T1"))
#remove random plot 
soil_annual_lm_n3  <- lm(mean_T ~ DAP_Canopy_Height_r15m,
                          data = filter(climate_modeling_annual, sensor == "T1"))
anova(soil_annual_lm, soil_annual_lm_n1a, soil_annual_lm_n1b, soil_annual_lm_n2, soil_annual_lm_n3)

#### plot annual soil temperature models 
avg_values <-climate_modeling_annual %>% subset(!is.na(mean_T)) %>% group_by(plot) %>% summarize(aspect.r15m_avg = mean(aspect.r15m), 
                                                                                    elevation.r15m_avg = mean(elevation_r15m), 
                                                                                    DAP_Canopy_Ht_r15m = mean(DAP_Canopy_Height_r15m))
##Create_Average Models for plotting 
soil_temp_avgs <- climate_modeling_annual %>% filter(sensor == "T1") %>% left_join(avg_values) %>% subset(!is.na(mean_T))
soil_annual_lm_plot <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_avg + elevation.r15m_avg + (1|plot), 
                            data = soil_temp_avgs)
soil_annual_lm_plot_elev <- lmer(mean_T ~ DAP_Canopy_Ht_r15m + aspect.r15m_avg + elevation_r15m + (1|plot), data = soil_temp_avgs)
soil_annual_lm_plot_asp <- lmer(mean_T ~ DAP_Canopy_Ht_r15m + aspect.r15m + elevation.r15m_avg + (1|plot), data = soil_temp_avgs)

soil_temp_avgs$DAP_Canopy_mod_r15m <- predict(soil_annual_lm_plot)
soil_temp_avgs$Elev_mod_r15m <- predict(soil_annual_lm_plot_elev)
soil_temp_avgs$Asp_mod_r15m <- predict(soil_annual_lm_plot_asp)

# soil_temp_avgs <- soil_temp_avgs %>% pivot_longer(cols = contains('r15m'), names_to = c("Model"),
#                                                   values_to = c("Pred")) %>%
#   left_join(climate_modeling_annual %>% 
#               select("Plotcode", "aspect.r15m", "elevation_r15m", "DAP_Canopy_Height_r15m") %>% 
#               pivot_longer(cols = contains("r15m"),  names_to = "Variable", values_to = "Value") %>% distinct()) %>% distinct() %>% 
#   mutate(var = case_when(Model == "DAP_Canopy_mod_r15m" ~ "Mean Canopy Height (m)", 
#                          Model == "Elev_mod_r15m" ~ "Elevation (m)", 
#                          Model == "Asp_mod_r15m" ~ "Aspect (Rad)"))

## plot average soil temperature models 


  ## Model selection 1 <- determine best resolution for modeling temperature range
  ## start with canopy height because seems promising for fitting 
  ## goal is a random slope by plot and a random intercept by month (unlikely that relationship would change month to month)
  slope <- lm(range_T1_m ~ DAP_Canopy_Height_r15m, data = climate_modeling)
lm.errvar1 <- lm(resid(slope) ~ log(DAP_Canopy_Height_r15m), data = climate_modeling)
  wt1 <- (1/(exp(lm.errvar1$fitted.values))) # error variatances 
  slope_wt <- lm(range_T1_m ~ DAP_Canopy_Height_r15m, data  = climate_modeling, weights = wt1)
  mixed.ranslope_r2m <- lmer(range_T1_m ~ DAP_Canopy_Height_r2m + (1 + DAP_Canopy_Height_r2m|plot) + (1|month), data = climate_modeling) #control=lmerControl(optimizer="bobyqa",                                                                                                                                         #optCtrl=list(maxfun=2e5))) 
  mixed.ranslope_r5m <- lmer(range_T1_m ~ DAP_Canopy_Height_r5m + (1 + DAP_Canopy_Height_r5m|plot) + (1|month), data = climate_modeling) 
  mixed.ranslope_r10m <- lmer(range_T1_m ~ DAP_Canopy_Height_r10m + (1 + DAP_Canopy_Height_r10m|plot) + (1|month), data = climate_modeling) 
  mixed.ranslope_r15m <- lmer(range_T1_m ~ DAP_Canopy_Height_r15m + (1 + DAP_Canopy_Height_r15m|plot) + (1|month), data = climate_modeling) 
  mixed.ranslope_r20m <- lmer(range_T1_m ~ DAP_Canopy_Height_r20m + (1 + DAP_Canopy_Height_r20m|plot) + (1|month), data = climate_modeling) 
  
  ### these models all get convergence warnings - I could put in an optimizer? 
  
  
  AIC(mixed.ranslope_r2m, mixed.ranslope_r5m, mixed.ranslope_r10m, mixed.ranslope_r15m, mixed.ranslope_r20m)
  anova(mixed.ranslope_r2m, mixed.ranslope_r5m, mixed.ranslope_r10m, mixed.ranslope_r15m, mixed.ranslope_r20m)
  
  
  
  
  ## model with month as a random intercept is best 
  #add predicted values to the dataset 
  complete_cases <- function(query, d) {
    cols <- c(grep(query, names(d)))
    i <- complete.cases(d[,cols]) #subset to present values for query of interest
    d <- d[i,]
    return(d)
  }
  climate_models_complete <- complete_cases('range_T1_m', climate_modeling)
  climate_models_complete$predicted_r15m_random <- predict(mixed.ranslope_r15m)
  climate_models_complete$residual <- climate_models_complete$range_T1_m - climate_models_complete$predicted_r15m_random
  ## check model assumptions 
  fit_plot <- ggplot(climate_models_complete %>% filter(range_T1_m < 10), aes(predicted_r15m_random, residual)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
  qnorm <- ggplot(climate_models_complete, aes(sample = residual)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
  hist <- ggplot(climate_models_complete, aes(residual)) + geom_density() + ggtitle("Error Distribution") +theme_bw() +ylim(0,1)
  
  ## model fails assumptions - logging variables to deal with heteroskacidity 
  climate_models_complete$log_range_T1_m <- log(climate_models_complete$range_T1_m)
  climate_models_complete$log_DAP_Canopy_Height_r15m <- log(climate_models_complete$DAP_Canopy_Cover_r15m)
  
  ## fit new model with logged variables 
  mixed.ranslope_r15m_log <- lmer(log_range_T1_m ~ log_DAP_Canopy_Height_r15m + (1 + log_DAP_Canopy_Height_r15m|plot) + (1|month), data = climate_models_complete)
  
  #compare to a more parsimounious model 
  ##Currently 15 meter seems like the best resolution - so we'll move on modeling from there 
  mixed.ranslope_r15m_lognomonth <- lmer(log_range_T1_m ~ log_DAP_Canopy_Height_r15m + (1 + log_DAP_Canopy_Height_r15m|plot),data = climate_models_complete) 
  mixed.ranslope_r15m_lognoplots <- lmer(log_range_T1_m ~ log_DAP_Canopy_Height_r15m + (1|month),data = climate_models_complete) 
  
  anova(mixed.ranslope_r15m_lognomonth, mixed.ranslope_r15m_log, mixed.ranslope_r15m_lognoplots)
  ##random intercepts by month is a better fit 
  
  
  climate_models_complete$log_predicted<- predict(mixed.ranslope_r15m_log)
  climate_models_complete$log_residual<- climate_models_complete$range_T1_m - climate_models_complete$log_predicted_r15m_random
  ##See if this fixes model assumptions 
  fit_plot <- ggplot(climate_models_complete, aes(log_predicted_r15m_random, log_residual)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
  qnorm <- ggplot(climate_models_complete, aes(sample = log_residual)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
  hist <- ggplot(climate_models_complete, aes(log_residual)) + geom_density() + ggtitle("Error Distribution") +theme_bw() +ylim(0,1)
  
  
  graph_mod <- ggplot(climate_models_complete) + 
    geom_point(aes(log_DAP_Canopy_Height_r15m, log_range_T1_m)) + 
    geom_line(aes(log_DAP_Canopy_Height_r15m, log_predicted, group = month, color = as.factor(month))) +
    facet_wrap(~plot) +
    labs(color = "month") +
    ylim(0,15) +
    theme_bw()
  
  

# Committee meeting graphs  -----------------------------------------------
graph_data <- climate_modeling %>% ungroup %>% dplyr::select(ends_with('m'), 'plot', 'Plotcode') %>% dplyr::select(-ends_with("_m")) %>% dplyr::distinct()
graph_data$plot <- as.factor(graph_data$plot)
  levels(graph_data$plot) <- seq(1,10, by = 1)
canopy_height <- ggplot(graph_data, aes(as.factor(plot), 
                                        as.numeric(DAP_Canopy_Height_r15m), 
                                        group = as.factor(plot), color = as.factor(plot))) + 
    geom_boxplot(aes(fill = as.factor(plot)),alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
    geom_point(alpha = 0.7, sive = 1.2, position = 'jitter')+
    ylab("Mean Canopy Height (m) 15 m radius") + 
    labs(color = "Plot") +
    xlab("Plot")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot()

canopy_cover <- ggplot(graph_data, aes(plot, as.numeric(DAP_Canopy_Cover_r15m), group = plot, color = plot)) + 
  geom_boxplot(aes(fill = plot), alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  geom_point(alpha = 0.7, sive = 1.2, position = 'jitter')+
  ylab("Mean Canopy Cover (m) 15 m radius") + 
  labs(color = "Plot") +
  xlab("Plot")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot()
  
slope <- ggplot(graph_data, aes(plot, as.numeric(slope.r15m), group = plot, color = plot)) + 
  geom_boxplot(aes(fill = plot),alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  geom_point(alpha = 0.7, sive = 1.2, position = 'jitter', outlier.color = NA)+
  ylab("Slope (m) 15 m radius") + 
  labs(color = "Plot") +
  xlab("Plot")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot()

elevation <- graph_data %>%
  group_by(plot) %>%
  mutate(elevation_r15m_n = as.numeric(elevation_r15m),
         elev_filtered = case_when(elevation_r15m_n - quantile(elevation_r15m_n)[4] > 1.5*IQR(elevation_r15m_n) ~ NA_real_,
                                  quantile(elevation_r15m_n)[2] - elevation_r15m_n > 1.5*IQR(elevation_r15m_n) ~ NA_real_,
                                  TRUE ~ elevation_r15m_n)) %>% ggplot( aes(group = plot, color = plot)) + 
  geom_boxplot(aes(plot, elevation_r15m_n, fill = plot),alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  geom_point(aes(plot, elevation_r15m_n), alpha = 0.7, sive = 1.2, position = 'jitter')+
  ylab("Elevation (m) 15 m radius") + 
  labs(color = "Plot") +
  xlab("Plot")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot()
solar_rad <- ggplot(graph_data, aes(plot, Sol_rad.r.15.m, group = plot, color = plot)) + 
  geom_boxplot(aes(fill = plot),alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  geom_point(alpha = 0.7, sive = 1.2, position = 'jitter', outlier.color = NA)+
  ylab("Solar Radiation (W/m^2)") + 
  labs(color = "Plot") +
  xlab("Plot")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot()

  


plot_grid(elevation, slope, ncol = 1, nrow = 2)
plot_grid(canopy_cover, canopy_height, ncol = 1)



# Checking for co-linearity among model predictors  -----------------------

canopy_elev <- ggplot(graph_data, aes(as.numeric(DAP_Canopy_Height_r15m),as.numeric(elevation_r15m), group = plot)) + 
  geom_point(aes(color = plot)) +
  stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
  
  ylab("Elevation (m)") + 
  labs(color = "Plot") +
  xlab("Mean Canopy Height (m)")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot()

canopy_elev_lm <- lme(DAP_Canopy_Height_r15m ~ elevation_r15m, random = ~elevation_r15m|plot, data = graph_data, method = "REML")
canopy_elev_control <- lme(range_T1_m ~ DAP_Canopy_Height_r15m + elevation_r15m + DAP_Canopy_Height_r15m*elevation_r15m, data =climate_models_complete, random = ~ 1|plot, weights = varPower())
canopy_DAP <- lme(range_T1_m ~ DAP_Canopy_Height_r15m, data =climate_models_complete, random = ~ 1|plot, weights = varPower())
par(mfrow = c(2,2))
plot(canopy_elev_lm)
summary(canopy_elev_lm)
canopy_aspect <- ggplot(graph_data, aes(as.numeric(DAP_Canopy_Height_r15m),(as.numeric(aspect.r10m)*180/pi))) + 
  geom_point(aes(color = plot)) +
  stat_smooth(method = "lm", alpha = 0.2) +
  ylab("Aspect (°)") + 
  labs(color = "Plot") +
  xlab("Mean Canopy Height (m)")+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot() +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)), legend.position = "none")

canopy_aspect_lm <- lm(as.numeric(DAP_Canopy_Height_r15m)~ as.numeric(aspect.r15m), data = graph_data)

aspect_eleve <- ggplot(graph_data, aes(as.numeric(elevation_r15m), as.numeric(aspect.r15m)*180/pi, group = plot)) + 
  geom_point(aes(color = plot)) +
  stat_smooth(aes(color = plot),method = "lm", alpha = 0.2) + 
  ylab("Aspect (°)") + 
  labs(color = "Plot") +
  xlab("Elevation (m)")+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot() +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)), legend.position = "none")
aspect_elev_lm <- lme(as.numeric(aspect.r15m)~ as.numeric(elevation_r15m), random = 
                        ~elevation_r15m | plot,  data = graph_data)
plot(aspect_elev_lm)
summary(aspect_elev_lm)
canopy_elev_leg <- get_legend(canopy_elev)
plot_grid(canopy_elev + theme(legend.position = "none"), canopy_aspect, aspect_eleve, canopy_elev_leg, rel_widths = c(1,1,1,0.15), nrow = 1)



