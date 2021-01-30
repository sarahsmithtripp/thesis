## Creates first simple models of microclimate 

### reads in data submitted to soiltemp database that is clipped in the microclimate_logger_parsing - removing erroneous values 
library(tidyverse)

library(cowplot)
library(lme4)
library(MuMIn)
library(lme4)
library(lmtest)
library(ggeffects)


seq <- seq(from =1 , to = 10, by = 1)

climate_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/Microclimate_Measurements/Microclimate_TMS_UserSoils_Dec-08-20.csv")
climate_data <- climate_data %>% 
  filter(DateTime_GMT > "2020-05-15" & DateTime_GMT < "2020-10-10") %>% 
  mutate(DateTime = as.Date(DateTime_GMT), 
         Plotcode = as.factor(Plotcode), 
         DateTime_GMT = lubridate::ymd_hms(DateTime_GMT),
         Hour =lubridate::hour(DateTime_GMT), 
         DateTime_Hour = lubridate::ymd_h(paste(DateTime, Hour)))



meta_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/CA_ST_SoilTempData/CA_ST_MetaData.csv", header = T)
meta_data_asp <- dplyr::select(meta_data, contains("aspect"),'Plotcode')
meta_data_asp[,1:5] <- apply(meta_data_asp[,1:5], 2, function(x) { cos((pi/4) * (x*180)/pi) } )
names(meta_data_asp) <- c(paste0(names(meta_data_asp)[1:5], "_con"), "Plotcode")
meta_data <- left_join(meta_data, meta_data_asp)

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

#pdf(file = "D:/Data/SmithTripp/Gavin_Lake/Figures/SoilTemp_DataExpl.pdf", width = 14, height = 9)

Range_T_graphs

max_T_graphs

min_T_graphs

#dev.off()
climate_modeling_sm <- climate_data_fix %>% group_by(Plotcode) %>% summarize(mean_sm_ct = mean(SM_Count), 
                                                                             mean_sm_vol = mean(vol_sm))
climate_modeling_annual <- climate_data_fix %>% 
  group_by(Plotcode) %>% 
  mutate(mean_sm = mean(vol_sm, na.rm = F)) %>%
  pivot_longer(cols = c("T1", "T2", "T3"), names_to = 'sensor', values_to = 'Temp_C') %>% 
  group_by(Plotcode, sensor, DateTime) %>% 
  mutate(range_d = max(Temp_C) - min(Temp_C), 
         min_d = min(Temp_C), 
         max_d = max(Temp_C))%>% 
  group_by(Plotcode, sensor, mean_sm) %>% 
  summarise(
    #mean_sm = mean(vol_sm, na.rm = T),
    range_T = mean(range_d, na.rm = T), 
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

climate_modeling_Cov_long <-climate_modeling_annual %>% pivot_longer(cols = contains("Dap_Canopy_Cov"), names_to ="Canopy_Radius", values_to = "Cover") 
lm_r_square_full <- function(data, resp_variable, fixed) {
  data$plot <- as.factor(data$plot)
  levels(data$plot) <- seq
  #data_vars <- distinct(data_transform)
  data_1 <- data %>% dplyr::select(paste(resp_variable), 'plot', 'Plotcode','DAP_Canopy_Height_r2m', 'Elevation', 'aspect.r10m_con')
  names(data_1) <- c('sensor','y', 'plot', 'Plotcode', 'Canopy_ht', 'Canopy_cov', 'elevation', 'aspect_con')
  data_2 <- data %>%dplyr::select(paste(resp_variable), 'plot', 'Plotcode','DAP_Canopy_Height_r5m', 'Elevation', 'aspect.r10m_con')
  names(data_2) <- c('sensor', 'y','plot', 'Plotcode', 'Canopy_ht', 'Canopy_cov', 'elevation', 'aspect_con')
  data_3 <- data %>% dplyr::select(paste(resp_variable), 'plot', 'Plotcode','DAP_Canopy_Height_r10m', 'Elevation', 'aspect.r10m')
  names(data_3) <- c('sensor', 'y', 'plot', 'Plotcode', 'Canopy_ht', 'Canopy_cov', 'elevation', 'aspect_con')
  data_4 <- data %>% dplyr::select(paste(resp_variable), 'plot', 'Plotcode','DAP_Canopy_Height_r15m', 'Elevation', 'aspect.r10m')
  names(data_4) <- c('sensor', 'y', 'plot', 'Plotcode', 'Canopy_ht', 'Canopy_cov', 'elevation', 'aspect_con')
  data_5 <- data %>% dplyr::select(paste(resp_variable), 'plot', 'Plotcode','DAP_Canopy_Height_r20m', 'Elevation', 'aspect.r10m')
  names(data_5) <- c('sensor','y', 'plot', 'Plotcode', 'Canopy_ht', 'Canopy_cov', 'elevation', 'aspect_con')
  
  
  #write models
  lm1 <- lmer(y ~ Canopy_ht + aspect_con + elevation + (1|plot), data = data_1)
  lm2 <- lmer(y ~ Canopy_ht + aspect_con + elevation + (1|plot), data = data_2)
  lm3 <- lmer(y ~ Canopy_ht + aspect_con + elevation + (1|plot), data = data_3)
  lm4 <- lmer(y ~ Canopy_ht + aspect_con + elevation + (1|plot), data = data_4)
  lm5 <- lmer(y ~ Canopy_ht + aspect_con + elevation + (1|plot), data = data_5)
  
  
  # gather R^2
  if(fixed == T) {
    #return fixed correlation
  r_squar <- c(r.squaredGLMM(lm1)[1], r.squaredGLMM(lm2)[1], r.squaredGLMM(lm3)[1], 
               r.squaredGLMM(lm4)[1], r.squaredGLMM(lm5)[1])

  }
  else if (fixed == F) {
    
    #return full model correlation
  r_squar <-  c(r.squaredGLMM(lm1)[2], r.squaredGLMM(lm2)[2], r.squaredGLMM(lm3)[2], 
                    r.squaredGLMM(lm4)[2], r.squaredGLMM(lm5)[2])
  }
}
lm_r_square <- function(data, resp_variable, det_variable) {
  det_variable_list <- names(data)[which(grepl(det_variable, names(data)))]
  data_det_vars <- data[, c(det_variable_list)]
  data_det_vars <- apply(data_det_vars, 2, as.numeric)
  
  data_vars <- data.frame(data[, c(resp_variable, 'Plotcode','plot')], data_det_vars)
  data_vars$plot <- as.factor(data_vars$plot)
  levels(data_vars$plot) <- seq
  #data_vars <- distinct(data_transform)
  data_1a <- data_vars[,c(resp_variable, det_variable_list[1], 'plot', 'Plotcode')] 
  names(data_1a) <- c('y','x', 'plot', 'Plotcode')
  data_2a <- data_vars[,c(resp_variable, det_variable_list[2],'plot', 'Plotcode')] 
  names(data_2a) <- c('y','x', 'plot', 'Plotcode')
  data_3a <- data_vars[,c(resp_variable, det_variable_list[3], 'plot', 'Plotcode')] 
  names(data_3a) <- c('y','x', 'plot', 'Plotcode')
  data_4a <- data_vars[,c(resp_variable, det_variable_list[4],'plot', 'Plotcode')] 
  names(data_4a) <- c('y','x', 'plot', 'Plotcode')
  data_5a <- data_vars[,c(resp_variable, det_variable_list[5], 'plot', 'Plotcode')] 
  names(data_5a) <- c('y','x', 'plot', 'Plotcode')
  
  
  #write models
  lm1 <- lm(y~x, data = data_1a, na.action = na.exclude)
  lm2 <- lm(y~x, data = data_2a, na.action = na.exclude)
  lm3 <- lm(y~x, data = data_3a, na.action = na.exclude)
  lm4 <- lm(y~x,data = data_4a, na.action = na.exclude)
  lm5 <- lm(y~x, data = data_5a, na.action = na.exclude)
  
  # gather R^2
  r_squar <- c(summary(lm1)$adj.r.squared, summary(lm2)$adj.r.squared, summary(lm3)$adj.r.squared, 
               summary(lm4)$adj.r.squared, summary(lm5)$adj.r.squared)
  return(r_squar)
}

soil_models_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T1"), resp_variable = 
                             "mean_T", det_variable = "DAP_Canopy_Height")
surface_models_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T2"), resp_variable = 
                                      "mean_T", det_variable = "DAP_Canopy_Height")
near_surface_models_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T3"), resp_variable = 
                                           "mean_T", det_variable = "DAP_Canopy_Height")
soil_moisture_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T3"), resp_variable = 
                                           "mean_sm", det_variable = "DAP_Canopy_Height")
# soil_models <- lm_r_square_full(data = climate_modeling_annual %>% filter(sensor == "T1"), resp_variable = 
#                              "mean_T", fixed = F)
# surface_models <- lm_r_square_full(data = climate_modeling_annual %>% filter(sensor == "T2"), resp_variable = 
#                                 "mean_T", fixed = F) 
# near_surface_models <- lm_r_square_full(data = climate_modeling_annual %>% filter(sensor == "T3"), resp_variable = 
#                                      "mean_T", fixed = F) 

model_R_all <- data.frame(Radius = c(2,5,10,15,20), 
                      Soil = soil_models_fixed, 
                      Surface = surface_models_fixed, Near_Surface =  near_surface_models_fixed, 
                      Soil_Moisture = soil_moisture_fixed)  %>% 
  pivot_longer(c = c('Soil', 'Surface', 'Near_Surface', "Soil_Moisture"), names_to = "Model", values_to = "adj.r") %>% 
  mutate(Model = factor(Model, levels =c('Soil', 'Surface', 'Near_Surface', "Soil_Moisture")))

# model_R_fixed <- data.frame(Radius = c(2,5,10,15,20), 
#                           Soil = soil_models_fixed, 
#                           Surface = surface_models_fixed, Near_Surface =  near_surface_models_fixed)  %>% 
#   pivot_longer(c = c('Soil', 'Surface', 'Near_Surface'), names_to = "Model", values_to = "Fixed") %>% 
#   left_join(model_R_all) %>%
#   pivot_longer(c = c('Random + Fixed', 'Fixed'), names_to = "R_squar_type", values_to = "adj.r")
#                       
# variation_fixed <- ggplot(model_R_fixed, aes(Radius, adj.r)) + geom_point(aes(color = Model), size = 3) +
#   geom_line(aes(color = Model), linetype = "dashed") +
#   facet_wrap(~ R_squar_type) +
#   ylab(paste('Adjusted Model R\u00b2')) + xlab("Radius (m)") + labs(color = "Model") +
#   scale_color_brewer(palette = "Dark2") +theme_bw(base_size = 20)
variation_fixed <- ggplot(model_R_all, aes(Radius, adj.r)) + geom_point(aes(color = Model), size = 4) +
  geom_line(aes(color = Model), size = 1, alpha = 0.6, linetype = "dashed") +  #, linetype = "dashed")  +
  ylab(paste('Adjusted Model R\u00b2')) + xlab("Radius (m)") + labs(color = "Annual Variable") +
  scale_color_brewer(palette = "Set1", labels =  c("Soil (C°)", "Surface (C°)", "Near Surface (C°)", "Soil Moisture (vol %)")) +theme_bw(base_size = 20) 

variation_fixed



facet_grid <- ggplot(climate_modeling_Ht_long %>% filter(Canopy_Radius %in% c("DAP_Canopy_Height_r2m", "DAP_Canopy_Height_r15m") & sensor == "T1"),
       aes(Mean_Canopy_Height, max_T)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.2, color = "grey") +
  ylab("Average Daily Max Temperature °C") + 
  xlab("Average Canopy Height (m)") +
  facet_grid(Canopy_Radius ~ sensor) +   theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +theme_bw(base_size = 15)

plot_grid(facet_grid, variation_fixed)


## soil moisture ### 

sm_r_square <- lm_r_square_full(data = climate_modeling_annual %>% filter(sensor == "T3"), resp_variable = 
                                                    "mean_sm", fixed = F) 

sm_r_square_fixed <- lm_r_square_full(data = climate_modeling_annual %>% filter(sensor == "T3"), resp_variable = 
                             "mean_sm", fixed = T) 




# Annual models  ----------------------------------------------------------

# Temperature -------------------------------------------------------------


# soil temperatur ---------------------------------------------------------



#soil_annual_lm_complex <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (DAP_Canopy_Height_r15m | plot) + (1|plot), data = filter(climate_modeling_annual, sensor == "T1"))
soil_annual_lm <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + Elevation + (1|plot), 
                       data = filter(climate_modeling_annual, sensor == "T1"))
test1 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + Elevation + (1 + DAP_Canopy_Height_r15m|plot) + (1 + aspect.r10m_con|plot) + (1 + Elevation | plot), 
                       data = filter(climate_modeling_annual, sensor == "T1"))
test2 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + Elevation + (1 + DAP_Canopy_Height_r15m|plot) + (1 + aspect.r10m_con|plot),
              data = filter(climate_modeling_annual, sensor == "T1"))
test3 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + Elevation + (1 + DAP_Canopy_Height_r15m|plot),
              data = filter(climate_modeling_annual, sensor == "T1"))
test4 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + Elevation + (1 + aspect.r10m_con|plot),
              data = filter(climate_modeling_annual, sensor == "T1"))
test5 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + Elevation + (1 + Elevation|plot),
                       data = filter(climate_modeling_annual, sensor == "T1"))
test6 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + Elevation + (1 + Elevation|plot) + (1 + aspect.r10m_con|plot),
              data = filter(climate_modeling_annual, sensor == "T1"))
test7 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + Elevation + (1 + Elevation|plot) + (1 + DAP_Canopy_Height_r15m|plot),
              data = filter(climate_modeling_annual, sensor == "T1"))
anova(test1, test2, test3, test4, test5, test6, test7, soil_annual_lm)
soil_annual_lm_cov <- lmer(mean_T ~ DAP_Canopy_Cover_r15m + aspect.r10m_con + Elevation + (1|plot), 
                       data = filter(climate_modeling_annual, sensor == "T1"))
ggplot(climate_modeling_annual, aes(DAP_Canopy_Height_r15m, DAP_Canopy_Cover_r15m, group = plot, color = as.factor(plot))) + 
  geom_point() + geom_smooth(method = "lm")
plot(soil_annual_lm)

#Nested model 1 - remove elevation because should be dealt with T1 
soil_annual_lm_n1b <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + (1|plot), 
                          data = filter(climate_modeling_annual, sensor == "T1"))
#Nested model 2 - remove aspect
soil_annual_lm_n1a <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation+ (1|plot), 
                          data = filter(climate_modeling_annual, sensor == "T1"))
#remove all variables not Canopy Height 
soil_annual_lm_n2 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (1|plot), 
                          data = filter(climate_modeling_annual, sensor == "T1"))
#remove random plot 
soil_annual_lm_n3  <- lm(mean_T ~ DAP_Canopy_Height_r15m,
                          data = filter(climate_modeling_annual, sensor == "T1"))
anova(soil_annual_lm, soil_annual_lm_n1a, soil_annual_lm_n1b, soil_annual_lm_n2, soil_annual_lm_n3)
summary(soil_annual_lm)
## soil_annaul_Lm_n1b is the best model 



# Surface Models  ---------------------------------------------------------

surface_annual_lm <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + Elevation + (1|plot), 
                       data = filter(climate_modeling_annual, sensor == "T2"))
surface_annual_lm_cov <- lmer(mean_T ~ DAP_Canopy_Cover_r15m + aspect.r10m_con + Elevation + (1|plot), 
                           data = filter(climate_modeling_annual, sensor == "T2"))
anova(surface_annual_lm, surface_annual_lm_cov)
ggplot(climate_modeling_annual, aes(DAP_Canopy_Height_r15m, DAP_Canopy_Cover_r15m, group = plot, color = plot)) + 
  geom_point() + geom_smooth(method = "lm")
plot(surface_annual_lm)

#Nested model 1 - remove elevation because should be dealt with T2 
surface_annual_lm_n1b <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + (1|plot), 
                           data = filter(climate_modeling_annual, sensor == "T2"))
#Nested model 2 - remove aspect
surface_annual_lm_n1a <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation+ (1|plot), 
                           data = filter(climate_modeling_annual, sensor == "T2"))
#remove all variables not Canopy Height 
surface_annual_lm_n2 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (1|plot), 
                          data = filter(climate_modeling_annual, sensor == "T2"))
#remove random plot 
surface_annual_lm_n3  <- lm(mean_T ~ DAP_Canopy_Height_r15m,
                         data = filter(climate_modeling_annual, sensor == "T2"))
anova(surface_annual_lm, surface_annual_lm_n1a, surface_annual_lm_n1b, surface_annual_lm_n2, surface_annual_lm_n3)
### best model is full model



# Near Surface  -----------------------------------------------------------


near_surface_annual_lm <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + Elevation + (1|plot), 
                          data = filter(climate_modeling_annual, sensor == "T3"))
near_surface_annual_lm_cov <- lmer(mean_T ~ DAP_Canopy_Cover_r15m + aspect.r10m_con + Elevation + (1|plot), 
                              data = filter(climate_modeling_annual, sensor == "T3"))
anova(near_surface_annual_lm, near_surface_annual_lm_cov)
ggplot(climate_modeling_annual, aes(DAP_Canopy_Height_r15m, DAP_Canopy_Cover_r15m, group = plot, color = plot)) + 
  geom_point() + geom_smooth(method = "lm")
plot(near_surface_annual_lm)

#Nested model 1 - remove elevation because should be dealt with T3 
near_surface_annual_lm_n1b <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con + (1|plot), 
                              data = filter(climate_modeling_annual, sensor == "T3"))
#Nested model 2 - remove aspect
near_surface_annual_lm_n1a <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation+ (1|plot), 
                              data = filter(climate_modeling_annual, sensor == "T3"))
#remove all variables not Canopy Height 
near_surface_annual_lm_n2 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (1|plot), 
                             data = filter(climate_modeling_annual, sensor == "T3"))
#remove random plot 
near_surface_annual_lm_n3  <- lm(mean_T ~ DAP_Canopy_Height_r15m,
                            data = filter(climate_modeling_annual, sensor == "T3"))
anova(near_surface_annual_lm, near_surface_annual_lm_n1a, near_surface_annual_lm_n1b, near_surface_annual_lm_n2, near_surface_annual_lm_n3)

### plot confidence intervales
model_confidence <- as.data.frame(rbind( 
                          cbind(confint(surface_annual_lm), rep("surface_model", 6)),
                          cbind(rbind(confint(soil_annual_lm_n1b), c("NA", "NA")), rep("soil_model", 6)),
                          cbind(confint(near_surface_annual_lm), rep("near_surface_model", 6))))
model_confidence$variable <- rep(rownames(model_confidence)[1:6], 3)
model_confidence_df <- model_confidence %>% mutate(lwr_est = as.numeric(.$`2.5 %`), 
                                                uppr_est = as.numeric(.$`97.5 %`),
                                                model = V3) %>% 
  pivot_longer(cols = ends_with("est"), names_to = "Interval", values_to = "Estimate")

model_confidence_df <- model_confidence_df[which(grepl("r15m", model_confidence_df$variable)),]


model_confidence_graph <- ggplot(model_confidence_df, #%>% filter(variable == "DAP_Canopy_Height_r15m"), 
                                 aes(model, Estimate)) + 
  geom_point(aes(color = model), size = 3) +geom_line(aes(color = model), size = 2) +  ylab("Slope Estimate") + xlab("") + labs(color = "Model") +
    scale_color_brewer(palette = "Dark2") + ylim(-0.5, 0.5) +
  facet_wrap(~variable) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 2) + 
  theme_bw(base_size = 20) + theme(legend.position = "bottom",axis.title.x=element_blank(),
                                                                                 axis.text.x=element_blank(),
                                                                                 axis.ticks.x=element_blank())
MuMIn::r.squaredGLMM(soil_annual_lm)




# Soil Moisture  ----------------------------------------------------------

soil_moist_annual_lm <- lmer(log(mean_sm) ~ DAP_Canopy_Height_r2m + aspect.r10m_con + Elevation + (1|plot), 
                               data = filter(climate_modeling_annual, sensor == "T3"))
soil_moist_annual_lm_cov <- lmer(mean_sm ~ DAP_Canopy_Cover_r3m + aspect.r10m_con + Elevation + (1|plot), 
                                   data = filter(climate_modeling_annual, sensor == "T3"))
anova(soil_moist_annual_lm, soil_moist_annual_lm_cov)

plot(soil_moist_annual_lm)

#Nested model 1 - remove elevation because should be dealt with T3 
soil_moist_annual_lm_n1b <- lmer(log(mean_sm) ~ DAP_Canopy_Height_r2m + aspect.r10m_con + (1|plot), 
                                   data = filter(climate_modeling_annual, sensor == "T3"))
#Nested model 2 - remove aspect
soil_moist_annual_lm_n1a <- lmer(log(mean_sm) ~ DAP_Canopy_Height_r2m + Elevation+ (1|plot), 
                                   data = filter(climate_modeling_annual, sensor == "T3"))
#remove all variables not Canopy Height 
soil_moist_annual_lm_n2 <- lmer(log(mean_sm) ~ DAP_Canopy_Height_r2m + (1|plot), 
                                  data = filter(climate_modeling_annual, sensor == "T3"))
#remove random plot 
soil_moist_annual_lm_n3  <- lm(log(mean_sm) ~ DAP_Canopy_Height_r2m,
                                 data = filter(climate_modeling_annual, sensor == "T3"))
anova(soil_moist_annual_lm, soil_moist_annual_lm_n1a, soil_moist_annual_lm_n1b, soil_moist_annual_lm_n2, soil_moist_annual_lm_n3)

MuMIn::r.squaredGLMM(soil_moist_annual_lm)

summary(soil_moist_annual_lm)



# Annual Model Graphs  ----------------------------------------------------

#### plot annual soil temperature models 

avg_values <-climate_modeling_annual %>% subset(!is.na(mean_T)) %>% group_by(plot) %>% summarize(#Plotcode = Plotcode,
#                                                                                                  DAP_Canopy_Height_r15m = DAP_Canopy_Height_r15m, 
#                                                                                                  sensor = sensor, 
#                                                                                                  mean_T = mean_T, 
                                                                                                 aspect.r10m_con_avg = mean(aspect.r10m_con), 
                                                                                                 elevation_avg = mean(Elevation), 
                                                                                                 aspect.r10m_con_avg = mean(aspect.r10m_con))
model_data <- function(data, sensor) {
  q <- which(grepl(paste0(sensor), data$sensor))
  data_mod <- data[q,]
  data_mod <- data_mod %>% subset(!is.na(mean_T)) %>% left_join(avg_values)
}
soil_model_dat <- model_data(climate_modeling_annual, sensor = "T1")
surface_model_dat <- model_data(climate_modeling_annual, "T2")
near_surface_model_dat <- model_data(climate_modeling_annual, "T3")

soil_annual_lm_avg <-   lmer(mean_T ~ DAP_Canopy_Height_r15m  + aspect.r10m_con_avg + elevation_avg+
                                 + (1|plot), data = soil_model_dat)
soil_model_dat$prediction <- predict(soil_annual_lm_avg)

surface_annual_lm_avg <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con_avg+ elevation_avg+ 
                                (1|plot), data = surface_model_dat)
surface_model_dat$prediction <- predict(surface_annual_lm_avg)
near_surface_annual_lm_avg <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r10m_con_avg + elevation_avg + 
                                 (1|plot), data = near_surface_model_dat)
near_surface_model_dat$prediction <- predict(near_surface_annual_lm_avg)

avg_values_predictions <- rbind(soil_model_dat, surface_model_dat, near_surface_model_dat)

avg_values_predictions <- avg_values_predictions %>% mutate(plot = as.factor(plot), 
         prediction = as.numeric(prediction)) %>%
  mutate(class = as.factor(case_when(sensor == "T1" ~ "Soil",
                           sensor == "T2" ~ "Surface", 
                           sensor == "T3" ~ "Near-Surface")))


levels(avg_values_predictions$plot) <- seq(1,10, by =1)
avg_values_predictions$class <- factor(avg_values_predictions$class,levels = c("Soil", "Surface", "Near-Surface"))
#levels(avg_values_predictions$sensor) <- c("Soil", "Surface", "Near-Surface")
annual_model_graph <- ggplot(avg_values_predictions, aes(group = plot)) + 
  geom_point(aes(DAP_Canopy_Height_r15m, mean_T, color = plot)) + 
  geom_line(aes(DAP_Canopy_Height_r15m, prediction, color = plot)) + 
  ylab("Mean Annual Temperature °C") + 
  labs(color = "Plot") +
  xlab("Canopy Height (m)")+
  facet_wrap(~class) +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  theme_bw(base_size = 15) #+ theme(legend.position = "none")
legend <- get_legend(annual_model_graph)
annual_model_graph <- annual_model_graph + theme(legend.position = "none")


## plot annual soil moisture 
soil_moist_ann_dat <- filter(climate_modeling_annual, sensor == "T1") %>% subset(!is.na(mean_sm)) %>% left_join(avg_values) %>% mutate(plot = as.factor(plot), 
                                                                                                                                       class = "Soil Moisture")
levels(soil_moist_ann_dat$plot) <- seq(1,10, by =1)
soil_moist_annual_lm_avg <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + aspect.r10m_con_avg + scale(elevation_avg) + (1|plot), data = soil_moist_ann_dat)
soil_moist_ann_dat$prediction <- as.numeric(predict(soil_moist_annual_lm_avg))
sm_annual_model_graph <- ggplot(soil_moist_ann_dat, aes(group = plot)) + 
  geom_point(aes(DAP_Canopy_Height_r2m, mean_sm, color = plot)) + 
  geom_line(aes(DAP_Canopy_Height_r2m, prediction, color = plot),size = 1) + 
  ylab("Mean Annual Soil Moisture (% vol)") + 
  labs(color = "Plot") + facet_wrap(~class) +
  xlab("Canopy Height (m)")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
 ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
 ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  theme_bw(base_size = 15) + theme(legend.position = "none")
sm_annual_model_graph

plot_grid(annual_model_graph, sm_annual_model_graph, legend, rel_widths = c(3, 1, 0.2), nrow = 1)

full_annual_mods <- soil_moist_ann_dat %>% 
  mutate(class = "Soil Moisture") %>% 
  rbind(avg_values_predictions) %>% 
  mutate(variable = case_when(class == "Near-Surface" | class == "Soil" | 
                                class == "Surface" ~ mean_T, 
                            class == "Soil Moisture" ~ mean_sm))
full_annual_model_graph <- ggplot(full_annual_mods, aes(group = plot)) + 
  geom_point(aes(DAP_Canopy_Height_r15m, variable, color = plot)) + 
  geom_line(aes(DAP_Canopy_Height_r15m, prediction, color = plot)) + 
  ylab("Mean Annual Temperature °C") + 
  labs(color = "Plot") +
  xlab("Canopy Height (m)")+
  facet_wrap(~class, scales = "free") +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  theme_bw(base_size = 15) #+ theme(legend.position = "none")
full_annual_model_graph
# Monthly Models  ---------------------------------------------------------

climate_modeling_month <- climate_data_fix %>% 
  pivot_longer(cols = c("T1", "T2", "T3"), names_to = 'sensor', values_to = 'Temp_C') %>% 
  group_by(Plotcode, month, sensor) %>%
  summarize(mean_T = mean(Temp_C, na.rm = T), 
            mean_sm = mean(vol_sm, na.rm = T)) %>%
  left_join(meta_data)


# soil monthly temperature  -----------------------------------------------
soil_dat <- climate_modeling_month %>% filter(sensor == "T1") %>% subset(!is.na(mean_T))
monthly_mods <- function(data, sensor, variable) {
  q <- which(grepl(paste0(sensor), data$sensor))
  data_sensor <- data[q,]
  var <- which(grepl(variable, names(data_sensor)))
  names(data_sensor[, var]) <- "variable"
  months <- split(data_sensor, f = data_sensor$month)
  m5 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'5')
  m6 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'6')
  m7 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'7')
  m8 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'8')
  m9 <- lmer(mean_T~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'9')
  return(list(m5,m6,m7,m8,m9))
}

get_slope <- function(model) { 
  name <- as.character(summary(model)$call)
  name <- str_sub(name[3], -2, -2)
  store <- data.frame(summary(model)$coefficients) 
  store$month <- name
  store$coeff <- rownames(store)
  model_confidence <- as.data.frame(confint(model))
  model_confidence$variable <- rep(rownames(model_confidence)[1:6])
  model_confidence_df <- model_confidence %>% mutate(lwr_est = as.numeric(.$`2.5 %`), 
                                                     uppr_est = as.numeric(.$`97.5 %`),
                                                     model = name, 
                                                     coeff = variable) %>% 
    pivot_longer(cols = ends_with("est"), names_to = "Interval", values_to = "Conf_Estimate")
  model_details <- left_join(store, model_confidence_df)
  return(model_details)
}

soil_month_models <- monthly_mods(climate_modeling_month, sensor = "T1", variable = "mean_T")
soil_coeff <- lapply(soil_month_models, get_slope) 
soil_coeff_df <- bind_rows(soil_coeff) %>% pivot_wider(names_from = coeff, values_from = Estimate)

soil_coeff_df$Estimate_all <-apply(soil_coeff_df[, c("(Intercept)", "DAP_Canopy_Height_r15m", "Elevation", "aspect.r10m_con")], 1,
                                   function(i){ paste(na.omit(i), collapse = " ") })
soil_coeff_df$Estimate_all <- as.numeric(soil_coeff_df$Estimate_all)
soil_coeff_df$class <- "Soil °C"
soil <- ggplot(soil_coeff_df) + 
  geom_point(aes(month, Estimate_all, color = month), size = 3) +geom_line(aes(month, Conf_Estimate, color = model), size = 1.5, linetype = "dotted") + 
  ylab("Confidence Interval for Model Estimate") + 
  geom_text(aes(month, Estimate_all, label = round(Estimate_all,2)), nudge_x = 0.35) + labs(color = "Month") +
  xlab("") + labs(color = "Month") +
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(~variable, scales = "free")+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  theme_bw(base_size = 20) + theme(legend.position = "none")

# Surface Monthly Temperature -----------------------------------------------------

surface_month_models <- monthly_mods(climate_modeling_month, sensor = "T2", variable = "mean_T")
surface_coeff <- lapply(surface_month_models, get_slope) 
surface_coeff_df <- bind_rows(surface_coeff) %>% pivot_wider(names_from = coeff, values_from = Estimate)


surface_coeff_df$Estimate_all <-apply(surface_coeff_df[, c("(Intercept)", "DAP_Canopy_Height_r15m", "Elevation", "aspect.r10m_con")], 1,
                                   function(i){ paste(na.omit(i), collapse = " ") })
surface_coeff_df$Estimate_all <- as.numeric(surface_coeff_df$Estimate_all)


surface_coeff_df$class <- "Surface °C"
surface <- ggplot(surface_coeff_df) + 
  geom_point(aes(month, Estimate_all, color = month), size = 3) +geom_line(aes(month, Conf_Estimate, color = model), size = 1.5, linetype = "dotted") + 
  ylab("Confidence Interval for Model Estimate") +
geom_text(aes(month, Estimate_all, label = round(Estimate_all,2)), nudge_x = 0.35) + labs(color = "Month") + labs(color = "Month") +
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(~variable, scales = "free")+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  theme_bw(base_size = 20) + theme(legend.position = "none")

# Near Surface Monthly Temperature ----------------------------------------

near_surface_month_models <- monthly_mods(climate_modeling_month, sensor = "T3", variable = "mean_T")
hist((climate_modeling_month %>% filter(sensor == "T3" & month == "7"))$mean_T)
near_surface_coeff <- lapply(near_surface_month_models, get_slope) 
near_surface_coeff_df <- bind_rows(near_surface_coeff) %>% pivot_wider(names_from = coeff, values_from = Estimate)

near_surface_coeff_df$Estimate_all <-apply(near_surface_coeff_df[, c("(Intercept)", "DAP_Canopy_Height_r15m", "Elevation", "aspect.r10m_con")], 1,
                                      function(i){ paste(na.omit(i), collapse = " ") })
near_surface_coeff_df$Estimate_all <- as.numeric(near_surface_coeff_df$Estimate_all)
near_surface_coeff_df$model_type <- "near_surface"
near_surface_coeff_df$class <- "Near Surface °C"
near_surface <- ggplot(near_surface_coeff_df) + 
  geom_point(aes(month, Estimate_all, color = month), size = 3) + geom_line(aes(month, Conf_Estimate, color = model), size = 1.5, linetype = "dotted") + 
  ylab("Confidence Interval for Model Estimate") + xlab("") + labs(color = "Month") +
  geom_text(aes(month, Estimate_all, label = round(Estimate_all,2)), nudge_x = 0.35) + labs(color = "Month") +
  scale_color_brewer(palette = "Dark2") + 
  facet_wrap(~variable, scales = "free")+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  theme_bw(base_size = 20) + theme(legend.position = "none")


# Soil Moisture Monthly  --------------------------------------------------
monthly_mods_moist <- function(data, sensor, variable, scaled) {
  q <- which(grepl(paste0(sensor), data$sensor))
  data_sensor <- data[q,]
  var <- which(grepl(variable, names(data_sensor)))
  names(data_sensor[, var]) <- "variable"

  if(scaled == T) { # build models from z scores of the data rather than actual data itself 
    data_scaled <- data.frame(data_sensor, scale(data_sensor$Elevation), scale(data_sensor$DAP_Canopy_Height_r2m), scale(data_sensor$aspect.r10m_con))
    data_scaled$DAP_Canopy_Height_r5m <- data_scaled$scale.data_sensor.DAP_Canopy_Height_r2m. ## Change to 2 meters here 
    data_scaled$elevation_r5m <- data_scaled$scale.data_sensor.Elevation ## change to elevation here 
    data_scaled$aspect.r10m_con <- data_scaled$scale.data_sensor.aspect.r10m_con.                         
    months <- split(data_scaled, f = data_scaled$month)
    m5 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r10m_con + (1|plot), data = months$'5')
    m6 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r10m_con + (1|plot), data = months$'6')
    m7 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r10m_con + (1|plot), data = months$'7')
    m8 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r10m_con + (1|plot), data = months$'8')
    m9 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r10m_con + (1|plot), data = months$'9')
    store_mods <- list(m5,m6,m7,m8,m9)
  }
else {
  months <- split(data_sensor, f = data_sensor$month)
  m5 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r10m_con + (1|plot), data = months$'5')
  m6 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r10m_con + (1|plot), data = months$'6')
  m7 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r10m_con + (1|plot), data = months$'7')
  m8 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r10m_con + (1|plot), data = months$'8')
  m9 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r10m_con + (1|plot), data = months$'9')
  store_mods <- list(m5,m6,m7,m8,m9)
}
}

soil_moist_month_models <- monthly_mods_moist(climate_modeling_month, sensor = "T1", variable = "mean_sm", scaled = T)
for(i in 1:length(soil_moist_month_models)) {
  print(i)
  print(plot(soil_moist_month_models[[i]]))
}

soil_moist_coeff <- lapply(soil_moist_month_models, get_slope) 
soil_moist_coeff_df <- bind_rows(soil_moist_coeff) %>% pivot_wider(names_from = coeff, values_from = Estimate)

m7 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + (1|plot), data = filter(climate_modeling_month, sensor == "T1" & month == "5"))

ggplot(climate_modeling_month %>% filter(month == "7"), aes(DAP_Canopy_Height_r5m, mean_sm)) + 
  geom_point() +
   stat_smooth(method = "lm")

soil_moist_coeff_df$Estimate_all <-apply(soil_moist_coeff_df[, c("(Intercept)", "DAP_Canopy_Height_r5m", "aspect.r10m_con", "elevation_r5m")], 1,
                                      function(i){ paste(na.omit(i), collapse = " ") })
soil_moist_coeff_df$Estimate_all <- as.numeric(soil_moist_coeff_df$Estimate_all)
soil_moist_coeff_df$class <- "Soil Moisture vol %"

soil_moist <- ggplot(soil_moist_coeff_df) + 
  geom_point(aes(month, Estimate_all, color = month), size = 3) +geom_line(aes(month, Conf_Estimate, color = model), size = 1.5, linetype = "dotted") + 
  ylab("Confidence Interval for Model Estimate") + xlab("") + labs(color = "Month") +
  #scale_color_brewer(palette = "Dark2") +
  facet_wrap(~variable, scales = "free")+
  ggtitle("Soil Moisture Models By Month") +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  theme_bw(base_size = 20) + theme(legend.position = "none",axis.title.x=element_blank(),
                                   axis.text.x=element_blank(),
                                   axis.ticks.x=element_blank())
DAP_data <- soil_moist_coeff_df %>% subset(!is.na(DAP_Canopy_Height_r15m)) %>% 
  select(c("month", "Conf_Estimate", "DAP_Canopy_Height_r15m", "variable"))
ggplot(DAP_data, aes(color = month)) + 
         geom_point(aes(month, DAP_Canopy_Height_r15m)) + 
  geom_line(aes(month, Conf_Estimate)) + 
  ylab("Confidence Interval for Model Estimate") + xlab("") + labs(color = "Month") +
  #scale_color_brewer(palette = "Dark2") + ylim(-0.01, 0.01) +
  ggtitle("Soil Moisture Models By Month") +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  theme_bw(base_size = 20) + theme(legend.position = "bottom",axis.title.x=element_blank(),
                                   axis.text.x=element_blank(),
                                   axis.ticks.x=element_blank())


# DAP Slope by Month  -----------------------------------------------------
all_month_models_DAP <- full_join(subset(soil_moist_coeff_df, !is.na(DAP_Canopy_Height_r5m)), 
                                  subset(soil_coeff_df, !is.na(DAP_Canopy_Height_r15m))) %>%
  full_join(subset(surface_coeff_df, !is.na(DAP_Canopy_Height_r15m))) %>%  full_join(subset(near_surface_coeff_df, !is.na(DAP_Canopy_Height_r15m))) %>% 
  mutate(month_ = lubridate::month(lubridate::as_date(case_when(month == "5" ~ "05/01/2021", 
                            month == "6" ~ "06/01/2021", 
                            month == "7" ~ "07/01/2021", 
                            month == "8" ~ "08/01/2021", 
                            month == "9" ~ "09/01/2021"), format = "%m/%d/%y"), label = T))

DAP_models_by_month <- ggplot(all_month_models_DAP) + 
  geom_point(aes(month_, Estimate_all), size = 3) +geom_line(aes(month_, Conf_Estimate), size = 1) + 
  ylab("Confidence Interval and Estimate for Model") + 
  geom_text(aes(month_, Estimate_all, label = round(Estimate_all,2)), nudge_x = 0.35) + labs(color = "Month") +
  scale_color_brewer(palette = "Dark2") + xlab("") +
  facet_wrap(~class, scales = "free")+ #ylim(c(-0.29, 0.019)) +
  ggtitle("Estimates for Canopy Height (m) as a fixed effect") +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  theme_bw(base_size = 20) + theme(legend.position = "bottom")
DAP_models_by_month


##facet_grid of all models for SI 
all_month_models <- full_join(soil_moist_coeff_df, soil_coeff_df) %>%
  full_join(surface_coeff_df) %>%  full_join(near_surface_coeff_df) %>% 
  mutate(variable_simp = case_when(variable == "DAP_Canopy_Height_r5m" | variable == "DAP_Canopy_Height_r15m" ~ "Canopy Height",
                                   variable == "elevation_r5m" | variable == "Elevation" ~ "Elevation", 
                                   variable == "aspect.r10m_con" |variable == "aspect.r10m_con" ~ "Aspect", 
                                   variable == "(Intercept)" ~"Intercept"), 
         month_ = lubridate::month(lubridate::as_date(case_when(month == "5" ~ "05/01/2021", 
                                                                       month == "6" ~ "06/01/2021", 
                                                                       month == "7" ~ "07/01/2021", 
                                                                       month == "8" ~ "08/01/2021", 
                                                                       month == "9" ~ "09/01/2021"), format = "%m/%d/%y"), label = T))

models_by_month <- ggplot(all_month_models) + 
  geom_point(aes(month_, Estimate_all), size = 3) +geom_line(aes(month_, Conf_Estimate), size = 1.5, linetype = "dotted") + 
  ylab("Confidence Interval and Estimate for Model") + 
  geom_text(aes(month_, Estimate_all, label = round(Estimate_all,2)), nudge_x = 0.35) + labs(color = "Month") +
  scale_color_brewer(palette = "Dark2") +
  facet_grid(cols = vars(class), rows = vars(variable_simp), scales = "free_y")+
  ggtitle("Estimates for Canopy Height (m) as a fixed effect") +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  theme_bw(base_size = 20) + theme(legend.position = "bottom")
models_by_month



# Hypothesis Grap h  ------------------------------------------------------

make_data <- data.frame(value = c(rep(-1, 6), rep(1, 2)), 
                        conf_estimate =c(rep(c(-1.5, -0.5), 3), c(1.5, 0.5)), 
                        interval = as.factor(rep(c("upp", "lwr"), 4)), 
                        variable = c("Soil", "Soil", "Surface","Surface", "Near Surface", "Near Surface", 
                        "Soil Moisture", "Soil Moisture")) %>% 
  mutate(variable_factor = fct_relevel(variable, "Soil Moisture", after = 3))

hypothesis_graph <- ggplot(make_data) + 
  geom_point(aes(variable_factor, value), size = 3) +geom_line(aes(variable_factor, conf_estimate), size = 1) + 
  ylab(expression(paste(beta, " of Canopy Impact"))) + 
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  xlab("") +
  theme_bw(base_size = 25) + theme(legend.position = "bottom")
hypothesis_graph

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
  mutate(Elevation_n = as.numeric(Elevation),
         elev_filtered = case_when(Elevation_n - quantile(Elevation_n)[4] > 1.5*IQR(Elevation_n) ~ NA_real_,
                                  quantile(Elevation_n)[2] - Elevation_n > 1.5*IQR(Elevation_n) ~ NA_real_,
                                  TRUE ~ Elevation_n)) %>% ggplot( aes(group = plot, color = plot)) + 
  geom_boxplot(aes(plot, Elevation_n, fill = plot),alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  geom_point(aes(plot, Elevation_n), alpha = 0.7, sive = 1.2, position = 'jitter')+
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


# Daily Modeling (FINALLY!!!)  --------------------------------------------
setwd("D:/Data/SmithTripp/Gavin_Lake/Model_Development/Daily_Model_Plots")

climate_modeling_day <- climate_data_fix %>%
  group_by(Plotcode, Day) %>% 
  mutate(max_sm = max(vol_sm), 
         range_sm = max(vol_sm)) %>%
  pivot_longer(cols = c("T1", "T2", "T3"), names_to = 'sensor', values_to = 'Temp_C') %>% 
  group_by(Plotcode, sensor, DateTime, max_sm, range_sm) %>% 
  summarize(max_T = max(Temp_C), 
         range_T = max(Temp_C) - min(Temp_C)) %>% 
  left_join(meta_data %>% select(-c("X.1", "X"))) %>% 
  mutate(DOY = lubridate::yday(DateTime), 
         sensor = as.factor(sensor)) %>% 
  group_by(sensor) %>% 
  filter(near(max_T, mean(max_T,na.rm = T), tol = 4*sd(max_T, na.rm =T))) #clip values more than 4 standard deviations from the mean, because likely erroneous


ggplot(climate_modeling_day, aes(color = sensor)) + 
  geom_density(aes(max_T))

ggplot(climate_modeling_day, aes(color = sensor)) + 
  geom_density(aes(range_T))

#temperature modeling by day 
#general model, T = fixed variables + logger (random) + plot(random)
max_soil_day_lm1<- lmer(max_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|Plotcode) + (1|plot),
                             data = filter(climate_modeling_day, sensor == "T1"),
                             control = lmerControl(optimizer = "Nelder_Mead"))
max_soil_day_lm1_log <- lmer(log(max_T) ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|Plotcode) + (1|plot),
                         data = filter(climate_modeling_day, sensor == "T1"), control = lmerControl(optimizer = "Nelder_Mead"))
tic()
jpeg(filename= "max_T_soil.jpeg")
plot(max_soil_day_lm1)
dev.off()
toc()
jpeg(filename = "log_max_T_soil.jpeg")
plot(max_soil_day_lm1_log)
dev.off()

max_soil_day_lmn2a_log <- lmer(log(max_T) ~ DAP_Canopy_Height_r15m + Elevation + (1|Plotcode) + (1|plot),
                             data = filter(climate_modeling_day, sensor == "T1"), control = lmerControl(optimizer = "Nelder_Mead"))
max_soil_day_lmn2b_log <- lmer(log(max_T) ~ DAP_Canopy_Height_r15m + aspect.r10m_con + (1|Plotcode) + (1|plot),
                               data = filter(climate_modeling_day, sensor == "T1"), control = lmerControl(optimizer = "Nelder_Mead"))

range_soil_day_lm1 <- lmer(log(range_T) ~ DAP_Canopy_Height_r15m + Elevation + aspect.r15m + (1|Plotcode) + (1|plot),
                           data = filter(climate_modeling_day, sensor == "T1"), 
                           control = lmerControl(optimizer = "Nelder_Mead"))


max_near_surface_day_lm1 <- lmer(max_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r15m + (1|Plotcode) + (1|plot), data = filter(climate_modeling_day, sensor == "T3"))
range_near_surface_day_lm1 <- lmer(max_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r15m + (1|Plotcode) + (1|plot), data = filter(climate_modeling_day, sensor == "T3"))
plot(max_soil_day_lm1)
plot(max_near_surface_day_lm1)



# Shh GAMM models..  ------------------------------------------------------

get_slope_random <- function(model) { 
  name <- as.character(summary(model)$call)
  name <- str_sub(name[3], -4, -3)
  store <- data.frame(summary(model)$coefficients) 
  store$month <- name
  store$coeff <- rownames(store)
  model_confidence <- as.data.frame(confint(model))
  model_confidence$variable <- rep(rownames(model_confidence)[1:7])
  model_confidence_df <- model_confidence %>% mutate(lwr_est = as.numeric(.$`2.5 %`), 
                                                     uppr_est = as.numeric(.$`97.5 %`),
                                                     model = name, 
                                                     coeff = variable) %>% 
    pivot_longer(cols = ends_with("est"), names_to = "Interval", values_to = "Conf_Estimate")
  model_details <- left_join(store, model_confidence_df)
  return(model_details)
}


#get model coefficients for daily models 
#log to deal with heteroskadicity 
lm1 <- lmer(log(max_T) ~ scale(DAP_Canopy_Height_r15m) + scale(Elevation) + scale(aspect.r15m) + (1|Plotcode) + (1|plot),
            data = filter(climate_modeling_day, sensor == "T1"))
lm1a <- lmer(log(max_T) ~ scale(DAP_Canopy_Height_r15m) + scale(Elevation) + scale(aspect.r15m) + (1+ DAP_Canopy_Height_r15m|Plotcode) + (1|plot),
            data = filter(climate_modeling_day, sensor == "T1"), control = lmerControl(optimize = "Nelder_Mead"))
anova(lm1, lm1a)
climate_modeling_day_graphs <- filter(climate_modeling_day, sensor == "T1") %>% subset(!is.na(max_T)) %>% mutate(resid =resid(lm1), 
                                                                                                                 predicted = predict(lm1), 
                                                                                                                 resid_lma = resid(lm1a))
ggplot(climate_modeling_day_graphs) + 
  geom_point(aes(predicted, resid))
ggplot(climate_modeling_day_graphs) + 
  geom_point(aes(DateTime, resid))

Canopy <- ggplot(climate_modeling_day_graphs) + 
  geom_point(aes(scale(DAP_Canopy_Height_r15m),max_T, color = DateTime)) + theme(legend.position = 
                                                                                 "none")
elevation <-  ggplot(climate_modeling_day_graphs) + 
  geom_point(aes(scale(Elevation),max_T, color = DateTime)) + theme(legend.position = "none")
  
slope <-  ggplot(climate_modeling_day_graphs) + 
  geom_point(aes(scale(aspect.r10m_con),max_T, color = DateTime)) 

plot_grid(Canopy, elevation, slope, nrow = 1)

## Fit in nlme so can add wieghts
library(nlme)

lme1_soil <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 + plot|Plotcode, 
            data = filter(climate_modeling_day, sensor == "T1"), correlation = corAR1(form = ~ DateTime), method = "ML")
lme2_soil <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 |Plotcode, 
            data = filter(climate_modeling_day, sensor == "T1"), correlation = corAR1(form = ~ DateTime), method = "ML")
lme3_soil <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + aspect.r10m_con + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 |Plotcode, 
                 data = filter(climate_modeling_day, sensor == "T1"), correlation = corAR1(form = ~ DateTime), method = "ML")
lme4_soil <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 |Plotcode, 
                 data = filter(climate_modeling_day, sensor == "T1"), correlation = corAR1(form = ~ DateTime), method = "ML")

anova(lme1_soil, lme2_soil, lme3_soil, lme4_soil)
## lme3_soil is best 

lme1_surface <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 + plot|Plotcode, 
                 data = filter(climate_modeling_day, sensor == "T2"), correlation = corAR1(form = ~ DateTime), method = "ML")
lme2_surface <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 |Plotcode, 
                 data = filter(climate_modeling_day, sensor == "T2"), correlation = corAR1(form = ~ DateTime), method = "ML")
lme3_surface <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + aspect.r10m_con + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 |Plotcode, 
                 data = filter(climate_modeling_day, sensor == "T2"), correlation = corAR1(form = ~ DateTime), method = "ML")
lme4_surface <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 |Plotcode, 
                 data = filter(climate_modeling_day, sensor == "T2"), correlation = corAR1(form = ~ DateTime), method = "ML")

anova(lme1_surface, lme2_surface, lme3_surface, lme4_surface)
## lme4_surface is best 

lme1_near_surface <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 + plot|Plotcode, 
                    data = filter(climate_modeling_day, sensor == "T3"), correlation = corAR1(form = ~ DateTime), method = "ML")
lme2_near_surface <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 |Plotcode, 
                    data = filter(climate_modeling_day, sensor == "T3"), correlation = corAR1(form = ~ DateTime), method = "ML")
lme3_near_surface <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + aspect.r10m_con + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 |Plotcode, 
                    data = filter(climate_modeling_day, sensor == "T3"), correlation = corAR1(form = ~ DateTime), method = "ML")
lme4_near_surface <- lme(log(max_T) ~ DAP_Canopy_Height_r15m + sin(0.0172*DOY) + cos(0.0172*DOY), random = ~1 |Plotcode, 
                    data = filter(climate_modeling_day, sensor == "T3"), correlation = corAR1(form = ~ DateTime), method = "ML")

anova(lme1_near_surface, lme2_near_surface, lme3_near_surface, lme4_near_surface)
## lmer4_near_surface is best
split_joinr <- function(data, sensor, formula, dependent) { 
  q <- which(grepl(paste0(sensor), data$sensor))
  data_sub <- data[q,]
  q_na <- which(complete.cases(data_sub[,paste0(dependent)]))
  data_sub <- data_sub[q_na,]
  data_sub$prediction <- predict(formula)
  data_sub$residual <- resid(formula)
  return(data_sub)
}
formula_ls <- list(lme3_soil, lme4_surface, lme4_near_surface)
sensor_ls <- c("T1", "T2", "T3")
dependent <- c("max_T")
model_out_ls <- list()
climate_day_ls <- climate_modeling_day %>% group_split(sensor)

for(i in 1:3){ 
  model_out_ls[[i]] <- split_joinr(data = climate_day_ls[[i]], 
    sensor = sensor_ls[i],
    formula = formula_ls[[i]], 
    dependent = dependent
  )
}
climate_day_models <- bind_rows(model_out_ls)

library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 87
mycolors <- colorRampPalette(rainbow(8, "Set2"))(nb.cols)

ggplot(climate_day_models) + 
  geom_point(aes(DateTime, log(max_T), color = Plotcode)) + 
  geom_line(aes(DateTime, prediction, color = Plotcode)) + 
  scale_fill_manual(values = rainbow(87)) + 
  facet_wrap(~ sensor)


lm2 <- lmer(log(max_T) ~ scale(DAP_Canopy_Height_r15m) + scale(Elevation) + scale(aspect.r15m) + (1|Plotcode) + (1|plot),
            data = filter(climate_modeling_day, sensor == "T2"))
lm3 <- lmer(log(max_T) ~ scale(DAP_Canopy_Height_r15m) + scale(Elevation) + scale(aspect.r15m) + (1|Plotcode) + (1|plot),
            data = filter(climate_modeling_day, sensor == "T3"))
max_t_mods <- list(lm1,lm2, lm3)
max_t_coeff <- lapply(max_t_mods, get_slope_random)

lm1 <- lmer(log(range_T) ~ scale(DAP_Canopy_Height_r15m) + scale(Elevation) + scale(aspect.r15m) + (1|Plotcode) + (1|plot),
            data = filter(climate_modeling_day, sensor == "T1"))
lm2 <- lmer(log(range_T) ~ scale(DAP_Canopy_Height_r15m) + scale(Elevation) + scale(aspect.r15m) + (1|Plotcode) + (1|plot),
            data = filter(climate_modeling_day, sensor == "T2"))
lm3 <- lmer(log(range_T) ~ scale(DAP_Canopy_Height_r15m) + scale(Elevation) + scale(aspect.r15m) + (1|Plotcode) + (1|plot),
            data = filter(climate_modeling_day, sensor == "T3"))
range_t_mods <- list(lm1,lm2,lm3)
range_t_coeff <- lapply(range_t_mods, get_slope_random)

 
soil_moist_coeff_df <- bind_rows(soil_moist_coeff) %>% pivot_wider(names_from = coeff, values_from = Estimate)

m7 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + (1|plot), data = filter(climate_modeling_month, sensor == "T1" & month == "5"))

ggplot(climate_modeling_month %>% filter(month == "7"), aes(DAP_Canopy_Height_r5m, mean_sm)) + 
  geom_point() +
  stat_smooth(method = "lm")

soil_moist_coeff_df$Estimate_all <-apply(soil_moist_coeff_df[, c("(Intercept)", "DAP_Canopy_Height_r5m", "aspect.r10m_con", "elevation_r5m")], 1,
                                         function(i){ paste(na.omit(i), collapse = " ") })
soil_moist_coeff_df$Estimate_all <- as.numeric(soil_moist_coeff_df$Estimate_all)
soil_moist_coeff_df$class <- "Soil Moisture vol %"


# Checking for co-linearity among model predictors  -----------------------

canopy_elev <- ggplot(graph_data, aes(as.numeric(DAP_Canopy_Height_r15m),as.numeric(Elevation), group = plot)) + 
  geom_point(aes(color = plot)) +
  stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
  
  ylab("Elevation (m)") + 
  labs(color = "Plot") +
  xlab("Mean Canopy Height (m)")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot()

canopy_elev_lm <- lme(DAP_Canopy_Height_r15m ~ Elevation, random = ~Elevation|plot, data = graph_data, method = "REML")
canopy_elev_control <- lme(range_T1_m ~ DAP_Canopy_Height_r15m + Elevation + DAP_Canopy_Height_r15m*Elevation, data =climate_models_complete, random = ~ 1|plot, weights = varPower())
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

aspect_eleve <- ggplot(graph_data, aes(as.numeric(Elevation), as.numeric(aspect.r15m)*180/pi, group = plot)) + 
  geom_point(aes(color = plot)) +
  stat_smooth(aes(color = plot),method = "lm", alpha = 0.2) + 
  ylab("Aspect (°)") + 
  labs(color = "Plot") +
  xlab("Elevation (m)")+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot() +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)), legend.position = "none")
aspect_elev_lm <- lme(as.numeric(aspect.r15m)~ as.numeric(Elevation), random = 
                        ~Elevation | plot,  data = graph_data)
plot(aspect_elev_lm)
summary(aspect_elev_lm)
canopy_elev_leg <- get_legend(canopy_elev)
plot_grid(canopy_elev + theme(legend.position = "none"), canopy_aspect, aspect_eleve, canopy_elev_leg, rel_widths = c(1,1,1,0.15), nrow = 1)



