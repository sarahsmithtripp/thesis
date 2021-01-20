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

climate_modeling_annual <- climate_data_fix %>% 
  #group_by(Plotcode) %>% 
  #mutate(mean_sm = mean(vol_sm, na.rm = T)) %>%
  pivot_longer(cols = c("T1", "T2", "T3"), names_to = 'sensor', values_to = 'Temp_C') %>% 
  group_by(Plotcode, sensor, DateTime) %>% 
  mutate(range_d = max(Temp_C) - min(Temp_C), 
         min_d = min(Temp_C), 
         max_d = max(Temp_C))%>% 
  group_by(Plotcode, sensor) %>% 
  summarise(
    mean_sm = mean(vol_sm, na.rm = T),
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
lm_r_square <- function(data, resp_variable, fixed) {
  data$plot <- as.factor(data$plot)
  levels(data$plot) <- fs_seq
  #data_vars <- distinct(data_transform)
  data_1 <- data %>% dplyr::select(paste(resp_variable), 'plot', 'Plotcode', contains("r2m"))
  names(data_1) <- c('y', 'plot', 'Plotcode', 'Canopy_ht', 'Canopy_cov', 'elevation', 'aspect', 'slope', 'max_canopy', 'aspect_con')
  data_2 <- data %>% dplyr::select(paste(resp_variable), 'plot', 'Plotcode', contains("r5m"))
  names(data_2) <- c('y','plot', 'Plotcode', 'Canopy_ht', 'Canopy_cov', 'elevation', 'aspect', 'slope', 'max_canopy', 'aspect_con')
  data_3 <- data %>% dplyr::select(paste(resp_variable), 'plot', 'Plotcode', contains("r10m"))
  names(data_3) <- c('y', 'plot', 'Plotcode', 'Canopy_ht', 'Canopy_cov', 'elevation', 'aspect', 'slope', 'max_canopy', 'aspect_con')
  data_4 <- data %>% dplyr::select(paste(resp_variable), 'plot', 'Plotcode', contains("r15m"))
  names(data_4) <- c('y', 'plot', 'Plotcode', 'Canopy_ht', 'Canopy_cov', 'elevation', 'aspect', 'slope', 'max_canopy', 'aspect_con')
  data_5 <- data %>% dplyr::select(paste(resp_variable), 'plot', 'Plotcode', contains("r15m"))
  names(data_5) <- c('y', 'plot', 'Plotcode', 'Canopy_ht', 'Canopy_cov', 'elevation', 'aspect', 'slope', 'max_canopy', 'aspect_con')
  
  
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

soil_models_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T1"), resp_variable = 
                             "mean_T", fixed = T)
surface_models_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T2"), resp_variable = 
                                "mean_T", fixed = T) 
near_surface_models_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T3"), resp_variable = 
                                                       "mean_T", fixed = T) 
soil_models <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T1"), resp_variable = 
                             "mean_T", fixed = F)
surface_models <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T2"), resp_variable = 
                                "mean_T", fixed = F) 
near_surface_models <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T3"), resp_variable = 
                                     "mean_T", fixed = F) 

model_R_all <- data.frame(Radius = c(2,5,10,15,20), 
                      Soil = soil_models, 
                      Surface = surface_models, Near_Surface =  near_surface_models)  %>% 
  pivot_longer(c = c('Soil', 'Surface', 'Near_Surface'), names_to = "Model", values_to = "Random + Fixed")

model_R_fixed <- data.frame(Radius = c(2,5,10,15,20), 
                          Soil = soil_models_fixed, 
                          Surface = surface_models_fixed, Near_Surface =  near_surface_models_fixed)  %>% 
  pivot_longer(c = c('Soil', 'Surface', 'Near_Surface'), names_to = "Model", values_to = "Fixed") %>% 
  left_join(model_R_all) %>%
  pivot_longer(c = c('Random + Fixed', 'Fixed'), names_to = "R_squar_type", values_to = "adj.r")
                      
variation_fixed <- ggplot(model_R_fixed, aes(Radius, adj.r)) + geom_point(aes(color = Model), size = 3) +
  geom_line(aes(color = Model), linetype = "dashed") +
  facet_wrap(~ R_squar_type) + 
  ylab(paste('Adjusted Model R\u00b2')) + xlab("Radius (m)") + labs(color = "Model") +
  scale_color_brewer(palette = "Dark2") +theme_bw(base_size = 20) 


facet_grid <- ggplot(climate_modeling_Ht_long %>% filter(Canopy_Radius %in% c("DAP_Canopy_Height_r2m", "DAP_Canopy_Height_r15m") & sensor == "T1"),
       aes(Mean_Canopy_Height, max_T)) + 
  geom_point() + 
  geom_smooth(method = "lm", alpha = 0.2, color = "grey") +
  ylab("Average Daily Max Temperature °C") + 
  xlab("Average Canopy Height (m)") +
  facet_grid(Canopy_Radius ~ sensor) +   theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +theme_bw(base_size = 15)

plot_grid(facet_grid, variation)


## soil moisture ### 

sm_r_square <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T3"), resp_variable = 
                                                    "mean_sm", fixed = F) 

sm_r_square_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T3"), resp_variable = 
                             "mean_sm", fixed = T) 




# Annual models  ----------------------------------------------------------

# Temperature -------------------------------------------------------------


# soil temperatur ---------------------------------------------------------



#soil_annual_lm_complex <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (DAP_Canopy_Height_r15m | plot) + (1|plot), data = filter(climate_modeling_annual, sensor == "T1"))
soil_annual_lm <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + elevation_r15m + (1|plot), 
                       data = filter(climate_modeling_annual, sensor == "T1"))
soil_annual_lm_cov <- lmer(mean_T ~ DAP_Canopy_Cover_r15m + aspect.r15m_con + elevation_r15m + (1|plot), 
                       data = filter(climate_modeling_annual, sensor == "T1"))
ggplot(climate_modeling_annual, aes(DAP_Canopy_Height_r15m, DAP_Canopy_Cover_r15m, group = plot, color = as.factor(plot))) + 
  geom_point() + geom_smooth(method = "lm")
plot(soil_annual_lm)

#Nested model 1 - remove elevation because should be dealt with T1 
soil_annual_lm_n1b <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + (1|plot), 
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
## soil_annaul_Lm_n1b is the best model 



# Surface Models  ---------------------------------------------------------

surface_annual_lm <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + elevation_r15m + (1|plot), 
                       data = filter(climate_modeling_annual, sensor == "T2"))
surface_annual_lm_cov <- lmer(mean_T ~ DAP_Canopy_Cover_r15m + aspect.r15m_con + elevation_r15m + (1|plot), 
                           data = filter(climate_modeling_annual, sensor == "T2"))
anova(surface_annual_lm, surface_annual_lm_cov)
ggplot(climate_modeling_annual, aes(DAP_Canopy_Height_r15m, DAP_Canopy_Cover_r15m, group = plot, color = plot)) + 
  geom_point() + geom_smooth(method = "lm")
plot(surface_annual_lm)

#Nested model 1 - remove elevation because should be dealt with T2 
surface_annual_lm_n1b <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + (1|plot), 
                           data = filter(climate_modeling_annual, sensor == "T2"))
#Nested model 2 - remove aspect
surface_annual_lm_n1a <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m+ (1|plot), 
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


near_surface_annual_lm <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + elevation_r15m + (1|plot), 
                          data = filter(climate_modeling_annual, sensor == "T3"))
near_surface_annual_lm_cov <- lmer(mean_T ~ DAP_Canopy_Cover_r15m + aspect.r15m_con + elevation_r15m + (1|plot), 
                              data = filter(climate_modeling_annual, sensor == "T3"))
anova(near_surface_annual_lm, near_surface_annual_lm_cov)
ggplot(climate_modeling_annual, aes(DAP_Canopy_Height_r15m, DAP_Canopy_Cover_r15m, group = plot, color = plot)) + 
  geom_point() + geom_smooth(method = "lm")
plot(near_surface_annual_lm)

#Nested model 1 - remove elevation because should be dealt with T3 
near_surface_annual_lm_n1b <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + (1|plot), 
                              data = filter(climate_modeling_annual, sensor == "T3"))
#Nested model 2 - remove aspect
near_surface_annual_lm_n1a <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m+ (1|plot), 
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

soil_moist_annual_lm <- lmer(log(mean_sm) ~ DAP_Canopy_Height_r5m + aspect.r5m_con + elevation_r5m + (1|plot), 
                               data = filter(climate_modeling_annual, sensor == "T3"))
soil_moist_annual_lm_cov <- lmer(mean_sm ~ DAP_Canopy_Cover_r5m + aspect.r5m_con + elevation_r5m + (1|plot), 
                                   data = filter(climate_modeling_annual, sensor == "T3"))
anova(soil_moist_annual_lm, soil_moist_annual_lm_cov)

plot(soil_moist_annual_lm)

#Nested model 1 - remove elevation because should be dealt with T3 
soil_moist_annual_lm_n1b <- lmer(log(mean_sm) ~ DAP_Canopy_Height_r5m + aspect.r5m_con + (1|plot), 
                                   data = filter(climate_modeling_annual, sensor == "T3"))
#Nested model 2 - remove aspect
soil_moist_annual_lm_n1a <- lmer(log(mean_sm) ~ DAP_Canopy_Height_r5m + elevation_r5m+ (1|plot), 
                                   data = filter(climate_modeling_annual, sensor == "T3"))
#remove all variables not Canopy Height 
soil_moist_annual_lm_n2 <- lmer(log(mean_sm) ~ DAP_Canopy_Height_r5m + (1|plot), 
                                  data = filter(climate_modeling_annual, sensor == "T3"))
#remove random plot 
soil_moist_annual_lm_n3  <- lm(log(mean_sm) ~ DAP_Canopy_Height_r5m,
                                 data = filter(climate_modeling_annual, sensor == "T3" & mean_sm))
anova(soil_moist_annual_lm, soil_moist_annual_lm_n1a, soil_moist_annual_lm_n1b, soil_moist_annual_lm_n2, soil_moist_annual_lm_n3)

MuMIn::r.squaredGLMM(soil_moist_annual_lm_n1b)




# Annual Model Graphs  ----------------------------------------------------

#### plot annual soil temperature models 

avg_values <-climate_modeling_annual %>% subset(!is.na(mean_T)) %>% group_by(plot) %>% summarize(#Plotcode = Plotcode,
#                                                                                                  DAP_Canopy_Height_r15m = DAP_Canopy_Height_r15m, 
#                                                                                                  sensor = sensor, 
#                                                                                                  mean_T = mean_T, 
                                                                                                 aspect.r15m_con_avg = mean(aspect.r15m_con), 
                                                                                                 elevation.r15m_con_avg = mean(elevation_r15m))
model_data <- function(data, sensor) {
  q <- which(grepl(paste0(sensor), data$sensor))
  data_mod <- data[q,]
  data_mod <- data_mod %>% subset(!is.na(mean_T)) %>% left_join(avg_values)
}
soil_model_dat <- model_data(climate_modeling_annual, sensor = "T1")
surface_model_dat <- model_data(climate_modeling_annual, "T2")
near_surface_model_dat <- model_data(climate_modeling_annual, "T3")

soil_annual_lm_n1b_avg <-   lmer(mean_T ~ DAP_Canopy_Height_r15m  + aspect.r15m_con_avg
                                 + (1|plot), data = soil_model_dat)
soil_model_dat$prediction <- predict(soil_annual_lm_n1b_avg)

surface_annual_lm_avg <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con_avg+ elevation.r15m_con_avg+ 
                                (1|plot), data = surface_model_dat)
surface_model_dat$prediction <- predict(surface_annual_lm_avg)
near_surface_annual_lm_avg <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con_avg + elevation.r15m_con_avg + 
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
annual_model_graph


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
  m5 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m + aspect.r15m + (1|plot), data = months$'5')
  m6 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m + aspect.r15m + (1|plot), data = months$'6')
  m7 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m + aspect.r15m + (1|plot), data = months$'7')
  m8 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m + aspect.r15m + (1|plot), data = months$'8')
  m9 <- lmer(mean_T~ DAP_Canopy_Height_r15m + elevation_r15m + aspect.r15m + (1|plot), data = months$'9')
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

soil_coeff_df$Estimate_all <-apply(soil_coeff_df[, c("(Intercept)", "DAP_Canopy_Height_r15m", "elevation_r15m", "aspect.r15m")], 1,
                                   function(i){ paste(na.omit(i), collapse = " ") })
soil_coeff_df$Estimate_all <- as.numeric(soil_coeff_df$Estimate_all)

soil <- ggplot(soil_coeff_df) + 
  geom_point(aes(month, Estimate_all, color = month), size = 3) +geom_line(aes(month, Conf_Estimate, color = model), size = 1.5, linetype = "dotted") + 
  ylab("Confidence Interval for Model Estimate") + xlab("") + labs(color = "Month") +
  scale_color_brewer(palette = "Dark2") + ylim(-0.5, 0.5) +
  facet_wrap(~variable)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  theme_bw(base_size = 20) + theme(legend.position = "none",axis.title.x=element_blank(),
                                   axis.text.x=element_blank(),
                                  axis.ticks.x=element_blank())

# Surface Monthly Temperature -----------------------------------------------------

surface_month_models <- monthly_mods(climate_modeling_month, sensor = "T3", variable = "mean_T")
surface_coeff <- lapply(near_surface_month_models, get_slope) 
surface_coeff_df <- bind_rows(near_surface_coeff) %>% pivot_wider(names_from = coeff, values_from = Estimate)

surface_coeff_df$Estimate_all <-apply(near_surface_coeff_df[, c("(Intercept)", "DAP_Canopy_Height_r15m", "elevation_r15m", "aspect.r15m")], 1,
                                   function(i){ paste(na.omit(i), collapse = " ") })
surface_coeff_df$Estimate_all <- as.numeric(near_surface_coeff_df$Estimate_all)

surface <- ggplot(surface_coeff_df) + 
  geom_point(aes(month, Estimate_all, color = month), size = 3) +geom_line(aes(month, Conf_Estimate, color = model), size = 1.5, linetype = "dotted") + 
  ylab("Confidence Interval for Model Estimate") + xlab("") + labs(color = "Month") +
  scale_color_brewer(palette = "Dark2") + ylim(-0.5, 0.5) +
  facet_wrap(~variable)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  theme_bw(base_size = 20) + theme(legend.position = "none",axis.title.x=element_blank(),
                                   axis.text.x=element_blank(),
                                   axis.ticks.x=element_blank())

# Near Surface Monthly Temperature ----------------------------------------

near_surface_month_models <- monthly_mods(climate_modeling_month, sensor = "T3", variable = "mean_T")

near_surface_coeff <- lapply(surface_month_models, get_slope) 
near_surface_coeff_df <- bind_rows(surface_coeff) %>% pivot_wider(names_from = coeff, values_from = Estimate)

near_surface_coeff_df$Estimate_all <-apply(surface_coeff_df[, c("(Intercept)", "DAP_Canopy_Height_r15m", "elevation_r15m", "aspect.r15m")], 1,
                                      function(i){ paste(na.omit(i), collapse = " ") })
near_surface_coeff_df$Estimate_all <- as.numeric(surface_coeff_df$Estimate_all)
near_surface_coeff_df$model_type <- "near_surface"

near_surface <- ggplot(surface_coeff_df) + 
  geom_point(aes(month, Estimate_all, color = month), size = 3) +geom_line(aes(month, Conf_Estimate, color = model), size = 1.5, linetype = "dotted") + 
  ylab("Confidence Interval for Model Estimate") + xlab("") + labs(color = "Month") +
  scale_color_brewer(palette = "Dark2") + ylim(-0.5, 0.5) +
  facet_wrap(~variable)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
  theme_bw(base_size = 20) + theme(legend.position = "none",axis.title.x=element_blank(),
                                   axis.text.x=element_blank(),
                                   axis.ticks.x=element_blank())


# Soil Moisture Monthly  --------------------------------------------------
monthly_mods_moist <- function(data, sensor, variable, scaled) {
  q <- which(grepl(paste0(sensor), data$sensor))
  data_sensor <- data[q,]
  var <- which(grepl(variable, names(data_sensor)))
  names(data_sensor[, var]) <- "variable"

  if(scaled == T) { # build models from z scores of the data rather than actual data itself 
    data_scaled <- data.frame(data_sensor, scale(data_sensor$elevation_r5m), scale(data_sensor$DAP_Canopy_Height_r5m), scale(data_sensor$aspect.r5m_con))
    data_scaled$DAP_Canopy_Height_r5m <- data_scaled$DAP_Canopy_Cover_r5m
    data_scaled$elevation_r5m <- data_scaled$scale.data_sensor.elevation_r5m.
    data_scaled$aspect.r5m_con <- data_scaled$scale.data_sensor.aspect.r5m_con.                         
    months <- split(data_scaled, f = data_scaled$month)
    m5 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r5m_con + (1|plot), data = months$'5')
    m6 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r5m_con + (1|plot), data = months$'6')
    m7 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r5m_con + (1|plot), data = months$'7')
    m8 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r5m_con + (1|plot), data = months$'8')
    m9 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r5m_con + (1|plot), data = months$'9')
    store_mods <- list(m5,m6,m7,m8,m9)
  }
else {
  months <- split(data_sensor, f = data_sensor$month)
  m5 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r5m_con + (1|plot), data = months$'5')
  m6 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r5m_con + (1|plot), data = months$'6')
  m7 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r5m_con + (1|plot), data = months$'7')
  m8 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r5m_con + (1|plot), data = months$'8')
  m9 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + elevation_r5m + aspect.r5m_con + (1|plot), data = months$'9')
  store_mods <- list(m5,m6,m7,m8,m9)
}
}

soil_moist_month_models <- monthly_mods_moist(climate_modeling_month, sensor = "T1", variable = "mean_sm", scaled = T)
soil_moist_coeff <- lapply(soil_moist_month_models, get_slope) 
soil_moist_coeff_df <- bind_rows(soil_moist_coeff) %>% pivot_wider(names_from = coeff, values_from = Estimate)

m7 <- lmer(mean_sm ~ DAP_Canopy_Height_r5m + (1|plot), data = filter(climate_modeling_month, sensor == "T1" & month == "5"))

ggplot(climate_modeling_month %>% filter(month == "7"), aes(DAP_Canopy_Height_r5m, mean_sm)) + 
  geom_point() +
   stat_smooth(method = "lm")

soil_moist_coeff_df$Estimate_all <-apply(soil_moist_coeff_df[, c("(Intercept)", "DAP_Canopy_Height_r5m", "aspect.r5m_con", "elevation_r5m")], 1,
                                      function(i){ paste(na.omit(i), collapse = " ") })
soil_moist_coeff_df$Estimate_all <- as.numeric(soil_moist_coeff_df$Estimate_all)


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







# Monthly Surface Temperature Modeling  -------------------------------------------
surface_dat <- climate_modeling_month %>% filter(sensor == "T2") %>% subset(!is.na(mean_T))

SMM1 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m + aspect.r15m + (1|plot) + (1|month), data = filter(climate_modeling_month, sensor == "T2"))
surface_month_lm <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m + aspect.r15m + (1|plot) + (1|month), data = filter(climate_modeling_month, sensor == "T2"))
surface_month_lm_cov <- lmer(mean_T ~ DAP_Canopy_Cover_r15m + aspect.r15m_con + elevation_r15m + (1|plot) + (1|month), 
                          data = filter(climate_modeling_month, sensor == "T2"))
anova(surface_month_lm, surface_month_lm_cov)


surface_dat$resid <- resid(surface_month_lm)
plot(surface_month_lm)

#Nested model 1 - remove elevation because should be dealt with T3 
surface_month_lm_n1b <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + (1|plot) + (1|month), 
                          data = filter(climate_modeling_month, sensor == "T2"))
surface_month_lm_n1b_mixed <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + (1|plot) + (1+ DAP_Canopy_Height_r15m|month), 
                                data = filter(climate_modeling_month, sensor == "T2"))
surface_month_lm_n1ba <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + (1|month), 
                           data = filter(climate_modeling_month, sensor == "T2"))
surface_month_lm_n1bb <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + (1|plot), 
                           data = filter(climate_modeling_month, sensor == "T2"))

#Nested model 2 - remove aspect
surface_month_lm_n1a <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m+ (1|plot) + (1|month), 
                          data = filter(climate_modeling_month, sensor == "T2"))
#remove and compare random variables 
surface_month_lm_n2 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (1|plot) + (1+DAP_Canopy_Height_r15m|month), 
                         data = filter(climate_modeling_month, sensor == "T2"))
surface_month_lm_n3a <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (1|month), 
                          data = filter(climate_modeling_month, sensor == "T2"))



surface_month_lm_n3b <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (1|plot), 
                          data = filter(climate_modeling_month, sensor == "T2"))
#remove random plot 
surface_month_lm_n4  <- lm(mean_T ~ DAP_Canopy_Height_r15m,
                        data = filter(climate_modeling_month, sensor == "T2"))
surface_dat$resid_n4 <- resid(surface_month_lm_n4)
surface_dat$resid_n2 <- resid(surface_month_lm_n2)
plot(surface_dat$month, surface_dat$resid_n4)
plot(surface_dat$month, surface_dat$resid_n2)
anova(surface_month_lm, surface_month_lm_n1a, surface_month_lm_n1b, surface_month_lm_n2, surface_month_lm_n3a, surface_month_lm_n3b, surface_month_lm_n4, 
      surface_month_lm_n1ba, surface_month_lm_n1bb, surface_month_lm_n1b_mixed)


#law of parsimony 
MuMIn::r.squaredGLMM(surface_month_lm)
plot(surface_month_lm)



# Monthly Near Surface Modeling  ------------------------------------------
near_surface_dat <- climate_modeling_month %>% filter(sensor == "T3") %>% subset(!is.na(mean_T)) %>% 
  mutate(log_t= log(mean_T)) 
SMM1 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m + aspect.r15m + (1 + DAP_Canopy_Height_r15m|plot) + (1+ DAP_Canopy_Height_r15m|month), data = near_surface_dat)
near_surface_month_lm <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m + aspect.r15m + (1|plot) + (1|month), data = filter(climate_modeling_month, sensor == "T3"))
near_surface_month_lm_cov <- lmer(mean_T ~ DAP_Canopy_Cover_r15m + aspect.r15m_con + elevation_r15m + (1|plot) + (1|month), 
                             data = filter(climate_modeling_month, sensor == "T3"))
anova(near_surface_month_lm, near_surface_month_lm_cov, SMM1)


near_surface_dat$resid <- resid(near_surface_month_lm)
plot(near_surface_month_lm)

#Nested model 1 - remove elevation because should be dealt with T3 
near_surface_month_lm_n1b <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + (1|plot) + (1|month), 
                             data = filter(climate_modeling_month, sensor == "T3"))
near_surface_month_lm_n1b_mixed <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + (1|plot) + (1+ DAP_Canopy_Height_r15m|month), 
                                   data = filter(climate_modeling_month, sensor == "T3"))
near_surface_month_lm_n1ba <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + (1|month), 
                              data = filter(climate_modeling_month, sensor == "T3"))
near_surface_month_lm_n1bb <- lmer(mean_T ~ DAP_Canopy_Height_r15m + aspect.r15m_con + (1|plot), 
                              data = filter(climate_modeling_month, sensor == "T3"))

#Nested model 2 - remove aspect
near_surface_month_lm_n1a <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_r15m+ (1|plot) + (1|month), 
                             data = filter(climate_modeling_month, sensor == "T3"))
#remove and compare random variables 
near_surface_month_lm_n2 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (1|plot) + (1+DAP_Canopy_Height_r15m|month), 
                            data = filter(climate_modeling_month, sensor == "T3"))
near_surface_month_lm_n3a <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (1|month), 
                             data = filter(climate_modeling_month, sensor == "T3"))



near_surface_month_lm_n3b <- lmer(mean_T ~ DAP_Canopy_Height_r15m + (1|plot), 
                             data = filter(climate_modeling_month, sensor == "T3"))
#remove random plot 
near_surface_month_lm_n4  <- lm(mean_T ~ DAP_Canopy_Height_r15m,
                           data = filter(climate_modeling_month, sensor == "T3"))
near_surface_dat$resid_n4 <- resid(near_surface_month_lm_n4)
near_surface_dat$resid_n2 <- resid(near_surface_month_lm_n2)
plot(near_surface_dat$month, near_surface_dat$resid_n4)
plot(near_surface_dat$month, near_surface_dat$resid_n2)
plot(near_surface_dat$month, near_surface_dat$resid)

anova(near_surface_month_lm, near_surface_month_lm_n1a, near_surface_month_lm_n1b, near_surface_month_lm_n2, near_surface_month_lm_n3a, near_surface_month_lm_n3b, near_surface_month_lm_n4, 
      near_surface_month_lm_n1ba, near_surface_month_lm_n1bb, near_surface_month_lm_n1b_mixed)


#law of parsimony 
MuMIn::r.squaredGLMM(near_surface_month_lm)
plot(near_surface_month_lm)





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



