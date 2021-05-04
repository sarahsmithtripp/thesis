##Sarah Smith-Tripp
#May 4th, 2021 
# Growing season linear models for Alex Fraser Research Forest 
# See metadata for site descriptions 

# Load Packages -----------------------------------------------------------
library(tidyverse)
library(cowplot)
library(lme4)
library(MuMIn)
library(lme4)
library(lmtest)
library(ggeffects)


# Set Working Directory to computer that you are working on (both 105)  --------
## If on .227 
#setwd("D:/Data/SmithTripp/Gavin_Lake")
## If .226 
setwd("X:/SmithTripp/Gavin_Lake")

# Load Microclimate Data and Meta-Data  -------------------------------------------------
#climate data 
climate_data <- read.csv("Microclimate_Measurements/Microclimate_TMS_UserSoils_Dec-08-20.csv")
climate_data <- climate_data %>% 
  filter(DateTime_GMT > "2020-05-15" & DateTime_GMT < "2020-10-10") %>% #filter to the study period 
  mutate(DateTime = as.Date(DateTime_GMT), 
         Plotcode = as.factor(Plotcode), 
         DateTime_GMT = lubridate::ymd_hms(DateTime_GMT),
         Hour =lubridate::hour(DateTime_GMT), 
         DateTime_Hour = lubridate::ymd_h(paste(DateTime, Hour)))

#Meta_Data 
meta_data <- read.csv("CA_ST_SoilTempData/CA_ST_MetaData.csv", header = T)
## Convert aspect to formula used in Thesis (see equation 1) 
meta_data_asp <- dplyr::select(meta_data, contains("aspect"),'Plotcode')
meta_data_asp[,1:5] <- apply(meta_data_asp[,1:5], 2, function(x) { cos((pi/4) * (x*180)/pi) } )
names(meta_data_asp) <- c(paste0(names(meta_data_asp)[1:5], "_con"), "Plotcode")
meta_data <- left_join(meta_data, meta_data_asp)

#Define sequence to use to order plots 
seq <- seq(from =1 , to = 10, by = 1)


# Fix known errors in Microclimate data  ----------------------------------
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


# Summarize climate data for the growing season  --------------------------
climate_modeling_annual <- climate_data_fix %>% 
  group_by(Plotcode, DateTime) %>% 
  mutate(mean_sm = mean(vol_sm, na.rm = T), 
         range_sm = max(vol_sm, na.rm = T) - min(vol_sm, na.rm = T) ) %>%
  pivot_longer(cols = c("T1", "T2", "T3"), names_to = 'sensor', values_to = 'Temp_C') %>% 
  group_by(Plotcode, sensor, DateTime) %>% 
  mutate(min_d = min(Temp_C), 
         max_d = max(Temp_C), 
         range_d = (max_d - min_d))%>% 
  group_by(Plotcode, sensor) %>% 
  summarise(mean_sm = mean(mean_sm, na.rm = T), 
            range_sm = mean(range_sm, na.rm = T),
            #mean_sm = mean(vol_sm, na.rm = T),
            range_T = mean(range_d, na.rm = T), 
            max_T = mean(max_d, na.rm = T),
            min_T = mean(min_d, na.rm = T), 
            mean_T = mean(Temp_C, na.rm = T)) %>% 
  left_join(meta_data, by = 'Plotcode')


# Correlation between growing season microclimate and canopy height --------

#Note: Previous iterations of this function would also plot graphs to check assumptions 
#These are removed here for simplicity 
lm_r_square <- function(data, resp_variable, det_variable, transform) {
  #Gather data of interest 
  det_variable_list <- names(data)[which(grepl(det_variable, names(data)))]
  #subset to specific data 
  data_det_vars <- data[, c(det_variable_list)]
  #make sure data is numerica 
  data_det_vars <- apply(data_det_vars, 2, as.numeric)
  if(transform == F) { 
    data_vars <- data.frame(data[, c(resp_variable, 'Plotcode','plot')], data_det_vars)
    data_vars$plot <- as.factor(data_vars$plot)
    levels(data_vars$plot) <- seq
#rename names of data to allow naming in microclimate modeling 
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
  }
  else if (transform == T) { #if transform do the same as above but first log transform the response variable 
    data_vars <- data.frame(data[, c(resp_variable, 'Plotcode','plot')], data_det_vars)
    data_vars[,1] <- log(data_vars[,1])
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
  }
  
  #write models using data 
  lm1 <- lm(y~x, data = data_1a, na.action = na.exclude)
  lm2 <- lm(y~x, data = data_2a, na.action = na.exclude)
  lm3 <- lm(y~x, data = data_3a, na.action = na.exclude)
  lm4 <- lm(y~x,data = data_4a, na.action = na.exclude)
  lm5 <- lm(y~x, data = data_5a, na.action = na.exclude)
  
  # gather R^2
  r_squar <- c(summary(lm1)$r.squared, summary(lm2)$r.squared, summary(lm3)$r.squared, 
               summary(lm4)$r.squared, summary(lm5)$r.squared)
  #ask function to return plots to check model assumptions 
  #resid_plots <- list(plot(lm1)[2], plot(lm2)[2], plot(lm3)[2], plot(lm4)[2], plot(lm5)[2])
  pear_corr <- r_squar ^ 0.5 
  #return(list(pear_corr, resid_plots))
  return(r_squar)
}

soil_models_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T1"), resp_variable = 
                                   "mean_T", det_variable = "DAP_Canopy_Height", transform = F)
surface_models_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T2"), resp_variable = 
                                      "mean_T", det_variable = "DAP_Canopy_Height", transform = F)

near_surface_models_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T3"), resp_variable = 
                                           "mean_T", det_variable = "DAP_Canopy_Height", transform = F)
soil_moisture_fixed <- lm_r_square(data = climate_modeling_annual %>% filter(sensor == "T3"), resp_variable = 
                                     "mean_sm", det_variable = "DAP_Canopy_Height", transform =T)


model_R_all <- data.frame(Radius = c(2,5,10,15,20),
                          Soil = soil_models_fixed,
                          Surface = surface_models_fixed, Near_Surface =  near_surface_models_fixed,
                          Soil_Moisture = soil_moisture_fixed)  %>%
  pivot_longer(c = c('Soil', 'Surface', 'Near_Surface', "Soil_Moisture"), names_to = "Model", values_to = "pear_corr") %>%
  mutate(Model = factor(Model, levels =c('Soil', 'Surface', 'Near_Surface', "Soil_Moisture")))

#Plot correlation of the two
## Figure Code
variation_fixed <- ggplot(model_R_all, aes(Radius, pear_corr, color = Model, pch = Model)) + geom_point(size = 4) +
  geom_line(size = 1, alpha = 0.6, linetype = "dashed") +  #, linetype = "dashed")  +
  ylab(expression(paste("Model ", R^{2}))) + xlab("Radius (m)") + labs(color = "Growing Season Mean", shape = "Growing Season Mean", pch = "Annual Variable") +
  scale_color_manual(values=c("#990000", "#cc0000", "#FF3333", "#6699CC"), labels =  c("Soil (C°)", "Surface (C°)", "Near Surface (C°)", "ln(Soil Moisture (vol %))"))+
  
  scale_shape_manual(values = c(15, 16, 17, 18), labels = c("Soil (C°)", "Surface (C°)", "Near Surface (C°)", "ln(Soil Moisture (vol %))")) +
  theme_bw(base_size = 20) 




# Functionalize Nested Model Fitting  -------------------------------------

annual_model_function <- function(variable, sensor, data, temp, transform) {
  q <- which(grepl(paste0(sensor), data$sensor))
  data_mod <- data[q,]
  y <- data_mod[, paste(variable)]
  names(y) <- "y"
  if(transform == T) { 
    y$y <- log(y$y)
  }
  data_mod <- cbind(data_mod, y)
  if(temp == T){ #run model with cnaopy at a 15 m radius 
    full_mod <- lmer(y ~ DAP_Canopy_Height_r15m + aspect.r10m_con + Elevation + (1|plot), 
                     data = data_mod)
    #nested Model 1 - remove elevation
    lm_n1b <- lmer(y ~ DAP_Canopy_Height_r15m + aspect.r10m_con + (1|plot), 
                   data = data_mod)
    #Nested model 2 - remove aspect
    lm_n1a <- lmer(y ~ DAP_Canopy_Height_r15m + Elevation+ (1|plot), 
                   data = data_mod)
    #remove all variables not Canopy Height 
    lm_n2 <- lmer(y ~ DAP_Canopy_Height_r15m + (1|plot), 
                  data = data_mod)
    #remove random plot 
    lm_n3  <- lm(y ~ DAP_Canopy_Height_r15m,
                 data = data_mod)
    anova(full_mod, lm_n1a, lm_n1b, lm_n2,lm_n3)
  }
  else if(temp == F) { # run model with canopy at a 2m radius 
    full_mod <- lmer(y ~ DAP_Canopy_Height_r2m + aspect.r10m_con + Elevation + (1|plot), 
                     data = data_mod)
    #nested Model 1 - remove elevation
    lm_n1b <- lmer(y ~ DAP_Canopy_Height_r2m + aspect.r10m_con + (1|plot), 
                   data = data_mod)
    #Nested model 2 - remove aspect
    lm_n1a <- lmer(y ~ DAP_Canopy_Height_r2m + Elevation+ (1|plot), 
                   data = data_mod)
    #remove all variables not Canopy Height 
    lm_n2 <- lmer(y ~ DAP_Canopy_Height_r2m + (1|plot), 
                  data = data_mod)
    #remove random plot 
    lm_n3  <- lm(y ~ DAP_Canopy_Height_r2m,
                 data = data_mod)
    anova(full_mod, lm_n1a, lm_n1b, lm_n2,lm_n3)
  }
}

# Growing Season Soil Temperature and Example Full Models  ----------------
## Similiarly analyses were conducted for all variables, 
##these have been omitted from the final code because the results were similar 
## across all sensors and variables

#Mean Growing Season Soil Temperature 
soil_temp_anova <- annual_model_function("mean_T", "T1", climate_modeling_annual, temp = T, transform = F)

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

#best model is n1a 
#Nested model 2 - remove aspect
soil_annual_lm_n1a <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation+ (1|plot), 
               data = filter(climate_modeling_annual, sensor == "T1"))

# Mean Range in Daily Soil Temperature 

soil_range_annual_anova  <- annual_model_function("range_T", "T1", climate_modeling_annual, temp = T, transform = T)

## Best model n2
soil_range_annual_lm <- lmer(log(range_T) ~ DAP_Canopy_Height_r15m + (1|plot), 
                             data = filter(climate_modeling_annual, sensor == "T1"))

# Growing Season Surface Temperature  -------------------------------------

#Mean growing season range in soil temperature 
surface_model_anova <- annual_model_function("mean_T", "T2", climate_modeling_annual, temp = T, transform = F)
surface_model_anova #Best Model n1a 

#Model n1a 
surface_annual_lm <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation + (1|plot), 
                          data = filter(climate_modeling_annual, sensor == "T2"))

## Mean growing season range in surface temperature 
surface_range_annual_anova  <- annual_model_function("range_T", "T2", climate_modeling_annual, temp = T, transform = T)

## Best model is model 3
surface_range_annual_lm <- lmer(log(range_T) ~ DAP_Canopy_Height_r15m + (1|plot),
                                data = filter(climate_modeling_annual, sensor == "T2"))

# Growing Season Near-Surface Temperature  --------------------------------
near_surface_anova <- annual_model_function("mean_T", "T3", climate_modeling_annual, temp = T, transform = F)

#n1a best model 
near_surface_annual_lm <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation + (1|plot), 
                               data = filter(climate_modeling_annual, sensor == "T3"))

## Mean Annual range in temperature 
near_surface_range_annual_anova  <- annual_model_function("range_T", "T3", climate_modeling_annual, temp = T, transform = T)


## Best model is model 3
near_surface_range_annual_lm <- lmer(log(range_T) ~ DAP_Canopy_Height_r15m + aspect.r10m_con + (1|plot), 
                                     data = filter(climate_modeling_annual, sensor == "T3"))



# Growing Season Soil Moisture  -------------------------------------------
soil_moist_mean_annual_anova <- annual_model_function("mean_sm", "T3", climate_modeling_annual %>% subset(!is.infinite(mean_sm)), temp = F, transform = T)

# best model is simplest model 
#remove all variables not Canopy Height 
soil_moist_annual_lm_n2 <- lmer(log(mean_sm) ~ DAP_Canopy_Height_r2m + (1|plot), 
                                data = filter(climate_modeling_annual, sensor == "T3"))

## Mean Annual range in soil Moist 
soil_moist_range_annual_anova  <- annual_model_function("range_sm", "T3", climate_modeling_annual %>% subset(!is.infinite(range_sm)), temp = F, transform = F)

#best model is model 2 
soil_moist_range_annual_lm <- lmer(range_sm ~ DAP_Canopy_Height_r15m + (1|plot), 
                                   data = climate_modeling_annual %>% filter(sensor == "T3") %>% subset(!is.infinite(range_sm)))

# Values produced from Models in Data Frame format  -----------------------
Model_Coefficients_Mean <- data.frame(rbind(summary(soil_annual_lm_n1a)$coefficients,
                                            summary(surface_annual_lm)$coefficients,
                                            summary(near_surface_annual_lm)$coefficients, 
                                            summary(soil_moist_annual_lm_n2)$coefficients))
Model_Coefficients_Mean$variable <- c(rownames(Model_Coefficients_Mean))

Range_Models <- data.frame(rbind(summary(soil_range_annual_lm)$call,
                                 summary(surface_range_annual_lm)$call,
                                 summary(near_surface_range_annual_lm)$call, 
                                 summary(soil_moist_range_annual_lm)$call))
Model_Coefficients_Range <- data.frame(rbind(summary(soil_range_annual_lm)$coefficients,
                                             summary(surface_range_annual_lm)$coefficients,
                                             summary(near_surface_range_annual_lm)$coefficients, 
                                             summary(soil_moist_range_annual_lm)$coefficients))



# Annual Model Graphs  ----------------------------------------------------
avg_values <-climate_modeling_annual %>% subset(!is.na(mean_T)) %>% group_by(plot) %>% summarize(#Plotcode = Plotcode,
  #                                                                                                  DAP_Canopy_Height_r15m = DAP_Canopy_Height_r15m, 
  #                                                                                                  sensor = sensor, 
  #                                                                                                  mean_T = mean_T, 
  aspect.r10m_con_avg = mean(aspect.r10m_con, na.rm =T),                                                                          elevation_avg = mean(Elevation)) %>%
  mutate(aspect.r10m_con_avg_nr = mean(aspect.r10m_con_avg, na.rm = T), 
         elevation_avg_nr = mean(elevation_avg))


#Writes models using average values for plotting in linear space 
model_data <- function(data, sensor, fit_mod) {
  q <- which(grepl(paste0(sensor), data$sensor))
  data_mod <- data[q,]
  data_mod <- data_mod %>% subset(!is.na(mean_T)) %>% left_join(avg_values)
  if(fit_mod == T) {
    model <- lmer(mean_T ~ DAP_Canopy_Height_r15m + elevation_avg+
                    + (1|plot), data = data_mod)
    data_mod$predict <- predict(model)
    #model_nr <- lm( mean_T ~ DAP_Canopy_Height_r15m  + aspect.r10m_con_avg_nr + elevation_avg_nr, data = data_mod)
    data_mod$predict_nr <- (data_mod$DAP_Canopy_Height_r15m *model@beta[2]) + (unique(avg_values$elevation_avg_nr) * model@beta[3])  + (mean(model@u) + model@beta[1])
    sum <- confint(model)
    data_mod$lower_nr <- ((data_mod$DAP_Canopy_Height_r15m *sum[4,1]) + (unique(avg_values$elevation_avg_nr)*sum[5,1])  + sum[3,1])
    data_mod$upper_nr <- ((data_mod$DAP_Canopy_Height_r15m *sum[4,2]) + (unique(avg_values$elevation_avg_nr)*sum[5,2]) + sum[3,2])
    data_mod
  }
  else if (fit_mod == F){
    data_mod
  }
}
  
# Predictions from models built with plot averages
soil_model_dat <- model_data(climate_modeling_annual, sensor = "T1", fit_mod = T)
surface_model_dat <- model_data(climate_modeling_annual, "T2", fit_mod = T)
near_surface_model_dat <- model_data(climate_modeling_annual, "T3", fit_mod = T)

#create and format dataframe of predictions 
avg_values_predictions <- rbind(soil_model_dat, surface_model_dat, near_surface_model_dat)

avg_values_predictions <- avg_values_predictions %>% mutate(plot = as.factor(plot), 
                                                            predict = as.numeric(predict),
                                                            predict_nr =as.numeric(predict_nr)) %>%
  mutate(class = as.factor(case_when(sensor == "T1" ~ "Soil",
                                     sensor == "T2" ~ "Surface", 
                                     sensor == "T3" ~ "Near-Surface")), 
         mean_T = as.numeric(mean_T))


levels(avg_values_predictions$plot) <- seq(1,10, by =1)

avg_values_predictions$class <- factor(avg_values_predictions$class,levels = c("Soil", "Surface", "Near-Surface"))

#Figure Code 
annual_model_graph <- ggplot(avg_values_predictions) + 
  geom_point(aes(DAP_Canopy_Height_r15m, mean_T, color = plot)) + 
  geom_line(aes(DAP_Canopy_Height_r15m, predict, color = plot), alpha = 0.5, size = 1) +
  geom_line(aes(DAP_Canopy_Height_r15m, predict_nr), color = "black" ,size = 2, alpha = 0.7) +
  #geom_line(aes(DAP_Canopy_Height_r15m, lower_nr)) +
  #geom_line(aes(DAP_Canopy_Height_r15m, upper_nr)) +
  xlim(0,25) +
  ylab("Mean Temperature °C") + 
  labs(color = "Plot") +
  xlab("Canopy Height (m)")+
  facet_wrap(~class, ncol = 1) +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  xlab("") +
  theme_bw(base_size = 14) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
legend <- get_legend(annual_model_graph)
annual_model_graph <- annual_model_graph + theme(legend.position = "none")

## plot annual soil moisture 
soil_moist_ann_dat <- filter(climate_modeling_annual, sensor == "T1") %>% subset(!is.na(mean_sm)) %>% left_join(avg_values) %>% mutate(plot = as.factor(plot), class = "Soil Moisture")

#create predictions for soil moisture 
levels(soil_moist_ann_dat$plot) <- seq(1,10, by =1)
soil_moist_annual_lm_avg <- lmer(log(mean_sm) ~ DAP_Canopy_Height_r2m + aspect.r10m_con_avg + elevation_avg + (1|plot), data = soil_moist_ann_dat)
soil_moist_ann_dat$prediction <- as.numeric(exp(predict(soil_moist_annual_lm_avg)))
soil_moist_ann_dat$prediction_nr <- exp((soil_moist_ann_dat$DAP_Canopy_Height_r2m *soil_moist_annual_lm_avg@beta[2]) + (unique(avg_values$elevation_avg_nr) * soil_moist_annual_lm_avg@beta[4]) + 
                                          (unique(avg_values$aspect.r10m_con_avg_nr) *soil_moist_annual_lm_avg@beta[3]) + (mean(soil_moist_annual_lm_avg@u) +soil_moist_annual_lm_avg@beta[1]))
soil_moist_ann_dat$class <- "Soil Moisture"

#figure code for growing season mean soil moisture 
sm_annual_model_graph <- ggplot(soil_moist_ann_dat, aes(group = plot)) + 
  geom_point(aes(DAP_Canopy_Height_r2m, mean_sm*100, color = plot)) + 
  geom_line(aes(DAP_Canopy_Height_r2m, prediction*100, color = plot),size = 1, alpha = 0.5) +
  geom_line(aes(DAP_Canopy_Height_r2m, prediction_nr*100), color = "black", size = 2, alpha = 0.7) +
  ylab("Mean Soil Moisture (% vol)") + 
  labs(color = "Plot") + facet_wrap(~class) +
  xlab("Canopy Height (m)")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  theme_bw(base_size = 14) + theme(legend.position = "none")

## Stack Mean Plots together 
mean_plots_stacked <- plot_grid(annual_model_graph, sm_annual_model_graph, rel_heights = c(3,1.2), ncol = 1)

mean_plot <- plot_grid(mean_plots_stacked, legend, rel_widths = c(1, 0.2), nrow = 1)

#temperature graphs
avg_values_predictions_range <- bind_rows(soil_temp_range, surface_temp_range, near_surface_temp_range)



avg_values_predictions_range <- avg_values_predictions_range %>% mutate(plot = as.factor(plot), 
                                                                        predict = as.numeric(prediction),
                                                                        predict_nr =as.numeric(prediction_nr)) %>%
  mutate(class = as.factor(case_when(sensor == "T1" ~ "Soil",
                                     sensor == "T2" ~ "Surface", 
                                     sensor == "T3" ~ "Near-Surface")), 
         range_T = exp(.$'log(range_T)'),
         class_ = as.factor(class), 
         class_ = fct_relevel(class_, "Near-Surface", after = 2))


levels(avg_values_predictions_range$plot) <- seq(1,10, by =1)
avg_values_predictions_range$class <- factor(avg_values_predictions_range$class,levels = c("Soil", "Surface", "Near-Surface"))

#soil temperature 
soil_temp_range <- model_data(climate_modeling_annual, sensor = "T1", fit_mod =F)
soil_temp_annual_range_avg <- lmer(log(range_T) ~ DAP_Canopy_Height_r15m + (1|plot), data = soil_temp_range)
soil_temp_range <- soil_temp_annual_range_avg@frame
soil_temp_range$prediction <- exp(as.numeric(predict(soil_temp_annual_range_avg)))

soil_temp_range$prediction_nr <- exp((soil_temp_range$DAP_Canopy_Height_r15m *soil_temp_annual_range_avg@beta[2])  + 
                                       + (mean(soil_temp_annual_range_avg@u) +soil_temp_annual_range_avg@beta[1]))
soil_temp_range$sensor <- "T1"

#surface temperature
surface_temp_range <- model_data(climate_modeling_annual, sensor = "T2", fit_mod =F)
surface_temp_annual_range_avg <- lmer(log(range_T) ~ DAP_Canopy_Height_r15m + (1|plot), data = surface_temp_range)
surface_temp_range <- surface_temp_annual_range_avg@frame
surface_temp_range$prediction <- exp(as.numeric(predict(surface_temp_annual_range_avg))) # convert out of log units
surface_temp_range$prediction_nr <- exp((surface_temp_range$DAP_Canopy_Height_r15m *surface_temp_annual_range_avg@beta[2]) + (mean(surface_temp_annual_range_avg@u) +surface_temp_annual_range_avg@beta[1]))
surface_temp_range$sensor <- "T2"

#near-surface temperature 
near_surface_temp_range <- model_data(climate_modeling_annual, sensor = "T3", fit_mod =F)
near_surface_temp_annual_range_avg <- lmer(log(range_T) ~ DAP_Canopy_Height_r15m +aspect.r10m_con_avg + (1|plot), data = near_surface_temp_range)
near_surface_temp_range <- near_surface_temp_annual_range_avg@frame 
near_surface_temp_range$prediction <- exp(as.numeric(predict(near_surface_temp_annual_range_avg)))
near_surface_temp_range$prediction_nr <- exp((near_surface_temp_range$DAP_Canopy_Height_r15m *near_surface_temp_annual_range_avg@beta[2]) +
                                               (unique(avg_values$aspect.r10m_con_avg_nr) *near_surface_temp_annual_range_avg@beta[3]) + 
                                               (mean(near_surface_temp_annual_range_avg@u) +near_surface_temp_annual_range_avg@beta[1]))
near_surface_temp_range$sensor <- "T3"

#Combine values from average range models into one data frame
avg_values_predictions_range <- bind_rows(soil_temp_range, surface_temp_range, near_surface_temp_range)


avg_values_predictions_range <- avg_values_predictions_range %>% mutate(plot = as.factor(plot), 
                                                                        predict = as.numeric(prediction),
                                                                        predict_nr =as.numeric(prediction_nr)) %>%
  mutate(class = as.factor(case_when(sensor == "T1" ~ "Soil",
                                     sensor == "T2" ~ "Surface", 
                                     sensor == "T3" ~ "Near-Surface")), 
         range_T = exp(.$'log(range_T)'),
         class_ = as.factor(class), 
         class_ = fct_relevel(class_, "Near-Surface", after = 2))


levels(avg_values_predictions_range$plot) <- seq(1,10, by =1)
avg_values_predictions_range$class <- factor(avg_values_predictions_range$class,levels = c("Soil", "Surface", "Near-Surface"))

#Figure code for range graphs 
annual_model_graph_range <- ggplot(avg_values_predictions_range) + 
  geom_point(aes(DAP_Canopy_Height_r15m,range_T, color = plot)) + 
  geom_line(aes(DAP_Canopy_Height_r15m, predict, color = plot), alpha = 0.5, size = 1) +
  geom_line(aes(DAP_Canopy_Height_r15m, predict_nr), color = "black" ,size = 2, alpha = 0.7) +
  #geom_line(aes(DAP_Canopy_Height_r15m, lower_nr)) +
  #geom_line(aes(DAP_Canopy_Height_r15m, upper_nr)) +
  xlim(0,25) +
  ylab("Mean Daily Range in Temperature °C") + 
  labs(color = "Plot") +
  xlab("Canopy Height (m)")+
  facet_wrap(~class_, ncol = 1, scales = "free_y" ) +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  xlab("") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_bw(base_size = 14) + theme(axis.title.x=element_blank(),
                                   axis.text.x=element_blank(),
                                   axis.ticks.x=element_blank())
annual_model_graph_range
legend <- get_legend(annual_model_graph_range)
annual_model_graph_range <- annual_model_graph_range + theme(legend.position = "none")


#soil moisture
soil_moist_ann_range_dat <- filter(climate_modeling_annual, sensor == "T1") %>% subset(!is.infinite(range_sm)) %>% left_join(avg_values) %>% mutate(plot = as.factor(plot), 
                                                                                                                                                    class = "Soil Moisture")

levels(soil_moist_ann_range_dat$plot) <- seq(1,10, by =1)
soil_moist_annual_range_lm_avg <- lmer(range_sm ~ DAP_Canopy_Height_r2m + (1|plot), 
                                       data = soil_moist_ann_range_dat)
soil_moist_ann_range_dat$prediction <- as.numeric(predict(soil_moist_annual_range_lm_avg))
soil_moist_ann_range_dat$prediction_nr <- (soil_moist_ann_range_dat$DAP_Canopy_Height_r2m *soil_moist_annual_range_lm_avg@beta[2]) + (mean(soil_moist_annual_range_lm_avg@u) +soil_moist_annual_range_lm_avg@beta[1])
soil_moist_ann_range_dat$class <- "Soil Moisture"


sm_annual_range_model_graph <- ggplot(soil_moist_ann_range_dat, aes(group = plot)) + 
  geom_point(aes(DAP_Canopy_Height_r2m, range_sm*100, color = plot)) + 
  geom_line(aes(DAP_Canopy_Height_r2m, prediction*100, color = plot),size = 0.5, alpha = 0.5) +
  geom_line(aes(DAP_Canopy_Height_r2m, prediction_nr*100), color = "black" ,size = 2, alpha = 0.7) +
  ylab("Mean Daily Range in Soil Moisture (% vol)") + 
  labs(color = "Plot") + facet_wrap(~class) +
  xlab("Canopy Height (m)")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  theme_bw(base_size = 14) + theme(legend.position = "none")
sm_annual_range_model_graph

## Stack range values into a data frame
range_plots_stacked <- plot_grid(annual_model_graph_range, sm_annual_range_model_graph, rel_heights = c(3,1.2), ncol = 1)

full_stack <- plot_grid(mean_plots_stacked, range_plots_stacked, legend, rel_widths = c(1,1,0.2), nrow = 1)
full_stack

#filename <- c("D:/Data/SmithTripp/Gavin_Lake/Figures/annual_mods.jpeg")
#filename <- c("C:/Users/user/Desktop/figure_save.jpeg") 
#save_plot(full_stack, filename = filename,
#base_height = 12, base_width = 9.5)
