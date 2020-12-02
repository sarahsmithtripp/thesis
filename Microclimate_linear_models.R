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
         DateTime_Hour = lubridate::ymd_h(paste(DateTime, Hour)), 
         Plot = factor(ifelse(grepl("CA_ST_fs10", .$Plotcode),'fs10', 
                          substr(Plotcode, 7, 9)), levels = fs_seq))
levels(climate_data$Plot)



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
    data_vars_gather <- pivot_longer(data_vars, cols = det_variable_list, names_to ="Canopy_Radius", values_to = "Mean_Canopy_Height") 
    graph <- ggplot(data_vars_gather, aes(Mean_Canopy_Height, range_T1_m, group = as.factor(month))) + 
      geom_point(aes(color = as.factor(month))) + 
      theme_bw() + 
      facet_wrap(~Canopy_Radius)
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

library(cowplot)
model_graphs <- function(data, nlme_model, lmer4_model, y) {
  lme4_plot <- ggpredict(lmer4_model, terms = c("x", "month", "plot"), type = "re") %>% 
    plot() +
    labs(x = "Canopy Height (m)", y = "Range T1  (deg C)", title = "Effect of Canopy Height on Range in Temperatures") + 
    theme_minimal()
  data$yhat.0 <- fitted(nlme_model, level = 0 ) #population averaged estimates
  data$yhat.1 <- fitted(nlme_model, level = 1) #plot level estimates
  data$resid.0 <- resid(nlme_model, level = 0) #estimate residuals 
  data$resid.1 <- resid(nlme_model, level = 1) # estimate the models final residuals (i.e. actual error in the model) 
  # get diagnostic plots
  lev1_residuals <- ggplot(data, aes(yhat.0, resid.0)) + geom_point() + ggtitle("Residual Plot, Population level") + theme_bw()
  fit_plot <- ggplot(data, aes(yhat.1, resid.1)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
  qnorm <- ggplot(data, aes(sample = resid.1)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
  hist <- ggplot(data, aes(resid.1)) + geom_density() + ggtitle("Error Distribution") +theme_bw() +ylim(0,1)
  stats_plots <- plot_grid(lev1_residuals, fit_plot, qnorm, hist)
  plots_list <- list(stats_plots, lme4_plot)
  return(plots_list)
}

package <- function(name, nlme_mod, lme4_mod, graphs) { 
  packaged_data_nlme <- list(name, nlme_mod, graphs[[1]], list(anova(nlme_mod), summary(nlme_mod)))
  packaged_data_lme4 <- list(name, lme4_mod, graphs[[2]], list(anova(lme4_mod), summary(lme4_mod)))
  packaged <- list(packaged_data_nlme, packaged_data_lme4)
  return(packaged)
}
library(lme4)
library(lmtest)
library(ggeffects)
lm_random_plots_graphs <- function(data, resp_variable, det_variable) {
  det_variable_list <- names(data)[which(grepl(det_variable, names(data)))]
  data_det_vars <- data[, c(det_variable_list)]
  data_det_vars <- apply(data_det_vars, 2, as.numeric)
  
  data_vars <- data.frame(data[, c(resp_variable, 'Plotcode', 'month', 'plot')], data_det_vars)
  data_vars$month <- as.factor(data_vars$month)
  data_vars$plot <- as.factor(data_vars$plot)
  levels(data_vars$plot) <- fs_seq
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
  lm1_nlme <- nlme::lme(y ~ x*plot , data_1a, random = ~1|month, method = "REML")
  lm1_lme4 <- lmer(y~x*plot + (1|month), data = data_1a, REML = T, na.action = na.exclude)
  # glm1 <- nlme::gls(y~x + plot, data = data_1a)
  # wghts1 <- nlme::varIdent(~1 | month)
  # M.glm1 <- nlme::gls(y ~ x  + plot, weights = wghts1, data = data_1a)
  lm2_nlme <- lmer(y~x + (1|month) + plot, data = data_2a, REML = T, na.action = na.exclude)
  lm2_lme4 <- lmer(y~x*plot + (1|month), data = data_2a, REML = T, na.action = na.exclude)
  
  lm3_nlme <- lmer(y~x + (1|month) + plot, data = data_3a, REML = T, na.action = na.exclude)
  lm3_lme4 <- lmer(y~x*plot + (1|month), data = data_3a, REML = T, na.action = na.exclude)
  
  lm4_nlme <- lmer(y~x + (1|month) + plot, data = data_4a, REML = T, na.action = na.exclude)
  lm4_lme4 <- lmer(y~x*plot + (1|month), data = data_4a, REML = T, na.action = na.exclude)
  
  lm5_nlme <- lmer(y~x + (1|month) + plot, data = data_5a, REML = T, na.action = na.exclude)
  lm5_lme4 <- lmer(y~x*plot + (1|month), data = data_5a, REML = T, na.action = na.exclude)
  
 #get model graphs 
 graphs_lm1 <- model_graphs(data_1a, lm1_nlme, lm1_lme4, y = y)
 graphs_lm2 <- model_graphs(data_2a, lm2_nlme, lm2_lme4, y = y)
 graphs_lm3 <- model_graphs(data_3a, lm3_nlme, lm3_lme4, y = y)
 graphs_lm4 <- model_graphs(data_4a, lm4_nlme, lm4_lme4, y = y)
 graphs_lm5 <- model_graphs(data_5a, lm5_nlme, lm5_lme4,  y = y)
 

 
 range_1 <- package(det_variable_list[1], lm1_nlme, lm1_lme4, graphs_lm1)
 range_2 <- package(det_variable_list[2], lm2_nlme, lm2_lme4, graphs_lm2)
 range_3 <- package(det_variable_list[3], lm3_nlme, lm3_lme4, graphs_lm3)
 range_4 <- package(det_variable_list[4], lm4_nlme, lm4_lme4, graphs_lm4)
 range_5 <- package(det_variable_list[5], lm5_nlme, lm5_lme4, graphs_lm5)
 
 return(list(range_1, range_2, range_3, range_4, range_5))
}

range_T1_models <- lm_random_plots_graphs(data = climate_modeling, resp_variable = "range_T1_m", det_variable = "DAP_Canopy_Height")
#structure of models 
## range_t1_models [[1]] <- first level defines the radius model
## range_modles[[1]][[1]] <- second level defines whether it is the nlme model or lmer model
## order of the last list is as follows list(name, lme4_mod, graphs[[2]], list(anova(lme4_mod), summary(lme4_mod)))



lm_random_plots_graphs_log <- function(climate_modeling, resp_variable, det_variable) {
  det_variable_list <- names(data)[which(grepl(det_variable, names(data)))]
  data_det_vars <- data[, c(det_variable_list)]
  data_det_vars <- apply(data_det_vars, 2, as.numeric) + 1
  data_det_vars_log <- apply(data_det_vars, 2, log)
  data_transform <- data.frame(data[, c(resp_variable, 'Plotcode', 'month','plot')], data_det_vars_log)
  data_transform <- distinct(data_transform)
  data_transform$month <- as.factor(data_vars$month)
  data_transform$plot <- as.factor(data_vars$plot)
  data_1a <- data_transform[,c(resp_variable, det_variable_list[1], 'month', 'plot')] 
  names(data_1a) <- c('y','x', 'month', 'plot')
  data_2a <- data_transform[,c(resp_variable, det_variable_list[2], 'month', 'plot')] 
  names(data_2a) <- c('y','x', 'month', 'plot')
  data_3a <- data_transform[,c(resp_variable, det_variable_list[3], 'month', 'plot')] 
  
  names(data_3a) <- c('y','x', 'month', 'plot')
  data_4a <- data_transform[,c(resp_variable, det_variable_list[4], 'month', 'plot')] 
  names(data_4a) <- c('y','x', 'month', 'plot')
  
  data_5a <- data_transform[,c(resp_variable, det_variable_list[5], 'month', 'plot')] 
  names(data_5a) <- c('y','x', 'month', 'plot')
  
  #write models
  lm1 <- lmer(y~x + (1|month) + plot, data = data_1a, REML = T, na.action = na.exclude)
  lm2 <- lmer(y~x + (1|month) + plot, data = data_2a, REML = T, na.action = na.exclude)
  lm3 <- lmer(y~x + (1|month) + plot, data = data_3a, REML = T, na.action = na.exclude)
  lm4 <- lmer(y~x + (1|month) + plot, data = data_4a, REML = T, na.action = na.exclude)
  lm5 <- lmer(y~x + (1|month) + plot, data = data_5a, REML = T, na.action = na.exclude)
  
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


range_T_mods_log <- lm_random_plots_graphs_log(climate_modeling, resp_variable = 'range_T1_m', det_variable = 'DAP_Canopy_Height')
  lm_random_plots_graphs(climate_modeling, resp_variable = 'range_T1_m', det_variable = 'DAP_Canopy_Height')


  
  ## Model selection 1 <- determine best resolution for modeling temperature range
  ## start with canopy height because seems promising for fitting 
  ## goal is a random slope by plot and a random intercept by month (unlikely that relationship would change month to month)
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