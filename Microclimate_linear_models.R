## Creates first simple models of microclimate 

### reads in data submitted to soiltemp database that is clipped in the microclimate_logger_parsing - removing erroneous values 
library(tidyverse)


climate_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/CA_ST_SoilTempData/CA_ST_Temperature_Data.csv", header = T)
climate_data <- climate_data %>% 
  filter(DateTime_GMT > "2020-05-15" & DateTime_GMT < "2020-10-10")

meta_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/CA_ST_SoilTempData/CA_ST_MetaData.csv", header = T)
meta_data <- meta_data[2:nrow(meta_data), ]


# develop microclimate summary data  --------------------------------------

climate_modeling <- climate_data %>% 
  group_by(Plotcode, month, Day) %>% 
  mutate(range_T1_d = max(T1) - min(T1), 
         max_T1_d = max(T1), 
         min_T1_d = min(T1))%>% 
  group_by(Plotcode, month) %>% 
  summarise(range_T1_m = mean(range_T1_d, na.rm = T), 
            max_T1_m = mean(max_T1_d, na.rm = T),
            min_T1_m = mean(min_T1_d, na.rm = T)) %>% 
  left_join(meta_data, by = 'Plotcode') %>% 
  mutate(plot = as.factor(gsub('.{1}$', '', plot_point)))
  #filter(range_T1_m < 10)


weird_data <- climate_modeling %>% 
  filter(range_T1_m > 10)

ggplot(climate_modeling, aes(log(as.numeric(DAP_Canopy_Height_r2m)+1))) + 
  geom_density()




library(cowplot)

summary(lm(range_T1_m ~ log(as.numeric(DAP_Canopy_Height_r2m) + 1) + month, climate_modeling))


#this function currently does NOT work 
plotter <- function(data,det_variable, resp_variable, transform) { 
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
    plots <- plot_grid(range_r2m, range_r5m, range_r10m, range_r15m, range_r20m + theme(legend.position = "none"), legend)
  } 
  
  else if(transform == F) { 
    data_det_vars <- data[, c(det_variable_list)]
    data_det_vars <- apply(data_det_vars, 2, as.numeric)
    
    data_vars <- data.frame(data[, c(resp_variable, 'Plotcode', 'month')], data_det_vars)
    #data_vars <- distinct(data_transform)
    data_1a <- data_vars[,c(resp_variable, det_variable_list[1], 'month')] 
    names(data_1a) <- c('y','x', 'month')
    data_2a <- data_vars[,c(resp_variable, det_variable_list[2], 'month')] 
    names(data_2a) <- c('y','x', 'month')
    data_3a <- data_vars[,c(resp_variable, det_variable_list[3], 'month')] 
    
    names(data_3a) <- c('y','x', 'month')
    data_4a <- data_vars[,c(resp_variable, det_variable_list[4], 'month')] 
    names(data_4a) <- c('y','x', 'month')
    data_5a <- data_vars[,c(resp_variable, det_variable_list[5], 'month')] 
    names(data_5a) <- c('y','x', 'month')
    range_r2m <- ggplot(data_1a, aes(x,y, group = month)) +
      geom_point(aes(color = as.factor(month))) + 
      xlab(det_variable_list[1]) + 
      ylab(resp_variable) + 
      theme_bw() +
      theme(legend.position = "none")
    
    range_r5m <- ggplot(data_2a, aes(x, y, group = month)) +
      geom_point(aes(color = as.factor(month))) + 
      xlab(det_variable_list[2]) + 
      ylab(resp_variable) + 
      theme_bw() +
      theme(legend.position = "none")
    range_r10m <- ggplot(data_3a, aes(x,y, group = month)) +
      geom_point(aes(color = as.factor(month))) +
      xlab(det_variable_list[3]) + 
      ylab(resp_variable) + 
      theme_bw() +
      theme(legend.position = "none")
    range_r15m <- ggplot(data_4a, aes(x,y, group = month)) +
      geom_point(aes(color = as.factor(month)))  + 
      xlab(det_variable_list[4]) + 
      ylab(resp_variable) + 
      theme_bw() +
      theme(legend.position = "none")
    range_r20m <- ggplot(data_5a, aes(x,y, group = month)) +
      xlab(det_variable_list[5]) + 
      ylab(resp_variable) + 
      geom_point(aes(color = as.factor(month))) + 
      theme_bw() 
      
    legend <- get_legend(range_r20m)
    plots <- plot_grid(range_r2m, range_r5m, range_r10m, range_r15m, range_r20m + theme(legend.position = "none"), legend)
  }
    
  }

Range_T_graphs <- plotter(climate_modeling, resp_variable = 'range_T1_m', det_variable =  "DAP_Canopy_Height", transform = T)
Range_T_graphs

max_T_graphs <- plotter(climate_modeling, resp_variable =  'max_T1_m', det_variable = "DAP_Canopy_Height", transform = F)
max_T_graphs

min_T_graphs <- plotter(climate_modeling, resp_variable = 'min_T1_m', det_variable = "DAP_Canopy_Height", transform = T)
min_T_graphs

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
 lm1 <- lme(y~ x + month, data = data_1a, method = "REML", random = ~ 1 |as.factor(plot), na.action = na.exclude, correlation = corAR1( form = 1|month))
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

