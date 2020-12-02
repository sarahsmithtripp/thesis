## SST 
### Dec - 01 - 2020 
## code snipets of linear model development 

## Overview of document 
## plot relationships 
## write models based on spatial resolution (r#m) where the # is the radius the value is averaged over
## explore if model can be simplified 
## test model assumptions 
## write model to fit model assumptions 

#libraries 
library(dplyr)
library(nlme)
library(lme4)
seq <- seq(from =1 , to = 10, by = 1)
fs_seq <- paste0("fs", seq) # used to define plot order

# develop data for modeling  ----------------------------------------------
climate_modeling <- read.csv("Summarized_Climate_Data_Dec012020.csv")

outliers <- climate_modeling %>% filter(range_T1_m > 10)


# plot relationships without models fitted (to visualize)  ----------------

# plot relationships in scatterplot form (this plots all of the relationships by each variable)
plotter <- function(data,det_variable, resp_variable, transform) { 
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
    data_vars <- data.frame(data[, c(resp_variable, 'Plotcode', 'month', 'plot')], data_det_vars)
    data_vars_gather <- pivot_longer(data_vars, cols = det_variable_list, names_to ="Canopy_Radius", values_to = "Mean_Canopy_Height") 
    graph <- ggplot(data_vars_gather, aes(Mean_Canopy_Height, range_T1_m, group = as.factor(plot))) + 
      geom_point(aes(color = plot)) + 
      theme_bw()  + ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
      facet_wrap(~Canopy_Radius)
  }
}
Range_T_graphs_height <- plotter(filter(climate_modeling, range_T1_m < 10), resp_variable = 'range_T1_m', det_variable =  "DAP_Canopy_Height", transform = F)






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

anova(mixed.ranslope_r15m_lognomonth, mixed.ranslope_r15m_log)
##random intercepts by month is a better fit 


climate_models_complete$log_predicted_r15m_random <- predict(mixed.ranslope_r15m_log)
climate_models_complete$log_residual<- climate_models_complete$range_T1_m - climate_models_complete$log_predicted_r15m_random
##See if this fixes model assumptions 
fit_plot <- ggplot(climate_models_complete, aes(log_predicted_r15m_random, log_residual)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
qnorm <- ggplot(climate_models_complete, aes(sample = log_residual)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
hist <- ggplot(climate_models_complete, aes(log_residual)) + geom_density() + ggtitle("Error Distribution") +theme_bw() +ylim(0,1)


graph_mod <- ggplot(climate_models_complete) + 
  geom_point(aes(log_DAP_Canopy_Height_r15m, log_range_T1_m)) + 
  geom_line(aes(log_DAP_Canopy_Height_r15m, log_predicted_r15m_random, group = month, color = as.factor(month))) +
  facet_wrap(~plot) +
  labs(color = "month") +
  ylim(0,15) +
  theme_bw()
