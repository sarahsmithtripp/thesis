## linear model development of soil moisture variables 

## SST 
# Dec 12th 2020 
library(tidyverse)
library(nlme)

climate_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/Microclimate_Measurements/Microclimate_TMS_UserSoils_Dec-08-20.csv")
climate_data <- climate_data %>% 
  filter(DateTime_GMT > "2020-05-15" & DateTime_GMT < "2020-10-10") %>% 
  mutate(DateTime = as.Date(DateTime_GMT), 
         Plotcode = as.factor(Plotcode), 
         DateTime_GMT = lubridate::ymd_hms(DateTime_GMT),
         Hour =lubridate::hour(DateTime_GMT), 
         DateTime_Hour = lubridate::ymd_h(paste(DateTime, Hour)))
logger_plotcode <- read.csv(file = "D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Microclimate_SiteData(veg-soil)/logger_plotcode.csv")



meta_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/CA_ST_SoilTempData/CA_ST_MetaData.csv", header = T)
meta_data$plot <- as.factor(meta_data$plot)
levels(meta_data$plot) <- seq(1,10, by = 1)

## colinearity between determinant variables 
canopy_solar <- ggplot(meta_data, aes(Sol_rad.r.15.m, DAP_Canopy_Height_r15m, group = plot)) + 
  geom_point(aes(color = plot)) +
  stat_smooth(method = "lm", alpha = 0.2) +
  xlab("Growing Season Total Solar Radiation (W/m^2)") + 
  labs(color = "Plot") +
  ylab("Mean Canopy Height (m)")+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot() +
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)), legend.position = "none")
canopy_solar_lm <- lme(DAP_Canopy_Height_r15m ~ Sol_rad.r.15.m, data =meta_data, random = ~ Sol_rad.r.15.m|plot)
#Variables are not correlated to input directly into the model 


# Summarize soil moisture modeling ----------------------------------------
SM_summary <- climate_data %>% 
  group_by(Plotcode) %>% 
  summarize( values = sum(vol_sm = 0),
    low_sm =sum(vol_sm < 0.25, na.rm =T), 
         med_sm = sum(vol_sm >=0.25 & vol_sm < 0.75, na.rm = T), 
         high_sm = sum(vol_sm >= 0.75, na.rm = T), 
    total_counts = sum(low_sm, med_sm, high_sm),
    fract_low = low_sm/total_counts,
    fract_med = med_sm/total_counts, 
    fract_high = high_sm/total_counts) %>% 
  pivot_longer(cols = c( ends_with("sm")), names_to = c("class"), values_to = "count") %>% 
  left_join(meta_data)
SM_annual <- climate_data %>% group_by(Plotcode) %>% summarise(mean_sm = mean(vol_sm, na.rm = T), 
                                                               sd_sm = sd(vol_sm, na.rm = T), min_sm= min(vol_sm, na.rm = T)) %>% left_join(meta_data)
SM_monthly <- climate_data %>% group_by(Plotcode, month) %>% summarise(min_sm = min(vol_sm, na.rm = T), max_sm = max(vol_sm, na.rm = T)) %>% 
  filter(min_sm != "Inf")%>% 
  left_join(meta_data)
SM_plot_summaries <- ggplot(SM_summary) + 
  geom_boxplot(aes(plot, log(count),color = class),alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  #geom_point(alpha = 0.7, sive = 1.2, position = 'jitter', outlier.color = NA)+
  ylab("Solar Radiation (W/m^2)") + 
  labs(color = "Plot") +
  xlab("Plot")+
  theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
  cowplot:: theme_cowplot()





mean_sm_covariates <- plot_grid(
  ggplot(SM_annual, aes(DAP_Canopy_Height_r2m, mean_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("Mean Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Mean Canopy Height (m)")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(Sol_rad.r.2.m, mean_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("Mean Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Sol_rad.r.15.m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(slope.r2m, mean_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("Mean Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Slope.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(TRI.r.2.m, mean_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("Mean Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("TRI.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(aspect.r2m, mean_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("Mean Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Aspect.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot()
  
)

sd_sm_covariates <- plot_grid(
  ggplot(SM_annual, aes(DAP_Canopy_Height_r15m, sd_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("sd Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Mean Canopy Height (m)")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(DAP_Canopy_Cover_r15m, sd_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("sd Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Mean Canopy Cover (%)")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(Sol_rad.r.15.m, sd_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("sd Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Sol_rad.r.15.m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(slope.r15m, sd_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("sd Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Slope.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(TRI.r.15.m, sd_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("sd Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("TRI.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(aspect.r15m, sd_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("sd Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Aspect.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot()
)

min_sm_covariates <- plot_grid(
  ggplot(SM_monthly, aes(DAP_Canopy_Height_r15m, min_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("min Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Mean Canopy Height (m)")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_monthly, aes(DAP_Canopy_Cover_r15m, min_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("min Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Mean Canopy Height (m)")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_monthly, aes(Sol_rad.r.15.m, min_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("min Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Sol_rad.r.15.m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_monthly, aes(slope.r15m, min_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("min Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Slope.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_monthly, aes(TRI.r.15.m, min_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("min Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("TRI.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_monthly, aes(aspect.r15m, min_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("min Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Aspect.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot()
)

max_sm_covariates <- plot_grid(
  ggplot(SM_monthly, aes(DAP_Canopy_Height_r15m, max_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("max Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Mean Canopy Height (m)")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_monthly, aes(DAP_Canopy_Cover_r15m, max_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("max Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Mean Canopy Cover %")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_monthly, aes(Sol_rad.r.15.m, max_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("max Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Sol_rad.r.15.m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_monthly, aes(slope.r15m, max_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("max Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Slope.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_monthly, aes(TRI.r.15.m, max_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("max Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("TRI.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_monthly, aes(aspect.r15m, max_sm, group = month)) + 
    geom_point(aes(color = plot)) +
    stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("max Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Aspect.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot()

  )

#annual  linear models  ----------------------------------------------------------

#mean soil moisture 
sm_canopy_height_elevat <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + aspect.r2m + (1|plot), data = SM_annual)
plot(sm_canopy_height)
sm_canopy_height <- lmer(mean_sm ~ DAP_Canopy_Height_r15m + (1|plot), data = SM_annual)
summary(sm_canopy_height)
SM_models <- SM_annual[complete.cases(SM_annual$mean_sm),]
SM_models$lm.canopy.ht.r2m <- predict(sm_canopy_height_elevat)
SM_models$lm.canopy.ht.rsd.r2m <- SM_models$mean_sm - SM_models$lm.canopy.ht.r2m
##See if this fixes model assumptions 
fit_plot <- ggplot(SM_models, aes(lm.canopy.ht.r2m, lm.canopy.ht.rsd.r2m)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
qnorm <- ggplot(SM_models, aes(sample = lm.canopy.ht.rsd)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
hist <- ggplot(SM_models, aes(lm.canopy.ht.rsd)) + geom_density() + ggtitle("Error Distribution") +theme_bw()

graph_mod <- ggplot(SM_models, aes(DAP_Canopy_Height_r2m, mean_sm, group = plot)) + 
  geom_point(aes(DAP_Canopy_Height_r15m, mean_sm)) +
  #tat_smooth(method = "lm", alpha = 0.2) +
  geom_line(aes(DAP_Canopy_Height_r2m, lm.canopy.ht, group = plot, color = plot)) +
  #facet_wrap(~plot) +
  labs(color = "month") +
  theme_bw()
confint(sm_canopy_height_elevat)


#SD soil moisture 
#sd soil moisture 
sm_canopy_height_elevat <- lmer(sd_sm ~ DAP_Canopy_Height_r15m + elevation_r15m + (1|plot), data = SM_annual)
plot(sm_canopy_height)
sm_canopy_height <- lmer(sd_sm ~ DAP_Canopy_Height_r15m + (1|plot), data = SM_annual)
summary(sm_canopy_height)
SM_models <- SM_annual[complete.cases(SM_annual$sd_sm),]
SM_models$lm.canopy.ht <- predict(sm_canopy_height)
SM_models$lm.canopy.ht.rsd <- SM_models$sd_sm - SM_models$lm.canopy.ht
##See if this fixes model assumptions 
fit_plot <- ggplot(SM_models, aes(lm.canopy.ht, lm.canopy.ht.rsd)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
qnorm <- ggplot(SM_models, aes(sample = lm.canopy.ht.rsd)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
hist <- ggplot(SM_models, aes(lm.canopy.ht.rsd)) + geom_density() + ggtitle("Error Distribution") +theme_bw()

graph_mod <- ggplot(SM_models, aes(DAP_Canopy_Height_r15m, sd_sm, group = plot, color = plot)) + 
  geom_point(aes(DAP_Canopy_Height_r15m, sd_sm)) +
  #stat_smooth(method = "lm", alpha = 0.2) +
  #facet_wrap(~plot) +
  labs(color = "month") +
  theme_bw()


plot_grid(ggplot(SM_models, aes(DAP_Canopy_Height_r2m, mean_sm, group = plot)) + 
            geom_point(aes(color = plot)) +
            geom_line(aes(DAP_Canopy_Height_r2m, lm.canopy.ht.r2m, group = plot, color = plot)) +
            #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
            ylab("Mean Volumetric Soil Moisture (%)") + 
            labs(color = "Plot") +
            xlab("Mean Canopy Height (m) - 2 m radius")+
            theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
            ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
            ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
            cowplot:: theme_cowplot(),
          ggplot(SM_annual, aes(DAP_Canopy_Height_r15m, mean_sm, group = plot)) + 
            geom_point(aes(color = plot)) +
            #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
            ylab("Mean Volumetric Soil Moisture (%)") + 
            labs(color = "Plot") +
            xlab("Mean Canopy Height (m) - 15 m radius")+
            theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
            ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
            ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
            cowplot:: theme_cowplot())
library(plotly)
plot_ly(x=SM_models$DAP_Canopy_Height_r2m, y=SM_models$mean_sm, z=SM_models$aspect.r2m, type="scatter3d", mode="markers", color= SM_models$mean_sm)

confint(sm_canopy_height)


## minimum 
SM_models <- SM_annual[which(SM_annual$min_sm != "Inf"),]
sm_canopy_height_elevat <- lmer(min_sm ~ DAP_Canopy_Height_r15m + elevation_r15m + (1|plot), data = SM_models)
plot(sm_canopy_height)
sm_canopy_height <- lmer(min_sm ~ DAP_Canopy_Height_r15m + (1|plot), data = SM_models)
summary(sm_canopy_height)

SM_models$lm.canopy.ht <- predict(sm_canopy_height)
SM_models$lm.canopy.ht.rmin <- SM_models$min_sm - SM_models$lm.canopy.ht
##See if this fixes model assumptions 
fit_plot <- ggplot(SM_models, aes(lm.canopy.ht, lm.canopy.ht.rmin)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
qnorm <- ggplot(SM_models, aes(sample = lm.canopy.ht.rmin)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
hist <- ggplot(SM_models, aes(lm.canopy.ht.rmin)) + geom_density() + ggtitle("Error Distribution") +theme_bw()

graph_mod <- ggplot(SM_models, aes(DAP_Canopy_Height_r15m, min_sm, group = plot, color = plot)) + 
  geom_point(aes(DAP_Canopy_Height_r15m, min_sm)) +
  #stat_smooth(method = "lm", alpha = 0.2) +
  geom_line(aes(DAP_Canopy_Height_r15m, lm.canopy.ht, group = plot, color = plot)) +
  #facet_wrap(~plot) +
  labs(color = "month") +
  theme_bw()
confint(sm_canopy_height)

graph_mod <- ggplot(SM_models, aes(aspect.r15m, min_sm, group = plot, color = plot)) + 
  geom_point(aes(aspect.r15m, min_sm)) +
  stat_smooth(method = "lm", alpha = 0.2) +
  #geom_line(aes(DAP_Canopy_Height_r15m, lm.canopy.ht, group = plot, color = plot)) +
  #facet_wrap(~plot) +
  labs(color = "month") +
  theme_bw()


SM_models <- SM_annual[which(SM_annual$min_sm != "Inf"),]
model <- lmer(min_sm ~ DAP_Canopy_Cover_r15m + aspect.r15m + (1|plot), data = SM_models)
model_cov <- lmer(min_sm ~ DAP_Canopy_Cover_r15m +  (1|plot), data = SM_models)
model_asp <- lmer(min_sm ~ aspect.r15m +  (1|plot), data = SM_models)
SM_models$lm.canopy.cov <- predict(model)
SM_models$lm.canopy.cov.rs <- SM_models$min_sm - SM_models$lm.canopy.cov
##See if this fixes model assumptions 
fit_plot <- ggplot(SM_models, aes(lm.canopy.cov, lm.canopy.cov.rs)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
qnorm <- ggplot(SM_models, aes(sample = lm.canopy.cov.rs)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
hist <- ggplot(SM_models, aes(lm.canopy.cov.rs)) + geom_density() + ggtitle("Error Distribution") +theme_bw()
graph_mod <- ggplot(SM_models, aes(aspect.r15m, min_sm, group = plot, color = plot)) + 
  geom_point(aes(aspect.r15m, min_sm)) +
  #stat_smooth(method = "lm", alpha = 0.2) +
  geom_line(aes(aspect.r15m, lm.canopy.cov, group = plot, color = plot)) +
  #facet_wrap(~plot) +
  labs(color = "month") +
  theme_bw()


plot(sm_canopy_height)
## Run a PCA because at this point I am ##### LOST 

PCA_data <- SM_annual %>% dplyr::select('mean_sm', contains(c("r5m", "r.5.", "r_5m"))) %>% subset(!is.na(mean_sm))
PCA <- prcomp(PCA_data[,1:length(names(PCA_data))], scale= T , center = T)
PCA_stepwise <- lm(mean_sm ~ . , data = PCA_data)
selectedMod <- step(PCA_stepwise, direction = "backward")
summary(selectedMod)
library(factoextra)
fviz_eig(PCA)
fviz_pca_biplot(PCA,
             col.ind = PCA_data$mean_sm, # Color by soil moisture modeling 
             gradient.cols = c("#FC4E07", "#E7B800", "#00AFBB"),
             repel = TRUE ) +  # Avoid text overlappi
              labs(color = "Mean Growing Season Vol. SM (%)")
## explore anomalies 
library(data.table)
SM_anomalies  <- climate_data %>%
  group_by(Plotcode, DateTime) %>% 
  summarize(vol_sm_day = mean(vol_sm, na.rm = T))  %>% 
  group_by(Plotcode) %>%
  mutate(Consec_Days = if_else(vol_sm_day < 0.39, row_number(), 0L)) %>%
  dplyr::select(c("Plotcode", "Consec_Days"))  %>% distinct %>% 
  group_by(Plotcode) %>% 
  summarize(max_consec_days = max(Consec_Days)) %>% left_join(meta_data %>% dplyr::select(-contains("X")))


ggplot(SM_anomalies, aes(DAP_Canopy_Height_r15m, max_consec_days, group = plot, color = plot)) + geom_point()
ggplot(SM_anomalies, aes(TRI.r.15.m, max_consec_days, group = plot, color = plot)) + geom_point()
ggplot(SM_anomalies, aes(elevation_r15m, max_consec_days, group = plot, color = plot)) + geom_point()


## Logistic modeling for count data 
logistic.glm.null <- glmer.nb(max_consec_days~1|plot, data = SM_anomalies)
summary(logistic.glm.null)
logistic.glm.fit <- glmer.nb(max_consec_days~ DAP_Canopy_Height_r15m + (1|plot), family = poisson(), data = SM_anomalies)
