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
                                                               sd_sm = sd(vol_sm, na.rm = T)) %>% left_join(meta_data)
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
  ggplot(SM_annual, aes(DAP_Canopy_Height_r15m, mean_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("Mean Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Mean Canopy Height (m)")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(Sol_rad.r.15.m, mean_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("Mean Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Sol_rad.r.15.m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(slope.r15m, mean_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("Mean Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Slope.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(TRI.r.15.m, mean_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("Mean Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("TRI.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot(),
  ggplot(SM_annual, aes(aspect.r15m, mean_sm, group = plot)) + 
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
  ggplot(SM_monthly, aes(aspect.r15m, max_sm, group = plot)) + 
    geom_point(aes(color = plot)) +
    #stat_smooth(aes(color = plot), method = "lm", alpha  = 0.2) +
    ylab("max Volumetric Soil Moisture (%)") + 
    labs(color = "Plot") +
    xlab("Aspect.r15m")+
    theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
    ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
    ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
    cowplot:: theme_cowplot()
)


# Calculate Rolling mean of SM drying  ------------------------------------

soil_drying 
test_dat <- filter(climate_data, Plotcode == unique(climate_data$Plotcode)[3]) %>% subset(!is.na(vol_sm))
rollingSlope.lm.fit <- function(vector) {
  vector <- !is.na(vector)
  a <- coef(.lm.fit(cbind(1, seq(vector)), vector))[2]
  return(a)
}

test_dat$roll_slope <- 
test_dat$roll_mean <- rollmean(test_dat$vol_sm, 672, align= "right", na.pad = T)
test_sum <- test_dat %>% mutate(week = week(DateTime_GMT)) %>% group_by(week) %>% 
  summarize(mean_slope = mean(roll_slope, na.rm =T ), 
            mean_mean = mean(roll_mean, na.rm = T))
ggplot(test_dat, aes(DateTime_GMT, roll_slope)) + geom_line()
ggplot(test_sum, aes(week, mean_slope)) + geom_point()


dfs <- split(climate_data, list(climate_data$Plotcode))
myRollr <- function(data){
  na_vals <- which(grepl("NA", data$vol_sm))
  data_mods <- data[complete.cases(data$vol_sm),]
  
  roll_slope <- as.numeric(c(rep("NA", 671), rollapplyr(data_mods$vol_sm, 672, rollingSlope.lm.fit)))
  data_mods <- data_mods%>% 
    mutate(roll_mean = zoo::rollapply(vol_sm, 672, mean, partial = TRUE, align = "right"))
  
  #write_csv(data, "rolled.csv", append = TRUE)
  return(data)
}
rolledData <- map_dfr(dfs, myRollr)

numCores <- detectCores()
cl <- makeCluster(numCores)
clusterEvalQ(cl, {
  library(dplyr)
  library(readr)
  library(zoo)
})
fn = "rolled_soilmoisture.csv"
if (file.exists(fn)) 
  #Delete file if it exists
  file.remove(fn)


newDfs <- parLapply(cl, dfs, myRollr, rollingSlope.lm.fit)
rolledData <- do.call("rbind", newDfs)
write_csv(rolledData, "rolled_soilmoisture.csv")

stopCluster(cl)
