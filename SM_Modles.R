## linear model development of soil moisture variables 

## SST 
# Dec 12th 2020 
library(tidyverse)
library(nlme)
library(lme4)
library(cowplot)
library(lubridate)


climate_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/Microclimate_Measurements/Microclimate_TMS_UserSoils_Dec-08-20.csv")
climate_data <- climate_data %>% 
  filter(DateTime_GMT > "2020-05-15" & DateTime_GMT < "2020-10-10") %>% 
  mutate(DateTime = as.Date(DateTime_GMT), 
         Plotcode = as.factor(Plotcode), 
         DateTime_GMT = lubridate::ymd_hms(DateTime_GMT),
         Hour =lubridate::hour(DateTime_GMT), 
         DateTime_Hour = lubridate::ymd_h(paste(DateTime, Hour)), 
         yday = lubridate::yday(DateTime_GMT))
logger_plotcode <- read.csv(file = "D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Microclimate_SiteData(veg-soil)/logger_plotcode.csv")

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
sm_model_long <-SM_annual %>% pivot_longer(cols = contains("Dap_Canopy_Height"), names_to ="Canopy_Radius", values_to = "Mean_Canopy_Height") 
ggplot(sm_model_long 
       %>% filter(Canopy_Radius %in% c("DAP_Canopy_Height_r2m", "DAP_Canopy_Height_r15m")),
       aes(Mean_Canopy_Height, mean_sm)) + 
  geom_point(aes(color = as.factor(plot))) + 
  #geom_smooth(method = "lm", alpha = 0.2, color = "grey") +
  ylab("Average Soil Moisture (% Vol)") + 
  xlab("Average Canopy Height (m)") +
  labs(color = "Plot") + 
  facet_wrap(~Canopy_Radius, nrow = 1) +   theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +theme_bw(base_size = 15)



#annual  linear models  ----------------------------------------------------------
library(lme4)
SM_aspect <- SM_annual %>% subset(!is.na(mean_sm)) %>% group_by(plot) %>% summarize(aspect.r2m_avg = mean(aspect.r2m)) %>% dplyr::select(c("plot", "aspect.r2m_avg"))
#mean soil moisture 
sm_canopy_height_elevat <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + aspect.r2m + (1|plot), data = SM_annual)
plot(sm_canopy_height_elevat)
sm_canopy_height <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + (1|plot), data = SM_annual)
sm_canopy_nonrandom <- lm(mean_sm ~ DAP_Canopy_Height_r2m, data = SM_annual)
sm_canopy_aspect <- lm(mean_sm~aspect.r2m, data = SM_annual)
summary(sm_canopy_height)
anova(sm_canopy_height_elevat, sm_canopy_height, sm_canopy_nonrandom, sm_canopy_aspect)
logLik(sm_canopy_height_elevat)
logLik(sm_canopy_height)
SM_range_mods <- SM_annual[complete.cases(SM_annual$mean_sm),]
SM_range_mods <- left_join(SM_range_mods, SM_aspect)
SM_pred <- SM_range_mods %>% select(c("mean_sm", "DAP_Canopy_Height_r2m", "aspect.r2m_avg", "plot")) %>% mutate(aspect.r2m = aspect.r2m_avg)
sm_canopy_aspect_plot <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + (1|plot), data= SM_pred)
SM_range_mods$lm.aspect.plot <- predict(lmer(mean_sm ~ aspect.r2m + (1|plot), data = SM_range_mods))
SM_range_mods$lm.canopy.aspect <- predict(sm_canopy_height_elevat)
SM_range_mods$lm.canopy.aspect.plt <- predict(sm_canopy_aspect_plot)
SM_range_mods$lm.canopy.aspect.rsd.r2m <- SM_range_mods$mean_sm - SM_range_mods$lm.canopy.aspect
confidence_intervals <- confint(sm_canopy_height_elevat, level = 0.95)
pred <- cbind(SM_range_mods, merTools::predictInterval(sm_canopy_aspect_plot, which = "full"))
cowplot::plot_grid(ggplot(pred, aes(DAP_Canopy_Height_r2m, mean_sm)) + 
            geom_point(aes(color = plot)) +
            geom_line(aes(DAP_Canopy_Height_r2m, lm.canopy.aspect.plt, group = plot, color = plot)) +
            #geom_smooth( size =NA, span = 0.5,method = "rml", alpha  = 0.2) +
            #geom_abline(aes(intercept=`(Intercept)`, slope=DAP_Canopy_Height_r2m), as.data.frame(t(fixef(sm_canopy_aspect_plot)))) + 
            geom_line(aes(DAP_Canopy_Height_r2m,lwr, color  = plot, group = plot), linetype = "dashed") +
            geom_line(aes(DAP_Canopy_Height_r2m, upr, color = plot, group = plot), linetype = "dashed") + 
            ylab("Mean Volumetric Soil Moisture (%)") + 
            labs(color = "Plot") +
            xlab("Mean Canopy Height (m)")+
            theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
            ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
            ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
            cowplot:: theme_cowplot() + theme(legend.position = "none"),
          ggplot(SM_range_mods, aes(aspect.r2m, mean_sm)) + 
            geom_point(aes(color = plot)) +
            geom_line(aes(aspect.r2m, lm.aspect.plot, color = plot)) + 
            #stat_smooth(method = "lm", alpha  = 0.2, color = "black") +
            ylab("Mean Volumetric Soil Moisture (%)") + 
            labs(color = "Plot") +
            xlab("Aspect (rad)")+
            theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
            ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
            ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
            cowplot:: theme_cowplot())

##See if this fixes model assumptions 
fit_plot <- ggplot(SM_range_mods, aes(lm.canopy.aspect, lm.canopy.aspect.rsd.r2m)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
qnorm <- ggplot(SM_range_mods, aes(sample = lm.canopy.aspect)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
hist <- ggplot(SM_range_mods, aes(lm.canopy.aspect.rsd.r2m)) + geom_density() + ggtitle("Error Distribution") +theme_bw()

graph_mod <- ggplot(SM_range_mods, aes(DAP_Canopy_Height_r2m, mean_sm, group = plot)) + 
  geom_point(aes(DAP_Canopy_Height_r15m, mean_sm)) +
  #tat_smooth(method = "lm", alpha = 0.2) +
  geom_line(aes(DAP_Canopy_Height_r2m, lm.canopy.ht, group = plot, color = plot)) +
  #facet_wrap(~plot) +
  labs(color = "month") +
  theme_bw()
confint(sm_canopy_height_elevat)


# Daytime range in soil moisture  -----------------------------------------

SM_annual_Daytime <- climate_data %>% filter(day_night == "day") %>% 
  group_by(Plotcode, yday) %>% summarise(range_day = max(vol_sm, na.rm =T) - min(vol_sm, na.rm = T), group = "drop_last") %>% 
  summarise(mean_range = mean(range_day,na.rm = T)) %>% filter(mean_range != "-Inf") %>%
  left_join(meta_data)
SM_annual_Nighttime <-climate_data %>% filter(day_night == "night") %>% 
  group_by(Plotcode, yday) %>% summarise(range_day = max(vol_sm, na.rm =T) - min(vol_sm, na.rm = T), group = "drop_last") %>% 
  summarise(mean_range = mean(range_day,na.rm = T)) %>% filter(mean_range != "-Inf") %>%
  left_join(meta_data)

sm_canopy_aspect_range_D <- lmer(mean_range ~ DAP_Canopy_Height_r2m + aspect.r2m + (1|plot), data = SM_annual_Daytime)
sm_canopy_aspect_range_N <- lmer(mean_range ~ DAP_Canopy_Height_r2m + aspect.r2m + (1|plot), data = SM_annual_Nighttime)

sm_canopy_nonrandom_range <- lm(mean_range ~ DAP_Canopy_Height_r2m, data = SM_annual_Daytime)
sm_canopy_aspect_range <- lm(mean_range~aspect.r2m, data = SM_annual_Daytime)
SM_canopy_rangdom <- lmer(mean_range~DAP_Canopy_Height_r2m+(1|plot), data = SM_annual_Daytime)
summary(sm_canopy_aspect_range)
anova(sm_canopy_aspect_range_D, sm_canopy_nonrandom_range, sm_canopy_aspect_range, SM_canopy_rangdom)

SM_range_mods <- SM_annual_Daytime[complete.cases(SM_annual_Daytime$mean_range),]
SM_range_mods <- left_join(SM_range_mods, SM_aspect)
SM_range_pred <- SM_range_mods %>% select(c("mean_range", "DAP_Canopy_Height_r2m", "aspect.r2m_avg", "plot")) %>% mutate(aspect.r2m = aspect.r2m_avg)
sm_range_aspect_plot_D <- lmer(mean_range ~ DAP_Canopy_Height_r2m + (1|plot), data= SM_range_pred)
SM_range_mods_N <- SM_annual_Nighttime[complete.cases(SM_annual_Nighttime$mean_range),]
SM_range_mods_N <- left_join(SM_range_mods_N, SM_aspect)
SM_range_pred_N <- SM_range_mods_N %>% select(c("mean_range", "DAP_Canopy_Height_r2m", "aspect.r2m_avg", "plot"))
sm_range_aspect_plot_N <- lmer(mean_range ~ DAP_Canopy_Height_r2m + (1|plot), data= SM_range_pred_N)

SM_range_pred$lm.aspect.plot <- predict(lmer(mean_sm ~ aspect.r2m + (1|plot), data = SM_range_mods))
SM_range_pred$lm.canopy.aspect <- predict(sm_canopy_aspect_range)
SM_range_pred$lm.canopy.aspect.plt_D <- predict(sm_range_aspect_plot_D)
SM_range_mods_N$lm.canopy.aspect.plt_N <- predict(sm_range_aspect_plot_N)
#SM_range_mods$lm.canopy.aspect.rsd.r2m <- SM_range_mods$mean_range - SM_range_mods$lm.canopy.aspect
confidence_intervals <- confint(sm_canopy_aspect_range_D, level = 0.95)
pred <- cbind(SM_range_mods, merTools::predictInterval(sm_range_aspect_plot, which = "full"))

cowplot::plot_grid(ggplot(SM_range_pred, aes(DAP_Canopy_Height_r2m, mean_range)) + 
                     geom_point(aes(color = plot)) +
                     geom_line(aes(DAP_Canopy_Height_r2m, lm.canopy.aspect.plt_D, group = plot, color = plot)) +
                     #geom_smooth( size =NA, span = 0.5,method = "rml", alpha  = 0.2) +
                     #geom_abline(aes(intercept=`(Intercept)`, slope=DAP_Canopy_Height_r2m), as.data.frame(t(fixef(sm_canopy_aspect_plot)))) + 
                     #geom_line(aes(DAP_Canopy_Height_r2m,lwr, color  = plot, group = plot), linetype = "dashed") +
                     #geom_line(aes(DAP_Canopy_Height_r2m, upr, color = plot, group = plot), linetype = "dashed") + 
                     ylab("Mean Daily range in Soil Moisture") + 
                     labs(color = "Plot") +
                     xlab("Mean Canopy Height (m)")+
                     theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
                     ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
                     ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
                     cowplot:: theme_cowplot() + theme(legend.position = "none"))
                   ggplot(SM_range_mods, aes(aspect.r2m, mean_range)) + 
                     geom_point(aes(color = plot)) +
                     geom_line(aes(aspect.r2m, lm.aspect.plot, color = plot)) + 
                     #stat_smooth(method = "lm", alpha  = 0.2, color = "black") +
                     ylab("Mean Volumetric Soil Moisture (%)") + 
                     labs(color = "Plot") +
                     xlab("Aspect (rad)")+
                     theme(axis.text.x =  element_text(margin = margin(r= 0.4, l = 0.4)))+
                     ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
                     ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + guides(fill = F) +
                     cowplot:: theme_cowplot())





#SD soil moisture 
#sd soil moisture 
sm_canopy_height_elevat <- lmer(sd_sm ~ DAP_Canopy_Height_r15m + elevation_r15m + (1|plot), data = SM_annual)
plot(sm_canopy_height)
sm_canopy_height <- lmer(sd_sm ~ DAP_Canopy_Height_r15m + (1|plot), data = SM_annual)
summary(sm_canopy_height)
SM_range_mods <- SM_annual[complete.cases(SM_annual$sd_sm),]
SM_range_mods$lm.canopy.ht <- predict(sm_canopy_height)
SM_range_mods$lm.canopy.ht.rsd <- SM_range_mods$sd_sm - SM_range_mods$lm.canopy.ht
##See if this fixes model assumptions 
fit_plot <- ggplot(SM_range_mods, aes(lm.canopy.ht, lm.canopy.ht.rsd)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
qnorm <- ggplot(SM_range_mods, aes(sample = lm.canopy.ht.rsd)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
hist <- ggplot(SM_range_mods, aes(lm.canopy.ht.rsd)) + geom_density() + ggtitle("Error Distribution") +theme_bw()

graph_mod <- ggplot(SM_range_mods, aes(DAP_Canopy_Height_r15m, sd_sm, group = plot, color = plot)) + 
  geom_point(aes(DAP_Canopy_Height_r15m, sd_sm)) +
  #stat_smooth(method = "lm", alpha = 0.2) +
  #facet_wrap(~plot) +
  labs(color = "month") +
  theme_bw()



library(plot3D)
x <- SM_range_mods$DAP_Canopy_Height_r2m
y <- SM_range_mods$aspect.r2m*180/pi

z <- SM_range_mods$mean_sm
# Compute the linear regression (z = ax + by + d)
#fit <- lme4::lmer(z ~ x + y)
# predict values on regular xy grid
grid.lines = 26
x.pred <- seq(min(x), max(x), length.out = grid.lines)
y.pred <- seq(min(y), max(y), length.out = grid.lines)
levels <- levels(SM_range_mods$plot)
xy <- expand.grid( DAP_Canopy_Height_r2m = x.pred, aspect.r2m = y.pred, plot = levels)
z.pred <- matrix(predict(sm_canopy_height_elevat, newdata = xy), 
                 nrow = grid.lines, ncol = grid.lines)
# fitted points for droplines to surface
fitpoints <- predict(sm_canopy_height_elevat)
plot3D::scatter3D(SM_range_mods$DAP_Canopy_Height_r2m, SM_range_mods$aspect.r2m *180/pi, SM_range_mods$mean_sm, colvar = SM_range_mods$mean_sm, theta = 60, phi = 5, 
                  surf = list(x = x.pred, y = y.pred, z = z.pred,  
                              facets = NA, fit = fitpoints), 
                  xlab = "Canopy Height (m)", ylab = "Aspect (rad)", zlab = "Mean Growing Season Soil Moisture (% vol)")
confint(sm_canopy_height)


## minimum 
SM_range_mods <- SM_annual[which(SM_annual$min_sm != "Inf"),]
sm_canopy_height_elevat <- lmer(min_sm ~ DAP_Canopy_Height_r15m + elevation_r15m + (1|plot), data = SM_range_mods)
plot(sm_canopy_height)
sm_canopy_height <- lmer(min_sm ~ DAP_Canopy_Height_r15m + (1|plot), data = SM_range_mods)
summary(sm_canopy_height)

SM_range_mods$lm.canopy.ht <- predict(sm_canopy_height)
SM_range_mods$lm.canopy.ht.rmin <- SM_range_mods$min_sm - SM_range_mods$lm.canopy.ht
##See if this fixes model assumptions 
fit_plot <- ggplot(SM_range_mods, aes(lm.canopy.ht, lm.canopy.ht.rmin)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
qnorm <- ggplot(SM_range_mods, aes(sample = lm.canopy.ht.rmin)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
hist <- ggplot(SM_range_mods, aes(lm.canopy.ht.rmin)) + geom_density() + ggtitle("Error Distribution") +theme_bw()

graph_mod <- ggplot(SM_range_mods, aes(DAP_Canopy_Height_r15m, min_sm, group = plot, color = plot)) + 
  geom_point(aes(DAP_Canopy_Height_r15m, min_sm)) +
  #stat_smooth(method = "lm", alpha = 0.2) +
  geom_line(aes(DAP_Canopy_Height_r15m, lm.canopy.ht, group = plot, color = plot)) +
  #facet_wrap(~plot) +
  labs(color = "month") +
  theme_bw()
confint(sm_canopy_height)

graph_mod <- ggplot(SM_range_mods, aes(aspect.r15m, min_sm, group = plot, color = plot)) + 
  geom_point(aes(aspect.r15m, min_sm)) +
  stat_smooth(method = "lm", alpha = 0.2) +
  #geom_line(aes(DAP_Canopy_Height_r15m, lm.canopy.ht, group = plot, color = plot)) +
  #facet_wrap(~plot) +
  labs(color = "month") +
  theme_bw()


SM_range_mods <- SM_annual[which(SM_annual$min_sm != "Inf"),]
model <- lmer(min_sm ~ DAP_Canopy_Cover_r15m + aspect.r15m + (1|plot), data = SM_range_mods)
model_cov <- lmer(min_sm ~ DAP_Canopy_Cover_r15m +  (1|plot), data = SM_range_mods)
model_asp <- lmer(min_sm ~ aspect.r15m +  (1|plot), data = SM_range_mods)
SM_range_mods$lm.canopy.cov <- predict(model)
SM_range_mods$lm.canopy.cov.rs <- SM_range_mods$min_sm - SM_range_mods$lm.canopy.cov
##See if this fixes model assumptions 
fit_plot <- ggplot(SM_range_mods, aes(lm.canopy.cov, lm.canopy.cov.rs)) + geom_point() + ggtitle("Residual plot, individual point level") +theme_bw()
qnorm <- ggplot(SM_range_mods, aes(sample = lm.canopy.cov.rs)) +stat_qq() + ggtitle("Normality Plot") + theme_bw()
hist <- ggplot(SM_range_mods, aes(lm.canopy.cov.rs)) + geom_density() + ggtitle("Error Distribution") +theme_bw()
graph_mod <- ggplot(SM_range_mods, aes(aspect.r15m, min_sm, group = plot, color = plot)) + 
  geom_point(aes(aspect.r15m, min_sm)) +
  #stat_smooth(method = "lm", alpha = 0.2) +
  geom_line(aes(aspect.r15m, lm.canopy.cov, group = plot, color = plot)) +
  #facet_wrap(~plot) +
  labs(color = "month") +
  theme_bw()


plot(sm_canopy_height)
## Run a PCA because at this point I am ##### LOST 

PCA_data <- SM_annual %>% dplyr::select('mean_sm', contains(c("DAP_Canopy_Height"))) %>% subset(!is.na(mean_sm))
PCA <- prcomp(PCA_data[2:length(names(PCA_data))], scale= T , center = T)
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
