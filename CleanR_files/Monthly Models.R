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



# Summarize Microclimate Data by month ------------------------------------

### Exploratory graphs attached here 
climate_modeling_month <- climate_data_fix %>% 
  tidyr::pivot_longer(cols = c("T1", "T2", "T3"), names_to = 'sensor', values_to = 'Temp_C') %>% 
  group_by(Plotcode, month, sensor) %>%
  summarize(mean_T = mean(Temp_C, na.rm = T), 
            mean_sm = mean(vol_sm, na.rm = T),
            range_sm = mean((max(vol_sm)- min(vol_sm)), na.rm = T),
            range_T = mean((max(Temp_C)- min(Temp_C)), na.rm = T)
  )%>%
  left_join(meta_data) %>% 
  filter(!(sensor == "T1" & mean_T > 35)) %>%
  mutate(variable = case_when(sensor == "T1" ~"Soil °C (-8 cm)", 
                              sensor == "T2" ~"Surface °C (0 cm)",
                              sensor == "T3" ~"Near-Surface °C (15 cm)"), 
         variable_ = factor(variable, levels = c("Soil °C (-8 cm)", "Surface °C (0 cm)", "Near-Surface °C (15 cm)")),
         month_ch = lubridate::month(month, label = T))

#Figure code for data figure (First figure in paper)
temp <- ggplot(climate_modeling_month %>% filter(sensor %in% c("T1", "T2")),
               aes(month_ch, mean_T, group = month)) + 
  geom_boxplot(alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  geom_point(alpha = 0.7, position = 'jitter', size = 1)+
  facet_wrap(~variable_, ncol = 2) +
  ylab("Mean Temperature °C") + ylim(7, 18)+
  labs(color = "Plot") +
  xlab("")+ # element_text(margin = margin(r= 0.4, l = 0.4)))+
  # ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  # ggthemes::scale_fill_tableau(palette = "Classic Cyclic") +
  theme_bw(base_size = 12) + 
  theme(#axis.text.x = element_blank(), axis.title.x = element_blank(), 
    panel.spacing = unit(3, "lines"))
legend_temp <- get_legend(temp)
temp <- temp +theme(legend.position = "none")
temp2 <-  ggplot(climate_modeling_month %>% filter(sensor %in% c("T3")),
                 aes(month_ch, mean_T, group = month)) + 
  geom_boxplot(alpha = 0.2, outlier.color = NA, position = position_dodge(0.8)) + 
  geom_point(alpha = 0.7, position = 'jitter', size = 1)+
  facet_wrap(~variable_, ncol = 2) + ylim(7, 18)  +
  ylab("Mean Temperature °C") + 
  labs(color = "Plot") +
  xlab("")+ # element_text(margin = margin(r= 0.4, l = 0.4)))+
  ggthemes::scale_color_tableau(palette = "Classic Cyclic") +
  ggthemes::scale_fill_tableau(palette = "Classic Cyclic") +
  theme_bw(base_size = 12) + theme(legend.position = "none")

soil_moist_plot_avg <- climate_data_fix %>% left_join(select(meta_data, c("Plotcode", "plot"))) %>% 
  mutate(week = lubridate::week(DateTime_GMT)) %>% 
  group_by(Plotcode, week) %>% 
  mutate(sm_plot = mean(vol_sm, na.rm = T)) %>%
  group_by(Plotcode, week) %>%
  summarise(plot = as.factor(plot), mean_sm = mean(vol_sm, na.rm = T),
            sd_sm = sd(vol_sm, na.rm = T), 
            upp_sm = mean_sm +sd_sm, 
            lwr_sm = mean_sm -sd_sm, variable = "Soil Moisture Vol %", 
            date = lubridate::ymd( "2020-01-01" ) + lubridate::weeks(week - 1 )) %>% 
  group_by(week) %>% 
  mutate(mean_all = mean(mean_sm, na.rm = T), 
         sd_all = sd(mean_sm, na.rm = T), 
         upp_sm_all = mean_all + sd_all, 
         lwr_sm_all = mean_all - sd_all)

## remove fs106 for low early values 
soil_moist_graph_data <- filter(soil_moist_plot_avg, Plotcode != "CA_ST_fs106")

soil_moist_2 <- ggplot(soil_moist_graph_data, aes(group = Plotcode)) +
  geom_line(aes(date, mean_sm*100), color = "grey") + 
  geom_line(aes(date, mean_all*100), size = 2) + 
  geom_ribbon(aes(date, ymax = upp_sm_all*100, ymin = lwr_sm_all*100), 
              alpha = 0.01) +
  facet_wrap(~variable, ncol = 1) +
  ylab("Mean Soil Moisture (vol %)") + 
  xlab("") +
  #ggthemes::scale_color_tableau(palette = "Classic Cyclic") + 
  #scale_fill_manual(values = rep("grey", 10)) + 
  scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week",
               date_labels = "%b") + 
  theme_bw(base_size = 12) + theme(legend.position = "none")

# Stack plots to make a figure gride 
pt1 <- plot_grid(temp2, soil_moist_2, ncol = 2, rel_widths = c(1, 1,2), rel_heights = c(1, 1.2))
pt2 <- plot_grid(temp, pt1, ncol = 1, rel_widths = c(1, 1.3))

# stack <- plot_grid(pt2, legend_temp, ncol = 2, rel_widths = c(1, 0.2))
# save_plot(paste0(getwd(), "/Figures/exploratory_figure.jpeg"), stack, base_width = 7, base_height = 4.7)

# Functionalize Monthly Temperature Model Writing  ------------------------------------

#Write linear models using full model (non-reduced)
monthly_mods <- function(data, sensor, variable) {
  q <- which(grepl(paste0(sensor), data$sensor))
  data_sensor <- data[q,]
  months <- split(data_sensor, f = data_sensor$month)
  if(variable == "mean"){
    m5 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'5')
    m6 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'6')
    m7 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'7')
    m8 <- lmer(mean_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'8')
    m9 <- lmer(mean_T~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'9')
    out <- list(m5,m6,m7,m8,m9)
  }
  else if (variable == "range") {
    m5 <- lmer(range_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'5')
    m6 <- lmer(range_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'6')
    m7 <- lmer(range_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'7')
    m8 <- lmer(range_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'8')
    m9 <- lmer(range_T ~ DAP_Canopy_Height_r15m + Elevation + aspect.r10m_con + (1|plot), data = months$'9')
    out <- list(m5,m6,m7,m8,m9) 
  }
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

#Function to get explanatory power of each model
get_fixed <- function(mumin_list) {
  fixed <- mumin_list[2]  # here number 2 is fixed and random effects 
  return(fixed)
}


#Get coefficients from the linear models 
mods_coeff_function <- function(data, sensor, variable, class_name){
  model_list <- monthly_mods(data, sensor = sensor, variable = variable)
  model_coeff <- lapply(model_list, get_slope) 
  model_radj <-lapply(model_list, MuMIn::r.squaredGLMM)
  model_fixed <- data.frame(t(rbind(month = c('5','6', '7', '8','9'), 
                                    r_fixed = bind_cols(lapply(model_radj, get_fixed)))))
  
  model_coeff_df <- bind_rows(model_coeff) %>% pivot_wider(names_from = coeff, values_from = Estimate) %>% 
    left_join(model_fixed)
  
  model_coeff_df$Estimate_all <-apply(model_coeff_df[, c("(Intercept)", "DAP_Canopy_Height_r15m", "Elevation", "aspect.r10m_con")], 1,
                                      function(i){ paste(na.omit(i), collapse = " ") })
  model_coeff_df$Estimate_all <- as.numeric(model_coeff_df$Estimate_all)
  
  model_coeff_df$class <- paste(class_name)
  return(model_coeff_df)
}


# Run Monthly Temperature Models  -----------------------------------------


## Soil Monthly Temperature Modeling 
soil_coeff_df <- mods_coeff_function(climate_modeling_month, sensor = "T1", variable = "mean", class_name = "Soil °C")
soil_coeff_df_range <- mods_coeff_function(climate_modeling_month, sensor = "T1", variable = "range", class_name = "Soil °C")

## Surface Monthly Temperature Modeling 
surface_coeff_df <- mods_coeff_function(climate_modeling_month, sensor = "T2", variable = "mean", class_name = "Surface °C")
surface_coeff_df_range <- mods_coeff_function(climate_modeling_month, sensor = "T2", variable = "range", class_name = "Surface °C")

## Near Surface Monthly Temperature Modeling 
near_surface_coeff_df <- mods_coeff_function(climate_modeling_month, sensor = "T3", variable = "mean", class_name = "Near-Surface °C")
near_surface_coeff_df_range <- mods_coeff_function(climate_modeling_month, sensor = "T3", variable = "range", class_name = "Near-Surface °C")


# Functionalize Soil Moisture Modeling  -----------------------------------
monthly_mods_moist <- function(data, sensor, variable, scaled) {
  q <- which(grepl(paste0(sensor), data$sensor))
  data_sensor <- data[q,]
  
  if(scaled == T) { # build models from z scores of the data rather than actual data itself 
    data_scaled <- data.frame(data_sensor, scale(data_sensor$Elevation), scale(data_sensor$DAP_Canopy_Height_r2m), scale(data_sensor$aspect.r10m_con))
    data_scaled$DAP_Canopy_Height_r5m <- data_scaled$scale.data_sensor.DAP_Canopy_Height_r2m. ## Change to 2 meters here 
    data_scaled$elevation_r5m <- data_scaled$scale.data_sensor.Elevation ## change to elevation here 
    data_scaled$aspect.r10m_con <- data_scaled$scale.data_sensor.aspect.r10m_con.
    data_scaled$mean_sm <- data_scaled$mean_sm * 100 # Actually put in vol % 
    months <- split(data_scaled, f = data_scaled$month)
    m5 <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'5')
    m6 <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'6')
    m7 <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'7')
    m8 <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'8')
    m9 <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'9')
    store_mods <- list(m5,m6,m7,m8,m9)
  }
  else {
    if(variable == "mean"){
      data$mean_sm <- data$mean_sm * 100
      months <- split(data_sensor, f = data_sensor$month)
      m5 <- lmer(mean_sm ~DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'5')
      m6 <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'6')
      m7 <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'7')
      m8 <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'8')
      m9 <- lmer(mean_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'9')
      store_mods <- list(m5,m6,m7,m8,m9) }
    else if (variable == "range"){
      data$range_sm <- data$range_sm * 100
      months <- split(data_sensor, f = data_sensor$month)
      m5 <- lmer(range_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'5')
      m6 <- lmer(range_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'6')
      m7 <- lmer(range_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'7')
      m8 <- lmer(range_sm ~ DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'8')
      m9 <- lmer(range_sm ~DAP_Canopy_Height_r2m + Elevation + aspect.r10m_con + (1|plot), data = months$'9')
      store_mods <- list(m5,m6,m7,m8,m9) }
  }
}
mods_coeff_function_moist<- function(data, sensor, variable, class_name,scaled){
  model_list <- monthly_mods_moist(data, sensor = sensor, variable = variable, scaled)
  model_coeff <- lapply(model_list, get_slope) 
  model_radj <-lapply(model_list, MuMIn::r.squaredGLMM)
  model_fixed <- data.frame(t(rbind(month = c('5','6', '7', '8','9'), 
                                    r_fixed = bind_cols(lapply(model_radj, get_fixed)))))
  
  model_coeff_df <- bind_rows(model_coeff) %>% pivot_wider(names_from = coeff, values_from = Estimate) %>% 
    left_join(model_fixed)
  
  model_coeff_df$Estimate_all <-apply(model_coeff_df[, c("(Intercept)", "DAP_Canopy_Height_r2m", "Elevation", "aspect.r10m_con")], 1,
                                      function(i){ paste(na.omit(i), collapse = " ") })
  model_coeff_df$Estimate_all <- as.numeric(model_coeff_df$Estimate_all)
  
  model_coeff_df$class <- paste(class_name)
  return(model_coeff_df)
}


# Run Monthly Soil Moisture Models  ---------------------------------------

soil_moist_coeff_df <- mods_coeff_function_moist(climate_modeling_month, sensor = "T2", variable = "mean", class_name =  "Soil Moisture vol %",
                                                 scaled = F)
soil_moist_coeff_df_range <- mods_coeff_function_moist(climate_modeling_month, sensor = "T2", variable = "range", class_name =  "Soil Moisture vol %",
                                                       scaled = F)


# Graph Monthly Model Estimates for DAP data  -----------------------------

all_month_models_DAP <- full_join(subset(soil_moist_coeff_df, !is.na(DAP_Canopy_Height_r2m)), 
                                  subset(soil_coeff_df, !is.na(DAP_Canopy_Height_r15m))) %>%
  full_join(subset(surface_coeff_df, !is.na(DAP_Canopy_Height_r15m))) %>%  full_join(subset(near_surface_coeff_df, !is.na(DAP_Canopy_Height_r15m))) %>% 
  #Format so that you can add dates to the graph 
  mutate(month_ = lubridate::month(lubridate::as_date(case_when(month == "5" ~ "05/01/2021", 
                                                                month == "6" ~ "06/01/2021", 
                                                                month == "7" ~ "07/01/2021", 
                                                                month == "8" ~ "08/01/2021", 
                                                                month == "9" ~ "09/01/2021"), format = "%m/%d/%y"), label = T), 
         r_fixed = as.numeric(r_fixed), 
         class_ = as.factor(class), 
         class_ = fct_relevel(class_, "Soil Moisture vol %", after = 3), class_ = fct_relevel(class_, "Surface °C", before = "Near-Surface °C"), class_ = fct_relevel(class_, "Soil °C", before = "Surface °C"),
         model_type = "Monthly Mean") 

all_month_range_DAP <- full_join(subset(soil_moist_coeff_df_range, !is.na(DAP_Canopy_Height_r2m)), 
                                 subset(soil_coeff_df_range, !is.na(DAP_Canopy_Height_r15m))) %>%
  full_join(subset(surface_coeff_df_range, !is.na(DAP_Canopy_Height_r15m))) %>%  full_join(subset(near_surface_coeff_df_range, !is.na(DAP_Canopy_Height_r15m))) %>% 
  #Format so that you can add dates to the graph
  mutate(month_ = lubridate::month(lubridate::as_date(case_when(month == "5" ~ "05/01/2021", 
                                                                month == "6" ~ "06/01/2021", 
                                                                month == "7" ~ "07/01/2021", 
                                                                month == "8" ~ "08/01/2021", 
                                                                month == "9" ~ "09/01/2021"), format = "%m/%d/%y"), label = T), 
         r_fixed = as.numeric(r_fixed), 
         class_ = as.factor(class), 
         class_ = fct_relevel(class_, "Soil Moisture vol %", after = 3), 
         model_type = "Monthly Mean Daily Range") %>% 
  bind_rows(all_month_models_DAP)


## Figure Code
DAP_models_by_month_nocol <- ggplot(filter(all_month_models_DAP, model_type == "Monthly Mean"),
                                    aes( color = class_, pch = class_)) +
  geom_point(aes(month_, Estimate_all), size = 4) + geom_line(aes(month_, Conf_Estimate), size = 1) + 
  #geom_text(aes(month_, Estimate_all, label = round(r_fixed,2)), nudge_x = 0.35) + labs(color = "Month") +
  #viridis::scale_color_viridis(option = "C") + xlab("") + labs(color = expression("Model Adjusted R"^2)) +
  facet_wrap(~class_, ncol =2, scales = "free_y") +xlab("Month") + #ylim(c(-0.29, 0.019)) +
  ylab(expression("Slope Estimate ("~hat(beta)~") for Canopy Height")) +
  scale_color_manual(values=c("#990000", "#cc0000", "#FF3333", "#6699CC"), labels =  c("Soil (C°)", "Surface (C°)", "Near Surface (C°)", "ln(Soil Moisture (vol %))"))+
  
  scale_shape_manual(values = c(15, 16, 17, 18), labels = c("Soil (C°)", "Surface (C°)", "Near Surface (C°)", "ln(Soil Moisture (vol %))")) +
  geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
  theme_bw(base_size = 16) + theme(legend.position = "none")
DAP_models_by_month_nocol

r_by_month <- ggplot(filter(all_month_models_DAP, model_type == "Monthly Mean"), aes(month_, r_fixed, pch = class_, color = class_)) + geom_point(size = 4) +
  geom_line(size = 1, alpha = 0.6, linetype = "dashed") +  #, linetype = "dashed")  +
  ylab(expression(paste("Adjusted Model  ", R^{2}))) + xlab("Month") + labs(color = "Variable", shape = "Variable", pch = "Annual Variable") +
  scale_color_manual(values=c("#990000", "#cc0000", "#FF3333", "#6699CC"), labels =  c("Soil (C°)", "Surface (C°)", "Near Surface (C°)",
                                                                                       "Soil Moisture (vol %)"))+
  
  scale_shape_manual(values = c(15, 16, 17, 18), labels =  c("Soil (C°)", "Surface (C°)", "Near Surface (C°)",
                                                             "Soil Moisture (vol %)")) +
  theme_bw(base_size = 16)

r_by_month_leg <- get_legend(r_by_month)
r_by_month <- r_by_month + theme(legend.position = "none")
right_side <- plot_grid(r_by_month, r_by_month_leg, nrow = 2 )
monthly_mods_R <- plot_grid(DAP_models_by_month_nocol, right_side, ncol = 2, rel_widths = c(2,1))

#save_plot(filename = "D:/Data/SmithTripp/DAP_monthly_mods.jpg", plot = monthly_mods_R, base_height = 5.5, base_width = 8.5)


# Figure for all months (possible for SI) ---------------------------------

# ##facet_grid of all models for SI 
# all_month_models <- full_join(soil_moist_coeff_df, soil_coeff_df) %>%
#   full_join(surface_coeff_df) %>%  full_join(near_surface_coeff_df) %>% 
#   mutate(variable_simp = case_when(variable == "DAP_Canopy_Height_r2m" | variable == "DAP_Canopy_Height_r15m" ~ "Canopy Height",
#                                    variable == "elevation_r5m" | variable == "Elevation" ~ "Elevation", 
#                                    variable == "aspect.r10m_con" |variable == "aspect.r10m_con" ~ "Aspect", 
#                                    variable == "(Intercept)" ~"Intercept"), 
#          month_ = lubridate::month(lubridate::as_date(case_when(month == "5" ~ "05/01/2021", 
#                                                                 month == "6" ~ "06/01/2021", 
#                                                                 month == "7" ~ "07/01/2021", 
#                                                                 month == "8" ~ "08/01/2021", 
#                                                                 month == "9" ~ "09/01/2021"), format = "%m/%d/%y"), label = T),
#          r_fixed = as.numeric(r_fixed), 
#          class_ = as.factor(class), 
#          class_ = fct_relevel(class_, "Soil Moisture vol %", after = 3) )
# 
# 
# models_by_month <- ggplot(all_month_models) + 
#   geom_point(aes(month_, Estimate_all), size = 3) +geom_line(aes(month_, Conf_Estimate), size = 0.5) + 
#   ylab("Confidence Interval and Estimate for Model") + 
#   geom_text(aes(month_, Estimate_all, label = round(r_fixed,2)), nudge_x = 0.35) + labs(color = "Month") +
#   scale_color_brewer(palette = "Dark2") + xlab("Month") +
#   facet_grid(cols = vars(class_), rows = vars(variable_simp), scales = "free_y")+
#   ggtitle("Fixed Effects for Full Mean Model") +
#   geom_hline(yintercept = 0, linetype = "dashed", size = 1) + 
#   theme_bw(base_size = 20) + theme(legend.position = "bottom")
# models_by_month
