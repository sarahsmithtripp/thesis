#SST 
# Code written to parse the CSV files for microclimate loggers 
# Code is based on the outputs of the TOMST .csv files 
# 04 May 2020 -> sourced to the outputs for the initial microclimate test

library(tidyverse)
library(lubridate)
library(chron)
#setwd("/Volumes/My Passport/BackUps/JuneFieldWork_Loggers")
setwd("D:/Data/SmithTripp/Gavin_Lake/Microclimate_Measurements")
#Create a folder repository for the data that you would like to read in
file_oct.list <- dir(paste0(getwd(),"/October"), full.names = T)
file_ju.list <- dir(paste0(getwd(),"/June-July"), full.names = T)
file.list <- do.call(c, list(file_ju.list, file_oct.list))
f_list <- file.list[which(grepl("binary", file.list))]
print(f_list)

for (i in 1:length(f_list)){ 
  readin <- read.csv(f_list[[i]], sep = ";", stringsAsFactors = F, header = F)
  l <- length(which(grepl("2020[.]", readin$V3))) #define the length of the data frame
  x <- data.frame(Logger = rep(paste0(substr(readin[2,3], 11, 18)), l)) #create an association
  x$DateTime <- readin[which(grepl("2020[.]", readin$V3)),3]#Time of measurement
  x$DateTime <- parse_datetime(x$DateTime, "%Y.%m.%d %H:%M")
  x$TZ <- readin[which(grepl("2020[.]", readin$V3)),4] #Time Zone of Measurement 
  x$T1 <- readin[c(which(grepl("2020[.]", readin$V3)) + 1 ), 1] #T1, soil measurment (deg C)
  x$T2 <- readin[c(which(grepl("2020[.]", readin$V3)) + 1 ), 2] #T2, surface measurment (deg C)
  x$T3 <- readin[c(which(grepl("2020[.]", readin$V3)) + 1 ), 3] #T3, air measurment (deg C)
  x$SM_Count <- readin[c(which(grepl("2020[.]", readin$V3)) + 1 ), 4] # Soil moisture count, raw soil moisture data
  
  # Aggregate microclimate data into one dataframe
  if( i == 1){
    data <- x
    
  } else {
    data <- rbind(data, x)
  }
}


#pivot data to be in long format were temperature measurements are stacked 
data_T <- data %>% 
  pivot_longer(cols = c("T1", "T2", "T3"), names_to= "sensor", values_to = "temperature") 
data_T$temperature <- as.numeric(data_T$temperature)


## Not working with correction
time_correction <- read.csv("times_difference.csv")

#subset to within the study period (loggers are running all of the time)
data_sub <-  data_T %>%
  mutate(DateTime_GMT = with_tz(DateTime, tzone ="America/Los_Angeles") ,
                                month = month(DateTime_GMT)) %>% 
  mutate(hour = hour(DateTime_GMT)) %>% 
  group_by(month) %>% 
  mutate(mean = mean(temperature, na.rm = T)) %>% 
  filter(DateTime >= "2020-05-05")

ggplot(filter(data_sub, sensor == "T1"), aes(hour, temperature, group = as.factor(month))) + 
  geom_smooth(aes(color = as.factor(month))) + 
  xlab("Hour of the Day") + 
  labs(color = "Month of Recording") + 
  theme_bw()

### Read in Vegetation and soil data 
Veg_Data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Microclimate_SiteData(veg-soil)/Veg-Soil-PlotFireSev.csv")
Veg_Data$logger<- as.factor(Veg_Data$logger)

#block data by area because the flights are flown by area
areas <- c("upperplots", "lowerplots", "midplots", "oldguy")

Veg_Data <- Veg_Data  %>% 
  mutate(area = as.factor(dplyr::case_when( plot_num == 'fs2' | plot_num == 'fs3' ~ "lowerplots",
                                            plot_num == "fs1"| plot_num == "fs6" | plot_num == "3.52" | plot_num == "fs5" ~ "upperplots",
                                            plot_num == "fs8" | plot_num == "fs7" | plot_num == "fs10" ~ "midplots",
                                            plot_num == "fs4" ~ "oldguy")),
         month = month(DateTime)) %>% 
  group_by(month, Logger) %>%
  mutate(mean_temp_month = mean(as.numeric(temperature), na.rm = T ),
         mean_soil_moistre = mean(as.numeric(data_veg$SM_Count), na.rm = T))


data_checker <- function(data, plot) {
  subset <- subset(data, plot == plot)
  subset$Logger <- as.numeric(as.character(subset$Logger))
  c <- c(plot, length(unique(subset$month)), length(unique(subset$Logger)))
  data <- list(c, data.frame(subset))
  return(data)
}

one <- data_checker(data_veg, plot_names[1])


# Plot some data  ---------------------------------------------------------

library(cowplot)
ggplot(data_veg) +  
  geom_boxplot(aes(area, temperature, color = sensor)) + 
  ylim(0,40)+
  facet_wrap(~ as.factor(month)) + 
  theme_bw()
#geom_point(aes(month, mean(temperature, na.rm = T)))

ggplot(data_veg) + 
  geom_boxplot(aes(area, as.numeric(SM_Count), color = area)) + 
  facet_wrap(~as.factor(month)) + theme_bw()

ggplot(data_veg, aes(group = as.factor(plot))) +  
  geom_boxplot(aes(plot, temperature)) +
  geom_point(aes(plot, mean_temp_month, color = as.factor(point))) +
  ylim(0,40)+
  facet_wrap(~ as.factor(month)) + 
  theme_bw()
#geom_point(aes(month, mean(temperature, na.rm = T)))

ggplot(data_veg) + 
  geom_boxplot(aes(area, as.numeric(SM_Count), color = area)) + 
  facet_wrap(~as.factor(month)) + theme_bw()

###Exploring the use of the xts package, which can ve ysed to calculate over given time periods 

data.xts <- .xts(x = data_veg$temperature, index = data_veg$DateTime)
data_record_1 <- data_veg %>% 
  subset(sensor == "T3") %>% 
  subset(Logger = unique(data_veg$Logger)[1])
slope_function <- function(data, logger, period) { 
  logger_ts <- logger_ts[, c('temperature', 'DateTime')]
  slope <-c(logger_ts$temperature) - c()
  slope_calc <- rollapply(temperature, period, coef(lm(temperature ~ time)))
}

pkgs <- c(
  "tidyr", "lubridate", "dplyr", 
  "broom", "tidyquant", "ggplot2", "purrr", 
  "stringr", "knitr"
)
#install.packages("tidyquant")
library(tidyquant)
library(stringr)
library(purrr)
library(broom)
library(xts)

custom_stat_fun_2 <- function(x, na.rm = TRUE) {
  # x     = numeric vector
  # na.rm = boolean, whether or not to remove NA's
  
  m  <- mean(x, na.rm = na.rm)
  s  <- sd(x, na.rm = na.rm)
  hi <- m + 2*s
  lo <- m - 2*s
  
  ret <- c(mean_day = m, stdev = s, hi.95 = hi, lo.95 = lo) 
  return(ret)
}

library(dplyr)
standard_dev_day_df <- function(data){
  apply_data <- data %>% 
    mutate(day = day(DateTime)) %>%
    group_by(day, sensor) %>% 
    mutate(T_mean_day = mean(temperature, na.rm = T),
           T_sd_day = sd(temperature, na.rm = T),
           sm_sd_day = sd(SM_Count, na.rm = T))
  return(apply_data)
}

missing_data_function <- function(data, query) { 
  index <- which(grepl(data$paste0(query), "NA"))
  length
  return(summarized_daily_data)
}

#clean up to apply to just T1 
data_veg_T1 <- data_veg %>% 
  subset(sensor = "T1") %>% 
  dplyr::ungroup()
data_veg_list <- split(data_veg_T1 , data_veg_T1$Logger)


data_pre_filter <- lapply(data_veg_list, standard_dev_day_df) 
data_missing <- lapply(data_pre_filter, missing_data_function)
data_cbind <- do.call(rbind, data_pre_filter)
ggplot(data_cbind, aes(group = Logger)) + 
  geom_point(aes(DateTime, temp_pres)) 



## 
#remotes::install_github("giocomai/ganttrify")
library(ganttrify)
## one to test data filtering on 
known_err <- data_pre_filter[which(grepl("94203245", names(data_veg_list)))]$'94203245'
test <-  
  library(ggplot2)
ggplot(data_cbind %>% 
         filter(area == 'oldguy' & sensor == "T1"), aes(group = Logger)) + 
  geom_line(aes(DateTime, temperature, color = point)) +
  geom_line(aes(DateTime,T_mean_day))

ggplot(data_cbind %>% 
         filter(area == 'oldguy' & sensor == "T1"), # point == c('.2', '.3','.6','.9')), 
       aes(group = Logger)) + 
  geom_point(aes(DateTime, T_sd_day, color = point ))
ggplot(known_err) +
  #geom_line(aes(day, T_mean_day)) + 
  geom_point(aes(DateTime, sm_sd_day)) + 
  ylim(c(0,15))
