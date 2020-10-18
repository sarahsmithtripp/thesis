#SST 
# Code written to parse the CSV files for microclimate loggers 
# Code is based on the outputs of the TOMST .csv files 
# 04 May 2020 -> sourced to the outputs for the initial microclimate test

library(tidyverse)
library(lubridate)
library(chron)
setwd("/Volumes/My Passport/BackUps/JuneFieldWork_Loggers")
#Create a folder repository for the data that you would like to read in
f_list <- list.files(path = ".","^binary_(.*)0.csv$") #read all binary files ending with 0 (i.e. the only measurement)

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

#subset to within the study period (loggers are running all of the time)
data_sub <-  data_T %>%
  mutate(month = month(DateTime)) %>% 
  group_by(month) %>% 
  mutate(mean = mean(temperature, na.rm = T)) %>%
  filter(DateTime >= "2020-05-05")


### Bind the data to the actual data frame heheh 
Veg_Data <- readxl::read_excel("/Volumes/My Passport/BackUps/Vegetation_Data_Clean.xlsx")
Veg_Data$Logger<- as.factor(Veg_Data$Logger)
Veg_Data$Plot <- as.factor(Veg_Data$Plot)


#merge data into one dataframe 
data_sub_veg <- left_join(data_sub, Veg_Data[,1:2], by = c("Logger"))
#add plot names to the veg data_Set 

plot_names <- c("cc", "4.66w","1.55w", "3.52w", "btw", "3.66w", "1.83w", "oldguy", "60.","cont")


#name the dividers used to delineate between plots 
plot_dividers <- c('w')

#Seperate the plot data 
plots <-  data_sub_veg%>% 
  tidyr::separate(col = "Plot", into = c("plot", "point"), sep = 'W', extra = "drop")


good_boy_plots<- filter(plots, !is.na(point))
## delineate plots that were not well named and have to be seperated by hand :( )
### write a function to deal with that bad boy data 
bad_plot_naming_sad <- function(delimiter, grabber, og_data) {
  data <- which(grepl(grabber, 
                      og_data$Plot))
  bad_boy_df <- og_data[data,]
  bad_boy_df_tidy <- tidyr::separate(bad_boy_df, col = "Plot", into = c("plot", "point"), sep = delimiter, extra = "drop")
  return(bad_boy_df_tidy)
}


### deal with that bad boy data god damn 
## clearcut
cc <- bad_plot_naming_sad(2, "CC", data_sub_veg)
##oldguy 
oldguy <- bad_plot_naming_sad(6, "OLDGUY", data_sub_veg)
## 60 
plot_60 <- bad_plot_naming_sad(3, "60.", data_sub_veg)
#cont 
cont <- bad_plot_naming_sad(4, "CONT", data_sub_veg)

bad_plots <- rbind(cc, oldguy, plot_60, cont)


#bind all data back into one data_frame 
data_veg <- rbind(bad_plots, good_boy_plots)

#block data by area because the flights are flown by area
areas <- c("upperplots", "lowerplots", "midplots", "oldguy")

data_veg <- data_veg  %>% 
  mutate(area = as.factor(dplyr::case_when( plot == '60.' | plot == 'CONT' ~ "lowerplots",
                                            plot == "1.55"| plot == "CC" | plot == "3.52" | plot == "4.66" ~ "upperplots",
                                            plot == "1.83" | plot == "3.66" | plot == "BT" ~ "midplots",
                                            plot == "OLDGUY" ~ "oldguy")),
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
