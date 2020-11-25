## code written to parse the loggers based on known errors 

## SST 
## NOvember 2020
library(tidyverse)
library(zoo)

setwd("D:/Data/SmithTripp/Gavin_Lake/Microclimate_Measurements")
## read in microclimate data 
microclimate_veg_data <- read.csv("microclimate_veg_data.csv")





# Work with Loggers that had their lids removed  --------------------------


## take off measurements that had the lid removed
filter_me <- function(data, sensor){
  df <- data[,c('Logger', 'DateTime_GMT', 'sensor', 'temperature')]
  t1_seq <- which(grepl(paste0(sensor), df$sensor))
  df <- df[t1_seq,]
  df <- df[,c('Logger', 'DateTime_GMT', 'temperature')]
  names(df) <- c('Logger', 'DateTime_GMT', paste0(sensor))
  return(df)
}

T1 <- filter_me(microclimate_veg_data, 'T1')
T2 <- filter_me(microclimate_veg_data, 'T2')
microclimate_veg_data_long <- microclimate_veg_data %>% 
  filter(sensor == 'T3') %>%
  rename(T3 = temperature) %>% 
  select(-sensor) %>% 
  left_join(T1, by = c('Logger', 'DateTime_GMT')) %>% 
  left_join(T2, by = c('Logger', 'DateTime_GMT')) %>% 
  mutate(lid_lost = paste0(Comments.Oct,"_",Comments.june.july)) 

microclimate_veg_data_long$FID <- seq(1, dim(microclimate_veg_data_long)[1], by = 1)


## select loggers that lost their lids twice 
lid_lid <- which(grepl("lid_lid", microclimate_veg_data_long$lid_lost))

#june_july <- shield_function(microclimate_veg_data_long[1:100000,], 'Comments.june.july', lid_lid)
june_july <- which(grepl("lid", microclimate_veg_data_long$Comments.june.july))
june_july_double_lost <- which(june_july %in% lid_lid)
june_july_query <- june_july[-june_july_double_lost]
june_july_df <- microclimate_veg_data_long[june_july_query,]
good_bad_combine <- function(data, dates, near) { ## near is that your are shifting to the closer dates or those are the correct dates, and far is that they are prior dates you want to null
  if (near == F) {
    data_bad <- data %>%
      filter(DateTime_GMT < dates)
    data_bad$T3 <- NA
    data_good <- data %>%
      filter(DateTime_GMT >= dates)
    data_out <- rbind(data_bad, data_good)
  }
  ## are you shifting for earlier dates or later dates?
  
  else if (near == T) {
    data_bad <- data %>%
      filter(DateTime_GMT >= dates)
    data_bad$T3 <- NA
    data_good <- data %>%
      filter(DateTime_GMT < dates)
    data_out <- rbind(data_bad, data_good)
  }
}

june_july_clean <- good_bad_combine(june_july_df, c("2020-07-04 12:30:00"), near = F)


oct <- which(grepl("lid", microclimate_veg_data_long$Comments.Oct))
oct_double_lost <- which(oct %in% lid_lid)
oct_query <- oct[-oct_double_lost]
oct_df <- microclimate_veg_data_long[oct_query,]
oct_df_clean <- good_bad_combine(oct_df, c("2020-07-04 12:30:00"), near = T )

double_lost <- microclimate_veg_data_long[lid_lid,]
double_lost$T3 <- NA

lid_issues_clean <- rbind(double_lost, oct_df_clean, june_july_clean)

all_lid_issues <- c(oct_query, june_july_query, lid_lid)

data_good_lids <- microclimate_veg_data_long[-all_lid_issues, ]

data_lid_clean <- rbind(lid_issues_clean, data_good_lids)

missing_data <- microclimate_veg_data_long %>% 
  filter(FID %in% setdiff(microclimate_veg_data_long$FID, data_lid_clean$FID))

data_lid_clean <- rbind(data_lid_clean, missing_data)


# Work with loggers that were pulled out of the ground  -------------------
data_lid_clean$day <- lubridate::yday(data_lid_clean$DateTime_GMT)
data_lid_clean <- data_lid_clean %>% 
  group_by(Logger, day) %>% 
  mutate(soil_temp_sd = sd(T1, na.rm = T)) %>% 
  arrange(soil_temp_sd)

pulled_out <- which(grepl("pulled", data_lid_clean$lid_lost))
pulled_out_df <- as.data.frame(data_lid_clean[pulled_out,])



# ggplot(pulled_out_df, aes(DateTime_GMT, T1, group = as.factor(Logger))) + 
#          geom_point(aes(color= as.factor(Logger))) + 
#   geom_vline(xintercept = as.numeric(as.Date('2020-07-04 12:20:00'))) +
#   facet_wrap(~as.factor(Logger))


bad_loggers <- unique(pulled_out_df$Logger)

find_bad_days <- function(bad_logger, data){
  data_logger <- data %>% filter(Logger == bad_logger)
  data_logger_sum <- data_logger %>% 
    group_by(day) %>% 
    summarize(soil_temp_sd = mean(soil_temp_sd))
  rolled <- rollmean(zoo(data_logger_sum$soil_temp_sd, order.by = data_logger_sum$day), 7, aligng = c("right"), na.pad = T)
  data_logger_sum$soil_tempmean_sd <- rolled
  plots <- ggplot(data_logger_sum, aes(day, soil_tempmean_sd)) + geom_line()
  return(list(data_logger_sum, plots, data_logger))
}

loggers <- lapply(bad_loggers, find_bad_days, data = pulled_out_df)
for(i in 1:length(bad_loggers)){
  print(min(loggers[[i]][[1]]$day))
  print(bad_loggers[i])
  print(loggers[[i]][[2]])
}

### manaully define good and bad days  based on graphs produced above
good_days <- c(185, 185, 185, 204, 184, 183, NA, 184, 184, 184)
above_below <- c(F, F, F, T, F, F, '', F,F, F)

logger_good_days <- data.frame(bad_loggers, good_days, above_below)

remove_bad_days <- function(df, good_day, above_below) {
  if(above_below == F) {
    logger_below <- df %>% filter(day < good_day)
    logger_below[,c('SM_Count', 'T1', 'T2','T3')] <- NA
    logger_above <- df %>% filter(day >= good_day)
    logger_clean <- as.data.frame(rbind(logger_above, logger_below))
  }
 else if(above_below == T) { 
    loggers_bad <- df %>% filter(day > good_day)
    loggers_bad[,c('SM_Count', 'T1', 'T2', 'T3')] <- NA
    logger_good <- df %>% filter(day > good_day)  
    logger_clean <- as.data.frame(rbind(loggers_bad, logger_good))
    }
 
}




logger_clean <- list()
for(i in 1:length(bad_loggers)) { 
  good_day <- logger_good_days[i, 2]
  above_below_log <- logger_good_days[i, 3]
  data <- remove_bad_days(loggers[[i]][[3]], good_day, above_below_log)
  logger_clean[[i]] <- data 
}

loggers[[7]][[3]][c('SM_Count', 'T1', 'T2', 'T3')] <- NA

logger_clean[[7]] <- loggers[[7]][[3]]



clean_logger_data <- bind_rows(logger_clean) %>% 
  distinct()


missing_data <- pulled_out_df %>% 
  filter(FID %in% setdiff(pulled_out_df$FID, clean_logger_data$FID))

missing_data[,c('SM_Count', 'T1', 'T2', 'T3')] <- NA

clean_logger_data <- rbind(missing_data, clean_logger_data)

data_not_pulled <- data_lid_clean[-pulled_out, ]


### bind together and write to a CSV because we have a clean mo-fo dataset!!!!!!!!!!!!!!

data_clean <- rbind(clean_logger_data, data_not_pulled)
data_clean <- distinct(data_clean)

find_dups <- data_clean %>% 
  group_by(FID) %>% 
  summarize(n = n()) %>% 
  filter(n >= 2)


dups <- data_clean %>% 
  filter(FID %in% find_dups$FID) %>% 
  filter(is.na(T3)) ### select just the T3 data because that is the only one that is wrong 

data_clean_no_dups <- data_clean %>% 
  filter(!FID %in% dups$FID)


data_finally_clean <- rbind(dups, data_clean_no_dups)


names(data_finally_clean)

data_finally_clean[,c('X', 'lid_lost','mean')] <- list(NULL)
#data_finally_clean$DateTime_GMT <- lubridate::ymd_hms(data_finally_clean$DateTime_GMT)
#write.csv(data_finally_clean, "microclimate_veg_data_clean.csv")




# Format data for submission to the soil temp database  -------------------
bad_data <- which(is.na(data_finally_clean$plot_point))
bad_data_df <- data_finally_clean[bad_data, c("T1", "T2", "T3", "SM_Count", "DateTime_GMT", "month", "Logger", "Original.Logger", "Comments.june.july", "FID")]

simple_data <- data_finally_clean[-bad_data, c("plot_point", "T1", "T2", "T3", "SM_Count", "DateTime_GMT", "month", "Logger", "Original.Logger", "Comments.june.july", "FID")]

bad_data_df <- bad_data_df %>%
  mutate(plot_point = case_when(Logger == 94203211 ~ 'fs96', 
         Logger == 94203258 ~ 'fs78', 
         Logger == 94203265 ~ 'fs106'))
library(lubridate)
get_time <- function(time) {
  time %>%
    str_split(" ") %>%
    map_chr(2) %>%
    hms()
}

simple_data_plots <- simple_data %>%
  full_join(bad_data_df, by = c("plot_point", "T1", "T2", "T3", "SM_Count", "DateTime_GMT", "month", "Original.Logger", "Comments.june.july", "FID")) %>%
  mutate(DateTime_GMT = lubridate::ymd_hms(DateTime_GMT), 
         Day = lubridate::mday(DateTime_GMT), 
         Plotcode = paste0('CA_ST_', .$plot_point), 
         #Time = lubridate::hms(DateTime_GMT), 
         Year = lubridate::year(DateTime_GMT),
        Time = get_time(DateTime_GMT)) %>% 
  filter(DateTime_GMT >= "2020-05-13")

## Read in soil data

soil_data_TMS <- readxl::read_excel("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Microclimate_SiteData(veg-soil)/Sample_Sites_Soil.xlsx", sheet = "Sheet1")
soil_data_TMS <- soil_data_TMS[,c('TMS_SoilType','logger...28')]
names(soil_data_TMS) <- c('TMS_SoilType','Logger.x') ## rename soil data to join

sm_logger.x.fickle <- simple_data_plots %>% filter(Logger.x !='NA') %>% filter(Logger.x > 94203290) %>% filter(DateTime_GMT > "2020-07-03")
sm_logger.x <- simple_data_plots %>% filter(Logger.x != 'NA')  %>% filter(Logger.x <= 94203290) %>%  full_join(sm_logger.x.fickle) %>% left_join(soil_data_TMS)
names(soil_data_TMS) <- c('TMS_SoilType', 'Logger.y') ## rename soil data to join to the older and now replaced loggers 
sm_logger.y <- simple_data_plots %>% filter(Logger.y !='NA') %>% left_join(soil_data_TMS)
sm_data_soil <- rbind(sm_logger.x, sm_logger.y)


### run script to get model coefficients 
source('D:/Data/SmithTripp/RFiles/thesis/SoilMoisture_Calibration.R', echo=F)


sm_data_soils <- left_join(sm_data_soil, soils_eq_df) 
dim(sm_data_soils) 
sm_data_soils[,c('a','b','c')] <- apply(sm_data_soils[,c('a','b','c')], 2, as.numeric)

### calibrate using provided TMS codes 
sm_data_soils$vol_sm <-  sm_data_soils$a*(sm_data_soils$SM_Count)^2 + sm_data_soils$b*(sm_data_soils$SM_Count) - sm_data_soils$c
sm_data_soils <- sm_data_soils %>% ## drop uncessary columns 
  select(-c('a','b','c'))

dim(sm_data_soils)


# Drop times that have duplicate measurements  ----------------------------
count_measures <- sm_data_soils %>% 
  group_by(DateTime_GMT, Plotcode) %>% count() %>% 
  filter(n > 1) %>% 
  left_join(sm_data_soils) 
  #distinct()

data_without_double_coundts <- sm_data_soils %>% filter(!FID %in% count_measures$FID)

count_measures_distinct <- count_measures %>% select(-c(FID)) %>% distinct() %>% group_by(DateTime_GMT, Plotcode) %>% count() %>% 
  filter(n>1) %>%
  left_join(sm_data_soils) %>% 
  filter(T1 < 40 & T1 > -10 & T2 < 40 & T2 > -10 & T3 < 55 & T3 > -15)
graph <- ggplot(filter(count_measures_distinct)) + #, Plotcode == "CA_ST_fs88")) + 
  geom_point(aes(DateTime_GMT, T1, color = Plotcode), size = 0.5, shape = 16) + 
  geom_point(aes(DateTime_GMT, T2, color = Plotcode), size = 0.5, shape = 17) + 
  geom_point(aes(DateTime_GMT, T3, color = Plotcode), size = 0.5, shape = 18) + 
  #scale_x_datetime(date_labels = "%B") +
  theme_bw() + 
  ylab("Temperature")
graph


#following these explorations I have decided all of this data is absolutely useless and we are better off without it 

sm_data_soils <- data_without_double_coundts
## Drop columns that do not need to be in the dataset 

simple_data_part <- sm_data_soils %>% 
  select(-c("Original.Logger", "Logger.y", "Logger.y", "plot_point"))


write.csv(simple_data_part, "Microclimate_filtered_Vol_Sm_Nov-22-20.csv")
