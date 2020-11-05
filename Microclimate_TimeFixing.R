## Written to correct the time measurements of the microclimate loggers 
## takes the time that the file was created and then cross references 
## to the last recording of the logger. 


library(lubridate)
library(dplyr)
## SST 2020-10-28
setwd("D:/Data/SmithTripp/Gavin_Lake/Microclimate_Measurements")

file.list <- dir(paste0(getwd(),"/October"), full.names = T)
command_oct.list <- file.list[which(grepl("command", file.list))]
file.list <- dir(paste0(getwd(),"/June-July"), full.names = T)
command_ju.list <- file.list[which(grepl("command", file.list))]
gettimes <- function(logger_file) { 
  logger1 <- strsplit(logger_file, c("d_")) 
  logger2 <- strsplit(logger1[[1]][2], ".c")
  logger3 <- logger2[[1]][1]
  
  ## get time on logger 
  readin <- read.csv(logger_file, sep = ";", stringsAsFactors = F, header = F,skipNul = TRUE)
  readin_sub <- readin[which(grepl("@C=", readin$V1)),]
  last_time1 <- strsplit(as.character(readin_sub[1]), "=")[[1]][2] # length(which(grepl("2020[.]", readin$V3))) #define the length of the data frame
  last_time2 <- strsplit(last_time1, "-")[[1]][1]
  last_time_measure <- parse_datetime(last_time2, "%Y/%m/%d, %H:%M:%S") # format to match file output
  mtime <- file.info(logger_file)$mtim## If looking for the creation change to ctim
  readin_time <- c(logger3, as.character(mtime), as.character(last_time_measure))
  return(readin_time)
}





times_oct <- lapply(command_oct.list, gettimes)
times_ju <- lapply(command_ju.list, gettimes)
modified_oct_times <- as.data.frame(do.call(rbind, times_oct))
modified_ju_times <- as.data.frame(do.call(rbind, times_ju))
modified_times <- as.data.frame(rbind(modified_ju_times, modified_oct_times))
names(modified_times) <- c("logger", "comp_time", "logger_time")
modified_times[,c("comp_time", "logger_time")]  <- sapply(modified_times[,c("comp_time", "logger_time")], strptime,"%Y-%m-%d %H:%M:%S" )
View(modified_times)


modified_times$diff_time <- difftime(modified_times$comp_time, modified_times$logger_time, units = c("mins"))
View(modified_times)

ggplot(modified_times, aes(x = as.numeric(diff_time), group = as.factor(month(logger_time)))) + 
  geom_density(aes(fill = as.factor(month(logger_time))), alpha = 0.5) + 
  theme_bw() + 
  xlab("Difference between computer time and logger time (min)") + 
  labs(fill = "Month of Data Collection")


#store as CSV to read-in for and correct later data analyses 
#write.csv(modified_times, "times_difference.csv")



# Calculate expected number of measurements  ------------------------------

names(modified_oct_times) <- c("logger", "oct_comp_time", "oct_logger_time")
names(modified_ju_times) <- c("logger", "ju_comp_time", "ju_logger_time")

expected_values <- modified_ju_times %>% 
  inner_join(modified_oct_times, by = "logger") %>% ## Change this to apply to all loggers but honestly fifty is enough for me! 
  mutate(time_btw_data = difftime(ju_comp_time, oct_comp_time, units = c("mins")), 
         expect_measurements = round(time_btw_data/15, 0))


#write.csv(expected_values, "expected_number_measurements.csv")
