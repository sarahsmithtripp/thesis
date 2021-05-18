## Williams Lake Rainfall 2020 
list <- list.files("D:/Data/SmithTripp/Gavin_Lake/Microclimate_Measurements/Williams_Lake_Climate_data/",
                   full.names = T)

climate_data <- lapply(list, read_csv)

climate_data <- bind_rows(climate_data) %>% 
  mutate(week = lubridate::week(`Date/Time (UTC)`)) %>% 
  group_by(week) %>% 
  mutate(precip = sum(`Precip. Amount (mm)`, na.rm = T))

ggplot(climate_data) + 
  geom_point(aes(week, precip))
