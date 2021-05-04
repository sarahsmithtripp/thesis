##SST 
#Code written to define the metadata for submission of this dataset to the soil temp dataset 


## Not updated to run within soil temp metadata

## Files to add 
#Vegetation and Soil Data
#LAI data 
#CAnopy Metrics 
#Topography Metrics 
#Microclimate Locations 
library(rgdal)
library(raster)
library(tidyverse)

## Load base files  which is the microclimate locations 
microclimate_locations <- readOGR("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Model_Inputs/microclimate_loc.shp")
sub <- filter(microclimate_locations@data, plt_pnt == "fs107")
sub[2,c("plt_pnt")] <- "fs108"

microclimate_locations@data <- microclimate_locations@data %>% filter(plt_pnt != "fs107")
microclimate_locations@data <- rbind(microclimate_locations@data, sub)
microclimate_locations@data <-  microclimate_locations@data %>% 
  mutate(Plotcode = paste0("CA_ST_", .$plt_pnt), 
         Dataset_manager = "Sarah Smith-Tripp", 
         Email = "ssmithtr@mail.ubc.ca", 
         Institute = "University of British Columbia", 
         EPSG = 4326,
         Logger_make = "TOMST", 
         Logger_model = "TMS-4",
         Sensor_accuracy = 0.5, 
         Sensor_notes = "", 
         Start_date_month = 5, 
         Start_date_day = 5 , 
         Start_date_year = 2020, 
         End_date_month = 10, 
         End_date_year = 2020, 
         End_date_day = 8, 
         Temporal_resolution = 15,
         UTC_Local = 'local', 
         Species_composition = "No",
         Species_trait = "No",
         Habitat_type = 1, 
         Habitat_sub_type = 1, 
         Plot_size = "Radius of plot defined in headings of measurements (r#m) - variable", 
         Data_open_access = "Moratorium", 
         Meta_data_open_access = "Blurred_coordinates_only", 
         GPS_accuracy = XYAccrc, 
         Elevation = hgh_DEM, 
         Latitude = lat, 
         Longitude = long,
         plot_point = plt_pnt, 
         Disturbance_type = 'fire', 
         Disturbance_sev_r50m_NBR = X_firmn)
##Load Vegetation Data 
veg_soil_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Microclimate_SiteData(veg-soil)/Veg-Soil-PlotFireSev.csv")


summary_veg_data <- veg_soil_data %>% 
  mutate(plot_point = paste0(.$plot_num,.$point)) %>% 
  group_by(plot_point) %>% 
  summarize(mean_veg_cm_r1m = mean(Veg_depth_cm), 
            mean_lit_cm_r1m = mean(Lit_depth_cm), 
            bulk.density_r1m = bulk.density, 
            perc_sand_r1m = X.sand, 
            perc_clay_r1m = X.clay, 
            perc_silt_r1m = X.silt, 
            'Soil Type_r1m' = Soil.Type) %>% 
  distinct()


# step 1 - bind locations to veg data 
names_ordered <- c('Plotcode',	'Dataset_manager',	'Email',	'Institute',
                   'Latitude', 'Longitude', 'Elevation', 
                   'EPSG', 'GPS_accuracy','plot_point', 
                   'Logger_make', "Logger_model",'Sensor_accuracy',	'Sensor_notes',	'Start_date_year',	'Start_date_month',
                   'Start_date_day','End_date_year',	'End_date_month',	'End_date_day',	'Temporal_resolution',
                   'UTC_Local',	'Species_composition','Species_trait',	'Plot_size', 'Habitat_type',
                   'Habitat_sub_type', 'Disturbance_type', 'Disturbance_sev_r50m_NBR', 'Data_open_access',	'Meta_data_open_access') 
microclimate_locations_df <-  microclimate_locations@data[, names_ordered] %>% 
  left_join(summary_veg_data, by = "plot_point")


## step 2 bind LAI data 
LAI_data <- readxl::read_excel("D:/Data/SmithTripp/Gavin_Lake/LAI_Data/LAI_Plots_Clean.xlsx")

#clean naming between LAI and canopy covert to join 
LAI_data <- LAI_data %>% 
  mutate(plot_match = as.factor(case_when( Plot == '-60' ~ "60",
                                           Plot == '4.66' ~ '4.66',
                                           Plot == '3.52' ~ '3.52',
                                           Plot == '1.83' ~ '1.83', 
                                           Plot == '1.55' ~ '1.55',
                                           Plot == '3.66' ~ '3.66',
                                           Plot == 'OG' ~ 'oldguy', 
                                           Plot == 'CC' ~ 'cc' ,
                                           Plot == 'CONT' ~ 'cont',
                                           Plot == 'BTW' ~ 'bt')))

#Read in plots with fire severity to order the loggers 
#write sequence to name for numbers 
seq <- seq(from =1 , to = 10, by = 1)
fs_seq <- paste0("fs", seq)
plots <- rgdal::readOGR("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/Plots.shp")
plots@data <- plots@data %>% 
  arrange(X_firemean) %>% 
  mutate(plot_match = plot)

plots@data$plot_num <- factor(fs_seq, levels = fs_seq)
#Drop all things note needed from plots before joining 
plots_df <- plots@data[, c("plot_match",
                           "plot_num")]
LAI_data <-  LAI_data %>% 
  left_join(plots_df, by = "plot_match")  %>% 
  mutate(plot_point = paste0(plot_num,.$Point), 
         LAI_r2.5m = LAI)


microclimate_locations_df_1 <- left_join(microclimate_locations_df, LAI_data[, c('plot_point', 'LAI_r2.5m')])


## Step 3, join canopy data 
canopy_metrics <- read.csv("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Model_Inputs/canopy_metrics.csv", header=T)
canopy_metrics$X <- NULL


microclimate_locations_df_2 <- left_join(microclimate_locations_df_1, canopy_metrics)

## Step 4, join topography data 
terrain_metrics <- read.csv("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Model_Inputs/terrain_metrics.csv", header=T)
terrain_metrics$X <- NULL 


microclimate_locations_df_3 <- microclimate_locations_df_2 %>% left_join(terrain_metrics) %>% distinct()



## Minor data fixing 
source('D:/Data/SmithTripp/RFiles/thesis/Ortho_Alignment_accuracy.R', echo=F)
print(pixel_mean_err)

microclimate_locations_df_3_GPS <- microclimate_locations_df_3[is.na(microclimate_locations_df_3$GPS_accuracy),]
GPS_accuracy <- microclimate_locations_df_3_GPS %>% 
  mutate(GPS_acc = case_when(plot_point == "fs62" |plot_point == "fs91" ~ pixel_mean_err[4, c("accuracy_alignment")], 
                          plot_point == "fs83" | plot_point == "fs108" ~ pixel_mean_err[2, c("accuracy_alignment")], 
                         plot_point == "fs25" ~ pixel_mean_err[1, c("accuracy_alignment")]))
microclimate_locations_df_3[is.na(microclimate_locations_df_3$GPS_accuracy), c("GPS_accuracy")] <- c(GPS_accuracy$GPS_acc)


#write.csv(microclimate_locations_df_3, "D:/Data/SmithTripp/Gavin_Lake/CA_ST_SoilTempData/CA_ST_MetaData_27-Nov-2020.csv")

microclimate_meta <- read.csv("D:/Data/SmithTripp/Gavin_Lake/CA_ST_SoilTempData/CA_ST_MetaData.csv")
#terrain_data <- read.csv("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Model_Inputs/terrain_metrics.csv")
#terrain_data <- terrain_data %>% dplyr::select("plot_point", starts_with("Sol"))
canopy_max <- canopy_metrics %>% dplyr::select("plot_point", contains("Max"))
microclimate_meta <- left_join(microclimate_meta, canopy_max)
write.csv(microclimate_meta, "D:/Data/SmithTripp/Gavin_Lake/CA_ST_SoilTempData/CA_ST_MetaData.csv")
