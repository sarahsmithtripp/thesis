##Written to digitize the calibration done in TMS Calibr Utility 
##Sarah Smith-Tripp 9-28-2020

# Set Working Directory to computer that you are working on (both 105)  --------
## If on .227 
#setwd("D:/Data/SmithTripp/Gavin_Lake")
## If .226 
#setwd("X:/SmithTripp/Gavin_Lake")

# Define soil type  -------------------------------------------------------

#clay, silt, sand 
# sand <- c(0,0, 100)
# loamy_sand_a <- c(3, 25, 72)
# loamy_sand_b <- c(5, 28, 67)
# sandy_loam_a <- c(5, 34, 61)
# sandy_loam_b <- c(7, 36, 57)
# loam <- c(25, 29, 47)
# silt_loam <- c(13, 66, 21)
# 
# soil_properties <- data.frame(sand, loamy_sand_a, loamy_sand_b,
#                               sandy_loam_a, sandy_loam_b,
#                               loam, silt_loam)

#classify possible soil types as per TMS3 use utilities  
soils <- c('loam','Loamy sand B','Sandy loam B','Loamy sand A ','loam','loam','loam','loam','Silt Loam','Silt Loam','Sandy loam B','Loamy sand B','Sandy loam B','Sandy loam B','Silt Loam','Sandy loam B','Sandy loam B','Sandy loam B','Sandy loam B','Sandy loam B','Sandy loam B','Sandy loam B','Sandy loam B','loam','Sandy loam B','loam','loam','Silt Loam','Sandy loam B','Silt Loam','Silt Loam','Sandy loam B','Silt Loam','loam','Sandy loam B','Silt Loam','Sandy loam B','Silt Loam','Silt Loam','loam','loam','loam','Silt Loam','Sandy loam B','Sandy loam B','Sandy loam B','loam','Silt Loam','Silt Loam','Silt Loam','Silt Loam','Sandy loam B','Silt Loam','Silt Loam','loam','Silt Loam','loam','loam','loam','Silt Loam','Silt Loam','Sandy loam B','Silt Loam','Silt Loam','Loamy sand A ','loam','Silt Loam','loam','Silt Loam','Sandy loam B','Silt Loam','loam','Silt Loam','Silt Loam','Silt Loam','Silt Loam','Silt Loam','loam','Silt Loam','Silt Loam','Sandy loam B','loam','Sandy loam B','Silt Loam','Silt Loam','Silt Loam','Silt Loam','Silt Loam','Silt Loam','Silt Loam','Silt Loam','Silt Loam')


soil_names <- unique(soils)
#define equation for calibration 

## these are defined for TMS soil types 
# sand_eq <- c(-0.000000003,	0.000161192,	-0.109956505)
# loamy_sand_a_eq <- c(-0.000000019,	0.00026561,	-0.154089291, "Loamy sand A")
# loamy_sand_b_eq <- c(-0.000000023,	0.000282473,	-0.167211156, "Loamy sand B")
# loam_eq <- c(-0.000000051,	0.000397984,	-0.291046437, "loam")
# sandy_loam_a_eq <- c(-0.000000038,	0.000339449,	-0.214921782)
# sandy_loam_b_eq <- c(-9e-10,	0.000261847,	-0.158618303, "Sandy loam B")
# silt_loam_eq <- c(0.000000017,	0.000118119,	-0.101168511, "Silt Loam")

## Here are the TMS user defined values for our soils 

TMS_userdef_Gavin_Lake <- read.csv("_Field-Collected-Data/Soil Data/GavinLake_Soils_SMCalibrModelCoeff.csv")
TMS_userdef_Gavin_Lake  <- as.data.frame(TMS_userdef_Gavin_Lake)
names(TMS_userdef_Gavin_Lake) <- c('%clay', '%silt', '%sand', '%total', 'a','b','c')
# soils_eq_df <- data.frame(TMS_SoilType = soil_names, 
#                           a = c(loam_eq[1], loamy_sand_b_eq[1], sandy_loam_b_eq[1], loamy_sand_a_eq[1], silt_loam_eq[1]), 
#                           b = c(loam_eq[2], loamy_sand_b_eq[2], sandy_loam_b_eq[2], loamy_sand_a_eq[2], silt_loam_eq[2]), 
#                           c = c(loam_eq[3], loamy_sand_b_eq[3], sandy_loam_b_eq[3], loamy_sand_a_eq[3], silt_loam_eq[3])) 

parameters_eq <- function(a,b,c, x){
  soil_moist <- a*(x)^2 + b*(x) - c
  return(soil_moist)
}

values <- seq(0, 3500, by = 60)
list_store <- list()
for(i in 1:length(TMS_userdef_Gavin_Lake$a)){
  print(i)
  name_a <- TMS_userdef_Gavin_Lake$a[i]
  name_b <- TMS_userdef_Gavin_Lake$b[i]
  name_c <- TMS_userdef_Gavin_Lake$c[i]
  out <- parameters_eq(a = name_a, b = name_b, c= name_c, x = values)

    list_store[[i]] <- data.frame(a = name_a, b = name_b, c= name_c, id = i, sm_count = values, sm_vol = out)
}

data_full <- as.data.frame(do.call(rbind, list_store))  #%>% left_join(TMS_userdef_Gavin_Lake) 
#names(data_full) <- c( "a", "b", "c", "id", "sm_count","sm_vol" ,"perc_clay_r1m", "perc_silt_r1m", "perc_sand_r1m",   "%total")  
ggplot(data_full) +
  geom_line(aes(sm_count, sm_vol, color = as.factor(id))) +
  xlab("Soil Moisture Count") +
  ylab("Soil Moisture Vol %") +
  labs(color = "Soil Type Unique ID") +
  scale_color_brewer(palette = "Paired")  +
  theme_bw()
