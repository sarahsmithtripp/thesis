##Written to digitize the calibration done in TMS Calibr Utility 
##Sarah smtih-Tripp 9-28-2020



# Define soil type  -------------------------------------------------------

#clay, silt, sand 
sand <- c(0,0, 100)
loamy_sand_a <- c(3, 25, 72)
loamy_sand_b <- c(5, 28, 67)
sandy_loam_a <- c(5, 34, 61)
sandy_loam_b <- c(7, 36, 57)
loam <- c(25, 29, 47)
silt_loam <- c(13, 66, 21)

soil_properties <- data.frame(sand, loamy_sand_a, loamy_sand_b,
                              sandy_loam_a, sandy_loam_b,
                              loam, silt_loam)



#define equation for calibration 

sand_eq <- c(-0.000000003,	0.000161192,	-0.109956505)
loamy_sand_a_eq <- c(-0.000000019,	0.00026561,	-0.154089291)
loamy_sand_b_eq <- c(-0.000000023,	0.000282473,	-0.167211156)
loam_eq <- c(-0.000000051,	0.000397984,	-0.291046437)
sandy_loam_a_eq <- c(-0.000000038,	0.000339449,	-0.214921782)
sandy_loam_b_eq <- c(-9e-10,	0.000261847,	-0.158618303)
silt_loam_eq <- c(0.000000017,	0.000118119,	-0.101168511)

soil_eq <- data.frame(sand_eq, loamy_sand_a_eq, loamy_sand_b_eq,
                      sandy_loam_a_eq, sandy_loam_b_eq,
                      loam_eq, silt_loam_eq)

parameters_eq <- function(a,b,c){
  soil_moist <- a*(x)^2 + b*(x) - c
}
