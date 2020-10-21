#Sarah Smith-Tripp
#Code written to read in XML data LEICA and output into a CSV 
## Two CSVs are written, one is called CG_points, which is in WGS UTM coords 
### The second is the CSV that includes point innacuracies - output is WGS 84 Lat/Lon
## Ground Control Points are written seperately from CSV points 
### update on July 22, data is cleaned and extraneous plot level points are removed 


library(xml2)
setwd("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/GPX_Waypoints")
#alex_frazer <- read_xml("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Sample_Location/alexfraser_UTM_Leica.xml")
alex_frazer <- read_xml("D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/tree_data/alexfraser.xml")
data <- as_list(alex_frazer)
data$LandXML$CgPoints

GPS_pts <- data$LandXML$Survey$GPSSetup
GPS_pts <- data$LandXML$HexagonLandXML
ts <- data$LandXML$CgPoints
time_out <- function(list, attr) {
  ts <- attr(list, attr)
  name <- attr(list, 'name')
  c <- c(name, ts)
  return(c)
}

time <- lapply(ts, time_out, attr = 'timeStamp')
time <- as.data.frame(do.call(rbind, time))

time_setup <- lapply(dat, time_out, attr = 'timeStamp')
Cg_pts <- data$LandXML$CgPoints
Cg_pts_list <- lapply(Cg_pts, attributes)
parse_cg <-  function(list_attributes) { 
  name <- list_attributes$CgPoint$oID
  lat <- list_attributes$CgPoint$latitude
  long <- list_attributes$CgPoint$longitude
  time <- list_attributes$CgPoint$timeStamp
  data <- list(name, lat, long, time)
  return(data)
} 


for(i in 1:length(Cg_pts_list)) { 
  list <- Cg_pts_list[[i]]
  name <- list$name
  lat <- list$latitude
  long <- list$longitude
  time <- list$timeStamp  #create an association
  
  x <- cbind(name, lat, long, time) 
  if( i == 1){
    data <- x
  } else {
    data <- rbind(data, x)
  }
}

#subset to select just tree points 
tree_points <- which(grepl("tree", data[,1]))
tree_point_1 <- which(grepl("trree", data[,1]))
tree_points <- c(tree_points, tree_point_1)
#remember the plot 12, 10, 11 and are based on locations of loggers 
#plot 11 is 60.5 and 12 is cont6, 10 is 15 meters south of cont 9 
p60.5 <- which(grepl("60.5", data[,1]))
pcont6 <- which(grepl("cont6", data[,1]))

tree_points <- c(tree_points, p60.5, pcont6)
tree_plots <- data[tree_points, ]
#export data 
write.csv(tree_plots, file = "D:/Data/SmithTripp/Gavin_Lake/Field_SiteData/Tree_Data/treeplot_locations_redo.csv")
write.csv(data, file = "GIS_Points.csv")

## Seperate GC points 
data <- as.data.frame(data)
gc_pts <- which(grepl("gc", data$name))
gc_pts <- data[c(gc_pts),]

write.csv(gc_pts, file = "GC_pts.csv")

##Dealing with the GPS points 
names <- lapply(GPS_pts, attributes)
for(i in 1:length(GPS_pts[2:211])) {
  name <- names[[i + 1]]$uniqueID
  list <- GPS_pts[[i + 1]]
  coords <- list$Coordinates$WGS84$Geodetic
  lat <- attributes(coords)$lat
  long <- attributes(coords)$lon
  hght <- attributes(coords)$hghtE
  XYAccuracy <- attributes(list$PointQuality)$CQPos
  x <- cbind(name, lat, long, hght, XYAccuracy)
  length = length(x)
  if (i == 1) {
    points <- x
  }
  else{
    if (length == 4) {
      next() #removes points that are irrelevant 
    }
    points <- rbind(points, x)
  }
}
points <-as.data.frame(points)
names(time) <- c("name", "timeStamp")
points <- dplyr::left_join(points, time, by = "name")
length(points$name)
gc_pts_accuracy <- which(grepl("gc", points$name))
plot_points <- points[c(is.na(match(seq(1:length(points$name)),gc_pts_accuracy))),] ## Subset to points within plots
gc_pts_accuracy <- points[c(gc_pts_accuracy),]


# export data  ------------------------------------------------------------


# ## Set option for # of digits very high so that it does not round csv output 
# options(digits = 22)
# write.csv(gc_pts_accuracy, file = "gc_pts_accuracy.csv")
# grab missing points 
#p60.5, bt7, 3.52 #1 (named as upper plots gc 4 @2.95(verified with GIS))
p60.5 <- which(grepl("60.5", data[,1]))
bt7 <- which(grepl("btw7", data[,1])) 
# check <- select 12 
bt7 <- 12
p3.52.1 <- which(grepl("upperplotsgc4@295", data[,1]))
missing_points <- data[c(p60.5, bt7, p3.52.1),]
#export data 
#write.csv(missing_points, file = "missingmicroclimate_pts.csv")

# #set the option back to something more reasonable 
# options(digits = 15)
# 

# end ---------------------------------------------------------------------

# update july 22 — clean the microclimate logger locations and wri --------
d <- plot_points


plot_names <- c("cc", "4.66w","1.55w", "3.52w", "btw", "3.66w", "1.83w", "oldguy", "60.","cont")

#remove extraneous points by selecting those specific to plots
query <- which(grepl(paste(plot_names, collapse = "|"), d$name))
d_plots <- d[query,]

#deal with points taken multiple times 
query_multi_pts <- which(grepl("@", d_plots$name))
d_multi_pts <- d_plots[query_multi_pts,]
#innacurate_pts <- which(d_multi_pts$XYAccuracy >1 )
#innacurate_pts <- d_multi_pts[innacurate_pts, ]
require(dplyr)
innacurate_pts <- d_multi_pts %>%
  rbind(d_plots) %>%
  tidyr::separate(col = "name", into = c("name", "time"), "@") 
#generally the second value is removed because the first value
#is assumed to be the one taken at the point, and the second value was an accident 
correct <- c(68, 39, 52, 18, 23,10,167, 165,114,132,108,129)
selected_correct_pts <- innacurate_pts[which (innacurate_pts$X %in% correct),]

#None of the selected symbols are taken at later times (again this is expected)
## thus I perform the same seperation and select those where time is equal to "NA"

filtered_pts <- d_plots %>%
  tidyr::separate(col = "name", into = c("name", "time"), "@")# %>%
filtered_pts <-  filter(filtered_pts, is.na(time))


#create a separate column for the plots 
plots_no_ext <- d_plots %>%
  tidyr::separate(col = "name", into = c("name", "time"), "@")# %>%
plots_no_ext <- plots_no_ext %>% 
  filter(is.na(time)) %>%
  mutate("pointname" = name)

#name the dividers used to delineate between plots 
plot_dividers <- c('w')
plots <- plots_no_ext %>% 
  tidyr::separate(col = "name", into = c("plot", "point"), sep = 'w', extra = "merge")


good_boy_plots<- filter(plots, !is.na(point))
## delineate plots that were not well named and have to be seperated by hand :( )
### write a function to deal with that bad boy data 
bad_plot_naming_sad <- function(delimiter, grabber, og_data) {
  data <- which(grepl(grabber, 
                      og_data$name))
  bad_boy_df <- og_data[data,]
  bad_boy_df_tidy <- tidyr::separate(bad_boy_df, col = "name", into = c("plot", "point"), sep = delimiter, extra = "merge")
  return(bad_boy_df_tidy)
}

### deal with that bad boy data god damn 
## clearcut
cc <- bad_plot_naming_sad(2, "cc", plots_no_ext)
##oldguy 
oldguy <- bad_plot_naming_sad(6, "oldguy", plots_no_ext)
## 60 
plot_60 <- bad_plot_naming_sad(3, "60.", plots_no_ext)
#cont 
cont <- bad_plot_naming_sad(4, "cont", plots_no_ext)

bad_plots <- rbind(cc, oldguy, plot_60, cont)


#bind poorly named plots to the main dataframe
plots_all <- rbind(bad_plots, good_boy_plots)
#remove extraneous points ... again 
ext_pts <- which(nchar(plots_all$point) > 3)
plots_all <- plots_all[-ext_pts,]

#classify plot as a factor 
plots_all$plot <- as.factor(plots_all$plot)

write.csv(plots_all, file = "microclimate_locations_WGS84.csv")

