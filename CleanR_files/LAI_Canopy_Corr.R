##SST 
##Compare LAI to Canopy Height / Canopy Cover Metrics 
library(tidyverse)
library(cowplot)
library(betareg)

##Load data 
meta_data <- read_csv("_SoilTemp/CA_ST_MetaData_27-Nov-2020.csv")
LAI_data_clean <- readxl::read_excel("_Field-Collected-Data/Leaf_Area_Index_Licor_Data/LAI_Plots_Clean.xlsx")

##Join_data 
data <- LAI_data_clean%>% mutate(Plotcode = paste0("CA_ST_fs",Plotcode)) %>% left_join(meta_data) %>% 
  mutate(outlier = case_when(LAI < 4 ~ F, LAI > 4 ~ T))

##Add correlation to data 
lm_eqn <- function(y, x, df){
  m <- lm(y ~ x, df);
  eq <- substitute(~~italic(R)^2~"="~r2, 
                   list(r2 = format(summary(m)$r.squared, digits = 3)))
  # eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
  #                  list(a = format(unname(coef(m)[1]), digits = 2),
  #                       b = format(unname(coef(m)[2]), digits = 2),
  #                       r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}


## fit beta-reg because percent cover data 
# first convert because dataset includes extremes
data$DAP_Canopy_Cover_r15m_con <- (data$DAP_Canopy_Cover_r15m * (90 - 1) + 0.5)/90
data$DAP_Canopy_Cover_r2m_con <-  (data$DAP_Canopy_Cover_r2m * (90 - 1) + 0.5)/90

d <- data %>% filter(Plotcode %>% c("CA_ST_fs1"))
mean(filter(data, )$DAP_Canopy_Height_r15m)
#Spearman's rank correlation
cor.test(data$DAP_Canopy_Cover_r15m, data$DAP_Canopy_Height_r15m, method = "spearman")
cor.test(data$DAP_Canopy_Cover_r2m, data$DAP_Canopy_Height_r2m, method = "spearman")
cor.test(data$LAI, data$DAP_Canopy_Height_r15m, method = "spearman")
cor.test(data$LAI, data$DAP_Canopy_Height_r2m, method = "spearman")
# get models without outliers - plot in console to check mod assumptions 
m15 <- lm(LAI ~ DAP_Canopy_Height_r15m, filter(data, LAI < 4))
r15 <- summary(m15)$r.squared
m2 <- lm(LAI ~ DAP_Canopy_Height_r2m, filter(data, LAI <4))

# get text to get to put on scatter plot graph 
r_graph <- c(lm_eqn(filter(data, LAI < 4)$LAI, filter(data, LAI <4)$DAP_Canopy_Height_r15m, filter(data, LAI < 4)),
             lm_eqn(filter(data, LAI < 4)$LAI, filter(data, LAI < 4)$DAP_Canopy_Height_r2m, filter(data, LAI <4)))

## Graph without outliers 
mean_plot <- ggplot(chm_mean_field_clean, aes(x=chm_2m_10cmres, y = fieldhtmean_trees-1.6)) +
  geom_point(aes(shape = outlier), size = 3) +
  geom_line(aes(chm_2m_10cmres, pred_dirt_mod), linetype = "dotted", size = 1) +
  stat_smooth(data = filter(chm_mean_field_clean, outlier == F),method = "lm") +
  scale_linetype_manual(values=c("dotted", NA))+ 
  labs(shape = "") +
  ylab("Mean measured height (m)") +
  xlab("Mean DAP Height (m)")+
  xlim(0,35) + 
  ylim(0,35) +
  geom_abline(intercept = 0, slope=1) + 
  #ggthemes::scale_color_tableau(palette = "Classic Cyclic", na.value = T, drop = F) +
  #ggthemes::scale_fill_tableau(palette = "Classic Cyclic") + 
  labs(color = "Plot") + 
  cowplot::theme_cowplot() + ggtitle(c("Mean DAP vs. Field Ht"))

##Plot_data 
g15 <- ggplot(data, aes(DAP_Canopy_Height_r15m, LAI)) + 
  geom_point(aes(pch = outlier))+geom_text(x = 5 , y = 3, label = r_graph[1], size = 6,parse = T) +
  geom_smooth(data = filter(data, outlier == F), method = "lm") + theme_bw(base_size = 16) + 
  xlab("Canopy Height (m) - 15 m radius") 
g2 <- ggplot(data, aes(DAP_Canopy_Height_r2m, LAI)) +
  geom_point(aes(pch = outlier))+geom_text(x =5, y = 3, label = r_graph[2], size = 6,parse = T) +
  geom_smooth(data = filter(data, outlier == F), method = "lm") + theme_bw(base_size = 16) + 
  xlab("Canopy Height (m) - 2 m radius") + theme(legend.position = "none")

library(cowplot)
plot_grid(g2, g15, rel_widths = c(1, 1.4))
