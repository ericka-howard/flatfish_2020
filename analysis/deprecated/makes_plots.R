library(tidyverse)
library(sf)
library(magrittr)
library(viridis)
library(here)

akp <- read_rds("./new_slushpile/intermediates/akp.rds")
fhs <- read_rds("./new_slushpile/intermediates/fhs.rds")
nrs <- read_rds("./new_slushpile/intermediates/nrs.rds")
yfs <- read_rds("./new_slushpile/intermediates/yfs.rds")

make_contour_plot <- function(df, df_name, y, y_name){
  yvar <- enquo(y)
  p <- ggplot(df)+
    geom_density2d(aes(x=length, y=!!yvar))+
    theme_light()+
    ggtitle(paste(df_name, y_name))+
    scale_fill_viridis()
  return(p)
}

p_nrs_temp <- make_contour_plot(nrs, "nrs", gear_temperature, "gear_temperature")
p_nrs_depth <- make_contour_plot(nrs, "nrs", bottom_depth, "bottom_depth")








#Save your plot so you can use and find it later. 
ggsave(filename = "foursquare_lengthbins_depth.tiff", 
       plot = ld_4square, width=400, height=400, 
       units = "mm", dpi=300, path = "./output/")
ggsave(filename = "foursquare_lengthbins_temp.tiff", 
       plot = lt_4square, width=400, height=400, 
       units = "mm", dpi=300,path = "./output/")
ggsave(filename = "foursquare_depthbins_depth.tiff", 
       plot = dd_4square, width=400, height=400, 
       units = "mm", dpi=300,path = "./output/")
ggsave(filename = "foursquare_tempbins_temp.tiff", 
       plot = tt_4square, width=400, height=400, 
       units = "mm", dpi=300,path = "./output/")