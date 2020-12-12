#' title: EBS Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates plots of Alaska plaice, flathead sole, northern rock sole, and yellowfin sole
#'          from 2000-2018 based on the RACE EBS survey
#' date: 2020-10-13
#' ---

# Libraries ---------

library(tidyverse)
library(sf)
library(magrittr)
library(viridis)
library(grDevices)
library(ggpubr)
library(here)

# Source Scripts -------

source(here("src", "analysis/functions.R"))
source(here("src", "analysis/data.R"))

# Analysis -----

# create bins for each species
akp <- make_length_bins(akp)
fhs <- make_length_bins(fhs)
nrs <- make_length_bins(nrs)
yfs <- make_length_bins(yfs)

# create boxplots of each type
# wc is warm vs cold years
# mhw is marine heat wave years (set in data_initial.R
# to be 2015, 2016, and 2019)
wc_depth <- coordinate_boxplots(groupvar = "wc", "depth")
wc_temp <- coordinate_boxplots(groupvar = "wc", "temp")
mhw_depth <- coordinate_boxplots(groupvar = "mhw", "depth")
mhw_temp <- coordinate_boxplots(groupvar = "mhw", "temp")

# create density plots of each type
wc_depth_density <- coordinate_density_plots("length", "depth")
wc_temp_density <- coordinate_density_plots("length", "temp")

# turn those lists of boxplots into foursquare plots
wc_depth_4square <- ggarrange(plotlist = wc_depth, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")
wc_temp_4square <- ggarrange(plotlist = wc_temp, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")
mhw_depth_4square <- ggarrange(plotlist = mhw_depth, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")
mhw_temp_4square <- ggarrange(plotlist = mhw_temp, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")

# turn those lists of density plots into foursquare plots
wc_depth_density_4square <- ggarrange(plotlist = wc_depth_density, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")
wc_temp_density_4square <- ggarrange(plotlist = wc_temp_density, ncol=2, nrow=2, common.legend = TRUE, legend = "bottom")

# add titles 
wc_depth_4square <- annotate_figure(wc_depth_4square, 
                              top= text_grob("Comparison of Length vs. Depth values for warm and cold years",
                                             face="bold",
                                             size=14),
                              bottom = "Length (mm)",
                              left = "Depth (m)")
wc_temp_4square <- annotate_figure(wc_temp_4square, 
                              top= text_grob("Comparison of length vs. temperature values for warm and cold years",
                                             face="bold",
                                             size=14),
                              bottom = "Length (mm)",
                              left = "Gear Temperature (°C)")
        
mhw_depth_4square <- annotate_figure(mhw_depth_4square, 
                             top= text_grob("Comparison of length vs. depth values for extreme marine heat wave years",
                                            face="bold",
                                            size=14),
                             bottom = "Length (mm)",
                             left = "Depth (m)")
        
mhw_temp_4square <- annotate_figure(mhw_temp_4square, 
                              top= text_grob("Comparison of length vs. temperature values for extreme marine heat wave years",
                                             face="bold",
                                             size=14),
                              bottom = "Length (mm)",
                              left = "Gear Temperature (°C)")

# add titles for density plots

wc_depth_density_4square <- annotate_figure(wc_depth_density_4square, 
                              top= text_grob("Comparison of Length vs. Depth values for warm and cold years",
                                             face="bold",
                                             size=14),
                              bottom = "Depth (m)")
wc_temp_density_4square <- annotate_figure(wc_temp_density_4square, 
                              top= text_grob("Comparison of length vs. temperature values for warm and cold years",
                                             face="bold",
                                             size=14),
                              bottom = "Gear Temperature (°C)")

# *** Save Outputs -----

ggsave(here("results", "foursquare_depth_warm_cold.tiff"),
       plot = wc_depth_4square, width=400, height=400,
       units = "mm", dpi=300)
ggsave(here("results", "foursquare_temp_warm_cold.tiff"),
       plot = wc_temp_4square, width=400, height=400,
       units = "mm", dpi=300)
ggsave(here("results", "foursquare_depth_marine_heat_wave.tiff"),
       plot = mhw_depth_4square, width=400, height=400,
       units = "mm", dpi=300)
ggsave(here("results","foursquare_temp_marine_heat_wave.tiff"),
       plot = mhw_temp_4square, width=400, height=400,
       units = "mm", dpi=300)

# density plots 
ggsave(here("results", "foursquare_density_depth.tiff"),
       plot = wc_depth_density_4square, width=400, height=400,
       units = "mm", dpi=300)
ggsave(here("results", "foursquare_density_temp.tiff"),
       plot = wc_temp_density_4square, width=400, height=400,
       units = "mm", dpi=300)
