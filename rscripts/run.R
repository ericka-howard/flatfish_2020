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

source("./rscripts/functions.R")
source("./rscripts/data.R")

# Analysis -----

# create bins for each species
akp <- coordinate_binning(akp)
fhs <- coordinate_binning(fhs)
nrs <- coordinate_binning(nrs)
yfs <- coordinate_binning(yfs)

# create boxplots of each type
ld <- coordinate_boxplots("length", "depth")
lt <- coordinate_boxplots("length", "temp")
dd <- coordinate_boxplots("depth", "depth")
tt <- coordinate_boxplots("temp", "temp")

# turn those lists of boxplots into foursquare plots
ld_4square <- ggarrange(plotlist = ld, ncol=2, nrow=2)
lt_4square <- print(ggarrange(plotlist = lt, ncol=2, nrow=2))
dd_4square <- print(ggarrange(plotlist = dd, ncol=2, nrow=2))
tt_4square <- print(ggarrange(plotlist = tt, ncol=2, nrow=2))


# *** Save Outputs -----

#Save your plot so you can use and find it later. 
ggsave(filename = "foursquare_lengthbins_depth.tiff", plot = ld_4square, width=400, height=400, units = "mm", dpi=300, path = "./output/")
ggsave(filename = "foursquare_lengthbins_temp.tiff", plot = lt_4square, width=400, height=400, units = "mm", dpi=300,path = "./output/")
ggsave(filename = "foursquare_depthbins_depth.tiff", plot = dd_4square, width=400, height=400, units = "mm", dpi=300,path = "./output/")
ggsave(filename = "foursquare_tempbins_temp.tiff", plot = tt_4square, width=400, height=400, units = "mm", dpi=300,path = "./output/")
