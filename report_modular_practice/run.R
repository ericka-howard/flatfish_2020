#' title: EBS Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates plots of Alaska plaice, flathead sole, northern rock sole, and yellowfin sole
#'          from 2000-2018 based on the RACE EBS survey
#' date: 2020-10-13
#' ---

# Source Scripts -------

source("./functions.R")
source("./data.R")

# Analysis -----

# create bins for each species
akp <- coordinate_binning(akp)
fhs <- coordinate_binning(fhs)
nrs <- coordinate_binning(nrs)
yfs <- coordinate_binning(yfs)

# create four squares of each boxplot type
ld <- coordinate_boxplots("length", "depth")
lt <- coordinate_boxplots("length", "temp")
dd <- coordinate_boxplots("depth", "depth")
tt <- coordinate_boxplots("temp", "temp")


# *** Save Outputs -----

#Save your plot so you can use and find it later. 
ggsave(filename = "foursquare_lengthbins_depth.png", plot = ld, path = "./outputs/")
ggsave(filname = "foursquare_lengthbins_temp.png", plot = lt, path = "./outputs/")
ggsave(filename = "foursquare_depthbins_depth.png", plot = dd, path = "./outputs/")
ggsave(filename = "foursquare_tempbins_temp.png", plot = tt, path = "./outputs/")