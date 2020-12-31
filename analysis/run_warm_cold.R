#' title: EBS Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates plots of Alaska plaice, flathead sole, northern rock sole, and yellowfin sole
#'          from 2000-2018 based on the RACE EBS survey for warm vs. cold years
#' date: 2020-12-31
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

source(here("analysis", "functions_warm_cold.R"))
source(here("analysis", "data.R"))

# Visualize -----
# *** Foursquares ----
# make plotlists
akp_plotlist <- akp %>%
        make_length_bins() %>%
        make_mhw_factor_levels_nice() %>%
        make_foursquare_plotlist("Alaska Plaice", 
                                 ylimits_depth = c(0, 100),
                                 ylimits_temp = c(-2.5, 10))

fhs_plotlist <- fhs %>%
        make_length_bins() %>%
        make_mhw_factor_levels_nice() %>%
        make_foursquare_plotlist("Flathead Sole", 
                                 ylimits_depth = c(25, 225),
                                 ylimits_temp = c(-2, 7))

nrs_plotlist <- nrs %>%
        make_length_bins() %>%
        make_mhw_factor_levels_nice() %>%
        make_foursquare_plotlist("Northern Rock Sole", 
                                 ylimits_depth = c(10, 110),
                                 ylimits_temp = c(-2, 9))

yfs_plotlist <- yfs %>%
        make_length_bins() %>%
        make_mhw_factor_levels_nice() %>%
        make_foursquare_plotlist("Yellowfin Sole", 
                                 ylimits_depth = c(10, 120),
                                 ylimits_temp = c(-2, 10))

# arrange in foursquare
akp_foursquare <- ggarrange(plotlist = akp_plotlist, 
                            ncol = 2, 
                            nrow = 2)
fhs_foursquare <- ggarrange(plotlist = fhs_plotlist, 
                            ncol = 2, 
                            nrow = 2)
nrs_foursquare <- ggarrange(plotlist = nrs_plotlist, 
                            ncol = 2, 
                            nrow = 2)
yfs_foursquare <- ggarrange(plotlist = yfs_plotlist, 
                            ncol = 2, 
                            nrow = 2)

# annotate the foursquares
akp_foursquare_annotated <- annotate_figure(
        akp_foursquare, 
        top = text_grob(
                "Comparison of Length with Depth and Temperature Values for Alaska Plaice in warm vs. cold years",
                face = "bold",
                size = 20),
        bottom = text_grob(
                "Length (mm)",
                size = 16))
fhs_foursquare_annotated <- annotate_figure(
        fhs_foursquare, 
        top = text_grob(
                "Comparison of Length with Depth and Temperature Values for Flathead Sole in warm vs. cold years",
                face = "bold",
                size = 20),
        bottom = text_grob(
                "Length (mm)",
                size = 16))
nrs_foursquare_annotated <- annotate_figure(
        nrs_foursquare, 
        top = text_grob(
                "Comparison of Length with Depth and Temperature Values for Northern Rock Sole in warm vs. cold years",
                face ="bold",
                size =20),
        bottom = text_grob(
                "Length (mm)",
                size = 16))
yfs_foursquare_annotated <- annotate_figure(
        yfs_foursquare, 
        top = text_grob(
                "Comparison of Length with Depth and Temperature Values for Yellowfin Sole in warm vs. cold years",
                face = "bold",
                size = 20),
        bottom = text_grob(
                "Length (mm)",
                size = 16))

# *** Density Plots
# create density plots of each type
wc_depth_density <- coordinate_density_plots("length", "depth")
wc_temp_density <- coordinate_density_plots("length", "temp")

# turn those lists of density plots into foursquare plots
wc_depth_density_4square <- ggarrange(
        plotlist = wc_depth_density, 
        ncol = 2, 
        nrow = 2, 
        common.legend = TRUE, 
        legend = "bottom")
wc_temp_density_4square <- ggarrange(
        plotlist = wc_temp_density, 
        ncol = 2, 
        nrow = 2, 
        common.legend = TRUE, 
        legend = "bottom")

# add titles for density plots
wc_depth_density_4square <- annotate_figure(
        wc_depth_density_4square, 
        top = text_grob(
                "Comparison of Length vs. Depth values for warm and cold years",
                face = "bold",
                size = 14),
        bottom = "Depth (m)")
wc_temp_density_4square <- annotate_figure(
        wc_temp_density_4square, 
        top = text_grob(
                "Comparison of length vs. temperature values for warm and cold years",
                face = "bold",
                size = 14),
        bottom = "Gear Temperature (°C)")

# *** Save Outputs -----

# Alaska plaice 4square
ggsave(here("results/foursquare_wc", 
            "wc_akp_foursquare_annotated.tiff"),
       plot = akp_foursquare_annotated, 
       width = 400, 
       height = 400,
       units = "mm", 
       dpi = 300)
# flathead sole 4square
ggsave(here("results/foursquare_wc", 
            "wc_fhs_foursquare_annotated.tiff"),
       plot = fhs_foursquare_annotated, 
       width = 400, 
       heigh = 400,
       units = "mm", 
       dpi = 300)
# northern rock sole 4square
ggsave(here("results/foursquare_wc", 
            "wc_nrs_foursquare_annotated.tiff"),
       plot = nrs_foursquare_annotated, 
       width = 400, 
       height = 400,
       units = "mm", 
       dpi = 300)
# yellowfin sole 4square
ggsave(here("results/foursquare_wc", 
            "wc_yfs_foursquare_annotated.tiff"),
       plot = yfs_foursquare_annotated, 
       width = 400, 
       height = 400,
       units = "mm", 
       dpi = 300)

# density plots 
ggsave(here("results", 
            "foursquare_density_depth.tiff"),
       plot = wc_depth_density_4square, 
       width = 400, 
       height = 400,
       units = "mm", 
       dpi = 300)
ggsave(here("results", 
            "foursquare_density_temp.tiff"),
       plot = wc_temp_density_4square, 
       width = 400, 
       height = 400,
       units = "mm", 
       dpi = 300)
