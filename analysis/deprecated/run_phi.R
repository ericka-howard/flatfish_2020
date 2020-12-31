#' title:   Eastern Bering Sea Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates contour and box plots of Alaska plaice, flathead sole,
#'          northern rock sole, and yellowfin sole from 2000-2018 based on the RACE EBS
#'          survey
#' date:    2020-11-24
#' author: Ericka B. Smith
#' ---


# Set environment ----

# *** Libraries ----

library(tidyverse)
library(sf)
library(magrittr)
library(viridis)
library(grDevices)
library(ggpubr)
library(here)

# *** Source Scripts ----
source(here("analysis", "functions_phi.R"))
source(here("analysis", "data_phi.R"))

# *** Remove NAs for just phi ----
akp <- akp[!is.na(akp$phi), ]
fhs <- fhs[!is.na(fhs$phi), ]
nrs <- nrs[!is.na(nrs$phi), ]
yfs <- yfs[!is.na(yfs$phi), ]

# Visualize -----
# make plotlists
akp_plotlist <- akp %>%
  make_length_bins() %>%
  make_factor_levels_nice() %>%
  make_plotlists("Alaska Plaice",
                 ylimits_phi = c(0, 7))

fhs_plotlist <- fhs %>%
  make_length_bins() %>%
  make_factor_levels_nice() %>%
  make_plotlists("Flathead Sole",
                 ylimits_phi = c(25, 225))

nrs_plotlist <- nrs %>%
  make_length_bins() %>%
  make_factor_levels_nice() %>%
  make_plotlists("Northern Rock Sole",
                 ylimits_temp = c(-2, 9))

yfs_plotlist <- yfs %>%
  make_length_bins() %>%
  make_factor_levels_nice() %>%
  make_plotlists("Yellowfin Sole",
                 ylimits_temp = c(-2, 10))

# arrange in foursquare
akp_mhw_pair <-
  ggarrange(plotlist = akp_plotlist[1:2],
            ncol = 2,
            nrow = 1)
fhs_mhw_pair <-
  ggarrange(plotlist = fhs_plotlist[1:2],
            ncol = 2,
            nrow = 1)
nrs_mhw_pair <-
  ggarrange(plotlist = nrs_plotlist[1:2],
            ncol = 2,
            nrow = 1)
yfs_mhw_pair <-
  ggarrange(plotlist = yfs_plotlist[1:2],
            ncol = 2,
            nrow = 1)

# annotate the foursquares
# akp_foursquare_annotated <- annotate_figure(akp_foursquare,
#                                          top= text_grob("Comparison of Length with Depth and Temperature Values for Alaska Plaice",
#                                                         face="bold",
#                                                         size=20),
#                                          bottom = text_grob("Length (mm)",
#                                                             size = 16))
# fhs_foursquare_annotated <- annotate_figure(fhs_foursquare,
#                                             top= text_grob("Comparison of Length with Depth and Temperature Values for Flathead Sole",
#                                                            face="bold",
#                                                            size=20),
#                                             bottom = text_grob("Length (mm)",
#                                                                size = 16))
# nrs_foursquare_annotated <- annotate_figure(nrs_foursquare,
#                                             top= text_grob("Comparison of Length with Depth and Temperature Values for Northern Rock Sole",
#                                                            face="bold",
#                                                            size=20),
#                                             bottom = text_grob("Length (mm)",
#                                                                size = 16))
# yfs_foursquare_annotated <- annotate_figure(yfs_foursquare,
#                                             top= text_grob("Comparison of Length with Depth and Temperature Values for Yellowfin Sole",
#                                                            face="bold",
#                                                            size=20),
#                                             bottom = text_grob("Length (mm)",
#                                                                size = 16))

# Save Outputs ----
# Alaska plaice
# ggsave(filename = "mhw_akp_foursquare_annotated.tiff",
#        plot = akp_foursquare_annotated,
#        width=400,
#        height=400,
#        units = "mm",
#        dpi=300,
#        path = "./new_slushpile/output")
# # flathead sole
# ggsave(filename = "mhw_fhs_foursquare_annotated.tiff",
#        plot = fhs_foursquare_annotated,
#        width=400,
#        height=400,
#        units = "mm",
#        dpi=300,
#        path = "./new_slushpile/output")
# # northern rock sole
# ggsave(filename = "mhw_nrs_foursquare_annotated.tiff",
#        plot = nrs_foursquare_annotated,
#        width=400,
#        height=400,
#        units = "mm",
#        dpi=300,
#        path = "./new_slushpile/output")
# # yellowfin sole
# ggsave(filename = "mhw_yfs_foursquare_annotated.tiff",
#        plot = yfs_foursquare_annotated,
#        width=400,
#        height=400,
#        units = "mm",
#        dpi=300,
#        path = "./new_slushpile/output")
