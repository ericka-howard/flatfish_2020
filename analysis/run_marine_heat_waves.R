#' title:   Eastern Bering Sea Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates contour and box plots of Alaska plaice, flathead sole,
#'          northern rock sole, and yellowfin sole from 2000-2018 based on the RACE EBS
#'          survey with groupings of extreme marine heat wave vs. cold years and
#'          saves them in results folder
#' date:    2021-04-26
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
library(cowplot)

# *** Source Scripts ----
source(here("analysis", "functions_marine_heat_wave.R"))
source(here("analysis", "data.R"))
source(here("analysis", "bubble_plot.R"))

# Visualize -----
# make bubble plots

fhs_bubble <- fhs_bubble_dat %>%
  make_bubble_plot()

nrs_bubble <- nrs_bubble_dat %>%
  make_bubble_plot()

# make plotlists
# akp_plotlist <- akp %>%
#   make_mhw_factor_levels_nice() %>%
#   make_length_bins() %>%
#   make_foursquare_plotlist("Alaska Plaice",
#                            ylimits_depth = c(100, 10),
#                            ylimits_temp = c(-2, 10))

fhs_plotlist <- fhs %>%
  make_mhw_factor_levels_nice() %>%
  make_length_bins() %>%
  make_foursquare_plotlist("Flathead Sole",
                           ylimits_depth = c(225, 25),
                           ylimits_temp = c(-2, 7))


nrs_plotlist <- nrs %>%
  make_mhw_factor_levels_nice() %>%
  make_length_bins() %>%
  make_foursquare_plotlist(
    "Northern Rock Sole",
    ylimits_depth = c(110, 10),
    ylimits_temp = c(-2, 9)
  )

# yfs_plotlist <- yfs %>%
#   make_mhw_factor_levels_nice() %>%
#   make_length_bins() %>%
#   make_foursquare_plotlist(
#     "Yellowfin Sole",
#     ylimits_depth = c(120, 10),
#     ylimits_temp = c(-2, 10)
#   )
################################################################
# arrange 
fhs_row2 <- plot_grid(fhs_plotlist[[1]], fhs_plotlist[[2]], nrow=1)
fhs_row3 <- plot_grid(fhs_plotlist[[3]], fhs_plotlist[[4]], nrow=1)
fhs_grid <- plot_grid(fhs_bubble, fhs_row2, fhs_row3, nrow=3, rel_heights = c(0.4, 0.3, 0.4))

#################################################################
# akp_foursquare <- ggarrange(plotlist = akp_plotlist,
#                             ncol = 2,
#                             nrow = 2)
fhs_foursquare <- ggarrange(plotlist = fhs_plotlist,
                            ncol = 3,
                            nrow = 3)
nrs_foursquare <- ggarrange(plotlist = nrs_plotlist,
                            ncol = 3,
                            nrow = 3)
# yfs_foursquare <- ggarrange(plotlist = yfs_plotlist,
#                             ncol = 2,
#                             nrow = 2)

# annotate the foursquares
# akp_foursquare_annotated <- annotate_figure(
#   akp_foursquare,
#   top = text_grob(
#     "Comparison of Length with Depth and Temperature Values for Alaska Plaice",
#     face = "bold",
#     size = 20
#   ),
#   bottom = text_grob("Length (mm)",
#                      size = 16)
# )
fhs_foursquare_annotated <- annotate_figure(
  fhs_foursquare,
  top = text_grob(
    "Flathead Sole",
    face = "bold",
    size = 20
  ),
  bottom = text_grob("Length (mm)",
                     size = 16)
)
nrs_foursquare_annotated <- annotate_figure(
  nrs_foursquare,
  top = text_grob(
    "Northern Rock Sole",
    face = "bold",
    size = 20
  ),
  bottom = text_grob("Length (mm)",
                     size = 16)
)
# yfs_foursquare_annotated <- annotate_figure(
#   yfs_foursquare,
#   top = text_grob(
#     "Comparison of Length with Depth and Temperature Values for Yellowfin Sole",
#     face = "bold",
#     size = 20
#   ),
#   bottom = text_grob("Length (mm)",
#                      size = 16)
# )

# Save Outputs ----
# Alaska plaice
# ggsave(
#   here(
#     "results/foursquare_mhw",
#     "mhw_akp_foursquare_annotated.tiff"
#   ),
#   plot = akp_foursquare_annotated,
#   width = 400,
#   height = 400,
#   units = "mm",
#   dpi = 300
# )
# flathead sole
ggsave(
  here(
    "results/mhw_3by3",
    "mhw_fhs_3by3.tiff"
  ),
  plot = fhs_grid,
  width = 400,
  height = 400,
  units = "mm",
  dpi = 300
)
# northern rock sole
ggsave(
  here(
    "results/foursquare_mhw",
    "mhw_nrs_foursquare_annotated.tiff"
  ),
  plot = nrs_foursquare_annotated,
  width = 400,
  height = 400,
  units = "mm",
  dpi = 300
)
# yellowfin sole
# ggsave(
#   here(
#     "results/foursquare_mhw",
#     "mhw_yfs_foursquare_annotated.tiff"
#   ),
#   plot = yfs_foursquare_annotated,
#   width = 400,
#   height = 400,
#   units = "mm",
#   dpi = 300
# )
