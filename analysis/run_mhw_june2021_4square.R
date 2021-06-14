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

# *** Make Plots -----

# contour and box plots
# akp_plotlist <- akp %>%
#   make_mhw_factor_levels_nice() %>%
#   make_length_bins() %>%
#   make_foursquare_plotlist("Alaska Plaice",
#                            ylimits_depth = c(100, 10),
#                            ylimits_temp = c(-2, 10))

# 
# fhs_plotlist <- fhs %>%
#   make_mhw_factor_levels_nice() %>%
#   make_length_bins() %>%
#   make_foursquare_plotlist("Flathead Sole",
#                            ylimits_depth = c(225, 25),
#                            ylimits_temp = c(-2, 7))
# 
# nrs_plotlist <- nrs %>%
#   make_mhw_factor_levels_nice() %>%
#   make_length_bins() %>%
#   make_foursquare_plotlist(
#     "Northern Rock Sole",
#     ylimits_depth = c(110, 10),
#     ylimits_temp = c(-2, 9)
#   )


yfs_plotlist <- yfs %>%
  make_mhw_factor_levels_nice() %>%
  make_length_bins() %>%
  make_foursquare_plotlist(
    "Yellowfin Sole",
    ylimits_depth = c(120, 10),
    ylimits_temp = c(-2, 10)
  )


# *** Arrange and Save Grid Image ----

arrange_save_grid <- function(fish_name, fish_plotlist) {
  # make title and legend
  title <-
    ggdraw() + draw_label(paste("\n", fish_name, "\n"),
                          fontface = 'bold',
                          size = 20)
  legend <- get_legend(fish_plotlist[[2]])
  # label plot panels
  panel_a <- annotate_figure(
    fish_plotlist[[1]],
    fig.lab = "A.",
    fig.lab.pos = "top.left",
    fig.lab.size = 14
  )
  panel_b <- annotate_figure(
    fish_plotlist[[2]] + theme(legend.position = "none"),
    fig.lab = "B.",
    fig.lab.pos = "top.left",
    fig.lab.size = 14
  )
  panel_c <- annotate_figure(
    fish_plotlist[[3]],
    fig.lab = "C.",
    fig.lab.pos = "top.left",
    fig.lab.size = 14
  )
  panel_d <- annotate_figure(
    fish_plotlist[[4]],
    fig.lab = "D.",
    fig.lab.pos = "top.left",
    fig.lab.size = 14
  )
  # make rows
  row1 <- plot_grid(panel_a, panel_b, nrow = 1)
  row2 <- plot_grid(panel_c, panel_d, nrow = 1)
  # make overall grid
  fish_grid <-
    plot_grid(
      title,
      row1,
      row2,
      legend,
      nrow = 4,
      rel_heights = c(0.04, 0.3, 0.3, 0.05)
    )
  # save overall grid
  # as tiff
  filename_tiff <-
    paste(fish_name, "_mhw", "_4sq", ".tiff", sep = "")
  ggsave(
    here("results/mhw_4sq",
         filename_tiff),
    plot = fish_grid,
    width = 500,
    height = 500,
    units = "mm",
    dpi = 600
  )
  # as png
  filename_png <-
    paste(fish_name, "_mhw", "_4sq", ".png", sep = "")
  ggsave(
    here("results/mhw_4sq",
         filename_png),
    plot = fish_grid,
    width = 500,
    height = 500,
    units = "mm"
  )
}

# arrange_save_grid("Alaska Plaice", akp_plotlist)
# arrange_save_grid("Flathead Sole", fhs_plotlist)
# arrange_save_grid("Northern Rock Sole", nrs_plotlist)
arrange_save_grid("Yellowfin Sole", yfs_plotlist)
