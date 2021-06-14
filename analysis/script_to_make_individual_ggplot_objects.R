#' title:   Eastern Bering Sea Continental Shelf Survey Flatfish Visualizations
#' purpose: This script contains the functions used to create ggplot objects
#' (saved as rds files) for contour and box plots of Alaska plaice, flathead
#' sole, northern rock sole, and yellowfin sole from 2000-2018 based on the
#' RACE EBS survey for extreme marine heat wave years (2016 & 2018) vs. Cold
#' years and warm vs. cold years
#' date:    2021-06-13
#' author: Ericka B. Smith


# Set environment ----

# *** Libraries ----

library(tidyverse)
library(sf)
library(magrittr)
library(viridis)
library(grDevices)
library(ggpubr)
library(here)

paste("Start Script: ", Sys.time())

# *** Source Scripts ----
source(here("analysis", "functions_to_make_individual_ggplot_objects.R"))

# *** Make Plots -----------
# (data handled within each fish category to preserve memory)

# Alaska Plaice -------------------------
paste("Start AKP: ", Sys.time())
akp <- read_rds(here("data/intermediates", "akp.rds"))

akp %>%
  make_factor_levels_nice() %>%
  make_length_bins() %>%
  make_plots(
    "Alaska Plaice",
    ylimits_depth = c(100, 10),
    ylimits_temp = c(-2, 10),
    is_marine_heat_wave = TRUE
  )
akp %>%
  make_factor_levels_nice() %>%
  make_length_bins() %>%
  make_plots(
    "Alaska Plaice",
    ylimits_depth = c(100, 10),
    ylimits_temp = c(-2, 10),
    is_marine_heat_wave = FALSE
  )
rm(akp)

# Flathead Sole ------------------------
paste("Start FHS: ", Sys.time())
fhs <- read_rds(here("data/intermediates", "fhs.rds"))

fhs %>%
  make_mhw_factor_levels_nice() %>%
  make_length_bins() %>%
  make_foursquare_plotlist(
    "Flathead Sole",
    ylimits_depth = c(225, 25),
    ylimits_temp = c(-2, 7),
    is_marine_heat_wave = TRUE
  )

fhs %>%
  make_mhw_factor_levels_nice() %>%
  make_length_bins() %>%
  make_foursquare_plotlist(
    "Flathead Sole",
    ylimits_depth = c(225, 25),
    ylimits_temp = c(-2, 7),
    is_marine_heat_wave = FALSE
  )

rm(fhs)

# Nothern Rock Sole -------------------
paste("Start NRS: ", Sys.time())
nrs <- read_rds(here("data/intermediates", "nrs.rds"))

nrs %>%
  make_mhw_factor_levels_nice() %>%
  make_length_bins() %>%
  make_foursquare_plotlist(
    "Northern Rock Sole",
    ylimits_depth = c(110, 10),
    ylimits_temp = c(-2, 9),
    is_marine_heat_wave = TRUE
  )

nrs %>%
  make_mhw_factor_levels_nice() %>%
  make_length_bins() %>%
  make_foursquare_plotlist(
    "Northern Rock Sole",
    ylimits_depth = c(110, 10),
    ylimits_temp = c(-2, 9),
    is_marine_heat_wave = FALSE
  )

rm(nrs)

# Yellowfin Sole --------------------------
paste("Start YFS: ", Sys.time())
yfs <- read_rds(here("data/intermediates", "yfs.rds"))

yfs_plotlist <- yfs %>%
  make_mhw_factor_levels_nice() %>%
  make_length_bins() %>%
  make_foursquare_plotlist(
    "Yellowfin Sole",
    ylimits_depth = c(120, 10),
    ylimits_temp = c(-2, 10),
    is_marine_heat_wave = TRUE
  )

yfs_plotlist <- yfs %>%
  make_mhw_factor_levels_nice() %>%
  make_length_bins() %>%
  make_foursquare_plotlist(
    "Yellowfin Sole",
    ylimits_depth = c(120, 10),
    ylimits_temp = c(-2, 10),
    is_marine_heat_wave = FALSE
  )

rm(yfs)

paste("End Script: ", Sys.time())
