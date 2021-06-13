#' title:   Eastern Bering Sea Continental Shelf Survey Flatfish Visualizations
#' purpose: This script contains the functions used to create bubble
#' plots of Alaska plaice, flathead sole, northern rock sole, and yellowfin
#' sole from 2000-2018 based on the RACE EBSsurvey for extreme marine heat
#' wave years (2016 & 2018) vs. Cold years.
#' date:    2021-04-27
#' author: Ericka B. Smith
#' ---

library(PBSmapping)
library(tidyverse)
library(ggpubr)
library(here)
library(viridis)

# *** Load data ----

akp_bubble_dat <-
  read_rds(here("data/intermediates", "akp_phi.rds"))
# fhs_bubble_dat <-
#   read_rds(here("data/intermediates", "fhs_phi.rds"))
# nrs_bubble_dat <-
#   read_rds(here("data/intermediates", "nrs_phi.rds"))
# yfs_bubble_dat <-
#   read_rds(here("data/intermediates", "yfs_phi.rds"))

# use PBSmapping to get spatial data
# data('nepacLLhigh')
# bs <- nepacLLhigh %>% dplyr::select(group=PID, POS=POS,lon=X, lat=Y)

# *** Functions ----

# creates 8 bins for length in length_bins variable
make_length_bins <- function(df) {
  cutoffs <-
    quantile(df$length, probs = seq(0, 1, 0.125), na.rm = TRUE)
  df$length_bins <-
    cut(df$length, breaks = cutoffs, include.lowest = TRUE)
  return(df)
}

data_wrangling <- function(df) {
  df_for_plot <- df %>%
    make_length_bins() %>%
    count(clust_id, cluster_lat, cluster_lon, length_bins) %>%
    group_by(length_bins) %>%
    mutate(total_in_bin = sum(n),
           prop = n / total_in_bin) %>%
    ungroup() %>%
    arrange(desc(prop))
  
  # get list of this species' length bins
  length_bin_levels <- levels(df_for_plot$length_bins)
  # grab length bin values for panels 1, 5, and 8

  panels_wanted <- length_bin_levels[c(1, 5, 8)]
  
  # for paper - split out panels 1, 5, and 8
  df_panel_plot <- df_for_plot %>%
    filter(length_bins %in% panels_wanted) %>%
    mutate(length_bins = fct_drop(length_bins)) %>%
    mutate(Proportion = prop,
           `Length Bin (mm)` = length_bins)
  return(df_panel_plot)
}

# makes the plots
make_bubble_plot <- function(df) {
  data('nepacLLhigh')
  bs <-
    nepacLLhigh %>% dplyr::select(
      group = PID,
      POS = POS,
      lon = X,
      lat = Y
    )
  df <- df %>%
    data_wrangling()
  ggplot() +
    geom_polygon(
      data = bs,
      aes(lon, lat, group = group),
      fill = 8,
      color = 'black'
    ) +
    xlab(expression(paste(Longitude ^ o,  ~ 'W'))) +
    ylab(expression(paste(Latitude ^ o,  ~ 'N'))) +
    coord_map(xlim = c(-179,-158), ylim = c(54, 63)) +
    geom_point(data = df,
               aes(cluster_lon,
                   cluster_lat,
                   alpha = Proportion,
                   size = Proportion)) +
    scale_size(range = c(0, 5)) +
    facet_wrap( ~ `Length Bin (mm)`, labeller = label_both) +
    theme_pubclean() +
    theme(
      panel.background = element_rect(fill = 'white'),
      legend.position = "bottom",
      panel.spacing.x = unit(1, "line"),
      panel.border = element_rect(color = "black",
                                  fill = "transparent"),
      strip.background = element_blank(),
      strip.text = element_text(size = 14),
      axis.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )
}


#
# akp_bubble <- akp_bubble_dat %>%
#   make_bubble_plot()

# fhs_bubble <- fhs_bubble_dat %>%
#   make_bubble_plot()
# 
# nrs_bubble <- nrs_bubble_dat %>%
#   make_bubble_plot()

# yfs_bubble <- yfs_bubble_dat %>%
#   make_bubble_plot()

