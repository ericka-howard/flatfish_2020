#' title:   Eastern Bering Sea Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates contour plots of Alaska plaice, flathead sole,
#'          northern rock sole, and yellowfin sole from 2000-2018 based on the RACE EBS
#'          survey. Computationally intensive.
#' date:    2020-11-24
#' author: Ericka B. Smith
#' ---

library(tidyverse)
library(sf)
library(magrittr)
library(viridis)
library(grDevices)
library(ggpubr)
library(hexbin)
#library(here)


# Analysis -----


akp <- read_rds("./new_slushpile/intermediates/akp.rds")
fhs <- read_rds("./new_slushpile/intermediates/fhs.rds")
nrs <- read_rds("./new_slushpile/intermediates/nrs.rds")
yfs <- read_rds("./new_slushpile/intermediates/yfs.rds")


make_contour_plot <- function(df, df_name, y, y_name) {
  yvar <- enquo(y)
  df$marine_heat_wave %<>% factor()
  levels(df$marine_heat_wave) <-
    c("Extreme Marine Heat Wave Years", "Other Years")
  ggplot(df) +
    geom_density2d(aes(
      x = length,
      y = !!yvar,
      color = marine_heat_wave
    ),
    size = 1) +
    facet_wrap( ~ marine_heat_wave) +
    labs(
      title = paste(df_name, "Length by", y_name, sep = " "),
      caption = "Extreme Marine Heat Wave Years are 2015, 2016, and 2019",
      x = " ",
      y = " "
    ) +
    scale_color_manual(values = c(
      "Extreme Marine Heat Wave Years" =
        "#95D055FF",
      "Other Years" =
        "#404788FF"
    )) +
    theme_pubr(base_size = 16) +
    theme(
      panel.spacing.x = unit(1, "line"),
      legend.position = "none",
      panel.border = element_rect(color = "black",
                                  fill = "transparent")
    )
}

akp_temp_contour <-
  make_contour_plot(akp, "Alaska Plaice", gear_temperature, "Temperature (°C)")
akp_depth_contour <-
  make_contour_plot(akp, "Alaska Plaice", bottom_depth, "Bottom Depth (m)")
fhs_temp_contour <-
  make_contour_plot(fhs, "Flathead Sole", gear_temperature, "Temperature (°C)")
fhs_depth_contour <-
  make_contour_plot(fhs, "Flathead Sole", bottom_depth, "Bottom Depth (m)")
nrs_temp_contour <-
  make_contour_plot(nrs, "Northern Rock Sole", gear_temperature, "Temperature (°C)")
nrs_depth_contour <-
  make_contour_plot(nrs, "Northern Rock Sole", bottom_depth, "Bottom Depth (m)")
yfs_temp_contour <-
  make_contour_plot(yfs, "Yellowfin Sole", gear_temperature, "Temperature (°C)")
yfs_depth_contour <-
  make_contour_plot(yfs, "Yellowfin Sole", bottom_depth, "Bottom Depth (m)")


#proceed with caution, computationally intensive
ggsave(
  filename = "mhw_akp_temp_contour.tiff",
  plot = akp_temp_contour,
  width = 400,
  height = 400,
  units = "mm",
  dpi = 300,
  path = "./new_slushpile/output"
)
ggsave(
  filename = "mhw_akp_depth_contour.tiff",
  plot = akp_depth_contour,
  width = 400,
  height = 400,
  units = "mm",
  dpi = 300,
  path = "./new_slushpile/output"
)
ggsave(
  filename = "mhw_fhs_temp_contour.tiff",
  plot = fhs_temp_contour,
  width = 400,
  height = 400,
  units = "mm",
  dpi = 300,
  path = "./new_slushpile/output"
)
ggsave(
  filename = "mhw_fhs_depth_contour.tiff",
  plot = fhs_depth_contour,
  width = 400,
  height = 400,
  units = "mm",
  dpi = 300,
  path = "./new_slushpile/output"
)
ggsave(
  filename = "mhw_nrs_temp_contour.tiff",
  plot = nrs_temp_contour,
  width = 400,
  height = 400,
  units = "mm",
  dpi = 300,
  path = "./new_slushpile/output"
)
ggsave(
  filename = "mhw_nrs_depth_contour.tiff",
  plot = nrs_depth_contour,
  width = 400,
  height = 400,
  units = "mm",
  dpi = 300,
  path = "./new_slushpile/output"
)
ggsave(
  filename = "mhw_yfs_temp_contour.tiff",
  plot = yfs_temp_contour,
  width = 400,
  height = 400,
  units = "mm",
  dpi = 300,
  path = "./new_slushpile/output"
)
ggsave(
  filename = "mhw_yfs_depth_contour.tiff",
  plot = yfs_depth_contour,
  width = 400,
  height = 400,
  units = "mm",
  dpi = 300,
  path = "./new_slushpile/output"
)
