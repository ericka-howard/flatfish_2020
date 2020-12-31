#' title:   Eastern Bering Sea Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates hex and box plots of Alaska plaice, flathead sole,
#'          northern rock sole, and yellowfin sole from 2000-2018 based on the RACE EBS
#'          survey based on length and phi=-ln(sedimentsize). Very repetitive.
#' date:    2020-12-31
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

# *** Load data ----

akp <- read_rds(here("data/intermediates", "akp_phi.rds"))

# Functions ----

make_factor_levels_nice <- function(df) {
  df$marine_heat_wave %<>% factor()
  levels(df$marine_heat_wave) <-
    c("Extreme Marine Heat Wave Years", "Other Years")
  df$warm_cold %<>% factor()
  levels(df$warm_cold) <- c("Cold Years", "Warm Years")
  return(df)
}

make_length_bins <- function(df) {
  cutoffs <- quantile(df$length, probs = seq(0, 1, 0.125), na.rm = TRUE)
  df$length_bins <-
    cut(df$length, breaks = cutoffs, include.lowest = TRUE)
  return(df)
}

# Data management ----

akp <- akp[!is.na(akp$phi), ]
akp <- akp %>%
  make_length_bins() %>%
  make_factor_levels_nice() %>%
  select(warm_cold, marine_heat_wave, phi, length, length_bins)


# Create visualizations ----
akp_mhw_boxplot <- ggplot(akp) +
  geom_boxplot(aes(x = length_bins, y = phi,
                   fill = marine_heat_wave),
               outlier.shape = NA) +
  coord_cartesian(ylim = c(0.25, 7)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "lightgrey"),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  theme_pubr() +
  scale_fill_manual(values =
                      c(
                        "Extreme Marine Heat Wave Years" =
                          "#95D055FF",
                        "Other Years" =
                          "#404788FF"
                      )) +
  labs(x = " ", y = " ", fill = " ")

akp_wc_boxplot <- ggplot(akp) +
  geom_boxplot(aes(x = length_bins, y = phi,
                   fill = warm_cold),
               outlier.shape = NA) +
  coord_cartesian(ylim = c(0.25, 7)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "lightgrey"),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  theme_pubr() +
  scale_fill_manual(values =
                      c("Warm Years" = "#95D055FF",
                        "Cold Years" = "#404788FF")) +
  labs(x = " ", y = " ", fill = " ")

akp_mhw_hex <- ggplot(akp) +
  geom_hex(aes(x = length, y = phi)) +
  facet_wrap( ~ marine_heat_wave) +
  coord_cartesian(ylim = c(0.25, 7)) +
  labs(title = " ", x = " ",
       y = " ") +
  theme_pubr() +
  scale_fill_gradient(low = '#EFE350FF', high = "#CC6A70FF", trans = "log") +
  theme(
    legend.position = "none",
    legend.title = element_blank())


akp_wc_hex <- ggplot(akp) +
  geom_hex(aes(x = length, y = phi)) +
  facet_wrap( ~ warm_cold) +
  coord_cartesian(ylim = c(0.25, 7)) +
  labs(title = " ", x = " ",
       y = " ") +
  theme_pubr() +
  scale_fill_gradient(low = '#EFE350FF', high = "#CC6A70FF", trans = "log") +
  theme(
    legend.position = "none",
    legend.title = element_blank())


# Put plots together ----

akp_plotlist <-
  list(akp_mhw_hex, akp_mhw_boxplot, akp_wc_hex, akp_wc_boxplot)


akp_mhw_pair <-
  ggarrange(plotlist = akp_plotlist[1:2],
            ncol = 2,
            nrow = 1)
akp_wc_pair <-
  ggarrange(plotlist = akp_plotlist[3:4],
            ncol = 2,
            nrow = 1)

akp_wc_annotated <- annotate_figure(
  akp_wc_pair,
  top = text_grob(
    "Comparison of length vs. phi values for Alaska Plaice for warm and cold years",
    face = "bold",
    size = 14
  ),
  bottom = "Length (mm)",
  left = "Phi"
)

akp_mhw_annotated <- annotate_figure(
  akp_mhw_pair,
  top = text_grob(
    "Comparison of length vs. phi values for Alaska Plaice for extreme marine heat wave years",
    face = "bold",
    size = 14
  ),
  bottom = "Length (mm)",
  left = "Phi"
)

# Save Results ----
ggsave(
  here("results/phi", "akp_wc_phi.tiff"),
  plot = akp_wc_annotated,
  width = 400,
  height = 200,
  units = "mm",
  dpi = 300
)
ggsave(
  here("results/phi", "akp_mhw_phi.tiff"),
  plot = akp_mhw_annotated,
  width = 400,
  height = 200,
  units = "mm",
  dpi = 300
)

rm(
  akp,
  akp_mhw_annotated,
  akp_mhw_boxplot,
  akp_mhw_hex,
  akp_mhw_pair,
  akp_plotlist,
  akp_wc_annotated,
  akp_wc_boxplot,
  akp_wc_hex,
  akp_wc_pair
)

######################################################################################################################################
######################################################################################################################################
######################################################################################################################################
## All code following is just the same as above, grotesquely repeated, but for different fishes. #####################################
## The run_phi.R, functions_phi.R, and data_phi.R files do not work and have been deprecated.    #####################################
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################


# Flathead Sole

# *** Load data ----

fhs <- read_rds(here("data/intermediates", "fhs_phi.rds"))

# Data management ----

fhs <- fhs[!is.na(fhs$phi), ]
fhs <- fhs %>%
  make_length_bins() %>%
  make_factor_levels_nice()  %>%
  select(warm_cold, marine_heat_wave, phi, length, length_bins)


# Create visualizations ----
fhs_mhw_boxplot <- ggplot(fhs) +
  geom_boxplot(aes(x = length_bins, y = phi,
                   fill = marine_heat_wave),
               outlier.shape = NA) +
  coord_cartesian(ylim = c(0.25, 7)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "lightgrey"),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  theme_pubr() +
  scale_fill_manual(values =
                      c(
                        "Extreme Marine Heat Wave Years" =
                          "#95D055FF",
                        "Other Years" =
                          "#404788FF"
                      )) +
  labs(x = " ", y = " ", fill = " ")

fhs_wc_boxplot <- ggplot(fhs) +
  geom_boxplot(aes(x = length_bins, y = phi,
                   fill = warm_cold),
               outlier.shape = NA) +
  coord_cartesian(ylim = c(0.25, 7)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "lightgrey"),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  theme_pubr() +
  scale_fill_manual(values =
                      c("Warm Years" = "#95D055FF",
                        "Cold Years" = "#404788FF")) +
  labs(x = " ", y = " ", fill = " ")

fhs_mhw_hex <- ggplot(fhs) +
  geom_hex(aes(x = length, y = phi)) +
  facet_wrap( ~ marine_heat_wave) +
  coord_cartesian(ylim = c(0.25, 7)) +
  labs(title = " ", x = " ",
       y = " ") +
  theme_pubr() +
  scale_fill_gradient(low = '#EFE350FF', high = "#CC6A70FF", trans = "log") +
  theme(
    legend.position = "none",
    legend.title = element_blank())


fhs_wc_hex <- ggplot(fhs) +
  geom_hex(aes(x = length, y = phi)) +
  facet_wrap( ~ warm_cold) +
  coord_cartesian(ylim = c(0.25, 7)) +
  labs(title = " ", x = " ",
       y = " ") +
  theme_pubr() +
  scale_fill_gradient(low = '#EFE350FF', high = "#CC6A70FF", trans = "log") +
  theme(
    legend.position = "none",
    legend.title = element_blank())


# Put plots together ----

fhs_plotlist <-
  list(fhs_mhw_hex, fhs_mhw_boxplot, fhs_wc_hex, fhs_wc_boxplot)


fhs_mhw_pair <-
  ggarrange(plotlist = fhs_plotlist[1:2],
            ncol = 2,
            nrow = 1)
fhs_wc_pair <-
  ggarrange(plotlist = fhs_plotlist[3:4],
            ncol = 2,
            nrow = 1)

fhs_wc_annotated <- annotate_figure(
  fhs_wc_pair,
  top = text_grob(
    "Comparison of length vs. phi values for Flathead Sole for warm and cold years",
    face = "bold",
    size = 14
  ),
  bottom = "Length (mm)",
  left = "Phi"
)

fhs_mhw_annotated <- annotate_figure(
  fhs_mhw_pair,
  top = text_grob(
    "Comparison of length vs. phi values for Flathead Sole for extreme marine heat wave years",
    face = "bold",
    size = 14
  ),
  bottom = "Length (mm)",
  left = "Phi"
)

# Save Results ----
ggsave(
  here("results/phi", "fhs_wc_phi.tiff"),
  plot = fhs_wc_annotated,
  width = 400,
  height = 200,
  units = "mm",
  dpi = 300
)
ggsave(
  here("results/phi", "fhs_mhw_phi.tiff"),
  plot = fhs_mhw_annotated,
  width = 400,
  height = 200,
  units = "mm",
  dpi = 300
)

rm(
  fhs,
  fhs_mhw_annotated,
  fhs_mhw_boxplot,
  fhs_mhw_hex,
  fhs_mhw_pair,
  fhs_plotlist,
  fhs_wc_annotated,
  fhs_wc_boxplot,
  fhs_wc_hex,
  fhs_wc_pair
)

######################################################################################################################################
######################################################################################################################################
######################################################################################################################################


# Northern Rock Sole

# *** Load data ----

nrs <- read_rds(here("data/intermediates", "nrs_phi.rds"))

# Data management ----

nrs <- nrs[!is.na(nrs$phi), ]
nrs <- nrs %>%
  make_length_bins() %>%
  make_factor_levels_nice()  %>%
  select(warm_cold, marine_heat_wave, phi, length, length_bins)


# Create visualizations ----
nrs_mhw_boxplot <- ggplot(nrs) +
  geom_boxplot(aes(x = length_bins, y = phi,
                   fill = marine_heat_wave),
               outlier.shape = NA) +
  coord_cartesian(ylim = c(0.25, 7)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "lightgrey"),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  theme_pubr() +
  scale_fill_manual(values =
                      c(
                        "Extreme Marine Heat Wave Years" =
                          "#95D055FF",
                        "Other Years" =
                          "#404788FF"
                      )) +
  labs(x = " ", y = " ", fill = " ")

nrs_wc_boxplot <- ggplot(nrs) +
  geom_boxplot(aes(x = length_bins, y = phi,
                   fill = warm_cold),
               outlier.shape = NA) +
  coord_cartesian(ylim = c(0.25, 7)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "lightgrey"),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  theme_pubr() +
  scale_fill_manual(values =
                      c("Warm Years" = "#95D055FF",
                        "Cold Years" = "#404788FF")) +
  labs(x = " ", y = " ", fill = " ")

nrs_mhw_hex <- ggplot(nrs) +
  geom_hex(aes(x = length, y = phi)) +
  facet_wrap( ~ marine_heat_wave) +
  coord_cartesian(ylim = c(0.25, 7)) +
  labs(title = " ", x = " ",
       y = " ") +
  theme_pubr() +
  scale_fill_gradient(low = '#EFE350FF', high = "#CC6A70FF", trans = "log") +
  theme(
    legend.position = "none",
    legend.title = element_blank())


nrs_wc_hex <- ggplot(nrs) +
  geom_hex(aes(x = length, y = phi)) +
  facet_wrap( ~ warm_cold) +
  coord_cartesian(ylim = c(0.25, 7)) +
  labs(title = " ", x = " ",
       y = " ") +
  theme_pubr() +
  scale_fill_gradient(low = '#EFE350FF', high = "#CC6A70FF", trans = "log") +
  theme(
    legend.position = "none",
    legend.title = element_blank())


# Put plots together ----

nrs_plotlist <-
  list(nrs_mhw_hex, nrs_mhw_boxplot, nrs_wc_hex, nrs_wc_boxplot)

nrs_mhw_pair <-
  ggarrange(plotlist = nrs_plotlist[1:2],
            ncol = 2,
            nrow = 1)
nrs_wc_pair <-
  ggarrange(plotlist = nrs_plotlist[3:4],
            ncol = 2,
            nrow = 1)

nrs_wc_annotated <- annotate_figure(
  nrs_wc_pair,
  top = text_grob(
    "Comparison of length vs. phi values for Northern Rock Sole for warm and cold years",
    face = "bold",
    size = 14
  ),
  bottom = "Length (mm)",
  left = "Phi"
)

nrs_mhw_annotated <- annotate_figure(
  nrs_mhw_pair,
  top = text_grob(
    "Comparison of length vs. phi values for Northern Rock Sole for extreme marine heat wave years",
    face = "bold",
    size = 14
  ),
  bottom = "Length (mm)",
  left = "Phi"
)

# Save Results ----
ggsave(
  here("results/phi", "nrs_wc_phi.tiff"),
  plot = nrs_wc_annotated,
  width = 400,
  height = 200,
  units = "mm",
  dpi = 300
)
ggsave(
  here("results/phi", "nrs_mhw_phi.tiff"),
  plot = nrs_mhw_annotated,
  width = 400,
  height = 200,
  units = "mm",
  dpi = 300
)

rm(
  nrs,
  nrs_mhw_annotated,
  nrs_mhw_boxplot,
  nrs_mhw_hex,
  nrs_mhw_pair,
  nrs_plotlist,
  nrs_wc_annotated,
  nrs_wc_boxplot,
  nrs_wc_hex,
  nrs_wc_pair
)

######################################################################################################################################
######################################################################################################################################
######################################################################################################################################



# Yellowfin Sole

# *** Load data ----

yfs <- read_rds(here("data/intermediates", "yfs_phi.rds"))

# Data management ----

yfs <- yfs[!is.na(yfs$phi), ]
yfs <- yfs %>%
  make_length_bins() %>%
  make_factor_levels_nice()  %>%
  select(warm_cold, marine_heat_wave, phi, length, length_bins)


# Create visualizations ----
yfs_mhw_boxplot <- ggplot(yfs) +
  geom_boxplot(aes(x = length_bins, y = phi,
                   fill = marine_heat_wave),
               outlier.shape = NA) +
  coord_cartesian(ylim = c(0.25, 7)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "lightgrey"),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  theme_pubr() +
  scale_fill_manual(values =
                      c(
                        "Extreme Marine Heat Wave Years" =
                          "#95D055FF",
                        "Other Years" =
                          "#404788FF"
                      )) +
  labs(x = " ", y = " ", fill = " ")

yfs_wc_boxplot <- ggplot(yfs) +
  geom_boxplot(aes(x = length_bins, y = phi,
                   fill = warm_cold),
               outlier.shape = NA) +
  coord_cartesian(ylim = c(0.25, 7)) +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "lightgrey"),
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  theme_pubr() +
  scale_fill_manual(values =
                      c("Warm Years" = "#95D055FF",
                        "Cold Years" = "#404788FF")) +
  labs(x = " ", y = " ", fill = " ")

yfs_mhw_hex <- ggplot(yfs) +
  geom_hex(aes(x = length, y = phi)) +
  facet_wrap( ~ marine_heat_wave) +
  coord_cartesian(ylim = c(0.25, 7)) +
  labs(title = " ", x = " ",
       y = " ") +
  theme_pubr() +
  scale_fill_gradient(low = '#EFE350FF', high = "#CC6A70FF", trans = "log") +
  theme(
    legend.position = "none",
    legend.title = element_blank())


yfs_wc_hex <- ggplot(yfs) +
  geom_hex(aes(x = length, y = phi)) +
  facet_wrap( ~ warm_cold) +
  coord_cartesian(ylim = c(0.25, 7)) +
  labs(title = " ", x = " ",
       y = " ") +
  theme_pubr() +
  scale_fill_gradient(low = '#EFE350FF', high = "#CC6A70FF", trans = "log") +
  theme(
    legend.position = "none",
    legend.title = element_blank())


# Put plots together ----

yfs_plotlist <-
  list(yfs_mhw_hex, yfs_mhw_boxplot, yfs_wc_hex, yfs_wc_boxplot)


yfs_mhw_pair <-
  ggarrange(plotlist = yfs_plotlist[1:2],
            ncol = 2,
            nrow = 1)
yfs_wc_pair <-
  ggarrange(plotlist = yfs_plotlist[3:4],
            ncol = 2,
            nrow = 1)

yfs_wc_annotated <- annotate_figure(
  yfs_wc_pair,
  top = text_grob(
    "Comparison of length vs. phi values for Yellowfin Sole for warm and cold years",
    face = "bold",
    size = 14
  ),
  bottom = "Length (mm)",
  left = "Phi"
)

yfs_mhw_annotated <- annotate_figure(
  yfs_mhw_pair,
  top = text_grob(
    "Comparison of length vs. phi values for Yellowfin Sole for extreme marine heat wave years",
    face = "bold",
    size = 14
  ),
  bottom = "Length (mm)",
  left = "Phi"
)

# Save Results ----
ggsave(
  here("results/phi", "yfs_wc_phi.tiff"),
  plot = yfs_wc_annotated,
  width = 400,
  height = 200,
  units = "mm",
  dpi = 300
)
ggsave(
  here("results/phi", "yfs_mhw_phi.tiff"),
  plot = yfs_mhw_annotated,
  width = 400,
  height = 200,
  units = "mm",
  dpi = 300
)

rm(
  yfs,
  yfs_mhw_annotated,
  yfs_mhw_boxplot,
  yfs_mhw_hex,
  yfs_mhw_pair,
  yfs_plotlist,
  yfs_wc_annotated,
  yfs_wc_boxplot,
  yfs_wc_hex,
  yfs_wc_pair
)

######################################################################################################################################
######################################################################################################################################
######################################################################################################################################