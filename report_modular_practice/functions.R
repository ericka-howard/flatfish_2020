#' title: EBS Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates plots of Alaska plaice, flathead sole, northern rock sole, and yellowfin sole
#'          from 2000-2018 based on the RACE EBS survey
#' date: 2020-10-13
#' ---

# Libraries -------

library(tidyverse)
library(sf)
library(magrittr)
library(viridis)
library(grDevices)
library(ggpubr)

# Directories -------

#outputfolder<-"./output/"

# Functions -------

# *** Data Management Functions -----

# makes all the column names lowercase
make_lowercase <- function(df){
  for (i in 1:length(names(df))){
    names(df)[i] <- tolower(names(df)[i])
  }
  return(df)
}

# takes interpolated haul lengths and split off a species
separate_spec <- function(spec_code){
  dat <- len_e %>%
    filter(species_code==spec_code)
  return(dat)
}

# *** Binning Functions (should combine) -----

# creates 8 bins for bottom_depth in depth_bins variable
make_depth_bins <- function(df){
  df$depth_bins <- cut(df$bottom_depth, breaks = 8)
  return(df)
}

# creates 8 bins for gear_temperature in temp_bins variable
make_temp_bins <- function(df){
  df$temp_bins <- cut(df$gear_temperature, breaks=8)
  return(df)
}

# creates 8 bins for length in length_bins variable
make_length_bins <- function(df){
  df$length_bins <- cut(df$length, breaks=8)
  return(df)
}

# *** Plotting Functions ------

# creates depth binned bottom_depth boxplots
plot_box_depth <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=depth_bins, y=length)) +
    theme_light()+
    ggtitle(paste(df_name, "depth", "by", "percentiles", sep = "_"))
}

# creates temp binned gear_temperature boxplots
plot_box_temp <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=temp_bins, y=length)) +
    theme_light()+
    ggtitle(paste(df_name, "temp", "by", "percentiles", sep = "_"))
}

# creates length binned bottom_depth boxplots
plot_box_ldepth <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=length_bins, y=bottom_depth),
                 outlier.shape = NA) +
    coord_cartesian(ylim=c(0, 225))+
    theme_light()+
    ggtitle(paste(df_name, "length", "by", "depth", "at", "percentiles", sep = "_"))
}

# creates length binned gear_temperature boxplots
plot_box_ltemp <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=length_bins, 
                     y=gear_temperature),
                 outlier.shape = NA) +
    coord_cartesian(ylim=c(-2.5, 12.5))+
    theme_light()+
    ggtitle(paste(df_name, "length", "by", "temp", "at", "percentiles", sep = "_"))
}

# *** Foursquare Functions -----

length_binned_temps <- function(){
  p1 <- plot_box_ltemp(akp, "akp")
  p2 <- plot_box_ltemp(fhs, "fhs")
  p3 <- plot_box_ltemp(nrs, "nrs")
  p4 <- plot_box_ltemp(yfs, "yfs")
  ggarrange(plots = list(p1, p2, p3, p4), ncol = 2, nrow = 2)
}

length_binned_depths <- function(){
  p1 <- plot_box_ldepth(akp, "akp")
  p2 <- plot_box_ldepth(fhs, "fhs")
  p3 <- plot_box_ldepth(nrs, "nrs")
  p4 <- plot_box_ldepth(yfs, "yfs")
  ggarrange(plots = list(p1, p2, p3, p4), ncol = 2, nrow = 2)
}

temp_binned_temps <- function(){
  p1 <- plot_box_temp(akp, "akp")
  p2 <- plot_box_temp(fhs, "fhs")
  p3 <- plot_box_temp(nrs, "nrs")
  p4 <- plot_box_temp(yfs, "yfs")
  ggarrange(plots = list(p1, p2, p3, p4), ncol=2, nrow=2)
}

depth_binned_depths <- function(){
  p1 <- plot_box_depth(akp, "akp")
  p2 <- plot_box_depth(fhs, "fhs")
  p3 <- plot_box_depth(nrs, "nrs")
  p4 <- plot_box_depth(yfs, "yfs")
  ggarrange(plots = list(p1, p2, p3, p4), ncol=2, nrow=2)
}

# *** Coordinator Functions -----

# returns a dataframe with all of the wanted binning
coordinate_binning <- function(df){
  df <- df %>% 
    make_depth_bins() %>%
    make_temp_bins() %>%
    make_length_bins()
}

coordinate_boxplots <- function(bins, var){
  the_plot <- case_when(
    bins == "length" & var == "temp" ~ length_binned_temps(),
    bins == "length" & var == "depth" ~ length_binned_depths(),
    bins == "temp" ~ temp_binned_temps(),
    bins == "depth" ~ depth_binned_depths())
}


