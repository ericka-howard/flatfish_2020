#' title: EBS Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates plots of Alaska plaice, flathead sole, northern rock sole, and yellowfin sole
#'          from 2000-2018 based on the RACE EBS survey
#' date: 2020-10-13
#' ---

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
  cutoffs <- quantile(df$bottom_depth, probs = seq(0,1,0.125), na.rm = TRUE)
  df$depth_bins <- cut(df$bottom_depth, breaks = cutoffs)
  return(df)
}

# creates 8 bins for gear_temperature in temp_bins variable
make_temp_bins <- function(df){
  cutoffs <- quantile(df$gear_temperature, probs = seq(0,1,0.125), na.rm = TRUE)
  df$temp_bins <- cut(df$gear_temperature, breaks=cutoffs)
  return(df)
}

# creates 8 bins for length in length_bins variable
make_length_bins <- function(df){
  cutoffs <- quantile(df$length, probs = seq(0,1,0.125), na.rm = TRUE)
  df$length_bins <- cut(df$length, breaks=cutoffs)
  return(df)
}

# *** Plotting Functions ------

# creates depth binned bottom_depth boxplots
plot_box_depth <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=depth_bins, y=length),
                 outlier.size = 1,
                 outlier.color = "grey",
                 outlier.shape=1) +
    theme_pubr()+
    labs(title=paste(df_name, "Depth", "by", "Length", sep = " "),
         subtitle="Data has been grouped such that each box contains 1/8 of the data")+
    xlab("Grouped Depth (m)")+
    ylab("Length(mm)")
}

# creates temp binned gear_temperature boxplots
plot_box_temp <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=temp_bins, y=length),
                 outlier.size = 1,
                 outlier.color = "grey",
                 outlier.shape=1) +
    theme_pubr()+
    labs(title=paste(df_name, "Temperature", "by", "Length", sep = " "),
         subtitle="Data has been grouped such that each box contains 1/8 of the data")+
    xlab("Grouped Temperature (°C)")+
    ylab("Length (mm)")
}

# creates length binned bottom_depth boxplots
plot_box_ldepth <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=length_bins, y=bottom_depth),
                 outlier.size = 1,
                 outlier.color = "grey",
                 outlier.shape=1) +
    coord_cartesian(ylim=c(0, 225))+
    theme_pubr()+
    labs(title=paste(df_name, "Length", "by", "Depth", sep = " "),
         subtitle="Data has been grouped such that each box contains 1/8 of the data")+
    xlab("Length Bins (mm)")+
    ylab("Bottom Depth (m)")
}

# creates length binned gear_temperature boxplots
plot_box_ltemp <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=length_bins, 
                     y=gear_temperature),
                 outlier.size = 1,
                 outlier.color = "grey",
                 outlier.shape=1) +
    coord_cartesian(ylim=c(-2.5, 12.5))+
    theme_pubr()+
    labs(title=paste(df_name, "Length", "by", "Temperature", sep = " "),
         subtitle="Data has been grouped such that each box contains 1/8 of the data")+
    xlab("Length Bins (mm)")+
    ylab("Gear Temperature (°C)")
}

# *** Foursquare Plot Functions -----

length_binned_temps <- function(){
  p1 <- plot_box_ltemp(akp, "Alaska Plaice")
  p2 <- plot_box_ltemp(fhs, "Flathead Sole")
  p3 <- plot_box_ltemp(nrs, "Northern Rock Sole")
  p4 <- plot_box_ltemp(yfs, "Yellowfin Sole")
  plotlist <- list(p1, p2, p3, p4)
  return(plotlist)
}

length_binned_depths <- function(){
  p1 <- plot_box_ldepth(akp, "Alaska Plaice")
  p2 <- plot_box_ldepth(fhs, "Flathead Sole")
  p3 <- plot_box_ldepth(nrs, "Northern Rock Sole")
  p4 <- plot_box_ldepth(yfs, "Yellowfin Sole")
  plotlist <- list(p1, p2, p3, p4)
  return(plotlist)
}

temp_binned_temps <- function(){
  p1 <- plot_box_temp(akp, "Alaska Plaice")
  p2 <- plot_box_temp(fhs, "Flathead Sole")
  p3 <- plot_box_temp(nrs, "Northern Rock Sole")
  p4 <- plot_box_temp(yfs, "Yellowfin Sole")
  plotlist <- list(p1, p2, p3, p4)
  return(plotlist)
}

depth_binned_depths <- function(){
  p1 <- plot_box_depth(akp, "Alaska Plaice")
  p2 <- plot_box_depth(fhs, "Flathead Sole")
  p3 <- plot_box_depth(nrs, "Northern Rock Sole")
  p4 <- plot_box_depth(yfs, "Yellowfin Sole")
  plotlist <- list(p1, p2, p3, p4)
  return(plotlist)
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
    bins == "temp" & var == "temp" ~ temp_binned_temps(),
    bins == "depth" & var =="depth" ~ depth_binned_depths())
}


