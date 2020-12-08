#' title: EBS Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates plots of Alaska plaice, flathead sole, northern 
#'          rock sole, and yellowfin solefrom 2000-2018 based on the RACE EBS 
#'          survey
#' date: 2020-12-08
#' ---


# *** Binning Function -----

# creates 8 bins for length in length_bins variable based on the probs
# argument, which is currently set at 1/8=12.5%=0.125
make_length_bins <- function(df){
  cutoffs <- quantile(df$length, probs = seq(0,1,0.125), na.rm = TRUE)
  df$length_bins <- cut(df$length, breaks = cutoffs, include.lowest = TRUE)
  return(df)
}

# *** Plotting Functions ------

# create theme for boxplots
th <- theme(
  legend.title = element_text(size = 14, face = "bold"),
  legend.text = element_text(size = 12),
  legend.background = element_rect(fill = "lightgrey"),
  legend.key.size = unit(1.5, "cm"),
  legend.key.width = unit(0.5, "cm")
)

# creates warm/cold length binned bottom_depth boxplots
plot_box_depth_wc <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=length_bins,
                     y=bottom_depth,
                     fill= warm_cold),
                 #color = warm_cold),
                 outlier.size = 1,
                 outlier.color = "grey",
                 outlier.shape=1) +
    coord_cartesian(ylim=c(0, 225))+
    th+
    theme_pubr()+
    scale_fill_manual(values = c("warm"="#95D055FF", "cold"="#404788FF"))+
    labs(title=df_name,
         fill = "Warm or Cold Year")+
    xlab(" ")+
    ylab(" ")
}

# creates warm/cold length binned gear_temperature boxplots
plot_box_temp_wc <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=length_bins, 
                     y=gear_temperature,
                     fill = warm_cold),
                 #color = warm_cold),
                 outlier.size = 1,
                 outlier.color = "grey",
                 outlier.shape=1) +
    coord_cartesian(ylim=c(-2.5, 12.5))+
    th+
    theme_pubr()+
    scale_fill_manual(values = c("warm"="#95D055FF", "cold"="#404788FF"))+
    labs(title=df_name,
         fill = "Warm or Cold Year")+
    xlab(" ")+
    ylab(" ")
}


# creates marine heat wave length binned bottom_depth boxplots
plot_box_depth_mhw <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=length_bins,
                     y=bottom_depth,
                     fill= marine_heat_wave),
                 outlier.size = 1,
                 outlier.color = "grey",
                 outlier.shape=1) +
    coord_cartesian(ylim=c(0, 225))+
    th+
    theme_pubr()+
    scale_fill_manual(values = c("Extreme Marine Heat Wave Years"=
                                   "#95D055FF", 
                                 "Non-Extreme Marine Heat Wave Years"=
                                   "#404788FF"))+
    labs(title=df_name,
         fill = " ")+
    xlab(" ")+
    ylab(" ")
}

# creates marine heat wave length binned gear_temperature boxplots
plot_box_temp_mhw <- function(df, df_name){
  ggplot(df) + 
    geom_boxplot(aes(x=length_bins, 
                     y=gear_temperature,
                     fill = marine_heat_wave),
                 outlier.size = 1,
                 outlier.color = "grey",
                 outlier.shape=1) +
    coord_cartesian(ylim=c(-2.5, 12.5))+
    th+
    theme_pubr()+
    scale_fill_manual(values = c("Extreme Marine Heat Wave Years"=
                                   "#95D055FF", 
                                 "Non-Extreme Marine Heat Wave Years"=
                                   "#404788FF"))+
    labs(title=df_name,
         fill = " ")+
    xlab(" ")+
    ylab(" ")
}


# now density plots

# first create theme for density
th_den <- theme(
  legend.title = element_text(size = 14, face = "bold"),
  legend.text = element_text(size = 12),
  legend.background = element_rect(fill = "lightgrey"),
  legend.key.size = unit(1.5, "cm"),
  legend.key.width = unit(0.5, "cm")
)


# creates length binned bottom_depth density plots
plot_den_ldepth <- function(df, df_name){
  ggplot(df) + 
    geom_density(aes(x=bottom_depth,
                     color= warm_cold)) +
    facet_wrap(~length_bins)+
    coord_cartesian(xlim=c(0, 225))+
    th_den+
    theme_pubr()+
    scale_color_manual(values = c("warm"="#95D055FF", "cold"="#404788FF"))+
    labs(title=df_name,
         fill = "Warm or Cold Year")+
    xlab(" ")+
    ylab(" ")
}

# creates length binned gear_temperature density plots
plot_den_ltemp <- function(df, df_name){
  ggplot(df) + 
    geom_density(aes(x=gear_temperature,
                     color = warm_cold)) +
    facet_wrap(~length_bins)+
    coord_cartesian(xlim=c(-2.5, 12.5))+
    th_den+
    theme_pubr()+
    scale_color_manual(values = c("warm"="#95D055FF", "cold"="#404788FF"))+
    labs(title=df_name,
         fill = "Warm or Cold Year")+
    xlab(" ")+
    ylab(" ")
}

# *** Foursquare Plot Functions -----

# boxplots

length_binned_temps_wc <- function(){
  p1 <- plot_box_temp_wc(akp, "Alaska Plaice")
  p2 <- plot_box_temp_wc(fhs, "Flathead Sole")
  p3 <- plot_box_temp_wc(nrs, "Northern Rock Sole")
  p4 <- plot_box_temp_wc(yfs, "Yellowfin Sole")
  plotlist <- list(p1, p2, p3, p4)
  return(plotlist)
}

length_binned_depths_wc <- function(){
  p1 <- plot_box_depth_wc(akp, "Alaska Plaice")
  p2 <- plot_box_depth_wc(fhs, "Flathead Sole")
  p3 <- plot_box_depth_wc(nrs, "Northern Rock Sole")
  p4 <- plot_box_depth_wc(yfs, "Yellowfin Sole")
  plotlist <- list(p1, p2, p3, p4)
  return(plotlist)
}

length_binned_temps_mhw <- function(){
  p1 <- plot_box_temp_mhw(akp, "Alaska Plaice")
  p2 <- plot_box_temp_mhw(fhs, "Flathead Sole")
  p3 <- plot_box_temp_mhw(nrs, "Northern Rock Sole")
  p4 <- plot_box_temp_mhw(yfs, "Yellowfin Sole")
  plotlist <- list(p1, p2, p3, p4)
  return(plotlist)
}

length_binned_depths_mhw <- function(){
  p1 <- plot_box_depth_mhw(akp, "Alaska Plaice")
  p2 <- plot_box_depth_mhw(fhs, "Flathead Sole")
  p3 <- plot_box_depth_mhw(nrs, "Northern Rock Sole")
  p4 <- plot_box_depth_mhw(yfs, "Yellowfin Sole")
  plotlist <- list(p1, p2, p3, p4)
  return(plotlist)
}


# density plots 

length_binned_temp_dens <- function(){
  p1 <- plot_den_ltemp(akp, "Alaska Plaice")
  p2 <- plot_den_ltemp(fhs, "Flathead Sole")
  p3 <- plot_den_ltemp(nrs, "Northern Rock Sole")
  p4 <- plot_den_ltemp(yfs, "Yellowfin Sole")
  plotlist <- list(p1, p2, p3, p4)
  return(plotlist)
}

length_binned_depth_dens <- function(){
  p1 <- plot_den_ldepth(akp, "Alaska Plaice")
  p2 <- plot_den_ldepth(fhs, "Flathead Sole")
  p3 <- plot_den_ldepth(nrs, "Northern Rock Sole")
  p4 <- plot_den_ldepth(yfs, "Yellowfin Sole")
  plotlist <- list(p1, p2, p3, p4)
  return(plotlist)
}

# *** Coordinator Functions -----

coordinate_boxplots <- function(groupvar, var){
  the_plot <- case_when(
    groupvar == "wc" & var == "temp" ~ length_binned_temps_wc(),
    groupvar == "wc" & var == "depth" ~ length_binned_depths_wc(),
    groupvar == "mhw" & var == "temp" ~ length_binned_temps_mhw(),
    groupvar == "mhw" & var =="depth" ~ length_binned_depths_mhw())
}

coordinate_density_plots <- function(bins, var){
  the_plot <- case_when(
    bins == "length" & var == "temp" ~ length_binned_temp_dens(),
    bins == "length" & var == "depth" ~ length_binned_depth_dens())
}




