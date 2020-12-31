#' title:   Eastern Bering Sea Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates contour and box plots of Alaska plaice, flathead sole,
#'          northern rock sole, and yellowfin sole from 2000-2018 based on the RACE EBS
#'          survey
#' date:    2020-12-31
#' ---

# Data Management Bits ----
# returns a dataframe with all of the wanted binning
make_wc_factor_levels_nice <- function(df) {
  df$warm_cold %<>% factor()
  levels(df$warm_cold) <- c("Cold Years", "Warm Years")
  return(df)
}

# creates 8 bins for length in length_bins variable
make_length_bins <- function(df) {
  cutoffs <- quantile(df$length, probs = seq(0, 1, 0.125), na.rm = TRUE)
  df$length_bins <-
    cut(df$length, breaks = cutoffs, include.lowest = TRUE)
  return(df)
}

# Plotting Functions ----
# creates length binned boxplots
make_boxplot <- function(df, df_name, y, y_name, ylims) {
  yvar <- enquo(y)
  ggplot(df) +
    geom_boxplot(aes(x = length_bins,
                     y = !!yvar,
                     fill = warm_cold),
                 outlier.shape = NA) +
    # outlier.size = 1,
    # outlier.color = "grey",
    # outlier.shape=1) +
    coord_cartesian(ylim = ylims) +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 12),
      legend.background = element_rect(fill = "lightgrey"),
      legend.key.size = unit(1.5, "cm"),
      legend.key.width = unit(0.5, "cm")
    ) +
    theme_pubr() +
    scale_fill_manual(values = c("Warm Years" = "#95D055FF", "Cold Years" =
                                   "#404788FF")) +
    labs(x = " ",
         y = " ",
         fill = " ")
}
# creates contour plots
make_contour_plot <- function(df, df_name, y, y_name, ylims) {
  yvar <- enquo(y)
  ggplot(df) +
    geom_density2d(aes(
      x = length,
      y = !!yvar,
      color = warm_cold
    ),
    size = 1) +
    facet_wrap( ~ warm_cold) +
    coord_cartesian(ylim = ylims) +
    labs(
      title = " ",
      x = " ",
      y = y_name,
      fill = " "
    ) +
    scale_color_manual(values = c("Warm Years" = "#95D055FF", "Cold Years" =
                                    "#404788FF")) +
    theme_pubr() +
    theme(
      panel.spacing.x = unit(1, "line"),
      legend.position = "none",
      panel.border = element_rect(color = "black",
                                  fill = "transparent"),
      strip.background = element_blank(),
      strip.text.x = element_blank(),
      axis.title.y = element_text(size = 16),
      legend.title = element_blank()
    )
}

# makes list for four square plots
make_foursquare_plotlist <-
  function(fish_df,
           fish_name,
           ylimits_depth,
           ylimits_temp) {
    p1 <- make_contour_plot(fish_df,
                            fish_name,
                            gear_temperature,
                            "Temperature (°C)",
                            ylims = ylimits_temp)
    p2 <- make_boxplot(fish_df,
                       fish_name,
                       gear_temperature,
                       "Temperature (°C)",
                       ylims = ylimits_temp)
    p3 <- make_contour_plot(fish_df,
                            fish_name,
                            bottom_depth,
                            "Bottom Depth (m)",
                            ylims = ylimits_depth)
    p3r <- p3 + scale_y_reverse()
    p4 <- make_boxplot(fish_df,
                       fish_name,
                       bottom_depth,
                       "Bottom Depth (m)",
                       ylims = ylimits_depth)
    p4r <- p4 + scale_y_reverse()
    plotlist <- list(p1, p2, p3r, p4r)
    return(plotlist)
  }