#' title:   Eastern Bering Sea Continental Shelf Survey Flatfish Visualizations
#' purpose: This script contains the functions used to create ggplot objects
#' (saved as rds files) for contour and box plots of Alaska plaice, flathead
#' sole, northern rock sole, and yellowfin sole from 2000-2018 based on the
#' RACE EBS survey for extreme marine heat wave years (2016 & 2018) vs. Cold
#' years and warm vs. cold years
#' date:    2021-06-13
#' author: Ericka B. Smith
#' ---

# Input Options
# 1) fish
# 2) marine heat wave or warm cold
# 3) temp or depth


# Data Management ----
# returns dataframe with yvars prepped for plotting
make_factor_levels_nice <- function(df) {
  df <- df %>%
    drop_na(marine_heat_wave)
  df$marine_heat_wave %<>% factor()
  df$warm_cold %<>% factor()
  levels(df$warm_cold) <- c("Cold Years", "Warm Years")
  return(df)
}
# creates 8 bins for plotting length in an easier to understand way
make_length_bins <- function(df) {
  cutoffs <-
    quantile(df$length, probs = seq(0, 1, 0.125), na.rm = TRUE)
  df$length_bins <-
    cut(df$length, breaks = cutoffs, include.lowest = TRUE)
  return(df)
}

# Plotting Functions ----
# creates length binned boxplots
# y & y_name should be depth or temp related
# y_lims will be associated with the same
# mwh_bool TRUE gives plots with marine heat wave  groupings,
# mhw_bool FALSE gives plots with warm_cold groupings
make_boxplot <- function(df, df_name, y, y_name, ylims, mhw_bool) {
  yvar <- enquo(y)
  ggplot(df) +
    geom_boxplot(
      aes(
        x = length_bins,
        y = !!yvar,
        fill = ifelse(mhw_bool == TRUE,
                      marine_heat_wave,
                      warm_cold),
        outlier.shape = NA
      )) +
        coord_cartesian(ylim = ylims) +
        theme_pubr() +
        theme(
          legend.position = "top",
          legend.title = element_blank(),
          legend.text = element_text(size = 16),
          axis.title = element_text(size = 16)
        ) +
        scale_fill_manual(values =
                            ifelse(
                              mhw_bool == TRUE,
                              c(
                                "Extreme Marine Heat Wave Years" = "#95D055FF",
                                "Cold Years" = "#404788FF"
                              ),
                              c("Warm Years" = "#95D055FF",
                                "Cold Years" = "#404788FF")
                            )) +
        labs(x = " ",
             y = " ",
             fill = " ")
}
# creates contour plots
# y & y_name should be depth or temp related
# y_lims will be associated with the same
# mwh_bool TRUE gives plots with marine heat wave  groupings,
# mhw_bool FALSE gives plots with warm_cold groupings
make_contour_plot <-
  function(df, df_name, y, y_name, ylims, mhw_bool) {
    yvar <- enquo(y)
    ggplot(df) +
      geom_density2d(aes(
        x = length,
        y = !!yvar,
        color = ifelse(mhw_bool == TRUE,
                       marine_heat_wave,
                       warm_cold)
      ),
      size = 1) +
      facet_wrap( ~ marine_heat_wave) +
      coord_cartesian(ylim = ylims) +
      labs(
        title = " ",
        x = " ",
        y = y_name,
        fill = " "
      ) +
      scale_color_manual(values =
                           ifelse(
                             mhw_bool == TRUE,
                             c(
                               "Extreme Marine Heat Wave Years" = "#95D055FF",
                               "Cold Years" = "#404788FF"
                             ),
                             c("Warm Years" = "#95D055FF",
                               "Cold Years" = "#404788FF")
                           )) +
      theme_pubr() +
      theme(
        panel.spacing.x = unit(1, "line"),
        panel.border = element_rect(color = "black",
                                    fill = "transparent"),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title = element_text(size = 16),
        legend.position = "none",
        legend.title = element_blank()
      )
  }

# runs through each type of plot for a given fish
# saves out results as rdata objects
make_plots <-
  function(fish_df,
           fish_name,
           ylimits_depth,
           ylimits_temp,
           is_marine_heat_wave) {
    p1 <- make_contour_plot(
      fish_df,
      fish_name,
      gear_temperature,
      "Temperature (°C)",
      ylims = ylimits_temp,
      mhw_bool = is_marine_heat_wave
    )
    saveRDS(p1, here(
      "results/individual_ggplot_objects",
      paste(
        fish_name,
        "_temp_contour_",
        ifelse(is_marine_heat_wave ==
                 TRUE,
               "mhw.rds",
               "wc.rds"),
        sep = ""
      )
    ))
    rm(p1)
    p2 <- make_boxplot(
      fish_df,
      fish_name,
      gear_temperature,
      "Temperature (°C)",
      ylims = ylimits_temp,
      mhw_bool = is_marine_heat_wave
    )
    saveRDS(p2, here(
      "results/individual_ggplot_objects",
      paste(
        fish_name,
        "_temp_box_",
        ifelse(is_marine_heat_wave ==
                 TRUE,
               "mhw.rds",
               "wc.rds"),
        sep = ""
      )
    ))
    rm(p2)
    p3 <- make_contour_plot(
      fish_df,
      fish_name,
      bottom_depth,
      "Bottom Depth (m)",
      ylims = ylimits_depth,
      mhw_bool = is_marine_heat_wave
    )
    p3r <- p3 + scale_y_reverse() + xlab("Length(mm)")
    saveRDS(p3, here(
      "results/individual_ggplot_objects",
      paste(
        fish_name,
        "_depth_contour_",
        ifelse(is_marine_heat_wave ==
                 TRUE,
               "mhw.rds",
               "wc.rds"),
        sep = ""
      )
    ))
    rm(p3)
    p4 <- make_boxplot(
      fish_df,
      fish_name,
      bottom_depth,
      "Bottom Depth (m)",
      ylims = ylimits_depth,
      mhw_bool = is_marine_heat_wave
    )
    p4r <- p4 +
      scale_y_reverse() +
      xlab("Length(mm)") +
      theme(legend.position = "none")
    saveRDS(p4, here(
      "results/individual_ggplot_objects",
      paste(
        fish_name,
        "_depth_box_",
        ifelse(is_marine_heat_wave ==
                 TRUE,
               "mhw.rds",
               "wc.rds"),
        sep = ""
      )
    ))
    rm(p4)
    }

# end of file
