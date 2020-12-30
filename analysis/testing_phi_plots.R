make_contour_plot_testing <- function(df, 
                              df_name, 
                              y, 
                              y_name, 
                              fill_var, 
                              fill_if_statement){
  yvar <- enquo(y)
  ggplot(df)+
    geom_density2d(aes(x=length, 
                       y=!!yvar, 
                       color=marine_heat_wave),
                   size=1)+
    facet_wrap(~marine_heat_wave)+
    #coord_cartesian(ylim=ylims)+
    labs(title = " ",
         x = " ",
         y = y_name,
         fill = " ")+
    # ifelse(fill_if_statement == "mhw",
    #        scale_fill_manual(values = 
    #                            c("Extreme Marine Heat Wave Years"=
    #                                "#95D055FF",
    #                              "Other Years"=
    #                                "#404788FF")),
    #        scale_fill_manual(values = 
    #                            c("Warm Years"=
    #                                "#95D055FF", 
    #                              "Cold Years"=
    #                                "#404788FF"))) +
    theme_pubr()+
    theme(panel.spacing.x = unit(1,"line"),
          legend.position = "none",
          panel.border = element_rect(color = "black",
                                      fill = "transparent"),
          strip.background = element_blank(),
          strip.text.x = element_blank(),
          axis.title.y = element_text(size = 16),
          legend.title = element_blank())
}

make_contour_plot_testing(df=akp,
                  df_name = "Alaska Plaice",
                  y=phi,
                  y_name = "Phi",
                  fill_var=marine_heat_wave,
                  fill_if_statement = "mhw")
