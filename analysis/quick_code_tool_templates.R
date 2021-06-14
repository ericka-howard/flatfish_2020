# Quick Code Example Templates

# save plots -------------------------------------------------
# as tiff
filename_tiff <- "alaska_plaice_marine_heat_wave_contour.tiff"
ggsave(
  here("results/name-of-plot-folder",
       filename_tiff),
  plot = the_plot_I_want,
  width = 500,
  height = 500,
  units = "mm",
  dpi = 600
)
# as png
filename_png <- "alaska_plaice_marine_heat_wave_contour.png"
ggsave(
  here("results/name-of-plot-folder",
       filename_png),
  plot = the_plot_I_want,
  width = 500,
  height = 500,
  units = "mm"
)


# making grids ---------------------------------------------

# 1) legend --------------------
# get a legend object to use in cowplot/gridExtra/whatever else
legend <- get_legend(ggplot_object_with_legend)

# 2) title ---------------------
title <-
  ggdraw() + draw_label(paste("\n", fish_name, "\n"),
                        fontface = 'bold',
                        size = 20)


# 3) panels --------------------
panel_a <- annotate_figure(
  fish_plot_a,
  fig.lab = "A.",
  fig.lab.pos = "top.left",
  fig.lab.size = 14
)
panel_b <- annotate_figure(
  fish_plot_b + theme(legend.position = "none"),
  fig.lab = "B.",
  fig.lab.pos = "top.left",
  fig.lab.size = 14
)
panel_c <- annotate_figure(
  fish_plot_c,
  fig.lab = "C.",
  fig.lab.pos = "top.left",
  fig.lab.size = 14
)
panel_d <- annotate_figure(
  fish_plot_d,
  fig.lab = "D.",
  fig.lab.pos = "top.left",
  fig.lab.size = 14
)
# 4) make into rows
row1 <- plot_grid(panel_a, panel_b, nrow = 1)
row2 <- plot_grid(panel_c, panel_d, nrow = 1)
# 5) put all together
fish_grid <-
  plot_grid(
    title,
    row1,
    row2,
    legend,
    nrow = 4,
    rel_heights = c(0.04, 0.3, 0.3, 0.05)
  )
