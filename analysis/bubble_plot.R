library(PBSmapping)
library(tidyverse)
library(ggpubr)
library(here)
library(viridis)

# *** Load data ----
# ctrl-F find and replace to switch fish
# akp, fhs, nrs, yfs
akp <- read_rds(here("data/intermediates", "akp_phi.rds"))

# use PBSmapping to get spatial data
data('nepacLLhigh')

# *** Functions ----

# creates 8 bins for length in length_bins variable
make_length_bins <- function(df) {
  cutoffs <-
    quantile(df$length, probs = seq(0, 1, 0.125), na.rm = TRUE)
  df$length_bins <-
    cut(df$length, breaks = cutoffs, include.lowest = TRUE)
  return(df)
}

# *** Data Wrangle ----

bs <- nepacLLhigh %>% dplyr::select(group=PID, POS=POS,lon=X, lat=Y)

akp_for_plot <- akp %>%
  make_length_bins() %>%
  count(clust_id, cluster_lat, cluster_lon, length_bins) %>%
  group_by(length_bins) %>%
  mutate(total_in_bin =sum(n),
         prop = n/total_in_bin) %>%
  ungroup() %>%
  arrange(desc(prop))

# *** Plot ----
ggplot() + 
  geom_polygon(data = bs, 
               aes(lon, lat, group = group), 
               fill=8, color='black') +
  theme(panel.background = element_rect(fill = 'white')) + 
  xlab(expression(paste(Longitude^o,~'W'))) +
  ylab(expression(paste(Latitude^o,~'W')))+
  coord_map(xlim = c(-179, -158), ylim = c(54, 63)) +
  geom_point(data = akp_for_plot, 
             aes(cluster_lon,
                 cluster_lat, 
                 alpha = prop,
                 size = prop))+
    scale_size(range = c(0, 5))+
    facet_wrap(~length_bins)+
    theme_pubclean()



