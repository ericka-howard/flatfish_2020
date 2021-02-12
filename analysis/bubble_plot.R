library(PBSmapping)
library(tidyverse)
library(ggpubr)
library(here)
library(viridis)

# *** Load data ----

akp <- read_rds(here("data/intermediates", "akp_phi.rds"))

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
  count(clust_id, cluster_lat, cluster_lon, length_bins)

new_akp <- akp_for_plot %>%
  group_by(length_bins) %>%
  mutate(total_in_bin =sum(n),
         prop = n/total_in_bin) %>%
  ungroup()

# going to try removing <0.01 just to see
newest_akp <- new_akp %>%
  arrange(desc(prop))
  
  # data %>%
  # arrange(desc(pop)) %>%
  # mutate(country = factor(country, country)) %>%
  # ggplot(aes(x=gdpPercap, y=lifeExp, size = pop)) +
  # geom_point(alpha=0.5) +
  # scale_size(range = c(.1, 24), name="Population (M)")

# *** Plot ----
ggplot() + 
  geom_polygon(data = bs, 
               aes(lon, lat, group = group), 
               fill=8, color='black') +
  theme(panel.background = element_rect(fill = 'white')) + 
  xlab(expression(paste(Longitude^o,~'W'))) +
  ylab(expression(paste(Latitude^o,~'W')))+
  coord_map(xlim = c(-179, -158), ylim = c(54, 63)) +
  geom_point(data = newest_akp, 
             aes(cluster_lon,
                 cluster_lat, 
                 #alpha = prop,
                 size = prop,#size=prop,
                 color = length_bins),
             alpha=0.3)+
    scale_size(range = c(0, 5))+
    scale_color_viridis(discrete = T)+
    #facet_wrap(~length_bins)+
    theme_pubclean()



