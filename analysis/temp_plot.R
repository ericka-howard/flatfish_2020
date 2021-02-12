library(PBSmapping)
library(tidyverse)
library(ggpubr)
library(here)
library(viridis)

# *** Load data ----
# use phi because it has the entire station list set up
phi <- read_rds(here("data/intermediates", "length_with_phi.rds"))

data('nepacLLhigh')
bs <- nepacLLhigh %>% dplyr::select(group=PID, POS=POS,lon=X, lat=Y)

temp_means_and_cvs <- phi %>%
  mutate(gear_temp_kelvins = gear_temperature+273.15) %>%
  group_by(clust_id, cluster_lat, cluster_lon) %>%
  summarise(means_c = mean(gear_temperature, na.rm = T),
            means_kelvins = mean(gear_temp_kelvins, na.rm = T),
            sds_kelvins = sd(gear_temp_kelvins, na.rm = T),
            cvs_kelvins = sds_kelvins/means_kelvins) %>%
  ungroup()


# *** Plot ----
mean_temps <- ggplot() + 
  geom_polygon(data = bs, 
               aes(lon, lat, group = group), 
               fill=8, color='black') +
  theme(panel.background = element_rect(fill = 'white')) + 
  xlab(expression(paste(Longitude^o,~'W'))) +
  ylab(expression(paste(Latitude^o,~'W')))+
  coord_map(xlim = c(-179, -158), ylim = c(54, 63)) +
  geom_point(data = temp_means_and_cvs, 
             aes(cluster_lon,
                 cluster_lat, 
                 color = means_c),
             size =3.5)+
  scale_colour_viridis(option="cividis")+
  theme_pubclean()+
  labs(title="Mean Temperature at Each Station 2000-2018 (°C)")


cv_temps_kelvins <- ggplot() + 
  geom_polygon(data = bs, 
               aes(lon, lat, group = group), 
               fill=8, color='black') +
  theme(panel.background = element_rect(fill = 'white')) + 
  xlab(expression(paste(Longitude^o,~'W'))) +
  ylab(expression(paste(Latitude^o,~'W')))+
  coord_map(xlim = c(-179, -158), ylim = c(54, 63)) +
  geom_point(data = temp_means_and_cvs, 
             aes(cluster_lon,
                 cluster_lat, 
                 color = cvs_kelvins),
             size =3.5)+
  scale_colour_viridis(option = "cividis")+
  theme_pubclean()+
  labs(title="Coefficient of Variation at Each Station 2000-2018",
       caption = "Note that coefficient of variation is calculated 
       after converting all temperatures to Kelvin")

ggarrange(mean_temps, cv_temps_kelvins, nrow=1)
