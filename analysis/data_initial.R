#' title: Eastern Bering Sea Continental Shelf Survey Flatfish Visualizations Initial Data Cleaning
#' purpose: This script cleans the data and creates separate species files for Alaska plaice
#'          flathead sole, northern rock sole, and yellowfin sole from 2000-2018 based on 
#'          the RACE EBS survey
#' date: 2020-12-7
#' ---


# Set environment ----

# *** Libraries ----
library(tidyverse)
library(here)
library(leaderCluster)

# *** Import Data -----
len <- read_csv(here("data", "Shelf_Flatfish_Haul_Catch_Length.csv"), guess_max = 10000)
catch <- read_csv(here("data", "Shelf_Flatfish_Haul_Catch.csv"), guess_max = 10000)
phi <- read_csv(here("data", "Arrowtooth_Eastern_BS_has_Phi.csv"), guess_max = 10000)

# (delete later)
# "Here is a file of Arrowtooth Flounder catch from the EBS groundfish survey with a column of 
# sediment size (phi). I think phi is = -ln_2(sediment_size). So the larger phi indicates fine 
# size (e.g., mud) and smaller phi indicates coarse size (e.g., sand). In general, there phi 
# increases with depth, so the two variables (depth and phi) are correlated and often redundant."
# -Lorenzo about phi dataset

# *** Create Var ----
# all categories wanted for output saved length data
# categories_wanted <- c("hauljoin", "species_code", "start_latitude", "end_latitude", 
#                        "start_longitude", "end_longitude", "stationid", "bottom_depth",
#                        "gear_temperature","length", "sex", "catch_weight", "year", 
#                        "n_haul", "warm_cold", "marine_heat_wave", "phi", "clust_id", 
#                        "cluster_lat", "cluster_lon")
# switched to be categories unwanted to generalize separate_species() func
categories_unwanted <- c("region", "vessel", "cruise", "haul", "performance", "start_time",
                         "duration", "distance_fished", "net_width", "net_measured",
                         "net_height", "stratum", "gear_depth", "bottom_type",
                         "surface_temperature","wire_length", "gear", "abundance_haul",
                         "frequency", "catch_number_fish","n_specimen", "length_prop")
index_categories_unwanted <- c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 19, 21, 22, 24,
                               25, 26, 29, 32, 34, 35)

# Functions -------
# makes all the column names lowercase
make_lowercase <- function(df){
  for (i in 1:length(names(df))){
    names(df)[i] <- tolower(names(df)[i])
  }
  return(df)
}
# separates out species and gets rid of unnecessary cols
separate_species <- function(spec_code, start_df){
  dat <- start_df %>%
    filter(species_code==spec_code)
  dat <- dat[,-index_categories_unwanted]
  dat <- dat %>%
    make_one_row_per_fish()
}
# lengthens df so that each observation (each fish) has a row
make_one_row_per_fish <- function(df){
  new_df <- df %>%
    uncount(round(n_haul)) %>%
    subset(select =c(-n_haul))
}


# Organize the different datasets -----
# 1) Catch data ----
catch <- catch %>%
  make_lowercase() %>%
  filter(cruise>= 199999 & cruise<201900,
         region=="BS") %>%
  mutate(year=floor((cruise/100))) 

# 2) Warm and cold years ----
# create dataframe of each individual haul from 50m to 100m 
warm_cold_df <- catch %>%
  filter(bottom_depth > 50 & bottom_depth <100) %>%
  distinct(hauljoin, .keep_all = TRUE)
# use that dataframe to get the overall average temp from 50-100m
overall_temp_mean <- mean(warm_cold_df$gear_temperature, na.rm = TRUE)
# create dataframe of yearly temp group and temperature
year_temp_categories <- warm_cold_df %>%
  group_by(year) %>%
  summarise(yr_temp = mean(gear_temperature, na.rm = TRUE)) %>%
  mutate(warm_cold = ifelse(yr_temp>overall_temp_mean, "warm", "cold")) 
# now create list of warm years
warm_years <- year_temp_categories$year[which(year_temp_categories$warm_cold == 'warm')]

# 3) Length data ----
# create len_e which has lengths extrapolated to haul level by species
len_extended <- len %>%
  make_lowercase() %>%
  # get only 2000-2018 and only Bering Sea
  filter(cruise>= 199999 & cruise<201900,
         region=="BS")%>%
  # create year variable
  mutate(year=floor((cruise/100))) %>%
  # expands the sample to get the number of each species in each haul
  # get all of one species for one haul together
  group_by(hauljoin,species_code) %>%
  # make 'n_specimen` which is the total for each haul for a given species
  # that were lengthed
  mutate(n_specimen = sum(frequency))%>%
  # now get all species of a specific length and a specific haul
  group_by(hauljoin,species_code, length) %>%
  # get 'length_prop' which is the proportion of the total specimens
  # for a given species and a given haul which are at each length
  mutate(length_prop=(frequency/n_specimen),
         # take that proportion and multiply it by the number of fish for
         # the given species and length that were caught in the given haul
         n_haul = (length_prop*catch_number_fish)) %>%
  ungroup() %>%
  # gets rid of missing temperature, depth, and length values
  filter(!is.na(gear_temperature),
         !is.na(bottom_depth),
         !is.na(length)) %>%
  # creates a variable that splits warm and cold up 
  mutate(warm_cold = ifelse(year %in% warm_years, "warm", "cold"),
         # add in var for which years were extreme marine heat wave
         marine_heat_wave = ifelse(year %in% c(2015, 2016, 2019), "Extreme Marine Heat Wave Years",
                                   "Other Years"))

# 5) Arrowtooth (phi) data ----
# get only overlap years
phi_filtered <- phi %>%
  filter(year>=2000 & year <=2010)%>%
  select(haul, year, lat, lon, depth, phi)
# make into matrix for leaderCluster function
phi_mat <- matrix(c(phi_filtered$lat,phi_filtered$lon), ncol=2)
# find clusters. radius was determined through trial and error
# see phi_data work session
phi_clusts <- leaderCluster(points=phi_mat, radius=.175)
# get df with cluster id matched with cluster centroid lat/lons
phi_centroids <- data.frame(clust_id = factor(seq(1, phi_clusts$num_clusters, by=1)), 
                            clust_lat = phi_clusts$cluster_centroids[,1],
                            clust_lon = phi_clusts$cluster_centroids[,2])
# put it all together
phi_updated <- phi_filtered %>% 
  mutate(clust_id = factor(phi_clusts$cluster_id)) %>%
  left_join(phi_centroids, by="clust_id")

lat_list <- c(len_extended$start_latitude, phi_updated$clust_lat)
lon_list <- c(len_extended$start_longitude, phi_updated$clust_lon)
combined_mat <- matrix(c(lat_list, lon_list), ncol=2)
combined_clusts <- leaderCluster(points=combined_mat, radius = 0.175)

split_cluster_ids_LENGTH <- combined_clusts$cluster_id[1:nrow(len_extended)]
split_cluster_ids_PHI <- combined_clusts$cluster_id[(nrow(len_extended)+1):length(combined_clusts$cluster_id)]

combined_centroids <- combined_centroids <- data.frame(clust_id = seq(1, combined_clusts$num_clusters, by=1), 
                                                       cluster_lat = combined_clusts$cluster_centroids[,1],
                                                       cluster_lon = combined_clusts$cluster_centroids[,2])

phi_final <- phi_updated %>% 
  mutate(clust_id = as.numeric(split_cluster_ids_PHI)) %>%
  select(phi, clust_id) %>%
  write_rds(path = here("data", "intermediates/phi.rds"))

possible_clust_ids <- unique(phi_final$clust_id)

len_updated_with_phi <- len_extended %>%
  mutate(clust_id = as.numeric(split_cluster_ids_LENGTH)) %>%
  left_join(combined_centroids, by="clust_id") %>%
  filter(clust_id %in% possible_clust_ids) %>%
  left_join(phi_final, by="clust_id")


# 6) Divide out by species ----
# Alaska plaice
akp <- separate_species(10285, len_extended)
akp_phi <- separate_species(10285, len_updated_with_phi)
# flathead sole
fhs <- separate_species(10130, len_extended)
fhs_phi <- separate_species(10130, len_updated_with_phi)
# northern rock sole
nrs <- separate_species(10261, len_extended)
nrs_phi <- separate_species(10261, len_updated_with_phi)
# yellowfin sole
yfs <- separate_species(10210, len_extended)
yfs_phi <- separate_species(10210, len_updated_with_phi)

  
# 7) Save output ------
write_rds(catch, path = here("data", "intermediates/catch.rds"))
write_rds(year_temp_categories, here(path = "data", "/intermediates/year_temp_categories.rds"))
write_rds(overall_temp_mean, here(path="data", "/intermediates/overall_temp_mean.rds"))
# just length
write_rds(len_extended, path = here("data", "intermediates/length_extended_orig.rds"))
# just phi
write_rds(phi_final, path = here("data", "intermediates/length_with_phi.rds"))
# akp 
write_rds(akp, path = here("data", "intermediates/akp.rds"))
write_rds(akp_phi, path = here("data", "intermediates/akp_phi.rds"))
# fhs
write_rds(fhs, path = here("data", "intermediates/fhs.rds"))
write_rds(fhs_phi, path = here("data", "intermediates/fhs_phi.rds"))
# nrs
write_rds(nrs, path = here("data", "intermediates/nrs.rds"))
write_rds(nrs_phi, path = here("data", "intermediates/nrs_phi.rds"))
# yfs
write_rds(yfs, path = here("data", "intermediates/yfs.rds"))
write_rds(yfs_phi, path = here("data", "intermediates/yfs_phi.rds"))