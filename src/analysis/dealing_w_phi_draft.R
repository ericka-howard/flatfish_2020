# This is the script I used to figure out how to get phi
# data added to our length data.
# It includes various QC checks

# load libraries
library(tidyverse)
library(here)
library(leaderCluster)

# load data
catch <- read_csv(here("data", "Shelf_Flatfish_Haul_Catch.csv"), guess_max = 10000)
phi <- read_csv(here("data", "Arrowtooth_Eastern_BS_has_Phi.csv"), guess_max = 10000)

# I hate uppercase
make_lowercase <- function(df){
  for (i in 1:length(names(df))){
    names(df)[i] <- tolower(names(df)[i])
  }
  return(df)
}

catch <- make_lowercase(catch)

# filter catch so that it goes 2000 to 2010 and
# only includes hauls 1-300
# then create year var
# and get rid of all vars except haul, lat, long, stationid, and year
catch_filter <- catch %>%
  filter(cruise>=200000 & cruise <=201001,
         region=="BS",
         haul <=300) %>%
  mutate(year=floor((cruise/100))) %>%
  select(haul, start_latitude, start_longitude, stationid,year)

# filter phi so it only includes 2000 to 2010
# get rid of unnecessary columns
phi_filter <- phi%>%
  filter(year>=2000 & year <=2010)%>%
  select(haul, year, lat, lon, phi)

# create an even small phi 
phi_dat <- phi[,c("lat","lon")]

# make a matrix of phi
phi_mat <- matrix(c(phi_dat$lat,phi_dat$lon), ncol=2)

# get cluster ids for spatial subsetting
clusts <- leaderCluster(points=phi_mat, radius=.175)

# add cluster ids to shorty phi data
phi_dat <- phi_dat %>% 
  mutate(clust_id = factor(clusts$cluster_id))

# plot a small amount to see if clusters are correct
phi_dat %>%
  filter(lon >(-170) &lon<(-169),
         lat >58 & lat <59) %>%
  ggplot()+
  geom_point(aes(x=lon, y=lat, color=clust_id))

# plot all together to see if it worked
compare_phi_clust <- phi_dat %>%
  group_by(clust_id) %>%
  summarise_at(vars(lat:lon), mean, na.rm=TRUE)

ggplot()+
  geom_point(data=phi_dat, aes(x=lon, y=lat), color="red", alpha=0.5)+
  geom_point(data=compare_phi_clust, aes(x=lon, y=lat))

# it worked!

# now to add that back to length data...
# get phi matched up with hauljoin vars
# first check if year matters or if we can just average phi
# oops just realized I used the phi that isn't filtered to 
# only be 2000-2010
phi_all_together <- phi %>%
  mutate(clust_id = clusts$cluster_id)

ggplot(phi_all_together)+
  geom_point(aes(x=clust_id, y=phi, color=year))

# not sure what that tells me
############################################################
# Let's try again with the correct years

phi_dat <- phi_filter[,c("lat","lon")]

# make a matrix of phi
phi_mat <- matrix(c(phi_dat$lat,phi_dat$lon), ncol=2)

# get cluster ids for spatial subsetting
clusts <- leaderCluster(points=phi_mat, radius=.175)

# add cluster ids to shorty phi data
phi_dat <- phi_dat %>% 
  mutate(clust_id = factor(clusts$cluster_id))

# plot a small amount to see if clusters are correct
phi_dat %>%
  filter(lon >(-170) &lon<(-169),
         lat >58 & lat <59) %>%
  ggplot()+
  geom_point(aes(x=lon, y=lat, color=clust_id))

# plot all together to see if it worked
compare_phi_clust <- phi_dat %>%
  group_by(clust_id) %>%
  summarise_at(vars(lat:lon), mean, na.rm=TRUE)

ggplot()+
  geom_point(data=phi_dat, aes(x=lon, y=lat), color="red", alpha=0.5)+
  geom_point(data=compare_phi_clust, aes(x=lon, y=lat))

# Cool, first part still looks correct, as expected.
# Back to trying to see if year matters for phi

phi_all_together <- phi_filter %>%
  mutate(clust_id = clusts$cluster_id,
         year = factor(year))
#  create a range variable for ranges of phi at a cluster over the years
phi_ranges <- phi_all_together %>%
  group_by(clust_id) %>%
  summarise(rang = range(phi, na.rm=TRUE)[2]-range(phi, na.rm = TRUE)[1])

# make a boxplot with all phi vals and a boxplot with all phi ranges        
ggplot()+
  geom_boxplot(data=phi_ranges, aes(x=rang), fill="orange")

ggplot(phi_all_together)+
  geom_boxplot(aes(x=phi))

# determine which clusters have "large" ranges for phi
large_range_clust_ids <- phi_ranges$clust_id[which(phi_ranges$rang >0.2)]

phi_all_together %>%
  filter(clust_id %in% large_range_clust_ids) %>%
  ggplot()+
  geom_point(aes(x=lon, y=lat))
  
