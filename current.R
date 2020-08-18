# load libraries
library(tidyverse)
library(sf)
library(magrittr)

# NOTE: [[[10130=fhs 10210=yfs 10261=nrs(not sp. or juvenile) 10285=akp]]]

# so important
make_lowercase <- function(df){
  for (i in 1:length(names(df))){
    names(df)[i] <- tolower(names(df)[i])
  }
  return(df)
}


# load data
catch <- read_csv("data/data2/Shelf_Flatfish_Haul_Catch.csv", guess_max = 10000)
len <- read_csv("data/data2/Shelf_Flatfish_Haul_Catch_length.csv", guess_max = 10000)
# bot <- read_csv("data/bottom_type.csv", guess_max = 10000)
spec <- read_csv("data/Shelf_Flatfish_Haul_Specimen.csv", guess_max = 10000)
station_info <- readRDS("data/station_info.rds")


# make all lowercase
catch <- make_lowercase(catch)
len <- make_lowercase(len)
# bot <- make_lowercase(bot)
spec <- make_lowercase(spec)
station_info <- make_lowercase(station_info)


# filter catch
catch <- catch %>%
  filter(cruise>= 199999 & cruise<201900,
         region=="BS") %>%
  mutate(year=floor((cruise/100)))

# filter specimen
hauljoin_list <- unique(catch$hauljoin) #"copy" filtering from catch data
spec <- spec %>%
  filter(hauljoin %in% (hauljoin_list)) %>%
  mutate(year=as.integer(substr(start_time,8,9)))
remove(hauljoin_list)                                   

# filter and extend lengths
len_e <- len %>%
  filter(cruise>= 199999 & cruise<201900,
         region=="BS")%>%
  mutate(year=floor((cruise/100))) %>%
  group_by(hauljoin,species_code) %>%
  mutate(n_subsamp = sum(frequency))%>%
  group_by(hauljoin,species_code, length) %>%
  mutate(ss_prop=(frequency/n_subsamp),
         n_haul = (ss_prop*catch_number_fish))
remove(len)

#big blue plots (just to look at sst)
ggplot(catch)+
  geom_point(aes(x=start_longitude, y=start_latitude, color=surface_temperature))+
  facet_wrap(~year)

# determine avg lats and lons for each row in catch
catch <- catch %>%
  mutate(avg_lon = (start_longitude+end_longitude)/2,
         avg_lat = (start_latitude+end_latitude)/2)

# determine average lat and lon for each stationid throughout
# time by using the catch data averages and, again, averaging
st_avg_lat <- tapply(catch$avg_lat,catch$stationid,mean)
st_avg_lon <- tapply(catch$avg_lon,catch$stationid,mean)
index <- match(catch$stationid,names(st_avg_lon))
catch$st_avg_lon <- st_avg_lon[index]
catch$st_avg_lat <- st_avg_lat[index]

# add those values into the other dfs
index <- match(len_e$stationid,names(st_avg_lon))
len_e$st_avg_lon <- st_avg_lon[index]
len_e$st_avg_lat <- st_avg_lat[index]

index <- match(spec$stationid,names(st_avg_lon))
spec$st_avg_lon <- st_avg_lon[index]
spec$st_avg_lat <- st_avg_lat[index]

# try an index of depths bc they shouldn't change much
catch <- catch %>%
  group_by(stationid) %>%
  mutate(avg_depth = mean(bottom_depth))

# add average depth by stationid to catch
st_avg_depth <- tapply(catch$avg_depth , catch$stationid,mean)
index <- match(catch$stationid , names(st_avg_depth))
catch$st_avg_depth <- st_avg_depth[index]

# shrink down to only have information about stations
names(catch)
nums <- c(18,33,34,36)
station_deets <- catch[,nums]
station_info <- distinct(station_deets)

# qc
# length(unique(catch$stationid))

# save this df since it should ~remain constant
saveRDS(station_info, file = "data/station_info.rds")

# check things out
ggplot(station_info)+
  geom_point(aes(x = st_avg_lon, y=st_avg_lat, color=st_avg_depth))

# did I leave this because it was causing issues or is it just forgotten?
station_info <- na.omit(station_info)

###################################################################################

# gets the median while accounting for the number of times that a length is repeated per haul
get_median <- function(l, n){
  new_list <- rep(l, round(n))
  return(median(new_list, na.rm = TRUE))
}

# take interpolated haul lengths and try to make them easily plot-able
# by determining medians
plot_dat <- len_e %>%
  filter(species_code==10130) %>% # fhs
  group_by(stationid, year) %>% # try including sex?
  summarise(med = get_median(length, n_haul))

# combine with station_info
plot_dat1 <- inner_join(plot_dat, station_info, by="stationid")

# somewhat boring histogram
ggplot(plot_dat1) + geom_histogram(binwidth=.5,aes(x=st_avg_depth))

# try out finding depth bins
plot_dat2 <- plot_dat1
plot_dat2$depth_bins <- cut(plot_dat2$st_avg_depth, 4, include.lowest = TRUE)

# plot now with facet_grid and depth bins, mostly useless bc just lat/lon
ggplot(plot_dat2) + 
  #geom_smooth(aes(x=st_avg_depth, y=med)) +
  geom_point(aes(x=st_avg_lon, y=st_avg_lat, color=med)) +
  facet_grid(year~depth_bins)

# plot similarly but with density functions for the medians
ggplot(plot_dat2) + 
  #geom_smooth(aes(x=st_avg_depth, y=med)) +
  geom_density(aes(x=med)) +
  facet_grid(year~depth_bins)

###############################################################################

# try to get a temperature line to remove some confounding
# function that returns the average based on input year
get_avg_temp(yr){
  avg <- which(yearly_avg_temps$avg_temp[year==yr])
}
# manipulation that uses that get_avg_temp function
plot_dat_t <- len_e %>%
  filter(species_code==10130) %>%
  group_by(stationid, year) %>%
  summarise(avg_temp= mean(gear_temperature, na.rm = TRUE))

# take this new data that has the avg temps for lines and combine it with
# the data we jsut made length bins for

yearly_avg_temps <- catch %>% group_by(year) %>% summarise(avg_temp = mean(gear_temperature, na.rm=TRUE))
names(yearly_avg_temps)[2] <- "yr_avg_temp"

plot_dat4 <- left_join(plot_dat3, yearly_avg_temps, by="year.x")

# check it out with 
ggplot(plot_dat4) + 
  geom_point(aes(x=avg_temp, y=med), alpha=0.5) +
  facet_grid(year.x~depth_bins)+
  geom_vline(aes(xintercept=yr_avg_temp), color="red")

# try using lat/lon in grid
ggplot(plot_dat2) + 
  geom_point(aes(x=year, y=st_avg_depth, color=med)) +
  facet_grid(st_avg_lat~st_avg_lon)

# NO. NEED TO TRY "residual" idea
