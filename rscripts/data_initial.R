#' title: EBS Continental Shelf Survey Flatfish Visualizations
#' purpose: This script cleans the data and creates separate species files for Alaska plaice
#'          flathead sole, northern rock sole, and yellowfin sole from 2000-2018 based on 
#'          the RACE EBS survey
#' date: 2020-11-21
#' ---

# Libraries ---------

library(tidyverse)
library(sf)
library(magrittr)
library(viridis)
library(grDevices)
library(ggpubr)
library(here)

# Import Data -----

len <- read_csv("././data/data2/Shelf_Flatfish_Haul_Catch_Length.csv", guess_max = 10000)
haul <- read_csv("././data/data2/Shelf_Flatfish_Haul_Catch.csv", guess_max = 10000)
catch <- read_csv("././data/data2/Shelf_Flatfish_Haul_Catch.csv", guess_max = 10000)

# Functions -------

# *** Data Management Functions -----

# makes all the column names lowercase
make_lowercase <- function(df){
  for (i in 1:length(names(df))){
    names(df)[i] <- tolower(names(df)[i])
  }
  return(df)
}

# takes interpolated haul lengths and split off a species
separate_spec <- function(spec_code){
  dat <- len_e %>%
    filter(species_code==spec_code)
  return(dat)
}

# Data Wrangle -----

# make all lowercase
len <- make_lowercase(len)
haul <- make_lowercase(haul)
catch <- make_lowercase(catch)

# get haul set up 
haul <- haul %>%
  # get only 2000-2018 and only Bering Sea
  filter(cruise>= 199999 & cruise<201900,
         region=="BS")%>%
  # create year variable
  mutate(year=floor((cruise/100)))

# filter catch
catch <- catch %>%
  filter(cruise>= 199999 & cruise<201900,
         region=="BS") %>%
  mutate(year=floor((cruise/100)))

# create dataframe of each individual haul from 50m to 100m 
# (for warm cold variable determination)
warm_cold_df <- haul %>%
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

# create len_e which has lengths extrapolated to haul level by species
len_e <- len %>%
  # get only 2000-2018 and only Bering Sea
  filter(cruise>= 199999 & cruise<201900,
         region=="BS")%>%
  # create year variable
  mutate(year=floor((cruise/100))) %>%
  # expands the sample to get the number of each species in each haul
  group_by(hauljoin,species_code) %>%
  mutate(n_subsamp = sum(frequency))%>%
  group_by(hauljoin,species_code, length) %>%
  mutate(ss_prop=(frequency/n_subsamp),
         n_haul = (ss_prop*catch_number_fish)) %>%
  # gets rid of missing temperature, depth, and length values
  filter(!is.na(gear_temperature),
         !is.na(bottom_depth),
         !is.na(length)) %>%
  # creates a variable that splits warm and cold up 
  mutate(warm_cold = ifelse(year %in% warm_years, "warm", "cold"))

# removes the large length dataset from our environment (bc we have the expanded one now)
remove(len)

# divide out species
akp <- separate_spec(10285)
fhs <- separate_spec(10130)
nrs <- separate_spec(10261)
yfs <- separate_spec(10210)
# removes the large length expanded dataset from our environment (bc we have one per species now)
remove(len_e)

# subset out only wanted categories (not done earlier bc don't want speciesid category)
categories_wanted <- c("hauljoin", "start_latitude", "end_latitude", "start_longitude", 
                       "end_longitude", "stationid", "bottom_depth","gear_temperature", 
                       "length", "sex", "catch_weight", "year", "n_haul", "warm_cold")
akp <- akp[,categories_wanted]
fhs <- fhs[,categories_wanted]
nrs <- nrs[,categories_wanted]
yfs <- yfs[,categories_wanted]

# write out all files
write_rds(akp, path = "././data/intermediates/akp.rds")
write_rds(fhs, path = "././data/intermediates/fhs.rds")
write_rds(nrs, path = "././data/intermediates/nrs.rds")
write_rds(yfs, path ="././data/intermediates/yfs.rds")
write_rds(year_temp_categories, path = "././data/intermediates/year_temp_categories.rds")
write_rds(catch, path = "././data/intermediates/catch.rds")
write_rds(haul, path = "././data/intermediates/haul.rds")