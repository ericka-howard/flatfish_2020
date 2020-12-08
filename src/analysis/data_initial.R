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
categories_wanted <- c("hauljoin", "species_code", "start_latitude", "end_latitude", 
                       "start_longitude", "end_longitude", "stationid", "bottom_depth",
                       "gear_temperature","length", "sex", "catch_weight", "year", 
                       "n_haul", "warm_cold", "marine_heat_wave")


# Functions -------
# makes all the column names lowercase
make_lowercase <- function(df){
  for (i in 1:length(names(df))){
    names(df)[i] <- tolower(names(df)[i])
  }
  return(df)
}
# separates out species and gets rid of unnecessary cols
separate_species <- function(spec_code){
  dat <- len_extended %>%
    filter(species_code==spec_code) %>%
    select(all_of(categories_wanted)) %>%
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


# 6) Divide out by species ----
# Alaska plaice
akp <- separate_species(10285)
# flathead sole
fhs <- separate_species(10130) 
# northern rock sole
nrs <- separate_species(10261) 
# yellowfin sole
yfs <- separate_species(10210)


# Write out all files ----
write_rds(akp, path = here("data", "intermediates/akp.rds"))
write_rds(fhs, path = here("data", "intermediates/fhs.rds"))
write_rds(nrs, path = here("data", "intermediates/nrs.rds"))
write_rds(yfs, path = here("data", "intermediates/yfs.rds"))
write_rds(year_temp_categories, here(path = "data", "/intermediates/year_temp_categories.rds"))
write_rds(catch, path = here("data", "intermediates/catch.rds"))