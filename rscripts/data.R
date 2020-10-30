#' title: EBS Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates plots of Alaska plaice, flathead sole, northern rock sole, and yellowfin sole
#'          from 2000-2018 based on the RACE EBS survey
#' date: 2020-10-13
#' ---

# Import Data -----

len <- read_csv(".././data/data2/Shelf_Flatfish_Haul_Catch_Length.csv", guess_max = 10000)
haul <- read_csv(".././data/data2/Shelf_Flatfish_Haul_Catch.csv", guess_max = 10000)

# Data Wrangle -----

# make all lowercase
len <- make_lowercase(len)
haul <- make_lowercase(haul)

# get haul set up 
haul <- haul %>%
  # get only 2000-2018 and only Bering Sea
  filter(cruise>= 199999 & cruise<201900,
         region=="BS")%>%
  # create year variable
  mutate(year=floor((cruise/100)))

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
         n_haul = (ss_prop*catch_number_fish))
# removes the large length dataset from our environment (bc we have the expanded one now)
remove(len)

# divide out species
akp <- separate_spec(10285)
fhs <- separate_spec(10130)
nrs <- separate_spec(10261)
yfs <- separate_spec(10210)
# removes the large length expanded dataset from our environment (bc we have one per species now)
remove(len_e)
