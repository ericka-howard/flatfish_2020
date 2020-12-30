#' title:   Eastern Bering Sea Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates contour and box plots of Alaska plaice, flathead sole, 
#'          northern rock sole, and yellowfin sole from 2000-2018 based on the RACE EBS 
#'          survey
#' date:    2020-11-24
#' ---

# Load in cleaned data
# Data was created in initial_data_cleaning.R
akp <- read_rds("./new_slushpile/intermediates/akp.rds")
fhs <- read_rds("./new_slushpile/intermediates/fhs.rds")
nrs <- read_rds("./new_slushpile/intermediates/nrs.rds")
yfs <- read_rds("./new_slushpile/intermediates/yfs.rds")
year_temp_categories <- read_rds("./new_slushpile/intermediates/year_temp_categories.rds")
catch <- read_rds("./new_slushpile/intermediates/catch.rds")
haul <- read_rds("./new_slushpile/intermediates/haul.rds")
