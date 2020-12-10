#' title: EBS Continental Shelf Survey Flatfish Visualizations
#' purpose: This script creates plots of Alaska plaice, flathead sole, northern rock sole, and yellowfin sole
#'          from 2000-2018 based on the RACE EBS survey
#' date: 2020-11-21
#' ---


akp <- read_rds("././data/intermediates/akp.rds")
fhs <- read_rds("././data/intermediates/fhs.rds")
nrs <- read_rds("././data/intermediates/nrs.rds")
yfs <- read_rds("././data/intermediates/yfs.rds")
year_temp_categories <- read_rds("././data/intermediates/year_temp_categories.rds")
catch <- read_rds("././data/intermediates/catch.rds")
