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
haul <- read_rds("././data/intermediates/haul.rds")


make_one_row_per_fish <- function(df){
  new_df <- df %>%
    uncount(round(n_haul)) %>%
    subset(select =c(-n_haul))
}

akp_long <- make_one_row_per_fish(akp)
fhs_long <- make_one_row_per_fish(fhs)
nrs_long <- make_one_row_per_fish(nrs)
yfs_long <- make_one_row_per_fish(yfs)