#' title:   Eastern Bering Sea Continental Shelf Survey Flatfish Visualizations
#' purpose: This script loads the phi data into the environment.
#' date:    2020-11-24
#' author: Ericka B. Smith
#' ---

# Load in cleaned data
# Data was created in data_initial.R
akp <- read_rds("./data/intermediates/akp_phi.rds")
fhs <- read_rds("./data/intermediates/fhs_phi.rds")
nrs <- read_rds("./data/intermediates/nrs_phi.rds")
yfs <- read_rds("./data/intermediates/yfs_phi.rds")
year_temp_categories <-
  read_rds("./data/intermediates/year_temp_categories.rds")
catch <- read_rds("./data/intermediates/catch.rds")
