#' title: EBS Continental Shelf Survey Flatfish Visualizations
#' purpose: This loads all of the cleaned data into the environment.
#' date: 2020-12-31
#' author: Ericka B. Smith
#' ---


akp <- read_rds(here("data/intermediates", "akp.rds"))
fhs <- read_rds(here("data/intermediates", "akp.rds"))
nrs <- read_rds(here("data/intermediates", "akp.rds"))
yfs <- read_rds(here("data/intermediates", "akp.rds"))
year_temp_categories <-
  read_rds(here("data/intermediates", "year_temp_categories.rds"))
