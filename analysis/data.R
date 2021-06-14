#' title: EBS Continental Shelf Survey Flatfish Visualizations
#' purpose: This loads all of the cleaned data into the environment.
#' date: 2021-04-26
#' author: Ericka B. Smith
#' ---

library(readr)
library(here)

# akp <- read_rds(here("data/intermediates", "akp.rds"))
# fhs <- read_rds(here("data/intermediates", "fhs.rds"))
# nrs <- read_rds(here("data/intermediates", "nrs.rds"))
yfs <- read_rds(here("data/intermediates", "yfs.rds"))

year_temp_categories <-
  read_rds(here("data/intermediates", "year_temp_categories.rds"))
