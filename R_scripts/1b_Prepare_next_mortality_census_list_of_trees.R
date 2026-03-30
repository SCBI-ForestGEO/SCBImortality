# prepare list of tress to visit during the mortality census

# load libraries ####
library(tidyverse)

# clear environment ####
rm(list = ls())

# Load data ####
load("data/allmort.RData")

# prepare list ####

## looking for trees that are >= 10 cm and were found either alive or dead standing in previous mortality census
last_main_census_year <- 2025

new_mort <- allmort %>% filter(survey_year %in% last_main_census_year,
                   last_main_census_dbh >= 100,
                   current_year_status %in% c("A", "AU", "DS")) %>%
  select(c("survey_year", "tag", "StemTag", "sp", "genus", "species", 
           "quadrat", "gy", "gx", "ly", "lx", 
           "last_main_census_dbh", "hom", "last_main_cenus_status",
           "current_year_status", 
           "percent_of_crown_intact", "percent_of_crown_living", 
           "fad", "sp_affected_by_eab", "current_year_comment" ))

# rename some columns:

old <- c(names(new_mort)[grepl("current_year", names(new_mort))], "percent_of_crown_intact", "percent_of_crown_living", "fad")
new <- paste(gsub("current_year_", "", old), last_main_census_year, sep = "_")


new_mort <- new_mort %>% 
  rename(all_of(setNames(old,  new)))


# save ####
write.csv(new_mort, paste0("begining_of_census_files/list_of_trees_for_", last_main_census_year+1, ".csv"), row.names = F)

