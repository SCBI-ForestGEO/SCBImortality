# Identify trees to cores: see issue #147
# select trees from any species for which the summed AWM over the past two years is ≥0.25 Mg C ha-1 yr-1.
# prioritize and do what time permits.

# load libraries
library(tidyverse)


# clear environment
rm(list = ls())

# load data
sp_mortrate_agbmort <- read.csv("R_results/mortrates_and_agbmort_by_species.csv")
load("data/allmort.RData")

# get the list of trees to core


## species for which the summed AWM over the past two years is ≥0.25 Mg C ha-1 yr-1.
sp_selected <-  sp_mortrate_agbmort %>%
  # filter for sp and yr with estimates passing threshold
  filter(variable == "biomass_mortality", survey_year >= max(sp_mortrate_agbmort$survey_year) - 2) %>% 
  group_by(sp) %>%
  reframe(estimate=sum(estimate)) %>%
  filter(estimate>=0.25) %>%
  pull(sp)
 


## now select dead trees within those species


trees_to_choose_from <- allmort %>% filter(sp %in% sp_selected) %>%
  mutate(tag_StemTag = paste(tag, StemTag, sep = "_"))


trees_to_choose_from <- trees_to_choose_from %>%
  # select trees that are dead 
  filter(!grepl("A", current_year_status)) %>%
  # get the year of death of each stem so we use the most recently dead as priority
  mutate(death_year = min(survey_year), .by= tag_StemTag) %>%
  # only keep trees that died within the last 5 years
  filter(death_year >= (max(trees_to_choose_from$survey_year) - 1))

# remove trees that are not standing anymore
stems_to_ignore <- trees_to_choose_from %>% filter(current_year_status %in% c("G", "DN", "DC")) %>% pull(tag_StemTag)

trees_to_choose_from <- trees_to_choose_from %>% filter(!tag_StemTag %in% stems_to_ignore)

# remove trees that have NA in last main census dbh ()
trees_to_choose_from <- trees_to_choose_from %>% filter(!is.na(last_main_census_dbh))

# now arrange by desc size and desc death year so we pick the first n larger most recent trees for each species
trees_to_choose_from <- trees_to_choose_from %>%
  arrange(sp, desc(death_year), desc(last_main_census_dbh))

# slice latest year for each stem

trees_to_choose_from <- trees_to_choose_from %>%
  slice_max(survey_year, by = tag_StemTag) %>%
  select(sp, death_year, tag ,StemTag, last_main_census_dbh, hom, quadrat, gy, gx, ly, lx,  percent_of_crown_intact, fad)

table(trees_to_choose_from$sp)
table(trees_to_choose_from$survey_year)
table(trees_to_choose_from$death_year)
table(trees_to_choose_from$sp,trees_to_choose_from$death_year)

# save 
write.csv(trees_to_choose_from, "candidate_trees_for_coring_2026.csv", row.names = F)

