# Identify trees to cores:
# select trees found dead for any species with ≥2 consecutive years of mortality rate ≥3 % yr-1 or AWM ≥0.5 Mg C ha-1 yr-1.
# max n per species : no max, but pause at 10 and continue only if time permits
# no min size, but prioritize larger trees (start at largest and work down)
# prioritize and do what time permits.
# We will need to clarify some specifics (e.g., max n & min size trees to core for species identified for coring) and use data in this repository to identify relevant species. Ideally, the two consecutive years would be the current and past year, which means the census has to be completed before we identify the species
library(tidyverse)

sp_mortrate_agbmort <- read.csv("R_results/mortrates_and_agbmort_by_species.csv")


# species with ≥2 consecutive years of mortality rate ≥3 % yr-1 
sp_selected_1 <- sp_mortrate_agbmort %>%
  # filter for sp and yr with estimates passing threshold
  filter(variable == "mortality_rates", estimate >= 3) %>% 
  group_by(sp) %>%
  reframe(estimate=estimate,
          survey_year = survey_year,
          survey_year_diff = c(NA, diff(survey_year))) %>%
  filter(survey_year_diff==1) %>%
  # select current year
  filter(survey_year == 2026) %>% 
  pull(sp) %>%
  unique()

# species with AWM ≥0.5 Mg C ha-1 yr-1
sp_selected_2 <-  sp_mortrate_agbmort %>%
  # filter for sp and yr with estimates passing threshold
  filter(variable == "biomass_mortality", estimate >= 0.5) %>% 
  group_by(sp) %>%
  reframe(estimate=estimate,
          survey_year = survey_year,
          survey_year_diff = c(NA, diff(survey_year))) %>%
  filter(survey_year_diff==1) %>%
  # select current year
  filter(survey_year == 2026) %>% 
  pull(sp) %>%
  unique()


# now select dead trees within those species
load("data/allmort.RData")

trees_to_choose_from <- allmort %>% filter(sp %in% c(sp_selected_1, sp_selected_2)) %>%
  mutate(tag_StemTag = paste(tag, StemTag, sep = "_"))


trees_to_choose_from <- trees_to_choose_from %>%
  # select trees that are dead 
  filter(!grepl("A", current_year_status)) %>%
  # get the year of death of each stem so we use the most recently dead as priority
  mutate(death_year = min(survey_year), .by= tag_StemTag) %>%
  # only keep trees that died within the last 5 years
  filter(death_year >= (max(trees_to_choose_from$survey_year) - 3))

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
  slice_max(survey_year, by = tag_StemTag)

table(trees_to_choose_from$sp)
table(trees_to_choose_from$survey_year)
table(trees_to_choose_from$death_year)


# save 
write.csv(trees_to_choose_from, "candidate_trees_for_coring_2026.csv", row.names = F)

