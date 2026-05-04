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
                   current_year_status %in% c("A", "AU", "DS")) 

last_mort <- allmort %>% filter(survey_year %in% last_main_census_year)

new_mort <- new_mort %>%
  select(c("survey_year", "tag", "StemTag", "sp", "genus", "species", 
           "quadrat", "gy", "gx", "ly", "lx", 
           "last_main_census_dbh", "hom", "last_main_cenus_status",
           "current_year_status", 
           "percent_of_crown_intact", "percent_of_crown_living", 
           "fad", "sp_affected_by_eab", "current_year_comment" ))

last_mort <- last_mort %>%
  select(c("survey_year", "tag", "StemTag", "sp", "genus", "species", 
           "quadrat", "gy", "gx", "ly", "lx", 
           "last_main_census_dbh", "hom", "last_main_cenus_status",
           "current_year_status", 
           "percent_of_crown_intact", "percent_of_crown_living", 
           "fad", "sp_affected_by_eab", "current_year_comment" ))




# check a few things
nrow(new_mort)


nrow(last_mort) - sum(!last_mort$current_year_status %in% c("A", "AU", "DS"))


x <- setdiff(paste(last_mort$tag, last_mort$StemTag)[last_mort$current_year_status %in% c("A", "AU", "DS")], paste(new_mort$tag, new_mort$StemTag))
length(x)

setdiff(paste(new_mort$tag, new_mort$StemTag), paste(last_mort$tag, last_mort$StemTag)[last_mort$current_year_status %in%  c("A", "AU", "DS")]) # Should be 0

missing_in_newmort <- last_mort[paste(last_mort$tag, last_mort$StemTag) %in% x, ] 
missing_in_newmort$current_year_status
missing_in_newmort$sp
missing_in_newmort$dbh_2023


last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% mutate(last_main_census_dbh_0 = is.na(last_main_census_dbh)) %>%  select( current_year_status, last_main_census_dbh_0) %>% table(useNA = "ifany")

# last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% filter(last_main_census_dbh %in% NA) %>% select(sp, last_main_cenus_status) %>% table(useNA = "ifany")
# 
# nrow(last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% filter(current_year_status %in% "DS", last_main_census_dbh %in% NA))
# nrow(last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% filter(current_year_status %in% "DS", last_main_census_dbh >0))
# nrow(last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% filter(current_year_status %in% "DS", last_main_census_dbh >0))
# 
# 
# last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% filter(last_main_census_dbh >0) %>% select(sp, last_main_cenus_status) %>% table(useNA = "ifany")
# last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% filter(last_main_census_dbh >0) %>% select(sp, current_year_status) %>% table(useNA = "ifany")
# last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% filter(last_main_census_dbh >0) %>% select(last_main_census_dbh) %>% range()
# 
# last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% filter(last_main_census_dbh %in% NA) %>% select(sp, last) %>% table(useNA = "ifany")
# last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% select(sp, current_year_status) %>% table(useNA = "ifany")
# last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% select(sp, current_year_status) %>% table(useNA = "ifany")
# last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% select(sp, current_year_status) %>% table(useNA = "ifany")
# 
# last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% select(sp, current_year_status, last_main_census_dbh) %>% group_by(current_year_status, sp) %>% reframe(n_NA = sum(is.na(last_main_census_dbh)), min_dbh = min(last_main_census_dbh, na.rm = T), max_dbh = max(last_main_census_dbh, na.rm = T)) %>% View()
# 
# last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% filter(is.na(last_main_census_dbh)) %>% View()


# ---  lets bring back the current_year_status A or AU + last_main_census_dbh is NA--- #

to_add <- last_mort[ paste(last_mort$tag, last_mort$StemTag) %in% x, ] %>% filter(is.na(last_main_census_dbh), current_year_status %in% c("A", "AU"))

nrow(to_add) # should be 16 for 2026 preparation

new_mort <- rbind(new_mort, to_add) %>% arrange(quadrat, tag, StemTag)

# new_mort_test <- allmort %>% filter(survey_year %in% last_main_census_year,
#                                last_main_census_dbh >= 100,
#                                current_year_status %in% c("A", "AU", "DS")) %>%
#   select(c("survey_year", "tag", "StemTag", "sp", "genus", "species", 
#            "quadrat", "gy", "gx", "ly", "lx", 
#            "last_main_census_dbh", "hom", "last_main_cenus_status",
#            "current_year_status", 
#            "percent_of_crown_intact", "percent_of_crown_living", 
#            "fad", "sp_affected_by_eab", "current_year_comment" ))


# rename some columns:
old <- c(names(new_mort)[grepl("current_year", names(new_mort))], "percent_of_crown_intact", "percent_of_crown_living", "fad")
new <- paste(gsub("current_year_", "", old), last_main_census_year, sep = "_")


new_mort <- new_mort %>% 
  rename(all_of(setNames(old,  new)))



# save ####
write.csv(new_mort, paste0("begining_of_census_files/list_of_trees_for_", last_main_census_year+1, ".csv"), row.names = F)

# # TO DELETE LATER
# 
# 
# last_mort_as_in_ESRI <- read.csv("C:/Users/herrmannV/Downloads/NZCBI_Mortality_20_may_2025/SCBI_stems_mortality_2025_1.csv")
# last_mort_as_in_ESRI <- read.csv("C:/Users/herrmannV/Downloads/NZCBI_Mortality_20_may_2025/SCBI_stems_mortality_2025_1.csv")
# 
# tree25_in_ESRI <- read.csv("C:/Users/herrmannV/Downloads/NZCBI_Mortality_20_may_2025 (1)/SCBI_trees_mortality_2025_all_0.csv")
# stem25_in_ESRI <- read.csv("C:/Users/herrmannV/Downloads/NZCBI_Mortality_20_may_2025 (1)/SCBI_stems_mortality_2025_1.csv") 
# 
# last_mort_as_in_ESRI <- bind_rows(tree25_in_ESRI, stem25_in_ESRI)
# 
# 
# nrow(last_mort_as_in_ESRI) - sum(!last_mort_as_in_ESRI$status_2025 %in% c("LI", "DS"))
# 
# y <- setdiff(paste(last_mort_as_in_ESRI$tag, last_mort_as_in_ESRI$StemTag)[last_mort_as_in_ESRI$status_2025 %in% c("LI", "DS")], paste(new_mort$tag, new_mort$StemTag))
# 
# length(y)
# 
# 
# 
# setdiff(paste(new_mort$tag, new_mort$StemTag), paste(last_mort_as_in_ESRI$tag, last_mort_as_in_ESRI$StemTag)[last_mort_as_in_ESRI$status_2025 %in% c("LI", "DS")])
# 
# missing_in_newmort <- last_mort_as_in_ESRI[paste(last_mort_as_in_ESRI$tag, last_mort_as_in_ESRI$StemTag) %in% y, ] 
# missing_in_newmort$status_2025
# missing_in_newmort$sp
# missing_in_newmort$dbh_2023