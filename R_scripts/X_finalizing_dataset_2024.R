### Finalizing Mortality Data - 2024 ###

library(tidyverse)

# Load raw data 
main_stems <- read.csv("raw_data/Field_Maps/SCBI_trees_mortality_2024_0.csv")
secondary_stems <- read.csv("raw_data/Field_Maps/SCBI_stems_mortality_2024_1.csv")
missing_secondary_stems <- read.csv("data/missing_secondary_stems.csv")

# Combine data into a single dataframe
full_dataset <- bind_rows(main_stems, secondary_stems, missing_secondary_stems)

# Remove duplicated secondary stems
full_dataset <- full_dataset %>%
  filter(mort_census_status != "not started")

# Fix dead_with_resprout
full_dataset$dead_with_resprout <- if_else(is.na(full_dataset$dead_with_resprout), 0, full_dataset$dead_with_resprout)
full_dataset$dead_with_resprout <- if_else(full_dataset$status_2024 == "LI", NA, full_dataset$dead_with_resprout)

unique(full_dataset$personnel)

write.csv(full_dataset, "data/full_dataset_test.csv")

# Fix personnel_list
full_dataset <- full_dataset %>%
  mutate(missing_personnel_IK = grepl("IK", full_dataset$comment_mortality_2024))

full_dataset <- full_dataset %>%
  mutate(missing_personnel_KJB = grepl("KJB", full_dataset$comment_mortality_2024))

full_dataset <- full_dataset %>%
  mutate(missing_personnel_ES = grepl("ES", full_dataset$comment_mortality_2024))

full_dataset <- full_dataset %>%
  mutate(missing_personnel_ÉS = grepl("ÉS", full_dataset$comment_mortality_2024))

full_dataset <- full_dataset %>%
  mutate(missing_personnel_EE = grepl("EE", full_dataset$comment_mortality_2024))

full_dataset <- full_dataset %>%
  mutate(missing_personnel_Es = grepl("Es", full_dataset$comment_mortality_2024))

full_dataset$personnel <- if_else(full_dataset$missing_personnel_IK == TRUE, paste0(full_dataset$personnel, ",IK"), full_dataset$personnel)
full_dataset$personnel <- if_else(full_dataset$missing_personnel_KJB == TRUE, paste0(full_dataset$personnel, ",KJB"), full_dataset$personnel)
full_dataset$personnel <- if_else(full_dataset$missing_personnel_ES == TRUE, paste0(full_dataset$personnel, ",ES"), full_dataset$personnel) 
full_dataset$personnel <- if_else(full_dataset$missing_personnel_ÉS == TRUE, paste0(full_dataset$personnel, ",ES"), full_dataset$personnel) 
full_dataset$personnel <- if_else(full_dataset$missing_personnel_EE == TRUE, paste0(full_dataset$personnel, ",ES"), full_dataset$personnel) 
full_dataset$personnel <- if_else(full_dataset$missing_personnel_Es == TRUE, paste0(full_dataset$personnel, ",ES"), full_dataset$personnel) 

full_dataset$missing_personnel_IK <- NULL
full_dataset$missing_personnel_KJB <- NULL
full_dataset$missing_personnel_ES <- NULL
full_dataset$missing_personnel_EE <- NULL
full_dataset$missing_personnel_Es <- NULL
full_dataset$missing_personnel_ÉS <- NULL

unique(full_dataset$personnel)

full_dataset$personnel <- if_else(full_dataset$StemTag != 1, NA_character_, full_dataset$personnel)

unique(full_dataset$personnel)

full_dataset$personnel <- as.character(full_dataset$personnel)

full_dataset <- full_dataset %>%
  mutate(personnel = gsub("Other,", "", personnel))

unique(full_dataset$personnel)

# Minor edits
full_dataset[["comment_mortality_2024"]] [1177] <- ""
full_dataset[["comment_mortality_2024"]] [1166] <- "Listed as quad 101 in QAQC"
full_dataset[["fad_2024"]] [1193] <- "DF,F"
full_dataset[["comment_mortality_2024"]] [1385] <- "DBH confirmed"
full_dataset[["comment_mortality_2024"]] [1708] <- "DBH confirmed"
full_dataset[["comment_mortality_2024"]] [1841] <- "Tree is broken 75 percent of the way up"
full_dataset[["comment_mortality_2024"]] [1841] <- "Tree is broken 70 percent of the way up. New growth does not equal diameter at breakage"
full_dataset[["comment_mortality_2024"]] [2421] <- ""
full_dataset[["comment_mortality_2024"]] [2440] <- "Tree broken 75 percent of the way up"
full_dataset[["comment_mortality_2024"]] [2484] <- "Not leaning at dbh, but crown leaning in the direction of 30 degrees"
full_dataset[["comment_mortality_2024"]] [2488] <- "Nree not leaning at dbh, but crown leaning in the direction of 60 degrees"
full_dataset[["comment_mortality_2024"]] [2516] <- "Tree is broken 60 percent of the way up. New growth does not equal diameter at breakage"
full_dataset[["comment_mortality_2024"]] [2516] <- "Not leaning at dbh, but crown leaning in direction of 39 degrees"
full_dataset[["fad_2024"]] [2854] <- "CR,W,R"
full_dataset[["rot_2024"]] [2854] <- 2
full_dataset[["comment_mortality_2024"]] [3122] <- "Not leaning at dbh, but crown is leaning in the direction of 278 degrees"
full_dataset[["comment_mortality_2024"]] [3409] <- "Tree broken 65 percent of the way up"
full_dataset[["comment_mortality_2024"]] [3658] <- "Tree broken 65 percent of the way up"
full_dataset[["comment_mortality_2024"]] [3669] <- "Stem is broken 25 percent of the way up, secondary stem has replaced original"
full_dataset[["comment_mortality_2024"]] [3796] <- "Tree bent over. I believe that 20140 and 20139 may have been ,used up last year; ES"
full_dataset[["comment_mortality_2024"]] [4186] <- ""
full_dataset[["comment_mortality_2024"]] [5945] <- "75 percent of trunk is gone. Stem is broken 80 percent"
full_dataset[["fad_2024"]] [5945] <- "B"
full_dataset[["fad_2024"]] [7096] <- "DF,K"
full_dataset[["fad_2024"]] [7106] <- "K"
full_dataset[["comment_mortality_2024"]] [7324] <- "Dead and down found in this location, unable to find tag"
full_dataset[["fad_2024"]] [7492] <- ""
full_dataset[["fad_choices"]] [7492] <- ""
full_dataset[["fad_2024"]] [7495] <- ""
full_dataset[["fad_choices"]] [7495] <- ""
full_dataset[["fad_2024"]] [7496] <- ""
full_dataset[["fad_choices"]] [7496] <- ""
full_dataset[["fad_2024"]] [7499] <- ""
full_dataset[["fad_choices"]] [7499] <- ""
full_dataset[["fad_2024"]] [7500] <- ""
full_dataset[["fad_choices"]] [7500] <- ""


# Final data product
write.csv(full_dataset, "data/mortality_2024.csv")

