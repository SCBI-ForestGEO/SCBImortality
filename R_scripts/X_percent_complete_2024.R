#Read in data
mortality_trees <- read.csv("raw_data/Field_Maps/SCBI_trees_mortality_2024_0.csv")
mortality_stems <- read.csv("raw_data/Field_Maps/SCBI_stems_mortality_2024_1.csv")

#Number of stems completed - Status as LI, DS, DC, DN
number_completed_trees <- sum(mortality_trees$status_2024 %in% c("LI", "DS", "DC", "DN"))
number_completed_stems <- sum(mortality_stems$status_2024 %in% c("LI", "DS", "DC", "DN"))                       
total_number <- number_completed_stems + number_completed_trees  
total_rows <- nrow(mortality_trees) + nrow(mortality_stems)

#Percentage 
percent_completed <- (total_number/total_rows) * 100

