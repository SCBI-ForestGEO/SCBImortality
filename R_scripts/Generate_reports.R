# Generate reports looking at latest mortality census raw data ####
## this script is run automatically when there is a push 

# clear environment ####
rm(list = ls())

# load libraries ####
library(here)

# load latest mortality data ####

## get the name of all mortality census raw data files
  all_mort_files <- list.files(here("raw_data"), pattern = "Mortality_Survey_.*csv", full.names = T)
  
## get the index of the latest mortality survey and second to last
  last <- which.max(as.numeric(substr(all_mort_files, nchar(all_mort_files)-7, nchar(all_mort_files)-4))) 
  # second_to_last <- last -1

## load the latest mortality survey
  
  mort <- read.csv(all_mort_files[last]) 
  # prev_mort <- read.csv(all_mort_files[second_to_last]) 
  
# load and clean up the 3rd main census ####
  main_census <-  read.csv(paste0("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.stem3.csv"))

## convert dbh to numeric
  main_census$dbh <- as.numeric(main_census$dbh)

## only keep trees > 10cm except for fraxinus and Chionanthus virginicus
  main_census <-  main_census[grepl("^fr..|^ch..", main_census$sp) | (!is.na(main_census$dbh) & main_census$dbh >= 100), ]

## remove trees that are dead ####
  main_census <- main_census[!main_census$status %in% "D",]
  
# load species table ####
  
  spptable <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.spptable.csv")
  

# check if all species exist in species table, if not save a file, if yes, delete that file ####
  reports_species_code_error_file <- file.path(here("testthat"), "reports/species_code_error.csv")
  
  idx_species_error <- !mort[,grepl("^sp", names(mort))] %in% spptable$sp
  
  if(any(idx_species_error)) {
    write.csv(mort[idx_species_error,], file = reports_species_code_error_file, row.names = F)
  } else {
    if(file.exists(reports_species_code_error_file) ) file.remove(reports_species_code_error_file)
  }
  
  
# for each quadrat censused, check all expected trees were censused ####
  reports_quad_censused_error_file <- file.path(here("testthat"), "reports/quadrat_censused_missing_stems.csv")
  
  
  idx_quadrat_censused <- main_census$quadrat %in% mort$quadrat
  
  
  tag_stem_missing_from_main_census <- paste(main_census$tag, main_census$StemTag)[idx_quadrat_censused] [!paste(main_census$tag, main_census$StemTag)[idx_quadrat_censused] %in% paste(mort$tag, mort$stem)]
  table(main_census[paste(main_census$tag, main_census$StemTag) %in% tag_stem_missing_from_main_census, ]$sp)
  

  
  if(length(tag_stem_missing_from_main_census) > 1) {
    write.csv(main_census[paste(main_census$tag, main_census$StemTag) %in% tag_stem_missing_from_main_census, ], file = reports_quad_censused_error_file, row.names = F)
  } else {
    if(file.exists(reports_quad_censused_error_file) ) file.remove(reports_quad_censused_error_file)
  }
  
  
  
# give a % completion status ####
  percent_completion <- round(sum(paste(main_census$tag, main_census$StemTag) %in% paste(mort$tag, mort$stem)) / nrow(main_census) * 100)
  
  png(file.path(here("testthat"), "reports/percent_completion.png"), width = 1, height = 1, units = "in", res = 150)
  par(mar = c(0,0,0,0))
  plot(0,0, axes = F, xlab = "", ylab = "", type = "n")
  text(0,0, paste(percent_completion, "%"))
  dev.off()
  # write.table(percent_completion, file = file.path(here("testthat"), "reports/percent_completion.txt"),  col.names = F, row.names = F)
  