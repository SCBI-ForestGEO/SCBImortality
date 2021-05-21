library(here)

test_that("All species are valid in latest mort census", {
  
  
  # read latest mortality census survey
  all_mort_files <- list.files(here("raw_data"), pattern = "Mortality_Survey_.*csv", full.names = T)
  
  i <- which.max(as.numeric(substr(all_mort_files, nchar(all_mort_files)-7, nchar(all_mort_files)-4)))
  
  mort <- read.csv(all_mort_files[i])
  
  
  # read species table
  
  spptable <- read.csv("https://raw.githubusercontent.com/SCBI-ForestGEO/SCBI-ForestGEO-Data/master/tree_main_census/data/census-csv-files/scbi.spptable.csv")
  
  # check if all species exist in species table
  
  if(!all(mort[,grepl("^sp", names(mort))] %in% spptable$sp)) {
    write.csv(mort[!mort[,grepl("^sp", names(mort))] %in% spptable$sp,], file = file.path(here("raw_data"), "testthat/reports/species_code_error.csv"), row.names = F)
  } else {
    file.remove(file.path(here("raw_data"), "testthat/reports/species_code_error.csv"))
  }
  
  expect_true(all(mort[,grepl("^sp", names(mort))] %in% spptable$sp))
})